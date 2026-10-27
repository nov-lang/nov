const std = @import("std");
const TestAllocations = @import("TestAllocations.zig");
const Diagnostics = @import("Diagnostics.zig");
const Ast = @import("Ast.zig");
const Module = @import("Module.zig");
const Ir = @import("Ir.zig");
const TypeResolver = @import("TypeResolver.zig");
const Literal = @import("Literal.zig");
const BodyChecker = @import("BodyChecker.zig");
const primitives = @import("primitives.zig");
const Sema = @This();
const Compilation = @import("Compilation.zig");
const Dependencies = @import("Dependencies.zig");

allocator: std.mem.Allocator,
ir: Ir,
entries: []Entry,
compilation: ?*Compilation = null,
/// True only after whole-module lowering and structural verification succeed.
/// Entry-point/backend/runtime configuration is still a separate obligation.
lowering_complete: bool = false,

pub const Status = enum { ignored, deferred, generic, unrequested, requested, namespace, type_alias, signature_only, external, constant_checked, body_checked, lowered, failed };
pub const Entry = struct {
    type_id: ?Ir.TypeId = null,
    /// Represented type, never the runtime type of this declaration.
    alias_type: ?Ir.TypeId = null,
    constant: ?Ir.ConstId = null,
    function: ?Ir.FunctionId = null,
    status: Status = .deferred,
    noreturn: bool = false,
};
pub const Error = Ir.BuildError || error{InvalidModule};

/// A partial semantic pass: resolves explicit annotations and literal constants,
/// leaving concrete generic method signatures dormant until referenced.
/// Deferred entries and signatures are not checked bodies or executable IR.
/// Owns all output storage; OOM rolls back diagnostics appended to the module.
pub fn analyzeAnnotations(module: *Module) Error!Sema {
    if (module.hasErrors()) return error.InvalidModule;
    const diagnostics_start = module.diagnostics.items.len;
    errdefer module.diagnostics.shrinkRetainingCapacity(diagnostics_start);
    module.clearSpecializations();
    module.resetAliases();
    try module.classifyAliases();
    var result: Sema = .{
        .allocator = module.allocator,
        .ir = Ir.init(module.allocator),
        .entries = &.{},
    };
    errdefer result.deinit();
    result.ir.source_len = @intCast(module.source.ast.source.len);
    try result.analyzeModuleAnnotations(module);
    try result.finalizeRecordLayouts(module);
    var exports: std.StringHashMapUnmanaged(ForeignSymbol) = .empty;
    defer exports.deinit(result.allocator);
    try result.validateExports(module, &exports);
    return result;
}

fn analyzeModuleAnnotations(result: *Sema, module: *Module) Error!void {
    try result.syncEntries(module);
    for (module.declarations.items, 0..) |decl, i| if (decl.concrete_type == null) {
        result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))] = .{};
    };
    for (module.declarations.items, 0..) |declaration, i| {
        if (declaration.is_test and !module.testEnabled()) {
            result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].status = .ignored;
            continue;
        }
        if (declaration.is_generic) {
            if (declaration.is_record) _ = try TypeResolver.validateGeneric(module, @fromBackingInt(@intCast(i)));
            result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].status = .generic;
            continue;
        }
        if (!declaration.is_record) continue;
        const init_node = module.source.ast.decl(declaration.node).ast.init_node;
        result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].alias_type = if (module.source.ast.tokens.items(.tag)[module.source.ast.nodes.items(.main_token)[init_node]] == .keyword_enum)
            try TypeResolver.resolve(module, &result.ir, init_node)
        else
            try result.ir.internType(.{ .record = .{ .source = module.source.id, .identity = @intCast(i), .fields = &.{}, .public = declaration.public, .tagged = module.source.ast.isUnion(init_node) } });
        result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].status = .type_alias;
    }
    var i: usize = 0;
    while (i < module.declarations.items.len) : (i += 1) {
        try result.syncEntries(module);
        const declaration = module.declarations.items[i];
        if (declaration.concrete_type != null or (declaration.is_test and !module.testEnabled())) continue;
        const full = module.source.ast.decl(declaration.node);
        if (declaration.is_namespace) {
            result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].status = .namespace;
            continue;
        }
        if (declaration.is_record or declaration.is_generic) continue;
        if (declaration.container) |owner| if (module.getDeclaration(owner).?.is_generic and declaration.concrete_type == null) {
            result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].status = .generic;
            continue;
        };
        if (TypeResolver.isThisAlias(&module.source.ast, declaration.node) and declaration.container != null) {
            result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].alias_type = try TypeResolver.resolveInContainer(module, &result.ir, full.ast.init_node, declaration.container, declaration.concrete_type);
            result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].status = .type_alias;
            continue;
        }
        if (declaration.container != null and (full.ast.is_mutable or module.source.ast.fullFnProto(full.ast.type_node) == null)) {
            try module.diagnostics.append(module.allocator, .{ .tag = .unsupported_declaration, .span = declaration.name_span });
            result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].status = .failed;
            continue;
        }
        if (declaration.is_type_alias) {
            if (primitives.isPrimitive(declaration.name)) {
                try module.diagnostics.append(module.allocator, .{ .tag = .unsupported_shadowing, .span = declaration.name_span });
                result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].status = .failed;
                continue;
            }
            result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].alias_type = try TypeResolver.resolve(module, &result.ir, full.ast.init_node);
            result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].status = if (result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].alias_type != null) .type_alias else .failed;
            continue;
        }
        if (full.ast.type_node == 0) {
            if (!full.ast.is_mutable and full.ast.init_node != 0) try result.checkScalarGlobal(module, i, full.ast.init_node, null);
            continue;
        }
        const type_id = (try TypeResolver.resolveInContainer(module, &result.ir, full.ast.type_node, declaration.container, declaration.concrete_type)) orelse {
            result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].status = .failed;
            continue;
        };
        result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].type_id = type_id;
        if (module.source.ast.fullFnProto(full.ast.type_node) != null) {
            if (declaration.is_test) {
                const signature = result.ir.getType(type_id).?.function;
                if (signature.params.len != 0 or result.ir.getType(signature.result).? != .void) {
                    try module.diagnostics.append(module.allocator, .{ .tag = .invalid_test_signature, .span = declaration.name_span });
                    result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].status = .failed;
                    continue;
                }
            }
            result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].noreturn = declaration.noreturn;
            if (declaration.noreturn and result.ir.getType(result.ir.getType(type_id).?.function.result).? != .void) {
                try module.diagnostics.append(module.allocator, .{ .tag = .noreturn_result, .span = declaration.name_span });
                result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].status = .failed;
                continue;
            }
            result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].status = if (declaration.extern_name != null) .external else .signature_only;
        } else if (full.ast.init_node != 0) {
            const annotation = module.tokenSpan(module.source.ast.firstToken(full.ast.type_node));
            if (!full.ast.is_mutable and module.source.ast.fullCall(full.ast.init_node) != null) {
                try result.checkScalarGlobal(module, i, full.ast.init_node, annotation);
            } else {
                try result.checkLiteral(module, i, full.ast.init_node, annotation);
            }
        }
    }
    i = 0;
    while (i < module.declarations.items.len) : (i += 1) {
        try result.syncEntries(module);
        const declaration = module.declarations.items[i];
        if (!declaration.is_record or declaration.is_generic) continue;
        const full = module.source.ast.decl(declaration.node);
        if (declaration.container != null or primitives.isPrimitive(declaration.name)) {
            try module.diagnostics.append(module.allocator, .{ .tag = if (declaration.container != null) .unsupported_declaration else .unsupported_shadowing, .span = declaration.name_span });
            result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].status = .failed;
            continue;
        }
        if (full.ast.is_mutable or full.ast.type_node != 0) {
            try module.diagnostics.append(module.allocator, .{ .tag = .unsupported_type, .span = declaration.name_span });
            continue;
        }
        if (module.source.ast.tokens.items(.tag)[module.source.ast.nodes.items(.main_token)[full.ast.init_node]] == .keyword_enum) continue;
        const tagged = module.source.ast.isUnion(full.ast.init_node);
        var fields: std.ArrayList(Ir.Field) = .empty;
        defer fields.deinit(module.allocator);
        for (module.source.ast.fullStruct(full.ast.init_node).?) |member| {
            if (module.source.ast.nodes.items(.tag)[member] != .container_field) continue;
            const data = module.source.ast.nodes.items(.data)[member];
            const token = module.source.ast.nodes.items(.main_token)[member];
            const span = module.tokenSpan(token);
            const name = module.source.ast.tokenSlice(token);
            var duplicate = module.lookupMethod(@fromBackingInt(@intCast(i)), name) != null;
            for (fields.items) |field| {
                if (std.mem.eql(u8, field.name, name)) duplicate = true;
            }
            if (duplicate) {
                try module.diagnostics.append(module.allocator, .{ .tag = .duplicate_field, .span = span });
                continue;
            }
            const ty = if (tagged and data.lhs == 0) try result.ir.internType(.void) else (try TypeResolver.resolve(module, &result.ir, data.lhs)) orelse continue;
            const ft = result.ir.getType(ty).?;
            if (!(if (tagged) Ir.isUnionPayload(ft) else Ir.isRecordField(ft))) {
                try module.diagnostics.append(module.allocator, .{ .tag = .unsupported_type, .span = module.tokenSpan(module.source.ast.firstToken(data.lhs)) });
                continue;
            }
            try fields.append(module.allocator, .{ .name = name, .ty = ty, .span = span });
        }
        if (tagged and fields.items.len == 0) try module.diagnostics.append(module.allocator, .{ .tag = .empty_union, .span = declaration.name_span });
        try result.ir.setRecordFields(result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].alias_type.?, fields.items);
    }
}

fn foreignScalar(ty: Ir.Type, result: bool) bool {
    return switch (ty) {
        .void => result,
        .boolean => true,
        .integer => |integer| switch (integer.bits) {
            8, 16, 32, 64 => true,
            else => false,
        },
        else => false,
    };
}

const ForeignSymbol = struct {
    span: Module.Span,
    type_id: Ir.TypeId,
    noreturn: bool,
    export_span: ?Module.Span,
};

fn validateExports(self: *Sema, module: *Module, names: *std.StringHashMapUnmanaged(ForeignSymbol)) Error!void {
    for (module.declarations.items, 0..) |declaration, i| {
        const name = declaration.extern_name orelse declaration.export_name orelse continue;
        const entry = &self.entries[self.entryIndex(module, @fromBackingInt(@intCast(i)))];
        const ty = self.ir.getType(entry.type_id orelse continue).?;
        var valid = ty == .function;
        if (ty == .function) {
            valid = foreignScalar(self.ir.getType(ty.function.result).?, true);
            for (ty.function.params) |param| {
                if (!foreignScalar(self.ir.getType(param).?, false)) valid = false;
            }
        }
        if (!valid) {
            try module.diagnostics.append(module.allocator, .{ .tag = if (declaration.extern_name != null) .invalid_extern_signature else .invalid_export_signature, .span = declaration.name_span });
            entry.status = .failed;
            continue;
        }
        if (entry.status == .failed) continue;
        const binding = try names.getOrPut(self.allocator, name);
        if (binding.found_existing) {
            const previous = binding.value_ptr;
            if (declaration.export_name != null) {
                if (previous.export_span) |span| {
                    try module.diagnostics.append(module.allocator, .{ .tag = .duplicate_export_name, .span = declaration.name_span, .related = span });
                } else previous.export_span = declaration.name_span;
            }
            if (previous.type_id != entry.type_id.? or previous.noreturn != entry.noreturn) {
                try module.diagnostics.append(module.allocator, .{ .tag = .conflicting_foreign_signature, .span = declaration.name_span, .related = previous.span });
                entry.status = .failed;
            }
        } else binding.value_ptr.* = .{
            .span = declaration.name_span,
            .type_id = entry.type_id.?,
            .noreturn = entry.noreturn,
            .export_span = if (declaration.export_name != null) declaration.name_span else null,
        };
    }
}

fn validateRecordLayouts(self: *Sema, root: *Module) Error!bool {
    if (try self.ir.recordLayoutIssue()) |issue| {
        const owner = if (self.compilation) |comp| comp.getModule(issue.span.source).? else root;
        try owner.diagnostics.append(owner.allocator, .{ .tag = if (issue.cycle) .record_layout_cycle else .record_layout_limit, .span = issue.span, .related = issue.related });
        return false;
    }
    return true;
}

/// Finalize the entire placeholder graph before defaults or any body can use it.
fn finalizeRecordLayouts(self: *Sema, root: *Module) Error!void {
    if (!try self.validateRecordLayouts(root)) return;
    if (self.compilation) |comp| {
        for (comp.modules.items) |module| try self.finalizeFieldDefaults(module);
    } else try self.finalizeFieldDefaults(root);
}

fn finalizeFieldDefaults(self: *Sema, module: *Module) Error!void {
    const ast = &module.source.ast;
    for (module.declarations.items, 0..) |declaration, i| {
        if (!declaration.is_record or declaration.is_generic) continue;
        const id = self.entries[self.entryIndex(module, @fromBackingInt(@intCast(i)))].alias_type orelse continue;
        const ty = self.ir.getType(id).?;
        if (ty != .record) continue;
        for (ast.fullStruct(ast.decl(declaration.node).ast.init_node).?) |member| {
            if (ast.nodes.items(.tag)[member] != .container_field) continue;
            const data = ast.nodes.items(.data)[member];
            if (data.rhs == 0) continue;
            const span = module.tokenSpan(ast.firstToken(data.rhs));
            if (ty.record.tagged) {
                try module.diagnostics.append(module.allocator, .{ .tag = .unsupported_union_default, .span = span });
                continue;
            }
            const name = ast.tokenSlice(ast.nodes.items(.main_token)[member]);
            for (ty.record.fields, 0..) |field, f| {
                if (!std.mem.eql(u8, name, field.name)) continue;
                if (self.ir.getType(field.ty).? == .record) {
                    try module.diagnostics.append(module.allocator, .{ .tag = .invalid_field_default, .span = span });
                    break;
                }
                const checked = try Literal.check(module, &self.ir, data.rhs, field.ty, field.span);
                switch (checked) {
                    .constant => |constant| @constCast(self.ir.types.items[@backingInt(id)].record.fields)[f].default = constant,
                    .failed => {},
                    .not_literal => try module.diagnostics.append(module.allocator, .{ .tag = .invalid_field_default, .span = span }),
                }
                break;
            }
        }
    }
}

/// Checks the bounded expression-body subset after collecting all annotations.
/// This does not resolve global initializers or produce executable operations.
pub fn analyzeFunctions(module: *Module) Error!Sema {
    const diagnostics_start = module.diagnostics.items.len;
    errdefer module.diagnostics.shrinkRetainingCapacity(diagnostics_start);
    var result = try analyzeAnnotations(module);
    errdefer result.deinit();
    if (module.hasErrors()) return result;
    while (true) {
        try result.syncEntries(module);
        var next: ?usize = null;
        for (result.entries, 0..) |entry, index| {
            if (entry.status == .signature_only or entry.status == .requested) {
                next = index;
                break;
            }
        }
        const index = next orelse break;
        const id: Module.DeclId = @fromBackingInt(@intCast(index));
        if (result.entries[index].status == .requested and try result.requestType(module, id) == null) {
            result.entries[index].status = .failed;
            continue;
        }
        const checked = try BodyChecker.checkDemand(module, &result, id);
        // Checking can instantiate containers and resize the entry table.
        try result.syncEntries(module);
        result.entries[index].status = if (checked) .body_checked else .failed;
    }
    // Demand checking can add layouts after annotation finalization.
    if (!module.hasErrors()) _ = try result.validateRecordLayouts(module);
    return result;
}

/// Owns one shared IR; entries are indexed by Compilation.DeclId.
pub fn analyzeCompilation(comp: *Compilation) Error!Sema {
    if (comp.hasErrors()) return error.InvalidModule;
    const starts = try comp.allocator.alloc(usize, comp.modules.items.len);
    defer comp.allocator.free(starts);
    for (comp.modules.items, starts) |module, *start| start.* = module.diagnostics.items.len;
    errdefer for (comp.modules.items, starts) |module, start| module.diagnostics.shrinkRetainingCapacity(start);
    const graph_diagnostics_start = comp.diagnostics.items.len;
    errdefer comp.diagnostics.shrinkRetainingCapacity(graph_diagnostics_start);
    try comp.prepareSemantics();
    var result: Sema = .{ .allocator = comp.allocator, .ir = Ir.init(comp.allocator), .entries = &.{}, .compilation = comp };
    errdefer result.deinit();
    for (comp.modules.items) |module| try result.ir.source_lengths.append(comp.allocator, @intCast(module.source.ast.source.len));
    if (comp.hasErrors()) return result;
    for (comp.modules.items) |module| {
        var dependencies = try Dependencies.analyze(module);
        dependencies.deinit();
    }
    if (comp.hasErrors()) return result;
    for (comp.modules.items) |module| try result.analyzeModuleAnnotations(module);
    try result.finalizeRecordLayouts(comp.modules.items[0]);
    var exports: std.StringHashMapUnmanaged(ForeignSymbol) = .empty;
    defer exports.deinit(result.allocator);
    for (comp.modules.items) |module| try result.validateExports(module, &exports);
    if (comp.hasErrors()) return result;
    while (true) {
        try result.syncEntries(comp.modules.items[0]);
        var next: ?usize = null;
        for (result.entries, 0..) |entry, i| if (entry.status == .signature_only or entry.status == .requested) {
            next = i;
            break;
        };
        const index = next orelse break;
        const ref = comp.declarations.items[index];
        const module = comp.getModule(ref.source).?;
        if (result.entries[index].status == .requested and try result.requestType(module, ref.local) == null) {
            result.entries[index].status = .failed;
            continue;
        }
        const checked = try BodyChecker.checkDemand(module, &result, ref.local);
        try result.syncEntries(module);
        result.entries[index].status = if (checked) .body_checked else .failed;
    }
    if (!comp.hasErrors()) _ = try result.validateRecordLayouts(comp.modules.items[0]);
    return result;
}

pub fn getCompilationEntry(self: *const Sema, id: Compilation.DeclId) ?Entry {
    const index = @backingInt(id);
    return if (index < self.entries.len) self.entries[index] else null;
}

/// New concrete containers may be discovered while checking a requested body.
/// No caller may retain entry pointers across type resolution or body checking.
pub fn entryIndex(self: *const Sema, module: *const Module, id: Module.DeclId) usize {
    if (self.compilation) |comp| return @backingInt(comp.declaration_ids.get(.{ .source = module.source.id, .local = id }).?);
    return @backingInt(id);
}

pub fn syncEntries(self: *Sema, module: *Module) Ir.BuildError!void {
    const old_len = self.entries.len;
    const count = if (self.compilation) |comp| blk: {
        for (comp.modules.items) |owner| for (owner.declarations.items, 0..) |_, i| {
            _ = comp.internDeclaration(.{ .source = owner.source.id, .local = @fromBackingInt(@intCast(i)) }) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.TooManyDeclarations => return error.TooManyEntries,
            };
        };
        break :blk comp.declarations.items.len;
    } else module.declarations.items.len;
    if (old_len == count) return;
    self.entries = try self.allocator.realloc(self.entries, count);
    for (self.entries[old_len..]) |*entry| entry.* = .{ .status = .unrequested };
}

/// Resolves a referenced concrete method, but queues its body rather than
/// recursively checking it. Publishing the signature first permits recursion.
pub fn requestType(self: *Sema, module: *Module, id: Module.DeclId) Ir.BuildError!?Ir.TypeId {
    try self.syncEntries(module);
    const index = @backingInt(id);
    if (self.entryIndex(module, id) >= self.entries.len) return error.InvalidTypeId;
    const declaration = module.getDeclaration(id).?;
    if (declaration.is_test and !module.testEnabled()) {
        try module.diagnostics.append(module.allocator, .{ .tag = .test_not_callable, .span = declaration.name_span });
        return null;
    }
    const entry = self.entries[self.entryIndex(module, @fromBackingInt(@intCast(index)))];
    if (entry.status != .unrequested and entry.status != .requested) return entry.type_id;
    if (declaration.concrete_type == null or declaration.is_type_alias) return null;
    // Failed speculative requests must be retried: probes may discard their
    // diagnostics. The worklist performs a final, non-speculative request too.
    self.entries[self.entryIndex(module, @fromBackingInt(@intCast(index)))].status = .requested;
    const full = module.source.ast.decl(declaration.node).ast;
    const ty = (try TypeResolver.resolveInContainer(module, &self.ir, full.type_node, declaration.container, declaration.concrete_type)) orelse return null;
    try self.syncEntries(module);
    if (declaration.noreturn and self.ir.getType(self.ir.getType(ty).?.function.result).? != .void) {
        try module.diagnostics.append(module.allocator, .{ .tag = .noreturn_result, .span = declaration.name_span });
        return null;
    }
    self.entries[self.entryIndex(module, @fromBackingInt(@intCast(index)))] = .{ .type_id = ty, .status = .signature_only, .noreturn = declaration.noreturn };
    return ty;
}

pub fn deinit(self: *Sema) void {
    self.ir.deinit();
    self.allocator.free(self.entries);
    self.* = undefined;
}

/// Entries use the source module's declaration numbering, but borrow no AST data.
pub fn getEntry(self: *const Sema, id: Module.DeclId) ?Entry {
    const index = @backingInt(id);
    if (index >= self.entries.len) return null;
    return self.entries[index];
}

/// Checks/infers only syntax-known scalars, with the declared type (if any)
/// constraining every ascription. In particular, do not invoke
/// body checking: effects, value references, blocks and computation are not
/// compile-time evaluation, even beneath an explicit type ascription.
fn checkScalarGlobal(self: *Sema, module: *Module, index: usize, initializer: Ast.Node.Index, declared_annotation: ?Module.Span) Error!void {
    const ast = &module.source.ast;
    var node = initializer;
    var expected = self.entries[self.entryIndex(module, @fromBackingInt(@intCast(index)))].type_id;
    var annotation = declared_annotation;
    while (ast.fullCall(node)) |call| {
        if (ast.nodes.items(.tag)[call.ast.fn_expr] != .builtin_literal or
            !std.mem.eql(u8, ast.tokenSlice(ast.nodes.items(.main_token)[call.ast.fn_expr]), "@as")) return;
        if (call.ast.args.len != 2) {
            try module.diagnostics.append(module.allocator, .{ .tag = .argument_count_mismatch, .span = module.tokenSpan(ast.nodes.items(.main_token)[node]) });
            self.entries[self.entryIndex(module, @fromBackingInt(@intCast(index)))].status = .failed;
            return;
        }
        const ty = (try TypeResolver.resolve(module, &self.ir, call.ast.args[0])) orelse {
            self.entries[self.entryIndex(module, @fromBackingInt(@intCast(index)))].status = .failed;
            return;
        };
        if (expected != null and expected.? != ty) {
            try module.diagnostics.append(module.allocator, .{ .tag = .expression_type_mismatch, .span = module.tokenSpan(ast.firstToken(node)), .related = annotation });
            self.entries[self.entryIndex(module, @fromBackingInt(@intCast(index)))].status = .failed;
            return;
        }
        expected = ty;
        annotation = module.tokenSpan(ast.firstToken(call.ast.args[0]));
        node = call.ast.args[1];
    }
    switch (ast.nodes.items(.tag)[node]) {
        .field_access => {
            const ty = (try Literal.qualifiedEnum(module, &self.ir, node)) orelse return;
            if (expected == null) expected = ty;
        },
        .enum_literal => if (expected == null) return,
        .identifier => {
            const name = ast.tokenSlice(ast.firstToken(node));
            if (module.lookup(name) != null or (!std.mem.eql(u8, name, "true") and !std.mem.eql(u8, name, "false"))) return;
            if (expected == null) expected = try self.ir.internType(.boolean);
        },
        .string_literal => {
            if (expected == null) expected = try self.ir.internType(.string);
        },
        .char_literal => {
            if (expected == null) expected = try self.ir.internType(.rune);
        },
        .number_literal => if (expected == null) return,
        .negation => {
            if (expected == null or ast.nodes.items(.tag)[ast.nodes.items(.data)[node].rhs] != .number_literal) return;
        },
        else => return,
    }
    self.entries[self.entryIndex(module, @fromBackingInt(@intCast(index)))].type_id = expected;
    try self.checkLiteral(module, index, node, annotation orelse module.tokenSpan(ast.firstToken(node)));
}

fn checkLiteral(self: *Sema, module: *Module, index: usize, initializer: Ast.Node.Index, annotation: Module.Span) Error!void {
    switch (try Literal.check(module, &self.ir, initializer, self.entries[self.entryIndex(module, @fromBackingInt(@intCast(index)))].type_id.?, annotation)) {
        .not_literal => {
            var node = initializer;
            var buffer: [2]Ast.Node.Index = undefined;
            while (module.source.ast.fullBlock(&buffer, node)) |block| {
                if (module.source.ast.firstToken(node) != block.ast.lbrace or block.ast.statements.len != 1) break;
                node = block.ast.statements[0];
            }
            if (module.source.ast.nodes.items(.tag)[node] == .string_literal and Literal.hasInterpolation(module.source.ast.tokenSlice(module.source.ast.nodes.items(.main_token)[node]))) {
                try module.diagnostics.append(module.allocator, .{ .tag = .unsupported_global_interpolation, .span = module.tokenSpan(module.source.ast.nodes.items(.main_token)[node]) });
                self.entries[self.entryIndex(module, @fromBackingInt(@intCast(index)))].status = .failed;
            }
        },
        .failed => self.entries[self.entryIndex(module, @fromBackingInt(@intCast(index)))].status = .failed,
        .constant => |constant| {
            self.entries[self.entryIndex(module, @fromBackingInt(@intCast(index)))].constant = constant;
            self.entries[self.entryIndex(module, @fromBackingInt(@intCast(index)))].status = .constant_checked;
        },
    }
}

test "export ABI integer widths are a finite allowlist for parameters and results" {
    for ([_]std.builtin.Signedness{ .signed, .unsigned }) |signedness| {
        for (0..130) |bits| {
            const ty: Ir.Type = .{ .integer = .{ .signedness = signedness, .bits = @intCast(bits) } };
            const allowed = bits == 8 or bits == 16 or bits == 32 or bits == 64;
            try std.testing.expectEqual(allowed, foreignScalar(ty, false));
            try std.testing.expectEqual(allowed, foreignScalar(ty, true));
        }
    }
    try std.testing.expect(foreignScalar(.boolean, false));
    try std.testing.expect(foreignScalar(.boolean, true));
    try std.testing.expect(!foreignScalar(.void, false));
    try std.testing.expect(foreignScalar(.void, true));
    try std.testing.expect(!foreignScalar(.string, false));
    try std.testing.expect(!foreignScalar(.string, true));
    try std.testing.expect(!foreignScalar(.rune, false));
    try std.testing.expect(!foreignScalar(.rune, true));
}

test "rune annotations aliases and immutable globals preserve scalar constants" {
    const source = "let inferred = 'é'\nlet annotated: Rune = '😀'\nlet escaped = @as(rune, '\\xE9')\nlet nested = @as(Rune, @as(rune, '\\u{10FFFF}'))\nlet Rune = rune\nlet zero: rune = '\\0'";
    var module = try Module.init(std.testing.allocator, "rune-globals.nov", source);
    defer module.deinit();
    var sema = try analyzeAnnotations(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    const rune = try sema.ir.internType(.rune);
    for ([_]struct { name: []const u8, value: u32 }{
        .{ .name = "inferred", .value = 0xe9 },
        .{ .name = "annotated", .value = 0x1f600 },
        .{ .name = "escaped", .value = 0xe9 },
        .{ .name = "nested", .value = 0x10ffff },
        .{ .name = "zero", .value = 0 },
    }) |case| {
        const entry = sema.getEntry(module.lookup(case.name).?).?;
        try std.testing.expectEqual(Status.constant_checked, entry.status);
        try std.testing.expectEqual(rune, entry.type_id.?);
        try std.testing.expectEqual(case.value, sema.ir.getConstant(entry.constant.?).?.value.rune);
    }
    try sema.ir.verify();
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
}

test "rune globals reject conversions malformed scalars and foreign ABI use" {
    for ([_]struct { source: []const u8, tag: Module.Diagnostic.Tag }{
        .{ .source = "let value: rune = 97", .tag = .literal_type_mismatch },
        .{ .source = "let value = @as(rune, 97)", .tag = .literal_type_mismatch },
        .{ .source = "let value: rune = \"a\"", .tag = .literal_type_mismatch },
        .{ .source = "let value: u32 = 'a'", .tag = .literal_type_mismatch },
        .{ .source = "let value = 'ab'", .tag = .invalid_literal },
        .{ .source = "let value = '\\u{DFFF}'", .tag = .invalid_literal },
        .{ .source = "@[extern]\nlet external: (rune) -> rune", .tag = .invalid_extern_signature },
        .{ .source = "@[export]\nlet exported: (x: rune) -> rune = x", .tag = .invalid_export_signature },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "rune-global-error.nov", case.source);
        defer module.deinit();
        var sema = try analyzeAnnotations(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{case.source});
    }
}

test "export ABI validates resolved aliases native integers and unused signatures" {
    for ([_][]const u8{ "bool", "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "int", "uint" }) |ty| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let Alias = {s}\n@[export]\nlet identity: (x: Alias) -> Alias = x", .{ty});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "abi.nov", source);
        defer module.deinit();
        var sema = try analyzeAnnotations(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
    }
    for ([_][]const u8{ "i128", "u128", "string", "rune", "[]i32", "() -> i32", "R", "E" }) |ty| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let R = struct {{ x: i32, }}\nlet E = enum {{ a, }}\n@[export]\nlet unused: (x: {s}) -> {s} = x", .{ ty, ty });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "abi.nov", source);
        defer module.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        var sema = try analyzeAnnotations(&module);
        defer sema.deinit();
        try std.testing.expect(module.hasErrors());
        try std.testing.expectEqual(Module.Diagnostic.Tag.invalid_export_signature, module.diagnostics.items[0].tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
    for ([_][]const u8{
        "@[export]\nlet bad: (x: void) = {}",
        "@[export]\nlet bad: () -> string = \"text\"",
        "@[export]\nlet bad: () -> i128 = 0",
        "@[export]\nlet bad: () -> () = {}",
    }) |bad| {
        var rejected = try Module.init(std.testing.allocator, "result-abi.nov", bad);
        defer rejected.deinit();
        var checked = try analyzeAnnotations(&rejected);
        defer checked.deinit();
        try std.testing.expect(rejected.hasErrors());
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{bad});
    }
    const source = "@[export]\nlet unit: () = {}\n@[export]\n@[noreturn]\nlet stop: () = loop {}";
    var module = try Module.init(std.testing.allocator, "abi.nov", source);
    defer module.deinit();
    var sema = try analyzeFunctions(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
}

fn testExportSpecializationOwnership(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "export-specializations.nov",
        \\@[export("owned_symbol")]
        \\let exported: () = {}
        \\let G: (T: type) -> type = struct {
        \\    value: T,
        \\    let method: () = {}
        \\}
        \\let Concrete = G(i32)
    );
    defer module.deinit();
    const original_count = module.declarations.items.len;
    for (0..2) |_| {
        var sema = analyzeAnnotations(&module) catch |err| {
            try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
            return err;
        };
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(module.declarations.items.len > original_count);
        for (module.declarations.items[original_count..]) |declaration| {
            try std.testing.expect(declaration.concrete_type != null);
            try std.testing.expect(declaration.export_name == null);
        }
        try std.testing.expectEqualStrings("owned_symbol", module.getDeclaration(module.lookup("exported").?).?.export_name.?);
    }
    module.clearSpecializations();
    try std.testing.expectEqual(original_count, module.declarations.items.len);
    try std.testing.expectEqualStrings("owned_symbol", module.getDeclaration(module.lookup("exported").?).?.export_name.?);
}

test "export ownership survives repeated generic specialization clearing and OOM" {
    try testExportSpecializationOwnership(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testExportSpecializationOwnership, .{});
}

fn testExportDuplicates(allocator: std.mem.Allocator) !void {
    var comp = Compilation.init(allocator);
    defer comp.deinit();
    const root = try comp.addSource("/app/root.nov", "let imported = @import(\"lib.nov\")\n@[export(\"shared_symbol\")]\nlet first: () = {}");
    const imported = try comp.addSource("/app/lib.nov", "@[export(\"shared_symbol\")]\nlet second: () = {}");
    const module = comp.getModule(root).?;
    const local = module.lookup("imported").?;
    try comp.imports.append(allocator, .{ .declaration = comp.declaration_ids.get(.{ .source = root, .local = local }).?, .target = imported, .named = false, .span = module.getDeclaration(local).?.name_span });
    var sema = analyzeCompilation(&comp) catch |err| {
        for (comp.modules.items) |m| try std.testing.expectEqual(@as(usize, 0), m.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    const diagnostic = comp.getModule(imported).?.diagnostics.items[0];
    try std.testing.expectEqual(Module.Diagnostic.Tag.duplicate_export_name, diagnostic.tag);
    try std.testing.expectEqual(imported, diagnostic.span.source);
    try std.testing.expectEqual(root, diagnostic.related.?.source);
    var rendered = std.Io.Writer.Allocating.init(allocator);
    defer rendered.deinit();
    Diagnostics.renderCompilation(&comp, &rendered.writer) catch return error.OutOfMemory;
    try std.testing.expect(std.mem.indexOf(u8, rendered.written(), "/app/lib.nov:2:") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered.written(), "/app/root.nov:3:") != null);
}

test "export duplicates across imported sources render qualified related spans and roll back OOM" {
    try testExportDuplicates(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testExportDuplicates, .{});
    const source = "@[export(\"shared_symbol\")]\nlet first: () = {}\n@[export(\"shared_symbol\")]\nlet second: () = {}";
    var module = try Module.init(std.testing.allocator, "duplicate.nov", source);
    defer module.deinit();
    var sema = try analyzeAnnotations(&module);
    defer sema.deinit();
    try std.testing.expectEqual(Module.Diagnostic.Tag.duplicate_export_name, module.diagnostics.items[0].tag);
    try std.testing.expect(module.diagnostics.items[0].related != null);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
}

test "annotated scalar ascriptions match inferred pairs and preserve plain literal groups" {
    const source =
        \\let inferred_bool = @as(Flag, @as(bool, true))
        \\let annotated_bool: Flag = @as(bool, @as(Flag, true))
        \\let inferred_int = @as(Count, @as(int, -42))
        \\let annotated_int: int = @as(Count, @as(int, -42))
        \\let inferred_string = @as(Text, @as(string, "hé\0"))
        \\let annotated_string: Text = @as(string, @as(Text, "hé\0"))
        \\let grouped_bool: bool = {{true}}
        \\let grouped_int: int = -{{42}}
        \\let grouped_string: string = {{"hé\0"}}
        \\let Flag = bool
        \\let Count = int
        \\let Text = string
    ;
    var module = try Module.init(std.testing.allocator, "annotated-as.nov", source);
    var sema = analyzeAnnotations(&module) catch |err| {
        module.deinit();
        return err;
    };
    const failed = module.hasErrors();
    module.deinit();
    defer sema.deinit();
    try std.testing.expect(!failed);
    for (sema.entries[0..9]) |entry| try std.testing.expectEqual(Status.constant_checked, entry.status);
    for (0..3) |i| {
        const inferred = sema.entries[2 * i];
        const annotated = sema.entries[2 * i + 1];
        try std.testing.expectEqual(inferred.type_id, annotated.type_id);
        try std.testing.expectEqual(inferred.constant, annotated.constant);
        try std.testing.expectEqual(inferred.constant, sema.entries[6 + i].constant);
    }
    try std.testing.expectEqualStrings("hé\x00", sema.ir.getConstant(sema.entries[5].constant.?).?.value.string);
    try sema.ir.verify();
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
}

test "annotated scalar ascriptions diagnose declared and nested mismatches bounds and interpolation" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let n: uint = @as(int, 1)", .tag = .expression_type_mismatch },
        .{ .source = "let n: uint = @as(int, @as(uint, 1))", .tag = .expression_type_mismatch },
        .{ .source = "let n: uint = @as(uint, @as(int, 1))", .tag = .expression_type_mismatch },
        .{ .source = "let s: string = @as(bool, true)", .tag = .expression_type_mismatch },
        .{ .source = "let n: i8 = @as(i8, 128)", .tag = .literal_out_of_range },
        .{ .source = "let n: u8 = @as(u8, @as(u8, -1))", .tag = .literal_out_of_range },
        .{ .source = "let n: int = @as(int, false)", .tag = .literal_type_mismatch },
        .{ .source = "let s: string = @as(string, @as(string, \"{true}\"))", .tag = .unsupported_global_interpolation },
        .{ .source = "let s: string = {{\"{true}\"}}", .tag = .unsupported_global_interpolation },
        .{ .source = "let n: int = @as(Missing, 1)", .tag = .unknown_type },
        .{ .source = "let n: int = @as(int)", .tag = .argument_count_mismatch },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "annotated-as-error.nov", case.source);
        defer module.deinit();
        var sema = try analyzeAnnotations(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try std.testing.expectEqual(Status.failed, sema.entries[0].status);
        try std.testing.expect(sema.entries[0].constant == null);
        try sema.ir.verify();
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{case.source});
    }
}

test "annotated ascriptions keep general global initialization deferred" {
    for ([_][]const u8{
        "let n: int = @as(int, 1 + 2)",
        "let n: int = @as(int, {1})",
        "let n: int = @as(int, missing())",
        "let n: int = @as(int, {@println(true)\n1})",
        "let n: int = @as(int, other)\nlet other: int = 1",
        "let a: []int = @as([]int, [1])",
        "let u: void = @as(void, {})",
        "let f: Fn = @as(Fn, other)\nlet Fn = () -> int\nlet other: () -> int = 1",
        "let mut n: int = @as(int, 1)",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "annotated-as-deferred.nov", source);
        defer module.deinit();
        var sema = try analyzeAnnotations(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expectEqual(Status.deferred, sema.entries[0].status);
        try std.testing.expect(sema.entries[0].constant == null);
        try sema.ir.verify();
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}

test "inferred globals own canonical scalar constants and keep aliases distinct" {
    var module = try Module.init(std.testing.allocator, "globals.nov", "let yes = true\nlet no = @as(bool, @as(bool, false))\nlet text = \"hé\\0\\{ok\\}\"\n" ++
        "let wrapped = @as(string, \"owned\")\nlet number = @as(Count, @as(int, -42))\nlet Count = int\nlet pending = 42\nlet chain = yes\nlet block = {true}\nlet mut mutable = true");
    var sema = analyzeAnnotations(&module) catch |err| {
        module.deinit();
        return err;
    };
    const errors = module.hasErrors();
    module.deinit();
    defer sema.deinit();
    try std.testing.expect(!errors);
    for (sema.entries[0..5]) |entry| {
        try std.testing.expectEqual(Status.constant_checked, entry.status);
        try std.testing.expectEqual(entry.type_id.?, sema.ir.getConstant(entry.constant.?).?.ty);
    }
    try std.testing.expectEqualStrings("hé\x00{ok}", sema.ir.getConstant(sema.entries[2].constant.?).?.value.string);
    try std.testing.expectEqualStrings("owned", sema.ir.getConstant(sema.entries[3].constant.?).?.value.string);
    try std.testing.expectEqual(@as(i128, -42), sema.ir.getConstant(sema.entries[4].constant.?).?.value.signed_integer);
    try std.testing.expectEqual(sema.entries[4].type_id, sema.entries[5].alias_type);
    try std.testing.expectEqual(Status.type_alias, sema.entries[5].status);
    for (sema.entries[6..]) |entry| {
        try std.testing.expectEqual(Status.deferred, entry.status);
        try std.testing.expect(entry.type_id == null and entry.constant == null);
    }
    try sema.ir.verify();
}

test "inferred global ascriptions diagnose exact types bounds arity and interpolation" {
    const Case = struct { expression: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .expression = "@as(i8, 128)", .tag = .literal_out_of_range },
        .{ .expression = "@as(u8, -1)", .tag = .literal_out_of_range },
        .{ .expression = "@as(int, @as(uint, 1))", .tag = .expression_type_mismatch },
        .{ .expression = "@as(bool, @as(string, \"x\"))", .tag = .expression_type_mismatch },
        .{ .expression = "@as(int, true)", .tag = .literal_type_mismatch },
        .{ .expression = "@as(bool, 1)", .tag = .literal_type_mismatch },
        .{ .expression = "@as(string, false)", .tag = .literal_type_mismatch },
        .{ .expression = "@as(Unknown, 1)", .tag = .unknown_type },
        .{ .expression = "@as(true, 1)", .tag = .not_a_type },
        .{ .expression = "@as()", .tag = .argument_count_mismatch },
        .{ .expression = "@as(int)", .tag = .argument_count_mismatch },
        .{ .expression = "@as(int, 1, 2)", .tag = .argument_count_mismatch },
        .{ .expression = "\"{true}\"", .tag = .unsupported_global_interpolation },
        .{ .expression = "@as(string, \"{true}\")", .tag = .unsupported_global_interpolation },
        .{ .expression = "\"\\xff\"", .tag = .invalid_literal },
    }) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let value = {s}", .{case.expression});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "invalid-global.nov", source);
        defer module.deinit();
        var sema = try analyzeAnnotations(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try std.testing.expectEqual(Status.failed, sema.entries[0].status);
        try std.testing.expect(sema.entries[0].constant == null);
        try sema.ir.verify();
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}

test "inferred global constants clean up on every allocation failure" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{"let yes = true\nlet text = @as(string, \"hé\\0\")\nlet n = @as(Count, @as(int, -42))\nlet Count = int"});
}

test "native aliases check literal boundaries without defaulting untyped globals" {
    const signed_min: i128 = std.math.minInt(isize);
    const signed_max: i128 = std.math.maxInt(isize);
    const unsigned_max: u128 = std.math.maxInt(usize);
    const Case = struct { name: []const u8, value: i128, valid: bool };
    for ([_]Case{
        .{ .name = "int", .value = signed_min, .valid = true },
        .{ .name = "int", .value = signed_max, .valid = true },
        .{ .name = "int", .value = signed_min - 1, .valid = false },
        .{ .name = "int", .value = signed_max + 1, .valid = false },
        .{ .name = "uint", .value = @intCast(unsigned_max), .valid = true },
        .{ .name = "uint", .value = @intCast(unsigned_max + 1), .valid = false },
        .{ .name = "uint", .value = -1, .valid = false },
        .{ .name = "uint", .value = 0, .valid = true },
    }) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let value: {s} = {d}\nlet pending = 42", .{ case.name, case.value });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "native.nov", source);
        defer module.deinit();
        var sema = try analyzeAnnotations(&module);
        defer sema.deinit();
        try std.testing.expectEqual(if (case.valid) Status.constant_checked else Status.failed, sema.entries[0].status);
        try std.testing.expectEqual(Status.deferred, sema.entries[1].status);
        try std.testing.expect(sema.entries[1].type_id == null);
        if (case.valid) {
            const constant = sema.ir.getConstant(sema.entries[0].constant.?).?;
            try std.testing.expectEqual(Ir.native_pointer_bits, sema.ir.getType(constant.ty).?.integer.bits);
            if (case.name[0] == 'i') {
                try std.testing.expectEqual(case.value, constant.value.signed_integer);
            } else {
                try std.testing.expectEqual(@as(u128, @intCast(case.value)), constant.value.unsigned_integer);
            }
            try std.testing.expect(!module.hasErrors());
        } else {
            try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
            try std.testing.expectEqual(Module.Diagnostic.Tag.literal_out_of_range, module.diagnostics.items[0].tag);
            try std.testing.expectEqualStrings(case.name, module.sourceSlice(module.diagnostics.items[0].related.?).?);
        }
        try sema.ir.verify();
    }
}

test "explicit annotations check literals and preserve integer boundaries" {
    var module = try Module.init(std.testing.allocator, "literals.nov", "let small: i8 = -128\nlet byte: u8 = 0xff\nlet binary: i32 = 0b10\nlet octal: i32 = 0o10\nlet separated: i32 = 1_000\n" ++
        "let minimum: i128 = -170141183460469231731687303715884105728\nlet maximum: u128 = 340282366920938463463374607431768211455\n" ++
        "let yes: bool = {true}\nlet no: bool = false\nlet grouped: i32 = {{42}}");
    defer module.deinit();
    var sema = try analyzeAnnotations(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    for (sema.entries) |entry| try std.testing.expectEqual(Status.constant_checked, entry.status);
    const small = sema.ir.getConstant(sema.getEntry(module.lookup("small").?).?.constant.?).?;
    try std.testing.expectEqual(@as(i128, -128), small.value.signed_integer);
    try std.testing.expectEqual(@as(u128, 255), sema.ir.getConstant(sema.getEntry(module.lookup("byte").?).?.constant.?).?.value.unsigned_integer);
    try std.testing.expectEqual(@as(i128, 1000), sema.ir.getConstant(sema.getEntry(module.lookup("separated").?).?.constant.?).?.value.signed_integer);
    try std.testing.expectEqual(std.math.minInt(i128), sema.ir.getConstant(sema.getEntry(module.lookup("minimum").?).?.constant.?).?.value.signed_integer);
    try std.testing.expectEqual(std.math.maxInt(u128), sema.ir.getConstant(sema.getEntry(module.lookup("maximum").?).?.constant.?).?.value.unsigned_integer);
    try std.testing.expect(sema.ir.getConstant(sema.getEntry(module.lookup("yes").?).?.constant.?).?.value.boolean);
    try std.testing.expect(sema.getEntry(@fromBackingInt(@intCast(999))) == null);
    try sema.ir.verify();
}

test "range and type errors have literal and annotation locations" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag, literal: []const u8 };
    const cases = [_]Case{
        .{ .source = "let x: i8 = 128", .tag = .literal_out_of_range, .literal = "128" },
        .{ .source = "let x: i8 = -129", .tag = .literal_out_of_range, .literal = "-129" },
        .{ .source = "let x: u8 = 256", .tag = .literal_out_of_range, .literal = "256" },
        .{ .source = "let x: u8 = -1", .tag = .literal_out_of_range, .literal = "-1" },
        .{ .source = "let x: u128 = 0x100000000000000000000000000000000", .tag = .literal_out_of_range, .literal = "0x100000000000000000000000000000000" },
        .{ .source = "let x: i128 = -0x80000000000000000000000000000001", .tag = .literal_out_of_range, .literal = "-0x80000000000000000000000000000001" },
        .{ .source = "let x: i128 = 170141183460469231731687303715884105728", .tag = .literal_out_of_range, .literal = "170141183460469231731687303715884105728" },
        .{ .source = "let x: i128 = -170141183460469231731687303715884105729", .tag = .literal_out_of_range, .literal = "-170141183460469231731687303715884105729" },
        .{ .source = "let x: u128 = 340282366920938463463374607431768211456", .tag = .literal_out_of_range, .literal = "340282366920938463463374607431768211456" },
        .{ .source = "let x: bool = 340282366920938463463374607431768211456", .tag = .literal_type_mismatch, .literal = "340282366920938463463374607431768211456" },
        .{ .source = "let x: void = 340282366920938463463374607431768211456", .tag = .literal_type_mismatch, .literal = "340282366920938463463374607431768211456" },
        .{ .source = "let x: bool = 1", .tag = .literal_type_mismatch, .literal = "1" },
        .{ .source = "let x: i32 = true", .tag = .literal_type_mismatch, .literal = "true" },
        .{ .source = "let x: void = 1", .tag = .literal_type_mismatch, .literal = "1" },
    };
    for (cases) |case| {
        var module = try Module.init(std.testing.allocator, "invalid.nov", case.source);
        defer module.deinit();
        var sema = try analyzeAnnotations(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        const diagnostic = module.diagnostics.items[0];
        try std.testing.expectEqual(case.tag, diagnostic.tag);
        try std.testing.expectEqualStrings(case.literal, module.sourceSlice(diagnostic.span).?);
        try std.testing.expect(diagnostic.related != null);
        try std.testing.expectEqual(Status.failed, sema.entries[0].status);
        try std.testing.expect(sema.entries[0].constant == null);
        try sema.ir.verify();
    }
}

test "large integer spellings canonicalize across bases and separators" {
    var source = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer source.deinit();
    const maximum = std.math.maxInt(u128);
    try source.writer.print("let decimal: u128 = {d}\nlet hex: u128 = 0x{x}\nlet binary: u128 = 0b{b}\nlet octal: u128 = 0o{o}\n", .{ maximum, maximum, maximum, maximum });
    try source.writer.writeAll("let separated: u128 = 0xFFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF\nlet minimum: i128 = -0x8000_0000_0000_0000_0000_0000_0000_0000");
    var module = try Module.init(std.testing.allocator, "bases.nov", source.written());
    defer module.deinit();
    var sema = try analyzeAnnotations(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    for (sema.entries[0..5]) |entry| {
        try std.testing.expectEqual(Status.constant_checked, entry.status);
        try std.testing.expectEqual(sema.entries[0].constant, entry.constant);
    }
    try std.testing.expectEqual(std.math.minInt(i128), sema.ir.getConstant(sema.entries[5].constant.?).?.value.signed_integer);
    try sema.ir.verify();
}

test "signatures are resolved but bodies and unannotated expressions stay deferred" {
    var module = try Module.init(std.testing.allocator, "deferred.nov", "let add: (a: i32, b: i32) -> i32 = missing(a, b)\nlet main: () = {}\nlet pending = 123\nlet expression: i32 = 1 + 2\nlet storage: i32");
    defer module.deinit();
    var sema = try analyzeAnnotations(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    const add = sema.getEntry(module.lookup("add").?).?;
    try std.testing.expectEqual(Status.signature_only, add.status);
    const signature = sema.ir.getType(add.type_id.?).?.function;
    try std.testing.expectEqual(@as(usize, 2), signature.params.len);
    try std.testing.expectEqual(signature.result, signature.params[0]);
    try std.testing.expectEqual(Status.signature_only, sema.getEntry(module.lookup("main").?).?.status);
    const pending = sema.getEntry(module.lookup("pending").?).?;
    try std.testing.expectEqual(Status.deferred, pending.status);
    try std.testing.expect(pending.type_id == null);
    try std.testing.expectEqual(Status.deferred, sema.getEntry(module.lookup("expression").?).?.status);
    try std.testing.expectEqual(Status.deferred, sema.getEntry(module.lookup("storage").?).?.status);
}

test "record 128 bit literal defaults preserve exact limits aliases and source free ownership" {
    const source = "let R=struct{lo:i128=-170141183460469231731687303715884105728,hi:Signed=170141183460469231731687303715884105727,top:Unsigned=340282366920938463463374607431768211455,}\nlet Signed=i128\nlet Unsigned=u128";
    var sema = blk: {
        var module = try Module.init(std.testing.allocator, "wide-defaults.nov", source);
        defer module.deinit();
        var result = try analyzeAnnotations(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        break :blk result;
    };
    defer sema.deinit();
    const fields = sema.ir.getType(sema.entries[0].alias_type.?).?.record.fields;
    try std.testing.expectEqual(@as(usize, 3), fields.len);
    try std.testing.expectEqual(std.math.minInt(i128), sema.ir.getConstant(fields[0].default.?).?.value.signed_integer);
    try std.testing.expectEqual(std.math.maxInt(i128), sema.ir.getConstant(fields[1].default.?).?.value.signed_integer);
    try std.testing.expectEqual(std.math.maxInt(u128), sema.ir.getConstant(fields[2].default.?).?.value.unsigned_integer);
    try sema.ir.verify();
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
}

test "record 128 bit defaults retain literal bounds and source diagnostics without widening arrays" {
    for ([_]struct { ty: []const u8, literal: []const u8 }{
        .{ .ty = "i128", .literal = "170141183460469231731687303715884105728" },
        .{ .ty = "i128", .literal = "-170141183460469231731687303715884105729" },
        .{ .ty = "u128", .literal = "340282366920938463463374607431768211456" },
        .{ .ty = "u128", .literal = "-1" },
    }) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let Wide={s}\nlet R=struct{{x:Wide={s},}}", .{ case.ty, case.literal });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "wide-bounds.nov", source);
        defer module.deinit();
        var sema = try analyzeAnnotations(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        const diagnostic = module.diagnostics.items[0];
        try std.testing.expectEqual(Module.Diagnostic.Tag.literal_out_of_range, diagnostic.tag);
        try std.testing.expectEqualStrings(case.literal, module.sourceSlice(diagnostic.span).?);
        try std.testing.expectEqualStrings("x", module.sourceSlice(diagnostic.related.?).?);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
    for ([_][]const u8{ "let f:(a:[]i128)={}", "let R=struct{x:[]u128,}" }) |source| {
        var module = try Module.init(std.testing.allocator, "wide-array.nov", source);
        defer module.deinit();
        var sema = try analyzeAnnotations(&module);
        defer sema.deinit();
        try std.testing.expect(module.hasErrors());
        try std.testing.expectEqual(Module.Diagnostic.Tag.unsupported_type, module.diagnostics.items[0].tag);
    }
}

test "invalid annotations and unsupported literals never become checked constants" {
    for ([_][]const u8{
        "let x: Unknown = 1",
        "let x: float = 1",
        "let f: (x: i32, x: i32) -> i32 = 1",
        "let x: i32 = 1e",
        "let x: i32 = 1.0",
        "let x: i32 = 'a'",
        "let x: bool = \"true\"",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "unsupported.nov", source);
        defer module.deinit();
        try std.testing.expect(!module.hasErrors());
        var sema = try analyzeAnnotations(&module);
        defer sema.deinit();
        try std.testing.expect(module.hasErrors());
        try std.testing.expectEqual(Status.failed, sema.entries[0].status);
        try std.testing.expect(sema.entries[0].constant == null);
        try sema.ir.verify();
    }
}

test "empty labeled and multistatement blocks stay deferred" {
    var module = try Module.init(std.testing.allocator, "blocks.nov", "let empty: void = {}\nlet labeled: i32 = label: {42}\nlet many: i32 = {1\n2}");
    defer module.deinit();
    var sema = try analyzeAnnotations(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    for (sema.entries) |entry| {
        try std.testing.expectEqual(Status.deferred, entry.status);
        try std.testing.expect(entry.constant == null);
    }
}

test "typed results retain no borrowed module storage" {
    var module = try Module.init(std.testing.allocator, "owned.nov", "let x: i32 = 42\nlet f: (i32) -> bool = unknown");
    var sema = analyzeAnnotations(&module) catch |err| {
        module.deinit();
        return err;
    };
    module.deinit();
    defer sema.deinit();
    try std.testing.expectEqual(@as(i128, 42), sema.ir.getConstant(sema.entries[0].constant.?).?.value.signed_integer);
    try std.testing.expectEqual(@as(usize, 1), sema.ir.getType(sema.entries[1].type_id.?).?.function.params.len);
    try sema.ir.verify();
}

test "annotation analysis refuses modules with syntax or collection errors" {
    for ([_][]const u8{ "let broken =", "let a = 1\nlet a = 2" }) |source| {
        var module = try Module.init(std.testing.allocator, "invalid.nov", source);
        defer module.deinit();
        const count = module.diagnostics.items.len;
        try std.testing.expectError(error.InvalidModule, analyzeAnnotations(&module));
        try std.testing.expectEqual(count, module.diagnostics.items.len);
    }
}

test "literal diagnostics render the actual value and annotation" {
    var module = try Module.init(std.testing.allocator, "range.nov", "let value: i8 = -129");
    defer module.deinit();
    var sema = try analyzeAnnotations(&module);
    defer sema.deinit();
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try Diagnostics.renderModule(&module, &output.writer);
    try std.testing.expectEqualStrings(
        "range.nov:1:17: error: integer literal does not fit the annotated type\nlet value: i8 = -129\n                ^~~~\n" ++
            "range.nov:1:12: note: type annotation here\nlet value: i8 = -129\n           ^~\n",
        output.written(),
    );
}

fn testAllocationFailures(allocator: std.mem.Allocator, source: []const u8) !void {
    var module = try Module.init(allocator, "oom.nov", source);
    defer module.deinit();
    var sema = analyzeAnnotations(&module) catch |err| {
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    try sema.ir.verify();
}

test "annotation analysis owns its storage and rolls back diagnostics on allocation failure" {
    for ([_][]const u8{
        "",
        "let x: i32 = 42\nlet y: bool = false",
        "let f: (a: i32, callback: () -> bool) -> i32 = a",
        "let x: i8 = 128\nlet bad: Unknown = 1",
        "let f: (x: void, x: i32) -> bool = true",
    }) |source| {
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}

test "extern ABI accepts scalar aliases and leaves bodyless signatures external" {
    for ([_][]const u8{ "bool", "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "int", "uint" }) |ty| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let Alias = {s}\n@[extern]\nlet foreign: (x: Alias) -> Alias\n@[noreturn]\n@[extern]\nlet stop: ()", .{ty});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "extern-abi.nov", source);
        defer module.deinit();
        var sema = try analyzeFunctions(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expectEqual(Status.external, sema.getEntry(module.lookup("foreign").?).?.status);
        try std.testing.expectEqual(Status.external, sema.getEntry(module.lookup("stop").?).?.status);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
    for ([_][]const u8{ "i128", "u128", "string", "rune", "[]i32", "() -> i32", "R", "E" }) |ty| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let R = struct {{ x: i32, }}\nlet E = enum {{ a, }}\n@[extern]\nlet foreign: (x: {s}) -> {s}", .{ ty, ty });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "extern-abi.nov", source);
        defer module.deinit();
        var sema = try analyzeAnnotations(&module);
        defer sema.deinit();
        try std.testing.expectEqual(Module.Diagnostic.Tag.invalid_extern_signature, module.diagnostics.items[0].tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}

fn testForeignSymbols(allocator: std.mem.Allocator, first: []const u8, second: []const u8, expected: ?Module.Diagnostic.Tag, separate: bool) !void {
    var comp = Compilation.init(allocator);
    defer comp.deinit();
    const source = try std.fmt.allocPrint(allocator, "{s}\n{s}", .{ first, if (separate) "" else second });
    defer allocator.free(source);
    const root = try comp.addSource("/app/first.nov", source);
    const other = if (separate) try comp.addSource("/app/second.nov", second) else root;
    var sema = analyzeCompilation(&comp) catch |err| {
        for (comp.modules.items) |module| try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    if (expected) |tag| {
        const module = comp.getModule(other).?;
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        const diagnostic = module.diagnostics.items[0];
        try std.testing.expectEqual(tag, diagnostic.tag);
        try std.testing.expectEqual(root, diagnostic.related.?.source);
        try std.testing.expectEqual(other, diagnostic.span.source);
        var rendered = std.Io.Writer.Allocating.init(allocator);
        defer rendered.deinit();
        Diagnostics.renderCompilation(&comp, &rendered.writer) catch return error.OutOfMemory;
        try std.testing.expect(std.mem.indexOf(u8, rendered.written(), "/app/first.nov:") != null);
        if (separate) try std.testing.expect(std.mem.indexOf(u8, rendered.written(), "/app/second.nov:") != null);
    } else try std.testing.expect(!comp.hasErrors());
}

fn testDisabledRequest(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "disabled-request.nov", "@[test]\nlet sample: () = {}");
    defer module.deinit();
    var sema = try analyzeAnnotations(&module);
    defer sema.deinit();
    const id = module.lookup("sample").?;
    try std.testing.expectEqual(Status.ignored, sema.getEntry(id).?.status);
    try std.testing.expectEqual(@as(?Ir.TypeId, null), try sema.requestType(&module, id));
    try std.testing.expectEqual(Module.Diagnostic.Tag.test_not_callable, module.diagnostics.items[0].tag);
    try std.testing.expectEqual(Status.ignored, sema.getEntry(id).?.status);
}

test "disabled test type requests fail without activating entries under OOM" {
    try testDisabledRequest(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testDisabledRequest, .{});
}

test "enabled test signature remains available for its own body check" {
    var module = try Module.init(std.testing.allocator, "enabled-request.nov", "@[test]\nlet sample: () = {}");
    defer module.deinit();
    module.tests_enabled = true;
    var sema = try analyzeAnnotations(&module);
    defer sema.deinit();
    const id = module.lookup("sample").?;
    try std.testing.expectEqual(Status.signature_only, sema.getEntry(id).?.status);
    try std.testing.expect(try sema.requestType(&module, id) != null);
    try std.testing.expect(!module.hasErrors());
}

test "extern shared symbols compare canonical signatures and noreturn across both sources under OOM" {
    const external = "@[extern(\"shared_symbol\")]\nlet first: (x: i32) -> i32";
    const exported = "@[export(\"shared_symbol\")]\nlet first: (x: i32) -> i32 = x";
    for ([_]bool{ false, true }) |separate| {
        for ([_]struct { first: []const u8, second: []const u8, tag: ?Module.Diagnostic.Tag = null }{
            .{ .first = external, .second = "let Alias = i32\n@[extern(\"shared_symbol\")]\nlet second: (x: Alias) -> Alias" },
            .{ .first = exported, .second = "@[extern(\"shared_symbol\")]\nlet second: (x: i32) -> i32" },
            .{ .first = external, .second = "@[export(\"shared_symbol\")]\nlet second: (x: i32) -> i32 = x" },
            .{ .first = external, .second = "@[extern(\"shared_symbol\")]\nlet second: (x: u32) -> i32", .tag = .conflicting_foreign_signature },
            .{ .first = external, .second = "@[extern(\"shared_symbol\")]\nlet second: (x: i32) -> u32", .tag = .conflicting_foreign_signature },
            .{ .first = exported, .second = "@[extern(\"shared_symbol\")]\nlet second: ()", .tag = .conflicting_foreign_signature },
            .{ .first = "@[noreturn]\n@[extern(\"shared_symbol\")]\nlet first: ()", .second = "@[extern(\"shared_symbol\")]\nlet second: ()", .tag = .conflicting_foreign_signature },
            .{ .first = exported, .second = "@[export(\"shared_symbol\")]\nlet second: (x: i32) -> i32 = x", .tag = .duplicate_export_name },
        }) |case| {
            try testForeignSymbols(std.testing.allocator, case.first, case.second, case.tag, separate);
            try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testForeignSymbols, .{ case.first, case.second, case.tag, separate });
        }
    }
}
