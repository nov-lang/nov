const std = @import("std");
const TestAllocations = @import("TestAllocations.zig");
const Ast = @import("Ast.zig");
const Source = @import("Source.zig");
const primitives = @import("primitives.zig");
const Compilation = @import("Compilation.zig");
const Literal = @import("Literal.zig");
const string_literal = @import("string_literal.zig");
const ExportNames = @import("ExportNames.zig");
const Ir = @import("Ir.zig");
const GenericFunctions = @import("GenericFunctions.zig");
const Module = @This();

allocator: std.mem.Allocator,
compilation: ?*Compilation = null,
tests_enabled: bool = false,
source: Source,
declarations: std.ArrayList(Declaration) = .empty,
diagnostics: std.ArrayList(Diagnostic) = .empty,
names: std.StringHashMapUnmanaged(DeclId) = .empty,

pub const DeclId = enum(u32) { _ };

/// Half-open, source-qualified byte range, not a clipped highlight.
pub const Span = Source.Span;

pub const TypeBinding = struct { name: []const u8, ty: Ir.TypeId };

pub fn hasTypeVariables(ast: *const Ast, node: Ast.Node.Index) bool {
    if (node == 0) return false;
    for (ast.tokens.items(.tag)[ast.firstToken(node) .. ast.lastToken(node) + 1]) |tag| {
        if (tag == .type_variable) return true;
    }
    return false;
}

pub const Declaration = struct {
    name: []const u8,
    name_span: Span,
    node: Ast.Node.Index,
    is_type_alias: bool = false,
    is_namespace: bool = false,
    is_record: bool = false,
    is_generic: bool = false,
    is_generic_function: bool = false,
    function_template: ?DeclId = null,
    /// Owned slice on specializations only; names borrow the defining signature.
    function_bindings: []const TypeBinding = &.{},
    is_test: bool = false,
    container: ?DeclId = null,
    public: bool = false,
    noreturn: bool = false,
    stringify: bool = false,
    /// Owned by this module; absent on generic templates and specializations.
    export_name: ?[]const u8 = null,
    /// Owned by this module; only validated top-level bodyless prototypes.
    extern_name: ?[]const u8 = null,
    /// Analysis-local canonical container TypeId for a specialized method.
    concrete_type: ?u32 = null,
    specialization_depth: usize = 0,
};

pub const Diagnostic = struct {
    tag: Tag,
    span: Span,
    related: ?Span = null,

    pub const Tag = enum {
        borrow_conflict,
        borrow_escape,
        unsupported_borrow,
        unsupported_iteration,
        duplicate_name,
        unsupported_attribute,
        invalid_test_declaration,
        invalid_test_signature,
        test_not_callable,
        invalid_export_declaration,
        invalid_export_attribute,
        invalid_export_name,
        invalid_export_signature,
        duplicate_export_name,
        invalid_extern_declaration,
        invalid_extern_attribute,
        invalid_extern_name,
        invalid_extern_signature,
        conflicting_foreign_signature,
        noreturn_result,
        noreturn_return,
        noreturn_completion,
        unbound_type_variable,
        generic_function_ambiguous,
        generic_function_inference_unsupported,
        generic_function_conflict,
        generic_function_context,
        generic_function_limit,
        invalid_generic_declaration,
        generic_needs_arguments,
        not_a_generic,
        generic_argument_count,
        generic_cycle,
        generic_limit,
        unsupported_generic_method,
        invalid_propagation_operand,
        invalid_propagation_return,
        propagation_error_mismatch,
        empty_union,
        empty_enum,
        enum_needs_type,
        unsupported_enum_value,
        match_covered_enum_prong,
        unsupported_union_default,
        invalid_union_constructor,
        union_needs_type,
        match_missing_variants,
        match_covered_union_prong,
        invalid_match_capture,
        duplicate_field,
        unknown_field,
        missing_field,
        invalid_field_default,
        record_layout_cycle,
        record_layout_limit,
        invalid_to_string,
        invalid_stringify_declaration,
        duplicate_stringifier,
        invalid_method_receiver,
        unknown_name,
        unsupported_declaration,
        unsupported_name,
        value_cycle,
        type_alias_cycle,
        type_alias_limit,
        not_a_value,
        unsupported_dependency_expression,
        missing_initializer,
        unknown_type,
        unsupported_type,
        not_a_type,
        duplicate_parameter,
        invalid_parameter_type,
        type_nesting_limit,
        invalid_literal,
        unsupported_literal,
        invalid_interpolation,
        unsupported_interpolation_type,
        unsupported_global_interpolation,
        unsupported_global_initializer,
        literal_type_mismatch,
        literal_out_of_range,
        unsupported_body_expression,
        string_slice_requires_string,
        unsupported_match_pattern,
        unsupported_match_subject,
        match_missing_catchall,
        match_missing_boolean_values,
        match_missing_true,
        match_missing_false,
        match_covered_boolean_prong,
        match_catchall_not_last,
        match_pattern_count,
        unsupported_local_declaration,
        immutable_assignment,
        unsupported_assignment_target,
        unsupported_array_length_write,
        array_literal_needs_type,
        unused_block_value,
        unreachable_code,
        loop_control_outside_loop,
        unsupported_loop_control,
        conditional_value_break,
        loop_missing_value_break,
        unsupported_shadowing,
        expression_type_mismatch,
        operator_not_supported,
        literal_needs_type,
        not_callable,
        argument_count_mismatch,
        unresolved_value_type,
        body_nesting_limit,
        unsupported_lowering,
    };
};

pub const InitError = std.mem.Allocator.Error || error{
    InvalidSource,
    SourceTooLarge,
    TooManyDeclarations,
};

/// Owns copies of the path and source, the AST, names and diagnostics. This is a
/// module-scope collection pass only: no expression/type checking is performed.
pub fn init(allocator: std.mem.Allocator, path: []const u8, source: []const u8) InitError!Module {
    return initSource(allocator, @fromBackingInt(@intCast(0)), path, source);
}

pub fn initSource(allocator: std.mem.Allocator, id: Source.Id, path: []const u8, text: []const u8) InitError!Module {
    var module: Module = .{
        .allocator = allocator,
        .source = try Source.init(allocator, id, path, text),
    };
    errdefer module.source.deinit();
    errdefer {
        for (module.declarations.items) |decl| {
            if (decl.export_name) |name| allocator.free(name);
            if (decl.extern_name) |name| allocator.free(name);
        }
        module.declarations.deinit(allocator);
    }
    errdefer module.diagnostics.deinit(allocator);
    errdefer module.names.deinit(allocator);
    // Recovered ASTs remain available for syntax diagnostics, but must not feed
    // declaration collection or later semantic phases.
    if (module.source.ast.errors.len == 0) {
        try module.collectDeclarations();
        try module.classifyAliases();
    }
    return module;
}

pub fn deinit(self: *Module) void {
    self.names.deinit(self.allocator);
    for (self.declarations.items) |decl| {
        if (decl.function_template != null) self.allocator.free(decl.function_bindings);
        if (decl.export_name) |name| self.allocator.free(name);
        if (decl.extern_name) |name| self.allocator.free(name);
    }
    self.declarations.deinit(self.allocator);
    self.diagnostics.deinit(self.allocator);
    self.source.deinit();
    self.* = undefined;
}

pub fn hasErrors(self: *const Module) bool {
    return self.source.ast.errors.len != 0 or self.diagnostics.items.len != 0;
}

pub fn lookup(self: *const Module, name: []const u8) ?DeclId {
    return self.names.get(name);
}

/// Returned names borrow the source; handles remain valid across table growth.
pub fn getDeclaration(self: *const Module, id: DeclId) ?Declaration {
    const index = @backingInt(id);
    if (index >= self.declarations.items.len) return null;
    return self.declarations.items[index];
}

pub fn sourceSlice(self: *const Module, span: Span) ?[]const u8 {
    return self.source.slice(span);
}

pub fn tokenSpan(self: *const Module, token: Ast.TokenIndex) Span {
    return self.source.tokenSpan(token);
}

/// Resolves a supplied identifier use against all collected module declarations.
/// Body traversal and local/parameter scopes are separate, subsequent passes.
pub fn resolveName(self: *Module, span: Span) (std.mem.Allocator.Error || error{InvalidSpan})!?DeclId {
    const name = self.sourceSlice(span) orelse return error.InvalidSpan;
    if (name.len == 0) return error.InvalidSpan;
    if (self.lookup(name)) |id| return id;
    try self.diagnostics.append(self.allocator, .{ .tag = .unknown_name, .span = span });
    return null;
}

/// Only immutable, unannotated module declarations can name types. An
/// identifier initializer is not evidence of a type until its target is known
/// to be a type, or a type position demands it (including otherwise ambiguous
/// unknown names and cycles). Unconstrained references to runtime values stay
/// ordinary value declarations; demanded candidates must still pass resolution.
fn aliasCandidate(self: *const Module, id: DeclId) bool {
    if (self.getDeclaration(id).?.is_test) return false;
    const full = self.source.ast.decl(self.getDeclaration(id).?.node).ast;
    if (full.is_mutable or full.type_node != 0 or full.init_node == 0) return false;
    return switch (self.source.ast.nodes.items(.tag)[full.init_node]) {
        .identifier, .field_access, .array_type, .fn_proto, .fn_proto_one, .container_decl, .call, .call_one, .call_comma, .call_one_comma => true,
        else => false,
    };
}

/// Imported test declarations are never activated, even if their flag is set.
pub fn testEnabled(self: *const Module) bool {
    if (!self.tests_enabled) return false;
    if (self.compilation) |comp| return comp.root == self.source.id;
    return true;
}

/// AST-wide semantic scans must not discover demands inside disabled tests.
pub fn inDisabledTest(self: *const Module, node: Ast.Node.Index) bool {
    if (self.testEnabled()) return false;
    return self.inTest(node, false);
}

/// Import discovery ignores entire test declarations in both modes. Imports in
/// enabled signatures or bodies remain semantic errors, never graph dependencies.
pub fn inTest(self: *const Module, node: Ast.Node.Index, body_only: bool) bool {
    if (node == 0) return false;
    const token = self.source.ast.firstToken(node);
    for (self.declarations.items) |decl| {
        if (!decl.is_test) continue;
        const root = if (body_only) self.source.ast.decl(decl.node).ast.init_node else decl.node;
        if (root == 0) continue;
        if (token >= self.source.ast.firstToken(root) and token <= self.source.ast.lastToken(root)) return true;
    }
    return false;
}

/// Alias demands are analysis-local: a previous test-mode pass must not make
/// free names in now-disabled bodies affect an ordinary build.
pub fn resetAliases(self: *Module) void {
    for (self.declarations.items) |*decl| {
        if (decl.concrete_type != null) continue;
        const full = self.source.ast.decl(decl.node).ast;
        const this_alias = decl.container != null and !full.is_mutable and full.type_node == 0 and full.init_node != 0 and blk: {
            const call = self.source.ast.fullCall(full.init_node) orelse break :blk false;
            break :blk call.ast.args.len == 0 and std.mem.eql(u8, self.source.ast.tokenSlice(self.source.ast.firstToken(call.ast.fn_expr)), "@This");
        };
        decl.is_generic_function = if (self.source.ast.declFunction(decl.node)) |f| hasTypeVariables(&self.source.ast, f.proto_node) else false;
        decl.is_type_alias = decl.is_record or this_alias;
        decl.is_generic = decl.is_record and full.type_node != 0 and (self.source.ast.isUnion(full.init_node) or self.source.ast.tokens.items(.tag)[self.source.ast.nodes.items(.main_token)[full.init_node]] == .keyword_struct);
    }
}

pub fn classifyAliases(self: *Module) std.mem.Allocator.Error!void {
    var work: std.ArrayList(Ast.Node.Index) = .empty;
    defer work.deinit(self.allocator);
    const tags = self.source.ast.nodes.items(.tag);
    const data = self.source.ast.nodes.items(.data);
    // Explicit type positions include local annotations and @as arguments.
    for (tags, 0..) |tag, i| {
        const node: Ast.Node.Index = @intCast(i);
        if (self.inDisabledTest(node)) continue;
        switch (tag) {
            .decl => {
                const full = self.source.ast.decl(node).ast;
                if (full.type_node != 0) try work.append(self.allocator, full.type_node);
            },
            .container_field => if (data[node].lhs != 0) try work.append(self.allocator, data[node].lhs),
            .container_decl_arg => try work.append(self.allocator, data[node].lhs),
            .fn_proto, .fn_proto_one, .array_type => try work.append(self.allocator, node),
            .call, .call_one, .call_comma, .call_one_comma => {
                const call = self.source.ast.fullCall(node).?;
                if (std.mem.eql(u8, self.source.ast.tokenSlice(self.source.ast.firstToken(call.ast.fn_expr)), "@as") and call.ast.args.len != 0)
                    try work.append(self.allocator, call.ast.args[0]);
            },
            else => {},
        }
    }
    for (self.declarations.items, 0..) |*declaration, i| {
        if (!self.aliasCandidate(@fromBackingInt(@intCast(i)))) continue;
        const init_node = self.source.ast.decl(declaration.node).ast.init_node;
        const name = self.source.ast.tokenSlice(self.source.ast.firstToken(init_node));
        if (self.source.ast.fullCall(init_node)) |call| {
            if (tags[call.ast.fn_expr] != .identifier) continue;
            const target = self.lookup(self.source.ast.tokenSlice(self.source.ast.firstToken(call.ast.fn_expr))) orelse continue;
            if (!self.getDeclaration(target).?.is_generic) continue;
        }
        if (tags[init_node] == .field_access) continue;
        if (tags[init_node] != .identifier or (primitives.isPrimitive(name) and !std.mem.eql(u8, name, "true") and !std.mem.eql(u8, name, "false") and self.lookup(name) == null)) {
            declaration.is_type_alias = true;
            try work.append(self.allocator, init_node);
        }
    }
    var function_changed = true;
    while (function_changed) {
        function_changed = false;
        for (self.declarations.items, 0..) |*declaration, i| {
            if (declaration.is_generic_function or !self.aliasCandidate(@fromBackingInt(@intCast(i)))) continue;
            const init_node = self.source.ast.decl(declaration.node).ast.init_node;
            if (GenericFunctions.target(self, init_node) == null) continue;
            declaration.is_generic_function = true;
            declaration.is_type_alias = false;
            function_changed = true;
        }
    }
    // Generic function aliases must stabilize before classifying application aliases.
    var generic_changed = true;
    while (generic_changed) {
        generic_changed = false;
        for (self.declarations.items, 0..) |*declaration, i| {
            if (declaration.is_generic or !self.aliasCandidate(@fromBackingInt(@intCast(i)))) continue;
            const init_node = self.source.ast.decl(declaration.node).ast.init_node;
            if (tags[init_node] != .identifier) continue;
            const target = self.lookup(self.source.ast.tokenSlice(self.source.ast.firstToken(init_node))) orelse continue;
            if (self.getDeclaration(target).?.is_generic) {
                declaration.is_generic = true;
                declaration.is_type_alias = true;
                generic_changed = true;
            }
        }
    }
    for (tags, 0..) |_, i| {
        const node: Ast.Node.Index = @intCast(i);
        if (self.inDisabledTest(node)) continue;
        const call = self.source.ast.fullCall(node) orelse continue;
        if (tags[call.ast.fn_expr] != .identifier) continue;
        const target = self.lookup(self.source.ast.tokenSlice(self.source.ast.firstToken(call.ast.fn_expr))) orelse continue;
        if (self.getDeclaration(target).?.is_generic) try work.append(self.allocator, node);
    }
    while (true) {
        while (work.pop()) |node| {
            if (self.source.ast.fullFnProto(node)) |proto| {
                var iterator = proto.iterate(&self.source.ast);
                while (iterator.next()) |param| try work.append(self.allocator, param.type_expr);
                if (proto.ast.return_type) |result| try work.append(self.allocator, result);
            } else if (tags[node] == .array_type) {
                try work.append(self.allocator, data[node].rhs);
            } else if (self.source.ast.fullCall(node)) |call| {
                for (call.ast.args) |arg| try work.append(self.allocator, arg);
            } else if (tags[node] == .identifier) {
                const name = self.source.ast.tokenSlice(self.source.ast.firstToken(node));
                if (self.lookup(name)) |id| {
                    const declaration = &self.declarations.items[@backingInt(id)];
                    if (!declaration.is_type_alias and self.aliasCandidate(id)) {
                        declaration.is_type_alias = true;
                        try work.append(self.allocator, self.source.ast.decl(declaration.node).ast.init_node);
                    }
                }
            }
        }
        var changed = false;
        for (self.declarations.items, 0..) |*declaration, i| {
            if (declaration.is_type_alias or !self.aliasCandidate(@fromBackingInt(@intCast(i)))) continue;
            const init_node = self.source.ast.decl(declaration.node).ast.init_node;
            if (self.source.ast.fullCall(init_node)) |call| {
                if (tags[call.ast.fn_expr] != .identifier) continue;
                const target = self.lookup(self.source.ast.tokenSlice(self.source.ast.firstToken(call.ast.fn_expr))) orelse continue;
                if (self.getDeclaration(target).?.is_generic) {
                    declaration.is_type_alias = true;
                    try work.append(self.allocator, init_node);
                    changed = true;
                }
                continue;
            }
            if (tags[init_node] != .identifier) continue;
            const target = self.lookup(self.source.ast.tokenSlice(self.source.ast.firstToken(init_node))) orelse continue;
            if (self.getDeclaration(target).?.is_type_alias) {
                declaration.is_type_alias = true;
                declaration.is_generic = self.getDeclaration(target).?.is_generic;
                changed = true;
            }
        }
        if (!changed) break;
    }
}

pub fn lookupMethod(self: *const Module, container: DeclId, name: []const u8) ?DeclId {
    return self.lookupConcreteMethod(container, null, name);
}

pub fn lookupConcreteMethod(self: *const Module, container: DeclId, concrete_type: ?u32, name: []const u8) ?DeclId {
    for (self.declarations.items, 0..) |decl, i| if (decl.container == container and decl.concrete_type == concrete_type and std.mem.eql(u8, decl.name, name)) return @fromBackingInt(@intCast(i));
    return null;
}

pub fn lookupStringifier(self: *const Module, container: DeclId) ?DeclId {
    return self.lookupConcreteStringifier(container, null);
}

pub fn lookupConcreteStringifier(self: *const Module, container: DeclId, concrete_type: ?u32) ?DeclId {
    for (self.declarations.items, 0..) |decl, i| if (decl.container == container and decl.concrete_type == concrete_type and decl.stringify) return @fromBackingInt(@intCast(i));
    return null;
}

/// TypeIds belong to one analysis IR; never reuse specializations in a fresh pass.
pub fn clearSpecializations(self: *Module) void {
    for (self.declarations.items, 0..) |decl, i| if (decl.concrete_type != null or decl.function_template != null) {
        for (self.declarations.items[i..]) |specialization| {
            if (specialization.function_template != null) self.allocator.free(specialization.function_bindings);
        }
        self.declarations.shrinkRetainingCapacity(i);
        return;
    };
}

fn collectDeclarations(self: *Module) (std.mem.Allocator.Error || error{TooManyDeclarations})!void {
    for (self.source.ast.rootDecls()) |node| try self.collectDeclaration(node, null);
    const top_count = self.declarations.items.len;
    for (0..top_count) |i| {
        const decl = self.declarations.items[i];
        if (!decl.is_record) continue;
        for (self.source.ast.fullStruct(self.source.ast.decl(decl.node).ast.init_node).?) |member| {
            if (self.source.ast.nodes.items(.tag)[member] != .container_field) try self.collectDeclaration(member, @fromBackingInt(@intCast(i)));
        }
    }
}

fn collectDeclaration(self: *Module, node: Ast.Node.Index, container: ?DeclId) (std.mem.Allocator.Error || error{TooManyDeclarations})!void {
    {
        const declaration = self.source.ast.fullDecl(node) orelse {
            try self.diagnostics.append(self.allocator, .{
                .tag = .unsupported_declaration,
                .span = self.tokenSpan(self.source.ast.firstToken(node)),
            });
            return;
        };
        const name_token = declaration.ast.let_token + 1 + @intFromBool(declaration.ast.is_mutable);
        const name = self.source.ast.tokenSlice(name_token);
        const name_span = self.tokenSpan(name_token);
        if (std.mem.startsWith(u8, name, "@\"")) {
            try self.diagnostics.append(self.allocator, .{ .tag = .unsupported_name, .span = name_span });
            return;
        }
        const previous_id = if (container) |owner| self.lookupMethod(owner, name) else self.lookup(name);
        if (previous_id) |previous_decl| {
            const previous = self.getDeclaration(previous_decl).?;
            try self.diagnostics.append(self.allocator, .{
                .tag = .duplicate_name,
                .span = name_span,
                .related = previous.name_span,
            });
            return;
        }
        if (self.declarations.items.len == std.math.maxInt(u32)) return error.TooManyDeclarations;
        const id: DeclId = @fromBackingInt(@intCast(self.declarations.items.len));
        if (container == null) try self.names.put(self.allocator, name, id);
        const is_record = declaration.ast.init_node != 0 and self.source.ast.fullStruct(declaration.ast.init_node) != null;
        const is_generic_function = if (self.source.ast.declFunction(node)) |f| hasTypeVariables(&self.source.ast, f.proto_node) else false;
        if (is_generic_function and declaration.ast.type_node == 0) try self.diagnostics.append(self.allocator, .{ .tag = .generic_function_context, .span = name_span });
        if (is_generic_function and declaration.ast.is_mutable) try self.diagnostics.append(self.allocator, .{ .tag = .generic_function_context, .span = name_span });
        const is_generic = is_record and declaration.ast.type_node != 0 and (self.source.ast.isUnion(declaration.ast.init_node) or self.source.ast.tokens.items(.tag)[self.source.ast.nodes.items(.main_token)[declaration.ast.init_node]] == .keyword_struct);
        const is_this_alias = container != null and !declaration.ast.is_mutable and declaration.ast.type_node == 0 and declaration.ast.init_node != 0 and blk: {
            const call = self.source.ast.fullCall(declaration.ast.init_node) orelse break :blk false;
            break :blk call.ast.args.len == 0 and std.mem.eql(u8, self.source.ast.tokenSlice(self.source.ast.firstToken(call.ast.fn_expr)), "@This");
        };
        var is_test = false;
        var public = false;
        var noreturn_annotation = false;
        var stringify = false;
        var export_name: ?[]const u8 = null;
        errdefer if (export_name) |symbol| self.allocator.free(symbol);
        var extern_name: ?[]const u8 = null;
        errdefer if (extern_name) |symbol| self.allocator.free(symbol);
        var export_seen = false;
        var extern_seen = false;
        for (declaration.attributes) |attribute| {
            const data = self.source.ast.nodes.items(.data)[attribute];
            const attr_name = self.source.ast.tokenSlice(data.lhs);
            const external = std.mem.eql(u8, attr_name, "extern");
            if (std.mem.eql(u8, attr_name, "stringify")) {
                if (stringify or self.source.ast.nodes.items(.tag)[attribute] != .attr_one or data.rhs != 0)
                    try self.diagnostics.append(self.allocator, .{ .tag = .invalid_stringify_declaration, .span = self.tokenSpan(self.source.ast.firstToken(attribute)) });
                stringify = true;
            } else if (std.mem.eql(u8, attr_name, "test")) {
                if (is_test or self.source.ast.nodes.items(.tag)[attribute] != .attr_one or data.rhs != 0)
                    try self.diagnostics.append(self.allocator, .{ .tag = .invalid_test_declaration, .span = self.tokenSpan(self.source.ast.firstToken(attribute)) });
                is_test = true;
            } else if (external or std.mem.eql(u8, attr_name, "export")) {
                const span = self.tokenSpan(self.source.ast.firstToken(attribute));
                const seen = if (external) extern_seen else export_seen;
                const conflict = if (external) export_seen else extern_seen;
                if (external) extern_seen = true else export_seen = true;
                const invalid_attribute: Diagnostic.Tag = if (external) .invalid_extern_attribute else .invalid_export_attribute;
                const invalid_declaration: Diagnostic.Tag = if (external) .invalid_extern_declaration else .invalid_export_declaration;
                const invalid_name: Diagnostic.Tag = if (external) .invalid_extern_name else .invalid_export_name;
                if (seen or conflict or self.source.ast.nodes.items(.tag)[attribute] != .attr_one) {
                    try self.diagnostics.append(self.allocator, .{ .tag = invalid_attribute, .span = span });
                    continue;
                }
                if (container != null or declaration.ast.is_mutable or is_record or is_generic or is_generic_function or (if (external) declaration.ast.init_node != 0 else declaration.ast.init_node == 0) or self.source.ast.declFunction(node) == null) {
                    try self.diagnostics.append(self.allocator, .{ .tag = invalid_declaration, .span = span });
                    continue;
                }
                const symbol_name = if (external) &extern_name else &export_name;
                if (data.rhs == 0) {
                    symbol_name.* = try self.allocator.dupe(u8, name);
                } else {
                    if (self.source.ast.nodes.items(.tag)[data.rhs] != .string_literal) {
                        try self.diagnostics.append(self.allocator, .{ .tag = invalid_attribute, .span = span });
                        continue;
                    }
                    const bytes = self.source.ast.getNodeSource(data.rhs);
                    if (Literal.hasInterpolation(bytes)) {
                        try self.diagnostics.append(self.allocator, .{ .tag = invalid_attribute, .span = span });
                        continue;
                    }
                    var decoded = std.Io.Writer.Allocating.init(self.allocator);
                    defer decoded.deinit();
                    switch (string_literal.parseWrite(&decoded.writer, bytes) catch return error.OutOfMemory) {
                        .success => {},
                        .failure => {
                            try self.diagnostics.append(self.allocator, .{ .tag = invalid_attribute, .span = span });
                            continue;
                        },
                    }
                    symbol_name.* = try self.allocator.dupe(u8, decoded.written());
                }
                if (!(if (external) ExportNames.validExternName(symbol_name.*.?) else ExportNames.validName(symbol_name.*.?))) {
                    self.allocator.free(symbol_name.*.?);
                    symbol_name.* = null;
                    try self.diagnostics.append(self.allocator, .{ .tag = invalid_name, .span = span });
                }
            } else if (self.source.ast.nodes.items(.tag)[attribute] == .attr_one and data.rhs == 0 and std.mem.eql(u8, self.source.ast.tokenSlice(data.lhs), "public") and !declaration.ast.is_mutable and !public) {
                public = true;
            } else if (self.source.ast.nodes.items(.tag)[attribute] == .attr_one and data.rhs == 0 and std.mem.eql(u8, self.source.ast.tokenSlice(data.lhs), "noreturn") and self.source.ast.declFunction(node) != null and !declaration.ast.is_mutable and !noreturn_annotation) {
                noreturn_annotation = true;
            } else try self.diagnostics.append(self.allocator, .{ .tag = .unsupported_attribute, .span = self.tokenSpan(self.source.ast.firstToken(attribute)) });
        }
        if (is_test and (container != null or declaration.ast.is_mutable or is_record or is_generic or is_generic_function or declaration.ast.init_node == 0 or self.source.ast.declFunction(node) == null or export_seen or extern_seen or noreturn_annotation))
            try self.diagnostics.append(self.allocator, .{ .tag = .invalid_test_declaration, .span = name_span });
        if (stringify) {
            if (container == null or declaration.ast.is_mutable or is_record or is_generic or is_generic_function or is_test or noreturn_annotation or export_seen or extern_seen or declaration.ast.init_node == 0 or self.source.ast.declFunction(node) == null) {
                try self.diagnostics.append(self.allocator, .{ .tag = .invalid_stringify_declaration, .span = name_span });
            } else if (self.lookupStringifier(container.?)) |previous| {
                try self.diagnostics.append(self.allocator, .{ .tag = .duplicate_stringifier, .span = name_span, .related = self.getDeclaration(previous).?.name_span });
            }
        }
        try self.declarations.append(self.allocator, .{ .stringify = stringify, .is_test = is_test, .name = name, .name_span = name_span, .node = node, .container = container, .is_record = is_record, .is_generic = is_generic, .is_generic_function = is_generic_function, .is_type_alias = is_record or is_this_alias, .public = public, .noreturn = noreturn_annotation, .export_name = export_name, .extern_name = extern_name });
    }
}

test "stringify metadata selects one arbitrary method per nominal container" {
    const source =
        \\let S=struct{
        \\let toString:(s:S)->string="ordinary"
        \\@[stringify]
        \\let format:(s:S)->string="marked"
        \\}
        \\let U=union{none,
        \\@[stringify]
        \\let display:(u:U)->string="union"
        \\}
        \\let E=enum{one,
        \\@[stringify]
        \\let render:(e:E)->string="enum"
        \\}
        \\let Box:(T:type)->type=struct{value:T,
        \\@[stringify]
        \\let format:(b:@This())->string="box"
        \\}
    ;
    var module = try Module.init(std.testing.allocator, "stringify.nov", source);
    defer module.deinit();
    try std.testing.expect(!module.hasErrors());
    for ([_][]const u8{ "S", "U", "E", "Box" }, [_][]const u8{ "format", "display", "render", "format" }) |container, name| {
        const method = module.lookupStringifier(module.lookup(container).?).?;
        try std.testing.expectEqualStrings(name, module.getDeclaration(method).?.name);
        try std.testing.expect(module.getDeclaration(method).?.stringify);
    }
    try std.testing.expect(!module.getDeclaration(module.lookupMethod(module.lookup("S").?, "toString").?).?.stringify);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
}

test "stringify rejects arguments duplicates placement and nonordinary methods" {
    for ([_][]const u8{
        "@[stringify]\nlet format:(s:int)->string=\"bad\"",
        "@[stringify]\nlet value=1",
        "@[stringify]\nlet Alias=int",
        "@[stringify]\nlet S=struct{}",
        "let S=struct{\n@[stringify(1)]\nlet format:(s:S)->string=\"bad\"\n}",
        "let S=struct{\n@[stringify(1,2)]\nlet format:(s:S)->string=\"bad\"\n}",
        "let S=struct{\n@[stringify]\n@[stringify]\nlet format:(s:S)->string=\"bad\"\n}",
        "let S=struct{\n@[stringify]\nlet mut format:(s:S)->string=\"bad\"\n}",
        "let S=struct{\n@[stringify]\nlet format:(s:S)->string\n}",
        "let S=struct{\n@[stringify]\nlet value=1\n}",
        "let S=struct{\n@[stringify]\nlet Alias=int\n}",
        "let S=struct{\n@[stringify]\nlet format:(s:`T)->string=\"bad\"\n}",
        "let S=struct{\n@[noreturn]\n@[stringify]\nlet format:(s:S)=loop{}\n}",
        "let S=struct{\n@[stringify]\n@[noreturn]\nlet format:(s:S)=loop{}\n}",
        "let S=struct{\n@[test]\n@[stringify]\nlet format:()={}\n}",
        "let S=struct{\n@[stringify]\n@[extern]\nlet format:(s:S)->string\n}",
        "let S=struct{\n@[export]\n@[stringify]\nlet format:(s:S)->string=\"bad\"\n}",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "bad-stringify.nov", source);
        defer module.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        var diagnosed = false;
        for (module.diagnostics.items) |diagnostic| if (diagnostic.tag == .invalid_stringify_declaration) {
            diagnosed = true;
        };
        try std.testing.expect(diagnosed);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
    const duplicate = "let S=struct{\n@[stringify]\nlet first:(s:S)->string=\"first\"\n@[stringify]\nlet second:(s:S)->string=\"second\"\n}";
    var module = try Module.init(std.testing.allocator, "duplicate-stringify.nov", duplicate);
    defer module.deinit();
    try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
    try std.testing.expectEqual(Diagnostic.Tag.duplicate_stringifier, module.diagnostics.items[0].tag);
    try std.testing.expectEqualStrings("second", module.sourceSlice(module.diagnostics.items[0].span).?);
    try std.testing.expectEqualStrings("first", module.sourceSlice(module.diagnostics.items[0].related.?).?);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{duplicate});
}

test "test declarations preserve metadata and reject placement and conflicts" {
    var valid = try Module.init(std.testing.allocator, "tests.nov", "@[public]\n@[test]\nlet sample: () -> void = {}");
    defer valid.deinit();
    try std.testing.expect(!valid.hasErrors());
    try std.testing.expect(valid.getDeclaration(valid.lookup("sample").?).?.is_test);
    try std.testing.expect(!valid.tests_enabled);
    for ([_][]const u8{
        "@[test(1)]\nlet sample: () = {}",
        "@[test]\n@[test]\nlet sample: () = {}",
        "@[test]\nlet mut sample: () = {}",
        "@[test]\nlet sample: ()",
        "@[test]\nlet sample = 1",
        "@[test]\n@[noreturn]\nlet sample: () = {}",
        "@[noreturn]\n@[test]\nlet sample: () = {}",
        "@[test]\n@[export]\nlet sample: () = {}",
        "@[extern]\n@[test]\nlet sample: ()",
        "let Container = struct {\n@[test]\nlet sample: () = {}\n}",
        "@[test]\nlet Container: (T: type) -> type = struct { value: T, }",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "bad-test.nov", source);
        defer module.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        try std.testing.expect(module.hasErrors());
        try std.testing.expectEqual(Diagnostic.Tag.invalid_test_declaration, module.diagnostics.items[0].tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}

test "disabled test scans do not classify free names or generic applications" {
    var module = try Module.init(std.testing.allocator, "tests.nov",
        \\let ambiguous = missing
        \\let Box: (T: type) -> type = struct { value: T, }
        \\@[test]
        \\let sample: () = {
        \\let x: ambiguous = missing
        \\let y: Box(unknown) = missing
        \\}
    );
    defer module.deinit();
    try std.testing.expect(!module.hasErrors());
    const ambiguous = module.lookup("ambiguous").?;
    try std.testing.expect(!module.getDeclaration(ambiguous).?.is_type_alias);
    module.tests_enabled = true;
    module.resetAliases();
    try module.classifyAliases();
    try std.testing.expect(module.getDeclaration(ambiguous).?.is_type_alias);
    module.tests_enabled = false;
    module.resetAliases();
    try module.classifyAliases();
    try std.testing.expect(!module.getDeclaration(ambiguous).?.is_type_alias);
}

test "export attributes own decoded names independently of visibility" {
    const source = "@[export]\nlet add: (a: i32) -> i32 = a\n@[public]\n@[export(\"nov_\\x61dd\")]\nlet other: () = {}\n";
    var module = try Module.init(std.testing.allocator, "exports.nov", source);
    defer module.deinit();
    try std.testing.expect(!module.hasErrors());
    const add = module.getDeclaration(module.lookup("add").?).?;
    try std.testing.expect(!add.public);
    try std.testing.expectEqualStrings("add", add.export_name.?);
    try std.testing.expect(add.export_name.?.ptr != add.name.ptr);
    const other = module.getDeclaration(module.lookup("other").?).?;
    try std.testing.expect(other.public);
    try std.testing.expectEqualStrings("nov_add", other.export_name.?);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
}

test "export rejects C11 external library names even without emitted headers" {
    for ([_][]const u8{ "sin", "sqrt", "time", "signal" }) |symbol| {
        for ([_]bool{ false, true }) |explicit| {
            const source = if (explicit)
                try std.fmt.allocPrint(std.testing.allocator, "@[export(\"{s}\")]\nlet ordinary: () = {{}}", .{symbol})
            else
                try std.fmt.allocPrint(std.testing.allocator, "@[export]\nlet {s}: () = {{}}", .{symbol});
            defer std.testing.allocator.free(source);
            var module = try Module.init(std.testing.allocator, "reserved-library-export.nov", source);
            defer module.deinit();
            try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
            try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
            try std.testing.expectEqual(Diagnostic.Tag.invalid_export_name, module.diagnostics.items[0].tag);
            try std.testing.expect(module.declarations.items[0].export_name == null);
            try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
        }
    }
}

test "export rejects misplaced repeated malformed interpolated and reserved attributes" {
    for ([_][]const u8{
        "@[export]\nlet value = true",
        "@[export]\nlet mut value: () = {}",
        "@[export]\nlet Alias = i32",
        "@[export]\nlet Record = struct { x: i32, }",
        "@[export]\nlet Generic: (T: type) -> type = struct { x: T, }",
        "let Record = struct { @[export]\nlet method: () = {}\n}",
        "@[export]\n@[export]\nlet f: () = {}",
        "@[export(1)]\nlet f: () = {}",
        "@[export(\"a\", \"b\")]\nlet f: () = {}",
        "@[export(\"{value}\")]\nlet f: () = {}",
        "@[export(\"bad-name\")]\nlet f: () = {}",
        "@[export(\"printf\")]\nlet f: () = {}",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "bad-export.nov", source);
        defer module.deinit();
        try std.testing.expect(module.hasErrors());
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}

test "collection makes later declarations available before resolving uses" {
    var module = try Module.init(std.testing.allocator, "forward.nov", "let first = later\nlet later = 42");
    defer module.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expectEqual(@as(usize, 2), module.declarations.items.len);
    const first = module.getDeclaration(module.lookup("first").?).?;
    const later_id = module.lookup("later").?;
    const initializer = module.source.ast.decl(first.node).ast.init_node;
    const token = module.source.ast.nodes.items(.main_token)[initializer];
    try std.testing.expectEqual(later_id, (try module.resolveName(module.tokenSpan(token))).?);
    try std.testing.expect(module.lookup("absent") == null);
    try std.testing.expect(module.getDeclaration(@fromBackingInt(@intCast(99))) == null);
}

test "duplicate diagnostics retain both name locations and the first binding" {
    var module = try Module.init(std.testing.allocator, "duplicate.nov", "let text = \"é\"\r\n\tlet name = 1\r\n\tlet name = 2");
    defer module.deinit();
    try std.testing.expect(module.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
    const diagnostic = module.diagnostics.items[0];
    try std.testing.expectEqual(Diagnostic.Tag.duplicate_name, diagnostic.tag);
    try std.testing.expectEqualStrings("name", module.sourceSlice(diagnostic.span).?);
    try std.testing.expectEqualStrings("name", module.sourceSlice(diagnostic.related.?).?);
    try std.testing.expect(diagnostic.related.?.start < diagnostic.span.start);
    const first = module.getDeclaration(module.lookup("name").?).?;
    try std.testing.expectEqual(diagnostic.related.?, first.name_span);
}

test "function and mutable declarations retain their AST without evaluating bodies" {
    var module = try Module.init(std.testing.allocator, "functions.nov", "let first: () -> i32 = second()\nlet second: () -> i32 = first()\nlet mut storage: i32");
    defer module.deinit();
    try std.testing.expect(!module.hasErrors());
    const first = module.getDeclaration(module.lookup("first").?).?;
    const declaration = module.source.ast.decl(first.node);
    try std.testing.expect(module.source.ast.fullFnProto(declaration.ast.type_node) != null);
    const call = module.source.ast.fullCall(declaration.ast.init_node).?;
    const callee = module.source.ast.nodes.items(.main_token)[call.ast.fn_expr];
    try std.testing.expectEqual(module.lookup("second"), try module.resolveName(module.tokenSpan(callee)));
    const storage = module.source.ast.decl(module.getDeclaration(module.lookup("storage").?).?.node);
    try std.testing.expect(storage.ast.is_mutable);
    try std.testing.expectEqual(@as(Ast.Node.Index, 0), storage.ast.init_node);
}

test "unknown lookup reports a source error and invalid spans do not mutate diagnostics" {
    var module = try Module.init(std.testing.allocator, "unknown.nov", "let value = absent");
    defer module.deinit();
    const initializer = module.source.ast.decl(module.getDeclaration(module.lookup("value").?).?.node).ast.init_node;
    const span = module.tokenSpan(module.source.ast.nodes.items(.main_token)[initializer]);
    try std.testing.expectEqual(@as(?DeclId, null), try module.resolveName(span));
    try std.testing.expectEqual(Diagnostic.Tag.unknown_name, module.diagnostics.items[0].tag);
    try std.testing.expectEqualStrings("absent", module.sourceSlice(module.diagnostics.items[0].span).?);
    try std.testing.expectError(error.InvalidSpan, module.resolveName(.{ .start = 12, .end = 3 }));
    try std.testing.expectError(error.InvalidSpan, module.resolveName(.{ .start = 0, .end = 999 }));
    try std.testing.expectError(error.InvalidSpan, module.resolveName(.{ .start = 0, .end = 0 }));
    try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
}

test "source modules own input and skip collection after syntax errors" {
    const allocator = std.testing.allocator;
    const source = try allocator.dupe(u8, "let answer = 42");
    const path = try allocator.dupe(u8, "owned.nov");
    var module = Module.init(allocator, path, source) catch |err| {
        allocator.free(source);
        allocator.free(path);
        return err;
    };
    allocator.free(source);
    allocator.free(path);
    defer module.deinit();
    try std.testing.expectEqualStrings("owned.nov", module.source.path);
    try std.testing.expectEqualStrings("let answer = 42", module.source.ast.source);
    try std.testing.expect(module.lookup("answer") != null);

    var broken = try Module.init(allocator, "broken.nov", "let broken =");
    defer broken.deinit();
    try std.testing.expect(broken.hasErrors());
    try std.testing.expect(broken.source.ast.errors.len != 0);
    try std.testing.expectEqual(@as(usize, 0), broken.declarations.items.len);
    try std.testing.expectError(error.InvalidSource, Module.init(allocator, "nul.nov", "let a = 1\x00let b = 2"));
}

test "struct methods have separate scopes stable module IDs and preserved public metadata" {
    const source = "@[public]\nlet A=struct{x:int,\n@[public]\nlet method:(a:A)->int=a.x\n}\nlet B=struct{x:int,\nlet method:(b:B)->int=b.x\n}\nlet method:()->int=0";
    var module = try Module.init(std.testing.allocator, "scopes.nov", source);
    defer module.deinit();
    try std.testing.expect(!module.hasErrors());
    const a = module.lookup("A").?;
    const b = module.lookup("B").?;
    try std.testing.expectEqual(@as(u32, 0), @backingInt(a));
    try std.testing.expectEqual(@as(u32, 1), @backingInt(b));
    try std.testing.expectEqual(@as(u32, 2), @backingInt(module.lookup("method").?));
    const am = module.lookupMethod(a, "method").?;
    const bm = module.lookupMethod(b, "method").?;
    try std.testing.expect(am != bm and am != module.lookup("method").?);
    try std.testing.expect(module.getDeclaration(a).?.public and module.getDeclaration(am).?.public);
    try std.testing.expect(!module.getDeclaration(bm).?.public);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
}

test "visibility metadata supports immutable declarations and noreturn in either order" {
    const source =
        \\let unmarked = true
        \\let value = true
        \\@[public]
        \\let alias = value
        \\let ns = @import("other.nov")
        \\let R = struct {
        \\let method: () = {}
        \\}
        \\let E = enum { x, }
        \\let U = union { x: bool, }
        \\let G: (T: type) -> type = struct { x: T, }
        \\@[noreturn]
        \\let stop: () = loop {}
        \\@[noreturn]
        \\let halt: () = loop {}
        \\@[public]
        \\@[noreturn]
        \\let exit: () = loop {}
        \\@[noreturn]
        \\@[public]
        \\let quit: () = loop {}
    ;
    var module = try Module.init(std.testing.allocator, "visibility.nov", source);
    defer module.deinit();
    try std.testing.expect(!module.hasErrors());
    const unmarked = module.getDeclaration(module.lookup("unmarked").?).?;
    try std.testing.expect(!unmarked.public);
    for ([_][]const u8{ "value", "ns", "R", "E", "U", "G", "stop", "halt" }) |name| {
        const decl = module.getDeclaration(module.lookup(name).?).?;
        try std.testing.expect(!decl.public);
    }
    for ([_][]const u8{ "alias", "exit", "quit" }) |name| {
        const decl = module.getDeclaration(module.lookup(name).?).?;
        try std.testing.expect(decl.public);
    }
    const method = module.getDeclaration(module.lookupMethod(module.lookup("R").?, "method").?).?;
    try std.testing.expect(!method.public);
    for ([_][]const u8{ "stop", "halt", "exit", "quit" }) |name| {
        try std.testing.expect(module.getDeclaration(module.lookup(name).?).?.noreturn);
    }
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
}

test "private unsupported duplicate and argument-bearing visibility attributes diagnose" {
    for ([_][]const u8{
        "@[private]\nlet value = true",
        "@[private]\n@[public]\nlet value = true",
        "@[public]\n@[private]\nlet value = true",
        "@[public]\n@[public]\nlet value = true",
        "@[private(1)]\nlet value = true",
        "@[private(1, 2)]\nlet value = true",
        "@[public(1)]\nlet value = true",
        "@[private]\nlet mut value: bool = true",
        "let R = struct {\n@[private]\n@[public]\nlet method: () = {}\n}",
        "let R = struct {\n@[public]\n@[private]\nlet method: () = {}\n}",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "invalid-visibility.nov", source);
        defer module.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(Diagnostic.Tag.unsupported_attribute, module.diagnostics.items[0].tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}

test "repeated private attributes are each unsupported" {
    const source = "@[private]\n@[private]\nlet value = true";
    var module = try Module.init(std.testing.allocator, "private.nov", source);
    defer module.deinit();
    try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
    try std.testing.expectEqual(@as(usize, 2), module.diagnostics.items.len);
    for (module.diagnostics.items) |diagnostic| {
        try std.testing.expectEqual(Diagnostic.Tag.unsupported_attribute, diagnostic.tag);
    }
    try std.testing.expect(!module.getDeclaration(module.lookup("value").?).?.public);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
}

test "struct attributes and duplicate method names diagnose without leaking declarations" {
    for ([_][]const u8{
        "@[unknown]\nlet A=struct{}",                   "@[public(1)]\nlet A=struct{}",
        "@[public]\n@[public]\nlet A=struct{}",         "@[public]\nlet mut value:bool=true",
        "let A=struct{\n@[private(1)]\nlet f:()={}\n}", "let A=struct{\nlet f:()={}\nlet f:()={}\n}",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "bad-scope.nov", source);
        defer module.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        try std.testing.expect(module.hasErrors());
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}

test "unsupported attributes and quoted names diagnose rather than silently bind" {
    var module = try Module.init(std.testing.allocator, "unsupported.nov", "@[export]\n@[tag]\nlet decorated = 1\nlet @\"quoted\" = 2\nlet valid = 3");
    defer module.deinit();
    try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
    try std.testing.expectEqual(@as(usize, 3), module.diagnostics.items.len);
    try std.testing.expectEqual(Diagnostic.Tag.invalid_export_declaration, module.diagnostics.items[0].tag);
    try std.testing.expectEqual(Diagnostic.Tag.unsupported_attribute, module.diagnostics.items[1].tag);
    try std.testing.expectEqual(Diagnostic.Tag.unsupported_name, module.diagnostics.items[2].tag);
    try std.testing.expect(module.lookup("decorated") != null);
    try std.testing.expect(module.lookup("valid") != null);
}

fn testAllocationFailures(allocator: std.mem.Allocator, source: []const u8) !void {
    var module = try Module.init(allocator, "oom.nov", source);
    defer module.deinit();
    if (source.len >= 3) _ = try module.resolveName(.{ .start = 0, .end = 3 });
}

test "declaration identities and source-backed names survive table growth" {
    var source = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer source.deinit();
    for (0..32) |i| try source.writer.print("let name_{d} = {d}\n", .{ i, i });
    var module = try Module.init(std.testing.allocator, "growth.nov", source.written());
    defer module.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expectEqual(@as(usize, 32), module.declarations.items.len);
    try std.testing.expectEqual(@as(u32, 0), @backingInt(module.lookup("name_0").?));
    try std.testing.expectEqual(@as(u32, 31), @backingInt(module.lookup("name_31").?));
    const first = module.getDeclaration(module.lookup("name_0").?).?;
    try std.testing.expectEqualStrings("name_0", first.name);
    try std.testing.expectEqualStrings("name_0", module.sourceSlice(first.name_span).?);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source.written()});
}

test "module ownership survives allocation failures during collection and diagnostics" {
    for ([_][]const u8{
        "",
        "let first = later\nlet later = 42",
        "let name = 1\nlet name = 2",
        "let broken =",
        "@[export]\nlet decorated = 1\nlet @\"quoted\" = 2",
    }) |source| {
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}

test "extern attributes own exact and decoded names independently of visibility" {
    const source = "@[extern]\nlet abs: (x: i32) -> i32\n@[public]\n@[extern(\"nov_\\x61dd\")]\nlet visible: ()\n@[extern]\n@[noreturn]\nlet abort: ()";
    var module = try Module.init(std.testing.allocator, "extern.nov", source);
    defer module.deinit();
    try std.testing.expect(!module.hasErrors());
    const abs = module.getDeclaration(module.lookup("abs").?).?;
    try std.testing.expectEqualStrings("abs", abs.extern_name.?);
    try std.testing.expect(abs.extern_name.?.ptr != abs.name.ptr);
    try std.testing.expect(!abs.public);
    const visible = module.getDeclaration(module.lookup("visible").?).?;
    try std.testing.expect(visible.public);
    try std.testing.expectEqualStrings("nov_add", visible.extern_name.?);
    try std.testing.expect(module.getDeclaration(module.lookup("abort").?).?.noreturn);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
}

test "extern and export reject actual least and fast integer header macros" {
    for ([_][]const u8{ "INT_LEAST8_MAX", "INT_LEAST64_MIN", "UINT_FAST32_MAX", "INT_FAST16_MIN" }) |name| {
        for ([_]bool{ false, true }) |external| {
            const source = try std.fmt.allocPrint(std.testing.allocator, "@[{s}(\"{s}\")]\nlet foreign: (){s}", .{ if (external) "extern" else "export", name, if (external) "" else " = {}" });
            defer std.testing.allocator.free(source);
            var module = try Module.init(std.testing.allocator, "macro.nov", source);
            defer module.deinit();
            try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
            try std.testing.expectEqual(if (external) Diagnostic.Tag.invalid_extern_name else Diagnostic.Tag.invalid_export_name, module.diagnostics.items[0].tag);
            try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
        }
    }
}

test "extern rejects bodies methods aliases constants generics and invalid attributes" {
    for ([_][]const u8{
        "@[extern]\nlet f: () = {}",
        "@[extern]\nlet mut f: ()",
        "@[extern]\nlet Alias = i32",
        "@[extern]\nlet value = true",
        "@[extern]\nlet Generic: (T: type) -> type = struct { x: T, }",
        "let R = struct { @[extern]\nlet method: ()\n}",
        "@[extern]\n@[extern]\nlet f: ()",
        "@[extern(1)]\nlet f: ()",
        "@[extern(\"a\", \"b\")]\nlet f: ()",
        "@[extern(\"{value}\")]\nlet f: ()",
        "@[extern(\"bad-name\")]\nlet f: ()",
        "@[extern(\"int\")]\nlet f: ()",
        "@[extern]\n@[export]\nlet f: ()",
        "@[export]\n@[extern]\nlet f: () = {}",
        "@[export]\n@[extern]\nlet f: ()",
        "@[extern]\n@[export]\nlet f: () = {}",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "bad-extern.nov", source);
        defer module.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        try std.testing.expect(module.hasErrors());
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}
