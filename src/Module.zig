const std = @import("std");
const Ast = @import("Ast.zig");
const Source = @import("Source.zig");
const primitives = @import("primitives.zig");
const Module = @This();

allocator: std.mem.Allocator,
compilation: ?*@import("Compilation.zig") = null,
source: Source,
declarations: std.ArrayList(Declaration) = .empty,
diagnostics: std.ArrayList(Diagnostic) = .empty,
names: std.StringHashMapUnmanaged(DeclId) = .empty,

pub const DeclId = enum(u32) { _ };

/// Half-open, source-qualified byte range, not a clipped highlight.
pub const Span = Source.Span;

pub const Declaration = struct {
    name: []const u8,
    name_span: Span,
    node: Ast.Node.Index,
    is_type_alias: bool = false,
    is_namespace: bool = false,
    is_record: bool = false,
    is_generic: bool = false,
    container: ?DeclId = null,
    public: bool = false,
    noreturn: bool = false,
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
        duplicate_name,
        unsupported_attribute,
        noreturn_result,
        noreturn_return,
        noreturn_completion,
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
    return initSource(allocator, @enumFromInt(0), path, source);
}

pub fn initSource(allocator: std.mem.Allocator, id: Source.Id, path: []const u8, text: []const u8) InitError!Module {
    var module: Module = .{
        .allocator = allocator,
        .source = try Source.init(allocator, id, path, text),
    };
    errdefer module.source.deinit();
    errdefer module.declarations.deinit(allocator);
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
    const index = @intFromEnum(id);
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
    const full = self.source.ast.decl(self.getDeclaration(id).?.node).ast;
    if (full.is_mutable or full.type_node != 0 or full.init_node == 0) return false;
    return switch (self.source.ast.nodes.items(.tag)[full.init_node]) {
        .identifier, .field_access, .array_type, .fn_proto, .fn_proto_one, .container_decl, .call, .call_one, .call_comma, .call_one_comma => true,
        else => false,
    };
}

pub fn classifyAliases(self: *Module) std.mem.Allocator.Error!void {
    var work: std.ArrayList(Ast.Node.Index) = .empty;
    defer work.deinit(self.allocator);
    const tags = self.source.ast.nodes.items(.tag);
    const data = self.source.ast.nodes.items(.data);
    // Explicit type positions include local annotations and @as arguments.
    for (tags, 0..) |tag, i| {
        const node: Ast.Node.Index = @intCast(i);
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
        if (!self.aliasCandidate(@enumFromInt(i))) continue;
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
    // Generic function aliases must stabilize before classifying application aliases.
    var generic_changed = true;
    while (generic_changed) {
        generic_changed = false;
        for (self.declarations.items, 0..) |*declaration, i| {
            if (declaration.is_generic or !self.aliasCandidate(@enumFromInt(i))) continue;
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
                    const declaration = &self.declarations.items[@intFromEnum(id)];
                    if (!declaration.is_type_alias and self.aliasCandidate(id)) {
                        declaration.is_type_alias = true;
                        try work.append(self.allocator, self.source.ast.decl(declaration.node).ast.init_node);
                    }
                }
            }
        }
        var changed = false;
        for (self.declarations.items, 0..) |*declaration, i| {
            if (declaration.is_type_alias or !self.aliasCandidate(@enumFromInt(i))) continue;
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
    for (self.declarations.items, 0..) |decl, i| if (decl.container == container and decl.concrete_type == concrete_type and std.mem.eql(u8, decl.name, name)) return @enumFromInt(i);
    return null;
}

/// TypeIds belong to one analysis IR; never reuse specializations in a fresh pass.
pub fn clearSpecializations(self: *Module) void {
    for (self.declarations.items, 0..) |decl, i| if (decl.concrete_type != null) {
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
            if (self.source.ast.nodes.items(.tag)[member] != .container_field) try self.collectDeclaration(member, @enumFromInt(i));
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
        const id: DeclId = @enumFromInt(self.declarations.items.len);
        if (container == null) try self.names.put(self.allocator, name, id);
        const is_record = declaration.ast.init_node != 0 and self.source.ast.fullStruct(declaration.ast.init_node) != null;
        const is_generic = is_record and declaration.ast.type_node != 0 and (self.source.ast.isUnion(declaration.ast.init_node) or self.source.ast.tokens.items(.tag)[self.source.ast.nodes.items(.main_token)[declaration.ast.init_node]] == .keyword_struct);
        const is_this_alias = container != null and !declaration.ast.is_mutable and declaration.ast.type_node == 0 and declaration.ast.init_node != 0 and blk: {
            const call = self.source.ast.fullCall(declaration.ast.init_node) orelse break :blk false;
            break :blk call.ast.args.len == 0 and std.mem.eql(u8, self.source.ast.tokenSlice(self.source.ast.firstToken(call.ast.fn_expr)), "@This");
        };
        var public = false;
        var noreturn_annotation = false;
        for (declaration.attributes) |attribute| {
            const data = self.source.ast.nodes.items(.data)[attribute];
            if (self.source.ast.nodes.items(.tag)[attribute] == .attr_one and data.rhs == 0 and std.mem.eql(u8, self.source.ast.tokenSlice(data.lhs), "public") and !declaration.ast.is_mutable and !public) {
                public = true;
            } else if (self.source.ast.nodes.items(.tag)[attribute] == .attr_one and data.rhs == 0 and std.mem.eql(u8, self.source.ast.tokenSlice(data.lhs), "noreturn") and self.source.ast.fullFnProto(declaration.ast.type_node) != null and !declaration.ast.is_mutable and !noreturn_annotation) {
                noreturn_annotation = true;
            } else try self.diagnostics.append(self.allocator, .{ .tag = .unsupported_attribute, .span = self.tokenSpan(self.source.ast.firstToken(attribute)) });
        }
        try self.declarations.append(self.allocator, .{ .name = name, .name_span = name_span, .node = node, .container = container, .is_record = is_record, .is_generic = is_generic, .is_type_alias = is_record or is_this_alias, .public = public, .noreturn = noreturn_annotation });
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
    try std.testing.expect(module.getDeclaration(@enumFromInt(99)) == null);
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
    try std.testing.expectEqual(@as(u32, 0), @intFromEnum(a));
    try std.testing.expectEqual(@as(u32, 1), @intFromEnum(b));
    try std.testing.expectEqual(@as(u32, 2), @intFromEnum(module.lookup("method").?));
    const am = module.lookupMethod(a, "method").?;
    const bm = module.lookupMethod(b, "method").?;
    try std.testing.expect(am != bm and am != module.lookup("method").?);
    try std.testing.expect(module.getDeclaration(a).?.public and module.getDeclaration(am).?.public);
    try std.testing.expect(!module.getDeclaration(bm).?.public);
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
}

test "struct attributes and duplicate method names diagnose without leaking declarations" {
    for ([_][]const u8{
        "@[unknown]\nlet A=struct{}",                "@[public(1)]\nlet A=struct{}",
        "@[public]\n@[public]\nlet A=struct{}",      "@[public]\nlet mut value:bool=true",
        "let A=struct{\n@[private]\nlet f:()={}\n}", "let A=struct{\nlet f:()={}\nlet f:()={}\n}",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "bad-scope.nov", source);
        defer module.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        try std.testing.expect(module.hasErrors());
        try std.testing.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}

test "unsupported attributes and quoted names diagnose rather than silently bind" {
    var module = try Module.init(std.testing.allocator, "unsupported.nov", "@[export]\n@[tag]\nlet decorated = 1\nlet @\"quoted\" = 2\nlet valid = 3");
    defer module.deinit();
    try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
    try std.testing.expectEqual(@as(usize, 3), module.diagnostics.items.len);
    try std.testing.expectEqual(Diagnostic.Tag.unsupported_attribute, module.diagnostics.items[0].tag);
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
    try std.testing.expectEqual(@as(u32, 0), @intFromEnum(module.lookup("name_0").?));
    try std.testing.expectEqual(@as(u32, 31), @intFromEnum(module.lookup("name_31").?));
    const first = module.getDeclaration(module.lookup("name_0").?).?;
    try std.testing.expectEqualStrings("name_0", first.name);
    try std.testing.expectEqualStrings("name_0", module.sourceSlice(first.name_span).?);
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source.written()});
}

test "module ownership survives allocation failures during collection and diagnostics" {
    for ([_][]const u8{
        "",
        "let first = later\nlet later = 42",
        "let name = 1\nlet name = 2",
        "let broken =",
        "@[export]\nlet decorated = 1\nlet @\"quoted\" = 2",
    }) |source| {
        try std.testing.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}
