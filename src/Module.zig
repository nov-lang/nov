const std = @import("std");
const Ast = @import("Ast.zig");
const Parser = @import("Parser.zig");
const Module = @This();

allocator: std.mem.Allocator,
path: []const u8,
ast: Ast,
declarations: std.ArrayList(Declaration) = .empty,
diagnostics: std.ArrayList(Diagnostic) = .empty,
names: std.StringHashMapUnmanaged(DeclId) = .empty,

pub const DeclId = enum(u32) { _ };

/// Half-open byte range in this module's owned source, not a clipped highlight.
pub const Span = struct {
    start: u32,
    end: u32,
};

pub const Declaration = struct {
    name: []const u8,
    name_span: Span,
    node: Ast.Node.Index,
};

pub const Diagnostic = struct {
    tag: Tag,
    span: Span,
    related: ?Span = null,

    pub const Tag = enum {
        duplicate_name,
        unknown_name,
        unsupported_declaration,
        unsupported_name,
        value_cycle,
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
        literal_type_mismatch,
        literal_out_of_range,
        unsupported_body_expression,
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
    if (source.len > std.math.maxInt(u32)) return error.SourceTooLarge;
    if (std.mem.indexOfScalar(u8, source, 0) != null) return error.InvalidSource;
    const owned_path = try allocator.dupe(u8, path);
    errdefer allocator.free(owned_path);
    const owned_source = try allocator.dupeZ(u8, source);
    errdefer allocator.free(owned_source);
    var module: Module = .{
        .allocator = allocator,
        .path = owned_path,
        .ast = try Parser.parse(allocator, owned_source),
    };
    errdefer module.ast.deinit(allocator);
    errdefer module.declarations.deinit(allocator);
    errdefer module.diagnostics.deinit(allocator);
    errdefer module.names.deinit(allocator);
    // Recovered ASTs remain available for syntax diagnostics, but must not feed
    // declaration collection or later semantic phases.
    if (module.ast.errors.len == 0) try module.collectDeclarations();
    return module;
}

pub fn deinit(self: *Module) void {
    self.names.deinit(self.allocator);
    self.declarations.deinit(self.allocator);
    self.diagnostics.deinit(self.allocator);
    const source = self.ast.source;
    self.ast.deinit(self.allocator);
    self.allocator.free(source);
    self.allocator.free(self.path);
    self.* = undefined;
}

pub fn hasErrors(self: *const Module) bool {
    return self.ast.errors.len != 0 or self.diagnostics.items.len != 0;
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
    if (span.start > span.end or span.end > self.ast.source.len) return null;
    return self.ast.source[span.start..span.end];
}

pub fn tokenSpan(self: *const Module, token: Ast.TokenIndex) Span {
    const start = self.ast.tokens.items(.start)[token];
    return .{ .start = start, .end = start + @as(u32, @intCast(self.ast.tokenSlice(token).len)) };
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

fn collectDeclarations(self: *Module) (std.mem.Allocator.Error || error{TooManyDeclarations})!void {
    for (self.ast.rootDecls()) |node| {
        const declaration = self.ast.fullDecl(node) orelse {
            try self.diagnostics.append(self.allocator, .{
                .tag = .unsupported_declaration,
                .span = self.tokenSpan(self.ast.firstToken(node)),
            });
            continue;
        };
        const name_token = declaration.ast.let_token + 1 + @intFromBool(declaration.ast.is_mutable);
        const name = self.ast.tokenSlice(name_token);
        const name_span = self.tokenSpan(name_token);
        if (std.mem.startsWith(u8, name, "@\"")) {
            try self.diagnostics.append(self.allocator, .{ .tag = .unsupported_name, .span = name_span });
            continue;
        }
        const entry = try self.names.getOrPut(self.allocator, name);
        if (entry.found_existing) {
            const previous = self.getDeclaration(entry.value_ptr.*).?;
            try self.diagnostics.append(self.allocator, .{
                .tag = .duplicate_name,
                .span = name_span,
                .related = previous.name_span,
            });
            continue;
        }
        if (self.declarations.items.len == std.math.maxInt(u32)) return error.TooManyDeclarations;
        entry.value_ptr.* = @enumFromInt(self.declarations.items.len);
        try self.declarations.append(self.allocator, .{ .name = name, .name_span = name_span, .node = node });
    }
}

test "collection makes later declarations available before resolving uses" {
    var module = try Module.init(std.testing.allocator, "forward.nov", "let first = later\nlet later = 42");
    defer module.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expectEqual(@as(usize, 2), module.declarations.items.len);
    const first = module.getDeclaration(module.lookup("first").?).?;
    const later_id = module.lookup("later").?;
    const initializer = module.ast.decl(first.node).ast.init_node;
    const token = module.ast.nodes.items(.main_token)[initializer];
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
    const declaration = module.ast.decl(first.node);
    try std.testing.expect(module.ast.fullFnProto(declaration.ast.type_node) != null);
    const call = module.ast.fullCall(declaration.ast.init_node).?;
    const callee = module.ast.nodes.items(.main_token)[call.ast.fn_expr];
    try std.testing.expectEqual(module.lookup("second"), try module.resolveName(module.tokenSpan(callee)));
    const storage = module.ast.decl(module.getDeclaration(module.lookup("storage").?).?.node);
    try std.testing.expect(storage.ast.is_mutable);
    try std.testing.expectEqual(@as(Ast.Node.Index, 0), storage.ast.init_node);
}

test "unknown lookup reports a source error and invalid spans do not mutate diagnostics" {
    var module = try Module.init(std.testing.allocator, "unknown.nov", "let value = absent");
    defer module.deinit();
    const initializer = module.ast.decl(module.getDeclaration(module.lookup("value").?).?.node).ast.init_node;
    const span = module.tokenSpan(module.ast.nodes.items(.main_token)[initializer]);
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
    try std.testing.expectEqualStrings("owned.nov", module.path);
    try std.testing.expectEqualStrings("let answer = 42", module.ast.source);
    try std.testing.expect(module.lookup("answer") != null);

    var broken = try Module.init(allocator, "broken.nov", "let broken =");
    defer broken.deinit();
    try std.testing.expect(broken.hasErrors());
    try std.testing.expect(broken.ast.errors.len != 0);
    try std.testing.expectEqual(@as(usize, 0), broken.declarations.items.len);
    try std.testing.expectError(error.InvalidSource, Module.init(allocator, "nul.nov", "let a = 1\x00let b = 2"));
}

test "unsupported attributes and quoted names diagnose rather than silently bind" {
    var module = try Module.init(std.testing.allocator, "unsupported.nov", "@[export]\n@[tag]\nlet decorated = 1\nlet @\"quoted\" = 2\nlet valid = 3");
    defer module.deinit();
    try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
    try std.testing.expectEqual(@as(usize, 2), module.diagnostics.items.len);
    try std.testing.expectEqual(Diagnostic.Tag.unsupported_declaration, module.diagnostics.items[0].tag);
    try std.testing.expectEqual(Diagnostic.Tag.unsupported_name, module.diagnostics.items[1].tag);
    try std.testing.expect(module.lookup("decorated") == null);
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
