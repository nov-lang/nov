const std = @import("std");
const Diagnostics = @import("Diagnostics.zig");
const Module = @import("Module.zig");
const Ast = @import("Ast.zig");
const isPrimitive = @import("primitives.zig").isPrimitive;
const Dependencies = @This();

allocator: std.mem.Allocator,
entries: []Entry,
edges: std.ArrayList(Edge) = .empty,
/// Dependency-first order of successful declarations, not a runtime initialization policy.
order: std.ArrayList(Module.DeclId) = .empty,

/// These states describe initializer dependencies only, not type/body checking.
pub const State = enum { unvisited, visiting, resolved, failed };
pub const Entry = struct {
    start: usize = 0,
    end: usize = 0,
    state: State = .unvisited,
};
pub const Edge = struct {
    target: Module.DeclId,
    span: Module.Span,
};
pub const Error = std.mem.Allocator.Error || error{InvalidModule};

/// Requires a successfully collected module. Results use module-relative IDs and
/// spans. On allocation failure the module's diagnostics are rolled back.
pub fn analyze(module: *Module) Error!Dependencies {
    if (module.hasErrors()) return error.InvalidModule;
    const diagnostics_start = module.diagnostics.items.len;
    errdefer module.diagnostics.shrinkRetainingCapacity(diagnostics_start);
    var result: Dependencies = .{
        .allocator = module.allocator,
        .entries = try module.allocator.alloc(Entry, module.declarations.items.len),
    };
    errdefer result.deinit();
    @memset(result.entries, .{});
    var work: std.ArrayList(Ast.Node.Index) = .empty;
    defer work.deinit(module.allocator);
    for (result.entries, 0..) |*entry, i| {
        entry.start = result.edges.items.len;
        try result.scanInitializer(module, @enumFromInt(i), &work);
        entry.end = result.edges.items.len;
    }
    try result.resolveGraph(module);
    return result;
}

pub fn deinit(self: *Dependencies) void {
    self.allocator.free(self.entries);
    self.edges.deinit(self.allocator);
    self.order.deinit(self.allocator);
    self.* = undefined;
}

pub fn getState(self: *const Dependencies, id: Module.DeclId) ?State {
    const index = @intFromEnum(id);
    if (index >= self.entries.len) return null;
    return self.entries[index].state;
}

/// Edges borrow this result; failed declarations may have only partial edges
/// because unsupported subtrees are not traversed.
pub fn getEdges(self: *const Dependencies, id: Module.DeclId) ?[]const Edge {
    const index = @intFromEnum(id);
    if (index >= self.entries.len) return null;
    const entry = self.entries[index];
    return self.edges.items[entry.start..entry.end];
}

fn scanInitializer(self: *Dependencies, module: *Module, id: Module.DeclId, work: *std.ArrayList(Ast.Node.Index)) std.mem.Allocator.Error!void {
    const entry = &self.entries[@intFromEnum(id)];
    const declaration = module.getDeclaration(id).?;
    const ast = &module.ast;
    const full = ast.decl(declaration.node);
    if (declaration.is_type_alias) return;
    if (full.ast.init_node == 0) {
        try module.diagnostics.append(self.allocator, .{ .tag = .missing_initializer, .span = declaration.name_span });
        entry.state = .failed;
        return;
    }
    // A named function declaration creates a function value; its body is not
    // evaluated while creating it. Signatures and bodies are checked later.
    if (full.ast.type_node != 0 and ast.fullFnProto(full.ast.type_node) != null) return;
    work.clearRetainingCapacity();
    try work.append(self.allocator, full.ast.init_node);
    while (work.pop()) |node| {
        const tag = ast.nodes.items(.tag)[node];
        const data = ast.nodes.items(.data)[node];
        const span = module.tokenSpan(ast.nodes.items(.main_token)[node]);
        switch (tag) {
            .number_literal, .char_literal, .string_literal => {},
            .identifier => {
                const name = module.sourceSlice(span).?;
                if (std.mem.startsWith(u8, name, "@\"")) {
                    try module.diagnostics.append(self.allocator, .{ .tag = .unsupported_name, .span = span });
                    entry.state = .failed;
                } else if (module.lookup(name)) |target| {
                    try self.edges.append(self.allocator, .{ .target = target, .span = span });
                } else if (!isPrimitive(name)) {
                    try module.diagnostics.append(self.allocator, .{ .tag = .unknown_name, .span = span });
                    entry.state = .failed;
                }
            },
            .negation, .bool_not, .bit_not => try work.append(self.allocator, data.rhs),
            .add,
            .sub,
            .mul,
            .div,
            .mod,
            .equal_equal,
            .bang_equal,
            .less_than,
            .greater_than,
            .less_or_equal,
            .greater_or_equal,
            .shl,
            .shr,
            .bit_and,
            .bit_xor,
            .bit_or,
            => {
                try work.append(self.allocator, data.rhs);
                try work.append(self.allocator, data.lhs);
            },
            .block, .block_two, .block_newline, .block_two_newline => {
                var buffer: [2]Ast.Node.Index = undefined;
                const block = ast.fullBlock(&buffer, node).?;
                var has_declarations = false;
                for (block.ast.statements) |statement| {
                    if (ast.nodes.items(.tag)[statement] == .decl) has_declarations = true;
                }
                if (has_declarations) {
                    // Do not resolve local names as module names before lexical
                    // scope rules are implemented.
                    try module.diagnostics.append(self.allocator, .{ .tag = .unsupported_dependency_expression, .span = span });
                    entry.state = .failed;
                    continue;
                }
                var i = block.ast.statements.len;
                while (i != 0) {
                    i -= 1;
                    try work.append(self.allocator, block.ast.statements[i]);
                }
            },
            else => {
                // Calls can read globals through function bodies; ignoring a
                // callee's body would hide cycles. Reject these until modeled.
                try module.diagnostics.append(self.allocator, .{ .tag = .unsupported_dependency_expression, .span = span });
                entry.state = .failed;
            },
        }
    }
}

fn resolveGraph(self: *Dependencies, module: *Module) std.mem.Allocator.Error!void {
    const Frame = struct { id: Module.DeclId, next: usize };
    var stack: std.ArrayList(Frame) = .empty;
    defer stack.deinit(self.allocator);
    for (self.entries, 0..) |*root, i| {
        if (root.state != .unvisited) continue;
        try stack.append(self.allocator, .{ .id = @enumFromInt(i), .next = root.start });
        root.state = .visiting;
        while (stack.items.len != 0) {
            const frame = &stack.items[stack.items.len - 1];
            const entry = &self.entries[@intFromEnum(frame.id)];
            if (frame.next == entry.end) {
                if (entry.state == .visiting) {
                    entry.state = .resolved;
                    try self.order.append(self.allocator, frame.id);
                }
                stack.items.len -= 1;
                if (entry.state == .failed and stack.items.len != 0) {
                    self.entries[@intFromEnum(stack.items[stack.items.len - 1].id)].state = .failed;
                }
                continue;
            }
            const edge = self.edges.items[frame.next];
            frame.next += 1;
            const target = &self.entries[@intFromEnum(edge.target)];
            switch (target.state) {
                .unvisited => {
                    try stack.append(self.allocator, .{ .id = edge.target, .next = target.start });
                    target.state = .visiting;
                },
                .visiting => {
                    try module.diagnostics.append(self.allocator, .{
                        .tag = .value_cycle,
                        .span = edge.span,
                        .related = module.getDeclaration(edge.target).?.name_span,
                    });
                    entry.state = .failed;
                },
                .failed => entry.state = .failed,
                .resolved => {},
            }
        }
    }
}

test "value dependencies resolve forward references in dependency-first order" {
    var module = try Module.init(std.testing.allocator, "forward.nov", "let a = {b + 1}\nlet b = c * 2\nlet c = 3");
    defer module.deinit();
    var result = try analyze(&module);
    defer result.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expectEqualSlices(Module.DeclId, &.{ module.lookup("c").?, module.lookup("b").?, module.lookup("a").? }, result.order.items);
    const edges = result.getEdges(module.lookup("a").?).?;
    try std.testing.expectEqual(@as(usize, 1), edges.len);
    try std.testing.expectEqual(module.lookup("b").?, edges[0].target);
    try std.testing.expectEqualStrings("b", module.sourceSlice(edges[0].span).?);
    try std.testing.expectEqual(State.resolved, result.getState(module.lookup("a").?).?);
    try std.testing.expect(result.getState(@enumFromInt(999)) == null);
    try std.testing.expect(result.getEdges(@enumFromInt(999)) == null);
}

test "self and indirect value cycles identify the reference closing the cycle" {
    const sources = [_][]const u8{
        "let a = a",
        "let a = b\nlet b = a",
        "let a = b\nlet b = c\nlet c = a",
    };
    for (sources) |source| {
        var module = try Module.init(std.testing.allocator, "cycle.nov", source);
        defer module.deinit();
        var result = try analyze(&module);
        defer result.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        const diagnostic = module.diagnostics.items[0];
        try std.testing.expectEqual(Module.Diagnostic.Tag.value_cycle, diagnostic.tag);
        try std.testing.expectEqualStrings("a", module.sourceSlice(diagnostic.span).?);
        try std.testing.expectEqual(module.getDeclaration(module.lookup("a").?).?.name_span, diagnostic.related.?);
        try std.testing.expectEqual(@as(usize, 0), result.order.items.len);
        for (result.entries) |entry| try std.testing.expectEqual(State.failed, entry.state);
    }
}

test "unknown dependencies fail their dependents but preserve unrelated declarations" {
    var module = try Module.init(std.testing.allocator, "unknown.nov", "let a = b\nlet b = missing\nlet good = true");
    defer module.deinit();
    var result = try analyze(&module);
    defer result.deinit();
    try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
    try std.testing.expectEqual(Module.Diagnostic.Tag.unknown_name, module.diagnostics.items[0].tag);
    try std.testing.expectEqual(State.failed, result.getState(module.lookup("a").?).?);
    try std.testing.expectEqual(State.failed, result.getState(module.lookup("b").?).?);
    try std.testing.expectEqual(State.resolved, result.getState(module.lookup("good").?).?);
    try std.testing.expectEqualSlices(Module.DeclId, &.{module.lookup("good").?}, result.order.items);
}

test "function bodies are not eager value dependencies" {
    var module = try Module.init(std.testing.allocator, "functions.nov", "let f: () -> i32 = g()\nlet g: () -> i32 = f()\nlet h: () -> i32 = alias\nlet alias = h");
    defer module.deinit();
    var result = try analyze(&module);
    defer result.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expectEqual(@as(usize, 0), result.getEdges(module.lookup("h").?).?.len);
    try std.testing.expectEqual(@as(usize, 1), result.getEdges(module.lookup("alias").?).?.len);
    for (result.entries) |entry| try std.testing.expectEqual(State.resolved, entry.state);
}

test "unsupported eager expressions are diagnosed rather than hiding dependencies" {
    for ([_][]const u8{
        "let value = f()\nlet f: () -> i32 = value",
        "let value = [value]",
        "let value = {\nlet local = 1\nlocal\n}",
        "let value = false and value",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "unsupported.nov", source);
        defer module.deinit();
        var result = try analyze(&module);
        defer result.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(Module.Diagnostic.Tag.unsupported_dependency_expression, module.diagnostics.items[0].tag);
        try std.testing.expectEqual(State.failed, result.getState(module.lookup("value").?).?);
    }
    var module = try Module.init(std.testing.allocator, "missing.nov", "let value: i32");
    defer module.deinit();
    var result = try analyze(&module);
    defer result.deinit();
    try std.testing.expectEqual(Module.Diagnostic.Tag.missing_initializer, module.diagnostics.items[0].tag);
}

test "dependency cycle diagnostics render the use and declaration locations" {
    var module = try Module.init(std.testing.allocator, "cycle.nov", "let a = b\nlet b = a");
    defer module.deinit();
    var result = try analyze(&module);
    defer result.deinit();
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try Diagnostics.renderModule(&module, &output.writer);
    try std.testing.expectEqualStrings(
        "cycle.nov:2:9: error: cyclic value dependency on 'a'\nlet b = a\n        ^\n" ++
            "cycle.nov:1:5: note: declaration in this cycle\nlet a = b\n    ^\n",
        output.written(),
    );
}

test "dependency analysis refuses invalid collected modules" {
    for ([_][]const u8{ "let broken =", "let a = 1\nlet a = 2" }) |source| {
        var module = try Module.init(std.testing.allocator, "invalid.nov", source);
        defer module.deinit();
        const count = module.diagnostics.items.len;
        try std.testing.expectError(error.InvalidModule, analyze(&module));
        try std.testing.expectEqual(count, module.diagnostics.items.len);
    }
}

fn testAllocationFailures(allocator: std.mem.Allocator, source: []const u8) !void {
    var module = try Module.init(allocator, "oom.nov", source);
    defer module.deinit();
    var result = analyze(&module) catch |err| {
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer result.deinit();
    for (result.entries) |entry| try std.testing.expect(entry.state == .resolved or entry.state == .failed);
}

test "dependency ownership and diagnostic rollback survive allocation failures" {
    for ([_][]const u8{
        "",
        "let a = b + b\nlet b = 1",
        "let a = b\nlet b = a",
        "let a = missing\nlet b = a",
        "let a = f()\nlet f: () -> i32 = a",
        "let a: i32",
    }) |source| {
        try std.testing.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}

test "long forward chains use an explicit work stack" {
    var source = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer source.deinit();
    for (0..256) |i| try source.writer.print("let name_{d} = name_{d}\n", .{ i, i + 1 });
    try source.writer.writeAll("let name_256 = 1");
    var module = try Module.init(std.testing.allocator, "chain.nov", source.written());
    defer module.deinit();
    var result = try analyze(&module);
    defer result.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expectEqual(@as(usize, 257), result.order.items.len);
    try std.testing.expectEqual(module.lookup("name_256").?, result.order.items[0]);
    try std.testing.expectEqual(module.lookup("name_0").?, result.order.items[256]);
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source.written()});
}
