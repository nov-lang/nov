const std = @import("std");
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const _value = @import("value.zig");
const isPrimitive = @import("primitives.zig").isPrimitive;

const Codegen = @This();

ast: *const Ast,
allocator: std.mem.Allocator,
arena: *std.heap.ArenaAllocator.State,

pub fn generate(allocator: std.mem.Allocator, ast: Ast) !void {
    var arena: std.heap.ArenaAllocator.State = .{};
    defer arena.promote(allocator).deinit();

    var codegen: Codegen = .{
        .allocator = allocator,
        .arena = arena,
        .ast = ast,
    };

    try codegen.generateNode(0);
}

const Nodegen = *const fn (*Codegen, Node.Index) anyerror!void;
const generators = std.enums.directEnumArray(Node.Tag, Nodegen, 0, .{
    // .block = generateBlock,
});

fn generateNode(self: *Codegen, node: Node.Index) !void {
    // synchronize

    if (generators[@intFromEnum(self.ast.nodes.items(.tag)[node])]) |gen| {
        return gen(self, node);
    }
}
