const std = @import("std");
const Ast = @import("Ast.zig");
const Tokenizer = @import("Scanner.zig");
const Parser = @import("Parser.zig");
const Node = Ast.Node;

pub fn main() !void {
    const source = "-123 * (45.67)";

    const ast = try Parser.parse(std.heap.page_allocator, source);
    std.debug.print("{}\n", .{ast});
    // for (ast.tokens.items) |token| {
    //     std.debug.print("{}\n", .{token});
    // }
}
