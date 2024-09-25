const std = @import("std");
const Ast = @import("Ast.zig");
const Node = Ast.Node;

pub fn main() !void {
    // (* (- 123) (group 45.67))
    // const source = "-123 * (45.67)\n";
    const source =
        \\-123 * (45.67)
        \\+ -123 * (45.67)
        \\
    ;

    const allocator = std.heap.page_allocator;
    var ast = try Ast.parse(allocator, source);
    defer ast.deinit(allocator);

    std.debug.print("Tokens:", .{});
    for (ast.tokens.items(.tag)) |tag| {
        std.debug.print(" {s}", .{@tagName(tag)});
    }
    std.debug.print("\nNodes:", .{});
    for (ast.nodes.items(.tag)) |tag| {
        std.debug.print(" {s}", .{@tagName(tag)});
    }
    std.debug.print("\nErrors:", .{});
    for (ast.errors) |parse_error| {
        try ast.renderError(parse_error, std.io.getStdOut().writer());
    }
    std.debug.print("\n", .{});
    const main_tokens = ast.nodes.items(.main_token);
    const node_tags = ast.nodes.items(.tag);
    const datas = ast.nodes.items(.data);
    std.debug.print("{} {} {} {} {} {} {}\n", .{ main_tokens[1], datas[2].lhs, node_tags[datas[2].lhs], datas[5].lhs, node_tags[datas[5].lhs], datas[0].lhs, node_tags[datas[0].lhs] });
    try @import("render.zig").renderTree(ast, std.io.getStdOut().writer());
    std.debug.print("\n", .{});

    // ast.rootDecls();
    // print(&ast, 0, std.io.getStdOut().writer());

}

// fn print(self: *Ast, index: Node.Index, writer: anytype) void {
//     const node = self.nodes.get(index);
//     if (node.data.lhs != 0) {
//         print(self, node.data.lhs, writer);
//     }
//     writer.writeAll(self.tokens.get(node.main_token).tag.symbol()) catch unreachable;
//     if (node.data.rhs != 0) {
//         print(self, node.data.rhs, writer);
//     }
// }
