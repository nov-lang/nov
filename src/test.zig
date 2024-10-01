const std = @import("std");
const Parser = @import("Parser.zig");
const Ast = @import("Ast.zig");
const Node = Ast.Node;

pub fn main() !void {
    // (* (- 123) (group 45.67))
    // const source = "-123 * (45.67)\n";
    const source =
        \\let x = 3
        \\let priv mut y = 4
        \\
    ;
    // const source =
    //     // \\let x: int = 3
    //     // \\let mut y = "salut"
    //     // \\ x + y x - y
    //     // \\{x + y
    //     \\{
    //     \\    let x = {
    //     \\        let y = 3
    //     \\        y
    //     \\    }
    //     \\}
    //     \\let x =
    //     \\x + y
    //     \\
    // ;
    // const source =
    //     \\let cond = true
    //     \\if (cond) {
    //     \\    let mut x = "outer"
    //     \\    {
    //     \\        x = (x + " ") * 3
    //     \\        x *= 2
    //     \\    }
    //     \\    print(x)
    //     \\} else {
    //     \\    print("Hello world!")
    //     \\}
    //     \\
    //     \\let mut i: int = 0
    //     \\loop {
    //     \\    print(i)
    //     \\    i += 1
    //     \\    if (i == 10) {
    //     \\        break
    //     \\    }
    //     \\}
    //     \\
    // ;

    // std.log.debug("Running parser with source: \n{s}\n", .{source});

    const allocator = std.heap.page_allocator;
    var ast = try Parser.parse(allocator, source);
    defer ast.deinit(allocator);

    // for (ast.rootDecls()) |decl| {
    //     // std.debug.print("{} {}\n", .{decl, ast.nodes.items(.tag)[decl]});
    //     for (ast.firstToken(decl)..ast.lastToken(decl) + 1) |token| {
    //         std.debug.print("{s} ", .{ast.tokenSlice(@intCast(token))});
    //     }
    //     std.debug.print("\n", .{});
    // }

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
        std.debug.print("\n", .{});
        const loc = ast.tokenLocation(0, parse_error.token);
        std.debug.print("{d}:{d}: {s}: ", .{
            loc.line,
            loc.column,
            @tagName(ast.tokens.items(.tag)[parse_error.token]),
        });
        try ast.renderError(parse_error, std.io.getStdOut().writer());
    }
    std.debug.print("\n", .{});

    // _ = ast.firstToken(0);
    // _ = ast.lastToken(0);

    // const main_tokens = ast.nodes.items(.main_token);
    // const node_tags = ast.nodes.items(.tag);
    // const datas = ast.nodes.items(.data);
    // std.debug.print("{} {} {} {} {} {} {}\n", .{ main_tokens[1], datas[2].lhs, node_tags[datas[2].lhs], datas[5].lhs, node_tags[datas[5].lhs], datas[0].lhs, node_tags[datas[0].lhs] });

    // var buffer = std.ArrayList(u8).init(allocator);
    // defer buffer.deinit();
    // try @import("render.zig").renderTree(&buffer, ast, .{});
    // std.debug.print("{s}\n", .{buffer.items});

}
