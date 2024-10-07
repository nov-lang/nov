const std = @import("std");
const Parser = @import("Parser.zig");
const Value = @import("Value.zig");
const Sema = @import("Sema.zig");

pub fn main() !void {
    const source = "0 + 1 + 1 + 0 + 3\n";
    // const source = "-123 * (45.67)\n";
    // const source =
    //     \\let x = -3
    //     \\let pub mut y: int = 4
    //     \\let z = blk: {
    //     \\    let a = 3
    //     \\    if a == 3 {
    //     \\        break :blk 3 + 4
    //     \\    } else {
    //     \\        3 + 4
    //     \\    }
    //     \\    x + y
    //     \\}
    //     \\{}
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

    std.debug.print(
        \\Running parser with source:
        \\```
        \\{s}
        \\```
        \\
    , .{source});

    const allocator = std.heap.page_allocator;
    var ast = try Parser.parse(allocator, source);
    defer ast.deinit(allocator);

    for (ast.rootStmts()) |stmt| {
        for (ast.firstToken(stmt)..ast.lastToken(stmt) + 1) |token| {
            std.debug.print("{s} ", .{ast.tokenSlice(@intCast(token))});
        }
        std.debug.print("\n", .{});
    }

    // std.debug.print("Tokens:", .{});
    // for (ast.tokens.items(.tag)) |tag| {
    //     std.debug.print(" {s}", .{@tagName(tag)});
    // }

    // std.debug.print("\nNodes:", .{});
    // for (ast.nodes.items(.tag)) |tag| {
    //     std.debug.print(" {s}", .{@tagName(tag)});
    // }

    if (ast.errors.len > 0) {
        const stderr = std.io.getStdErr().writer();
        try stderr.writeAll("\n");
        for (ast.errors) |parse_error| {
            const loc = ast.tokenLocation(0, parse_error.token);
            try stderr.print(Color.bold ++ "{s}:{d}:{d}: " ++
                Color.red.toSeq() ++ "error: " ++ Color.reset ++ Color.bold, .{
                "source",
                loc.line,
                loc.column,
            });
            try ast.renderError(parse_error, stderr);
            try stderr.print(Color.reset ++ "\n{s}\n", .{ast.source[loc.line_start..loc.line_end]});
            try stderr.writeByteNTimes(' ', loc.column - 1);
            try stderr.writeAll(Color.green.toSeq());
            switch (ast.tokenSlice(parse_error.token).len) {
                1 => try stderr.writeAll("^"),
                else => |len| try stderr.writeByteNTimes('~', len),
            }
            try stderr.writeAll("\n" ++ Color.reset);
        }
        std.process.exit(1);
    }

    const nir = try Sema.generate(allocator, ast);
    for (0..nir.instructions.len) |i| {
        std.debug.print("Instruction {}: {}\n", .{ i, nir.instructions.get(i) });
    }

    // const nir = try Sema.generate(allocator, &ast);
    // for (Sema.root) |ref| {
    //     switch (ref) {
    //         .type => unreachable,
    //         .value => unreachable,
    //         .index => {
    //             std.debug.print("Index: {}: ", .{ref.index});
    //             std.debug.print("{}\n", .{nir.instructions.items(.tag)[ref.index]});
    //         },
    //     }
    // }

    // const s = try Value.String.init(allocator, "Hello, world!");
    // std.debug.print("{}\n", .{s.value()});
    // const n = Value.create(128);
    // std.debug.print("{}\n", .{Value.eql(.uint, n, Value.create(128))});
    // std.debug.print("Value: {}\n", .{n});

    // var x = try Value.BigInt.init(allocator);
    // x.mutable.set(123);
    // std.debug.print("{obj(b)}\n", .{x.value()});
}

const Color = enum(u8) {
    black = 30,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    default,
    bright_black = 90,
    bright_red,
    bright_green,
    bright_yellow,
    bright_blue,
    bright_magenta,
    bright_cyan,
    bright_white,

    const csi = "\x1b[";
    const reset = csi ++ "0m";
    const bold = csi ++ "1m";

    fn toSeq(comptime fg: Color) []const u8 {
        return comptime csi ++ std.fmt.digits2(@intFromEnum(fg)) ++ "m";
    }
};
