const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");
const Ast = @import("Ast.zig");
const Sema = @import("Sema.zig");

pub fn main() !u8 {
    const source =
        // \\let add_neg = (a: int, b: int) -> int {
        // \\    let sum = a + b
        // \\    -sum
        // \\}
        // \\let x = MyStruct{
        // \\    .a = 1,
        // \\    .b = 2,
        // \\}
        \\let a = 1 + 2 + 3 + 4
        \\let a = 1 + 2 +
        \\    3 + 4
        \\let a = 1 + 2
        \\    + 3 + 4
        \\let a = (
        \\    1 + 2 + 3
        \\    + 4
        \\)
        \\let s = (
        // \\     if a == 3 { "true" } else { "false" } +
        \\    "this is my string\n" +
        \\    "this is on a new line\n" +
        \\
        \\    "yes"
        \\)
        // \\
        // \\let sum = a + b
        // \\-sum
        \\
    ;
    const neighbours =
        1 + 2 + 3 + 4;
    _ = neighbours;
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

    // try dumpAst(ast);
    // var ast = try readAst(allocator);
    // defer ast.deinit(allocator);

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
        return 1;
    }

    // const nir = try Sema.generate(allocator, ast);
    // for (0..nir.instructions.len) |i| {
    //     std.debug.print("Instruction {}: {}\n", .{ i, nir.instructions.get(i) });
    // }

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

    return 0;
}

fn readAst(allocator: std.mem.Allocator) !Ast {
    var f = try std.fs.cwd().openFile("ast.json", .{});
    defer f.close();
    var br = std.io.bufferedReader(f.reader());

    const AstJson = struct {
        source: []u8,
        token_tags: []Tokenizer.Token.Tag,
        token_starts: []Tokenizer.ByteOffset,
        node_tags: []Ast.Node.Tag,
        node_tokens: []Ast.TokenIndex,
        node_data: []Ast.Node.Data,
        extra_data: []Ast.Node.Index,
    };

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var jr = std.json.reader(arena.allocator(), br.reader());
    const ast_json = try std.json.parseFromTokenSourceLeaky(
        AstJson,
        arena.allocator(),
        &jr,
        .{ .allocate = .alloc_if_needed },
    );

    var tokens: Ast.TokenList = .{};
    try tokens.ensureTotalCapacity(allocator, ast_json.token_tags.len);
    for (ast_json.token_tags, ast_json.token_starts) |tag, start| {
        tokens.appendAssumeCapacity(.{ .tag = tag, .start = start });
    }

    var nodes: Ast.NodeList = .{};
    try nodes.ensureTotalCapacity(allocator, ast_json.node_tags.len);
    for (ast_json.node_tags, ast_json.node_tokens, ast_json.node_data) |tag, main_token, data| {
        nodes.appendAssumeCapacity(.{ .tag = tag, .main_token = main_token, .data = data });
    }

    return Ast{
        .source = try allocator.dupeZ(u8, ast_json.source),
        .tokens = tokens.toOwnedSlice(),
        .nodes = nodes.toOwnedSlice(),
        .extra_data = try allocator.dupe(u32, ast_json.extra_data),
        .errors = &[0]Ast.Error{},
    };
}

fn dumpAst(ast: Ast) !void {
    var f = try std.fs.cwd().createFile("ast.json", .{});
    defer f.close();
    var bw = std.io.bufferedWriter(f.writer());
    defer bw.flush() catch {};
    const writer = bw.writer();

    try writer.writeAll("{");
    try writer.writeAll("\"source\":");
    try std.json.stringify(ast.source, .{ .emit_strings_as_arrays = true }, writer);
    try writer.writeAll(",\"token_tags\":");
    try std.json.stringify(ast.tokens.items(.tag), .{}, writer);
    try writer.writeAll(",\"token_starts\":");
    try std.json.stringify(ast.tokens.items(.start), .{}, writer);
    try writer.writeAll(",\"node_tags\":");
    try std.json.stringify(ast.nodes.items(.tag), .{}, writer);
    try writer.writeAll(",\"node_tokens\":");
    try std.json.stringify(ast.nodes.items(.main_token), .{}, writer);
    try writer.writeAll(",\"node_data\":");
    try std.json.stringify(ast.nodes.items(.data), .{}, writer);
    try writer.writeAll(",\"extra_data\":");
    try std.json.stringify(ast.extra_data, .{}, writer);
    try writer.writeAll("}");
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
