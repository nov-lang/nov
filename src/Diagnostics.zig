const std = @import("std");
const Ast = @import("Ast.zig");

pub fn render(ast: Ast, path: []const u8, writer: *std.Io.Writer) !void {
    for (ast.errors) |parse_error| {
        const offset = ast.errorOffset(parse_error);
        const start = ast.tokens.items(.start)[parse_error.token] + offset;
        var line: usize = 1;
        var line_start: usize = 0;
        for (ast.source[0..start], 0..) |byte, i| {
            if (byte == '\n') {
                line += 1;
                line_start = i + 1;
            }
        }
        const line_end = std.mem.indexOfScalarPos(u8, ast.source, start, '\n') orelse ast.source.len;
        const column = start - line_start;
        try writer.print("{s}:{d}:{d}: error: ", .{ path, line, column + 1 });
        try ast.renderError(parse_error, writer);
        try writer.print("\n{s}\n", .{std.mem.trimEnd(u8, ast.source[line_start..line_end], "\r")});
        for (ast.source[line_start..start]) |byte| {
            try writer.writeByte(if (byte == '\t') '\t' else ' ');
        }
        const width = if (offset != 0) 1 else @max(1, @min(ast.tokenSlice(parse_error.token).len, line_end - start));
        try writer.writeByte('^');
        try writer.splatByteAll('~', width - 1);
        try writer.writeByte('\n');
    }
}

test "diagnostics include filename source and caret at EOF" {
    var ast = try @import("Parser.zig").parse(std.testing.allocator, "let broken =");
    defer ast.deinit(std.testing.allocator);
    try std.testing.expect(ast.errors.len > 0);
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try render(ast, "broken.nov", &output.writer);
    try std.testing.expect(std.mem.startsWith(u8, output.written(), "broken.nov:1:13: error: "));
    try std.testing.expect(std.mem.endsWith(u8, output.written(), "\nlet broken =\n            ^\n"));
}

test "diagnostics point after previous token and preserve tabs" {
    var ast = try @import("Parser.zig").parse(std.testing.allocator, "\tlet x = 1 let y = 2\n");
    defer ast.deinit(std.testing.allocator);
    try std.testing.expect(ast.errors.len > 0);
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try render(ast, "tabs.nov", &output.writer);
    try std.testing.expect(std.mem.indexOf(u8, output.written(), "\n\t") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.written(), "^") != null);
}
