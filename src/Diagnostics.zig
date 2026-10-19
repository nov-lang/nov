const std = @import("std");
const Ast = @import("Ast.zig");
const Module = @import("Module.zig");

const Location = struct {
    start: usize,
    line: usize = 1,
    line_start: usize = 0,
    line_end: usize,

    fn init(source: []const u8, start: usize) Location {
        var location: Location = .{
            .start = start,
            .line_end = std.mem.indexOfScalarPos(u8, source, start, '\n') orelse source.len,
        };
        for (source[0..start], 0..) |byte, i| {
            if (byte == '\n') {
                location.line += 1;
                location.line_start = i + 1;
            }
        }
        return location;
    }

    fn renderPrefix(location: Location, path: []const u8, kind: []const u8, writer: *std.Io.Writer) !void {
        try writer.print("{s}:{d}:{d}: {s}: ", .{ path, location.line, location.start - location.line_start + 1, kind });
    }

    fn renderExcerpt(location: Location, source: []const u8, length: usize, writer: *std.Io.Writer) !void {
        try writer.print("\n{s}\n", .{std.mem.trimEnd(u8, source[location.line_start..location.line_end], "\r")});
        for (source[location.line_start..location.start]) |byte| {
            try writer.writeByte(if (byte == '\t') '\t' else ' ');
        }
        const width = @max(1, @min(length, location.line_end - location.start));
        try writer.writeByte('^');
        try writer.splatByteAll('~', width - 1);
        try writer.writeByte('\n');
    }
};

pub fn renderModule(module: *const Module, writer: *std.Io.Writer) !void {
    try render(module.ast, module.path, writer);
    for (module.diagnostics.items) |diagnostic| {
        const location = Location.init(module.ast.source, diagnostic.span.start);
        try location.renderPrefix(module.path, "error", writer);
        switch (diagnostic.tag) {
            .duplicate_name => try writer.print("duplicate declaration '{s}'", .{module.sourceSlice(diagnostic.span).?}),
            .unknown_name => try writer.print("unknown name '{s}'", .{module.sourceSlice(diagnostic.span).?}),
            .unsupported_declaration => try writer.writeAll("declaration attributes are not supported in semantic analysis yet"),
            .unsupported_name => try writer.writeAll("quoted identifiers are not supported in semantic analysis yet"),
        }
        try location.renderExcerpt(module.ast.source, diagnostic.span.end - diagnostic.span.start, writer);
        if (diagnostic.tag == .duplicate_name) {
            if (diagnostic.related) |related| {
                const original = Location.init(module.ast.source, related.start);
                try original.renderPrefix(module.path, "note", writer);
                try writer.writeAll("first declaration here");
                try original.renderExcerpt(module.ast.source, related.end - related.start, writer);
            }
        }
    }
}

pub fn render(ast: Ast, path: []const u8, writer: *std.Io.Writer) !void {
    for (ast.errors) |parse_error| {
        const offset = ast.errorOffset(parse_error);
        const start = ast.tokens.items(.start)[parse_error.token] + offset;
        const location = Location.init(ast.source, start);
        try location.renderPrefix(path, "error", writer);
        try ast.renderError(parse_error, writer);
        try location.renderExcerpt(ast.source, if (offset != 0) 1 else ast.tokenSlice(parse_error.token).len, writer);
    }
}

test "module duplicate diagnostics include the first binding with tabs and CRLF" {
    var module = try Module.init(std.testing.allocator, "duplicate.nov", "let text = \"é\"\r\n\tlet name = 1\r\n\tlet name = 2");
    defer module.deinit();
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try renderModule(&module, &output.writer);
    try std.testing.expectEqualStrings(
        "duplicate.nov:3:6: error: duplicate declaration 'name'\n\tlet name = 2\n\t    ^~~~\n" ++
            "duplicate.nov:2:6: note: first declaration here\n\tlet name = 1\n\t    ^~~~\n",
        output.written(),
    );
}

test "module unknown names render only after explicit resolution" {
    var module = try Module.init(std.testing.allocator, "unknown.nov", "let value = absent");
    defer module.deinit();
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try renderModule(&module, &output.writer);
    try std.testing.expectEqualStrings("", output.written());
    try std.testing.expectEqual(@as(?Module.DeclId, null), try module.resolveName(.{ .start = 12, .end = 18 }));
    try renderModule(&module, &output.writer);
    try std.testing.expectEqualStrings(
        "unknown.nov:1:13: error: unknown name 'absent'\nlet value = absent\n            ^~~~~~\n",
        output.written(),
    );
}

test "module unsupported attributes and quoted names render semantic diagnostics" {
    var module = try Module.init(std.testing.allocator, "unsupported.nov", "@[export]\nlet decorated = 1\nlet @\"quoted\" = 2");
    defer module.deinit();
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try renderModule(&module, &output.writer);
    try std.testing.expectEqualStrings(
        "unsupported.nov:1:1: error: declaration attributes are not supported in semantic analysis yet\n@[export]\n^~\n" ++
            "unsupported.nov:3:5: error: quoted identifiers are not supported in semantic analysis yet\nlet @\"quoted\" = 2\n    ^~~~~~~~~\n",
        output.written(),
    );
}

test "syntax-only module rendering matches parser rendering" {
    for ([_][]const u8{
        "let broken =",
        "let f: () = {\r\n\touter: loop {}\r\n}\r\n",
        "\tlet x = 1 let y = 2\n",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "syntax.nov", source);
        defer module.deinit();
        try std.testing.expect(module.ast.errors.len > 0);
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        var expected = std.Io.Writer.Allocating.init(std.testing.allocator);
        defer expected.deinit();
        try render(module.ast, module.path, &expected.writer);
        var output = std.Io.Writer.Allocating.init(std.testing.allocator);
        defer output.deinit();
        try renderModule(&module, &output.writer);
        try std.testing.expectEqualStrings(expected.written(), output.written());
    }
}

test "unsupported loop diagnostics identify the keyword with tabs and CRLF" {
    var ast = try @import("Parser.zig").parse(std.testing.allocator, "let f: () = {\r\n\touter: loop {}\r\n}\r\n");
    defer ast.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 1), ast.errors.len);
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try render(ast, "loop.nov", &output.writer);
    try std.testing.expectEqualStrings(
        "loop.nov:2:9: error: loop expressions are not implemented yet\n\touter: loop {}\n\t       ^~~~\n",
        output.written(),
    );
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
