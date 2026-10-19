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
            .value_cycle => try writer.print("cyclic value dependency on '{s}'", .{module.sourceSlice(diagnostic.span).?}),
            .unsupported_dependency_expression => try writer.writeAll("dependency analysis for this expression is not implemented yet"),
            .missing_initializer => try writer.writeAll("dependency analysis requires an initializer"),
            .unknown_type => try writer.print("unknown type '{s}'", .{module.sourceSlice(diagnostic.span).?}),
            .unsupported_type => try writer.writeAll("this type is not supported in semantic analysis yet"),
            .not_a_type => try writer.writeAll("expected a type, found a value"),
            .duplicate_parameter => try writer.print("duplicate parameter '{s}'", .{module.sourceSlice(diagnostic.span).?}),
            .invalid_parameter_type => try writer.writeAll("void is not a parameter type"),
            .type_nesting_limit => try writer.writeAll("type annotation nesting exceeds the current analysis limit of 128"),
            .invalid_literal => try writer.writeAll("invalid numeric literal"),
            .unsupported_literal => try writer.writeAll("this literal is not supported in semantic analysis yet"),
            .literal_type_mismatch => try writer.writeAll("literal does not match the annotated type"),
            .literal_out_of_range => try writer.writeAll("integer literal does not fit the annotated type"),
            .unsupported_body_expression => try writer.writeAll("body checking for this expression is not implemented yet"),
            .unsupported_local_declaration => try writer.writeAll("local declarations require an explicitly typed value (mutable void, local function definitions and discards are not supported yet)"),
            .immutable_assignment => try writer.writeAll("assignment requires a mutable local"),
            .unsupported_assignment_target => try writer.writeAll("assignment target must be a direct local identifier"),
            .unreachable_code => try writer.writeAll("unreachable statement after return"),
            .unused_block_value => try writer.writeAll("non-final block expression must return void; discarding values is not supported yet"),
            .unsupported_shadowing => try writer.writeAll("shadowing is not supported in body analysis yet"),
            .expression_type_mismatch => try writer.writeAll("expression does not match the expected type"),
            .operator_not_supported => try writer.writeAll("operator is not supported for this operand type"),
            .literal_needs_type => try writer.writeAll("integer literal needs an expected type (no default chosen)"),
            .not_callable => try writer.writeAll("expression is not callable"),
            .argument_count_mismatch => try writer.writeAll("argument count does not match the function signature"),
            .unresolved_value_type => try writer.writeAll("declaration needs a resolved type before use here"),
            .body_nesting_limit => try writer.writeAll("recursive body checking exceeds the current analysis limit of 128"),
            .unsupported_lowering => try writer.writeAll("lowering this source form is not implemented yet"),
        }
        try location.renderExcerpt(module.ast.source, diagnostic.span.end - diagnostic.span.start, writer);
        const note: ?[]const u8 = switch (diagnostic.tag) {
            .duplicate_name => "first declaration here",
            .value_cycle => "declaration in this cycle",
            .duplicate_parameter => "first parameter here",
            .literal_type_mismatch, .literal_out_of_range => "type annotation here",
            .expression_type_mismatch, .literal_needs_type => "expected type required here",
            .immutable_assignment => "immutable declaration here",
            .unsupported_shadowing => "conflicting declaration here",
            .operator_not_supported => "operand type specified here",
            .argument_count_mismatch, .not_callable => "type specified here",
            .unresolved_value_type => "declaration here",
            else => null,
        };
        if (note) |message| {
            if (diagnostic.related) |related| {
                const original = Location.init(module.ast.source, related.start);
                try original.renderPrefix(module.path, "note", writer);
                try writer.writeAll(message);
                try original.renderExcerpt(module.ast.source, related.end - related.start, writer);
            }
        }
    }
}

pub fn renderBackend(module: *const Module, diagnostic: @import("CEmitter.zig").Diagnostic, writer: *std.Io.Writer) !void {
    const location = Location.init(module.ast.source, diagnostic.span.start);
    try location.renderPrefix(module.path, "error", writer);
    try writer.writeAll(diagnostic.reason.message());
    try location.renderExcerpt(module.ast.source, diagnostic.span.end - diagnostic.span.start, writer);
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

test "module value cycle diagnostics include the target declaration" {
    var module = try Module.init(std.testing.allocator, "cycle.nov", "let first = second\nlet second = first");
    defer module.deinit();
    try std.testing.expect(!module.hasErrors());
    try module.diagnostics.append(module.allocator, .{
        .tag = .value_cycle,
        .span = .{ .start = 12, .end = 18 },
        .related = module.getDeclaration(module.lookup("second").?).?.name_span,
    });
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try renderModule(&module, &output.writer);
    try std.testing.expectEqualStrings(
        "cycle.nov:1:13: error: cyclic value dependency on 'second'\nlet first = second\n            ^~~~~~\n" ++
            "cycle.nov:2:5: note: declaration in this cycle\nlet second = first\n    ^~~~~~\n",
        output.written(),
    );
}

test "module duplicate parameter diagnostics include the first parameter" {
    var module = try Module.init(std.testing.allocator, "parameters.nov", "let value: (x: i32, x: bool)");
    defer module.deinit();
    try std.testing.expect(!module.hasErrors());
    try module.diagnostics.append(module.allocator, .{
        .tag = .duplicate_parameter,
        .span = .{ .start = 20, .end = 21 },
        .related = .{ .start = 12, .end = 13 },
    });
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try renderModule(&module, &output.writer);
    try std.testing.expectEqualStrings(
        "parameters.nov:1:21: error: duplicate parameter 'x'\nlet value: (x: i32, x: bool)\n                    ^\n" ++
            "parameters.nov:1:13: note: first parameter here\nlet value: (x: i32, x: bool)\n            ^\n",
        output.written(),
    );
}

test "module literal diagnostics include the type annotation when present" {
    const Case = struct { tag: Module.Diagnostic.Tag, message: []const u8 };
    for ([_]Case{
        .{ .tag = .literal_out_of_range, .message = "integer literal does not fit the annotated type" },
        .{ .tag = .literal_type_mismatch, .message = "literal does not match the annotated type" },
    }) |case| {
        for ([_]bool{ true, false }) |with_related| {
            var module = try Module.init(std.testing.allocator, "literal.nov", "let value: u8 = 256");
            defer module.deinit();
            try std.testing.expect(!module.hasErrors());
            try module.diagnostics.append(module.allocator, .{
                .tag = case.tag,
                .span = .{ .start = 16, .end = 19 },
                .related = if (with_related) .{ .start = 11, .end = 13 } else null,
            });
            var output = std.Io.Writer.Allocating.init(std.testing.allocator);
            defer output.deinit();
            try renderModule(&module, &output.writer);
            const expected = try std.fmt.allocPrint(
                std.testing.allocator,
                "literal.nov:1:17: error: {s}\nlet value: u8 = 256\n                ^~~\n{s}",
                .{ case.message, if (with_related) "literal.nov:1:12: note: type annotation here\nlet value: u8 = 256\n           ^~\n" else "" },
            );
            defer std.testing.allocator.free(expected);
            try std.testing.expectEqualStrings(expected, output.written());
        }
    }
}

test "module type and literal diagnostics render messages and source excerpts" {
    const Case = struct {
        tag: Module.Diagnostic.Tag,
        token: []const u8,
        underline: []const u8,
        message: []const u8,
    };
    for ([_]Case{
        .{ .tag = .unknown_type, .token = "Missing", .underline = "^~~~~~~", .message = "unknown type 'Missing'" },
        .{ .tag = .unsupported_type, .token = "i32", .underline = "^~~", .message = "this type is not supported in semantic analysis yet" },
        .{ .tag = .not_a_type, .token = "value", .underline = "^~~~~", .message = "expected a type, found a value" },
        .{ .tag = .invalid_parameter_type, .token = "void", .underline = "^~~~", .message = "void is not a parameter type" },
        .{ .tag = .type_nesting_limit, .token = "i32", .underline = "^~~", .message = "type annotation nesting exceeds the current analysis limit of 128" },
        .{ .tag = .invalid_literal, .token = "123", .underline = "^~~", .message = "invalid numeric literal" },
        .{ .tag = .unsupported_literal, .token = "1.5", .underline = "^~~", .message = "this literal is not supported in semantic analysis yet" },
    }) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let value = {s}", .{case.token});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "semantic.nov", source);
        defer module.deinit();
        try std.testing.expect(!module.hasErrors());
        try module.diagnostics.append(module.allocator, .{
            .tag = case.tag,
            .span = .{ .start = 12, .end = @intCast(source.len) },
        });
        var output = std.Io.Writer.Allocating.init(std.testing.allocator);
        defer output.deinit();
        try renderModule(&module, &output.writer);
        const expected = try std.fmt.allocPrint(
            std.testing.allocator,
            "semantic.nov:1:13: error: {s}\n{s}\n            {s}\n",
            .{ case.message, source, case.underline },
        );
        defer std.testing.allocator.free(expected);
        try std.testing.expectEqualStrings(expected, output.written());
    }
}

test "module body diagnostics render messages and optional related notes" {
    const Case = struct {
        tag: Module.Diagnostic.Tag,
        message: []const u8,
        note: ?[]const u8 = null,
    };
    for ([_]Case{
        .{ .tag = .expression_type_mismatch, .message = "expression does not match the expected type", .note = "expected type required here" },
        .{ .tag = .argument_count_mismatch, .message = "argument count does not match the function signature", .note = "type specified here" },
        .{ .tag = .unsupported_body_expression, .message = "body checking for this expression is not implemented yet" },
        .{ .tag = .unsupported_local_declaration, .message = "local declarations require an explicitly typed value (mutable void, local function definitions and discards are not supported yet)" },
        .{ .tag = .immutable_assignment, .message = "assignment requires a mutable local", .note = "immutable declaration here" },
        .{ .tag = .unsupported_assignment_target, .message = "assignment target must be a direct local identifier" },
        .{ .tag = .unused_block_value, .message = "non-final block expression must return void; discarding values is not supported yet" },
        .{ .tag = .unsupported_shadowing, .message = "shadowing is not supported in body analysis yet", .note = "conflicting declaration here" },
        .{ .tag = .operator_not_supported, .message = "operator is not supported for this operand type", .note = "operand type specified here" },
        .{ .tag = .literal_needs_type, .message = "integer literal needs an expected type (no default chosen)", .note = "expected type required here" },
        .{ .tag = .not_callable, .message = "expression is not callable", .note = "type specified here" },
        .{ .tag = .unresolved_value_type, .message = "declaration needs a resolved type before use here", .note = "declaration here" },
        .{ .tag = .body_nesting_limit, .message = "recursive body checking exceeds the current analysis limit of 128" },
        .{ .tag = .unsupported_lowering, .message = "lowering this source form is not implemented yet" },
    }) |case| {
        for ([_]bool{ true, false }) |with_related| {
            var module = try Module.init(std.testing.allocator, "body.nov", "let value: i32 = 123");
            defer module.deinit();
            try std.testing.expect(!module.hasErrors());
            try module.diagnostics.append(module.allocator, .{
                .tag = case.tag,
                .span = .{ .start = 17, .end = 20 },
                .related = if (with_related) .{ .start = 11, .end = 14 } else null,
            });
            var output = std.Io.Writer.Allocating.init(std.testing.allocator);
            defer output.deinit();
            try renderModule(&module, &output.writer);
            var expected = std.Io.Writer.Allocating.init(std.testing.allocator);
            defer expected.deinit();
            try expected.writer.print(
                "body.nov:1:18: error: {s}\nlet value: i32 = 123\n                 ^~~\n",
                .{case.message},
            );
            if (with_related) {
                if (case.note) |note| {
                    try expected.writer.print(
                        "body.nov:1:12: note: {s}\nlet value: i32 = 123\n           ^~~\n",
                        .{note},
                    );
                }
            }
            try std.testing.expectEqualStrings(expected.written(), output.written());
        }
    }
}

test "module unsupported dependency expressions render diagnostics" {
    var module = try Module.init(std.testing.allocator, "expression.nov", "let value = value()");
    defer module.deinit();
    try std.testing.expect(!module.hasErrors());
    try module.diagnostics.append(module.allocator, .{
        .tag = .unsupported_dependency_expression,
        .span = .{ .start = 12, .end = 19 },
    });
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try renderModule(&module, &output.writer);
    try std.testing.expectEqualStrings(
        "expression.nov:1:13: error: dependency analysis for this expression is not implemented yet\nlet value = value()\n            ^~~~~~~\n",
        output.written(),
    );
}

test "module missing initializers render diagnostics" {
    var module = try Module.init(std.testing.allocator, "initializer.nov", "let value: i32");
    defer module.deinit();
    try std.testing.expect(!module.hasErrors());
    try module.diagnostics.append(module.allocator, .{
        .tag = .missing_initializer,
        .span = module.getDeclaration(module.lookup("value").?).?.name_span,
    });
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try renderModule(&module, &output.writer);
    try std.testing.expectEqualStrings(
        "initializer.nov:1:5: error: dependency analysis requires an initializer\nlet value: i32\n    ^~~~~\n",
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
