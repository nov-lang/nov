// Originally based on https://github.com/ziglang/zig/blob/master/lib/std/zig/Ast.zig
// See https://github.com/ziglang/zig/blob/master/LICENSE for additional LICENSE details

const std = @import("std");
const Render = @import("Render.zig");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;
const assert = std.debug.assert;

const Ast = @This();

source: [:0]const u8,
tokens: TokenList.Slice,
nodes: NodeList.Slice,
extra_data: []Node.Index,
errors: []const Error,

pub const TokenIndex = u32;
pub const TokenList = std.MultiArrayList(struct {
    tag: Token.Tag,
    start: Tokenizer.ByteOffset,
});
pub const NodeList = std.MultiArrayList(Node);

pub fn deinit(self: *Ast, allocator: std.mem.Allocator) void {
    self.tokens.deinit(allocator);
    self.nodes.deinit(allocator);
    allocator.free(self.extra_data);
    allocator.free(self.errors);
    self.* = undefined;
}

pub fn render(self: Ast, allocator: std.mem.Allocator) Render.Error![]u8 {
    var buffer = std.Io.Writer.Allocating.init(allocator);
    defer buffer.deinit();
    try Render.renderTree(&buffer, self, .{});
    return buffer.toOwnedSlice();
}

pub fn renderToArrayList(
    self: Ast,
    allocator: std.mem.Allocator,
    buffer: *std.ArrayList(u8),
    fixups: Render.Fixups,
) Render.Error!void {
    var writer = std.Io.Writer.Allocating.fromArrayList(allocator, buffer);
    defer buffer.* = writer.toArrayList();
    return Render.renderTree(&writer, self, fixups);
}

test "function prototype accessors expose parameters and return types" {
    const Case = struct {
        source: [:0]const u8,
        names: []const ?[]const u8,
        types: []const []const u8,
        return_type: ?[]const u8,
    };
    const cases = [_]Case{
        .{ .source = "let f: () = {}", .names = &.{}, .types = &.{}, .return_type = null },
        .{ .source = "let f: (x: int) -> int = x", .names = &.{"x"}, .types = &.{"int"}, .return_type = "int" },
        .{ .source = "let f: (int) = {}", .names = &.{null}, .types = &.{"int"}, .return_type = null },
        .{ .source = "let f: (a: int, bool, b: *mut int) -> bool = true", .names = &.{ "a", null, "b" }, .types = &.{ "int", "bool", "*" }, .return_type = "bool" },
        .{ .source = "let f: (\n a: int,\n b: bool,\n) = {}", .names = &.{ "a", "b" }, .types = &.{ "int", "bool" }, .return_type = null },
        .{ .source = "let f: (callback: (x: int) -> int) = {}", .names = &.{"callback"}, .types = &.{"("}, .return_type = null },
    };
    for (cases) |case| {
        var tree = try Parser.parse(std.testing.allocator, case.source);
        defer tree.deinit(std.testing.allocator);
        try std.testing.expectEqual(@as(usize, 0), tree.errors.len);
        const decl_node = tree.rootDecls()[0];
        const declaration = tree.decl(decl_node);
        const proto_node = declaration.ast.type_node;
        const proto = tree.fullFnProto(proto_node).?;
        try std.testing.expectEqual(proto_node, proto.ast.proto_node);
        try std.testing.expectEqual(tree.nodes.items(.main_token)[proto_node], proto.lparen);
        try std.testing.expectEqualStrings("(", tree.tokenSlice(proto.lparen));
        try std.testing.expectEqual(case.types.len, proto.ast.params.len);
        if (case.types.len == 1) {
            try std.testing.expectEqual(@intFromPtr(&tree.nodes.items(.data)[proto_node].lhs), @intFromPtr(proto.ast.params.ptr));
        }
        if (case.return_type) |expected| {
            try std.testing.expectEqualStrings(expected, tree.tokenSlice(tree.firstToken(proto.ast.return_type.?)));
        } else {
            try std.testing.expectEqual(@as(?Node.Index, null), proto.ast.return_type);
        }
        var iterator = proto.iterate(&tree);
        for (case.types, case.names, 0..) |expected_type, expected_name, i| {
            const param = iterator.next().?;
            try std.testing.expectEqual(proto.ast.params[i], param.type_expr);
            try std.testing.expectEqualStrings(expected_type, tree.tokenSlice(tree.firstToken(param.type_expr)));
            if (expected_name) |name| {
                try std.testing.expectEqualStrings(name, tree.tokenSlice(param.name_token.?));
            } else {
                try std.testing.expectEqual(@as(?TokenIndex, null), param.name_token);
            }
        }
        try std.testing.expect(iterator.next() == null);
        try std.testing.expect(iterator.next() == null);
        try std.testing.expect(tree.fullFnProto(decl_node) == null);
        try std.testing.expect(tree.fullFnProto(declaration.ast.init_node) == null);
    }
}

test "call accessors expose targets and AST-owned arguments" {
    const sources = [_][:0]const u8{
        "let value = f()",
        "let value = f(1)",
        "let value = f(1, 2)",
        "let value = f(\n1,\n2,\n3,\n)",
    };
    for (sources, 0..) |source, count| {
        var tree = try Parser.parse(std.testing.allocator, source);
        defer tree.deinit(std.testing.allocator);
        try std.testing.expectEqual(@as(usize, 0), tree.errors.len);
        const decl_node = tree.rootDecls()[0];
        const call_node = tree.decl(decl_node).ast.init_node;
        const call = tree.fullCall(call_node).?;
        try std.testing.expectEqual(count, call.ast.args.len);
        try std.testing.expectEqualStrings("(", tree.tokenSlice(call.ast.lparen));
        try std.testing.expectEqualStrings("f", tree.tokenSlice(tree.firstToken(call.ast.fn_expr)));
        for (call.ast.args, 0..) |arg, i| {
            try std.testing.expectEqualStrings((&[_][]const u8{ "1", "2", "3" })[i], tree.tokenSlice(tree.firstToken(arg)));
        }
        if (count == 1) {
            try std.testing.expectEqual(@intFromPtr(&tree.nodes.items(.data)[call_node].rhs), @intFromPtr(call.ast.args.ptr));
        }
        try std.testing.expect(tree.fullCall(decl_node) == null);
        try std.testing.expect(tree.fullCall(call.ast.fn_expr) == null);
    }
}

test "nested call views retain independent argument slices" {
    var tree = try Parser.parse(std.testing.allocator, "let value = outer(inner(1), empty())");
    defer tree.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 0), tree.errors.len);
    const outer = tree.fullCall(tree.decl(tree.rootDecls()[0]).ast.init_node).?;
    const inner = tree.fullCall(outer.ast.args[0]).?;
    const empty = tree.fullCall(outer.ast.args[1]).?;
    try std.testing.expectEqual(@as(usize, 2), outer.ast.args.len);
    try std.testing.expectEqual(@as(usize, 1), inner.ast.args.len);
    try std.testing.expectEqual(@as(usize, 0), empty.ast.args.len);
    try std.testing.expectEqualStrings("outer", tree.tokenSlice(tree.firstToken(outer.ast.fn_expr)));
    try std.testing.expectEqualStrings("inner", tree.tokenSlice(tree.firstToken(inner.ast.fn_expr)));
    try std.testing.expectEqualStrings("empty", tree.tokenSlice(tree.firstToken(empty.ast.fn_expr)));
    try std.testing.expectEqualStrings("1", tree.tokenSlice(tree.firstToken(inner.ast.args[0])));
}

test "expression token boundaries follow delimiters rather than newline counts" {
    const expressions = [_][:0]const u8{
        "[]",                "[1]",    "[1, 2,]",  "[1, 2, 3,\n]",    "[[1], [2, 3],]",       "a[0]",                              "a[-1]",                         "make()[0]",               "a[call(0)]",
        "{}",                "{\n\n}", "{1}",      "{1\n}",           "{\n1\n\n; tail\n}",    "{1\n2}",                            "{1\n2\n3\n\n}",                 "{{1}\n{2}\n3}",           "named: {1}",
        "{\r\n\t1\r\n\r\n}", "f()",    "f(\n\n)",  "f(1)",            "f(1,)",                "f(\n1\n)",                          "f(1, 2)",                       "f(1, 2,\n\n)",            "f(g(1), h(2, 3))",
        "()",                "(\n\n)", "(x: int)", "(x: int,\n\n)",   "(x: int, y: bool,\n)", "(callback: (x: int) -> int,\n)",    "() -> int",                     "(x: int, y: int) -> int", "1 + f(2, 3)",
        "{1} * 3",           "-f(1)",  "#f(1)",    "f(\"(\", \")\")", "{\"}\"}",              "f(1, ; ) ignored in comment\n2\n)", "{; } ignored in comment\n1\n}",
    };
    for (expressions) |expression| {
        errdefer std.debug.print("expression span case: {s}\n", .{expression});
        const source = try std.fmt.allocPrintSentinel(std.testing.allocator, "let value = {s}\nlet next = 0\n", .{expression}, 0);
        defer std.testing.allocator.free(source);
        var tree = try Parser.parse(std.testing.allocator, source);
        defer tree.deinit(std.testing.allocator);
        try std.testing.expectEqual(@as(usize, 0), tree.errors.len);
        const node = tree.decl(tree.rootDecls()[0]).ast.init_node;
        const first = tree.firstToken(node);
        const last = tree.lastToken(node);
        const starts = tree.tokens.items(.start);
        const end = starts[last] + tree.tokenSlice(last).len;
        try std.testing.expectEqualStrings(expression, tree.source[starts[first]..end]);
        const span = tree.nodeToSpan(node);
        try std.testing.expect(span.start <= span.main);
        try std.testing.expect(span.main < span.end);
        try std.testing.expect(std.mem.indexOfScalar(u8, tree.source[span.start..span.end], '\n') == null);
        if (std.mem.indexOfScalar(u8, expression, '\n') == null) {
            try std.testing.expectEqualStrings(expression, tree.source[span.start..span.end]);
        }
    }
}

test "block accessors expose ordered statements without assigning result semantics" {
    const Case = struct { source: [:0]const u8, count: usize };
    const cases = [_]Case{
        .{ .source = "let value = {}", .count = 0 },
        .{ .source = "let value = {\n\n}", .count = 0 },
        .{ .source = "let value = {1}", .count = 1 },
        .{ .source = "let value = {1\n2}", .count = 2 },
        .{ .source = "let value = {1\n2\n3}", .count = 3 },
        .{ .source = "let f: () = {\nlet x = 1\nlet y = 2\nx + y\n}", .count = 3 },
    };
    for (cases) |case| {
        var tree = try Parser.parse(std.testing.allocator, case.source);
        defer tree.deinit(std.testing.allocator);
        try std.testing.expectEqual(@as(usize, 0), tree.errors.len);
        const decl_node = tree.rootDecls()[0];
        const node = tree.decl(decl_node).ast.init_node;
        var buffer: [2]Node.Index = undefined;
        const block = tree.fullBlock(&buffer, node).?;
        try std.testing.expectEqual(case.count, block.ast.statements.len);
        try std.testing.expectEqualStrings("{", tree.tokenSlice(block.ast.lbrace));
        try std.testing.expectEqualStrings("}", tree.tokenSlice(block.ast.rbrace));
        var previous = block.ast.lbrace;
        for (block.ast.statements) |statement| {
            const first = tree.firstToken(statement);
            const last = tree.lastToken(statement);
            try std.testing.expect(previous < first);
            try std.testing.expect(first <= last);
            try std.testing.expect(last < block.ast.rbrace);
            previous = last;
        }
        try std.testing.expect(tree.fullBlock(&buffer, decl_node) == null);
    }
}

test "recovered blocks retain statement access and closing token spans" {
    var tree = try Parser.parse(std.testing.allocator, "let value = {\nlet = 0\nlet valid = 1\nvalid\n\n}\nlet next = 0");
    defer tree.deinit(std.testing.allocator);
    try std.testing.expect(tree.errors.len != 0);
    try std.testing.expectEqual(@as(usize, 2), tree.rootDecls().len);
    var buffer: [2]Node.Index = undefined;
    const node = tree.decl(tree.rootDecls()[0]).ast.init_node;
    const block = tree.fullBlock(&buffer, node).?;
    try std.testing.expectEqual(@as(usize, 2), block.ast.statements.len);
    try std.testing.expectEqualStrings("}", tree.tokenSlice(tree.lastToken(node)));
    try std.testing.expectEqualStrings("valid", tree.tokenSlice(tree.firstToken(block.ast.statements[1])));
    const span = tree.nodeToSpan(node);
    try std.testing.expectEqualStrings("{", tree.source[span.start..span.end]);
}

test "type-only declarations end at their annotated type" {
    var tree = try Parser.parse(std.testing.allocator, "let value: *mut int\nlet next = 0");
    defer tree.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 0), tree.errors.len);
    const node = tree.rootDecls()[0];
    try std.testing.expectEqualStrings("int", tree.tokenSlice(tree.lastToken(node)));
    const span = tree.nodeToSpan(node);
    try std.testing.expectEqualStrings("let value: *mut int", tree.source[span.start..span.end]);
}

test "render empty AST" {
    const allocator = std.testing.allocator;
    var ast = try Parser.parse(allocator, "");
    defer ast.deinit(allocator);
    try std.testing.expectEqual(@as(usize, 0), ast.errors.len);

    const rendered = try ast.render(allocator);
    defer allocator.free(rendered);
    try std.testing.expectEqualStrings("", rendered);

    var buffer: std.ArrayList(u8) = .empty;
    defer buffer.deinit(allocator);
    try buffer.appendSlice(allocator, "prefix");
    try ast.renderToArrayList(allocator, &buffer, .{});
    try std.testing.expectEqualStrings("prefix", buffer.items);
}

test "render simple declaration" {
    const allocator = std.testing.allocator;
    var ast = try Parser.parse(allocator, "let answer=42\n");
    defer ast.deinit(allocator);
    try std.testing.expectEqual(@as(usize, 0), ast.errors.len);

    const rendered = try ast.render(allocator);
    defer allocator.free(rendered);
    try std.testing.expectEqualStrings("let answer = 42\n", rendered);

    var buffer: std.ArrayList(u8) = .empty;
    defer buffer.deinit(allocator);
    try buffer.appendSlice(allocator, "prefix\n");
    try ast.renderToArrayList(allocator, &buffer, .{});
    try std.testing.expectEqualStrings("prefix\nlet answer = 42\n", buffer.items);
}

pub const Location = struct {
    line: usize,
    column: usize,
    line_start: usize,
    line_end: usize,
};

pub fn tokenLocation(self: Ast, start_offset: u32, token_index: u32) Location {
    var loc: Location = .{
        .line = 0,
        .column = 0,
        .line_start = start_offset,
        .line_end = self.source.len,
    };
    const token_start = self.tokens.items(.start)[token_index];

    // Scan to by line until we go past the token start
    while (std.mem.indexOfScalarPos(u8, self.source, loc.line_start, '\n')) |i| {
        if (i >= token_start) {
            break; // Went past
        }
        loc.line += 1;
        loc.line_start = i + 1;
    }

    const offset = loc.line_start;
    for (self.source[offset..], 0..) |c, i| {
        if (i + offset == token_start) {
            loc.line_end = i + offset;
            while (loc.line_end < self.source.len and self.source[loc.line_end] != '\n') {
                loc.line_end += 1;
            }
            return loc;
        }
        if (c == '\n') {
            loc.line += 1;
            loc.column = 0;
            loc.line_start = i + 1;
        } else {
            loc.column += 1;
        }
    }
    return loc;
}

pub fn tokenSlice(self: Ast, token_index: TokenIndex) []const u8 {
    const token_starts = self.tokens.items(.start);
    const token_tags = self.tokens.items(.tag);
    const token_tag = token_tags[token_index];

    // Many tokens can be determined entirely by their tag.
    if (token_tag.lexeme()) |lexeme| {
        return lexeme;
    }

    // For some tokens, re-tokenization is needed to find the end.
    var tokenizer: Tokenizer = .{
        .buffer = self.source,
        .index = token_starts[token_index],
    };
    const token = tokenizer.next();
    assert(token.tag == token_tag);
    return self.source[token.start..token.end];
}

pub fn extraData(tree: Ast, index: usize, comptime T: type) T {
    const info = @typeInfo(T).@"struct";
    var result: T = undefined;
    inline for (info.field_names, info.field_types, 0..) |name, field_type, i| {
        comptime assert(field_type == Node.Index);
        @field(result, name) = tree.extra_data[index + i];
    }
    return result;
}

pub fn rootDecls(self: Ast) []const Node.Index {
    // Root is always index 0.
    const nodes_data = self.nodes.items(.data);
    return self.extra_data[nodes_data[0].lhs..nodes_data[0].rhs];
}

/// Returns an extra offset for column and byte offset of errors that
/// should point after the token in the error message.
pub fn errorOffset(tree: Ast, parse_error: Error) u32 {
    return if (parse_error.token_is_prev)
        @as(u32, @intCast(tree.tokenSlice(parse_error.token).len))
    else
        0;
}

pub fn renderError(self: Ast, parse_error: Error, writer: anytype) !void {
    const token_tags = self.tokens.items(.tag);
    switch (parse_error.tag) {
        .unsupported_enum_backing => return writer.writeAll("explicit enum backing types are unsupported; use enum { ... }"),
        .chained_range_operators => return writer.writeAll("range operators cannot be chained"),
        .chained_comparison_operators => {
            return writer.writeAll("comparison operators cannot be chained");
        },
        .attr_without_args => {
            return writer.writeAll("expected arguments after '('; remove the parentheses to declare an attribute without arguments");
        },
        // .decl_between_fields => {
        //     return writer.writeAll("declarations are not allowed between container fields");
        // },
        .expected_block => {
            return writer.print("expected block, found '{s}'", .{
                token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
            });
        },
        .expected_block_or_assignment => {
            return writer.print("expected block or assignment, found '{s}'", .{
                token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
            });
        },
        // .expected_block_or_expr => {
        //     return writer.print("expected block or expression, found '{s}'", .{
        //         token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
        //     });
        // },
        // .expected_block_or_field => {
        //     return writer.print("expected block or field, found '{s}'", .{
        //         token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
        //     });
        // },
        // .expected_container_members => {
        //     return writer.print("expected test, comptime, var decl, or container field, found '{s}'", .{
        //         token_tags[parse_error.token].symbol(),
        //     });
        // },
        .unsupported_loop => return writer.writeAll("unsupported loop form; expected 'loop condition { ... }'"),
        .unsupported_range_loop => return writer.writeAll("unsupported iteration loop; expected 'loop name in iterable { ... }' or 'loop &name in local_array { ... }'; multiple/zip bindings, steps and continuations are unsupported"),
        .expected_decl => {
            return writer.print("expected declaration, found '{s}'", .{
                token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
            });
        },
        .expected_expr => {
            return writer.print("expected expression, found '{s}'", .{
                token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
            });
        },
        .expected_expr_or_assignment => {
            return writer.print("expected expression or assignment, found '{s}'", .{
                token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
            });
        },
        .expected_expr_or_decl => {
            return writer.print("expected expression or declaration, found '{s}'", .{
                token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
            });
        },
        .expected_labelable => {
            return writer.print("expected 'for' or '{{', found '{s}'", .{
                token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
            });
        },
        .expected_param_list => {
            return writer.print("expected parameter list, found '{s}'", .{
                token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
            });
        },
        .expected_prefix_expr => {
            return writer.print("expected prefix expression, found '{s}'", .{
                token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
            });
        },
        // .expected_primary_type_expr => {
        //     return writer.print("expected primary type expression, found '{s}'", .{
        //         token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
        //     });
        // },
        .expected_newline_or_else => {
            return writer.writeAll("expected new line or 'else' after statement");
        },
        .expected_newline_or_lbrace => {
            return writer.writeAll("expected new line or block after function prototype");
        },
        .expected_newline_after_decl => {
            return writer.writeAll("expected new line after declaration");
        },
        .expected_newline_after_stmt => {
            return writer.writeAll("expected new line after statement");
        },
        .expected_newline_after_attr => {
            return writer.writeAll("expected new line after attribute");
        },
        .expected_newline_after_match_prong => {
            return writer.writeAll("expected new line after match prong");
        },
        .expected_statement => {
            return writer.print("expected statement, found '{s}'", .{
                token_tags[parse_error.token].symbol(),
            });
        },
        .expected_suffix_op => {
            return writer.print("expected dereference, unwrap, or field access, found '{s}'", .{
                token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
            });
        },
        .expected_type_expr => {
            return writer.print("expected type expression, found '{s}'", .{
                token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
            });
        },
        // .expected_loop_payload => {
        //     return writer.print("expected loop payload, found '{s}'", .{
        //         token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
        //     });
        // },
        // .expected_container => {
        //     return writer.print("expected a struct, enum or union, found '{s}'", .{
        //         token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
        //     });
        // },
        // .extern_fn_body => {
        //     return writer.writeAll("extern functions have no body");
        // },
        // .ptr_mod_on_array_child_type => {
        //     return writer.print("pointer modifier '{s}' not allowed on array child type", .{
        //         token_tags[parse_error.token].symbol(),
        //     });
        // },
        // .invalid_bit_range => {
        //     return writer.writeAll("bit range not allowed on slices and arrays");
        // },
        .same_line_doc_comment => {
            return writer.writeAll("same line documentation comment");
        },
        .unattached_doc_comment => {
            return writer.writeAll("unattached documentation comment");
        },
        // .varargs_nonfinal => {
        //     return writer.writeAll("function prototype has parameter after varargs");
        // },
        // .expected_continue_expr => {
        //     return writer.writeAll("expected ':' before while continue expression");
        // },
        // .expected_comma_after_field => {
        //     return writer.writeAll("expected ',' after field");
        // },
        .expected_comma_after_arg => {
            return writer.writeAll("expected ',' after argument");
        },
        .expected_comma_after_param => {
            return writer.writeAll("expected ',' after parameter");
        },
        // .expected_comma_after_initializer => {
        //     return writer.writeAll("expected ',' after initializer");
        // },
        // .expected_comma_after_for_operand => {
        //     return writer.writeAll("expected ',' after for operand");
        // },
        // .expected_comma_after_capture => {
        //     return writer.writeAll("expected ',' after for capture");
        // },
        // .expected_initializer => {
        //     return writer.writeAll("expected field initializer");
        // },
        .mismatched_binary_op_whitespace => {
            return writer.print("binary operator `{s}` has whitespace on one side, but not the other.", .{token_tags[parse_error.token].lexeme().?});
        },
        .invalid_ampersand_ampersand => {
            return writer.writeAll("ambiguous use of '&&'; use 'and' for logical AND, or change whitespace to ' & &' for bitwise AND");
        },
        .ambiguous_unary_operator => {
            return writer.print("unary operator `{s}` is ambiguous; remove a new line to make it binary or add a new line.", .{token_tags[parse_error.token].lexeme().?});
        },
        .nov_style_container => {
            return writer.print("to declare a container do 'let {s} = {s}'", .{
                self.tokenSlice(parse_error.token), parse_error.extra.expected_tag.symbol(),
            });
        },
        // .previous_field => {
        //     return writer.writeAll("field before declarations here");
        // },
        // .next_field => {
        //     return writer.writeAll("field after declarations here");
        // },
        // .expected_var_const => {
        //     return writer.writeAll("expected 'var' or 'const' before variable declaration");
        // },
        // .var_const_decl => {
        //     return writer.writeAll("use 'var' or 'const' to declare variable");
        // },
        // .extra_for_capture => {
        //     return writer.writeAll("extra capture in for loop");
        // },
        // .for_input_not_captured => {
        //     return writer.writeAll("for input is not captured");
        // },
        .expected_token => {
            const found_tag = token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)];
            const expected_symbol = parse_error.extra.expected_tag.symbol();
            return writer.print("expected '{s}', found '{s}'", .{
                expected_symbol, found_tag.symbol(),
            });
        },
    }
}

pub fn firstToken(self: Ast, node: Node.Index) TokenIndex {
    const tags: []const Node.Tag = self.nodes.items(.tag);
    const datas = self.nodes.items(.data);
    const main_tokens = self.nodes.items(.main_token);
    const token_tags = self.tokens.items(.tag);
    var end_offset: TokenIndex = 0;
    var n = node;
    while (true) switch (tags[n]) {
        .root => return 0,
        .range => {
            if (datas[n].lhs == 0) return main_tokens[n] - end_offset;
            n = datas[n].lhs;
        },

        .decl,
        .bool_not,
        .negation,
        .bit_not,
        .ref_of,
        .ref_type,
        .match,
        .@"if",
        .if_else,
        .loop_range,
        .loop_condition,
        .loop_cont,
        .loop_else,
        .@"continue",
        .@"break",
        .@"return",
        .identifier,
        .type_variable,
        .number_literal,
        .char_literal,
        .string_literal,
        .builtin_literal,
        .fn_proto_one,
        .fn_proto,
        .function_expr,
        .attr_one,
        .attr,
        .array_type,
        .array_init_two,
        .array_init_two_comma,
        .array_init,
        .array_init_comma,
        .@"defer",
        .@"comptime",
        .container_decl,
        .container_decl_trailing,
        .container_decl_two,
        .container_decl_two_trailing,
        .container_decl_arg_trailing,
        .container_field,
        => return main_tokens[n] - end_offset,

        .enum_literal,
        => return main_tokens[n] - 1 - end_offset,

        .container_decl_arg,
        .field_access,
        .unwrap,
        .deref,
        .equal_equal,
        .bang_equal,
        .less_than,
        .greater_than,
        .less_or_equal,
        .greater_or_equal,
        .assign_mul,
        .assign_div,
        .assign_mod,
        .assign_add,
        .assign_sub,
        .assign,
        .mul,
        .div,
        .mod,
        .add,
        .sub,
        .shl,
        .shr,
        .bit_and,
        .bit_xor,
        .bit_or,
        .bool_and,
        .bool_or,
        .call_one,
        .call_one_comma,
        .call,
        .call_comma,
        .function_pipe,
        .bind,
        .in,
        .attr_decl_one,
        .slice,
        .array_access,
        .match_case_one,
        => n = datas[n].lhs,

        .match_case,
        .attr_decl,
        => {
            const extra = self.extraData(datas[n].lhs, Node.SubRange);
            assert(extra.end - extra.start > 0);
            n = self.extra_data[extra.start];
        },

        .block,
        .block_newline,
        .block_two,
        .block_two_newline,
        => {
            // Look for a label.
            const lbrace = main_tokens[n];
            if (lbrace >= 2 and token_tags[lbrace - 1] == .colon and
                token_tags[lbrace - 2] == .identifier)
            {
                // A bare block continuation also follows `identifier :`, but
                // that colon belongs to the loop header, not a block label.
                const separator = for (tags, 0..) |tag, i| {
                    if (tag != .loop_cont and tag != .loop_else) continue;
                    const loop = self.fullLoop(@intCast(i)).?;

                    const header_end = if (loop.condition == 0) main_tokens[i] else self.lastToken(loop.condition);
                    if (header_end + 1 == lbrace - 1) break true;
                } else false;
                if (!separator) end_offset += 2;
            }
            return lbrace - end_offset;
        },
    };
}

pub fn lastToken(self: Ast, node: Node.Index) TokenIndex {
    const tags: []const Node.Tag = self.nodes.items(.tag);
    const datas = self.nodes.items(.data);
    const main_tokens = self.nodes.items(.main_token);
    var n = node;
    const end_offset: TokenIndex = 0;
    while (true) switch (tags[n]) {
        .root => return @intCast(self.tokens.len - 1),
        .range => {
            if (datas[n].rhs == 0) return main_tokens[n] + end_offset;
            n = datas[n].rhs;
        },
        .@"continue" => return if (datas[n].lhs == 0) main_tokens[n] + end_offset else datas[n].lhs + end_offset,
        .@"break" => {
            if (datas[n].rhs == 0) return if (datas[n].lhs == 0) main_tokens[n] + end_offset else datas[n].lhs + end_offset;
            n = datas[n].rhs;
        },
        .@"return" => {
            if (datas[n].rhs == 0) return main_tokens[n] + end_offset;
            n = datas[n].rhs;
        },
        else => {
            // TODO
            // TODO: merge similar cases once all are implemented
            std.log.debug("Unhandled tag: {}", .{tags[n]});
            unreachable;
        },

        .bool_not,
        .negation,
        .bit_not,
        .ref_of,
        .ref_type,
        .equal_equal,
        .bang_equal,
        .less_than,
        .greater_than,
        .less_or_equal,
        .greater_or_equal,
        .assign_mul,
        .assign_div,
        .assign_mod,
        .assign_add,
        .assign_sub,
        .assign,
        .mul,
        .div,
        .mod,
        .add,
        .sub,
        .shl,
        .shr,
        .bit_and,
        .bit_xor,
        .bit_or,
        .bool_and,
        .bool_or,
        .@"if",
        .loop_range,
        .loop_condition,
        .match_case_one,
        .match_case,
        .function_pipe,
        .bind,
        .in,
        .@"comptime",
        .attr_decl_one,
        .attr_decl,
        .array_type,
        .@"defer",
        => n = datas[n].rhs,

        .loop_cont => n = self.extraData(datas[n].rhs, Node.LoopCont).body,
        .loop_else => n = self.extraData(datas[n].rhs, Node.LoopElse).else_expr,

        .field_access,
        .unwrap,
        .deref,
        => return datas[n].rhs + end_offset,

        .number_literal,
        .char_literal,
        .identifier,
        .type_variable,
        .string_literal,
        .enum_literal,
        .builtin_literal,
        => return main_tokens[n] + end_offset,

        .decl => {
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else {
                const declaration: Node.Decl = @bitCast(datas[n].lhs);
                n = declaration.type_node;
            }
        },

        .block,
        .block_newline,
        .block_two,
        .block_two_newline,
        .array_init,
        .array_init_comma,
        .array_init_two,
        .array_init_two_comma,
        => return self.matchingDelimiter(main_tokens[n]) + end_offset,

        .function_expr => {
            n = datas[n].rhs;
        },
        .fn_proto_one,
        .fn_proto,
        => {
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else {
                return self.matchingDelimiter(main_tokens[n]) + end_offset;
            }
        },

        .if_else => {
            const extra = self.extraData(datas[n].rhs, Node.If);
            assert(extra.else_expr != 0);
            n = extra.else_expr;
        },

        .call_one,
        .call_one_comma,
        .call,
        .call_comma,
        => return self.matchingDelimiter(main_tokens[n]) + end_offset,

        .container_decl => return self.matchingDelimiter(main_tokens[n] + 1) + end_offset,
        .container_decl_arg, .array_access, .slice => return self.matchingDelimiter(main_tokens[n]) + end_offset,
        .container_field => {
            if (datas[n].lhs == 0 and datas[n].rhs == 0) return main_tokens[n] + end_offset;
            n = if (datas[n].rhs != 0) datas[n].rhs else datas[n].lhs;
        },

        .attr_one, .attr => return self.matchingDelimiter(main_tokens[n]) + end_offset,

        .match => return self.matchingDelimiter(self.fullMatch(n).?.ast.lbrace) + end_offset,
    };
}

// Child-node offsets cannot locate closers across optional newlines, comments,
// and trailing commas. Token tags also keep delimiters inside literals opaque.
fn matchingDelimiter(self: Ast, opening: TokenIndex) TokenIndex {
    const token_tags = self.tokens.items(.tag);
    const open_tag = token_tags[opening];
    const close_tag: Token.Tag = switch (open_tag) {
        .l_brace => .r_brace,
        .l_paren => .r_paren,
        .l_bracket, .at_sign_l_bracket => .r_bracket,
        else => unreachable,
    };
    var depth: usize = 1;
    var token = opening + 1;
    while (token < token_tags.len) : (token += 1) {
        if (token_tags[token] == open_tag or (open_tag == .at_sign_l_bracket and token_tags[token] == .l_bracket)) {
            depth += 1;
        } else if (token_tags[token] == close_tag) {
            depth -= 1;
            if (depth == 0) return token;
        }
    }
    unreachable; // Parsed block/prototype/call nodes have a closing delimiter.
}

pub fn tokensOnSameLine(self: Ast, token1: TokenIndex, token2: TokenIndex) bool {
    const token_starts = self.tokens.items(.start);
    const source = self.source[token_starts[token1]..token_starts[token2]];
    return std.mem.indexOfScalar(u8, source, '\n') == null;
}

pub fn getNodeSource(self: Ast, node: Node.Index) []const u8 {
    const token_starts = self.tokens.items(.start);
    const first_token = self.firstToken(node);
    const last_token = self.lastToken(node);
    const start = token_starts[first_token];
    const end = token_starts[last_token] + self.tokenSlice(last_token).len;
    return self.source[start..end];
}

pub fn declarationAttributes(self: Ast, node: Node.Index) []const Node.Index {
    const data = &self.nodes.items(.data)[node];
    return switch (self.nodes.items(.tag)[node]) {
        .attr_decl_one => @as(*const [1]Node.Index, &data.lhs),
        .attr_decl => blk: {
            const range = self.extraData(data.lhs, Node.SubRange);
            break :blk self.extra_data[range.start..range.end];
        },
        else => &.{},
    };
}

/// Ordered field/declaration nodes borrowed from this AST, including attribute wrappers.
pub fn isUnion(self: Ast, node: Node.Index) bool {
    return self.nodes.items(.tag)[node] == .container_decl and self.tokens.items(.tag)[self.nodes.items(.main_token)[node]] == .keyword_union;
}

pub fn fullStruct(self: Ast, node: Node.Index) ?[]const Node.Index {
    if (self.nodes.items(.tag)[node] != .container_decl) return null;
    const data = self.nodes.items(.data)[node];
    return self.extra_data[data.lhs..data.rhs];
}

/// Constructor operands remain in source order, not layout order.
pub const StructInit = struct { type_node: Node.Index, fields: []const Node.Index };
pub fn fullStructInit(self: Ast, node: Node.Index) ?StructInit {
    if (self.nodes.items(.tag)[node] != .container_decl_arg) return null;
    const data = self.nodes.items(.data)[node];
    const range = self.extraData(data.rhs, Node.SubRange);
    return .{ .type_node = data.lhs, .fields = self.extra_data[range.start..range.end] };
}

// TODO: all of this

pub fn decl(self: Ast, node: Node.Index) full.Decl {
    if (self.nodes.items(.tag)[node] == .attr_decl_one or self.nodes.items(.tag)[node] == .attr_decl) {
        var result = self.decl(self.nodes.items(.data)[node].rhs);
        result.attributes = self.declarationAttributes(node);
        return result;
    }
    assert(self.nodes.items(.tag)[node] == .decl);
    const let_token = self.nodes.items(.main_token)[node];
    const data = self.nodes.items(.data)[node];
    const extra: Node.Decl = @bitCast(data.lhs);
    return .{
        .attributes = &.{},
        .ast = .{
            .let_token = let_token,
            .is_mutable = extra.mutable,
            .type_node = extra.type_node,
            .init_node = data.rhs,
        },
    };
}

pub fn ifSimple(self: Ast, node: Node.Index) full.If {
    assert(self.nodes.items(.tag)[node] == .@"if");
    const data = self.nodes.items(.data)[node];
    return (.{
        .else_token = undefined,
        .ast = .{
            .if_token = self.nodes.items(.main_token)[node],
            .cond_expr = data.lhs,
            .then_expr = data.rhs,
            .else_expr = 0,
        },
    });
}

pub fn ifElse(self: Ast, node: Node.Index) full.If {
    assert(self.nodes.items(.tag)[node] == .if_else);
    const data = self.nodes.items(.data)[node];
    const extra = self.extraData(data.rhs, Node.If);
    return (.{
        .else_token = self.lastToken(extra.then_expr) + 1,
        .ast = .{
            .if_token = self.nodes.items(.main_token)[node],
            .cond_expr = data.lhs,
            .then_expr = extra.then_expr,
            .else_expr = extra.else_expr,
        },
    });
}

pub fn fnProtoOne(self: Ast, node: Node.Index) full.FnProto {
    assert(self.nodes.items(.tag)[node] == .fn_proto_one);
    const data = &self.nodes.items(.data)[node];
    const params: *const [1]Node.Index = @ptrCast(&data.lhs);
    return self.fullFnProtoComponents(.{
        .proto_node = node,
        .fn_token = self.nodes.items(.main_token)[node],
        .return_type = if (data.rhs == 0) null else data.rhs,
        .params = if (data.lhs == 0) params[0..0] else params[0..1],
    });
}

pub fn fnProto(self: Ast, node: Node.Index) full.FnProto {
    assert(self.nodes.items(.tag)[node] == .fn_proto);
    const data = self.nodes.items(.data)[node];
    const params_range = self.extraData(data.lhs, Node.SubRange);
    const params = self.extra_data[params_range.start..params_range.end];
    return self.fullFnProtoComponents(.{
        .proto_node = node,
        .fn_token = self.nodes.items(.main_token)[node],
        .return_type = if (data.rhs == 0) null else data.rhs,
        .params = params,
    });
}

pub fn sliceInitOne(self: Ast, buffer: *[1]Node.Index, node: Node.Index) full.SliceInit {
    assert(self.nodes.items(.tag)[node] == .slice_init_one);
    const data = self.nodes.items(.data)[node];
    buffer[0] = data.rhs;
    const elements = if (data.rhs == 0) buffer[0..0] else buffer[0..1];
    return .{
        .ast = .{
            .lbrace = self.nodes.items(.main_token)[node],
            .elements = elements,
            .type_expr = data.lhs,
        },
    };
}

pub fn sliceInitDotTwo(self: Ast, buffer: *[2]Node.Index, node: Node.Index) full.SliceInit {
    assert(self.nodes.items(.tag)[node] == .slice_init_dot_two);
    const data = self.nodes.items(.data)[node];
    buffer.* = .{ data.lhs, data.rhs };
    const elements = if (data.rhs != 0)
        buffer[0..2]
    else if (data.lhs != 0)
        buffer[0..1]
    else
        buffer[0..0];
    return .{
        .ast = .{
            .lbrace = self.nodes.items(.main_token)[node],
            .elements = elements,
            .type_expr = 0,
        },
    };
}

pub fn sliceInitDot(self: Ast, node: Node.Index) full.SliceInit {
    assert(self.nodes.items(.tag)[node] == .slice_init_dot);
    const data = self.nodes.items(.data)[node];
    return .{
        .ast = .{
            .lbrace = self.nodes.items(.main_token)[node],
            .elements = self.extra_data[data.lhs..data.rhs],
            .type_expr = 0,
        },
    };
}

pub fn sliceInit(self: Ast, node: Node.Index) full.SliceInit {
    assert(self.nodes.items(.tag)[node] == .slice_init or
        self.nodes.items(.tag)[node] == .slice_init_comma);
    const data = self.nodes.items(.data)[node];
    const elem_range = self.extraData(data.rhs, Node.SubRange);
    return .{
        .ast = .{
            .lbrace = self.nodes.items(.main_token)[node],
            .elements = self.extra_data[elem_range.start..elem_range.end],
            .type_expr = data.lhs,
        },
    };
}

pub fn slice(self: Ast, node: Node.Index) full.Slice {
    assert(self.nodes.items(.tag)[node] == .slice);
    const data = self.nodes.items(.data)[node];
    const bounds = self.nodes.items(.data)[data.rhs];
    return .{
        .ast = .{
            .sliced = data.lhs,
            .lbracket = self.nodes.items(.main_token)[node],
            .start = bounds.lhs,
            .end = bounds.rhs,
        },
    };
}

pub fn matchCaseOne(tree: Ast, node: Node.Index) full.MatchCase {
    const data = &tree.nodes.items(.data)[node];
    const values: *const [1]Node.Index = @ptrCast(&data.lhs);
    return .{
        .ast = .{
            .values = if (data.lhs == 0) values[0..0] else values[0..1],
            .capture_token = tree.matchCapture(node),
            .arrow_token = tree.nodes.items(.main_token)[node],
            .target_expr = data.rhs,
        },
    };
}

pub fn matchCase(tree: Ast, node: Node.Index) full.MatchCase {
    const data = tree.nodes.items(.data)[node];
    const extra = tree.extraData(data.lhs, Node.SubRange);
    return .{
        .ast = .{
            .values = tree.extra_data[extra.start..extra.end],
            .capture_token = tree.matchCapture(node),
            .arrow_token = tree.nodes.items(.main_token)[node],
            .target_expr = data.rhs,
        },
    };
}

// TODO: for

pub fn callOne(tree: Ast, node: Node.Index) full.Call {
    assert(tree.nodes.items(.tag)[node] == .call_one);
    const data = &tree.nodes.items(.data)[node];
    const args: *const [1]Node.Index = @ptrCast(&data.rhs);
    return .{
        .ast = .{
            .lparen = tree.nodes.items(.main_token)[node],
            .fn_expr = data.lhs,
            .args = if (data.rhs != 0) args[0..1] else args[0..0],
        },
    };
}

pub fn callFull(tree: Ast, node: Node.Index) full.Call {
    assert(tree.nodes.items(.tag)[node] == .call);
    const data = tree.nodes.items(.data)[node];
    const extra = tree.extraData(data.rhs, Node.SubRange);
    return .{
        .ast = .{
            .lparen = tree.nodes.items(.main_token)[node],
            .fn_expr = data.lhs,
            .args = tree.extra_data[extra.start..extra.end],
        },
    };
}

fn fullFnProtoComponents(self: Ast, info: full.FnProto.Components) full.FnProto {
    const token_tags = self.tokens.items(.tag);
    assert(token_tags[info.fn_token] == .l_paren);
    return .{
        .ast = info,
        .lparen = info.fn_token,
    };
}

pub fn fullDecl(self: Ast, node: Node.Index) ?full.Decl {
    return switch (self.nodes.items(.tag)[node]) {
        .decl, .attr_decl_one, .attr_decl => self.decl(node),
        else => null,
    };
}

pub fn fullRange(self: Ast, node: Node.Index) ?Node.RangeLoop {
    if (self.nodes.items(.tag)[node] != .loop_range) return null;
    return self.extraData(self.nodes.items(.data)[node].lhs, Node.RangeLoop);
}

pub fn fullLoop(self: Ast, node: Node.Index) ?struct { condition: Node.Index, continuation: Node.Index, body: Node.Index, else_expr: Node.Index = 0 } {
    return switch (self.nodes.items(.tag)[node]) {
        .loop_condition => blk: {
            const data = self.nodes.items(.data)[node];
            break :blk .{ .condition = data.lhs, .continuation = 0, .body = data.rhs };
        },
        .loop_else => blk: {
            const data = self.nodes.items(.data)[node];
            const extra = self.extraData(data.rhs, Node.LoopElse);
            break :blk .{ .condition = data.lhs, .continuation = extra.continuation, .body = extra.body, .else_expr = extra.else_expr };
        },
        .loop_cont => blk: {
            const data = self.nodes.items(.data)[node];
            const extra = self.extraData(data.rhs, Node.LoopCont);
            break :blk .{ .condition = data.lhs, .continuation = extra.continuation, .body = extra.body };
        },
        else => null,
    };
}

pub fn fullIf(self: Ast, node: Node.Index) ?full.If {
    return switch (self.nodes.items(.tag)[node]) {
        .@"if" => self.ifSimple(node),
        .if_else => self.ifElse(node),
        else => null,
    };
}

/// Declaration bodies share one semantic view without rewriting expression syntax.
pub fn declFunction(self: Ast, node: Node.Index) ?struct { proto_node: Node.Index, body: Node.Index } {
    const declaration = self.decl(node).ast;
    if (self.fullFnProto(declaration.type_node) != null) return .{ .proto_node = declaration.type_node, .body = declaration.init_node };
    if (declaration.type_node == 0 and declaration.init_node != 0) {
        if (self.functionExpr(declaration.init_node)) |function| return .{ .proto_node = function.proto.ast.proto_node, .body = function.body };
    }
    return null;
}

pub fn functionExpr(self: Ast, node: Node.Index) ?struct { proto: full.FnProto, body: Node.Index } {
    if (self.nodes.items(.tag)[node] != .function_expr) return null;
    const data = self.nodes.items(.data)[node];
    return .{ .proto = self.fullFnProto(data.lhs).?, .body = data.rhs };
}

pub fn fullFnProto(self: Ast, node: Node.Index) ?full.FnProto {
    return switch (self.nodes.items(.tag)[node]) {
        .fn_proto_one => self.fnProtoOne(node),
        .fn_proto => self.fnProto(node),
        else => null,
    };
}

pub fn fullSliceInit(self: Ast, buffer: *[2]Node.Index, node: Node.Index) ?full.SliceInit {
    return switch (self.nodes.items(.tag)[node]) {
        .slice_init_one => self.sliceInitOne(buffer[0..1], node),
        .slice_init_dot_two => self.sliceInitDotTwo(buffer, node),
        .slice_init_dot => self.sliceInitDot(node),
        .slice_init => self.sliceInit(node),
        else => null,
    };
}

pub fn fullSlice(self: Ast, node: Node.Index) ?full.Slice {
    return switch (self.nodes.items(.tag)[node]) {
        .slice => self.slice(node),
        else => null,
    };
}

pub fn fullMatch(self: Ast, node: Node.Index) ?full.Match {
    if (self.nodes.items(.tag)[node] != .match) return null;
    const data = self.nodes.items(.data)[node];
    const subjects = self.extraData(data.lhs, Node.SubRange);
    const cases = self.extraData(data.rhs, Node.SubRange);
    const values = self.extra_data[subjects.start..subjects.end];
    var lbrace = self.lastToken(values[values.len - 1]) + 1;
    while (self.tokens.items(.tag)[lbrace] == .newline) : (lbrace += 1) {}
    return .{ .ast = .{
        .match_token = self.nodes.items(.main_token)[node],
        .lbrace = lbrace,
        .subjects = values,
        .cases = self.extra_data[cases.start..cases.end],
    } };
}

fn matchCapture(self: Ast, node: Node.Index) ?TokenIndex {
    const next = self.nodes.items(.main_token)[node] + 1;
    return if (self.tokens.items(.tag)[next] == .pipe) next + 1 else null;
}

pub fn fullMatchCase(self: Ast, node: Node.Index) ?full.MatchCase {
    return switch (self.nodes.items(.tag)[node]) {
        .match_case_one => self.matchCaseOne(node),
        .match_case => self.matchCase(node),
        else => null,
    };
}

pub fn fullCall(self: Ast, node: Node.Index) ?full.Call {
    return switch (self.nodes.items(.tag)[node]) {
        .call => self.callFull(node),
        .call_one => self.callOne(node),
        else => null,
    };
}

/// Compact block statements borrow `buffer`; keep it alive and unchanged while
/// using the returned view. Larger blocks borrow AST storage instead.
pub fn fullBlock(self: Ast, buffer: *[2]Node.Index, node: Node.Index) ?full.Block {
    const data = self.nodes.items(.data)[node];
    const statements: []const Node.Index = switch (self.nodes.items(.tag)[node]) {
        .block_two, .block_two_newline => blk: {
            buffer.* = .{ data.lhs, data.rhs };
            const count: usize = if (data.lhs == 0) 0 else if (data.rhs == 0) 1 else 2;
            break :blk buffer[0..count];
        },
        .block, .block_newline => self.extra_data[data.lhs..data.rhs],
        else => return null,
    };
    return .{ .ast = .{
        .lbrace = self.nodes.items(.main_token)[node],
        .rbrace = self.lastToken(node),
        .statements = statements,
    } };
}

/// Fully assembled AST node information.
pub const full = struct {
    // TODO: do we really need a separate struct for Decl just have a AttrDecl struct
    pub const Decl = struct {
        attributes: []const Node.Index,
        ast: Components,

        pub const Components = struct {
            let_token: TokenIndex,
            is_mutable: bool,
            type_node: Node.Index,
            init_node: Node.Index,
        };
    };

    pub const Block = struct {
        ast: Components,

        pub const Components = struct {
            lbrace: TokenIndex,
            rbrace: TokenIndex,
            /// Ordered statements, without inferring whether the last produces a value.
            /// Borrows the caller's buffer for compact blocks, otherwise AST storage.
            statements: []const Node.Index,
        };
    };

    pub const If = struct {
        /// Populated only if else_expr != 0
        else_token: TokenIndex,
        ast: Components,

        pub const Components = struct {
            if_token: TokenIndex,
            cond_expr: Node.Index,
            then_expr: Node.Index,
            else_expr: Node.Index,
        };
    };

    pub const FnProto = struct {
        lparen: TokenIndex,
        ast: Components,

        pub const Components = struct {
            proto_node: Node.Index,
            fn_token: TokenIndex,
            return_type: ?Node.Index,
            /// Borrows AST storage; valid until the AST is mutated or deinitialized.
            params: []const Node.Index,
        };

        pub const Param = struct {
            first_doc_comment: ?TokenIndex,
            name_token: ?TokenIndex,
            comptime_noalias: ?TokenIndex,
            anytype_ellipsis3: ?TokenIndex,
            type_expr: Node.Index,
        };

        pub fn iterate(proto: FnProto, tree: *const Ast) Iterator {
            return .{ .tree = tree, .params = proto.ast.params };
        }

        pub const Iterator = struct {
            tree: *const Ast,
            params: []const Node.Index,
            index: usize = 0,

            pub fn next(it: *Iterator) ?Param {
                if (it.index == it.params.len) return null;
                const type_expr = it.params[it.index];
                it.index += 1;
                const first_token = it.tree.firstToken(type_expr);
                const token_tags = it.tree.tokens.items(.tag);
                const name_token = if (first_token >= 2 and
                    token_tags[first_token - 1] == .colon and
                    token_tags[first_token - 2] == .identifier)
                    first_token - 2
                else
                    null;
                return .{
                    .first_doc_comment = null,
                    .name_token = name_token,
                    .comptime_noalias = null,
                    .anytype_ellipsis3 = null,
                    .type_expr = type_expr,
                };
            }
        };
    };

    pub const SliceInit = struct {
        ast: Components,

        pub const Components = struct {
            lbrace: TokenIndex,
            elements: []const Node.Index,
            type_expr: Node.Index,
        };
    };

    pub const Slice = struct {
        ast: Components,

        pub const Components = struct {
            sliced: Node.Index,
            lbracket: TokenIndex,
            start: Node.Index,
            end: Node.Index,
        };
    };

    pub const Match = struct {
        ast: struct {
            match_token: TokenIndex,
            lbrace: TokenIndex,
            subjects: []const Node.Index,
            cases: []const Node.Index,
        },
    };

    pub const MatchCase = struct {
        ast: Components,

        pub const Components = struct {
            capture_token: ?TokenIndex = null,
            /// Positional patterns; bit_or nodes represent literal alternatives.
            /// Wildcards are identifier nodes spelling `_`.
            values: []const Node.Index,
            arrow_token: TokenIndex,
            target_expr: Node.Index,
        };
    };

    pub const Call = struct {
        ast: Components,

        pub const Components = struct {
            lparen: TokenIndex,
            fn_expr: Node.Index,
            /// Borrows AST storage; valid until the AST is mutated or deinitialized.
            args: []const Node.Index,
        };
    };
};

pub const Error = struct {
    tag: Tag,
    is_note: bool = false,
    token_is_prev: bool = false,
    token: TokenIndex,
    extra: union {
        none: void,
        expected_tag: Token.Tag,
    } = .{ .none = {} },

    pub const Tag = enum {
        chained_comparison_operators,
        chained_range_operators,
        attr_without_args,
        mismatched_binary_op_whitespace,
        invalid_ampersand_ampersand,
        ambiguous_unary_operator,
        same_line_doc_comment,
        unattached_doc_comment,
        nov_style_container,
        unsupported_loop,
        unsupported_enum_backing,
        unsupported_range_loop,
        expected_decl,
        expected_expr,
        expected_expr_or_assignment,
        expected_expr_or_decl,
        expected_suffix_op,
        expected_type_expr,
        expected_prefix_expr,
        expected_newline_or_else,
        expected_newline_or_lbrace,
        expected_newline_after_decl,
        expected_newline_after_stmt,
        expected_newline_after_attr,
        expected_newline_after_match_prong,
        expected_comma_after_arg,
        expected_comma_after_param,
        expected_block,
        expected_block_or_assignment,
        expected_statement,
        expected_labelable,
        expected_param_list,

        /// `expected_tag` is populated.
        expected_token,
    };
};

pub const Node = struct {
    tag: Tag,
    main_token: TokenIndex,
    data: Data,

    pub const Index = u32;

    pub const Tag = enum(u8) {
        /// sub_list[lhs..rhs]
        root,
        /// `let mut? x: type? = rhs`. `Decl[lhs]`.
        /// rhs may be omitted.
        /// Can be local or global.
        /// main_token is `let`
        decl,
        /// `@[lhs]`.
        /// `@[lhs(rhs)]`.
        /// main_token is `@[`.
        attr_one,
        /// `@[lhs(a, b, c)]`. `SubRange[rhs]`.
        /// main_token is `@[
        attr,
        /// lhs is the attr
        /// rhs is the decl
        attr_decl_one,
        /// `SubRange[lhs]` of attr
        /// rhs is the decl
        attr_decl,
        /// `lhs.a`.
        /// main_token is the dot.
        /// rhs is the identifier token index.
        field_access,
        /// `lhs.!`.
        /// main_token is the dot.
        /// rhs is the `!` token index.
        unwrap,
        /// `lhs.*`.
        /// main_token is the dot.
        /// rhs is the `*` token index.
        deref,
        /// `lhs == rhs`. main_token is op.
        equal_equal,
        /// `lhs != rhs`. main_token is op.
        bang_equal,
        /// `lhs < rhs`. main_token is op.
        less_than,
        /// `lhs > rhs`. main_token is op.
        greater_than,
        /// `lhs <= rhs`. main_token is op.
        less_or_equal,
        /// `lhs >= rhs`. main_token is op.
        greater_or_equal,
        /// `lhs *= rhs`. main_token is op.
        assign_mul,
        /// `lhs /= rhs`. main_token is op.
        assign_div,
        /// `lhs %= rhs`. main_token is op.
        assign_mod,
        /// `lhs += rhs`. main_token is op.
        assign_add,
        /// `lhs -= rhs`. main_token is op.
        assign_sub,
        /// `lhs = rhs`. main_token is op.
        assign,
        // TODO: assign_destructure
        // a, b = x, y
        // how to return multiple values from a function if we do not have tuples?
        /// `lhs * rhs`. main_token is op.
        mul,
        /// `lhs / rhs`. main_token is op.
        div,
        /// `lhs % rhs`. main_token is op.
        mod,
        /// `lhs + rhs`. main_token is op.
        add,
        /// `lhs - rhs`. main_token is op.
        sub,
        /// `lhs << rhs`. main_token is op.
        shl,
        /// `lhs >> rhs`. main_token is op.
        shr,
        /// `lhs & rhs`. main_token is op.
        bit_and,
        /// `lhs ^ rhs`. main_token is op.
        bit_xor,
        /// `lhs | rhs`. main_token is op.
        bit_or,
        /// `lhs |> rhs`. main_token is op.
        function_pipe,
        // TODO: lhs >>= |args| expr
        /// `lhs >>= rhs`. main_token is op.
        bind,
        /// `lhs in rhs`. main_token is op.
        in,
        /// `lhs and rhs`. main_token is op.
        bool_and,
        /// `lhs or rhs`. main_token is op.
        bool_or,
        /// `!rhs`. lhs unused. main_token is op.
        bool_not,
        /// `-rhs`. lhs unused. main_token is op.
        negation,
        /// `~rhs`. lhs unused. main_token is op.
        bit_not,
        /// `&rhs`. lhs unused. main_token is op.
        ref_of,
        /// `*mut rhs`.
        /// main_token is the `*`.
        /// lhs is the `mut` token index if any.
        ref_type,
        /// `[]rhs`. lhs is unused.
        array_type,
        /// `lhs[rhs]`.
        /// rhs is the `range` node index.
        /// main_token is the lbracket.
        slice,
        /// `lhs[rhs]`.
        /// main_token is the lbracket.
        array_access,
        /// `[lhs, rhs]`. lhs and rhs can be omitted.
        /// main_token points at the lbracket.
        array_init_two,
        /// `[lhs, rhs,]`. lhs and rhs can be omitted.
        /// main_token points at the lbracket.
        array_init_two_comma,
        /// `[a, b, c]`. `sub_list[lhs..rhs]`.
        /// main_token points at the lbracket.
        array_init,
        /// `[a, b, c,]`. `sub_list[lhs..rhs]`.
        /// main_token points at the lbracket.
        array_init_comma,
        // TODO: struct init
        /// `lhs(rhs)`. rhs can be omitted.
        /// main_token is the `(`.
        call_one,
        /// `lhs(rhs,)`. rhs can be omitted.
        /// main_token is the lparen.
        call_one_comma,
        /// `lhs(a, b, c)`. `SubRange[rhs]`.
        /// main_token is the `(`.
        call,
        /// `lhs(a, b, c,)`. `SubRange[rhs]`.
        /// main_token is the `(`.
        call_comma,
        /// `match a, b { ... }`. Subjects `SubRange[lhs]`, cases `SubRange[rhs]`.
        match,
        /// `lhs => rhs`.
        /// main_token is the `=>`
        match_case_one,
        /// `a, b, c => rhs`. Positional patterns `SubRange[lhs]`.
        /// main_token is the `=>`.
        match_case,
        /// `lhs..rhs`.
        /// `lhs..=rhs`.
        /// rhs can be omitted.
        /// main_token is the `..` or `..=`.
        range,
        /// Iteration over a range, array or string. `RangeLoop[lhs]`, rhs is the final body/else.
        loop_range,
        /// `loop lhs rhs`: optional boolean condition (0 for unconditional) and lexical body block.
        loop_condition,
        /// Optional condition in lhs (0 for unconditional), LoopCont extra in rhs.
        loop_cont,
        /// Condition in lhs, LoopElse extra in rhs; exhaustion is outside loop control scope.
        loop_else,
        /// `if lhs {}`. rhs is the block.
        @"if",
        /// `if lhs {} else {}`. `If[rhs]`.
        if_else,
        /// `continue :lhs`.
        /// lhs is token index of label if any. rhs is unused.
        @"continue",
        /// `break :lhs rhs`
        /// lhs and rhs may be omitted.
        @"break",
        /// `return rhs`. rhs can be omitted. lhs is unused.
        @"return",
        // `defer rhs`. rhs is the deferred expression. lhs is unused.
        @"defer",
        /// `#rhs`. lhs unused.
        /// main_token is the `#`.
        @"comptime",
        /// `(a: lhs) -> rhs`.
        /// lhs and rhs can be omitted.
        /// main_token is the `(`.
        fn_proto_one,
        /// Anonymous function: lhs prototype, rhs block body.
        function_expr,
        /// `(a: b, c: d) -> rhs`. `SubRange[lhs]`.
        /// rhs can be omitted.
        /// main_token is the `(`
        fn_proto,
        /// Both lhs and rhs unused.
        number_literal,
        /// Both lhs and rhs unused.
        char_literal,
        /// Both lhs and rhs unused.
        builtin_literal,
        /// Both lhs and rhs unused.
        /// Most identifiers will not have explicit AST nodes, however for expressions
        /// which could be one of many different kinds of AST nodes, there will be an
        /// identifier AST node for it.
        identifier,
        /// A backtick-prefixed signature type variable; both data fields unused.
        type_variable,
        /// lhs is the dot token index, rhs unused.
        /// main_token is the identifier.
        enum_literal,
        /// main_token is the complete outer string literal token.
        /// extra_data[lhs..rhs] holds placeholder expression nodes in source order;
        /// zero entries represent invalid placeholder syntax.
        string_literal,
        /// `{lhs rhs}`. lhs and rhs can be omitted.
        /// main_token points at the lbrace.
        block_two,
        /// Same as block_two but there is known to be a newline before the rbrace.
        block_two_newline,
        /// `{}`. `sub_list[lhs..rhs]`.
        /// main_token points at the lbrace.
        block,
        /// Same as block but there is known to be a newline before the rbrace.
        block_newline,
        /// Delivered `struct { ... }` members in `extra_data[lhs..rhs]`.
        /// main_token is `struct`, `union`, `opaque`, `enum` keyword.
        container_decl,
        /// Same as container_decl but there is known to be a trailing comma
        /// or newline before the rbrace.
        container_decl_trailing,
        /// `struct [lhs, rhs]`, `union [lhs, rhs]`, `opaque [lhs, rhs]`, `enum [lhs, rhs]`.
        /// lhs or rhs can be omitted.
        /// main_token is `struct`, `union`, `opaque`, `enum` keyword.
        container_decl_two,
        /// Same as container_decl_two except there is known to be a trailing comma
        /// or newline before the rbrace.
        container_decl_two_trailing,
        /// Explicit `lhs[ .field = value, .same_name, ... ]` constructor.
        /// `SubRange[rhs]` contains container_field nodes with lhs=0;
        /// main_token is the `[`. Shorthand fields have rhs=0.
        container_decl_arg,
        /// Same as container_decl_arg but there is known to be a trailing
        /// comma or newline before the rbrace.
        container_decl_arg_trailing,
        /// Declaration `a: lhs = rhs,` (rhs optional), or constructor `.a = rhs`
        /// (lhs=0), or shorthand `.a` (lhs=rhs=0).
        /// Constructor punctuation belongs to the surrounding initializer.
        /// main_token is the field name identifier.
        /// lastToken() does not include the possible trailing comma.
        container_field,
    };

    pub const Data = struct {
        lhs: Index,
        rhs: Index,
    };

    pub const RangeLoop = struct {
        /// Identifier, or ref_of(identifier) for a scoped element loan.
        binding: Index,
        source: Index,
        body: Index,
        else_expr: Index,
    };

    pub const SubRange = struct {
        /// Index into sub_list.
        start: Index,
        /// Index into sub_list.
        end: Index,
    };

    pub const Decl = packed struct(Index) {
        mutable: bool,
        /// Can be Parser.null_node
        type_node: u31,
    };

    pub const LoopElse = struct {
        continuation: Index,
        body: Index,
        else_expr: Index,
    };

    pub const LoopCont = struct {
        continuation: Index,
        body: Index,
    };

    pub const If = struct {
        then_expr: Index,
        /// can be a block or another if expression
        else_expr: Index,
    };

    // pub const Loop = struct {
    //     cont_expr: Index,
    //     then_expr: Index,
    //     else_expr: Index,
    // };

    // pub const LoopCont = struct {
    //     cont_expr: Index,
    //     then_expr: Index,
    // };

    // pub const Loop = packed struct(u32) {
    //     inputs: u31,
    //     has_else: bool,
    // };
};

pub const Span = struct {
    start: u32,
    end: u32,
    main: u32,
};

/// A single-line diagnostic highlight, not necessarily the node's full extent.
/// Use firstToken/lastToken for the complete token range of multiline nodes.
pub fn nodeToSpan(self: Ast, node: u32) Span {
    return tokensToSpan(
        self,
        self.firstToken(node),
        self.lastToken(node),
        self.nodes.items(.main_token)[node],
    );
}

pub fn tokenToSpan(self: Ast, token: Ast.TokenIndex) Span {
    return tokensToSpan(self, token, token, token);
}

pub fn tokensToSpan(self: Ast, start: Ast.TokenIndex, end: Ast.TokenIndex, main: Ast.TokenIndex) Span {
    const token_starts = self.tokens.items(.start);
    var start_tok = start;
    var end_tok = end;

    if (self.tokensOnSameLine(start, end)) {
        // do nothing
    } else if (self.tokensOnSameLine(start, main)) {
        end_tok = main;
    } else if (self.tokensOnSameLine(main, end)) {
        start_tok = main;
    } else {
        start_tok = main;
        end_tok = main;
    }
    const start_off = token_starts[start_tok];
    const end_off = token_starts[end_tok] + @as(u32, @intCast(self.tokenSlice(end_tok).len));
    return .{ .start = start_off, .end = end_off, .main = token_starts[main] };
}
