const std = @import("std");
const Ast = @import("Ast.zig");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");

/// Supports top-level let/mut declarations with simple types, identifiers,
/// number/character/string literals, arithmetic, bitwise/comparison/boolean
/// operators, unary -/!/~, line/doc comments, and multiline continuations.
/// All other AST tags and quoted identifiers/BOMs/shebangs are unsupported.
/// Parse errors return InvalidSource; unsupported or unpreservable output
/// returns UnsupportedSyntax. Allocation failures propagate as OutOfMemory.
/// The caller owns the returned allocation (free it with allocator.free).
/// No output is returned unless reparsing preserves
/// the AST, every non-newline token, and every comment (apart from trailing space).
/// Unsupported syntax, including any failed preservation check, is rejected.
pub fn format(allocator: std.mem.Allocator, source: [:0]const u8) ![]u8 {
    // The tokenizer treats NUL as EOF and skips BOMs/shebangs. Do not silently
    // discard either source suffixes or file headers.
    if (std.mem.indexOfScalar(u8, source, 0) != null) return error.InvalidSource;
    var ast = try Parser.parse(allocator, source);
    defer ast.deinit(allocator);
    if (ast.errors.len != 0) return error.InvalidSource;
    if (std.mem.startsWith(u8, source, "\xEF\xBB\xBF") or
        std.mem.startsWith(u8, source, "#!")) return error.UnsupportedSyntax;
    try checkSupported(ast);

    const rendered = try ast.render(allocator);
    errdefer allocator.free(rendered);
    const terminated = try allocator.dupeZ(u8, rendered);
    defer allocator.free(terminated);
    var result = try Parser.parse(allocator, terminated);
    defer result.deinit(allocator);
    if (result.errors.len != 0) return error.UnsupportedSyntax;
    try checkSupported(result);
    if (!sameStructure(ast, result) or !sameTokensAndComments(source, terminated)) {
        return error.UnsupportedSyntax;
    }
    return rendered;
}

fn checkSupported(ast: Ast) !void {
    // Inspect ALL nodes, not only roots: unsupported expressions can be nested
    // inside otherwise renderable declarations and operators. New tags fail closed.
    for (ast.nodes.items(.tag)) |tag| switch (tag) {
        .root,
        .decl,
        .identifier,
        .number_literal,
        .char_literal,
        .string_literal,
        .add,
        .sub,
        .mul,
        .div,
        .mod,
        .shl,
        .shr,
        .bit_and,
        .bit_or,
        .bit_xor,
        .equal_equal,
        .bang_equal,
        .less_than,
        .less_or_equal,
        .greater_than,
        .greater_or_equal,
        .bool_and,
        .bool_or,
        .negation,
        .bool_not,
        .bit_not,
        => {},
        else => return error.UnsupportedSyntax,
    };
    for (ast.tokens.items(.tag), 0..) |tag, i| {
        if (tag == .identifier and std.mem.startsWith(u8, ast.tokenSlice(@intCast(i)), "@")) {
            // Quoted identifiers use additional normalization paths that are not
            // yet covered by this formatter's exact-token preservation contract.
            return error.UnsupportedSyntax;
        }
    }
}

fn sameStructure(a: Ast, b: Ast) bool {
    const tags = a.nodes.items(.tag);
    if (!std.mem.eql(Ast.Node.Tag, tags, b.nodes.items(.tag)) or
        !std.mem.eql(Ast.Node.Index, a.extra_data, b.extra_data)) return false;
    for (tags, a.nodes.items(.data), b.nodes.items(.data)) |tag, x, y| {
        switch (tag) {
            .identifier, .number_literal, .char_literal, .string_literal => {},
            // The parser leaves unused node data undefined; never read it.
            .negation, .bool_not, .bit_not => if (x.rhs != y.rhs) return false,
            else => if (x.lhs != y.lhs or x.rhs != y.rhs) return false,
        }
    }
    return true;
}

const SignificantTokens = struct {
    tokenizer: Tokenizer,
    previous_end: usize = 0,

    fn next(self: *SignificantTokens) struct { token: Tokenizer.Token, gap: []const u8 } {
        while (true) {
            const token = self.tokenizer.next();
            if (token.tag == .newline) continue;
            const gap = self.tokenizer.buffer[self.previous_end..token.start];
            self.previous_end = token.end;
            return .{ .token = token, .gap = gap };
        }
    }
};

fn sameTokensAndComments(a: [:0]const u8, b: [:0]const u8) bool {
    var left: SignificantTokens = .{ .tokenizer = Tokenizer.init(a) };
    var right: SignificantTokens = .{ .tokenizer = Tokenizer.init(b) };
    while (true) {
        const x = left.next();
        const y = right.next();
        if (x.token.tag != y.token.tag) return false;
        const left_token = a[x.token.start..x.token.end];
        const right_token = b[y.token.start..y.token.end];
        const same_token = if (x.token.tag == .doc_comment)
            std.mem.eql(u8, std.mem.trimEnd(u8, left_token, &std.ascii.whitespace), std.mem.trimEnd(u8, right_token, &std.ascii.whitespace))
        else
            std.mem.eql(u8, left_token, right_token);
        if (!same_token or !sameComments(x.gap, y.gap)) return false;
        if (x.token.tag == .eof) return true;
    }
}

fn nextComment(gap: *[]const u8) ?[]const u8 {
    const start = std.mem.indexOfScalar(u8, gap.*, ';') orelse return null;
    const end = std.mem.indexOfScalarPos(u8, gap.*, start, '\n') orelse gap.len;
    const comment = std.mem.trimEnd(u8, gap.*[start..end], &std.ascii.whitespace);
    gap.* = gap.*[end..];
    return comment;
}

fn sameComments(a: []const u8, b: []const u8) bool {
    var left = a;
    var right = b;
    while (nextComment(&left)) |x| {
        const y = nextComment(&right) orelse return false;
        if (!std.mem.eql(u8, x, y)) return false;
    }
    return nextComment(&right) == null;
}

fn expectFormatted(source: [:0]const u8, expected: []const u8) !void {
    const allocator = std.testing.allocator;
    const formatted = try format(allocator, source);
    defer allocator.free(formatted);
    try std.testing.expectEqualStrings(expected, formatted);
    const terminated = try allocator.dupeZ(u8, formatted);
    defer allocator.free(terminated);
    const again = try format(allocator, terminated);
    defer allocator.free(again);
    try std.testing.expectEqualStrings(formatted, again);
}

test "declarations arithmetic strings and idempotence" {
    try expectFormatted("", "");
    try expectFormatted(" \n\t\n", "");

    try expectFormatted("let answer=1 + 2 * 3\nlet text=\"hello; world\"\n", "let answer = 1 + 2 * 3\nlet text = \"hello; world\"\n");
    try expectFormatted("let mut count:i32= -42\nlet value:i32\nlet ch='a'\n", "let mut count: i32 = -42\nlet value: i32\nlet ch = 'a'\n");
    try expectFormatted("let flag=!ready\nlet bits=~mask\n", "let flag = !ready\nlet bits = ~mask\n");
}

test "comments before between and after declarations" {
    try expectFormatted("; header\n\nlet x=1 ; inline\n; between\nlet y=\"; not a comment\"\n; tail", "; header\n\nlet x = 1 ; inline\n; between\nlet y = \"; not a comment\"\n; tail\n");
    try expectFormatted("; comment only", "; comment only\n");
    try expectFormatted("\n; leading\nlet x=1\n\n; trailing\n", "; leading\nlet x = 1\n\n; trailing\n");
}

test "multiline declarations and operators preserve comments" {
    try expectFormatted("let x=\n  1 +\n  2 *\n  3\n", "let x =\n    1 +\n    2 *\n        3\n");
    try expectFormatted("let x= ; initializer\n  1 + ; addition\n  2\n", "let x = ; initializer\n    1 + ; addition\n    2\n");
}

test "documentation comments" {
    try expectFormatted(";;; Doc   \nlet x=1\n", ";;; Doc\nlet x = 1\n");
    try expectFormatted(";;; Doc\r\nlet x=1\r\n", ";;; Doc\nlet x = 1\n");
    try expectFormatted(";;; First\n;;; Second\nlet x=1\n", ";;; First\n;;; Second\nlet x = 1\n");
    try expectFormatted("\n;;; Leading\nlet x=1\n", ";;; Leading\nlet x = 1\n");
    try expectFormatted("let x=1\n\n;;; Next\nlet y=2\n", "let x = 1\n\n;;; Next\nlet y = 2\n");
}

test "invalid sources are rejected" {
    for ([_][:0]const u8{ "let =\n", "let x=\n", "let x=42", "let x=1\x00let y=2\n" }) |source| {
        try std.testing.expectError(error.InvalidSource, format(std.testing.allocator, source));
    }
}

test "unsupported syntax fails closed including nested expressions" {
    for ([_][:0]const u8{
        "let x=f()\n",                       "let x=1 + f(2)\n",      "let x=a.b\n",
        "let x=[1, 2]\n",                    "let x={ 1 }\n",         "let x=#true\n",
        "let x=if ready { 1 } else { 2 }\n", "@[test]\nlet x=1\n",    "let f=(x:i32) -> i32\n",
        "let x=&value\n",                    "let x:[]i32\n",         "let x=a[0]\n",
        "let x=.value\n",                    "let x=a |> b\n",        "let @\"x\"=1\n",
        "#!nov\nlet x=1\n",                  "\xEF\xBB\xBFlet x=1\n",
    }) |source| {
        var ast = try Parser.parse(std.testing.allocator, source);
        defer ast.deinit(std.testing.allocator);
        if (ast.errors.len != 0) std.debug.print("invalid unsupported test fixture: {s}\n", .{source});
        try std.testing.expectEqual(@as(usize, 0), ast.errors.len);
        try std.testing.expectError(error.UnsupportedSyntax, format(std.testing.allocator, source));
    }
}

test "every unsupported AST tag is rejected before rendering" {
    const supported = [_]Ast.Node.Tag{
        .root,      .decl,          .identifier,   .number_literal,   .char_literal, .string_literal,
        .add,       .sub,           .mul,          .div,              .mod,          .shl,
        .shr,       .bit_and,       .bit_or,       .bit_xor,          .equal_equal,  .bang_equal,
        .less_than, .less_or_equal, .greater_than, .greater_or_equal, .bool_and,     .bool_or,
        .negation,  .bool_not,      .bit_not,
    };
    var ast = try Parser.parse(std.testing.allocator, "let x=1\n");
    defer ast.deinit(std.testing.allocator);
    // Place each tag under a supported root/declaration. No undefined fields
    // of unsupported nodes may be inspected by the preflight check.
    for (std.enums.values(Ast.Node.Tag)) |tag| {
        ast.nodes.items(.tag)[1] = tag;
        if (std.mem.indexOfScalar(Ast.Node.Tag, &supported, tag) != null) {
            try checkSupported(ast);
        } else {
            try std.testing.expectError(error.UnsupportedSyntax, checkSupported(ast));
        }
    }
}

test "whitespace and additional supported operators are idempotent" {
    try expectFormatted("let x=1\n\nlet y=2\n", "let x = 1\n\nlet y = 2\n");
    try expectFormatted("let x=1\r\nlet y=2\r\n", "let x = 1\nlet y = 2\n");
    try expectFormatted("let x=1 +\n 2\n", "let x = 1 +\n    2\n");
    try expectFormatted("let x=a and b or !c\nlet y=a << 2 | b & ~c\nlet z=a <= b\n", "let x = a and b or !c\nlet y = a << 2 | b & ~c\nlet z = a <= b\n");
}

test "preservation checks detect code comment and structural loss" {
    try std.testing.expect(!sameTokensAndComments("let x=1\n", "let x=2\n"));
    try std.testing.expect(!sameTokensAndComments("let x=1\n; tail\n", "let x=1\n"));
    try std.testing.expect(!sameTokensAndComments("let x=1\n;;; docs\nlet y=2\n", "let x=1\nlet y=2\n"));
    try std.testing.expect(!sameTokensAndComments("; head\nlet x=1\n", "let x=1\n; head\n"));
    var a = try Parser.parse(std.testing.allocator, "let x=1 + 2 * 3\n");
    defer a.deinit(std.testing.allocator);
    var b = try Parser.parse(std.testing.allocator, "let x=1 * 2 + 3\n");
    defer b.deinit(std.testing.allocator);
    try std.testing.expect(!sameStructure(a, b));
}
