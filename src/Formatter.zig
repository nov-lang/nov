const std = @import("std");
const Ast = @import("Ast.zig");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");

/// Supports scalar declarations/operators plus function signatures, calls,
/// local blocks, if/else, condition loops, returns, unlabeled loop controls,
/// and direct identifier assignments. Preserves line/doc comments and tokens.
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
        .fn_proto_one,
        .fn_proto,
        .call_one,
        .call_one_comma,
        .call,
        .call_comma,
        .block_two,
        .block_two_newline,
        .block,
        .block_newline,
        .@"if",
        .if_else,
        .loop_condition,
        .@"return",
        .@"break",
        .@"continue",
        .assign,
        .assign_add,
        .assign_sub,
        .assign_mul,
        .assign_div,
        .assign_mod,
        .identifier,
        .number_literal,
        .char_literal,
        .string_literal,
        .builtin_literal,
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
    for (ast.nodes.items(.tag), ast.nodes.items(.data), ast.nodes.items(.main_token)) |tag, data, main| {
        switch (tag) {
            .builtin_literal => {
                const name = ast.tokenSlice(main);
                if (!std.mem.eql(u8, name, "@print") and !std.mem.eql(u8, name, "@println"))
                    return error.UnsupportedSyntax;
            },
            .@"break" => if (data.lhs != 0 or data.rhs != 0) return error.UnsupportedSyntax,
            .@"continue" => if (data.lhs != 0) return error.UnsupportedSyntax,
            .block_two, .block_two_newline, .block, .block_newline => {
                if (main >= 2 and ast.tokens.items(.tag)[main - 1] == .colon)
                    return error.UnsupportedSyntax;
            },
            .assign, .assign_add, .assign_sub, .assign_mul, .assign_div, .assign_mod => {
                if (ast.nodes.items(.tag)[data.lhs] != .identifier) return error.UnsupportedSyntax;
            },
            else => {},
        }
    }
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
            .identifier, .number_literal, .char_literal, .string_literal, .builtin_literal => {},
            // The parser leaves unused node data undefined; never read it.
            .negation, .bool_not, .bit_not, .@"return" => if (x.rhs != y.rhs) return false,
            .@"continue" => if ((x.lhs == 0) != (y.lhs == 0)) return false,
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

test "format declarations without a final newline" {
    try expectFormatted("let x=42", "let x = 42\n");
    try expectFormatted("let x:int", "let x: int\n");
    try expectFormatted("let x=42 ; trailing comment", "let x = 42 ; trailing comment\n");
}

test "invalid sources are rejected" {
    for ([_][:0]const u8{ "let =\n", "let x=\n", "let x=", "let x=1\x00let y=2\n" }) |source| {
        try std.testing.expectError(error.InvalidSource, format(std.testing.allocator, source));
    }
}

test "unsupported syntax fails closed including nested expressions" {
    for ([_][:0]const u8{
        "let x=@import(\"memory\")\n",
        "let x=call({&value})\n",
        "let x=a.b\n",
        "let x=[1, 2]\n",
        "let x=#true\n",
        "@[test]\nlet x=1\n",
        "let f: () -> void = loop true {break 1}\n",
        "let f: () -> void = loop true {continue :outer}\n",
        "let f: () -> void = outer: {}\n",
        "let x=&value\n",
        "let x:[]i32\n",
        "let x=a[0]\n",
        "let x=.value\n",
        "let x=a |> b\n",
        "let @\"x\"=1\n",
        "#!nov\nlet x=1\n",
        "\xEF\xBB\xBFlet x=1\n",
    }) |source| {
        var ast = try Parser.parse(std.testing.allocator, source);
        defer ast.deinit(std.testing.allocator);
        if (ast.errors.len != 0) std.debug.print("invalid unsupported test fixture: {s}\n", .{source});
        try std.testing.expectEqual(@as(usize, 0), ast.errors.len);
        try std.testing.expectError(error.UnsupportedSyntax, format(std.testing.allocator, source));
    }
}

test "executable local blocks round trip" {
    for ([_][:0]const u8{
        "let f: () -> i32 = {\nlet n: i32 = 1\nn\n}\n",
        "let x: i32 = {\nlet n: i32 = 1\nn\n}\n",
    }) |source| {
        var ast = try Parser.parse(std.testing.allocator, source);
        defer ast.deinit(std.testing.allocator);
        try std.testing.expectEqual(@as(usize, 0), ast.errors.len);
        const formatted = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(formatted);
        try expectFormatted(source, formatted);
    }
}

test "every unsupported AST tag is rejected before rendering" {
    const supported = [_]Ast.Node.Tag{
        .root,           .decl,            .identifier,   .number_literal,   .char_literal,      .string_literal,
        .add,            .sub,             .mul,          .div,              .mod,               .shl,
        .shr,            .bit_and,         .bit_or,       .bit_xor,          .equal_equal,       .bang_equal,
        .less_than,      .less_or_equal,   .greater_than, .greater_or_equal, .bool_and,          .bool_or,
        .negation,       .bool_not,        .bit_not,      .fn_proto_one,     .fn_proto,          .call_one,
        .call_one_comma, .call,            .call_comma,   .block_two,        .block_two_newline, .block,
        .block_newline,  .@"if",           .if_else,      .loop_condition,   .@"return",         .@"break",
        .@"continue",    .assign,          .assign_add,   .assign_sub,       .assign_mul,        .assign_div,
        .assign_mod,     .builtin_literal,
    };
    var ast = try Parser.parse(std.testing.allocator, "let x=1\n");
    defer ast.deinit(std.testing.allocator);
    // Place each tag under a supported root/declaration. No undefined fields
    // of unsupported nodes may be inspected by the preflight check.
    for (std.enums.values(Ast.Node.Tag)) |tag| {
        ast.nodes.items(.tag)[1] = tag;
        if (std.mem.indexOfScalar(Ast.Node.Tag, &supported, tag) != null) {
            // This synthetic leaf has no valid child/label data for these tags.
            if (tag == .builtin_literal or tag == .@"break" or tag == .@"continue" or tag == .assign or
                tag == .assign_add or tag == .assign_sub or tag == .assign_mul or
                tag == .assign_div or tag == .assign_mod) continue;
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

test "function signatures calls and delimiters are idempotent" {
    try expectFormatted("let f:(x:int,y:int)->int=x + y\nlet main:()->void=@println(f(1,2,),)\n", "let f: (x: int, y: int) -> int = x + y\nlet main: () -> void = @println(f(1, 2,),)\n");
    try expectFormatted("let f:(cb:(int)->int,x:int)->int=cb(x)\n", "let f: (cb: (int) -> int, x: int) -> int = cb(x)\n");
    try expectFormatted("let f:(\n x:int, ; param\n y:int,\n)->int=add(\n x, ; arg\n y,\n)\n", "let f: (\n    x: int, ; param\n    y: int,\n) -> int = add(\n    x, ; arg\n    y,\n)\n");
    try expectFormatted("let f:()->void=@println(\n; empty\n)\n", "let f: () -> void = @println(\n    ; empty\n)\n");
}

test "blocks controls assignments and precedence are idempotent" {
    try expectFormatted("let f:(n:int)->int={let mut x:int=1\nx += n * 2\nif x > 0 {return x} else if x == 0 {return -1} else {return 0}}\n", "let f: (n: int) -> int = {\n    let mut x: int = 1\n    x += n * 2\n    if x > 0 {\n        return x\n    } else if x == 0 {\n        return -1\n    } else {\n        return 0\n    }\n}\n");
    try expectFormatted("let main:()->void={loop {true} {if false {break} else {continue}}\nreturn}\n", "let main: () -> void = {\n    loop {\n        true\n    } {\n        if false {\n            break\n        } else {\n            continue\n        }\n    }\n    return\n}\n");
    try expectFormatted("let x={1 + 2} * 3 - 4 / {5 - 6}\n", "let x = {\n    1 + 2\n} * 3 - 4 / {\n    5 - 6\n}\n");
    try expectFormatted("let f:()->bool=false and {return true} or !false\n", "let f: () -> bool = false and {\n    return true\n} or !false\n");
}

test "nested list expansion and empty block comments stabilize on the first pass" {
    try expectFormatted("let f:()->void=@println({let x:int=1\nx})\n", "let f: () -> void = @println(\n    {\n        let x: int = 1\n        x\n    }\n)\n");
    try expectFormatted("let f:()->void={\n; empty body\n}\n", "let f: () -> void = {\n    ; empty body\n}\n");
    try expectFormatted("let f:()->void={}\nlet g:(int,int)->int=call(1,2)\n", "let f: () -> void = {}\nlet g: (int, int) -> int = call(1, 2)\n");
    for ([_][:0]const u8{
        "let f:()->void={loop true {let x:int={break}\n@println(x)}}",
        "let f:()->void={loop true {loop false {continue}\nbreak}}",
        "let f:()->void={return\n}\nlet g:()->void={}",
        "let f:(\n)->void=call(\n)",
        "let f:(\n x:int\n)->int=call(\n x\n)",
        "let f:()->void=call(1 ; before comma\n,2 ; before close\n)",
        "let f:(x:int)->int={let n:int=x\n + 1\nn}",
    }) |source| {
        const formatted = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(formatted);
        try expectFormatted(source, formatted);
    }
}

test "comments and continuations inside executable forms survive" {
    const source = ";;; function\nlet f:(x:int)->int={ ; open\n; local\nlet mut n:int= ; init\n x + ; add\n 1\n\nn *= ; store\n 2\nif n > 0 {; then\nreturn ; result\n n ; value\n} else {return 0} ; branch\n} ; end\n";
    const formatted = try format(std.testing.allocator, source);
    defer std.testing.allocator.free(formatted);
    try expectFormatted(source, formatted);
}

fn testExecutableAllocationFailures(allocator: std.mem.Allocator) !void {
    const output = try format(allocator, "let f:(n:int)->int={let mut x:int=n\nloop x > 0 {x -= 1\nif x == 2 {break}\ncontinue}\nreturn x}\n");
    defer allocator.free(output);
}

test "executable formatting survives allocation failures" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testExecutableAllocationFailures, .{});
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
