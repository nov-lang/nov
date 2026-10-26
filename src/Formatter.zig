const std = @import("std");
const TestAllocations = @import("TestAllocations.zig");
const Ast = @import("Ast.zig");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const primitives = @import("primitives.zig");
const Literal = @import("Literal.zig");
const TypeResolver = @import("TypeResolver.zig");

/// Supports scalar declarations/operators plus function signatures, calls,
/// local blocks, if/else, loops with optional conditions/continuations, returns,
/// unlabeled loop controls (including value-bearing breaks),
/// direct identifier assignments, scalar array types/literals/indexing/.len,
/// membership, bounded integer range loops, scalar match/prongs, unary pipes,
/// and indexed assignments to direct identifiers.
/// Formatting is syntactic,
/// not a scope, mutability, or type check. Preserves line/doc comments and tokens.
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
    const terminated = try allocator.dupeSentinel(u8, rendered, 0);
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

fn fieldDefaultSupported(ast: Ast, node: Ast.Node.Index) bool {
    var current = node;
    var buffer: [2]Ast.Node.Index = undefined;
    while (ast.fullBlock(&buffer, current)) |block| {
        if (block.ast.statements.len != 1) return false;
        current = block.ast.statements[0];
    }
    const tag = ast.nodes.items(.tag)[current];
    if (tag == .negation) return ast.nodes.items(.tag)[ast.nodes.items(.data)[current].rhs] == .number_literal;
    if (tag == .number_literal or tag == .enum_literal) return true;
    if (tag == .field_access) return ast.nodes.items(.tag)[ast.nodes.items(.data)[current].lhs] == .identifier;
    if (tag == .string_literal) return !Literal.hasInterpolation(ast.tokenSlice(ast.firstToken(current)));
    if (tag == .identifier) {
        const name = ast.tokenSlice(ast.firstToken(current));
        return std.mem.eql(u8, name, "true") or std.mem.eql(u8, name, "false");
    }
    return false;
}

fn checkSupported(ast: Ast) !void {
    // Inspect ALL nodes, not only roots: unsupported expressions can be nested
    // inside otherwise renderable declarations and operators. New tags fail closed.
    for (ast.nodes.items(.tag)) |tag| switch (tag) {
        .root,
        .container_decl,
        .container_decl_arg,
        .container_field,
        .unwrap,
        .enum_literal,
        .attr_one,
        .attr_decl_one,
        .attr_decl,
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
        .match,
        .match_case_one,
        .match_case,
        .function_pipe,
        .range,
        .loop_range,
        .loop_condition,
        .loop_cont,
        .loop_else,
        .@"return",
        .@"break",
        .@"continue",
        .assign,
        .assign_add,
        .assign_sub,
        .assign_mul,
        .assign_div,
        .assign_mod,
        .array_type,
        .array_init_two,
        .array_init_two_comma,
        .array_init,
        .array_init_comma,
        .array_access,
        .field_access,
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
        .in,
        .equal_equal,
        .bang_equal,
        .less_than,
        .less_or_equal,
        .greater_than,
        .greater_or_equal,
        .bool_and,
        .bool_or,
        .ref_of,
        .negation,
        .bool_not,
        .bit_not,
        => {},
        else => return error.UnsupportedSyntax,
    };
    for (ast.nodes.items(.tag), ast.nodes.items(.data), ast.nodes.items(.main_token), 0..) |tag, data, main, i| {
        switch (tag) {
            .ref_of => {
                if (ast.nodes.items(.tag)[data.rhs] != .identifier and
                    (ast.nodes.items(.tag)[data.rhs] != .array_access or
                        ast.nodes.items(.tag)[ast.nodes.items(.data)[data.rhs].lhs] != .identifier)) return error.UnsupportedSyntax;
                var binding = false;
                for (ast.nodes.items(.tag), 0..) |parent_tag, parent| {
                    if (parent_tag == .decl and ast.decl(@intCast(parent)).ast.init_node == i) {
                        binding = true;
                        break;
                    }
                }
                if (!binding) return error.UnsupportedSyntax;
            },
            .container_decl => {
                for (ast.fullStruct(@intCast(i)).?) |member| {
                    if (ast.nodes.items(.tag)[member] == .container_field) continue;
                    const declaration = ast.fullDecl(member) orelse return error.UnsupportedSyntax;
                    if (!TypeResolver.isThisAlias(&ast, member) and (declaration.ast.is_mutable or declaration.ast.type_node == 0 or ast.fullFnProto(declaration.ast.type_node) == null)) return error.UnsupportedSyntax;
                }
            },
            .container_field => if (data.lhs != 0) {
                const field_tag = ast.nodes.items(.tag)[data.lhs];
                switch (field_tag) {
                    .identifier, .field_access, .call, .call_one => {},
                    else => return error.UnsupportedSyntax,
                }
                const name = ast.tokenSlice(ast.firstToken(data.lhs));
                if (field_tag == .identifier and primitives.isPrimitive(name)) {
                    const allowed = [_][]const u8{ "void", "bool", "string", "int", "uint", "i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128" };
                    for (allowed) |scalar| {
                        if (std.mem.eql(u8, name, scalar)) break;
                    } else return error.UnsupportedSyntax;
                }
                if (data.rhs != 0 and !fieldDefaultSupported(ast, data.rhs)) return error.UnsupportedSyntax;
            },
            .array_type => {
                switch (ast.nodes.items(.tag)[data.rhs]) {
                    .field_access, .call, .call_one => continue,
                    .identifier => {},
                    else => return error.UnsupportedSyntax,
                }
                const name = ast.tokenSlice(ast.nodes.items(.main_token)[data.rhs]);
                const scalar_types = [_][]const u8{ "bool", "string", "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "int", "uint" };
                for (scalar_types) |scalar| {
                    if (std.mem.eql(u8, name, scalar)) break;
                } else if (primitives.isPrimitive(name)) return error.UnsupportedSyntax;
            },
            .array_init_two, .array_init_two_comma, .array_init, .array_init_comma => {
                const buffer = [_]Ast.Node.Index{ data.lhs, data.rhs };
                const items = switch (tag) {
                    .array_init_two, .array_init_two_comma => buffer[0..@as(usize, if (data.rhs != 0) 2 else if (data.lhs != 0) 1 else 0)],
                    else => ast.extra_data[data.lhs..data.rhs],
                };
                for (items) |item| switch (ast.nodes.items(.tag)[item]) {
                    .array_init_two, .array_init_two_comma, .array_init, .array_init_comma => return error.UnsupportedSyntax,
                    else => {},
                };
            },
            .array_access => if (ast.nodes.items(.tag)[data.lhs] == .array_access) return error.UnsupportedSyntax,
            .attr_one => if (data.rhs != 0 or (!std.mem.eql(u8, ast.tokenSlice(data.lhs), "public") and !std.mem.eql(u8, ast.tokenSlice(data.lhs), "noreturn"))) return error.UnsupportedSyntax,
            .builtin_literal => {
                const name = ast.tokenSlice(main);
                if (!std.mem.eql(u8, name, "@print") and !std.mem.eql(u8, name, "@println") and !std.mem.eql(u8, name, "@as") and !std.mem.eql(u8, name, "@panic") and !std.mem.eql(u8, name, "@This") and !std.mem.eql(u8, name, "@import"))
                    return error.UnsupportedSyntax;
            },
            .@"break" => if (data.lhs != 0) return error.UnsupportedSyntax,
            .@"continue" => if (data.lhs != 0) return error.UnsupportedSyntax,
            .block_two, .block_two_newline, .block, .block_newline => {
                if (ast.firstToken(@intCast(i)) != main)
                    return error.UnsupportedSyntax;
            },
            .assign, .assign_add, .assign_sub, .assign_mul, .assign_div, .assign_mod => {
                const lhs_tag = ast.nodes.items(.tag)[data.lhs];
                if (lhs_tag == .identifier) continue;
                if (lhs_tag != .array_access and lhs_tag != .field_access) return error.UnsupportedSyntax;
                var base = ast.nodes.items(.data)[data.lhs].lhs;
                if (lhs_tag == .field_access) {
                    while (ast.nodes.items(.tag)[base] == .field_access) base = ast.nodes.items(.data)[base].lhs;
                }
                if (ast.nodes.items(.tag)[base] != .identifier) return error.UnsupportedSyntax;
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
        a.extra_data.len != b.extra_data.len) return false;
    if (!std.mem.eql(Ast.Node.Index, a.extra_data, b.extra_data)) return false;
    for (tags, a.nodes.items(.data), b.nodes.items(.data)) |tag, x, y| {
        switch (tag) {
            .attr_one => if (!std.mem.eql(u8, a.tokenSlice(x.lhs), b.tokenSlice(y.lhs)) or x.rhs != y.rhs) return false,
            .identifier, .number_literal, .char_literal, .string_literal, .builtin_literal, .enum_literal => {},
            // The parser leaves unused node data undefined; never read it.
            .ref_of, .negation, .bool_not, .bit_not, .@"return", .array_type => if (x.rhs != y.rhs) return false,
            .field_access, .unwrap => if (x.lhs != y.lhs or !std.mem.eql(u8, a.tokenSlice(x.rhs), b.tokenSlice(y.rhs))) return false,
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
    const terminated = try allocator.dupeSentinel(u8, formatted, 0);
    defer allocator.free(terminated);
    const again = try format(allocator, terminated);
    defer allocator.free(again);
    try std.testing.expectEqualStrings(formatted, again);
}

test "generic union signatures instantiations and propagation format idempotently" {
    try expectFormatted("let Result:(T:type,E:type)->type=union{ok:T,err:E,}\nlet R=Result(int,string)\nlet f:(r:R)->R=.ok[r.!]", "let Result: (T: type, E: type) -> type = union {\n    ok: T,\n    err: E,\n}\nlet R = Result(int, string)\nlet f: (r: R) -> R = .ok[r.!]\n");
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{"let G:(T:type)->type=union{ok:T,err:void,}\nlet R=G(int)\nlet f:(r:R)->R=G(int).ok[r.!]"});
}

test "unions constructors captures and payloadless variants format idempotently" {
    try expectFormatted("let U=union{ok:int,err:string,none,}\nlet f:(u:U)->int=match u{.ok=>|v|v\n.err=>0\n.none=>1}\nlet g:()->U=.none[]", "let U = union {\n    ok: int,\n    err: string,\n    none,\n}\nlet f: (u: U) -> int = match u {\n    .ok => |v| v\n    .err => 0\n    .none => 1\n}\nlet g: () -> U = .none[]\n");
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{"let U=union{ok:int,none,}\nlet f:(u:U)->int=match u{.ok=>|v|v\n.none=>0}"});
}

test "declarations arithmetic strings and idempotence" {
    try expectFormatted("", "");
    try expectFormatted(" \n\t\n", "");

    try expectFormatted("let answer=1 + 2 * 3\nlet text=\"hello; world\"\n", "let answer = 1 + 2 * 3\nlet text = \"hello; world\"\n");
    try expectFormatted("let mut count:i32= -42\nlet value:i32\nlet ch='a'\n", "let mut count: i32 = -42\nlet value: i32\nlet ch = 'a'\n");
    try expectFormatted("let flag=!ready\nlet bits=~mask\n", "let flag = !ready\nlet bits = ~mask\n");
}

test "bitwise syntax roundtrips shared precedence and match alternatives" {
    try expectFormatted("let f:(a:i8,b:i8)->bool=~a&b^1|2==0", "let f: (a: i8, b: i8) -> bool = ~a & b ^ 1 | 2 == 0\n");
    try expectFormatted("let f:(a:i8)->i8=match a{0|1=>a&3\n_=>a^2}", "let f: (a: i8) -> i8 = match a {\n    0 | 1 => a & 3\n    _ => a ^ 2\n}\n");
}

test "string compounds format syntactically without semantic restrictions" {
    try expectFormatted("let f:()->void={let mut s=\"é\\0\"\ns+=s\ns+={s=\"new\"\n\"😀\"}}", "let f: () -> void = {\n    let mut s = \"é\\0\"\n    s += s\n    s += {\n        s = \"new\"\n        \"😀\"\n    }\n}\n");
    try expectFormatted("let f:(s:string)->void={s+=true\ns-=\"a\"}", "let f: (s: string) -> void = {\n    s += true\n    s -= \"a\"\n}\n");
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

test "static integer borrow formatting is idempotent" {
    const source: [:0]const u8 = "let main:()={let mut x:uint=1\nlet earlierx=x\nlet y=&x\ny+=1\n@println(x,earlierx)}";
    const formatted = try format(std.testing.allocator, source);
    defer std.testing.allocator.free(formatted);
    const terminated = try std.testing.allocator.dupeSentinel(u8, formatted, 0);
    defer std.testing.allocator.free(terminated);
    const again = try format(std.testing.allocator, terminated);
    defer std.testing.allocator.free(again);
    try std.testing.expectEqualStrings(formatted, again);
    try std.testing.expect(std.mem.indexOf(u8, formatted, "let y = &x") != null);
}

test "unsupported syntax fails closed including nested expressions" {
    for ([_][:0]const u8{
        "let x=@unknown(\"memory\")\n",
        "let x=call({&value})\n",
        "let x=a[0..2].b\n",
        "let x=[[1], [2]]\n",
        "let x:[][]int\n",
        "let x:[]i128\n",
        "let x:[]*int\n",
        "let x:[]() -> int\n",
        "let x=a[0][1]\n",
        "let x=a[0..1]\n",
        "let f:()->void={make().inner.len=1}\n",
        "let f:()->void={make()[0]+=1}\n",
        "let f:()->void={a[0][0]-=1}\n",
        "let f:()->void={make().inner.len%=1}\n",
        "let f:()->void={make()[0]=1}\n",
        "let x=#true\n",
        "@[test]\nlet x=1\n",
        "let f: () -> void = loop true {break :outer 1}\n",
        "let f: () -> void = loop true {continue :outer}\n",
        "let f: () -> void = outer: {}\n",
        "let x=&value.field\n",

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

test "nominal struct formatting preserves public attributes fields defaults methods constructor comments and interpolation" {
    for ([_][:0]const u8{
        "let R=struct{lo:i128=-170141183460469231731687303715884105728,top:u128=340282366920938463463374607431768211455,}\nlet f:(r:R)={}",
        "@[public]\nlet R=struct{; open\nx:int=0, ; default\n; next field\ny:string,\n\n@[public]\nlet toString:(self:R)->string=\"{self.x}:{self.y}\"\n}\nlet main:()={let r=R[.x=1,.y=\"hello\"]\n@println(r)}",
        "let R=struct{\n; empty\n}\nlet main:()={let r=R[\n; empty constructor\n]\n@println(true)}",
        "let R=struct{x:string,}\nlet main:()={let r=R[\n.x=\"hello\", ; value\n]\n@println(r.x)}",
    }) |source| {
        const result = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(result);
        try expectFormatted(source, result);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
    }
}

test "ascription preserves type syntax comments nested calls interpolation and allocation cleanup" {
    for ([_][:0]const u8{
        "let a=@as(\n[]int, ; type\n[1,2],)\nlet f=@as((n:int)->int,id)\nlet s=\"{@as(int, 1)}\"",
        "let n=@as(int,@as(int,1))\nlet f=@as(()->void,unit)",
    }) |source| {
        const result = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(result);
        try expectFormatted(source, result);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
    }
}

fn testMatchFormatAllocationFailures(allocator: std.mem.Allocator, source: [:0]const u8) !void {
    const result = try format(allocator, source);
    defer allocator.free(result);
}

test "match and pipes preserve comments tokens private blocks and multiline prongs" {
    for ([_][:0]const u8{
        "let f:(n:int)->int=match n {0 | 1=>n\n_=>n}\nlet main:()=10 |> f |> @println\n",
        "let f:(n:int)->int=match n, ; subject\nn {; prongs\n0 | 1, ; alternatives and position\n_, => {; block\nlet x:int=n\nx} ; selected\n; fallback\n_ => n ; tail\n}\n",
        "let f:(n:int)->int=match n {0=>match n {0=>1\n_=>2}\n_=>3}\n",
        "let x=match true {} ; empty\n",
        "let x=match true {; empty\n}\n",
    }) |source| {
        const result = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(result);
        try expectFormatted(source, result);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
    }
}

test "inferred local declarations retain omitted annotations" {
    try expectFormatted("let f:(n:int)->void={let yes=true\nlet mut value=1+n\nlet callback=f\nlet a:[]int=[n]\nlet mut copy=a\nvalue+=copy[0]\n@println(value,yes)}\n", "let f: (n: int) -> void = {\n    let yes = true\n    let mut value = 1 + n\n    let callback = f\n    let a: []int = [n]\n    let mut copy = a\n    value += copy[0]\n    @println(value, yes)\n}\n");
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
        .root,            .decl,           .identifier,         .number_literal,       .char_literal,   .string_literal,
        .add,             .sub,            .mul,                .div,                  .mod,            .shl,
        .shr,             .bit_and,        .bit_or,             .bit_xor,              .equal_equal,    .bang_equal,
        .less_than,       .less_or_equal,  .greater_than,       .greater_or_equal,     .bool_and,       .bool_or,
        .ref_of,          .negation,       .bool_not,           .bit_not,              .fn_proto_one,   .fn_proto,
        .call_one,        .call_one_comma, .call,               .call_comma,           .block_two,      .block_two_newline,
        .block,           .block_newline,  .@"if",              .if_else,              .range,          .loop_range,
        .loop_condition,  .loop_cont,      .loop_else,          .@"return",            .@"break",       .@"continue",
        .assign,          .assign_add,     .assign_sub,         .assign_mul,           .assign_div,     .assign_mod,
        .builtin_literal, .array_type,     .array_init_two,     .array_init_two_comma, .array_init,     .array_init_comma,
        .array_access,    .field_access,   .in,                 .match,                .match_case_one, .match_case,
        .function_pipe,   .container_decl, .container_decl_arg, .container_field,      .unwrap,         .enum_literal,
        .attr_one,        .attr_decl_one,  .attr_decl,
    };
    var ast = try Parser.parse(std.testing.allocator, "let x=1\n");
    defer ast.deinit(std.testing.allocator);
    // Place each tag under a supported root/declaration. No undefined fields
    // of unsupported nodes may be inspected by the preflight check.
    for (std.enums.values(Ast.Node.Tag)) |tag| {
        ast.nodes.items(.tag)[1] = tag;
        if (std.mem.indexOfScalar(Ast.Node.Tag, &supported, tag) != null) {
            // This synthetic leaf has no valid child/label data for these tags.
            if (tag == .ref_of or tag == .container_decl or tag == .attr_one or tag == .container_field or tag == .builtin_literal or tag == .@"break" or tag == .@"continue" or tag == .assign or
                tag == .assign_add or tag == .assign_sub or tag == .assign_mul or
                tag == .assign_div or tag == .assign_mod or tag == .array_type or
                tag == .array_init_two or tag == .array_init_two_comma or tag == .array_init or
                tag == .array_init_comma or tag == .array_access or tag == .field_access) continue;
            try checkSupported(ast);
        } else {
            try std.testing.expectError(error.UnsupportedSyntax, checkSupported(ast));
        }
    }
}

test "membership formatting preserves syntactic operands and range expressions" {
    try expectFormatted("let f:(a:[]i8)->bool=-1 in a\n", "let f: (a: []i8) -> bool = -1 in a\n");
    try expectFormatted("let f:()->bool=true in [false,true,]\n", "let f: () -> bool = true in [false, true,]\n");
    try expectFormatted("let f:(n:int)->bool=n in [\n; values\n1,2,]\n", "let f: (n: int) -> bool = n in [\n    ; values\n    1,\n    2,\n]\n");
    try expectFormatted("let f: (n: int) -> bool = n in 1..3\n", "let f: (n: int) -> bool = n in 1..3\n");
    try std.testing.expectError(error.UnsupportedSyntax, format(std.testing.allocator, "let f: (n: int, a: []int) -> bool = n in a[1..3]\n"));
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
        "let f:()->void=loop {}",
        "let f:()->void=loop ; body\n{; entry\nloop false {continue}\nbreak}",
        "let f:()->void={loop {break} ; end\n{}}",
        "let f:()->void=loop {true} {loop {break}\nbreak}",
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

test "standalone block comments use statement indentation" {
    try expectFormatted(
        "let main:()->void={@println(true)\n; @println(\"{describe(1, 5, 5)}\\n{describe(5, 1, 3)}\")\nlet max:u64=1\n; before close\n}\n",
        "let main: () -> void = {\n    @println(true)\n    ; @println(\"{describe(1, 5, 5)}\\n{describe(5, 1, 3)}\")\n    let max: u64 = 1\n    ; before close\n}\n",
    );
    const expected =
        \\let main: () -> void = {
        \\    ; before first statement
        \\    let mut n = 0 ; inline declaration
        \\    ; after declaration
        \\    if true {
        \\        ; before loop
        \\        loop n < 2 {
        \\            n += 1 ; inline assignment
        \\            ; after assignment
        \\            @println(n) ; inline call
        \\            ; after call
        \\            continue ; inline continue
        \\            ; before loop close
        \\        }
        \\        ; after loop
        \\    } else {
        \\        ; empty else block
        \\    }
        \\    ; after if
        \\    match n {
        \\        0 => {
        \\            ; before nested block
        \\            {
        \\                @println(n)
        \\                ; before nested close
        \\            }
        \\            ; before prong close
        \\        }
        \\        ; between prongs
        \\        _ => {
        \\            ; empty prong block
        \\        }
        \\    }
        \\    ; after match
        \\    return ; inline return
        \\    ; before function close
        \\}
        \\; after function
        \\
    ;
    try expectFormatted(expected, expected);
}

test "comments and continuations inside executable forms survive" {
    const source = ";;; function\nlet f:(x:int)->int={ ; open\n; local\nlet mut n:int= ; init\n x + ; add\n 1\n\nn *= ; store\n 2\nif n > 0 {; then\nreturn ; result\n n ; value\n} else {return 0} ; branch\n} ; end\n";
    const formatted = try format(std.testing.allocator, source);
    defer std.testing.allocator.free(formatted);
    try expectFormatted(source, formatted);
}

fn testExecutableAllocationFailures(allocator: std.mem.Allocator) !void {
    const output = try format(allocator, "let f:(n:int)->int={let mut x:int=n\nloop {if x == 0 {break}\nx -= 1\nif x == 2 {break}\ncontinue}\nreturn x}\n");
    defer allocator.free(output);
}

test "executable formatting survives allocation failures" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testExecutableAllocationFailures, .{});
}

test "scalar arrays render types literals indexing length and direct stores" {
    try expectFormatted("let f:()->void={a[-1]+=1\na[0]-=2\na[0]*=3\na[0]/=4\na[0]%=5}\n", "let f: () -> void = {\n    a[-1] += 1\n    a[0] -= 2\n    a[0] *= 3\n    a[0] /= 4\n    a[0] %= 5\n}\n");
    try expectFormatted("let f:(a:[]int)->[]bool={let mut b:[]bool=[true,false,]\nb[-1]=!a[0] == 0\nb += []\nreturn b}\n", "let f: (a: []int) -> []bool = {\n    let mut b: []bool = [true, false,]\n    b[-1] = !a[0] == 0\n    b += []\n    return b\n}\n");
    try expectFormatted("let x:[]int=[]\nlet y=[1,2,3,]\nlet n=make()[0] + -a[-1] * a.len\n", "let x: []int = []\nlet y = [1, 2, 3,]\nlet n = make()[0] + -a[-1] * a.len\n");
    for ([_][]const u8{ "bool", "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "int", "uint" }) |name| {
        const source = try std.fmt.allocPrintSentinel(std.testing.allocator, "let f:(a:[]{s})->[]{s}=a\n", .{ name, name }, 0);
        defer std.testing.allocator.free(source);
        const formatted = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(formatted);
        try expectFormatted(source, formatted);
    }
}

test "array comments continuations and expanding lists stabilize on the first pass" {
    try expectFormatted("let x:[]int=[\n; empty\n]\n", "let x: []int = [\n    ; empty\n]\n");
    try expectFormatted("let x=[{1}]\n", "let x = [\n    {\n        1\n    }\n]\n");
    for ([_][:0]const u8{
        "let f:(a:[]int)->int=a[\n -1]\n",
        "let f:(a:[]int)->int=a[; index\n -1].len\n",
        "let f:()->[]int=[\n1, ; first\n2 ; before comma\n,3, ; tail\n]\n",
        "let f:()->[]int=[1 + ; operator\n 2,3]\n",
        "let f:(a:[]int)->int=a[0 +\n 1]\n",
        "let f:(a:[]int)->int=a[1 + {2}]\n",
        "let f:(a:[]int)->int=a[{1}]\n",
        "let f:(a:[]int)->int=a[0].len + -a[-1]\n",
        "let f:()->void={let mut a:[]int= ; literal\n [1,2]\na[-1]= ; store\n 3\na += ; append\n []}\n",
        "let f:()->void={a[; index\n -1] += ; RHS\n {2}\na[0] -= ; subtract\n 1\na[0] *= 2\na[0] /= 3\na[0] %= 4}\n",
        "let f:()->void=call([1,{2}],[])\n",
        "let f:()->[]int=[1,{let x:int=2\nx},3,]\n",
    }) |source| {
        const formatted = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(formatted);
        try expectFormatted(source, formatted);
    }
}

fn testArrayAllocationFailures(allocator: std.mem.Allocator) !void {
    const output = try format(allocator, "let f:(a:[]int)->[]int={let mut b:[]int=[\n1, ; element\n{a[-1]},3,]\nb[0]=a.len\nb[-1] += {a[0]}\nb[0] -= 1\nb[0] *= 2\nb[0] /= 2\nb[0] %= 3\nb += []\nlet found = 1 in b\nlet absent = false in []\nreturn b}\n");
    defer allocator.free(output);
}

test "array formatting survives every allocation failure" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testArrayAllocationFailures, .{});
}

test "record interpolation formatting preserves captures aliases conversions and allocation cleanup" {
    const source = "let Alias=Error\nlet Error=struct{s:string,\nlet toString:(e:Alias)->string=e.s\n}\nlet U=union{err:Error,}\nlet f:(u:U)=match u{.err=>|err|@panic(\"Error: {err}\")}\nlet g:(e:Alias)->string=\"{e} {Error.toString(e)} {e}\"";
    const formatted = try format(std.testing.allocator, source);
    defer std.testing.allocator.free(formatted);
    try expectFormatted(source, formatted);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
}

test "interpolation formatting preserves literal chunks escapes and placeholders" {
    try expectFormatted("let f:(n:int,s:string)->string=\"é\\{\\}\\0{n}:{s}😀\"", "let f: (n: int, s: string) -> string = \"é\\{\\}\\0{n}:{s}😀\"\n");
}

test "conditional loop exhaustion formatting preserves tokens comments and structure" {
    try expectFormatted("let x=loop false{}else{true}", "let x = loop false {} else {\n    true\n}\n");
    for ([_][:0]const u8{
        "let x=loop c : step() {break 1} else {2}",
        "let x=loop {true} : {tick()} {break [1,2,]} else {[]}",
        "let x=loop c {; body\nbreak true ; break\n} else {; fallback\nfalse}",
        "let x=loop c {} else if b {return} else {loop d {} else {continue}}",
    }) |source| {
        const formatted = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(formatted);
        const terminated = try std.testing.allocator.dupeSentinel(u8, formatted, 0);
        defer std.testing.allocator.free(terminated);
        const twice = try format(std.testing.allocator, terminated);
        defer std.testing.allocator.free(twice);
        try std.testing.expectEqualStrings(formatted, twice);
    }
}

test "loop continuations and value breaks are idempotent" {
    try expectFormatted("let value=loop i<10:i+=1{break 1+2*3}", "let value = loop i < 10 : i += 1 {\n    break 1 + 2 * 3\n}\n");
    try expectFormatted("let value=loop:tick(){break 42}", "let value = loop : tick() {\n    break 42\n}\n");
    for ([_][:0]const u8{
        "let value=loop {break 1 + 2 * 3}",
        "let value=loop : tick(1,2,) {break loop : next() {break 7}}",
        "let value=loop true : a[0] += ; step\n1 {continue}",
        "let value=loop {true} : tick() {; body\nbreak [1,2,3,]}",
        "let value=loop ; condition\ntrue : tick() {; entry\nbreak ; value\n1 + ; add\n2 * 3 ; tail\n} ; end",

        "let value=loop true : tick(; argument\n1,2,) {break call(1,2,)}",
        "let value=loop true : i += 1 + ; RHS\n2 {break {1 + 2} * 3}",
        "let value=[loop : tick() {break 1},loop {break 2},3]",
        "let value=call(loop : tick() {break 1},loop {break 2},)",
    }) |source| {
        const formatted = format(std.testing.allocator, source) catch |err| {
            std.debug.print("loop format fixture: {s}\n", .{source});
            return err;
        };
        defer std.testing.allocator.free(formatted);
        try expectFormatted(source, formatted);
    }
}

test "range headers preserve comments newlines tokens and AST on the first pass" {
    for ([_][:0]const u8{
        "let f: ()=loop i in 0..10 {@println(i)}",
        "let f: ()=loop _ in ; lower\n-{{128}}.. ; upper\n0 {break} ; done",
        "let f: ()=loop i in {let a: uint = 0\na}..{limit()} {loop j in 0..i {continue}}",
        "let f: ()=loop i in begin(; argument\n0)..end(10,) {; body\n@println(i)}",
    }) |source| {
        const formatted = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(formatted);
        try expectFormatted(source, formatted);
    }
}

test "inclusive range formatting preserves comments and is idempotent" {
    const source = "let f: ()=loop _ in ; lower\n-{{128}}..= ; upper\n0 {continue}";
    const formatted = try format(std.testing.allocator, source);
    defer std.testing.allocator.free(formatted);
    try expectFormatted(source, formatted);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{"let f: ()=loop i in 0..= ; upper\n2 {loop j in 0..=i {continue}}"});
    try expectFormatted("let x=0..=2", "let x = 0..=2\n");
}

test "range else formatting preserves comments newlines and idempotent metadata" {
    for ([_][:0]const u8{
        "let f: ()=loop i in ; lower\n{0}.. ; upper\n2 {break true} else {; fallback\nfalse}",
        "let f: ()=loop _ in 0..=2 {continue} else if ready {; branch\ntrue} else {false} ; done",
        "let f: ()=loop i in 0..=2 {loop j in 0..i {} else {continue}} else {loop _ in 0..0 {} else {return}}",
    }) |source| {
        const formatted = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(formatted);
        try expectFormatted(source, formatted);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
    }
}

test "range formatting survives every allocation failure" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{"let f: ()=loop _ in ; lower\n-{{128}}.. ; upper\n0 {loop i in 0..2 {continue}}"});
}

test "bare block continuations format without accepting genuine labels" {
    try expectFormatted("let value=loop ready:{tick()}{break 1}", "let value = loop ready : {\n    tick()\n} {\n    break 1\n}\n");
    try expectFormatted("let value=loop:{}{break 1}", "let value = loop : {} {\n    break 1\n}\n");
    for ([_][:0]const u8{
        "let value=loop i < limit : {i += 1} {break 1}",
        "let value=loop true : {} {break 1}",
        "let value=loop ready : {1} + 2 {break 1}",

        "let value=loop {true} : {tick()} {break 1}",
        "let value=loop ready : {; step\nloop inner : {tick()} {break}} {break 1}",
        "let value=loop ready : {tick() ; end step\n} {break 1}",
    }) |source| {
        const formatted = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(formatted);
        try expectFormatted(source, formatted);
    }
    for ([_][:0]const u8{
        "let value=label: {}",
        "let value=loop ready : label: {} {break 1}",
        "let value=loop : label: {} {break 1}",
        "let value=loop {label: {true}} : {} {break 1}",
        "let value=loop ready : {label: {tick()}} {break 1}",
    }) |source| {
        var ast = try Parser.parse(std.testing.allocator, source);
        defer ast.deinit(std.testing.allocator);
        try std.testing.expectEqual(@as(usize, 0), ast.errors.len);
        try std.testing.expectError(error.UnsupportedSyntax, format(std.testing.allocator, source));
    }
}

fn testBlockContinuationAllocationFailures(allocator: std.mem.Allocator) !void {
    const output = try format(allocator, "let value=loop ready : {; step\nloop i < limit : {i += 1} {break}} {break loop : {tick()} {break 1}}");
    defer allocator.free(output);
}

test "bare block continuation formatting survives every allocation failure" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testBlockContinuationAllocationFailures, .{});
}

test "loop structure checks inspect extras without reading unused fields" {
    const source = "let value=loop : tick() {break -1}";
    var a = try Parser.parse(std.testing.allocator, source);
    defer a.deinit(std.testing.allocator);
    var b = try Parser.parse(std.testing.allocator, source);
    defer b.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 0), a.errors.len);
    try std.testing.expectEqual(@as(usize, 0), b.errors.len);
    for (b.nodes.items(.tag), b.nodes.items(.data)) |tag, *data| {
        switch (tag) {
            .negation => data.lhs = 12345,
            .identifier, .number_literal => data.* = .{ .lhs = 12345, .rhs = 54321 },
            else => {},
        }
    }
    try std.testing.expect(sameStructure(a, b));
    // A non-loop accessor must not inspect undefined leaf data.
    for (a.nodes.items(.tag), 0..) |tag, i| {
        if (tag == .identifier) try std.testing.expect(a.fullLoop(@intCast(i)) == null);
    }
    const node = b.decl(b.rootDecls()[0]).ast.init_node;
    const extra_index = b.nodes.items(.data)[node].rhs;
    const saved = b.extra_data[extra_index];
    b.extra_data[extra_index] = b.fullLoop(node).?.body;
    try std.testing.expect(!sameStructure(a, b));
    b.extra_data[extra_index] = saved;
    b.extra_data[extra_index + 1] = saved;
    try std.testing.expect(!sameStructure(a, b));
}

fn testLoopAllocationFailures(allocator: std.mem.Allocator) !void {
    const output = try format(allocator, "let value=loop : tick(1,2,) {break loop i < 10 : i += ; step\n1 {if true {continue}\nbreak [1,2,3,]}} ; end");
    defer allocator.free(output);
    const exhaustion = try format(allocator, "let x=loop {true} : {} {break [1,2,]} else {; fallback\n[3,4,]}");
    defer allocator.free(exhaustion);
}

test "continuation and value loop formatting survives every allocation failure" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testLoopAllocationFailures, .{});
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

test "namespace import formatting preserves literal spellings comments and idempotence" {
    for ([_][:0]const u8{
        \\let std=@import("std"); named namespace
        \\let Result=std.Result
        ,
        \\; paths retain escapes and relative spelling
        \\let sibling=@import("./sub/../helper.nov")
        \\let escaped=@import("\x2e/helper.nov")
        ,
        \\let module=@import( ; literal argument
        \\"../helper.nov")
        \\let alias=module
        ,
        // Formatting is syntactic; the loader, not the formatter, validates calls.
        \\let computed=@import(path)
        \\let missing=@import()
        ,
    }) |source| {
        const formatted = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(formatted);
        const terminated = try std.testing.allocator.dupeSentinel(u8, formatted, 0);
        defer std.testing.allocator.free(terminated);
        try std.testing.expect(sameTokensAndComments(source, terminated));
        const again = try format(std.testing.allocator, terminated);
        defer std.testing.allocator.free(again);
        try std.testing.expectEqualStrings(formatted, again);
    }
}
