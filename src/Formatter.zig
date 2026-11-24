const std = @import("std");
const TestAllocations = @import("TestAllocations.zig");
const Ast = @import("Ast.zig");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const primitives = @import("primitives.zig");
const Literal = @import("Literal.zig");
const TypeResolver = @import("TypeResolver.zig");
const Module = @import("Module.zig");

/// Supports scalar declarations/operators plus function signatures, calls,
/// local blocks, if/else, loops with optional conditions/continuations, returns,
/// unlabeled loop controls (including value-bearing breaks),
/// direct identifier assignments, scalar array types/literals/indexing/.len,
/// membership, positional zip iteration (including open range sources), scalar match/prongs, unary pipes,
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
    for (ast.nodes.items(.tag), ast.nodes.items(.data)) |tag, data| {
        if (tag == .attr_one and std.mem.eql(u8, ast.tokenSlice(data.lhs), "default")) {
            var module = try Module.init(allocator, "format.nov", source);
            defer module.deinit();
            if (module.hasErrors()) return error.UnsupportedSyntax;
            break;
        }
    }

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
    return fieldDefaultSupportedDepth(ast, node, 0);
}

fn fieldDefaultSupportedDepth(ast: Ast, node: Ast.Node.Index, depth: usize) bool {
    if (depth >= 128) return false;
    var current = node;
    var buffer: [2]Ast.Node.Index = undefined;
    while (ast.fullBlock(&buffer, current)) |block| {
        if (block.ast.statements.len != 1) return false;
        current = block.ast.statements[0];
    }
    const tag = ast.nodes.items(.tag)[current];
    if (tag == .array_init or tag == .array_init_comma or tag == .array_init_two or tag == .array_init_two_comma) {
        const data = ast.nodes.items(.data)[current];
        const children = [2]Ast.Node.Index{ data.lhs, data.rhs };
        const elements = if (tag == .array_init or tag == .array_init_comma) ast.extra_data[data.lhs..data.rhs] else children[0 .. @as(usize, @intFromBool(data.lhs != 0)) + @as(usize, @intFromBool(data.rhs != 0))];
        for (elements) |element| if (!fieldDefaultSupportedDepth(ast, element, depth + 1)) return false;
        return true;
    }
    if (tag == .negation) return ast.nodes.items(.tag)[ast.nodes.items(.data)[current].rhs] == .number_literal;
    if (tag == .number_literal or tag == .enum_literal or tag == .char_literal) return true;
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
        .struct_init,
        .container_field,
        .unwrap,
        .enum_literal,
        .attr_one,
        .attr_decl_one,
        .attr_decl,
        .decl,
        .fn_proto_one,
        .fn_proto,
        .function_expr,
        .inferred_type,
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

        .ref_type,
        .array_init_two,
        .array_init_two_comma,
        .array_init,
        .array_init_comma,
        .array_access,
        .slice,
        .field_access,
        .deref,
        .identifier,
        .type_variable,
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
                    if ((parent_tag == .decl and ast.decl(@intCast(parent)).ast.init_node == i) or
                        (parent_tag == .assign and ast.nodes.items(.data)[parent].rhs == i) or
                        (parent_tag == .function_pipe and ast.nodes.items(.data)[parent].lhs == i))
                    {
                        binding = true;
                        break;
                    }
                    if (parent_tag == .loop_range) {
                        for (ast.iterationBindings(@intCast(parent))) |capture| {
                            if (capture == i) {
                                binding = true;
                                break;
                            }
                        }
                        if (binding) break;
                    }
                    if (parent_tag == .call or parent_tag == .call_one) {
                        for (ast.fullCall(@intCast(parent)).?.ast.args) |arg| {
                            if (arg == i) {
                                binding = true;
                                break;
                            }
                        }
                        if (binding) break;
                    }
                }
                if (!binding) return error.UnsupportedSyntax;
            },
            .container_decl => {
                for (ast.fullStruct(@intCast(i)).?) |member| {
                    if (ast.nodes.items(.tag)[member] == .container_field) continue;
                    const declaration = ast.fullDecl(member) orelse return error.UnsupportedSyntax;
                    if (!TypeResolver.isThisAlias(&ast, member) and (declaration.ast.is_mutable or ast.declFunction(member) == null)) return error.UnsupportedSyntax;
                }
            },
            .container_field => if (data.lhs != 0) {
                const field_tag = ast.nodes.items(.tag)[data.lhs];
                switch (field_tag) {
                    .identifier, .type_variable, .field_access, .call, .call_one, .array_type, .range, .ref_type, .fn_proto, .fn_proto_one => {},
                    else => return error.UnsupportedSyntax,
                }
                const name = ast.tokenSlice(ast.firstToken(data.lhs));
                if (field_tag == .identifier and primitives.isPrimitive(name)) {
                    const allowed = [_][]const u8{ "void", "bool", "str", "rune", "float", "f32", "f64", "int", "uint", "i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128" };
                    for (allowed) |scalar| {
                        if (std.mem.eql(u8, name, scalar)) break;
                    } else return error.UnsupportedSyntax;
                }
                if (data.rhs != 0 and !fieldDefaultSupported(ast, data.rhs)) return error.UnsupportedSyntax;
            },
            .array_type => {
                switch (ast.nodes.items(.tag)[data.rhs]) {
                    .type_variable, .field_access, .call, .call_one, .array_type, .range, .ref_type, .fn_proto, .fn_proto_one => continue,
                    .identifier => {},
                    else => return error.UnsupportedSyntax,
                }
                const name = ast.tokenSlice(ast.nodes.items(.main_token)[data.rhs]);
                const scalar_types = [_][]const u8{ "bool", "str", "rune", "float", "f32", "f64", "i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128", "int", "uint" };
                for (scalar_types) |scalar| {
                    if (std.mem.eql(u8, name, scalar)) break;
                } else if (primitives.isPrimitive(name)) return error.UnsupportedSyntax;
            },
            .array_init_two, .array_init_two_comma, .array_init, .array_init_comma => {},
            .attr_one => {
                const name = ast.tokenSlice(data.lhs);
                if (std.mem.eql(u8, name, "export") or std.mem.eql(u8, name, "extern")) {
                    if (data.rhs != 0 and ast.nodes.items(.tag)[data.rhs] != .string_literal) return error.UnsupportedSyntax;
                } else if (std.mem.eql(u8, name, "operator")) {
                    if (Module.operatorArgument(&ast, data.rhs) == null) return error.UnsupportedSyntax;
                } else if (data.rhs != 0 or (!std.mem.eql(u8, name, "public") and !std.mem.eql(u8, name, "noreturn") and !std.mem.eql(u8, name, "test") and !std.mem.eql(u8, name, "stringify") and !std.mem.eql(u8, name, "default"))) return error.UnsupportedSyntax;
            },
            .builtin_literal => {
                const name = ast.tokenSlice(main);
                if (!std.mem.eql(u8, name, "@print") and !std.mem.eql(u8, name, "@println") and !std.mem.eql(u8, name, "@as") and !std.mem.eql(u8, name, "@panic") and !std.mem.eql(u8, name, "@assert") and !std.mem.eql(u8, name, "@This") and !std.mem.eql(u8, name, "@TypeOf") and !std.mem.eql(u8, name, "@len") and !std.mem.eql(u8, name, "@clear") and !std.mem.eql(u8, name, "@stringToBytes") and !std.mem.eql(u8, name, "@stringFromBytes") and !std.mem.eql(u8, name, "@import") and !std.mem.eql(u8, name, "@default") and !std.mem.eql(u8, name, "@min") and !std.mem.eql(u8, name, "@max"))
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
                if (lhs_tag != .array_access and lhs_tag != .field_access and lhs_tag != .deref) return error.UnsupportedSyntax;
                var base = ast.nodes.items(.data)[data.lhs].lhs;
                while (ast.nodes.items(.tag)[base] == .field_access or ast.nodes.items(.tag)[base] == .deref) base = ast.nodes.items(.data)[base].lhs;
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
            .identifier, .inferred_type, .type_variable, .number_literal, .char_literal, .string_literal, .builtin_literal, .enum_literal => {},
            // The parser leaves unused node data undefined; never read it.
            .ref_of, .negation, .bool_not, .bit_not, .@"return", .array_type => if (x.rhs != y.rhs) return false,
            .ref_type => if ((x.lhs == 0) != (y.lhs == 0) or x.rhs != y.rhs) return false,
            .field_access, .unwrap, .deref => if (x.lhs != y.lhs or !std.mem.eql(u8, a.tokenSlice(x.rhs), b.tokenSlice(y.rhs))) return false,
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

test "positional selectors preserve chains floats ranges comments interpolation writes and OOM" {
    try expectFormatted("let x=pair.0.1\nlet y=p.*.0\nlet z=make().0\nlet f=pair.0(0.1,1e-2)", "let x = pair.0.1\nlet y = p.*.0\nlet z = make().0\nlet f = pair.0(0.1, 1e-2)\n");
    for ([_][:0]const u8{
        "let pair=.[1,2]\nlet x=pair.0 ; first\n; next\nlet y=pair.1",
        "let x=pair.0..pair.1\nlet y=pair.0..=2\nlet z=pair.0+0.1\nlet a=pair[0]",
        "let main:()={pair.0=1\npair.1+=2\npair.0-=1\npair.1*=2\npair.0/=2\npair.1%=2\npair.0.1=3\np.*.0=4}",
        "let f:(x:`T)->`T=pair.0(x)\nlet F=(pair.0)->pair.1",
        "let text=\"{pair.0.1}:{pair.0(0.1)}:{\"{make().0}\"}\"",
        "let huge=pair.999999999999999999999999999999999999999999",
    }) |source| {
        const rendered = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(rendered);
        try expectFormatted(source, rendered);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
    }
}

test "positional selectors reject invalid decimal spelling during formatting under OOM" {
    for ([_][:0]const u8{
        "let x=pair.00",  "let x=pair.01",  "let x=pair.0x1",  "let x=pair.0b1",  "let x=pair.0o1",
        "let x=pair.1_0", "let x=pair.1e2", "let x=pair.1e+2", "let x=pair.1p-2", "let x=pair.1abc",
        "let x=pair.-1",  "let x=pair.+1",  "let x=pair.",     "let x=pair.0.01", "let x=pair.0.1e2",
    }) |source| {
        try std.testing.expectError(error.InvalidSource, format(std.testing.allocator, source));
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testInvalidPositionalFormatAllocationFailures, .{source});
    }
}

fn testInvalidPositionalFormatAllocationFailures(allocator: std.mem.Allocator, source: [:0]const u8) !void {
    const result = format(allocator, source) catch |err| switch (err) {
        error.InvalidSource => return,
        else => return err,
    };
    defer allocator.free(result);
    return error.ExpectedInvalidSource;
}

test "inferred closures format bare parameters callbacks results and comments" {
    try expectFormatted("let f=(x){x*2}", "let f = (x) { x * 2 }\n");
    try expectFormatted("let ys=xs.map((x){x*2})", "let ys = xs.map((x) { x * 2 })\n");
    try expectFormatted("let f=(x,y:int)->int{x+y}", "let f = (x, y: int) -> int { x + y }\n");
    try expectFormatted("let f=(x:&int){x}\nlet g=(){@print(1)\n}", "let f = (x: &int) { x }\nlet g = () { @print(1) }\n");
    try expectFormatted("let f=(x){return (y){x+y}}", "let f = (x) {\n    return (y) { x + y }\n}\n");
    try expectFormatted("let T=(int)->int\nlet f:(x:int)={}", "let T = (int) -> int\nlet f: (x: int) = {}\n");
    for ([_][:0]const u8{
        "let f=(\n; first\nx,\n; typed\ny:&int,\n){\n; body\nreturn (z){x+z}\n}",
        "let text=\"{map((x){\"{x}\"})}\"",
        "let f=(x,y:int){x+y}\nlet g=(){@print(1)\n}",
    }) |source| {
        const rendered = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(rendered);
        try expectFormatted(source, rendered);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
    }
}

test "generic union signatures instantiations and propagation format idempotently" {
    try expectFormatted("let Result:(T:type,E:type)->type=union{ok:T,err:E,}\nlet R=Result(int,str)\nlet f:(r:R)->R=.ok[r.!]", "let Result: (T: type, E: type) -> type = union {\n    ok: T,\n    err: E,\n}\nlet R = Result(int, str)\nlet f: (r: R) -> R = .ok[r.!]\n");
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{"let G:(T:type)->type=union{ok:T,err:void,}\nlet R=G(int)\nlet f:(r:R)->R=G(int).ok[r.!]"});
}

test "unions constructors captures and payloadless variants format idempotently" {
    try expectFormatted("let U=union{ok:int,err:str,none,}\nlet f:(u:U)->int=match u{.ok[v] => v\n.err=>0\n.none=>1}\nlet g:()->U=.none[]", "let U = union {\n    ok: int,\n    err: str,\n    none,\n}\nlet f: (u: U) -> int = match u {\n    .ok[v] => v\n    .err => 0\n    .none => 1\n}\nlet g: () -> U = .none[]\n");
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{"let U=union{ok:int,none,}\nlet f:(u:U)->int=match u{.ok[v] => v\n.none=>0}"});
}

test "match bracket captures preserve comments and reject removed syntax" {
    try expectFormatted("let f:(u:U)=match u{.ok[ value ]=>@println(value)\n.err[error]=>@println(error)\n.none=>{}}", "let f: (u: U) = match u {\n    .ok[value] => @println(value)\n    .err[error] => @println(error)\n    .none => {}\n}\n");
    for ([_][:0]const u8{
        "let f=match u{.ok[; capture\nvalue]=>value\n_=>0}",
        "let f=match u{.ok ; variant\n[\nvalue]=>value\n_=>0}",
        "let f=match u{.ok[\nvalue ; binding\n]=>value\n_=>0}",
        "let f=match u{.ok[value],true=>value\n_=>0}",
    }) |source| {
        const formatted = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(formatted);
        try expectFormatted(source, formatted);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
    }
    try std.testing.expectError(error.InvalidSource, format(std.testing.allocator, "let f=match u{.ok=>|value|value\n_=>0}"));
}

test "min max builtin forms and comments format idempotently under OOM" {
    try expectFormatted("let value=@max(1,2,3)\nlet other=@min([3,1,2])", "let value = @max(1, 2, 3)\nlet other = @min([3, 1, 2])\n");
    const source = "let f:(items:[]`T)->`T=@min(items)\nlet value=@max(\n1, ; first\n2,\n3,\n)\nlet query:@TypeOf(@min(@as([]u8,[])))=0";
    const rendered = try format(std.testing.allocator, source);
    defer std.testing.allocator.free(rendered);
    try expectFormatted(source, rendered);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
}

test "test attributes and assert arguments format and survive OOM" {
    try expectFormatted("@[test]\nlet check:()=@assert(true,\"ok\")", "@[test]\nlet check: () = @assert(true, \"ok\")\n");
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{"@[test]\nlet check:()={@assert(true)\n@assert(false,\"message\")}"});
}

test "rune annotations arrays defaults escapes and comments format idempotently" {
    try expectFormatted("let R=struct{value:rune='\\xE9',}\nlet f:(xs:[]rune)->rune=xs[0]", "let R = struct {\n    value: rune = '\\xE9',\n}\nlet f: (xs: []rune) -> rune = xs[0]\n");
    const source = "let Rune=rune\nlet x:Rune='😀' ; scalar\nlet f:(r:rune)->str=\"{r}\"\nlet values:[]rune=['é','\\0','\\u{10FFFF}']";
    const formatted = try format(std.testing.allocator, source);
    defer std.testing.allocator.free(formatted);
    try expectFormatted(source, formatted);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
}

test "string slicing formatter forms chains comments interpolation and OOM" {
    try expectFormatted("let f:(s:str)->str=s[ 1 .. -1 ][..][0..]", "let f: (s: str) -> str = s[1..-1][..][0..]\n");
    for ([_][:0]const u8{
        "let x=text[..end()]\nlet y=text[start()..]",
        "let x=a[0..2].b\nlet y=a[0..1]",
        "let x=text[; lower\n{0}..; upper\n{2}]",
        "let x=\"{text[..]}\"",
        "let x=text[0..=2]",
    }) |source| {
        const result = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(result);
        try expectFormatted(source, result);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
    }
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
    try expectFormatted("let f:(s:str)->void={s+=true\ns-=\"a\"}", "let f: (s: str) -> void = {\n    s += true\n    s -= \"a\"\n}\n");
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

test "backtick signatures format without changing names or syntax" {
    try expectFormatted("let first:(items:[]`T)->`T=items[0]", "let first: (items: []`T) -> `T = items[0]\n");
    try expectFormatted("let add:(x:`a,y:`b)->`c=x+y", "let add: (x: `a, y: `b) -> `c = x + y\n");
    const source = "let apply:(f:(`T)->`U,items:[][]`T)->`U=f(items[0][0]) ; keep\n";
    const result = try format(std.testing.allocator, source);
    defer std.testing.allocator.free(result);
    try expectFormatted(source, result);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
}

test "string byte indexing formats nested chains comments and arbitrary bases idempotently" {
    try expectFormatted("let x=a[0][1]", "let x = a[0][1]\n");
    try expectFormatted("let f:(elements:[]str)->u8=elements[0][1]", "let f: (elements: []str) -> u8 = elements[0][1]\n");
    for ([_][:0]const u8{
        "let f:(elements:[]str)->u8=elements[ ; element\n0][ ; byte\n-1] ; tail\n",
        "let text:()->str=\"é\\0z\"\nlet f:()->u8=text()[2]",
        "let f:()->u8=\"é\\0z\"[-1]",
        "let U=union{text:str,}\nlet f:(s:str)->u8=U.text[s].text[0]",
        "let f:(elements:[]str)->u8={let alias=&elements\nalias[0][1]}",
    }) |source| {
        errdefer std.debug.print("string byte formatter fixture: {s}\n", .{source});
        const result = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(result);
        try expectFormatted(source, result);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
    }
}

test "unsupported syntax fails closed including nested expressions" {
    for ([_][:0]const u8{
        "let x=@unknown(\"memory\")\n",
        "let x=call({&value})\n",

        "let x:[]void\n",

        "let f:()->void={make().inner.len=1}\n",
        "let f:()->void={make()[0]+=1}\n",
        "let f:()->void={a[0][0]-=1}\n",
        "let f:()->void={make().inner.len%=1}\n",
        "let f:()->void={make()[0]=1}\n",
        "let x=#true\n",
        "@[test(true)]\nlet x=1\n",
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

test "export formatting preserves exact symbol literals comments delimiters and attribute combinations" {
    for ([_][:0]const u8{
        "@[export]\nlet add:(x:i32)->i32=x",
        "@[public]\n@[export(\"nov_add\")]\nlet add:(x:i32)->i32=x",
        "@[export(\"nov_\\x61dd\",)]\nlet add:(x:i32)->i32=x",
        "@[export( ; symbol\n\"nov_add\", ; keep comma\n)] ; end\n@[noreturn]\nlet stop:()=loop{}",
    }) |source| {
        const result = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(result);
        try expectFormatted(source, result);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
    }
}

test "extern formatting preserves bodyless declarations comments escapes and trailing commas" {
    for ([_][:0]const u8{
        "@[extern]\nlet abs:(x:i32)->i32",
        "@[public]\n@[extern(\"nov_\\x61dd\",)]\nlet add:(x:i32,y:i32)->i32",
        "@[extern( ; symbol\n\"foreign_stop\", ; keep comma\n)] ; end\n@[noreturn]\nlet stop:() ; no body\n@[extern]\nlet unit:()",
    }) |source| {
        const result = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(result);
        try expectFormatted(source, result);
        try std.testing.expect(std.mem.indexOf(u8, result, "=") == null);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
    }
    for ([_][:0]const u8{ "@[extern(1)]\nlet f:()", "@[extern(\"a\",\"b\")]\nlet f:()" }) |source| {
        try std.testing.expectError(error.UnsupportedSyntax, format(std.testing.allocator, source));
    }
}

test "anonymous constructors format named positional contextual comments interpolation and OOM" {
    try expectFormatted("let a=.[x=1,y=2]", "let a = .[ x = 1, y = 2 ]\n");
    try expectFormatted("let a=.[1,2]", "let a = .[ 1, 2 ]\n");
    try expectFormatted("let a=R.[]", "let a = R.[]\n");
    for ([_][:0]const u8{
        "let a=.[1,\"a\"]\nlet b=[1,2]",
        "let a=.[; open\na=1, ; first\nb=.[2,3],\n]",
        "let a=R.[; open\nwidth,\nheight=2 ; last\n]",
        "let a=.[; empty\n]",
        "let a=\"{.[a=1]}:{.[1,2]}\"",
        "let a=U.tag[1]\nlet b=.tag[2]\nlet c=U.none[]\nlet d=.none[]",
    }) |source| {
        const result = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(result);
        try expectFormatted(source, result);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
    }
}

test "constructor shorthand formatting preserves tokens comments and structure" {
    try expectFormatted("let value=Name.[width,height]", "let value = Name.[ width, height ]\n");
    try expectFormatted("let value:Name=.[width,height]", "let value: Name = .[ width, height ]\n");
    try expectFormatted("let value=.[x,y]", "let value = .[ x, y ]\n");
    try expectFormatted("let value=.[x=x,y=y]", "let value = .[ x = x, y = y ]\n");
    try expectFormatted("let value=Name.[.width]", "let value = Name.[ .width ]\n");
    try expectFormatted("let value=.[.red]", "let value = .[ .red ]\n");
    for ([_][:0]const u8{
        "let f:()=R.[width,height=height,depth,]",
        "let f:()=R.[\n; start\nwidth, ; supplied\nheight=height,\ndepth ; last\n]",
        "let f:()=R.[width ; last without comma\n]",
        "let value:Name=.[; open\nwidth, ; first\nheight ; last\n]",
        "let value=.[; inferred positional\nx, ; first\ny,\n]",
        "let value=.[height=2, ; explicit first\nwidth]",
        "let value=.[width, ; identifier first\nheight=2]",
        "let value=\"{Name.[width,height]}:{.[width,height=2]}\"",
        "let value=Name.[; enum operand, not shorthand\n.red,\n]",
    }) |source| {
        const result = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(result);
        try expectFormatted(source, result);
        try std.testing.expect(std.mem.indexOf(u8, result, ".width") == null);
        try std.testing.expect(std.mem.indexOf(u8, result, "width =") == null);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
    }
}

test "nominal struct formatting preserves public attributes fields defaults methods constructor comments and interpolation" {
    for ([_][:0]const u8{
        "let R=struct{lo:i128=-170141183460469231731687303715884105728,top:u128=340282366920938463463374607431768211455,}\nlet f:(r:R)={}",
        "@[public]\nlet R=struct{; open\nx:int=0, ; default\n; next field\ny:str,\n\n@[public]\n@[stringify]\nlet toString:(self:R)->str=\"{self.x}:{self.y}\"\n}\nlet main:()={let r=R.[x=1,y=\"hello\"]\n@println(r)}",
        "let R=struct{\n; empty\n}\nlet main:()={let r=R.[\n; empty constructor\n]\n@println(true)}",
        "let R=struct{x:str,}\nlet main:()={let r=R.[\nx=\"hello\", ; value\n]\n@println(r.x)}",
    }) |source| {
        const result = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(result);
        try expectFormatted(source, result);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
    }
}

test "storable struct fields preserve direct function types and callable projections under OOM" {
    for ([_][:0]const u8{
        "let S=struct{callback:(int)->int,combine:(int,int)->int,}\nlet f:(s:S)->int=s.callback(s.combine(1,2))",
        "let S=struct{factory:()->(int)->int,update:(&mut int,int),}\nlet f:(s:S)->int=s.factory()(1)",
        "let S=struct{\n; callback signature\ncallback:(int)->int, ; field\nrange:@TypeOf(0..1),\n}\nlet f:(s:S)->int={let callback=s.callback\ncallback(1)}",
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

test "iteration continuations format before bodies with stable tokens structure and allocation cleanup" {
    try expectFormatted("let f=loop item in items:index+=1{continue}else{false}", "let f = loop item in items : index += 1 {\n    continue\n} else { false }\n");
    try expectFormatted("let f=loop item in items:{tick()}{break item}", "let f = loop item in items : { tick() } {\n    break item\n}\n");
    for ([_][:0]const u8{
        "let f=loop item in [1,2] : index += 1 {continue} else {}",
        "let f=loop rune in \"é😀\" : index += 1 {break rune} else {'?'}",
        "let f=loop i in 0..2 : tick() {}",
        "let f=loop i in 0..=2 : {tick()} {} else {}",
        "let f=loop &item in items : index += 1 {item+=1} else {}",
        "let f=loop item,index in items,0.. : tick() {}",
        "let f=loop &item,&other in items,others : {index += 1} {} else {}",
        "let f=loop item in items : {} {} else if ready {false} else {true}",
        "let f=loop i in 0..{limit()} : {tick()} {}",
        "let f=loop item,index in items,0.. : {tick()} + 1 {}",
        "let f=loop & ; capture\nitem in ; source\nitems : {; step\nindex += ; increment\n1} {continue} else {; exhaustion\n}",
        "let f=[loop item in items : tick() {},1]\nlet g=call(loop i in 0.. : {} {},2)",
        "let text=\"{loop item in items : {tick()} {break item} else {0}}\"",
        "let f=loop item in items : {loop other in others : {tick()} {continue}} {continue}",
    }) |source| {
        errdefer std.debug.print("iteration continuation formatter fixture: {s}\n", .{source});
        const result = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(result);
        try expectFormatted(source, result);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
    }
}

test "zip iteration formatting preserves comments interpolation and allocation cleanup" {
    try expectFormatted("let f=loop item,index in items,0.. {item} else {false}", "let f = loop item, index in items, 0.. {\n    item\n} else { false }\n");
    for ([_][:0]const u8{
        "let f=loop &item,&other,index in items,others,0.. {continue} else {}",
        "let f=loop x,y in [1,2],\"ab\" {}",
        "let f=loop x,y in 0..2,0..=1 {}",
        "let f=loop index in 0.. {}",
        "let f=[loop index in 0.. {break index},1]\nlet g=call(loop index in 0.. {break index},2)",
        "let f=loop x,y in 0..{limit()},items {}",
        "let f=loop x,y in items,0..{limit()} {}",
        "let f=loop & ; loan\nitem, ; capture\nindex in ; source\nitems, ; counter\n0.. ; body\n{continue} else {}",
        "let f=loop x,y in 0.. ; comma\n,items {}",
        "let f=loop item,index in items,0.. {@println(\"{item}:{index}\")} else {}",
    }) |source| {
        errdefer std.debug.print("zip formatter fixture: {s}\n", .{source});
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

test "local reference annotations format independently of binding mutability and survive OOM" {
    try expectFormatted("let f:()={let a:&int=&owner\nlet mut b:&mut []f64=&items\n}", "let f: () = {\n    let a: &int = &owner\n    let mut b: &mut []f64 = &items\n}\n");
    for ([_][:0]const u8{
        "let f:()={let mut p:&int=&x\nlet q:&mut int=&y\np=&z\nq.*+=1\n@println(p.*,q.*)}",
        "let f:(p:&mut int)->&int=p",
        "let R=struct{p:&mut int,}\nlet a:[]&int",
        "let f:()={let p:& ; readonly\nint=&x\nlet q:&mut ; mutable\nint=&y\n}",
    }) |source| {
        errdefer std.debug.print("reference formatter fixture: {s}\n", .{source});
        const result = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(result);
        try expectFormatted(source, result);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
    }
}

test "every unsupported AST tag is rejected before rendering" {
    const supported = [_]Ast.Node.Tag{
        .type_variable, .root,             .decl,               .identifier,        .number_literal,       .char_literal,   .string_literal,
        .add,           .sub,              .mul,                .div,               .mod,                  .shl,            .shr,
        .bit_and,       .bit_or,           .bit_xor,            .equal_equal,       .bang_equal,           .less_than,      .less_or_equal,
        .greater_than,  .greater_or_equal, .bool_and,           .bool_or,           .ref_of,               .negation,       .bool_not,
        .inferred_type, .bit_not,          .fn_proto_one,       .fn_proto,          .function_expr,        .call_one,       .call_one_comma,
        .call,          .call_comma,       .block_two,          .block_two_newline, .block,                .block_newline,  .@"if",
        .if_else,       .range,            .loop_range,         .loop_condition,    .loop_cont,            .loop_else,      .@"return",
        .@"break",      .@"continue",      .assign,             .assign_add,        .assign_sub,           .assign_mul,     .assign_div,
        .assign_mod,    .builtin_literal,  .array_type,         .array_init_two,    .array_init_two_comma, .array_init,     .array_init_comma,
        .array_access,  .slice,            .field_access,       .in,                .match,                .match_case_one, .match_case,
        .function_pipe, .container_decl,   .container_decl_arg, .struct_init,       .container_field,      .unwrap,         .enum_literal,
        .attr_one,      .attr_decl_one,    .attr_decl,          .ref_type,          .deref,
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
                tag == .array_init_comma or tag == .array_access or tag == .field_access or tag == .deref) continue;
            try checkSupported(ast);
        } else {
            try std.testing.expectError(error.UnsupportedSyntax, checkSupported(ast));
        }
    }
}

test "source range types format aliases arrays signatures casts and fields" {
    try expectFormatted("let R=..u8\nlet A=[]..u8\nlet S=struct{r:..u8,}\nlet f:(r:..`T)->[]..`T=[r]\n", "let R = ..u8\nlet A = []..u8\nlet S = struct {\n    r: ..u8,\n}\nlet f: (r: ..`T) -> []..`T = [r]\n");
    const source = "let f:()={let r=@as(..u8,0..1)\nlet a=[1,2]\nlet s=a[..1]\n}\n";
    const result = try format(std.testing.allocator, source);
    defer std.testing.allocator.free(result);
    try expectFormatted(source, result);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
}

test "membership formatting preserves syntactic operands and range expressions" {
    try expectFormatted("let f:(a:[]i8)->bool=-1 in a\n", "let f: (a: []i8) -> bool = -1 in a\n");
    try expectFormatted("let f:()->bool=true in [false,true,]\n", "let f: () -> bool = true in [false, true,]\n");
    try expectFormatted("let f:(n:int)->bool=n in [\n; values\n1,2,]\n", "let f: (n: int) -> bool = n in [\n    ; values\n    1,\n    2,\n]\n");
    try expectFormatted("let f: (n: int) -> bool = n in 1..3\n", "let f: (n: int) -> bool = n in 1..3\n");
    try expectFormatted("let f: (n: int, a: []int) -> bool = n in a[1..3]\n", "let f: (n: int, a: []int) -> bool = n in a[1..3]\n");
}

test "whitespace and additional supported operators are idempotent" {
    try expectFormatted("let x=1\n\nlet y=2\n", "let x = 1\n\nlet y = 2\n");
    try expectFormatted("let x=1\r\nlet y=2\r\n", "let x = 1\nlet y = 2\n");
    try expectFormatted("let x=1 +\n 2\n", "let x = 1 +\n    2\n");
    try expectFormatted("let x=a and b or !c\nlet y=a << 2 | b & ~c\nlet z=a <= b\n", "let x = a and b or !c\nlet y = a << 2 | b & ~c\nlet z = a <= b\n");
}

test "function signatures calls and delimiters are idempotent" {
    try expectFormatted("let f:(x:int,y:int)->int=x + y\nlet main:()->void=@println(f(1,2,),)\n", "let f: (x: int, y: int) -> int = x + y\nlet main: () -> void = @println(\n    f(\n        1,\n        2,\n    ),\n)\n");
    try expectFormatted("let f:(cb:(int)->int,x:int)->int=cb(x)\n", "let f: (cb: (int) -> int, x: int) -> int = cb(x)\n");
    try expectFormatted("let f:(\n x:int, ; param\n y:int,\n)->int=add(\n x, ; arg\n y,\n)\n", "let f: (\n    x: int, ; param\n    y: int,\n) -> int = add(\n    x, ; arg\n    y,\n)\n");
    try expectFormatted("let f:()->void=@println(\n; empty\n)\n", "let f: () -> void = @println(\n    ; empty\n)\n");
}

test "calls normalize source newlines and expand only trailing comma lists under OOM" {
    for ([_][2][:0]const u8{
        .{ "let x=filter(raw,(score){score>=0})", "let x = filter(raw, (score) { score >= 0 })\n" },
        .{ "let valid=filter(raw,(score){score>=0})", "let valid = filter(raw, (score) { score >= 0 })\n" },
        .{ "let valid=filter(\nraw,\n(score){\nscore>=0\n}\n)", "let valid = filter(raw, (score) { score >= 0 })\n" },
        .{ "let valid=raw.filter(\n(score){\nscore>=0\n},\n)", "let valid = raw.filter(\n    (score) { score >= 0 },\n)\n" },
        .{ "let x=outer(\ninner(\n1,\n2,\n),\nmap(xs,(x){x+1}),\n)", "let x = outer(\n    inner(\n        1,\n        2,\n    ),\n    map(xs, (x) { x + 1 }),\n)\n" },
        .{ "let x=f(\n)", "let x = f()\n" },
        .{ "let x=f(g(\n),(){\n})", "let x = f(g(), () {})\n" },
        .{ "let x=f(1,2,)", "let x = f(\n    1,\n    2,\n)\n" },
        .{ "let x=f((x:int)->int{x+1})", "let x = f((x: int) -> int { x + 1 })\n" },
        .{ "let x=f(1 ; before close\n)", "let x = f(1 ; before close\n)\n" },
        .{ "let x=f(1,) ; tail", "let x = f(\n    1,\n) ; tail\n" },
        .{ "let x=f({x>0},if c {1}else{2},[1,2])", "let x = f({ x > 0 }, if c { 1 } else { 2 }, [1, 2])\n" },
        .{ "let x=f(1) ; trailing\nlet y=g(2)", "let x = f(1) ; trailing\nlet y = g(2)\n" },
        .{ "let x=outer(inner(1),2) ; trailing", "let x = outer(inner(1), 2) ; trailing\n" },
        .{ "let x=f(1, ; argument\n2,)", "let x = f(\n    1, ; argument\n    2,\n)\n" },
        .{ "let x=f(\n; argument\n1\n)", "let x = f(\n    ; argument\n    1)\n" },
        .{ "let x=f(; empty\n)", "let x = f( ; empty\n)\n" },
        .{ "let x=outer(inner(1) ; inner tail\n,2)", "let x = outer(inner(1) ; inner tail\n    , 2)\n" },
    }) |case| {
        errdefer std.debug.print("compact call fixture: {s}\n", .{case[0]});
        try expectFormatted(case[0], case[1]);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{case[0]});
    }
}

test "calls without trailing commas preserve only internal expression breaks under OOM" {
    for ([_][2][:0]const u8{
        .{ "let x=f((x){tick(x)\nx})", "let x = f((x) {\n    tick(x)\n    x\n})\n" },
        .{ "let x=f(loop {break 1})", "let x = f(loop {\n    break 1\n})\n" },
        .{ "let x=f(loop {})", "let x = f(loop {\n})\n" },
        .{ "let x=f(match n {0=>1\n_=>2})", "let x = f(match n {\n    0 => 1\n    _ => 2\n})\n" },
        .{ "let x=f([\n1,\n2,\n])", "let x = f([\n    1,\n    2,\n])\n" },
        .{ "let x=outer(inner([\n1,\n2\n]),(x){x>0})", "let x = outer(inner([\n    1,\n    2\n]), (x) { x > 0 })\n" },
        .{ "let x=f((x){x ; body\n})", "let x = f((x) {\n    x ; body\n})\n" },
        .{ "let x=f(a +\n b)", "let x = f(a +\n    b)\n" },
        .{ "let x=f(a + ; operator\nb,)", "let x = f(\n    a + ; operator\n        b,\n)\n" },
        .{ "let x=f((x){x +\n 1})", "let x = f((x) {\n    x +\n        1\n})\n" },
        .{ "let x=f((){if c {tick()}})", "let x = f(() {\n    if c {\n        tick()\n    }\n})\n" },
        .{ "let x=f(()->void{if c {a()}else{b()}})", "let x = f(() -> void {\n    if c {\n        a()\n    } else {\n        b()\n    }\n})\n" },
        .{ "let x=f((c){if c {1}else{2}})", "let x = f((c) {\n    if c { 1 } else { 2 }\n})\n" },
    }) |case| {
        errdefer std.debug.print("multiline call fixture: {s}\n", .{case[0]});
        try expectFormatted(case[0], case[1]);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{case[0]});
    }
}

fn testFunctionListAllocationFailures(allocator: std.mem.Allocator, source: [:0]const u8, expected: []const u8) !void {
    const rendered = try format(allocator, source);
    defer allocator.free(rendered);
    try std.testing.expectEqualStrings(expected, rendered);
    const terminated = try allocator.dupeSentinel(u8, rendered, 0);
    defer allocator.free(terminated);
    try std.testing.expect(sameTokensAndComments(source, terminated));
    const again = try format(allocator, terminated);
    defer allocator.free(again);
    try std.testing.expectEqualStrings(rendered, again);
}

test "trailing commas alone control all function lists with exact idempotence tokens and OOM" {
    for ([_][2][:0]const u8{
        .{ "let x=f(\n1,\n2\n)", "let x = f(1, 2)\n" },
        .{ "let x=f(1,)", "let x = f(\n    1,\n)\n" },
        .{ "let x=@max(1,2,)", "let x = @max(\n    1,\n    2,\n)\n" },
        .{ "let x=@as(\n[]int,\nxs\n)", "let x = @as([]int, xs)\n" },
        .{ "let T=Option(\n[]int\n)", "let T = Option([]int)\n" },
        .{ "let T=Option([]int,)", "let T = Option(\n    []int,\n)\n" },
        .{ "let x=f(1, ; trailing comma\n; before close\n)", "let x = f(\n    1, ; trailing comma\n    ; before close\n)\n" },
        .{ "let x=f(1 ; no trailing comma\n; before close\n)", "let x = f(1 ; no trailing comma\n    ; before close\n)\n" },
        .{ "let x=f(1, ; separator only\n2)", "let x = f(1, ; separator only\n    2)\n" },
        .{ "let f:(\nx:int,\ny:int\n)->int=x+y", "let f: (x: int, y: int) -> int = x + y\n" },
        .{ "let f:(x:int,)->int=x", "let f: (\n    x: int,\n) -> int = x\n" },
        .{ "let F=(\nint,\nstr\n)->bool", "let F = (int, str) -> bool\n" },
        .{ "let F=(int,)->int", "let F = (\n    int,\n) -> int\n" },
        .{ "let F=(int, ; trailing type\n)->int", "let F = (\n    int, ; trailing type\n) -> int\n" },
        .{ "let f=(\nx,\ny:int\n){x+y}", "let f = (x, y: int) { x + y }\n" },
        .{ "let f=(x,){x}", "let f = (\n    x,\n) { x }\n" },
        .{ "let f=(x:int,)->int{x}", "let f = (\n    x: int,\n) -> int { x }\n" },
        .{ "let f=(x, ; trailing inferred\n){x}", "let f = (\n    x, ; trailing inferred\n) { x }\n" },
        .{ "let f:(x:int, ; separator only\ny:int)->int=x+y", "let f: (x: int, ; separator only\n    y: int) -> int = x + y\n" },
        .{ "let f:(x:int, ; trailing named\n)->int=x", "let f: (\n    x: int, ; trailing named\n) -> int = x\n" },
        .{ "let x=f((x){tick(x)\nx},2)", "let x = f((x) {\n    tick(x)\n    x\n}, 2)\n" },
        .{ "let x=outer(inner(1,),flat(\n2,\n3\n))", "let x = outer(inner(\n    1,\n), flat(2, 3))\n" },
        .{ "let x=outer(inner(\n1,\n2\n),flat(3,),)", "let x = outer(\n    inner(1, 2),\n    flat(\n        3,\n    ),\n)\n" },
        .{ "let f:(cb:(int,)->int,)->Option([]int)=make((x,){x},)", "let f: (\n    cb: (\n        int,\n    ) -> int,\n) -> Option([]int) = make(\n    (\n        x,\n    ) { x },\n)\n" },
        .{ "let F=(\n)->int\nlet f=(\n){1}\nlet x=f(\n)", "let F = () -> int\nlet f = () { 1 }\nlet x = f()\n" },
    }) |case| {
        errdefer std.debug.print("trailing comma policy fixture: {s}\n", .{case[0]});
        try testFunctionListAllocationFailures(std.testing.allocator, case[0], case[1]);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testFunctionListAllocationFailures, .{ case[0], case[1] });
    }
}

test "Probe method and Option array type application normalize without trailing commas under OOM" {
    const source =
        \\let Probe = struct {
        \\    key: int,
        \\    run: (int, int) -> bool,
        \\
        \\    @[public]
        \\    @[operator(.less_than)]
        \\    let lessThan: (
        \\        left: Probe,
        \\        right: Probe
        \\    ) -> bool = left.run(left.key, right.key) and left.key < right.key
        \\}
        \\let outcomes: []std.Option(
        \\    []int
        \\) = items
    ;
    const expected =
        \\let Probe = struct {
        \\    key: int,
        \\    run: (int, int) -> bool,
        \\
        \\    @[public]
        \\    @[operator(.less_than)]
        \\    let lessThan: (left: Probe, right: Probe) -> bool = left.run(left.key, right.key) and left.key < right.key
        \\}
        \\let outcomes: []std.Option([]int) = items
        \\
    ;
    try testFunctionListAllocationFailures(std.testing.allocator, source, expected);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testFunctionListAllocationFailures, .{ source, expected });
    const comma_method = "let Probe=struct{\nlet lessThan:(left:Probe,right:Probe,)->bool=left.key<right.key\n}";
    const comma_expected = "let Probe = struct {\n    let lessThan: (\n        left: Probe,\n        right: Probe,\n    ) -> bool = left.key < right.key\n}\n";
    try testFunctionListAllocationFailures(std.testing.allocator, comma_method, comma_expected);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testFunctionListAllocationFailures, .{ comma_method, comma_expected });
}

test "long function signatures and calls remain inline without trailing commas under OOM" {
    for ([_][2][:0]const u8{
        .{ "let x=f(\n" ++ repeatedFormatText("a", 120) ++ ",\n2\n)", "let x = f(" ++ repeatedFormatText("a", 120) ++ ", 2)\n" },
        .{ "let f:(\n" ++ repeatedFormatText("a", 100) ++ ":int,\ny:int\n)->int=y", "let f: (" ++ repeatedFormatText("a", 100) ++ ": int, y: int) -> int = y\n" },
        .{ "let f=(\n" ++ repeatedFormatText("a", 100) ++ ":int,\ny:int\n)->int{y}", "let f = (" ++ repeatedFormatText("a", 100) ++ ": int, y: int) -> int {\n    y\n}\n" },
        .{ "let F=(\n" ++ repeatedFormatText("A", 110) ++ ",\nint\n)->int", "let F = (" ++ repeatedFormatText("A", 110) ++ ", int) -> int\n" },
    }) |case| {
        errdefer std.debug.print("long inline list fixture: {s}\n", .{case[0]});
        try testFunctionListAllocationFailures(std.testing.allocator, case[0], case[1]);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testFunctionListAllocationFailures, .{ case[0], case[1] });
    }
}

test "blocks controls assignments and precedence are idempotent" {
    try expectFormatted("let f:(n:int)->int={let mut x:int=1\nx += n * 2\nif x > 0 {return x} else if x == 0 {return -1} else {return 0}}\n", "let f: (n: int) -> int = {\n    let mut x: int = 1\n    x += n * 2\n    if x > 0 { return x } else if x == 0 { return -1 } else { return 0 }\n}\n");
    try expectFormatted("let main:()->void={loop {true} {if false {break} else {continue}}\nreturn}\n", "let main: () -> void = {\n    loop { true } {\n        if false {\n            break\n        } else {\n            continue\n        }\n    }\n    return\n}\n");
    try expectFormatted("let x={1 + 2} * 3 - 4 / {5 - 6}\n", "let x = { 1 + 2 } * 3 - 4 / { 5 - 6 }\n");
    try expectFormatted("let f:()->bool=false and {return true} or !false\n", "let f: () -> bool = false and { return true } or !false\n");
}

test "single expression blocks compact scalar comparisons and multiline wrappers" {
    try expectFormatted("let value={x>y}", "let value = { x > y }\n");
    try expectFormatted("let value={true}\nlet count={42}", "let value = { true }\nlet count = { 42 }\n");
    try expectFormatted("let value={\n x>y\n}\n", "let value = { x > y }\n");
    try expectFormatted("let f:(x:int,y:int)->bool={\n x>y\n}\n", "let f: (x: int, y: int) -> bool = { x > y }\n");
    try expectFormatted("let f=(x){\n x>0\n}\n", "let f = (x) { x > 0 }\n");
}

test "single statement blocks compact executable forms under allocation failures" {
    const cases = .{
        .{ "let f:()->int={return 1}", "let f: () -> int = { return 1 }\n" },
        .{ "let f:()={return}", "let f: () = { return }\n" },
        .{ "let value=loop {break 1}", "let value = loop {\n    break 1\n}\n" },
        .{ "let f:()=loop {break}", "let f: () = loop {\n    break\n}\n" },
        .{ "let f:()=loop {continue}", "let f: () = loop {\n    continue\n}\n" },
        .{ "let f:()={x=1}", "let f: () = { x = 1 }\n" },
        .{ "let f:()={a[0]+=1}", "let f: () = { a[0] += 1 }\n" },
        .{ "let f:()={call(1,2)}", "let f: () = { call(1, 2) }\n" },
        .{ "let value={[1,2]}", "let value = { [1, 2] }\n" },
        .{ "let value={items[-1]}", "let value = { items[-1] }\n" },
    };
    inline for (cases) |case| {
        try expectFormatted(case[0], case[1]);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{case[0]});
    }
}

test "loop bodies expand including empty bodies while continuations remain compact under OOM" {
    for ([_][2][:0]const u8{
        .{ "let x=loop {}", "let x = loop {\n}\n" },
        .{ "let x=loop c:{}{}", "let x = loop c : {} {\n}\n" },
        .{ "let x=loop c:{tick()}{continue}else{0}", "let x = loop c : { tick() } {\n    continue\n} else { 0 }\n" },
        .{ "let x=loop i in 0..2:{tick()}{}", "let x = loop i in 0..2 : { tick() } {\n}\n" },
        .{ "let x=loop item in items:{}{}", "let x = loop item in items : {} {\n}\n" },
        .{ "let x=loop a,b in xs,ys:{tick()}{break a}", "let x = loop a, b in xs, ys : { tick() } {\n    break a\n}\n" },
        .{ "let f:()=loop c {}else{}", "let f: () = loop c {\n} else {\n}\n" },
        .{ "let x=loop {break if c {1}else{2}}", "let x = loop {\n    break if c { 1 } else { 2 }\n}\n" },
        .{ "let x=loop c:if d {tick()}else{tock()}{}", "let x = loop c : if d {\n    tick()\n} else {\n    tock()\n} {\n}\n" },
        .{ "let x=loop c {}else if d {1}else{2}", "let x = loop c {\n} else if d { 1 } else { 2 }\n" },
        .{ "let f:()=loop c {}else if d {a()}else{b()}", "let f: () = loop c {\n} else if d {\n    a()\n} else {\n    b()\n}\n" },
    }) |case| {
        errdefer std.debug.print("loop layout fixture: {s}\n", .{case[0]});
        try expectFormatted(case[0], case[1]);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{case[0]});
    }
}

test "conditional layout follows unused statements and used values under OOM" {
    for ([_][2][:0]const u8{
        .{ "let f:()={if c {tick()}}", "let f: () = {\n    if c {\n        tick()\n    }\n}\n" },
        .{ "let f:()->void={if c {a()}else{b()}}", "let f: () -> void = {\n    if c {\n        a()\n    } else {\n        b()\n    }\n}\n" },
        .{ "let f:()->int={if c {a()}else if d {b()}else{z()}\n1}", "let f: () -> int = {\n    if c {\n        a()\n    } else if d {\n        b()\n    } else {\n        z()\n    }\n    1\n}\n" },
        .{ "let f:()=if c {}else{}", "let f: () = if c {\n} else {\n}\n" },
        .{ "let f:()->void=if c {a()}else{b()}", "let f: () -> void = if c {\n    a()\n} else {\n    b()\n}\n" },
        .{ "let x=if c {1}else if d {2}else{3}", "let x = if c { 1 } else if d { 2 } else { 3 }\n" },
        .{ "let f:()={let x=if c {1}else{2}\nuse(x)}", "let f: () = {\n    let x = if c { 1 } else { 2 }\n    use(x)\n}\n" },
        .{ "let f:()->int={return if c {1}else{2}}", "let f: () -> int = {\n    return if c { 1 } else { 2 }\n}\n" },
        .{ "let f:()=use(if c {1}else{2})", "let f: () = use(if c { 1 } else { 2 })\n" },
        .{ "let x=1+if c {2}else{3}", "let x = 1 + if c { 2 } else { 3 }\n" },
        .{ "let f:()={x=if c {1}else{2}\ntick()}", "let f: () = {\n    x = if c { 1 } else { 2 }\n    tick()\n}\n" },
        .{ "let f:()={1+if c {2}else{3}\ntick()}", "let f: () = {\n    1 + if c { 2 } else { 3 }\n    tick()\n}\n" },
        .{ "let f:()->int={return if c {1}else{2}\n0}", "let f: () -> int = {\n    return if c { 1 } else { 2 }\n    0\n}\n" },
        .{ "let x=if c {}else{}", "let x = if c {} else {}\n" },
        .{ "let f=(c){if c {a()}else{b()}\nif c {1}else{2}}", "let f = (c) {\n    if c {\n        a()\n    } else {\n        b()\n    }\n    if c { 1 } else { 2 }\n}\n" },
        .{ "let f:()->int={tick()\nif c {1}else{2}}", "let f: () -> int = {\n    tick()\n    if c { 1 } else { 2 }\n}\n" },
        .{ "let f:()->int=if c {1}else{2}", "let f: () -> int = if c { 1 } else { 2 }\n" },
        .{ "let f=(c){if c {1}else{2}}", "let f = (c) {\n    if c { 1 } else { 2 }\n}\n" },
        .{ "let f=()->void{if c {tick()}}", "let f = () -> void {\n    if c {\n        tick()\n    }\n}\n" },
        .{ "let f:()->int={match x {0=>if c {1}else{2}\n_=>match y {0=>if d {3}else{4}\n_=>5}}}", "let f: () -> int = {\n    match x {\n        0 => if c { 1 } else { 2 }\n        _ => match y {\n            0 => if d { 3 } else { 4 }\n            _ => 5\n        }\n    }\n}\n" },
        .{ "let f:()->void={match x {0=>if c {a()}else{b()}\n_=>{}}}", "let f: () -> void = {\n    match x {\n        0 => if c {\n            a()\n        } else {\n            b()\n        }\n        _ => {}\n    }\n}\n" },
        .{ "let f:()->int={match x {0=>if c {a()}else{b()}\n_=>{}}\n1}", "let f: () -> int = {\n    match x {\n        0 => if c {\n            a()\n        } else {\n            b()\n        }\n        _ => {}\n    }\n    1\n}\n" },
    }) |case| {
        errdefer std.debug.print("conditional context fixture: {s}\n", .{case[0]});
        try expectFormatted(case[0], case[1]);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{case[0]});
    }
}

test "inferred closure tails expand void conditionals without else under OOM" {
    const source = "let f=(){if c {tick()}}";
    try expectFormatted(source, "let f = () {\n    if c {\n        tick()\n    }\n}\n");
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
}

fn repeatedFormatText(comptime text: []const u8, comptime count: usize) [text.len * count]u8 {
    var bytes: [text.len * count]u8 = undefined;
    for (0..count) |i| @memcpy(bytes[i * text.len ..][0..text.len], text);
    return bytes;
}

test "call lists ignore width including Unicode prefix indent suffix and punctuation under OOM" {
    for ([_][2][:0]const u8{
        .{ "let x=f(" ++ repeatedFormatText("a", 89) ++ ")", "let x = f(" ++ repeatedFormatText("a", 89) ++ ")\n" },
        .{ "let x=f(" ++ repeatedFormatText("a", 90) ++ ")", "let x = f(" ++ repeatedFormatText("a", 90) ++ ")\n" },
        .{ "let x=f(" ++ repeatedFormatText("a", 88) ++ ",)", "let x = f(\n    " ++ repeatedFormatText("a", 88) ++ ",\n)\n" },
        .{ "let x=f(" ++ repeatedFormatText("a", 89) ++ ",)", "let x = f(\n    " ++ repeatedFormatText("a", 89) ++ ",\n)\n" },
        .{ "let x=f(1)+" ++ repeatedFormatText("a", 85), "let x = f(1) + " ++ repeatedFormatText("a", 85) ++ "\n" },
        .{ "let x=f(1)+" ++ repeatedFormatText("a", 86), "let x = f(1) + " ++ repeatedFormatText("a", 86) ++ "\n" },
        .{ "let x=f(1); " ++ repeatedFormatText("c", 85), "let x = f(1) ; " ++ repeatedFormatText("c", 85) ++ "\n" },
        .{ "let x=f(1); " ++ repeatedFormatText("c", 86), "let x = f(1) ; " ++ repeatedFormatText("c", 86) ++ "\n" },
        .{ "let f:()={let x=f(" ++ repeatedFormatText("a", 85) ++ ")\nuse(x)}", "let f: () = {\n    let x = f(" ++ repeatedFormatText("a", 85) ++ ")\n    use(x)\n}\n" },
        .{ "let f:()={let x=f(" ++ repeatedFormatText("a", 86) ++ ")\nuse(x)}", "let f: () = {\n    let x = f(" ++ repeatedFormatText("a", 86) ++ ")\n    use(x)\n}\n" },
        .{ "let x=f(\"" ++ repeatedFormatText("é😀", 43) ++ "e\")", "let x = f(\"" ++ repeatedFormatText("é😀", 43) ++ "e\")\n" },
        .{ "let x=f(\"" ++ repeatedFormatText("é😀", 44) ++ "\")", "let x = f(\"" ++ repeatedFormatText("é😀", 44) ++ "\")\n" },
        .{ "let x=f(1); " ++ repeatedFormatText("é😀", 42) ++ "e", "let x = f(1) ; " ++ repeatedFormatText("é😀", 42) ++ "e\n" },
        .{ "let x=f(1); " ++ repeatedFormatText("é😀", 43), "let x = f(1) ; " ++ repeatedFormatText("é😀", 43) ++ "\n" },
    }) |case| {
        errdefer std.debug.print("width independent call fixture: {s}\n", .{case[0]});
        try expectFormatted(case[0], case[1]);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{case[0]});
    }
}

test "inline calls retain independent block width expansion under OOM" {
    for ([_][2][:0]const u8{
        .{ "let " ++ repeatedFormatText("a", 81) ++ "=f((x){x})", "let " ++ repeatedFormatText("a", 81) ++ " = f((x) { x })\n" },
        .{ "let " ++ repeatedFormatText("a", 82) ++ "=f((x){x})", "let " ++ repeatedFormatText("a", 82) ++ " = f((x) {\n    x\n})\n" },
        .{ "let x=f(g(" ++ repeatedFormatText("a", 86) ++ "))", "let x = f(g(" ++ repeatedFormatText("a", 86) ++ "))\n" },
        .{ "let x=f(g(" ++ repeatedFormatText("a", 87) ++ "))", "let x = f(g(" ++ repeatedFormatText("a", 87) ++ "))\n" },
        .{ "let x=f(g(" ++ repeatedFormatText("a", 94) ++ "))", "let x = f(g(" ++ repeatedFormatText("a", 94) ++ "))\n" },

        .{ "let x=f({g(" ++ repeatedFormatText("a", 86) ++ ")})", "let x = f({\n    g(" ++ repeatedFormatText("a", 86) ++ ")\n})\n" },
        .{ "let x=f((x){" ++ repeatedFormatText("a", 90) ++ "})", "let x = f((x) {\n    " ++ repeatedFormatText("a", 90) ++ "\n})\n" },
        .{ "let x=f(1,g((x){x}),2) ; tail", "let x = f(1, g((x) { x }), 2) ; tail\n" },
    }) |case| {
        errdefer std.debug.print("call containment fixture: {s}\n", .{case[0]});
        try expectFormatted(case[0], case[1]);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{case[0]});
    }
}

test "array width policy remains independent of inline function lists under OOM" {
    for ([_][2][:0]const u8{
        .{ "let x=[f(" ++ repeatedFormatText("a", 87) ++ ")]", "let x = [f(" ++ repeatedFormatText("a", 87) ++ ")]\n" },
        .{ "let x=[f(" ++ repeatedFormatText("a", 88) ++ ")]", "let x = [\n    f(" ++ repeatedFormatText("a", 88) ++ ")\n]\n" },
        .{ "let x=f([g(" ++ repeatedFormatText("a", 88) ++ ")])", "let x = f([\n    g(" ++ repeatedFormatText("a", 88) ++ ")\n])\n" },
    }) |case| {
        errdefer std.debug.print("unchanged array policy fixture: {s}\n", .{case[0]});
        try expectFormatted(case[0], case[1]);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{case[0]});
    }
}

test "Hidden signatures and sibling type calls without trailing commas stay inline under OOM" {
    const source =
        \\let Hidden: (T: type) -> type = struct {
        \\    let map: (self: Hidden(T), convert: (T) -> `U) -> Hidden(`U) = Hidden(`U).[ value = convert(self.value) ]
        \\}
    ;
    try expectFormatted(source,
        \\let Hidden: (T: type) -> type = struct {
        \\    let map: (self: Hidden(T), convert: (T) -> `U) -> Hidden(`U) = Hidden(`U).[
        \\        value = convert(self.value)
        \\    ]
        \\}
        \\
    );
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
}

test "function definitions expand their body before a fitting signature under OOM" {
    const expected = "let replace: (text: str, pattern: str, replacement: str) -> str = {\n    join(split(text, pattern), replacement)\n}\n";
    for ([_][:0]const u8{
        "let replace:(text:str,pattern:str,replacement:str)->str={join(split(text,pattern),replacement)}",
        "let replace:(\ntext:str,\npattern:str,\nreplacement:str\n)->str={join(split(text,pattern),replacement)}",
        "let replace:(text:str,pattern:str,replacement:str)->str={\njoin(split(text,pattern),replacement)\n}",
    }) |source| {
        try expectFormatted(source, expected);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
    }
    const closure = "let replace=(text:str,pattern:str,replacement:str)->str{join(split(text,pattern),replacement)}";
    try expectFormatted(closure, "let replace = (text: str, pattern: str, replacement: str) -> str {\n    join(split(text, pattern), replacement)\n}\n");
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{closure});
}

test "function body layout retains the whole line width boundary under OOM" {
    const prefix = "let f: (x: int) -> int = { ";
    const count = 100 - prefix.len - " }".len;
    const exact = prefix ++ repeatedFormatText("a", count) ++ " }\n";
    try std.testing.expectEqual(@as(usize, 100), exact.len - 1);
    const source = "let f:(x:int)->int={" ++ repeatedFormatText("a", count) ++ "}";
    const longer = "let f:(x:int)->int={" ++ repeatedFormatText("a", count + 1) ++ "}";
    try expectFormatted(source, exact);
    try expectFormatted(longer, "let f: (x: int) -> int = {\n    " ++ repeatedFormatText("a", count + 1) ++ "\n}\n");
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{longer});
    try expectFormatted("let f:(\nx:int, ; parameter\ny:int\n)->int={x+y}", "let f: (x: int, ; parameter\n    y: int) -> int = { x + y }\n");
}

test "calls normalize bounded deep nesting exactly and survive allocation failures" {
    for ([_][2][:0]const u8{
        .{ "let x=" ++ repeatedFormatText("f(\n", 30) ++ "1" ++ repeatedFormatText("\n)", 30), "let x = " ++ repeatedFormatText("f(", 30) ++ "1" ++ repeatedFormatText(")", 30) ++ "\n" },
        .{ "let x=" ++ repeatedFormatText("f(", 31) ++ "1" ++ repeatedFormatText(")", 31), "let x = " ++ repeatedFormatText("f(", 31) ++ "1" ++ repeatedFormatText(")", 31) ++ "\n" },
    }) |case| {
        errdefer std.debug.print("nested call stress fixture: {s}\n", .{case[0]});
        try expectFormatted(case[0], case[1]);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{case[0]});
    }
}

test "compact blocks measure 100 Unicode scalars across the whole physical line under OOM" {
    for ([_][2][:0]const u8{
        .{ "let x={" ++ repeatedFormatText("a", 88) ++ "}", "let x = { " ++ repeatedFormatText("a", 88) ++ " }\n" },
        .{ "let x={" ++ repeatedFormatText("a", 89) ++ "}", "let x = {\n    " ++ repeatedFormatText("a", 89) ++ "\n}\n" },
        .{ "let x={1}+" ++ repeatedFormatText("a", 84), "let x = { 1 } + " ++ repeatedFormatText("a", 84) ++ "\n" },
        .{ "let x={1}+" ++ repeatedFormatText("a", 85), "let x = {\n    1\n} + " ++ repeatedFormatText("a", 85) ++ "\n" },
        .{ "let x={1}; " ++ repeatedFormatText("c", 84), "let x = { 1 } ; " ++ repeatedFormatText("c", 84) ++ "\n" },
        .{ "let x={1}; " ++ repeatedFormatText("c", 85), "let x = {\n    1\n} ; " ++ repeatedFormatText("c", 85) ++ "\n" },
        .{ "let f:()={let x={" ++ repeatedFormatText("a", 84) ++ "}\nuse(x)}", "let f: () = {\n    let x = { " ++ repeatedFormatText("a", 84) ++ " }\n    use(x)\n}\n" },
        .{ "let f:()={let x={" ++ repeatedFormatText("a", 85) ++ "}\nuse(x)}", "let f: () = {\n    let x = {\n        " ++ repeatedFormatText("a", 85) ++ "\n    }\n    use(x)\n}\n" },
        .{ "let x={\"" ++ repeatedFormatText("é😀", 43) ++ "\"}", "let x = { \"" ++ repeatedFormatText("é😀", 43) ++ "\" }\n" },
        .{ "let x={\"" ++ repeatedFormatText("é😀", 43) ++ "e\"}", "let x = {\n    \"" ++ repeatedFormatText("é😀", 43) ++ "e\"\n}\n" },
        .{ "let x={1}; " ++ repeatedFormatText("é😀", 42), "let x = { 1 } ; " ++ repeatedFormatText("é😀", 42) ++ "\n" },
        .{ "let x={1}; " ++ repeatedFormatText("é😀", 42) ++ "e", "let x = {\n    1\n} ; " ++ repeatedFormatText("é😀", 42) ++ "e\n" },
    }) |case| {
        errdefer std.debug.print("physical line width fixture: {s}\n", .{case[0]});
        try expectFormatted(case[0], case[1]);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{case[0]});
    }
}

test "conditional branches share width decisions without splitting long tokens under OOM" {
    for ([_][2][:0]const u8{
        .{ "let x=if c {1}else{" ++ repeatedFormatText("a", 72) ++ "}", "let x = if c { 1 } else { " ++ repeatedFormatText("a", 72) ++ " }\n" },
        .{ "let x=if c {1}else{" ++ repeatedFormatText("a", 73) ++ "}", "let x = if c {\n    1\n} else {\n    " ++ repeatedFormatText("a", 73) ++ "\n}\n" },
        .{ "let x=if c {1}else if d {2}else{" ++ repeatedFormatText("a", 56) ++ "}", "let x = if c { 1 } else if d { 2 } else { " ++ repeatedFormatText("a", 56) ++ " }\n" },
        .{ "let x=if c {1}else if d {2}else{" ++ repeatedFormatText("a", 57) ++ "}", "let x = if c {\n    1\n} else if d {\n    2\n} else {\n    " ++ repeatedFormatText("a", 57) ++ "\n}\n" },
        .{ "let x=if c {" ++ repeatedFormatText("a", 73) ++ "}else{1}", "let x = if c {\n    " ++ repeatedFormatText("a", 73) ++ "\n} else {\n    1\n}\n" },
        .{ "let x=if c {1}else{2}; " ++ repeatedFormatText("c", 71), "let x = if c {\n    1\n} else {\n    2\n} ; " ++ repeatedFormatText("c", 71) ++ "\n" },
        .{ "let f:()->str={\"" ++ repeatedFormatText("x", 76) ++ "\"}", "let f: () -> str = {\n    \"" ++ repeatedFormatText("x", 76) ++ "\"\n}\n" },
        .{ "let x=if c {\"" ++ repeatedFormatText("é😀", 60) ++ "\"}else{\"short\"}", "let x = if c {\n    \"" ++ repeatedFormatText("é😀", 60) ++ "\"\n} else {\n    \"short\"\n}\n" },
        .{ "let x=\"" ++ repeatedFormatText("é😀", 60) ++ "\"", "let x = \"" ++ repeatedFormatText("é😀", 60) ++ "\"\n" },
        .{ "let x=" ++ repeatedFormatText("a", 110) ++ "+" ++ repeatedFormatText("b", 110), "let x = " ++ repeatedFormatText("a", 110) ++ " + " ++ repeatedFormatText("b", 110) ++ "\n" },
    }) |case| {
        errdefer std.debug.print("shared width fixture: {s}\n", .{case[0]});
        try expectFormatted(case[0], case[1]);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{case[0]});
    }
}

test "single statement blocks retain comments declarations and multiline expressions" {
    try expectFormatted("let value={; entry\nx>y}", "let value = { ; entry\n    x > y\n}\n");
    try expectFormatted("let value={x>y ; tail\n}", "let value = {\n    x > y ; tail\n}\n");
    try expectFormatted("let value={x + ; addition\ny}", "let value = {\n    x + ; addition\n        y\n}\n");
    try expectFormatted("let value={x\ny}", "let value = {\n    x\n    y\n}\n");
    try expectFormatted("let value={let x=1\n}", "let value = {\n    let x = 1\n}\n");
    try expectFormatted("let value={x +\n y}", "let value = {\n    x +\n        y\n}\n");
    try expectFormatted("let value={call(\n1,\n2\n)}", "let value = { call(1, 2) }\n");
    try expectFormatted("let value={}\nlet other={\n}", "let value = {}\nlet other = {}\n");
}

test "single statement blocks retain nested blocks and control flow" {
    try expectFormatted("let value={{1}}", "let value = {\n    { 1 }\n}\n");
    try expectFormatted("let value={if ready {1} else {2}}", "let value = {\n    if ready { 1 } else { 2 }\n}\n");
    try expectFormatted("let value={loop {break 1}}", "let value = {\n    loop {\n        break 1\n    }\n}\n");
    try expectFormatted("let value={match x {0=>1\n_=>2}}", "let value = {\n    match x {\n        0 => 1\n        _ => 2\n    }\n}\n");
}

test "compact blocks nested in lists stabilize on the first pass" {
    try expectFormatted("let value=call({x>y})", "let value = call({ x > y })\n");
    try expectFormatted("let value=[{x>y},{false}]", "let value = [\n    { x > y },\n    { false }\n]\n");
    try expectFormatted("let value=call([{1}])", "let value = call([\n    { 1 }\n])\n");
    try expectFormatted("let value=[call({1})]", "let value = [\n    call({ 1 })\n]\n");
    try expectFormatted("let value=items[{1}]", "let value = items[\n    { 1 }]\n");
}

test "nested list expansion and empty block comments stabilize on the first pass" {
    try expectFormatted("let f:()->void=@println({let x:int=1\nx})\n", "let f: () -> void = @println({\n    let x: int = 1\n    x\n})\n");
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
    try expectFormatted("let f:()={let mut a=[1,2]\n@clear(a)}\n", "let f: () = {\n    let mut a = [1, 2]\n    @clear(a)\n}\n");
    try expectFormatted("let n=@len(\"é\\0😀\")\n", "let n = @len(\"é\\0😀\")\n");
    try expectFormatted("let x:[]int=[]\nlet y=[1,2,3,]\nlet n=make()[0] + -a[-1] * @len(a)\n", "let x: []int = []\nlet y = [1, 2, 3,]\nlet n = make()[0] + -a[-1] * @len(a)\n");
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
    try expectFormatted("let x=[{1}]\n", "let x = [\n    { 1 }\n]\n");
    for ([_][:0]const u8{
        "let f:(a:[]int)->int=a[\n -1]\n",
        "let f:(a:[]int)->uint=@len(a[; index\n -1])\n",
        "let f:()->[]int=[\n1, ; first\n2 ; before comma\n,3, ; tail\n]\n",
        "let f:()->[]int=[1 + ; operator\n 2,3]\n",
        "let f:(a:[]int)->int=a[0 +\n 1]\n",
        "let f:(a:[]int)->int=a[1 + {2}]\n",
        "let f:(a:[]int)->int=a[{1}]\n",
        "let f:(a:[]int)->uint=@len(a[0]) + -a[-1]\n",
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
    const output = try format(allocator, "let f:(a:[]int)->[]int={let mut b:[]int=[\n1, ; element\n{a[-1]},3,]\nb[0]=@len(a)\nb[-1] += {a[0]}\nb[0] -= 1\nb[0] *= 2\nb[0] /= 2\nb[0] %= 3\nb += []\nlet found = 1 in b\nlet absent = false in []\nreturn b}\n");
    defer allocator.free(output);
}

test "array formatting survives every allocation failure" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testArrayAllocationFailures, .{});
}

test "quoted interpolation formatting preserves nested literal bytes and idempotence" {
    const source: [:0]const u8 =
        \\let greet:(name:str)->str="Hello {name}!"
        \\let main:()={@println("{"a"}","{greet("Ada")}","{'é'}","{"nested {"value"}"}","{"\"\\\u{1f600}"}")}
    ;
    const formatted = try format(std.testing.allocator, source);
    defer std.testing.allocator.free(formatted);
    const terminated = try std.testing.allocator.dupeSentinel(u8, formatted, 0);
    defer std.testing.allocator.free(terminated);
    const again = try format(std.testing.allocator, terminated);
    defer std.testing.allocator.free(again);
    try std.testing.expectEqualStrings(formatted, again);
    for ([_][]const u8{ "\"{\"a\"}\"", "\"{greet(\"Ada\")}\"", "\"{'é'}\"", "\"{\"nested {\"value\"}\"}\"" }) |literal| {
        try std.testing.expect(std.mem.indexOf(u8, formatted, literal) != null);
    }
}

test "array valued struct defaults format and survive OOM" {
    const source = "let R=struct{xs:[]u8=[],rows:[][]int=[[1,2],[],[3]],}\nlet G:(T:type)->type=struct{values:[]T=[],}";
    try expectFormatted(source, "let R = struct {\n    xs: []u8 = [],\n    rows: [][]int = [[1, 2], [], [3]],\n}\nlet G: (T: type) -> type = struct {\n    values: []T = [],\n}\n");
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
}

test "string byte conversions format nested calls and survive OOM" {
    try expectFormatted(
        "let encode:(text:str)->[]u8=@stringToBytes(text)\nlet decode:(bytes:[]u8)->str=@stringFromBytes(bytes)",
        "let encode: (text: str) -> []u8 = @stringToBytes(text)\nlet decode: (bytes: []u8) -> str = @stringFromBytes(bytes)\n",
    );
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{"let run:()={let bytes=@stringToBytes(\"é\\0\")\n@println(@stringFromBytes(bytes))}"});
}

test "TypeOf formatting preserves annotations type arguments and interpolation" {
    try expectFormatted(
        "let n=1\nlet T=@TypeOf(n)\nlet f:(x:int)->@TypeOf(x)={let y:[]@TypeOf(x)=[x]\n@println(@TypeOf(y),\"{@TypeOf(x)}\")\nx}",
        "let n = 1\nlet T = @TypeOf(n)\nlet f: (x: int) -> @TypeOf(x) = {\n    let y: []@TypeOf(x) = [x]\n    @println(@TypeOf(y), \"{@TypeOf(x)}\")\n    x\n}\n",
    );
}

test "operator formatting preserves literal tags qualified syntax comments and OOM" {
    try expectFormatted("let S=struct{\n@[operator(.equal)]\nlet eq:(a:S,b:S)->bool=true\n}", "let S = struct {\n    @[operator(.equal)]\n    let eq: (a: S, b: S) -> bool = true\n}\n");
    for (std.meta.tags(Module.Operator)) |operator| {
        const source = try std.fmt.allocPrintSentinel(std.testing.allocator, "let S=struct{{\n@[operator(.{s})]\nlet eq:(a:S,b:S)->bool=true\n}}", .{@tagName(operator)}, 0);
        defer std.testing.allocator.free(source);
        const result = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(result);
        try expectFormatted(source, result);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
    }
    for ([_][:0]const u8{
        "let S=struct{\n@[public]\n@[operator(std.builtin.Operator.equal)]\nlet compare:(a:S,b:S)->bool=true\n}",
        "let S=struct{\n@[operator( ; tag\n.equal, ; comma\n)] ; end\nlet compare:(a:S,b:S)->bool=true\n}",
    }) |source| {
        const result = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(result);
        try expectFormatted(source, result);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
    }
    for ([_][:0]const u8{ "@[operator]\nlet f:()={}", "@[operator(1)]\nlet f:()={}", "@[operator(.unknown)]\nlet f:()={}", "@[operator(.equal,.add)]\nlet f:()={}" }) |source| {
        try std.testing.expectError(error.UnsupportedSyntax, format(std.testing.allocator, source));
    }
}

test "default formatting preserves factories and rejects invalid annotation placement under OOM" {
    const source = "let Box:(T:type)->type=struct{value:T,\n@[public]\n@[default]\nlet make:()->@This()=unresolved\n}";
    const formatted = try format(std.testing.allocator, source);
    defer std.testing.allocator.free(formatted);
    try std.testing.expect(std.mem.indexOf(u8, formatted, "@[default]") != null);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
    for ([_][:0]const u8{
        "@[default]\nlet make:()->int=0",
        "let S=struct{\n@[default(1)]\nlet make:()->S=unresolved\n}",
        "let S=struct{\n@[default]\n@[default]\nlet make:()->S=unresolved\n}",
        "let S=struct{\n@[default]\nlet first:()->S=unresolved\n@[default]\nlet second:()->S=unresolved\n}",
        "let S=struct{\n@[default]\n@[stringify]\nlet make:()->S=unresolved\n}",
        "let S=struct{\n@[default]\nx:int,\n}",
        "let f:()={\n@[default]\nlet make:()->int=0\n}",
    }) |invalid| {
        if (format(std.testing.allocator, invalid)) |result| {
            std.testing.allocator.free(result);
            return error.TestUnexpectedResult;
        } else |err| {
            try std.testing.expect(err == error.UnsupportedSyntax or err == error.InvalidSource);
        }
    }
}

test "stringify formatting preserves argument free attributes and arbitrary method names" {
    try expectFormatted("let R=struct{x:int,\n@[public]\n@[stringify]\nlet format:(self:R)->str=\"{self.x}\"\n}", "let R = struct {\n    x: int,\n    @[public]\n    @[stringify]\n    let format: (self: R) -> str = \"{self.x}\"\n}\n");
    try std.testing.expectError(error.UnsupportedSyntax, format(std.testing.allocator, "let R=struct{@[stringify(1)]\nlet format:(self:R)->str=\"R\"\n}"));
}

test "record interpolation formatting preserves captures aliases conversions and allocation cleanup" {
    const source = "let Alias=Error\nlet Error=struct{s:str,\n@[stringify]\nlet toString:(e:Alias)->str=e.s\n}\nlet U=union{err:Error,}\nlet f:(u:U)=match u{.err[err] => @panic(\"Error: {err}\")}\nlet g:(e:Alias)->str=\"{e} {Error.toString(e)} {e}\"";
    const formatted = try format(std.testing.allocator, source);
    defer std.testing.allocator.free(formatted);
    try expectFormatted(source, formatted);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
}

test "interpolation formatting preserves literal chunks escapes and placeholders" {
    try expectFormatted("let f:(n:int,s:str)->str=\"é\\{\\}\\0{n}:{s}😀\"", "let f: (n: int, s: str) -> str = \"é\\{\\}\\0{n}:{s}😀\"\n");
}

test "conditional loop exhaustion formatting preserves tokens comments and structure" {
    try expectFormatted("let x=loop false{}else{true}", "let x = loop false {\n} else { true }\n");
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

test "reference arguments format direct borrows indexed owners and pipes" {
    try expectFormatted(
        "let f:(p:&mut int)={p.*+=1}\nlet main:()={let mut n=1\nf(&n)\nlet mut a=[1]\nf(&a[-1])\nlet done:void=&n |> f\n}",
        "let f: (p: &mut int) = { p.* += 1 }\nlet main: () = {\n    let mut n = 1\n    f(&n)\n    let mut a = [1]\n    f(&a[-1])\n    let done: void = &n |> f\n}\n",
    );
}

test "iteration formatting preserves copied and borrowed captures comments and nesting" {
    try expectFormatted("let f:()=loop &item in items {item+=1}", "let f: () = loop &item in items {\n    item += 1\n}\n");
    for ([_][:0]const u8{
        "let f:()=loop item in [true,false,] {continue} else {}",
        "let f:()=loop rune in \"é\\0😀\" {break rune} else {'?'}",
        "let f:()=loop & ; capture\nitem in ; source\nitems {item+=1} else {; exhaustion\n}",
        "let f:()=loop &item in items {loop rune in \"é\" {@print(item,rune)}}",
    }) |source| {
        const formatted = try format(std.testing.allocator, source);
        defer std.testing.allocator.free(formatted);
        try expectFormatted(source, formatted);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testMatchFormatAllocationFailures, .{source});
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
    try expectFormatted("let value=loop ready:{tick()}{break 1}", "let value = loop ready : { tick() } {\n    break 1\n}\n");
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
