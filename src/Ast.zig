// Based on https://github.com/ziglang/zig/blob/master/lib/std/zig/Ast.zig
// See https://github.com/ziglang/zig/blob/master/LICENSE for LICENSE details

const std = @import("std");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;

const Ast = @This();

source: [:0]const u8,
tokens: TokenList.Slice,
nodes: NodeList.Slice,
extra_data: []Node.Index,
errors: []const Error,

pub const TokenIndex = u32;
pub const ByteOffset = u32;

pub const TokenList = std.MultiArrayList(struct {
    tag: Token.Tag,
    start: ByteOffset,
});
pub const NodeList = std.MultiArrayList(Node);

pub const Location = struct {
    line: usize,
    column: usize,
    line_start: usize,
    line_end: usize,
};

pub fn parse(allocator: std.mem.Allocator, source: [:0]const u8) !Ast {
    var tokens: TokenList = .{};
    defer tokens.deinit(allocator);

    // TODO
    // const estimated_token_count = source.len / 8;
    // try tokens.ensureTotalCapacity(allocator, estimated_token_count);

    var tokenizer = Tokenizer.init(source);
    while (true) {
        const token = tokenizer.next();
        try tokens.append(allocator, .{
            .tag = token.tag,
            .start = @intCast(token.loc.start),
        });
        if (token.tag == .eof) {
            break;
        }
    }

    var parser: Parser = .{
        .source = source,
        .allocator = allocator,
        .token_tags = tokens.items(.tag),
        .token_starts = tokens.items(.start),
        .tok_i = 0,
        .nodes = .{},
        .extra_data = .{},
        .errors = .{},
        .scratch = .{},
    };
    defer parser.nodes.deinit(allocator);
    defer parser.extra_data.deinit(allocator);
    defer parser.errors.deinit(allocator);
    defer parser.scratch.deinit(allocator);

    // TODO
    // const estimated_node_count = (tokens.len + 2) / 2;
    // try parser.nodes.ensureTotalCapacity(allocator, estimated_node_count);

    try parser.parse();

    return .{
        .source = source,
        .tokens = tokens.toOwnedSlice(),
        .nodes = parser.nodes.toOwnedSlice(),
        .extra_data = try parser.extra_data.toOwnedSlice(allocator),
        .errors = try parser.errors.toOwnedSlice(allocator),
    };
}

pub fn deinit(self: *Ast, allocator: std.mem.Allocator) void {
    self.tokens.deinit(allocator);
    self.nodes.deinit(allocator);
    allocator.free(self.extra_data);
    allocator.free(self.errors);
}

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

pub fn extraData(tree: Ast, index: usize, comptime T: type) T {
    const fields = std.meta.fields(T);
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        comptime std.debug.assert(field.type == Node.Index);
        @field(result, field.name) = tree.extra_data[index + i];
    }
    return result;
}

pub fn rootDecls(self: Ast) []const Node.Index {
    // Root is always index 0.
    const nodes_data = self.nodes.items(.data);
    return self.extra_data[nodes_data[0].lhs..nodes_data[0].rhs];
}

pub fn renderError(self: Ast, parse_error: Error, writer: anytype) !void {
    const token_tags = self.tokens.items(.tag);
    switch (parse_error.tag) {
        .chained_comparison_operators => {
            return writer.writeAll("comparison operators cannot be chained");
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
        .expected_expr_or_var_decl => {
            return writer.print("expected expression or var decl, found '{s}'", .{
                token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
            });
        },
        // .expected_labelable => {
        //     return writer.print("expected 'while', 'for', 'inline', or '{{', found '{s}'", .{
        //         token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
        //     });
        // },
        // .expected_param_list => {
        //     return writer.print("expected parameter list, found '{s}'", .{
        //         token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
        //     });
        // },
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
        .expected_statement => {
            return writer.print("expected statement, found '{s}'", .{
                token_tags[parse_error.token].symbol(),
            });
        },
        // .expected_suffix_op => {
        //     return writer.print("expected pointer dereference, optional unwrap, or field access, found '{s}'", .{
        //         token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
        //     });
        // },
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
        // .extra_addrspace_qualifier => {
        //     return writer.writeAll("extra addrspace qualifier");
        // },
        // .extra_align_qualifier => {
        //     return writer.writeAll("extra align qualifier");
        // },
        // .extra_allowzero_qualifier => {
        //     return writer.writeAll("extra allowzero qualifier");
        // },
        // .extra_const_qualifier => {
        //     return writer.writeAll("extra const qualifier");
        // },
        // .extra_volatile_qualifier => {
        //     return writer.writeAll("extra volatile qualifier");
        // },
        // .ptr_mod_on_array_child_type => {
        //     return writer.print("pointer modifier '{s}' not allowed on array child type", .{
        //         token_tags[parse_error.token].symbol(),
        //     });
        // },
        // .invalid_bit_range => {
        //     return writer.writeAll("bit range not allowed on slices and arrays");
        // },
        // .same_line_doc_comment => {
        //     return writer.writeAll("same line documentation comment");
        // },
        // .unattached_doc_comment => {
        //     return writer.writeAll("unattached documentation comment");
        // },
        // .test_doc_comment => {
        //     return writer.writeAll("documentation comments cannot be attached to tests");
        // },
        // .comptime_doc_comment => {
        //     return writer.writeAll("documentation comments cannot be attached to comptime blocks");
        // },
        // .varargs_nonfinal => {
        //     return writer.writeAll("function prototype has parameter after varargs");
        // },
        // .expected_continue_expr => {
        //     return writer.writeAll("expected ':' before while continue expression");
        // },

        .expected_newline_after_decl => {
            return writer.writeAll("expected new line after declaration");
        },
        .expected_newline_after_stmt => {
            return writer.writeAll("expected new line after statement");
        },
        // .expected_comma_after_field => {
        //     return writer.writeAll("expected ',' after field");
        // },
        .expected_comma_after_arg => {
            return writer.writeAll("expected ',' after argument");
        },
        // .expected_comma_after_param => {
        //     return writer.writeAll("expected ',' after parameter");
        // },
        // .expected_comma_after_initializer => {
        //     return writer.writeAll("expected ',' after initializer");
        // },
        .expected_comma_after_match_prong => {
            return writer.writeAll("expected ',' after match prong");
        },
        // .expected_comma_after_for_operand => {
        //     return writer.writeAll("expected ',' after for operand");
        // },
        // .expected_comma_after_capture => {
        //     return writer.writeAll("expected ',' after for capture");
        // },
        // .expected_initializer => {
        //     return writer.writeAll("expected field initializer");
        // },
        // .mismatched_binary_op_whitespace => {
        //     return writer.print("binary operator `{s}` has whitespace on one side, but not the other.", .{token_tags[parse_error.token].lexeme().?});
        // },
        // .zig_style_container => {
        //     return writer.print("to declare a container do 'const {s} = {s}'", .{
        //         self.tokenSlice(parse_error.token), parse_error.extra.expected_tag.symbol(),
        //     });
        // },
        // .previous_field => {
        //     return writer.writeAll("field before declarations here");
        // },
        // .next_field => {
        //     return writer.writeAll("field after declarations here");
        // },
        // .expected_var_const => {
        //     return writer.writeAll("expected 'var' or 'const' before variable declaration");
        // },
        .wrong_equal_var_decl => {
            return writer.writeAll("variable initialized with '==' instead of '='");
        },
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

// TODO: firstToken()
// TODO: lastToken()
// TODO: tokensOnSameLine()
// TODO: getNodeSource()

// TODO: globalVarDecl() ...

// TODO: fullVarDecl() ...

pub const full = struct {
    // TODO
};

pub const Error = struct {
    tag: Tag,
    token_is_prev: bool = false,
    token: TokenIndex,
    extra: union {
        none: void,
        expected_tag: Token.Tag,
    } = .{ .none = {} },

    pub const Tag = enum {
        chained_comparison_operators,
        wrong_equal_var_decl,
        expected_expr,
        expected_expr_or_assignment,
        expected_expr_or_var_decl,
        expected_type_expr,
        expected_prefix_expr,
        expected_newline_or_else,
        expected_newline_or_lbrace,
        expected_newline_after_decl,
        expected_newline_after_stmt,
        expected_comma_after_arg, // TODO: we don't care about commas
        expected_comma_after_match_prong,
        expected_block,
        expected_block_or_assignment,
        expected_statement,

        /// `expected_tag` is populated.
        expected_token,
    };
};

pub const Node = struct {
    tag: Tag,
    main_token: TokenIndex,
    data: Data,

    pub const Index = u32;

    comptime {
        std.debug.assert(@sizeOf(Tag) == 1);
    }

    // TODO: why separate xxx and xxx_one?
    pub const Tag = enum {
        /// sub_list[lhs..rhs]
        root,
        /// `let mut a: lhs = rhs` or `let a: lhs = rhs`.
        /// lhs and rhs may be unused. Can be local or global.
        /// main_token is `let` or `mut`
        var_decl,
        /// `lhs.a`. main_token is the dot. rhs is the identifier token index.
        field_access,
        /// `lhs.?`. main_token is the dot. rhs is the `?` token index.
        unwrap_optional,
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
        /// `lhs <<= rhs`. main_token is op.
        assign_shl,
        /// `lhs >>= rhs`. main_token is op.
        assign_shr,
        /// `lhs &= rhs`. main_token is op.
        assign_bit_and,
        /// `lhs ^= rhs`. main_token is op.
        assign_bit_xor,
        /// `lhs |= rhs`. main_token is op.
        assign_bit_or,
        /// `lhs = rhs`. main_token is op.
        assign,
        // TODO: multi_assign (assign_destructure)
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
        /// `lhs ?? rhs`. main_token is op.
        optional_fallback,
        /// `lhs and rhs`. main_token is op.
        bool_and,
        /// `lhs or rhs`. main_token is op.
        bool_or,
        /// `!lhs`. rhs unused. main_token is op.
        bool_not,
        /// `-lhs`. rhs unused. main_token is op.
        negation,
        /// `~lhs`. rhs unused. main_token is op.
        bit_not,
        /// `?lhs`. rhs unused. main_token is the `?`.
        optional_type,
        /// `[]lhs`. rhs unused. main_token is the lbracket.
        slice_type,
        /// `lhs[rhs..]`
        /// main_token is the lbracket.
        slice_open,
        /// `lhs[b..c]`. rhs is index into Slice
        /// main_token is the lbracket.
        slice,
        /// `lhs[rhs]`.
        slice_access,
        // TODO: slice_init (array_init)
        // TODO: if support "struct" struct_init
        /// `lhs(rhs)`. rhs can be omitted.
        /// main_token is the lparen.
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
        /// `match(lhs) {}`. `SubRange[rhs]`.
        match,
        /// Same as switch except there is known to be a trailing comma
        /// before the final rbrace
        match_comma,
        /// `lhs => rhs`. If lhs is omitted it means `_`.
        /// main_token is the `=>`
        match_case_one,
        /// `a, b, c => rhs`. `SubRange[lhs]`.
        /// main_token is the `=>`
        match_case,
        /// `lhs..rhs`.
        match_range,
        /// `loop {lhs}`. rhs is unused.
        loop,
        // TODO: while
        // TODO: for
        /// `if (lhs) rhs`.
        /// `if (lhs) |a| rhs`.
        @"if",
        /// `if (lhs) a else b`. `If[rhs]`.
        /// `if (lhs) |x| a else b`. `If[rhs]`.
        /// `if (lhs) |x| a else |y| b`. `If[rhs]`.
        if_else,
        /// `continue`. lhs and rhs unused.
        @"continue",
        /// `break lhs`
        /// lhs may be omitted. rhs is unused.
        @"break",
        /// `return lhs`. lhs can be omitted. rhs is unused.
        @"return",
        /// `fn (a: lhs) rhs`. lhs and rhs can be omitted.
        /// main_token is the `fn` keyword.
        fn_proto_one,
        /// `fn (a: b, c: d) rhs`. `sub_range_list[lhs]`.
        /// rhs can be omitted.
        /// main_token is the `fn` keyword.
        fn_proto,
        /// lhs is the fn_proto.
        /// rhs is the function body block.
        fn_decl,
        /// Both lhs and rhs unused.
        int_literal,
        /// Both lhs and rhs unused.
        float_literal,
        /// Both lhs and rhs unused.
        no_op,
        /// Both lhs and rhs unused.
        /// Most identifiers will not have explicit AST nodes, however for expressions
        /// which could be one of many different kinds of AST nodes, there will be an
        /// identifier AST node for it.
        identifier,
        // TODO: enum_literal
        /// main_token is the string literal token
        /// Both lhs and rhs unused.
        string_literal,
        /// main_token is the first token index (redundant with lhs)
        /// lhs is the first token index; rhs is the last token index.
        /// Could be a series of multiline_string_literal_line tokens, or a single
        /// string_literal token.
        multiline_string_literal,
        /// `(lhs)`. main_token is the `(`; rhs is the token index of the `)`.
        grouped_expression,
        // TODO: error_set_decl
        // TODO: container
        /// `{lhs rhs}`. rhs or lhs can be omitted.
        /// main_token points at the lbrace.
        block_two, // TODO: why
        /// `{}`. `sub_list[lhs..rhs]`.
        /// main_token points at the lbrace.
        block,
        // TODO: error
        /// `lhs |> rhs`
        /// lhs is an expression
        /// rhs is a function
        /// main_token is the pipe.
        pipe,
        /// `lhs -> rhs`
        /// lhs is the lambda parameters
        /// rhs is the lambda body
        /// main_token is the arrow.
        lambda,
    };

    pub const Data = struct {
        lhs: Index,
        rhs: Index,
    };

    pub const SubRange = struct {
        /// Index into sub_list.
        start: Index,
        /// Index into sub_list.
        end: Index,
    };

    pub const If = struct {
        then_expr: Index,
        else_expr: Index,
    };

    pub const Slice = struct {
        start: Index,
        end: Index,
    };
};

// TODO: nodeToSpan()
// TODO: tokenToSpan()
// TODO: tokensToSpan()
