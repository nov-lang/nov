// Originally based on https://github.com/ziglang/zig/blob/master/lib/std/zig/Ast.zig
// See https://github.com/ziglang/zig/blob/master/LICENSE for additional LICENSE details

const std = @import("std");
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
    const fields = std.meta.fields(T);
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        comptime assert(field.type == Node.Index);
        @field(result, field.name) = tree.extra_data[index + i];
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
        .expected_newline_after_decl => {
            return writer.writeAll("expected new line after declaration");
        },
        .expected_newline_after_stmt => {
            return writer.writeAll("expected new line after statement");
        },
        .expected_newline_after_attr => {
            return writer.writeAll("expected new line after attribute");
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
        .mismatched_binary_op_whitespace => {
            return writer.print("binary operator `{s}` has whitespace on one side, but not the other.", .{token_tags[parse_error.token].lexeme().?});
        },
        .invalid_ampersand_ampersand => {
            return writer.writeAll("ambiguous use of '&&'; use 'and' for logical AND, or change whitespace to ' & &' for bitwise AND");
        },
        // .nov_style_container => {
        //     return writer.print("to declare a container do 'let {s} = {s}'", .{
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
    var n = node;
    while (true) switch (tags[n]) {
        .root => return 0,

        .decl,
        .bool_not,
        .negation,
        .bit_not,
        .optional_type,
        .match,
        .@"if",
        .if_else,
        .@"continue",
        .@"break",
        .@"return",
        .identifier,
        .bool_literal,
        .number_literal,
        .char_literal,
        .string_literal,
        .unreachable_literal,
        .grouped_expression,
        .block_two,
        .block,
        .loop,
        .fn_args_one,
        .fn_args,
        .attr_one,
        .attr,
        => return main_tokens[n],

        .field_access,
        .unwrap_option,
        .unwrap_result,
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
        .assign_shl,
        .assign_shr,
        .assign_bit_and,
        .assign_bit_xor,
        .assign_bit_or,
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
        .call,
        .match_range,
        .function_pipe,
        .fn_proto,
        .fn_expr,
        .attr_decl_one,
        => n = datas[n].lhs,

        .match_case_one => {
            // TODO
            if (datas[n].lhs == 0) {
                return main_tokens[n] - 1; // underscore token
            } else {
                n = datas[n].lhs;
            }
        },

        .match_case,
        .attr_decl,
        => {
            const extra = self.extraData(datas[n].lhs, Node.SubRange);
            assert(extra.end - extra.start > 0);
            n = self.extra_data[extra.start];
        },
    };
}

pub fn lastToken(self: Ast, node: Node.Index) TokenIndex {
    const tags: []const Node.Tag = self.nodes.items(.tag);
    const datas = self.nodes.items(.data);
    const main_tokens = self.nodes.items(.main_token);
    var n = node;
    var end_offset: TokenIndex = 0;
    while (true) switch (tags[n]) {
        .root => return @intCast(self.tokens.len - 1),
        else => {
            // TODO
            // TODO: merge similar cases once all are implemented
            std.log.debug("Unhandled tag: {}", .{tags[n]});
            unreachable;
        },

        .bool_not,
        .negation,
        .bit_not,
        .optional_type,
        .loop,
        => n = datas[n].lhs,

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
        .assign_shl,
        .assign_shr,
        .assign_bit_and,
        .assign_bit_xor,
        .assign_bit_or,
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
        .match_case_one,
        .match_case,
        .match_range,
        .function_pipe,
        .decl,
        .fn_proto,
        .fn_expr,
        .attr_decl_one,
        .attr_decl,
        => n = datas[n].rhs,

        .field_access,
        .unwrap_option,
        .unwrap_result,
        .grouped_expression,
        => return datas[n].rhs + end_offset,

        .bool_literal,
        .number_literal,
        .char_literal,
        .identifier,
        .string_literal,
        .unreachable_literal,
        => return main_tokens[n] + end_offset,

        .block => {
            assert(datas[n].rhs - datas[n].lhs > 0);
            end_offset += 2; // newline rbrace
            n = self.extra_data[datas[n].rhs - 1]; // last statement
        },

        .block_two => {
            if (datas[n].rhs != 0) {
                end_offset += 2; // newline rbrace
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                end_offset += 2; // newline rbrace
                n = datas[n].lhs;
            } else {
                end_offset += 1; // rbrace
                return main_tokens[n] + end_offset;
            }
        },

        .if_else => {
            const extra = self.extraData(datas[n].rhs, Node.If);
            assert(extra.else_expr != 0);
            n = extra.else_expr;
        },

        .call_one => {
            end_offset += 1; // for the rparen
            if (datas[n].rhs == 0) {
                return main_tokens[n] + end_offset;
            }
            n = datas[n].rhs;
        },

        .attr_one => {
            end_offset += 1; // for the rbracket
            if (datas[n].rhs == 0) {
                return main_tokens[n] + end_offset;
            }
            end_offset += 1; // for the rparen
            n = datas[n].rhs;
        },

        .fn_args_one => {
            end_offset += 1; // for the rparen
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                n = datas[n].lhs;
            } else {
                return main_tokens[n] + end_offset;
            }
        },

        // TODO: idk about that
        .fn_args => {
            assert(datas[n].rhs - datas[n].lhs > 0);
            end_offset += 1; // for the rparen
            n = self.extra_data[datas[n].rhs - 1]; // last argument
        },

        .attr => {
            const extra = self.extraData(datas[n].rhs, Node.SubRange);
            assert(extra.end - extra.start > 0);
            end_offset += 2; // for the rparen rbracket
            n = self.extra_data[extra.end - 1]; // last attribute
        },
    };
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

// TODO: all of this

pub fn decl(self: Ast, node: Node.Index) full.Decl {
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
    const data = self.nodes.items(.data)[node];
    const params: *const [1]Node.Index = @ptrCast(&data.lhs);
    return self.fullFnProtoComponents(.{
        .proto_node = node,
        .fn_token = self.nodes.items(.main_token)[node],
        .return_type = data.rhs,
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
        .return_type = data.rhs,
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

pub fn sliceOpen(self: Ast, node: Node.Index) full.Slice {
    assert(self.nodes.items(.tag)[node] == .slice_open);
    const data = self.nodes.items(.data)[node];
    return .{
        .ast = .{
            .sliced = data.lhs,
            .lbracket = self.nodes.items(.main_token)[node],
            .start = data.rhs,
            .end = 0,
        },
    };
}

pub fn slice(self: Ast, node: Node.Index) full.Slice {
    assert(self.nodes.items(.tag)[node] == .slice);
    const data = self.nodes.items(.data)[node];
    const extra = self.extraData(data.rhs, Node.Slice);
    return .{
        .ast = .{
            .sliced = data.lhs,
            .lbracket = self.nodes.items(.main_token)[node],
            .start = extra.start,
            .end = extra.end,
        },
    };
}

pub fn matchCaseOne(tree: Ast, node: Node.Index) full.MatchCase {
    const data = &tree.nodes.items(.data)[node];
    const values: *const [1]Node.Index = @ptrCast(&data.lhs);
    return .{
        .ast = .{
            .values = if (data.lhs == 0) values[0..0] else values[0..1],
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
            .arrow_token = tree.nodes.items(.main_token)[node],
            .target_expr = data.rhs,
        },
    };
}

// TODO: for

pub fn callOne(tree: Ast, node: Node.Index) full.Call {
    const data = tree.nodes.items(.data)[node];
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
    var result: full.FnProto = .{
        .ast = info,
        .lparen = undefined,
    };

    const after_fn_token = info.fn_token + 1;
    if (token_tags[after_fn_token] == .identifier) {
        result.name_token = after_fn_token;
        result.lparen = after_fn_token + 1;
    } else {
        result.lparen = after_fn_token;
    }
    assert(token_tags[result.lparen] == .l_paren);

    return result;
}

pub fn fullDecl(self: Ast, node: Node.Index) ?full.Decl {
    return switch (self.nodes.items(.tag)[node]) {
        .decl => self.decl(node),
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

pub fn fullFnProto(self: Ast, node: Node.Index) ?full.FnProto {
    return switch (self.nodes.items(.tag)[node]) {
        .fn_proto_one => self.fnProtoOne(node),
        .fn_proto => self.fnProto(node),
        .fn_decl => self.fullFnProto(self.nodes.items(.data)[node].lhs),
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
        .slice_open => self.sliceOpen(node),
        .slice => self.slice(node),
        else => null,
    };
}

// pub fn fullMatch(self: Ast, node: Node.Index) ?full.Match {
//     return switch (self.nodes.items(.tag)[node]) {
//         .match => self.match(node),
//         else => null,
//     };
// }

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
            params: []const Node.Index,
        };

        pub const Param = struct {
            first_doc_comment: ?TokenIndex,
            name_token: ?TokenIndex,
            comptime_noalias: ?TokenIndex,
            anytype_ellipsis3: ?TokenIndex,
            type_expr: Node.Index,
        };

        // TODO: Iterator
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

    // pub const Match = struct {
    //     ast: Components,

    //     pub const Components = struct {
    //         match_token: TokenIndex,
    //         expr: Node.Index,
    //         cases: []const Node.Index,
    //     };
    // };

    pub const MatchCase = struct {
        ast: Components,

        pub const Components = struct {
            /// If empty, this is the `_` case.
            values: []const Node.Index,
            arrow_token: TokenIndex,
            target_expr: Node.Index,
        };
    };

    pub const Call = struct {
        ast: Components,

        pub const Components = struct {
            lparen: TokenIndex,
            fn_proto: Node.Index,
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
        attr_without_args,
        mismatched_binary_op_whitespace,
        invalid_ampersand_ampersand,
        same_line_doc_comment,
        unattached_doc_comment,
        expected_decl,
        expected_expr,
        expected_expr_or_assignment,
        expected_expr_or_decl,
        expected_type_expr,
        expected_prefix_expr,
        expected_newline_or_else,
        expected_newline_or_lbrace,
        expected_newline_after_decl,
        expected_newline_after_stmt,
        expected_newline_after_attr,
        expected_comma_after_arg,
        expected_comma_after_match_prong,
        expected_block,
        expected_block_or_assignment,
        expected_statement,
        expected_labelable,

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
        /// `lhs.a`. main_token is the dot. rhs is the identifier token index.
        field_access,
        /// `lhs.?`. main_token is the dot. rhs is the `?` token index.
        unwrap_option,
        /// `lhs.!`. main_token is the dot. rhs is the `!` token index.
        unwrap_result,
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
        // TODO: multi_assign (assign_destructure in zig), WE DO NOT HAVE TUPLES!
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
        /// `lhs(rhs)`. rhs can be omitted.
        /// main_token is the `(`.
        call_one,
        /// `lhs(a, b, c)`. `SubRange[rhs]`.
        /// main_token is the `(`.
        call,
        /// `match lhs {}`. `SubRange[rhs]`.
        match,
        /// `lhs => rhs`. If lhs is omitted it means `_`.
        /// main_token is the `=>`
        match_case_one,
        /// `a, b, c => rhs`. `SubRange[lhs]`.
        /// main_token is the `=>`.
        match_case,
        /// `lhs..rhs`.
        match_range,
        /// `loop {}`. lhs is the block. rhs is unused.
        loop,
        // TODO: for
        /// `if lhs {}`. rhs is the block.
        @"if",
        /// `if lhs {} else {}`. `If[rhs]`.
        if_else,
        /// `continue`. lhs is token index of label if any. rhs is unused.
        @"continue",
        /// `break :lhs rhs`
        /// both lhs and rhs may be omitted.
        @"break",
        /// `return lhs`. lhs can be omitted. rhs is unused.
        @"return",
        /// `(lhs: rhs)` lhs and rhs can be omitted.
        /// main_token is the `(`.
        fn_args_one,
        /// `(a: b, c: d)` sub_list[lhs..rhs].
        /// main_token is the `(`.
        fn_args,
        /// `lhs -> rhs`.
        /// lhs is fn_args.
        /// main_token is the `->`.
        fn_proto,
        /// `lhs rhs`.
        /// lhs is the fn_proto.
        /// rhs is the function body block.
        fn_expr,
        /// Both lhs and rhs unused.
        number_literal,
        /// Both lhs and rhs unused.
        char_literal,
        /// Both lhs and rhs unused.
        bool_literal,
        /// Both lhs and rhs unused.
        unreachable_literal,
        /// Both lhs and rhs unused.
        /// Most identifiers will not have explicit AST nodes, however for expressions
        /// which could be one of many different kinds of AST nodes, there will be an
        /// identifier AST node for it.
        identifier,
        /// main_token is the string literal token
        /// Both lhs and rhs unused.
        string_literal,
        /// `(lhs)`. main_token is the `(`
        /// rhs is the token index of the `)`.
        grouped_expression,
        /// `{lhs rhs}`. lhs and rhs can be omitted.
        /// main_token points at the lbrace.
        block_two,
        /// `{}`. `sub_list[lhs..rhs]`.
        /// main_token points at the lbrace.
        block,
        // TODO: struct, enum, container, class idk, generics too
        // TODO: builtins: import, typeof
        // TODO: array / slice
        // TODO: defer?
        // TODO: Result and Option syntax sugar
        // TODO: async/await/resume/suspend/nosuspend/yield
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

    pub const Decl = packed struct(Index) {
        mutable: bool,
        /// Can be Parser.null_node
        type_node: u31,
    };

    pub const If = struct {
        then_expr: Index,
        /// can be a block or another if expression
        else_expr: Index,
    };
};

pub const Span = struct {
    start: u32,
    end: u32,
    main: u32,
};

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
