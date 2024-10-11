// Based on https://github.com/ziglang/zig/blob/master/lib/std/zig/Ast.zig
// See https://github.com/ziglang/zig/blob/master/LICENSE for LICENSE details

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
pub const TokenList = std.MultiArrayList(Token);
pub const NodeList = std.MultiArrayList(Node);

pub const Location = struct {
    line: usize,
    column: usize,
    line_start: usize,
    line_end: usize,
};

pub fn parse(allocator: std.mem.Allocator, source: [:0]const u8) Parser.Error!Ast {
    var tokens: TokenList = .{};
    defer tokens.deinit(allocator);

    // TODO
    // const estimated_token_count = source.len / 8;
    // try tokens.ensureTotalCapacity(allocator, estimated_token_count);

    var tokenizer = Tokenizer.init(source);
    while (true) {
        const token = tokenizer.next();
        try tokens.append(allocator, token);
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
    return self.source[token.loc.start..token.loc.end];
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

pub fn firstToken(self: Ast, node: Node.Index) TokenIndex {
    const tags = self.nodes.items(.tag);
    const datas = self.nodes.items(.data);
    const main_tokens = self.nodes.items(.main_token);
    var n = node;
    while (true) switch (tags[n]) {
        .root => return 0,
        .no_op => unreachable,

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
        .int_literal,
        .float_literal,
        .string_literal,
        .multiline_string_literal,
        .grouped_expression,
        .slice_type,
        .block_two,
        .block,
        // .container_decl,
        // .container_decl_two,
        // .container_decl_arg,
        .loop,
        // .while_simple,
        // .while_cont,
        // .@"while",
        // .for_simple,
        // .@"for",

        // change if adding pub keyword
        .fn_decl,
        .fn_proto_one,
        .fn_proto,
        .var_decl,
        .mut_var_decl,
        => return main_tokens[n],

        .slice_init_dot,
        .slice_init_dot_two,
        // .struct_init_dot,
        // .struct_init_dot_two,
        => return main_tokens[n] - 1,

        .field_access,
        .unwrap_optional,
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
        .optional_fallback,
        .bool_and,
        .bool_or,
        .slice_open,
        .slice,
        .slice_access,
        .slice_init_one,
        .slice_init,
        // .struct_init_one,
        // .struct_init,
        .call_one,
        .call,
        .match_range,
        // .for_range,
        .pipe,
        .lambda,
        => n = datas[n].lhs,

        // .assign_destructure => {
        //     const extra_idx = datas[n].lhs;
        //     const lhs_len = self.extra_data[extra_idx];
        //     assert(lhs_len > 0);
        //     n = self.extra_data[extra_idx + 1];
        // },

        // .container_field_init,
        // .container_field_align,
        // .container_field,
        // => {
        //     const name_token = main_tokens[n];
        //     if (name_token > 0 and token_tags[name_token - 1] == .keyword_comptime) {
        //         end_offset += 1;
        //     }
        //     return name_token - end_offset;
        // },

        .match_case_one => {
            if (datas[n].lhs == 0) {
                return main_tokens[n] - 1; // underscore token
            } else {
                n = datas[n].lhs;
            }
        },
        .match_case => {
            const extra = self.extraData(datas[n].lhs, Node.SubRange);
            assert(extra.end - extra.start > 0);
            n = self.extra_data[extra.start];
        },
    };
}

// TODO: some field are probably wrong
pub fn lastToken(self: Ast, node: Node.Index) TokenIndex {
    const tags = self.nodes.items(.tag);
    const datas = self.nodes.items(.data);
    const main_tokens = self.nodes.items(.main_token);
    var n = node;
    var end_offset: TokenIndex = 0;
    while (true) switch (tags[n]) {
        .root => return @as(TokenIndex, @intCast(self.tokens.len - 1)),
        .no_op => unreachable,

        .bool_not,
        .negation,
        .bit_not,
        .optional_type,
        .loop,
        .slice_type,
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
        // .assign_destructure,
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
        .optional_fallback,
        .bool_and,
        .bool_or,
        .@"if",
        // .while_simple,
        // .for_simple,
        .match_case_one,
        .match_case,
        .match_range,
        .pipe,
        .lambda,
        => n = datas[n].rhs,

        // .for_range => if (datas[n].rhs != 0) {
        //     n = datas[n].rhs;
        // } else {
        //     return main_tokens[n] + end_offset;
        // },

        .field_access,
        .unwrap_optional,
        .grouped_expression,
        .multiline_string_literal,
        // .error_value,
        => return datas[n].rhs + end_offset,

        .int_literal,
        .float_literal,
        .identifier,
        .string_literal,
        => return main_tokens[n] + end_offset,

        .@"return" => if (datas[n].lhs != 0) {
            n = datas[n].lhs;
        } else {
            return main_tokens[n] + end_offset;
        },

        .call,
        => {
            end_offset += 1; // for the rparen
            const params = self.extraData(datas[n].rhs, Node.SubRange);
            if (params.end - params.start == 0) {
                return main_tokens[n] + end_offset;
            }
            n = self.extra_data[params.end - 1]; // last parameter
        },
        .match => {
            const cases = self.extraData(datas[n].rhs, Node.SubRange);
            if (cases.end - cases.start == 0) {
                end_offset += 3; // rparen, lbrace, rbrace
                n = datas[n].lhs; // condition expression
            } else {
                end_offset += 1; // for the rbrace
                n = self.extra_data[cases.end - 1]; // last case
            }
        },
        // .container_decl_arg => {
        //     const members = self.extraData(datas[n].rhs, Node.SubRange);
        //     if (members.end - members.start == 0) {
        //         end_offset += 3; // for the rparen + lbrace + rbrace
        //         n = datas[n].lhs;
        //     } else {
        //         end_offset += 1; // for the rbrace
        //         n = self.extra_data[members.end - 1]; // last parameter
        //     }
        // },
        .slice_init,
        // .struct_init,
        => {
            const elements = self.extraData(datas[n].rhs, Node.SubRange);
            assert(elements.end - elements.start > 0);
            end_offset += 1; // for the rbrace
            n = self.extra_data[elements.end - 1]; // last element
        },
        .slice_init_dot,
        .block,
        // .struct_init_dot,
        // .container_decl,
        => {
            assert(datas[n].rhs - datas[n].lhs > 0);
            end_offset += 1; // for the rbrace
            n = self.extra_data[datas[n].rhs - 1]; // last statement
        },
        .call_one,
        .slice_access,
        => {
            end_offset += 1; // for the rparen/rbracket
            if (datas[n].rhs == 0) {
                return main_tokens[n] + end_offset;
            }
            n = datas[n].rhs;
        },
        .slice_init_dot_two,
        .block_two,
        // .struct_init_dot_two,
        // .container_decl_two,
        => {
            if (datas[n].rhs != 0) {
                end_offset += 1; // for the rparen/rbrace
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                end_offset += 1; // for the rparen/rbrace
                n = datas[n].lhs;
            } else {
                switch (tags[n]) {
                    .slice_init_dot_two,
                    .block_two,
                    // .struct_init_dot_two,
                    => end_offset += 1, // rbrace
                    // .container_decl_two => {
                    //     var i: u32 = 2; // lbrace + rbrace
                    //     while (token_tags[main_tokens[n] + i] == .container_doc_comment) i += 1;
                    //     end_offset += i;
                    // },
                    else => unreachable,
                }
                return main_tokens[n] + end_offset;
            }
        },
        .mut_var_decl, .var_decl => {
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                n = datas[n].lhs;
            } else {
                end_offset += 1; // from `let` token to name
                return main_tokens[n] + end_offset;
            }
        },
        // .container_field_init => {
        //     if (datas[n].rhs != 0) {
        //         n = datas[n].rhs;
        //     } else if (datas[n].lhs != 0) {
        //         n = datas[n].lhs;
        //     } else {
        //         return main_tokens[n] + end_offset;
        //     }
        // },
        // .container_field_align => {
        //     if (datas[n].rhs != 0) {
        //         end_offset += 1; // for the rparen
        //         n = datas[n].rhs;
        //     } else if (datas[n].lhs != 0) {
        //         n = datas[n].lhs;
        //     } else {
        //         return main_tokens[n] + end_offset;
        //     }
        // },
        // .container_field => {
        //     const extra = self.extraData(datas[n].rhs, Node.ContainerField);
        //     if (extra.value_expr != 0) {
        //         n = extra.value_expr;
        //     } else if (extra.align_expr != 0) {
        //         end_offset += 1; // for the rparen
        //         n = extra.align_expr;
        //     } else if (datas[n].lhs != 0) {
        //         n = datas[n].lhs;
        //     } else {
        //         return main_tokens[n] + end_offset;
        //     }
        // },

        .slice_init_one,
        // .struct_init_one,
        => {
            end_offset += 1; // rbrace
            if (datas[n].rhs == 0) {
                return main_tokens[n] + end_offset;
            } else {
                n = datas[n].rhs;
            }
        },
        .slice_open => {
            end_offset += 2; // ellipsis2 + rbracket, or comma + rparen
            n = datas[n].rhs;
            assert(n != 0);
        },
        .slice => {
            const extra = self.extraData(datas[n].rhs, Node.Slice);
            assert(extra.end != 0); // should have used slice_open
            end_offset += 1; // rbracket
            n = extra.end;
        },

        .@"continue", .@"break" => {
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                return datas[n].lhs + end_offset;
            } else {
                return main_tokens[n] + end_offset;
            }
        },
        .fn_decl, .fn_proto_one, .fn_proto => {
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else {
                n = datas[n].lhs;
            }
        },
        // .while_cont => {
        //     const extra = self.extraData(datas[n].rhs, Node.WhileCont);
        //     assert(extra.then_expr != 0);
        //     n = extra.then_expr;
        // },
        // .@"while" => {
        //     const extra = self.extraData(datas[n].rhs, Node.While);
        //     assert(extra.else_expr != 0);
        //     n = extra.else_expr;
        // },
        .if_else => {
            const extra = self.extraData(datas[n].rhs, Node.If);
            assert(extra.else_expr != 0);
            n = extra.else_expr;
        },
        // .@"for" => {
        //     const extra = @as(Node.For, @bitCast(datas[n].rhs));
        //     n = self.extra_data[datas[n].lhs + extra.inputs + @intFromBool(extra.has_else)];
        // },
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

pub fn varDecl(self: Ast, node: Node.Index) full.VarDecl {
    assert(self.nodes.items(.tag)[node] == .var_decl);
    const data = self.nodes.items(.data)[node];
    return (.{
        .ast = .{
            .let_token = self.nodes.items(.main_token)[node],
            .mut_token = null,
            .type_node = data.lhs,
            .init_node = data.rhs,
        },
    });
}

pub fn mutVarDecl(self: Ast, node: Node.Index) full.VarDecl {
    assert(self.nodes.items(.tag)[node] == .mut_var_decl);
    const data = self.nodes.items(.data)[node];
    return (.{
        .ast = .{
            .let_token = self.nodes.items(.main_token)[node],
            .mut_token = self.nodes.items(.main_token)[node] + 1,
            .type_node = data.lhs,
            .init_node = data.rhs,
        },
    });
}

// TODO: assignDestructure

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
            .else_expr = extra.then_expr,
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

// TODO: while
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

pub fn fullVarDecl(self: Ast, node: Node.Index) ?full.VarDecl {
    return switch (self.nodes.items(.tag)[node]) {
        .var_decl => self.varDecl(node),
        .mut_var_decl => self.mutVarDecl(node),
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
    pub const VarDecl = struct {
        ast: Components,

        pub const Components = struct {
            let_token: TokenIndex,
            mut_token: ?TokenIndex,
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

    // TODO
    pub const While = struct {
        ast: Components,
        // payload_token: ?TokenIndex,
        // error_token: ?TokenIndex,
        /// Populated only if else_expr != 0.
        else_token: TokenIndex,

        pub const Components = struct {
            while_token: TokenIndex,
            cond_expr: Node.Index,
            cont_expr: Node.Index,
            then_expr: Node.Index,
            else_expr: Node.Index,
        };
    };

    // TODO: For

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
            fn_expr: Node.Index,
            args: []const Node.Index,
        };
    };
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
        expected_comma_after_arg,
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
        assert(@sizeOf(Tag) == 1);
    }

    // TODO: why separate xxx and xxx_one?
    pub const Tag = enum {
        /// sub_list[lhs..rhs]
        root,
        /// `let a: lhs = rhs`. lhs and rhs may be unused.
        /// Can be local or global.
        /// main_token is `let`
        var_decl,
        /// `let mut a: lhs = rhs`. lhs and rhs may be unused.
        /// Can be local or global.
        /// main_token is `let`
        mut_var_decl,
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
        /// `lhs{rhs}`. rhs can be omitted.
        slice_init_one,
        /// `.{lhs, rhs}`. lhs and rhs can be omitted.
        slice_init_dot_two,
        /// `.{a, b}`. `sub_list[lhs..rhs]`.
        slice_init_dot,
        /// `lhs{a, b}`. `sub_range_list[rhs]`.
        slice_init,
        // TODO: if support "struct" struct_init
        /// `lhs(rhs)`. rhs can be omitted.
        /// main_token is the lparen.
        call_one,
        /// `lhs(a, b, c)`. `SubRange[rhs]`.
        /// main_token is the `(`.
        call,
        /// `match(lhs) {}`. `SubRange[rhs]`.
        match,
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

    pub const While = struct {
        cont_expr: Index,
        then_expr: Index,
        else_expr: Index,
    };

    // pub const WhileCont = struct {
    //     cont_expr: Index,
    //     then_expr: Index,
    // };

    // pub const For = packed struct(u32) {
    //     inputs: u32,
    //     has_else: bool,
    // };
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
