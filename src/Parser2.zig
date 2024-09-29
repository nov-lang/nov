const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const TokenIndex = Ast.TokenIndex;
const assert = std.debug.assert;

const Parser = @This();

source: []const u8,
allocator: std.mem.Allocator,
tokens: Ast.TokenList.Slice,
tok_i: TokenIndex,
nodes: Ast.NodeList,
errors: std.ArrayListUnmanaged(Ast.Error),
depth: u32,

const null_node = Ast.root_i; // TODO: replace null with null_node?

pub const Error = error{ParseError} || std.mem.Allocator.Error;

// pub const TypeRegistry = struct {
//     allocator: std.mem.Allocator,
// };

pub fn parse(self: *Parser) Error!void {
    var nodes: std.ArrayListUnmanaged(Node.Index) = .{};
    errdefer nodes.deinit(self.allocator);
    const root = try self.addNode(undefined);
    assert(root == Ast.root_i);

    // TODO: add decl/statement to main block/func
    while (true) {
        switch (self.tokens.items(.tag)[self.tok_i]) {
            .newline => self.tok_i += 1,
            .eof => break,
            else => {
                const stmt = self.expectStatement() catch |err| switch (err) {
                    error.OutOfMemory => return err,
                    error.ParseError => {
                        // TODO
                        // self.synchronize();
                        continue;
                    },
                };
                try nodes.append(self.allocator, stmt);
            },
        }
    }

    self.nodes.set(root, .{
        .root = .{
            .nodes = try nodes.toOwnedSlice(self.allocator),
        },
    });
}

fn addNode(self: *Parser, node: Ast.Node) Error!Node.Index {
    const result: Node.Index = @intCast(self.nodes.len);
    try self.nodes.append(self.allocator, node);
    return result;
}

/// Declaration <- DeclProto (EQUAL Expr)? NEWLINE
/// DeclProto <- KEYWORD_let KEYWORD_priv? KEYWORD_mut? IDENTIFIER (COLON TypeExpr)?
fn expectDeclaration(self: *Parser) Error!Node.Index {
    const let_token = self.assertToken(.keyword_let);
    const is_pub = blk: {
        if (self.consume(.keyword_priv) == null) {
            break :blk self.depth == 0;
        }
        if (self.depth > 0) {
            // TODO: warn unnecessary priv
            break :blk false;
        } else {
            break :blk true;
        }
    };
    const is_mut = self.consume(.keyword_mut) != null;
    // const is_rec = self.consume(.keyword_rec) != null;

    const name = try self.expectToken(.identifier);
    const type_expr = if (self.consume(.colon) != null) try self.expectTypeExpr() else null;
    const initializer = if (self.consume(.equal) != null) try self.expectExpr() else null;

    if (type_expr == null and initializer == null) {
        return self.failExpected(.equal);
    }

    try self.expectNewline(.expected_newline_after_decl, true);

    return self.addNode(.{
        .declaration = .{
            .token = let_token,
            .name = name,
            .value = initializer,
            .type = type_expr,
            .mutable = is_mut,
            .public = is_pub,
        },
    });
}

/// TypeExpr <- PrefixTypeOp* (IDENTIFIER / TupleTypeExpr / FuncTypeExpr)
/// PrefixTypeOp <- QUESTIONMARK / SliceTypeStart
/// SliceTypeStart <- LBRACKET RBRACKET
/// TupleTypeExpr <- LPAREN TypeExpr (COMMA TypeExpr)* RPAREN
fn parseTypeExpr(self: *Parser) Error!?Node.Index {
    switch (self.tokens.items(.tag)[self.tok_i]) {
        .question_mark => return try self.addNode(.{
            .type_expr = .{
                .token = self.nextToken(),
                .node = try self.expectTypeExpr(),
                .optional = true,
            },
        }),
        .l_bracket => {
            _ = self.nextToken(); // discard l_bracket
            _ = try self.expectToken(.r_bracket);
            return try self.addNode(.{
                .type_expr = .{
                    .token = self.nextToken(),
                    .node = try self.expectTypeExpr(),
                    .slice = true,
                },
            });
        },
        .l_paren => {
            // TODO: implement tuples
            return try self.expectFuncTypeExpr();
        },
        // .identifier => return self.addNode(.{
        //     .type_expr = .{
        //         .token = self.nextToken(),
        //         .node = self.addNode(.{
        //             .identifier = .{
        //                 .token = self.nextToken(),
        //             },
        //         }),
        //     },
        // }),
        else => return null,
    }
}

/// FuncTypeExpr <- LPAREN FuncParams RPAREN ARROW LPAREN TypeExpr? RPAREN
fn expectFuncTypeExpr(self: *Parser) Error!?Node.Index {
    const main_token = self.assertToken(.l_paren);
    const params = try self.parseFuncParams();
    errdefer self.allocator.free(params);
    _ = self.assertToken(.r_paren);

    _ = try self.expectToken(.minus_r_angle_bracket);
    _ = try self.expectToken(.l_paren);
    const return_type = try self.parseTypeExpr();
    _ = try self.expectToken(.r_paren);

    _ = main_token;
    _ = return_type;
    unreachable; // TODO
    // return self.addNode(.{
    //     .type_expr = .{
    //         .token = l_paren,
    //         .node = self.addNode(.{
    //             .func_type = .{
    //                 .params = params,
    //                 .return_type = return_type,
    //             },
    //         }),
    //     },
    // });
}

/// FuncParams <- (FuncParam COMMA)*
/// FuncParam <- (IDENTIFIER COLON)? TypeExpr
fn parseFuncParams(self: *Parser) Error![]const Node.Index {
    var params: std.ArrayListUnmanaged(Node.Index) = .{};
    errdefer params.deinit(self.allocator);

    while (true) {
        if (self.tokens.items(.tag)[self.tok_i] == .r_paren) {
            break;
        }

        // TODO: parse params
    }

    return params.toOwnedSlice(self.allocator);
}

fn expectTypeExpr(self: *Parser) Error!Node.Index {
    return try self.parseTypeExpr() orelse return self.fail(.expected_type_expr);
}

fn expectStatement(self: *Parser) Error!Node.Index {
    return switch (self.tokens.items(.tag)[self.tok_i]) {
        .keyword_let => self.expectDeclaration(),
        .keyword_loop => self.expectLoopStatement(),
        // .keyword_while => self.parseWhileStatement(),
        // .keyword_for => self.parseForStatement(),
        // .keyword_match => self.parseMatchStatement(),
        .l_brace => try self.parseBlockStatement() orelse return self.fail(.expected_expr_or_assignment), // TODO

        .keyword_return => self.expectReturnStatement(),
        .keyword_break => self.expectBreakStatement(),
        .keyword_continue => self.expectContinueStatement(),
        // TODO
        else => try self.parseExpr() orelse self.fail(.expected_expr_or_assignment),
    };
}

fn expectStatementRecoverable(self: *Parser) Error!?Node.Index {
    while (true) {
        return self.expectStatement() catch |err| switch (err) {
            error.OutOfMemory => return err,
            error.ParseError => {
                self.findNextStmt();
                switch (self.tokens.items(.tag)[self.tok_i]) {
                    .r_brace => return null,
                    .eof => return error.ParseError,
                    else => continue,
                }
            },
        };
    }
}

// TODO: replace BlockStatement with Block
/// LoopStatement <- KEYWORD_loop Block
fn expectLoopStatement(self: *Parser) Error!Node.Index {
    const loop_token = self.assertToken(.keyword_loop);
    const body = try self.expectBlockStatement();

    return self.addNode(.{
        .loop = .{
            .token = loop_token,
            .body = body,
        },
    });
}

/// ReturnStatement <- KEYWORD_return Expr? NEWLINE
fn expectReturnStatement(self: *Parser) Error!Node.Index {
    const return_token = self.assertToken(.keyword_return);
    const expr = if (self.tokens.items(.tag)[self.tok_i] == .newline) null else try self.parseExpr();
    try self.expectNewline(.expected_newline_after_stmt, true);

    return self.addNode(.{
        .@"return" = .{
            .token = return_token,
            .value = expr,
        },
    });
}

/// BreakStatement <- KEYWORD_break Expr? NEWLINE
fn expectBreakStatement(self: *Parser) Error!Node.Index {
    const break_token = self.assertToken(.keyword_break);
    const expr = if (self.tokens.items(.tag)[self.tok_i] == .newline) null else try self.parseExpr();
    try self.expectNewline(.expected_newline_after_stmt, true);

    return self.addNode(.{
        .@"break" = .{
            .token = break_token,
            .value = expr,
        },
    });
}

/// ContinueStatement <- KEYWORD_continue NEWLINE
fn expectContinueStatement(self: *Parser) Error!Node.Index {
    const continue_token = self.assertToken(.keyword_continue);
    try self.expectNewline(.expected_newline_after_stmt, true);

    return self.addNode(.{
        .@"continue" = .{
            .token = continue_token,
        },
    });
}

/// BlockStatement <- Block / AssignExpr NEWLINE
fn parseBlockStatement(self: *Parser) Error!?Node.Index {
    if (try self.parseBlock()) |block| {
        return block;
    }

    if (try self.parseAssignExpr()) |expr| {
        try self.expectNewline(.expected_newline_after_stmt, true);
        return expr;
    }

    return null;
}

fn expectBlockStatement(self: *Parser) Error!Node.Index {
    return try self.parseBlockStatement() orelse self.fail(.expected_block_or_expr);
}

fn parseBlock(self: *Parser) Error!?Node.Index {
    const lbrace = self.consume(.l_brace) orelse return null;
    var statements: std.ArrayListUnmanaged(Node.Index) = .{};
    errdefer statements.deinit(self.allocator);

    self.depth += 1;
    defer self.depth -= 1;

    while (true) {
        self.discardNewlines();
        if (self.tokens.items(.tag)[self.tok_i] == .r_brace) {
            break;
        }
        const statement = try self.expectStatementRecoverable() orelse break;
        try statements.append(self.allocator, statement);
    }
    _ = try self.expectToken(.r_brace);

    return try self.addNode(.{
        .block = .{
            .token = lbrace,
            .statements = try statements.toOwnedSlice(self.allocator),
        },
    });
}

// TODO
fn parseAssignExpr(self: *Parser) Error!?Node.Index {
    return self.parseExpr();
}

fn parseExpr(self: *Parser) Error!?Node.Index {
    return self.parsePrecedence(.assignment);
}

fn expectExpr(p: *Parser) Error!Node.Index {
    return try p.parseExpr() orelse return p.fail(.expected_expr);
}

const Precedence = enum(i8) {
    none = -1,
    assignment = 0,
    bool_or = 10,
    bool_and = 20,
    equality = 30,
    comparison = 40,
    bitwise = 50,
    shift = 60,
    term = 70,
    factor = 80,
    unary = 90,
    null_coalescing = 100,
    call = 110,
    primary = 120,
    _,
};

const Rule = struct {
    // prefix: ?*const fn (*Parser, bool) Error!Node.Index = null,
    // infix: ?*const fn (*Parser, bool, Node.Index) Error!Node.Index = null,
    prec: Precedence = .none,
    assoc: enum { none, left } = .left,
};

// const rules = std.enums.directEnumArrayDefault(Token.Tag, Rule, .{}, 0, .{
//     .keyword_or = .{ .prec = .bool_or },

//     .keyword_and = .{ .prec = .bool_and, .op = .bool_and },

//     .equal_equal = .{ .prec = .equality, .op = .equal_equal, .assoc = .none },
//     .bang_equal = .{ .prec = .equality, .op = .bang_equal, .assoc = .none },

//     .l_angle_bracket = .{ .prec = .comparison, .op = .less_than, .assoc = .none },
//     .r_angle_bracket = .{ .prec = .comparison, .op = .greater_than, .assoc = .none },
//     .l_angle_bracket_equal = .{ .prec = .comparison, .op = .less_or_equal, .assoc = .none },
//     .r_angle_bracket_equal = .{ .prec = .comparison, .op = .greater_or_equal, .assoc = .none },

//     .ampersand = .{ .prec = .bitwise, .op = .bit_and },
//     .caret = .{ .prec = .bitwise, .op = .bit_xor },
//     .pipe = .{ .prec = .bitwise, .op = .bit_or },

//     .l_angle_bracket_angle_bracket = .{ .prec = .shift, .op = .shl },
//     .r_angle_bracket_angle_bracket = .{ .prec = .shift, .op = .shr },

//     .plus = .{ .prec = .term, .op = .add },
//     .minus = .{ .prec = .term, .op = .sub },

//     .asterisk = .{ .prec = .factor, .op = .mul },
//     .slash = .{ .prec = .factor, .op = .div },
//     .percent = .{ .prec = .factor, .op = .mod },
// });

const rules = std.enums.directEnumArrayDefault(Token.Tag, Rule, .{}, 0, .{
    .keyword_or = .{ .prec = .bool_or },

    .keyword_and = .{ .prec = .bool_and },

    .equal_equal = .{ .prec = .equality, .assoc = .none },
    .bang_equal = .{ .prec = .equality, .assoc = .none },

    .l_angle_bracket = .{ .prec = .comparison, .assoc = .none },
    .r_angle_bracket = .{ .prec = .comparison, .assoc = .none },
    .l_angle_bracket_equal = .{ .prec = .comparison, .assoc = .none },
    .r_angle_bracket_equal = .{ .prec = .comparison, .assoc = .none },

    .ampersand = .{ .prec = .bitwise },
    .caret = .{ .prec = .bitwise },
    .pipe = .{ .prec = .bitwise },

    .l_angle_bracket_angle_bracket = .{ .prec = .shift },
    .r_angle_bracket_angle_bracket = .{ .prec = .shift },

    .plus = .{ .prec = .term },
    .minus = .{ .prec = .term },

    .asterisk = .{ .prec = .factor },
    .slash = .{ .prec = .factor },
    .percent = .{ .prec = .factor },
});

fn parsePrecedence(self: *Parser, min_prec: Precedence) Error!?Node.Index {
    assert(@intFromEnum(min_prec) >= 0);
    var node = try self.parsePrefixExpr() orelse return null;

    var banned_prec: Precedence = .none;

    while (true) {
        const tok_tag = self.tokens.items(.tag)[self.tok_i];
        const rule = rules[@intFromEnum(tok_tag)];
        if (@intFromEnum(rule.prec) < @intFromEnum(min_prec)) {
            break;
        }
        if (@intFromEnum(rule.prec) == @intFromEnum(banned_prec)) {
            return self.fail(.chained_comparison_operators);
        }

        const op_token = self.nextToken();
        // Special-case handling for "catch"
        // if (tok_tag == .keyword_catch) {
        //     _ = try self.parsePayload();
        // }
        const rhs = try self.parsePrecedence(@enumFromInt(@intFromEnum(rule.prec) + 1)) orelse {
            try self.warn(.expected_expr);
            return node;
        };

        // {
        //     const tok_len = tok_tag.lexeme().?.len;
        //     const char_before = self.source[self.token_starts[oper_token] - 1];
        //     const char_after = self.source[self.token_starts[oper_token] + tok_len];
        //     if (tok_tag == .ampersand and char_after == '&') {
        //         // warn about '&&' being used instead of 'and'
        //         try self.warnMsg(.{ .tag = .invalid_ampersand_ampersand, .token = oper_token });
        //     } else if (std.ascii.isWhitespace(char_before) != std.ascii.isWhitespace(char_after)) {
        //         try self.warnMsg(.{ .tag = .mismatched_binary_op_whitespace, .token = oper_token });
        //     }
        // }

        node = try self.addNode(.{
            .binary = .{
                .token = op_token,
                .left = node,
                .right = rhs,
            },
        });

        if (rule.assoc == .none) {
            banned_prec = rule.prec;
        }
    }

    return node;
}

/// PrefixExpr <- PrefixOp* PrimaryExpr
/// PrefixOp <- EXCLAMATIONMARK / MINUS / TILDE
fn parsePrefixExpr(self: *Parser) Error!?Node.Index {
    switch (self.tokens.items(.tag)[self.tok_i]) {
        .bang, .minus, .tilde => return try self.addNode(.{
            .unary = .{
                .token = self.nextToken(),
                .expr = (try self.parsePrefixExpr()).?, // TODO
            },
        }),
        else => return self.parsePrimaryExpr(),
    }
}

// TODO XXX
/// PrimaryTypeExpr
///     <- BUILTINIDENTIFIER FnCallArguments
///      / ContainerDecl
///      / DOT IDENTIFIER
///      / DOT InitList
///      / ErrorSetDecl
///      / FLOAT
///      / FnProto
///      / GroupedExpr
///      / IDENTIFIER
///      / IfTypeExpr
///      / INTEGER
///      / KEYWORD_error DOT IDENTIFIER
///      / STRINGLITERAL
///
/// ContainerDecl <- (KEYWORD_extern / KEYWORD_packed)? ContainerDeclAuto
///
/// ContainerDeclAuto <- ContainerDeclType LBRACE container_doc_comment? ContainerMembers RBRACE
///
/// InitList
///     <- LBRACE FieldInit (COMMA FieldInit)* COMMA? RBRACE
///      / LBRACE Expr (COMMA Expr)* COMMA? RBRACE
///      / LBRACE RBRACE
///
/// ErrorSetDecl <- KEYWORD_error LBRACE IdentifierList RBRACE
///
/// GroupedExpr <- LPAREN Expr RPAREN
///
/// IfTypeExpr <- IfPrefix TypeExpr (KEYWORD_else Payload? TypeExpr)?
fn parsePrimaryTypeExpr(self: *Parser) Error!?Node.Index {
    switch (self.tokens.items(.tag)[self.tok_i]) {
        .int_literal => {
            const start = self.tokens.items(.start)[self.tok_i];
            const end = self.tokens.items(.end)[self.tok_i];
            return try self.addNode(.{
                .int_literal = .{
                    .token = self.nextToken(),
                    .value = std.fmt.parseUnsigned(u64, self.source[start..end], 0) catch {
                        // TODO: fail
                        unreachable;
                    },
                },
            });
        },
        .float_literal => {
            const start = self.tokens.items(.start)[self.tok_i];
            const end = self.tokens.items(.end)[self.tok_i];
            return try self.addNode(.{
                .float = .{
                    .token = self.nextToken(),
                    .value = std.fmt.parseFloat(f64, self.source[start..end]) catch {
                        // TODO: fail
                        unreachable;
                    },
                },
            });
        },
        // .string_literal => {
        //     return self.addNode(.{
        //         .tag = .string_literal,
        //         .main_token = self.nextToken(),
        //         .data = .{
        //             .lhs = undefined,
        //             .rhs = undefined,
        //         },
        //     });
        // },
        .identifier => return try self.addNode(.{
            .identifier = .{
                .token = self.nextToken(),
            },
        }),
        // .keyword_fn => return self.parseFnProto(),
        // .keyword_if => return self.parseIf(expectTypeExpr),
        // // .keyword_struct,
        // // .keyword_opaque,
        // // .keyword_enum,
        // // .keyword_union,
        // // => return self.parseContainerDeclAuto(),
        // .multiline_string_literal_line => {
        //     const first_line = self.nextToken();
        //     while (self.tokens.items(.tag)[self.tok_i] == .multiline_string_literal_line) {
        //         self.tok_i += 1;
        //     }
        //     return self.addNode(.{
        //         .tag = .multiline_string_literal,
        //         .main_token = first_line,
        //         .data = .{
        //             .lhs = first_line,
        //             .rhs = self.tok_i - 1,
        //         },
        //     });
        // },
        // .period => switch (self.tokens.items(.tag)[self.tok_i + 1]) {
        //     .identifier => return self.addNode(.{
        //         .tag = .enum_literal,
        //         .data = .{
        //             .lhs = self.nextToken(), // dot
        //             .rhs = undefined,
        //         },
        //         .main_token = self.nextToken(), // identifier
        //     }),
        //     .l_brace => {
        //         const lbrace = self.tok_i + 1;
        //         self.tok_i = lbrace + 1;

        //         // If there are 0, 1, or 2 items, we can use ArrayInitDotTwo/StructInitDotTwo;
        //         // otherwise we use the full ArrayInitDot/StructInitDot.

        //         const scratch_top = self.scratch.items.len;
        //         defer self.scratch.shrinkRetainingCapacity(scratch_top);
        //         const field_init = try self.parseFieldInit();
        //         if (field_init != 0) {
        //             try self.scratch.append(self.allocator, field_init);
        //             while (true) {
        //                 switch (self.tokens.items(.tag)[self.tok_i]) {
        //                     .comma => self.tok_i += 1,
        //                     .r_brace => {
        //                         self.tok_i += 1;
        //                         break;
        //                     },
        //                     .colon, .r_paren, .r_bracket => return self.failExpected(.r_brace),
        //                     // Likely just a missing comma; give error but continue parsing.
        //                     else => try self.warn(.expected_comma_after_initializer),
        //                 }
        //                 if (self.consume(.r_brace)) |_| break;
        //                 const next = try self.expectFieldInit();
        //                 try self.scratch.append(self.allocator, next);
        //             }
        //             const comma = (self.tokens.items(.tag)[self.tok_i - 2] == .comma);
        //             const inits = self.scratch.items[scratch_top..];
        //             switch (inits.len) {
        //                 0 => unreachable,
        //                 1 => return self.addNode(.{
        //                     .tag = if (comma) .struct_init_dot_two_comma else .struct_init_dot_two,
        //                     .main_token = lbrace,
        //                     .data = .{
        //                         .lhs = inits[0],
        //                         .rhs = 0,
        //                     },
        //                 }),
        //                 2 => return self.addNode(.{
        //                     .tag = if (comma) .struct_init_dot_two_comma else .struct_init_dot_two,
        //                     .main_token = lbrace,
        //                     .data = .{
        //                         .lhs = inits[0],
        //                         .rhs = inits[1],
        //                     },
        //                 }),
        //                 else => {
        //                     const span = try self.listToSpan(inits);
        //                     return self.addNode(.{
        //                         .tag = if (comma) .struct_init_dot_comma else .struct_init_dot,
        //                         .main_token = lbrace,
        //                         .data = .{
        //                             .lhs = span.start,
        //                             .rhs = span.end,
        //                         },
        //                     });
        //                 },
        //             }
        //         }

        //         while (true) {
        //             if (self.consume(.r_brace)) |_| break;
        //             const elem_init = try self.expectExpr();
        //             try self.scratch.append(self.allocator, elem_init);
        //             switch (self.tokens.items(.tag)[self.tok_i]) {
        //                 .comma => self.tok_i += 1,
        //                 .r_brace => {
        //                     self.tok_i += 1;
        //                     break;
        //                 },
        //                 .colon, .r_paren, .r_bracket => return self.failExpected(.r_brace),
        //                 // Likely just a missing comma; give error but continue parsing.
        //                 else => try self.warn(.expected_comma_after_initializer),
        //             }
        //         }
        //         const comma = (self.tokens.items(.tag)[self.tok_i - 2] == .comma);
        //         const inits = self.scratch.items[scratch_top..];
        //         switch (inits.len) {
        //             0 => return self.addNode(.{
        //                 .tag = .struct_init_dot_two,
        //                 .main_token = lbrace,
        //                 .data = .{
        //                     .lhs = 0,
        //                     .rhs = 0,
        //                 },
        //             }),
        //             1 => return self.addNode(.{
        //                 .tag = if (comma) .array_init_dot_two_comma else .array_init_dot_two,
        //                 .main_token = lbrace,
        //                 .data = .{
        //                     .lhs = inits[0],
        //                     .rhs = 0,
        //                 },
        //             }),
        //             2 => return self.addNode(.{
        //                 .tag = if (comma) .array_init_dot_two_comma else .array_init_dot_two,
        //                 .main_token = lbrace,
        //                 .data = .{
        //                     .lhs = inits[0],
        //                     .rhs = inits[1],
        //                 },
        //             }),
        //             else => {
        //                 const span = try self.listToSpan(inits);
        //                 return self.addNode(.{
        //                     .tag = if (comma) .array_init_dot_comma else .array_init_dot,
        //                     .main_token = lbrace,
        //                     .data = .{
        //                         .lhs = span.start,
        //                         .rhs = span.end,
        //                     },
        //                 });
        //             },
        //         }
        //     },
        //     else => return null_node,
        // },
        // .keyword_error => switch (self.tokens.items(.tag)[self.tok_i + 1]) {
        //     .l_brace => {
        //         const error_token = self.tok_i;
        //         self.tok_i += 2;
        //         while (true) {
        //             if (self.consume(.r_brace)) |_| break;
        //             _ = try self.eatDocComments();
        //             _ = try self.expectToken(.identifier);
        //             switch (self.tokens.items(.tag)[self.tok_i]) {
        //                 .comma => self.tok_i += 1,
        //                 .r_brace => {
        //                     self.tok_i += 1;
        //                     break;
        //                 },
        //                 .colon, .r_paren, .r_bracket => return self.failExpected(.r_brace),
        //                 // Likely just a missing comma; give error but continue parsing.
        //                 else => try self.warn(.expected_comma_after_field),
        //             }
        //         }
        //         return self.addNode(.{
        //             .tag = .error_set_decl,
        //             .main_token = error_token,
        //             .data = .{
        //                 .lhs = undefined,
        //                 .rhs = self.tok_i - 1, // rbrace
        //             },
        //         });
        //     },
        //     else => {
        //         const main_token = self.nextToken();
        //         const period = self.consume(.period);
        //         if (period == null) try self.warnExpected(.period);
        //         const identifier = self.consume(.identifier);
        //         if (identifier == null) try self.warnExpected(.identifier);
        //         return self.addNode(.{
        //             .tag = .error_value,
        //             .main_token = main_token,
        //             .data = .{
        //                 .lhs = period orelse 0,
        //                 .rhs = identifier orelse 0,
        //             },
        //         });
        //     },
        // },
        .l_paren => {
            const node = try self.addNode(.{
                .grouping = .{
                    .token = self.nextToken(),
                    .expr = try self.expectExpr(),
                },
            });
            _ = try self.expectToken(.r_paren);
            return node;
        },
        else => return null,
    }
}

/// SuffixExpr
///     <- PrimaryTypeExpr (SuffixOp / FnCallArguments)*
///
/// FnCallArguments <- LPAREN ExprList RPAREN
///
/// ExprList <- (Expr COMMA)* Expr?
fn parseSuffixExpr(self: *Parser) Error!?Node.Index {
    var res = try self.parsePrimaryTypeExpr() orelse return null;
    res = res;
    return res;
    // TODO;
    // while (true) {
    //     if (try self.parseSuffixOp(res)) |suffix_op| {
    //         res = suffix_op;
    //         continue;
    //     }
    //     const lparen = self.consume(.l_paren) orelse return res;
    //     const scratch_top = self.scratch.items.len;
    //     defer self.scratch.shrinkRetainingCapacity(scratch_top);
    //     while (true) {
    //         if (self.consume(.r_paren)) |_| break;
    //         const param = try self.expectExpr();
    //         try self.scratch.append(self.allocator, param);
    //         switch (self.tokens.items(.tag)[self.tok_i]) {
    //             .comma => self.tok_i += 1,
    //             .r_paren => {
    //                 self.tok_i += 1;
    //                 break;
    //             },
    //             .colon, .r_brace, .r_bracket => return self.failExpected(.r_paren),
    //             // Likely just a missing comma; give error but continue parsing.
    //             else => try self.warn(.expected_comma_after_arg),
    //         }
    //     }
    //     const params = self.scratch.items[scratch_top..];
    //     res = switch (params.len) {
    //         0 => try self.addNode(.{
    //             .tag = .call_one,
    //             .main_token = lparen,
    //             .data = .{
    //                 .lhs = res,
    //                 .rhs = 0,
    //             },
    //         }),
    //         1 => try self.addNode(.{
    //             .tag = .call_one,
    //             .main_token = lparen,
    //             .data = .{
    //                 .lhs = res,
    //                 .rhs = params[0],
    //             },
    //         }),
    //         else => try self.addNode(.{
    //             .tag = .call,
    //             .main_token = lparen,
    //             .data = .{
    //                 .lhs = res,
    //                 .rhs = try self.addExtra(try self.listToSpan(params)),
    //             },
    //         }),
    //     };
    // }
}

/// SuffixOp
///     <- LBRACKET Expr (DOT2 (Expr? (COLON Expr)?)?)? RBRACKET
///      / DOT IDENTIFIER // TODO field access
///      / DOTASTERISK
///      / DOTQUESTIONMARK // TODO unwrap_optional
fn parseSuffixOp(self: *Parser, lhs: Node.Index) Error!?Node.Index {
    _ = self;
    _ = lhs;
    return null;
    // switch (self.tokens.items(.tag)[self.tok_i]) {
    //     .l_bracket => {
    //         const lbracket = self.nextToken();
    //         const index_expr = try self.expectExpr();

    //         if (self.consume(.ellipsis2)) |_| {
    //             const end_expr = try self.parseExpr();
    //             if (self.consume(.colon)) |_| {
    //                 const sentinel = try self.expectExpr();
    //                 _ = try self.expectToken(.r_bracket);
    //                 return self.addNode(.{
    //                     .tag = .slice_sentinel,
    //                     .main_token = lbracket,
    //                     .data = .{
    //                         .lhs = lhs,
    //                         .rhs = try self.addExtra(Node.SliceSentinel{
    //                             .start = index_expr,
    //                             .end = end_expr,
    //                             .sentinel = sentinel,
    //                         }),
    //                     },
    //                 });
    //             }
    //             _ = try self.expectToken(.r_bracket);
    //             if (end_expr == 0) {
    //                 return self.addNode(.{
    //                     .tag = .slice_open,
    //                     .main_token = lbracket,
    //                     .data = .{
    //                         .lhs = lhs,
    //                         .rhs = index_expr,
    //                     },
    //                 });
    //             }
    //             return self.addNode(.{
    //                 .tag = .slice,
    //                 .main_token = lbracket,
    //                 .data = .{
    //                     .lhs = lhs,
    //                     .rhs = try self.addExtra(Node.Slice{
    //                         .start = index_expr,
    //                         .end = end_expr,
    //                     }),
    //                 },
    //             });
    //         }
    //         _ = try self.expectToken(.r_bracket);
    //         return self.addNode(.{
    //             .tag = .array_access,
    //             .main_token = lbracket,
    //             .data = .{
    //                 .lhs = lhs,
    //                 .rhs = index_expr,
    //             },
    //         });
    //     },
    //     .period_asterisk => return self.addNode(.{
    //         .tag = .deref,
    //         .main_token = self.nextToken(),
    //         .data = .{
    //             .lhs = lhs,
    //             .rhs = undefined,
    //         },
    //     }),
    //     .invalid_periodasterisks => {
    //         try self.warn(.asterisk_after_ptr_deref);
    //         return self.addNode(.{
    //             .tag = .deref,
    //             .main_token = self.nextToken(),
    //             .data = .{
    //                 .lhs = lhs,
    //                 .rhs = undefined,
    //             },
    //         });
    //     },
    //     .period => switch (self.tokens.items(.tag)[self.tok_i + 1]) {
    //         .identifier => return self.addNode(.{
    //             .tag = .field_access,
    //             .main_token = self.nextToken(),
    //             .data = .{
    //                 .lhs = lhs,
    //                 .rhs = self.nextToken(),
    //             },
    //         }),
    //         .question_mark => return self.addNode(.{
    //             .tag = .unwrap_optional,
    //             .main_token = self.nextToken(),
    //             .data = .{
    //                 .lhs = lhs,
    //                 .rhs = self.nextToken(),
    //             },
    //         }),
    //         .l_brace => {
    //             // this a misplaced `.{`, handle the error somewhere else
    //             return null_node;
    //         },
    //         else => {
    //             self.tok_i += 1;
    //             try self.warn(.expected_suffix_op);
    //             return null_node;
    //         },
    //     },
    //     else => return null_node,
    // }
}

/// PrimaryExpr
///     <- IfExpr
///      / KEYWORD_break Expr?
///      / KEYWORD_continue
///      / KEYWORD_return Expr?
///      / Block
///      / CurlySuffixExpr // TODO
fn parsePrimaryExpr(self: *Parser) Error!?Node.Index {
    return switch (self.tokens.items(.tag)[self.tok_i]) {
        .keyword_if => try self.expectIfExpr(),
        .keyword_break => try self.addNode(.{
            .@"break" = .{
                .token = self.nextToken(),
                .value = try self.parseExpr(),
            },
        }),
        .keyword_continue => try self.addNode(.{
            .@"continue" = .{
                .token = self.nextToken(),
            },
        }),
        .keyword_return => try self.addNode(.{
            .@"return" = .{
                .token = self.nextToken(),
                .value = try self.parseExpr(),
            },
        }),
        .l_brace => try self.parseBlock(),
        else => {
            // TODO
            return try self.parseSuffixExpr();
            // return self.parseCurlySuffixExpr(),
        },
    };
}

/// IfExpr <- KEYWORD_if LPAREN Expr RPAREN Expr (KEYWORD_else Expr)?
fn expectIfExpr(self: *Parser) Error!?Node.Index {
    const if_token = self.assertToken(.keyword_if);
    _ = try self.expectToken(.l_paren);
    const condition = try self.expectExpr();
    _ = try self.expectToken(.r_paren);

    const then_expr = try expectExpr(self);
    if (self.consume(.keyword_else) == null) {
        return try self.addNode(.{
            .@"if" = .{
                .token = if_token,
                .condition = condition,
                .then_expr = then_expr,
            },
        });
    }

    const else_expr = try expectExpr(self);
    return try self.addNode(.{
        .@"if" = .{
            .token = if_token,
            .condition = condition,
            .then_expr = then_expr,
            .else_expr = else_expr,
        },
    });
}

fn warnExpected(self: *Parser, expected_token: Token.Tag) Error!void {
    @setCold(true);
    try self.warnMsg(.{
        .tag = .expected_token,
        .token = self.tok_i,
        .extra = .{ .expected_tag = expected_token },
    });
}

fn warn(self: *Parser, error_tag: Ast.Error.Tag) Error!void {
    @setCold(true);
    try self.warnMsg(.{ .tag = error_tag, .token = self.tok_i });
}

fn warnMsg(self: *Parser, msg: Ast.Error) Error!void {
    @setCold(true);
    switch (msg.tag) {
        .expected_newline_after_decl,
        .expected_newline_after_stmt,
        .expected_newline_or_else,
        .expected_newline_or_lbrace,
        // .expected_comma_after_field,
        .expected_comma_after_arg,
        // .expected_comma_after_param,
        // .expected_comma_after_initializer,
        .expected_comma_after_match_prong,
        // .expected_comma_after_for_operand,
        // .expected_comma_after_capture,
        .expected_token,
        .expected_block,
        .expected_block_or_assignment,
        // .expected_block_or_expr,
        // .expected_block_or_field,
        .expected_expr,
        .expected_expr_or_assignment,
        // .expected_labelable,
        // .expected_param_list,
        .expected_prefix_expr,
        // .expected_primary_type_expr,
        // .expected_suffix_op,
        .expected_type_expr,
        // .expected_loop_payload,
        // .expected_container,
        => if (msg.token != 0 and !self.tokensOnSameLine(msg.token - 1, msg.token)) {
            var copy = msg;
            copy.token_is_prev = true;
            copy.token -= 1;
            return self.errors.append(self.allocator, copy);
        },
        else => {},
    }
    try self.errors.append(self.allocator, msg);
}

fn failExpected(self: *Parser, expected_token: Token.Tag) Error {
    @setCold(true);
    return self.failMsg(.{
        .tag = .expected_token,
        .token = self.tok_i,
        .extra = .{ .expected_tag = expected_token },
    });
}

fn fail(self: *Parser, tag: Ast.Error.Tag) Error {
    @setCold(true);
    return self.failMsg(.{ .tag = tag, .token = self.tok_i });
}

fn failMsg(self: *Parser, msg: Ast.Error) Error {
    @setCold(true);
    try self.warnMsg(msg);
    return error.ParseError;
}

fn tokensOnSameLine(self: *Parser, tok1: TokenIndex, tok2: TokenIndex) bool {
    const token_starts = self.tokens.items(.start);
    return std.mem.indexOfScalar(u8, self.source[token_starts[tok1]..token_starts[tok2]], '\n') == null;
}

/// Returns current token and advances the token index
fn nextToken(self: *Parser) TokenIndex {
    const result = self.tok_i;
    self.tok_i += 1;
    return result;
}

fn expectToken(self: *Parser, tag: Token.Tag) Error!TokenIndex {
    if (self.tokens.items(.tag)[self.tok_i] != tag) {
        return self.failExpected(tag);
    }
    return self.nextToken();
}

fn expectNewline(self: *Parser, error_tag: Ast.Error.Tag, recoverable: bool) Error!void {
    if (self.tokens.items(.tag)[self.tok_i] == .newline) {
        _ = self.nextToken();
        return;
    }
    try self.warn(error_tag);
    if (!recoverable) {
        return error.ParseError;
    }
}

fn discardNewlines(self: *Parser) void {
    while (self.tokens.items(.tag)[self.tok_i] == .newline) {
        self.tok_i += 1;
    }
}

fn consume(self: *Parser, tag: Token.Tag) ?TokenIndex {
    return if (self.tokens.items(.tag)[self.tok_i] == tag) self.nextToken() else null;
}

fn assertToken(self: *Parser, tag: Token.Tag) TokenIndex {
    const token = self.nextToken();
    assert(self.tokens.items(.tag)[token] == tag);
    return token;
}

fn findNextStmt(self: *Parser) void {
    var level: u32 = 0;
    while (true) {
        const tok = self.nextToken();
        switch (self.tokens.items(.tag)[tok]) {
            .l_brace => level += 1,
            .r_brace => {
                if (level == 0) {
                    self.tok_i -= 1;
                    return;
                }
                level -= 1;
            },
            .newline => {
                if (level == 0) {
                    return;
                }
            },
            .eof => {
                self.tok_i -= 1;
                return;
            },
            else => {},
        }
    }
}
