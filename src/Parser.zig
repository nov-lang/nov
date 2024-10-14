// Originally based on https://github.com/ziglang/zig/blob/master/lib/std/zig/Parse.zig
// See https://github.com/ziglang/zig/blob/master/LICENSE for additional LICENSE details

// TODO
// let x = [ 1, 2, ] ; no error, terminator is ']\n'
// let x = [
//     1,
//     2,
// ] ; no error, we ignore newlines and the expr ends on ']\n'
// let x = MyStruct{ .a = 1, .b = 2, } ; no error, terminator is '}\n'
// let x = MyStruct{
//     .a = 1,
//     .b = 2,
// } ; no error, we ignore newlines and the expr ends on '}\n'
// ; note that it's fine to have a trailing comma in a struct literal

const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const TokenIndex = Ast.TokenIndex;
const Allocator = std.mem.Allocator;
const null_node: Node.Index = 0;
const Error = error{ParseError} || Allocator.Error;

const Parser = @This();

source: []const u8,
allocator: Allocator,
token_tags: []const Token.Tag,
token_starts: []const Tokenizer.ByteOffset,
tok_i: TokenIndex,
nodes: Ast.NodeList,
extra_data: std.ArrayListUnmanaged(Node.Index),
errors: std.ArrayListUnmanaged(Ast.Error),
scratch: std.ArrayListUnmanaged(Node.Index),

pub fn parse(allocator: Allocator, source: [:0]const u8) Allocator.Error!Ast {
    var tokens: Ast.TokenList = .{};
    defer tokens.deinit(allocator);

    // TODO: wrong ratio
    const estimated_token_count = source.len / 8;
    try tokens.ensureTotalCapacity(allocator, estimated_token_count);

    var tokenizer = Tokenizer.init(source);
    while (true) {
        const token = tokenizer.next();
        try tokens.append(allocator, .{
            .tag = token.tag,
            .start = token.start,
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

    // TODO: wrong ratio
    const estimated_node_count = (tokens.len + 2) / 2;
    try parser.nodes.ensureTotalCapacity(allocator, estimated_node_count);

    try parser.parseRoot();

    return .{
        .source = source,
        .tokens = tokens.toOwnedSlice(),
        .nodes = parser.nodes.toOwnedSlice(),
        .extra_data = try parser.extra_data.toOwnedSlice(allocator),
        .errors = try parser.errors.toOwnedSlice(allocator),
    };
}

/// Root <- (doc_comment? Attr* Decl)*
fn parseRoot(self: *Parser) Allocator.Error!void {
    // root node must be index 0
    self.nodes.appendAssumeCapacity(.{
        .tag = .root,
        .main_token = 0,
        .data = undefined,
    });

    while (true) {
        const doc_comment = try self.eatDocComments();

        switch (self.token_tags[self.tok_i]) {
            .eof => {
                if (doc_comment) |tok| {
                    try self.warnMsg(.{
                        .tag = .unattached_doc_comment,
                        .token = tok,
                    });
                }
                break;
            },
            .newline => self.tok_i += 1,
            .at_sign_l_bracket => {
                const attr_decl = self.expectAttrDecl() catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    error.ParseError => {
                        self.findNextTopLevelDecl();
                        continue;
                    },
                };
                try self.scratch.append(self.allocator, attr_decl);
            },
            .keyword_let => {
                const decl = self.parseDecl() catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    error.ParseError => {
                        self.findNextTopLevelDecl();
                        continue;
                    },
                };
                try self.scratch.append(self.allocator, decl);
            },
            else => {
                const is_c_container = self.parseCStyleContainer() catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    error.ParseError => false,
                };

                if (!is_c_container) {
                    try self.warn(.expected_decl);
                    self.findNextTopLevelDecl();
                }
            },
        }
    }

    const root_span = try self.listToSpan(self.scratch.items[0..]);
    self.nodes.items(.data)[0] = .{
        .lhs = root_span.start,
        .rhs = root_span.end,
    };
}

/// AttrDecl <- Attr+ Decl
fn expectAttrDecl(self: *Parser) Error!Node.Index {
    const scratch_top = self.scratch.items.len;
    defer self.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        switch (self.token_tags[self.tok_i]) {
            .newline => self.tok_i += 1,
            .at_sign_l_bracket => {
                const attr = try self.expectAttr();
                try self.scratch.append(self.allocator, attr);
            },
            .keyword_let => break,
            else => return self.fail(.expected_decl),
        }
    }
    const decl = try self.parseDecl();
    const attrs = self.scratch.items[scratch_top..];
    return switch (attrs.len) {
        0 => unreachable,
        1 => self.addNode(.{
            .tag = .attr_decl_one,
            .main_token = undefined,
            .data = .{
                .lhs = attrs[0],
                .rhs = decl,
            },
        }),
        else => self.addNode(.{
            .tag = .attr_decl,
            .main_token = undefined,
            .data = .{
                .lhs = try self.addExtra(try self.listToSpan(attrs)),
                .rhs = decl,
            },
        }),
    };
}

/// Attr <- AT_SIGN_LBRACKET IDENTIFIER ExprList RBRACKET NEWLINE
fn expectAttr(self: *Parser) Error!Node.Index {
    const main_token = try self.expectToken(.at_sign_l_bracket);
    const name = try self.expectToken(.identifier);

    var is_multi = false;
    const args = if (self.eatToken(.l_paren) == null)
        null_node
    else switch (try self.parseExprList(.r_paren, .expected_comma_after_arg)) {
        .zero_or_one => |arg| blk: {
            if (arg == null_node) {
                const tok = self.tok_i - 2;
                std.debug.assert(self.token_tags[tok] == .l_paren);
                try self.warnMsg(.{ .tag = .attr_without_args, .token = tok });
            }
            break :blk arg;
        },
        .multi => |span| blk: {
            is_multi = true;
            break :blk try self.addExtra(span);
        },
    };

    _ = try self.expectToken(.r_bracket);
    try self.expectNewLine(.expected_newline_after_attr);

    return self.addNode(.{
        .tag = if (is_multi) .attr else .attr_one,
        .main_token = main_token,
        .data = .{
            .lhs = name,
            .rhs = args,
        },
    });
}

/// Decl <- DeclProto (EQUAL Expr)? NEWLINE
/// DeclProto <- KEYWORD_let KEYWORD_mut? IDENTIFIER (COLON TypeExpr)?
fn parseDecl(self: *Parser) Error!Node.Index {
    const let_token = self.assertToken(.keyword_let);
    const is_mut = self.eatToken(.keyword_mut) != null;

    _ = try self.expectToken(.identifier);
    const type_expr = if (self.eatToken(.colon) != null) try self.expectTypeExpr() else null_node;
    const initializer = if (self.eatToken(.equal) != null) try self.expectExpr() else null_node;

    if (type_expr == null_node and initializer == null_node) {
        return self.failExpected(.equal);
    }

    try self.expectNewLine(.expected_newline_after_decl);

    return self.addNode(.{
        .tag = .decl,
        .main_token = let_token,
        .data = .{
            .lhs = @bitCast(Node.Decl{
                .mutable = is_mut,
                .type_node = @intCast(type_expr),
            }),
            .rhs = initializer,
        },
    });
}

fn parseExpr(self: *Parser) Error!Node.Index {
    return self.parseExprPrecedence(.assignment);
}

fn expectExpr(self: *Parser) Error!Node.Index {
    const node = try self.parseExpr();
    if (node == null_node) {
        return self.fail(.expected_expr);
    }
    return node;
}

const Precedence = enum(i8) {
    none = -1,
    assignment = 0, // = *= /= %= += -= <<=
    piped_call = 10, // |> >>=
    bool_or = 20, // or
    bool_and = 30, // and
    comparison = 40, // == != < > <= >=
    bitwise = 50, // & ^ | in
    shift = 60, // << >>
    term = 70, // + -
    factor = 80, // * / %

    unary, // !x -x ~x &x *T ?T
    curly_suffix, // x{}
    result_union, // E!T
    call, // x() x[] x.y x.? x.! x.*
    primary, // literals, identifiers, (expr)

    _,
};

const Assoc = enum {
    left,
    // right,
    none,
};

const Rule = struct {
    prec: Precedence,
    tag: Node.Tag,
    assoc: Assoc = .left,
};

const rules = std.enums.directEnumArrayDefault(Token.Tag, Rule, .{ .prec = .none, .tag = .root }, 0, .{
    .pipe_arrow = .{ .prec = .piped_call, .tag = .function_pipe },
    .r_angle_bracket_angle_bracket_equal = .{ .prec = .piped_call, .tag = .bind },

    .keyword_or = .{ .prec = .bool_or, .tag = .bool_or },

    .keyword_and = .{ .prec = .bool_and, .tag = .bool_and },

    .equal_equal = .{ .prec = .comparison, .tag = .equal_equal, .assoc = .none },
    .bang_equal = .{ .prec = .comparison, .tag = .bang_equal, .assoc = .none },
    .l_angle_bracket = .{ .prec = .comparison, .tag = .less_than, .assoc = .none },
    .r_angle_bracket = .{ .prec = .comparison, .tag = .greater_than, .assoc = .none },
    .l_angle_bracket_equal = .{ .prec = .comparison, .tag = .less_or_equal, .assoc = .none },
    .r_angle_bracket_equal = .{ .prec = .comparison, .tag = .greater_or_equal, .assoc = .none },

    .ampersand = .{ .prec = .bitwise, .tag = .bit_and },
    .caret = .{ .prec = .bitwise, .tag = .bit_xor },
    .pipe = .{ .prec = .bitwise, .tag = .bit_or },
    .keyword_in = .{ .prec = .bitwise, .tag = .in },

    .l_angle_bracket_angle_bracket = .{ .prec = .shift, .tag = .shl },
    .r_angle_bracket_angle_bracket = .{ .prec = .shift, .tag = .shr },

    .plus = .{ .prec = .term, .tag = .add },
    .minus = .{ .prec = .term, .tag = .sub },

    .asterisk = .{ .prec = .factor, .tag = .mul },
    .slash = .{ .prec = .factor, .tag = .div },
    .percent = .{ .prec = .factor, .tag = .mod },
});

// TODO: handle chained method call e.g.
// let x = list
//     .map()
//     .filter()
//     .reuduce()
fn parseExprPrecedence(self: *Parser, min_prec: Precedence) Error!Node.Index {
    std.debug.assert(@intFromEnum(min_prec) >= 0);
    // allow for `let x =\n1 +\n 2` but not `let x =\n\n1 + 2`
    _ = self.eatToken(.newline);
    var node = try self.parsePrefixExpr();
    if (node == null_node) {
        return null_node;
    }

    var banned_prec: Precedence = .none;

    while (true) {
        // allow for `let x = 1\n+ 2` but not `let x = 1\n\n+ 2`
        const ate_newline = self.eatToken(.newline) != null;
        const tok_tag = self.token_tags[self.tok_i];
        const rule = rules[@intFromEnum(tok_tag)];
        if (@intFromEnum(rule.prec) < @intFromEnum(min_prec)) {
            if (ate_newline) {
                self.tok_i -= 1;
            }
            return node;
        }
        if (@intFromEnum(rule.prec) == @intFromEnum(banned_prec)) {
            return self.fail(.chained_comparison_operators);
        }

        const op_token = self.nextToken();
        // since everything is an expression it's not possible to distinguish
        // let add_neg: `(a: int, b: int) -> int = {
        //     let sum = a + b
        //     -sum
        // }
        // from
        // let add_neg: `(a: int, b: int) -> int = {
        //     let sum = a + b - sum
        // }
        // so we ask the user to add or remove a newline
        if (ate_newline) switch (self.token_tags[op_token]) {
            .minus, // a - b or -a
            .ampersand, // a & b or &a
            .asterisk, // a * b or *T
            => try self.warnMsg(.{ .tag = .ambiguous_unary_operator, .token = op_token }),
            else => {},
        };
        const rhs = try self.parseExprPrecedence(@enumFromInt(@intFromEnum(rule.prec) + 1));
        if (rhs == null_node) {
            // return self.fail(.expected_expr);
            try self.warn(.expected_expr);
            return node;
        }

        {
            const tok_len = tok_tag.lexeme().?.len;
            const char_before = self.source[self.token_starts[op_token] - 1];
            const char_after = self.source[self.token_starts[op_token] + tok_len];
            if (tok_tag == .ampersand and char_after == '&') {
                // without types we don't know if '&&' was intended as 'bitwise_and ref_of', or a c-style logical_and
                // The best the parser can do is recommend changing it to 'and' or ' & &'
                try self.warnMsg(.{ .tag = .invalid_ampersand_ampersand, .token = op_token });
            } else if (std.ascii.isWhitespace(char_before) != std.ascii.isWhitespace(char_after)) {
                try self.warnMsg(.{ .tag = .mismatched_binary_op_whitespace, .token = op_token });
            }
        }

        node = try self.addNode(.{
            .tag = rule.tag,
            .main_token = op_token,
            .data = .{
                .lhs = node,
                .rhs = rhs,
            },
        });

        if (rule.assoc == .none) {
            banned_prec = rule.prec;
        }
    }
}

/// PrefixExpr <- PrefixOp* PrimaryExpr
/// PrefixOp <- EXCLAMATIONMARK / MINUS / TILDE / AMPERSAND
fn parsePrefixExpr(self: *Parser) Error!Node.Index {
    const tag: Node.Tag = switch (self.token_tags[self.tok_i]) {
        .bang => .bool_not,
        .minus => .negation,
        .tilde => .bit_not,
        .ampersand => .ref_of,
        // .keyword_await => .@"await",
        else => return self.parsePrimaryExpr(),
    };
    return self.addNode(.{
        .tag = tag,
        .main_token = self.nextToken(),
        .data = .{
            .lhs = undefined,
            .rhs = try self.expectPrefixExpr(),
        },
    });
}

fn expectPrefixExpr(self: *Parser) Error!Node.Index {
    const node = try self.parsePrefixExpr();
    if (node == null_node) {
        return self.fail(.expected_prefix_expr);
    }
    return node;
}

/// PrimaryExpr
///     <- IfExpr
///      / KEYWORD_break BreakLabel? Expr?
///      / KEYWORD_continue BreakLabel?
///      / KEYWORD_return Expr?
///      / BlockLabel? LoopExpr
///      / Block
///      / CurlySuffixExpr
// TODO:
// KEYWORD_nosuspend Expr
// KEYWORD_resume Expr
fn parsePrimaryExpr(self: *Parser) Error!Node.Index {
    switch (self.token_tags[self.tok_i]) {
        .keyword_if => return self.parseIfExpr(),
        .keyword_break => return self.addNode(.{
            .tag = .@"break",
            .main_token = self.nextToken(),
            .data = .{
                .lhs = try self.parseBreakLabel(),
                .rhs = try self.parseExpr(),
            },
        }),
        .keyword_continue => return self.addNode(.{
            .tag = .@"continue",
            .main_token = self.nextToken(),
            .data = .{
                .lhs = try self.parseBreakLabel(),
                .rhs = undefined,
            },
        }),
        .keyword_return => return self.addNode(.{
            .tag = .@"return",
            .main_token = self.nextToken(),
            .data = .{
                .lhs = undefined,
                .rhs = try self.parseExpr(),
            },
        }),
        .keyword_defer => return self.addNode(.{
            .tag = .@"defer",
            .main_token = self.nextToken(),
            .data = .{
                .lhs = undefined,
                .rhs = try self.expectExpr(),
            },
        }),
        .identifier => {
            if (self.token_tags[self.tok_i + 1] == .colon) {
                switch (self.token_tags[self.tok_i + 2]) {
                    .keyword_for => {
                        self.tok_i += 2;
                        return self.parseForExpr();
                    },
                    .l_brace => {
                        self.tok_i += 2;
                        return self.parseBlock();
                    },
                    else => return self.parseCurlySuffixExpr(),
                }
            } else {
                return self.parseCurlySuffixExpr();
            }
        },
        .keyword_for => return self.parseForExpr(),
        .l_brace => return self.parseBlock(),
        .l_bracket => return self.parseArrayLiteral(), // TODO: move to parsePrimaryTypeExpr, make it like zig?
        // TODO: here?
        // .keyword_match => return self.parseMatchExpr(),
        else => return self.parseCurlySuffixExpr(),
    }
}

/// BreakLabel <- COLON IDENTIFIER
fn parseBreakLabel(self: *Parser) Error!TokenIndex {
    _ = self.eatToken(.colon) orelse return null_node;
    return self.expectToken(.identifier);
}

/// IfExpr <- KEYWORD_if Expr Block (KEYWORD_else (IfExpr / Block))?
fn parseIfExpr(self: *Parser) Error!Node.Index {
    const if_token = self.assertToken(.keyword_if);
    const condition = try self.expectExpr();
    const then_expr = try expectBlock(self);

    const else_expr = try self.parseElseExpr();
    if (else_expr == null_node) {
        return self.addNode(.{
            .tag = .@"if",
            .main_token = if_token,
            .data = .{
                .lhs = condition,
                .rhs = then_expr,
            },
        });
    }
    return self.addNode(.{
        .tag = .if_else,
        .main_token = if_token,
        .data = .{
            .lhs = condition,
            .rhs = try self.addExtra(Node.If{
                .then_expr = then_expr,
                .else_expr = else_expr,
            }),
        },
    });
}

fn parseElseExpr(self: *Parser) Error!Node.Index {
    return if (self.eatToken(.keyword_else) == null)
        null_node
    else if (self.token_tags[self.tok_i] == .keyword_if)
        self.parseIfExpr()
    else
        self.expectBlock();
}

// TODO
// for x in y
// for x, y, z in a, b, c
// for x in y..z
// for mut x, mut y in a, b, c
// for _ in 0..10
// for x > 10
// for {}
// for i < 100 : i += 1
// for ... {} else
/// IfExpr <- KEYWORD_if Expr Block (KEYWORD_else (IfExpr / Block))?
/// ForExpr <- KEYWORD_for ... Block
/// ForEachInput <- IDENTIFIER+ KEYWORD_in (Expr (DOT2 Expr?)?)+
///
fn parseForExpr(self: *Parser) Error!Node.Index {
    const for_token = self.assertToken(.keyword_for);
    _ = for_token;
    unreachable;

    // const inputs = try self.parseForEachInput();
}

/// AssignExpr <- Expr (AssignOp Expr)?
/// AssignOp
///     <- ASTERISKEQUAL
///      / SLASHEQUAL
///      / PERCENTEQUAL
///      / PLUSEQUAL
///      / MINUSEQUAL
///      / LARROW2EQUAL
///      / EQUAL
fn parseAssignExpr(self: *Parser) Error!Node.Index {
    const lhs = try self.parseExpr();
    if (lhs == null_node) {
        return null_node;
    }
    // const tok = self.token_tags[self.tok_i];
    // if (tok == .comma) {
    //     // TODO: handle multi-assign
    // }
    const tag = assignOpNode(self.token_tags[self.tok_i]) orelse return lhs;
    return self.addNode(.{
        .tag = tag,
        .main_token = self.nextToken(),
        .data = .{
            .lhs = lhs,
            .rhs = try self.expectExpr(),
        },
    });
}

fn assignOpNode(tok: Token.Tag) ?Node.Tag {
    return switch (tok) {
        .asterisk_equal => .assign_mul,
        .slash_equal => .assign_div,
        .percent_equal => .assign_mod,
        .plus_equal => .assign_add,
        .minus_equal => .assign_sub,
        .l_angle_bracket_angle_bracket_equal => .push,
        .equal => .assign,
        else => null,
    };
}

/// Block <- LBRACE (Decl / ExprStmt)* Expr? RBRACE
/// ExprStmt <- Expr NEWLINE
fn parseBlock(self: *Parser) Error!Node.Index {
    const lbrace = self.eatToken(.l_brace) orelse return null_node;
    const scratch_top = self.scratch.items.len;
    defer self.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        switch (self.token_tags[self.tok_i]) {
            .newline => self.tok_i += 1,
            .r_brace => {
                self.tok_i += 1;
                break;
            },
            .eof => return self.failExpected(.r_brace),
            .keyword_let => {
                const decl = self.parseDecl() catch |err| switch (err) {
                    error.OutOfMemory => return err,
                    error.ParseError => {
                        self.findNextStmt();
                        continue;
                    },
                };
                try self.scratch.append(self.allocator, decl);
            },
            else => {
                const expr = try self.parseAssignExpr();
                if (expr == null_node) {
                    try self.warn(.expected_expr);
                    self.findNextStmt();
                    continue;
                }

                try self.scratch.append(self.allocator, expr);
                if (self.token_tags[self.tok_i] != .r_brace) {
                    try self.expectNewLine(.expected_newline_after_stmt);
                }
            },
        }
    }
    const statements = self.scratch.items[scratch_top..];
    switch (statements.len) {
        0 => return self.addNode(.{
            .tag = .block_two,
            .main_token = lbrace,
            .data = .{
                .lhs = 0,
                .rhs = 0,
            },
        }),
        1 => return self.addNode(.{
            .tag = .block_two,
            .main_token = lbrace,
            .data = .{
                .lhs = statements[0],
                .rhs = 0,
            },
        }),
        2 => return self.addNode(.{
            .tag = .block_two,
            .main_token = lbrace,
            .data = .{
                .lhs = statements[0],
                .rhs = statements[1],
            },
        }),
        else => {
            const span = try self.listToSpan(statements);
            return self.addNode(.{
                .tag = .block,
                .main_token = lbrace,
                .data = .{
                    .lhs = span.start,
                    .rhs = span.end,
                },
            });
        },
    }
}

fn expectBlock(self: *Parser) Error!Node.Index {
    const block = try self.parseBlock();
    if (block == null_node) {
        return self.fail(.expected_block);
    }
    return block;
}

// TODO: use parseList or make it more like parseBlock, this is too long
/// ArrayLiteral <- LBRACKET (Expr COMMA)* Expr? RBRACKET
fn parseArrayLiteral(self: *Parser) Error!Node.Index {
    const lbracket = self.assertToken(.l_bracket);
    if (self.eatToken(.r_bracket) != null) {
        return self.addNode(.{
            .tag = .array_init_two,
            .main_token = lbracket,
            .data = .{
                .lhs = null_node,
                .rhs = null_node,
            },
        });
    }

    self.eatNewLines();
    const scratch_top = self.scratch.items.len;
    defer self.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const expr = try self.expectExpr();
        try self.scratch.append(self.allocator, expr);
        self.eatNewLines();
        switch (self.token_tags[self.tok_i]) {
            .comma => self.tok_i += 1,
            .r_bracket => {
                self.tok_i += 1;
                break;
            },
            .newline => unreachable,
            .colon, .r_brace, .r_paren => return self.failExpected(.r_bracket),
            // Likely just a missing comma; give error but continue parsing.
            else => try self.warn(.expected_comma_after_arg),
        }
    }
    const exprs = self.scratch.items[scratch_top..];
    switch (exprs.len) {
        0 => return self.addNode(.{
            .tag = .array_init_two,
            .main_token = lbracket,
            .data = .{
                .lhs = null_node,
                .rhs = null_node,
            },
        }),
        1 => return self.addNode(.{
            .tag = .array_init_two,
            .main_token = lbracket,
            .data = .{
                .lhs = exprs[0],
                .rhs = null_node,
            },
        }),
        2 => return self.addNode(.{
            .tag = .array_init_two,
            .main_token = lbracket,
            .data = .{
                .lhs = exprs[0],
                .rhs = exprs[1],
            },
        }),
        else => {
            const span = try self.listToSpan(exprs);
            return self.addNode(.{
                .tag = .array_init,
                .main_token = lbracket,
                .data = .{
                    .lhs = span.start,
                    .rhs = span.end,
                },
            });
        },
    }
}

// TODO: define InitList, for container only (and map?)
/// CurlySuffixExpr <- TypeExpr InitList?
///
/// InitList
///     <- LBRACE FieldInit (COMMA FieldInit)* COMMA? RBRACE
///      / LBRACE Expr (COMMA Expr)* COMMA? RBRACE
///      / LBRACE RBRACE
fn parseCurlySuffixExpr(self: *Parser) Error!Node.Index {
    const lhs = try self.parseTypeExpr();
    return lhs;
    // TODO
}

/// TypeExpr <- PrefixTypeOp* ResultUnionExpr
/// PrefixTypeOp <- QUESTIONMARK / AMPERSAND / ArrayTypeStart
/// ArrayTypeStart <- LBRACKET RBRACKET
fn parseTypeExpr(self: *Parser) Error!Node.Index {
    switch (self.token_tags[self.tok_i]) {
        .question_mark => return self.addNode(.{
            .tag = .optional_type,
            .main_token = self.nextToken(),
            .data = .{
                .lhs = undefined,
                .rhs = try self.expectTypeExpr(),
            },
        }),
        .asterisk => return self.addNode(.{
            .tag = .ref_type,
            .main_token = self.nextToken(),
            .data = .{
                .lhs = undefined,
                .rhs = try self.expectTypeExpr(),
            },
        }),
        .l_bracket => {
            const lbracket = self.nextToken();
            _ = try self.expectToken(.r_bracket);
            return self.addNode(.{
                .tag = .array_type,
                .main_token = lbracket,
                .data = .{
                    .lhs = undefined,
                    .rhs = try self.expectTypeExpr(),
                },
            });
        },
        else => return self.parseResultUnionExpr(),
    }
}

fn expectTypeExpr(self: *Parser) Error!Node.Index {
    const node = try self.parseTypeExpr();
    if (node == null_node) {
        return self.fail(.expected_type_expr);
    }
    return node;
}

/// ResultUnionExpr <- SuffixExpr (EXCLAMATIONMARK TypeExpr)?
fn parseResultUnionExpr(self: *Parser) Error!Node.Index {
    const suffix_expr = try self.parseSuffixExpr();
    if (suffix_expr == null_node) return null_node;
    const bang = self.eatToken(.bang) orelse return suffix_expr;
    return self.addNode(.{
        .tag = .result_union,
        .main_token = bang,
        .data = .{
            .lhs = suffix_expr,
            .rhs = try self.expectTypeExpr(),
        },
    });
}

/// SuffixExpr <- PrimaryTypeExpr (SuffixOp / FnCallArguments)*
/// FnCallArguments <- LPAREN ExprList RPAREN
fn parseSuffixExpr(self: *Parser) Error!Node.Index {
    var res = try self.parsePrimaryTypeExpr();
    if (res == null_node) {
        return null_node;
    }

    while (true) {
        const suffix_op = try self.parseSuffixOp(res);
        if (suffix_op != null_node) {
            res = suffix_op;
            continue;
        }
        const lparen = self.eatToken(.l_paren) orelse return res;
        const params = try self.parseExprList(.r_paren, .expected_comma_after_arg);
        res = try switch (params) {
            .zero_or_one => |param| self.addNode(.{
                .tag = .call_one,
                .main_token = lparen,
                .data = .{
                    .lhs = res,
                    .rhs = param,
                },
            }),
            .multi => |span| self.addNode(.{
                .tag = .call,
                .main_token = lparen,
                .data = .{
                    .lhs = res,
                    .rhs = try self.addExtra(span),
                },
            }),
        };
    }
}

// TODO: add doc comment
// TODO: complete all cases
fn parsePrimaryTypeExpr(self: *Parser) Error!Node.Index {
    switch (self.token_tags[self.tok_i]) {
        .keyword_unreachable => return self.addNode(.{
            .tag = .unreachable_literal,
            .main_token = self.nextToken(),
            .data = .{
                .lhs = undefined,
                .rhs = undefined,
            },
        }),
        .number_literal => return self.addNode(.{
            .tag = .number_literal,
            .main_token = self.nextToken(),
            .data = .{
                .lhs = undefined,
                .rhs = undefined,
            },
        }),
        .char_literal => return self.addNode(.{
            .tag = .char_literal,
            .main_token = self.nextToken(),
            .data = .{
                .lhs = undefined,
                .rhs = undefined,
            },
        }),
        .string_literal => return self.addNode(.{
            .tag = .string_literal,
            .main_token = self.nextToken(),
            .data = .{
                .lhs = undefined,
                .rhs = undefined,
            },
        }),
        .builtin => return self.addNode(.{
            .tag = .builtin_literal,
            .main_token = self.nextToken(),
            .data = .{
                .lhs = undefined,
                .rhs = undefined,
            },
        }),
        .grave_accent => return self.parseFnProto(),
        // .keyword_if => return self.parseIf(expectTypeExpr), // do not add yet
        // .keyword_for => return self.parseFor(expectTypeExpr) // do not add yet
        // .keyword_match => return self.expectMatchExpr(false), // TODO
        // TODO
        // .keyword_struct,
        // .keyword_opaque,
        // .keyword_enum,
        // .keyword_union,
        // => return self.parseContainerDeclAuto(),
        .identifier => switch (self.token_tags[self.tok_i + 1]) {
            .colon => switch (self.token_tags[self.tok_i + 2]) {
                // TODO
                // .keyword_for => {
                //     self.tok_i += 2;
                //     return self.parseFor(expectTypeExpr);
                // },
                // .keyword_match => {
                //     self.tok_i += 2;
                //     return self.expectMatchExpr(true);
                // },
                .l_brace => {
                    self.tok_i += 2;
                    return self.parseBlock();
                },
                else => return self.addNode(.{
                    .tag = .identifier,
                    .main_token = self.nextToken(),
                    .data = .{
                        .lhs = undefined,
                        .rhs = undefined,
                    },
                }),
            },
            else => return self.addNode(.{
                .tag = .identifier,
                .main_token = self.nextToken(),
                .data = .{
                    .lhs = undefined,
                    .rhs = undefined,
                },
            }),
        },
        // TODO: see README proposal
        .period => switch (self.token_tags[self.tok_i + 1]) {
            .identifier => return self.addNode(.{
                .tag = .enum_literal,
                .data = .{
                    .lhs = self.nextToken(), // period
                    .rhs = undefined,
                },
                .main_token = self.nextToken(), // identifier
            }),
            else => return null_node,
        },
        .l_paren => return self.addNode(.{
            .tag = .grouped_expression,
            .main_token = self.nextToken(),
            .data = .{
                .lhs = try self.expectExpr(),
                .rhs = blk: {
                    self.eatNewLines();
                    break :blk try self.expectToken(.r_paren);
                },
            },
        }),
        else => return null_node,
    }
}

/// SuffixOp
///     <- LBRACKET Expr (DOT2 Expr?)? RBRACKET
///      / DOT IDENTIFIER
///      / DOTQUESTIONMARK
///      / DOTEXCLAMATIONMARK
///      / DOTASTERISK
fn parseSuffixOp(self: *Parser, lhs: Node.Index) Error!Node.Index {
    switch (self.token_tags[self.tok_i]) {
        .l_bracket => {
            const lbracket = self.nextToken();
            const index_expr = try self.expectExpr();

            if (self.eatToken(.ellipsis2) != null) {
                const end_expr = try self.parseExpr();
                _ = try self.expectToken(.r_bracket);
                if (end_expr == null_node) {
                    return self.addNode(.{
                        .tag = .slice_open,
                        .main_token = lbracket,
                        .data = .{
                            .lhs = lhs,
                            .rhs = index_expr,
                        },
                    });
                }
                return self.addNode(.{
                    .tag = .slice,
                    .main_token = lbracket,
                    .data = .{
                        .lhs = lhs,
                        .rhs = try self.addExtra(Node.Slice{
                            .start = index_expr,
                            .end = end_expr,
                        }),
                    },
                });
            }
            _ = try self.expectToken(.r_bracket);
            return self.addNode(.{
                .tag = .array_access,
                .main_token = lbracket,
                .data = .{
                    .lhs = lhs,
                    .rhs = index_expr,
                },
            });
        },
        .period => switch (self.token_tags[self.tok_i + 1]) {
            .identifier => return self.addNode(.{
                .tag = .field_access,
                .main_token = self.nextToken(),
                .data = .{
                    .lhs = lhs,
                    .rhs = self.nextToken(),
                },
            }),
            .question_mark => return self.addNode(.{
                .tag = .unwrap_option,
                .main_token = self.nextToken(),
                .data = .{
                    .lhs = lhs,
                    .rhs = self.nextToken(),
                },
            }),
            .bang => return self.addNode(.{
                .tag = .unwrap_result,
                .main_token = self.nextToken(),
                .data = .{
                    .lhs = lhs,
                    .rhs = self.nextToken(),
                },
            }),
            .asterisk => return self.addNode(.{
                .tag = .deref,
                .main_token = self.nextToken(),
                .data = .{
                    .lhs = lhs,
                    .rhs = self.nextToken(),
                },
            }),
            // .l_brace => {
            //     // this a misplaced `.{`, handle the error somewhere else
            //     return null_node;
            // },
            else => {
                self.tok_i += 1;
                try self.warn(.expected_suffix_op);
                return null_node;
            },
        },
        else => return null_node,
    }
}

// TODO: handle generic list and async ret type
// TODO: for ResultUnion: FnRetType <- ARROW EXCLAMATIONMARK? TypeExpr
/// FnProto <- GRAVEACCENT GenericList? ParamDeclList AsyncRetType? FnRetType?
/// AsyncRetType <- MINUSCARET TypeExpr
/// FnRetType <- ARROW TypeExpr
fn parseFnProto(self: *Parser) Error!Node.Index {
    const fn_token = self.assertToken(.grave_accent);

    // We want the fn proto node to be before its children in the array. TODO: why?
    const fn_proto_index = try self.reserveNode(.fn_proto);
    errdefer self.unreserveNode(fn_proto_index);

    // const generics = try self.parseGenericList();
    const params = try self.parseParamDeclList();

    // const async_ret_type = if (self.eatToken(.minus_caret) != null) try self.expectTypeExpr() else null_node;
    const ret_type = if (self.eatToken(.arrow) != null) try self.expectTypeExpr() else null_node;

    return switch (params) {
        .zero_or_one => |param| self.setNode(fn_proto_index, .{
            .tag = .fn_proto_simple,
            .main_token = fn_token,
            .data = .{
                .lhs = param,
                .rhs = ret_type,
            },
        }),
        .multi => |span| self.setNode(fn_proto_index, .{
            .tag = .fn_proto_multi,
            .main_token = fn_token,
            .data = .{
                .lhs = try self.addExtra(span),
                .rhs = ret_type,
            },
        }),
    };
}

/// Give a helpful error message for those transitioning from
/// C's 'struct Foo {};' to Nov's 'const Foo = struct {};'.
fn parseCStyleContainer(self: *Parser) Error!bool {
    const main_token = self.tok_i;
    switch (self.token_tags[self.tok_i]) {
        .keyword_enum, .keyword_union, .keyword_struct => {},
        else => return false,
    }
    const identifier = self.tok_i + 1;
    if (self.token_tags[identifier] != .identifier) return false;
    self.tok_i += 2;

    try self.warnMsg(.{
        .tag = .nov_style_container,
        .is_note = true,
        .token = identifier,
        .extra = .{ .expected_tag = self.token_tags[main_token] },
    });

    _ = try self.expectToken(.l_brace);
    // _ = try self.parseContainerMembers(); // TODO
    _ = try self.expectToken(.r_brace);
    try self.expectNewLine(.expected_newline_after_decl);
    return true;
}

/// GenericList <- LANGLEBRACKET (IDENTIFIER COMMA)* IDENTIFIER COMMA? RANGLEBRACKET
fn parseGenericList(self: *Parser) Error!SmallSpan {
    _ = self;
    unreachable;
}

/// ParamDeclList <- LPAREN (ParamDecl COMMA)* ParamDecl? RPAREN
fn parseParamDeclList(self: *Parser) Error!SmallSpan {
    _ = try self.expectToken(.l_paren);
    return self.parseList(
        .r_paren,
        .expected_comma_after_param,
        expectParamDecl,
    );
}

/// ParamDecl <- (IDENTIFIER COLON)? TypeExpr
fn expectParamDecl(self: *Parser) Error!Node.Index {
    if (self.token_tags[self.tok_i] == .identifier and
        self.token_tags[self.tok_i + 1] == .colon)
    {
        self.tok_i += 2;
    }
    return self.expectTypeExpr();
}

/// ExprList <- (Expr COMMA)* Expr?
fn parseExprList(
    self: *Parser,
    comptime ending_token_tag: Token.Tag,
    comptime error_tag: Ast.Error.Tag,
) Error!SmallSpan {
    return self.parseList(ending_token_tag, error_tag, expectExpr);
}

const SmallSpan = union(enum) {
    zero_or_one: Node.Index,
    multi: Node.SubRange,
};

// this should ignore new lines and handle commas correctly
// it's fine to ignore new lines since everything is comma separated
fn parseList(
    self: *Parser,
    comptime ending_token_tag: Token.Tag,
    comptime error_tag: Ast.Error.Tag,
    comptime parseFn: fn (*Parser) Error!Node.Index,
) Error!SmallSpan {
    if (self.eatToken(ending_token_tag) != null) {
        return .{ .zero_or_one = null_node };
    }

    const scratch_top = self.scratch.items.len;
    defer self.scratch.shrinkRetainingCapacity(scratch_top);
    var comma = true;
    sw: switch (self.token_tags[self.tok_i]) {
        ending_token_tag => self.tok_i += 1, // break
        .newline => {
            self.tok_i += 1;
            continue :sw self.token_tags[self.tok_i];
        },
        .comma => {
            if (comma) {
                _ = try parseFn(self); // should return the correct warning
                unreachable;
            }
            comma = true;
            self.tok_i += 1;
            continue :sw self.token_tags[self.tok_i];
        },
        else => {
            if (!comma) {
                try self.warn(error_tag);
            }
            const item = try parseFn(self);
            try self.scratch.append(self.allocator, item);
            comma = false;
            continue :sw self.token_tags[self.tok_i];
        },
    }
    const items = self.scratch.items[scratch_top..];
    return switch (items.len) {
        0 => .{ .zero_or_one = null_node },
        1 => .{ .zero_or_one = items[0] },
        else => .{ .multi = try self.listToSpan(items) },
    };
}

fn listToSpan(self: *Parser, list: []const Node.Index) Allocator.Error!Node.SubRange {
    try self.extra_data.appendSlice(self.allocator, list);
    return .{
        .start = @intCast(self.extra_data.items.len - list.len),
        .end = @intCast(self.extra_data.items.len),
    };
}

fn addNode(self: *Parser, node: Ast.Node) Allocator.Error!Node.Index {
    const result: Node.Index = @intCast(self.nodes.len);
    try self.nodes.append(self.allocator, node);
    return result;
}

fn setNode(self: *Parser, index: usize, node: Ast.Node) Node.Index {
    self.nodes.set(index, node);
    return @intCast(index);
}

fn reserveNode(self: *Parser, tag: Ast.Node.Tag) Allocator.Error!usize {
    try self.nodes.resize(self.allocator, self.nodes.len + 1);
    self.nodes.items(.tag)[self.nodes.len - 1] = tag;
    return self.nodes.len - 1;
}

fn unreserveNode(self: *Parser, node_index: usize) void {
    if (self.nodes.len == node_index) {
        self.nodes.resize(self.allocator, self.nodes.len - 1) catch unreachable;
    } else {
        // There is zombie node left in the tree, let's make it as inoffensive as possible
        // (sadly there's no no-op node)
        self.nodes.items(.tag)[node_index] = .unreachable_literal;
        self.nodes.items(.main_token)[node_index] = self.tok_i;
    }
}

fn addExtra(self: *Parser, extra: anytype) Allocator.Error!Node.Index {
    const fields = std.meta.fields(@TypeOf(extra));
    try self.extra_data.ensureUnusedCapacity(self.allocator, fields.len);
    const result: Node.Index = @intCast(self.extra_data.items.len);
    inline for (fields) |field| {
        comptime std.debug.assert(field.type == Node.Index);
        self.extra_data.appendAssumeCapacity(@field(extra, field.name));
    }
    return result;
}

/// Returns current token and advances the token index
fn nextToken(self: *Parser) TokenIndex {
    const result = self.tok_i;
    self.tok_i += 1;
    return result;
}

fn expectToken(self: *Parser, tag: Token.Tag) Error!TokenIndex {
    if (self.token_tags[self.tok_i] != tag) {
        return self.failExpected(tag);
    }
    return self.nextToken();
}

fn expectNewLine(self: *Parser, error_tag: Ast.Error.Tag) Error!void {
    switch (self.token_tags[self.tok_i]) {
        .newline => self.tok_i += 1,
        .doc_comment => {
            try self.warnMsg(.{
                .tag = .same_line_doc_comment,
                .token = self.tok_i,
            });
            self.tok_i += 1;
            _ = self.eatToken(.newline);
        },
        else => try self.warn(error_tag),
    }
}

// TODO: result is weird because newline is a token and used like a semicolon
fn tokensOnSameLine(self: *Parser, tok1: TokenIndex, tok2: TokenIndex) bool {
    return std.mem.indexOfScalar(u8, self.source[self.token_starts[tok1]..self.token_starts[tok2]], '\n') == null;
}

fn eatNewLines(self: *Parser) void {
    while (self.token_tags[self.tok_i] == .newline) {
        self.tok_i += 1;
    }
}

/// Skips over doc comment tokens. Returns the first one, if any.
fn eatDocComments(self: *Parser) Allocator.Error!?TokenIndex {
    if (self.eatToken(.doc_comment)) |first_doc_comment| {
        while (true) {
            switch (self.token_tags[self.tok_i]) {
                .doc_comment, .newline => self.tok_i += 1,
                else => break,
            }
        }
        return first_doc_comment;
    }
    return null;
}

fn eatToken(self: *Parser, tag: Token.Tag) ?TokenIndex {
    return if (self.token_tags[self.tok_i] == tag) self.nextToken() else null;
}

fn assertToken(self: *Parser, tag: Token.Tag) TokenIndex {
    const token = self.nextToken();
    std.debug.assert(self.token_tags[token] == tag);
    return token;
}

fn findNextTopLevelDecl(self: *Parser) void {
    var level: usize = 0;
    while (true) : (self.tok_i += 1) {
        switch (self.token_tags[self.tok_i]) {
            .l_paren, .l_bracket, .l_brace => level += 1,
            .r_paren, .r_bracket, .r_brace => level -|= 1,
            .at_sign_l_bracket, .keyword_let => {
                if (level == 0) {
                    return;
                }
            },
            .newline => {
                if (level == 0) {
                    self.tok_i += 1;
                    return;
                }
            },
            .eof => return,
            else => {},
        }
    }
}

fn findNextStmt(self: *Parser) void {
    var level: usize = 0;
    while (true) : (self.tok_i += 1) {
        switch (self.token_tags[self.tok_i]) {
            .l_brace => level += 1,
            .r_brace => {
                if (level == 0) {
                    return;
                }
                level -= 1;
            },
            .newline => {
                if (level == 0) {
                    self.tok_i += 1;
                    return;
                }
            },
            .eof => return,
            else => {},
        }
    }
}

fn warnExpected(self: *Parser, expected_token: Token.Tag) Allocator.Error!void {
    @branchHint(.cold);
    try self.warnMsg(.{
        .tag = .expected_token,
        .token = self.tok_i,
        .extra = .{ .expected_tag = expected_token },
    });
}

fn warn(self: *Parser, error_tag: Ast.Error.Tag) Allocator.Error!void {
    @branchHint(.cold);
    try self.warnMsg(.{ .tag = error_tag, .token = self.tok_i });
}

fn warnMsg(self: *Parser, msg: Ast.Error) Allocator.Error!void {
    @branchHint(.cold);
    switch (msg.tag) {
        .expected_newline_after_decl,
        .expected_newline_after_stmt, // TODO: weird
        .expected_newline_after_attr,
        .expected_newline_or_else,
        .expected_newline_or_lbrace,
        // .expected_comma_after_field,
        .expected_comma_after_arg,
        .expected_comma_after_param,
        // .expected_comma_after_initializer,
        // .expected_comma_after_match_prong,
        // .expected_comma_after_for_operand,
        // .expected_comma_after_capture,
        // .expected_token,
        .expected_block,
        .expected_block_or_assignment,
        // .expected_block_or_expr,
        // .expected_block_or_field,
        // .expected_expr, // TODO: not prev?
        .expected_expr_or_assignment,
        // .expected_labelable,
        .expected_param_list,
        .expected_prefix_expr,
        // .expected_primary_type_expr,
        .expected_suffix_op,
        .expected_type_expr,
        // .expected_loop_payload,
        // .expected_container,
        => if (msg.token != 0) {
            // => if (msg.token != 0 and !self.tokensOnSameLine(msg.token - 1, msg.token)) {
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
    @branchHint(.cold);
    return self.failMsg(.{
        .tag = .expected_token,
        .token = self.tok_i,
        .extra = .{ .expected_tag = expected_token },
    });
}

fn fail(self: *Parser, tag: Ast.Error.Tag) Error {
    @branchHint(.cold);
    return self.failMsg(.{ .tag = tag, .token = self.tok_i });
}

fn failMsg(self: *Parser, msg: Ast.Error) Error {
    @branchHint(.cold);
    try self.warnMsg(msg);
    return error.ParseError;
}
