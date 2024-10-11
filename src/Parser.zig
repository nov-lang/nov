//! Originally based on https://github.com/ziglang/zig/blob/master/lib/std/zig/Parse.zig
//! See https://github.com/ziglang/zig/blob/master/LICENSE for additional LICENSE details

// TODO: (add tests)
// let a = 1 + 2 ; no error, terminator is '\n'
// let a = 1 + 2 + ; error: expected expression, found '\n'
//     4 + 5
// let a = 1 + 2
//    + 4 + 5 ; error: expected expression, found '+'
// let a = (
//     1 + 2 + ; no error, we ignore newlines and the expr ends on ')\n'
//     3 + 4
// )
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
// TODO: just do what go does?

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
ignore_newlines: bool,

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
        .ignore_newlines = false,
    };
    defer parser.nodes.deinit(allocator);
    defer parser.extra_data.deinit(allocator);
    defer parser.errors.deinit(allocator);
    defer parser.scratch.deinit(allocator);

    // TODO: wrong ratio
    const estimated_node_count = (tokens.len + 2) / 2;
    try parser.nodes.ensureTotalCapacity(allocator, estimated_node_count);

    // root node must be index 0
    parser.nodes.appendAssumeCapacity(.{
        .tag = .root,
        .main_token = 0,
        .data = undefined,
    });
    try parser.parseTopLevel();

    return .{
        .source = source,
        .tokens = tokens.toOwnedSlice(),
        .nodes = parser.nodes.toOwnedSlice(),
        .extra_data = try parser.extra_data.toOwnedSlice(allocator),
        .errors = try parser.errors.toOwnedSlice(allocator),
    };
}

/// TopLevel <- Stmt*
fn parseTopLevel(self: *Parser) Allocator.Error!void {
    while (true) {
        switch (self.token_tags[self.tok_i]) {
            .eof => break,
            .newline => self.tok_i += 1,
            else => {
                const stmt = self.expectStmt() catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    error.ParseError => {
                        self.synchronize();
                        continue;
                    },
                };
                try self.scratch.append(self.allocator, stmt);
            },
        }
    }

    const root_span = try self.listToSpan(self.scratch.items[0..]);
    self.nodes.items(.data)[0] = .{
        .lhs = root_span.start,
        .rhs = root_span.end,
    };
}

/// Stmt <- Decl / ExprStmt
fn expectStmt(self: *Parser) Error!Node.Index {
    std.debug.assert(self.token_tags[self.tok_i] != .newline);
    switch (self.token_tags[self.tok_i]) {
        .keyword_let => return self.expectDecl(),
        else => return self.expectExprStmt(),
    }
}

/// Decl <- DeclProto (EQUAL Expr)? NEWLINE
/// DeclProto <- KEYWORD_let KEYWORD_mut? IDENTIFIER (COLON TypeExpr)?
fn expectDecl(self: *Parser) Error!Node.Index {
    const let_token = self.assertToken(.keyword_let);
    const is_mut = self.eatToken(.keyword_mut) != null;

    _ = try self.expectToken(.identifier);
    const type_expr = if (self.eatToken(.colon) != null) try self.expectTypeExpr() else null_node;
    const initializer = if (self.eatToken(.equal) != null) try self.expectExpr() else null_node;

    if (type_expr == null_node and initializer == null_node) {
        return self.failExpected(.equal);
    }

    try self.expectNewline(.expected_newline_after_decl, true);

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

// TODO: return should always be `()` or add a way to ignore the return value
// and consider that everything is an expression that returns `()` by default
/// ExprStmt <- Expr NEWLINE
fn expectExprStmt(self: *Parser) Error!Node.Index {
    const expr = try self.parseExpr();
    if (expr == null_node) {
        return self.fail(.expected_expr);
    }
    try self.expectNewline(.expected_newline_after_stmt, true);
    return expr;
}

const Precedence = enum(i8) {
    none = -1,
    assignment = 0,
    // range
    piped_call = 10,
    bool_or = 20,
    bool_and = 30,
    comparison = 40,
    bitwise = 50,
    shift = 60,
    term = 70,
    factor = 80,
    // as / cast
    unary = 90,
    null_coalescing = 100,
    call = 110,
    primary = 120,
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

    .l_angle_bracket_angle_bracket = .{ .prec = .shift, .tag = .shl },
    .r_angle_bracket_angle_bracket = .{ .prec = .shift, .tag = .shr },

    .plus = .{ .prec = .term, .tag = .add },
    .minus = .{ .prec = .term, .tag = .sub },

    .asterisk = .{ .prec = .factor, .tag = .mul },
    .slash = .{ .prec = .factor, .tag = .div },
    .percent = .{ .prec = .factor, .tag = .mod },

    .question_mark_question_mark = .{ .prec = .null_coalescing, .tag = .optional_fallback },
});

fn parseExprPrecedence(self: *Parser, min_prec: Precedence) Error!Node.Index {
    std.debug.assert(@intFromEnum(min_prec) >= 0);
    var node = try self.parsePrefixExpr();
    if (node == null_node) {
        return null_node;
    }

    var banned_prec: Precedence = .none;

    while (true) {
        const tok_tag = self.currentTokenTag();
        const rule = rules[@intFromEnum(tok_tag)];
        if (@intFromEnum(rule.prec) < @intFromEnum(min_prec)) {
            return node;
        }
        if (@intFromEnum(rule.prec) == @intFromEnum(banned_prec)) {
            return self.fail(.chained_comparison_operators);
        }

        const op_token = self.nextToken();
        const rhs = try self.parseExprPrecedence(@enumFromInt(@intFromEnum(rule.prec) + 1));
        if (rhs == null_node) {
            try self.warn(.expected_expr);
            return node;
        }

        {
            const tok_len = tok_tag.lexeme().?.len;
            const char_before = self.source[self.token_starts[op_token] - 1];
            const char_after = self.source[self.token_starts[op_token] + tok_len];
            if (tok_tag == .ampersand and char_after == '&') {
                // warn about '&&' being used instead of 'and'
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
/// PrefixOp <- EXCLAMATIONMARK / MINUS / TILDE
fn parsePrefixExpr(self: *Parser) Error!Node.Index {
    // an unary operator should never be on its own line so we don't use currentTokenTag()
    const tag: Node.Tag = switch (self.token_tags[self.tok_i]) {
        .bang => .bool_not,
        .minus => .negation,
        .tilde => .bit_not,
        // .keyword_try => .@"try",
        // .keyword_await => .@"await",
        else => return self.parsePrimaryExpr(),
    };
    return self.addNode(.{
        .tag = tag,
        .main_token = self.nextToken(),
        .data = .{
            .lhs = try self.expectPrefixExpr(),
            .rhs = undefined,
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

// TODO
/// PrimaryExpr
///     <- IfExpr
///      / KEYWORD_break BreakLabel? Expr?
///      / KEYWORD_continue BreakLabel?
///      / KEYWORD_return Expr?
///      / BlockLabel? LoopExpr
///      / Block
///      / CurlySuffixExpr
fn parsePrimaryExpr(self: *Parser) Error!Node.Index {
    switch (self.currentTokenTag()) {
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
                .lhs = try self.parseExpr(),
                .rhs = undefined,
            },
        }),
        // keyword_nosuspend
        // keyword_resume
        // TODO: loop + block label here or everytime before a block?
        .identifier => {
            if (self.token_tags[self.tok_i + 1] == .colon) {
                switch (self.token_tags[self.tok_i + 2]) {
                    // .keyword_for => {
                    //     self.tok_i += 2;
                    //     return self.parseForExpr();
                    // },
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
        // .keyword_for => return self.parseForExpr(),
        // .keyword_match => return self.parseMatchExpr(),
        .l_brace => return self.parseBlock(),
        // .keyword_enum, .keyword_struct, .keyword_union => {
        //     // TODO: warn
        // },

        else => return self.parseCurlySuffixExpr(),
    }
}

/// IfExpr <- KEYWORD_if Expr Block (KEYWORD_else (IfExpr / Block))?
fn parseIfExpr(self: *Parser) Error!Node.Index {
    const if_token = self.assertToken(.keyword_if);
    const condition = try self.expectExpr();
    const then_expr = try expectBlock(self);

    if (self.eatToken(.keyword_else) == null) {
        return self.addNode(.{
            .tag = .@"if",
            .main_token = if_token,
            .data = .{
                .lhs = condition,
                .rhs = then_expr,
            },
        });
    }

    // an else if/block shouldn't be on a new line so we don't use currentTokenTag()
    const else_expr = if (self.token_tags[self.tok_i] == .keyword_if)
        try self.parseIfExpr()
    else
        try self.expectBlock();

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

/// BreakLabel <- COLON IDENTIFIER
fn parseBreakLabel(self: *Parser) Error!TokenIndex {
    _ = self.eatToken(.colon) orelse return null_node;
    return self.expectToken(.identifier);
}

/// Block <- LBRACE (Decl | ExprStmt)* RBRACE
fn parseBlock(self: *Parser) Error!Node.Index {
    const lbrace = self.eatToken(.l_brace) orelse return null_node;
    const scratch_top = self.scratch.items.len;
    defer self.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        switch (self.token_tags[self.tok_i]) {
            .newline => self.tok_i += 1,
            .r_brace => break,
            .eof => return self.failExpected(.r_brace),
            else => {
                // TODO!: do not warn if the last statement doesn't end with a newline
                const stmt = self.expectStmt() catch |err| switch (err) {
                    error.OutOfMemory => return err,
                    error.ParseError => {
                        self.synchronize();
                        continue;
                    },
                };
                try self.scratch.append(self.allocator, stmt);
            },
        }
    }
    _ = try self.expectToken(.r_brace);
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

// TODO: define InitList
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

/// TypeExpr <- PrefixTypeOp* SuffixExpr
/// PrefixTypeOp <- QUESTIONMARK / ArrayTypeStart
/// ArrayTypeStart <- LBRACKET RBRACKET
fn parseTypeExpr(self: *Parser) Error!Node.Index {
    // an unary operator should never be on its own line so we don't use currentTokenTag()
    switch (self.token_tags[self.tok_i]) {
        .question_mark => return self.addNode(.{
            .tag = .optional_type,
            .main_token = self.nextToken(),
            .data = .{
                .lhs = try self.expectTypeExpr(),
                .rhs = undefined,
            },
        }),
        // TODO
        // .l_bracket => {
        //     const lbracket = self.nextToken();
        //     _ = try self.expectToken(.r_bracket);
        //     return self.addNode(.{
        //         .tag = .slice_type,
        //         .main_token = lbracket,
        //         .data = .{
        //             .lhs = try self.expectTypeExpr(),
        //             .rhs = undefined,
        //         },
        //     });
        // },
        else => return self.parseSuffixExpr(),
    }
}

fn expectTypeExpr(self: *Parser) Error!Node.Index {
    const node = try self.parseTypeExpr();
    if (node == null_node) {
        return self.fail(.expected_type_expr);
    }
    return node;
}

// TODO
/// SuffixExpr <- PrimaryTypeExpr (SuffixOp / FnCallArguments)*
/// FnCallArguments <- LPAREN ExprList RPAREN
/// ExprList <- (Expr COMMA)* Expr?
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
        const scratch_top = self.scratch.items.len;
        defer self.scratch.shrinkRetainingCapacity(scratch_top);
        while (true) {
            if (self.eatToken(.r_paren) != null) break;
            const param = try self.expectExpr();
            try self.scratch.append(self.allocator, param);
            // TODO: self.currentTokenTag()?
            switch (self.token_tags[self.tok_i]) {
                .newline => {}, // TODO?
                .comma => self.tok_i += 1,
                .r_paren => {
                    self.tok_i += 1;
                    break;
                },
                .colon, .r_brace, .r_bracket => return self.failExpected(.r_paren),
                // Likely just a missing comma; give error but continue parsing.
                else => try self.warn(.expected_comma_after_arg),
            }
        }
        const params = self.scratch.items[scratch_top..];
        res = switch (params.len) {
            0 => try self.addNode(.{
                .tag = .call_one,
                .main_token = lparen,
                .data = .{
                    .lhs = res,
                    .rhs = null_node,
                },
            }),
            1 => try self.addNode(.{
                .tag = .call_one,
                .main_token = lparen,
                .data = .{
                    .lhs = res,
                    .rhs = params[0],
                },
            }),
            else => try self.addNode(.{
                .tag = .call,
                .main_token = lparen,
                .data = .{
                    .lhs = res,
                    .rhs = try self.addExtra(try self.listToSpan(params)),
                },
            }),
        };
    }
}

// TODO: add doc comment
// TODO: complete all cases
/// TupleTypeExpr <- LPAREN TypeExpr (COMMA TypeExpr)* RPAREN // TODO
fn parsePrimaryTypeExpr(self: *Parser) Error!Node.Index {
    switch (self.currentTokenTag()) {
        .keyword_true, .keyword_false => return self.addNode(.{
            .tag = .bool_literal,
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
        .string_literal => {
            return self.addNode(.{
                .tag = .string_literal,
                .main_token = self.nextToken(),
                .data = .{
                    .lhs = undefined,
                    .rhs = undefined,
                },
            });
        },
        .identifier => { // TODO check if is fn
            return self.addNode(.{
                .tag = .identifier,
                .main_token = self.nextToken(),
                .data = .{
                    .lhs = undefined,
                    .rhs = undefined,
                },
            });
        },
        // .keyword_if => return self.parseIf(expectTypeExpr), // TODO: why separate expr and type expr?
        .l_paren => {
            // TODO: handle function
            return self.addNode(.{
                .tag = .grouped_expression,
                .main_token = self.nextToken(),
                .data = .{
                    .lhs = blk: {
                        self.ignore_newlines = true;
                        const expr = try self.expectExpr();
                        self.ignore_newlines = false;
                        break :blk expr;
                    },
                    .rhs = try self.expectToken(.r_paren),
                },
            });
        },
        else => return null_node,
    }
}

// TODO
/// SuffixOp
///     <- LBRACKET Expr (DOT2 Expr?)? RBRACKET
///      / DOT IDENTIFIER
///      / DOTQUESTIONMARK // TODO: replace with EXCLAMATIONMARK or QUESTIONMARK
fn parseSuffixOp(self: *Parser, lhs: Node.Index) Error!Node.Index {
    _ = self;
    _ = lhs;
    return null_node;
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
    // TODO
    // if (self.ignore_newlines) {
    //     while (self.token_tags[self.tok_i] == .newline) {
    //         self.tok_i += 1;
    //     }
    // }
    const result = self.tok_i;
    self.tok_i += 1;
    return result;
}

/// Returns current token tag
/// If ignore_newlines is true, it skips newlines until it finds a non-newline token
/// Use self.token_tags[self.tok_i] instead to avoid the check e.g. if it's
/// guaranteed that the current token is not a newline or there shouldn't be a
/// need to skip newlines
fn currentTokenTag(self: *Parser) Token.Tag {
    if (self.ignore_newlines) {
        while (self.token_tags[self.tok_i] == .newline) {
            self.tok_i += 1;
        }
    }
    return self.token_tags[self.tok_i];
}

fn expectToken(self: *Parser, tag: Token.Tag) Error!TokenIndex {
    if (self.token_tags[self.tok_i] != tag) {
        return self.failExpected(tag);
    }
    return self.nextToken();
}

fn expectNewline(self: *Parser, error_tag: Ast.Error.Tag, recoverable: bool) Error!void {
    if (self.token_tags[self.tok_i] == .newline) {
        _ = self.nextToken();
        return;
    }
    try self.warn(error_tag);
    if (!recoverable) {
        return error.ParseError;
    }
}

// TODO: result is weird because newline is a token and used like a semicolon
fn tokensOnSameLine(self: *Parser, tok1: TokenIndex, tok2: TokenIndex) bool {
    return std.mem.indexOfScalar(u8, self.source[self.token_starts[tok1]..self.token_starts[tok2]], '\n') == null;
}

fn eatToken(self: *Parser, tag: Token.Tag) ?TokenIndex {
    return if (self.token_tags[self.tok_i] == tag) self.nextToken() else null;
}

fn assertToken(self: *Parser, tag: Token.Tag) TokenIndex {
    const token = self.nextToken();
    std.debug.assert(self.token_tags[token] == tag);
    return token;
}

// the problem with having the same synchronize for parseTopLevel and parseBlock
// is that we have always one bug, either:
// `}}` in top level -> infinite loop
// `....}` in block -> expected `}` but found `EOF`
// I opted for the second one because an infinite loop is bad
fn synchronize(self: *Parser) void {
    var level: usize = 0;
    while (true) : (self.tok_i += 1) {
        switch (self.token_tags[self.tok_i]) {
            .l_brace => level += 1,
            .r_brace => {
                if (level == 0) {
                    self.tok_i += 1; // the bug is here
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
        // .expected_expr, // TODO: not prev?
        .expected_expr_or_assignment,
        // .expected_labelable,
        // .expected_param_list,
        .expected_prefix_expr,
        // .expected_primary_type_expr,
        // .expected_suffix_op,
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
