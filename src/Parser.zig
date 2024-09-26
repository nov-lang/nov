// Based on https://github.com/ziglang/zig/blob/master/lib/std/zig/Parse.zig
// See https://github.com/ziglang/zig/blob/master/LICENSE for LICENSE details

const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const TokenIndex = Ast.TokenIndex;
const assert = std.debug.assert;

const null_node: Node.Index = 0; // use null instead?

const Parser = @This();

source: []const u8,
allocator: std.mem.Allocator,
token_tags: []const Token.Tag,
token_starts: []const Ast.ByteOffset,
tok_i: TokenIndex,
nodes: Ast.NodeList,
extra_data: std.ArrayListUnmanaged(Node.Index),
errors: std.ArrayListUnmanaged(Ast.Error),
scratch: std.ArrayListUnmanaged(Node.Index),

pub const Error = error{ParseError} || std.mem.Allocator.Error;

// TODO: should treat global scope as a function
pub fn parse(self: *Parser) Error!void {
    try self.nodes.append(self.allocator, .{
        .tag = .root,
        .main_token = 0,
        .data = undefined,
    });
    // const root_members = try self.parseContainerMembers();
    const root_members = try self.parseTopLevel();
    const root_decls = try root_members.toSpan(self);
    if (self.token_tags[self.tok_i] != .eof) {
        try self.warnExpected(.eof);
    }
    self.nodes.items(.data)[0] = .{
        .lhs = root_decls.start,
        .rhs = root_decls.end,
    };
}

const Members = struct {
    len: usize,
    lhs: Node.Index,
    rhs: Node.Index,

    fn toSpan(self: Members, p: *Parser) Error!Node.SubRange {
        if (self.len <= 2) {
            const nodes = [2]Node.Index{ self.lhs, self.rhs };
            return p.listToSpan(nodes[0..self.len]);
        } else {
            return .{ .start = self.lhs, .end = self.rhs };
        }
    }
};

fn listToSpan(self: *Parser, list: []const Node.Index) Error!Node.SubRange {
    try self.extra_data.appendSlice(self.allocator, list);
    return .{
        .start = @intCast(self.extra_data.items.len - list.len),
        .end = @intCast(self.extra_data.items.len),
    };
}

fn addNode(self: *Parser, node: Ast.Node) Error!Node.Index {
    const result: Node.Index = @intCast(self.nodes.len);
    try self.nodes.append(self.allocator, node);
    return result;
}

fn setNode(self: *Parser, i: usize, node: Ast.Node) Node.Index {
    self.nodes.set(i, node);
    return @intCast(i);
}

fn reserveNode(self: *Parser, tag: Ast.Node.Tag) Error!usize {
    try self.nodes.resize(self.allocator, self.nodes.len + 1);
    self.nodes.items(.tag)[self.nodes.len - 1] = tag;
    return self.nodes.len - 1;
}

fn unreserveNode(self: *Parser, node_index: usize) void {
    if (self.nodes.len == node_index) {
        self.nodes.resize(self.allocator, self.nodes.len - 1) catch unreachable;
    } else {
        // There is zombie node left in the tree, let's make it as inoffensive as possible
        self.nodes.items(.tag)[node_index] = .no_op;
        self.nodes.items(.main_token)[node_index] = self.tok_i;
    }
}

fn addExtra(self: *Parser, extra: anytype) Error!Node.Index {
    const fields = std.meta.fields(@TypeOf(extra));
    try self.extra_data.ensureUnusedCapacity(self.allocator, fields.len);
    const result: Node.Index = @intCast(self.extra_data.items.len);
    inline for (fields) |field| {
        comptime assert(field.type == Node.Index);
        self.extra_data.appendAssumeCapacity(@field(extra, field.name));
    }
    return result;
}

/// GlobalMembers <- (Decl | Statement)*
fn parseTopLevel(self: *Parser) Error!Members {
    const scratch_top = self.scratch.items.len;
    defer self.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        switch (self.token_tags[self.tok_i]) {
            .newline => {
                self.tok_i += 1;
            },
            .eof => {
                break;
            },
            .keyword_let,
            .keyword_fn,
            => {
                const top_level_decl = try self.expectTopLevelDeclRecoverable();
                if (top_level_decl != 0) {
                    try self.scratch.append(self.allocator, top_level_decl);
                }
            },
            .keyword_if,
            .keyword_loop,
            .keyword_while,
            .keyword_for,
            .l_brace,
            => {
                const top_level_stmt = try self.expectTopLevelStatementRecoverable();
                if (top_level_stmt != 0) {
                    try self.scratch.append(self.allocator, top_level_stmt);
                }
            },
            else => {
                const top_level_stmt = try self.expectTopLevelStatementRecoverable();
                if (top_level_stmt != 0) {
                    try self.scratch.append(self.allocator, top_level_stmt);
                }
            },
        }
    }

    const items = self.scratch.items[scratch_top..];
    switch (items.len) {
        0 => return .{
            .len = 0,
            .lhs = 0,
            .rhs = 0,
        },
        1 => return .{
            .len = 1,
            .lhs = items[0],
            .rhs = 0,
        },
        2 => return .{
            .len = 2,
            .lhs = items[0],
            .rhs = items[1],
        },
        else => {
            const span = try self.listToSpan(items);
            return .{
                .len = items.len,
                .lhs = span.start,
                .rhs = span.end,
            };
        },
    }
}

// TODO
fn parseContainerMembers(self: *Parser) Error!Members {
    _ = self;
    unreachable;
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

/// Decl <- FnProto (NEWLINE / Block) / VarDecl
fn expectTopLevelDecl(self: *Parser) Error!Node.Index {
    const fn_proto = try self.parseFnProto();
    if (fn_proto != 0) {
        switch (self.token_tags[self.tok_i]) {
            .newline => {
                self.tok_i += 1;
                return fn_proto;
            },
            .l_brace => {
                const fn_decl_index = try self.reserveNode(.fn_decl);
                errdefer self.unreserveNode(fn_decl_index);

                const body_block = try self.parseBlock();
                assert(body_block != 0);
                return self.setNode(fn_decl_index, .{
                    .tag = .fn_decl,
                    .main_token = self.nodes.items(.main_token)[fn_proto],
                    .data = .{
                        .lhs = fn_proto,
                        .rhs = body_block,
                    },
                });
            },
            else => {
                // Since parseBlock only return error.ParseError on
                // a missing '}' we can assume this function was
                // supposed to end here.
                try self.warn(.expected_newline_or_lbrace);
                return null_node;
            },
        }
    }

    const var_decl = try self.parseGlobalVarDecl();
    assert(var_decl != 0);
    return var_decl;
}

fn expectTopLevelDeclRecoverable(self: *Parser) Error!Node.Index {
    return self.expectTopLevelDecl() catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ParseError => {
            self.findNextContainerMember();
            return null_node;
        },
    };
}
/// FnProto <- KEYWORD_fn IDENTIFIER? LPAREN ParamDeclList RPAREN ByteAlign? AddrSpace? LinkSection? CallConv? EXCLAMATIONMARK? TypeExpr
/// FnProto <- IDENTIFIER LPAREN ParamDeclList RPAREN EXCLAMATIONMARK? TypeExpr?
fn parseFnProto(self: *Parser) Error!Node.Index {
    const fn_token = self.consume(.keyword_fn) orelse return null_node;

    // We want the fn proto node to be before its children in the array.
    const fn_proto_index = try self.reserveNode(.fn_proto);
    errdefer self.unreserveNode(fn_proto_index);

    _ = self.consume(.identifier);
    const params = try self.parseParamDeclList();
    _ = self.consume(.bang);
    const return_type_expr = try self.parseTypeExpr();

    switch (params) {
        .zero_or_one => |param| return self.setNode(fn_proto_index, .{
            .tag = .fn_proto_one,
            .main_token = fn_token,
            .data = .{
                .lhs = param,
                .rhs = return_type_expr,
            },
        }),
        .multi => |span| {
            return self.setNode(fn_proto_index, .{
                .tag = .fn_proto,
                .main_token = fn_token,
                .data = .{
                    .lhs = try self.addExtra(Node.SubRange{
                        .start = span.start,
                        .end = span.end,
                    }),
                    .rhs = return_type_expr,
                },
            });
        },
    }
}

/// VarDeclProto <- KEYWORD_let KEYWORD_mut? IDENTIFIER (COLON TypeExpr)?
/// Returns a `*_var_decl` node with its rhs (init expression) initialized to 0.
fn parseVarDeclProto(self: *Parser) Error!Node.Index {
    const main_token = self.consume(.keyword_let) orelse return null_node;
    const is_mut = self.consume(.keyword_mut) != null;

    _ = try self.expectToken(.identifier);
    const type_node: Node.Index = if (self.consume(.colon) == null) 0 else try self.expectTypeExpr();
    return self.addNode(.{
        .tag = if (is_mut) .mut_var_decl else .var_decl,
        .main_token = main_token,
        .data = .{
            .lhs = type_node,
            .rhs = 0,
        },
    });
}

/// GlobalVarDecl <- VarDeclProto (EQUAL Expr?) NEWLINE
fn parseGlobalVarDecl(self: *Parser) Error!Node.Index {
    const var_decl = try self.parseVarDeclProto();
    if (var_decl == 0) {
        return null_node;
    }

    const init_node: Node.Index = switch (self.token_tags[self.tok_i]) {
        .equal_equal => blk: {
            try self.warn(.wrong_equal_var_decl);
            self.tok_i += 1;
            break :blk try self.expectExpr();
        },
        .equal => blk: {
            self.tok_i += 1;
            break :blk try self.expectExpr();
        },
        else => 0,
    };

    self.nodes.items(.data)[var_decl].rhs = init_node;

    try self.expectNewline(.expected_newline_after_decl, false);
    return var_decl;
}

// TODO: expectContainerField() (not yet)

/// Statement
///     <- IfStatement
///      / LoopStatement
///      / WhileStatement
///      / ForStatement
///      / MatchStatement
///      / Block
///      / VarDeclExprStatement
fn expectStatement(self: *Parser, allow_defer_var: bool) Error!Node.Index {
    // TODO:
    // self.discardNewlines();

    return switch (self.token_tags[self.tok_i]) {
        .keyword_if => self.expectIfStatement(),
        .keyword_loop => self.expectLoopStatement(),
        .keyword_while => self.expectWhileStatement(),
        .keyword_for => self.expectForStatement(),
        .keyword_match => self.expectMatchExpr(),
        .l_brace => self.parseBlock(),
        else => blk: {
            std.log.debug("expectStatement: {}", .{self.token_tags[self.tok_i]});
            // TODO: ?
            if (allow_defer_var) {
                break :blk self.expectVarDeclExprStatement();
            } else {
                const assign = try self.expectAssignExpr();
                std.log.debug("(self.token_tags[self.tok_i]: {})", .{self.token_tags[self.tok_i]});
                try self.expectNewline(.expected_newline_after_stmt, true);
                break :blk assign;
            }
        },
    };
}

fn expectStatementRecoverable(self: *Parser) Error!Node.Index {
    while (true) {
        return self.expectStatement(true) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.ParseError => {
                self.findNextStmt(); // findNextStmt
                switch (self.token_tags[self.tok_i]) {
                    .r_brace => return null_node,
                    .eof => return error.ParseError,
                    else => continue,
                }
            },
        };
    }
}

fn expectTopLevelStatementRecoverable(self: *Parser) Error!Node.Index {
    return self.expectStatement(true) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ParseError => {
            self.findNextContainerMember(); // findNextContainerMember
            return null_node;
        },
    };
}

// TODO
/// VarDeclExprStatement
///    <- VarDeclProto (COMMA (VarDeclProto / Expr))* EQUAL Expr NEWLINE
///     / Expr (AssignOp Expr / (COMMA (VarDeclProto / Expr))+ EQUAL Expr)? NEWLINE
fn expectVarDeclExprStatement(self: *Parser) Error!Node.Index {
    const scratch_top = self.scratch.items.len;
    defer self.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        const var_decl_proto = try self.parseVarDeclProto();
        if (var_decl_proto != 0) {
            try self.scratch.append(self.allocator, var_decl_proto);
        } else {
            const expr = try self.parseExpr();
            if (expr == 0) {
                if (self.scratch.items.len == scratch_top) {
                    // We parsed nothing
                    return self.fail(.expected_statement);
                } else {
                    // We've had at least one LHS, but had a bad comma
                    return self.fail(.expected_expr_or_var_decl);
                }
            }
            try self.scratch.append(self.allocator, expr);
        }
        _ = self.consume(.comma) orelse break;
    }

    const lhs_count = self.scratch.items.len - scratch_top;
    assert(lhs_count > 0);

    const equal_token = self.consume(.equal) orelse eql: {
        if (lhs_count > 1) {
            // Definitely a destructure, so allow recovering from ==
            if (self.consume(.equal_equal)) |tok| {
                try self.warnMsg(.{ .tag = .wrong_equal_var_decl, .token = tok });
                break :eql tok;
            }
            return self.failExpected(.equal);
        }
        const lhs = self.scratch.items[scratch_top];
        switch (self.nodes.items(.tag)[lhs]) {
            .var_decl, .mut_var_decl => {
                // Definitely a var decl, so allow recovering from ==
                if (self.consume(.equal_equal)) |tok| {
                    try self.warnMsg(.{ .tag = .wrong_equal_var_decl, .token = tok });
                    break :eql tok;
                }
                return self.failExpected(.equal);
            },
            else => {},
        }

        const expr = try self.finishAssignExpr(lhs);
        try self.expectNewline(.expected_newline_after_stmt, true);
        return expr;
    };

    const rhs = try self.expectExpr();
    try self.expectNewline(.expected_newline_after_stmt, true);

    if (lhs_count == 1) {
        const lhs = self.scratch.items[scratch_top];
        switch (self.nodes.items(.tag)[lhs]) {
            .var_decl, .mut_var_decl => {
                self.nodes.items(.data)[lhs].rhs = rhs;
                return lhs;
            },
            else => {},
        }
        const expr = try self.addNode(.{
            .tag = .assign,
            .main_token = equal_token,
            .data = .{ .lhs = lhs, .rhs = rhs },
        });
        return expr;
    }

    unreachable; // TODO: multi_assign

    // const extra_start = self.extra_data.items.len;
    // try self.extra_data.ensureUnusedCapacity(self.allocator, lhs_count + 1);
    // self.extra_data.appendAssumeCapacity(@intCast(lhs_count));
    // self.extra_data.appendSliceAssumeCapacity(self.scratch.items[scratch_top..]);

    // return self.addNode(.{
    //     .tag = .assign_destructure,
    //     .main_token = equal_token,
    //     .data = .{
    //         .lhs = @intCast(extra_start),
    //         .rhs = rhs,
    //     },
    // });
}

/// IfStatement
///     <- IfPrefix Block ( KEYWORD_else Statement )?
///      / IfPrefix AssignExpr ( NEWLINE / KEYWORD_else Statement )
fn expectIfStatement(self: *Parser) Error!Node.Index {
    const if_token = self.assertToken(.keyword_if);
    _ = try self.expectToken(.l_paren);
    const condition = try self.expectExpr();
    _ = try self.expectToken(.r_paren);
    // _ = try self.parsePtrPayload(); // if (cond) | |<-

    var else_required = false;
    const then_expr = blk: {
        const block = try self.parseBlock();
        if (block != 0) break :blk block;
        const assign_expr = try self.parseAssignExpr();
        if (assign_expr == 0) {
            return self.fail(.expected_block_or_assignment);
        }
        if (self.consume(.newline)) |_| {
            return self.addNode(.{
                .tag = .@"if",
                .main_token = if_token,
                .data = .{
                    .lhs = condition,
                    .rhs = assign_expr,
                },
            });
        }
        else_required = true;
        break :blk assign_expr;
    };
    _ = self.consume(.keyword_else) orelse {
        if (else_required) {
            try self.warn(.expected_newline_or_else);
        }
        return self.addNode(.{
            .tag = .@"if",
            .main_token = if_token,
            .data = .{
                .lhs = condition,
                .rhs = then_expr,
            },
        });
    };
    // _ = try self.parsePayload(); // else | |<-
    const else_expr = try self.expectStatement(false);
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

/// LoopStatement <- KEYWORD_loop Block
fn expectLoopStatement(self: *Parser) Error!Node.Index {
    const loop_token = self.assertToken(.keyword_loop);
    const block = try self.parseBlock();
    if (block == 0) {
        return self.fail(.expected_block);
    }
    return self.addNode(.{
        .tag = .loop,
        .main_token = loop_token,
        .data = .{
            .lhs = block,
            .rhs = undefined,
        },
    });
}

// TODO
/// WhilePrefix <- KEYWORD_while LPAREN Expr RPAREN PtrPayload? WhileContinueExpr?
///
/// WhileStatement
///     <- WhilePrefix Block ( KEYWORD_else Payload? Statement )?
///      / WhilePrefix AssignExpr ( SEMICOLON / KEYWORD_else Payload? Statement )
fn expectWhileStatement(self: *Parser) Error!Node.Index {
    _ = self;
    unreachable;
    // const while_token = self.assertToken(.keyword_while);
    // _ = try self.expectToken(.l_paren);
    // const condition = try self.expectExpr();
    // _ = try self.expectToken(.r_paren);
    // _ = try self.parsePtrPayload();
    // const cont_expr = try self.parseWhileContinueExpr();

    // // TODO propose to change the syntax so that semicolons are always required
    // // inside while statements, even if there is an `else`.
    // var else_required = false;
    // const then_expr = blk: {
    //     const block_expr = try self.parseBlock();
    //     if (block_expr != 0) break :blk block_expr;
    //     const assign_expr = try self.parseAssignExpr();
    //     if (assign_expr == 0) {
    //         return self.fail(.expected_block_or_assignment);
    //     }
    //     if (self.consume(.semicolon)) |_| {
    //         if (cont_expr == 0) {
    //             return self.addNode(.{
    //                 .tag = .while_simple,
    //                 .main_token = while_token,
    //                 .data = .{
    //                     .lhs = condition,
    //                     .rhs = assign_expr,
    //                 },
    //             });
    //         } else {
    //             return self.addNode(.{
    //                 .tag = .while_cont,
    //                 .main_token = while_token,
    //                 .data = .{
    //                     .lhs = condition,
    //                     .rhs = try self.addExtra(Node.WhileCont{
    //                         .cont_expr = cont_expr,
    //                         .then_expr = assign_expr,
    //                     }),
    //                 },
    //             });
    //         }
    //     }
    //     else_required = true;
    //     break :blk assign_expr;
    // };
    // _ = self.consume(.keyword_else) orelse {
    //     if (else_required) {
    //         try self.warn(.expected_semi_or_else);
    //     }
    //     if (cont_expr == 0) {
    //         return self.addNode(.{
    //             .tag = .while_simple,
    //             .main_token = while_token,
    //             .data = .{
    //                 .lhs = condition,
    //                 .rhs = then_expr,
    //             },
    //         });
    //     } else {
    //         return self.addNode(.{
    //             .tag = .while_cont,
    //             .main_token = while_token,
    //             .data = .{
    //                 .lhs = condition,
    //                 .rhs = try self.addExtra(Node.WhileCont{
    //                     .cont_expr = cont_expr,
    //                     .then_expr = then_expr,
    //                 }),
    //             },
    //         });
    //     }
    // };
    // _ = try self.parsePayload();
    // const else_expr = try self.expectStatement(false);
    // return self.addNode(.{
    //     .tag = .@"while",
    //     .main_token = while_token,
    //     .data = .{
    //         .lhs = condition,
    //         .rhs = try self.addExtra(Node.While{
    //             .cont_expr = cont_expr,
    //             .then_expr = then_expr,
    //             .else_expr = else_expr,
    //         }),
    //     },
    // });
}

// TODO
/// ForStatement
///     <- ForPrefix Block ( KEYWORD_else Statement )?
///      / ForPrefix AssignExpr ( SEMICOLON / KEYWORD_else Statement )
fn expectForStatement(self: *Parser) Error!Node.Index {
    _ = self;
    unreachable;
    // const for_token = self.assertToken(.keyword_for);

    // const scratch_top = self.scratch.items.len;
    // defer self.scratch.shrinkRetainingCapacity(scratch_top);
    // const inputs = try self.forPrefix();

    // var else_required = false;
    // var seen_semicolon = false;
    // const then_expr = blk: {
    //     const block_expr = try self.parseBlock();
    //     if (block_expr != 0) break :blk block_expr;
    //     const assign_expr = try self.parseAssignExpr();
    //     if (assign_expr == 0) {
    //         return self.fail(.expected_block_or_assignment);
    //     }
    //     if (self.consume(.semicolon)) |_| {
    //         seen_semicolon = true;
    //         break :blk assign_expr;
    //     }
    //     else_required = true;
    //     break :blk assign_expr;
    // };
    // var has_else = false;
    // if (!seen_semicolon and self.consume(.keyword_else) != null) {
    //     try self.scratch.append(self.allocator, then_expr);
    //     const else_stmt = try self.expectStatement(false);
    //     try self.scratch.append(self.allocator, else_stmt);
    //     has_else = true;
    // } else if (inputs == 1) {
    //     if (else_required) try self.warn(.expected_semi_or_else);
    //     return self.addNode(.{
    //         .tag = .for_simple,
    //         .main_token = for_token,
    //         .data = .{
    //             .lhs = self.scratch.items[scratch_top],
    //             .rhs = then_expr,
    //         },
    //     });
    // } else {
    //     if (else_required) try self.warn(.expected_semi_or_else);
    //     try self.scratch.append(self.allocator, then_expr);
    // }
    // return self.addNode(.{
    //     .tag = .@"for",
    //     .main_token = for_token,
    //     .data = .{
    //         .lhs = (try self.listToSpan(self.scratch.items[scratch_top..])).start,
    //         .rhs = @as(u32, @bitCast(Node.For{
    //             .inputs = @as(u31, @intCast(inputs)),
    //             .has_else = has_else,
    //         })),
    //     },
    // });
}

/// ForPrefix <- KEYWORD_for LPAREN ForInput (COMMA ForInput)* COMMA? RPAREN ForPayload
///
/// ForInput <- Expr (DOT2 Expr?)?
///
/// ForPayload <- PIPE ASTERISK? IDENTIFIER (COMMA ASTERISK? IDENTIFIER)* PIPE
// fn forPrefix(self: *Parser) Error!usize {
//     const start = self.scratch.items.len;
//     _ = try self.expectToken(.l_paren);

//     while (true) {
//         var input = try self.expectExpr();
//         if (self.consume(.ellipsis2)) |ellipsis| {
//             input = try self.addNode(.{
//                 .tag = .for_range,
//                 .main_token = ellipsis,
//                 .data = .{
//                     .lhs = input,
//                     .rhs = try self.parseExpr(),
//                 },
//             });
//         }

//         try self.scratch.append(self.allocator, input);
//         switch (self.token_tags[self.tok_i]) {
//             .comma => self.tok_i += 1,
//             .r_paren => {
//                 self.tok_i += 1;
//                 break;
//             },
//             .colon, .r_brace, .r_bracket => return self.failExpected(.r_paren),
//             // Likely just a missing comma; give error but continue parsing.
//             else => try self.warn(.expected_comma_after_for_operand),
//         }
//         if (self.consume(.r_paren)) |_| break;
//     }
//     const inputs = self.scratch.items.len - start;

//     _ = self.consume(.pipe) orelse {
//         try self.warn(.expected_loop_payload);
//         return inputs;
//     };

//     var warned_excess = false;
//     var captures: u32 = 0;
//     while (true) {
//         _ = self.consume(.asterisk);
//         const identifier = try self.expectToken(.identifier);
//         captures += 1;
//         if (captures > inputs and !warned_excess) {
//             try self.warnMsg(.{ .tag = .extra_for_capture, .token = identifier });
//             warned_excess = true;
//         }
//         switch (self.token_tags[self.tok_i]) {
//             .comma => self.tok_i += 1,
//             .pipe => {
//                 self.tok_i += 1;
//                 break;
//             },
//             // Likely just a missing comma; give error but continue parsing.
//             else => try self.warn(.expected_comma_after_capture),
//         }
//         if (self.consume(.pipe)) |_| break;
//     }

//     if (captures < inputs) {
//         const index = self.scratch.items.len - captures;
//         const input = self.nodes.items(.main_token)[self.scratch.items[index]];
//         try self.warnMsg(.{ .tag = .for_input_not_captured, .token = input });
//     }
//     return inputs;
// }

/// Blocktatement <- Block / AssignExpr NEWLINE
fn parseBlockStatement(self: *Parser) Error!Node.Index {
    const block = try self.parseBlock();
    if (block != 0) {
        return block;
    }
    const assign_expr = try self.parseAssignExpr();
    if (assign_expr != 0) {
        try self.expectNewline(.expected_newline_after_stmt, true);
        return assign_expr;
    }
    return null_node;
}

fn expectBlockStatement(self: *Parser) Error!Node.Index {
    const node = try self.parseBlockStatement();
    if (node == 0) {
        return self.fail(.expected_block_or_expr);
    }
    return node;
}

/// AssignExpr <- Expr (AssignOp Expr / (COMMA Expr)+ EQUAL Expr)?
///
/// AssignOp
///     <- ASTERISKEQUAL
///      / SLASHEQUAL
///      / PERCENTEQUAL
///      / PLUSEQUAL
///      / MINUSEQUAL
///      / LANGLEBRACKET2EQUAL
///      / RANGLEBRACKET2EQUAL
///      / AMPERSANDEQUAL
///      / CARETEQUAL
///      / PIPEEQUAL
///      / EQUAL
fn parseAssignExpr(self: *Parser) Error!Node.Index {
    const expr = try self.parseExpr();
    if (expr == 0) return null_node;
    return self.finishAssignExpr(expr);
}

/// SingleAssignExpr <- Expr (AssignOp Expr)?
fn parseSingleAssignExpr(self: *Parser) Error!Node.Index {
    const lhs = try self.parseExpr();
    if (lhs == 0) return null_node;
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

fn finishAssignExpr(self: *Parser, lhs: Node.Index) Error!Node.Index {
    const tok = self.token_tags[self.tok_i];
    // if (tok == .comma) return self.finishAssignDestructureExpr(lhs);
    const tag = assignOpNode(tok) orelse return lhs;
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
        .l_angle_bracket_angle_bracket_equal => .assign_shl,
        .r_angle_bracket_angle_bracket_equal => .assign_shr,
        .ampersand_equal => .assign_bit_and,
        .caret_equal => .assign_bit_xor,
        .equal => .assign,
        else => null,
    };
}

// fn finishAssignDestructureExpr(self: *Parser, first_lhs: Node.Index) Error!Node.Index {
//     const scratch_top = self.scratch.items.len;
//     defer self.scratch.shrinkRetainingCapacity(scratch_top);

//     try self.scratch.append(self.allocator, first_lhs);

//     while (self.consume(.comma)) |_| {
//         const expr = try self.expectExpr();
//         try self.scratch.append(self.allocator, expr);
//     }

//     const equal_token = try self.expectToken(.equal);

//     const rhs = try self.expectExpr();

//     const lhs_count = self.scratch.items.len - scratch_top;
//     assert(lhs_count > 1); // we already had first_lhs, and must have at least one more lvalue

//     const extra_start = self.extra_data.items.len;
//     try self.extra_data.ensureUnusedCapacity(self.allocator, lhs_count + 1);
//     self.extra_data.appendAssumeCapacity(@intCast(lhs_count));
//     self.extra_data.appendSliceAssumeCapacity(self.scratch.items[scratch_top..]);

//     return self.addNode(.{
//         .tag = .assign_destructure,
//         .main_token = equal_token,
//         .data = .{
//             .lhs = @intCast(extra_start),
//             .rhs = rhs,
//         },
//     });
// }

fn expectSingleAssignExpr(self: *Parser) Error!Node.Index {
    const expr = try self.parseSingleAssignExpr();
    if (expr == 0) {
        return self.fail(.expected_expr_or_assignment);
    }
    return expr;
}

fn expectAssignExpr(self: *Parser) Error!Node.Index {
    const expr = try self.parseAssignExpr();
    if (expr == 0) {
        return self.fail(.expected_expr_or_assignment);
    }
    return expr;
}

fn parseExpr(self: *Parser) Error!Node.Index {
    return self.parseExprPrecedence(.assignment);
}

fn expectExpr(p: *Parser) Error!Node.Index {
    const node = try p.parseExpr();
    if (node == 0) {
        return p.fail(.expected_expr);
    } else {
        return node;
    }
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
    _,
};

const Assoc = enum {
    left,
    none,
};

const Rule = struct {
    prec: Precedence,
    tag: Node.Tag,
    assoc: Assoc = .left,
};

const rules = std.enums.directEnumArrayDefault(Token.Tag, Rule, .{ .prec = .none, .tag = .root }, 0, .{
    .keyword_or = .{ .prec = .bool_or, .tag = .bool_or },

    .keyword_and = .{ .prec = .bool_and, .tag = .bool_and },

    .equal_equal = .{ .prec = .equality, .tag = .equal_equal, .assoc = .none },
    .bang_equal = .{ .prec = .equality, .tag = .bang_equal, .assoc = .none },

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
});

fn parseExprPrecedence(self: *Parser, min_prec: Precedence) Error!Node.Index {
    assert(@intFromEnum(min_prec) >= 0);
    var node = try self.parsePrefixExpr();
    if (node == 0) {
        return null_node;
    }

    var banned_prec: Precedence = .none;

    while (true) {
        const tok_tag = self.token_tags[self.tok_i];
        const info = rules[@intFromEnum(tok_tag)];
        if (@intFromEnum(info.prec) < @intFromEnum(min_prec)) {
            break;
        }
        if (@intFromEnum(info.prec) == @intFromEnum(banned_prec)) {
            return self.fail(.chained_comparison_operators);
        }

        const oper_token = self.nextToken();
        // Special-case handling for "catch"
        // if (tok_tag == .keyword_catch) {
        //     _ = try self.parsePayload();
        // }
        const rhs = try self.parseExprPrecedence(@enumFromInt(@intFromEnum(info.prec) + 1));
        if (rhs == 0) {
            try self.warn(.expected_expr);
            return node;
        }

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
            .tag = info.tag,
            .main_token = oper_token,
            .data = .{
                .lhs = node,
                .rhs = rhs,
            },
        });

        if (info.assoc == .none) {
            banned_prec = info.prec;
        }
    }

    return node;
}

/// PrefixExpr <- PrefixOp* PrimaryExpr
///
/// PrefixOp
///     <- EXCLAMATIONMARK
///      / MINUS
///      / TILDE
fn parsePrefixExpr(self: *Parser) Error!Node.Index {
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
    if (node == 0) {
        return self.fail(.expected_prefix_expr);
    }
    return node;
}

// TODO: not sure about this
/// TypeExpr <- PrefixTypeOp* ErrorUnionExpr
///
/// PrefixTypeOp <- QUESTIONMARK / SliceTypeStart
///
/// SliceTypeStart <- LBRACKET RBRACKET
fn parseTypeExpr(self: *Parser) Error!Node.Index {
    switch (self.token_tags[self.tok_i]) {
        .question_mark => return self.addNode(.{
            .tag = .optional_type,
            .main_token = self.nextToken(),
            .data = .{
                .lhs = try self.expectTypeExpr(),
                .rhs = undefined,
            },
        }),
        .l_bracket => {
            const lbracket = self.nextToken();
            _ = try self.expectToken(.r_bracket);
            const elem_type = try self.expectTypeExpr();
            return self.addNode(.{
                .tag = .slice_type,
                .main_token = lbracket,
                .data = .{
                    .lhs = elem_type,
                    .rhs = undefined,
                },
            });
        },
        else => return self.parseErrorUnionExpr(),
    }
}

fn expectTypeExpr(self: *Parser) Error!Node.Index {
    const node = try self.parseTypeExpr();
    if (node == 0) {
        return self.fail(.expected_type_expr);
    }
    return node;
}

/// PrimaryExpr
///     <- IfExpr
///      / KEYWORD_break Expr?
///      / KEYWORD_continue
///      / KEYWORD_return Expr?
///      / Block
///      / CurlySuffixExpr
fn parsePrimaryExpr(self: *Parser) Error!Node.Index {
    switch (self.token_tags[self.tok_i]) {
        .keyword_if => return self.parseIfExpr(),
        .keyword_break => {
            return self.addNode(.{
                .tag = .@"break",
                .main_token = self.nextToken(),
                .data = .{
                    .lhs = try self.parseExpr(),
                    .rhs = undefined,
                },
            });
        },
        .keyword_continue => {
            return self.addNode(.{
                .tag = .@"continue",
                .main_token = self.nextToken(),
                .data = .{
                    .lhs = undefined,
                    .rhs = undefined,
                },
            });
        },
        .keyword_return => {
            return self.addNode(.{
                .tag = .@"return",
                .main_token = self.nextToken(),
                .data = .{
                    .lhs = try self.parseExpr(),
                    .rhs = undefined,
                },
            });
        },
        .l_brace => return self.parseBlock(),
        else => return self.parseCurlySuffixExpr(),
    }
}

/// IfExpr <- IfPrefix Expr (KEYWORD_else Payload? Expr)?
fn parseIfExpr(self: *Parser) Error!Node.Index {
    return self.parseIf(expectExpr);
}

// TODO: last value of the block is the return value
//       -> would this be easier/better with block labels?
/// Block <- LBRACE Statement* RBRACE
fn parseBlock(self: *Parser) Error!Node.Index {
    const lbrace = self.consume(.l_brace) orelse return null_node;
    const scratch_top = self.scratch.items.len;
    defer self.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        self.discardNewlines();
        if (self.token_tags[self.tok_i] == .r_brace) break;
        const statement = try self.expectStatementRecoverable();
        if (statement == 0) break;
        try self.scratch.append(self.allocator, statement);
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

// TODO
/// CurlySuffixExpr <- TypeExpr InitList?
///
/// InitList
///     <- LBRACE FieldInit (COMMA FieldInit)* COMMA? RBRACE
///      / LBRACE Expr (COMMA Expr)* COMMA? RBRACE
///      / LBRACE RBRACE
fn parseCurlySuffixExpr(self: *Parser) Error!Node.Index {
    const lhs = try self.parseTypeExpr();
    return lhs;
    // if (lhs == 0) return null_node;
    // const lbrace = self.consume(.l_brace) orelse return lhs;

    // If there are 0 or 1 items, we can use ArrayInitOne/StructInitOne;
    // otherwise we use the full ArrayInit/StructInit.

    // const scratch_top = self.scratch.items.len;
    // defer self.scratch.shrinkRetainingCapacity(scratch_top);
    // const field_init = try self.parseFieldInit();
    // if (field_init != 0) {
    //     try self.scratch.append(self.allocator, field_init);
    //     while (true) {
    //         switch (self.token_tags[self.tok_i]) {
    //             .comma => self.tok_i += 1,
    //             .r_brace => {
    //                 self.tok_i += 1;
    //                 break;
    //             },
    //             .colon, .r_paren, .r_bracket => return self.failExpected(.r_brace),
    //             // Likely just a missing comma; give error but continue parsing.
    //             else => try self.warn(.expected_comma_after_initializer),
    //         }
    //         if (self.consume(.r_brace)) |_| break;
    //         const next = try self.expectFieldInit();
    //         try self.scratch.append(self.allocator, next);
    //     }
    //     const comma = (self.token_tags[self.tok_i - 2] == .comma);
    //     const inits = self.scratch.items[scratch_top..];
    //     switch (inits.len) {
    //         0 => unreachable,
    //         1 => return self.addNode(.{
    //             .tag = if (comma) .struct_init_one_comma else .struct_init_one,
    //             .main_token = lbrace,
    //             .data = .{
    //                 .lhs = lhs,
    //                 .rhs = inits[0],
    //             },
    //         }),
    //         else => return self.addNode(.{
    //             .tag = if (comma) .struct_init_comma else .struct_init,
    //             .main_token = lbrace,
    //             .data = .{
    //                 .lhs = lhs,
    //                 .rhs = try self.addExtra(try self.listToSpan(inits)),
    //             },
    //         }),
    //     }
    // }

    // while (true) {
    //     if (self.consume(.r_brace)) |_| break;
    //     const elem_init = try self.expectExpr();
    //     try self.scratch.append(self.allocator, elem_init);
    //     switch (self.token_tags[self.tok_i]) {
    //         .comma => self.tok_i += 1,
    //         .r_brace => {
    //             self.tok_i += 1;
    //             break;
    //         },
    //         .colon, .r_paren, .r_bracket => return self.failExpected(.r_brace),
    //         // Likely just a missing comma; give error but continue parsing.
    //         else => try self.warn(.expected_comma_after_initializer),
    //     }
    // }
    // const comma = (self.token_tags[self.tok_i - 2] == .comma);
    // const inits = self.scratch.items[scratch_top..];
    // switch (inits.len) {
    //     0 => return self.addNode(.{
    //         .tag = .struct_init_one,
    //         .main_token = lbrace,
    //         .data = .{
    //             .lhs = lhs,
    //             .rhs = 0,
    //         },
    //     }),
    //     1 => return self.addNode(.{
    //         .tag = if (comma) .array_init_one_comma else .array_init_one,
    //         .main_token = lbrace,
    //         .data = .{
    //             .lhs = lhs,
    //             .rhs = inits[0],
    //         },
    //     }),
    //     else => return self.addNode(.{
    //         .tag = if (comma) .array_init_comma else .array_init,
    //         .main_token = lbrace,
    //         .data = .{
    //             .lhs = lhs,
    //             .rhs = try self.addExtra(try self.listToSpan(inits)),
    //         },
    //     }),
    // }
}

// TODO: no error union (yet)
/// ErrorUnionExpr <- SuffixExpr (EXCLAMATIONMARK TypeExpr)?
fn parseErrorUnionExpr(self: *Parser) Error!Node.Index {
    const suffix_expr = try self.parseSuffixExpr();
    return suffix_expr;
    // if (suffix_expr == 0) return null_node;
    // const bang = self.consume(.bang) orelse return suffix_expr;
    // return self.addNode(.{
    //     .tag = .error_union,
    //     .main_token = bang,
    //     .data = .{
    //         .lhs = suffix_expr,
    //         .rhs = try self.expectTypeExpr(),
    //     },
    // });
}

// TODO: we don't care about commas
/// SuffixExpr
///     <- PrimaryTypeExpr (SuffixOp / FnCallArguments)*
///
/// FnCallArguments <- LPAREN ExprList RPAREN
///
/// ExprList <- (Expr COMMA)* Expr?
fn parseSuffixExpr(self: *Parser) Error!Node.Index {
    var res = try self.parsePrimaryTypeExpr();
    if (res == 0) return res;
    while (true) {
        const suffix_op = try self.parseSuffixOp(res);
        if (suffix_op != 0) {
            res = suffix_op;
            continue;
        }
        const lparen = self.consume(.l_paren) orelse return res;
        const scratch_top = self.scratch.items.len;
        defer self.scratch.shrinkRetainingCapacity(scratch_top);
        while (true) {
            if (self.consume(.r_paren)) |_| break;
            const param = try self.expectExpr();
            try self.scratch.append(self.allocator, param);
            switch (self.token_tags[self.tok_i]) {
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
                    .rhs = 0,
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

// TODO
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
fn parsePrimaryTypeExpr(self: *Parser) Error!Node.Index {
    switch (self.token_tags[self.tok_i]) {
        .int_literal => return self.addNode(.{
            .tag = .int_literal,
            .main_token = self.nextToken(),
            .data = .{
                .lhs = undefined,
                .rhs = undefined,
            },
        }),
        .float_literal => return self.addNode(.{
            .tag = .float_literal,
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
        .keyword_fn => return self.parseFnProto(),
        .keyword_if => return self.parseIf(expectTypeExpr),
        // .keyword_struct,
        // .keyword_opaque,
        // .keyword_enum,
        // .keyword_union,
        // => return self.parseContainerDeclAuto(),
        .multiline_string_literal_line => {
            const first_line = self.nextToken();
            while (self.token_tags[self.tok_i] == .multiline_string_literal_line) {
                self.tok_i += 1;
            }
            return self.addNode(.{
                .tag = .multiline_string_literal,
                .main_token = first_line,
                .data = .{
                    .lhs = first_line,
                    .rhs = self.tok_i - 1,
                },
            });
        },
        // .period => switch (self.token_tags[self.tok_i + 1]) {
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
        //                 switch (self.token_tags[self.tok_i]) {
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
        //             const comma = (self.token_tags[self.tok_i - 2] == .comma);
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
        //             switch (self.token_tags[self.tok_i]) {
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
        //         const comma = (self.token_tags[self.tok_i - 2] == .comma);
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
        // .keyword_error => switch (self.token_tags[self.tok_i + 1]) {
        //     .l_brace => {
        //         const error_token = self.tok_i;
        //         self.tok_i += 2;
        //         while (true) {
        //             if (self.consume(.r_brace)) |_| break;
        //             _ = try self.eatDocComments();
        //             _ = try self.expectToken(.identifier);
        //             switch (self.token_tags[self.tok_i]) {
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
        .l_paren => return self.addNode(.{
            .tag = .grouped_expression,
            .main_token = self.nextToken(),
            .data = .{
                .lhs = try self.expectExpr(),
                .rhs = try self.expectToken(.r_paren),
            },
        }),
        else => return null_node,
    }
}

// TODO: parseFieldInit (not yet)
// TODO: expectFieldInit (not yet)

// TODO: we don't care about commas
/// MatchExpr <- KEYWORD_match LPAREN Expr RPAREN LBRACE MatchProngList RBRACE
fn expectMatchExpr(self: *Parser) !Node.Index {
    const match_token = self.assertToken(.keyword_match);
    _ = try self.expectToken(.l_paren);
    const expr_node = try self.expectExpr();
    _ = try self.expectToken(.r_paren);
    _ = try self.expectToken(.l_brace);
    const cases = try self.parseMatchProngList();
    _ = try self.expectToken(.r_brace);

    return self.addNode(.{
        .tag = .match,
        .main_token = match_token,
        .data = .{
            .lhs = expr_node,
            .rhs = try self.addExtra(Node.SubRange{
                .start = cases.start,
                .end = cases.end,
            }),
        },
    });
}

/// MatchProng <- MatchCase EQUALRARROW AssignExpr
///
/// MatchCase <- MatchItem (COMMA MatchItem)* COMMA? / UNDERSCORE
fn parseMatchProng(self: *Parser) Error!Node.Index {
    const scratch_top = self.scratch.items.len;
    defer self.scratch.shrinkRetainingCapacity(scratch_top);

    if (self.consume(.underscore) == null) {
        while (true) {
            const item = try self.parseMatchItem();
            if (item == 0) break;
            try self.scratch.append(self.allocator, item);
            if (self.consume(.comma) == null) break;
        }
        if (scratch_top == self.scratch.items.len) {
            return null_node;
        }
    }
    const arrow_token = try self.expectToken(.equal_r_angle_bracket);

    const items = self.scratch.items[scratch_top..];
    switch (items.len) {
        0 => return self.addNode(.{
            .tag = .match_case_one,
            .main_token = arrow_token,
            .data = .{
                .lhs = 0,
                .rhs = try self.expectSingleAssignExpr(),
            },
        }),
        1 => return self.addNode(.{
            .tag = .match_case_one,
            .main_token = arrow_token,
            .data = .{
                .lhs = items[0],
                .rhs = try self.expectSingleAssignExpr(),
            },
        }),
        else => return self.addNode(.{
            .tag = .match_case,
            .main_token = arrow_token,
            .data = .{
                .lhs = try self.addExtra(try self.listToSpan(items)),
                .rhs = try self.expectSingleAssignExpr(),
            },
        }),
    }
}

/// MatchProngList <- (MatchProng COMMA)* MatchProng?
fn parseMatchProngList(self: *Parser) Error!Node.SubRange {
    const scratch_top = self.scratch.items.len;
    defer self.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        const item = try parseMatchProng(self);
        if (item == 0) break;

        try self.scratch.append(self.allocator, item);

        switch (self.token_tags[self.tok_i]) {
            .comma => self.tok_i += 1,
            // All possible delimiters.
            .colon, .r_paren, .r_brace, .r_bracket => break,
            // Likely just a missing comma; give error but continue parsing.
            else => try self.warn(.expected_comma_after_match_prong),
        }
    }
    return self.listToSpan(self.scratch.items[scratch_top..]);
}

/// MatchItem <- Expr (DOT2 Expr)?
fn parseMatchItem(self: *Parser) Error!Node.Index {
    const expr = try self.parseExpr();
    if (expr == 0) return null_node;

    if (self.consume(.ellipsis2)) |token| {
        return self.addNode(.{
            .tag = .match_range,
            .main_token = token,
            .data = .{
                .lhs = expr,
                .rhs = try self.expectExpr(),
            },
        });
    }
    return expr;
}

// TODO
/// SuffixOp
///     <- LBRACKET Expr (DOT2 (Expr? (COLON Expr)?)?)? RBRACKET
///      / DOT IDENTIFIER
///      / DOTASTERISK
///      / DOTQUESTIONMARK
fn parseSuffixOp(self: *Parser, lhs: Node.Index) Error!Node.Index {
    _ = self;
    _ = lhs;
    return null_node;
    // switch (self.token_tags[self.tok_i]) {
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
    //     .period => switch (self.token_tags[self.tok_i + 1]) {
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

// TODO: parseContainerDeclAuto (not yet)

const SmallSpan = union(enum) {
    zero_or_one: Node.Index,
    multi: Node.SubRange,
};

// TODO
/// ParamDeclList <- (ParamDecl COMMA)* ParamDecl?
fn parseParamDeclList(self: *Parser) !SmallSpan {
    _ = self;
    unreachable;
    // _ = try self.expectToken(.l_paren);
    // const scratch_top = self.scratch.items.len;
    // defer self.scratch.shrinkRetainingCapacity(scratch_top);
    // var varargs: union(enum) { none, seen, nonfinal: TokenIndex } = .none;
    // while (true) {
    //     if (self.consume(.r_paren)) |_| break;
    //     if (varargs == .seen) varargs = .{ .nonfinal = self.tok_i };
    //     const param = try self.expectTypeExpr();
    //     if (param != 0) {
    //         try self.scratch.append(self.allocator, param);
    //     } else if (self.token_tags[self.tok_i - 1] == .ellipsis3) {
    //         if (varargs == .none) varargs = .seen;
    //     }
    //     switch (self.token_tags[self.tok_i]) {
    //         .comma => self.tok_i += 1,
    //         .r_paren => {
    //             self.tok_i += 1;
    //             break;
    //         },
    //         .colon, .r_brace, .r_bracket => return self.failExpected(.r_paren),
    //         // Likely just a missing comma; give error but continue parsing.
    //         else => try self.warn(.expected_comma_after_param),
    //     }
    // }
    // if (varargs == .nonfinal) {
    //     try self.warnMsg(.{ .tag = .varargs_nonfinal, .token = varargs.nonfinal });
    // }
    // const params = self.scratch.items[scratch_top..];
    // return switch (params.len) {
    //     0 => SmallSpan{ .zero_or_one = 0 },
    //     1 => SmallSpan{ .zero_or_one = params[0] },
    //     else => SmallSpan{ .multi = try self.listToSpan(params) },
    // };
}

// TODO: parseBuiltinCall (not yet)

/// IfPrefix <- KEYWORD_if LPAREN Expr RPAREN
fn parseIf(self: *Parser, comptime bodyParseFn: fn (*Parser) Error!Node.Index) Error!Node.Index {
    const if_token = self.consume(.keyword_if) orelse return null_node;
    _ = try self.expectToken(.l_paren);
    const condition = try self.expectExpr();
    _ = try self.expectToken(.r_paren);

    const then_expr = try bodyParseFn(self);
    assert(then_expr != 0);

    _ = self.consume(.keyword_else) orelse return self.addNode(.{
        .tag = .@"if",
        .main_token = if_token,
        .data = .{
            .lhs = condition,
            .rhs = then_expr,
        },
    });
    const else_expr = try bodyParseFn(self);
    assert(else_expr != 0);

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

fn tokensOnSameLine(self: *Parser, tok1: TokenIndex, tok2: TokenIndex) bool {
    return std.mem.indexOfScalar(u8, self.source[self.token_starts[tok1]..self.token_starts[tok2]], '\n') == null;
}

fn discardNewlines(self: *Parser) void {
    while (self.token_tags[self.tok_i] == .newline) {
        self.tok_i += 1;
    }
}

fn consume(self: *Parser, tag: Token.Tag) ?TokenIndex {
    return if (self.token_tags[self.tok_i] == tag) self.nextToken() else null;
}

fn assertToken(self: *Parser, tag: Token.Tag) TokenIndex {
    const token = self.nextToken();
    assert(self.token_tags[token] == tag);
    return token;
}

/// Attempts to find next container member by searching for certain tokens
fn findNextContainerMember(self: *Parser) void {
    var level: u32 = 0;
    while (true) {
        const tok = self.nextToken();
        switch (self.token_tags[tok]) {
            // Any of these can start a new top level declaration.
            .keyword_let,
            .keyword_fn,
            => {
                if (level == 0) {
                    self.tok_i -= 1;
                    return;
                }
            },
            .identifier => {
                if (self.token_tags[tok + 1] == .comma and level == 0) {
                    self.tok_i -= 1;
                    return;
                }
            },
            .comma, .newline => {
                // this decl was likely meant to end here
                if (level == 0) {
                    return;
                }
            },
            .l_paren, .l_bracket, .l_brace => level += 1,
            .r_paren, .r_bracket => {
                if (level != 0) level -= 1;
            },
            .r_brace => {
                if (level == 0) {
                    // end of container, exit
                    self.tok_i -= 1;
                    return;
                }
                level -= 1;
            },
            .eof => {
                self.tok_i -= 1;
                return;
            },
            else => {},
        }
    }
}

/// Attempts to find the next statement by searching for a newline
fn findNextStmt(self: *Parser) void {
    var level: u32 = 0;
    while (true) {
        const tok = self.nextToken();
        switch (self.token_tags[tok]) {
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

// TODO: add a bunch of tests
// see https://github.com/ziglang/zig/blob/master/lib/std/zig/parser_test.zig
// and https://github.com/ziglang/zig/blob/master/lib/std/zig/render.zig
