// Originally based on https://github.com/ziglang/zig/blob/master/lib/std/zig/Parse.zig
// See https://github.com/ziglang/zig/blob/master/LICENSE for additional LICENSE details

const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const TokenIndex = Ast.TokenIndex;
const assert = std.debug.assert;
const null_node: Node.Index = 0;

const Parser = @This();

source: []const u8,
allocator: std.mem.Allocator,
token_tags: []const Token.Tag,
token_starts: []const u32,
tok_i: TokenIndex,
nodes: Ast.NodeList,
extra_data: std.ArrayListUnmanaged(Node.Index),
errors: std.ArrayListUnmanaged(Ast.Error),
scratch: std.ArrayListUnmanaged(Node.Index),

pub const Error = error{ParseError} || std.mem.Allocator.Error;

pub fn parse(allocator: std.mem.Allocator, source: [:0]const u8) Error!Ast {
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

/// TopLevel <- (Decl | Expr)*
fn parseTopLevel(self: *Parser) Error!void {
    while (true) {
        switch (self.token_tags[self.tok_i]) {
            .eof => {
                break;
            },
            .newline => {
                self.tok_i += 1;
            },
            .keyword_let => {
                const decl = self.expectTopLevelDecl() catch |err| switch (err) {
                    error.OutOfMemory => return err,
                    error.ParseError => {
                        // TODO
                        // self.synchronize();
                        continue;
                    },
                };
                if (decl != null_node) {
                    try self.scratch.append(self.allocator, decl);
                }
            },
            else => {
                const expr = self.expectExpr() catch |err| switch (err) {
                    error.OutOfMemory => return err,
                    error.ParseError => {
                        // TODO
                        // self.synchronize();
                        continue;
                    },
                };
                if (expr != null_node) {
                    try self.scratch.append(self.allocator, expr);
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

fn expectTopLevelDecl(self: *Parser) Error!Node.Index {
    const let_token = self.assertToken(.keyword_let);
    const is_pub = self.consume(.keyword_priv) == null;
    const is_mut = self.consume(.keyword_mut) != null;

    _ = try self.expectToken(.identifier);
    // TODO
    const type_expr = null_node; //if (self.consume(.colon) != null) try self.expectTypeExpr() else null_node;
    const initializer = if (self.consume(.equal) != null) try self.expectExpr() else null_node;

    if (type_expr == null_node and initializer == null_node) {
        return self.failExpected(.equal);
    }

    try self.expectNewline(.expected_newline_after_decl, true);

    return self.addNode(.{
        .tag = .global_decl,
        .main_token = let_token,
        .data = .{
            .lhs = @bitCast(Node.Decl{
                .public = is_pub,
                .mutable = is_mut,
                .type_node = @intCast(type_expr),
            }),
            .rhs = initializer,
        },
    });
}

fn parseDecl(self: *Parser) Error!Node.Index {
    const let_token = self.consume(.keyword_let) orelse return null_node;
    if (self.consume(.keyword_priv) != null) {
        // TODO: warn
    }
    const is_mut = self.consume(.keyword_mut) != null;

    _ = try self.expectToken(.identifier);
    // TODO
    const type_expr = null_node; //if (self.consume(.colon) != null) try self.expectTypeExpr() else null_node;
    const initializer = if (self.consume(.equal) != null) try self.expectExpr() else null_node;

    if (type_expr == null_node and initializer == null_node) {
        return self.failExpected(.equal);
    }

    try self.expectNewline(.expected_newline_after_decl, true);

    return self.addNode(.{
        .tag = .local_decl,
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

// TODO: parseExprPrec(0)
fn parseExpr(self: *Parser) Error!Node.Index {
    switch (self.token_tags[self.tok_i]) {
        .int_literal => return self.addNode(.{
            .tag = .int_literal,
            .main_token = self.nextToken(),
            .data = .{
                .lhs = undefined,
                .rhs = undefined,
            },
        }),
        else => unreachable,
    }
}

fn expectExpr(self: *Parser) Error!Node.Index {
    const node = try self.parseExpr();
    if (node == null_node) {
        return self.fail(.expected_expr);
    } else {
        return node;
    }
}

// TODO: Precedence, Assoc, Rule, rules
// fn parseExprPrec(self: *Parser, min_prec: u8) Error!Node.Index {
//     var lhs = try self.parsePrefixExpr();
//     while (true) {
//         const op = self.token_tags[self.tok_i];
//         const prec = self.getOpPrecedence(op);
//         if (prec < min_prec) {
//             break;
//         }
//         self.tok_i += 1;
//         const rhs = try self.parseExprPrec(prec + 1);
//         lhs = self.addNode(.{
//             .binary = .{
//                 .lhs = lhs,
//                 .rhs = rhs,
//                 .op = op,
//             },
//         });
//     }
//     return lhs;
// }

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
    std.debug.assert(self.token_tags[token] == tag);
    return token;
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
