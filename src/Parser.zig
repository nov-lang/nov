const std = @import("std");
const Tokenizer = @import("Scanner.zig");
const Token = Tokenizer.Token;
const Ast = @import("Ast.zig");
const debug = @import("debug.zig");

const Parser = @This();

ast: Ast,
tokenizer: Tokenizer,
current: Token,
previous: Token,
allocator: std.mem.Allocator,
had_error: bool = false,
panic_mode: bool = false,

pub fn init(allocator: std.mem.Allocator) Parser {
    return .{
        .ast = Ast.init(allocator),
        .tokenizer = undefined,
        .current = undefined,
        .previous = undefined,
        .allocator = allocator,
    };
}

pub fn deinit(self: *Parser) void {
    _ = self;
}

pub fn parse(allocator: std.mem.Allocator, source: [:0]const u8) !Ast {
    var self = Parser.init(allocator);
    defer self.deinit();
    errdefer self.ast.deinit();
    self.tokenizer = Tokenizer.init(source);

    // const body_node = try self.ast.appendNode(.{
    //     .expr = .{
    //         .block = .{
    //             .statements = ...,
    //         },
    //     },
    // });

    try self.advance();
    while (!(try self.match(.eof))) {
        if (try self.declarationOrStatement()) |node| {
            _ = node; // TODO
            // try self.ast.appendNode(node);

        } else {
            self.errorAtPrevious("Expected statement", .{});
        }
    }

    return self.ast;
}

const Precedence = enum {
    none,
    assignment, // = += -= *= /= %=
    @"or", // or
    @"and", // and
    equality, // == !=
    comparison, // < > <= >=
    term, // + -
    factor, // * / %
    unary, // ! -
    call, // . ()
    primary,

    fn get(comptime tag: Token.Tag) Precedence {
        return switch (tag) {
            else => .none,
            .equal,
            .plus_equal,
            .minus_equal,
            .asterisk_equal,
            .slash_equal,
            .percent_equal,
            => .assignment,
            .keyword_or => .@"or",
            .keyword_and => .@"and",
            .equal_equal, .bang_equal => .equality,
            .l_angle_bracket,
            .r_angle_bracket,
            .l_angle_bracket_equal,
            .r_angle_bracket_equal,
            => .comparison,
            .minus, .plus => .term,
            .asterisk, .slash, .percent => .factor,
            // .bang => .unary,
            // .l_paren => .call,
            // .identifier, .string, .number, .bool => .primary,
        };
    }
};

const Rule = struct {
    precedence: Precedence,
    prefix: ?*const fn (*Parser, bool) anyerror!*Ast.Node,
    infix: ?*const fn (*Parser, bool, *Ast.Node) anyerror!*Ast.Node,
};

// must be in the same order as Scanner.Token.Tag
const rules = [_]Rule{
    .{ .precedence = Precedence.get(.l_paren), .prefix = grouping, .infix = null },
    .{ .precedence = Precedence.get(.r_paren), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.l_brace), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.r_brace), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.comma), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.period), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.colon), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.minus), .prefix = unary, .infix = binary },
    .{ .precedence = Precedence.get(.plus), .prefix = null, .infix = binary },
    .{ .precedence = Precedence.get(.asterisk), .prefix = null, .infix = binary },
    .{ .precedence = Precedence.get(.slash), .prefix = null, .infix = binary },
    .{ .precedence = Precedence.get(.percent), .prefix = null, .infix = binary },
    .{ .precedence = Precedence.get(.bang), .prefix = unary, .infix = null },
    .{ .precedence = Precedence.get(.bang_equal), .prefix = null, .infix = binary },
    .{ .precedence = Precedence.get(.equal), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.equal_equal), .prefix = null, .infix = binary },
    .{ .precedence = Precedence.get(.l_angle_bracket), .prefix = null, .infix = binary },
    .{ .precedence = Precedence.get(.r_angle_bracket), .prefix = null, .infix = binary },
    .{ .precedence = Precedence.get(.l_angle_bracket_equal), .prefix = null, .infix = binary },
    .{ .precedence = Precedence.get(.r_angle_bracket_equal), .prefix = null, .infix = binary },
    .{ .precedence = Precedence.get(.plus_equal), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.minus_equal), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.asterisk_equal), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.slash_equal), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.percent_equal), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.keyword_and), .prefix = null, .infix = binary }, // TODO: binary?
    .{ .precedence = Precedence.get(.keyword_or), .prefix = null, .infix = binary }, // TODO: binary?
    .{ .precedence = Precedence.get(.keyword_else), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.keyword_if), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.keyword_match), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.keyword_print), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.keyword_return), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.keyword_let), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.keyword_mut), .prefix = null, .infix = null }, // TODO
    .{ .precedence = Precedence.get(.keyword_loop), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.keyword_while), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.keyword_for), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.keyword_break), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.keyword_continue), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.identifier), .prefix = variable, .infix = null },
    .{ .precedence = Precedence.get(.string), .prefix = null, .infix = null }, // TODO: prefix string
    .{ .precedence = Precedence.get(.number), .prefix = literal, .infix = null },
    .{ .precedence = Precedence.get(.bool), .prefix = null, .infix = null }, // TODO: prefix literal
    .{ .precedence = Precedence.get(.eof), .prefix = null, .infix = null },
    .{ .precedence = Precedence.get(.invalid), .prefix = null, .infix = null },
};

comptime {
    if (std.enums.values(Token.Tag).len != rules.len) {
        @compileError("Mismatch between Token.Tag and rules");
    }
}

inline fn getRule(tag: Token.Tag) Rule {
    return rules[@intFromEnum(tag)];
}

fn advance(self: *Parser) !void {
    self.previous = self.current;
    while (true) {
        self.current = self.tokenizer.next();

        if (self.current.tag == .invalid) {
            if (self.current.literal) |lit| {
                self.errorAtCurrent("{s}", .{lit.string});
            } else {
                self.errorAtCurrent("Unknown error", .{});
            }
        } else {
            _ = try self.ast.appendToken(self.current); // TODO: append invalid too?
            break;
        }
    }
}

fn match(self: *Parser, tag: Token.Tag) !bool {
    if (!self.check(tag)) {
        return false;
    }
    try self.advance();
    return true;
}

inline fn check(self: *Parser, tag: Token.Tag) bool {
    return self.current.tag == tag;
}

inline fn currentCode(self: *Parser) usize {
    return self.chunk.code.items.len;
}

fn declarationOrStatement(self: *Parser) !?*Ast.Node {
    return try self.declaration() orelse try self.statement();
}

fn declaration(self: *Parser) !?*Ast.Node {
    const node = if (try self.match(.keyword_let))
        self.letDeclaration()
    else
        null;

    if (self.panic_mode) {
        try self.synchronize();
    }

    return node;
}

fn letDeclaration(self: *Parser) !*Ast.Node {
    if (true) return error.Unimplemented;
    const is_mut = self.match(.keyword_mut);
    _ = is_mut; // TODO
    const global = self.parseVariable("Expect variable name");
    const tag = blk: {
        if (!self.match(.colon)) {
            break :blk null;
        }
        self.consume(.identifier, "Expect type after ':'");
        const type_name = self.previous.literal.?.string;
        // TODO: support custom types
        if (std.mem.eql(u8, type_name, "number")) {
            break :blk Token.Tag.number;
        } else if (std.mem.eql(u8, type_name, "bool")) {
            break :blk Token.Tag.bool;
        } else if (std.mem.eql(u8, type_name, "string")) {
            break :blk Token.Tag.string;
        } else {
            self.errorAtPrevious("Invalid type", .{});
            break :blk null;
        }
    };
    if (self.match(.equal)) {
        self.expression();
        if (tag != null and self.previous.tag != tag.?) {
            self.errorAtPrevious("Type mismatch: expected '{s}' got '{s}'", .{
                @tagName(tag.?),
                @tagName(self.previous.tag),
            });
        }
    } else {
        if (tag) |t| {
            switch (t) {
                // TODO: very wrong
                .number => self.emitConstant(.{ .number = undefined }),
                .bool => self.emitConstant(.{ .bool = undefined }),
                .string => self.emitConstant(.{ .string = undefined }),
                else => unreachable,
            }
        } else {
            self.errorAtPrevious("Expect type or '=' after variable name", .{});
        }
        // self.emitByte(@intFromEnum(Chunk.OpCode.void));
    }
    // self.consume(.semicolon, "Expect ';' after value");
    self.defineVariable(global);
}

fn statement(self: *Parser) !?*Ast.Node {
    // TODO: allowed in global scope?

    // if (self.match(.keyword_print)) {
    //     self.printStatement();
    // } else if (self.match(.keyword_if)) {
    //     return self.ifStatement();
    // } else if (self.match(.keyword_loop)) {
    //     self.loopStatement();
    // } else if (self.match(.keyword_while)) {
    //     self.whileStatement();
    // } else if (self.match(.keyword_for)) {
    //     self.forStatement();
    // } else if (self.match(.l_brace)) {
    //     beginScope();
    //     self.block();
    //     endScope();
    // }

    return self.expressionStatement();
}

// fn ifStatement(self: *Parser) !*Ast.Node {
//     self.consume(.l_paren, "Expected '(' after 'if'");
//     // try self.beginScope();
//     const condition = try self.expression();
//     self.consume(.r_paren, "Expected ')' after 'if' condition");

//     try self.consume(.l_brace, "Expected '{' after 'if' condition");
//     const body = try self.block();
//     // try self.endScope();

//     var else_branch: ?*Ast.Node = null;
//     if (self.match(.keyword_else)) {
//         try self.consume(.l_brace, "Expected '{' after 'else'");
//         // try self.beginScope();
//         else_branch = try self.block();
//         // try self.endScope();
//     }

//     return self.ast.appendNode(.{
//         .expr = .{
//             .if = .{
//             },
//         },
//     });
// }

fn parsePrecedence(self: *Parser, precedence: Precedence) !*Ast.Node {
    try self.advance();
    const prefixRule = getRule(self.previous.tag).prefix orelse {
        self.errorAtPrevious("Expect expression", .{});
        return error.what;
    };
    const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.assignment);
    var node = try prefixRule(self, can_assign);
    while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.current.tag).precedence)) {
        try self.advance();
        const infixRule = getRule(self.previous.tag).infix.?;
        node = try infixRule(self, can_assign, node);
    }

    if (can_assign and (try self.match(.equal))) {
        self.errorAtPrevious("Invalid assignment target", .{});
    }

    return node;
}

fn expression(self: *Parser) !*Ast.Node {
    // TODO: don't allow assignment everywhere
    return self.parsePrecedence(.assignment);
}

fn expressionStatement(self: *Parser) !*Ast.Node {
    const expr = try self.expression();
    // try self.consume(.semicolon, "Expected ';' after expression");
    return self.ast.appendNode(.{
        .expr = .{
            .expression = expr,
        },
    });
}

fn block(self: *Parser) !*Ast.Node {
    var statements = std.ArrayList(*Ast.Node).init(self.allocator);
    errdefer statements.deinit();
    while (!self.check(.r_brace) and !self.check(.eof)) {
        if (try self.declarationOrStatement()) |node| {
            try statements.append(node);
        }
        // else {
        //     self.errorAtCurrent("Expected statement", .{});
        // }
    }

    try self.consume(.r_brace, "Expected '}' after block");

    return try self.ast.appendNode(.{
        .expr = .{
            .block = .{ .statements = try statements.toOwnedSlice() },
        },
    });
}

fn consume(self: *Parser, tag: Token.Tag, message: []const u8) !void {
    if (self.check(tag)) {
        try self.advance();
        return;
    }
    self.errorAtCurrent("{s}", .{message});
}

fn errorAtCurrent(self: *Parser, comptime fmt: []const u8, args: anytype) void {
    self.errorAt(&self.current, fmt, args);
}

fn errorAtPrevious(self: *Parser, comptime fmt: []const u8, args: anytype) void {
    self.errorAt(&self.previous, fmt, args);
}

fn errorAt(self: *Parser, token: *const Token, comptime fmt: []const u8, args: anytype) void {
    if (self.panic_mode) {
        return;
    }
    self.panic_mode = true;
    debug.print("{d}:{d}: Error", .{ token.line, token.col });

    // if (token.tag == .eof) {
    //     debug.print(" at end");
    // } else if (token.tag == .invalid) {
    //     // Nothing.
    // } else {
    //     debug.print(" at '{}'", .{token.tag.lexeme() orelse ""});
    // }

    debug.print(": " ++ fmt ++ "\n", args);
    self.had_error = true;
}

fn synchronize(self: *Parser) !void {
    self.panic_mode = false;

    while (self.current.tag != .eof) {
        // if (self.previous.tag == .semicolon) {
        //     return;
        // }

        switch (self.current.tag) {
            .keyword_let,
            .keyword_if,
            .keyword_match,
            .keyword_loop,
            .keyword_while,
            .keyword_for,
            .keyword_break,
            .keyword_continue,
            .keyword_print,
            .keyword_return,
            => return,
            else => {},
        }

        try self.advance();
    }
}

fn grouping(self: *Parser, _: bool) !*Ast.Node {
    const expr = try self.expression();
    try self.consume(.r_paren, "Expected ')' after expression");
    return self.ast.appendNode(.{
        .expr = .{
            .grouping = .{ .expression = expr },
        },
    });
}

fn unary(self: *Parser, _: bool) !*Ast.Node {
    const operator = self.previous.tag;
    const left = try self.parsePrecedence(.unary);

    return self.ast.appendNode(.{
        .expr = .{
            .unary = .{
                .operator = operator,
                .right = left,
            },
        },
    });
}

fn binary(self: *Parser, _: bool, left: *Ast.Node) !*Ast.Node {
    const operator = self.previous.tag;
    const rule = getRule(operator);

    const right = try self.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));
    return self.ast.appendNode(.{
        .expr = .{
            .binary = .{
                .left = left,
                .operator = operator,
                .right = right,
            },
        },
    });
}

fn variable(self: *Parser, can_assign: bool) !*Ast.Node {
    return self.namedVariable(self.previous, can_assign);
}

fn namedVariable(self: *Parser, name: Token, can_assign: bool) !*Ast.Node {
    _ = self;
    _ = name;
    _ = can_assign;
    return error.Unimplemented;
}

fn literal(self: *Parser, _: bool) !*Ast.Node {
    var node: Ast.Node = undefined;
    switch (self.previous.tag) {
        .number => node.expr = .{
            .number = .{ .value = self.previous.literal.?.number },
        },
        else => unreachable,
    }

    return self.ast.appendNode(node);
}
