const std = @import("std");
const builtin = @import("builtin");
const Chunk = @import("Chunk.zig");
const Scanner = @import("Scanner.zig");
const Value = @import("value.zig").Value;
const Token = Scanner.Token;
const debug = @import("debug.zig");

// TODO: parser produces AST for codegen that produces bytecode

pub fn compile(source: [:0]const u8, chunk: *Chunk) bool {
    var parser = Parser.init(source, chunk);
    parser.advance();
    parser.expression();
    parser.consume(.eof, "Expect end of expression.");
    endCompiler(&parser);
    return !parser.had_error;
    // var scanner = Scanner.init(source);
    // var line: usize = 0;
    // while (true) {
    //     const token = scanner.next();
    //     if (token.line != line) {
    //         debug.print("{d:0>4} ", .{token.line});
    //         line = token.line;
    //     } else {
    //         debug.print("   | ", .{});
    //     }
    //     debug.print("{}\n", .{token});
    //     if (token.tag == .eof) {
    //         break;
    //     }
    // }
}

fn endCompiler(parser: *Parser) void {
    parser.emitByte(@intFromEnum(Chunk.OpCode.@"return"));
    if (builtin.mode == .Debug) {
        if (!parser.had_error) {
            debug.disassembleChunk(parser.chunk, "code");
        }
    }
}

fn grouping(parser: *Parser) void {
    parser.expression();
    parser.consume(.r_paren, "Expect ')' after expression.");
}

fn unary(parser: *Parser) void {
    const tag = parser.previous.tag;
    parser.parsePrecedence(.unary);
    switch (tag) {
        .bang => parser.emitByte(@intFromEnum(Chunk.OpCode.not)),
        .minus => parser.emitByte(@intFromEnum(Chunk.OpCode.negation)),
        else => unreachable,
    }
}

fn binary(parser: *Parser) void {
    const tag = parser.previous.tag;
    const rule = Parser.getRule(tag);
    parser.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1)); // TODO why?
    switch (tag) {
        .bang_equal => parser.emitTwoBytes(
            @intFromEnum(Chunk.OpCode.equal),
            @intFromEnum(Chunk.OpCode.not),
        ),
        .equal_equal => parser.emitByte(@intFromEnum(Chunk.OpCode.equal)),
        .l_angle_bracket => parser.emitByte(@intFromEnum(Chunk.OpCode.less)),
        .l_angle_bracket_equal => parser.emitTwoBytes(
            @intFromEnum(Chunk.OpCode.greater),
            @intFromEnum(Chunk.OpCode.not),
        ),
        .r_angle_bracket => parser.emitByte(@intFromEnum(Chunk.OpCode.greater)),
        .r_angle_bracket_equal => parser.emitTwoBytes(
            @intFromEnum(Chunk.OpCode.less),
            @intFromEnum(Chunk.OpCode.not),
        ),
        .keyword_or => parser.emitByte(@intFromEnum(Chunk.OpCode.@"or")),
        .keyword_and => {
            parser.emitTwoBytes(
                @intFromEnum(Chunk.OpCode.@"or"),
                @intFromEnum(Chunk.OpCode.not),
            );
        },
        .plus => parser.emitByte(@intFromEnum(Chunk.OpCode.add)),
        .minus => parser.emitByte(@intFromEnum(Chunk.OpCode.sub)),
        .asterisk => parser.emitByte(@intFromEnum(Chunk.OpCode.mul)),
        .slash => parser.emitByte(@intFromEnum(Chunk.OpCode.div)),
        else => unreachable,
    }
}

fn literal(parser: *Parser) void {
    switch (parser.previous.tag) {
        .number => parser.number(),
        .bool => {
            const value = parser.previous.literal.?.bool;
            parser.emitConstant(.{ .bool = value });
        },
        else => unreachable,
    }
}

pub const Parser = struct {
    scanner: Scanner,
    chunk: *Chunk,
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,

    const Precedence = enum {
        none,
        assignment, // = += -= *= /=
        @"or", // or
        @"and", // and
        equality, // == !=
        comparison, // < > <= >=
        term, // + -
        factor, // * /
        unary, // ! -
        call, // . ()
        primary,
    };

    const ParseFn = *const fn (*Parser) void;

    const Rule = struct {
        precedence: Precedence,
        prefix: ?ParseFn,
        infix: ?ParseFn,
    };

    // must tbe in the same order as Scanner.Token.Tag
    const rules = [_]Rule{
        .{ .precedence = .none, .prefix = grouping, .infix = null }, // l_paren
        .{ .precedence = .none, .prefix = null, .infix = null }, // r_paren
        .{ .precedence = .none, .prefix = null, .infix = null }, // l_brace
        .{ .precedence = .none, .prefix = null, .infix = null }, // r_brace
        .{ .precedence = .none, .prefix = null, .infix = null }, // comma
        .{ .precedence = .none, .prefix = null, .infix = null }, // period
        .{ .precedence = .term, .prefix = unary, .infix = binary }, // minus
        .{ .precedence = .term, .prefix = null, .infix = binary }, // plus
        .{ .precedence = .factor, .prefix = null, .infix = binary }, // slash
        .{ .precedence = .factor, .prefix = null, .infix = binary }, // asterisk
                       //.unary?
        .{ .precedence = .none, .prefix = unary, .infix = null }, // bang
        .{ .precedence = .equality, .prefix = null, .infix = binary }, // bang_equal
        .{ .precedence = .assignment, .prefix = null, .infix = binary }, // equal
        .{ .precedence = .equality, .prefix = null, .infix = binary }, // equal_equal
        .{ .precedence = .comparison, .prefix = null, .infix = binary }, // l_angle_bracket
        .{ .precedence = .comparison, .prefix = null, .infix = binary }, // r_angle_bracket
        .{ .precedence = .comparison, .prefix = null, .infix = binary }, // l_angle_bracket_equal
        .{ .precedence = .comparison, .prefix = null, .infix = binary }, // r_angle_bracket_equal
        .{ .precedence = .assignment, .prefix = null, .infix = binary }, // plus_equal
        .{ .precedence = .assignment, .prefix = null, .infix = binary }, // minus_equal
        .{ .precedence = .assignment, .prefix = null, .infix = binary }, // asterisk_equal
        .{ .precedence = .assignment, .prefix = null, .infix = binary }, // slash_equal
        .{ .precedence = .@"and", .prefix = null, .infix = binary }, // keyword_and
        .{ .precedence = .@"or", .prefix = null, .infix = binary }, // keyword_or
        .{ .precedence = .none, .prefix = null, .infix = null }, // keyword_else
        .{ .precedence = .none, .prefix = null, .infix = null }, // keyword_for
        .{ .precedence = .none, .prefix = null, .infix = null }, // keyword_if
        .{ .precedence = .none, .prefix = null, .infix = null }, // keyword_print
        .{ .precedence = .none, .prefix = null, .infix = null }, // keyword_return
        .{ .precedence = .none, .prefix = null, .infix = null }, // keyword_let
        .{ .precedence = .none, .prefix = null, .infix = null }, // keyword_while
        .{ .precedence = .none, .prefix = null, .infix = null }, // identifier
        .{ .precedence = .none, .prefix = string, .infix = null }, // string
        .{ .precedence = .none, .prefix = number, .infix = null }, // number
        .{ .precedence = .none, .prefix = literal, .infix = null }, // bool
        .{ .precedence = .none, .prefix = null, .infix = null }, // eof
        .{ .precedence = .none, .prefix = null, .infix = null }, // invalid
    };

    inline fn getRule(tag: Token.Tag) Rule {
        return rules[@intFromEnum(tag)];
    }

    pub fn init(source: [:0]const u8, chunk: *Chunk) Parser {
        return .{
            .scanner = Scanner.init(source),
            .chunk = chunk,
            .current = undefined,
            .previous = undefined,
            .had_error = false,
            .panic_mode = false,
        };
    }

    fn advance(self: *Parser) void {
        self.previous = self.current;
        while (true) {
            self.current = self.scanner.next();
            if (self.current.tag != .invalid) {
                break;
            }
            self.errorAtCurrent(self.current.literal.?.string);
        }
    }

    fn expression(self: *Parser) void {
        self.parsePrecedence(.assignment);
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) void {
        self.advance();
        const prefixRule = getRule(self.previous.tag).prefix orelse {
            self.errorAtPrevious("Expect expression.");
            return;
        };
        prefixRule(self);
        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.current.tag).precedence)) {
            self.advance();
            const infixRule = getRule(self.previous.tag).infix.?;
            infixRule(self);
        }
    }

    fn consume(self: *Parser, tag: Token.Tag, message: []const u8) void {
        if (self.current.tag == tag) {
            self.advance();
            return;
        }
        self.errorAtCurrent(message);
    }

    fn emitByte(self: *Parser, byte: u8) void {
        self.chunk.write(byte, self.previous.line) catch unreachable;
    }

    fn emitTwoBytes(self: *Parser, byte1: u8, byte2: u8) void {
        self.emitByte(byte1);
        self.emitByte(byte2);
    }

    fn makeConstant(self: *Parser, value: Value) u8 {
        const constant = self.chunk.addConstant(value) catch unreachable;
        if (constant > std.math.maxInt(u8)) {
            self.errorAtPrevious("Too many constants in one chunk.");
            return 0;
        }
        return @intCast(constant);
    }

    fn emitConstant(self: *Parser, value: Value) void {
        self.emitTwoBytes(
            @intFromEnum(Chunk.OpCode.constant),
            self.makeConstant(value),
        );
    }

    fn number(self: *Parser) void {
        const value = self.previous.literal.?.number; // TODO: parse number here?
        self.emitConstant(.{ .number = value });
    }

    fn string(self: *Parser) void {
        const value = self.previous.literal.?.string;
        self.emitConstant(.{ .string = value });
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) void {
        self.errorAt(&self.current, message);
    }

    fn errorAtPrevious(self: *Parser, message: []const u8) void {
        self.errorAt(&self.previous, message);
    }

    fn errorAt(self: *Parser, token: *const Token, message: []const u8) void {
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

        debug.print(": {s}\n", .{message});
        self.had_error = true;
    }
};
