const std = @import("std");
const builtin = @import("builtin");
const Chunk = @import("Chunk.zig");
const Scanner = @import("Scanner.zig");
const Value = @import("value.zig").Value;
const Token = Scanner.Token;
const debug = @import("debug.zig");

// TODO: parser produces AST for codegen that produces bytecode

var current: *Compiler = undefined; // TODO: a global!?

pub fn compile(source: [:0]const u8, chunk: *Chunk) bool {
    var parser = Parser.init(source, chunk);
    var compiler = Compiler.init(&parser);
    current = &compiler;
    parser.advance();
    while (!parser.match(.eof)) {
        parser.declaration();
    }
    // parser.expression();
    // parser.consume(.eof, "Expect end of expression");
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

const Compiler = struct {
    locals: std.BoundedArray(Local, 256),
    scope_depth: usize,
    parser: *Parser,

    pub fn init(parser: *Parser) Compiler {
        return .{
            .locals = .{},
            .scope_depth = 0,
            .parser = parser,
        };
    }
};

const Local = struct {
    name: Token,
    depth: usize,
};

fn endCompiler(parser: *Parser) void {
    parser.emitByte(@intFromEnum(Chunk.OpCode.@"return"));
    if (builtin.mode == .Debug) {
        if (!parser.had_error) {
            debug.disassembleChunk(parser.chunk, "code");
        }
    }
}

fn beginScope() void {
    current.scope_depth += 1;
}

fn endScope() void {
    current.scope_depth -= 1;
    var it = std.mem.reverseIterator(current.locals.constSlice());
    while (it.next()) |local| {
        if (local.depth <= current.scope_depth) {
            break;
        }
        current.parser.emitByte(@intFromEnum(Chunk.OpCode.pop));
        _ = current.locals.pop(); // TODO: will the iterator be invalidated?
    }
}

fn addLocal(name: Token) void {
    const local: Local = .{
        .name = name,
        .depth = current.scope_depth,
    };
    current.locals.append(local) catch @panic("Too many local variables");
}

// should be in Parser.zig
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

    const Rule = struct {
        precedence: Precedence,
        prefix: ?*const fn (*Parser, bool) void,
        infix: ?*const fn (*Parser, bool) void,
    };

    // must tbe in the same order as Scanner.Token.Tag
    const rules = [_]Rule{
        .{ .precedence = .none, .prefix = grouping, .infix = null }, // l_paren
        .{ .precedence = .none, .prefix = null, .infix = null }, // r_paren
        .{ .precedence = .none, .prefix = null, .infix = null }, // l_brace
        .{ .precedence = .none, .prefix = null, .infix = null }, // r_brace
        .{ .precedence = .none, .prefix = null, .infix = null }, // comma
        .{ .precedence = .none, .prefix = null, .infix = null }, // period
        .{ .precedence = .none, .prefix = null, .infix = null }, // colon
        .{ .precedence = .term, .prefix = unary, .infix = binary }, // minus
        .{ .precedence = .term, .prefix = null, .infix = binary }, // plus
        .{ .precedence = .factor, .prefix = null, .infix = binary }, // asterisk
        .{ .precedence = .factor, .prefix = null, .infix = binary }, // slash
        .{ .precedence = .factor, .prefix = null, .infix = binary }, // percent
                       //.unary?
        .{ .precedence = .none, .prefix = unary, .infix = null }, // bang
        .{ .precedence = .equality, .prefix = null, .infix = binary }, // bang_equal
        .{ .precedence = .assignment, .prefix = null, .infix = null }, // equal
        .{ .precedence = .equality, .prefix = null, .infix = binary }, // equal_equal
        .{ .precedence = .comparison, .prefix = null, .infix = binary }, // l_angle_bracket
        .{ .precedence = .comparison, .prefix = null, .infix = binary }, // r_angle_bracket
        .{ .precedence = .comparison, .prefix = null, .infix = binary }, // l_angle_bracket_equal
        .{ .precedence = .comparison, .prefix = null, .infix = binary }, // r_angle_bracket_equal
        .{ .precedence = .assignment, .prefix = null, .infix = null }, // plus_equal
        .{ .precedence = .assignment, .prefix = null, .infix = null }, // minus_equal
        .{ .precedence = .assignment, .prefix = null, .infix = null }, // asterisk_equal
        .{ .precedence = .assignment, .prefix = null, .infix = null }, // slash_equal
        .{ .precedence = .assignment, .prefix = null, .infix = null }, // percent_equal
        .{ .precedence = .@"and", .prefix = null, .infix = @"and" }, // keyword_and
        .{ .precedence = .@"or", .prefix = null, .infix = @"or" }, // keyword_or
        .{ .precedence = .none, .prefix = null, .infix = null }, // keyword_else
        .{ .precedence = .none, .prefix = null, .infix = null }, // keyword_if
        .{ .precedence = .none, .prefix = null, .infix = null }, // keyword_match
        .{ .precedence = .none, .prefix = null, .infix = null }, // keyword_print
        .{ .precedence = .none, .prefix = null, .infix = null }, // keyword_return
        .{ .precedence = .none, .prefix = null, .infix = null }, // keyword_let
        .{ .precedence = .none, .prefix = null, .infix = null }, // keyword_mut // TODO
        .{ .precedence = .none, .prefix = null, .infix = null }, // keyword_loop
        .{ .precedence = .none, .prefix = null, .infix = null }, // keyword_while
        .{ .precedence = .none, .prefix = null, .infix = null }, // keyword_for
        .{ .precedence = .none, .prefix = null, .infix = null }, // keyword_break
        .{ .precedence = .none, .prefix = null, .infix = null }, // keyword_continue
        .{ .precedence = .none, .prefix = variable, .infix = null }, // identifier
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

    fn declaration(self: *Parser) void {
        if (self.match(.keyword_let)) {
            self.letDeclaration();
        } else {
            self.statement();
        }

        if (self.panic_mode) {
            self.synchronize();
        }
    }

    fn letDeclaration(self: *Parser) void {
        const global = self.parseVariable("Expect variable name");
        // TODO: check type and infer type if there is none
        if (self.match(.colon)) {
            self.consume(.identifier, "Expect type after ':'"); // TODO
        }
        if (self.match(.equal)) {
            self.expression();
        } else {
            // self.emitByte(@intFromEnum(Chunk.OpCode.nil));
        }
        // self.consume(.semicolon, "Expect ';' after value");
        self.defineVariable(global);
    }

    fn defineVariable(self: *Parser, global: u8) void {
        if (current.scope_depth > 0) {
            return;
        }
        self.emitTwoBytes(@intFromEnum(Chunk.OpCode.define_global), global);
    }

    fn parseVariable(self: *Parser, error_message: []const u8) u8 {
        self.consume(.identifier, error_message);
        self.declareVariable();
        if (current.scope_depth > 0) {
            return 0;
        }
        return self.identifierConstant(self.previous);
    }

    fn identifierConstant(self: *Parser, name: Token) u8 {
        return self.makeConstant(.{ .string = name.literal.?.string }); // TODO: dupe string?
    }

    fn declareVariable(self: *Parser) void {
        if (current.scope_depth == 0) {
            return;
        }
        const name = self.previous;
        // reverse iterate?
        for (current.locals.constSlice()) |local| {
            // if (local.depth != -1 and local.depth < current.scope_depth) {
            //     // allow shadowing, check below for rest of implementation
            //     // https://craftinginterpreters.com/local-variables.html#another-scope-edge-case
            //     break;
            // }
            if (eqlIdentifier(name, local.name)) {
                self.errorAtPrevious("Variable with this name already declared in this scope");
            }
        }
        addLocal(name);
    }

    // TODO: make braces mandatory for if, else, loop, while, for
    fn statement(self: *Parser) void {
        if (self.match(.keyword_print)) {
            self.printStatement();
        } else if (self.match(.keyword_if)) {
            self.ifStatement();
        } else if (self.match (.keyword_loop)) {
            self.loopStatement();
        } else if (self.match(.keyword_while)) {
            self.whileStatement();
        } else if (self.match(.keyword_for)) {
            self.forStatement();
        } else if (self.match(.l_brace)) {
            beginScope();
            self.block();
            endScope();
        } else {
            self.expressionStatement();
        }
    }

    fn check(self: *Parser, tag: Token.Tag) bool {
        return self.current.tag == tag;
    }

    fn block(self: *Parser) void {
        while (!self.check(.r_brace) and !self.check(.eof)) {
            self.declaration();
        }
        self.consume(.r_brace, "Expect '}' after block");
    }

    fn expressionStatement(self: *Parser) void {
        self.expression();
        // self.consume(.semicolon, "Expect ';' after value");
        self.emitByte(@intFromEnum(Chunk.OpCode.pop));
    }

    fn match(self: *Parser, tag: Token.Tag) bool {
        if (!self.check(tag)) {
            return false;
        }
        self.advance();
        return true;
    }

    fn printStatement(self: *Parser) void {
        self.expression();
        // self.consume(.semicolon, "Expect ';' after value");
        self.emitByte(@intFromEnum(Chunk.OpCode.print));
    }

    fn ifStatement(self: *Parser) void {
        self.consume(.l_paren, "Expect '(' after 'if'");
        self.expression();
        self.consume(.r_paren, "Expect ')' after condition");

        const then_jump = self.emitJump(@intFromEnum(Chunk.OpCode.jump_if_false));
        self.emitByte(@intFromEnum(Chunk.OpCode.pop));
        self.statement();
        const else_jump = self.emitJump(@intFromEnum(Chunk.OpCode.jump));

        self.patchJump(then_jump);
        self.emitByte(@intFromEnum(Chunk.OpCode.pop));

        if (self.match(.keyword_else)) {
            self.statement();
        }
        self.patchJump(else_jump);
    }

    fn emitJump(self: *Parser, instruction: u8) usize {
        self.emitByte(instruction);
        self.emitByte(0xff);
        self.emitByte(0xff);
        return self.chunk.code.items.len - 2;
    }

    fn patchJump(self: *Parser, offset: usize) void {
        // -2 to adjust for the bytecode for the jump offset itself.
        const jump = self.chunk.code.items.len - offset - 2;
        if (jump > std.math.maxInt(u16)) {
            self.errorAtPrevious("Too much code to jump over");
        }
        self.chunk.code.items[offset] = @truncate(jump >> 8);
        self.chunk.code.items[offset + 1] = @truncate(jump);
    }

    fn loopStatement(self: *Parser) void {
        const loop_start = self.chunk.code.items.len;
        self.statement();
        self.emitLoop(loop_start);
    }

    fn whileStatement(self: *Parser) void {
        const loop_start = self.chunk.code.items.len;
        self.consume(.l_paren, "Expect '(' after 'while'");
        self.expression();
        self.consume(.r_paren, "Expect ')' after condition");

        const exit_jump = self.emitJump(@intFromEnum(Chunk.OpCode.jump_if_false));
        self.emitByte(@intFromEnum(Chunk.OpCode.pop));
        self.statement();
        self.emitLoop(loop_start);

        self.patchJump(exit_jump);
        self.emitByte(@intFromEnum(Chunk.OpCode.pop));
    }

    // TODO: foreach loop, see https://craftinginterpreters.com/jumping-back-and-forth.html#for-statements
    // for (0..) / for (0..x) / for (obj) / for (obj[0..]) / for (obj, obj2, x..)
    fn forStatement(self: *Parser) void {
        _ = self;
    }

    fn emitLoop(self: *Parser, loop_start: usize) void {
        self.emitByte(@intFromEnum(Chunk.OpCode.loop));
        const offset = self.chunk.code.items.len - loop_start + 2;
        if (offset > std.math.maxInt(u16)) {
            self.errorAtPrevious("Loop body too large");
        }
        self.emitByte(@truncate(offset >> 8));
        self.emitByte(@truncate(offset));
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) void {
        self.advance();
        const prefixRule = getRule(self.previous.tag).prefix orelse {
            self.errorAtPrevious("Expect expression");
            return;
        };
        const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.assignment);
        prefixRule(self, can_assign);
        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.current.tag).precedence)) {
            self.advance();
            const infixRule = getRule(self.previous.tag).infix.?;
            infixRule(self, can_assign);
        }

        if (can_assign and self.match(.equal)) {
            self.errorAtPrevious("Invalid assignment target");
        }
    }

    fn consume(self: *Parser, tag: Token.Tag, message: []const u8) void {
        if (self.check(tag)) {
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
            self.errorAtPrevious("Too many constants in one chunk");
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

    fn number(self: *Parser, _: bool) void {
        const value = self.previous.literal.?.number; // TODO: parse number here?
        self.emitConstant(.{ .number = value });
    }

    fn string(self: *Parser, _: bool) void {
        const value = self.previous.literal.?.string;
        self.emitConstant(.{ .string = value }); // TODO: could be invalid pointer later
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

    fn synchronize(self: *Parser) void {
        self.panic_mode = false;

        while (self.current.tag != .eof) {
            // if (self.previous.tag == .semicolon) {
            //     return;
            // }

            switch (self.current.tag) {
                .keyword_let,
                .keyword_for,
                .keyword_if,
                .keyword_while,
                .keyword_print,
                .keyword_return,
                => return,
                else => {},
            }

            self.advance();
        }
    }
};

// all of this below should be in Parser

fn grouping(parser: *Parser, _: bool) void {
    parser.expression();
    parser.consume(.r_paren, "Expect ')' after expression");
}

fn unary(parser: *Parser, _: bool) void {
    const tag = parser.previous.tag;
    parser.parsePrecedence(.unary);
    switch (tag) {
        .bang => parser.emitByte(@intFromEnum(Chunk.OpCode.not)),
        .minus => parser.emitByte(@intFromEnum(Chunk.OpCode.negation)),
        else => unreachable,
    }
}

fn binary(parser: *Parser, _: bool) void {
    const tag = parser.previous.tag;
    const rule = Parser.getRule(tag);
    parser.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));
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
        .plus => parser.emitByte(@intFromEnum(Chunk.OpCode.add)),
        .minus => parser.emitByte(@intFromEnum(Chunk.OpCode.sub)),
        .asterisk => parser.emitByte(@intFromEnum(Chunk.OpCode.mul)),
        .slash => parser.emitByte(@intFromEnum(Chunk.OpCode.div)),
        .percent => parser.emitByte(@intFromEnum(Chunk.OpCode.mod)),
        else => unreachable,
    }
}

fn literal(parser: *Parser, can_assign: bool) void {
    switch (parser.previous.tag) {
        .number => parser.number(can_assign),
        .bool => {
            const value = parser.previous.literal.?.bool;
            parser.emitConstant(.{ .bool = value });
        },
        else => unreachable,
    }
}

fn variable(parser: *Parser, can_assign: bool) void {
    namedVariable(parser, parser.previous, can_assign);
}

fn namedVariable(parser: *Parser, name: Token, can_assign: bool) void {
    var get_op = @intFromEnum(Chunk.OpCode.get_local);
    var set_op = @intFromEnum(Chunk.OpCode.set_local);
    const arg = resolveLocal(current, name) orelse blk: {
        get_op = @intFromEnum(Chunk.OpCode.get_global);
        set_op = @intFromEnum(Chunk.OpCode.set_global);
        break :blk parser.identifierConstant(name);
    };

    if (can_assign) {
        if (parser.match(.equal)) {
            parser.expression();
            parser.emitTwoBytes(set_op, arg);
            return;
        } else if (parser.match(.plus_equal)) {
            parser.emitTwoBytes(get_op, arg);
            parser.expression();
            parser.emitByte(@intFromEnum(Chunk.OpCode.add));
            parser.emitTwoBytes(set_op, arg);
            return;
        } else if (parser.match(.minus_equal)) {
            parser.emitTwoBytes(get_op, arg);
            parser.expression();
            parser.emitByte(@intFromEnum(Chunk.OpCode.sub));
            parser.emitTwoBytes(set_op, arg);
            return;
        } else if (parser.match(.asterisk_equal)) {
            parser.emitTwoBytes(get_op, arg);
            parser.expression();
            parser.emitByte(@intFromEnum(Chunk.OpCode.mul));
            parser.emitTwoBytes(set_op, arg);
            return;
        } else if (parser.match(.slash_equal)) {
            parser.emitTwoBytes(get_op, arg);
            parser.expression();
            parser.emitByte(@intFromEnum(Chunk.OpCode.div));
            parser.emitTwoBytes(set_op, arg);
            return;
        } else if (parser.match(.percent_equal)) {
            parser.emitTwoBytes(get_op, arg);
            parser.expression();
            parser.emitByte(@intFromEnum(Chunk.OpCode.mod));
            parser.emitTwoBytes(set_op, arg);
            return;
        }
    }

    parser.emitTwoBytes(get_op, arg);
}

fn eqlIdentifier(a: Token, b: Token) bool {
    return std.mem.eql(u8, a.literal.?.string, b.literal.?.string);
}

fn resolveLocal(compiler: *Compiler, name: Token) ?u8 {
    var it = std.mem.reverseIterator(compiler.locals.constSlice());
    while (it.next()) |local| {
        if (eqlIdentifier(name, local.name)) {
            return @intCast(it.index);
        }
    }
    return null;
}

fn @"and"(parser: *Parser, _: bool) void {
    const end_jump = parser.emitJump(@intFromEnum(Chunk.OpCode.jump_if_false));
    parser.emitByte(@intFromEnum(Chunk.OpCode.pop));
    parser.parsePrecedence(.@"and");
    parser.patchJump(end_jump);
}

fn @"or"(parser: *Parser, _: bool) void {
    const else_jump = parser.emitJump(@intFromEnum(Chunk.OpCode.jump_if_false));
    const end_jump = parser.emitJump(@intFromEnum(Chunk.OpCode.jump));
    parser.patchJump(else_jump);
    parser.emitByte(@intFromEnum(Chunk.OpCode.pop));
    parser.parsePrecedence(.@"or");
    parser.patchJump(end_jump);
}
