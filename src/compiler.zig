const std = @import("std");
const builtin = @import("builtin");
const Chunk = @import("Chunk.zig");
const Scanner = @import("Scanner.zig");
const Value = @import("value.zig").Value;
const String = @import("value.zig").String;
const Token = Scanner.Token;
const debug = @import("debug.zig");

// TODO: parser produces AST for codegen that produces bytecode

var current: *Compiler = undefined; // TODO: a global!?

pub fn compile(arena_allocator: std.mem.Allocator, source: [:0]const u8, chunk: *Chunk) bool {
    var parser = Parser.init(source, chunk, arena_allocator);
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
    parser.emitOpCode(.@"return");
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
        current.parser.emitOpCode(.pop);
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
    allocator: std.mem.Allocator,

    const Precedence = enum {
        none,
        assignment, // = += -= *= /=
        @"or", // or
        @"and", // and
        equality, // == !=
        comparison, // < > <= >=
        term, // + -
        factor, // * / %
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

    pub fn init(source: [:0]const u8, chunk: *Chunk, allocator: std.mem.Allocator) Parser {
        return .{
            .scanner = Scanner.init(source),
            .chunk = chunk,
            .current = undefined,
            .previous = undefined,
            .had_error = false,
            .panic_mode = false,
            .allocator = allocator,
        };
    }

    fn advance(self: *Parser) void {
        self.previous = self.current;
        while (true) {
            self.current = self.scanner.next();
            if (self.current.tag != .invalid) {
                break;
            }
            self.errorAtCurrent("{s}", .{self.current.literal.?.string});
        }
    }

    fn expression(self: *Parser) void {
        // TODO: don't allow assignment everywhere
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

    fn defineVariable(self: *Parser, global: u24) void {
        if (current.scope_depth > 0) {
            return;
        }
        self.emitCodeArg(.define_global, global);
    }

    fn parseVariable(self: *Parser, error_message: []const u8) u24 {
        self.consume(.identifier, error_message);
        self.declareVariable();
        if (current.scope_depth > 0) {
            return 0;
        }
        return self.identifierConstant(self.previous);
    }

    fn identifierConstant(self: *Parser, name: Token) u24 {
        // TODO: could be invalid pointer later (do not use Value.createObject with this allocator!)
        const value = String.create(self.allocator, name.literal.?.string) catch unreachable;
        return self.makeConstant(value);
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
                self.errorAtPrevious(
                    "Variable with name '{s}' already declared in this scope",
                    .{name.literal.?.string},
                );
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
        } else if (self.match(.keyword_loop)) {
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
        self.emitOpCode(.pop);
    }

    fn match(self: *Parser, tag: Token.Tag) bool {
        if (!self.check(tag)) {
            return false;
        }
        self.advance();
        return true;
    }

    inline fn currentCode(self: *Parser) usize {
        return self.chunk.code.items.len;
    }

    fn printStatement(self: *Parser) void {
        self.expression();
        // self.consume(.semicolon, "Expect ';' after value");
        self.emitOpCode(.print);
    }

    fn ifStatement(self: *Parser) void {
        self.consume(.l_paren, "Expect '(' after 'if'");
        self.expression();
        self.consume(.r_paren, "Expect ')' after condition");

        const then_jump = self.emitJump(.jump_if_false);
        self.emitOpCode(.pop);
        self.statement();
        const else_jump = self.emitJump(.jump);

        self.patchJump(then_jump);
        self.emitOpCode(.pop);

        if (self.match(.keyword_else)) {
            self.statement();
        }
        self.patchJump(else_jump);
    }

    fn emitJump(self: *Parser, instruction: Chunk.OpCode) usize {
        self.emitCodeArg(instruction, std.math.maxInt(u24));
        return self.currentCode() - 1;
    }

    fn patchJump(self: *Parser, offset: usize) void {
        const jump = self.currentCode() - offset - 1;
        if (jump > std.math.maxInt(u16)) {
            self.errorAtPrevious("Too much code to jump over", .{});
        }
        const instruction: u8 = @truncate(self.chunk.code.items[offset]);
        self.chunk.code.items[offset] = @as(u32, @intCast(instruction)) | (@as(u32, @intCast(jump)) << 8);
    }

    fn loopStatement(self: *Parser) void {
        const loop_start = self.currentCode();
        self.statement();
        self.emitLoop(loop_start);
    }

    fn whileStatement(self: *Parser) void {
        const loop_start = self.currentCode();
        self.consume(.l_paren, "Expect '(' after 'while'");
        self.expression();
        self.consume(.r_paren, "Expect ')' after condition");

        const exit_jump = self.emitJump(.jump_if_false);
        self.emitOpCode(.pop);
        self.statement();
        self.emitLoop(loop_start);

        self.patchJump(exit_jump);
        self.emitOpCode(.pop);
    }

    // TODO: foreach loop, see https://craftinginterpreters.com/jumping-back-and-forth.html#for-statements
    // for (0..) / for (0..x) / for (obj) / for (obj[0..]) / for (obj, obj2, x..)
    fn forStatement(self: *Parser) void {
        _ = self;
    }

    fn emitLoop(self: *Parser, loop_start: usize) void {
        self.emitOpCode(.loop);
        const offset = self.currentCode() - loop_start + 1;
        if (offset > std.math.maxInt(u24)) {
            self.errorAtPrevious("Loop body too large", .{});
        }
        self.emitCodeArg(.loop, @intCast(offset));
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) void {
        self.advance();
        const prefixRule = getRule(self.previous.tag).prefix orelse {
            self.errorAtPrevious("Expect expression", .{});
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
            self.errorAtPrevious("Invalid assignment target", .{});
        }
    }

    fn consume(self: *Parser, tag: Token.Tag, message: []const u8) void {
        if (self.check(tag)) {
            self.advance();
            return;
        }
        self.errorAtCurrent("{s}", .{message});
    }

    fn emit(self: *Parser, code: u32) void {
        self.chunk.write(code, self.previous.line) catch unreachable;
    }

    fn emitOpCode(self: *Parser, code: Chunk.OpCode) void {
        self.emit(@intFromEnum(code));
    }

    fn emitCodeArg(self: *Parser, code: Chunk.OpCode, arg: u24) void {
        self.emit(@as(u32, @intFromEnum(code)) | (@as(u32, @intCast(arg)) << 8));
    }

    fn makeConstant(self: *Parser, value: Value) u24 {
        return self.chunk.addConstant(value) catch unreachable;
    }

    fn emitConstant(self: *Parser, value: Value) void {
        self.emitCodeArg(.constant, self.makeConstant(value));
    }

    fn number(self: *Parser, _: bool) void {
        const value = self.previous.literal.?.number; // TODO: parse number here?
        self.emitConstant(Value.create(value));
    }

    fn string(self: *Parser, _: bool) void {
        // TODO: could be invalid pointer later (do not use Value.createObject with this allocator!)
        // TODO: very very wrong same for identifierConstant
        const value = String.create(self.allocator, self.previous.literal.?.string) catch unreachable;
        self.emitConstant(value);
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

    fn synchronize(self: *Parser) void {
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
        .bang => parser.emitOpCode(.not),
        .minus => parser.emitOpCode(.negation),
        else => unreachable,
    }
}

fn binary(parser: *Parser, _: bool) void {
    const tag = parser.previous.tag;
    const rule = Parser.getRule(tag);
    parser.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));
    switch (tag) {
        .bang_equal => {
            parser.emitOpCode(.equal);
            parser.emitOpCode(.not);
        },
        .equal_equal => parser.emitOpCode(.equal),
        .l_angle_bracket => parser.emitOpCode(.less),
        .l_angle_bracket_equal => {
            parser.emitOpCode(.greater);
            parser.emitOpCode(.not);
        },
        .r_angle_bracket => parser.emitOpCode(.greater),
        .r_angle_bracket_equal => {
            parser.emitOpCode(.less);
            parser.emitOpCode(.not);
        },
        .plus => parser.emitOpCode(.add),
        .minus => parser.emitOpCode(.sub),
        .asterisk => parser.emitOpCode(.mul),
        .slash => parser.emitOpCode(.div),
        .percent => parser.emitOpCode(.mod),
        else => unreachable,
    }
}

fn literal(parser: *Parser, can_assign: bool) void {
    switch (parser.previous.tag) {
        .number => parser.number(can_assign),
        .bool => {
            const value = parser.previous.literal.?.bool;
            parser.emitConstant(Value.create(value));
        },
        else => unreachable,
    }
}

fn variable(parser: *Parser, can_assign: bool) void {
    namedVariable(parser, parser.previous, can_assign);
}

fn namedVariable(parser: *Parser, name: Token, can_assign: bool) void {
    var get_op: Chunk.OpCode = .get_local;
    var set_op: Chunk.OpCode = .set_local;
    const arg = resolveLocal(current, name) orelse blk: {
        get_op = .get_global;
        set_op = .set_global;
        break :blk parser.identifierConstant(name);
    };

    if (can_assign) {
        if (parser.match(.equal)) {
            parser.expression();
            parser.emitCodeArg(set_op, arg);
            return;
        } else if (parser.match(.plus_equal)) {
            parser.emitCodeArg(get_op, arg);
            parser.expression();
            parser.emitOpCode(.add);
            parser.emitCodeArg(set_op, arg);
            return;
        } else if (parser.match(.minus_equal)) {
            parser.emitCodeArg(get_op, arg);
            parser.expression();
            parser.emitOpCode(.sub);
            parser.emitCodeArg(set_op, arg);
            return;
        } else if (parser.match(.asterisk_equal)) {
            parser.emitCodeArg(get_op, arg);
            parser.expression();
            parser.emitOpCode(.mul);
            parser.emitCodeArg(set_op, arg);
            return;
        } else if (parser.match(.slash_equal)) {
            parser.emitCodeArg(get_op, arg);
            parser.expression();
            parser.emitOpCode(.div);
            parser.emitCodeArg(set_op, arg);
            return;
        } else if (parser.match(.percent_equal)) {
            parser.emitCodeArg(get_op, arg);
            parser.expression();
            parser.emitOpCode(.mod);
            parser.emitCodeArg(set_op, arg);
            return;
        }
    }

    parser.emitCodeArg(get_op, arg);
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
    const end_jump = parser.emitJump(.jump_if_false);
    parser.emitOpCode(.pop);
    parser.parsePrecedence(.@"and");
    parser.patchJump(end_jump);
}

fn @"or"(parser: *Parser, _: bool) void {
    const else_jump = parser.emitJump(.jump_if_false);
    const end_jump = parser.emitJump(.jump);
    parser.patchJump(else_jump);
    parser.emitOpCode(.pop);
    parser.parsePrecedence(.@"or");
    parser.patchJump(end_jump);
}
