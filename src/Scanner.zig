const std = @import("std");
const err = @import("main.zig").err;

const Scanner = @This();

buffer: [:0]const u8,
index: usize,
line: usize,

pub const Token = struct {
    tag: Tag,
    literal: ?Literal,
    line: usize, // TODO: add column (check std.zig.Token.Loc)

    pub const Tag = enum {
        // add %, ^, &, |, ~, <<, >>, %=, ^=, &=, |=, ~=, <<=, >>=?
        l_paren,
        r_paren,
        l_brace,
        r_brace,
        comma,
        period,
        minus,
        plus,
        slash,
        asterisk,
        bang,
        bang_equal,
        equal,
        equal_equal,
        l_angle_bracket,
        r_angle_bracket,
        l_angle_bracket_equal,
        r_angle_bracket_equal,
        plus_equal,
        minus_equal,
        asterisk_equal,
        slash_equal,
        keyword_and,
        keyword_or,
        keyword_else,
        // keyword_false,
        // keyword_true,
        keyword_for,
        keyword_if,
        keyword_print,
        keyword_return,
        keyword_let,
        keyword_while,
        // keyword_nil,
        // keyword_mut,
        // keyword_define,
        // keyword_const,
        identifier,
        string,
        number,
        // int,
        // float,
        eof,
        invalid,

        pub fn lexeme(tag: Tag) ?[]const u8 {
            return switch (tag) {
                .eof,
                .invalid,
                .identifier,
                .string,
                .number,
                => null,

                .l_paren => "(",
                .r_paren => ")",
                .l_brace => "{",
                .r_brace => "}",
                .comma => ",",
                .period => ".",
                .minus => "-",
                .plus => "+",
                .slash => "/",
                .asterisk => "*",
                .bang => "!",
                .bang_equal => "!=",
                .equal => "=",
                .equal_equal => "==",
                .l_angle_bracket => "<",
                .r_angle_bracket => ">",
                .l_angle_bracket_equal => "<=",
                .r_angle_bracket_equal => ">=",
                .plus_equal => "+=",
                .minus_equal => "-=",
                .asterisk_equal => "*=",
                .slash_equal => "/=",
                .keyword_and => "and",
                .keyword_or => "or",
                .keyword_else => "else",
                // .keyword_false => "false",
                // .keyword_true => "true",
                .keyword_for => "for",
                .keyword_if => "if",
                .keyword_print => "print",
                .keyword_return => "return",
                .keyword_let => "let",
                .keyword_while => "while",
            };
        }
    };

    pub const Literal = union {
        identifier: []const u8,
        string: []const u8,
        number: f64,
    };

    pub const keywords = std.StaticStringMap(Tag).initComptime(.{
        .{ "and", .keyword_and },
        .{ "or", .keyword_or },
        .{ "else", .keyword_else },
        // .{ "false", .keyword_false },
        // .{ "true", .keyword_true },
        .{ "for", .keyword_for },
        .{ "if", .keyword_if },
        .{ "print", .keyword_print },
        .{ "return", .keyword_return },
        .{ "let", .keyword_let },
        .{ "while", .keyword_while },
    });

    pub fn getKeyword(bytes: []const u8) ?Tag {
        return keywords.get(bytes);
    }

    pub fn format(
        self: Token,
        comptime fmt: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        if (fmt.len != 0) {
            std.fmt.invalidFmtError(fmt, self);
        }
        try writer.print("{d}: {}: ", .{ self.line, self.tag });
        if (self.literal) |literal| {
            switch (self.tag) {
                .identifier => try writer.writeAll(literal.identifier),
                .string => try writer.writeAll(literal.string),
                .number => try writer.print("{d}", .{literal.number}),
                else => unreachable,
            }
        } else {
            try writer.print("{?s}", .{self.tag.lexeme()});
        }
    }
};

pub fn init(buffer: [:0]const u8) Scanner {
    return .{
        .buffer = buffer,
        .index = 0,
        .line = 1,
    };
}

pub fn next(self: *Scanner) ?Token {
    while (true) {
        const c = self.buffer[self.index];
        switch (c) {
            0 => {
                if (self.index != self.buffer.len) {
                    err(self.line, "Unexpected character: '\\0'", .{});
                    self.index += 1;
                    return self.token(.invalid);
                }
                // return self.token(.eof);
                return null;
            },
            ' ', '\r', '\t' => self.index += 1,
            '\n' => {
                self.index += 1;
                self.line += 1;
            },
            ';', '#' => {
                const eoc = std.mem.indexOfScalar(u8, self.buffer[self.index..], '\n') orelse {
                    self.index = self.buffer.len;
                    return null;
                };
                self.index += eoc;
            },
            '(' => return self.token(.l_paren),
            ')' => return self.token(.r_paren),
            '{' => return self.token(.l_brace),
            '}' => return self.token(.r_brace),
            ',' => return self.token(.comma),
            '.' => return self.token(.period),
            '-' => return self.tokenWithEqual(.minus),
            '+' => return self.tokenWithEqual(.plus),
            '/' => return self.tokenWithEqual(.slash),
            '*' => return self.tokenWithEqual(.asterisk),
            '!' => return self.tokenWithEqual(.bang),
            '=' => return self.tokenWithEqual(.equal),
            '<' => return self.tokenWithEqual(.l_angle_bracket),
            '>' => return self.tokenWithEqual(.r_angle_bracket),
            '"' => return self.string(),
            '0'...'9' => return self.number(),
            'A'...'Z', 'a'...'z', '_' => return self.identifier(),
            else => {
                err(self.line, "Unexpected character: {c}", .{c});
                self.index += 1;
                return self.token(.invalid);
            },
        }
    }
}

inline fn token(self: *Scanner, comptime tag: Token.Tag) Token {
    self.index += comptime if (tag.lexeme()) |lexeme| lexeme.len else 0;
    return .{
        .tag = tag,
        .literal = null,
        .line = self.line,
    };
}

inline fn tokenWithEqual(self: *Scanner, comptime tag: Token.Tag) Token {
    if (self.buffer[self.index + 1] == '=') {
        @setEvalBranchQuota(100_000);
        const tag_equal = comptime std.meta.stringToEnum(Token.Tag, @tagName(tag) ++ "_equal").?;
        return self.token(tag_equal);
    } else {
        return self.token(tag);
    }
}

fn string(self: *Scanner) Token {
    std.debug.assert(self.buffer[self.index] == '"');
    self.index += 1;
    const start = self.index;
    const end = std.mem.indexOfScalar(u8, self.buffer[self.index..], '"') orelse {
        err(self.line, "Unterminated string", .{});
        self.index = self.buffer.len;
        return self.token(.invalid);
    };
    self.index += end + 1;
    std.debug.assert(self.buffer[self.index - 1] == '"');
    const bytes = self.buffer[start .. start + end];

    if (std.mem.indexOfScalar(u8, bytes, '\n')) |_| {
        err(self.line, "Multiline string", .{});
        return self.token(.invalid);
    }

    return .{
        .tag = .string,
        .literal = .{ .string = bytes },
        .line = self.line,
    };
}

fn number(self: *Scanner) Token {
    std.debug.assert(std.ascii.isDigit(self.buffer[self.index]));
    const start = self.index;
    self.index += 1;
    while (std.ascii.isDigit(self.buffer[self.index])) {
        self.index += 1;
    }

    if (self.buffer[self.index] == '.' and std.ascii.isDigit(self.buffer[self.index + 1])) {
        self.index += 2;
        while (std.ascii.isDigit(self.buffer[self.index])) {
            self.index += 1;
        }
    } else {
        // unsigned integer
    }

    const end = self.index;
    const value = std.fmt.parseFloat(f64, self.buffer[start..end]) catch unreachable;
    return .{
        .tag = .number,
        .literal = .{ .number = value },
        .line = self.line,
    };
}

fn identifier(self: *Scanner) Token {
    const start = self.index;
    self.index += 1;
    while (true) {
        switch (self.buffer[self.index]) {
            'A'...'Z', 'a'...'z', '0'...'9', '_' => self.index += 1,
            else => break,
        }
    }
    const end = self.index;
    const bytes = self.buffer[start..end];
    const tag = Token.getKeyword(bytes) orelse .identifier;
    return .{
        .tag = tag,
        .literal = if (tag == .identifier) .{ .identifier = bytes } else null,
        .line = self.line,
    };
}

// pub fn scanTokens(self: *Scanner) !void {
//     while (!self.isAtEnd()) {
//         self.start = self.current;
//         try self.scanToken();
//     }
//     try self.tokens.append(.{
//         .tag = .eof,
//         .lexeme = "",
//         .literal = null,
//         .line = self.line,
//     });
// }

// fn isAtEnd(self: *const Scanner) bool {
//     return self.index >= self.source.len;
// }

// fn scanToken(self: *Scanner) !void {
//     const c = self.advance();
//     switch (c) {
//         '(' => try self.addToken(.@"(", null),
//         ')' => try self.addToken(.@")", null),
//         '{' => try self.addToken(.@"{", null),
//         '}' => try self.addToken(.@"}", null),
//         ',' => try self.addToken(.@",", null),
//         '.' => try self.addToken(.@".", null),
//         '-' => try self.addToken(.@"-", null),
//         '+' => try self.addToken(.@"+", null),
//         ';' => try self.addToken(.@";", null),
//         '*' => try self.addToken(.@"*", null),
//         '!' => try self.addToken(if (self.match('=')) .@"!=" else .@"!", null),
//         '=' => try self.addToken(if (self.match('=')) .@"==" else .@"=", null),
//         '<' => try self.addToken(if (self.match('=')) .@"<=" else .@"<", null),
//         '>' => try self.addToken(if (self.match('=')) .@">=" else .@">", null),
//         '/' => {
//             if (self.match('/')) {
//                 while (self.peek() != '\n' and !self.isAtEnd()) {
//                     _ = self.advance();
//                 }
//             } else {
//                 try self.addToken(.@"/", null);
//             }
//         },
//         ' ', '\r', '\t' => {},
//         '\n' => self.line += 1,
//         '"' => try self.string(),
//         '0'...'9' => try self.number(),
//         'A'...'Z', 'a'...'z', '_' => try self.identifier(),
//         else => err(self.line, "Unexpected character."),
//     }
// }

// fn advance(self: *Scanner) u8 {
//     const c = self.source[self.current];
//     self.current += 1;
//     return c;
// }

// fn addToken(self: *Scanner, tag: Token.Tag, literal: ?Token.Literal) !void {
//     const text = self.source[self.start..self.current];
//     try self.tokens.append(.{
//         .tag = tag,
//         .lexeme = text,
//         .literal = literal,
//         .line = self.line,
//     });
// }

// fn match(self: *Scanner, expected: u8) bool {
//     if (self.buffer[self.index] != expected) {
//         return false;
//     }
//     self.index += 1;
//     return true;
// }

// fn peek(self: *const Scanner) u8 {
//     if (self.isAtEnd()) {
//         return '\x00';
//     }
//     return self.source[self.current];
// }

// fn peekNext(self: *const Scanner) u8 {
//     if (self.current + 1 >= self.source.len) {
//         return '\x00';
//     }
//     return self.source[self.current + 1];
// }

// fn string(self: *Scanner) !void {
//     while (self.peek() != '"' and !self.isAtEnd()) {
//         if (self.peek() == '\n') {
//             self.line += 1;
//         }
//         _ = self.advance();
//     }

//     if (self.isAtEnd()) {
//         err(self.line, "Unterminated string.");
//         return;
//     }

//     _ = self.advance();

//     const value = self.source[self.start + 1 .. self.current - 1];
//     try self.addToken(.string, .{ .string = value });
// }

// fn number(self: *Scanner) !void {
//     while (std.ascii.isDigit(self.peek())) {
//         _ = self.advance();
//     }

//     if (self.peek() == '.' and std.ascii.isDigit(self.peekNext())) {
//         _ = self.advance();
//         while (std.ascii.isDigit(self.peek())) {
//             _ = self.advance();
//         }
//     }

//     const value = try std.fmt.parseFloat(f64, self.source[self.start..self.current]);
//     try self.addToken(.number, .{ .number = value });
// }

// fn identifier(self: *Scanner) !void {
//     while (true) {
//         switch (self.peek()) {
//             'A'...'Z', 'a'...'z', '0'...'9', '_' => _ = self.advance(),
//             else => break,
//         }
//     }

//     const text = self.source[self.start..self.current];
//     const tag = Token.getKeyword(text) orelse .identifier;
//     try self.addToken(tag, null);
// }
