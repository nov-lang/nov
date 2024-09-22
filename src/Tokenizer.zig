const std = @import("std");

const Tokenizer = @This();

buffer: [:0]const u8,
index: usize,

pub const Token = struct {
    tag: Tag,
    loc: Loc,

    pub const Loc = struct {
        start: usize,
        end: usize,
    };

    // must be the same order as compiler.Parser.rules
    pub const Tag = enum {
        // add ^, &, |, ~, <<, >>, %=, ^=, &=, |=, ~=, <<=, >>=?
        // add ' for char literals
        l_paren,
        r_paren,
        l_brace,
        r_brace,
        comma,
        period,
        colon,
        minus,
        plus,
        asterisk,
        slash,
        percent,
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
        percent_equal,
        keyword_and,
        keyword_or,
        keyword_else,
        keyword_if,
        keyword_match,
        keyword_print,
        keyword_return,
        keyword_let,
        keyword_mut,
        keyword_loop,
        keyword_while, // TODO: remove?
        keyword_for,
        keyword_break,
        keyword_continue,
        keyword_true,
        keyword_false,
        keyword_bool,
        keyword_number, // TODO: replace with int/float
        // keyword_int,
        // keyword_float,
        keyword_str,
        // keyword_char,
        // keyword_nil,
        // keyword_void,
        identifier,
        string_literal,
        number_literal,
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
                .bool,
                => null,

                .l_paren => "(",
                .r_paren => ")",
                .l_brace => "{",
                .r_brace => "}",
                .comma => ",",
                .period => ".",
                .colon => ":",
                .minus => "-",
                .plus => "+",
                .asterisk => "*",
                .slash => "/",
                .percent => "%",
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
                .percent_equal => "%=",
                .keyword_and => "and",
                .keyword_or => "or",
                .keyword_else => "else",
                .keyword_if => "if",
                .keyword_match => "match",
                .keyword_print => "print",
                .keyword_return => "return",
                .keyword_let => "let",
                .keyword_mut => "mut",
                .keyword_loop => "loop",
                .keyword_while => "while",
                .keyword_for => "for",
                .keyword_break => "break",
                .keyword_continue => "continue",
            };
        }

        pub fn symbol(tag: Tag) []const u8 {
            return tag.lexeme() orelse switch (tag) {
                else => unreachable, // TODO
            };
        }
    };

    pub const keywords = blk: {
        const values = std.enums.values(Tag);
        var kvs: std.BoundedArray(struct { []const u8, Tag }, values.len) = .{};
        for (values) |tag| {
            if (std.mem.startsWith(u8, @tagName(tag), "keyword_")) {
                kvs.append(.{ tag.lexeme().?, tag }) catch unreachable;
            }
        }
        break :blk std.StaticStringMap(Tag).initComptime(kvs.constSlice());
    };

    pub fn getKeyword(bytes: []const u8) ?Tag {
        return keywords.get(bytes);
    }

    // pub fn format(
    //     self: Token,
    //     comptime fmt: []const u8,
    //     _: std.fmt.FormatOptions,
    //     writer: anytype,
    // ) @TypeOf(writer).Error!void {
    //     if (fmt.len != 0) {
    //         std.fmt.invalidFmtError(fmt, self);
    //     }
    //     try writer.print("{d}:{d}: {}: ", .{ self.line, self.col, self.tag });
    //     if (self.literal) |literal| {
    //         switch (self.tag) {
    //             .string, .identifier => try writer.writeAll(literal.string),
    //             .number => try writer.print("{d}", .{literal.number}),
    //             .bool => try writer.print("{}", .{literal.bool}),
    //             else => unreachable,
    //         }
    //     } else {
    //         try writer.print("{?s}", .{self.tag.lexeme()});
    //     }
    // }
};

pub fn init(buffer: [:0]const u8) Tokenizer {
    return .{
        .buffer = buffer,
        .index = 0,
        .line = 1,
        .col = 1,
    };
}

// TODO: check std.zig.Tokenizer
pub fn next(self: *Tokenizer) Token {
    while (true) {
        const c = self.buffer[self.index];
        switch (c) {
            0 => {
                if (self.index != self.buffer.len) {
                    defer self.advance();
                    return .{
                        .tag = .invalid,
                        .literal = .{ .string = "Unexpected character" },
                        .line = self.line,
                        .col = self.col,
                    };
                }
                return self.token(.eof);
            },
            ' ', '\r', '\t' => self.advance(),
            '\n' => {
                self.index += 1;
                self.line += 1;
                self.col = 1;
            },
            ';', '#' => {
                const eoc = std.mem.indexOfScalar(u8, self.buffer[self.index..], '\n') orelse {
                    self.index = self.buffer.len;
                    return self.token(.eof); // invalid?
                };
                self.index += eoc;
                // self.col will be reset by next iteration
            },
            '(' => return self.token(.l_paren),
            ')' => return self.token(.r_paren),
            '{' => return self.token(.l_brace),
            '}' => return self.token(.r_brace),
            ',' => return self.token(.comma),
            '.' => return self.token(.period),
            ':' => return self.token(.colon),
            '-' => return self.tokenWithEqual(.minus),
            '+' => return self.tokenWithEqual(.plus),
            '*' => return self.tokenWithEqual(.asterisk),
            '/' => return self.tokenWithEqual(.slash),
            '%' => return self.tokenWithEqual(.percent),
            '!' => return self.tokenWithEqual(.bang),
            '=' => return self.tokenWithEqual(.equal),
            '<' => return self.tokenWithEqual(.l_angle_bracket),
            '>' => return self.tokenWithEqual(.r_angle_bracket),
            '"' => return self.string(),
            '0'...'9' => return self.number(),
            'A'...'Z', 'a'...'z', '_' => return self.identifier(),
            else => {
                defer self.advance();
                return .{
                    .tag = .invalid,
                    .literal = .{ .string = "Unexpected character" },
                    .line = self.line,
                    .col = self.col,
                };
            },
        }
    }
}

inline fn advance(self: *Tokenizer) void {
    self.index += 1;
    self.col += 1;
}

inline fn token(self: *Tokenizer, comptime tag: Token.Tag) Token {
    const len = comptime if (tag.lexeme()) |lexeme| lexeme.len else 0;
    self.index += len;
    defer self.col += len;
    return .{
        .tag = tag,
        .literal = null,
        .line = self.line,
        .col = self.col,
    };
}

inline fn tokenWithEqual(self: *Tokenizer, comptime tag: Token.Tag) Token {
    if (self.buffer[self.index + 1] == '=') {
        @setEvalBranchQuota(100_000);
        const tag_equal = comptime std.meta.stringToEnum(Token.Tag, @tagName(tag) ++ "_equal").?;
        return self.token(tag_equal);
    } else {
        return self.token(tag);
    }
}

// TODO: escape sequences
// TODO: multiline strings
// TODO: string interpolation?
fn string(self: *Tokenizer) Token {
    std.debug.assert(self.buffer[self.index] == '"');
    const col = self.col;
    self.advance();
    const start = self.index;
    const end = std.mem.indexOfScalar(u8, self.buffer[self.index..], '"') orelse {
        self.index = self.buffer.len;
        return .{
            .tag = .invalid,
            .literal = .{ .string = "Unterminated string" },
            .line = self.line,
            .col = col,
        };
    };
    self.index += end + 1;
    self.col += end + 1;
    std.debug.assert(self.buffer[self.index - 1] == '"');
    const bytes = self.buffer[start .. start + end];

    if (std.mem.indexOfScalar(u8, bytes, '\n')) |i| {
        self.col = bytes.len - i; // TODO
        return .{
            .tag = .invalid,
            .literal = .{ .string = "Multiline string" },
            .line = self.line,
            .col = col,
        };
    }

    return .{
        .tag = .string,
        .literal = .{ .string = bytes },
        .line = self.line,
        .col = col,
    };
}

fn number(self: *Tokenizer) Token {
    std.debug.assert(std.ascii.isDigit(self.buffer[self.index]));
    const col = self.col;
    const start = self.index;
    self.advance();
    while (std.ascii.isDigit(self.buffer[self.index])) {
        self.advance();
    }

    if (self.buffer[self.index] == '.' and std.ascii.isDigit(self.buffer[self.index + 1])) {
        self.index += 2;
        self.col += 2;
        while (std.ascii.isDigit(self.buffer[self.index])) {
            self.advance();
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
        .col = col,
    };
}

fn identifier(self: *Tokenizer) Token {
    const col = self.col;
    const start = self.index;
    self.advance();
    while (true) {
        switch (self.buffer[self.index]) {
            'A'...'Z', 'a'...'z', '0'...'9', '_' => self.advance(),
            else => break,
        }
    }
    const end = self.index;
    const bytes = self.buffer[start..end];

    const tag = Token.getKeyword(bytes) orelse .identifier;
    return .{
        .tag = tag,
        .literal = if (tag == .identifier) .{ .string = bytes } else null,
        .line = self.line,
        .col = col,
    };
}
