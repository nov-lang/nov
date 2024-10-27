// Originally based on https://github.com/ziglang/zig/blob/master/lib/std/zig/tokenizer.zig
// See https://github.com/ziglang/zig/blob/master/LICENSE for additional LICENSE details

const std = @import("std");

const Tokenizer = @This();

buffer: [:0]const u8,
index: ByteOffset,

pub const ByteOffset = u32;

pub const Token = struct {
    tag: Tag,
    start: ByteOffset,
    end: ByteOffset,

    pub const Tag = enum {
        eof,
        invalid,
        identifier,
        builtin,
        string_literal,
        char_literal,
        number_literal,
        doc_comment,
        newline,
        l_paren,
        r_paren,
        l_bracket,
        r_bracket,
        l_brace,
        r_brace,
        at_sign_l_bracket,
        octothorpe,
        comma,
        period,
        ellipsis2,
        ellipsis3,
        colon,
        question_mark,
        bang,
        bang_equal,
        equal,
        equal_equal,
        equal_arrow,
        l_angle_bracket,
        l_angle_bracket_equal,
        l_angle_bracket_angle_bracket,
        r_angle_bracket,
        r_angle_bracket_equal,
        r_angle_bracket_angle_bracket,
        r_angle_bracket_angle_bracket_equal,
        plus,
        plus_equal,
        minus,
        minus_equal,
        minus_caret,
        arrow,
        asterisk,
        asterisk_equal,
        slash,
        slash_equal,
        percent,
        percent_equal,
        pipe,
        pipe_arrow,
        caret,
        ampersand,
        tilde,
        keyword_let,
        keyword_match,
        keyword_loop,
        keyword_if,
        keyword_else,
        keyword_and,
        keyword_or,
        keyword_mut,
        keyword_return,
        keyword_break,
        keyword_continue,
        keyword_in,
        keyword_is,
        keyword_defer,
        keyword_enum,
        keyword_struct,
        keyword_union,
        // unused reserved keywords
        keyword_async,
        keyword_await,
        keyword_yield,
        keyword_resume,
        keyword_suspend,
        keyword_nosuspend,
        keyword_opaque,

        pub fn lexeme(tag: Tag) ?[]const u8 {
            return switch (tag) {
                .eof,
                .invalid,
                .identifier,
                .builtin,
                .string_literal,
                .char_literal,
                .number_literal,
                .doc_comment,
                => null,

                .newline => "\n",
                .l_paren => "(",
                .r_paren => ")",
                .l_bracket => "[",
                .r_bracket => "]",
                .l_brace => "{",
                .r_brace => "}",
                .at_sign_l_bracket => "@[",
                .octothorpe => "#",
                .comma => ",",
                .period => ".",
                .ellipsis2 => "..",
                .ellipsis3 => "...",
                .colon => ":",
                .question_mark => "?",
                .bang => "!",
                .bang_equal => "!=",
                .equal => "=",
                .equal_equal => "==",
                .equal_arrow => "=>",
                .l_angle_bracket => "<",
                .l_angle_bracket_equal => "<=",
                .l_angle_bracket_angle_bracket => "<<",
                .r_angle_bracket => ">",
                .r_angle_bracket_equal => ">=",
                .r_angle_bracket_angle_bracket => ">>",
                .r_angle_bracket_angle_bracket_equal => ">>=",
                .plus => "+",
                .plus_equal => "+=",
                .minus => "-",
                .minus_equal => "-=",
                .minus_caret => "-^",
                .arrow => "->",
                .asterisk => "*",
                .asterisk_equal => "*=",
                .slash => "/",
                .slash_equal => "/=",
                .percent => "%",
                .percent_equal => "%=",
                .pipe => "|",
                .pipe_arrow => "|>",
                .caret => "^",
                .ampersand => "&",
                .tilde => "~",
                .keyword_and => "and",
                .keyword_or => "or",
                .keyword_if => "if",
                .keyword_else => "else",
                .keyword_match => "match",
                .keyword_return => "return",
                .keyword_let => "let",
                .keyword_mut => "mut",
                .keyword_loop => "loop",
                .keyword_break => "break",
                .keyword_continue => "continue",
                .keyword_in => "in",
                .keyword_async => "async",
                .keyword_await => "await",
                .keyword_yield => "yield",
                .keyword_resume => "resume",
                .keyword_suspend => "suspend",
                .keyword_nosuspend => "nosuspend",
                .keyword_enum => "enum",
                .keyword_struct => "struct",
                .keyword_union => "union",
                .keyword_defer => "defer",
                .keyword_is => "is",
                .keyword_opaque => "opaque",
            };
        }

        pub fn symbol(tag: Tag) []const u8 {
            if (tag == .newline) {
                return "a new line";
            }

            return tag.lexeme() orelse switch (tag) {
                .eof => "EOF",
                .invalid => "invalid bytes",
                .identifier => "an identifier",
                .builtin => "a builtin function",
                .string_literal => "a string literal",
                .char_literal => "a character literal",
                .number_literal => "a number literal",
                .doc_comment => "a document comment",
                else => unreachable,
            };
        }
    };

    pub const keywords = blk: {
        const values = std.enums.values(Tag);
        var kvs: std.BoundedArray(struct { []const u8, Tag }, values.len) = .{};
        @setEvalBranchQuota(10_000);
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
};

pub fn init(buffer: [:0]const u8) Tokenizer {
    var src_start: ByteOffset = 0;
    // Skip the UTF-8 BOM if present
    if (std.mem.startsWith(u8, buffer, "\xEF\xBB\xBF")) {
        src_start = 3;
    }
    // Skip the shebang line if present
    if (std.mem.startsWith(u8, buffer[src_start..], "#!")) {
        if (std.mem.indexOfScalarPos(u8, buffer, src_start + 2, '\n')) |nl| {
            src_start = @intCast(nl + 1);
        } else {
            src_start = @intCast(buffer.len);
        }
    }

    return .{
        .buffer = buffer,
        .index = src_start,
    };
}

const State = enum {
    start,
    identifier,
    builtin,
    at_sign,
    string_literal,
    string_literal_backslash,
    char_literal,
    char_literal_backslash,
    line_comment_1,
    line_comment_2,
    line_comment_3,
    line_comment,
    doc_comment,
    equal,
    bang,
    pipe,
    percent,
    asterisk,
    plus,
    minus,
    slash,
    l_angle_bracket,
    r_angle_bracket,
    r_angle_bracket_angle_bracket,
    period,
    period_2,
    int,
    int_period,
    int_exponent,
    float,
    float_exponent,
    invalid,
};

pub fn next(self: *Tokenizer) Token {
    var result: Token = .{
        .tag = undefined,
        .start = self.index,
        .end = undefined,
    };
    state: switch (State.start) {
        .start => switch (self.buffer[self.index]) {
            0 => {
                if (self.index == self.buffer.len) {
                    return .{
                        .tag = .eof,
                        .start = self.index,
                        .end = self.index,
                    };
                } else {
                    continue :state .invalid;
                }
            },
            '\n' => {
                result.tag = .newline;
                self.index += 1;
            },
            '(' => {
                result.tag = .l_paren;
                self.index += 1;
            },
            ')' => {
                result.tag = .r_paren;
                self.index += 1;
            },
            '[' => {
                result.tag = .l_bracket;
                self.index += 1;
            },
            ']' => {
                result.tag = .r_bracket;
                self.index += 1;
            },
            '{' => {
                result.tag = .l_brace;
                self.index += 1;
            },
            '}' => {
                result.tag = .r_brace;
                self.index += 1;
            },
            '#' => {
                result.tag = .octothorpe;
                self.index += 1;
            },
            ',' => {
                result.tag = .comma;
                self.index += 1;
            },
            ':' => {
                result.tag = .colon;
                self.index += 1;
            },
            '~' => {
                result.tag = .tilde;
                self.index += 1;
            },
            '&' => {
                result.tag = .ampersand;
                self.index += 1;
            },
            '^' => {
                result.tag = .caret;
                self.index += 1;
            },
            '?' => {
                result.tag = .question_mark;
                self.index += 1;
            },
            ' ', '\r', '\t' => {
                self.index += 1;
                result.start = self.index;
                continue :state .start;
            },
            '"' => {
                result.tag = .string_literal;
                continue :state .string_literal;
            },
            '\'' => {
                result.tag = .char_literal;
                continue :state .char_literal;
            },
            'A'...'Z', 'a'...'z', '_' => {
                result.tag = .identifier;
                continue :state .identifier;
            },
            '0'...'9' => {
                result.tag = .number_literal;
                self.index += 1;
                continue :state .int;
            },
            ';' => continue :state .line_comment_1,
            '@' => continue :state .at_sign,
            '=' => continue :state .equal,
            '!' => continue :state .bang,
            '|' => continue :state .pipe,
            '%' => continue :state .percent,
            '*' => continue :state .asterisk,
            '+' => continue :state .plus,
            '-' => continue :state .minus,
            '/' => continue :state .slash,
            '<' => continue :state .l_angle_bracket,
            '>' => continue :state .r_angle_bracket,
            '.' => continue :state .period,
            else => continue :state .invalid,
        },
        .asterisk => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                '=' => {
                    result.tag = .asterisk_equal;
                    self.index += 1;
                },
                else => result.tag = .asterisk,
            }
        },
        .percent => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                '=' => {
                    result.tag = .percent_equal;
                    self.index += 1;
                },
                else => result.tag = .percent,
            }
        },
        .plus => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                '=' => {
                    result.tag = .plus_equal;
                    self.index += 1;
                },
                else => result.tag = .plus,
            }
        },
        .at_sign => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                0, '\n' => result.tag = .invalid,
                '[' => {
                    result.tag = .at_sign_l_bracket;
                    self.index += 1;
                },
                '"' => {
                    result.tag = .identifier;
                    continue :state .string_literal;
                },
                'A'...'Z', 'a'...'z', '_' => {
                    result.tag = .builtin;
                    continue :state .builtin;
                },
                else => continue :state .invalid,
            }
        },
        .builtin => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                'A'...'Z', 'a'...'z', '0'...'9', '_' => continue :state .builtin,
                else => {},
            }
        },
        .identifier => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                'A'...'Z', 'a'...'z', '0'...'9', '_' => continue :state .identifier,
                else => {
                    const ident = self.buffer[result.start..self.index];
                    if (Token.getKeyword(ident)) |tag| {
                        result.tag = tag;
                    }
                },
            }
        },
        .string_literal => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                0 => if (self.index == self.buffer.len) {
                    result.tag = .invalid;
                } else {
                    continue :state .invalid;
                },
                '"' => self.index += 1,
                '\n' => result.tag = .invalid,
                '\\' => continue :state .string_literal_backslash,
                else => continue :state .string_literal,
            }
        },
        .string_literal_backslash => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                0, '\n' => result.tag = .invalid,
                else => continue :state .string_literal,
            }
        },
        .char_literal => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                0 => {
                    if (self.index == self.buffer.len) {
                        result.tag = .invalid;
                    } else {
                        continue :state .invalid;
                    }
                },
                '\'' => self.index += 1,
                '\n' => result.tag = .invalid,
                '\\' => continue :state .char_literal_backslash,
                else => continue :state .char_literal,
            }
        },
        .char_literal_backslash => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                0, '\n' => result.tag = .invalid,
                else => continue :state .char_literal,
            }
        },
        .bang => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                '=' => {
                    result.tag = .bang_equal;
                    self.index += 1;
                },
                else => result.tag = .bang,
            }
        },
        .pipe => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                '>' => {
                    result.tag = .pipe_arrow;
                    self.index += 1;
                },
                else => result.tag = .pipe,
            }
        },
        .equal => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                '=' => {
                    result.tag = .equal_equal;
                    self.index += 1;
                },
                '>' => {
                    result.tag = .equal_arrow;
                    self.index += 1;
                },
                else => result.tag = .equal,
            }
        },
        .minus => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                '=' => {
                    result.tag = .minus_equal;
                    self.index += 1;
                },
                '>' => {
                    result.tag = .arrow;
                    self.index += 1;
                },
                '^' => {
                    result.tag = .minus_caret;
                    self.index += 1;
                },
                else => result.tag = .minus,
            }
        },
        .l_angle_bracket => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                '<' => {
                    result.tag = .l_angle_bracket_angle_bracket;
                    self.index += 1;
                },
                '=' => {
                    result.tag = .l_angle_bracket_equal;
                    self.index += 1;
                },
                else => result.tag = .l_angle_bracket,
            }
        },
        .r_angle_bracket => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                '>' => continue :state .r_angle_bracket_angle_bracket,
                '=' => {
                    result.tag = .r_angle_bracket_equal;
                    self.index += 1;
                },
                else => result.tag = .r_angle_bracket,
            }
        },
        .r_angle_bracket_angle_bracket => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                '=' => {
                    result.tag = .r_angle_bracket_angle_bracket_equal;
                    self.index += 1;
                },
                else => result.tag = .r_angle_bracket_angle_bracket,
            }
        },
        .period => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                '.' => continue :state .period_2,
                else => result.tag = .period,
            }
        },
        .period_2 => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                '.' => {
                    result.tag = .ellipsis3;
                    self.index += 1;
                },
                else => result.tag = .ellipsis2,
            }
        },
        .slash => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                '=' => {
                    result.tag = .slash_equal;
                    self.index += 1;
                },
                else => result.tag = .slash,
            }
        },
        .line_comment_1 => switch (self.buffer[self.index + 1]) {
            ';' => {
                self.index += 1;
                continue :state .line_comment_2;
            },
            else => continue :state .line_comment,
        },
        .line_comment_2 => switch (self.buffer[self.index + 1]) {
            ';' => {
                self.index += 1;
                continue :state .line_comment_3;
            },
            else => continue :state .line_comment,
        },
        .line_comment_3 => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                ';' => continue :state .line_comment,
                0, '\n' => result.tag = .doc_comment,
                else => {
                    result.tag = .doc_comment;
                    continue :state .doc_comment;
                },
            }
        },
        .line_comment => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                0 => {
                    if (self.index == self.buffer.len) {
                        return .{
                            .tag = .eof,
                            .start = self.index,
                            .end = self.index,
                        };
                    } else {
                        continue :state .invalid;
                    }
                },
                '\n' => {
                    result.tag = .newline;
                    result.start = self.index;
                    self.index += 1;
                },
                else => continue :state .line_comment,
            }
        },
        .doc_comment => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                0, '\n' => {}, // return
                else => continue :state .doc_comment,
            }
        },
        .int => switch (self.buffer[self.index]) {
            '.' => continue :state .int_period,
            '_', 'a'...'d', 'f'...'o', 'q'...'z', 'A'...'D', 'F'...'O', 'Q'...'Z', '0'...'9' => {
                self.index += 1;
                continue :state .int;
            },
            'e', 'E', 'p', 'P' => continue :state .int_exponent,
            else => {}, // return
        },
        .int_exponent => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                '-', '+' => {
                    self.index += 1;
                    continue :state .float;
                },
                else => continue :state .int,
            }
        },
        .int_period => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                '_', 'a'...'d', 'f'...'o', 'q'...'z', 'A'...'D', 'F'...'O', 'Q'...'Z', '0'...'9' => {
                    self.index += 1;
                    continue :state .float;
                },
                'e', 'E', 'p', 'P' => {
                    continue :state .float_exponent;
                },
                else => self.index -= 1,
            }
        },
        .float => switch (self.buffer[self.index]) {
            '_', 'a'...'d', 'f'...'o', 'q'...'z', 'A'...'D', 'F'...'O', 'Q'...'Z', '0'...'9' => {
                self.index += 1;
                continue :state .float;
            },
            'e', 'E', 'p', 'P' => continue :state .float_exponent,
            else => {}, // return
        },
        .float_exponent => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                '-', '+' => {
                    self.index += 1;
                    continue :state .float;
                },
                else => continue :state .float,
            }
        },
        .invalid => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                0 => if (self.index == self.buffer.len) {
                    result.tag = .invalid;
                } else {
                    continue :state .invalid;
                },
                '\n' => result.tag = .invalid,
                else => continue :state .invalid,
            }
        },
    }

    result.end = self.index;
    return result;
}

test "all keywords" {
    const source = try std.mem.joinZ(std.testing.allocator, " ", Token.keywords.keys());
    defer std.testing.allocator.free(source);
    try testTokenize(source, Token.keywords.values());
}

test "all tokens with lexeme" {
    var builder: std.MultiArrayList(struct { lexeme: []const u8, tag: Token.Tag }) = .{};
    defer builder.deinit(std.testing.allocator);
    for (std.enums.values(Token.Tag)) |tag| {
        if (tag.lexeme()) |lexeme| {
            try builder.append(std.testing.allocator, .{ .lexeme = lexeme, .tag = tag });
        }
    }
    const source = try std.mem.joinZ(std.testing.allocator, " ", builder.items(.lexeme));
    defer std.testing.allocator.free(source);
    try testTokenize(source, builder.items(.tag));
}

test "line comment followed by statement" {
    try testTokenize(
        \\; comment
        \\let x: int = 32
        \\
    , &.{
        .newline,
        .keyword_let,
        .identifier,
        .colon,
        .identifier,
        .equal,
        .number_literal,
        .newline,
    });
}

test "statement with comment" {
    try testTokenize(
        \\let x = "string" ; this is a comment
        \\
    , &.{
        .keyword_let,
        .identifier,
        .equal,
        .string_literal,
        .newline,
    });
}

test "newline in string literal" {
    try testTokenize(
        \\"
        \\"
    , &.{
        .invalid,
        .newline,
        .invalid,
    });
}

test "line comment and doc comment" {
    try testTokenize(";", &.{});
    try testTokenize(";\n", &.{.newline});
    try testTokenize(";;", &.{});
    try testTokenize(";;;", &.{.doc_comment});
    try testTokenize(";;;\n", &.{ .doc_comment, .newline });
    try testTokenize(";;;;", &.{});
}

test "string identifier and builtin fns" {
    try testTokenize(
        \\@[public]
        \\let @"if" = @import("std")
        \\
    , &.{
        .at_sign_l_bracket,
        .identifier,
        .r_bracket,
        .newline,
        .keyword_let,
        .identifier,
        .equal,
        .builtin,
        .l_paren,
        .string_literal,
        .r_paren,
        .newline,
    });
}

test "UTF-8 BOM is recognized and skipped" {
    try testTokenize("\xEF\xBB\xBFa\n", &.{
        .identifier,
        .newline,
    });
}

test "shebang line is recognized and skipped" {
    try testTokenize(
        \\#!/usr/bin/env nov run
        \\a
        \\
    , &.{
        .identifier,
        .newline,
    });
}

test "assignment operators" {
    try testTokenize(
        \\x = 1
        \\x += 1
        \\x -= 1
        \\x *= 1
        \\x /= 1
        \\x %= 1
        \\
    , &.{
        .identifier, .equal,          .number_literal, .newline,
        .identifier, .plus_equal,     .number_literal, .newline,
        .identifier, .minus_equal,    .number_literal, .newline,
        .identifier, .asterisk_equal, .number_literal, .newline,
        .identifier, .slash_equal,    .number_literal, .newline,
        .identifier, .percent_equal,  .number_literal, .newline,
    });
}

test "pipe, monad and builtin" {
    try testTokenize(
        \\"hello" |> toArray >>= |x| @println(x)
        \\
    , &.{
        .string_literal,
        .pipe_arrow,
        .identifier,
        .r_angle_bracket_angle_bracket_equal,
        .pipe,
        .identifier,
        .pipe,
        .builtin,
        .l_paren,
        .identifier,
        .r_paren,
        .newline,
    });
}

test "range literals" {
    try testTokenize("0..9", &.{ .number_literal, .ellipsis2, .number_literal });
    try testTokenize("0x00..0x09", &.{ .number_literal, .ellipsis2, .number_literal });
    try testTokenize("0b00..0b11", &.{ .number_literal, .ellipsis2, .number_literal });
    try testTokenize("0o00..0o11", &.{ .number_literal, .ellipsis2, .number_literal });

    try testTokenize("0...9", &.{ .number_literal, .ellipsis3, .number_literal });
    try testTokenize("'0'...'9'", &.{ .char_literal, .ellipsis3, .char_literal });
    try testTokenize("0x00...0x09", &.{ .number_literal, .ellipsis3, .number_literal });
    try testTokenize("0b00...0b11", &.{ .number_literal, .ellipsis3, .number_literal });
    try testTokenize("0o00...0o11", &.{ .number_literal, .ellipsis3, .number_literal });
}

test "code point literal with hex escape" {
    try testTokenize(
        \\'\x1b'
    , &.{.char_literal});
    try testTokenize(
        \\'\x1'
    , &.{.char_literal});
}

test "code point literal with unicode escapes" {
    // Valid unicode escapes
    try testTokenize(
        \\'\u{3}'
    , &.{.char_literal});
    try testTokenize(
        \\'\u{01}'
    , &.{.char_literal});
    try testTokenize(
        \\'\u{2a}'
    , &.{.char_literal});
    try testTokenize(
        \\'\u{3f9}'
    , &.{.char_literal});
    try testTokenize(
        \\'\u{6E09aBc1523}'
    , &.{.char_literal});
    try testTokenize(
        \\"\u{440}"
    , &.{.string_literal});

    // Invalid unicode escapes
    try testTokenize(
        \\'\u'
    , &.{.char_literal});
    try testTokenize(
        \\'\u{{'
    , &.{.char_literal});
    try testTokenize(
        \\'\u{}'
    , &.{.char_literal});
    try testTokenize(
        \\'\u{s}'
    , &.{.char_literal});
    try testTokenize(
        \\'\u{2z}'
    , &.{.char_literal});
    try testTokenize(
        \\'\u{4a'
    , &.{.char_literal});

    // Test old-style unicode literals
    try testTokenize(
        \\'\u0333'
    , &.{.char_literal});
    try testTokenize(
        \\'\U0333'
    , &.{.char_literal});
}

test "code point literal with unicode code point" {
    try testTokenize(
        \\'💩'
    , &.{.char_literal});
}

test "float literal e exponent" {
    try testTokenize("a = 4.94065645841246544177e-324\n", &.{
        .identifier,
        .equal,
        .number_literal,
        .newline,
    });
}

test "float literal p exponent" {
    try testTokenize("a = 0x1.a827999fcef32p+1022\n", &.{
        .identifier,
        .equal,
        .number_literal,
        .newline,
    });
}

test "chars" {
    try testTokenize("'c'", &.{.char_literal});
}

test "invalid token characters" {
    try testTokenize("`", &.{.invalid});
    try testTokenize("$", &.{.invalid});
    try testTokenize("'c", &.{.invalid});
    try testTokenize("'", &.{.invalid});
    try testTokenize("@", &.{.invalid});
    try testTokenize("''", &.{.char_literal});
    try testTokenize("'\n'", &.{ .invalid, .newline, .invalid });
}

test "int literals decimal" {
    try testTokenize("0", &.{.number_literal});
    try testTokenize("1", &.{.number_literal});
    try testTokenize("2", &.{.number_literal});
    try testTokenize("3", &.{.number_literal});
    try testTokenize("4", &.{.number_literal});
    try testTokenize("5", &.{.number_literal});
    try testTokenize("6", &.{.number_literal});
    try testTokenize("7", &.{.number_literal});
    try testTokenize("8", &.{.number_literal});
    try testTokenize("9", &.{.number_literal});
    try testTokenize("1..", &.{ .number_literal, .ellipsis2 });
    try testTokenize("0a", &.{.number_literal});
    try testTokenize("9b", &.{.number_literal});
    try testTokenize("1z", &.{.number_literal});
    try testTokenize("1z_1", &.{.number_literal});
    try testTokenize("9z3", &.{.number_literal});

    try testTokenize("0_0", &.{.number_literal});
    try testTokenize("0001", &.{.number_literal});
    try testTokenize("01234567890", &.{.number_literal});
    try testTokenize("012_345_6789_0", &.{.number_literal});
    try testTokenize("0_1_2_3_4_5_6_7_8_9_0", &.{.number_literal});

    try testTokenize("00_", &.{.number_literal});
    try testTokenize("0_0_", &.{.number_literal});
    try testTokenize("0__0", &.{.number_literal});
    try testTokenize("0_0f", &.{.number_literal});
    try testTokenize("0_0_f", &.{.number_literal});
    try testTokenize("0_0_f_00", &.{.number_literal});
    try testTokenize("1_,", &.{ .number_literal, .comma });

    try testTokenize("0e0", &.{.number_literal});
    try testTokenize("1e0", &.{.number_literal});
    try testTokenize("1e100", &.{.number_literal});
    try testTokenize("1e", &.{.number_literal});
}

test "float literals decimal" {
    try testTokenize("._", &.{ .period, .identifier });
    try testTokenize("._1", &.{ .period, .identifier });
    try testTokenize("1.", &.{ .number_literal, .period });
    try testTokenize("1.+", &.{ .number_literal, .period, .plus });
    try testTokenize(".1234", &.{ .period, .number_literal });

    try testTokenize("0.0", &.{.number_literal});
    try testTokenize("1.0", &.{.number_literal});
    try testTokenize("0._", &.{.number_literal});
    try testTokenize("10.0", &.{.number_literal});
    try testTokenize("1.0e100", &.{.number_literal});
    try testTokenize("1.0e+100", &.{.number_literal});
    try testTokenize("1.0e-100", &.{.number_literal});
    try testTokenize("1_0_0_0.0_0_0_0_0_1e1_0_0_0", &.{.number_literal});

    try testTokenize("1.e100", &.{.number_literal});
    try testTokenize("1.0e1f0", &.{.number_literal});
    try testTokenize("1.0p100", &.{.number_literal});
    try testTokenize("1.0p-100", &.{.number_literal});
    try testTokenize("1.0p1f0", &.{.number_literal});
    try testTokenize("1.0_,", &.{ .number_literal, .comma });
    try testTokenize("1_.0", &.{.number_literal});
    try testTokenize("1._", &.{.number_literal});
    try testTokenize("1.a", &.{.number_literal});
    try testTokenize("1.z", &.{.number_literal});
    try testTokenize("1._0", &.{.number_literal});
    try testTokenize("1._+", &.{ .number_literal, .plus });
    try testTokenize("1._e", &.{.number_literal});
    try testTokenize("1.0e", &.{.number_literal});
    try testTokenize("1.0e,", &.{ .number_literal, .comma });
    try testTokenize("1.0e_", &.{.number_literal});
    try testTokenize("1.0e+_", &.{.number_literal});
    try testTokenize("1.0e-_", &.{.number_literal});
    try testTokenize("1.0e0_+", &.{ .number_literal, .plus });
}

test "number literals binary" {
    try testTokenize("0b0", &.{.number_literal});
    try testTokenize("0b1", &.{.number_literal});
    try testTokenize("0b2", &.{.number_literal});
    try testTokenize("0b3", &.{.number_literal});
    try testTokenize("0b4", &.{.number_literal});
    try testTokenize("0b5", &.{.number_literal});
    try testTokenize("0b6", &.{.number_literal});
    try testTokenize("0b7", &.{.number_literal});
    try testTokenize("0b8", &.{.number_literal});
    try testTokenize("0b9", &.{.number_literal});
    try testTokenize("0ba", &.{.number_literal});
    try testTokenize("0bb", &.{.number_literal});
    try testTokenize("0bc", &.{.number_literal});
    try testTokenize("0bd", &.{.number_literal});
    try testTokenize("0be", &.{.number_literal});
    try testTokenize("0bf", &.{.number_literal});
    try testTokenize("0bz", &.{.number_literal});

    try testTokenize("0b0000_0000", &.{.number_literal});
    try testTokenize("0b1111_1111", &.{.number_literal});
    try testTokenize("0b10_10_10_10", &.{.number_literal});
    try testTokenize("0b0_1_0_1_0_1_0_1", &.{.number_literal});
    try testTokenize("0b1.", &.{ .number_literal, .period });
    try testTokenize("0b1.0", &.{.number_literal});

    try testTokenize("0B0", &.{.number_literal});
    try testTokenize("0b_", &.{.number_literal});
    try testTokenize("0b_0", &.{.number_literal});
    try testTokenize("0b1_", &.{.number_literal});
    try testTokenize("0b0__1", &.{.number_literal});
    try testTokenize("0b0_1_", &.{.number_literal});
    try testTokenize("0b1e", &.{.number_literal});
    try testTokenize("0b1p", &.{.number_literal});
    try testTokenize("0b1e0", &.{.number_literal});
    try testTokenize("0b1p0", &.{.number_literal});
    try testTokenize("0b1_,", &.{ .number_literal, .comma });
}

test "number literals octal" {
    try testTokenize("0o0", &.{.number_literal});
    try testTokenize("0o1", &.{.number_literal});
    try testTokenize("0o2", &.{.number_literal});
    try testTokenize("0o3", &.{.number_literal});
    try testTokenize("0o4", &.{.number_literal});
    try testTokenize("0o5", &.{.number_literal});
    try testTokenize("0o6", &.{.number_literal});
    try testTokenize("0o7", &.{.number_literal});
    try testTokenize("0o8", &.{.number_literal});
    try testTokenize("0o9", &.{.number_literal});
    try testTokenize("0oa", &.{.number_literal});
    try testTokenize("0ob", &.{.number_literal});
    try testTokenize("0oc", &.{.number_literal});
    try testTokenize("0od", &.{.number_literal});
    try testTokenize("0oe", &.{.number_literal});
    try testTokenize("0of", &.{.number_literal});
    try testTokenize("0oz", &.{.number_literal});

    try testTokenize("0o01234567", &.{.number_literal});
    try testTokenize("0o0123_4567", &.{.number_literal});
    try testTokenize("0o01_23_45_67", &.{.number_literal});
    try testTokenize("0o0_1_2_3_4_5_6_7", &.{.number_literal});
    try testTokenize("0o7.", &.{ .number_literal, .period });
    try testTokenize("0o7.0", &.{.number_literal});

    try testTokenize("0O0", &.{.number_literal});
    try testTokenize("0o_", &.{.number_literal});
    try testTokenize("0o_0", &.{.number_literal});
    try testTokenize("0o1_", &.{.number_literal});
    try testTokenize("0o0__1", &.{.number_literal});
    try testTokenize("0o0_1_", &.{.number_literal});
    try testTokenize("0o1e", &.{.number_literal});
    try testTokenize("0o1p", &.{.number_literal});
    try testTokenize("0o1e0", &.{.number_literal});
    try testTokenize("0o1p0", &.{.number_literal});
    try testTokenize("0o_,", &.{ .number_literal, .comma });
}

test "number literals hexadecimal" {
    try testTokenize("0x0", &.{.number_literal});
    try testTokenize("0x1", &.{.number_literal});
    try testTokenize("0x2", &.{.number_literal});
    try testTokenize("0x3", &.{.number_literal});
    try testTokenize("0x4", &.{.number_literal});
    try testTokenize("0x5", &.{.number_literal});
    try testTokenize("0x6", &.{.number_literal});
    try testTokenize("0x7", &.{.number_literal});
    try testTokenize("0x8", &.{.number_literal});
    try testTokenize("0x9", &.{.number_literal});
    try testTokenize("0xa", &.{.number_literal});
    try testTokenize("0xb", &.{.number_literal});
    try testTokenize("0xc", &.{.number_literal});
    try testTokenize("0xd", &.{.number_literal});
    try testTokenize("0xe", &.{.number_literal});
    try testTokenize("0xf", &.{.number_literal});
    try testTokenize("0xA", &.{.number_literal});
    try testTokenize("0xB", &.{.number_literal});
    try testTokenize("0xC", &.{.number_literal});
    try testTokenize("0xD", &.{.number_literal});
    try testTokenize("0xE", &.{.number_literal});
    try testTokenize("0xF", &.{.number_literal});
    try testTokenize("0x0z", &.{.number_literal});
    try testTokenize("0xz", &.{.number_literal});

    try testTokenize("0x0123456789ABCDEF", &.{.number_literal});
    try testTokenize("0x0123_4567_89AB_CDEF", &.{.number_literal});
    try testTokenize("0x01_23_45_67_89AB_CDE_F", &.{.number_literal});
    try testTokenize("0x0_1_2_3_4_5_6_7_8_9_A_B_C_D_E_F", &.{.number_literal});

    try testTokenize("0X0", &.{.number_literal});
    try testTokenize("0x_", &.{.number_literal});
    try testTokenize("0x_1", &.{.number_literal});
    try testTokenize("0x1_", &.{.number_literal});
    try testTokenize("0x0__1", &.{.number_literal});
    try testTokenize("0x0_1_", &.{.number_literal});
    try testTokenize("0x_,", &.{ .number_literal, .comma });
    try testTokenize("0x1p0", &.{.number_literal});
    try testTokenize("0xfp0", &.{.number_literal});

    try testTokenize("0x1e", &.{.number_literal});
    try testTokenize("0x1e0", &.{.number_literal});
    try testTokenize("0x1p", &.{.number_literal});
    try testTokenize("0xfp0z1", &.{.number_literal});
    try testTokenize("0x0p0", &.{.number_literal});
    try testTokenize("0x0_p0", &.{.number_literal});
    try testTokenize("0xffp10", &.{.number_literal});
}

test "float literals hexadecimal" {
    try testTokenize("0x1.", &.{ .number_literal, .period });
    try testTokenize("0xF.", &.{ .number_literal, .period });
    try testTokenize("0x1.+0xF.", &.{ .number_literal, .period, .plus, .number_literal, .period });
    try testTokenize("0x0_.0.0", &.{ .number_literal, .period, .number_literal });

    try testTokenize("0x1.0", &.{.number_literal});
    try testTokenize("0xF.0", &.{.number_literal});
    try testTokenize("0xF.F", &.{.number_literal});
    try testTokenize("0xF.Fp0", &.{.number_literal});
    try testTokenize("0xF.FP0", &.{.number_literal});
    try testTokenize("0x1.0+0xF.0", &.{ .number_literal, .plus, .number_literal });

    try testTokenize("0x0123456.789ABCDEF", &.{.number_literal});
    try testTokenize("0x0_123_456.789_ABC_DEF", &.{.number_literal});
    try testTokenize("0x0_1_2_3_4_5_6.7_8_9_A_B_C_D_E_F", &.{.number_literal});
    try testTokenize("0x0.0p0", &.{.number_literal});
    try testTokenize("0xff.ffp10", &.{.number_literal});
    try testTokenize("0xff.ffP10", &.{.number_literal});
    try testTokenize("0xff_ff.ff_ffp1_0_0_0", &.{.number_literal});
    try testTokenize("0xf_f_f_f.f_f_f_fp+1_000", &.{.number_literal});
    try testTokenize("0xf_f_f_f.f_f_f_fp-1_00_0", &.{.number_literal});

    try testTokenize("0xff.p10", &.{.number_literal});
    try testTokenize("0xff.ffpff", &.{.number_literal});
    try testTokenize("0x0.p", &.{.number_literal});
    try testTokenize("0x0.z", &.{.number_literal});
    try testTokenize("0x0._", &.{.number_literal});
    try testTokenize("0x0_.0", &.{.number_literal});
    try testTokenize("0x0._0", &.{.number_literal});
    try testTokenize("0x0.0_", &.{.number_literal});
    try testTokenize("0x0_.p0", &.{.number_literal});
    try testTokenize("0x0._p0", &.{.number_literal});
    try testTokenize("0x0.0_p0", &.{.number_literal});
    try testTokenize("0x0._0p0", &.{.number_literal});
    try testTokenize("0x0.0p_0", &.{.number_literal});
    try testTokenize("0x0.0p+_0", &.{.number_literal});
    try testTokenize("0x0.0p-_0", &.{.number_literal});
    try testTokenize("0x0.0p0_", &.{.number_literal});
}

test "multi line string literal with only 1 backslash" {
    try testTokenize("x \\\n", &.{ .identifier, .invalid, .newline });
}

test "invalid token with unfinished escape right before eof" {
    try testTokenize("\"\\", &.{.invalid});
    try testTokenize("'\\", &.{.invalid});
    try testTokenize("'\\u", &.{.invalid});
}

test "null byte before eof" {
    try testTokenize("123 \x00 456", &.{ .number_literal, .invalid });
    try testTokenize(";\x00", &.{.invalid});
    try testTokenize("\x00", &.{.invalid});
    try testTokenize("; NUL\x00", &.{.invalid});
    try testTokenize(";;;\x00", &.{ .doc_comment, .invalid });
    try testTokenize(";;; NUL\x00", &.{ .doc_comment, .invalid });
}

fn testTokenize(source: [:0]const u8, expected_token_tags: []const Token.Tag) !void {
    var tokenizer = Tokenizer.init(source);
    for (expected_token_tags) |expected_token_tag| {
        const token = tokenizer.next();
        try std.testing.expectEqual(expected_token_tag, token.tag);
    }
    const last_token = tokenizer.next();
    try std.testing.expectEqual(Token.Tag.eof, last_token.tag);
    try std.testing.expectEqual(source.len, last_token.start);
    try std.testing.expectEqual(source.len, last_token.end);
}
