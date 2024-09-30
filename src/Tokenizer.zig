// Originally based on https://github.com/ziglang/zig/blob/master/lib/std/zig/tokenizer.zig
// See https://github.com/ziglang/zig/blob/master/LICENSE for additional LICENSE details

const std = @import("std");

const Tokenizer = @This();

buffer: [:0]const u8,
index: u32,

pub const Token = struct {
    tag: Tag,
    start: u32,
    end: u32,

    pub const Tag = enum {
        newline,
        l_paren,
        r_paren,
        l_bracket,
        r_bracket,
        l_brace,
        r_brace,
        comma,
        period,
        ellipsis2,
        colon,
        underscore,
        question_mark,
        question_mark_question_mark,
        bang,
        bang_equal,
        equal,
        equal_equal,
        equal_arrow,
        l_angle_bracket,
        l_angle_bracket_equal,
        l_angle_bracket_angle_bracket,
        l_angle_bracket_angle_bracket_equal,
        r_angle_bracket,
        r_angle_bracket_equal,
        r_angle_bracket_angle_bracket,
        r_angle_bracket_angle_bracket_equal,
        plus,
        plus_equal,
        minus,
        minus_equal,
        arrow,
        asterisk,
        asterisk_equal,
        slash,
        slash_equal,
        percent,
        percent_equal,
        pipe,
        pipe_equal,
        pipe_arrow,
        caret,
        caret_equal,
        ampersand,
        ampersand_equal,
        tilde,
        tilde_equal,
        keyword_true,
        keyword_false,
        keyword_and,
        keyword_or,
        keyword_if,
        keyword_else,
        keyword_match,
        keyword_return,
        keyword_let,
        keyword_mut,
        keyword_loop,
        keyword_while,
        keyword_for,
        keyword_break,
        keyword_continue,
        keyword_in,
        keyword_priv,
        // unused reserved keywords
        keyword_import,
        keyword_async,
        keyword_await,
        keyword_yield,
        keyword_throw,
        keyword_try,
        keyword_catch,
        keyword_pure,
        identifier,
        string_literal,
        multiline_string_literal_line,
        int_literal,
        float_literal,
        eof,
        invalid,

        pub fn lexeme(tag: Tag) ?[]const u8 {
            return switch (tag) {
                .eof,
                .invalid,
                .identifier,
                .string_literal,
                .multiline_string_literal_line,
                .int_literal,
                .float_literal,
                => null,

                .newline => "\n",
                .l_paren => "(",
                .r_paren => ")",
                .l_bracket => "[",
                .r_bracket => "]",
                .l_brace => "{",
                .r_brace => "}",
                .comma => ",",
                .period => ".",
                .ellipsis2 => "..",
                .colon => ":",
                .underscore => "_",
                .question_mark => "?",
                .question_mark_question_mark => "??",
                .bang => "!",
                .bang_equal => "!=",
                .equal => "=",
                .equal_equal => "==",
                .equal_arrow => "=>",
                .l_angle_bracket => "<",
                .l_angle_bracket_equal => "<=",
                .l_angle_bracket_angle_bracket => "<<",
                .l_angle_bracket_angle_bracket_equal => "<<=",
                .r_angle_bracket => ">",
                .r_angle_bracket_equal => ">=",
                .r_angle_bracket_angle_bracket => ">>",
                .r_angle_bracket_angle_bracket_equal => ">>=",
                .plus => "+",
                .plus_equal => "+=",
                .minus => "-",
                .minus_equal => "-=",
                .arrow => "->",
                .asterisk => "*",
                .asterisk_equal => "*=",
                .slash => "/",
                .slash_equal => "/=",
                .percent => "%",
                .percent_equal => "%=",
                .pipe => "|",
                .pipe_equal => "|=",
                .pipe_arrow => "|>",
                .caret => "^",
                .caret_equal => "^=",
                .ampersand => "&",
                .ampersand_equal => "&=",
                .tilde => "~",
                .tilde_equal => "~=",
                .keyword_true => "true",
                .keyword_false => "false",
                .keyword_and => "and",
                .keyword_or => "or",
                .keyword_if => "if",
                .keyword_else => "else",
                .keyword_match => "match",
                .keyword_return => "return",
                .keyword_let => "let",
                .keyword_mut => "mut",
                .keyword_loop => "loop",
                .keyword_while => "while",
                .keyword_for => "for",
                .keyword_break => "break",
                .keyword_continue => "continue",
                .keyword_in => "in",
                .keyword_priv => "priv",
                .keyword_import => "import",
                .keyword_async => "async",
                .keyword_await => "await",
                .keyword_yield => "yield",
                .keyword_throw => "throw",
                .keyword_try => "try",
                .keyword_catch => "catch",
                .keyword_pure => "pure",
            };
        }

        pub fn symbol(tag: Tag) []const u8 {
            if (tag == .newline) {
                return "\\n";
            }

            return tag.lexeme() orelse switch (tag) {
                .eof => "EOF",
                .invalid => "invalid bytes",
                .identifier => "an identifier",
                .string_literal, .multiline_string_literal_line => "a string literal",
                .int_literal => "an integer literal",
                .float_literal => "a float literal",
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
    // Skip the UTF-8 BOM if present
    const src_start: u32 = if (std.mem.startsWith(u8, buffer, "\xEF\xBB\xBF")) 3 else 0;
    return .{
        .buffer = buffer,
        .index = src_start,
    };
}

const State = enum {
    start,
    ampersand,
    caret,
    identifier,
    string_literal,
    string_literal_single_quote,
    string_literal_backslash,
    multiline_string_literal_line,
    comment_line,
    equal,
    bang,
    pipe,
    percent,
    asterisk,
    plus,
    minus,
    slash,
    tilde,
    l_angle_bracket,
    l_angle_bracket_angle_bracket,
    r_angle_bracket,
    r_angle_bracket_angle_bracket,
    backslash,
    period,
    int,
    int_period,
    int_exponent,
    float,
    float_exponent,
    underscore,
    question_mark,
};

pub fn next(self: *Tokenizer) Token {
    var state: State = .start;
    var result: Token = .{
        .tag = .eof,
        .start = self.index,
        .end = undefined,
    };
    while (true) : (self.index += 1) {
        const c = self.buffer[self.index];
        switch (state) {
            .start => switch (c) {
                0 => {
                    if (self.index != self.buffer.len) {
                        result.tag = .invalid;
                        result.start = self.index;
                        self.index += 1;
                        result.end = self.index;
                        return result;
                    }
                    break;
                },
                ' ', '\r', '\t' => {
                    result.start = self.index + 1;
                },
                '"' => {
                    state = .string_literal;
                    result.tag = .string_literal;
                },
                '\'' => {
                    state = .string_literal_single_quote;
                    result.tag = .string_literal;
                },
                '#', ';' => {
                    state = .comment_line;
                },
                '_' => {
                    state = .underscore;
                },
                'A'...'Z', 'a'...'z' => {
                    state = .identifier;
                    result.tag = .identifier;
                },
                '?' => {
                    state = .question_mark;
                },
                '=' => {
                    state = .equal;
                },
                '!' => {
                    state = .bang;
                },
                '|' => {
                    state = .pipe;
                },
                '\n' => {
                    result.tag = .newline;
                    self.index += 1;
                    break;
                },
                '(' => {
                    result.tag = .l_paren;
                    self.index += 1;
                    break;
                },
                ')' => {
                    result.tag = .r_paren;
                    self.index += 1;
                    break;
                },
                '[' => {
                    result.tag = .l_bracket;
                    self.index += 1;
                    break;
                },
                ']' => {
                    result.tag = .r_bracket;
                    self.index += 1;
                    break;
                },
                '{' => {
                    result.tag = .l_brace;
                    self.index += 1;
                    break;
                },
                '}' => {
                    result.tag = .r_brace;
                    self.index += 1;
                    break;
                },
                ',' => {
                    result.tag = .comma;
                    self.index += 1;
                    break;
                },
                ':' => {
                    result.tag = .colon;
                    self.index += 1;
                    break;
                },
                '%' => {
                    state = .percent;
                },
                '*' => {
                    state = .asterisk;
                },
                '+' => {
                    state = .plus;
                },
                '-' => {
                    state = .minus;
                },
                '/' => {
                    state = .slash;
                },
                '<' => {
                    state = .l_angle_bracket;
                },
                '>' => {
                    state = .r_angle_bracket;
                },
                '&' => {
                    state = .ampersand;
                },
                '^' => {
                    state = .caret;
                },
                '~' => {
                    state = .tilde;
                },
                '\\' => {
                    state = .backslash;
                    result.tag = .multiline_string_literal_line;
                },
                '.' => {
                    state = .period;
                },
                '0'...'9' => {
                    state = .int;
                    result.tag = .int_literal;
                },
                else => {
                    result.tag = .invalid;
                    self.index += 1;
                    break;
                },
            },
            .question_mark => switch (c) {
                '?' => {
                    result.tag = .question_mark_question_mark;
                    self.index += 1;
                    break;
                },
                else => {
                    result.tag = .question_mark;
                    break;
                },
            },
            .ampersand => switch (c) {
                '=' => {
                    result.tag = .ampersand_equal;
                    self.index += 1;
                    break;
                },
                else => {
                    result.tag = .ampersand;
                    break;
                },
            },
            .asterisk => switch (c) {
                '=' => {
                    result.tag = .asterisk_equal;
                    self.index += 1;
                    break;
                },
                else => {
                    result.tag = .asterisk;
                    break;
                },
            },
            .percent => switch (c) {
                '=' => {
                    result.tag = .percent_equal;
                    self.index += 1;
                    break;
                },
                else => {
                    result.tag = .percent;
                    break;
                },
            },
            .plus => switch (c) {
                '=' => {
                    result.tag = .plus_equal;
                    self.index += 1;
                    break;
                },
                else => {
                    result.tag = .plus;
                    break;
                },
            },
            .caret => switch (c) {
                '=' => {
                    result.tag = .caret_equal;
                    self.index += 1;
                    break;
                },
                else => {
                    result.tag = .caret;
                    break;
                },
            },
            .tilde => switch (c) {
                '=' => {
                    result.tag = .tilde_equal;
                    self.index += 1;
                    break;
                },
                else => {
                    result.tag = .tilde;
                    break;
                },
            },
            .underscore => switch (c) {
                'A'...'Z', 'a'...'z', '0'...'9', '_' => {
                    state = .identifier;
                    result.tag = .identifier;
                },
                else => {
                    result.tag = .underscore;
                    break;
                },
            },
            .identifier => switch (c) {
                'A'...'Z', 'a'...'z', '0'...'9', '_' => {},
                else => {
                    if (Token.getKeyword(self.buffer[result.start..self.index])) |tag| {
                        result.tag = tag;
                    }
                    break;
                },
            },
            .backslash => switch (c) {
                '\\' => {
                    state = .multiline_string_literal_line;
                },
                else => {
                    result.tag = .invalid;
                    break;
                },
            },
            .string_literal => switch (c) {
                '\\' => {
                    state = .string_literal_backslash;
                },
                '"' => {
                    self.index += 1;
                    break;
                },
                0 => {
                    if (self.index == self.buffer.len) {
                        result.tag = .invalid;
                        break;
                    }
                },
                '\n' => {
                    result.tag = .invalid;
                    break;
                },
                else => {},
            },
            .string_literal_single_quote => switch (c) {
                '\\' => {
                    state = .string_literal_backslash;
                },
                '\'' => {
                    self.index += 1;
                    break;
                },
                0 => {
                    if (self.index == self.buffer.len) {
                        result.tag = .invalid;
                        break;
                    }
                },
                '\n' => {
                    result.tag = .invalid;
                    break;
                },
                else => {},
            },
            .string_literal_backslash => switch (c) {
                0, '\n' => {
                    result.tag = .invalid;
                    break;
                },
                else => {
                    state = .string_literal;
                },
            },
            .multiline_string_literal_line => switch (c) {
                0 => {
                    break;
                },
                '\n' => {
                    self.index += 1;
                    break;
                },
                else => {},
            },
            .bang => switch (c) {
                '=' => {
                    result.tag = .bang_equal;
                    self.index += 1;
                    break;
                },
                else => {
                    result.tag = .bang;
                    break;
                },
            },
            .pipe => switch (c) {
                '=' => {
                    result.tag = .pipe_equal;
                    self.index += 1;
                    break;
                },
                '>' => {
                    result.tag = .pipe_arrow;
                    self.index += 1;
                    break;
                },
                else => {
                    result.tag = .pipe;
                    break;
                },
            },
            .equal => switch (c) {
                '=' => {
                    result.tag = .equal_equal;
                    self.index += 1;
                    break;
                },
                '>' => {
                    result.tag = .equal_arrow;
                    self.index += 1;
                    break;
                },
                else => {
                    result.tag = .equal;
                    break;
                },
            },
            .minus => switch (c) {
                '=' => {
                    result.tag = .minus_equal;
                    self.index += 1;
                    break;
                },
                '>' => {
                    result.tag = .arrow;
                    self.index += 1;
                    break;
                },
                else => {
                    result.tag = .minus;
                    break;
                },
            },
            .l_angle_bracket => switch (c) {
                '=' => {
                    result.tag = .l_angle_bracket_equal;
                    self.index += 1;
                    break;
                },
                '<' => {
                    state = .l_angle_bracket_angle_bracket;
                },
                else => {
                    result.tag = .l_angle_bracket;
                    break;
                },
            },
            .l_angle_bracket_angle_bracket => switch (c) {
                '=' => {
                    result.tag = .l_angle_bracket_angle_bracket_equal;
                    self.index += 1;
                    break;
                },
                else => {
                    result.tag = .l_angle_bracket_angle_bracket;
                    break;
                },
            },
            .r_angle_bracket => switch (c) {
                '=' => {
                    result.tag = .r_angle_bracket_equal;
                    self.index += 1;
                    break;
                },
                '>' => {
                    state = .r_angle_bracket_angle_bracket;
                },
                else => {
                    result.tag = .r_angle_bracket;
                    break;
                },
            },
            .r_angle_bracket_angle_bracket => switch (c) {
                '=' => {
                    result.tag = .r_angle_bracket_angle_bracket_equal;
                    self.index += 1;
                    break;
                },
                else => {
                    result.tag = .r_angle_bracket_angle_bracket;
                    break;
                },
            },
            .period => switch (c) {
                '.' => {
                    result.tag = .ellipsis2;
                    self.index += 1;
                    break;
                },
                '0'...'9' => {
                    state = .float;
                    result.tag = .float_literal;
                },
                else => {
                    result.tag = .period;
                    break;
                },
            },
            .slash => switch (c) {
                '=' => {
                    result.tag = .slash_equal;
                    self.index += 1;
                    break;
                },
                else => {
                    result.tag = .slash;
                    break;
                },
            },
            .comment_line => switch (c) {
                0 => {
                    if (self.index != self.buffer.len) {
                        result.tag = .invalid;
                        self.index += 1;
                    }
                    break;
                },
                '\n' => {
                    result.tag = .newline;
                    result.start = self.index;
                    self.index += 1;
                    break;
                },
                else => {},
            },
            .int => switch (c) {
                '.' => {
                    state = .int_period;
                },
                '_', 'a'...'d', 'f'...'o', 'q'...'z', 'A'...'D', 'F'...'O', 'Q'...'Z', '0'...'9' => {},
                'e', 'E', 'p', 'P' => {
                    state = .int_exponent;
                },
                else => {
                    break;
                },
            },
            .int_exponent => switch (c) {
                '-', '+' => {
                    state = .float;
                    result.tag = .float_literal;
                },
                else => {
                    self.index -= 1;
                    state = .int;
                },
            },
            .int_period => switch (c) {
                '.' => {
                    self.index -= 1;
                    break;
                },
                else => {
                    self.index -= 1;
                    state = .float;
                    result.tag = .float_literal;
                },
            },
            .float => switch (c) {
                '_', 'a'...'d', 'f'...'o', 'q'...'z', 'A'...'D', 'F'...'O', 'Q'...'Z', '0'...'9' => {},
                'e', 'E', 'p', 'P' => {
                    state = .float_exponent;
                },
                else => {
                    break;
                },
            },
            .float_exponent => switch (c) {
                '-', '+' => {
                    state = .float;
                },
                else => {
                    self.index -= 1;
                    state = .float;
                },
            },
        }
    }

    if (result.tag == .eof) {
        result.start = self.index;
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

test "_" {
    try testTokenize(
        \\_ => 0,
        \\
    , &.{
        .underscore,
        .equal_arrow,
        .int_literal,
        .comma,
        .newline,
    });
    try testTokenize(
        \\_0 => 0,
        \\
    , &.{
        .identifier,
        .equal_arrow,
        .int_literal,
        .comma,
        .newline,
    });
    try testTokenize(
        \\_xxx => 0,
        \\
    , &.{
        .identifier,
        .equal_arrow,
        .int_literal,
        .comma,
        .newline,
    });
}

test "line comment followed by statement" {
    try testTokenize(
        \\# comment
        \\let x: int = 32
        \\
    , &.{
        .newline,
        .keyword_let,
        .identifier,
        .colon,
        .identifier,
        .equal,
        .int_literal,
        .newline,
    });
}

test "string literal with single quotes" {
    try testTokenize(
        \\'hello "world"'
        \\
    , &.{
        .string_literal,
        .newline,
    });
}

test "multiline string literal" {
    try testTokenize(
        \\\\hello
        \\\\world
        \\\\
    , &.{
        .multiline_string_literal_line,
        .multiline_string_literal_line,
        .multiline_string_literal_line,
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

test "multiple comment lines" {
    try testTokenize(
        \\# a
        \\; b
        \\# a
        \\# c
        \\
    , &.{
        .newline,
        .newline,
        .newline,
        .newline,
    });
}

test "fake multiline string" {
    try testTokenize(
        \\"     \
        \\      \
        \\"
    , &.{
        .invalid,
        .newline,
        .invalid,
        .newline,
        .invalid,
    });
}

test "UTF-8 BOM is recognized and skipped" {
    try testTokenize("\xEF\xBB\xBFa\n", &.{
        .identifier,
        .newline,
    });
}

test "small program" {
    try testTokenize(
        \\doNothing(arg: str) str {
        \\    return arg
        \\}
        \\main() {
        \\    doNothing("hello")
        \\}
        \\
    , &.{
        .identifier,
        .l_paren,
        .identifier,
        .colon,
        .identifier,
        .r_paren,
        .identifier,
        .l_brace,
        .newline,
        .keyword_return,
        .identifier,
        .newline,
        .r_brace,
        .newline,
        .identifier,
        .l_paren,
        .r_paren,
        .l_brace,
        .newline,
        .identifier,
        .l_paren,
        .string_literal,
        .r_paren,
        .newline,
        .r_brace,
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
        \\x &= 1
        \\x |= 1
        \\x ^= 1
        \\x <<= 1
        \\x >>= 1
        \\
    , &.{
        .identifier, .equal,                               .int_literal, .newline,
        .identifier, .plus_equal,                          .int_literal, .newline,
        .identifier, .minus_equal,                         .int_literal, .newline,
        .identifier, .asterisk_equal,                      .int_literal, .newline,
        .identifier, .slash_equal,                         .int_literal, .newline,
        .identifier, .percent_equal,                       .int_literal, .newline,
        .identifier, .ampersand_equal,                     .int_literal, .newline,
        .identifier, .pipe_equal,                          .int_literal, .newline,
        .identifier, .caret_equal,                         .int_literal, .newline,
        .identifier, .l_angle_bracket_angle_bracket_equal, .int_literal, .newline,
        .identifier, .r_angle_bracket_angle_bracket_equal, .int_literal, .newline,
    });
}

test "pipe into functions" {
    try testTokenize(
        \\"81" |> int_of_string |> float_of_int |> sqrt |> int_of_float
        \\
    , &.{
        .string_literal,
        .pipe_arrow,
        .identifier,
        .pipe_arrow,
        .identifier,
        .pipe_arrow,
        .identifier,
        .pipe_arrow,
        .identifier,
        .newline,
    });
}

test "range literals" {
    try testTokenize("0..9", &.{ .int_literal, .ellipsis2, .int_literal });
    try testTokenize("0x00..0x09", &.{ .int_literal, .ellipsis2, .int_literal });
    try testTokenize("0b00..0b11", &.{ .int_literal, .ellipsis2, .int_literal });
    try testTokenize("0o00..0o11", &.{ .int_literal, .ellipsis2, .int_literal });
}

test "int literals decimal" {
    try testTokenize("0", &.{.int_literal});
    try testTokenize("1", &.{.int_literal});
    try testTokenize("2", &.{.int_literal});
    try testTokenize("3", &.{.int_literal});
    try testTokenize("4", &.{.int_literal});
    try testTokenize("5", &.{.int_literal});
    try testTokenize("6", &.{.int_literal});
    try testTokenize("7", &.{.int_literal});
    try testTokenize("8", &.{.int_literal});
    try testTokenize("9", &.{.int_literal});
    try testTokenize("1..", &.{ .int_literal, .ellipsis2 });
    try testTokenize("0a", &.{.int_literal});
    try testTokenize("9b", &.{.int_literal});
    try testTokenize("1z", &.{.int_literal});
    try testTokenize("1z_1", &.{.int_literal});
    try testTokenize("9z3", &.{.int_literal});

    try testTokenize("0_0", &.{.int_literal});
    try testTokenize("0001", &.{.int_literal});
    try testTokenize("01234567890", &.{.int_literal});
    try testTokenize("012_345_6789_0", &.{.int_literal});
    try testTokenize("0_1_2_3_4_5_6_7_8_9_0", &.{.int_literal});

    try testTokenize("00_", &.{.int_literal});
    try testTokenize("0_0_", &.{.int_literal});
    try testTokenize("0__0", &.{.int_literal});
    try testTokenize("0_0f", &.{.int_literal});
    try testTokenize("0_0_f", &.{.int_literal});
    try testTokenize("0_0_f_00", &.{.int_literal});
    try testTokenize("1_,", &.{ .int_literal, .comma });

    try testTokenize("0e0", &.{.int_literal});
    try testTokenize("1e0", &.{.int_literal});
    try testTokenize("1e100", &.{.int_literal});
    try testTokenize("1e", &.{.int_literal});
}

test "float literals decimal" {
    try testTokenize("._", &.{ .period, .underscore });
    try testTokenize("._1", &.{ .period, .identifier });

    try testTokenize("1.", &.{.float_literal});
    try testTokenize("1.+", &.{ .float_literal, .plus });
    try testTokenize(".1234", &.{.float_literal});
    try testTokenize("0.0", &.{.float_literal});
    try testTokenize("1.0", &.{.float_literal});
    try testTokenize("0._", &.{.float_literal});
    try testTokenize("10.0", &.{.float_literal});
    try testTokenize("1.0e100", &.{.float_literal});
    try testTokenize("1.0e+100", &.{.float_literal});
    try testTokenize("1.0e-100", &.{.float_literal});
    try testTokenize("1_0_0_0.0_0_0_0_0_1e1_0_0_0", &.{.float_literal});

    try testTokenize("1.e100", &.{.float_literal});
    try testTokenize("1.0e1f0", &.{.float_literal});
    try testTokenize("1.0p100", &.{.float_literal});
    try testTokenize("1.0p-100", &.{.float_literal});
    try testTokenize("1.0p1f0", &.{.float_literal});
    try testTokenize("1.0_,", &.{ .float_literal, .comma });
    try testTokenize("1_.0", &.{.float_literal});
    try testTokenize("1._", &.{.float_literal});
    try testTokenize("1.a", &.{.float_literal});
    try testTokenize("1.z", &.{.float_literal});
    try testTokenize("1._0", &.{.float_literal});
    try testTokenize("1._+", &.{ .float_literal, .plus });
    try testTokenize("1._e", &.{.float_literal});
    try testTokenize("1.0e", &.{.float_literal});
    try testTokenize("1.0e,", &.{ .float_literal, .comma });
    try testTokenize("1.0e_", &.{.float_literal});
    try testTokenize("1.0e+_", &.{.float_literal});
    try testTokenize("1.0e-_", &.{.float_literal});
    try testTokenize("1.0e0_+", &.{ .float_literal, .plus });
}

test "number literals binary" {
    try testTokenize("0b0", &.{.int_literal});
    try testTokenize("0b1", &.{.int_literal});
    try testTokenize("0b2", &.{.int_literal});
    try testTokenize("0b3", &.{.int_literal});
    try testTokenize("0b4", &.{.int_literal});
    try testTokenize("0b5", &.{.int_literal});
    try testTokenize("0b6", &.{.int_literal});
    try testTokenize("0b7", &.{.int_literal});
    try testTokenize("0b8", &.{.int_literal});
    try testTokenize("0b9", &.{.int_literal});
    try testTokenize("0ba", &.{.int_literal});
    try testTokenize("0bb", &.{.int_literal});
    try testTokenize("0bc", &.{.int_literal});
    try testTokenize("0bd", &.{.int_literal});
    try testTokenize("0be", &.{.int_literal});
    try testTokenize("0bf", &.{.int_literal});
    try testTokenize("0bz", &.{.int_literal});

    try testTokenize("0b0000_0000", &.{.int_literal});
    try testTokenize("0b1111_1111", &.{.int_literal});
    try testTokenize("0b10_10_10_10", &.{.int_literal});
    try testTokenize("0b0_1_0_1_0_1_0_1", &.{.int_literal});
    try testTokenize("0b1.", &.{.float_literal});
    try testTokenize("0b1.0", &.{.float_literal});

    try testTokenize("0B0", &.{.int_literal});
    try testTokenize("0b_", &.{.int_literal});
    try testTokenize("0b_0", &.{.int_literal});
    try testTokenize("0b1_", &.{.int_literal});
    try testTokenize("0b0__1", &.{.int_literal});
    try testTokenize("0b0_1_", &.{.int_literal});
    try testTokenize("0b1e", &.{.int_literal});
    try testTokenize("0b1p", &.{.int_literal});
    try testTokenize("0b1e0", &.{.int_literal});
    try testTokenize("0b1p0", &.{.int_literal});
    try testTokenize("0b1_,", &.{ .int_literal, .comma });
}

test "number literals octal" {
    try testTokenize("0o0", &.{.int_literal});
    try testTokenize("0o1", &.{.int_literal});
    try testTokenize("0o2", &.{.int_literal});
    try testTokenize("0o3", &.{.int_literal});
    try testTokenize("0o4", &.{.int_literal});
    try testTokenize("0o5", &.{.int_literal});
    try testTokenize("0o6", &.{.int_literal});
    try testTokenize("0o7", &.{.int_literal});
    try testTokenize("0o8", &.{.int_literal});
    try testTokenize("0o9", &.{.int_literal});
    try testTokenize("0oa", &.{.int_literal});
    try testTokenize("0ob", &.{.int_literal});
    try testTokenize("0oc", &.{.int_literal});
    try testTokenize("0od", &.{.int_literal});
    try testTokenize("0oe", &.{.int_literal});
    try testTokenize("0of", &.{.int_literal});
    try testTokenize("0oz", &.{.int_literal});

    try testTokenize("0o01234567", &.{.int_literal});
    try testTokenize("0o0123_4567", &.{.int_literal});
    try testTokenize("0o01_23_45_67", &.{.int_literal});
    try testTokenize("0o0_1_2_3_4_5_6_7", &.{.int_literal});
    try testTokenize("0o7.", &.{.float_literal});
    try testTokenize("0o7.0", &.{.float_literal});

    try testTokenize("0O0", &.{.int_literal});
    try testTokenize("0o_", &.{.int_literal});
    try testTokenize("0o_0", &.{.int_literal});
    try testTokenize("0o1_", &.{.int_literal});
    try testTokenize("0o0__1", &.{.int_literal});
    try testTokenize("0o0_1_", &.{.int_literal});
    try testTokenize("0o1e", &.{.int_literal});
    try testTokenize("0o1p", &.{.int_literal});
    try testTokenize("0o1e0", &.{.int_literal});
    try testTokenize("0o1p0", &.{.int_literal});
    try testTokenize("0o_,", &.{ .int_literal, .comma });
}

test "number literals hexadecimal" {
    try testTokenize("0x0", &.{.int_literal});
    try testTokenize("0x1", &.{.int_literal});
    try testTokenize("0x2", &.{.int_literal});
    try testTokenize("0x3", &.{.int_literal});
    try testTokenize("0x4", &.{.int_literal});
    try testTokenize("0x5", &.{.int_literal});
    try testTokenize("0x6", &.{.int_literal});
    try testTokenize("0x7", &.{.int_literal});
    try testTokenize("0x8", &.{.int_literal});
    try testTokenize("0x9", &.{.int_literal});
    try testTokenize("0xa", &.{.int_literal});
    try testTokenize("0xb", &.{.int_literal});
    try testTokenize("0xc", &.{.int_literal});
    try testTokenize("0xd", &.{.int_literal});
    try testTokenize("0xe", &.{.int_literal});
    try testTokenize("0xf", &.{.int_literal});
    try testTokenize("0xA", &.{.int_literal});
    try testTokenize("0xB", &.{.int_literal});
    try testTokenize("0xC", &.{.int_literal});
    try testTokenize("0xD", &.{.int_literal});
    try testTokenize("0xE", &.{.int_literal});
    try testTokenize("0xF", &.{.int_literal});
    try testTokenize("0x0z", &.{.int_literal});
    try testTokenize("0xz", &.{.int_literal});

    try testTokenize("0x0123456789ABCDEF", &.{.int_literal});
    try testTokenize("0x0123_4567_89AB_CDEF", &.{.int_literal});
    try testTokenize("0x01_23_45_67_89AB_CDE_F", &.{.int_literal});
    try testTokenize("0x0_1_2_3_4_5_6_7_8_9_A_B_C_D_E_F", &.{.int_literal});

    try testTokenize("0X0", &.{.int_literal});
    try testTokenize("0x_", &.{.int_literal});
    try testTokenize("0x_1", &.{.int_literal});
    try testTokenize("0x1_", &.{.int_literal});
    try testTokenize("0x0__1", &.{.int_literal});
    try testTokenize("0x0_1_", &.{.int_literal});
    try testTokenize("0x_,", &.{ .int_literal, .comma });
    try testTokenize("0x1p0", &.{.int_literal});
    try testTokenize("0xfp0", &.{.int_literal});

    try testTokenize("0x1e", &.{.int_literal});
    try testTokenize("0x1e0", &.{.int_literal});
    try testTokenize("0x1p", &.{.int_literal});
    try testTokenize("0xfp0z1", &.{.int_literal});
    try testTokenize("0x0p0", &.{.int_literal});
    try testTokenize("0x0_p0", &.{.int_literal});
    try testTokenize("0xffp10", &.{.int_literal});
}

test "float literals hexadecimal" {
    try testTokenize("0x1.", &.{.float_literal});
    try testTokenize("0xF.", &.{.float_literal});
    try testTokenize("0x1.+0xF.", &.{ .float_literal, .plus, .float_literal });
    try testTokenize("0x0_.0.0", &.{ .float_literal, .float_literal });

    try testTokenize("0x1.0", &.{.float_literal});
    try testTokenize("0xF.0", &.{.float_literal});
    try testTokenize("0xF.F", &.{.float_literal});
    try testTokenize("0xF.Fp0", &.{.float_literal});
    try testTokenize("0xF.FP0", &.{.float_literal});
    try testTokenize("0x1.0+0xF.0", &.{ .float_literal, .plus, .float_literal });

    try testTokenize("0x0123456.789ABCDEF", &.{.float_literal});
    try testTokenize("0x0_123_456.789_ABC_DEF", &.{.float_literal});
    try testTokenize("0x0_1_2_3_4_5_6.7_8_9_A_B_C_D_E_F", &.{.float_literal});
    try testTokenize("0x0.0p0", &.{.float_literal});
    try testTokenize("0xff.ffp10", &.{.float_literal});
    try testTokenize("0xff.ffP10", &.{.float_literal});
    try testTokenize("0xff_ff.ff_ffp1_0_0_0", &.{.float_literal});
    try testTokenize("0xf_f_f_f.f_f_f_fp+1_000", &.{.float_literal});
    try testTokenize("0xf_f_f_f.f_f_f_fp-1_00_0", &.{.float_literal});

    try testTokenize("0xff.p10", &.{.float_literal});
    try testTokenize("0xff.ffpff", &.{.float_literal});
    try testTokenize("0x0.p", &.{.float_literal});
    try testTokenize("0x0.z", &.{.float_literal});
    try testTokenize("0x0._", &.{.float_literal});
    try testTokenize("0x0_.0", &.{.float_literal});
    try testTokenize("0x0._0", &.{.float_literal});
    try testTokenize("0x0.0_", &.{.float_literal});
    try testTokenize("0x0_.p0", &.{.float_literal});
    try testTokenize("0x0._p0", &.{.float_literal});
    try testTokenize("0x0.0_p0", &.{.float_literal});
    try testTokenize("0x0._0p0", &.{.float_literal});
    try testTokenize("0x0.0p_0", &.{.float_literal});
    try testTokenize("0x0.0p+_0", &.{.float_literal});
    try testTokenize("0x0.0p-_0", &.{.float_literal});
    try testTokenize("0x0.0p0_", &.{.float_literal});
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
    try testTokenize("123 \x00 456", &.{ .int_literal, .invalid, .int_literal });
    try testTokenize("#\x00", &.{.invalid});
    try testTokenize("\\\\\x00", &.{ .multiline_string_literal_line, .invalid });
    try testTokenize("\x00", &.{.invalid});
    try testTokenize("# NUL\x00", &.{.invalid});
    try testTokenize("#\x00", &.{.invalid});
    try testTokenize("# NUL\x00", &.{.invalid});
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
