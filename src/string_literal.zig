// Originally based on https://github.com/ziglang/zig/blob/master/lib/std/zig/string_literal.zig
// See https://github.com/ziglang/zig/blob/master/LICENSE for additional LICENSE details
// Identifier interpolation is checked/lowered from the original token by BodyChecker.

const std = @import("std");
const assert = std.debug.assert;
const utf8Encode = std.unicode.utf8Encode;

pub const ParseError = error{
    OutOfMemory,
    InvalidLiteral,
};

pub const ParsedCharLiteral = union(enum) {
    success: u21,
    failure: Error,
};

pub const Result = union(enum) {
    success,
    failure: Error,
};

pub const Error = union(enum) {
    /// The character after backslash is missing or not recognized.
    invalid_escape_character: usize,
    /// Expected hex digit at this index.
    expected_hex_digit: usize,
    /// Unicode escape sequence had no digits with rbrace at this index.
    empty_unicode_escape_sequence: usize,
    /// Expected hex digit or '}' at this index.
    expected_hex_digit_or_rbrace: usize,
    /// Invalid unicode codepoint at this index.
    invalid_unicode_codepoint: usize,
    /// Expected '{' at this index.
    expected_lbrace: usize,
    /// Expected '}' at this index.
    expected_rbrace: usize,
    /// Expected '\'' at this index.
    expected_single_quote: usize,
    /// The character at this index cannot be represented without an escape sequence.
    invalid_character: usize,
    /// `''`. Not returned for string literals.
    empty_char_literal,
};

/// Parses exactly one Unicode scalar between single quotes, including escapes.
/// Malformed or truncated source is reported without assuming valid UTF-8.
pub fn parseCharLiteral(slice: []const u8) ParsedCharLiteral {
    if (slice.len == 0 or slice[0] != '\'') return .{ .failure = .{ .expected_single_quote = 0 } };
    if (slice.len < 2 or slice[slice.len - 1] != '\'') return .{ .failure = .{ .expected_single_quote = slice.len } };
    if (slice.len == 2) return .{ .failure = .empty_char_literal };

    switch (slice[1]) {
        '\\' => {
            var offset: usize = 1;
            const result = parseEscapeSequence(slice, &offset);
            if (result == .success and (offset + 1 != slice.len or slice[offset] != '\''))
                return .{ .failure = .{ .expected_single_quote = offset } };

            return result;
        },
        0, '\n', '\r', '\'' => return .{ .failure = .{ .invalid_character = 1 } },
        else => {
            const inner = slice[1 .. slice.len - 1];
            const n = std.unicode.utf8ByteSequenceLength(inner[0]) catch return .{ .failure = .{ .invalid_unicode_codepoint = 1 } };
            if (inner.len < n) return .{ .failure = .{ .invalid_unicode_codepoint = 1 } };
            const codepoint = std.unicode.utf8Decode(inner[0..n]) catch return .{ .failure = .{ .invalid_unicode_codepoint = 1 } };
            if (inner.len != n) return .{ .failure = .{ .expected_single_quote = 1 + n } };
            return .{ .success = codepoint };
        },
    }
}

/// Parse an escape sequence from `slice[offset..]`. If parsing is successful,
/// offset is updated to reflect the characters consumed.
pub fn parseEscapeSequence(slice: []const u8, offset: *usize) ParsedCharLiteral {
    assert(slice.len > offset.*);
    assert(slice[offset.*] == '\\');

    if (slice.len == offset.* + 1)
        return .{ .failure = .{ .invalid_escape_character = offset.* + 1 } };

    offset.* += 2;
    switch (slice[offset.* - 1]) {
        'n' => return .{ .success = '\n' },
        'r' => return .{ .success = '\r' },
        '\\' => return .{ .success = '\\' },
        't' => return .{ .success = '\t' },
        '\'' => return .{ .success = '\'' },
        '"' => return .{ .success = '"' },
        '0' => return .{ .success = '\x00' },
        '{' => return .{ .success = '{' },
        '}' => return .{ .success = '}' },
        'x' => {
            var value: u8 = 0;
            var i: usize = offset.*;
            while (i < offset.* + 2) : (i += 1) {
                if (i == slice.len) return .{ .failure = .{ .expected_hex_digit = i } };

                const c = slice[i];
                switch (c) {
                    '0'...'9' => {
                        value *= 16;
                        value += c - '0';
                    },
                    'a'...'f' => {
                        value *= 16;
                        value += c - 'a' + 10;
                    },
                    'A'...'F' => {
                        value *= 16;
                        value += c - 'A' + 10;
                    },
                    else => {
                        return .{ .failure = .{ .expected_hex_digit = i } };
                    },
                }
            }
            offset.* = i;
            return .{ .success = value };
        },
        'u' => {
            var i: usize = offset.*;
            if (i >= slice.len or slice[i] != '{') return .{ .failure = .{ .expected_lbrace = i } };
            i += 1;
            if (i >= slice.len) return .{ .failure = .{ .expected_hex_digit_or_rbrace = i } };
            if (slice[i] == '}') return .{ .failure = .{ .empty_unicode_escape_sequence = i } };

            var value: u32 = 0;
            while (i < slice.len) : (i += 1) {
                const c = slice[i];
                switch (c) {
                    '0'...'9' => {
                        value *= 16;
                        value += c - '0';
                    },
                    'a'...'f' => {
                        value *= 16;
                        value += c - 'a' + 10;
                    },
                    'A'...'F' => {
                        value *= 16;
                        value += c - 'A' + 10;
                    },
                    '}' => {
                        i += 1;
                        break;
                    },
                    else => return .{ .failure = .{ .expected_hex_digit_or_rbrace = i } },
                }
                if (value > 0x10ffff) {
                    return .{ .failure = .{ .invalid_unicode_codepoint = i } };
                }
            } else {
                return .{ .failure = .{ .expected_rbrace = i } };
            }
            // Preserve the string decoder's source location for surrogate escapes:
            // the opening brace, rather than the closing brace after validation.
            if (value >= 0xd800 and value <= 0xdfff) return .{ .failure = .{ .invalid_unicode_codepoint = offset.* } };
            offset.* = i;
            return .{ .success = @as(u21, @intCast(value)) };
        },
        else => return .{ .failure = .{ .invalid_escape_character = offset.* - 1 } },
    }
}

test "rune literals decode one scalar and reject malformed source without panics" {
    const Case = struct { text: []const u8, value: u21 };
    for ([_]Case{
        .{ .text = "'a'", .value = 97 },
        .{ .text = "'é'", .value = 0xe9 },
        .{ .text = "'😀'", .value = 0x1f600 },
        .{ .text = "'\\n'", .value = 10 },
        .{ .text = "'\\0'", .value = 0 },
        .{ .text = "'\\xE9'", .value = 0xe9 },
        .{ .text = "'\\u{10FFFF}'", .value = 0x10ffff },
        .{ .text = "'\\u{D7FF}'", .value = 0xd7ff },
        .{ .text = "'\\u{E000}'", .value = 0xe000 },
    }) |case| try std.testing.expectEqual(case.value, parseCharLiteral(case.text).success);
    for ([_][]const u8{
        "",            "'",           "''",            "a",                       "'a",                 "'ab'",
        "'é'",
        "'😀a'",
        "'\\u{D800}'", "'\\u{DFFF}'", "'\\u{110000}'", "'\\u{FFFFFFFFFFFFFFFF}'", "'\\u{'",             "'\\x'",
        "'\\'",        "'\xff'",      "'\xc0\x80'",    "'\xed\xa0\x80'",          "'\xf4\x90\x80\x80'", "'\xe2\x82'",
        "'\x00'",      "'\n'",
    }) |text| try std.testing.expect(parseCharLiteral(text) == .failure);
}

test "surrogate escapes retain shared string and rune diagnostic locations" {
    try std.testing.expectEqual(ParsedCharLiteral{ .failure = .{ .invalid_unicode_codepoint = 3 } }, parseCharLiteral("'\\u{D800}'"));
    var decoded = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer decoded.deinit();
    try std.testing.expectEqual(Result{ .failure = .{ .invalid_unicode_codepoint = 4 } }, try parseWrite(&decoded.writer, "\"a\\u{D800}\""));
}

test parseCharLiteral {
    try std.testing.expectEqual(
        ParsedCharLiteral{ .success = 'a' },
        parseCharLiteral("'a'"),
    );
    try std.testing.expectEqual(
        ParsedCharLiteral{ .success = 'ä' },
        parseCharLiteral("'ä'"),
    );
    try std.testing.expectEqual(
        ParsedCharLiteral{ .success = 0 },
        parseCharLiteral("'\\x00'"),
    );
    try std.testing.expectEqual(
        ParsedCharLiteral{ .success = 0x4f },
        parseCharLiteral("'\\x4f'"),
    );
    try std.testing.expectEqual(
        ParsedCharLiteral{ .success = 0x4f },
        parseCharLiteral("'\\x4F'"),
    );
    try std.testing.expectEqual(
        ParsedCharLiteral{ .success = 0x3041 },
        parseCharLiteral("'ぁ'"),
    );
    try std.testing.expectEqual(
        ParsedCharLiteral{ .success = 0 },
        parseCharLiteral("'\\u{0}'"),
    );
    try std.testing.expectEqual(
        ParsedCharLiteral{ .success = 0x3041 },
        parseCharLiteral("'\\u{3041}'"),
    );
    try std.testing.expectEqual(
        ParsedCharLiteral{ .success = 0x7f },
        parseCharLiteral("'\\u{7f}'"),
    );
    try std.testing.expectEqual(
        ParsedCharLiteral{ .success = 0x7fff },
        parseCharLiteral("'\\u{7FFF}'"),
    );
    try std.testing.expectEqual(
        ParsedCharLiteral{ .failure = .{ .expected_hex_digit = 4 } },
        parseCharLiteral("'\\x0'"),
    );
    try std.testing.expectEqual(
        ParsedCharLiteral{ .failure = .{ .expected_single_quote = 5 } },
        parseCharLiteral("'\\x000'"),
    );
    try std.testing.expectEqual(
        ParsedCharLiteral{ .failure = .{ .invalid_escape_character = 2 } },
        parseCharLiteral("'\\y'"),
    );
    try std.testing.expectEqual(
        ParsedCharLiteral{ .failure = .{ .expected_lbrace = 3 } },
        parseCharLiteral("'\\u'"),
    );
    try std.testing.expectEqual(
        ParsedCharLiteral{ .failure = .{ .expected_lbrace = 3 } },
        parseCharLiteral("'\\uFFFF'"),
    );
    try std.testing.expectEqual(
        ParsedCharLiteral{ .failure = .{ .empty_unicode_escape_sequence = 4 } },
        parseCharLiteral("'\\u{}'"),
    );
    try std.testing.expectEqual(
        ParsedCharLiteral{ .failure = .{ .invalid_unicode_codepoint = 9 } },
        parseCharLiteral("'\\u{FFFFFF}'"),
    );
    try std.testing.expectEqual(
        ParsedCharLiteral{ .failure = .{ .expected_hex_digit_or_rbrace = 8 } },
        parseCharLiteral("'\\u{FFFF'"),
    );
    try std.testing.expectEqual(
        ParsedCharLiteral{ .failure = .{ .expected_single_quote = 9 } },
        parseCharLiteral("'\\u{FFFF}x'"),
    );
    try std.testing.expectEqual(
        ParsedCharLiteral{ .failure = .{ .invalid_character = 1 } },
        parseCharLiteral("'\x00'"),
    );
}

/// Parses `bytes` as a Zig string literal and writes the result to the std.io.Writer type.
/// Asserts `bytes` has '"' at beginning and end.
pub fn parseWrite(writer: *std.Io.Writer, bytes: []const u8) std.Io.Writer.Error!Result {
    assert(bytes.len >= 2 and bytes[0] == '"' and bytes[bytes.len - 1] == '"');

    var index: usize = 1;
    b: switch (bytes[index]) {
        '\\' => {
            const escape_char_index = index + 1;
            const result = parseEscapeSequence(bytes, &index);
            switch (result) {
                .success => |codepoint| {
                    if (bytes[escape_char_index] == 'u') {
                        var buf: [4]u8 = undefined;
                        const len = utf8Encode(codepoint, &buf) catch {
                            return .{ .failure = .{ .invalid_unicode_codepoint = escape_char_index + 1 } };
                        };
                        try writer.writeAll(buf[0..len]);
                    } else {
                        try writer.writeByte(@as(u8, @intCast(codepoint)));
                    }
                },
                .failure => |err| return .{ .failure = err },
            }
            continue :b bytes[index];
        },
        '\n' => return .{ .failure = .{ .invalid_character = index } },
        '"' => return .success,
        else => {
            try writer.writeByte(bytes[index]);
            index += 1;
            continue :b bytes[index];
        },
    }
}

/// Higher level API. Does not return extra info about parse errors.
/// Caller owns returned memory.
pub fn parseAlloc(allocator: std.mem.Allocator, bytes: []const u8) ParseError![]u8 {
    var writer: std.Io.Writer.Allocating = .init(allocator);
    defer writer.deinit();

    const result = parseWrite(&writer.writer, bytes) catch return error.OutOfMemory;

    return switch (result) {
        .success => writer.toOwnedSlice(),
        .failure => error.InvalidLiteral,
    };
}

test parseAlloc {
    const expect = std.testing.expect;
    const expectError = std.testing.expectError;
    const eql = std.mem.eql;

    // I cba doing an arena
    var fixed_buf_mem: [4096]u8 = undefined;
    var fixed_buf_alloc = std.heap.FixedBufferAllocator.init(&fixed_buf_mem);
    const alloc = fixed_buf_alloc.allocator();

    try expectError(error.InvalidLiteral, parseAlloc(alloc, "\"\\x6\""));
    try expect(eql(u8, "foo\nbar", try parseAlloc(alloc, "\"foo\\nbar\"")));
    try expect(eql(u8, "\x12foo", try parseAlloc(alloc, "\"\\x12foo\"")));
    try expect(eql(u8, "bytes\u{1234}foo", try parseAlloc(alloc, "\"bytes\\u{1234}foo\"")));
    try expect(eql(u8, "foo", try parseAlloc(alloc, "\"foo\"")));
    try expect(eql(u8, "foo", try parseAlloc(alloc, "\"f\x6f\x6f\"")));
    try expect(eql(u8, "f💯", try parseAlloc(alloc, "\"f\u{1f4af}\"")));
}
