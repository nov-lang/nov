const std = @import("std");

const Frame = enum { string, placeholder, rune, identifier };
const max_depth = 128;

/// Returns the offset after the closing quote, or null for an unterminated
/// literal, raw newline/NUL, or nesting beyond the fixed 128-frame limit.
/// start must point at the opening double quote (not an @ prefix).
pub fn stringEnd(bytes: []const u8, start: usize) ?usize {
    if (start >= bytes.len or bytes[start] != '"') return null;
    return scan(bytes, start + 1, .string);
}

/// Returns the matching closing brace's index. start is immediately after {.
/// Quotes, rune literals, quoted identifiers and nested braces are balanced
/// lexically; accepting a placeholder's expression grammar is the caller's job.
pub fn placeholderEnd(bytes: []const u8, start: usize) ?usize {
    return scan(bytes, start, .placeholder);
}

fn scan(bytes: []const u8, start: usize, initial: Frame) ?usize {
    var stack: [max_depth]Frame = undefined;
    stack[0] = initial;
    var depth: usize = 1;
    var i = start;
    while (i < bytes.len) {
        const c = bytes[i];
        if (c == 0 or c == '\n') return null;
        const frame = stack[depth - 1];
        if (frame != .placeholder and c == '\\') {
            i += 1;
            if (i >= bytes.len or bytes[i] == 0 or bytes[i] == '\n') return null;
            // Unicode escape braces belong to the escape, not interpolation.
            if (bytes[i] == 'u' and i + 1 < bytes.len and bytes[i + 1] == '{') {
                i += 2;
                while (i < bytes.len and std.ascii.isHex(bytes[i])) : (i += 1) {}
                if (i < bytes.len and bytes[i] == '}') i += 1;
            } else {
                i += 1;
            }
            continue;
        }
        var push: ?Frame = null;
        var close = false;
        switch (frame) {
            .string => switch (c) {
                '"' => close = true,
                '{' => push = .placeholder,
                else => {},
            },
            .identifier => close = c == '"',
            .rune => close = c == '\'',
            .placeholder => switch (c) {
                '}' => close = true,
                '{' => push = .placeholder,
                '"' => push = .string,
                '\'' => push = .rune,
                '@' => if (i + 1 < bytes.len and bytes[i + 1] == '"') {
                    push = .identifier;
                    i += 1;
                },
                else => {},
            },
        }
        if (close) {
            depth -= 1;
            if (depth == 0) return if (initial == .placeholder) i else i + 1;
        } else if (push) |next| {
            if (depth == stack.len) return null;
            stack[depth] = next;
            depth += 1;
        }
        i += 1;
    }
    return null;
}

test "quoted interpolation scanner balances strings runes escapes and identifiers" {
    for ([_][]const u8{
        "\"{\"a\"}\"",
        "\"{greet(\"Ada\")}\"",
        "\"{f('{', '}', '\\'', '\\\\', '\\u{7d}')}\"",
        "\"{\"nested {f(\"deep\")}\"}\"",
        "\"{\"\\\"\\u{7b}\\{\\}\"}\"",
        "\"{@\"unbalanced { identifier\"}\"",
        "\"\\{literal\\}\"",
    }) |bytes| {
        try std.testing.expectEqual(@as(?usize, bytes.len), stringEnd(bytes, 0));
    }
    const fragment = "f(\"}\", '{', @\"{\", {value})}suffix";
    try std.testing.expectEqual(@as(?usize, fragment.len - "}suffix".len), placeholderEnd(fragment, 0));
    // Doubled braces are nested syntax, never an alternative literal escape.
    try std.testing.expectEqual(@as(?usize, 3), placeholderEnd("{x}}", 0));
}

test "quoted interpolation scanner rejects malformed input and bounded nesting" {
    for ([_][]const u8{ "", "\"", "\"{\"x\"", "\"{'}\"", "\"x\n\"", "\"x\x00\"", "\"\\", "\"\\\n\"", "\"{\"x\n\"}\"" }) |bytes| {
        try std.testing.expectEqual(@as(?usize, null), stringEnd(bytes, 0));
    }
    try std.testing.expectEqual(@as(?usize, null), placeholderEnd("\"unterminated", 0));
    try std.testing.expectEqual(@as(?usize, null), placeholderEnd("", 1));
    var bytes: [258]u8 = undefined;
    @memset(bytes[0..128], '{');
    @memset(bytes[128..256], '}');
    try std.testing.expectEqual(@as(?usize, 255), placeholderEnd(bytes[0..256], 1));
    try std.testing.expectEqual(@as(?usize, null), placeholderEnd(bytes[0..256], 0));
    bytes[0] = '"';
    @memset(bytes[1..129], '{');
    @memset(bytes[129..257], '}');
    bytes[257] = '"';
    try std.testing.expectEqual(@as(?usize, null), stringEnd(&bytes, 0));
}
