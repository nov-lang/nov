const std = @import("std");
const Ast = @import("Ast.zig");
const Parser = @import("Parser.zig");
const Source = @This();

/// Compilation-local identity. Zero is also the standalone Module identity.
pub const Id = enum(u32) { _ };
pub const Span = struct {
    source: Id = @enumFromInt(0),
    start: u32,
    end: u32,
};

allocator: std.mem.Allocator,
id: Id,
path: []const u8,
ast: Ast,

pub const InitError = std.mem.Allocator.Error || error{ InvalidSource, SourceTooLarge };

/// The AST borrows the sentinel-terminated bytes owned here. Moving Source is
/// safe; copying it is not an ownership transfer. No semantic data lives here.
pub fn init(allocator: std.mem.Allocator, id: Id, path: []const u8, text: []const u8) InitError!Source {
    if (text.len > std.math.maxInt(u32)) return error.SourceTooLarge;
    if (std.mem.indexOfScalar(u8, text, 0) != null) return error.InvalidSource;
    const owned_path = try allocator.dupe(u8, path);
    errdefer allocator.free(owned_path);
    const owned_text = try allocator.dupeZ(u8, text);
    errdefer allocator.free(owned_text);
    return .{
        .allocator = allocator,
        .id = id,
        .path = owned_path,
        .ast = try Parser.parse(allocator, owned_text),
    };
}

pub fn deinit(self: *Source) void {
    const text = self.ast.source;
    self.ast.deinit(self.allocator);
    self.allocator.free(text);
    self.allocator.free(self.path);
    self.* = undefined;
}

pub fn slice(self: *const Source, span: Span) ?[]const u8 {
    if (span.source != self.id or span.start > span.end or span.end > self.ast.source.len) return null;
    return self.ast.source[span.start..span.end];
}

pub fn tokenSpan(self: *const Source, token: Ast.TokenIndex) Span {
    const start = self.ast.tokens.items(.start)[token];
    return .{ .source = self.id, .start = start, .end = start + @as(u32, @intCast(self.ast.tokenSlice(token).len)) };
}

test "source ranges cannot address another AST with equal offsets" {
    var source = try Source.init(std.testing.allocator, @enumFromInt(3), "source.nov", "let value = true;");
    defer source.deinit();
    const span = source.tokenSpan(1);
    try std.testing.expectEqualStrings("value", source.slice(span).?);
    try std.testing.expect(source.slice(.{ .start = span.start, .end = span.end }) == null);
}
