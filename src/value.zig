const std = @import("std");

pub const Value = union(enum) {
    bool: bool,
    number: f64,
    string: []const u8, // TODO: []u8

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        if (fmt.len != 0) {
            std.fmt.invalidFmtError(fmt, self);
        }
        switch (self) {
            .bool => |value| try writer.print("{}", .{value}),
            .number => |value| try writer.print("{d}", .{value}),
            .string => |value| try writer.print("{s}", .{value}),
        }
    }
};
