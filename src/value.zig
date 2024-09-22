const std = @import("std");

pub const Value = union(enum) {
    bool: bool,
    number: f64,
    // int: i64,
    // uint: u64,
    // float: f64,
    string: *String,
    // function: *Function,
    // list: *List,
    // map: *Map,

    pub fn create(value: anytype) Value {
        switch (@TypeOf(value)) {
            bool => return .{ .bool = value },
            *String => return .{ .string = value },
            []u8, []const u8 => @compileError("Use createObject() instead"),
            else => switch (@typeInfo(@TypeOf(value))) {
                .ComptimeInt, .Int => return .{ .number = @floatFromInt(value) },
                .Float => return .{ .number = @floatCast(value) },
                else => @compileError("Unsupported type: " ++ @typeName(@TypeOf(value))),
            },
        }
    }

    // for strings []const u8 are duplicated but []u8 are not
    pub fn createObject(allocator: std.mem.Allocator, value: anytype) !Value {
        switch (@TypeOf(value)) {
            []u8 => return String.create(allocator, value),
            []const u8 => return String.dupe(allocator, value),
            else => return Value.create(value),
        }
    }

    pub fn dupe(self: Value, allocator: std.mem.Allocator) !Value {
        switch (self) {
            .string => return String.dupe(allocator, self.string.data),
            else => return self,
        }
    }

    pub fn destroy(self: Value, allocator: std.mem.Allocator) void {
        switch (self) {
            .string => self.string.destroy(allocator),
            else => {},
        }
    }

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
            .string => |value| try writer.print("{s}", .{value.data}),
        }
    }

    pub fn eql(self: Value, other: Value) bool {
        if (std.meta.activeTag(self) != std.meta.activeTag(other)) {
            return false;
        }
        return switch (self) {
            .bool => self.bool == other.bool,
            .number => self.number == other.number,
            .string => std.mem.eql(u8, self.string.data, other.string.data),
        };
    }
};

pub const String = struct {
    data: []const u8,

    pub fn create(allocator: std.mem.Allocator, data: []const u8) !Value {
        const string = try allocator.create(String);
        string.data = data;
        return .{ .string = string };
    }

    pub fn dupe(allocator: std.mem.Allocator, const_data: []const u8) !Value {
        const data = try allocator.dupe(u8, const_data);
        return String.create(allocator, data);
    }

    pub fn destroy(self: *String, allocator: std.mem.Allocator) void {
        allocator.free(self.data);
        allocator.destroy(self);
    }
};
