const std = @import("std");
const Chunk = @import("Chunk.zig");

pub const Value = union(enum) {
    bool: bool,
    number: f64,
    // int: i64,
    // uint: u64,
    // float: f64,
    string: *String,
    function: *Function,
    // list: *List,
    // map: *Map,

    pub fn create(value: anytype) Value {
        switch (@TypeOf(value)) {
            bool => return .{ .bool = value },
            *String => return .{ .string = value },
            *Function => return .{ .function = value },
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
            []u8 => return create(try String.init(allocator, value)),
            []const u8 => return create(try String.dupe(allocator, value)),
            Function.Kind => return create(try Function.init(allocator, value, null)),
            else => return create(value),
        }
    }

    pub fn dupe(self: Value, allocator: std.mem.Allocator) !Value {
        switch (self) {
            .string => |str| return create(try String.dupe(allocator, str.data)),
            .function => unreachable,
            else => return self,
        }
    }

    pub fn destroy(self: Value, allocator: std.mem.Allocator) void {
        switch (self) {
            .bool, .number => {},
            inline else => |value| value.deinit(allocator),
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
            .function => |value| if (value.name) |name| {
                try writer.print("<fn {s}>", .{name});
            } else {
                try writer.print("<{s}>", .{@tagName(value.kind)});
            },
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
            .function => self.function == other.function, // compare ptr
        };
    }
};

pub const String = struct {
    data: []const u8,

    pub fn init(allocator: std.mem.Allocator, data: []const u8) !*String {
        const string = try allocator.create(String);
        string.* = .{ .data = data };
        return string;
    }

    pub fn dupe(allocator: std.mem.Allocator, const_data: []const u8) !*String {
        const data = try allocator.dupe(u8, const_data);
        return String.init(allocator, data);
    }

    pub fn deinit(self: *String, allocator: std.mem.Allocator) void {
        allocator.free(self.data);
        allocator.destroy(self);
    }
};

pub const Function = struct {
    arity: u32,
    chunk: Chunk,
    name: ?[]const u8,
    kind: Kind,

    pub const Kind = enum {
        function,
        script,
    };

    pub fn init(allocator: std.mem.Allocator, kind: Kind, name: ?[]const u8) !*Function {
        const function = try allocator.create(Function);
        function.* = .{
            .arity = 0,
            .chunk = Chunk.init(allocator),
            // .name = if (name) |n| try allocator.dupe(u8, n) else null,
            .name = name,
            .kind = kind,
        };
        return function;
    }

    pub fn deinit(self: *Function, allocator: std.mem.Allocator) void {
        self.chunk.deinit();
        allocator.destroy(self);
    }
};
