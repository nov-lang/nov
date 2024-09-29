const std = @import("std");
const Chunk = @import("Chunk.zig");

// TODO: move true/false/nil to keywords and make this a map to the Type enum based on Value
pub const primitives = std.StaticStringMap(void).initComptime(.{
    .{"bool"},
    .{"int"},
    .{"uint"},
    .{"float"},
    .{"string"},
    .{"false"},
    .{"true"},
    .{"nil"},
    .{"any"},
    // .{"undefined"},
    // .{"void"},
    // .{"error"},
});

pub fn isPrimitive(name: []const u8) bool {
    return primitives.has(name);
}

pub const Value = union(enum) {
    bool: bool,
    int: i64,
    uint: u64,
    float: f64,
    string: *String,
    function: *Function,
    // list: *List,
    // tuple: *Tuple,
    // map: *Map,

    pub fn create(value: anytype) Value {
        switch (@TypeOf(value)) {
            bool => return .{ .bool = value },
            *String => return .{ .string = value },
            *Function => return .{ .function = value },
            []u8, []const u8 => @compileError("Use createObject() instead"),
            else => switch (@typeInfo(@TypeOf(value))) {
                .ComptimeInt => return .{ .int = value },
                .Int => |info| switch (info.signedness) {
                    .signed => return .{ .int = value },
                    .unsigned => return .{ .uint = value },
                },
                .Float => return .{ .float = value },
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
            .bool, .int, .uint, .float => {},
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
            inline .int, .uint, .float => |value| try writer.print("{d}", .{value}),
            .string => |value| try writer.print("{s}", .{value.data}),
            .function => |value| if (value.name) |name| {
                try writer.print("<fn {s}>", .{name});
            } else {
                try writer.print("<{s}>", .{@tagName(value.kind)});
            },
        }
    }

    pub fn eql(self: Value, other: Value) bool {
        if (std.meta.activeTag(self) == std.meta.activeTag(other)) {
            return switch (self) {
                .bool => self.bool == other.bool,
                .int => self.int == other.int,
                .uint => self.uint == other.uint,
                .float => self.float == other.float,
                .string => std.mem.eql(u8, self.string.data, other.string.data),
                .function => self.function == other.function, // compare ptr
            };
        }

        switch (self) {
            inline .int, .uint => |self_int| {
                switch (other) {
                    inline .int, .uint => |other_int| return self_int == other_int,
                    .float => |other_float| return @as(f64, @floatFromInt(self_int)) == other_float,
                    else => {},
                }
            },
            .float => |self_float| {
                switch (other) {
                    inline .int, .uint => |other_int| return self_float == @as(f64, @floatFromInt(other_int)),
                    .float => |other_float| return self_float == other_float,
                    else => {},
                }
            },
            else => {},
        }

        return false;
    }
};

pub const String = struct {
    data: []const u8,

    pub fn init(allocator: std.mem.Allocator, data: []const u8) !*String {
        const string = try allocator.create(String);
        string.* = .{ .data = data };
        return string;
    }

    pub fn dupe(allocator: std.mem.Allocator, data: []const u8) !*String {
        const dupe_data = try allocator.dupe(u8, data);
        return String.init(allocator, dupe_data);
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
