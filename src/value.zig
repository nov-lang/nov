const std = @import("std");
const Chunk = @import("Chunk.zig");
const big_int = std.math.big.int;

// make this a map to the Type enum based on Value?
// void/nil type name and value is `()`
pub const primitives = std.StaticStringMap(void).initComptime(.{
    .{"bool"},
    .{"int"},
    .{"uint"},
    .{"float"},
    .{"string"},
    // .{"any"},
    // .{"nil"}, // replaced by Option(T)
    // .{"undefined"}, // should be catched during compile time
    // .{"error"},
    // .{"()"},
    // .{"type"},
});

pub fn isPrimitive(name: []const u8) bool {
    return primitives.has(name);
}

pub const Value = union {
    undefined: void, // will cause crash when using a undefined value in ReleaseSafe build
    bool: bool,
    int: i63, // used by int and uint, look at isObj to understand why it's an i63
    float: f64,
    obj: *Object,

    const obj_mask: u64 = 0xfffc000000000000;

    pub fn create(value: anytype) Value {
        return switch (@TypeOf(value)) {
            bool => .{ .bool = value },
            *Object => .{ .obj = @ptrFromInt(obj_mask | @intFromPtr(value)) },
            else => |T| switch (@typeInfo(T)) {
                .comptime_int => if (value > std.math.maxInt(i63))
                    .{ .int = @bitCast(@as(u63, value)) }
                else
                    .{ .int = value },
                .int => |info| switch (info.signedness) {
                    .signed => .{ .int = value },
                    .unsigned => .{ .int = @bitCast(@as(u63, value)) },
                },
                .comptime_float, .float => .{ .float = value },
                else => @compileError("Unsupported type: " ++ @typeName(T)),
            },
        };
    }

    // TODO: from previous Value implem
    // for strings []const u8 are duplicated but []u8 are not
    // pub fn createObject(allocator: std.mem.Allocator, value: anytype) !Value {
    //     switch (@TypeOf(value)) {
    //         []u8 => return create(try String.init(allocator, value)),
    //         []const u8 => return create(try String.dupe(allocator, value)),
    //         Function.Kind => return create(try Function.init(allocator, value, null)),
    //         else => return create(value),
    //     }
    // }

    // we could define `Value` as an extern union to do `@as(u64, @bitCast(value))`
    // instead but that would remove the runtime safety checks for using not active field
    // or especially the `undefined` field
    pub fn isObj(self: Value) bool {
        return @as(*const u64, @ptrCast(&self)).* & obj_mask == obj_mask;
    }

    /// `b`: bool
    /// `i`: int decimal, `ix`: int hexadecimal, `io`: int octal, `ib`: int binary
    /// `u`: uint decimal, `ux`: uint hexadecimal, `uo`: uint octal, `ub`: uint binary
    /// `f`: float decimal, `fx`: float hexadecimal, `fb`: float binary, `fe`: float scientific
    /// `o`: obj
    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        if (comptime std.mem.eql(u8, fmt, "b")) {
            return std.fmt.formatBuf(if (self.bool) "true" else "false", options, writer);
        } else if (comptime fmt.len > 0 and fmt[0] == 'i') {
            return std.fmt.formatIntValue(self.int, fmt[1..], options, writer);
        } else if (comptime fmt.len > 0 and fmt[0] == 'u') {
            return std.fmt.formatIntValue(@as(u64, @bitCast(self.int)), fmt[1..], options, writer);
        } else if (comptime fmt.len > 0 and fmt[0] == 'f') {
            return formatFloatValue(self.float, fmt[1..], options, writer);
        } else if (comptime std.mem.eql(u8, fmt, "o")) {
            return self.obj.format("", options, writer);
        } else {
            std.fmt.invalidFmtError(fmt, self);
        }
    }

    comptime {
        if (std.debug.runtime_safety == false) {
            std.debug.assert(@sizeOf(Value) == 8);
        }
    }
};

pub const ValueType = enum {
    bool,
    int,
    uint,
    float,
    string,
    function,
    array,
    map,
    @"struct",
    @"enum",
    @"union",

    pub fn isObj(self: ValueType) bool {
        return switch (self) {
            .bool,
            .int,
            .uint,
            .float,
            => false,
            .string,
            .function,
            .array,
            .map,
            .@"struct",
            .@"enum",
            .@"union",
            => true,
        };
    }
};

pub const Object = packed struct(u64) {
    tag: Tag,
    is_marked: bool = false,
    next: u55 = 0,

    pub const Tag = enum(u8) {
        // see std.builtin.Type for Fn, Struct, Enum, Union
        string,
        function,
        array,
        map,
        // TODO: each user object has a vtable?
        @"struct", // todo: allow for struct[field name] access or @field()?
        @"enum",
        @"union",

        pub fn Type(comptime tag: Tag) type {
            return comptime switch (tag) {
                .string => String,
                .function => Function,
                .array => Array,
                .map => Map,
                // .@"struct" => Struct,
                // .@"enum" => Enum,
                // .@"union" => Union,
                else => @compileError("TODO: Implement " ++ @tagName(tag)),
            };
        }
    };

    // pub fn as(self: *Object, comptime T: type) *T {
    pub fn as(self: *Object, comptime tag: Object.Tag) *tag.Type() {
        std.debug.assert(self.tag == tag);
        return @alignCast(@fieldParentPtr("object", self));
    }

    pub fn getNext(self: Object) ?*Object {
        return if (self.next == 0) null else @ptrFromInt(self.next);
    }

    pub fn setNext(self: *Object, next: ?*const Object) void {
        self.next = if (next) |n| @intCast(@intFromPtr(n)) else 0;
    }

    /// not implemented yet
    /// `s`: string
    /// `f`: function
    /// `a`: array
    /// `m`: map
    /// `s`: struct // TODO: duplicate with `s` for string
    /// `e`: enum
    /// `u`: union
    pub fn format(
        self: *Object,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        // TODO: invalidFmtError prevents from having custom formats on different objects
        return switch (self.tag) {
            .string => self.as(.string).format(fmt, options, writer),
            .function => self.as(.function).format(fmt, options, writer),
            .array => self.as(.array).format(fmt, options, writer),
            .map => self.as(.map).format(fmt, options, writer),
            else => unreachable, // TODO
        };

        // if (comptime std.mem.eql(u8, fmt, "s")) {
        //     std.debug.assert(self.tag == .string);
        //     return self.as(.string).format("", options, writer);
        // } else if (comptime std.mem.eql(u8, fmt, "f")) {
        //     std.debug.assert(self.tag == .function);
        //     return self.as(.Function).format("", options, writer);
        // } else {
        //     std.fmt.invalidFmtError(fmt, self);
        // }
    }
};

pub const String = struct {
    object: Object = .{ .tag = .string },
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

    pub fn obj(self: *String) *Object {
        return &self.object;
    }

    pub fn value(self: *String) Value {
        return Value.create(&self.object);
    }

    pub fn format(
        self: String,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        if (fmt.len == 0 or comptime std.mem.eql(u8, fmt, "s")) {
            return std.fmt.formatBuf(self.data, options, writer);
        } else {
            // std.fmt.invalidFmtError(fmt, self);
        }
    }
};

pub const Function = struct {
    object: Object = .{ .tag = .function },
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

    pub fn obj(self: *Function) *Object {
        return &self.object;
    }

    pub fn value(self: *Function) Value {
        return Value.create(&self.object);
    }

    pub fn format(
        self: Function,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        _ = options;
        if (fmt.len != 0) {
            // return std.fmt.invalidFmtError(fmt, self);
        }
        // TODO: name: args types -> return type
        if (self.name) |name| {
            try writer.print("<fn {s}>", .{name});
        } else {
            try writer.print("<{s}>", .{@tagName(self.kind)});
        }
    }
};

/// type: []T
/// value: [1, 2, 3, 4]
/// access: array[n] where n is an int
/// each values have the same type
pub const Array = struct {
    object: Object = .{ .tag = .array },
    values: std.ArrayListUnmanaged(Value) = .{},
    type: ValueType,

    pub fn init(allocator: std.mem.Allocator, value_type: ValueType) !*Array {
        const array = try allocator.create(Array);
        array.* = .{ .type = value_type };
        return array;
    }

    pub fn deinit(self: *Array, allocator: std.mem.Allocator) void {
        self.values.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn obj(self: *Array) *Object {
        return &self.object;
    }

    pub fn value(self: *Array) Value {
        return Value.create(&self.object);
    }

    pub fn format(
        self: Array,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        // TODO: use fmt to format each values
        // can't use type because it's runtime known
        // or have formatValue function that takes a tag instead of a fmt
        _ = fmt;
        try writer.print("[");
        for (self.values.items) |val| {
            try val.format("i", options, writer);
            try writer.print(", ");
        }
        try writer.print("]");
    }
};

/// type: [K]V
/// value: {1: 2, 3: 4} // idk
///        [1: 2, 3: 4]
/// access: map[k] where k is a key of type K
pub const Map = struct {
    object: Object = .{ .tag = .map },
    map: std.AutoHashMapUnmanaged(Value, Value) = .{},
    key_type: ValueType,
    value_type: ValueType,

    pub fn init(allocator: std.mem.Allocator, key_type: ValueType, value_type: ValueType) !*Map {
        const map = try allocator.create(Map);
        map.* = .{ .key_type = key_type, .value_type = value_type };
        return map;
    }

    pub fn deinit(self: *Map, allocator: std.mem.Allocator) void {
        self.map.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn obj(self: *Map) *Object {
        return &self.object;
    }

    pub fn value(self: *Map) Value {
        return Value.create(&self.object);
    }

    pub fn format(
        self: Map,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        _ = self;
        _ = fmt;
        _ = options;
        // TODO
    }
};

// extracted from std.fmt.formatFloatValue
fn formatFloatValue(
    value: anytype,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    var buf: [std.fmt.format_float.bufferSize(.decimal, f64)]u8 = undefined;

    if (fmt.len == 0 or comptime std.mem.eql(u8, fmt, "d")) {
        const s = std.fmt.formatFloat(&buf, value, .{
            .mode = .decimal,
            .precision = options.precision,
        }) catch |err| switch (err) {
            error.BufferTooSmall => "(float)",
        };
        return std.fmt.formatBuf(s, options, writer);
    } else if (comptime std.mem.eql(u8, fmt, "e")) {
        const s = std.fmt.formatFloat(&buf, value, .{
            .mode = .scientific,
            .precision = options.precision,
        }) catch |err| switch (err) {
            error.BufferTooSmall => "(float)",
        };
        return std.fmt.formatBuf(s, options, writer);
    } else if (comptime std.mem.eql(u8, fmt, "x")) {
        var buf_stream = std.io.fixedBufferStream(&buf);
        std.fmt.formatFloatHexadecimal(value, options, buf_stream.writer()) catch |err| switch (err) {
            error.NoSpaceLeft => unreachable,
        };
        return std.fmt.formatBuf(buf_stream.getWritten(), options, writer);
    } else if (comptime std.mem.eql(u8, fmt, "b")) {
        return std.fmt.formatInt(@as(u64, @bitCast(value)), 2, .lower, options, writer);
    } else {
        std.fmt.invalidFmtError(fmt, value);
    }
}

test "Value.isObj" {
    const expect = std.testing.expect;
    var o: Object = undefined;
    try expect(Value.create(10).isObj() == false);
    try expect(Value.create(-1.0).isObj() == false);
    try expect(Value.create(&o).isObj() == true);
    try expect((Value{ .undefined = {} }).isObj() == false);
    try expect(Value.create(true).isObj() == false);
    try expect(Value.create(false).isObj() == false);
    try expect(Value.create(std.math.nan(f64)).isObj() == false);
    try expect(Value.create(-std.math.nan(f64)).isObj() == false);
}
