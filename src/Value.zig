const std = @import("std");
const Chunk = @import("Chunk.zig");
const big_int = std.math.big.int;

const Value = @This();

raw: u64,

// make this a map to the Type enum based on Value?
// void/nil type name and value is `()`
pub const primitives = std.StaticStringMap(void).initComptime(.{
    .{"bool"},
    .{"int"},
    .{"uint"},
    .{"float"},
    .{"string"},
    .{"any"},
    // .{"nil"},
    // .{"undefined"},
    // .{"error"},
    // .{"()"},
});

pub fn isPrimitive(name: []const u8) bool {
    return primitives.has(name);
}

// TODO: support 32-bit systems
const Tag = enum(u3) {
    bool = 0b000,
    int = 0b001,
    uint = 0b010,
    // nil = 0b011,
    // undefined = 0b100,
    obj, // uses ptr_mask
    float, // uses whole raw

    fn mask(comptime self: Tag) u64 {
        return comptime switch (self) {
            .obj => ptr_mask,
            .float => @compileError("Float tag has no mask"),
            else => qnan | @as(u64, @intFromEnum(self)) << 32,
        };
    }

    fn Type(comptime self: Tag) type {
        return comptime switch (self) {
            .bool => bool,
            .int => i32,
            .uint => u32,
            .obj => *Object,
            .float => f64,
        };
    }
};

const sign_mask: u64 = 1 << 63;
const qnan: u64 = 0x7ffc000000000000;
const ptr_mask: u64 = qnan | sign_mask;
const tag_mask: u64 = ptr_mask | (((1 << 3) - 1) << 32);

const false_val: Value = .{ .raw = Tag.bool.mask() };
const true_val: Value = .{ .raw = Tag.bool.mask() | 1 };

pub fn create(value: anytype) Value {
    return switch (@TypeOf(value)) {
        bool => if (value) true_val else false_val,
        i32 => .{ .raw = Tag.int.mask() | @as(u32, @bitCast(value)) },
        u32 => .{ .raw = Tag.uint.mask() | value },
        f64 => .{ .raw = @bitCast(value) },
        *Object => .{ .raw = Tag.obj.mask() | @intFromPtr(value) },
        // []u8, []const u8 => @compileError("Use createObject() instead"),
        else => |T| switch (@typeInfo(T)) {
            .ComptimeInt => if (value < 0)
                .{ .raw = Tag.int.mask() | @as(u32, @bitCast(@as(i32, value))) }
            else
                .{ .raw = Tag.uint.mask() | @as(u32, value) },
            .ComptimeFloat => .{ .raw = @bitCast(@as(f64, value)) },
            else => @compileError("Unsupported type: " ++ @typeName(T)),
        },
    };
}

// TODO
// for strings []const u8 are duplicated but []u8 are not
// pub fn createObject(allocator: std.mem.Allocator, value: anytype) !Value {
//     switch (@TypeOf(value)) {
//         []u8 => return create(try String.init(allocator, value)),
//         []const u8 => return create(try String.dupe(allocator, value)),
//         Function.Kind => return create(try Function.init(allocator, value, null)),
//         else => return create(value),
//     }
// }

// TODO
// pub fn destroy(self: Value, allocator: std.mem.Allocator) void {
//     switch (self) {
//         .bool, .int, .uint, .float => {},
//         inline else => |value| value.deinit(allocator),
//     }
// }

// TODO: replace Tag with type and use Object.as()?
pub fn as(self: Value, comptime tag: Tag) tag.Type() {
    return switch (tag) {
        .bool => self.raw == true_val.raw,
        .int, .uint => @bitCast(@as(u32, @truncate(self.raw))),
        .float => @bitCast(self.raw),
        .obj => @ptrFromInt(self.raw & ~ptr_mask),
    };
}

pub fn is(self: Value, comptime tag: Tag) bool {
    return switch (tag) {
        .bool, .int, .uint => self.raw & tag_mask == tag.mask(),
        .obj => self.raw & ptr_mask == tag.mask(),
        .float => self.raw & qnan != qnan,
    };
}

pub fn format(
    self: Value,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    if (fmt.len == 0) {
        if (self.is(.bool)) {
            return writer.print("{}", .{self.as(.bool)});
        } else if (self.is(.obj)) {
            return self.as(.obj).format("", options, writer);
        } else if (self.is(.int)) {
            return writer.print("{d}", .{self.as(.int)});
        } else if (self.is(.uint)) {
            return writer.print("{d}", .{self.as(.uint)});
        } else if (self.is(.float)) {
            return writer.print("{d}", .{self.as(.float)});
        } else {
            unreachable;
        }
    }

    const actual_fmt = comptime blk: {
        const start = std.mem.indexOfScalar(u8, fmt, '(') orelse break :blk "";
        if (fmt[fmt.len - 1] != ')') {
            std.fmt.invalidFmtError(fmt, self);
        }
        break :blk fmt[start + 1 .. fmt.len - 1];
    };

    if (comptime std.mem.eql(u8, fmt, "bool")) {
        if (actual_fmt.len != 0) {
            std.fmt.invalidFmtError(fmt, self);
        }
        std.debug.assert(self.is(.bool));
        return std.fmt.formatBuf(if (self.as(.bool)) "true" else "false", options, writer);
    } else if (comptime std.mem.startsWith(u8, fmt, "obj")) {
        std.debug.assert(self.is(.obj));
        return self.as(.obj).format(actual_fmt, options, writer);
    } else if (comptime std.mem.startsWith(u8, fmt, "int")) {
        std.debug.assert(self.is(.int));
        return std.fmt.formatIntValue(self.as(.int), actual_fmt, options, writer);
    } else if (comptime std.mem.startsWith(u8, fmt, "uint")) {
        std.debug.assert(self.is(.uint));
        return std.fmt.formatIntValue(self.as(.uint), actual_fmt, options, writer);
    } else if (comptime std.mem.startsWith(u8, fmt, "float")) {
        std.debug.assert(self.is(.float));
        return formatFloatValue(self.as(.float), actual_fmt, options, writer);
    } else {
        std.fmt.invalidFmtError(fmt, self);
    }
}

pub fn eql(comptime tag: Tag, a: Value, b: Value) bool {
    std.debug.assert(a.is(tag) and b.is(tag));
    if (tag == .obj) {
        return false; // TODO Object.eql(a.as(.obj), b.as(.obj));
    }
    return a.as(tag) == b.as(tag);
    // if (std.meta.activeTag(self) == std.meta.activeTag(other)) {
    //     return switch (self) {
    //         .bool => self.bool == other.bool,
    //         .int => self.int == other.int,
    //         .uint => self.uint == other.uint,
    //         .float => self.float == other.float,
    //         .string => std.mem.eql(u8, self.string.data, other.string.data),
    //         .function => self.function == other.function, // compare ptr
    //     };
    // }

    // switch (self) {
    //     inline .int, .uint => |self_int| {
    //         switch (other) {
    //             inline .int, .uint => |other_int| return self_int == other_int,
    //             .float => |other_float| return @as(f64, @floatFromInt(self_int)) == other_float,
    //             else => {},
    //         }
    //     },
    //     .float => |self_float| {
    //         switch (other) {
    //             inline .int, .uint => |other_int| return self_float == @as(f64, @floatFromInt(other_int)),
    //             .float => |other_float| return self_float == other_float,
    //             else => {},
    //         }
    //     },
    //     else => {},
    // }

    // return false;
}

pub const Object = packed struct(u64) {
    tag: Object.Tag,
    is_marked: bool = false,
    next: u55 = 0,

    pub const Tag = enum(u8) {
        string, // String
        function, // Function
        list, // List // TODO: [a, b, c, d] same as OCaml, type is `InnerType list` or `List<InnerType>` or idk
        tuple, // Tuple // TODO: (a, b, c, d) same as OCaml
        map, // Map
        big_int, // BigInt

        pub fn Type(comptime self: @This()) type {
            return comptime switch (self) {
                .string => String,
                .function => Function,
                .big_int => BigInt,
                else => @compileError("Unsupported tag"),
            };
        }
    };

    // pub fn as(self: *Object, comptime T: type) *T {
    pub fn as(self: *Object, comptime tag: Object.Tag) *tag.Type() {
        return @alignCast(@fieldParentPtr("object", self));
    }

    pub fn setNext(self: *Object, next: ?*const Object) void {
        self.next = if (next) |n| @intCast(@intFromPtr(n)) else 0;
    }

    pub fn getNext(self: *Object) ?*Object {
        return if (self.next == 0) null else @ptrFromInt(self.next);
    }

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
            .big_int => self.as(.big_int).format(fmt, options, writer),
            else => unreachable, // TODO
        };

        // if (comptime std.mem.eql(u8, fmt, "s") or
        //     std.mem.eql(u8, fmt, "str") or
        //     std.mem.eql(u8, fmt, "string"))
        // {
        //     std.debug.assert(self.tag == .string);
        //     return self.as(String).format("", options, writer);
        // }

        // if (comptime std.mem.eql(u8, fmt, "f") or
        //     std.mem.eql(u8, fmt, "fn") or
        //     std.mem.eql(u8, fmt, "function"))
        // {
        //     std.debug.assert(self.tag == .function);
        //     return self.as(Function).format("", options, writer);
        // }

        // std.fmt.invalidFmtError(fmt, self);
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

// TODO: 3 possibilities
// - always use big int for integers (nan boxing feels kinda useless)
// - allow seemless conversion between int and big int (best but most complex)
// - treat big int as a separate type (easiest but prone to overflow errors when using int)
pub const BigInt = struct {
    object: Object = .{ .tag = .big_int },
    mutable: big_int.Mutable,

    pub fn init(allocator: std.mem.Allocator) !BigInt {
        const managed = try big_int.Managed.init(allocator);
        return .{ .mutable = managed.toMutable() };
    }

    pub fn deinit(self: *BigInt, allocator: std.mem.Allocator) void {
        allocator.free(self.value.limbs);
        allocator.destroy(self);
    }

    pub fn obj(self: *BigInt) *Object {
        return &self.object;
    }

    pub fn value(self: *BigInt) Value {
        return Value.create(&self.object);
    }

    pub fn orderScalar(self: BigInt, scalar: anytype) std.math.Order {
        return self.mutable.toConst().order(.{
            .limbs = &.{@abs(scalar)},
            .positive = scalar >= 0,
        });
    }

    pub fn eqlScalar(self: BigInt, scalar: anytype) bool {
        return self.orderScalar(scalar) == .eq;
    }

    pub fn format(
        self: BigInt,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        return self.mutable.toConst().format(fmt, options, writer);
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

    if (fmt.len == 0 or comptime std.mem.eql(u8, fmt, "e")) {
        const s = std.fmt.formatFloat(&buf, value, .{
            .mode = .scientific,
            .precision = options.precision,
        }) catch |err| switch (err) {
            error.BufferTooSmall => "(float)",
        };
        return std.fmt.formatBuf(s, options, writer);
    } else if (comptime std.mem.eql(u8, fmt, "d")) {
        const s = std.fmt.formatFloat(&buf, value, .{
            .mode = .decimal,
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
    } else {
        std.fmt.invalidFmtError(fmt, value);
    }
}

test Value {
    const expectEqual = std.testing.expectEqual;
    const expect = std.testing.expect;

    try expectEqual(true, Value.create(true).as(.bool));
    try expectEqual(false, Value.create(false).as(.bool));
    try expectEqual(5.3, Value.create(5.3).as(.float));
    try expectEqual(-32, Value.create(-32).as(.int));
    try expectEqual(std.math.maxInt(u32), Value.create(std.math.maxInt(u32)).as(.uint));
    try expectEqual(std.math.maxInt(i32), Value.create(std.math.maxInt(i32)).as(.int));
    try expectEqual(-1, Value.create(std.math.maxInt(u32)).as(.int));
    var obj: Object = undefined;
    try expectEqual(&obj, Value.create(&obj).as(.obj));

    try expect(Value.create(true).is(.bool));
    try expect(Value.create(false).is(.bool));
    try expect(Value.create(5.3).is(.float));
    try expect(Value.create(-32).is(.int));
    try expect(Value.create(std.math.maxInt(u32)).is(.uint));
    try expect(Value.create(@as(i32, std.math.maxInt(i32))).is(.int));
    try expect(!Value.create(@as(i32, std.math.maxInt(i32))).is(.uint));
    try expect(Value.create(&obj).is(.obj));

    const inf = Value.create(std.math.inf(f64));
    try expect(inf.is(.float));
    try expect(!inf.is(.int));
    try expect(!inf.is(.uint));
    try expect(!inf.is(.bool));
    try expect(!inf.is(.obj));

    const nan = Value.create(std.math.nan(f64));
    try expect(nan.is(.float));
    try expect(!nan.is(.int));
    try expect(!nan.is(.uint));
    try expect(!nan.is(.bool));
    try expect(!nan.is(.obj));
}
