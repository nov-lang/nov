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
});

pub fn isPrimitive(name: []const u8) bool {
    return primitives.has(name);
}

pub const Value = union {
    undefined: void, // will cause crash when using a undefined value in ReleaseSafe build
    bool: bool,
    int: i64, // used by int and uint
    float: f64,
    obj: *Object,

    pub fn create(value: anytype) Value {
        return switch (@TypeOf(value)) {
            bool => .{ .bool = value },
            *Object => .{ .obj = value },
            else => |T| switch (@typeInfo(T)) {
                .ComptimeInt => if (value > std.math.maxInt(i64))
                    .{ .int = @bitCast(@as(u64, value)) }
                else
                    .{ .int = value },
                .Int => |info| switch (info.signedness) {
                    .signed => .{ .int = value },
                    .unsigned => .{ .int = @bitCast(@as(u64, value)) },
                },
                .ComptimeFloat, .Float => .{ .float = value },
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
};

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

        pub fn Type(comptime tag: Tag) type {
            return comptime switch (tag) {
                .string => String,
                .function => Function,
                .big_int => BigInt,
                else => @compileError("Unsupported tag: " ++ @tagName(tag)),
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
