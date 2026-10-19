const std = @import("std");
const Ir = @This();

// This is the owned type/constant foundation, not yet an executable IR. Integer
// widths describe resolved values; no literal defaults or overflow policy live here.
allocator: std.mem.Allocator,
types: std.ArrayList(Type) = .empty,
constants: std.ArrayList(Constant) = .empty,

pub const TypeId = enum(u32) { _ };
pub const ConstId = enum(u32) { _ };

pub const Type = union(enum) {
    void,
    boolean,
    integer: Integer,
    function: Function,

    pub const Integer = struct {
        signedness: std.builtin.Signedness,
        bits: u16,
    };

    pub const Function = struct {
        /// Input may be borrowed; internType copies this slice on insertion.
        params: []const TypeId,
        result: TypeId,
    };
};

pub const Constant = struct {
    ty: TypeId,
    value: Value,

    pub const Value = union(enum) {
        void,
        boolean: bool,
        signed_integer: i128,
        unsigned_integer: u128,
    };
};

pub const ValidationError = error{
    InvalidTypeId,
    InvalidIntegerWidth,
    VoidParameter,
    NonConstantType,
    ConstantTypeMismatch,
    IntegerOutOfRange,
};
pub const InternError = ValidationError || std.mem.Allocator.Error || error{TooManyEntries};
pub const VerifyError = ValidationError || error{ NonCanonicalType, NonCanonicalConstant };

pub fn init(allocator: std.mem.Allocator) Ir {
    return .{ .allocator = allocator };
}

pub fn deinit(self: *Ir) void {
    for (self.types.items) |ty| {
        switch (ty) {
            .function => |function| self.allocator.free(function.params),
            else => {},
        }
    }
    self.types.deinit(self.allocator);
    self.constants.deinit(self.allocator);
    self.* = undefined;
}

/// Handles survive pool growth. Returned function parameter slices borrow this IR.
pub fn getType(self: *const Ir, id: TypeId) ?Type {
    const index = @intFromEnum(id);
    if (index >= self.types.items.len) return null;
    return self.types.items[index];
}

pub fn getConstant(self: *const Ir, id: ConstId) ?Constant {
    const index = @intFromEnum(id);
    if (index >= self.constants.items.len) return null;
    return self.constants.items[index];
}

pub fn internType(self: *Ir, ty: Type) InternError!TypeId {
    try self.validateType(ty, self.types.items.len);
    for (self.types.items, 0..) |existing, i| {
        if (typeEql(existing, ty)) return @enumFromInt(i);
    }
    if (self.types.items.len == std.math.maxInt(u32)) return error.TooManyEntries;
    try self.types.ensureUnusedCapacity(self.allocator, 1);
    const owned: Type = switch (ty) {
        .function => |function| .{ .function = .{
            .params = try self.allocator.dupe(TypeId, function.params),
            .result = function.result,
        } },
        else => ty,
    };
    const id: TypeId = @enumFromInt(self.types.items.len);
    self.types.appendAssumeCapacity(owned);
    return id;
}

pub fn internConstant(self: *Ir, constant: Constant) InternError!ConstId {
    try self.validateConstant(constant);
    for (self.constants.items, 0..) |existing, i| {
        if (std.meta.eql(existing, constant)) return @enumFromInt(i);
    }
    if (self.constants.items.len == std.math.maxInt(u32)) return error.TooManyEntries;
    const id: ConstId = @enumFromInt(self.constants.items.len);
    try self.constants.append(self.allocator, constant);
    return id;
}

/// Checks references, resolved value ranges and canonical identities. Deliberately
/// simple linear interning/quadratic duplicate checks suit the initial small pool.
pub fn verify(self: *const Ir) VerifyError!void {
    for (self.types.items, 0..) |ty, i| {
        try self.validateType(ty, i);
        for (self.types.items[0..i]) |previous| {
            if (typeEql(previous, ty)) return error.NonCanonicalType;
        }
    }
    for (self.constants.items, 0..) |constant, i| {
        try self.validateConstant(constant);
        for (self.constants.items[0..i]) |previous| {
            if (std.meta.eql(previous, constant)) return error.NonCanonicalConstant;
        }
    }
}

fn validateType(self: *const Ir, ty: Type, before: usize) ValidationError!void {
    switch (ty) {
        .void, .boolean => {},
        .integer => |integer| {
            if (integer.bits == 0 or integer.bits > 128) return error.InvalidIntegerWidth;
        },
        .function => |function| {
            // Types form a DAG; recursive function values do not require cycles
            // in their signature types. Only refer to previously interned types.
            if (@intFromEnum(function.result) >= before) return error.InvalidTypeId;
            for (function.params) |param| {
                if (@intFromEnum(param) >= before) return error.InvalidTypeId;
                if (self.getType(param).? == .void) return error.VoidParameter;
            }
        },
    }
}

fn validateConstant(self: *const Ir, constant: Constant) ValidationError!void {
    const ty = self.getType(constant.ty) orelse return error.InvalidTypeId;
    try self.validateType(ty, self.types.items.len);
    switch (ty) {
        .void => if (constant.value != .void) return error.ConstantTypeMismatch,
        .boolean => if (constant.value != .boolean) return error.ConstantTypeMismatch,
        .function => return error.NonConstantType,
        .integer => |integer| switch (integer.signedness) {
            .signed => {
                const value = switch (constant.value) {
                    .signed_integer => |value| value,
                    else => return error.ConstantTypeMismatch,
                };
                if (integer.bits < 128) {
                    const limit = @as(i128, 1) << @as(u7, @intCast(integer.bits - 1));
                    if (value < -limit or value >= limit) return error.IntegerOutOfRange;
                }
            },
            .unsigned => {
                const value = switch (constant.value) {
                    .unsigned_integer => |value| value,
                    else => return error.ConstantTypeMismatch,
                };
                if (integer.bits < 128 and value >= (@as(u128, 1) << @as(u7, @intCast(integer.bits)))) {
                    return error.IntegerOutOfRange;
                }
            },
        },
    }
}

fn typeEql(a: Type, b: Type) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .void, .boolean => true,
        .integer => |integer| std.meta.eql(integer, b.integer),
        .function => |function| function.result == b.function.result and
            std.mem.eql(TypeId, function.params, b.function.params),
    };
}

test "types and constants have canonical typed identities" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const boolean = try ir.internType(.boolean);
    const signed = try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 32 } });
    const unsigned = try ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = 32 } });
    try std.testing.expectEqual(boolean, try ir.internType(.boolean));
    try std.testing.expectEqual(signed, try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 32 } }));
    try std.testing.expect(signed != unsigned);
    const one = try ir.internConstant(.{ .ty = signed, .value = .{ .signed_integer = 1 } });
    try std.testing.expectEqual(one, try ir.internConstant(.{ .ty = signed, .value = .{ .signed_integer = 1 } }));
    const unsigned_one = try ir.internConstant(.{ .ty = unsigned, .value = .{ .unsigned_integer = 1 } });
    try std.testing.expect(one != unsigned_one);
    _ = try ir.internConstant(.{ .ty = boolean, .value = .{ .boolean = true } });
    const void_type = try ir.internType(.void);
    _ = try ir.internConstant(.{ .ty = void_type, .value = .void });
    try std.testing.expectEqual(@as(i128, 1), ir.getConstant(one).?.value.signed_integer);
    try std.testing.expect(ir.getType(@enumFromInt(999)) == null);
    try std.testing.expect(ir.getConstant(@enumFromInt(999)) == null);
    try ir.verify();
}

test "function types own parameter lists and preserve order" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const boolean = try ir.internType(.boolean);
    const integer = try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 64 } });
    var params = [_]TypeId{ boolean, integer };
    const function = try ir.internType(.{ .function = .{ .params = &params, .result = integer } });
    params[0] = integer;
    try std.testing.expectEqual(boolean, ir.getType(function).?.function.params[0]);
    try std.testing.expectEqual(function, try ir.internType(.{ .function = .{ .params = &.{ boolean, integer }, .result = integer } }));
    const reversed = try ir.internType(.{ .function = .{ .params = &.{ integer, boolean }, .result = integer } });
    try std.testing.expect(function != reversed);
    const other_result = try ir.internType(.{ .function = .{ .params = &.{ boolean, integer }, .result = boolean } });
    try std.testing.expect(function != other_result);
    _ = try ir.internType(.{ .function = .{ .params = &.{function}, .result = function } });
    _ = try ir.internType(.{ .function = .{ .params = &.{}, .result = boolean } });
    try ir.verify();
}

test "integer constants enforce explicit signedness and width" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    for ([_]u16{ 1, 8, 16, 32, 64, 128 }) |bits| {
        const signed = try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = bits } });
        const unsigned = try ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = bits } });
        const signed_max: i128 = if (bits == 128) std.math.maxInt(i128) else (@as(i128, 1) << @as(u7, @intCast(bits - 1))) - 1;
        const signed_min: i128 = -signed_max - 1;
        const unsigned_max: u128 = if (bits == 128) std.math.maxInt(u128) else (@as(u128, 1) << @as(u7, @intCast(bits))) - 1;
        _ = try ir.internConstant(.{ .ty = signed, .value = .{ .signed_integer = signed_min } });
        _ = try ir.internConstant(.{ .ty = signed, .value = .{ .signed_integer = signed_max } });
        _ = try ir.internConstant(.{ .ty = unsigned, .value = .{ .unsigned_integer = unsigned_max } });
        if (bits != 128) {
            try std.testing.expectError(error.IntegerOutOfRange, ir.internConstant(.{ .ty = signed, .value = .{ .signed_integer = signed_min - 1 } }));
            try std.testing.expectError(error.IntegerOutOfRange, ir.internConstant(.{ .ty = signed, .value = .{ .signed_integer = signed_max + 1 } }));
            try std.testing.expectError(error.IntegerOutOfRange, ir.internConstant(.{ .ty = unsigned, .value = .{ .unsigned_integer = unsigned_max + 1 } }));
        }
        try std.testing.expectError(error.ConstantTypeMismatch, ir.internConstant(.{ .ty = unsigned, .value = .{ .signed_integer = -1 } }));
        try std.testing.expectError(error.ConstantTypeMismatch, ir.internConstant(.{ .ty = signed, .value = .{ .unsigned_integer = 1 } }));
    }
    try ir.verify();
}

test "invalid pool entries are rejected without mutation" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const boolean = try ir.internType(.boolean);
    const void_type = try ir.internType(.void);
    const missing: TypeId = @enumFromInt(99);
    try std.testing.expectError(error.InvalidIntegerWidth, ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 0 } }));
    try std.testing.expectError(error.InvalidIntegerWidth, ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = 129 } }));
    try std.testing.expectError(error.InvalidTypeId, ir.internType(.{ .function = .{ .params = &.{missing}, .result = boolean } }));
    try std.testing.expectError(error.InvalidTypeId, ir.internType(.{ .function = .{ .params = &.{}, .result = missing } }));
    try std.testing.expectError(error.VoidParameter, ir.internType(.{ .function = .{ .params = &.{void_type}, .result = boolean } }));
    try std.testing.expectError(error.InvalidTypeId, ir.internConstant(.{ .ty = missing, .value = .void }));
    try std.testing.expectError(error.ConstantTypeMismatch, ir.internConstant(.{ .ty = boolean, .value = .{ .signed_integer = 1 } }));
    const function = try ir.internType(.{ .function = .{ .params = &.{}, .result = void_type } });
    try std.testing.expectError(error.NonConstantType, ir.internConstant(.{ .ty = function, .value = .void }));
    try std.testing.expectEqual(@as(usize, 3), ir.types.items.len);
    try std.testing.expectEqual(@as(usize, 0), ir.constants.items.len);
    try ir.verify();
}

test "verifier detects corrupt references and noncanonical entries" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const boolean = try ir.internType(.boolean);
    _ = try ir.internConstant(.{ .ty = boolean, .value = .{ .boolean = true } });
    ir.constants.items[0].ty = @enumFromInt(99);
    try std.testing.expectError(error.InvalidTypeId, ir.verify());
    ir.constants.items[0].ty = boolean;
    try ir.constants.append(ir.allocator, ir.constants.items[0]);
    try std.testing.expectError(error.NonCanonicalConstant, ir.verify());
    ir.constants.items.len -= 1;
    try ir.types.append(ir.allocator, .boolean);
    try std.testing.expectError(error.NonCanonicalType, ir.verify());
    ir.types.items.len -= 1;
    try ir.verify();
}

test "verifier rejects invalid type graphs and malformed constant values" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const boolean = try ir.internType(.boolean);
    const function = try ir.internType(.{ .function = .{ .params = &.{boolean}, .result = boolean } });
    const index = @intFromEnum(function);
    ir.types.items[index].function.result = function;
    try std.testing.expectError(error.InvalidTypeId, ir.verify());
    ir.types.items[index].function.result = boolean;
    const params = @constCast(ir.types.items[index].function.params);
    params[0] = @enumFromInt(999);
    try std.testing.expectError(error.InvalidTypeId, ir.verify());
    params[0] = boolean;
    ir.types.items[@intFromEnum(boolean)] = .{ .integer = .{ .signedness = .signed, .bits = 0 } };
    try std.testing.expectError(error.InvalidIntegerWidth, ir.verify());
    ir.types.items[@intFromEnum(boolean)] = .boolean;
    const truth = try ir.internConstant(.{ .ty = boolean, .value = .{ .boolean = true } });
    ir.constants.items[@intFromEnum(truth)].value = .void;
    try std.testing.expectError(error.ConstantTypeMismatch, ir.verify());
    ir.constants.items[@intFromEnum(truth)].value = .{ .boolean = true };
    const integer = try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 8 } });
    const number = try ir.internConstant(.{ .ty = integer, .value = .{ .signed_integer = 127 } });
    ir.constants.items[@intFromEnum(number)].value = .{ .signed_integer = 128 };
    try std.testing.expectError(error.IntegerOutOfRange, ir.verify());
    ir.constants.items[@intFromEnum(number)].value = .{ .signed_integer = 127 };
    try ir.verify();
}

test "type and constant handles remain stable across pool growth" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const boolean = try ir.internType(.boolean);
    const truth = try ir.internConstant(.{ .ty = boolean, .value = .{ .boolean = true } });
    for (1..129) |bits| {
        const integer = try ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = @intCast(bits) } });
        _ = try ir.internConstant(.{ .ty = integer, .value = .{ .unsigned_integer = 0 } });
    }
    try std.testing.expect(ir.getType(boolean).? == .boolean);
    try std.testing.expect(ir.getConstant(truth).?.value.boolean);
    try ir.verify();
}

fn testAllocationFailures(allocator: std.mem.Allocator) !void {
    var ir = Ir.init(allocator);
    defer ir.deinit();
    const boolean = try ir.internType(.boolean);
    const function = ir.internType(.{ .function = .{ .params = &.{boolean}, .result = boolean } }) catch |err| {
        try ir.verify();
        return err;
    };
    _ = try ir.internType(.{ .function = .{ .params = &.{function}, .result = boolean } });
    for ([_]bool{ false, true }) |value| {
        _ = ir.internConstant(.{ .ty = boolean, .value = .{ .boolean = value } }) catch |err| {
            try ir.verify();
            return err;
        };
    }
    for ([_]u16{ 8, 16, 32, 64, 128 }) |bits| {
        const integer = ir.internType(.{ .integer = .{ .signedness = .signed, .bits = bits } }) catch |err| {
            try ir.verify();
            return err;
        };
        _ = ir.internType(.{ .function = .{ .params = &.{ boolean, integer }, .result = integer } }) catch |err| {
            try ir.verify();
            return err;
        };
        _ = ir.internConstant(.{ .ty = integer, .value = .{ .signed_integer = 0 } }) catch |err| {
            try ir.verify();
            return err;
        };
    }
    try ir.verify();
}

test "type and constant ownership survives every allocation failure" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{});
}
