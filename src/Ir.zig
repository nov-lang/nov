const std = @import("std");
const Ir = @This();

// Integer widths describe resolved values; no literal defaults or overflow policy
// live here. Spans borrow offsets into a single externally owned source.
allocator: std.mem.Allocator,
types: std.ArrayList(Type) = .empty,
constants: std.ArrayList(Constant) = .empty,
source_len: u32 = 0,
functions: std.ArrayList(Function) = .empty,

/// The current pipeline targets the machine running Nov, not a separate target.
/// Native aliases are canonical fixed-width integers once resolved.
pub const native_pointer_bits = @bitSizeOf(usize);

pub const TypeId = enum(u32) { _ };
pub const ConstId = enum(u32) { _ };
pub const FunctionId = enum(u32) { _ };
/// Zero-based instruction index, local to a single body. Region-private values
/// are visible only within that region (or as its explicit result).
pub const ValueId = enum(u32) { _ };
pub const Span = struct { start: u32, end: u32 };
pub const Function = struct {
    name: []const u8,
    ty: TypeId,
    span: Span,
    body: ?Body = null,
};
pub const Body = struct { instructions: []const Instruction, result: ValueId };
pub const Instruction = struct { ty: TypeId, span: Span, op: Operation };
pub const UnaryOp = enum { negate, bit_not, bool_not };
pub const BinaryOp = enum { add, sub, mul, div, mod, eq, ne, lt, le, gt, ge };
pub const Operation = union(enum) {
    constant: ConstId,
    parameter: u32,
    function_ref: FunctionId,
    unary: struct { op: UnaryOp, operand: ValueId },
    binary: struct { op: BinaryOp, lhs: ValueId, rhs: ValueId },
    call: struct { callee: ValueId, args: []const ValueId },
    print: struct { args: []const ValueId, newline: bool },
    /// Instructions [rhs_start, this instruction) form a conditional RHS region.
    /// Regions nest or are disjoint; only this merged boolean escapes. Empty
    /// regions and results captured from enclosing regions are valid.
    short_circuit: struct { kind: enum { @"and", @"or" }, lhs: ValueId, rhs_start: u32, rhs: ValueId },
    /// Then instructions occupy [then_start, else_start), else instructions
    /// [else_start, merge). Only the selected result escapes these sibling scopes.
    /// An absent else is an empty region and is valid only for a void result.
    branch: struct { condition: ValueId, then_start: u32, else_start: u32, then_value: ValueId, else_value: ?ValueId },
};

pub const Type = union(enum) {
    void,
    boolean,
    integer: Integer,
    function: Type.Function,

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
pub const VerifyError = ValidationError || error{
    NonCanonicalType,
    NonCanonicalConstant,
    InvalidFunctionId,
    InvalidFunctionType,
    InvalidFunctionName,
    DuplicateFunctionName,
    InvalidSourceSpan,
    MissingFunctionBody,
    InvalidValueId,
    InvalidParameter,
    OperandTypeMismatch,
    InvalidOperation,
    InvalidCall,
    InvalidReturn,
    InvalidConstId,
    InvalidRegion,
};
pub const BuildError = VerifyError || std.mem.Allocator.Error || error{ TooManyEntries, BodyAlreadySet };

pub fn init(allocator: std.mem.Allocator) Ir {
    return .{ .allocator = allocator };
}

pub fn deinit(self: *Ir) void {
    self.clearFunctions();
    self.functions.deinit(self.allocator);
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

/// Names and nested body slices in the returned view borrow this IR.
pub fn getFunction(self: *const Ir, id: FunctionId) ?Function {
    const index = @intFromEnum(id);
    if (index >= self.functions.items.len) return null;
    return self.functions.items[index];
}

pub fn addFunction(self: *Ir, name: []const u8, ty: TypeId, span: Span) BuildError!FunctionId {
    try self.validateFunction(.{ .name = name, .ty = ty, .span = span });
    for (self.functions.items) |existing| {
        if (std.mem.eql(u8, name, existing.name)) return error.DuplicateFunctionName;
    }
    if (self.functions.items.len == std.math.maxInt(u32)) return error.TooManyEntries;
    const owned_name = try self.allocator.dupe(u8, name);
    errdefer self.allocator.free(owned_name);
    const id: FunctionId = @enumFromInt(self.functions.items.len);
    try self.functions.append(self.allocator, .{ .name = owned_name, .ty = ty, .span = span });
    return id;
}

/// Validation and allocation complete before publishing any part of the body.
/// Referenced function declarations may still be awaiting their own bodies.
pub fn setFunctionBody(self: *Ir, id: FunctionId, instructions: []const Instruction, result: ValueId) BuildError!void {
    const function = self.getFunction(id) orelse return error.InvalidFunctionId;
    if (function.body != null) return error.BodyAlreadySet;
    if (instructions.len > std.math.maxInt(u32)) return error.TooManyEntries;
    try self.validateFunction(function);
    try self.validateBody(function, .{ .instructions = instructions, .result = result });
    const owned = try self.allocator.dupe(Instruction, instructions);
    var copied: usize = 0;
    errdefer {
        for (owned[0..copied]) |instruction| self.freeOperands(instruction.op);
        self.allocator.free(owned);
    }
    for (owned) |*instruction| {
        switch (instruction.op) {
            .call => |*call| call.args = try self.allocator.dupe(ValueId, call.args),
            .print => |*print| print.args = try self.allocator.dupe(ValueId, print.args),
            else => {},
        }
        copied += 1;
    }
    self.functions.items[@intFromEnum(id)].body = .{ .instructions = owned, .result = result };
}

fn freeOperands(self: *Ir, op: Operation) void {
    switch (op) {
        .call => |call| self.allocator.free(call.args),
        .print => |print| self.allocator.free(print.args),
        else => {},
    }
}

/// Invalidates all function handles and borrowed function/body views, not pools.
pub fn clearFunctions(self: *Ir) void {
    for (self.functions.items) |function| {
        self.allocator.free(function.name);
        if (function.body) |body| {
            for (body.instructions) |instruction| {
                self.freeOperands(instruction.op);
            }
            self.allocator.free(body.instructions);
        }
    }
    self.functions.clearRetainingCapacity();
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
    // Check every declaration before bodies, allowing forward and recursive refs.
    for (self.functions.items, 0..) |function, i| {
        try self.validateFunction(function);
        for (self.functions.items[0..i]) |previous| {
            if (std.mem.eql(u8, previous.name, function.name)) return error.DuplicateFunctionName;
        }
    }
    for (self.functions.items) |function| {
        if (function.body == null) return error.MissingFunctionBody;
    }
    for (self.functions.items) |function| try self.validateBody(function, function.body.?);
}

fn validateSpan(self: *const Ir, span: Span) VerifyError!void {
    if (span.start > span.end or span.end > self.source_len) return error.InvalidSourceSpan;
}

fn validateFunction(self: *const Ir, function: Function) VerifyError!void {
    if (function.name.len == 0) return error.InvalidFunctionName;
    const ty = self.getType(function.ty) orelse return error.InvalidTypeId;
    if (ty != .function) return error.InvalidFunctionType;
    try self.validateType(ty, self.types.items.len);
    try self.validateSpan(function.span);
}

fn operandType(body: Body, id: ValueId, before: usize) VerifyError!TypeId {
    return regionOperandType(body, id, before, null);
}

fn regionOperandType(body: Body, id: ValueId, before: usize, yielding: ?usize) VerifyError!TypeId {
    const index = @intFromEnum(id);
    if (index >= before) return error.InvalidValueId;
    for (body.instructions, 0..) |instruction, owner| {
        var buffer: [2]Region = undefined;
        for (regions(instruction.op, owner, &buffer)) |region| {
            if (index >= region.start and index < region.end and
                !(before >= region.start and before < region.end) and
                !(before == region.end and yielding == owner)) return error.InvalidValueId;
        }
    }
    return body.instructions[index].ty;
}

const Region = struct { start: usize, end: usize };

fn regions(op: Operation, end: usize, buffer: *[2]Region) []const Region {
    switch (op) {
        .short_circuit => |r| {
            buffer[0] = .{ .start = r.rhs_start, .end = end };
            return buffer[0..1];
        },
        .branch => |r| {
            buffer.* = .{ .{ .start = r.then_start, .end = r.else_start }, .{ .start = r.else_start, .end = end } };
            return buffer;
        },
        else => return &.{},
    }
}

fn validateBody(self: *const Ir, function: Function, body: Body) VerifyError!void {
    const signature = (self.getType(function.ty) orelse return error.InvalidTypeId).function;
    if (body.instructions.len > std.math.maxInt(u32)) return error.InvalidRegion;
    // Validate structure before resolving operands. A region includes its merge
    // in its parent's scope, never in a sibling's scope.
    for (body.instructions, 0..) |instruction, end| {
        switch (instruction.op) {
            .short_circuit => |r| {
                if (r.rhs_start > end or @intFromEnum(r.lhs) >= r.rhs_start) return error.InvalidRegion;
            },
            .branch => |r| {
                if (r.then_start > r.else_start or r.else_start > end or
                    @intFromEnum(r.condition) >= r.then_start or
                    (r.else_value == null and r.else_start != end)) return error.InvalidRegion;
            },
            else => {},
        }
        var buffer: [2]Region = undefined;
        for (regions(instruction.op, end, &buffer)) |region| {
            for (body.instructions[0..end], 0..) |previous, previous_end| {
                var previous_buffer: [2]Region = undefined;
                for (regions(previous.op, previous_end, &previous_buffer)) |previous_region| {
                    // The nested merge belongs to the surrounding scope too.
                    if (previous_end >= region.start and previous_region.start < region.start) return error.InvalidRegion;
                }
            }
        }
    }
    for (body.instructions, 0..) |instruction, i| {
        const ty = self.getType(instruction.ty) orelse return error.InvalidTypeId;
        try self.validateSpan(instruction.span);
        switch (instruction.op) {
            .constant => |id| {
                const constant = self.getConstant(id) orelse return error.InvalidConstId;
                try self.validateConstant(constant);
                if (constant.ty != instruction.ty) return error.OperandTypeMismatch;
            },
            .parameter => |index| {
                if (index >= signature.params.len) return error.InvalidParameter;
                if (signature.params[index] != instruction.ty) return error.OperandTypeMismatch;
            },
            .function_ref => |id| {
                const referenced = self.getFunction(id) orelse return error.InvalidFunctionId;
                try self.validateFunction(referenced);
                if (referenced.ty != instruction.ty) return error.OperandTypeMismatch;
            },
            .unary => |unary| {
                const operand = try operandType(body, unary.operand, i);
                if (operand != instruction.ty) return error.OperandTypeMismatch;
                switch (unary.op) {
                    .negate => if (ty != .integer or ty.integer.signedness != .signed) return error.InvalidOperation,
                    .bit_not => if (ty != .integer) return error.InvalidOperation,
                    .bool_not => if (ty != .boolean) return error.InvalidOperation,
                }
            },
            .binary => |binary| {
                const lhs = try operandType(body, binary.lhs, i);
                const rhs = try operandType(body, binary.rhs, i);
                if (lhs != rhs) return error.OperandTypeMismatch;
                const operand = self.getType(lhs) orelse return error.InvalidTypeId;
                switch (binary.op) {
                    .add, .sub, .mul, .div, .mod => {
                        if (operand != .integer) return error.InvalidOperation;
                        if (instruction.ty != lhs) return error.OperandTypeMismatch;
                    },
                    .eq, .ne, .lt, .le, .gt, .ge => {
                        if (operand != .integer and !((binary.op == .eq or binary.op == .ne) and operand == .boolean)) return error.InvalidOperation;
                        if (ty != .boolean) return error.OperandTypeMismatch;
                    },
                }
            },
            .short_circuit => |region| {
                if (ty != .boolean) return error.OperandTypeMismatch;
                if (try operandType(body, region.lhs, region.rhs_start) != instruction.ty) return error.OperandTypeMismatch;
                if (try regionOperandType(body, region.rhs, i, i) != instruction.ty) return error.OperandTypeMismatch;
            },
            .branch => |r| {
                const condition = self.getType(try operandType(body, r.condition, r.then_start)) orelse return error.InvalidTypeId;
                if (condition != .boolean) return error.OperandTypeMismatch;
                if (try regionOperandType(body, r.then_value, r.else_start, i) != instruction.ty) return error.OperandTypeMismatch;
                if (r.else_value) |value| {
                    if (try regionOperandType(body, value, i, i) != instruction.ty) return error.OperandTypeMismatch;
                } else if (ty != .void) return error.OperandTypeMismatch;
            },
            .print => |print| {
                if (ty != .void) return error.InvalidOperation;
                for (print.args) |arg| {
                    const operand = self.getType(try operandType(body, arg, i)) orelse return error.InvalidTypeId;
                    if (operand != .boolean and operand != .integer) return error.InvalidOperation;
                }
            },
            .call => |call| {
                const callee_id = try operandType(body, call.callee, i);
                const callee = self.getType(callee_id) orelse return error.InvalidTypeId;
                if (callee != .function) return error.InvalidCall;
                try self.validateType(callee, self.types.items.len);
                if (call.args.len != callee.function.params.len) return error.InvalidCall;
                for (call.args, callee.function.params) |arg, param| {
                    if (try operandType(body, arg, i) != param) return error.OperandTypeMismatch;
                }
                if (instruction.ty != callee.function.result) return error.InvalidCall;
            },
        }
    }
    if (try operandType(body, body.result, body.instructions.len) != signature.result) return error.InvalidReturn;
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

fn testPrintAllocationFailures(allocator: std.mem.Allocator) !void {
    var ir = Ir.init(allocator);
    defer ir.deinit();
    const void_ty = try ir.internType(.void);
    const boolean = try ir.internType(.boolean);
    const signature = try ir.internType(.{ .function = .{ .params = &.{boolean}, .result = void_ty } });
    const function = try ir.addFunction("print", signature, .{ .start = 0, .end = 0 });
    var args = [_]ValueId{@enumFromInt(0)};
    const instructions = [_]Instruction{
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .parameter = 0 } },
        .{ .ty = void_ty, .span = .{ .start = 0, .end = 0 }, .op = .{ .print = .{ .args = &args, .newline = false } } },
        .{ .ty = void_ty, .span = .{ .start = 0, .end = 0 }, .op = .{ .print = .{ .args = &args, .newline = true } } },
    };
    ir.setFunctionBody(function, &instructions, @enumFromInt(2)) catch |err| {
        try std.testing.expect(ir.functions.items[0].body == null);
        return err;
    };
    args[0] = @enumFromInt(999);
    try ir.verify();
    const body = ir.functions.items[0].body.?;
    try std.testing.expectEqual(@as(u32, 0), @intFromEnum(body.instructions[1].op.print.args[0]));
    try std.testing.expectEqual(@as(u32, 0), @intFromEnum(body.instructions[2].op.print.args[0]));
}

test "print operands are owned and body publication is atomic under OOM" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testPrintAllocationFailures, .{});
}

test "print verification checks result types scalar arguments and prior operands" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const void_ty = try ir.internType(.void);
    const boolean = try ir.internType(.boolean);
    const unit = try ir.internConstant(.{ .ty = void_ty, .value = .void });
    const signature = try ir.internType(.{ .function = .{ .params = &.{boolean}, .result = void_ty } });
    const function = try ir.addFunction("print", signature, .{ .start = 0, .end = 0 });
    var instructions = [_]Instruction{
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .parameter = 0 } },
        .{ .ty = void_ty, .span = .{ .start = 0, .end = 0 }, .op = .{ .print = .{ .args = &.{@enumFromInt(0)}, .newline = true } } },
    };
    instructions[1].ty = boolean;
    try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(function, &instructions, @enumFromInt(1)));
    instructions[1].ty = void_ty;
    for ([_]u32{ 1, 99 }) |index| {
        instructions[1].op.print.args = &.{@enumFromInt(index)};
        try std.testing.expectError(error.InvalidValueId, ir.setFunctionBody(function, &instructions, @enumFromInt(1)));
    }
    instructions[1].op.print.args = &.{@enumFromInt(0)};
    instructions[0] = .{ .ty = void_ty, .span = .{ .start = 0, .end = 0 }, .op = .{ .constant = unit } };
    try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(function, &instructions, @enumFromInt(1)));
    instructions[0] = .{ .ty = signature, .span = .{ .start = 0, .end = 0 }, .op = .{ .function_ref = function } };
    try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(function, &instructions, @enumFromInt(1)));
    instructions[1].op.print.args = &.{};
    try ir.setFunctionBody(function, &instructions, @enumFromInt(1));
    try ir.verify();
    const owned = @constCast(ir.functions.items[0].body.?.instructions);
    owned[1].ty = boolean;
    try std.testing.expectError(error.InvalidOperation, ir.verify());
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

const test_span: Span = .{ .start = 0, .end = 1 };
const TestFunctions = struct {
    ir: Ir,
    boolean: TypeId,
    integer: TypeId,
    unsigned: TypeId,
    signature: TypeId,
    function: FunctionId,
    number: ConstId,

    fn init(allocator: std.mem.Allocator) !TestFunctions {
        var ir = Ir.init(allocator);
        errdefer ir.deinit();
        ir.source_len = 10;
        const boolean = try ir.internType(.boolean);
        const integer = try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 32 } });
        const unsigned = try ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = 32 } });
        const signature = try ir.internType(.{ .function = .{ .params = &.{integer}, .result = integer } });
        const number = try ir.internConstant(.{ .ty = integer, .value = .{ .signed_integer = 1 } });
        const function = try ir.addFunction("identity", signature, test_span);
        return .{ .ir = ir, .boolean = boolean, .integer = integer, .unsigned = unsigned, .signature = signature, .function = function, .number = number };
    }

    fn instruction(ty: TypeId, op: Operation) Instruction {
        return .{ .ty = ty, .span = test_span, .op = op };
    }

    fn reject(self: *TestFunctions, expected: VerifyError, instructions: []const Instruction, result: ValueId) !void {
        try std.testing.expectError(expected, self.ir.setFunctionBody(self.function, instructions, result));
        try std.testing.expect(self.ir.getFunction(self.function).?.body == null);
    }
};

fn testConditionalAllocationFailures(allocator: std.mem.Allocator) !void {
    var ir = Ir.init(allocator);
    defer ir.deinit();
    const boolean = try ir.internType(.boolean);
    const void_ty = try ir.internType(.void);
    const signature = try ir.internType(.{ .function = .{ .params = &.{boolean}, .result = boolean } });
    const function = try ir.addFunction("conditional", signature, .{ .start = 0, .end = 0 });
    var call_args = [_]ValueId{@enumFromInt(0)};
    var print_args = [_]ValueId{@enumFromInt(2)};
    var instructions = [_]Instruction{
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .parameter = 0 } },
        .{ .ty = signature, .span = .{ .start = 0, .end = 0 }, .op = .{ .function_ref = function } },
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .call = .{ .callee = @enumFromInt(1), .args = &call_args } } },
        .{ .ty = void_ty, .span = .{ .start = 0, .end = 0 }, .op = .{ .print = .{ .args = &print_args, .newline = true } } },
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .short_circuit = .{ .kind = .@"or", .lhs = @enumFromInt(0), .rhs_start = 2, .rhs = @enumFromInt(2) } } },
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .branch = .{ .condition = @enumFromInt(0), .then_start = 1, .else_start = 5, .then_value = @enumFromInt(4), .else_value = @enumFromInt(0) } } },
    };
    ir.setFunctionBody(function, &instructions, @enumFromInt(5)) catch |err| {
        try std.testing.expect(ir.getFunction(function).?.body == null);
        return err;
    };
    call_args[0] = @enumFromInt(99);
    print_args[0] = @enumFromInt(99);
    instructions[5].op.branch.then_start = 99;
    try ir.verify();
    // Removing a nested body frees the same owned slices as final destruction.
    ir.clearFunctions();
    try ir.verify();
}

test "conditional body operands are owned and publication is atomic under OOM" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testConditionalAllocationFailures, .{});
}

test "if regions validate sibling isolation captures yields types and boundaries" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const ir = &fixture.ir;
    const boolean = fixture.boolean;
    const signature = try ir.internType(.{ .function = .{ .params = &.{}, .result = boolean } });
    const function = try ir.addFunction("branch", signature, test_span);
    const constant = try ir.internConstant(.{ .ty = boolean, .value = .{ .boolean = true } });
    const ins = TestFunctions.instruction;
    const valid = [_]Instruction{
        ins(boolean, .{ .constant = constant }),
        ins(boolean, .{ .constant = constant }),
        ins(boolean, .{ .short_circuit = .{ .kind = .@"and", .lhs = @enumFromInt(0), .rhs_start = 1, .rhs = @enumFromInt(1) } }),
        ins(boolean, .{ .constant = constant }),
        ins(boolean, .{ .branch = .{ .condition = @enumFromInt(0), .then_start = 1, .else_start = 3, .then_value = @enumFromInt(2), .else_value = @enumFromInt(3) } }),
        ins(boolean, .{ .unary = .{ .op = .bool_not, .operand = @enumFromInt(4) } }),
    };
    var cases: std.ArrayList(struct { index: usize, instruction: Instruction, err: VerifyError }) = .empty;
    defer cases.deinit(std.testing.allocator);
    const original = valid[4].op.branch;
    var r = original;
    r.then_start = 5;
    try cases.append(std.testing.allocator, .{ .index = 4, .instruction = ins(boolean, .{ .branch = r }), .err = error.InvalidRegion });
    r = original;
    r.else_start = 99;
    try cases.append(std.testing.allocator, .{ .index = 4, .instruction = ins(boolean, .{ .branch = r }), .err = error.InvalidRegion });
    r = original;
    r.condition = @enumFromInt(1);
    try cases.append(std.testing.allocator, .{ .index = 4, .instruction = ins(boolean, .{ .branch = r }), .err = error.InvalidRegion });
    r = original;
    r.else_start = 2; // Splits a nested region from its merge.
    try cases.append(std.testing.allocator, .{ .index = 4, .instruction = ins(boolean, .{ .branch = r }), .err = error.InvalidRegion });
    r = original;
    r.then_value = @enumFromInt(1); // A nested RHS cannot escape via the outer yield.
    try cases.append(std.testing.allocator, .{ .index = 4, .instruction = ins(boolean, .{ .branch = r }), .err = error.InvalidValueId });
    r = original;
    r.else_value = @enumFromInt(2);
    try cases.append(std.testing.allocator, .{ .index = 4, .instruction = ins(boolean, .{ .branch = r }), .err = error.InvalidValueId });
    r = original;
    r.then_value = @enumFromInt(3);
    try cases.append(std.testing.allocator, .{ .index = 4, .instruction = ins(boolean, .{ .branch = r }), .err = error.InvalidValueId });
    r = original;
    r.else_value = @enumFromInt(4);
    try cases.append(std.testing.allocator, .{ .index = 4, .instruction = ins(boolean, .{ .branch = r }), .err = error.InvalidValueId });
    r = original;
    r.else_start = 4;
    r.else_value = null;
    try cases.append(std.testing.allocator, .{ .index = 4, .instruction = ins(boolean, .{ .branch = r }), .err = error.OperandTypeMismatch });
    try cases.append(std.testing.allocator, .{ .index = 3, .instruction = ins(boolean, .{ .unary = .{ .op = .bool_not, .operand = @enumFromInt(2) } }), .err = error.InvalidValueId });
    try cases.append(std.testing.allocator, .{ .index = 5, .instruction = ins(boolean, .{ .unary = .{ .op = .bool_not, .operand = @enumFromInt(3) } }), .err = error.InvalidValueId });
    try cases.append(std.testing.allocator, .{ .index = 0, .instruction = ins(fixture.integer, .{ .constant = fixture.number }), .err = error.OperandTypeMismatch });
    for (cases.items) |case| {
        var invalid = valid;
        invalid[case.index] = case.instruction;
        try std.testing.expectError(case.err, ir.setFunctionBody(function, &invalid, @enumFromInt(5)));
        try std.testing.expect(ir.getFunction(function).?.body == null);
    }
    try std.testing.expectError(error.InvalidValueId, ir.setFunctionBody(function, &valid, @enumFromInt(3)));
    try ir.setFunctionBody(function, &valid, @enumFromInt(5));
    try ir.setFunctionBody(fixture.function, &.{ins(fixture.integer, .{ .parameter = 0 })}, @enumFromInt(0));
    const owned = @constCast(ir.getFunction(function).?.body.?.instructions);
    for (cases.items) |case| {
        owned[case.index] = case.instruction;
        try std.testing.expectError(case.err, ir.verify());
        owned[case.index] = valid[case.index];
    }
    owned[4].op.branch.then_value = @enumFromInt(0);
    owned[4].op.branch.else_value = @enumFromInt(0);
    try ir.verify();
    owned[4].op.branch.then_start = 4;
    owned[4].op.branch.else_start = 4;
    try ir.verify(); // Empty branches can yield captured values.
}

test "conditional regions reject crossing ranges and private value escapes" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const ir = &fixture.ir;
    const boolean = fixture.boolean;
    const signature = try ir.internType(.{ .function = .{ .params = &.{}, .result = boolean } });
    const function = try ir.addFunction("conditional", signature, test_span);
    const constant = try ir.internConstant(.{ .ty = boolean, .value = .{ .boolean = true } });
    const ins = TestFunctions.instruction;
    const void_ty = try ir.internType(.void);
    const valid = [_]Instruction{
        ins(boolean, .{ .constant = constant }),
        ins(boolean, .{ .constant = constant }),
        ins(boolean, .{ .short_circuit = .{ .kind = .@"and", .lhs = @enumFromInt(0), .rhs_start = 1, .rhs = @enumFromInt(1) } }),
        ins(boolean, .{ .constant = constant }),
        ins(boolean, .{ .short_circuit = .{ .kind = .@"or", .lhs = @enumFromInt(0), .rhs_start = 3, .rhs = @enumFromInt(3) } }),
        ins(boolean, .{ .unary = .{ .op = .bool_not, .operand = @enumFromInt(2) } }),
    };
    // Test insertion as well as verification after post-publication corruption.
    const cases = [_]struct { index: usize, instruction: Instruction, err: VerifyError }{
        .{ .index = 4, .instruction = ins(boolean, .{ .short_circuit = .{ .kind = .@"or", .lhs = @enumFromInt(0), .rhs_start = 2, .rhs = @enumFromInt(3) } }), .err = error.InvalidRegion },
        .{ .index = 2, .instruction = ins(boolean, .{ .short_circuit = .{ .kind = .@"and", .lhs = @enumFromInt(1), .rhs_start = 1, .rhs = @enumFromInt(1) } }), .err = error.InvalidRegion },
        .{ .index = 2, .instruction = ins(boolean, .{ .short_circuit = .{ .kind = .@"and", .lhs = @enumFromInt(0), .rhs_start = 99, .rhs = @enumFromInt(1) } }), .err = error.InvalidRegion },
        .{ .index = 2, .instruction = ins(boolean, .{ .short_circuit = .{ .kind = .@"and", .lhs = @enumFromInt(0), .rhs_start = 1, .rhs = @enumFromInt(2) } }), .err = error.InvalidValueId },
        .{ .index = 4, .instruction = ins(boolean, .{ .short_circuit = .{ .kind = .@"or", .lhs = @enumFromInt(0), .rhs_start = 3, .rhs = @enumFromInt(1) } }), .err = error.InvalidValueId },
        .{ .index = 5, .instruction = ins(boolean, .{ .unary = .{ .op = .bool_not, .operand = @enumFromInt(1) } }), .err = error.InvalidValueId },
        .{ .index = 0, .instruction = ins(fixture.integer, .{ .constant = fixture.number }), .err = error.OperandTypeMismatch },
        .{ .index = 1, .instruction = ins(fixture.integer, .{ .constant = fixture.number }), .err = error.OperandTypeMismatch },
        .{ .index = 2, .instruction = ins(fixture.integer, valid[2].op), .err = error.OperandTypeMismatch },
        .{ .index = 5, .instruction = ins(void_ty, .{ .print = .{ .args = &.{@enumFromInt(1)}, .newline = true } }), .err = error.InvalidValueId },
        .{ .index = 5, .instruction = ins(boolean, .{ .binary = .{ .op = .eq, .lhs = @enumFromInt(1), .rhs = @enumFromInt(0) } }), .err = error.InvalidValueId },
        .{ .index = 5, .instruction = ins(boolean, .{ .call = .{ .callee = @enumFromInt(1), .args = &.{} } }), .err = error.InvalidValueId },
        .{ .index = 2, .instruction = .{ .ty = boolean, .span = .{ .start = 0, .end = 99 }, .op = valid[2].op }, .err = error.InvalidSourceSpan },
    };
    for (cases) |case| {
        var invalid = valid;
        invalid[case.index] = case.instruction;
        try std.testing.expectError(case.err, ir.setFunctionBody(function, &invalid, @enumFromInt(5)));
        try std.testing.expect(ir.getFunction(function).?.body == null);
    }
    try std.testing.expectError(error.InvalidValueId, ir.setFunctionBody(function, &valid, @enumFromInt(1)));
    try ir.setFunctionBody(function, &valid, @enumFromInt(5));
    // Complete the unrelated fixture declaration before verifying.
    try ir.setFunctionBody(fixture.function, &.{ins(fixture.integer, .{ .parameter = 0 })}, @enumFromInt(0));
    try ir.verify();
    const owned = @constCast(ir.getFunction(function).?.body.?.instructions);
    for (cases) |case| {
        owned[case.index] = case.instruction;
        try std.testing.expectError(case.err, ir.verify());
        owned[case.index] = valid[case.index];
    }
    // Nesting and enclosing captures, including an empty region, are legal.
    owned[5].op.unary.operand = @enumFromInt(4);
    owned[4].op.short_circuit.rhs_start = 1;
    owned[4].op.short_circuit.rhs = @enumFromInt(2);
    try ir.verify();
    owned[2].op.short_circuit.rhs_start = 2;
    owned[2].op.short_circuit.rhs = @enumFromInt(0);
    try ir.verify();
    owned[4].op.short_circuit.rhs = @enumFromInt(1);
    try ir.verify();
    owned[5].op.unary.operand = @enumFromInt(2);
    try std.testing.expectError(error.InvalidValueId, ir.verify());
}

test "functions own names instructions and call arguments with forward and recursive references" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const ir = &fixture.ir;
    var name = [_]u8{ 'l', 'a', 't', 'e', 'r' };
    const later = try ir.addFunction(&name, fixture.signature, .{ .start = 1, .end = 10 });
    name[0] = 'x';
    try std.testing.expectEqualStrings("later", ir.getFunction(later).?.name);
    var args = [_]ValueId{@enumFromInt(0)};
    var instructions = [_]Instruction{
        TestFunctions.instruction(fixture.integer, .{ .parameter = 0 }),
        TestFunctions.instruction(fixture.signature, .{ .function_ref = later }),
        TestFunctions.instruction(fixture.integer, .{ .call = .{ .callee = @enumFromInt(1), .args = &args } }),
    };
    try ir.setFunctionBody(fixture.function, &instructions, @enumFromInt(2));
    try std.testing.expectError(error.MissingFunctionBody, ir.verify());
    args[0] = @enumFromInt(999);
    instructions[0].ty = fixture.boolean;
    const stored = ir.getFunction(fixture.function).?.body.?;
    try std.testing.expectEqual(fixture.integer, stored.instructions[0].ty);
    try std.testing.expectEqual(@as(ValueId, @enumFromInt(0)), stored.instructions[2].op.call.args[0]);
    // The second function refers to itself, but no body is required at insertion.
    instructions[0].ty = fixture.integer;
    args[0] = @enumFromInt(0);
    try ir.setFunctionBody(later, &instructions, @enumFromInt(2));
    try ir.verify();
    try std.testing.expectError(error.BodyAlreadySet, ir.setFunctionBody(later, &.{}, @enumFromInt(0)));
    try std.testing.expect(ir.getFunction(@enumFromInt(999)) == null);
    const capacity = ir.functions.capacity;
    ir.clearFunctions();
    try std.testing.expectEqual(capacity, ir.functions.capacity);
    try std.testing.expectEqual(@as(usize, 0), ir.functions.items.len);
    try ir.verify();
    _ = try ir.addFunction("identity", fixture.signature, test_span);
    ir.clearFunctions();
}

test "all integer unary binary and comparison operations have exact types" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const ir = &fixture.ir;
    for ([_]TypeId{ fixture.integer, fixture.unsigned }) |integer| {
        const signature = try ir.internType(.{ .function = .{ .params = &.{integer}, .result = integer } });
        for (std.enums.values(BinaryOp)) |op| {
            const comparison = switch (op) {
                .add, .sub, .mul, .div, .mod => false,
                else => true,
            };
            const result_type = if (comparison) fixture.boolean else integer;
            const ty = try ir.internType(.{ .function = .{ .params = &.{integer}, .result = result_type } });
            ir.clearFunctions();
            const function = try ir.addFunction("binary", ty, test_span);
            try ir.setFunctionBody(function, &.{
                TestFunctions.instruction(integer, .{ .parameter = 0 }),
                TestFunctions.instruction(result_type, .{ .binary = .{ .op = op, .lhs = @enumFromInt(0), .rhs = @enumFromInt(0) } }),
            }, @enumFromInt(1));
            try ir.verify();
        }
        for ([_]UnaryOp{ .negate, .bit_not }) |op| {
            if (integer == fixture.unsigned and op == .negate) continue;
            ir.clearFunctions();
            const function = try ir.addFunction("unary", signature, test_span);
            try ir.setFunctionBody(function, &.{
                TestFunctions.instruction(integer, .{ .parameter = 0 }),
                TestFunctions.instruction(integer, .{ .unary = .{ .op = op, .operand = @enumFromInt(0) } }),
            }, @enumFromInt(1));
            try ir.verify();
        }
    }
    ir.clearFunctions();
    const boolean_signature = try ir.internType(.{ .function = .{ .params = &.{fixture.boolean}, .result = fixture.boolean } });
    const function = try ir.addFunction("boolean", boolean_signature, test_span);
    try ir.setFunctionBody(function, &.{
        TestFunctions.instruction(fixture.boolean, .{ .parameter = 0 }),
        TestFunctions.instruction(fixture.boolean, .{ .unary = .{ .op = .bool_not, .operand = @enumFromInt(0) } }),
        TestFunctions.instruction(fixture.boolean, .{ .binary = .{ .op = .eq, .lhs = @enumFromInt(0), .rhs = @enumFromInt(1) } }),
        TestFunctions.instruction(fixture.boolean, .{ .binary = .{ .op = .ne, .lhs = @enumFromInt(1), .rhs = @enumFromInt(2) } }),
    }, @enumFromInt(3));
    try ir.verify();
}

test "function metadata validation rejects corruption and preserves declarations" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const ir = &fixture.ir;
    try std.testing.expectError(error.InvalidFunctionName, ir.addFunction("", fixture.signature, test_span));
    try std.testing.expectError(error.DuplicateFunctionName, ir.addFunction("identity", fixture.signature, test_span));
    try std.testing.expectError(error.InvalidFunctionType, ir.addFunction("bad", fixture.integer, test_span));
    try std.testing.expectError(error.InvalidTypeId, ir.addFunction("bad", @enumFromInt(999), test_span));
    for ([_]Span{ .{ .start = 2, .end = 1 }, .{ .start = 0, .end = 11 } }) |span| {
        try std.testing.expectError(error.InvalidSourceSpan, ir.addFunction("bad", fixture.signature, span));
    }
    try std.testing.expectEqual(@as(usize, 1), ir.functions.items.len);
    try std.testing.expectError(error.InvalidFunctionId, ir.setFunctionBody(@enumFromInt(999), &.{}, @enumFromInt(0)));
    try std.testing.expectError(error.MissingFunctionBody, ir.verify());
    try ir.setFunctionBody(fixture.function, &.{TestFunctions.instruction(fixture.integer, .{ .parameter = 0 })}, @enumFromInt(0));
    const original = ir.functions.items[0];
    ir.functions.items[0].name = "";
    try std.testing.expectError(error.InvalidFunctionName, ir.verify());
    ir.functions.items[0] = original;
    ir.functions.items[0].ty = fixture.integer;
    try std.testing.expectError(error.InvalidFunctionType, ir.verify());
    ir.functions.items[0].ty = @enumFromInt(999);
    try std.testing.expectError(error.InvalidTypeId, ir.verify());
    ir.functions.items[0] = original;
    ir.functions.items[0].span.end = 11;
    try std.testing.expectError(error.InvalidSourceSpan, ir.verify());
    ir.functions.items[0] = original;
    const other = try ir.addFunction("other", fixture.signature, test_span);
    const other_name = ir.functions.items[@intFromEnum(other)].name;
    ir.functions.items[@intFromEnum(other)].name = original.name;
    try std.testing.expectError(error.DuplicateFunctionName, ir.verify());
    ir.functions.items[@intFromEnum(other)].name = other_name;
    try ir.setFunctionBody(other, &.{TestFunctions.instruction(fixture.integer, .{ .parameter = 0 })}, @enumFromInt(0));
    ir.source_len = 0;
    try std.testing.expectError(error.InvalidSourceSpan, ir.verify());
    ir.source_len = 10;
    try ir.verify();
}

test "body validation rejects invalid ids parameter types spans and returns" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const integer = fixture.integer;
    try fixture.reject(error.InvalidValueId, &.{}, @enumFromInt(0));
    try fixture.reject(error.InvalidTypeId, &.{TestFunctions.instruction(@enumFromInt(999), .{ .parameter = 0 })}, @enumFromInt(0));
    try fixture.reject(error.InvalidParameter, &.{TestFunctions.instruction(integer, .{ .parameter = 1 })}, @enumFromInt(0));
    try fixture.reject(error.OperandTypeMismatch, &.{TestFunctions.instruction(fixture.boolean, .{ .parameter = 0 })}, @enumFromInt(0));
    try fixture.reject(error.InvalidConstId, &.{TestFunctions.instruction(integer, .{ .constant = @enumFromInt(999) })}, @enumFromInt(0));
    try fixture.reject(error.OperandTypeMismatch, &.{TestFunctions.instruction(fixture.boolean, .{ .constant = fixture.number })}, @enumFromInt(0));
    try fixture.reject(error.InvalidFunctionId, &.{TestFunctions.instruction(fixture.signature, .{ .function_ref = @enumFromInt(999) })}, @enumFromInt(0));
    try fixture.reject(error.OperandTypeMismatch, &.{TestFunctions.instruction(integer, .{ .function_ref = fixture.function })}, @enumFromInt(0));
    const truth = try fixture.ir.internConstant(.{ .ty = fixture.boolean, .value = .{ .boolean = true } });
    try fixture.reject(error.InvalidReturn, &.{TestFunctions.instruction(fixture.boolean, .{ .constant = truth })}, @enumFromInt(0));
    try fixture.reject(error.InvalidValueId, &.{TestFunctions.instruction(integer, .{ .constant = fixture.number })}, @enumFromInt(1));
    for ([_]Span{ .{ .start = 3, .end = 2 }, .{ .start = 10, .end = 11 } }) |span| {
        try fixture.reject(error.InvalidSourceSpan, &.{.{ .ty = integer, .span = span, .op = .{ .parameter = 0 } }}, @enumFromInt(0));
    }
    try fixture.ir.setFunctionBody(fixture.function, &.{TestFunctions.instruction(integer, .{ .constant = fixture.number })}, @enumFromInt(0));
    const body = &fixture.ir.functions.items[0].body.?;
    const instructions = @constCast(body.instructions);
    instructions[0].op = .{ .constant = @enumFromInt(999) };
    try std.testing.expectError(error.InvalidConstId, fixture.ir.verify());
    instructions[0].op = .{ .constant = fixture.number };
    instructions[0].span.end = 11;
    try std.testing.expectError(error.InvalidSourceSpan, fixture.ir.verify());
    instructions[0].span = test_span;
    body.result = @enumFromInt(1);
    try std.testing.expectError(error.InvalidValueId, fixture.ir.verify());
    body.result = @enumFromInt(0);
    try fixture.ir.verify();
}

test "SSA operands must precede their use and operation types must agree" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const integer = fixture.integer;
    const first = TestFunctions.instruction(integer, .{ .parameter = 0 });
    for ([_]u32{ 1, 2, 999 }) |invalid| {
        try fixture.reject(error.InvalidValueId, &.{ first, TestFunctions.instruction(integer, .{ .unary = .{ .op = .negate, .operand = @enumFromInt(invalid) } }) }, @enumFromInt(1));
        try fixture.reject(error.InvalidValueId, &.{ first, TestFunctions.instruction(integer, .{ .binary = .{ .op = .add, .lhs = @enumFromInt(0), .rhs = @enumFromInt(invalid) } }) }, @enumFromInt(1));
        try fixture.reject(error.InvalidValueId, &.{ first, TestFunctions.instruction(integer, .{ .binary = .{ .op = .add, .lhs = @enumFromInt(invalid), .rhs = @enumFromInt(0) } }) }, @enumFromInt(1));
    }
    try fixture.reject(error.OperandTypeMismatch, &.{ first, TestFunctions.instruction(fixture.unsigned, .{ .unary = .{ .op = .bit_not, .operand = @enumFromInt(0) } }) }, @enumFromInt(1));
    try fixture.reject(error.InvalidOperation, &.{ first, TestFunctions.instruction(integer, .{ .unary = .{ .op = .bool_not, .operand = @enumFromInt(0) } }) }, @enumFromInt(1));
    const unsigned_constant = try fixture.ir.internConstant(.{ .ty = fixture.unsigned, .value = .{ .unsigned_integer = 1 } });
    const unsigned_first = TestFunctions.instruction(fixture.unsigned, .{ .constant = unsigned_constant });
    try fixture.reject(error.InvalidOperation, &.{ unsigned_first, TestFunctions.instruction(fixture.unsigned, .{ .unary = .{ .op = .negate, .operand = @enumFromInt(0) } }) }, @enumFromInt(1));
    try fixture.reject(error.OperandTypeMismatch, &.{ first, unsigned_first, TestFunctions.instruction(integer, .{ .binary = .{ .op = .add, .lhs = @enumFromInt(0), .rhs = @enumFromInt(1) } }) }, @enumFromInt(2));
    try fixture.reject(error.OperandTypeMismatch, &.{ first, TestFunctions.instruction(fixture.unsigned, .{ .binary = .{ .op = .add, .lhs = @enumFromInt(0), .rhs = @enumFromInt(0) } }) }, @enumFromInt(1));
    try fixture.reject(error.OperandTypeMismatch, &.{ first, TestFunctions.instruction(integer, .{ .binary = .{ .op = .lt, .lhs = @enumFromInt(0), .rhs = @enumFromInt(0) } }) }, @enumFromInt(1));
    const truth = try fixture.ir.internConstant(.{ .ty = fixture.boolean, .value = .{ .boolean = true } });
    const boolean_first = TestFunctions.instruction(fixture.boolean, .{ .constant = truth });
    for ([_]UnaryOp{ .negate, .bit_not }) |op| {
        try fixture.reject(error.InvalidOperation, &.{ boolean_first, TestFunctions.instruction(fixture.boolean, .{ .unary = .{ .op = op, .operand = @enumFromInt(0) } }) }, @enumFromInt(1));
    }
    for ([_]BinaryOp{ .add, .sub, .mul, .div, .mod, .lt, .le, .gt, .ge }) |op| {
        try fixture.reject(error.InvalidOperation, &.{ boolean_first, TestFunctions.instruction(fixture.boolean, .{ .binary = .{ .op = op, .lhs = @enumFromInt(0), .rhs = @enumFromInt(0) } }) }, @enumFromInt(1));
    }
    // A value in another function cannot satisfy an operand in this body.
    const other = try fixture.ir.addFunction("other", fixture.signature, test_span);
    try fixture.ir.setFunctionBody(other, &.{first}, @enumFromInt(0));
    try fixture.reject(error.InvalidValueId, &.{TestFunctions.instruction(integer, .{ .unary = .{ .op = .negate, .operand = @enumFromInt(0) } })}, @enumFromInt(0));
}

test "calls validate callee order signatures arity arguments and results" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const integer = fixture.integer;
    const first = TestFunctions.instruction(integer, .{ .parameter = 0 });
    const reference = TestFunctions.instruction(fixture.signature, .{ .function_ref = fixture.function });
    try fixture.reject(error.InvalidCall, &.{ first, TestFunctions.instruction(integer, .{ .call = .{ .callee = @enumFromInt(0), .args = &.{} } }) }, @enumFromInt(1));
    for ([_]u32{ 2, 999 }) |invalid| {
        try fixture.reject(error.InvalidValueId, &.{ first, reference, TestFunctions.instruction(integer, .{ .call = .{ .callee = @enumFromInt(invalid), .args = &.{@enumFromInt(0)} } }) }, @enumFromInt(2));
        try fixture.reject(error.InvalidValueId, &.{ first, reference, TestFunctions.instruction(integer, .{ .call = .{ .callee = @enumFromInt(1), .args = &.{@enumFromInt(invalid)} } }) }, @enumFromInt(2));
    }
    for ([_][]const ValueId{ &.{}, &.{ @enumFromInt(0), @enumFromInt(0) } }) |args| {
        try fixture.reject(error.InvalidCall, &.{ first, reference, TestFunctions.instruction(integer, .{ .call = .{ .callee = @enumFromInt(1), .args = args } }) }, @enumFromInt(2));
    }
    try fixture.reject(error.OperandTypeMismatch, &.{ first, reference, TestFunctions.instruction(integer, .{ .call = .{ .callee = @enumFromInt(1), .args = &.{@enumFromInt(1)} } }) }, @enumFromInt(2));
    try fixture.reject(error.InvalidCall, &.{ first, reference, TestFunctions.instruction(fixture.boolean, .{ .call = .{ .callee = @enumFromInt(1), .args = &.{@enumFromInt(0)} } }) }, @enumFromInt(2));
    try fixture.ir.setFunctionBody(fixture.function, &.{ first, reference, TestFunctions.instruction(integer, .{ .call = .{ .callee = @enumFromInt(1), .args = &.{@enumFromInt(0)} } }) }, @enumFromInt(2));
    const instructions = @constCast(fixture.ir.getFunction(fixture.function).?.body.?.instructions);
    const original = instructions[2];
    instructions[2].op.call.callee = @enumFromInt(2);
    try std.testing.expectError(error.InvalidValueId, fixture.ir.verify());
    instructions[2] = original;
    @constCast(instructions[2].op.call.args)[0] = @enumFromInt(2);
    try std.testing.expectError(error.InvalidValueId, fixture.ir.verify());
    @constCast(instructions[2].op.call.args)[0] = @enumFromInt(0);
    try fixture.ir.verify();
}

fn testFunctionAllocationFailures(allocator: std.mem.Allocator) !void {
    var fixture = try TestFunctions.init(allocator);
    defer fixture.ir.deinit();
    const ir = &fixture.ir;
    const first = TestFunctions.instruction(fixture.integer, .{ .parameter = 0 });
    ir.setFunctionBody(fixture.function, &.{first}, @enumFromInt(0)) catch |err| {
        try std.testing.expect(ir.getFunction(fixture.function).?.body == null);
        return err;
    };
    const original_body = ir.getFunction(fixture.function).?.body.?.instructions;
    // Force both name allocation and eventual array growth while preserving bodies.
    for (0..20) |i| {
        var name_buffer: [32]u8 = undefined;
        const name = try std.fmt.bufPrint(&name_buffer, "function{d}", .{i});
        const old_len = ir.functions.items.len;
        const function = ir.addFunction(name, fixture.signature, test_span) catch |err| {
            try std.testing.expectEqual(old_len, ir.functions.items.len);
            try ir.verify();
            return err;
        };
        const instructions = [_]Instruction{
            first,
            TestFunctions.instruction(fixture.signature, .{ .function_ref = fixture.function }),
            TestFunctions.instruction(fixture.integer, .{ .call = .{ .callee = @enumFromInt(1), .args = &.{@enumFromInt(0)} } }),
            TestFunctions.instruction(fixture.integer, .{ .call = .{ .callee = @enumFromInt(1), .args = &.{@enumFromInt(2)} } }),
        };
        ir.setFunctionBody(function, &instructions, @enumFromInt(3)) catch |err| {
            try std.testing.expect(ir.getFunction(function).?.body == null);
            try std.testing.expectEqualStrings(name, ir.getFunction(function).?.name);
            try std.testing.expectEqual(original_body.ptr, ir.getFunction(fixture.function).?.body.?.instructions.ptr);
            try std.testing.expectError(error.MissingFunctionBody, ir.verify());
            return err;
        };
        try ir.verify();
    }
    ir.clearFunctions();
    try ir.verify();
    const reused = try ir.addFunction("reused", fixture.signature, test_span);
    try ir.setFunctionBody(reused, &.{first}, @enumFromInt(0));
    try ir.verify();
}

test "void and higher order functions accept zero width source spans" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const span: Span = .{ .start = 0, .end = 0 };
    const void_type = try ir.internType(.void);
    const void_value = try ir.internConstant(.{ .ty = void_type, .value = .void });
    const signature = try ir.internType(.{ .function = .{ .params = &.{}, .result = void_type } });
    const identity_type = try ir.internType(.{ .function = .{ .params = &.{signature}, .result = signature } });
    const call_type = try ir.internType(.{ .function = .{ .params = &.{signature}, .result = void_type } });
    const leaf = try ir.addFunction("leaf", signature, span);
    const identity = try ir.addFunction("identity", identity_type, span);
    const call = try ir.addFunction("call", call_type, span);
    try ir.setFunctionBody(leaf, &.{.{ .ty = void_type, .span = span, .op = .{ .constant = void_value } }}, @enumFromInt(0));
    try ir.setFunctionBody(identity, &.{.{ .ty = signature, .span = span, .op = .{ .parameter = 0 } }}, @enumFromInt(0));
    try ir.setFunctionBody(call, &.{
        .{ .ty = signature, .span = span, .op = .{ .parameter = 0 } },
        .{ .ty = void_type, .span = span, .op = .{ .call = .{ .callee = @enumFromInt(0), .args = &.{} } } },
    }, @enumFromInt(1));
    try ir.verify();
}

test "body insertion checks referenced signatures and verification checks pools first" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const ir = &fixture.ir;
    const other = try ir.addFunction("other", fixture.signature, test_span);
    const reference = TestFunctions.instruction(fixture.signature, .{ .function_ref = other });
    ir.functions.items[@intFromEnum(other)].ty = fixture.integer;
    try fixture.reject(error.InvalidFunctionType, &.{reference}, @enumFromInt(0));
    ir.functions.items[@intFromEnum(other)].ty = @enumFromInt(999);
    try fixture.reject(error.InvalidTypeId, &.{reference}, @enumFromInt(0));
    ir.functions.items[@intFromEnum(other)].ty = fixture.signature;
    const signature = &ir.types.items[@intFromEnum(fixture.signature)].function;
    signature.result = @enumFromInt(999);
    try fixture.reject(error.InvalidTypeId, &.{reference}, @enumFromInt(0));
    try std.testing.expectError(error.InvalidTypeId, ir.verify());
    signature.result = fixture.integer;
    ir.constants.items[@intFromEnum(fixture.number)].ty = @enumFromInt(999);
    try std.testing.expectError(error.InvalidTypeId, ir.verify());
    ir.constants.items[@intFromEnum(fixture.number)].ty = fixture.integer;
    try std.testing.expectError(error.MissingFunctionBody, ir.verify());
}

test "function ownership and body publication survive every allocation failure" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testFunctionAllocationFailures, .{});
}
