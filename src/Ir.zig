const std = @import("std");
const Ir = @This();
const Source = @import("Source.zig");

// Integer widths describe resolved values; no literal defaults or overflow policy
// live here. Source lengths and qualified spans remain valid after AST release.
allocator: std.mem.Allocator,
types: std.ArrayList(Type) = .empty,
constants: std.ArrayList(Constant) = .empty,
/// Legacy standalone-source bound. Compilation IR uses source_lengths instead.
source_len: u32 = 0,
source_lengths: std.ArrayList(u32) = .empty,
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
pub const Span = Source.Span;
pub const Function = struct {
    name: []const u8,
    ty: TypeId,
    span: Span,
    body: ?Body = null,
    public: bool = false,
    noreturn: bool = false,
};
/// `result` is either a continuing value or the final terminal instruction.
pub const Body = struct { instructions: []const Instruction, result: ValueId };
pub const Instruction = struct { ty: TypeId, span: Span, op: Operation };
pub const UnaryOp = enum { negate, bit_not, bool_not };
pub const BinaryOp = enum { add, sub, mul, div, mod, bit_and, bit_or, bit_xor, eq, ne, lt, le, gt, ge };
pub const FieldRead = struct { base: ValueId, index: u32 };
pub const Operation = union(enum) {
    union_init: struct { index: u32, payload: ?ValueId },
    union_tag: FieldRead,
    /// Always checked at runtime, even for externally constructed or malformed values.
    union_payload: FieldRead,
    record_init: []const ValueId,
    field_read: FieldRead,
    /// Update only this field of the current mutable local, not a record snapshot.
    field_store: struct { slot: ValueId, index: u32, value: ValueId, path: []const u32 = &.{} },
    /// Decimal integers, true/false booleans, or a shared immutable string.
    to_string: ValueId,
    array_literal: []const ValueId,
    /// Independent scalar element storage; every value boundary is explicit.
    array_clone: ValueId,
    /// Byte length for strings; element count for arrays.
    array_len: ValueId,
    array_index: struct { array: ValueId, index: ValueId },
    /// Allocation-free scalar equality scan; operands retain their read/copy rules.
    array_contains: struct { value: ValueId, array: ValueId },
    array_set: struct { array: ValueId, index: ValueId, value: ValueId },
    array_append: struct { lhs: ValueId, rhs: ValueId },
    /// Terminal control flow, never a value. Its optional operand is body-local.
    return_value: ?ValueId,
    /// Terminal panic with an exact string operand; has no continuing value.
    panic: ValueId,
    /// Terminal exits target the innermost containing loop, including its condition.
    loop_break: ?ValueId,
    loop_continue,
    /// Condition [condition_start, body_start) is reexecuted before every body.
    /// Body ends at continuation_start, else_start, or the merge; continuation
    /// ends at else_start or the merge. Body continue enters the continuation;
    /// condition/continuation continue reenters the condition. Break/return skip
    /// continuation and exhaustion. Exhaustion [else_start, merge) runs only on
    /// condition false and belongs to the enclosing control scope. All regions
    /// are private siblings; only outer slots carry state across backedges.
    /// `returns` means terminal exhaustion with no breaks owned by this loop.
    loop: struct {
        unconditional: bool = false,
        condition_start: u32,
        body_start: u32,
        continuation_start: ?u32 = null,
        condition: ValueId,
        body: ValueId,
        continuation: ?ValueId = null,
        /// Exhaustion is private but outside this loop's control scope.
        else_start: ?u32 = null,
        else_value: ?ValueId = null,
        returns: bool = false,
    },
    // Slot identities are instruction IDs, but are not expression values.
    local_slot: ValueId,
    local_load: ValueId,
    local_store: struct { slot: ValueId, value: ValueId },
    constant: ConstId,
    parameter: u32,
    function_ref: FunctionId,
    unary: struct { op: UnaryOp, operand: ValueId },
    binary: struct { op: BinaryOp, lhs: ValueId, rhs: ValueId },
    call: struct { callee: ValueId, args: []const ValueId, noreturn: bool = false },
    print: struct { args: []const ValueId, newline: bool },
    /// Instructions [rhs_start, this instruction) form a conditional RHS region.
    /// Regions nest or are disjoint; only this merged boolean escapes. Empty
    /// regions and results captured from enclosing regions are valid.
    short_circuit: struct { kind: enum { @"and", @"or" }, lhs: ValueId, rhs_start: u32, rhs: ValueId },
    /// Then instructions occupy [then_start, else_start), else instructions
    /// [else_start, merge). Only the selected result escapes these sibling scopes.
    /// An absent else is an empty region and is valid only for a void result.
    /// Yields may instead name terminal instructions. `returns` is verified to
    /// mean both paths terminate; such a merge has void type and no value.
    branch: struct { condition: ValueId, then_start: u32, else_start: u32, then_value: ValueId, else_value: ?ValueId, returns: bool = false },
};

pub const Field = struct { name: []const u8, ty: TypeId, span: Span, default: ?ConstId = null };
/// Nominal by-value layout. Tagged layouts have one active scalar or record payload.
pub const Record = struct {
    source: Source.Id = @enumFromInt(0),
    identity: u32,
    fields: []const Field,
    public: bool = false,
    tagged: bool = false,
    /// Empty for ordinary aggregates; canonical arguments distinguish nominal instances.
    type_args: []const TypeId = &.{},
    instance_state: enum { complete, building, failed } = .complete,
};
pub const EnumTag = struct { name: []const u8, span: Span };
pub const Enumeration = struct { source: Source.Id = @enumFromInt(0), identity: u32, name: []const u8, tags: []const EnumTag, public: bool = false };
pub const Type = union(enum) {
    enumeration: Enumeration,
    record: Record,
    void,
    boolean,
    string,
    integer: Integer,
    function: Type.Function,
    array: TypeId,

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
        enum_tag: u32,
        void,
        boolean: bool,
        signed_integer: i128,
        unsigned_integer: u128,
        string: []const u8,
    };
};

pub const ValidationError = error{
    InvalidTypeId,
    InvalidIntegerWidth,
    VoidParameter,
    NonConstantType,
    InvalidArrayElement,
    ConstantTypeMismatch,
    IntegerOutOfRange,
};
pub const InternError = ValidationError || std.mem.Allocator.Error || error{TooManyEntries};
pub const VerifyError = ValidationError || error{
    CyclicRecordLayout,
    RecordNestingLimit,
    OutOfMemory,
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
    UnreachableInstruction,
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
            .record => |record| {
                self.freeFields(record.fields);
                self.allocator.free(record.type_args);
            },
            .enumeration => |enumeration| {
                self.allocator.free(enumeration.name);
                for (enumeration.tags) |tag| self.allocator.free(tag.name);
                self.allocator.free(enumeration.tags);
            },
            .function => |function| self.allocator.free(function.params),
            else => {},
        }
    }
    self.types.deinit(self.allocator);
    for (self.constants.items) |constant| if (constant.value == .string) {
        self.allocator.free(constant.value.string);
    };
    self.constants.deinit(self.allocator);
    self.source_lengths.deinit(self.allocator);
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
        if (span.source == existing.span.source and std.mem.eql(u8, name, existing.name)) return error.DuplicateFunctionName;
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
            .array_literal, .record_init => |*elements| elements.* = try self.allocator.dupe(ValueId, elements.*),
            .field_store => |*store| store.path = try self.allocator.dupe(u32, store.path),
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
        .array_literal, .record_init => |elements| self.allocator.free(elements),
        .field_store => |store| self.allocator.free(store.path),
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

fn freeFields(self: *Ir, fields: []const Field) void {
    for (fields) |field| self.allocator.free(field.name);
    self.allocator.free(fields);
}

pub fn setRecordFields(self: *Ir, id: TypeId, fields: []const Field) InternError!void {
    const ty = self.getType(id) orelse return error.InvalidTypeId;
    if (ty != .record) return error.InvalidTypeId;
    try self.validateType(.{ .record = .{ .source = ty.record.source, .identity = ty.record.identity, .tagged = ty.record.tagged, .type_args = ty.record.type_args, .fields = fields } }, self.types.items.len);
    const owned = try self.allocator.dupe(Field, fields);
    var copied: usize = 0;
    errdefer {
        for (owned[0..copied]) |field| self.allocator.free(field.name);
        self.allocator.free(owned);
    }
    for (owned) |*field| {
        field.name = try self.allocator.dupe(u8, field.name);
        copied += 1;
    }
    self.freeFields(ty.record.fields);
    self.types.items[@intFromEnum(id)].record.fields = owned;
}

pub fn internType(self: *Ir, ty: Type) InternError!TypeId {
    try self.validateType(ty, self.types.items.len);
    for (self.types.items, 0..) |existing, i| {
        if (typeEql(existing, ty)) {
            if (ty == .enumeration) {
                if (!std.mem.eql(u8, existing.enumeration.name, ty.enumeration.name) or existing.enumeration.public != ty.enumeration.public or existing.enumeration.tags.len != ty.enumeration.tags.len) return error.InvalidTypeId;
                for (existing.enumeration.tags, ty.enumeration.tags) |a, b| if (!std.mem.eql(u8, a.name, b.name) or !std.meta.eql(a.span, b.span)) return error.InvalidTypeId;
            }
            return @enumFromInt(i);
        }
    }
    if (self.types.items.len == std.math.maxInt(u32)) return error.TooManyEntries;
    try self.types.ensureUnusedCapacity(self.allocator, 1);
    const owned: Type = switch (ty) {
        .enumeration => |enumeration| blk: {
            const name = try self.allocator.dupe(u8, enumeration.name);
            errdefer self.allocator.free(name);
            const tags = try self.allocator.dupe(EnumTag, enumeration.tags);
            var copied: usize = 0;
            errdefer {
                for (tags[0..copied]) |tag| self.allocator.free(tag.name);
                self.allocator.free(tags);
            }
            for (tags) |*tag| {
                tag.name = try self.allocator.dupe(u8, tag.name);
                copied += 1;
            }
            break :blk .{ .enumeration = .{ .source = enumeration.source, .identity = enumeration.identity, .name = name, .tags = tags, .public = enumeration.public } };
        },
        .record => |record| blk: {
            if (record.fields.len != 0) return error.InvalidTypeId;
            const args = try self.allocator.dupe(TypeId, record.type_args);
            errdefer self.allocator.free(args);
            break :blk .{ .record = .{ .source = record.source, .identity = record.identity, .fields = try self.allocator.alloc(Field, 0), .public = record.public, .tagged = record.tagged, .type_args = args, .instance_state = record.instance_state } };
        },
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
        if (constantEql(existing, constant)) return @enumFromInt(i);
    }
    if (self.constants.items.len == std.math.maxInt(u32)) return error.TooManyEntries;
    const id: ConstId = @enumFromInt(self.constants.items.len);
    try self.constants.ensureUnusedCapacity(self.allocator, 1);
    var owned = constant;
    if (constant.value == .string) owned.value = .{ .string = try self.allocator.dupe(u8, constant.value.string) };
    self.constants.appendAssumeCapacity(owned);
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
            if (constantEql(previous, constant)) return error.NonCanonicalConstant;
        }
    }
    // Check every declaration before bodies, allowing forward and recursive refs.
    for (self.functions.items, 0..) |function, i| {
        try self.validateFunction(function);
        for (self.functions.items[0..i]) |previous| {
            if (previous.span.source == function.span.source and std.mem.eql(u8, previous.name, function.name)) return error.DuplicateFunctionName;
        }
    }
    for (self.functions.items) |function| {
        if (function.body == null) return error.MissingFunctionBody;
    }
    if (try self.recordLayoutIssue()) |issue| return if (issue.cycle) error.CyclicRecordLayout else error.RecordNestingLimit;
    for (self.functions.items) |function| try self.validateBody(function, function.body.?);
}

/// Bounds all later recursive backend walks, including source-free forged IR.
pub const max_record_depth = 128;
pub const LayoutIssue = struct { span: Span, related: Span, cycle: bool };

/// Iterative tri-color DFS, linear in graph size even for diamond layouts.
/// Completed subtree heights enforce the limit independently of visitation order.
pub fn recordLayoutIssue(self: *const Ir) (std.mem.Allocator.Error || error{InvalidTypeId})!?LayoutIssue {
    var has_record = false;
    for (self.types.items) |ty| {
        if (ty == .record) has_record = true;
    }
    if (!has_record) return null;
    const State = struct { color: enum { white, gray, black } = .white, height: usize = 1 };
    const states = try self.allocator.alloc(State, self.types.items.len);
    defer self.allocator.free(states);
    @memset(states, .{});
    const Frame = struct { id: TypeId, next: usize = 0 };
    var stack: [max_record_depth]Frame = undefined;
    for (self.types.items, 0..) |ty, root| {
        if (ty != .record or states[root].color != .white) continue;
        stack[0] = .{ .id = @enumFromInt(root) };
        states[root].color = .gray;
        var len: usize = 1;
        while (len != 0) {
            const frame = &stack[len - 1];
            const index = @intFromEnum(frame.id);
            const fields = self.types.items[index].record.fields;
            if (frame.next == fields.len) {
                states[index].color = .black;
                len -= 1;
                continue;
            }
            const field = fields[frame.next];
            const child = @intFromEnum(field.ty);
            const ft = self.getType(field.ty) orelse return error.InvalidTypeId;
            if (ft != .record) {
                frame.next += 1;
                continue;
            }
            const related = if (ft.record.fields.len != 0) ft.record.fields[0].span else field.span;
            if (states[child].color == .gray) return .{ .span = field.span, .related = related, .cycle = true };
            if (states[child].color == .white) {
                if (len == stack.len) return .{ .span = field.span, .related = related, .cycle = false };
                states[child].color = .gray;
                stack[len] = .{ .id = field.ty };
                len += 1;
                continue;
            }
            states[index].height = @max(states[index].height, states[child].height + 1);
            if (states[index].height > max_record_depth) return .{ .span = field.span, .related = related, .cycle = false };
            frame.next += 1;
        }
    }
    return null;
}

fn validateSpan(self: *const Ir, span: Span) VerifyError!void {
    const index = @intFromEnum(span.source);
    const length = if (self.source_lengths.items.len != 0)
        if (index < self.source_lengths.items.len) self.source_lengths.items[index] else return error.InvalidSourceSpan
    else if (index == 0) self.source_len else return error.InvalidSourceSpan;
    if (span.start > span.end or span.end > length) return error.InvalidSourceSpan;
}

fn validateFunction(self: *const Ir, function: Function) VerifyError!void {
    if (function.name.len == 0) return error.InvalidFunctionName;
    const ty = self.getType(function.ty) orelse return error.InvalidTypeId;
    if (ty != .function) return error.InvalidFunctionType;
    try self.validateType(ty, self.types.items.len);
    try self.validateSpan(function.span);
    if (function.noreturn and self.getType(ty.function.result).? != .void) return error.InvalidReturn;
}

fn operandType(body: Body, id: ValueId, before: usize) VerifyError!TypeId {
    const ty = try regionOperandType(body, id, before, null);
    if (body.instructions[@intFromEnum(id)].op == .local_slot) return error.InvalidValueId;
    return ty;
}

// Empty siblings can share an end offset. Only the precise yielding region
// may expose its private result, not another region ending at the same merge.
const Yield = struct { owner: usize, start: usize };

fn regionOperandType(body: Body, id: ValueId, before: usize, yielding: ?Yield) VerifyError!TypeId {
    const index = @intFromEnum(id);
    if (index >= before or index >= body.instructions.len) return error.InvalidValueId;
    if (yielding != null and body.instructions[index].op == .local_slot) return error.InvalidValueId;
    for (body.instructions, 0..) |instruction, owner| {
        var buffer: [4]Region = undefined;
        for (regions(instruction.op, owner, &buffer)) |region| {
            if (index >= region.start and index < region.end and
                !(before >= region.start and before < region.end) and
                !(before == region.end and yielding != null and yielding.?.owner == owner and yielding.?.start == region.start)) return error.InvalidValueId;
        }
    }
    if (terminates(body, id) and yielding == null) return error.InvalidValueId;
    return body.instructions[index].ty;
}

pub fn terminates(body: Body, id: ValueId) bool {
    return switch (body.instructions[@intFromEnum(id)].op) {
        .panic, .return_value, .loop_break, .loop_continue => true,
        .branch => |r| r.returns,
        .loop => |r| r.returns,
        .call => |call| call.noreturn,
        else => false,
    };
}

pub fn enclosingLoop(body: Body, index: usize) ?usize {
    for (body.instructions[index + 1 ..], index + 1..) |instruction, end| {
        if (instruction.op == .loop and index >= instruction.op.loop.condition_start and index < (instruction.op.loop.else_start orelse end)) return end;
    }
    return null;
}

const Region = struct { start: usize, end: usize };

fn regions(op: Operation, end: usize, buffer: *[4]Region) []const Region {
    switch (op) {
        .short_circuit => |r| {
            buffer[0] = .{ .start = r.rhs_start, .end = end };
            return buffer[0..1];
        },
        .loop => |r| {
            const control_end = r.else_start orelse end;
            const continuation_start = r.continuation_start orelse control_end;
            buffer[0] = .{ .start = r.condition_start, .end = r.body_start };
            buffer[1] = .{ .start = r.body_start, .end = continuation_start };
            var count: usize = 2;
            if (r.continuation_start != null) {
                buffer[count] = .{ .start = continuation_start, .end = control_end };
                count += 1;
            }
            if (r.else_start) |start| {
                buffer[count] = .{ .start = start, .end = end };
                count += 1;
            }
            return buffer[0..count];
        },
        .branch => |r| {
            buffer[0] = .{ .start = r.then_start, .end = r.else_start };
            buffer[1] = .{ .start = r.else_start, .end = end };
            return buffer[0..2];
        },
        else => return &.{},
    }
}

fn slotType(body: Body, id: ValueId, before: usize) VerifyError!TypeId {
    const ty = try regionOperandType(body, id, before, null);
    if (body.instructions[@intFromEnum(id)].op != .local_slot) return error.InvalidOperation;
    return ty;
}

fn validateBody(self: *const Ir, function: Function, body: Body) VerifyError!void {
    const signature = (self.getType(function.ty) orelse return error.InvalidTypeId).function;
    if (body.instructions.len > std.math.maxInt(u32)) return error.InvalidRegion;
    // Validate structure before resolving operands. A region includes its merge
    // in its parent's scope, never in a sibling's scope.
    for (body.instructions, 0..) |instruction, end| {
        switch (instruction.op) {
            .loop => |r| {
                const control_end = r.else_start orelse end;
                if ((r.else_start == null) != (r.else_value == null)) return error.InvalidRegion;
                if (r.else_start) |start| {
                    if (r.unconditional or start <= r.body_start or start > end or @intFromEnum(r.else_value.?) >= end) return error.InvalidRegion;
                }
                if (r.condition_start >= r.body_start or r.body_start >= end or
                    @intFromEnum(r.condition) < r.condition_start or @intFromEnum(r.condition) >= r.body_start or
                    @intFromEnum(r.body) < r.body_start or @intFromEnum(r.body) >= (r.continuation_start orelse control_end)) return error.InvalidRegion;
                if ((r.continuation_start == null) != (r.continuation == null)) return error.InvalidRegion;
                if (r.continuation_start) |start| {
                    if (start <= r.body_start or start >= control_end or @intFromEnum(r.continuation.?) < start or @intFromEnum(r.continuation.?) >= control_end) return error.InvalidRegion;
                }
            },
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
        var buffer: [4]Region = undefined;
        for (regions(instruction.op, end, &buffer)) |region| {
            for (body.instructions[0..end], 0..) |previous, previous_end| {
                var previous_buffer: [4]Region = undefined;
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
        if (terminates(body, @enumFromInt(i))) {
            var region_end = body.instructions.len;
            var result = body.result;
            for (body.instructions, 0..) |owner, end| {
                var buffer: [4]Region = undefined;
                for (regions(owner.op, end, &buffer), 0..) |region, r| {
                    if (i >= region.start and i < region.end and region.end <= region_end) {
                        region_end = region.end;
                        result = switch (owner.op) {
                            .branch => |b| if (r == 0) b.then_value else b.else_value.?,
                            .short_circuit => |s| s.rhs,
                            .loop => |l| switch (r) {
                                0 => l.condition,
                                1 => l.body,
                                else => if (l.continuation != null and r == 2) l.continuation.? else l.else_value.?,
                            },
                            else => unreachable,
                        };
                    }
                }
            }
            if (i + 1 != region_end or @intFromEnum(result) != i) return error.UnreachableInstruction;
        }
        switch (instruction.op) {
            .union_init => |init_value| {
                if (ty != .record or !ty.record.tagged or init_value.index >= ty.record.fields.len) return error.InvalidOperation;
                const payload_ty = ty.record.fields[init_value.index].ty;
                if (init_value.payload) |payload| {
                    if (self.getType(payload_ty).? == .void or try operandType(body, payload, i) != payload_ty) return error.OperandTypeMismatch;
                } else if (self.getType(payload_ty).? != .void) return error.OperandTypeMismatch;
            },
            .union_tag, .union_payload => |read| {
                const base = self.getType(try operandType(body, read.base, i)).?;
                if (base != .record or !base.record.tagged or read.index >= base.record.fields.len) return error.InvalidOperation;
                if (instruction.op == .union_tag) {
                    if (ty != .boolean) return error.OperandTypeMismatch;
                } else if (instruction.ty != base.record.fields[read.index].ty) return error.OperandTypeMismatch;
            },
            .record_init => |values| {
                if (ty != .record or ty.record.tagged or values.len != ty.record.fields.len) return error.InvalidOperation;
                for (values, ty.record.fields) |value, field| if (try operandType(body, value, i) != field.ty) return error.OperandTypeMismatch;
            },
            .field_read => |read| {
                const base = self.getType(try operandType(body, read.base, i)).?;
                if (base != .record or base.record.tagged or read.index >= base.record.fields.len) return error.InvalidOperation;
                if (instruction.ty != base.record.fields[read.index].ty) return error.OperandTypeMismatch;
            },
            .to_string => |value| {
                if (ty != .string) return error.InvalidOperation;
                const operand = self.getType(try operandType(body, value, i)).?;
                if (operand != .boolean and operand != .integer and operand != .string) return error.InvalidOperation;
            },
            .array_literal => |elements| {
                if (ty != .array) return error.InvalidOperation;
                for (elements) |element| {
                    if (try operandType(body, element, i) != ty.array) return error.OperandTypeMismatch;
                }
            },
            .array_clone => |value| {
                if (ty != .array) return error.InvalidOperation;
                if (try operandType(body, value, i) != instruction.ty) return error.OperandTypeMismatch;
            },
            .array_len => |value| {
                if (!isNativeInteger(ty, .unsigned)) return error.OperandTypeMismatch;
                const base = self.getType(try operandType(body, value, i)).?;
                if (base != .array and base != .string) return error.InvalidOperation;
            },
            .array_index => |r| {
                const array = self.getType(try operandType(body, r.array, i)).?;
                if (array != .array) return error.InvalidOperation;
                if (array.array != instruction.ty or !isNativeInteger(self.getType(try operandType(body, r.index, i)).?, .signed)) return error.OperandTypeMismatch;
            },
            .array_contains => |r| {
                if (ty != .boolean) return error.InvalidOperation;
                const value = try operandType(body, r.value, i);
                const array = self.getType(try operandType(body, r.array, i)).?;
                if (array != .array) return error.InvalidOperation;
                if (value != array.array) return error.OperandTypeMismatch;
            },
            .array_set => |r| {
                if (ty != .array) return error.InvalidOperation;
                if (try operandType(body, r.array, i) != instruction.ty or
                    try operandType(body, r.value, i) != ty.array or
                    !isNativeInteger(self.getType(try operandType(body, r.index, i)).?, .signed)) return error.OperandTypeMismatch;
            },
            .array_append => |r| {
                if (ty != .array) return error.InvalidOperation;
                if (try operandType(body, r.lhs, i) != instruction.ty or try operandType(body, r.rhs, i) != instruction.ty) return error.OperandTypeMismatch;
            },
            .local_slot => |value| {
                if (ty != .boolean and ty != .integer and ty != .function and ty != .array and ty != .string and ty != .record and ty != .enumeration) return error.InvalidOperation;
                if (try operandType(body, value, i) != instruction.ty) return error.OperandTypeMismatch;
            },
            .local_load => |slot| {
                if (try slotType(body, slot, i) != instruction.ty) return error.OperandTypeMismatch;
            },
            .field_store => |store| {
                if (ty != .void) return error.InvalidOperation;
                var base = self.getType(try slotType(body, store.slot, i)).?;
                if (store.path.len >= max_record_depth) return error.InvalidOperation;
                for (store.path) |index| {
                    if (base != .record or base.record.tagged or index >= base.record.fields.len) return error.InvalidOperation;
                    base = self.getType(base.record.fields[index].ty) orelse return error.InvalidTypeId;
                }
                if (base != .record or base.record.tagged or store.index >= base.record.fields.len) return error.InvalidOperation;
                if (try operandType(body, store.value, i) != base.record.fields[store.index].ty) return error.OperandTypeMismatch;
            },
            .local_store => |store| {
                if (ty != .void) return error.InvalidOperation;
                if (try slotType(body, store.slot, i) != try operandType(body, store.value, i)) return error.OperandTypeMismatch;
            },
            .loop_break, .loop_continue => {
                if (ty != .void) return error.InvalidOperation;
                const owner = enclosingLoop(body, i) orelse return error.InvalidRegion;
                if (instruction.op == .loop_break) {
                    // The enclosing merge is later in the stream, so its type
                    // has not yet passed the per-instruction validation below.
                    const loop_ty = self.getType(body.instructions[owner].ty) orelse return error.InvalidTypeId;
                    if (instruction.op.loop_break) |value| {
                        if (!body.instructions[owner].op.loop.unconditional and body.instructions[owner].op.loop.else_value == null) return error.InvalidOperation;
                        if (try operandType(body, value, i) != body.instructions[owner].ty) return error.OperandTypeMismatch;
                    } else if (loop_ty != .void) return error.OperandTypeMismatch;
                }
            },
            .loop => |r| {
                if (!r.unconditional and r.else_value == null and ty != .void) return error.InvalidOperation;
                if (r.unconditional) {
                    const condition = body.instructions[@intFromEnum(r.condition)];
                    if (condition.op != .constant) return error.InvalidOperation;
                    const constant = self.getConstant(condition.op.constant) orelse return error.InvalidConstId;
                    if (constant.value != .boolean or !constant.value.boolean) return error.InvalidOperation;
                }
                var has_break = false;
                var condition_has_break = false;
                var valued_break = false;
                for (body.instructions, 0..) |control, index| {
                    if (control.op == .loop_break and enclosingLoop(body, index) == i) {
                        has_break = true;
                        if (index < r.body_start) condition_has_break = true;
                        if (control.op.loop_break != null) valued_break = true;
                    }
                }
                if (ty != .void and r.else_value == null and !valued_break) return error.InvalidOperation;
                const condition_terminal = terminates(body, r.condition) and !condition_has_break;
                const returns = condition_terminal or (if (r.else_value) |value| terminates(body, value) and !has_break else r.unconditional and !has_break);
                if (r.returns != returns or (returns and ty != .void)) return error.InvalidReturn;
                if (r.else_value) |value| {
                    const else_ty = try regionOperandType(body, value, i, .{ .owner = i, .start = r.else_start.? });
                    if (!terminates(body, value) and else_ty != instruction.ty) return error.OperandTypeMismatch;
                }
                const condition_ty = try regionOperandType(body, r.condition, r.body_start, .{ .owner = i, .start = r.condition_start });
                if (!terminates(body, r.condition) and self.getType(condition_ty).? != .boolean) return error.OperandTypeMismatch;
                const body_ty = try regionOperandType(body, r.body, r.continuation_start orelse r.else_start orelse i, .{ .owner = i, .start = r.body_start });
                if (!terminates(body, r.body) and self.getType(body_ty).? != .void) return error.OperandTypeMismatch;
                if (r.continuation) |value| {
                    const continuation_ty = try regionOperandType(body, value, r.else_start orelse i, .{ .owner = i, .start = r.continuation_start.? });
                    if (!terminates(body, value) and self.getType(continuation_ty).? != .void) return error.OperandTypeMismatch;
                }
            },
            .panic => |value| {
                if (ty != .void) return error.InvalidOperation;
                if (self.getType(try operandType(body, value, i)).? != .string) return error.OperandTypeMismatch;
            },
            .return_value => |value| {
                if (function.noreturn) return error.InvalidReturn;
                if (ty != .void) return error.InvalidReturn;
                if (value) |v| {
                    if (try operandType(body, v, i) != signature.result) return error.InvalidReturn;
                } else if (self.getType(signature.result).? != .void) return error.InvalidReturn;
            },
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
                    .add, .sub, .mul, .div, .mod, .bit_and, .bit_or, .bit_xor => {
                        if (operand != .integer and !(operand == .string and binary.op == .add)) return error.InvalidOperation;
                        if (instruction.ty != lhs) return error.OperandTypeMismatch;
                    },
                    .eq, .ne, .lt, .le, .gt, .ge => {
                        if (operand != .integer and !((binary.op == .eq or binary.op == .ne) and (operand == .boolean or operand == .string or operand == .enumeration))) return error.InvalidOperation;
                        if (ty != .boolean) return error.OperandTypeMismatch;
                    },
                }
            },
            .short_circuit => |region| {
                if (ty != .boolean) return error.OperandTypeMismatch;
                if (try operandType(body, region.lhs, region.rhs_start) != instruction.ty) return error.OperandTypeMismatch;
                const rhs_ty = try regionOperandType(body, region.rhs, i, .{ .owner = i, .start = region.rhs_start });
                if (!terminates(body, region.rhs) and rhs_ty != instruction.ty) return error.OperandTypeMismatch;
            },
            .branch => |r| {
                const condition = self.getType(try operandType(body, r.condition, r.then_start)) orelse return error.InvalidTypeId;
                if (condition != .boolean) return error.OperandTypeMismatch;
                const then_ty = try regionOperandType(body, r.then_value, r.else_start, .{ .owner = i, .start = r.then_start });
                const then_returns = terminates(body, r.then_value);
                if (!then_returns and then_ty != instruction.ty) return error.OperandTypeMismatch;
                var else_returns = false;
                if (r.else_value) |value| {
                    const else_ty = try regionOperandType(body, value, i, .{ .owner = i, .start = r.else_start });
                    else_returns = terminates(body, value);
                    if (!else_returns and else_ty != instruction.ty) return error.OperandTypeMismatch;
                } else if (ty != .void) return error.OperandTypeMismatch;
                if (r.returns != (then_returns and else_returns) or (r.returns and ty != .void)) return error.InvalidReturn;
            },
            .print => |print| {
                if (ty != .void) return error.InvalidOperation;
                for (print.args) |arg| {
                    const operand = self.getType(try operandType(body, arg, i)) orelse return error.InvalidTypeId;
                    if (operand != .boolean and operand != .integer and operand != .string) return error.InvalidOperation;
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
                const reference = body.instructions[@intFromEnum(call.callee)].op;
                const direct_noreturn = if (reference == .function_ref) (self.getFunction(reference.function_ref) orelse return error.InvalidFunctionId).noreturn else false;
                if (call.noreturn and !direct_noreturn) return error.InvalidCall;
            },
        }
    }
    const result_ty = try regionOperandType(body, body.result, body.instructions.len, .{ .owner = body.instructions.len, .start = 0 });
    if (body.instructions[@intFromEnum(body.result)].op == .local_slot) return error.InvalidValueId;
    if (function.noreturn and !terminates(body, body.result)) return error.InvalidReturn;
    if (!terminates(body, body.result) and result_ty != signature.result) return error.InvalidReturn;
}

fn validateType(self: *const Ir, ty: Type, before: usize) ValidationError!void {
    switch (ty) {
        .enumeration => |enumeration| {
            if (enumeration.name.len == 0 or enumeration.tags.len == 0 or enumeration.tags.len > std.math.maxInt(u32)) return error.InvalidTypeId;
            for (enumeration.tags, 0..) |tag, i| {
                if (tag.name.len == 0) return error.InvalidTypeId;
                self.validateSpan(tag.span) catch return error.InvalidTypeId;
                for (enumeration.tags[0..i]) |previous| if (std.mem.eql(u8, previous.name, tag.name)) return error.InvalidTypeId;
            }
        },
        .record => |record| {
            for (record.type_args) |arg| if (@intFromEnum(arg) >= before) return error.InvalidTypeId;
            if (record.type_args.len != 0 and !record.tagged) return error.InvalidTypeId;
            for (record.fields, 0..) |field, i| {
                if (@intFromEnum(field.ty) >= self.types.items.len) return error.InvalidTypeId;
                const ft = self.getType(field.ty).?;
                if (!(if (record.tagged) isUnionPayload(ft) else isRecordField(ft))) return error.InvalidArrayElement;
                if (record.tagged and field.default != null) return error.InvalidTypeId;
                if (field.name.len == 0) return error.InvalidTypeId;
                self.validateSpan(field.span) catch return error.InvalidTypeId;
                for (record.fields[0..i]) |previous| if (std.mem.eql(u8, previous.name, field.name)) return error.InvalidTypeId;
                if (field.default) |default| {
                    const constant = self.getConstant(default) orelse return error.ConstantTypeMismatch;
                    if (constant.ty != field.ty) return error.ConstantTypeMismatch;
                }
            }
        },
        .void, .boolean, .string => {},
        .integer => |integer| {
            if (integer.bits == 0 or integer.bits > 128) return error.InvalidIntegerWidth;
        },
        .array => |element| {
            if (@intFromEnum(element) >= before) return error.InvalidTypeId;
            if (!isArrayElement(self.getType(element).?)) return error.InvalidArrayElement;
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
        .enumeration => |enumeration| {
            if (constant.value != .enum_tag) return error.ConstantTypeMismatch;
            if (constant.value.enum_tag >= enumeration.tags.len) return error.ConstantTypeMismatch;
        },
        .void => if (constant.value != .void) return error.ConstantTypeMismatch,
        .boolean => if (constant.value != .boolean) return error.ConstantTypeMismatch,
        .string => {
            if (constant.value != .string) return error.ConstantTypeMismatch;
            if (!std.unicode.utf8ValidateSlice(constant.value.string)) return error.ConstantTypeMismatch;
        },
        .function, .array, .record => return error.NonConstantType,
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

pub fn isNativeInteger(ty: Type, signedness: std.builtin.Signedness) bool {
    return ty == .integer and ty.integer.bits == native_pointer_bits and ty.integer.signedness == signedness;
}

fn constantEql(a: Constant, b: Constant) bool {
    if (a.ty != b.ty or std.meta.activeTag(a.value) != std.meta.activeTag(b.value)) return false;
    if (a.value == .string) return std.mem.eql(u8, a.value.string, b.value.string);
    return std.meta.eql(a, b);
}

pub fn isManaged(ty: Type) bool {
    return ty == .array or ty == .string or ty == .record;
}

/// Untagged records retain nominal identity as union payloads. Layout graph
/// validation independently rejects cycles and excessive by-value depth.
pub fn isUnionPayload(ty: Type) bool {
    return isRecordField(ty) or ty == .void or (ty == .record and !ty.record.tagged);
}

/// Record fields use semantic integer limits, not array/backend storage widths.
pub fn isRecordField(ty: Type) bool {
    return ty == .enumeration or ty == .boolean or ty == .string or (ty == .record and !ty.record.tagged) or (ty == .integer and ty.integer.bits > 0 and ty.integer.bits <= 128);
}

pub fn isArrayElement(ty: Type) bool {
    return ty == .boolean or (ty == .integer and switch (ty.integer.bits) {
        8, 16, 32, 64 => true,
        else => false,
    });
}

fn typeEql(a: Type, b: Type) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .void, .boolean, .string => true,
        .enumeration => |enumeration| enumeration.source == b.enumeration.source and enumeration.identity == b.enumeration.identity,
        .integer => |integer| std.meta.eql(integer, b.integer),
        .record => |record| record.source == b.record.source and record.identity == b.record.identity and record.tagged == b.record.tagged and std.mem.eql(TypeId, record.type_args, b.record.type_args),
        .array => |element| element == b.array,
        .function => |function| function.result == b.function.result and
            std.mem.eql(TypeId, function.params, b.function.params),
    };
}

test "shared pools retain source-qualified nominal identities after AST release" {
    const Compilation = @import("Compilation.zig");
    var ir = block: {
        var compilation = Compilation.init(std.testing.allocator);
        defer compilation.deinit();
        const first = try compilation.addSource("first.nov", "let A = true");
        const second = try compilation.addSource("second.nov", "let A = false");
        const pool = &compilation.ir;
        const boolean = try pool.internType(.boolean);
        const span: Span = .{ .source = second, .start = 4, .end = 5 };
        const field = [_]Field{.{ .name = "x", .ty = boolean, .span = span }};
        const a = try pool.internType(.{ .record = .{ .source = first, .identity = 0, .fields = &.{}, .tagged = true, .type_args = &.{boolean} } });
        const b = try pool.internType(.{ .record = .{ .source = second, .identity = 0, .fields = &.{}, .tagged = true, .type_args = &.{boolean} } });
        try pool.setRecordFields(a, &field);
        try pool.setRecordFields(b, &field);
        try std.testing.expect(a != b);
        try std.testing.expectEqual(b, try pool.internType(.{ .record = .{ .source = second, .identity = 0, .fields = &.{}, .tagged = true, .type_args = &.{boolean} } }));
        const tags = [_]EnumTag{.{ .name = "tag", .span = span }};
        const e1 = try pool.internType(.{ .enumeration = .{ .source = first, .identity = 1, .name = "E", .tags = &tags } });
        const e2 = try pool.internType(.{ .enumeration = .{ .source = second, .identity = 1, .name = "E", .tags = &tags } });
        try std.testing.expect(e1 != e2);
        const output = compilation.ir;
        compilation.ir = Ir.init(std.testing.allocator);
        break :block output;
    };
    defer ir.deinit();
    try ir.verify();
    const unit = try ir.internType(.void);
    const signature = try ir.internType(.{ .function = .{ .params = &.{}, .result = unit } });
    try std.testing.expectError(error.InvalidSourceSpan, ir.addFunction("bad", signature, .{ .source = @enumFromInt(2), .start = 0, .end = 0 }));
    try std.testing.expectError(error.InvalidSourceSpan, ir.addFunction("bad", signature, .{ .source = @enumFromInt(1), .start = 0, .end = 1000 }));
}

test "record field integer widths follow IR limits independently of array elements" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const record = try ir.internType(.{ .record = .{ .identity = 0, .fields = &.{} } });
    for ([_]std.builtin.Signedness{ .signed, .unsigned }) |signedness| {
        for (1..129) |bits| {
            const ty: Type = .{ .integer = .{ .bits = @intCast(bits), .signedness = signedness } };
            try std.testing.expect(isRecordField(ty));
            const integer = try ir.internType(ty);
            try ir.setRecordFields(record, &.{.{ .name = "x", .ty = integer, .span = .{ .start = 0, .end = 0 } }});
            try ir.verify();
            if (!isArrayElement(ty)) try std.testing.expectError(error.InvalidArrayElement, ir.internType(.{ .array = integer }));
        }
    }
    try std.testing.expect(!isRecordField(.{ .integer = .{ .bits = 0, .signedness = .signed } }));
    try std.testing.expect(!isRecordField(.{ .integer = .{ .bits = 129, .signedness = .unsigned } }));
    try std.testing.expect(!isRecordField(.void));
    try std.testing.expect(isRecordField(ir.getType(record).?));
}

test "generic nominal keys own canonical arguments and do not collapse identical layouts" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const boolean = try ir.internType(.boolean);
    const integer = try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 32 } });
    var args = [_]TypeId{boolean};
    const first = try ir.internType(.{ .record = .{ .identity = 7, .fields = &.{}, .tagged = true, .type_args = &args } });
    args[0] = integer;
    try std.testing.expectEqual(boolean, ir.getType(first).?.record.type_args[0]);
    const second = try ir.internType(.{ .record = .{ .identity = 7, .fields = &.{}, .tagged = true, .type_args = &args } });
    try std.testing.expect(first != second);
    try std.testing.expectEqual(first, try ir.internType(.{ .record = .{ .identity = 7, .fields = &.{}, .tagged = true, .type_args = &.{boolean} } }));
    try ir.verify();
    const owned = @constCast(ir.types.items[@intFromEnum(first)].record.type_args);
    owned[0] = first;
    try std.testing.expectError(error.InvalidTypeId, ir.verify());
    owned[0] = boolean;
}

test "record payload layouts reject tagged fields and forged nominal operands" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const span: Span = .{ .start = 0, .end = 0 };
    const u = try ir.internType(.{ .record = .{ .identity = 0, .fields = &.{}, .tagged = true } });
    const r = try ir.internType(.{ .record = .{ .identity = 1, .fields = &.{} } });
    const other = try ir.internType(.{ .record = .{ .identity = 2, .fields = &.{} } });
    const text = try ir.internType(.string);
    const array = try ir.internType(.{ .array = try ir.internType(.boolean) });
    try ir.setRecordFields(r, &.{.{ .name = "s", .ty = text, .span = span }});
    try ir.setRecordFields(other, &.{.{ .name = "s", .ty = text, .span = span }});
    try ir.setRecordFields(u, &.{.{ .name = "ok", .ty = r, .span = span }});
    for ([_]TypeId{ u, array }) |invalid| {
        try std.testing.expectError(error.InvalidArrayElement, ir.setRecordFields(r, &.{.{ .name = "nested", .ty = invalid, .span = span }}));
    }
    try std.testing.expectError(error.InvalidArrayElement, ir.setRecordFields(u, &.{.{ .name = "nested", .ty = u, .span = span }}));
    const sig = try ir.internType(.{ .function = .{ .params = &.{other}, .result = u } });
    const f = try ir.addFunction("f", sig, span);
    const body = [_]Instruction{
        .{ .ty = other, .span = span, .op = .{ .parameter = 0 } },
        .{ .ty = u, .span = span, .op = .{ .union_init = .{ .index = 0, .payload = @enumFromInt(0) } } },
    };
    try std.testing.expectError(error.OperandTypeMismatch, ir.setFunctionBody(f, &body, @enumFromInt(1)));
    ir.clearFunctions();
    try ir.verify();
    const fields = @constCast(ir.types.items[@intFromEnum(r)].record.fields);
    fields[0].ty = u;
    try std.testing.expectError(error.InvalidArrayElement, ir.verify());
    fields[0].ty = text;
    ir.types.items[@intFromEnum(r)].record.tagged = true;
    try std.testing.expectError(error.InvalidArrayElement, ir.verify());
    ir.types.items[@intFromEnum(r)].record.tagged = false;
    try ir.verify();
}

test "union IR rejects invalid constructors reads tags and private capture escape" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const boolean = try ir.internType(.boolean);
    const unit = try ir.internType(.void);
    const u = try ir.internType(.{ .record = .{ .identity = 0, .fields = &.{}, .tagged = true } });
    try ir.setRecordFields(u, &.{ .{ .name = "ok", .ty = boolean, .span = .{ .start = 0, .end = 0 } }, .{ .name = "none", .ty = unit, .span = .{ .start = 0, .end = 0 } } });
    const sig = try ir.internType(.{ .function = .{ .params = &.{u}, .result = boolean } });
    const f = try ir.addFunction("f", sig, .{ .start = 0, .end = 0 });
    var body = [_]Instruction{
        .{ .ty = u, .span = .{ .start = 0, .end = 0 }, .op = .{ .parameter = 0 } },
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .union_payload = .{ .base = @enumFromInt(0), .index = 0 } } },
    };
    body[1].op.union_payload.index = 2;
    try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(f, &body, @enumFromInt(1)));
    body[1].op = .{ .field_read = .{ .base = @enumFromInt(0), .index = 0 } };
    try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(f, &body, @enumFromInt(1)));
    body[1].ty = u;
    body[1].op = .{ .union_init = .{ .index = 0, .payload = null } };
    try std.testing.expectError(error.OperandTypeMismatch, ir.setFunctionBody(f, &body, @enumFromInt(0)));
    body[1].op = .{ .record_init = &.{ @enumFromInt(0), @enumFromInt(0) } };
    try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(f, &body, @enumFromInt(0)));
    body[1].ty = unit;
    body[1].op = .{ .union_tag = .{ .base = @enumFromInt(0), .index = 0 } };
    try std.testing.expectError(error.OperandTypeMismatch, ir.setFunctionBody(f, &body, @enumFromInt(0)));
    body[1].ty = boolean;
    body[1].op = .{ .union_payload = .{ .base = @enumFromInt(0), .index = 0 } };
    const private_body = [_]Instruction{
        body[0],
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .union_tag = .{ .base = @enumFromInt(0), .index = 0 } } },
        body[1],
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .branch = .{ .condition = @enumFromInt(1), .then_start = 2, .else_start = 3, .then_value = @enumFromInt(2), .else_value = @enumFromInt(1) } } },
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .unary = .{ .op = .bool_not, .operand = @enumFromInt(2) } } },
    };
    try std.testing.expectError(error.InvalidValueId, ir.setFunctionBody(f, &private_body, @enumFromInt(4)));
    try ir.setFunctionBody(f, &body, @enumFromInt(1));
    try ir.verify();
}

test "string constants are canonical owned UTF8 and operations verify exact types" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const string = try ir.internType(.string);
    const integer = try ir.internType(.{ .integer = .{ .bits = native_pointer_bits, .signedness = .unsigned } });
    const boolean = try ir.internType(.boolean);
    const signature = try ir.internType(.{ .function = .{ .params = &.{}, .result = string } });
    var bytes = [_]u8{ 'a', 0, 'b' };
    const literal = try ir.internConstant(.{ .ty = string, .value = .{ .string = &bytes } });
    bytes[0] = 'z';
    try std.testing.expectEqualStrings("a\x00b", ir.getConstant(literal).?.value.string);
    try std.testing.expectEqual(literal, try ir.internConstant(.{ .ty = string, .value = .{ .string = "a\x00b" } }));
    try std.testing.expectError(error.ConstantTypeMismatch, ir.internConstant(.{ .ty = string, .value = .{ .string = "\xff" } }));
    try std.testing.expectError(error.InvalidArrayElement, ir.internType(.{ .array = string }));
    const f = try ir.addFunction("f", signature, .{ .start = 0, .end = 0 });
    var instructions = [_]Instruction{
        .{ .ty = string, .span = .{ .start = 0, .end = 0 }, .op = .{ .constant = literal } },
        .{ .ty = string, .span = .{ .start = 0, .end = 0 }, .op = .{ .binary = .{ .op = .add, .lhs = @enumFromInt(0), .rhs = @enumFromInt(0) } } },
        .{ .ty = integer, .span = .{ .start = 0, .end = 0 }, .op = .{ .array_len = @enumFromInt(1) } },
    };
    instructions[1].op.binary.op = .sub;
    try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(f, &instructions, @enumFromInt(1)));
    instructions[1].op.binary.op = .lt;
    instructions[1].ty = boolean;
    try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(f, &instructions, @enumFromInt(0)));
    instructions[1].op.binary.op = .add;
    instructions[1].ty = string;
    instructions[2].ty = boolean;
    try std.testing.expectError(error.OperandTypeMismatch, ir.setFunctionBody(f, &instructions, @enumFromInt(1)));
    instructions[2].ty = integer;
    try ir.setFunctionBody(f, &instructions, @enumFromInt(1));
    try ir.verify();
}

test "string equality verifier requires exact string operands and boolean results" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const string = try ir.internType(.string);
    const boolean = try ir.internType(.boolean);
    const signature = try ir.internType(.{ .function = .{ .params = &.{ string, boolean }, .result = boolean } });
    const f = try ir.addFunction("equal", signature, .{ .start = 0, .end = 0 });
    var instructions = [_]Instruction{
        .{ .ty = string, .span = .{ .start = 0, .end = 0 }, .op = .{ .parameter = 0 } },
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .parameter = 1 } },
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .binary = .{ .op = .eq, .lhs = @enumFromInt(0), .rhs = @enumFromInt(0) } } },
    };
    for ([_]BinaryOp{ .eq, .ne }) |op| {
        instructions[2].op.binary.op = op;
        try ir.validateBody(ir.getFunction(f).?, .{ .instructions = &instructions, .result = @enumFromInt(2) });
        instructions[2].ty = string;
        try std.testing.expectError(error.OperandTypeMismatch, ir.setFunctionBody(f, &instructions, @enumFromInt(2)));
        instructions[2].ty = boolean;
        instructions[2].op.binary.rhs = @enumFromInt(1);
        try std.testing.expectError(error.OperandTypeMismatch, ir.setFunctionBody(f, &instructions, @enumFromInt(2)));
        instructions[2].op.binary.rhs = @enumFromInt(0);
    }
    for ([_]BinaryOp{ .lt, .le, .gt, .ge }) |op| {
        instructions[2].op.binary.op = op;
        try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(f, &instructions, @enumFromInt(2)));
    }
    instructions[2].op.binary.op = .ne;
    try ir.setFunctionBody(f, &instructions, @enumFromInt(2));
    try ir.verify();
}

test "scalar array types operations and owned operands are verified" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    ir.source_len = 1;
    const integer = try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = native_pointer_bits } });
    const uint = try ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = native_pointer_bits } });
    const boolean = try ir.internType(.boolean);
    const array = try ir.internType(.{ .array = integer });
    try std.testing.expectEqual(array, try ir.internType(.{ .array = integer }));
    try std.testing.expectError(error.InvalidArrayElement, ir.internType(.{ .array = array }));
    const unit = try ir.internType(.void);
    try std.testing.expectError(error.InvalidArrayElement, ir.internType(.{ .array = unit }));
    try std.testing.expectError(error.InvalidTypeId, ir.internType(.{ .array = @enumFromInt(999) }));
    const wide = try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 128 } });
    try std.testing.expectError(error.InvalidArrayElement, ir.internType(.{ .array = wide }));
    const signature = try ir.internType(.{ .function = .{ .params = &.{}, .result = array } });
    try std.testing.expectError(error.InvalidArrayElement, ir.internType(.{ .array = signature }));
    const number = try ir.internConstant(.{ .ty = integer, .value = .{ .signed_integer = 0 } });
    const function = try ir.addFunction("arrays", signature, .{ .start = 0, .end = 0 });
    const make = TestFunctions.instruction;
    var elements = [_]ValueId{@enumFromInt(0)};
    const instructions = [_]Instruction{
        make(integer, .{ .constant = number }),
        make(array, .{ .array_literal = &elements }),
        make(array, .{ .array_clone = @enumFromInt(1) }),
        make(integer, .{ .array_index = .{ .array = @enumFromInt(2), .index = @enumFromInt(0) } }),
        make(array, .{ .array_set = .{ .array = @enumFromInt(2), .index = @enumFromInt(0), .value = @enumFromInt(3) } }),
        make(array, .{ .array_append = .{ .lhs = @enumFromInt(2), .rhs = @enumFromInt(4) } }),
        make(uint, .{ .array_len = @enumFromInt(5) }),
    };
    try ir.setFunctionBody(function, &instructions, @enumFromInt(5));
    elements[0] = @enumFromInt(999);
    try ir.verify();
    const owned = @constCast(ir.functions.items[0].body.?.instructions);
    const Case = struct { index: usize, replacement: Instruction, err: VerifyError };
    for ([_]Case{
        .{ .index = 0, .replacement = make(boolean, .{ .constant = number }), .err = error.OperandTypeMismatch },
        .{ .index = 2, .replacement = make(array, .{ .array_clone = @enumFromInt(0) }), .err = error.OperandTypeMismatch },
        .{ .index = 2, .replacement = make(array, .{ .array_clone = @enumFromInt(99) }), .err = error.InvalidValueId },
        .{ .index = 3, .replacement = make(integer, .{ .array_index = .{ .array = @enumFromInt(0), .index = @enumFromInt(0) } }), .err = error.InvalidOperation },
        .{ .index = 3, .replacement = make(boolean, instructions[3].op), .err = error.OperandTypeMismatch },
        .{ .index = 4, .replacement = make(array, .{ .array_set = .{ .array = @enumFromInt(2), .index = @enumFromInt(2), .value = @enumFromInt(3) } }), .err = error.OperandTypeMismatch },
        .{ .index = 5, .replacement = make(array, .{ .array_append = .{ .lhs = @enumFromInt(2), .rhs = @enumFromInt(0) } }), .err = error.OperandTypeMismatch },
        .{ .index = 6, .replacement = make(integer, instructions[6].op), .err = error.OperandTypeMismatch },
    }) |case| {
        const saved = owned[case.index];
        owned[case.index] = case.replacement;
        defer owned[case.index] = saved;
        try std.testing.expectError(case.err, ir.verify());
    }
}

test "to_string verifies scalar prior operands result types spans and slot isolation" {
    const make = TestFunctions.instruction;
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    ir.source_len = 1;
    const boolean = try ir.internType(.boolean);
    const integer = try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 64 } });
    const string = try ir.internType(.string);
    const array = try ir.internType(.{ .array = integer });
    const unit = try ir.internType(.void);
    const signature = try ir.internType(.{ .function = .{ .params = &.{ integer, boolean, string, array }, .result = string } });
    const function = try ir.addFunction("format", signature, .{ .start = 0, .end = 1 });
    const body = [_]Instruction{
        make(integer, .{ .parameter = 0 }),
        make(boolean, .{ .parameter = 1 }),
        make(string, .{ .parameter = 2 }),
        make(array, .{ .parameter = 3 }),
        make(signature, .{ .function_ref = function }),
        make(unit, .{ .constant = try ir.internConstant(.{ .ty = unit, .value = .void }) }),
        make(integer, .{ .local_slot = @enumFromInt(0) }),
        make(string, .{ .to_string = @enumFromInt(0) }),
        make(string, .{ .to_string = @enumFromInt(1) }),
        make(string, .{ .to_string = @enumFromInt(2) }),
    };
    try ir.setFunctionBody(function, &body, @enumFromInt(9));
    try ir.verify();
    const owned = @constCast(ir.functions.items[0].body.?.instructions);
    for ([_]struct { value: u32, err: VerifyError }{
        .{ .value = 3, .err = error.InvalidOperation },
        .{ .value = 4, .err = error.InvalidOperation },
        .{ .value = 5, .err = error.InvalidOperation },
        .{ .value = 6, .err = error.InvalidValueId },
        .{ .value = 9, .err = error.InvalidValueId },
        .{ .value = 99, .err = error.InvalidValueId },
    }) |case| {
        owned[9].op.to_string = @enumFromInt(case.value);
        try std.testing.expectError(case.err, ir.verify());
    }
    owned[9] = body[9];
    owned[9].ty = integer;
    try std.testing.expectError(error.InvalidOperation, ir.verify());
    owned[9] = body[9];
    owned[9].span.end = 2;
    try std.testing.expectError(error.InvalidSourceSpan, ir.verify());
    owned[9] = body[9];
    try ir.verify();
}

test "membership verifies exact prior values result spans and slot isolation" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    ir.source_len = 1;
    const boolean = try ir.internType(.boolean);
    const integer = try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 16 } });
    const other = try ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = 16 } });
    const array = try ir.internType(.{ .array = integer });
    const signature = try ir.internType(.{ .function = .{ .params = &.{ integer, array, other }, .result = boolean } });
    const function = try ir.addFunction("contains", signature, .{ .start = 0, .end = 0 });
    const make = TestFunctions.instruction;
    const instructions = [_]Instruction{
        make(integer, .{ .parameter = 0 }),
        make(array, .{ .parameter = 1 }),
        make(other, .{ .parameter = 2 }),
        make(integer, .{ .local_slot = @enumFromInt(0) }),
        make(boolean, .{ .array_contains = .{ .value = @enumFromInt(0), .array = @enumFromInt(1) } }),
    };
    try ir.setFunctionBody(function, &instructions, @enumFromInt(4));
    const owned = @constCast(ir.functions.items[0].body.?.instructions);
    const Case = struct { instruction: Instruction, err: VerifyError };
    for ([_]Case{
        .{ .instruction = make(integer, instructions[4].op), .err = error.InvalidOperation },
        .{ .instruction = make(boolean, .{ .array_contains = .{ .value = @enumFromInt(2), .array = @enumFromInt(1) } }), .err = error.OperandTypeMismatch },
        .{ .instruction = make(boolean, .{ .array_contains = .{ .value = @enumFromInt(1), .array = @enumFromInt(1) } }), .err = error.OperandTypeMismatch },
        .{ .instruction = make(boolean, .{ .array_contains = .{ .value = @enumFromInt(0), .array = @enumFromInt(0) } }), .err = error.InvalidOperation },
        .{ .instruction = make(boolean, .{ .array_contains = .{ .value = @enumFromInt(4), .array = @enumFromInt(1) } }), .err = error.InvalidValueId },
        .{ .instruction = make(boolean, .{ .array_contains = .{ .value = @enumFromInt(0), .array = @enumFromInt(99) } }), .err = error.InvalidValueId },
        .{ .instruction = make(boolean, .{ .array_contains = .{ .value = @enumFromInt(3), .array = @enumFromInt(1) } }), .err = error.InvalidValueId },
        .{ .instruction = .{ .ty = boolean, .span = .{ .start = 0, .end = 2 }, .op = instructions[4].op }, .err = error.InvalidSourceSpan },
    }) |case| {
        owned[4] = case.instruction;
        try std.testing.expectError(case.err, ir.verify());
    }
    owned[4] = instructions[4];
    try ir.verify();
}

test "array operands cannot leak from a private region" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    ir.source_len = 1;
    const boolean = try ir.internType(.boolean);
    const uint = try ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = native_pointer_bits } });
    const array = try ir.internType(.{ .array = boolean });
    const truth = try ir.internConstant(.{ .ty = boolean, .value = .{ .boolean = true } });
    const signature = try ir.internType(.{ .function = .{ .params = &.{}, .result = uint } });
    const function = try ir.addFunction("private", signature, .{ .start = 0, .end = 0 });
    const make = TestFunctions.instruction;
    const body = [_]Instruction{
        make(boolean, .{ .constant = truth }),
        make(array, .{ .array_literal = &.{} }),
        make(array, .{ .array_literal = &.{} }),
        make(array, .{ .branch = .{ .condition = @enumFromInt(0), .then_start = 1, .else_start = 2, .then_value = @enumFromInt(1), .else_value = @enumFromInt(2) } }),
        make(uint, .{ .array_len = @enumFromInt(1) }),
    };
    try std.testing.expectError(error.InvalidValueId, ir.setFunctionBody(function, &body, @enumFromInt(4)));
    var valid = body;
    valid[4].op.array_len = @enumFromInt(3);
    try ir.setFunctionBody(function, &valid, @enumFromInt(4));
    try ir.verify();
    // Membership must not bypass the same prior-reference/private-region checks.
    valid[4] = make(boolean, .{ .array_contains = .{ .value = @enumFromInt(0), .array = @enumFromInt(1) } });
    const bool_signature = try ir.internType(.{ .function = .{ .params = &.{}, .result = boolean } });
    const bool_function = try ir.addFunction("private_contains", bool_signature, .{ .start = 0, .end = 0 });
    try std.testing.expectError(error.InvalidValueId, ir.setFunctionBody(bool_function, &valid, @enumFromInt(4)));
    valid[4].op.array_contains.array = @enumFromInt(3);
    try ir.setFunctionBody(bool_function, &valid, @enumFromInt(4));
    try ir.verify();
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

test "local slots require prior visible identities exact types and explicit loads" {
    var t = try TestFunctions.init(std.testing.allocator);
    defer t.ir.deinit();
    const unit = try t.ir.internType(.void);
    const make = TestFunctions.instruction;
    const base = [_]Instruction{
        make(t.integer, .{ .constant = t.number }),
        make(t.integer, .{ .local_slot = @enumFromInt(0) }),
        make(t.integer, .{ .local_load = @enumFromInt(1) }),
        make(unit, .{ .local_store = .{ .slot = @enumFromInt(1), .value = @enumFromInt(2) } }),
        make(t.integer, .{ .local_load = @enumFromInt(1) }),
    };
    const Case = struct { index: usize, instruction: Instruction, err: VerifyError };
    for ([_]Case{
        .{ .index = 1, .instruction = make(t.integer, .{ .local_slot = @enumFromInt(1) }), .err = error.InvalidValueId },
        .{ .index = 1, .instruction = make(t.boolean, .{ .local_slot = @enumFromInt(0) }), .err = error.OperandTypeMismatch },
        .{ .index = 1, .instruction = make(unit, .{ .local_slot = @enumFromInt(0) }), .err = error.InvalidOperation },
        .{ .index = 2, .instruction = make(t.integer, .{ .local_load = @enumFromInt(0) }), .err = error.InvalidOperation },
        .{ .index = 2, .instruction = make(t.integer, .{ .local_load = @enumFromInt(99) }), .err = error.InvalidValueId },
        .{ .index = 2, .instruction = make(t.boolean, .{ .local_load = @enumFromInt(1) }), .err = error.OperandTypeMismatch },
        .{ .index = 2, .instruction = make(t.integer, .{ .unary = .{ .op = .negate, .operand = @enumFromInt(1) } }), .err = error.InvalidValueId },
        .{ .index = 3, .instruction = make(unit, .{ .local_store = .{ .slot = @enumFromInt(2), .value = @enumFromInt(0) } }), .err = error.InvalidOperation },
        .{ .index = 3, .instruction = make(unit, .{ .local_store = .{ .slot = @enumFromInt(1), .value = @enumFromInt(1) } }), .err = error.InvalidValueId },
        .{ .index = 3, .instruction = make(t.integer, .{ .local_store = .{ .slot = @enumFromInt(1), .value = @enumFromInt(2) } }), .err = error.InvalidOperation },
    }) |case| {
        var bad = base;
        bad[case.index] = case.instruction;
        try t.reject(case.err, &bad, @enumFromInt(4));
    }
    try t.reject(error.InvalidValueId, base[0..2], @enumFromInt(1));
    try t.ir.setFunctionBody(t.function, &base, @enumFromInt(4));
    try t.ir.verify();
}

test "region private slots cannot escape into siblings or surrounding loads and stores" {
    var t = try TestFunctions.init(std.testing.allocator);
    defer t.ir.deinit();
    const unit = try t.ir.internType(.void);
    const truth = try t.ir.internConstant(.{ .ty = t.boolean, .value = .{ .boolean = true } });
    const make = TestFunctions.instruction;
    const base = [_]Instruction{
        make(t.boolean, .{ .constant = truth }),
        make(t.integer, .{ .constant = t.number }),
        make(t.integer, .{ .local_slot = @enumFromInt(1) }),
        make(t.integer, .{ .local_load = @enumFromInt(2) }),
        make(t.integer, .{ .constant = t.number }),
        make(t.integer, .{ .branch = .{ .condition = @enumFromInt(0), .then_start = 1, .else_start = 4, .then_value = @enumFromInt(3), .else_value = @enumFromInt(4) } }),
        make(t.integer, .{ .local_load = @enumFromInt(2) }),
    };
    try t.reject(error.InvalidValueId, &base, @enumFromInt(6));
    var bad = base;
    bad[4] = make(t.integer, .{ .local_load = @enumFromInt(2) });
    try t.reject(error.InvalidValueId, &bad, @enumFromInt(5));
    bad = base;
    bad[6] = make(unit, .{ .local_store = .{ .slot = @enumFromInt(2), .value = @enumFromInt(5) } });
    try t.reject(error.InvalidValueId, &bad, @enumFromInt(5));
    bad = base;
    bad[5].op.branch.then_value = @enumFromInt(2);
    try t.reject(error.InvalidValueId, bad[0..6], @enumFromInt(5));
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

test "loop verification isolates regions slots backedges controls and terminal tails" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const unit = try fixture.ir.internType(.void);
    const ins = TestFunctions.instruction;
    const good = [_]Instruction{
        ins(fixture.integer, .{ .parameter = 0 }),
        ins(fixture.integer, .{ .local_slot = @enumFromInt(0) }),
        ins(fixture.integer, .{ .local_load = @enumFromInt(1) }),
        ins(fixture.integer, .{ .constant = fixture.number }),
        ins(fixture.boolean, .{ .binary = .{ .op = .lt, .lhs = @enumFromInt(2), .rhs = @enumFromInt(3) } }),
        ins(fixture.integer, .{ .local_slot = @enumFromInt(0) }),
        ins(fixture.integer, .{ .local_load = @enumFromInt(5) }),
        ins(unit, .{ .local_store = .{ .slot = @enumFromInt(1), .value = @enumFromInt(6) } }),
        ins(unit, .{ .loop = .{ .condition_start = 2, .body_start = 5, .condition = @enumFromInt(4), .body = @enumFromInt(7) } }),
        ins(fixture.integer, .{ .local_load = @enumFromInt(1) }),
    };
    for (0..15) |case| {
        var bad = good;
        const expected: VerifyError = switch (case) {
            0 => b: {
                bad[8].op.loop.condition_start = 6;
                break :b error.InvalidRegion;
            },
            1 => b: {
                bad[8].op.loop.body_start = 9;
                break :b error.InvalidRegion;
            },
            2 => b: {
                bad[8].op.loop.condition = @enumFromInt(8);
                break :b error.InvalidRegion;
            },
            3 => b: {
                bad[8].op.loop.body = @enumFromInt(99);
                break :b error.InvalidRegion;
            },
            4 => b: {
                bad[8].op.loop.condition = @enumFromInt(2);
                break :b error.OperandTypeMismatch;
            },
            5 => b: {
                bad[8].op.loop.body = @enumFromInt(5);
                break :b error.InvalidValueId;
            },
            6 => b: {
                bad[9].op.local_load = @enumFromInt(5);
                break :b error.InvalidValueId;
            },
            7 => b: {
                bad[6].op.local_load = @enumFromInt(2);
                break :b error.InvalidValueId;
            },
            8 => b: {
                bad[3].op = .{ .local_load = @enumFromInt(5) };
                break :b error.InvalidValueId;
            },
            9 => b: {
                bad[6] = ins(unit, .{ .loop_break = null });
                break :b error.UnreachableInstruction;
            },
            10 => b: {
                bad[9] = ins(unit, .loop_continue);
                break :b error.InvalidRegion;
            },
            11 => b: {
                bad[7] = ins(fixture.integer, .{ .local_load = @enumFromInt(1) });
                break :b error.OperandTypeMismatch;
            },
            12 => b: {
                bad[8].ty = fixture.integer;
                break :b error.InvalidOperation;
            },
            13 => b: {
                bad[7] = ins(unit, .{ .short_circuit = .{ .kind = .@"and", .lhs = @enumFromInt(4), .rhs_start = 3, .rhs = @enumFromInt(6) } });
                break :b error.InvalidRegion;
            },
            14 => b: {
                bad[3] = ins(fixture.integer, .{ .local_slot = @enumFromInt(2) });
                bad[4].op.binary.rhs = @enumFromInt(0);
                bad[6].op.local_load = @enumFromInt(3);
                break :b error.InvalidValueId;
            },
            else => unreachable,
        };
        errdefer std.debug.print("loop verifier case {d}\n", .{case});
        try fixture.reject(expected, &bad, @enumFromInt(9));
    }
    var terminal = good;
    terminal[7] = ins(unit, .loop_continue);
    try fixture.ir.setFunctionBody(fixture.function, &terminal, @enumFromInt(9));
    try fixture.ir.verify();
}

const LoopTests = struct {
    fn reject(fixture: *TestFunctions, expected: VerifyError, instructions: []const Instruction, result: ValueId) !void {
        try fixture.reject(expected, instructions, result);
        // Exercise verification of already-published malformed IR as well as
        // validation before publication, without transferring slice ownership.
        fixture.ir.functions.items[0].body = .{ .instructions = instructions, .result = result };
        defer fixture.ir.functions.items[0].body = null;
        try std.testing.expectError(expected, fixture.ir.verify());
    }
};

test "empty exhaustion yields cannot expose sibling or nested private values" {
    for (0..6) |case| {
        var t = try TestFunctions.init(std.testing.allocator);
        defer t.ir.deinit();
        const unit = try t.ir.internType(.void);
        const nothing = try t.ir.internConstant(.{ .ty = unit, .value = .void });
        const falsity = try t.ir.internConstant(.{ .ty = t.boolean, .value = .{ .boolean = false } });
        const seven = try t.ir.internConstant(.{ .ty = t.integer, .value = .{ .signed_integer = 7 } });
        const ins = TestFunctions.instruction;
        const minimal = [_]Instruction{
            ins(t.boolean, .{ .constant = falsity }),
            ins(t.integer, .{ .constant = seven }),
            ins(unit, .{ .constant = nothing }),
            ins(t.integer, .{ .loop = .{ .condition_start = 0, .condition = @enumFromInt(0), .body_start = 1, .body = @enumFromInt(2), .else_start = 3, .else_value = @enumFromInt(1) } }),
        };
        if (case == 0) {
            try LoopTests.reject(&t, error.InvalidValueId, &minimal, @enumFromInt(3));
            continue;
        }
        const body_private = [_]Instruction{
            ins(t.integer, .{ .constant = seven }),
            ins(t.boolean, .{ .constant = falsity }),
            ins(t.integer, .{ .constant = seven }),
            ins(unit, .{ .constant = nothing }),
            ins(t.integer, .{ .loop = .{ .condition_start = 1, .condition = @enumFromInt(1), .body_start = 2, .body = @enumFromInt(3), .else_start = 4, .else_value = @enumFromInt(0) } }),
        };
        const continuation_private = [_]Instruction{
            ins(t.integer, .{ .constant = seven }),
            ins(t.boolean, .{ .constant = falsity }),
            ins(unit, .{ .constant = nothing }),
            ins(t.integer, .{ .constant = seven }),
            ins(unit, .{ .constant = nothing }),
            ins(t.integer, .{ .loop = .{ .condition_start = 1, .condition = @enumFromInt(1), .body_start = 2, .body = @enumFromInt(2), .continuation_start = 3, .continuation = @enumFromInt(4), .else_start = 5, .else_value = @enumFromInt(0) } }),
        };
        const nested_body = [_]Instruction{
            ins(t.integer, .{ .constant = seven }),
            ins(t.boolean, .{ .constant = falsity }),
            ins(t.boolean, .{ .constant = falsity }),
            ins(t.integer, .{ .constant = seven }),
            ins(t.integer, .{ .constant = seven }),
            ins(t.integer, .{ .branch = .{ .condition = @enumFromInt(2), .then_start = 3, .else_start = 4, .then_value = @enumFromInt(3), .else_value = @enumFromInt(4) } }),
            ins(unit, .{ .constant = nothing }),
            ins(t.integer, .{ .loop = .{ .condition_start = 1, .condition = @enumFromInt(1), .body_start = 2, .body = @enumFromInt(6), .else_start = 7, .else_value = @enumFromInt(0) } }),
        };
        const nested_exhaustion = [_]Instruction{
            ins(t.integer, .{ .constant = seven }),
            ins(t.boolean, .{ .constant = falsity }),
            ins(unit, .{ .constant = nothing }),
            ins(t.boolean, .{ .constant = falsity }),
            ins(t.integer, .{ .constant = seven }),
            ins(t.integer, .{ .constant = seven }),
            ins(t.integer, .{ .branch = .{ .condition = @enumFromInt(3), .then_start = 4, .else_start = 5, .then_value = @enumFromInt(4), .else_value = @enumFromInt(5) } }),
            ins(t.integer, .{ .loop = .{ .condition_start = 1, .condition = @enumFromInt(1), .body_start = 2, .body = @enumFromInt(2), .else_start = 3, .else_value = @enumFromInt(6) } }),
        };
        const enclosing_branch = [_]Instruction{
            ins(t.boolean, .{ .constant = falsity }),
            ins(t.integer, .{ .constant = seven }), // visible in the enclosing then region
            ins(t.boolean, .{ .constant = falsity }),
            ins(t.integer, .{ .constant = seven }), // private to the inner loop body
            ins(unit, .{ .constant = nothing }),
            ins(t.integer, .{ .loop = .{ .condition_start = 2, .condition = @enumFromInt(2), .body_start = 3, .body = @enumFromInt(4), .else_start = 5, .else_value = @enumFromInt(1) } }),
            ins(t.integer, .{ .constant = seven }),
            ins(t.integer, .{ .branch = .{ .condition = @enumFromInt(0), .then_start = 1, .else_start = 6, .then_value = @enumFromInt(5), .else_value = @enumFromInt(6) } }),
        };
        const good: []const Instruction = switch (case) {
            1 => &body_private,
            2 => &continuation_private,
            3 => &nested_body,
            4 => &nested_exhaustion,
            5 => &enclosing_branch,
            else => unreachable,
        };
        const private: []const ValueId = switch (case) {
            1 => &.{@enumFromInt(2)},
            2 => &.{@enumFromInt(3)},
            3 => &.{ @enumFromInt(3), @enumFromInt(4), @enumFromInt(5) },
            4 => &.{ @enumFromInt(4), @enumFromInt(5) },
            5 => &.{@enumFromInt(3)},
            else => unreachable,
        };
        const result: ValueId = @enumFromInt(good.len - 1);
        for (private) |value| {
            const bad = try std.testing.allocator.dupe(Instruction, good);
            defer std.testing.allocator.free(bad);
            const loop_index = if (case == 5) 5 else good.len - 1;
            bad[loop_index].op.loop.else_value = value;
            errdefer std.debug.print("exhaustion visibility case {d}, private value {d}\n", .{ case, @intFromEnum(value) });
            try LoopTests.reject(&t, error.InvalidValueId, bad, result);
        }
        try t.ir.setFunctionBody(t.function, good, result);
        try t.ir.verify();
    }
}

test "loop exhaustion regions verify boundaries privacy types and control ownership" {
    var t = try TestFunctions.init(std.testing.allocator);
    defer t.ir.deinit();
    const unit = try t.ir.internType(.void);
    const nothing = try t.ir.internConstant(.{ .ty = unit, .value = .void });
    const truth = try t.ir.internConstant(.{ .ty = t.boolean, .value = .{ .boolean = true } });
    const ins = TestFunctions.instruction;
    const good = [_]Instruction{
        ins(t.integer, .{ .parameter = 0 }), // 0 outer value
        ins(t.boolean, .{ .constant = truth }), // 1 condition
        ins(t.integer, .{ .parameter = 0 }), // 2 body
        ins(t.integer, .{ .local_slot = @enumFromInt(2) }),
        ins(unit, .{ .constant = nothing }),
        ins(t.integer, .{ .parameter = 0 }), // 5 continuation
        ins(unit, .{ .constant = nothing }),
        ins(t.integer, .{ .parameter = 0 }), // 7 exhaustion
        ins(t.integer, .{ .local_slot = @enumFromInt(7) }),
        ins(t.integer, .{ .local_load = @enumFromInt(8) }),
        ins(t.integer, .{ .loop = .{ .condition_start = 1, .body_start = 2, .continuation_start = 5, .else_start = 7, .condition = @enumFromInt(1), .body = @enumFromInt(4), .continuation = @enumFromInt(6), .else_value = @enumFromInt(9) } }),
    };
    for (0..19) |case| {
        var bad = good;
        const r = &bad[10].op.loop;
        const expected: VerifyError = switch (case) {
            0 => b: {
                r.else_start = null;
                break :b error.InvalidRegion;
            },
            1 => b: {
                r.else_value = null;
                break :b error.InvalidRegion;
            },
            2 => b: {
                r.else_start = 2;
                break :b error.InvalidRegion;
            },
            3 => b: {
                r.else_start = 11;
                break :b error.InvalidRegion;
            },
            4 => b: {
                r.else_start = std.math.maxInt(u32);
                break :b error.InvalidRegion;
            },
            5 => b: {
                r.else_value = @enumFromInt(10);
                break :b error.InvalidRegion;
            },
            6 => b: {
                r.else_value = @enumFromInt(8);
                break :b error.InvalidValueId;
            },
            7 => b: {
                r.else_value = @enumFromInt(2);
                break :b error.InvalidValueId;
            },
            8 => b: {
                r.else_value = @enumFromInt(5);
                break :b error.InvalidValueId;
            },
            9 => b: {
                bad[9].op.local_load = @enumFromInt(3);
                break :b error.InvalidValueId;
            },
            10 => b: {
                bad[9] = ins(unit, .{ .constant = nothing });
                break :b error.OperandTypeMismatch;
            },
            11 => b: {
                bad[9] = ins(unit, .loop_continue);
                break :b error.InvalidRegion;
            },
            12 => b: {
                bad[9] = ins(unit, .{ .loop_break = null });
                break :b error.InvalidRegion;
            },
            13 => b: {
                r.returns = true;
                break :b error.InvalidReturn;
            },
            14 => b: {
                r.unconditional = true;
                break :b error.InvalidRegion;
            },
            15 => b: {
                r.continuation_start = 7;
                break :b error.InvalidRegion;
            },
            16 => b: {
                r.body = @enumFromInt(7);
                break :b error.InvalidRegion;
            },
            17 => b: {
                bad[9].span.end = t.ir.source_len + 1;
                break :b error.InvalidSourceSpan;
            },
            18 => b: {
                bad[7] = ins(unit, .{ .return_value = @enumFromInt(0) });
                break :b error.UnreachableInstruction;
            },
            else => unreachable,
        };
        errdefer std.debug.print("exhaustion verifier case {d}\n", .{case});
        try LoopTests.reject(&t, expected, &bad, @enumFromInt(10));
    }
    try LoopTests.reject(&t, error.InvalidValueId, &good, @enumFromInt(9));
    var terminal = good;
    terminal[9] = ins(unit, .{ .return_value = @enumFromInt(0) });
    terminal[10].ty = unit;
    try LoopTests.reject(&t, error.InvalidReturn, &terminal, @enumFromInt(10));
    terminal[10].op.loop.returns = true;
    try t.ir.validateBody(t.ir.functions.items[0], .{ .instructions = &terminal, .result = @enumFromInt(10) });
    terminal[10].ty = t.integer;
    try LoopTests.reject(&t, error.InvalidReturn, &terminal, @enumFromInt(10));
    try t.ir.setFunctionBody(t.function, &good, @enumFromInt(10));
    try t.ir.verify();
    try std.testing.expectEqual(@as(?usize, null), enclosingLoop(t.ir.functions.items[0].body.?, 9));
}

test "loop continuation boundaries types and private sibling operands are verified" {
    var t = try TestFunctions.init(std.testing.allocator);
    defer t.ir.deinit();
    const unit = try t.ir.internType(.void);
    const nothing = try t.ir.internConstant(.{ .ty = unit, .value = .void });
    const truth = try t.ir.internConstant(.{ .ty = t.boolean, .value = .{ .boolean = true } });
    const ins = TestFunctions.instruction;
    const good = [_]Instruction{
        ins(t.integer, .{ .parameter = 0 }), // 0: outer value and slot
        ins(t.integer, .{ .local_slot = @enumFromInt(0) }),
        ins(t.boolean, .{ .constant = truth }), // 2: condition
        ins(t.integer, .{ .local_slot = @enumFromInt(0) }),
        ins(t.integer, .{ .local_load = @enumFromInt(3) }),
        ins(t.integer, .{ .parameter = 0 }), // 5: body
        ins(t.integer, .{ .local_slot = @enumFromInt(5) }),
        ins(t.integer, .{ .local_load = @enumFromInt(6) }),
        ins(unit, .{ .constant = nothing }),
        ins(t.integer, .{ .parameter = 0 }), // 9: continuation
        ins(t.integer, .{ .local_slot = @enumFromInt(9) }),
        ins(t.integer, .{ .local_load = @enumFromInt(10) }),
        ins(unit, .{ .constant = nothing }),
        ins(unit, .{ .loop = .{ .condition_start = 2, .body_start = 5, .continuation_start = 9, .condition = @enumFromInt(2), .body = @enumFromInt(8), .continuation = @enumFromInt(12) } }),
        ins(t.integer, .{ .local_load = @enumFromInt(1) }),
    };
    for (0..24) |case| {
        var bad = good;
        const r = &bad[13].op.loop;
        const expected: VerifyError = switch (case) {
            0 => b: {
                r.continuation_start = null;
                break :b error.InvalidRegion;
            },
            1 => b: {
                r.continuation = null;
                break :b error.InvalidRegion;
            },
            2 => b: {
                r.continuation_start = 5;
                break :b error.InvalidRegion;
            },
            3 => b: {
                r.continuation_start = 4;
                break :b error.InvalidRegion;
            },
            4 => b: {
                r.continuation_start = 13;
                break :b error.InvalidRegion;
            },
            5 => b: {
                r.continuation_start = std.math.maxInt(u32);
                break :b error.InvalidRegion;
            },
            6 => b: {
                r.continuation = @enumFromInt(8);
                break :b error.InvalidRegion;
            },
            7 => b: {
                r.continuation = @enumFromInt(13);
                break :b error.InvalidRegion;
            },
            8 => b: {
                r.continuation = @enumFromInt(std.math.maxInt(u32));
                break :b error.InvalidRegion;
            },
            9 => b: {
                r.body = @enumFromInt(9);
                break :b error.InvalidRegion;
            },
            10 => b: {
                r.body = @enumFromInt(7);
                break :b error.OperandTypeMismatch;
            },
            11 => b: {
                r.continuation = @enumFromInt(11);
                break :b error.OperandTypeMismatch;
            },
            12 => b: {
                r.continuation = @enumFromInt(10);
                break :b error.InvalidValueId;
            },
            13 => b: {
                bad[11] = ins(unit, .loop_continue);
                break :b error.UnreachableInstruction;
            },
            14 => b: {
                bad[12] = ins(unit, .loop_continue);
                r.continuation = @enumFromInt(9);
                break :b error.UnreachableInstruction;
            },
            15 => b: {
                bad[8] = ins(unit, .loop_continue);
                r.body = @enumFromInt(5);
                break :b error.UnreachableInstruction;
            },
            16 => b: {
                bad[4] = ins(unit, .loop_continue);
                break :b error.UnreachableInstruction;
            },
            17 => b: {
                bad[12] = ins(t.integer, .loop_continue);
                break :b error.InvalidOperation;
            },
            18 => b: {
                bad[12] = ins(t.integer, .{ .loop_break = null });
                break :b error.InvalidOperation;
            },
            19 => b: {
                bad[12] = ins(unit, .{ .loop_break = @enumFromInt(9) });
                break :b error.InvalidOperation;
            },
            20 => b: {
                bad[13].ty = t.integer;
                break :b error.InvalidOperation;
            },
            21 => b: {
                bad[12] = ins(unit, .{ .loop_break = null });
                bad[13].ty = @enumFromInt(999);
                break :b error.InvalidTypeId;
            },
            // A nested region may not straddle the body/continuation boundary.
            22 => b: {
                bad[11] = ins(t.integer, .{ .branch = .{ .condition = @enumFromInt(2), .then_start = 8, .else_start = 10, .then_value = @enumFromInt(8), .else_value = @enumFromInt(10) } });
                break :b error.InvalidRegion;
            },
            23 => b: {
                bad[12].span.end = 11;
                break :b error.InvalidSourceSpan;
            },
            else => unreachable,
        };
        errdefer std.debug.print("continuation verifier case {d}\n", .{case});
        try LoopTests.reject(&t, expected, &bad, @enumFromInt(14));
    }
    // Each region's values and slot identities are private, even when an
    // instruction in a later sibling or after the loop references them.
    for ([_]usize{ 4, 7, 11 }) |source| {
        for ([_]usize{ 7, 11, 14 }) |destination| {
            if (source >= destination) continue;
            for (0..3) |kind| {
                var bad = good;
                bad[destination] = switch (kind) {
                    0 => ins(t.integer, .{ .unary = .{ .op = .negate, .operand = @enumFromInt(source) } }),
                    1 => ins(t.integer, .{ .local_load = @enumFromInt(source - 1) }),
                    2 => ins(unit, .{ .local_store = .{ .slot = @enumFromInt(source - 1), .value = @enumFromInt(0) } }),
                    else => unreachable,
                };
                try LoopTests.reject(&t, error.InvalidValueId, &bad, @enumFromInt(14));
            }
        }
    }
    for ([_]usize{ 4, 8, 12 }) |tail| {
        for (0..3) |kind| {
            var valid = good;
            valid[tail] = ins(unit, switch (kind) {
                0 => .loop_continue,
                1 => .{ .loop_break = null },
                2 => .{ .return_value = @enumFromInt(0) },
                else => unreachable,
            });
            if (tail == 4) valid[13].op.loop.condition = @enumFromInt(4);
            const terminal_condition = tail == 4 and kind != 1;
            if (terminal_condition) {
                valid[13].op.loop.returns = true;
                try LoopTests.reject(&t, error.UnreachableInstruction, &valid, @enumFromInt(14));
            }
            try t.ir.setFunctionBody(t.function, valid[0..(if (terminal_condition) @as(usize, 14) else 15)], @enumFromInt(if (terminal_condition) @as(u32, 13) else 14));
            try t.ir.verify();
            const body = t.ir.functions.items[0].body.?;
            t.ir.allocator.free(body.instructions);
            t.ir.functions.items[0].body = null;
        }
    }
    try t.ir.setFunctionBody(t.function, &good, @enumFromInt(14));
    try t.ir.verify();
}

test "unconditional loop break values require visible prior exact typed operands" {
    var t = try TestFunctions.init(std.testing.allocator);
    defer t.ir.deinit();
    const unit = try t.ir.internType(.void);
    const truth = try t.ir.internConstant(.{ .ty = t.boolean, .value = .{ .boolean = true } });
    const falsity = try t.ir.internConstant(.{ .ty = t.boolean, .value = .{ .boolean = false } });
    const ins = TestFunctions.instruction;
    const good = [_]Instruction{
        ins(t.integer, .{ .parameter = 0 }),
        ins(t.integer, .{ .local_slot = @enumFromInt(0) }),
        ins(t.boolean, .{ .constant = truth }),
        ins(t.integer, .{ .constant = t.number }),
        ins(unit, .{ .loop_break = @enumFromInt(3) }),
        ins(unit, .loop_continue),
        ins(t.integer, .{ .loop = .{ .unconditional = true, .condition_start = 2, .body_start = 3, .continuation_start = 5, .condition = @enumFromInt(2), .body = @enumFromInt(4), .continuation = @enumFromInt(5) } }),
    };
    for (0..17) |case| {
        var bad = good;
        const expected: VerifyError = switch (case) {
            0 => b: {
                bad[4].op.loop_break = null;
                break :b error.OperandTypeMismatch;
            },
            1 => b: {
                bad[4].op.loop_break = @enumFromInt(4);
                break :b error.InvalidValueId;
            },
            2 => b: {
                bad[4].op.loop_break = @enumFromInt(5);
                break :b error.InvalidValueId;
            },
            3 => b: {
                bad[4].op.loop_break = @enumFromInt(std.math.maxInt(u32));
                break :b error.InvalidValueId;
            },
            4 => b: {
                bad[4].op.loop_break = @enumFromInt(1);
                break :b error.InvalidValueId;
            },
            5 => b: {
                bad[4].op.loop_break = @enumFromInt(2);
                break :b error.InvalidValueId;
            },
            6 => b: {
                bad[3] = ins(t.boolean, .{ .constant = truth });
                break :b error.OperandTypeMismatch;
            },
            7 => b: {
                bad[4].ty = t.integer;
                break :b error.InvalidOperation;
            },
            8 => b: {
                bad[6].ty = @enumFromInt(999);
                break :b error.InvalidTypeId;
            },
            9 => b: {
                bad[4].op.loop_break = null;
                bad[6].ty = @enumFromInt(999);
                break :b error.InvalidTypeId;
            },
            10 => b: {
                bad[4].op = .loop_continue;
                break :b error.InvalidOperation;
            },
            11 => b: {
                bad[4].op = .{ .return_value = @enumFromInt(3) };
                break :b error.InvalidOperation;
            },
            12 => b: {
                bad[2].op.constant = falsity;
                break :b error.InvalidOperation;
            },
            13 => b: {
                bad[2] = ins(t.integer, .{ .parameter = 0 });
                break :b error.InvalidOperation;
            },
            14 => b: {
                bad[2] = ins(t.integer, .{ .constant = t.number });
                break :b error.InvalidOperation;
            },
            15 => b: {
                bad[5].op = .{ .loop_break = @enumFromInt(3) };
                break :b error.InvalidValueId;
            },
            16 => b: {
                bad[6].op.loop.unconditional = false;
                break :b error.InvalidOperation;
            },
            else => unreachable,
        };
        errdefer std.debug.print("break value verifier case {d}\n", .{case});
        try LoopTests.reject(&t, expected, &bad, @enumFromInt(6));
    }
    // A terminal instruction in the body is not a value in the continuation.
    var bad = good;
    bad[5].op = .{ .loop_break = @enumFromInt(4) };
    try LoopTests.reject(&t, error.InvalidValueId, &bad, @enumFromInt(6));
    // The continuation may provide the value instead of the body.
    var valid = good;
    valid[4].op = .loop_continue;
    valid[5].op = .{ .loop_break = @enumFromInt(0) };
    try t.ir.setFunctionBody(t.function, &valid, @enumFromInt(6));
    try t.ir.verify();
}

test "nested loop breaks target only the innermost loop including continuations" {
    var t = try TestFunctions.init(std.testing.allocator);
    defer t.ir.deinit();
    const unit = try t.ir.internType(.void);
    const truth = try t.ir.internConstant(.{ .ty = t.boolean, .value = .{ .boolean = true } });
    const ins = TestFunctions.instruction;
    const good = [_]Instruction{
        ins(t.integer, .{ .parameter = 0 }),
        ins(t.boolean, .{ .constant = truth }),
        ins(t.boolean, .{ .constant = truth }),
        ins(unit, .loop_continue),
        ins(t.integer, .{ .parameter = 0 }),
        ins(unit, .{ .loop_break = @enumFromInt(4) }),
        ins(t.integer, .{ .loop = .{ .unconditional = true, .condition_start = 2, .body_start = 3, .continuation_start = 4, .condition = @enumFromInt(2), .body = @enumFromInt(3), .continuation = @enumFromInt(5) } }),
        ins(unit, .{ .loop_break = @enumFromInt(6) }),
        ins(t.integer, .{ .loop = .{ .unconditional = true, .condition_start = 1, .body_start = 2, .condition = @enumFromInt(1), .body = @enumFromInt(7) } }),
    };
    var bad = good;
    bad[7].op = .loop_continue;
    try LoopTests.reject(&t, error.InvalidOperation, &bad, @enumFromInt(8));
    bad = good;
    bad[5].op.loop_break = null;
    try LoopTests.reject(&t, error.OperandTypeMismatch, &bad, @enumFromInt(8));
    bad = good;
    bad[7].op.loop_break = @enumFromInt(4);
    try LoopTests.reject(&t, error.InvalidValueId, &bad, @enumFromInt(8));
    bad = good;
    bad[6].op.loop.body = @enumFromInt(5);
    try LoopTests.reject(&t, error.InvalidRegion, &bad, @enumFromInt(8));
    const body: Body = .{ .instructions = &good, .result = @enumFromInt(8) };
    try std.testing.expectEqual(@as(?usize, 6), enclosingLoop(body, 2));
    try std.testing.expectEqual(@as(?usize, 6), enclosingLoop(body, 3));
    try std.testing.expectEqual(@as(?usize, 6), enclosingLoop(body, 5));
    try std.testing.expectEqual(@as(?usize, 8), enclosingLoop(body, 6));
    try std.testing.expectEqual(@as(?usize, 8), enclosingLoop(body, 7));
    try std.testing.expectEqual(@as(?usize, null), enclosingLoop(body, 8));
    try t.ir.setFunctionBody(t.function, &good, @enumFromInt(8));
    try t.ir.verify();
}

test "unconditional loops preserve explicit resolved break types without defaults" {
    const Kind = enum { boolean, signed, unsigned, string, array, function };
    for (std.enums.values(Kind)) |kind| {
        var ir = Ir.init(std.testing.allocator);
        defer ir.deinit();
        ir.source_len = 1;
        const unit = try ir.internType(.void);
        const boolean = try ir.internType(.boolean);
        const integer = try ir.internType(.{ .integer = .{ .bits = 8, .signedness = .signed } });
        const ty = switch (kind) {
            .boolean => boolean,
            .signed => integer,
            .unsigned => try ir.internType(.{ .integer = .{ .bits = 128, .signedness = .unsigned } }),
            .string => try ir.internType(.string),
            .array => try ir.internType(.{ .array = integer }),
            .function => try ir.internType(.{ .function = .{ .params = &.{boolean}, .result = integer } }),
        };
        const signature = try ir.internType(.{ .function = .{ .params = &.{ty}, .result = ty } });
        const function = try ir.addFunction("typed", signature, test_span);
        const truth = try ir.internConstant(.{ .ty = boolean, .value = .{ .boolean = true } });
        const ins = TestFunctions.instruction;
        const good = [_]Instruction{
            ins(ty, .{ .parameter = 0 }),
            ins(boolean, .{ .constant = truth }),
            ins(unit, .{ .loop_break = @enumFromInt(0) }),
            ins(ty, .{ .loop = .{ .unconditional = true, .condition_start = 1, .body_start = 2, .condition = @enumFromInt(1), .body = @enumFromInt(2) } }),
        };
        var bad = good;
        bad[2].op.loop_break = null;
        try std.testing.expectError(error.OperandTypeMismatch, ir.setFunctionBody(function, &bad, @enumFromInt(3)));
        try std.testing.expect(ir.getFunction(function).?.body == null);
        bad[2].op = .loop_continue;
        try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(function, &bad, @enumFromInt(3)));
        try std.testing.expect(ir.getFunction(function).?.body == null);
        try ir.setFunctionBody(function, &good, @enumFromInt(3));
        try ir.verify();
        try std.testing.expectEqual(ty, ir.getFunction(function).?.body.?.instructions[3].ty);
    }
}

test "void loops accept bare breaks but only unconditional loops accept unit values" {
    for ([_]bool{ false, true }) |unconditional| {
        for ([_]bool{ false, true }) |valued| {
            var ir = Ir.init(std.testing.allocator);
            defer ir.deinit();
            ir.source_len = 1;
            const unit = try ir.internType(.void);
            const boolean = try ir.internType(.boolean);
            const signature = try ir.internType(.{ .function = .{ .params = &.{}, .result = unit } });
            const function = try ir.addFunction("unit", signature, test_span);
            const truth = try ir.internConstant(.{ .ty = boolean, .value = .{ .boolean = true } });
            const nothing = try ir.internConstant(.{ .ty = unit, .value = .void });
            const ins = TestFunctions.instruction;
            const instructions = [_]Instruction{
                ins(boolean, .{ .constant = truth }),
                ins(unit, .{ .constant = nothing }),
                ins(unit, .{ .loop_break = if (valued) @enumFromInt(1) else null }),
                ins(unit, .{ .loop = .{ .unconditional = unconditional, .condition_start = 0, .body_start = 1, .condition = @enumFromInt(0), .body = @enumFromInt(2) } }),
            };
            if (valued and !unconditional) {
                try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(function, &instructions, @enumFromInt(3)));
                try std.testing.expect(ir.getFunction(function).?.body == null);
            } else {
                try ir.setFunctionBody(function, &instructions, @enumFromInt(3));
                try ir.verify();
            }
        }
    }
}

fn testLoopAllocationFailures(allocator: std.mem.Allocator) !void {
    var t = try TestFunctions.init(allocator);
    defer t.ir.deinit();
    const unit = try t.ir.internType(.void);
    const array = try t.ir.internType(.{ .array = t.integer });
    const truth = try t.ir.internConstant(.{ .ty = t.boolean, .value = .{ .boolean = true } });
    const ins = TestFunctions.instruction;
    var args = [_]ValueId{@enumFromInt(0)};
    var instructions = [_]Instruction{
        ins(t.integer, .{ .parameter = 0 }),
        ins(t.signature, .{ .function_ref = t.function }),
        ins(t.boolean, .{ .constant = truth }),
        ins(array, .{ .array_literal = &args }),
        ins(unit, .{ .print = .{ .args = &args, .newline = false } }),
        ins(t.integer, .{ .call = .{ .callee = @enumFromInt(1), .args = &args } }),
        ins(array, .{ .array_literal = &args }),
        ins(unit, .{ .print = .{ .args = &args, .newline = true } }),
        ins(unit, .loop_continue),
        ins(t.integer, .{ .call = .{ .callee = @enumFromInt(1), .args = &args } }),
        ins(array, .{ .array_literal = &args }),
        ins(unit, .{ .print = .{ .args = &args, .newline = true } }),
        ins(unit, .{ .loop_break = @enumFromInt(9) }),
        ins(t.integer, .{ .loop = .{ .unconditional = true, .condition_start = 2, .body_start = 5, .continuation_start = 9, .condition = @enumFromInt(2), .body = @enumFromInt(8), .continuation = @enumFromInt(12) } }),
    };
    t.ir.setFunctionBody(t.function, &instructions, @enumFromInt(13)) catch |err| {
        try std.testing.expect(t.ir.getFunction(t.function).?.body == null);
        try std.testing.expectError(error.MissingFunctionBody, t.ir.verify());
        return err;
    };
    args[0] = @enumFromInt(999);
    instructions[12].op.loop_break = null;
    instructions[13].op.loop.continuation_start = null;
    try t.ir.verify();
    const body = t.ir.getFunction(t.function).?.body.?;
    try std.testing.expectEqual(@as(?u32, 9), body.instructions[13].op.loop.continuation_start);
    try std.testing.expectEqual(@as(?ValueId, @enumFromInt(9)), body.instructions[12].op.loop_break);
    for ([_]usize{ 3, 6, 10 }) |i| try std.testing.expectEqual(@as(ValueId, @enumFromInt(0)), body.instructions[i].op.array_literal[0]);
    for ([_]usize{ 4, 7, 11 }) |i| try std.testing.expectEqual(@as(ValueId, @enumFromInt(0)), body.instructions[i].op.print.args[0]);
    for ([_]usize{ 5, 9 }) |i| try std.testing.expectEqual(@as(ValueId, @enumFromInt(0)), body.instructions[i].op.call.args[0]);
    try std.testing.expectError(error.BodyAlreadySet, t.ir.setFunctionBody(t.function, &instructions, @enumFromInt(13)));
    try t.ir.verify();
    t.ir.clearFunctions();
    try t.ir.verify();
}

test "loop continuation and break body publication is atomic under every OOM" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testLoopAllocationFailures, .{});
}

test "conditional body operands are owned and publication is atomic under OOM" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testConditionalAllocationFailures, .{});
}

test "loop verifier derives terminal conditions and distinguishes owned breaks" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    ir.source_len = 1;
    const unit = try ir.internType(.void);
    const string = try ir.internType(.string);
    const boolean = try ir.internType(.boolean);
    const signature = try ir.internType(.{ .function = .{ .params = &.{}, .result = unit } });
    const function = try ir.addFunction("condition", signature, .{ .start = 0, .end = 1 });
    const text = try ir.internConstant(.{ .ty = string, .value = .{ .string = "stop" } });
    const empty = try ir.internConstant(.{ .ty = unit, .value = .void });
    const truth = try ir.internConstant(.{ .ty = boolean, .value = .{ .boolean = true } });
    const ins = TestFunctions.instruction;
    const good = [_]Instruction{
        ins(string, .{ .constant = text }),
        ins(unit, .{ .panic = @enumFromInt(0) }),
        ins(unit, .{ .constant = empty }),
        ins(unit, .{ .loop = .{ .condition_start = 0, .body_start = 2, .condition = @enumFromInt(1), .body = @enumFromInt(2), .returns = true } }),
    };
    try ir.validateBody(ir.functions.items[0], .{ .instructions = &good, .result = @enumFromInt(3) });
    var bad = good;
    bad[3].op.loop.returns = false;
    try std.testing.expectError(error.InvalidReturn, ir.validateBody(ir.functions.items[0], .{ .instructions = &bad, .result = @enumFromInt(3) }));
    for ([_]Operation{ .loop_continue, .{ .return_value = null }, .{ .call = .{ .callee = @enumFromInt(0), .args = &.{}, .noreturn = true } } }) |op| {
        var terminal = good;
        terminal[1].op = op;
        if (op == .call) {
            ir.functions.items[0].noreturn = true;
            terminal[0] = ins(signature, .{ .function_ref = function });
        }
        try ir.validateBody(ir.functions.items[0], .{ .instructions = &terminal, .result = @enumFromInt(3) });
        ir.functions.items[0].noreturn = false;
    }
    bad = good;
    bad[1].op = .{ .loop_break = null };
    try std.testing.expectError(error.InvalidReturn, ir.validateBody(ir.functions.items[0], .{ .instructions = &bad, .result = @enumFromInt(3) }));
    bad[3].op.loop.returns = false;
    try ir.validateBody(ir.functions.items[0], .{ .instructions = &bad, .result = @enumFromInt(3) });
    bad = good;
    bad[2].op = .{ .loop_break = null };
    try ir.validateBody(ir.functions.items[0], .{ .instructions = &bad, .result = @enumFromInt(3) });
    const branch = [_]Instruction{
        ins(boolean, .{ .constant = truth }),
        ins(string, .{ .constant = text }),
        ins(unit, .{ .panic = @enumFromInt(1) }),
        ins(string, .{ .constant = text }),
        ins(unit, .{ .panic = @enumFromInt(3) }),
        ins(unit, .{ .branch = .{ .condition = @enumFromInt(0), .then_start = 1, .else_start = 3, .then_value = @enumFromInt(2), .else_value = @enumFromInt(4), .returns = true } }),
        ins(unit, .{ .constant = empty }),
        ins(unit, .{ .loop = .{ .condition_start = 0, .body_start = 6, .condition = @enumFromInt(5), .body = @enumFromInt(6), .returns = true } }),
    };
    try ir.validateBody(ir.functions.items[0], .{ .instructions = &branch, .result = @enumFromInt(7) });
    var mixed = branch;
    mixed[4].op = .{ .loop_break = null };
    try std.testing.expectError(error.InvalidReturn, ir.validateBody(ir.functions.items[0], .{ .instructions = &mixed, .result = @enumFromInt(7) }));
    mixed[7].op.loop.returns = false;
    try ir.validateBody(ir.functions.items[0], .{ .instructions = &mixed, .result = @enumFromInt(7) });
    const nested = [_]Instruction{
        ins(unit, .{ .constant = empty }),
        ins(unit, .{ .loop_break = null }),
        ins(unit, .{ .constant = empty }),
        ins(unit, .{ .loop = .{ .condition_start = 0, .body_start = 2, .condition = @enumFromInt(1), .body = @enumFromInt(2) } }),
        ins(string, .{ .constant = text }),
        ins(unit, .{ .panic = @enumFromInt(4) }),
        ins(unit, .{ .constant = empty }),
        ins(unit, .{ .loop = .{ .condition_start = 0, .body_start = 6, .condition = @enumFromInt(5), .body = @enumFromInt(6), .returns = true } }),
    };
    try ir.validateBody(ir.functions.items[0], .{ .instructions = &nested, .result = @enumFromInt(7) });
    const nested_terminal = good ++ [_]Instruction{
        ins(unit, .{ .constant = empty }),
        ins(unit, .{ .loop = .{ .condition_start = 0, .body_start = 4, .condition = @enumFromInt(3), .body = @enumFromInt(4), .returns = true } }),
    };
    try ir.validateBody(ir.functions.items[0], .{ .instructions = &nested_terminal, .result = @enumFromInt(5) });
    const after = good ++ [_]Instruction{ins(unit, .{ .constant = empty })};
    try std.testing.expectError(error.UnreachableInstruction, ir.validateBody(ir.functions.items[0], .{ .instructions = &after, .result = @enumFromInt(4) }));
}

test "noreturn verifier rejects false terminal call claims returns and completion" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    ir.source_len = 1;
    const unit = try ir.internType(.void);
    const signature = try ir.internType(.{ .function = .{ .params = &.{}, .result = unit } });
    const function = try ir.addFunction("stop", signature, .{ .start = 0, .end = 1 });
    ir.functions.items[0].noreturn = true;
    const ins = TestFunctions.instruction;
    const good = [_]Instruction{
        ins(signature, .{ .function_ref = function }),
        ins(unit, .{ .call = .{ .callee = @enumFromInt(0), .args = &.{}, .noreturn = true } }),
    };
    try ir.setFunctionBody(function, &good, @enumFromInt(1));
    try ir.verify();
    ir.functions.items[0].noreturn = false;
    try std.testing.expectError(error.InvalidCall, ir.verify());
    ir.functions.items[0].noreturn = true;
    var bad = good;
    bad[1].op.call.noreturn = false;
    try std.testing.expectError(error.InvalidReturn, ir.validateBody(ir.functions.items[0], .{ .instructions = &bad, .result = @enumFromInt(1) }));
    bad = good;
    bad[1].op = .{ .return_value = null };
    try std.testing.expectError(error.InvalidReturn, ir.validateBody(ir.functions.items[0], .{ .instructions = &bad, .result = @enumFromInt(1) }));
    const value = try ir.internConstant(.{ .ty = unit, .value = .void });
    const after = good ++ [_]Instruction{ins(unit, .{ .constant = value })};
    try std.testing.expectError(error.UnreachableInstruction, ir.validateBody(ir.functions.items[0], .{ .instructions = &after, .result = @enumFromInt(2) }));
    const indirect_signature = try ir.internType(.{ .function = .{ .params = &.{signature}, .result = unit } });
    ir.functions.items[0].ty = indirect_signature;
    bad = good;
    bad[0].op = .{ .parameter = 0 };
    try std.testing.expectError(error.InvalidCall, ir.validateBody(ir.functions.items[0], .{ .instructions = &bad, .result = @enumFromInt(1) }));
    ir.functions.items[0].ty = try ir.internType(.{ .function = .{ .params = &.{}, .result = try ir.internType(.boolean) } });
    try std.testing.expectError(error.InvalidReturn, ir.verify());
}

test "panic validates exact string prior visible operands void type and terminal tails" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const unit = try fixture.ir.internType(.void);
    const string = try fixture.ir.internType(.string);
    const text = try fixture.ir.internConstant(.{ .ty = string, .value = .{ .string = "hé\x00!" } });
    const ins = TestFunctions.instruction;
    const good = [_]Instruction{
        ins(string, .{ .constant = text }),
        ins(unit, .{ .panic = @enumFromInt(0) }),
    };
    var bad = good;
    bad[1].op.panic = @enumFromInt(1);
    try fixture.reject(error.InvalidValueId, &bad, @enumFromInt(1));
    bad[1].op.panic = @enumFromInt(999);
    try fixture.reject(error.InvalidValueId, &bad, @enumFromInt(1));
    bad = good;
    bad[0] = ins(fixture.integer, .{ .parameter = 0 });
    try fixture.reject(error.OperandTypeMismatch, &bad, @enumFromInt(1));
    bad = good;
    bad[1].ty = string;
    try fixture.reject(error.InvalidOperation, &bad, @enumFromInt(1));
    const after = good ++ [_]Instruction{ins(fixture.integer, .{ .parameter = 0 })};
    try fixture.reject(error.UnreachableInstruction, &after, @enumFromInt(2));
    const consume = good ++ [_]Instruction{ins(unit, .{ .panic = @enumFromInt(1) })};
    try fixture.reject(error.UnreachableInstruction, &consume, @enumFromInt(2));
    const boolean = try fixture.ir.internConstant(.{ .ty = fixture.boolean, .value = .{ .boolean = true } });
    const private = [_]Instruction{
        ins(fixture.boolean, .{ .constant = boolean }),
        ins(string, .{ .constant = text }),
        ins(unit, .{ .panic = @enumFromInt(1) }),
        ins(unit, .{ .panic = @enumFromInt(1) }),
        ins(unit, .{ .branch = .{ .condition = @enumFromInt(0), .then_start = 1, .else_start = 3, .then_value = @enumFromInt(2), .else_value = @enumFromInt(3), .returns = true } }),
    };
    try fixture.reject(error.InvalidValueId, &private, @enumFromInt(4));
    try fixture.ir.setFunctionBody(fixture.function, &good, @enumFromInt(1));
    try fixture.ir.verify();
}

test "explicit returns validate signatures prior values spans and terminal tails" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const unit = try fixture.ir.internType(.void);
    const ins = TestFunctions.instruction;
    const good = [_]Instruction{
        ins(fixture.integer, .{ .parameter = 0 }),
        ins(unit, .{ .return_value = @enumFromInt(0) }),
    };
    var bad = good;
    bad[1].op.return_value = null;
    try fixture.reject(error.InvalidReturn, &bad, @enumFromInt(1));
    bad = good;
    bad[1].op.return_value = @enumFromInt(1);
    try fixture.reject(error.InvalidValueId, &bad, @enumFromInt(1));
    bad = good;
    bad[1].ty = fixture.integer;
    try fixture.reject(error.InvalidReturn, &bad, @enumFromInt(1));
    bad = good;
    bad[1].span.end = 11;
    try fixture.reject(error.InvalidSourceSpan, &bad, @enumFromInt(1));
    try fixture.reject(error.UnreachableInstruction, &good, @enumFromInt(0));
    const after = good ++ [_]Instruction{ins(fixture.integer, .{ .parameter = 0 })};
    try fixture.reject(error.UnreachableInstruction, &after, @enumFromInt(2));
    const wrong = [_]Instruction{
        ins(unit, .{ .constant = try fixture.ir.internConstant(.{ .ty = unit, .value = .void }) }),
        ins(unit, .{ .return_value = @enumFromInt(0) }),
    };
    try fixture.reject(error.InvalidReturn, &wrong, @enumFromInt(1));
    try fixture.ir.setFunctionBody(fixture.function, &good, @enumFromInt(1));
    try fixture.ir.verify();
}

test "void returns accept unit operands and reject nonvoid values" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    ir.source_len = 1;
    const unit = try ir.internType(.void);
    const boolean = try ir.internType(.boolean);
    const sig = try ir.internType(.{ .function = .{ .params = &.{}, .result = unit } });
    const f = try ir.addFunction("unit", sig, test_span);
    const ins = TestFunctions.instruction;
    var instructions = [_]Instruction{
        ins(unit, .{ .constant = try ir.internConstant(.{ .ty = unit, .value = .void }) }),
        ins(unit, .{ .return_value = @enumFromInt(0) }),
    };
    try ir.setFunctionBody(f, &instructions, @enumFromInt(1));
    try ir.verify();
    instructions[0] = ins(boolean, .{ .constant = try ir.internConstant(.{ .ty = boolean, .value = .{ .boolean = true } }) });
    const bad = try ir.addFunction("bad", sig, test_span);
    try std.testing.expectError(error.InvalidReturn, ir.setFunctionBody(bad, &instructions, @enumFromInt(1)));
    const bare = [_]Instruction{ins(unit, .{ .return_value = null })};
    try ir.setFunctionBody(bad, &bare, @enumFromInt(0));
    try ir.verify();
}

test "short circuit terminal RHS cannot escape or provide a fabricated live value" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const unit = try fixture.ir.internType(.void);
    const boolean_constant = try fixture.ir.internConstant(.{ .ty = fixture.boolean, .value = .{ .boolean = true } });
    const ins = TestFunctions.instruction;
    const good = [_]Instruction{
        ins(fixture.boolean, .{ .constant = boolean_constant }),
        ins(fixture.integer, .{ .parameter = 0 }),
        ins(unit, .{ .return_value = @enumFromInt(1) }),
        ins(fixture.boolean, .{ .short_circuit = .{ .kind = .@"and", .lhs = @enumFromInt(0), .rhs_start = 1, .rhs = @enumFromInt(2) } }),
        ins(fixture.integer, .{ .parameter = 0 }),
    };
    var bad = good;
    bad[4] = ins(unit, .{ .return_value = @enumFromInt(1) });
    try fixture.reject(error.InvalidValueId, &bad, @enumFromInt(4));
    bad = good;
    bad[2] = ins(unit, .{ .constant = try fixture.ir.internConstant(.{ .ty = unit, .value = .void }) });
    try fixture.reject(error.OperandTypeMismatch, &bad, @enumFromInt(4));
    bad = good;
    bad[3].op.short_circuit.rhs = @enumFromInt(0);
    try fixture.reject(error.UnreachableInstruction, &bad, @enumFromInt(4));
    try fixture.ir.setFunctionBody(fixture.function, &good, @enumFromInt(4));
    try fixture.ir.verify();
}

test "returning branches verify merge flags reachability and private return operands" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const unit = try fixture.ir.internType(.void);
    const boolean_constant = try fixture.ir.internConstant(.{ .ty = fixture.boolean, .value = .{ .boolean = true } });
    const ins = TestFunctions.instruction;
    const good = [_]Instruction{
        ins(fixture.boolean, .{ .constant = boolean_constant }),
        ins(fixture.integer, .{ .parameter = 0 }),
        ins(unit, .{ .return_value = @enumFromInt(1) }),
        ins(fixture.integer, .{ .parameter = 0 }),
        ins(unit, .{ .return_value = @enumFromInt(3) }),
        ins(unit, .{ .branch = .{ .condition = @enumFromInt(0), .then_start = 1, .else_start = 3, .then_value = @enumFromInt(2), .else_value = @enumFromInt(4), .returns = true } }),
    };
    var bad = good;
    bad[5].op.branch.returns = false;
    try fixture.reject(error.InvalidReturn, &bad, @enumFromInt(5));
    bad = good;
    bad[4].op.return_value = @enumFromInt(1);
    try fixture.reject(error.InvalidValueId, &bad, @enumFromInt(5));
    bad = good;
    bad[5].ty = fixture.integer;
    try fixture.reject(error.InvalidReturn, &bad, @enumFromInt(5));
    const after = good ++ [_]Instruction{ins(fixture.integer, .{ .parameter = 0 })};
    try fixture.reject(error.UnreachableInstruction, &after, @enumFromInt(6));
    // A non-returning branch cannot claim its successor is unreachable.
    bad = good;
    bad[4] = ins(fixture.integer, .{ .parameter = 0 });
    bad[5].ty = fixture.integer;
    try fixture.reject(error.InvalidReturn, &bad, @enumFromInt(5));
    try fixture.ir.setFunctionBody(fixture.function, &good, @enumFromInt(5));
    try fixture.ir.verify();
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
                .add, .sub, .mul, .div, .mod, .bit_and, .bit_or, .bit_xor => false,
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

test "bitwise binary verifier rejects invalid operands result types and ids" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const integer = fixture.integer;
    const first = TestFunctions.instruction(integer, .{ .parameter = 0 });
    const unsigned_constant = try fixture.ir.internConstant(.{ .ty = fixture.unsigned, .value = .{ .unsigned_integer = 1 } });
    const second = TestFunctions.instruction(fixture.unsigned, .{ .constant = unsigned_constant });
    for ([_]BinaryOp{ .bit_and, .bit_or, .bit_xor }) |op| {
        try fixture.reject(error.OperandTypeMismatch, &.{ first, second, TestFunctions.instruction(integer, .{ .binary = .{ .op = op, .lhs = @enumFromInt(0), .rhs = @enumFromInt(1) } }) }, @enumFromInt(2));
        try fixture.reject(error.OperandTypeMismatch, &.{ first, TestFunctions.instruction(fixture.unsigned, .{ .binary = .{ .op = op, .lhs = @enumFromInt(0), .rhs = @enumFromInt(0) } }) }, @enumFromInt(1));
        try fixture.reject(error.OperandTypeMismatch, &.{ first, TestFunctions.instruction(fixture.boolean, .{ .binary = .{ .op = op, .lhs = @enumFromInt(0), .rhs = @enumFromInt(0) } }) }, @enumFromInt(1));
        for ([_]u32{ 1, 999 }) |invalid| {
            try fixture.reject(error.InvalidValueId, &.{ first, TestFunctions.instruction(integer, .{ .binary = .{ .op = op, .lhs = @enumFromInt(0), .rhs = @enumFromInt(invalid) } }) }, @enumFromInt(1));
        }
        const narrow = try fixture.ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 8 } });
        const number = try fixture.ir.internConstant(.{ .ty = narrow, .value = .{ .signed_integer = 1 } });
        try fixture.reject(error.OperandTypeMismatch, &.{ first, TestFunctions.instruction(narrow, .{ .constant = number }), TestFunctions.instruction(integer, .{ .binary = .{ .op = op, .lhs = @enumFromInt(0), .rhs = @enumFromInt(1) } }) }, @enumFromInt(2));
        const string = try fixture.ir.internType(.string);
        const constant = try fixture.ir.internConstant(.{ .ty = string, .value = .{ .string = "" } });
        const array = try fixture.ir.internType(.{ .array = integer });
        for ([_]Instruction{
            TestFunctions.instruction(string, .{ .constant = constant }),
            TestFunctions.instruction(array, .{ .array_literal = &.{} }),
            TestFunctions.instruction(fixture.signature, .{ .function_ref = fixture.function }),
        }) |invalid| {
            try fixture.reject(error.InvalidOperation, &.{ invalid, TestFunctions.instruction(invalid.ty, .{ .binary = .{ .op = op, .lhs = @enumFromInt(0), .rhs = @enumFromInt(0) } }) }, @enumFromInt(1));
        }
    }
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
    for ([_]BinaryOp{ .add, .sub, .mul, .div, .mod, .bit_and, .bit_or, .bit_xor, .lt, .le, .gt, .ge }) |op| {
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

test "nested records bounded graph validation rejects forged cycles and deep layouts" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const span: Span = .{ .start = 0, .end = 0 };
    var ids: [max_record_depth + 1]TypeId = undefined;
    for (&ids, 0..) |*id, i| id.* = try ir.internType(.{ .record = .{ .identity = @intCast(i), .fields = &.{} } });
    for (ids[0 .. max_record_depth - 1], ids[1..max_record_depth]) |id, child| {
        try ir.setRecordFields(id, &.{.{ .name = "next", .ty = child, .span = span }});
    }
    try ir.verify();
    try ir.setRecordFields(ids[max_record_depth], &.{.{ .name = "head", .ty = ids[0], .span = span }});
    try std.testing.expectError(error.RecordNestingLimit, ir.verify());
    try ir.setRecordFields(ids[max_record_depth], &.{});
    try ir.setRecordFields(ids[max_record_depth - 1], &.{.{ .name = "back", .ty = ids[0], .span = span }});
    try std.testing.expectError(error.CyclicRecordLayout, ir.verify());
    try ir.setRecordFields(ids[max_record_depth - 1], &.{});
    const fields = @constCast(ir.types.items[@intFromEnum(ids[0])].record.fields);
    const saved = fields[0].ty;
    fields[0].ty = ids[0];
    try std.testing.expectError(error.CyclicRecordLayout, ir.verify());
    fields[0].ty = @enumFromInt(std.math.maxInt(u32));
    try std.testing.expectError(error.InvalidTypeId, ir.verify());
    fields[0].ty = saved;
    try ir.verify();
}

test "nested records diamond graph traversal completes without expanding value paths" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    var child = try ir.internType(.{ .record = .{ .identity = 0, .fields = &.{} } });
    for (1..100) |i| {
        const parent = try ir.internType(.{ .record = .{ .identity = @intCast(i), .fields = &.{} } });
        try ir.setRecordFields(parent, &.{
            .{ .name = "left", .ty = child, .span = .{ .start = 0, .end = 0 } },
            .{ .name = "right", .ty = child, .span = .{ .start = 0, .end = 0 } },
        });
        child = parent;
    }
    try ir.verify();
}
