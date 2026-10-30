const std = @import("std");
const TestAllocations = @import("TestAllocations.zig");
const Ir = @This();
const Source = @import("Source.zig");
const Compilation = @import("Compilation.zig");
const BorrowCheck = @import("BorrowCheck.zig");
const ExportNames = @import("ExportNames.zig");

// Integer widths describe resolved values; no literal defaults or overflow policy
// live here. Source lengths and qualified spans remain valid after AST release.
allocator: std.mem.Allocator,
types: std.ArrayList(Type) = .empty,
constants: std.ArrayList(Constant) = .empty,
/// Legacy standalone-source bound. Compilation IR uses source_lengths instead.
source_len: u32 = 0,
source_lengths: std.ArrayList(u32) = .empty,
functions: std.ArrayList(Function) = .empty,
/// Owned display names and optional ordinary nominal-to-string functions.
formatting: std.ArrayList(TypeFormatting) = .empty,

/// Compile-time canonical demand keys, independent of runtime environments.
local_generic_instances: std.ArrayList(LocalGenericInstance) = .empty,
pub const LocalGenericInstance = struct { span: Span, outer: []const TypeId, arguments: []const TypeId };
pub const TypeFormatting = struct { ty: TypeId, display_name: []const u8, formatter: ?FunctionId = null };

/// The current pipeline targets the machine running Nov, not a separate target.
/// Native aliases are canonical fixed-width integers once resolved.
pub const native_pointer_bits = @bitSizeOf(usize);
pub const native_float_bits: u16 = if (native_pointer_bits == 32) 32 else 64;

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
    is_test: bool = false,
    noreturn: bool = false,
    export_name: ?[]const u8 = null,
    extern_name: ?[]const u8 = null,
    is_closure: bool = false,
    environment_layout: ?FunctionId = null,
    captures: []const Capture = &.{},
};
/// `result` is either a continuing value or the final terminal instruction.
pub const Body = struct { instructions: []const Instruction, result: ValueId };
pub const Capture = struct { ty: TypeId, mutable: bool, environment_layout: ?FunctionId = null };
pub const Instruction = struct { ty: TypeId, span: Span, op: Operation, boxed: bool = false };
pub const UnaryOp = enum { negate, bit_not, bool_not };
pub const BinaryOp = enum { add, sub, mul, div, mod, bit_and, bit_or, bit_xor, eq, ne, lt, le, gt, ge };
pub const FieldRead = struct { base: ValueId, index: u32 };
pub const StringCursor = struct { array: ValueId, index: ValueId };
pub const Operation = union(enum) {
    // Capabilities are unit-typed, never ordinary runtime values.
    borrow_reserve: ValueId,
    // Null index denotes the whole owner; a nonnull index selects an array element.
    borrow_start: struct { reservation: ValueId, index: ?ValueId },
    borrow_read: ValueId,
    borrow_store: struct { borrow: ValueId, value: ValueId, path: []const u32 },
    range_init: struct { start: ValueId, end: ValueId, inclusive: bool },
    range_start: ValueId,
    range_end: ValueId,
    range_inclusive: ValueId,
    union_init: struct { index: u32, payload: ?ValueId },
    union_tag: FieldRead,
    /// Always checked at runtime, even for externally constructed or malformed values.
    union_payload: FieldRead,
    record_init: []const ValueId,
    field_read: FieldRead,
    /// Update only this field of the current mutable local, not a record snapshot.
    field_store: struct { slot: ValueId, index: u32, value: ValueId, path: []const u32 = &.{} },
    /// Default structural formatting; strings and runes retain their raw UTF8 bytes.
    to_string: ValueId,
    array_literal: []const ValueId,
    /// Eager recursive array backing copy, including record fields/elements.
    /// Immutable strings and closure environments remain shared.
    array_clone: ValueId,
    /// Unicode scalar count for strings; element count for arrays.
    array_len: ValueId,
    /// Internal nonnegative signed cursor limit: array elements or string bytes.
    iteration_len: ValueId,
    /// Forward UTF8 cursor operations (byte offsets, not language scalar indices).
    string_decode: StringCursor,
    string_next: StringCursor,
    /// Prior visible array/string value indexed by canonical signed pointer-sized int.
    /// Arrays yield their exact element type; immutable strings yield distinct runes.
    /// String offsets count Unicode scalars.
    array_index: struct { array: ValueId, index: ValueId },
    /// Scalar bounds; explicit inclusive end names the final included scalar.
    /// Omitted bounds default to zero and scalar length. Shares owner.
    string_slice: struct { base: ValueId, start: ?ValueId, end: ?ValueId, inclusive: bool = false },
    /// Allocation-free scalar/string equality scan; record equality is unsupported.
    array_contains: struct { value: ValueId, array: ValueId },
    array_set: struct { array: ValueId, index: ValueId, value: ValueId },
    array_append: struct { lhs: ValueId, rhs: ValueId },
    /// Terminal control flow, never a value. Its optional operand is body-local.
    return_value: ?ValueId,
    /// Terminal panic with an exact string operand; has no continuing value.
    panic: ValueId,
    /// Eager operands; failure exits at runtime but success continues.
    assertion: struct { condition: ValueId, message: ?ValueId, location: []const u8 },
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
    closure_init: struct { function: FunctionId, captures: []const ValueId },
    closure_specialize: struct { function: FunctionId, environment: ValueId },
    capture_value: u32,
    capture_cell: u32,
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

// Cache per body validation, never across metadata/type mutations. Each type
// and field edge is visited at most once, including shared nominal DAGs. The
// explicit stack also bounds traversal of forged cycles before layout checking.
const DefaultPrintability = struct {
    const State = enum { unknown, visiting, printable, unsupported };
    const Frame = struct { id: TypeId, next: usize = 0 };
    states: []State = &.{},
    stack: []Frame = &.{},

    fn deinit(self: *DefaultPrintability, allocator: std.mem.Allocator) void {
        allocator.free(self.states);
        allocator.free(self.stack);
    }

    fn check(self: *DefaultPrintability, ir: *const Ir, id: TypeId) std.mem.Allocator.Error!bool {
        if (@backingInt(id) >= ir.types.items.len) return false;
        if (self.states.len == 0) {
            const states = try ir.allocator.alloc(State, ir.types.items.len);
            errdefer ir.allocator.free(states);
            const stack = try ir.allocator.alloc(Frame, ir.types.items.len);
            for (ir.types.items, states) |ty, *state| state.* = switch (ty) {
                .boolean, .integer, .float, .rune, .string, .enumeration => .printable,
                .array, .record => .unknown,
                else => .unsupported,
            };
            // Hooks terminate structural traversal. verify() validates every
            // metadata entry independently, even if no traversal reaches it.
            for (ir.formatting.items) |entry| {
                const ty = ir.getType(entry.ty) orelse continue;
                if (entry.formatter != null and (ty == .record or ty == .enumeration)) states[@backingInt(entry.ty)] = .printable;
            }
            self.* = .{ .states = states, .stack = stack };
        }
        switch (self.states[@backingInt(id)]) {
            .printable => return true,
            .unsupported => return false,
            .unknown => {},
            .visiting => unreachable,
        }
        self.states[@backingInt(id)] = .visiting;
        self.stack[0] = .{ .id = id };
        var len: usize = 1;
        while (len != 0) {
            const frame = &self.stack[len - 1];
            const ty = ir.getType(frame.id).?;
            const child: ?TypeId = switch (ty) {
                .array => |element| if (frame.next == 0) element else null,
                .record => |record| blk: {
                    while (frame.next < record.fields.len) {
                        const field = record.fields[frame.next];
                        const field_ty = ir.getType(field.ty);
                        if (!record.tagged or field_ty == null or field_ty.? != .void) break :blk field.ty;
                        frame.next += 1;
                    }
                    break :blk null;
                },
                else => unreachable,
            };
            if (child) |child_id| {
                const index = @backingInt(child_id);
                if (index >= self.states.len) {
                    self.states[@backingInt(frame.id)] = .unsupported;
                    len -= 1;
                    continue;
                }
                switch (self.states[index]) {
                    .printable => frame.next += 1,
                    .unsupported, .visiting => {
                        self.states[@backingInt(frame.id)] = .unsupported;
                        len -= 1;
                    },
                    .unknown => {
                        self.states[index] = .visiting;
                        self.stack[len] = .{ .id = child_id };
                        len += 1;
                    },
                }
            } else {
                self.states[@backingInt(frame.id)] = .printable;
                len -= 1;
            }
        }
        return self.states[@backingInt(id)] == .printable;
    }
};

pub const Field = struct { name: []const u8, ty: TypeId, span: Span, default: ?ConstId = null };
/// Nominal by-value layout. Tagged layouts have one active scalar or record payload.
pub const Record = struct {
    source: Source.Id = @fromBackingInt(@intCast(0)),
    identity: u32,
    fields: []const Field,
    public: bool = false,
    tagged: bool = false,
    /// Empty for ordinary aggregates; canonical arguments distinguish nominal instances.
    type_args: []const TypeId = &.{},
    instance_state: enum { complete, building, failed } = .complete,
    /// Frontend-only publication state; speculative layouts have no method IDs.
    methods_published: bool = false,
};
pub const EnumTag = struct { name: []const u8, span: Span };
pub const Enumeration = struct { source: Source.Id = @fromBackingInt(@intCast(0)), identity: u32, name: []const u8, tags: []const EnumTag, public: bool = false };
pub const Type = union(enum) {
    enumeration: Enumeration,
    record: Record,
    void,
    boolean,
    rune,
    string,
    integer: Integer,
    float: u16,
    function: Type.Function,
    array: TypeId,
    /// Reusable integer bounds; inclusivity belongs to each value.
    range: TypeId,

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
        rune: u32,
        signed_integer: i128,
        unsigned_integer: u128,
        /// IEEE bits for finite values, infinities or quiet NaNs. Signaling NaNs
        /// cannot survive every supported C ABI unchanged. f32 uses the low 32
        /// bits with zero upper bits.
        float_bits: u64,
        string: []const u8,
    };
};

pub const ValidationError = error{
    InvalidTypeId,
    InvalidIntegerWidth,
    InvalidFloatWidth,
    FloatOutOfRange,
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
    InvalidTypeFormatting,
    InvalidFunctionName,
    DuplicateFunctionName,
    InvalidExportName,
    InvalidExportSignature,
    DuplicateExportName,
    InvalidExternName,
    InvalidExternSignature,
    InvalidForeignFunction,
    ExternFunctionBody,
    ConflictingForeignSignature,
    InvalidSourceSpan,
    MissingFunctionBody,
    InvalidValueId,
    InvalidParameter,
    OperandTypeMismatch,
    InvalidOperation,
    BorrowConflict,
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
    for (self.local_generic_instances.items) |instance| {
        self.allocator.free(instance.outer);
        self.allocator.free(instance.arguments);
    }
    self.local_generic_instances.deinit(self.allocator);
    for (self.formatting.items) |entry| self.allocator.free(entry.display_name);
    self.formatting.deinit(self.allocator);
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

/// Names are copied; registration may precede formatter body publication.
/// Replaces the complete entry for this canonical nominal type atomically.
pub fn setTypeFormatting(self: *Ir, ty: TypeId, display_name: []const u8, formatter: ?FunctionId) BuildError!void {
    const entry: TypeFormatting = .{ .ty = ty, .display_name = display_name, .formatter = formatter };
    try self.validateTypeFormatting(entry);
    const name = try self.allocator.dupe(u8, display_name);
    errdefer self.allocator.free(name);
    for (self.formatting.items) |*existing| {
        if (existing.ty != ty) continue;
        self.allocator.free(existing.display_name);
        existing.* = .{ .ty = ty, .display_name = name, .formatter = formatter };
        return;
    }
    try self.formatting.append(self.allocator, .{ .ty = ty, .display_name = name, .formatter = formatter });
}

/// The returned name borrows this IR, until replacement or deinitialization.
pub fn getTypeFormatting(self: *const Ir, ty: TypeId) ?TypeFormatting {
    for (self.formatting.items) |entry| if (entry.ty == ty) return entry;
    return null;
}

fn validateTypeFormatting(self: *const Ir, entry: TypeFormatting) VerifyError!void {
    const ty = self.getType(entry.ty) orelse return error.InvalidTypeId;
    if (ty != .record and ty != .enumeration) return error.InvalidTypeFormatting;
    if (entry.display_name.len == 0 or std.mem.indexOfScalar(u8, entry.display_name, 0) != null) return error.InvalidTypeFormatting;
    if (entry.formatter) |id| {
        const function = self.getFunction(id) orelse return error.InvalidFunctionId;
        const signature = self.getType(function.ty) orelse return error.InvalidTypeId;
        if (signature != .function or function.is_closure or function.noreturn) return error.InvalidTypeFormatting;
        if (signature.function.params.len != 1 or signature.function.params[0] != entry.ty or (self.getType(signature.function.result) orelse return error.InvalidTypeId) != .string) return error.InvalidTypeFormatting;
    }
}

/// Handles survive pool growth. Returned function parameter slices borrow this IR.
pub fn getType(self: *const Ir, id: TypeId) ?Type {
    const index = @backingInt(id);
    if (index >= self.types.items.len) return null;
    return self.types.items[index];
}

pub fn getConstant(self: *const Ir, id: ConstId) ?Constant {
    const index = @backingInt(id);
    if (index >= self.constants.items.len) return null;
    return self.constants.items[index];
}

/// Names and nested body slices in the returned view borrow this IR.
pub fn getFunction(self: *const Ir, id: FunctionId) ?Function {
    const index = @backingInt(id);
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
    const id: FunctionId = @fromBackingInt(@intCast(self.functions.items.len));
    try self.functions.append(self.allocator, .{ .name = owned_name, .ty = ty, .span = span });
    return id;
}

/// Owns the exact foreign symbol separately from the Nov declaration name.
pub fn setExportName(self: *Ir, id: FunctionId, name: []const u8) BuildError!void {
    const function = self.getFunction(id) orelse return error.InvalidFunctionId;
    var exported = function;
    exported.export_name = name;
    try self.validateFunction(exported);
    try self.validateForeignSymbol(id, exported);
    const owned = try self.allocator.dupe(u8, name);
    const slot = &self.functions.items[@backingInt(id)];
    if (slot.export_name) |old| self.allocator.free(old);
    slot.export_name = owned;
}

/// External declarations own a symbol but never a Nov body.
pub fn setExternName(self: *Ir, id: FunctionId, name: []const u8) BuildError!void {
    var function = self.getFunction(id) orelse return error.InvalidFunctionId;
    function.extern_name = name;
    try self.validateFunction(function);
    try self.validateForeignSymbol(id, function);
    const owned = try self.allocator.dupe(u8, name);
    const slot = &self.functions.items[@backingInt(id)];
    if (slot.extern_name) |old| self.allocator.free(old);
    slot.extern_name = owned;
}

fn validateForeignSymbol(self: *const Ir, id: FunctionId, function: Function) VerifyError!void {
    const symbol = function.export_name orelse function.extern_name orelse return;
    for (self.functions.items, 0..) |other, i| {
        if (i == @backingInt(id)) continue;
        const previous = other.export_name orelse other.extern_name orelse continue;
        if (!std.mem.eql(u8, previous, symbol)) continue;
        if (function.export_name != null and other.export_name != null) return error.DuplicateExportName;
        if (function.ty != other.ty or function.noreturn != other.noreturn) return error.ConflictingForeignSignature;
    }
}

/// Validation and allocation complete before publishing any part of the body.
/// Referenced function declarations may still be awaiting their own bodies.
pub fn setFunctionBody(self: *Ir, id: FunctionId, instructions: []const Instruction, result: ValueId) BuildError!void {
    const function = self.getFunction(id) orelse return error.InvalidFunctionId;
    if (function.extern_name != null) return error.ExternFunctionBody;
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
            .closure_init => |*closure| closure.captures = try self.allocator.dupe(ValueId, closure.captures),
            .call => |*call| call.args = try self.allocator.dupe(ValueId, call.args),
            .print => |*print| print.args = try self.allocator.dupe(ValueId, print.args),
            .assertion => |*assertion| assertion.location = try self.allocator.dupe(u8, assertion.location),
            .array_literal, .record_init => |*elements| elements.* = try self.allocator.dupe(ValueId, elements.*),
            .field_store => |*store| store.path = try self.allocator.dupe(u32, store.path),
            .borrow_store => |*store| store.path = try self.allocator.dupe(u32, store.path),
            else => {},
        }
        copied += 1;
    }
    self.functions.items[@backingInt(id)].body = .{ .instructions = owned, .result = result };
}

fn freeOperands(self: *Ir, op: Operation) void {
    switch (op) {
        .closure_init => |closure| self.allocator.free(closure.captures),
        .call => |call| self.allocator.free(call.args),
        .print => |print| self.allocator.free(print.args),
        .assertion => |assertion| self.allocator.free(assertion.location),
        .array_literal, .record_init => |elements| self.allocator.free(elements),
        .field_store => |store| self.allocator.free(store.path),
        .borrow_store => |store| self.allocator.free(store.path),
        else => {},
    }
}

/// Invalidates all function handles and borrowed function/body views, not pools.
pub fn clearFunctions(self: *Ir) void {
    for (self.formatting.items) |*entry| entry.formatter = null;
    for (self.functions.items) |function| {
        self.allocator.free(function.name);
        self.allocator.free(function.captures);
        if (function.export_name) |name| self.allocator.free(name);
        if (function.extern_name) |name| self.allocator.free(name);
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
    self.types.items[@backingInt(id)].record.fields = owned;
}

pub fn internType(self: *Ir, ty: Type) InternError!TypeId {
    try self.validateType(ty, self.types.items.len);
    for (self.types.items, 0..) |existing, i| {
        if (typeEql(existing, ty)) {
            if (ty == .enumeration) {
                if (!std.mem.eql(u8, existing.enumeration.name, ty.enumeration.name) or existing.enumeration.public != ty.enumeration.public or existing.enumeration.tags.len != ty.enumeration.tags.len) return error.InvalidTypeId;
                for (existing.enumeration.tags, ty.enumeration.tags) |a, b| if (!std.mem.eql(u8, a.name, b.name) or !std.meta.eql(a.span, b.span)) return error.InvalidTypeId;
            }
            return @fromBackingInt(@intCast(i));
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
            break :blk .{ .record = .{ .source = record.source, .identity = record.identity, .fields = try self.allocator.alloc(Field, 0), .public = record.public, .tagged = record.tagged, .type_args = args, .instance_state = record.instance_state, .methods_published = record.methods_published } };
        },
        .function => |function| .{ .function = .{
            .params = try self.allocator.dupe(TypeId, function.params),
            .result = function.result,
        } },
        else => ty,
    };
    const id: TypeId = @fromBackingInt(@intCast(self.types.items.len));
    self.types.appendAssumeCapacity(owned);
    return id;
}

pub fn internConstant(self: *Ir, constant: Constant) InternError!ConstId {
    try self.validateConstant(constant);
    for (self.constants.items, 0..) |existing, i| {
        if (constantEql(existing, constant)) return @fromBackingInt(@intCast(i));
    }
    if (self.constants.items.len == std.math.maxInt(u32)) return error.TooManyEntries;
    const id: ConstId = @fromBackingInt(@intCast(self.constants.items.len));
    try self.constants.ensureUnusedCapacity(self.allocator, 1);
    var owned = constant;
    if (constant.value == .string) owned.value = .{ .string = try self.allocator.dupe(u8, constant.value.string) };
    self.constants.appendAssumeCapacity(owned);
    return id;
}

/// Checks references, resolved value ranges and canonical identities. Deliberately
/// simple linear interning/quadratic duplicate checks suit the initial small pool.
pub fn verify(self: *const Ir) VerifyError!void {
    for (self.formatting.items, 0..) |entry, i| {
        try self.validateTypeFormatting(entry);
        for (self.formatting.items[0..i]) |previous| if (previous.ty == entry.ty) return error.InvalidTypeFormatting;
    }
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
            const a = previous.export_name orelse previous.extern_name orelse continue;
            const b = function.export_name orelse function.extern_name orelse continue;
            if (std.mem.eql(u8, a, b)) {
                if (previous.export_name != null and function.export_name != null) return error.DuplicateExportName;
                if (previous.ty != function.ty or previous.noreturn != function.noreturn) return error.ConflictingForeignSignature;
            }
        }
    }
    for (self.functions.items) |function| {
        if (function.body == null and function.extern_name == null) return error.MissingFunctionBody;
    }
    if (try self.recordLayoutIssue()) |issue| return if (issue.cycle) error.CyclicRecordLayout else error.RecordNestingLimit;
    for (self.functions.items) |function| if (function.body) |body| try self.validateBody(function, body);
}

/// Bounds all later recursive backend walks, including source-free forged IR.
pub const max_record_depth = 128;
pub const LayoutIssue = struct { span: Span, related: Span, cycle: bool };

/// Iterative tri-color DFS, linear in graph size even for diamond layouts.
/// Completed subtree heights enforce the limit independently of visitation order.
pub fn recordLayoutIssue(self: *const Ir) (std.mem.Allocator.Error || error{InvalidTypeId})!?LayoutIssue {
    var has_record = false;
    for (self.types.items) |ty| {
        if (ty == .record or ty == .array) has_record = true;
    }
    if (!has_record) return null;
    const State = struct { color: enum { white, gray, black } = .white, height: usize = 1 };
    const states = try self.allocator.alloc(State, self.types.items.len);
    defer self.allocator.free(states);
    @memset(states, .{});
    const Frame = struct { id: TypeId, next: usize = 0, span: Span = .{ .start = 0, .end = 0 } };
    var stack: [max_record_depth]Frame = undefined;
    for (self.types.items, 0..) |ty, root| {
        if ((ty != .record and ty != .array) or states[root].color != .white) continue;
        stack[0] = .{ .id = @fromBackingInt(@intCast(root)) };
        states[root].color = .gray;
        var len: usize = 1;
        while (len != 0) {
            const frame = &stack[len - 1];
            const index = @backingInt(frame.id);
            const current = self.types.items[index];
            const count = if (current == .array) @as(usize, 1) else current.record.fields.len;
            if (frame.next == count) {
                states[index].color = .black;
                len -= 1;
                continue;
            }
            const field: Field = if (current == .array)
                .{ .name = "", .ty = current.array, .span = frame.span }
            else
                current.record.fields[frame.next];
            const child_id = field.ty;
            const ft = self.getType(child_id) orelse return error.InvalidTypeId;
            const child = @backingInt(child_id);
            if (ft != .record and ft != .array) {
                frame.next += 1;
                continue;
            }
            const related = if (ft == .record and ft.record.fields.len != 0) ft.record.fields[0].span else field.span;
            if (states[child].color == .gray) return .{ .span = field.span, .related = related, .cycle = true };
            if (states[child].color == .white) {
                if (len == stack.len) return .{ .span = field.span, .related = related, .cycle = false };
                states[child].color = .gray;
                stack[len] = .{ .id = child_id, .span = field.span };
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
    const index = @backingInt(span.source);
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
    if (!function.is_closure and (function.captures.len != 0 or function.environment_layout != null)) return error.InvalidFunctionType;
    if (function.is_closure and (function.export_name != null or function.extern_name != null or function.is_test)) return error.InvalidForeignFunction;
    for (function.captures) |capture| {
        const captured = self.getType(capture.ty) orelse return error.InvalidTypeId;
        if (capture.mutable and captured == .void) return error.InvalidOperation;
        if (capture.environment_layout) |layout| {
            const carrier = self.getFunction(layout) orelse return error.InvalidFunctionId;
            if (capture.mutable or !carrier.is_closure or carrier.ty != capture.ty) return error.InvalidOperation;
        }
    }
    if (function.is_test and (ty.function.params.len != 0 or self.getType(ty.function.result).? != .void or function.noreturn or function.export_name != null or function.extern_name != null)) return error.InvalidFunctionType;
    try self.validateSpan(function.span);
    if (function.noreturn and self.getType(ty.function.result).? != .void) return error.InvalidReturn;
    if (function.extern_name) |name| {
        if (function.export_name != null) return error.InvalidForeignFunction;
        if (function.body != null) return error.ExternFunctionBody;
        if (!ExportNames.validExternName(name)) return error.InvalidExternName;
        if (!self.exportScalar(ty.function.result, true)) return error.InvalidExternSignature;
        for (ty.function.params) |param| if (!self.exportScalar(param, false)) return error.InvalidExternSignature;
    }
    if (function.export_name) |name| {
        if (!ExportNames.validName(name)) return error.InvalidExportName;
        if (!self.exportScalar(ty.function.result, true)) return error.InvalidExportSignature;
        for (ty.function.params) |param| if (!self.exportScalar(param, false)) return error.InvalidExportSignature;
    }
}

fn exportScalar(self: *const Ir, id: TypeId, result: bool) bool {
    return switch (self.getType(id).?) {
        .void => result,
        .boolean => true,
        .float => |bits| bits == 32 or bits == 64,
        .integer => |integer| switch (integer.bits) {
            8, 16, 32, 64 => true,
            else => false,
        },
        else => false,
    };
}

fn environmentLayout(self: *const Ir, function: Function, body: Body, value: ValueId, before: usize, depth: usize) VerifyError!?FunctionId {
    if (depth >= 128) return error.InvalidOperation;
    _ = try regionOperandType(body, value, before, null);
    return switch (body.instructions[@backingInt(value)].op) {
        .closure_init => |closure| closure.function,
        .local_slot => |source| blk: {
            // Template carriers are immutable. A forged store must not change
            // an environment's schema after its provenance was established.
            for (body.instructions) |instruction| if (instruction.op == .local_store and instruction.op.local_store.slot == value) return error.InvalidOperation;
            break :blk self.environmentLayout(function, body, source, @backingInt(value), depth + 1);
        },
        .local_load => |source| self.environmentLayout(function, body, source, @backingInt(value), depth + 1),
        .capture_value => |index| if (index < function.captures.len) function.captures[index].environment_layout else error.InvalidOperation,
        else => error.InvalidOperation,
    };
}

fn operandType(body: Body, id: ValueId, before: usize) VerifyError!TypeId {
    const ty = try regionOperandType(body, id, before, null);
    if (isSlot(body.instructions[@backingInt(id)])) return error.InvalidValueId;
    return ty;
}

// Empty siblings can share an end offset. Only the precise yielding region
// may expose its private result, not another region ending at the same merge.
const Yield = struct { owner: usize, start: usize };

fn regionOperandType(body: Body, id: ValueId, before: usize, yielding: ?Yield) VerifyError!TypeId {
    const index = @backingInt(id);
    if (index >= before or index >= body.instructions.len) return error.InvalidValueId;
    if (yielding != null and isSlot(body.instructions[index])) return error.InvalidValueId;
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
    return switch (body.instructions[@backingInt(id)].op) {
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

pub fn isSlot(instruction: Instruction) bool {
    return instruction.op == .local_slot or instruction.op == .capture_cell;
}

fn slotType(body: Body, id: ValueId, before: usize) VerifyError!TypeId {
    const ty = try regionOperandType(body, id, before, null);
    if (!isSlot(body.instructions[@backingInt(id)])) return error.InvalidOperation;
    return ty;
}

pub fn verifyBodyStructure(self: *const Ir, function: FunctionId, body: Body) VerifyError!void {
    try self.validateBodyStructure(self.getFunction(function) orelse return error.InvalidFunctionId, body);
}

fn validateBody(self: *const Ir, function: Function, body: Body) VerifyError!void {
    try self.validateBodyStructure(function, body);
    if (try self.borrowConflict(body) != null) return error.BorrowConflict;
}

pub const BorrowConflict = struct { access: Span, origin: Span };
pub fn borrowConflict(self: *const Ir, body: Body) VerifyError!?BorrowConflict {
    return BorrowCheck.check(self.allocator, body);
}

pub fn borrowOwner(body: Body, borrow: ValueId) ValueId {
    const reservation = body.instructions[@backingInt(borrow)].op.borrow_start.reservation;
    return body.instructions[@backingInt(reservation)].op.borrow_reserve;
}

fn borrowElement(self: *const Ir, body: Body, borrow: ValueId, before: usize) VerifyError!TypeId {
    _ = try regionOperandType(body, borrow, before, null);
    if (body.instructions[@backingInt(borrow)].op != .borrow_start) return error.InvalidOperation;
    const slot = borrowOwner(body, borrow);
    const owner_ty = body.instructions[@backingInt(slot)].ty;
    const owner = self.getType(owner_ty).?;
    return if (body.instructions[@backingInt(borrow)].op.borrow_start.index == null) owner_ty else owner.array;
}

fn validateBodyStructure(self: *const Ir, function: Function, body: Body) VerifyError!void {
    var printability: DefaultPrintability = .{};
    defer printability.deinit(self.allocator);
    if (function.extern_name != null) return error.ExternFunctionBody;
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
                    if (r.unconditional or start <= r.body_start or start > end or @backingInt(r.else_value.?) >= end) return error.InvalidRegion;
                }
                if (r.condition_start >= r.body_start or r.body_start >= end or
                    @backingInt(r.condition) < r.condition_start or @backingInt(r.condition) >= r.body_start or
                    @backingInt(r.body) < r.body_start or @backingInt(r.body) >= (r.continuation_start orelse control_end)) return error.InvalidRegion;
                if ((r.continuation_start == null) != (r.continuation == null)) return error.InvalidRegion;
                if (r.continuation_start) |start| {
                    if (start <= r.body_start or start >= control_end or @backingInt(r.continuation.?) < start or @backingInt(r.continuation.?) >= control_end) return error.InvalidRegion;
                }
            },
            .short_circuit => |r| {
                if (r.rhs_start > end or @backingInt(r.lhs) >= r.rhs_start) return error.InvalidRegion;
            },
            .branch => |r| {
                if (r.then_start > r.else_start or r.else_start > end or
                    @backingInt(r.condition) >= r.then_start or
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
        if (terminates(body, @fromBackingInt(@intCast(i)))) {
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
            if (i + 1 != region_end or @backingInt(result) != i) return error.UnreachableInstruction;
        }
        if (instruction.boxed and instruction.op != .local_slot and instruction.op != .capture_cell) return error.InvalidOperation;
        switch (instruction.op) {
            .closure_specialize => |closure| {
                const target = self.getFunction(closure.function) orelse return error.InvalidFunctionId;
                if (!target.is_closure or target.ty != instruction.ty) return error.InvalidOperation;
                _ = try operandType(body, closure.environment, i);
                const layout = target.environment_layout orelse return error.InvalidOperation;
                if (try self.environmentLayout(function, body, closure.environment, i, 0) != layout) return error.InvalidOperation;
                const carrier = self.getFunction(layout) orelse return error.InvalidFunctionId;
                if (target.captures.len != carrier.captures.len) return error.InvalidOperation;
                for (target.captures, carrier.captures) |a, b| if (a.ty != b.ty or a.mutable != b.mutable or a.environment_layout != b.environment_layout) return error.InvalidOperation;
            },
            .closure_init => |closure| {
                const target = self.getFunction(closure.function) orelse return error.InvalidFunctionId;
                if (!target.is_closure or target.environment_layout != null or target.ty != instruction.ty or closure.captures.len != target.captures.len) return error.InvalidOperation;
                for (closure.captures, target.captures) |value, capture| {
                    if (capture.mutable) {
                        if (try slotType(body, value, i) != capture.ty or !body.instructions[@backingInt(value)].boxed) return error.InvalidOperation;
                    } else {
                        if (try operandType(body, value, i) != capture.ty) return error.OperandTypeMismatch;
                        if (capture.environment_layout) |layout| if (try self.environmentLayout(function, body, value, i, 0) != layout) return error.InvalidOperation;
                    }
                }
            },
            .capture_value, .capture_cell => |index| {
                if (!function.is_closure or index >= function.captures.len) return error.InvalidOperation;
                const capture = function.captures[index];
                if (capture.ty != instruction.ty or capture.mutable != (instruction.op == .capture_cell) or capture.mutable != instruction.boxed) return error.InvalidOperation;
            },
            .borrow_reserve => |slot| {
                if (ty != .void) return error.InvalidOperation;
                const owner = self.getType(try slotType(body, slot, i)).?;
                if (owner == .void) return error.InvalidOperation;
            },
            .borrow_start => |start| {
                if (ty != .void) return error.InvalidOperation;
                _ = try regionOperandType(body, start.reservation, i, null);
                if (body.instructions[@backingInt(start.reservation)].op != .borrow_reserve) return error.InvalidOperation;
                const slot = body.instructions[@backingInt(start.reservation)].op.borrow_reserve;
                const owner = self.getType(try slotType(body, slot, i)).?;
                if (start.index) |index| {
                    if (owner != .array) return error.InvalidOperation;
                    if (!isNativeInteger(self.getType(try operandType(body, index, i)).?, .signed)) return error.OperandTypeMismatch;
                }
                // One normalized index per reservation; capabilities cannot be forked.
                for (body.instructions[0..i]) |previous| {
                    if (previous.op == .borrow_start and previous.op.borrow_start.reservation == start.reservation) return error.InvalidOperation;
                }
            },
            .borrow_read => |borrow| {
                if (instruction.ty != try self.borrowElement(body, borrow, i)) return error.OperandTypeMismatch;
            },
            .borrow_store => |store| {
                if (ty != .void or store.path.len > max_record_depth) return error.InvalidOperation;
                var field_ty = try self.borrowElement(body, store.borrow, i);

                for (store.path) |index| {
                    const record = self.getType(field_ty).?;
                    if (record != .record or record.record.tagged or index >= record.record.fields.len) return error.InvalidOperation;
                    field_ty = record.record.fields[index].ty;
                }
                if (try operandType(body, store.value, i) != field_ty) return error.OperandTypeMismatch;
            },
            .range_init => |r| {
                if (ty != .range) return error.InvalidOperation;
                if (try operandType(body, r.start, i) != ty.range or try operandType(body, r.end, i) != ty.range) return error.OperandTypeMismatch;
            },
            .range_start, .range_end, .range_inclusive => |value| {
                const range_ty = self.getType(try operandType(body, value, i)) orelse return error.InvalidTypeId;
                if (range_ty != .range) return error.InvalidOperation;
                if (instruction.op == .range_inclusive) {
                    if (ty != .boolean) return error.OperandTypeMismatch;
                } else if (instruction.ty != range_ty.range) return error.OperandTypeMismatch;
            },
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
                const operand = try operandType(body, value, i);
                if (!try printability.check(self, operand)) return error.InvalidOperation;
            },
            .array_literal => |elements| {
                if (ty != .array) return error.InvalidOperation;
                for (elements) |element| {
                    if (try operandType(body, element, i) != ty.array) return error.OperandTypeMismatch;
                }
            },
            .array_clone => |value| {
                if (!try self.needsArrayCopy(instruction.ty)) return error.InvalidOperation;
                if (try operandType(body, value, i) != instruction.ty) return error.OperandTypeMismatch;
            },
            .array_len => |value| {
                if (!isNativeInteger(ty, .unsigned)) return error.OperandTypeMismatch;
                const base = self.getType(try operandType(body, value, i)).?;
                if (base != .array and base != .string) return error.InvalidOperation;
            },
            .iteration_len => |value| {
                if (!isNativeInteger(ty, .signed)) return error.OperandTypeMismatch;
                const base = self.getType(try operandType(body, value, i)).?;
                if (base != .array and base != .string) return error.InvalidOperation;
            },
            .string_decode, .string_next => |r| {
                if (self.getType(try operandType(body, r.array, i)).? != .string) return error.InvalidOperation;
                if (!isNativeInteger(self.getType(try operandType(body, r.index, i)).?, .signed)) return error.OperandTypeMismatch;
                if (instruction.op == .string_decode) {
                    if (ty != .rune) return error.OperandTypeMismatch;
                } else if (!isNativeInteger(ty, .signed)) return error.OperandTypeMismatch;
            },
            .string_slice => |r| {
                if (ty != .string) return error.OperandTypeMismatch;
                if (self.getType(try operandType(body, r.base, i)).? != .string) return error.InvalidOperation;
                for ([_]?ValueId{ r.start, r.end }) |bound| {
                    if (bound) |value| {
                        if (!isNativeInteger(self.getType(try operandType(body, value, i)).?, .signed)) return error.OperandTypeMismatch;
                    }
                }
            },
            .array_index => |r| {
                const array = self.getType(try operandType(body, r.array, i)).?;
                const index = self.getType(try operandType(body, r.index, i)).?;
                switch (array) {
                    .array => |element| if (element != instruction.ty) return error.OperandTypeMismatch,
                    .string => if (ty != .rune) return error.OperandTypeMismatch,
                    else => return error.InvalidOperation,
                }
                if (!isNativeInteger(index, .signed)) return error.OperandTypeMismatch;
            },
            .array_contains => |r| {
                if (ty != .boolean) return error.InvalidOperation;
                const value = try operandType(body, r.value, i);
                const array = self.getType(try operandType(body, r.array, i)).?;
                if (array != .array) return error.InvalidOperation;
                if (value != array.array) return error.OperandTypeMismatch;
                if (!hasEquality(self.getType(value).?)) return error.InvalidOperation;
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
                if (ty != .boolean and ty != .integer and ty != .float and ty != .rune and ty != .function and ty != .array and ty != .string and ty != .record and ty != .enumeration and ty != .range) return error.InvalidOperation;
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
                    const condition = body.instructions[@backingInt(r.condition)];
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
                if (referenced.is_test or referenced.is_closure) return error.InvalidOperation;
                if (referenced.ty != instruction.ty) return error.OperandTypeMismatch;
            },
            .unary => |unary| {
                const operand = try operandType(body, unary.operand, i);
                if (operand != instruction.ty) return error.OperandTypeMismatch;
                switch (unary.op) {
                    .negate => if (ty != .float and (ty != .integer or ty.integer.signedness != .signed)) return error.InvalidOperation,
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
                        if (operand != .integer and !(operand == .float and (binary.op == .add or binary.op == .sub or binary.op == .mul or binary.op == .div)) and !(operand == .string and binary.op == .add)) return error.InvalidOperation;
                        if (instruction.ty != lhs) return error.OperandTypeMismatch;
                    },
                    .eq, .ne, .lt, .le, .gt, .ge => {
                        if (operand != .integer and operand != .float and !((binary.op == .eq or binary.op == .ne) and (operand == .boolean or operand == .rune or operand == .string or operand == .enumeration))) return error.InvalidOperation;
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
            .assertion => |assertion| {
                if (ty != .void) return error.InvalidOperation;
                if (self.getType(try operandType(body, assertion.condition, i)).? != .boolean) return error.OperandTypeMismatch;
                if (assertion.message) |message| {
                    if (self.getType(try operandType(body, message, i)).? != .string) return error.OperandTypeMismatch;
                }
                if (assertion.location.len == 0 or std.mem.indexOfScalar(u8, assertion.location, 0) != null) return error.InvalidOperation;
            },
            .print => |print| {
                if (ty != .void) return error.InvalidOperation;
                for (print.args) |arg| {
                    const operand = try operandType(body, arg, i);
                    if (!try printability.check(self, operand)) return error.InvalidOperation;
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
                const reference = body.instructions[@backingInt(call.callee)].op;
                const direct_noreturn = if (reference == .function_ref) (self.getFunction(reference.function_ref) orelse return error.InvalidFunctionId).noreturn else false;
                if (call.noreturn and !direct_noreturn) return error.InvalidCall;
            },
        }
    }
    const result_ty = try regionOperandType(body, body.result, body.instructions.len, .{ .owner = body.instructions.len, .start = 0 });
    if (body.instructions[@backingInt(body.result)].op == .local_slot) return error.InvalidValueId;
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
            for (record.type_args) |arg| if (@backingInt(arg) >= before) return error.InvalidTypeId;
            for (record.fields, 0..) |field, i| {
                if (@backingInt(field.ty) >= self.types.items.len) return error.InvalidTypeId;
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
        .void, .boolean, .rune, .string => {},
        .float => |bits| if (bits != 32 and bits != 64) return error.InvalidFloatWidth,
        .integer => |integer| {
            if (integer.bits == 0 or integer.bits > 128) return error.InvalidIntegerWidth;
        },
        .range => |element| {
            if (@backingInt(element) >= before) return error.InvalidTypeId;
            if (self.getType(element).? != .integer) return error.InvalidTypeId;
        },
        .array => |element| {
            if (@backingInt(element) >= before) return error.InvalidTypeId;
            if (!isArrayElement(self.getType(element).?)) return error.InvalidArrayElement;
        },
        .function => |function| {
            // Types form a DAG; recursive function values do not require cycles
            // in their signature types. Only refer to previously interned types.
            if (@backingInt(function.result) >= before) return error.InvalidTypeId;
            for (function.params) |param| {
                if (@backingInt(param) >= before) return error.InvalidTypeId;
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
        .rune => {
            if (constant.value != .rune) return error.ConstantTypeMismatch;
            if (!isRuneValue(constant.value.rune)) return error.ConstantTypeMismatch;
        },
        .string => {
            if (constant.value != .string) return error.ConstantTypeMismatch;
            if (!std.unicode.utf8ValidateSlice(constant.value.string)) return error.ConstantTypeMismatch;
        },
        .float => |bits| {
            if (constant.value != .float_bits) return error.ConstantTypeMismatch;
            const pattern = constant.value.float_bits;
            if (bits == 32 and pattern > std.math.maxInt(u32)) return error.FloatOutOfRange;
            const exponent_mask: u64 = if (bits == 32) 0x7f800000 else 0x7ff0000000000000;
            const fraction_mask: u64 = if (bits == 32) 0x007fffff else 0x000fffffffffffff;
            const quiet_mask: u64 = if (bits == 32) 0x00400000 else 0x0008000000000000;
            if (pattern & exponent_mask == exponent_mask and pattern & fraction_mask != 0 and pattern & quiet_mask == 0) return error.FloatOutOfRange;
        },
        .function, .array, .record, .range => return error.NonConstantType,
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

/// Unicode scalar values exclude surrogate code points.
pub fn isRuneValue(value: u32) bool {
    return value <= 0x10ffff and (value < 0xd800 or value > 0xdfff);
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
    return (ty != .array and isRecordField(ty)) or ty == .void or (ty == .record and !ty.record.tagged);
}

/// Query the layout DAG, visiting scalar diamonds only once. Closure values
/// intentionally do not traverse their signatures or shared environments.
pub fn needsArrayCopy(self: *const Ir, id: TypeId) std.mem.Allocator.Error!bool {
    const ty = self.getType(id) orelse return false;
    if (ty == .array) return true;
    if (ty != .record) return false;
    const visited = try self.allocator.alloc(bool, self.types.items.len);
    defer self.allocator.free(visited);
    @memset(visited, false);
    const work = try self.allocator.alloc(TypeId, self.types.items.len);
    defer self.allocator.free(work);
    work[0] = id;
    visited[@backingInt(id)] = true;
    var len: usize = 1;
    while (len != 0) {
        len -= 1;
        const current = self.getType(work[len]).?.record;
        for (current.fields) |field| {
            const ft = self.getType(field.ty) orelse continue;
            if (ft == .array) return true;
            if (ft != .record or visited[@backingInt(field.ty)]) continue;
            visited[@backingInt(field.ty)] = true;
            work[len] = field.ty;
            len += 1;
        }
    }
    return false;
}

/// Record fields use semantic integer limits, not array/backend storage widths.
pub fn isRecordField(ty: Type) bool {
    return (ty == .float and (ty.float == 32 or ty.float == 64)) or ty == .array or ty == .enumeration or ty == .boolean or ty == .rune or ty == .string or (ty == .record and !ty.record.tagged) or (ty == .integer and ty.integer.bits > 0 and ty.integer.bits <= 128);
}

pub fn isArrayElement(ty: Type) bool {
    return switch (ty) {
        .void => false,
        .integer => |integer| integer.bits > 0 and integer.bits <= 128,
        .float => |bits| bits == 32 or bits == 64,
        .boolean, .rune, .string, .enumeration, .record, .function, .array, .range => true,
    };
}

/// Storage support does not imply equality (notably for closures and aggregates).
pub fn hasEquality(ty: Type) bool {
    return switch (ty) {
        .boolean, .integer, .float, .rune, .string, .enumeration => true,
        else => false,
    };
}

fn typeEql(a: Type, b: Type) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .void, .boolean, .rune, .string => true,
        .enumeration => |enumeration| enumeration.source == b.enumeration.source and enumeration.identity == b.enumeration.identity,
        .integer => |integer| std.meta.eql(integer, b.integer),
        .float => |bits| bits == b.float,
        .record => |record| record.source == b.record.source and record.identity == b.record.identity and record.tagged == b.record.tagged and std.mem.eql(TypeId, record.type_args, b.record.type_args),
        .range => |element| element == b.range,
        .array => |element| element == b.array,
        .function => |function| function.result == b.function.result and
            std.mem.eql(TypeId, function.params, b.function.params),
    };
}

test "shared pools retain source-qualified nominal identities after AST release" {
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
    try std.testing.expectError(error.InvalidSourceSpan, ir.addFunction("bad", signature, .{ .source = @fromBackingInt(@intCast(2)), .start = 0, .end = 0 }));
    try std.testing.expectError(error.InvalidSourceSpan, ir.addFunction("bad", signature, .{ .source = @fromBackingInt(@intCast(1)), .start = 0, .end = 1000 }));
}

test "record and array integer widths follow semantic IR limits" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const record = try ir.internType(.{ .record = .{ .identity = 0, .fields = &.{} } });
    for ([_]std.builtin.Signedness{ .signed, .unsigned }) |signedness| {
        for (1..129) |bits| {
            const ty: Type = .{ .integer = .{ .bits = @intCast(bits), .signedness = signedness } };
            try std.testing.expect(isRecordField(ty));
            const integer = try ir.internType(ty);
            try ir.setRecordFields(record, &.{.{ .name = "x", .ty = integer, .span = .{ .start = 0, .end = 0 } }});
            try std.testing.expect(isArrayElement(ty));
            _ = try ir.internType(.{ .array = integer });
            try ir.verify();
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
    const owned = @constCast(ir.types.items[@backingInt(first)].record.type_args);
    owned[0] = first;
    try std.testing.expectError(error.InvalidTypeId, ir.verify());
    owned[0] = boolean;
}

test "generic structs IR owns nominal arguments accepts array fields and rejects recursive keys" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const integer = try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 32 } });
    const boolean = try ir.internType(.boolean);
    const array = try ir.internType(.{ .array = integer });
    var args = [_]TypeId{integer};
    const a = try ir.internType(.{ .record = .{ .identity = 7, .type_args = &args, .fields = &.{} } });
    args[0] = boolean;
    const b = try ir.internType(.{ .record = .{ .identity = 7, .type_args = &args, .fields = &.{} } });
    const other = try ir.internType(.{ .record = .{ .identity = 8, .type_args = &.{integer}, .fields = &.{} } });
    try std.testing.expect(a != b and a != other);
    try std.testing.expectEqual(integer, ir.getType(a).?.record.type_args[0]);
    try std.testing.expectEqual(a, try ir.internType(.{ .record = .{ .identity = 7, .type_args = &.{integer}, .fields = &.{} } }));
    const field: Field = .{ .name = "value", .ty = integer, .span = .{ .start = 0, .end = 0 } };
    try ir.setRecordFields(a, &.{field});
    try ir.verify();
    const owned_args = @constCast(ir.types.items[@backingInt(a)].record.type_args);
    for ([_]TypeId{ a, b, other, @fromBackingInt(@intCast(std.math.maxInt(u32))) }) |bad| {
        owned_args[0] = bad;
        try std.testing.expectError(error.InvalidTypeId, ir.verify());
    }
    owned_args[0] = integer;
    const fields = @constCast(ir.types.items[@backingInt(a)].record.fields);
    const saved = fields[0];
    fields[0].ty = array;
    try ir.verify();
    fields[0] = saved;
    const wrong_default = try ir.internConstant(.{ .ty = boolean, .value = .{ .boolean = true } });
    fields[0].default = wrong_default;
    try std.testing.expectError(error.ConstantTypeMismatch, ir.verify());
    fields[0] = saved;
    try ir.verify();
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
    try std.testing.expectError(error.InvalidArrayElement, ir.setRecordFields(r, &.{.{ .name = "nested", .ty = u, .span = span }}));
    try std.testing.expectError(error.InvalidArrayElement, ir.setRecordFields(u, &.{.{ .name = "nested", .ty = array, .span = span }}));
    try std.testing.expectError(error.InvalidArrayElement, ir.setRecordFields(u, &.{.{ .name = "nested", .ty = u, .span = span }}));
    const sig = try ir.internType(.{ .function = .{ .params = &.{other}, .result = u } });
    const f = try ir.addFunction("f", sig, span);
    const body = [_]Instruction{
        .{ .ty = other, .span = span, .op = .{ .parameter = 0 } },
        .{ .ty = u, .span = span, .op = .{ .union_init = .{ .index = 0, .payload = @fromBackingInt(@intCast(0)) } } },
    };
    try std.testing.expectError(error.OperandTypeMismatch, ir.setFunctionBody(f, &body, @fromBackingInt(@intCast(1))));
    ir.clearFunctions();
    try ir.verify();
    const fields = @constCast(ir.types.items[@backingInt(r)].record.fields);
    fields[0].ty = u;
    try std.testing.expectError(error.InvalidArrayElement, ir.verify());
    fields[0].ty = text;
    ir.types.items[@backingInt(r)].record.tagged = true;
    try std.testing.expectError(error.InvalidArrayElement, ir.verify());
    ir.types.items[@backingInt(r)].record.tagged = false;
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
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .union_payload = .{ .base = @fromBackingInt(@intCast(0)), .index = 0 } } },
    };
    body[1].op.union_payload.index = 2;
    try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(f, &body, @fromBackingInt(@intCast(1))));
    body[1].op = .{ .field_read = .{ .base = @fromBackingInt(@intCast(0)), .index = 0 } };
    try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(f, &body, @fromBackingInt(@intCast(1))));
    body[1].ty = u;
    body[1].op = .{ .union_init = .{ .index = 0, .payload = null } };
    try std.testing.expectError(error.OperandTypeMismatch, ir.setFunctionBody(f, &body, @fromBackingInt(@intCast(0))));
    body[1].op = .{ .record_init = &.{ @fromBackingInt(@intCast(0)), @fromBackingInt(@intCast(0)) } };
    try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(f, &body, @fromBackingInt(@intCast(0))));
    body[1].ty = unit;
    body[1].op = .{ .union_tag = .{ .base = @fromBackingInt(@intCast(0)), .index = 0 } };
    try std.testing.expectError(error.OperandTypeMismatch, ir.setFunctionBody(f, &body, @fromBackingInt(@intCast(0))));
    body[1].ty = boolean;
    body[1].op = .{ .union_payload = .{ .base = @fromBackingInt(@intCast(0)), .index = 0 } };
    const private_body = [_]Instruction{
        body[0],
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .union_tag = .{ .base = @fromBackingInt(@intCast(0)), .index = 0 } } },
        body[1],
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .branch = .{ .condition = @fromBackingInt(@intCast(1)), .then_start = 2, .else_start = 3, .then_value = @fromBackingInt(@intCast(2)), .else_value = @fromBackingInt(@intCast(1)) } } },
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .unary = .{ .op = .bool_not, .operand = @fromBackingInt(@intCast(2)) } } },
    };
    try std.testing.expectError(error.InvalidValueId, ir.setFunctionBody(f, &private_body, @fromBackingInt(@intCast(4))));
    try ir.setFunctionBody(f, &body, @fromBackingInt(@intCast(1)));
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
    _ = try ir.internType(.{ .array = string });
    const f = try ir.addFunction("f", signature, .{ .start = 0, .end = 0 });
    var instructions = [_]Instruction{
        .{ .ty = string, .span = .{ .start = 0, .end = 0 }, .op = .{ .constant = literal } },
        .{ .ty = string, .span = .{ .start = 0, .end = 0 }, .op = .{ .binary = .{ .op = .add, .lhs = @fromBackingInt(@intCast(0)), .rhs = @fromBackingInt(@intCast(0)) } } },
        .{ .ty = integer, .span = .{ .start = 0, .end = 0 }, .op = .{ .array_len = @fromBackingInt(@intCast(1)) } },
    };
    instructions[1].op.binary.op = .sub;
    try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(f, &instructions, @fromBackingInt(@intCast(1))));
    instructions[1].op.binary.op = .lt;
    instructions[1].ty = boolean;
    try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(f, &instructions, @fromBackingInt(@intCast(0))));
    instructions[1].op.binary.op = .add;
    instructions[1].ty = string;
    instructions[2].ty = boolean;
    try std.testing.expectError(error.OperandTypeMismatch, ir.setFunctionBody(f, &instructions, @fromBackingInt(@intCast(1))));
    instructions[2].ty = integer;
    try ir.setFunctionBody(f, &instructions, @fromBackingInt(@intCast(1)));
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
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .binary = .{ .op = .eq, .lhs = @fromBackingInt(@intCast(0)), .rhs = @fromBackingInt(@intCast(0)) } } },
    };
    for ([_]BinaryOp{ .eq, .ne }) |op| {
        instructions[2].op.binary.op = op;
        try ir.validateBody(ir.getFunction(f).?, .{ .instructions = &instructions, .result = @fromBackingInt(@intCast(2)) });
        instructions[2].ty = string;
        try std.testing.expectError(error.OperandTypeMismatch, ir.setFunctionBody(f, &instructions, @fromBackingInt(@intCast(2))));
        instructions[2].ty = boolean;
        instructions[2].op.binary.rhs = @fromBackingInt(@intCast(1));
        try std.testing.expectError(error.OperandTypeMismatch, ir.setFunctionBody(f, &instructions, @fromBackingInt(@intCast(2))));
        instructions[2].op.binary.rhs = @fromBackingInt(@intCast(0));
    }
    for ([_]BinaryOp{ .lt, .le, .gt, .ge }) |op| {
        instructions[2].op.binary.op = op;
        try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(f, &instructions, @fromBackingInt(@intCast(2))));
    }
    instructions[2].op.binary.op = .ne;
    try ir.setFunctionBody(f, &instructions, @fromBackingInt(@intCast(2)));
    try ir.verify();
}

test "string scalar indexing verifies exact types and prior visible operands without source" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const string = try ir.internType(.string);
    const integer = try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = native_pointer_bits } });
    const rune = try ir.internType(.rune);
    const byte = try ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = 8 } });
    const wide = try ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = 16 } });
    const uint = try ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = native_pointer_bits } });
    const narrow = try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 8 } });
    const boolean = try ir.internType(.boolean);
    const params = [_]TypeId{ string, integer, wide, boolean, byte, uint, narrow };
    const signature = try ir.internType(.{ .function = .{ .params = &params, .result = rune } });
    const function = try ir.addFunction("byte", signature, .{ .start = 0, .end = 0 });
    const make = TestFunctions.instruction;
    var instructions: [8]Instruction = undefined;
    for (params, 0..) |ty, i| instructions[i] = make(ty, .{ .parameter = @intCast(i) });
    instructions[7] = make(rune, .{ .array_index = .{ .array = @fromBackingInt(0), .index = @fromBackingInt(1) } });
    // No source text or nonempty source span is needed to verify the operation.
    for (&instructions) |*instruction| instruction.span = .{ .start = 0, .end = 0 };
    try ir.setFunctionBody(function, &instructions, @fromBackingInt(7));
    try ir.verify();
    const owned = @constCast(ir.functions.items[0].body.?.instructions);
    const saved = owned[7];
    for ([_]TypeId{ string, integer, byte, wide, boolean, uint, narrow }) |ty| {
        owned[7].ty = ty;
        try std.testing.expectError(error.OperandTypeMismatch, ir.verify());
    }
    owned[7] = saved;
    for ([_]u32{ 0, 2, 3, 4, 5, 6 }) |index| {
        owned[7].op.array_index.index = @fromBackingInt(index);
        try std.testing.expectError(error.OperandTypeMismatch, ir.verify());
    }
    owned[7] = saved;
    for ([_]u32{ 7, 99 }) |index| {
        owned[7].op.array_index.index = @fromBackingInt(index);
        try std.testing.expectError(error.InvalidValueId, ir.verify());
        owned[7] = saved;
        owned[7].op.array_index.array = @fromBackingInt(index);
        try std.testing.expectError(error.InvalidValueId, ir.verify());
        owned[7] = saved;
    }
    owned[7].op.array_index.array = @fromBackingInt(3);
    try std.testing.expectError(error.InvalidOperation, ir.verify());
    owned[7] = saved;
    try ir.verify();
}

test "string slicing IR verifies optional native bounds types and prior operands without source" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const string = try ir.internType(.string);
    const integer = try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = native_pointer_bits } });
    const uint = try ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = native_pointer_bits } });
    const narrow = try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 8 } });
    const boolean = try ir.internType(.boolean);
    const rune = try ir.internType(.rune);
    const params = [_]TypeId{ string, integer, uint, narrow, boolean, rune };
    const signature = try ir.internType(.{ .function = .{ .params = &params, .result = string } });
    const function = try ir.addFunction("slice", signature, .{ .start = 0, .end = 0 });
    var instructions: [7]Instruction = undefined;
    for (params, 0..) |ty, i| instructions[i] = .{ .ty = ty, .span = .{ .start = 0, .end = 0 }, .op = .{ .parameter = @intCast(i) } };
    instructions[6] = .{ .ty = string, .span = .{ .start = 0, .end = 0 }, .op = .{ .string_slice = .{ .base = @fromBackingInt(0), .start = @fromBackingInt(1), .end = @fromBackingInt(1) } } };
    try ir.setFunctionBody(function, &instructions, @fromBackingInt(6));
    try ir.verify();
    const owned = @constCast(ir.functions.items[0].body.?.instructions);
    const saved = owned[6];
    for ([_]?ValueId{ null, @fromBackingInt(1) }) |start| {
        for ([_]?ValueId{ null, @fromBackingInt(1) }) |end| {
            owned[6].op.string_slice.start = start;
            owned[6].op.string_slice.end = end;
            try ir.verify();
        }
    }
    for ([_]bool{ false, true }) |end| {
        for ([_]u32{ 0, 2, 3, 4, 5, 6, 99 }) |bad| {
            owned[6] = saved;
            if (end) owned[6].op.string_slice.end = @fromBackingInt(bad) else owned[6].op.string_slice.start = @fromBackingInt(bad);
            try std.testing.expectError(if (bad >= 6) error.InvalidValueId else error.OperandTypeMismatch, ir.verify());
        }
    }
    for (params[1..]) |ty| {
        owned[6] = saved;
        owned[6].ty = ty;
        try std.testing.expectError(error.OperandTypeMismatch, ir.verify());
    }
    owned[6] = saved;
    owned[6].op.string_slice.base = @fromBackingInt(4);
    try std.testing.expectError(error.InvalidOperation, ir.verify());
    owned[6] = saved;
    owned[6].op.string_slice.base = @fromBackingInt(6);
    try std.testing.expectError(error.InvalidValueId, ir.verify());
    owned[6] = saved;
    try ir.verify();
}

test "storable arrays source free exact membership and rejected aggregate equality" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const span: Span = .{ .start = 0, .end = 0 };
    const unit = try ir.internType(.void);
    const boolean = try ir.internType(.boolean);
    const integer = try ir.internType(.{ .integer = .{ .bits = 32, .signedness = .signed } });
    const enumeration = try ir.internType(.{ .enumeration = .{ .name = "E", .identity = 0, .tags = &.{.{ .name = "a", .span = span }} } });
    const other_enum = try ir.internType(.{ .enumeration = .{ .name = "F", .identity = 1, .tags = &.{.{ .name = "a", .span = span }} } });
    const record = try ir.internType(.{ .record = .{ .identity = 0, .fields = &.{} } });
    const tagged = try ir.internType(.{ .record = .{ .identity = 1, .tagged = true, .fields = &.{} } });
    try ir.setRecordFields(tagged, &.{.{ .name = "none", .ty = unit, .span = span }});
    const function_type = try ir.internType(.{ .function = .{ .params = &.{integer}, .result = integer } });
    const range = try ir.internType(.{ .range = integer });
    const nested = try ir.internType(.{ .array = integer });
    for ([_]TypeId{ enumeration, record, tagged, function_type, range, nested }) |element| {
        const array = try ir.internType(.{ .array = element });
        const signature = try ir.internType(.{ .function = .{ .params = &.{ element, array }, .result = boolean } });
        const f = try ir.addFunction("contains", signature, span);
        var instructions = [_]Instruction{
            .{ .ty = element, .span = span, .op = .{ .parameter = 0 } },
            .{ .ty = array, .span = span, .op = .{ .parameter = 1 } },
            .{ .ty = boolean, .span = span, .op = .{ .array_contains = .{ .value = @fromBackingInt(0), .array = @fromBackingInt(1) } } },
        };
        if (element == enumeration) {
            try ir.setFunctionBody(f, &instructions, @fromBackingInt(2));
            try ir.verify();
            ir.clearFunctions();
            const mismatched = try ir.internType(.{ .function = .{ .params = &.{ other_enum, array }, .result = boolean } });
            const wrong = try ir.addFunction("mismatch", mismatched, span);
            instructions[0].ty = other_enum;
            try std.testing.expectError(error.OperandTypeMismatch, ir.setFunctionBody(wrong, &instructions, @fromBackingInt(2)));
        } else {
            try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(f, &instructions, @fromBackingInt(2)));
            instructions[2].op = .{ .binary = .{ .op = .eq, .lhs = @fromBackingInt(0), .rhs = @fromBackingInt(0) } };
            try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(f, &instructions, @fromBackingInt(2)));
        }
        ir.clearFunctions();
        try std.testing.expectEqual(element == nested, try ir.needsArrayCopy(element));
    }
}

test "storable arrays bound recursive copy depth including every nested array edge" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const record = try ir.internType(.{ .record = .{ .identity = 0, .fields = &.{} } });
    const array = try ir.internType(.{ .array = record });
    const nested = try ir.internType(.{ .array = array });
    try ir.setRecordFields(record, &.{.{ .name = "cycle", .ty = nested, .span = .{ .start = 0, .end = 0 } }});
    try std.testing.expectError(error.CyclicRecordLayout, ir.verify());
    try std.testing.expect(try ir.needsArrayCopy(record));
    try ir.setRecordFields(record, &.{});
    var child = record;
    for (0..max_record_depth - 1) |_| child = try ir.internType(.{ .array = child });
    try ir.verify();
    _ = try ir.internType(.{ .array = child });
    try std.testing.expectError(error.RecordNestingLimit, ir.verify());
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
    _ = try ir.internType(.{ .array = array });
    const unit = try ir.internType(.void);
    try std.testing.expectError(error.InvalidArrayElement, ir.internType(.{ .array = unit }));
    try std.testing.expectError(error.InvalidTypeId, ir.internType(.{ .array = @fromBackingInt(@intCast(999)) }));
    const wide = try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 128 } });
    _ = try ir.internType(.{ .array = wide });
    const signature = try ir.internType(.{ .function = .{ .params = &.{}, .result = array } });
    _ = try ir.internType(.{ .array = signature });
    const number = try ir.internConstant(.{ .ty = integer, .value = .{ .signed_integer = 0 } });
    const function = try ir.addFunction("arrays", signature, .{ .start = 0, .end = 0 });
    const make = TestFunctions.instruction;
    var elements = [_]ValueId{@fromBackingInt(@intCast(0))};
    const instructions = [_]Instruction{
        make(integer, .{ .constant = number }),
        make(array, .{ .array_literal = &elements }),
        make(array, .{ .array_clone = @fromBackingInt(@intCast(1)) }),
        make(integer, .{ .array_index = .{ .array = @fromBackingInt(@intCast(2)), .index = @fromBackingInt(@intCast(0)) } }),
        make(array, .{ .array_set = .{ .array = @fromBackingInt(@intCast(2)), .index = @fromBackingInt(@intCast(0)), .value = @fromBackingInt(@intCast(3)) } }),
        make(array, .{ .array_append = .{ .lhs = @fromBackingInt(@intCast(2)), .rhs = @fromBackingInt(@intCast(4)) } }),
        make(uint, .{ .array_len = @fromBackingInt(@intCast(5)) }),
    };
    try ir.setFunctionBody(function, &instructions, @fromBackingInt(@intCast(5)));
    elements[0] = @fromBackingInt(@intCast(999));
    try ir.verify();
    const owned = @constCast(ir.functions.items[0].body.?.instructions);
    const Case = struct { index: usize, replacement: Instruction, err: VerifyError };
    for ([_]Case{
        .{ .index = 0, .replacement = make(boolean, .{ .constant = number }), .err = error.OperandTypeMismatch },
        .{ .index = 2, .replacement = make(array, .{ .array_clone = @fromBackingInt(@intCast(0)) }), .err = error.OperandTypeMismatch },
        .{ .index = 2, .replacement = make(array, .{ .array_clone = @fromBackingInt(@intCast(99)) }), .err = error.InvalidValueId },
        .{ .index = 3, .replacement = make(integer, .{ .array_index = .{ .array = @fromBackingInt(@intCast(0)), .index = @fromBackingInt(@intCast(0)) } }), .err = error.InvalidOperation },
        .{ .index = 3, .replacement = make(boolean, instructions[3].op), .err = error.OperandTypeMismatch },
        .{ .index = 4, .replacement = make(array, .{ .array_set = .{ .array = @fromBackingInt(@intCast(2)), .index = @fromBackingInt(@intCast(2)), .value = @fromBackingInt(@intCast(3)) } }), .err = error.OperandTypeMismatch },
        .{ .index = 5, .replacement = make(array, .{ .array_append = .{ .lhs = @fromBackingInt(@intCast(2)), .rhs = @fromBackingInt(@intCast(0)) } }), .err = error.OperandTypeMismatch },
        .{ .index = 6, .replacement = make(integer, instructions[6].op), .err = error.OperandTypeMismatch },
    }) |case| {
        const saved = owned[case.index];
        owned[case.index] = case.replacement;
        defer owned[case.index] = saved;
        try std.testing.expectError(case.err, ir.verify());
    }
}

fn testTypeFormattingOwnership(allocator: std.mem.Allocator) !void {
    var ir = Ir.init(allocator);
    defer ir.deinit();
    const span: Span = .{ .start = 0, .end = 0 };
    const record = try ir.internType(.{ .record = .{ .identity = 0, .fields = &.{} } });
    const string = try ir.internType(.string);
    const signature = try ir.internType(.{ .function = .{ .params = &.{record}, .result = string } });
    const formatter = try ir.addFunction("format", signature, span);
    const literal = try ir.internConstant(.{ .ty = string, .value = .{ .string = "custom" } });
    try ir.setFunctionBody(formatter, &.{.{ .ty = string, .span = span, .op = .{ .constant = literal } }}, @fromBackingInt(0));
    var name = [_]u8{ 'E', 'n', 't', 'r', 'y' };
    try ir.setTypeFormatting(record, &name, null);
    name[0] = 'X';
    try std.testing.expectEqualStrings("Entry", ir.getTypeFormatting(record).?.display_name);
    ir.setTypeFormatting(record, "Renamed", formatter) catch |err| {
        try std.testing.expectEqualStrings("Entry", ir.getTypeFormatting(record).?.display_name);
        try std.testing.expect(ir.getTypeFormatting(record).?.formatter == null);
        return err;
    };
    try std.testing.expectEqualStrings("Renamed", ir.getTypeFormatting(record).?.display_name);
    try std.testing.expectEqual(formatter, ir.getTypeFormatting(record).?.formatter.?);
    try std.testing.expectEqual(@as(usize, 1), ir.formatting.items.len);
    try ir.verify();
    try std.testing.expectError(error.InvalidTypeFormatting, ir.setTypeFormatting(string, "string", null));
    try std.testing.expectError(error.InvalidTypeFormatting, ir.setTypeFormatting(record, "", null));
    try std.testing.expectError(error.InvalidFunctionId, ir.setTypeFormatting(record, "Entry", @fromBackingInt(99)));
    ir.functions.items[@backingInt(formatter)].is_closure = true;
    try std.testing.expectError(error.InvalidTypeFormatting, ir.verify());
    ir.functions.items[@backingInt(formatter)].is_closure = false;
    ir.formatting.items[0].formatter = @fromBackingInt(99);
    try std.testing.expectError(error.InvalidFunctionId, ir.verify());
    ir.formatting.items[0].formatter = formatter;
    ir.clearFunctions();
    try std.testing.expect(ir.getTypeFormatting(record).?.formatter == null);
    try ir.verify();
}

test "type formatting metadata owns names validates hooks and replaces atomically under OOM" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testTypeFormattingOwnership, .{});
}

test "default printing memoizes shared type DAGs for empty arrays and respects hook changes" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const span: Span = .{ .start = 0, .end = 0 };
    const unit = try ir.internType(.void);
    const string = try ir.internType(.string);
    const integer = try ir.internType(.{ .integer = .{ .bits = 64, .signedness = .signed } });
    var previous = integer;
    var leaf: TypeId = undefined;
    for (0..64) |i| {
        const record = try ir.internType(.{ .record = .{ .identity = @intCast(i), .fields = &.{} } });
        try ir.setRecordFields(record, &.{
            .{ .name = "a", .ty = previous, .span = span },
            .{ .name = "b", .ty = previous, .span = span },
        });
        if (i == 0) leaf = record;
        previous = record;
    }
    const array = try ir.internType(.{ .array = previous });
    const signature = try ir.internType(.{ .function = .{ .params = &.{}, .result = unit } });
    const function = try ir.addFunction("print_empty", signature, span);
    try ir.setFunctionBody(function, &.{
        .{ .ty = array, .span = span, .op = .{ .array_literal = &.{} } },
        .{ .ty = unit, .span = span, .op = .{ .print = .{ .args = &.{ @fromBackingInt(0), @fromBackingInt(0) }, .newline = true } } },
        .{ .ty = string, .span = span, .op = .{ .to_string = @fromBackingInt(0) } },
        .{ .ty = unit, .span = span, .op = .{ .print = .{ .args = &.{@fromBackingInt(2)}, .newline = true } } },
    }, @fromBackingInt(3));
    try ir.verify();

    const callback_array = try ir.internType(.{ .array = signature });
    try ir.setRecordFields(leaf, &.{.{ .name = "callbacks", .ty = callback_array, .span = span }});
    try std.testing.expectError(error.InvalidOperation, ir.verify());
    const hook_signature = try ir.internType(.{ .function = .{ .params = &.{previous}, .result = string } });
    const hook = try ir.addFunction("hook", hook_signature, span);
    const text = try ir.internConstant(.{ .ty = string, .value = .{ .string = "hook" } });
    try ir.setFunctionBody(hook, &.{.{ .ty = string, .span = span, .op = .{ .constant = text } }}, @fromBackingInt(0));
    try ir.setTypeFormatting(previous, "Root", hook);
    try ir.verify();
    try ir.setTypeFormatting(leaf, "Leaf", null);
    ir.formatting.items[1].formatter = @fromBackingInt(99);
    // Even metadata hidden beneath an overriding hook must be verified.
    try std.testing.expectError(error.InvalidFunctionId, ir.verify());
    ir.formatting.items[1].formatter = null;
    try ir.verify();
    try ir.setTypeFormatting(previous, "Root", null);
    try std.testing.expectError(error.InvalidOperation, ir.verify());
}

test "default printing rejects nested functions ranges and void" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const span: Span = .{ .start = 0, .end = 0 };
    const unit = try ir.internType(.void);
    const string = try ir.internType(.string);
    const integer = try ir.internType(.{ .integer = .{ .bits = 64, .signedness = .signed } });
    const function = try ir.internType(.{ .function = .{ .params = &.{}, .result = unit } });
    const range = try ir.internType(.{ .range = integer });
    const functions = try ir.internType(.{ .array = function });
    const ranges = try ir.internType(.{ .array = range });
    const nested_functions = try ir.internType(.{ .array = functions });
    const nested_ranges = try ir.internType(.{ .array = ranges });
    for ([_]TypeId{ function, range, functions, ranges, nested_functions, nested_ranges }) |id| {
        ir.clearFunctions();
        const signature = try ir.internType(.{ .function = .{ .params = &.{id}, .result = string } });
        const f = try ir.addFunction("reject", signature, span);
        const parameter: Instruction = .{ .ty = id, .span = span, .op = .{ .parameter = 0 } };
        try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(f, &.{ parameter, .{ .ty = string, .span = span, .op = .{ .to_string = @fromBackingInt(0) } } }, @fromBackingInt(1)));
        try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(f, &.{ parameter, .{ .ty = unit, .span = span, .op = .{ .print = .{ .args = &.{@fromBackingInt(0)}, .newline = true } } }, .{ .ty = string, .span = span, .op = .{ .constant = try ir.internConstant(.{ .ty = string, .value = .{ .string = "" } }) } } }, @fromBackingInt(2)));
    }
    var printability: DefaultPrintability = .{};
    defer printability.deinit(ir.allocator);
    try std.testing.expect(!try printability.check(&ir, unit));
}

test "to_string verifies printable prior operands result types spans and slot isolation" {
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
        make(integer, .{ .local_slot = @fromBackingInt(@intCast(0)) }),
        make(string, .{ .to_string = @fromBackingInt(@intCast(0)) }),
        make(string, .{ .to_string = @fromBackingInt(@intCast(1)) }),
        make(string, .{ .to_string = @fromBackingInt(@intCast(2)) }),
    };
    try ir.setFunctionBody(function, &body, @fromBackingInt(@intCast(9)));
    try ir.verify();
    const owned = @constCast(ir.functions.items[0].body.?.instructions);
    owned[9].op.to_string = @fromBackingInt(3);
    try ir.verify();
    for ([_]struct { value: u32, err: VerifyError }{
        .{ .value = 4, .err = error.InvalidOperation },
        .{ .value = 5, .err = error.InvalidOperation },
        .{ .value = 6, .err = error.InvalidValueId },
        .{ .value = 9, .err = error.InvalidValueId },
        .{ .value = 99, .err = error.InvalidValueId },
    }) |case| {
        owned[9].op.to_string = @fromBackingInt(@intCast(case.value));
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
        make(integer, .{ .local_slot = @fromBackingInt(@intCast(0)) }),
        make(boolean, .{ .array_contains = .{ .value = @fromBackingInt(@intCast(0)), .array = @fromBackingInt(@intCast(1)) } }),
    };
    try ir.setFunctionBody(function, &instructions, @fromBackingInt(@intCast(4)));
    const owned = @constCast(ir.functions.items[0].body.?.instructions);
    const Case = struct { instruction: Instruction, err: VerifyError };
    for ([_]Case{
        .{ .instruction = make(integer, instructions[4].op), .err = error.InvalidOperation },
        .{ .instruction = make(boolean, .{ .array_contains = .{ .value = @fromBackingInt(@intCast(2)), .array = @fromBackingInt(@intCast(1)) } }), .err = error.OperandTypeMismatch },
        .{ .instruction = make(boolean, .{ .array_contains = .{ .value = @fromBackingInt(@intCast(1)), .array = @fromBackingInt(@intCast(1)) } }), .err = error.OperandTypeMismatch },
        .{ .instruction = make(boolean, .{ .array_contains = .{ .value = @fromBackingInt(@intCast(0)), .array = @fromBackingInt(@intCast(0)) } }), .err = error.InvalidOperation },
        .{ .instruction = make(boolean, .{ .array_contains = .{ .value = @fromBackingInt(@intCast(4)), .array = @fromBackingInt(@intCast(1)) } }), .err = error.InvalidValueId },
        .{ .instruction = make(boolean, .{ .array_contains = .{ .value = @fromBackingInt(@intCast(0)), .array = @fromBackingInt(@intCast(99)) } }), .err = error.InvalidValueId },
        .{ .instruction = make(boolean, .{ .array_contains = .{ .value = @fromBackingInt(@intCast(3)), .array = @fromBackingInt(@intCast(1)) } }), .err = error.InvalidValueId },
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
        make(array, .{ .branch = .{ .condition = @fromBackingInt(@intCast(0)), .then_start = 1, .else_start = 2, .then_value = @fromBackingInt(@intCast(1)), .else_value = @fromBackingInt(@intCast(2)) } }),
        make(uint, .{ .array_len = @fromBackingInt(@intCast(1)) }),
    };
    try std.testing.expectError(error.InvalidValueId, ir.setFunctionBody(function, &body, @fromBackingInt(@intCast(4))));
    var valid = body;
    valid[4].op.array_len = @fromBackingInt(@intCast(3));
    try ir.setFunctionBody(function, &valid, @fromBackingInt(@intCast(4)));
    try ir.verify();
    // Membership must not bypass the same prior-reference/private-region checks.
    valid[4] = make(boolean, .{ .array_contains = .{ .value = @fromBackingInt(@intCast(0)), .array = @fromBackingInt(@intCast(1)) } });
    const bool_signature = try ir.internType(.{ .function = .{ .params = &.{}, .result = boolean } });
    const bool_function = try ir.addFunction("private_contains", bool_signature, .{ .start = 0, .end = 0 });
    try std.testing.expectError(error.InvalidValueId, ir.setFunctionBody(bool_function, &valid, @fromBackingInt(@intCast(4))));
    valid[4].op.array_contains.array = @fromBackingInt(@intCast(3));
    try ir.setFunctionBody(bool_function, &valid, @fromBackingInt(@intCast(4)));
    try ir.verify();
}

fn testPrintAllocationFailures(allocator: std.mem.Allocator) !void {
    var ir = Ir.init(allocator);
    defer ir.deinit();
    const void_ty = try ir.internType(.void);
    const boolean = try ir.internType(.boolean);
    const signature = try ir.internType(.{ .function = .{ .params = &.{boolean}, .result = void_ty } });
    const function = try ir.addFunction("print", signature, .{ .start = 0, .end = 0 });
    var args = [_]ValueId{@fromBackingInt(@intCast(0))};
    const instructions = [_]Instruction{
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .parameter = 0 } },
        .{ .ty = void_ty, .span = .{ .start = 0, .end = 0 }, .op = .{ .print = .{ .args = &args, .newline = false } } },
        .{ .ty = void_ty, .span = .{ .start = 0, .end = 0 }, .op = .{ .print = .{ .args = &args, .newline = true } } },
    };
    ir.setFunctionBody(function, &instructions, @fromBackingInt(@intCast(2))) catch |err| {
        try std.testing.expect(ir.functions.items[0].body == null);
        return err;
    };
    args[0] = @fromBackingInt(@intCast(999));
    try ir.verify();
    const body = ir.functions.items[0].body.?;
    try std.testing.expectEqual(@as(u32, 0), @backingInt(body.instructions[1].op.print.args[0]));
    try std.testing.expectEqual(@as(u32, 0), @backingInt(body.instructions[2].op.print.args[0]));
}

test "local slots require prior visible identities exact types and explicit loads" {
    var t = try TestFunctions.init(std.testing.allocator);
    defer t.ir.deinit();
    const unit = try t.ir.internType(.void);
    const make = TestFunctions.instruction;
    const base = [_]Instruction{
        make(t.integer, .{ .constant = t.number }),
        make(t.integer, .{ .local_slot = @fromBackingInt(@intCast(0)) }),
        make(t.integer, .{ .local_load = @fromBackingInt(@intCast(1)) }),
        make(unit, .{ .local_store = .{ .slot = @fromBackingInt(@intCast(1)), .value = @fromBackingInt(@intCast(2)) } }),
        make(t.integer, .{ .local_load = @fromBackingInt(@intCast(1)) }),
    };
    const Case = struct { index: usize, instruction: Instruction, err: VerifyError };
    for ([_]Case{
        .{ .index = 1, .instruction = make(t.integer, .{ .local_slot = @fromBackingInt(@intCast(1)) }), .err = error.InvalidValueId },
        .{ .index = 1, .instruction = make(t.boolean, .{ .local_slot = @fromBackingInt(@intCast(0)) }), .err = error.OperandTypeMismatch },
        .{ .index = 1, .instruction = make(unit, .{ .local_slot = @fromBackingInt(@intCast(0)) }), .err = error.InvalidOperation },
        .{ .index = 2, .instruction = make(t.integer, .{ .local_load = @fromBackingInt(@intCast(0)) }), .err = error.InvalidOperation },
        .{ .index = 2, .instruction = make(t.integer, .{ .local_load = @fromBackingInt(@intCast(99)) }), .err = error.InvalidValueId },
        .{ .index = 2, .instruction = make(t.boolean, .{ .local_load = @fromBackingInt(@intCast(1)) }), .err = error.OperandTypeMismatch },
        .{ .index = 2, .instruction = make(t.integer, .{ .unary = .{ .op = .negate, .operand = @fromBackingInt(@intCast(1)) } }), .err = error.InvalidValueId },
        .{ .index = 3, .instruction = make(unit, .{ .local_store = .{ .slot = @fromBackingInt(@intCast(2)), .value = @fromBackingInt(@intCast(0)) } }), .err = error.InvalidOperation },
        .{ .index = 3, .instruction = make(unit, .{ .local_store = .{ .slot = @fromBackingInt(@intCast(1)), .value = @fromBackingInt(@intCast(1)) } }), .err = error.InvalidValueId },
        .{ .index = 3, .instruction = make(t.integer, .{ .local_store = .{ .slot = @fromBackingInt(@intCast(1)), .value = @fromBackingInt(@intCast(2)) } }), .err = error.InvalidOperation },
    }) |case| {
        var bad = base;
        bad[case.index] = case.instruction;
        try t.reject(case.err, &bad, @fromBackingInt(@intCast(4)));
    }
    try t.reject(error.InvalidValueId, base[0..2], @fromBackingInt(@intCast(1)));
    try t.ir.setFunctionBody(t.function, &base, @fromBackingInt(@intCast(4)));
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
        make(t.integer, .{ .local_slot = @fromBackingInt(@intCast(1)) }),
        make(t.integer, .{ .local_load = @fromBackingInt(@intCast(2)) }),
        make(t.integer, .{ .constant = t.number }),
        make(t.integer, .{ .branch = .{ .condition = @fromBackingInt(@intCast(0)), .then_start = 1, .else_start = 4, .then_value = @fromBackingInt(@intCast(3)), .else_value = @fromBackingInt(@intCast(4)) } }),
        make(t.integer, .{ .local_load = @fromBackingInt(@intCast(2)) }),
    };
    try t.reject(error.InvalidValueId, &base, @fromBackingInt(@intCast(6)));
    var bad = base;
    bad[4] = make(t.integer, .{ .local_load = @fromBackingInt(@intCast(2)) });
    try t.reject(error.InvalidValueId, &bad, @fromBackingInt(@intCast(5)));
    bad = base;
    bad[6] = make(unit, .{ .local_store = .{ .slot = @fromBackingInt(@intCast(2)), .value = @fromBackingInt(@intCast(5)) } });
    try t.reject(error.InvalidValueId, &bad, @fromBackingInt(@intCast(5)));
    bad = base;
    bad[5].op.branch.then_value = @fromBackingInt(@intCast(2));
    try t.reject(error.InvalidValueId, bad[0..6], @fromBackingInt(@intCast(5)));
}

test "print operands are owned and body publication is atomic under OOM" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testPrintAllocationFailures, .{});
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
        .{ .ty = void_ty, .span = .{ .start = 0, .end = 0 }, .op = .{ .print = .{ .args = &.{@fromBackingInt(@intCast(0))}, .newline = true } } },
    };
    instructions[1].ty = boolean;
    try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(function, &instructions, @fromBackingInt(@intCast(1))));
    instructions[1].ty = void_ty;
    for ([_]u32{ 1, 99 }) |index| {
        instructions[1].op.print.args = &.{@fromBackingInt(@intCast(index))};
        try std.testing.expectError(error.InvalidValueId, ir.setFunctionBody(function, &instructions, @fromBackingInt(@intCast(1))));
    }
    instructions[1].op.print.args = &.{@fromBackingInt(@intCast(0))};
    instructions[0] = .{ .ty = void_ty, .span = .{ .start = 0, .end = 0 }, .op = .{ .constant = unit } };
    try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(function, &instructions, @fromBackingInt(@intCast(1))));
    instructions[0] = .{ .ty = signature, .span = .{ .start = 0, .end = 0 }, .op = .{ .function_ref = function } };
    try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(function, &instructions, @fromBackingInt(@intCast(1))));
    instructions[1].op.print.args = &.{};
    try ir.setFunctionBody(function, &instructions, @fromBackingInt(@intCast(1)));
    try ir.verify();
    const owned = @constCast(ir.functions.items[0].body.?.instructions);
    owned[1].ty = boolean;
    try std.testing.expectError(error.InvalidOperation, ir.verify());
}

test "float pools preserve IEEE identities and reject signaling NaNs and noncanonical bits" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    for ([_]u16{ 0, 16, 80, 128 }) |bits| try std.testing.expectError(error.InvalidFloatWidth, ir.internType(.{ .float = bits }));
    for ([_]u16{ 32, 64 }) |width| {
        const ty = try ir.internType(.{ .float = width });
        try std.testing.expectEqual(ty, try ir.internType(.{ .float = width }));
        const patterns: []const u64 = if (width == 32)
            &.{ 0, 0x80000000, 0x7f800000, 0xff800000, 0x7fc00001, 0x7fc00002, 0xffc00001 }
        else
            &.{ 0, 0x8000000000000000, 0x7ff0000000000000, 0xfff0000000000000, 0x7ff8000000000001, 0x7ff8000000000002, 0xfff8000000000001 };
        var ids: [7]ConstId = undefined;
        for (patterns, 0..) |bits, i| {
            const constant: Constant = .{ .ty = ty, .value = .{ .float_bits = bits } };
            ids[i] = try ir.internConstant(constant);
            try std.testing.expectEqual(bits, ir.getConstant(ids[i]).?.value.float_bits);
            try std.testing.expectEqual(ids[i], try ir.internConstant(constant));
            for (ids[0..i]) |previous| try std.testing.expect(previous != ids[i]);
        }
        const signaling: []const u64 = if (width == 32)
            &.{ 0x7f800001, 0xff800001, 0x7fbfffff, 0xffbfffff }
        else
            &.{ 0x7ff0000000000001, 0xfff0000000000001, 0x7ff7ffffffffffff, 0xfff7ffffffffffff };
        for (signaling) |pattern| {
            const count = ir.constants.items.len;
            try std.testing.expectError(error.FloatOutOfRange, ir.internConstant(.{ .ty = ty, .value = .{ .float_bits = pattern } }));
            try std.testing.expectEqual(count, ir.constants.items.len);
            const index = @backingInt(ids[0]);
            const saved = ir.constants.items[index];
            ir.constants.items[index].value = .{ .float_bits = pattern };
            try std.testing.expectError(error.FloatOutOfRange, ir.verify());
            ir.constants.items[index] = saved;
        }
        try std.testing.expect(isArrayElement(ir.getType(ty).?));
        try std.testing.expect(isRecordField(ir.getType(ty).?));
        try std.testing.expect(isUnionPayload(ir.getType(ty).?));
        try std.testing.expect(hasEquality(ir.getType(ty).?));
        try std.testing.expect(!isManaged(ir.getType(ty).?));
        try std.testing.expect(!try ir.needsArrayCopy(ty));
        try std.testing.expectError(error.ConstantTypeMismatch, ir.internConstant(.{ .ty = ty, .value = .{ .signed_integer = 1 } }));
    }
    const narrow = try ir.internType(.{ .float = 32 });
    try std.testing.expectError(error.FloatOutOfRange, ir.internConstant(.{ .ty = narrow, .value = .{ .float_bits = 0x100000000 } }));
    try ir.verify();
}

test "float IR operations exact types storage printing membership and foreign ABI verify" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    ir.source_len = 1;
    const boolean = try ir.internType(.boolean);
    const unit = try ir.internType(.void);
    const string = try ir.internType(.string);
    for ([_]u16{ 32, 64 }) |width| {
        const ty = try ir.internType(.{ .float = width });
        for (std.enums.values(BinaryOp)) |op| {
            ir.clearFunctions();
            const arithmetic = op == .add or op == .sub or op == .mul or op == .div;
            const integer_only = op == .mod or op == .bit_and or op == .bit_or or op == .bit_xor;
            const result = if (arithmetic or integer_only) ty else boolean;
            const signature = try ir.internType(.{ .function = .{ .params = &.{ty}, .result = result } });
            const function = try ir.addFunction("float_binary", signature, test_span);
            const instructions = [_]Instruction{
                TestFunctions.instruction(ty, .{ .parameter = 0 }),
                TestFunctions.instruction(result, .{ .binary = .{ .op = op, .lhs = @fromBackingInt(0), .rhs = @fromBackingInt(0) } }),
            };
            if (integer_only) {
                try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(function, &instructions, @fromBackingInt(1)));
            } else {
                try ir.setFunctionBody(function, &instructions, @fromBackingInt(1));
                try ir.verify();
            }
        }
        ir.clearFunctions();
        const array = try ir.internType(.{ .array = ty });
        const record = try ir.internType(.{ .record = .{ .identity = width, .fields = &.{} } });
        try ir.setRecordFields(record, &.{.{ .name = "value", .ty = ty, .span = test_span }});
        const tagged = try ir.internType(.{ .record = .{ .identity = width + 1, .tagged = true, .fields = &.{} } });
        try ir.setRecordFields(tagged, &.{.{ .name = "some", .ty = ty, .span = test_span }});
        const signature = try ir.internType(.{ .function = .{ .params = &.{ty}, .result = string } });
        const function = try ir.addFunction("float_storage", signature, test_span);
        try ir.setFunctionBody(function, &.{
            TestFunctions.instruction(ty, .{ .parameter = 0 }),
            TestFunctions.instruction(ty, .{ .unary = .{ .op = .negate, .operand = @fromBackingInt(0) } }),
            TestFunctions.instruction(ty, .{ .local_slot = @fromBackingInt(1) }),
            TestFunctions.instruction(ty, .{ .local_load = @fromBackingInt(2) }),
            TestFunctions.instruction(array, .{ .array_literal = &.{@fromBackingInt(3)} }),
            TestFunctions.instruction(boolean, .{ .array_contains = .{ .value = @fromBackingInt(0), .array = @fromBackingInt(4) } }),
            TestFunctions.instruction(record, .{ .record_init = &.{@fromBackingInt(0)} }),
            TestFunctions.instruction(tagged, .{ .union_init = .{ .index = 0, .payload = @fromBackingInt(0) } }),
            TestFunctions.instruction(unit, .{ .print = .{ .args = &.{ @fromBackingInt(0), @fromBackingInt(4), @fromBackingInt(6), @fromBackingInt(7) }, .newline = true } }),
            TestFunctions.instruction(string, .{ .to_string = @fromBackingInt(0) }),
        }, @fromBackingInt(9));
        const foreign_signature = try ir.internType(.{ .function = .{ .params = &.{ty}, .result = ty } });
        const foreign = try ir.addFunction("float_foreign", foreign_signature, test_span);
        try ir.setExternName(foreign, "float_foreign_symbol");
        try ir.verify();
        ir.clearFunctions();
        const exported = try ir.addFunction("float_export", foreign_signature, test_span);
        try ir.setExportName(exported, "float_export_symbol");
        try ir.setFunctionBody(exported, &.{TestFunctions.instruction(ty, .{ .parameter = 0 })}, @fromBackingInt(0));
        try ir.verify();
        try std.testing.expectError(error.InvalidTypeId, ir.internType(.{ .range = ty }));
    }
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
    try std.testing.expect(ir.getType(@fromBackingInt(@intCast(999))) == null);
    try std.testing.expect(ir.getConstant(@fromBackingInt(@intCast(999))) == null);
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
    const missing: TypeId = @fromBackingInt(@intCast(99));
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
    ir.constants.items[0].ty = @fromBackingInt(@intCast(99));
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
    const index = @backingInt(function);
    ir.types.items[index].function.result = function;
    try std.testing.expectError(error.InvalidTypeId, ir.verify());
    ir.types.items[index].function.result = boolean;
    const params = @constCast(ir.types.items[index].function.params);
    params[0] = @fromBackingInt(@intCast(999));
    try std.testing.expectError(error.InvalidTypeId, ir.verify());
    params[0] = boolean;
    ir.types.items[@backingInt(boolean)] = .{ .integer = .{ .signedness = .signed, .bits = 0 } };
    try std.testing.expectError(error.InvalidIntegerWidth, ir.verify());
    ir.types.items[@backingInt(boolean)] = .boolean;
    const truth = try ir.internConstant(.{ .ty = boolean, .value = .{ .boolean = true } });
    ir.constants.items[@backingInt(truth)].value = .void;
    try std.testing.expectError(error.ConstantTypeMismatch, ir.verify());
    ir.constants.items[@backingInt(truth)].value = .{ .boolean = true };
    const integer = try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 8 } });
    const number = try ir.internConstant(.{ .ty = integer, .value = .{ .signed_integer = 127 } });
    ir.constants.items[@backingInt(number)].value = .{ .signed_integer = 128 };
    try std.testing.expectError(error.IntegerOutOfRange, ir.verify());
    ir.constants.items[@backingInt(number)].value = .{ .signed_integer = 127 };
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
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{});
}

test "rune pools preserve scalar identity reject invalid constants and survive OOM" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testRunePools, .{});
}

fn testRunePools(allocator: std.mem.Allocator) !void {
    var ir = Ir.init(allocator);
    defer ir.deinit();
    const rune = try ir.internType(.rune);
    const integer = try ir.internType(.{ .integer = .{ .bits = 32, .signedness = .unsigned } });
    try std.testing.expectEqual(rune, try ir.internType(.rune));
    try std.testing.expect(rune != integer);
    try std.testing.expect(!isManaged(.rune));
    try std.testing.expect(isArrayElement(.rune) and isRecordField(.rune) and isUnionPayload(.rune));
    _ = try ir.internType(.{ .array = rune });
    try std.testing.expectError(error.InvalidTypeId, ir.internType(.{ .range = rune }));
    for ([_]u32{ 0, 0x7f, 0x80, 0x7ff, 0x800, 0xd7ff, 0xe000, 0xffff, 0x10000, 0x10ffff }) |value| {
        try std.testing.expect(isRuneValue(value));
        const id = try ir.internConstant(.{ .ty = rune, .value = .{ .rune = value } });
        try std.testing.expectEqual(id, try ir.internConstant(.{ .ty = rune, .value = .{ .rune = value } }));
    }
    for ([_]u32{ 0xd800, 0xdbff, 0xdfff, 0x110000, 0xffffffff }) |value| {
        try std.testing.expect(!isRuneValue(value));
        try std.testing.expectError(error.ConstantTypeMismatch, ir.internConstant(.{ .ty = rune, .value = .{ .rune = value } }));
    }
    try std.testing.expectError(error.ConstantTypeMismatch, ir.internConstant(.{ .ty = rune, .value = .{ .unsigned_integer = 65 } }));
    try std.testing.expectError(error.ConstantTypeMismatch, ir.internConstant(.{ .ty = integer, .value = .{ .rune = 65 } }));
    try ir.verify();
    ir.constants.items[0].value.rune = 0xd800;
    try std.testing.expectError(error.ConstantTypeMismatch, ir.verify());
}

test "rune operations verify exact types equality only and foreign exclusion without source" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const rune = try ir.internType(.rune);
    const boolean = try ir.internType(.boolean);
    const string = try ir.internType(.string);
    const integer = try ir.internType(.{ .integer = .{ .bits = 8, .signedness = .unsigned } });
    const signature = try ir.internType(.{ .function = .{ .params = &.{rune}, .result = boolean } });
    const span: Span = .{ .start = 0, .end = 0 };
    const function = try ir.addFunction("equal", signature, span);
    try std.testing.expectError(error.InvalidExportSignature, ir.setExportName(function, "equal"));
    try std.testing.expectError(error.InvalidExternSignature, ir.setExternName(function, "equal"));
    const ins = TestFunctions.instruction;
    var body = [_]Instruction{
        ins(rune, .{ .parameter = 0 }),
        ins(string, .{ .to_string = @fromBackingInt(0) }),
        ins(boolean, .{ .binary = .{ .op = .eq, .lhs = @fromBackingInt(0), .rhs = @fromBackingInt(0) } }),
    };
    for (&body) |*instruction| instruction.span = span;
    try ir.setFunctionBody(function, &body, @fromBackingInt(2));
    const owned = @constCast(ir.functions.items[0].body.?.instructions);
    for (std.enums.values(BinaryOp)) |op| {
        owned[2].op.binary.op = op;
        if (op == .eq or op == .ne) try ir.verify() else try std.testing.expectError(error.InvalidOperation, ir.verify());
    }
    owned[2] = body[2];
    owned[2].ty = rune;
    try std.testing.expectError(error.OperandTypeMismatch, ir.verify());
    owned[2] = body[2];
    owned[1].ty = rune;
    try std.testing.expectError(error.InvalidOperation, ir.verify());
    owned[1] = body[1];
    owned[0].ty = integer;
    try std.testing.expectError(error.OperandTypeMismatch, ir.verify());
    owned[0] = body[0];
    ir.functions.items[0].export_name = try ir.allocator.dupe(u8, "equal");
    try std.testing.expectError(error.InvalidExportSignature, ir.verify());
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
    var call_args = [_]ValueId{@fromBackingInt(@intCast(0))};
    var print_args = [_]ValueId{@fromBackingInt(@intCast(2))};
    var instructions = [_]Instruction{
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .parameter = 0 } },
        .{ .ty = signature, .span = .{ .start = 0, .end = 0 }, .op = .{ .function_ref = function } },
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .call = .{ .callee = @fromBackingInt(@intCast(1)), .args = &call_args } } },
        .{ .ty = void_ty, .span = .{ .start = 0, .end = 0 }, .op = .{ .print = .{ .args = &print_args, .newline = true } } },
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .short_circuit = .{ .kind = .@"or", .lhs = @fromBackingInt(@intCast(0)), .rhs_start = 2, .rhs = @fromBackingInt(@intCast(2)) } } },
        .{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .branch = .{ .condition = @fromBackingInt(@intCast(0)), .then_start = 1, .else_start = 5, .then_value = @fromBackingInt(@intCast(4)), .else_value = @fromBackingInt(@intCast(0)) } } },
    };
    ir.setFunctionBody(function, &instructions, @fromBackingInt(@intCast(5))) catch |err| {
        try std.testing.expect(ir.getFunction(function).?.body == null);
        return err;
    };
    call_args[0] = @fromBackingInt(@intCast(99));
    print_args[0] = @fromBackingInt(@intCast(99));
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
        ins(fixture.integer, .{ .local_slot = @fromBackingInt(@intCast(0)) }),
        ins(fixture.integer, .{ .local_load = @fromBackingInt(@intCast(1)) }),
        ins(fixture.integer, .{ .constant = fixture.number }),
        ins(fixture.boolean, .{ .binary = .{ .op = .lt, .lhs = @fromBackingInt(@intCast(2)), .rhs = @fromBackingInt(@intCast(3)) } }),
        ins(fixture.integer, .{ .local_slot = @fromBackingInt(@intCast(0)) }),
        ins(fixture.integer, .{ .local_load = @fromBackingInt(@intCast(5)) }),
        ins(unit, .{ .local_store = .{ .slot = @fromBackingInt(@intCast(1)), .value = @fromBackingInt(@intCast(6)) } }),
        ins(unit, .{ .loop = .{ .condition_start = 2, .body_start = 5, .condition = @fromBackingInt(@intCast(4)), .body = @fromBackingInt(@intCast(7)) } }),
        ins(fixture.integer, .{ .local_load = @fromBackingInt(@intCast(1)) }),
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
                bad[8].op.loop.condition = @fromBackingInt(@intCast(8));
                break :b error.InvalidRegion;
            },
            3 => b: {
                bad[8].op.loop.body = @fromBackingInt(@intCast(99));
                break :b error.InvalidRegion;
            },
            4 => b: {
                bad[8].op.loop.condition = @fromBackingInt(@intCast(2));
                break :b error.OperandTypeMismatch;
            },
            5 => b: {
                bad[8].op.loop.body = @fromBackingInt(@intCast(5));
                break :b error.InvalidValueId;
            },
            6 => b: {
                bad[9].op.local_load = @fromBackingInt(@intCast(5));
                break :b error.InvalidValueId;
            },
            7 => b: {
                bad[6].op.local_load = @fromBackingInt(@intCast(2));
                break :b error.InvalidValueId;
            },
            8 => b: {
                bad[3].op = .{ .local_load = @fromBackingInt(@intCast(5)) };
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
                bad[7] = ins(fixture.integer, .{ .local_load = @fromBackingInt(@intCast(1)) });
                break :b error.OperandTypeMismatch;
            },
            12 => b: {
                bad[8].ty = fixture.integer;
                break :b error.InvalidOperation;
            },
            13 => b: {
                bad[7] = ins(unit, .{ .short_circuit = .{ .kind = .@"and", .lhs = @fromBackingInt(@intCast(4)), .rhs_start = 3, .rhs = @fromBackingInt(@intCast(6)) } });
                break :b error.InvalidRegion;
            },
            14 => b: {
                bad[3] = ins(fixture.integer, .{ .local_slot = @fromBackingInt(@intCast(2)) });
                bad[4].op.binary.rhs = @fromBackingInt(@intCast(0));
                bad[6].op.local_load = @fromBackingInt(@intCast(3));
                break :b error.InvalidValueId;
            },
            else => unreachable,
        };
        errdefer std.debug.print("loop verifier case {d}\n", .{case});
        try fixture.reject(expected, &bad, @fromBackingInt(@intCast(9)));
    }
    var terminal = good;
    terminal[7] = ins(unit, .loop_continue);
    try fixture.ir.setFunctionBody(fixture.function, &terminal, @fromBackingInt(@intCast(9)));
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
            ins(t.integer, .{ .loop = .{ .condition_start = 0, .condition = @fromBackingInt(@intCast(0)), .body_start = 1, .body = @fromBackingInt(@intCast(2)), .else_start = 3, .else_value = @fromBackingInt(@intCast(1)) } }),
        };
        if (case == 0) {
            try LoopTests.reject(&t, error.InvalidValueId, &minimal, @fromBackingInt(@intCast(3)));
            continue;
        }
        const body_private = [_]Instruction{
            ins(t.integer, .{ .constant = seven }),
            ins(t.boolean, .{ .constant = falsity }),
            ins(t.integer, .{ .constant = seven }),
            ins(unit, .{ .constant = nothing }),
            ins(t.integer, .{ .loop = .{ .condition_start = 1, .condition = @fromBackingInt(@intCast(1)), .body_start = 2, .body = @fromBackingInt(@intCast(3)), .else_start = 4, .else_value = @fromBackingInt(@intCast(0)) } }),
        };
        const continuation_private = [_]Instruction{
            ins(t.integer, .{ .constant = seven }),
            ins(t.boolean, .{ .constant = falsity }),
            ins(unit, .{ .constant = nothing }),
            ins(t.integer, .{ .constant = seven }),
            ins(unit, .{ .constant = nothing }),
            ins(t.integer, .{ .loop = .{ .condition_start = 1, .condition = @fromBackingInt(@intCast(1)), .body_start = 2, .body = @fromBackingInt(@intCast(2)), .continuation_start = 3, .continuation = @fromBackingInt(@intCast(4)), .else_start = 5, .else_value = @fromBackingInt(@intCast(0)) } }),
        };
        const nested_body = [_]Instruction{
            ins(t.integer, .{ .constant = seven }),
            ins(t.boolean, .{ .constant = falsity }),
            ins(t.boolean, .{ .constant = falsity }),
            ins(t.integer, .{ .constant = seven }),
            ins(t.integer, .{ .constant = seven }),
            ins(t.integer, .{ .branch = .{ .condition = @fromBackingInt(@intCast(2)), .then_start = 3, .else_start = 4, .then_value = @fromBackingInt(@intCast(3)), .else_value = @fromBackingInt(@intCast(4)) } }),
            ins(unit, .{ .constant = nothing }),
            ins(t.integer, .{ .loop = .{ .condition_start = 1, .condition = @fromBackingInt(@intCast(1)), .body_start = 2, .body = @fromBackingInt(@intCast(6)), .else_start = 7, .else_value = @fromBackingInt(@intCast(0)) } }),
        };
        const nested_exhaustion = [_]Instruction{
            ins(t.integer, .{ .constant = seven }),
            ins(t.boolean, .{ .constant = falsity }),
            ins(unit, .{ .constant = nothing }),
            ins(t.boolean, .{ .constant = falsity }),
            ins(t.integer, .{ .constant = seven }),
            ins(t.integer, .{ .constant = seven }),
            ins(t.integer, .{ .branch = .{ .condition = @fromBackingInt(@intCast(3)), .then_start = 4, .else_start = 5, .then_value = @fromBackingInt(@intCast(4)), .else_value = @fromBackingInt(@intCast(5)) } }),
            ins(t.integer, .{ .loop = .{ .condition_start = 1, .condition = @fromBackingInt(@intCast(1)), .body_start = 2, .body = @fromBackingInt(@intCast(2)), .else_start = 3, .else_value = @fromBackingInt(@intCast(6)) } }),
        };
        const enclosing_branch = [_]Instruction{
            ins(t.boolean, .{ .constant = falsity }),
            ins(t.integer, .{ .constant = seven }), // visible in the enclosing then region
            ins(t.boolean, .{ .constant = falsity }),
            ins(t.integer, .{ .constant = seven }), // private to the inner loop body
            ins(unit, .{ .constant = nothing }),
            ins(t.integer, .{ .loop = .{ .condition_start = 2, .condition = @fromBackingInt(@intCast(2)), .body_start = 3, .body = @fromBackingInt(@intCast(4)), .else_start = 5, .else_value = @fromBackingInt(@intCast(1)) } }),
            ins(t.integer, .{ .constant = seven }),
            ins(t.integer, .{ .branch = .{ .condition = @fromBackingInt(@intCast(0)), .then_start = 1, .else_start = 6, .then_value = @fromBackingInt(@intCast(5)), .else_value = @fromBackingInt(@intCast(6)) } }),
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
            1 => &.{@fromBackingInt(@intCast(2))},
            2 => &.{@fromBackingInt(@intCast(3))},
            3 => &.{ @fromBackingInt(@intCast(3)), @fromBackingInt(@intCast(4)), @fromBackingInt(@intCast(5)) },
            4 => &.{ @fromBackingInt(@intCast(4)), @fromBackingInt(@intCast(5)) },
            5 => &.{@fromBackingInt(@intCast(3))},
            else => unreachable,
        };
        const result: ValueId = @fromBackingInt(@intCast(good.len - 1));
        for (private) |value| {
            const bad = try std.testing.allocator.dupe(Instruction, good);
            defer std.testing.allocator.free(bad);
            const loop_index = if (case == 5) 5 else good.len - 1;
            bad[loop_index].op.loop.else_value = value;
            errdefer std.debug.print("exhaustion visibility case {d}, private value {d}\n", .{ case, @backingInt(value) });
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
        ins(t.integer, .{ .local_slot = @fromBackingInt(@intCast(2)) }),
        ins(unit, .{ .constant = nothing }),
        ins(t.integer, .{ .parameter = 0 }), // 5 continuation
        ins(unit, .{ .constant = nothing }),
        ins(t.integer, .{ .parameter = 0 }), // 7 exhaustion
        ins(t.integer, .{ .local_slot = @fromBackingInt(@intCast(7)) }),
        ins(t.integer, .{ .local_load = @fromBackingInt(@intCast(8)) }),
        ins(t.integer, .{ .loop = .{ .condition_start = 1, .body_start = 2, .continuation_start = 5, .else_start = 7, .condition = @fromBackingInt(@intCast(1)), .body = @fromBackingInt(@intCast(4)), .continuation = @fromBackingInt(@intCast(6)), .else_value = @fromBackingInt(@intCast(9)) } }),
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
                r.else_value = @fromBackingInt(@intCast(10));
                break :b error.InvalidRegion;
            },
            6 => b: {
                r.else_value = @fromBackingInt(@intCast(8));
                break :b error.InvalidValueId;
            },
            7 => b: {
                r.else_value = @fromBackingInt(@intCast(2));
                break :b error.InvalidValueId;
            },
            8 => b: {
                r.else_value = @fromBackingInt(@intCast(5));
                break :b error.InvalidValueId;
            },
            9 => b: {
                bad[9].op.local_load = @fromBackingInt(@intCast(3));
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
                r.body = @fromBackingInt(@intCast(7));
                break :b error.InvalidRegion;
            },
            17 => b: {
                bad[9].span.end = t.ir.source_len + 1;
                break :b error.InvalidSourceSpan;
            },
            18 => b: {
                bad[7] = ins(unit, .{ .return_value = @fromBackingInt(@intCast(0)) });
                break :b error.UnreachableInstruction;
            },
            else => unreachable,
        };
        errdefer std.debug.print("exhaustion verifier case {d}\n", .{case});
        try LoopTests.reject(&t, expected, &bad, @fromBackingInt(@intCast(10)));
    }
    try LoopTests.reject(&t, error.InvalidValueId, &good, @fromBackingInt(@intCast(9)));
    var terminal = good;
    terminal[9] = ins(unit, .{ .return_value = @fromBackingInt(@intCast(0)) });
    terminal[10].ty = unit;
    try LoopTests.reject(&t, error.InvalidReturn, &terminal, @fromBackingInt(@intCast(10)));
    terminal[10].op.loop.returns = true;
    try t.ir.validateBody(t.ir.functions.items[0], .{ .instructions = &terminal, .result = @fromBackingInt(@intCast(10)) });
    terminal[10].ty = t.integer;
    try LoopTests.reject(&t, error.InvalidReturn, &terminal, @fromBackingInt(@intCast(10)));
    try t.ir.setFunctionBody(t.function, &good, @fromBackingInt(@intCast(10)));
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
        ins(t.integer, .{ .local_slot = @fromBackingInt(@intCast(0)) }),
        ins(t.boolean, .{ .constant = truth }), // 2: condition
        ins(t.integer, .{ .local_slot = @fromBackingInt(@intCast(0)) }),
        ins(t.integer, .{ .local_load = @fromBackingInt(@intCast(3)) }),
        ins(t.integer, .{ .parameter = 0 }), // 5: body
        ins(t.integer, .{ .local_slot = @fromBackingInt(@intCast(5)) }),
        ins(t.integer, .{ .local_load = @fromBackingInt(@intCast(6)) }),
        ins(unit, .{ .constant = nothing }),
        ins(t.integer, .{ .parameter = 0 }), // 9: continuation
        ins(t.integer, .{ .local_slot = @fromBackingInt(@intCast(9)) }),
        ins(t.integer, .{ .local_load = @fromBackingInt(@intCast(10)) }),
        ins(unit, .{ .constant = nothing }),
        ins(unit, .{ .loop = .{ .condition_start = 2, .body_start = 5, .continuation_start = 9, .condition = @fromBackingInt(@intCast(2)), .body = @fromBackingInt(@intCast(8)), .continuation = @fromBackingInt(@intCast(12)) } }),
        ins(t.integer, .{ .local_load = @fromBackingInt(@intCast(1)) }),
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
                r.continuation = @fromBackingInt(@intCast(8));
                break :b error.InvalidRegion;
            },
            7 => b: {
                r.continuation = @fromBackingInt(@intCast(13));
                break :b error.InvalidRegion;
            },
            8 => b: {
                r.continuation = @fromBackingInt(@intCast(std.math.maxInt(u32)));
                break :b error.InvalidRegion;
            },
            9 => b: {
                r.body = @fromBackingInt(@intCast(9));
                break :b error.InvalidRegion;
            },
            10 => b: {
                r.body = @fromBackingInt(@intCast(7));
                break :b error.OperandTypeMismatch;
            },
            11 => b: {
                r.continuation = @fromBackingInt(@intCast(11));
                break :b error.OperandTypeMismatch;
            },
            12 => b: {
                r.continuation = @fromBackingInt(@intCast(10));
                break :b error.InvalidValueId;
            },
            13 => b: {
                bad[11] = ins(unit, .loop_continue);
                break :b error.UnreachableInstruction;
            },
            14 => b: {
                bad[12] = ins(unit, .loop_continue);
                r.continuation = @fromBackingInt(@intCast(9));
                break :b error.UnreachableInstruction;
            },
            15 => b: {
                bad[8] = ins(unit, .loop_continue);
                r.body = @fromBackingInt(@intCast(5));
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
                bad[12] = ins(unit, .{ .loop_break = @fromBackingInt(@intCast(9)) });
                break :b error.InvalidOperation;
            },
            20 => b: {
                bad[13].ty = t.integer;
                break :b error.InvalidOperation;
            },
            21 => b: {
                bad[12] = ins(unit, .{ .loop_break = null });
                bad[13].ty = @fromBackingInt(@intCast(999));
                break :b error.InvalidTypeId;
            },
            // A nested region may not straddle the body/continuation boundary.
            22 => b: {
                bad[11] = ins(t.integer, .{ .branch = .{ .condition = @fromBackingInt(@intCast(2)), .then_start = 8, .else_start = 10, .then_value = @fromBackingInt(@intCast(8)), .else_value = @fromBackingInt(@intCast(10)) } });
                break :b error.InvalidRegion;
            },
            23 => b: {
                bad[12].span.end = 11;
                break :b error.InvalidSourceSpan;
            },
            else => unreachable,
        };
        errdefer std.debug.print("continuation verifier case {d}\n", .{case});
        try LoopTests.reject(&t, expected, &bad, @fromBackingInt(@intCast(14)));
    }
    // Each region's values and slot identities are private, even when an
    // instruction in a later sibling or after the loop references them.
    for ([_]usize{ 4, 7, 11 }) |source| {
        for ([_]usize{ 7, 11, 14 }) |destination| {
            if (source >= destination) continue;
            for (0..3) |kind| {
                var bad = good;
                bad[destination] = switch (kind) {
                    0 => ins(t.integer, .{ .unary = .{ .op = .negate, .operand = @fromBackingInt(@intCast(source)) } }),
                    1 => ins(t.integer, .{ .local_load = @fromBackingInt(@intCast(source - 1)) }),
                    2 => ins(unit, .{ .local_store = .{ .slot = @fromBackingInt(@intCast(source - 1)), .value = @fromBackingInt(@intCast(0)) } }),
                    else => unreachable,
                };
                try LoopTests.reject(&t, error.InvalidValueId, &bad, @fromBackingInt(@intCast(14)));
            }
        }
    }
    for ([_]usize{ 4, 8, 12 }) |tail| {
        for (0..3) |kind| {
            var valid = good;
            valid[tail] = ins(unit, switch (kind) {
                0 => .loop_continue,
                1 => .{ .loop_break = null },
                2 => .{ .return_value = @fromBackingInt(@intCast(0)) },
                else => unreachable,
            });
            if (tail == 4) valid[13].op.loop.condition = @fromBackingInt(@intCast(4));
            const terminal_condition = tail == 4 and kind != 1;
            if (terminal_condition) {
                valid[13].op.loop.returns = true;
                try LoopTests.reject(&t, error.UnreachableInstruction, &valid, @fromBackingInt(@intCast(14)));
            }
            try t.ir.setFunctionBody(t.function, valid[0..(if (terminal_condition) @as(usize, 14) else 15)], @fromBackingInt(@intCast(if (terminal_condition) @as(u32, 13) else 14)));
            try t.ir.verify();
            const body = t.ir.functions.items[0].body.?;
            t.ir.allocator.free(body.instructions);
            t.ir.functions.items[0].body = null;
        }
    }
    try t.ir.setFunctionBody(t.function, &good, @fromBackingInt(@intCast(14)));
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
        ins(t.integer, .{ .local_slot = @fromBackingInt(@intCast(0)) }),
        ins(t.boolean, .{ .constant = truth }),
        ins(t.integer, .{ .constant = t.number }),
        ins(unit, .{ .loop_break = @fromBackingInt(@intCast(3)) }),
        ins(unit, .loop_continue),
        ins(t.integer, .{ .loop = .{ .unconditional = true, .condition_start = 2, .body_start = 3, .continuation_start = 5, .condition = @fromBackingInt(@intCast(2)), .body = @fromBackingInt(@intCast(4)), .continuation = @fromBackingInt(@intCast(5)) } }),
    };
    for (0..17) |case| {
        var bad = good;
        const expected: VerifyError = switch (case) {
            0 => b: {
                bad[4].op.loop_break = null;
                break :b error.OperandTypeMismatch;
            },
            1 => b: {
                bad[4].op.loop_break = @fromBackingInt(@intCast(4));
                break :b error.InvalidValueId;
            },
            2 => b: {
                bad[4].op.loop_break = @fromBackingInt(@intCast(5));
                break :b error.InvalidValueId;
            },
            3 => b: {
                bad[4].op.loop_break = @fromBackingInt(@intCast(std.math.maxInt(u32)));
                break :b error.InvalidValueId;
            },
            4 => b: {
                bad[4].op.loop_break = @fromBackingInt(@intCast(1));
                break :b error.InvalidValueId;
            },
            5 => b: {
                bad[4].op.loop_break = @fromBackingInt(@intCast(2));
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
                bad[6].ty = @fromBackingInt(@intCast(999));
                break :b error.InvalidTypeId;
            },
            9 => b: {
                bad[4].op.loop_break = null;
                bad[6].ty = @fromBackingInt(@intCast(999));
                break :b error.InvalidTypeId;
            },
            10 => b: {
                bad[4].op = .loop_continue;
                break :b error.InvalidOperation;
            },
            11 => b: {
                bad[4].op = .{ .return_value = @fromBackingInt(@intCast(3)) };
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
                bad[5].op = .{ .loop_break = @fromBackingInt(@intCast(3)) };
                break :b error.InvalidValueId;
            },
            16 => b: {
                bad[6].op.loop.unconditional = false;
                break :b error.InvalidOperation;
            },
            else => unreachable,
        };
        errdefer std.debug.print("break value verifier case {d}\n", .{case});
        try LoopTests.reject(&t, expected, &bad, @fromBackingInt(@intCast(6)));
    }
    // A terminal instruction in the body is not a value in the continuation.
    var bad = good;
    bad[5].op = .{ .loop_break = @fromBackingInt(@intCast(4)) };
    try LoopTests.reject(&t, error.InvalidValueId, &bad, @fromBackingInt(@intCast(6)));
    // The continuation may provide the value instead of the body.
    var valid = good;
    valid[4].op = .loop_continue;
    valid[5].op = .{ .loop_break = @fromBackingInt(@intCast(0)) };
    try t.ir.setFunctionBody(t.function, &valid, @fromBackingInt(@intCast(6)));
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
        ins(unit, .{ .loop_break = @fromBackingInt(@intCast(4)) }),
        ins(t.integer, .{ .loop = .{ .unconditional = true, .condition_start = 2, .body_start = 3, .continuation_start = 4, .condition = @fromBackingInt(@intCast(2)), .body = @fromBackingInt(@intCast(3)), .continuation = @fromBackingInt(@intCast(5)) } }),
        ins(unit, .{ .loop_break = @fromBackingInt(@intCast(6)) }),
        ins(t.integer, .{ .loop = .{ .unconditional = true, .condition_start = 1, .body_start = 2, .condition = @fromBackingInt(@intCast(1)), .body = @fromBackingInt(@intCast(7)) } }),
    };
    var bad = good;
    bad[7].op = .loop_continue;
    try LoopTests.reject(&t, error.InvalidOperation, &bad, @fromBackingInt(@intCast(8)));
    bad = good;
    bad[5].op.loop_break = null;
    try LoopTests.reject(&t, error.OperandTypeMismatch, &bad, @fromBackingInt(@intCast(8)));
    bad = good;
    bad[7].op.loop_break = @fromBackingInt(@intCast(4));
    try LoopTests.reject(&t, error.InvalidValueId, &bad, @fromBackingInt(@intCast(8)));
    bad = good;
    bad[6].op.loop.body = @fromBackingInt(@intCast(5));
    try LoopTests.reject(&t, error.InvalidRegion, &bad, @fromBackingInt(@intCast(8)));
    const body: Body = .{ .instructions = &good, .result = @fromBackingInt(@intCast(8)) };
    try std.testing.expectEqual(@as(?usize, 6), enclosingLoop(body, 2));
    try std.testing.expectEqual(@as(?usize, 6), enclosingLoop(body, 3));
    try std.testing.expectEqual(@as(?usize, 6), enclosingLoop(body, 5));
    try std.testing.expectEqual(@as(?usize, 8), enclosingLoop(body, 6));
    try std.testing.expectEqual(@as(?usize, 8), enclosingLoop(body, 7));
    try std.testing.expectEqual(@as(?usize, null), enclosingLoop(body, 8));
    try t.ir.setFunctionBody(t.function, &good, @fromBackingInt(@intCast(8)));
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
            ins(unit, .{ .loop_break = @fromBackingInt(@intCast(0)) }),
            ins(ty, .{ .loop = .{ .unconditional = true, .condition_start = 1, .body_start = 2, .condition = @fromBackingInt(@intCast(1)), .body = @fromBackingInt(@intCast(2)) } }),
        };
        var bad = good;
        bad[2].op.loop_break = null;
        try std.testing.expectError(error.OperandTypeMismatch, ir.setFunctionBody(function, &bad, @fromBackingInt(@intCast(3))));
        try std.testing.expect(ir.getFunction(function).?.body == null);
        bad[2].op = .loop_continue;
        try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(function, &bad, @fromBackingInt(@intCast(3))));
        try std.testing.expect(ir.getFunction(function).?.body == null);
        try ir.setFunctionBody(function, &good, @fromBackingInt(@intCast(3)));
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
                ins(unit, .{ .loop_break = if (valued) @fromBackingInt(@intCast(1)) else null }),
                ins(unit, .{ .loop = .{ .unconditional = unconditional, .condition_start = 0, .body_start = 1, .condition = @fromBackingInt(@intCast(0)), .body = @fromBackingInt(@intCast(2)) } }),
            };
            if (valued and !unconditional) {
                try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(function, &instructions, @fromBackingInt(@intCast(3))));
                try std.testing.expect(ir.getFunction(function).?.body == null);
            } else {
                try ir.setFunctionBody(function, &instructions, @fromBackingInt(@intCast(3)));
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
    var args = [_]ValueId{@fromBackingInt(@intCast(0))};
    var instructions = [_]Instruction{
        ins(t.integer, .{ .parameter = 0 }),
        ins(t.signature, .{ .function_ref = t.function }),
        ins(t.boolean, .{ .constant = truth }),
        ins(array, .{ .array_literal = &args }),
        ins(unit, .{ .print = .{ .args = &args, .newline = false } }),
        ins(t.integer, .{ .call = .{ .callee = @fromBackingInt(@intCast(1)), .args = &args } }),
        ins(array, .{ .array_literal = &args }),
        ins(unit, .{ .print = .{ .args = &args, .newline = true } }),
        ins(unit, .loop_continue),
        ins(t.integer, .{ .call = .{ .callee = @fromBackingInt(@intCast(1)), .args = &args } }),
        ins(array, .{ .array_literal = &args }),
        ins(unit, .{ .print = .{ .args = &args, .newline = true } }),
        ins(unit, .{ .loop_break = @fromBackingInt(@intCast(9)) }),
        ins(t.integer, .{ .loop = .{ .unconditional = true, .condition_start = 2, .body_start = 5, .continuation_start = 9, .condition = @fromBackingInt(@intCast(2)), .body = @fromBackingInt(@intCast(8)), .continuation = @fromBackingInt(@intCast(12)) } }),
    };
    t.ir.setFunctionBody(t.function, &instructions, @fromBackingInt(@intCast(13))) catch |err| {
        try std.testing.expect(t.ir.getFunction(t.function).?.body == null);
        try std.testing.expectError(error.MissingFunctionBody, t.ir.verify());
        return err;
    };
    args[0] = @fromBackingInt(@intCast(999));
    instructions[12].op.loop_break = null;
    instructions[13].op.loop.continuation_start = null;
    try t.ir.verify();
    const body = t.ir.getFunction(t.function).?.body.?;
    try std.testing.expectEqual(@as(?u32, 9), body.instructions[13].op.loop.continuation_start);
    try std.testing.expectEqual(@as(?ValueId, @fromBackingInt(@intCast(9))), body.instructions[12].op.loop_break);
    for ([_]usize{ 3, 6, 10 }) |i| try std.testing.expectEqual(@as(ValueId, @fromBackingInt(@intCast(0))), body.instructions[i].op.array_literal[0]);
    for ([_]usize{ 4, 7, 11 }) |i| try std.testing.expectEqual(@as(ValueId, @fromBackingInt(@intCast(0))), body.instructions[i].op.print.args[0]);
    for ([_]usize{ 5, 9 }) |i| try std.testing.expectEqual(@as(ValueId, @fromBackingInt(@intCast(0))), body.instructions[i].op.call.args[0]);
    try std.testing.expectError(error.BodyAlreadySet, t.ir.setFunctionBody(t.function, &instructions, @fromBackingInt(@intCast(13))));
    try t.ir.verify();
    t.ir.clearFunctions();
    try t.ir.verify();
}

test "loop continuation and break body publication is atomic under every OOM" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testLoopAllocationFailures, .{});
}

test "conditional body operands are owned and publication is atomic under OOM" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testConditionalAllocationFailures, .{});
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
        ins(unit, .{ .panic = @fromBackingInt(@intCast(0)) }),
        ins(unit, .{ .constant = empty }),
        ins(unit, .{ .loop = .{ .condition_start = 0, .body_start = 2, .condition = @fromBackingInt(@intCast(1)), .body = @fromBackingInt(@intCast(2)), .returns = true } }),
    };
    try ir.validateBody(ir.functions.items[0], .{ .instructions = &good, .result = @fromBackingInt(@intCast(3)) });
    var bad = good;
    bad[3].op.loop.returns = false;
    try std.testing.expectError(error.InvalidReturn, ir.validateBody(ir.functions.items[0], .{ .instructions = &bad, .result = @fromBackingInt(@intCast(3)) }));
    for ([_]Operation{ .loop_continue, .{ .return_value = null }, .{ .call = .{ .callee = @fromBackingInt(@intCast(0)), .args = &.{}, .noreturn = true } } }) |op| {
        var terminal = good;
        terminal[1].op = op;
        if (op == .call) {
            ir.functions.items[0].noreturn = true;
            terminal[0] = ins(signature, .{ .function_ref = function });
        }
        try ir.validateBody(ir.functions.items[0], .{ .instructions = &terminal, .result = @fromBackingInt(@intCast(3)) });
        ir.functions.items[0].noreturn = false;
    }
    bad = good;
    bad[1].op = .{ .loop_break = null };
    try std.testing.expectError(error.InvalidReturn, ir.validateBody(ir.functions.items[0], .{ .instructions = &bad, .result = @fromBackingInt(@intCast(3)) }));
    bad[3].op.loop.returns = false;
    try ir.validateBody(ir.functions.items[0], .{ .instructions = &bad, .result = @fromBackingInt(@intCast(3)) });
    bad = good;
    bad[2].op = .{ .loop_break = null };
    try ir.validateBody(ir.functions.items[0], .{ .instructions = &bad, .result = @fromBackingInt(@intCast(3)) });
    const branch = [_]Instruction{
        ins(boolean, .{ .constant = truth }),
        ins(string, .{ .constant = text }),
        ins(unit, .{ .panic = @fromBackingInt(@intCast(1)) }),
        ins(string, .{ .constant = text }),
        ins(unit, .{ .panic = @fromBackingInt(@intCast(3)) }),
        ins(unit, .{ .branch = .{ .condition = @fromBackingInt(@intCast(0)), .then_start = 1, .else_start = 3, .then_value = @fromBackingInt(@intCast(2)), .else_value = @fromBackingInt(@intCast(4)), .returns = true } }),
        ins(unit, .{ .constant = empty }),
        ins(unit, .{ .loop = .{ .condition_start = 0, .body_start = 6, .condition = @fromBackingInt(@intCast(5)), .body = @fromBackingInt(@intCast(6)), .returns = true } }),
    };
    try ir.validateBody(ir.functions.items[0], .{ .instructions = &branch, .result = @fromBackingInt(@intCast(7)) });
    var mixed = branch;
    mixed[4].op = .{ .loop_break = null };
    try std.testing.expectError(error.InvalidReturn, ir.validateBody(ir.functions.items[0], .{ .instructions = &mixed, .result = @fromBackingInt(@intCast(7)) }));
    mixed[7].op.loop.returns = false;
    try ir.validateBody(ir.functions.items[0], .{ .instructions = &mixed, .result = @fromBackingInt(@intCast(7)) });
    const nested = [_]Instruction{
        ins(unit, .{ .constant = empty }),
        ins(unit, .{ .loop_break = null }),
        ins(unit, .{ .constant = empty }),
        ins(unit, .{ .loop = .{ .condition_start = 0, .body_start = 2, .condition = @fromBackingInt(@intCast(1)), .body = @fromBackingInt(@intCast(2)) } }),
        ins(string, .{ .constant = text }),
        ins(unit, .{ .panic = @fromBackingInt(@intCast(4)) }),
        ins(unit, .{ .constant = empty }),
        ins(unit, .{ .loop = .{ .condition_start = 0, .body_start = 6, .condition = @fromBackingInt(@intCast(5)), .body = @fromBackingInt(@intCast(6)), .returns = true } }),
    };
    try ir.validateBody(ir.functions.items[0], .{ .instructions = &nested, .result = @fromBackingInt(@intCast(7)) });
    const nested_terminal = good ++ [_]Instruction{
        ins(unit, .{ .constant = empty }),
        ins(unit, .{ .loop = .{ .condition_start = 0, .body_start = 4, .condition = @fromBackingInt(@intCast(3)), .body = @fromBackingInt(@intCast(4)), .returns = true } }),
    };
    try ir.validateBody(ir.functions.items[0], .{ .instructions = &nested_terminal, .result = @fromBackingInt(@intCast(5)) });
    const after = good ++ [_]Instruction{ins(unit, .{ .constant = empty })};
    try std.testing.expectError(error.UnreachableInstruction, ir.validateBody(ir.functions.items[0], .{ .instructions = &after, .result = @fromBackingInt(@intCast(4)) }));
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
        ins(unit, .{ .call = .{ .callee = @fromBackingInt(@intCast(0)), .args = &.{}, .noreturn = true } }),
    };
    try ir.setFunctionBody(function, &good, @fromBackingInt(@intCast(1)));
    try ir.verify();
    ir.functions.items[0].noreturn = false;
    try std.testing.expectError(error.InvalidCall, ir.verify());
    ir.functions.items[0].noreturn = true;
    var bad = good;
    bad[1].op.call.noreturn = false;
    try std.testing.expectError(error.InvalidReturn, ir.validateBody(ir.functions.items[0], .{ .instructions = &bad, .result = @fromBackingInt(@intCast(1)) }));
    bad = good;
    bad[1].op = .{ .return_value = null };
    try std.testing.expectError(error.InvalidReturn, ir.validateBody(ir.functions.items[0], .{ .instructions = &bad, .result = @fromBackingInt(@intCast(1)) }));
    const value = try ir.internConstant(.{ .ty = unit, .value = .void });
    const after = good ++ [_]Instruction{ins(unit, .{ .constant = value })};
    try std.testing.expectError(error.UnreachableInstruction, ir.validateBody(ir.functions.items[0], .{ .instructions = &after, .result = @fromBackingInt(@intCast(2)) }));
    const indirect_signature = try ir.internType(.{ .function = .{ .params = &.{signature}, .result = unit } });
    ir.functions.items[0].ty = indirect_signature;
    bad = good;
    bad[0].op = .{ .parameter = 0 };
    try std.testing.expectError(error.InvalidCall, ir.validateBody(ir.functions.items[0], .{ .instructions = &bad, .result = @fromBackingInt(@intCast(1)) }));
    ir.functions.items[0].ty = try ir.internType(.{ .function = .{ .params = &.{}, .result = try ir.internType(.boolean) } });
    try std.testing.expectError(error.InvalidReturn, ir.verify());
}

test "assert borrow CFG rejects loop backedge conflicts and preserves branch exclusivity" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAssertBorrowRegions, .{});
}

fn testAssertBorrowRegions(allocator: std.mem.Allocator) !void {
    var ir = Ir.init(allocator);
    defer ir.deinit();
    ir.source_len = 100;
    const unit = try ir.internType(.void);
    const boolean = try ir.internType(.boolean);
    const signature = try ir.internType(.{ .function = .{ .params = &.{boolean}, .result = unit } });
    const function = try ir.addFunction("f", signature, .{ .start = 0, .end = 1 });
    const yes = try ir.internConstant(.{ .ty = boolean, .value = .{ .boolean = true } });
    const ins = TestFunctions.instruction;
    const prefix = [_]Instruction{
        ins(boolean, .{ .constant = yes }),
        ins(boolean, .{ .local_slot = @fromBackingInt(0) }),
        ins(unit, .{ .borrow_reserve = @fromBackingInt(1) }),
        ins(unit, .{ .borrow_start = .{ .reservation = @fromBackingInt(2), .index = null } }),
        ins(boolean, .{ .parameter = 0 }),
    };
    const assertion = ins(unit, .{ .assertion = .{ .condition = @fromBackingInt(0), .message = null, .location = "test.nov:1:1" } });
    var loop = prefix ++ [_]Instruction{
        ins(boolean, .{ .borrow_read = @fromBackingInt(3) }),
        ins(unit, .{ .local_store = .{ .slot = @fromBackingInt(1), .value = @fromBackingInt(0) } }),
        assertion,
        ins(unit, .{ .loop = .{ .unconditional = true, .condition_start = 4, .body_start = 5, .condition = @fromBackingInt(4), .body = @fromBackingInt(7), .returns = true } }),
    };
    loop[4] = ins(boolean, .{ .constant = yes });
    const loop_body: Body = .{ .instructions = &loop, .result = @fromBackingInt(8) };
    try ir.verifyBodyStructure(function, loop_body);
    const conflict = (try ir.borrowConflict(loop_body)).?;
    try std.testing.expectEqual(loop[6].span, conflict.access);
    try std.testing.expectEqual(loop[2].span, conflict.origin);
    if (ir.setFunctionBody(function, &loop, loop_body.result)) |_| {
        return error.TestExpectedError;
    } else |err| {
        if (err == error.OutOfMemory) return err;
        try std.testing.expectEqual(error.BorrowConflict, err);
    }
    try std.testing.expect(ir.getFunction(function).?.body == null);
    const branch = prefix ++ [_]Instruction{
        ins(unit, .{ .local_store = .{ .slot = @fromBackingInt(1), .value = @fromBackingInt(0) } }),
        assertion,
        ins(boolean, .{ .borrow_read = @fromBackingInt(3) }),
        ins(unit, .{ .print = .{ .args = &.{@fromBackingInt(7)}, .newline = false } }),
        ins(unit, .{ .branch = .{ .condition = @fromBackingInt(4), .then_start = 5, .else_start = 7, .then_value = @fromBackingInt(6), .else_value = @fromBackingInt(8) } }),
    };
    const branch_body: Body = .{ .instructions = &branch, .result = @fromBackingInt(9) };
    try ir.verifyBodyStructure(function, branch_body);
    try std.testing.expect(try ir.borrowConflict(branch_body) == null);
    try ir.setFunctionBody(function, &branch, branch_body.result);
    try ir.verify();
}

test "assertion validates visible operands types location and owned publication rollback" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAssertionOwnership, .{});
}

fn testAssertionOwnership(allocator: std.mem.Allocator) !void {
    var ir = Ir.init(allocator);
    defer ir.deinit();
    ir.source_len = 100;
    const unit = try ir.internType(.void);
    const boolean = try ir.internType(.boolean);
    const string = try ir.internType(.string);
    const signature = try ir.internType(.{ .function = .{ .params = &.{}, .result = unit } });
    const function = try ir.addFunction("check", signature, .{ .start = 0, .end = 0 });
    const yes = try ir.internConstant(.{ .ty = boolean, .value = .{ .boolean = true } });
    const text = try ir.internConstant(.{ .ty = string, .value = .{ .string = "message" } });
    const ins = TestFunctions.instruction;
    var location = [_]u8{ 'a', ':', '1', ':', '2' };
    const good = [_]Instruction{
        ins(boolean, .{ .constant = yes }),
        ins(string, .{ .constant = text }),
        ins(unit, .{ .assertion = .{ .condition = @fromBackingInt(0), .message = @fromBackingInt(1), .location = &location } }),
    };
    var bad = good;
    bad[2].op.assertion.condition = @fromBackingInt(2);
    try std.testing.expectError(error.InvalidValueId, ir.validateBody(ir.getFunction(function).?, .{ .instructions = &bad, .result = @fromBackingInt(2) }));
    bad = good;
    bad[2].op.assertion.message = @fromBackingInt(0);
    try std.testing.expectError(error.OperandTypeMismatch, ir.validateBody(ir.getFunction(function).?, .{ .instructions = &bad, .result = @fromBackingInt(2) }));
    bad = good;
    bad[2].ty = boolean;
    try std.testing.expectError(error.InvalidOperation, ir.validateBody(ir.getFunction(function).?, .{ .instructions = &bad, .result = @fromBackingInt(2) }));
    bad = good;
    bad[2].op.assertion.location = "";
    try std.testing.expectError(error.InvalidOperation, ir.validateBody(ir.getFunction(function).?, .{ .instructions = &bad, .result = @fromBackingInt(2) }));
    const private = good ++ [_]Instruction{
        ins(unit, .{ .branch = .{ .condition = @fromBackingInt(0), .then_start = 1, .else_start = 3, .then_value = @fromBackingInt(2), .else_value = null } }),
        ins(unit, .{ .assertion = .{ .condition = @fromBackingInt(0), .message = @fromBackingInt(1), .location = &location } }),
    };
    try std.testing.expectError(error.InvalidValueId, ir.validateBody(ir.getFunction(function).?, .{ .instructions = &private, .result = @fromBackingInt(4) }));
    try ir.setFunctionBody(function, &good, @fromBackingInt(2));
    location[0] = 'b';
    try std.testing.expectEqualStrings("a:1:2", ir.getFunction(function).?.body.?.instructions[2].op.assertion.location);
    try ir.verify();
}

test "test functions reject ordinary IR references and invalid signatures" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    ir.source_len = 100;
    const unit = try ir.internType(.void);
    const signature = try ir.internType(.{ .function = .{ .params = &.{}, .result = unit } });
    const function = try ir.addFunction("test", signature, .{ .start = 0, .end = 0 });
    ir.functions.items[0].is_test = true;
    const ins = TestFunctions.instruction;
    try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(function, &.{ins(signature, .{ .function_ref = function })}, @fromBackingInt(0)));
    ir.functions.items[0].noreturn = true;
    try std.testing.expectError(error.InvalidFunctionType, ir.verify());
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
        ins(unit, .{ .panic = @fromBackingInt(@intCast(0)) }),
    };
    var bad = good;
    bad[1].op.panic = @fromBackingInt(@intCast(1));
    try fixture.reject(error.InvalidValueId, &bad, @fromBackingInt(@intCast(1)));
    bad[1].op.panic = @fromBackingInt(@intCast(999));
    try fixture.reject(error.InvalidValueId, &bad, @fromBackingInt(@intCast(1)));
    bad = good;
    bad[0] = ins(fixture.integer, .{ .parameter = 0 });
    try fixture.reject(error.OperandTypeMismatch, &bad, @fromBackingInt(@intCast(1)));
    bad = good;
    bad[1].ty = string;
    try fixture.reject(error.InvalidOperation, &bad, @fromBackingInt(@intCast(1)));
    const after = good ++ [_]Instruction{ins(fixture.integer, .{ .parameter = 0 })};
    try fixture.reject(error.UnreachableInstruction, &after, @fromBackingInt(@intCast(2)));
    const consume = good ++ [_]Instruction{ins(unit, .{ .panic = @fromBackingInt(@intCast(1)) })};
    try fixture.reject(error.UnreachableInstruction, &consume, @fromBackingInt(@intCast(2)));
    const boolean = try fixture.ir.internConstant(.{ .ty = fixture.boolean, .value = .{ .boolean = true } });
    const private = [_]Instruction{
        ins(fixture.boolean, .{ .constant = boolean }),
        ins(string, .{ .constant = text }),
        ins(unit, .{ .panic = @fromBackingInt(@intCast(1)) }),
        ins(unit, .{ .panic = @fromBackingInt(@intCast(1)) }),
        ins(unit, .{ .branch = .{ .condition = @fromBackingInt(@intCast(0)), .then_start = 1, .else_start = 3, .then_value = @fromBackingInt(@intCast(2)), .else_value = @fromBackingInt(@intCast(3)), .returns = true } }),
    };
    try fixture.reject(error.InvalidValueId, &private, @fromBackingInt(@intCast(4)));
    try fixture.ir.setFunctionBody(fixture.function, &good, @fromBackingInt(@intCast(1)));
    try fixture.ir.verify();
}

test "explicit returns validate signatures prior values spans and terminal tails" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const unit = try fixture.ir.internType(.void);
    const ins = TestFunctions.instruction;
    const good = [_]Instruction{
        ins(fixture.integer, .{ .parameter = 0 }),
        ins(unit, .{ .return_value = @fromBackingInt(@intCast(0)) }),
    };
    var bad = good;
    bad[1].op.return_value = null;
    try fixture.reject(error.InvalidReturn, &bad, @fromBackingInt(@intCast(1)));
    bad = good;
    bad[1].op.return_value = @fromBackingInt(@intCast(1));
    try fixture.reject(error.InvalidValueId, &bad, @fromBackingInt(@intCast(1)));
    bad = good;
    bad[1].ty = fixture.integer;
    try fixture.reject(error.InvalidReturn, &bad, @fromBackingInt(@intCast(1)));
    bad = good;
    bad[1].span.end = 11;
    try fixture.reject(error.InvalidSourceSpan, &bad, @fromBackingInt(@intCast(1)));
    try fixture.reject(error.UnreachableInstruction, &good, @fromBackingInt(@intCast(0)));
    const after = good ++ [_]Instruction{ins(fixture.integer, .{ .parameter = 0 })};
    try fixture.reject(error.UnreachableInstruction, &after, @fromBackingInt(@intCast(2)));
    const wrong = [_]Instruction{
        ins(unit, .{ .constant = try fixture.ir.internConstant(.{ .ty = unit, .value = .void }) }),
        ins(unit, .{ .return_value = @fromBackingInt(@intCast(0)) }),
    };
    try fixture.reject(error.InvalidReturn, &wrong, @fromBackingInt(@intCast(1)));
    try fixture.ir.setFunctionBody(fixture.function, &good, @fromBackingInt(@intCast(1)));
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
        ins(unit, .{ .return_value = @fromBackingInt(@intCast(0)) }),
    };
    try ir.setFunctionBody(f, &instructions, @fromBackingInt(@intCast(1)));
    try ir.verify();
    instructions[0] = ins(boolean, .{ .constant = try ir.internConstant(.{ .ty = boolean, .value = .{ .boolean = true } }) });
    const bad = try ir.addFunction("bad", sig, test_span);
    try std.testing.expectError(error.InvalidReturn, ir.setFunctionBody(bad, &instructions, @fromBackingInt(@intCast(1))));
    const bare = [_]Instruction{ins(unit, .{ .return_value = null })};
    try ir.setFunctionBody(bad, &bare, @fromBackingInt(@intCast(0)));
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
        ins(unit, .{ .return_value = @fromBackingInt(@intCast(1)) }),
        ins(fixture.boolean, .{ .short_circuit = .{ .kind = .@"and", .lhs = @fromBackingInt(@intCast(0)), .rhs_start = 1, .rhs = @fromBackingInt(@intCast(2)) } }),
        ins(fixture.integer, .{ .parameter = 0 }),
    };
    var bad = good;
    bad[4] = ins(unit, .{ .return_value = @fromBackingInt(@intCast(1)) });
    try fixture.reject(error.InvalidValueId, &bad, @fromBackingInt(@intCast(4)));
    bad = good;
    bad[2] = ins(unit, .{ .constant = try fixture.ir.internConstant(.{ .ty = unit, .value = .void }) });
    try fixture.reject(error.OperandTypeMismatch, &bad, @fromBackingInt(@intCast(4)));
    bad = good;
    bad[3].op.short_circuit.rhs = @fromBackingInt(@intCast(0));
    try fixture.reject(error.UnreachableInstruction, &bad, @fromBackingInt(@intCast(4)));
    try fixture.ir.setFunctionBody(fixture.function, &good, @fromBackingInt(@intCast(4)));
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
        ins(unit, .{ .return_value = @fromBackingInt(@intCast(1)) }),
        ins(fixture.integer, .{ .parameter = 0 }),
        ins(unit, .{ .return_value = @fromBackingInt(@intCast(3)) }),
        ins(unit, .{ .branch = .{ .condition = @fromBackingInt(@intCast(0)), .then_start = 1, .else_start = 3, .then_value = @fromBackingInt(@intCast(2)), .else_value = @fromBackingInt(@intCast(4)), .returns = true } }),
    };
    var bad = good;
    bad[5].op.branch.returns = false;
    try fixture.reject(error.InvalidReturn, &bad, @fromBackingInt(@intCast(5)));
    bad = good;
    bad[4].op.return_value = @fromBackingInt(@intCast(1));
    try fixture.reject(error.InvalidValueId, &bad, @fromBackingInt(@intCast(5)));
    bad = good;
    bad[5].ty = fixture.integer;
    try fixture.reject(error.InvalidReturn, &bad, @fromBackingInt(@intCast(5)));
    const after = good ++ [_]Instruction{ins(fixture.integer, .{ .parameter = 0 })};
    try fixture.reject(error.UnreachableInstruction, &after, @fromBackingInt(@intCast(6)));
    // A non-returning branch cannot claim its successor is unreachable.
    bad = good;
    bad[4] = ins(fixture.integer, .{ .parameter = 0 });
    bad[5].ty = fixture.integer;
    try fixture.reject(error.InvalidReturn, &bad, @fromBackingInt(@intCast(5)));
    try fixture.ir.setFunctionBody(fixture.function, &good, @fromBackingInt(@intCast(5)));
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
        ins(boolean, .{ .short_circuit = .{ .kind = .@"and", .lhs = @fromBackingInt(@intCast(0)), .rhs_start = 1, .rhs = @fromBackingInt(@intCast(1)) } }),
        ins(boolean, .{ .constant = constant }),
        ins(boolean, .{ .branch = .{ .condition = @fromBackingInt(@intCast(0)), .then_start = 1, .else_start = 3, .then_value = @fromBackingInt(@intCast(2)), .else_value = @fromBackingInt(@intCast(3)) } }),
        ins(boolean, .{ .unary = .{ .op = .bool_not, .operand = @fromBackingInt(@intCast(4)) } }),
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
    r.condition = @fromBackingInt(@intCast(1));
    try cases.append(std.testing.allocator, .{ .index = 4, .instruction = ins(boolean, .{ .branch = r }), .err = error.InvalidRegion });
    r = original;
    r.else_start = 2; // Splits a nested region from its merge.
    try cases.append(std.testing.allocator, .{ .index = 4, .instruction = ins(boolean, .{ .branch = r }), .err = error.InvalidRegion });
    r = original;
    r.then_value = @fromBackingInt(@intCast(1)); // A nested RHS cannot escape via the outer yield.
    try cases.append(std.testing.allocator, .{ .index = 4, .instruction = ins(boolean, .{ .branch = r }), .err = error.InvalidValueId });
    r = original;
    r.else_value = @fromBackingInt(@intCast(2));
    try cases.append(std.testing.allocator, .{ .index = 4, .instruction = ins(boolean, .{ .branch = r }), .err = error.InvalidValueId });
    r = original;
    r.then_value = @fromBackingInt(@intCast(3));
    try cases.append(std.testing.allocator, .{ .index = 4, .instruction = ins(boolean, .{ .branch = r }), .err = error.InvalidValueId });
    r = original;
    r.else_value = @fromBackingInt(@intCast(4));
    try cases.append(std.testing.allocator, .{ .index = 4, .instruction = ins(boolean, .{ .branch = r }), .err = error.InvalidValueId });
    r = original;
    r.else_start = 4;
    r.else_value = null;
    try cases.append(std.testing.allocator, .{ .index = 4, .instruction = ins(boolean, .{ .branch = r }), .err = error.OperandTypeMismatch });
    try cases.append(std.testing.allocator, .{ .index = 3, .instruction = ins(boolean, .{ .unary = .{ .op = .bool_not, .operand = @fromBackingInt(@intCast(2)) } }), .err = error.InvalidValueId });
    try cases.append(std.testing.allocator, .{ .index = 5, .instruction = ins(boolean, .{ .unary = .{ .op = .bool_not, .operand = @fromBackingInt(@intCast(3)) } }), .err = error.InvalidValueId });
    try cases.append(std.testing.allocator, .{ .index = 0, .instruction = ins(fixture.integer, .{ .constant = fixture.number }), .err = error.OperandTypeMismatch });
    for (cases.items) |case| {
        var invalid = valid;
        invalid[case.index] = case.instruction;
        try std.testing.expectError(case.err, ir.setFunctionBody(function, &invalid, @fromBackingInt(@intCast(5))));
        try std.testing.expect(ir.getFunction(function).?.body == null);
    }
    try std.testing.expectError(error.InvalidValueId, ir.setFunctionBody(function, &valid, @fromBackingInt(@intCast(3))));
    try ir.setFunctionBody(function, &valid, @fromBackingInt(@intCast(5)));
    try ir.setFunctionBody(fixture.function, &.{ins(fixture.integer, .{ .parameter = 0 })}, @fromBackingInt(@intCast(0)));
    const owned = @constCast(ir.getFunction(function).?.body.?.instructions);
    for (cases.items) |case| {
        owned[case.index] = case.instruction;
        try std.testing.expectError(case.err, ir.verify());
        owned[case.index] = valid[case.index];
    }
    owned[4].op.branch.then_value = @fromBackingInt(@intCast(0));
    owned[4].op.branch.else_value = @fromBackingInt(@intCast(0));
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
        ins(boolean, .{ .short_circuit = .{ .kind = .@"and", .lhs = @fromBackingInt(@intCast(0)), .rhs_start = 1, .rhs = @fromBackingInt(@intCast(1)) } }),
        ins(boolean, .{ .constant = constant }),
        ins(boolean, .{ .short_circuit = .{ .kind = .@"or", .lhs = @fromBackingInt(@intCast(0)), .rhs_start = 3, .rhs = @fromBackingInt(@intCast(3)) } }),
        ins(boolean, .{ .unary = .{ .op = .bool_not, .operand = @fromBackingInt(@intCast(2)) } }),
    };
    // Test insertion as well as verification after post-publication corruption.
    const cases = [_]struct { index: usize, instruction: Instruction, err: VerifyError }{
        .{ .index = 4, .instruction = ins(boolean, .{ .short_circuit = .{ .kind = .@"or", .lhs = @fromBackingInt(@intCast(0)), .rhs_start = 2, .rhs = @fromBackingInt(@intCast(3)) } }), .err = error.InvalidRegion },
        .{ .index = 2, .instruction = ins(boolean, .{ .short_circuit = .{ .kind = .@"and", .lhs = @fromBackingInt(@intCast(1)), .rhs_start = 1, .rhs = @fromBackingInt(@intCast(1)) } }), .err = error.InvalidRegion },
        .{ .index = 2, .instruction = ins(boolean, .{ .short_circuit = .{ .kind = .@"and", .lhs = @fromBackingInt(@intCast(0)), .rhs_start = 99, .rhs = @fromBackingInt(@intCast(1)) } }), .err = error.InvalidRegion },
        .{ .index = 2, .instruction = ins(boolean, .{ .short_circuit = .{ .kind = .@"and", .lhs = @fromBackingInt(@intCast(0)), .rhs_start = 1, .rhs = @fromBackingInt(@intCast(2)) } }), .err = error.InvalidValueId },
        .{ .index = 4, .instruction = ins(boolean, .{ .short_circuit = .{ .kind = .@"or", .lhs = @fromBackingInt(@intCast(0)), .rhs_start = 3, .rhs = @fromBackingInt(@intCast(1)) } }), .err = error.InvalidValueId },
        .{ .index = 5, .instruction = ins(boolean, .{ .unary = .{ .op = .bool_not, .operand = @fromBackingInt(@intCast(1)) } }), .err = error.InvalidValueId },
        .{ .index = 0, .instruction = ins(fixture.integer, .{ .constant = fixture.number }), .err = error.OperandTypeMismatch },
        .{ .index = 1, .instruction = ins(fixture.integer, .{ .constant = fixture.number }), .err = error.OperandTypeMismatch },
        .{ .index = 2, .instruction = ins(fixture.integer, valid[2].op), .err = error.OperandTypeMismatch },
        .{ .index = 5, .instruction = ins(void_ty, .{ .print = .{ .args = &.{@fromBackingInt(@intCast(1))}, .newline = true } }), .err = error.InvalidValueId },
        .{ .index = 5, .instruction = ins(boolean, .{ .binary = .{ .op = .eq, .lhs = @fromBackingInt(@intCast(1)), .rhs = @fromBackingInt(@intCast(0)) } }), .err = error.InvalidValueId },
        .{ .index = 5, .instruction = ins(boolean, .{ .call = .{ .callee = @fromBackingInt(@intCast(1)), .args = &.{} } }), .err = error.InvalidValueId },
        .{ .index = 2, .instruction = .{ .ty = boolean, .span = .{ .start = 0, .end = 99 }, .op = valid[2].op }, .err = error.InvalidSourceSpan },
    };
    for (cases) |case| {
        var invalid = valid;
        invalid[case.index] = case.instruction;
        try std.testing.expectError(case.err, ir.setFunctionBody(function, &invalid, @fromBackingInt(@intCast(5))));
        try std.testing.expect(ir.getFunction(function).?.body == null);
    }
    try std.testing.expectError(error.InvalidValueId, ir.setFunctionBody(function, &valid, @fromBackingInt(@intCast(1))));
    try ir.setFunctionBody(function, &valid, @fromBackingInt(@intCast(5)));
    // Complete the unrelated fixture declaration before verifying.
    try ir.setFunctionBody(fixture.function, &.{ins(fixture.integer, .{ .parameter = 0 })}, @fromBackingInt(@intCast(0)));
    try ir.verify();
    const owned = @constCast(ir.getFunction(function).?.body.?.instructions);
    for (cases) |case| {
        owned[case.index] = case.instruction;
        try std.testing.expectError(case.err, ir.verify());
        owned[case.index] = valid[case.index];
    }
    // Nesting and enclosing captures, including an empty region, are legal.
    owned[5].op.unary.operand = @fromBackingInt(@intCast(4));
    owned[4].op.short_circuit.rhs_start = 1;
    owned[4].op.short_circuit.rhs = @fromBackingInt(@intCast(2));
    try ir.verify();
    owned[2].op.short_circuit.rhs_start = 2;
    owned[2].op.short_circuit.rhs = @fromBackingInt(@intCast(0));
    try ir.verify();
    owned[4].op.short_circuit.rhs = @fromBackingInt(@intCast(1));
    try ir.verify();
    owned[5].op.unary.operand = @fromBackingInt(@intCast(2));
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
    var args = [_]ValueId{@fromBackingInt(@intCast(0))};
    var instructions = [_]Instruction{
        TestFunctions.instruction(fixture.integer, .{ .parameter = 0 }),
        TestFunctions.instruction(fixture.signature, .{ .function_ref = later }),
        TestFunctions.instruction(fixture.integer, .{ .call = .{ .callee = @fromBackingInt(@intCast(1)), .args = &args } }),
    };
    try ir.setFunctionBody(fixture.function, &instructions, @fromBackingInt(@intCast(2)));
    try std.testing.expectError(error.MissingFunctionBody, ir.verify());
    args[0] = @fromBackingInt(@intCast(999));
    instructions[0].ty = fixture.boolean;
    const stored = ir.getFunction(fixture.function).?.body.?;
    try std.testing.expectEqual(fixture.integer, stored.instructions[0].ty);
    try std.testing.expectEqual(@as(ValueId, @fromBackingInt(@intCast(0))), stored.instructions[2].op.call.args[0]);
    // The second function refers to itself, but no body is required at insertion.
    instructions[0].ty = fixture.integer;
    args[0] = @fromBackingInt(@intCast(0));
    try ir.setFunctionBody(later, &instructions, @fromBackingInt(@intCast(2)));
    try ir.verify();
    try std.testing.expectError(error.BodyAlreadySet, ir.setFunctionBody(later, &.{}, @fromBackingInt(@intCast(0))));
    try std.testing.expect(ir.getFunction(@fromBackingInt(@intCast(999))) == null);
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
                TestFunctions.instruction(result_type, .{ .binary = .{ .op = op, .lhs = @fromBackingInt(@intCast(0)), .rhs = @fromBackingInt(@intCast(0)) } }),
            }, @fromBackingInt(@intCast(1)));
            try ir.verify();
        }
        for ([_]UnaryOp{ .negate, .bit_not }) |op| {
            if (integer == fixture.unsigned and op == .negate) continue;
            ir.clearFunctions();
            const function = try ir.addFunction("unary", signature, test_span);
            try ir.setFunctionBody(function, &.{
                TestFunctions.instruction(integer, .{ .parameter = 0 }),
                TestFunctions.instruction(integer, .{ .unary = .{ .op = op, .operand = @fromBackingInt(@intCast(0)) } }),
            }, @fromBackingInt(@intCast(1)));
            try ir.verify();
        }
    }
    ir.clearFunctions();
    const boolean_signature = try ir.internType(.{ .function = .{ .params = &.{fixture.boolean}, .result = fixture.boolean } });
    const function = try ir.addFunction("boolean", boolean_signature, test_span);
    try ir.setFunctionBody(function, &.{
        TestFunctions.instruction(fixture.boolean, .{ .parameter = 0 }),
        TestFunctions.instruction(fixture.boolean, .{ .unary = .{ .op = .bool_not, .operand = @fromBackingInt(@intCast(0)) } }),
        TestFunctions.instruction(fixture.boolean, .{ .binary = .{ .op = .eq, .lhs = @fromBackingInt(@intCast(0)), .rhs = @fromBackingInt(@intCast(1)) } }),
        TestFunctions.instruction(fixture.boolean, .{ .binary = .{ .op = .ne, .lhs = @fromBackingInt(@intCast(1)), .rhs = @fromBackingInt(@intCast(2)) } }),
    }, @fromBackingInt(@intCast(3)));
    try ir.verify();
}

test "export IR owns source names replacements and clears exports" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const ir = &fixture.ir;
    try std.testing.expect(ir.getFunction(fixture.function).?.export_name == null);
    {
        const source = try std.testing.allocator.dupe(u8, "foreign_identity");
        defer std.testing.allocator.free(source);
        try ir.setExportName(fixture.function, source);
        @memset(source, 'x');
    }
    try std.testing.expectEqualStrings("foreign_identity", ir.getFunction(fixture.function).?.export_name.?);
    try std.testing.expectEqualStrings("identity", ir.getFunction(fixture.function).?.name);
    // A borrowed export slice may itself be the replacement input.
    try ir.setExportName(fixture.function, ir.getFunction(fixture.function).?.export_name.?);
    try ir.setExportName(fixture.function, "replacement");
    try std.testing.expectEqualStrings("replacement", ir.getFunction(fixture.function).?.export_name.?);
    try ir.setFunctionBody(fixture.function, &.{TestFunctions.instruction(fixture.integer, .{ .parameter = 0 })}, @fromBackingInt(@intCast(0)));
    try ir.verify();
    ir.clearFunctions();
    try ir.verify();
    const reused = try ir.addFunction("identity", fixture.signature, test_span);
    try std.testing.expect(ir.getFunction(reused).?.export_name == null);
    try ir.setExportName(reused, "replacement");
    try ir.setFunctionBody(reused, &.{TestFunctions.instruction(fixture.integer, .{ .parameter = 0 })}, @fromBackingInt(@intCast(0)));
    try ir.verify();
}

test "export IR accepts bool native integers scalar widths and void results" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    ir.source_len = 1;
    const boolean = try ir.internType(.boolean);
    const unit = try ir.internType(.void);
    const unit_value = try ir.internConstant(.{ .ty = unit, .value = .void });
    var scalars: std.ArrayList(TypeId) = .empty;
    defer scalars.deinit(std.testing.allocator);
    try scalars.append(std.testing.allocator, boolean);
    for ([_]std.builtin.Signedness{ .signed, .unsigned }) |signedness| {
        for ([_]u16{ 8, 16, 32, 64, native_pointer_bits }) |bits| {
            try scalars.append(std.testing.allocator, try ir.internType(.{ .integer = .{ .signedness = signedness, .bits = bits } }));
        }
    }
    for (scalars.items) |scalar| {
        const signature = try ir.internType(.{ .function = .{ .params = &.{scalar}, .result = scalar } });
        const function = try ir.addFunction("identity", signature, test_span);
        try ir.setExportName(function, "scalar_identity");
        try ir.setFunctionBody(function, &.{TestFunctions.instruction(scalar, .{ .parameter = 0 })}, @fromBackingInt(@intCast(0)));
        try ir.verify();
        ir.clearFunctions();
        const sink_type = try ir.internType(.{ .function = .{ .params = &.{scalar}, .result = unit } });
        const sink = try ir.addFunction("sink", sink_type, test_span);
        try ir.setExportName(sink, "scalar_sink");
        try ir.setFunctionBody(sink, &.{TestFunctions.instruction(unit, .{ .constant = unit_value })}, @fromBackingInt(@intCast(0)));
        try ir.verify();
        ir.clearFunctions();
    }
    const signature = try ir.internType(.{ .function = .{ .params = &.{}, .result = unit } });
    const function = try ir.addFunction("unit", signature, test_span);
    try ir.setExportName(function, "unit_result");
    try ir.setFunctionBody(function, &.{TestFunctions.instruction(unit, .{ .constant = unit_value })}, @fromBackingInt(@intCast(0)));
    try ir.verify();
}

test "export IR setter and verifier reject unsafe names without replacing metadata" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const ir = &fixture.ir;
    try ir.setExportName(fixture.function, "valid_export");
    try ir.setFunctionBody(fixture.function, &.{TestFunctions.instruction(fixture.integer, .{ .parameter = 0 })}, @fromBackingInt(@intCast(0)));
    const original = ir.functions.items[0];
    defer ir.functions.items[0] = original;
    for ([_][]const u8{ "", "_private", "9bad", "a-b", "a\x00b", "é", "main", "while", "bool", "INT64_MAX", "INT_LEAST8_MAX", "INT_LEAST64_MIN", "UINT_FAST32_MAX", "INT_FAST16_MIN", "PRId64", "uint32_t", "FILE", "malloc", "sin", "sqrt", "time", "signal", "p0", "v12", "nov_frame", "nov_roots", "nov_target", "nov_rt_x", "nov_gc_x", "nov_fn_x", "nov_record_x", "nov_range_x", "nov_stringFoo", "nov_arrayFoo", "nov_M0" }) |name| {
        try std.testing.expectError(error.InvalidExportName, ir.setExportName(fixture.function, name));
        try std.testing.expectEqual(original.export_name.?.ptr, ir.functions.items[0].export_name.?.ptr);
        ir.functions.items[0].export_name = name;
        const result = ir.verify();
        ir.functions.items[0].export_name = original.export_name;
        try std.testing.expectError(error.InvalidExportName, result);
    }
    try std.testing.expectError(error.InvalidFunctionId, ir.setExportName(@fromBackingInt(@intCast(999)), "valid_export"));
    try ir.verify();
}

test "export IR rejects nonscalar signatures including forged export metadata" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const ir = &fixture.ir;
    const string = try ir.internType(.string);
    const array = try ir.internType(.{ .array = fixture.integer });
    const range = try ir.internType(.{ .range = fixture.integer });
    const record = try ir.internType(.{ .record = .{ .identity = 0, .fields = &.{} } });
    const tagged = try ir.internType(.{ .record = .{ .identity = 1, .fields = &.{}, .tagged = true } });
    const enumeration = try ir.internType(.{ .enumeration = .{ .identity = 2, .name = "E", .tags = &.{.{ .name = "tag", .span = test_span }} } });
    const original = ir.functions.items[0];
    defer ir.functions.items[0] = original;
    for ([_]TypeId{ string, array, range, record, tagged, enumeration, fixture.signature }) |nonscalar| {
        for ([_]bool{ false, true }) |as_result| {
            const signature = try ir.internType(.{ .function = .{
                .params = if (as_result) &.{} else &.{nonscalar},
                .result = if (as_result) nonscalar else fixture.integer,
            } });
            ir.functions.items[0].ty = signature;
            try std.testing.expectError(error.InvalidExportSignature, ir.setExportName(fixture.function, "bad_signature"));
            try std.testing.expect(ir.functions.items[0].export_name == null);
            ir.functions.items[0].export_name = "forged_export";
            const result = ir.verify();
            ir.functions.items[0] = original;
            try std.testing.expectError(error.InvalidExportSignature, result);
        }
    }
    for ([_]std.builtin.Signedness{ .signed, .unsigned }) |signedness| {
        for ([_]u16{ 1, 7, 24, 65, 128 }) |bits| {
            const integer = try ir.internType(.{ .integer = .{ .signedness = signedness, .bits = bits } });
            for ([_]bool{ false, true }) |as_result| {
                const signature = try ir.internType(.{ .function = .{
                    .params = if (as_result) &.{} else &.{integer},
                    .result = if (as_result) integer else fixture.integer,
                } });
                ir.functions.items[0].ty = signature;
                try std.testing.expectError(error.InvalidExportSignature, ir.setExportName(fixture.function, "bad_width"));
                ir.functions.items[0].export_name = "forged_export";
                const result = ir.verify();
                ir.functions.items[0] = original;
                try std.testing.expectError(error.InvalidExportSignature, result);
            }
        }
    }
}

test "export IR duplicate names are compilation wide and malformed ids are rejected" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const ir = &fixture.ir;
    try ir.source_lengths.appendSlice(std.testing.allocator, &.{ 10, 10 });
    const other = try ir.addFunction("identity", fixture.signature, .{ .source = @fromBackingInt(@intCast(1)), .start = 0, .end = 1 });
    try ir.setExportName(fixture.function, "first_export");
    try ir.setExportName(other, "second_export");
    try std.testing.expectError(error.DuplicateExportName, ir.setExportName(other, "first_export"));
    const original = ir.functions.items[@backingInt(other)];
    defer ir.functions.items[@backingInt(other)] = original;
    try std.testing.expectEqualStrings("second_export", original.export_name.?);
    ir.functions.items[@backingInt(other)].export_name = ir.functions.items[0].export_name;
    try std.testing.expectError(error.DuplicateExportName, ir.verify());
    ir.functions.items[@backingInt(other)] = original;
    ir.functions.items[@backingInt(other)].ty = @fromBackingInt(@intCast(999));
    try std.testing.expectError(error.InvalidTypeId, ir.setExportName(other, "replacement"));
    try std.testing.expectError(error.InvalidTypeId, ir.verify());
    ir.functions.items[@backingInt(other)].ty = fixture.integer;
    try std.testing.expectError(error.InvalidFunctionType, ir.verify());
    ir.functions.items[@backingInt(other)] = original;
    const unit = try ir.internType(.void);
    const later_signature = try ir.internType(.{ .function = .{ .params = &.{ fixture.integer, fixture.integer }, .result = fixture.integer } });
    ir.functions.items[@backingInt(other)].ty = later_signature;
    const signature = &ir.types.items[@backingInt(later_signature)].function;
    const old_result = signature.result;
    defer signature.result = old_result;
    signature.result = @fromBackingInt(@intCast(999));
    try std.testing.expectError(error.InvalidTypeId, ir.setExportName(other, "replacement"));
    try std.testing.expectError(error.InvalidTypeId, ir.verify());
    signature.result = old_result;
    const params = @constCast(signature.params);
    const old_param = params[0];
    defer params[0] = old_param;
    params[0] = @fromBackingInt(@intCast(999));
    try std.testing.expectError(error.InvalidTypeId, ir.setExportName(other, "replacement"));
    try std.testing.expectError(error.InvalidTypeId, ir.verify());
    params[0] = unit;
    try std.testing.expectError(error.VoidParameter, ir.setExportName(other, "replacement"));
    try std.testing.expectError(error.VoidParameter, ir.verify());
    params[0] = old_param;
    try std.testing.expectEqualStrings("second_export", ir.getFunction(other).?.export_name.?);
}

fn testExportAllocationFailures(allocator: std.mem.Allocator) !void {
    var fixture = try TestFunctions.init(allocator);
    defer fixture.ir.deinit();
    const ir = &fixture.ir;
    try ir.setFunctionBody(fixture.function, &.{TestFunctions.instruction(fixture.integer, .{ .parameter = 0 })}, @fromBackingInt(@intCast(0)));
    ir.setExportName(fixture.function, "initial_export") catch |err| {
        try std.testing.expect(ir.getFunction(fixture.function).?.export_name == null);
        try ir.verify();
        return err;
    };
    const original = ir.getFunction(fixture.function).?;
    ir.setExportName(fixture.function, "replacement_export") catch |err| {
        const retained = ir.getFunction(fixture.function).?;
        try std.testing.expectEqual(original.export_name.?.ptr, retained.export_name.?.ptr);
        try std.testing.expectEqualStrings("initial_export", retained.export_name.?);
        try std.testing.expectEqual(original.name.ptr, retained.name.ptr);
        try std.testing.expectEqual(original.body.?.instructions.ptr, retained.body.?.instructions.ptr);
        try ir.verify();
        return err;
    };
    try std.testing.expectEqualStrings("replacement_export", ir.getFunction(fixture.function).?.export_name.?);
    try ir.verify();
    ir.clearFunctions();
    try ir.verify();
}

test "export IR setter ownership and replacement are atomic under every OOM" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testExportAllocationFailures, .{});
}

test "function metadata validation rejects corruption and preserves declarations" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const ir = &fixture.ir;
    try std.testing.expectError(error.InvalidFunctionName, ir.addFunction("", fixture.signature, test_span));
    try std.testing.expectError(error.DuplicateFunctionName, ir.addFunction("identity", fixture.signature, test_span));
    try std.testing.expectError(error.InvalidFunctionType, ir.addFunction("bad", fixture.integer, test_span));
    try std.testing.expectError(error.InvalidTypeId, ir.addFunction("bad", @fromBackingInt(@intCast(999)), test_span));
    for ([_]Span{ .{ .start = 2, .end = 1 }, .{ .start = 0, .end = 11 } }) |span| {
        try std.testing.expectError(error.InvalidSourceSpan, ir.addFunction("bad", fixture.signature, span));
    }
    try std.testing.expectEqual(@as(usize, 1), ir.functions.items.len);
    try std.testing.expectError(error.InvalidFunctionId, ir.setFunctionBody(@fromBackingInt(@intCast(999)), &.{}, @fromBackingInt(@intCast(0))));
    try std.testing.expectError(error.MissingFunctionBody, ir.verify());
    try ir.setFunctionBody(fixture.function, &.{TestFunctions.instruction(fixture.integer, .{ .parameter = 0 })}, @fromBackingInt(@intCast(0)));
    const original = ir.functions.items[0];
    ir.functions.items[0].name = "";
    try std.testing.expectError(error.InvalidFunctionName, ir.verify());
    ir.functions.items[0] = original;
    ir.functions.items[0].ty = fixture.integer;
    try std.testing.expectError(error.InvalidFunctionType, ir.verify());
    ir.functions.items[0].ty = @fromBackingInt(@intCast(999));
    try std.testing.expectError(error.InvalidTypeId, ir.verify());
    ir.functions.items[0] = original;
    ir.functions.items[0].span.end = 11;
    try std.testing.expectError(error.InvalidSourceSpan, ir.verify());
    ir.functions.items[0] = original;
    const other = try ir.addFunction("other", fixture.signature, test_span);
    const other_name = ir.functions.items[@backingInt(other)].name;
    ir.functions.items[@backingInt(other)].name = original.name;
    try std.testing.expectError(error.DuplicateFunctionName, ir.verify());
    ir.functions.items[@backingInt(other)].name = other_name;
    try ir.setFunctionBody(other, &.{TestFunctions.instruction(fixture.integer, .{ .parameter = 0 })}, @fromBackingInt(@intCast(0)));
    ir.source_len = 0;
    try std.testing.expectError(error.InvalidSourceSpan, ir.verify());
    ir.source_len = 10;
    try ir.verify();
}

test "body validation rejects invalid ids parameter types spans and returns" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const integer = fixture.integer;
    try fixture.reject(error.InvalidValueId, &.{}, @fromBackingInt(@intCast(0)));
    try fixture.reject(error.InvalidTypeId, &.{TestFunctions.instruction(@fromBackingInt(@intCast(999)), .{ .parameter = 0 })}, @fromBackingInt(@intCast(0)));
    try fixture.reject(error.InvalidParameter, &.{TestFunctions.instruction(integer, .{ .parameter = 1 })}, @fromBackingInt(@intCast(0)));
    try fixture.reject(error.OperandTypeMismatch, &.{TestFunctions.instruction(fixture.boolean, .{ .parameter = 0 })}, @fromBackingInt(@intCast(0)));
    try fixture.reject(error.InvalidConstId, &.{TestFunctions.instruction(integer, .{ .constant = @fromBackingInt(@intCast(999)) })}, @fromBackingInt(@intCast(0)));
    try fixture.reject(error.OperandTypeMismatch, &.{TestFunctions.instruction(fixture.boolean, .{ .constant = fixture.number })}, @fromBackingInt(@intCast(0)));
    try fixture.reject(error.InvalidFunctionId, &.{TestFunctions.instruction(fixture.signature, .{ .function_ref = @fromBackingInt(@intCast(999)) })}, @fromBackingInt(@intCast(0)));
    try fixture.reject(error.OperandTypeMismatch, &.{TestFunctions.instruction(integer, .{ .function_ref = fixture.function })}, @fromBackingInt(@intCast(0)));
    const truth = try fixture.ir.internConstant(.{ .ty = fixture.boolean, .value = .{ .boolean = true } });
    try fixture.reject(error.InvalidReturn, &.{TestFunctions.instruction(fixture.boolean, .{ .constant = truth })}, @fromBackingInt(@intCast(0)));
    try fixture.reject(error.InvalidValueId, &.{TestFunctions.instruction(integer, .{ .constant = fixture.number })}, @fromBackingInt(@intCast(1)));
    for ([_]Span{ .{ .start = 3, .end = 2 }, .{ .start = 10, .end = 11 } }) |span| {
        try fixture.reject(error.InvalidSourceSpan, &.{.{ .ty = integer, .span = span, .op = .{ .parameter = 0 } }}, @fromBackingInt(@intCast(0)));
    }
    try fixture.ir.setFunctionBody(fixture.function, &.{TestFunctions.instruction(integer, .{ .constant = fixture.number })}, @fromBackingInt(@intCast(0)));
    const body = &fixture.ir.functions.items[0].body.?;
    const instructions = @constCast(body.instructions);
    instructions[0].op = .{ .constant = @fromBackingInt(@intCast(999)) };
    try std.testing.expectError(error.InvalidConstId, fixture.ir.verify());
    instructions[0].op = .{ .constant = fixture.number };
    instructions[0].span.end = 11;
    try std.testing.expectError(error.InvalidSourceSpan, fixture.ir.verify());
    instructions[0].span = test_span;
    body.result = @fromBackingInt(@intCast(1));
    try std.testing.expectError(error.InvalidValueId, fixture.ir.verify());
    body.result = @fromBackingInt(@intCast(0));
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
        try fixture.reject(error.OperandTypeMismatch, &.{ first, second, TestFunctions.instruction(integer, .{ .binary = .{ .op = op, .lhs = @fromBackingInt(@intCast(0)), .rhs = @fromBackingInt(@intCast(1)) } }) }, @fromBackingInt(@intCast(2)));
        try fixture.reject(error.OperandTypeMismatch, &.{ first, TestFunctions.instruction(fixture.unsigned, .{ .binary = .{ .op = op, .lhs = @fromBackingInt(@intCast(0)), .rhs = @fromBackingInt(@intCast(0)) } }) }, @fromBackingInt(@intCast(1)));
        try fixture.reject(error.OperandTypeMismatch, &.{ first, TestFunctions.instruction(fixture.boolean, .{ .binary = .{ .op = op, .lhs = @fromBackingInt(@intCast(0)), .rhs = @fromBackingInt(@intCast(0)) } }) }, @fromBackingInt(@intCast(1)));
        for ([_]u32{ 1, 999 }) |invalid| {
            try fixture.reject(error.InvalidValueId, &.{ first, TestFunctions.instruction(integer, .{ .binary = .{ .op = op, .lhs = @fromBackingInt(@intCast(0)), .rhs = @fromBackingInt(@intCast(invalid)) } }) }, @fromBackingInt(@intCast(1)));
        }
        const narrow = try fixture.ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 8 } });
        const number = try fixture.ir.internConstant(.{ .ty = narrow, .value = .{ .signed_integer = 1 } });
        try fixture.reject(error.OperandTypeMismatch, &.{ first, TestFunctions.instruction(narrow, .{ .constant = number }), TestFunctions.instruction(integer, .{ .binary = .{ .op = op, .lhs = @fromBackingInt(@intCast(0)), .rhs = @fromBackingInt(@intCast(1)) } }) }, @fromBackingInt(@intCast(2)));
        const string = try fixture.ir.internType(.string);
        const constant = try fixture.ir.internConstant(.{ .ty = string, .value = .{ .string = "" } });
        const array = try fixture.ir.internType(.{ .array = integer });
        for ([_]Instruction{
            TestFunctions.instruction(string, .{ .constant = constant }),
            TestFunctions.instruction(array, .{ .array_literal = &.{} }),
            TestFunctions.instruction(fixture.signature, .{ .function_ref = fixture.function }),
        }) |invalid| {
            try fixture.reject(error.InvalidOperation, &.{ invalid, TestFunctions.instruction(invalid.ty, .{ .binary = .{ .op = op, .lhs = @fromBackingInt(@intCast(0)), .rhs = @fromBackingInt(@intCast(0)) } }) }, @fromBackingInt(@intCast(1)));
        }
    }
}

test "SSA operands must precede their use and operation types must agree" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const integer = fixture.integer;
    const first = TestFunctions.instruction(integer, .{ .parameter = 0 });
    for ([_]u32{ 1, 2, 999 }) |invalid| {
        try fixture.reject(error.InvalidValueId, &.{ first, TestFunctions.instruction(integer, .{ .unary = .{ .op = .negate, .operand = @fromBackingInt(@intCast(invalid)) } }) }, @fromBackingInt(@intCast(1)));
        try fixture.reject(error.InvalidValueId, &.{ first, TestFunctions.instruction(integer, .{ .binary = .{ .op = .add, .lhs = @fromBackingInt(@intCast(0)), .rhs = @fromBackingInt(@intCast(invalid)) } }) }, @fromBackingInt(@intCast(1)));
        try fixture.reject(error.InvalidValueId, &.{ first, TestFunctions.instruction(integer, .{ .binary = .{ .op = .add, .lhs = @fromBackingInt(@intCast(invalid)), .rhs = @fromBackingInt(@intCast(0)) } }) }, @fromBackingInt(@intCast(1)));
    }
    try fixture.reject(error.OperandTypeMismatch, &.{ first, TestFunctions.instruction(fixture.unsigned, .{ .unary = .{ .op = .bit_not, .operand = @fromBackingInt(@intCast(0)) } }) }, @fromBackingInt(@intCast(1)));
    try fixture.reject(error.InvalidOperation, &.{ first, TestFunctions.instruction(integer, .{ .unary = .{ .op = .bool_not, .operand = @fromBackingInt(@intCast(0)) } }) }, @fromBackingInt(@intCast(1)));
    const unsigned_constant = try fixture.ir.internConstant(.{ .ty = fixture.unsigned, .value = .{ .unsigned_integer = 1 } });
    const unsigned_first = TestFunctions.instruction(fixture.unsigned, .{ .constant = unsigned_constant });
    try fixture.reject(error.InvalidOperation, &.{ unsigned_first, TestFunctions.instruction(fixture.unsigned, .{ .unary = .{ .op = .negate, .operand = @fromBackingInt(@intCast(0)) } }) }, @fromBackingInt(@intCast(1)));
    try fixture.reject(error.OperandTypeMismatch, &.{ first, unsigned_first, TestFunctions.instruction(integer, .{ .binary = .{ .op = .add, .lhs = @fromBackingInt(@intCast(0)), .rhs = @fromBackingInt(@intCast(1)) } }) }, @fromBackingInt(@intCast(2)));
    try fixture.reject(error.OperandTypeMismatch, &.{ first, TestFunctions.instruction(fixture.unsigned, .{ .binary = .{ .op = .add, .lhs = @fromBackingInt(@intCast(0)), .rhs = @fromBackingInt(@intCast(0)) } }) }, @fromBackingInt(@intCast(1)));
    try fixture.reject(error.OperandTypeMismatch, &.{ first, TestFunctions.instruction(integer, .{ .binary = .{ .op = .lt, .lhs = @fromBackingInt(@intCast(0)), .rhs = @fromBackingInt(@intCast(0)) } }) }, @fromBackingInt(@intCast(1)));
    const truth = try fixture.ir.internConstant(.{ .ty = fixture.boolean, .value = .{ .boolean = true } });
    const boolean_first = TestFunctions.instruction(fixture.boolean, .{ .constant = truth });
    for ([_]UnaryOp{ .negate, .bit_not }) |op| {
        try fixture.reject(error.InvalidOperation, &.{ boolean_first, TestFunctions.instruction(fixture.boolean, .{ .unary = .{ .op = op, .operand = @fromBackingInt(@intCast(0)) } }) }, @fromBackingInt(@intCast(1)));
    }
    for ([_]BinaryOp{ .add, .sub, .mul, .div, .mod, .bit_and, .bit_or, .bit_xor, .lt, .le, .gt, .ge }) |op| {
        try fixture.reject(error.InvalidOperation, &.{ boolean_first, TestFunctions.instruction(fixture.boolean, .{ .binary = .{ .op = op, .lhs = @fromBackingInt(@intCast(0)), .rhs = @fromBackingInt(@intCast(0)) } }) }, @fromBackingInt(@intCast(1)));
    }
    // A value in another function cannot satisfy an operand in this body.
    const other = try fixture.ir.addFunction("other", fixture.signature, test_span);
    try fixture.ir.setFunctionBody(other, &.{first}, @fromBackingInt(@intCast(0)));
    try fixture.reject(error.InvalidValueId, &.{TestFunctions.instruction(integer, .{ .unary = .{ .op = .negate, .operand = @fromBackingInt(@intCast(0)) } })}, @fromBackingInt(@intCast(0)));
}

test "calls validate callee order signatures arity arguments and results" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const integer = fixture.integer;
    const first = TestFunctions.instruction(integer, .{ .parameter = 0 });
    const reference = TestFunctions.instruction(fixture.signature, .{ .function_ref = fixture.function });
    try fixture.reject(error.InvalidCall, &.{ first, TestFunctions.instruction(integer, .{ .call = .{ .callee = @fromBackingInt(@intCast(0)), .args = &.{} } }) }, @fromBackingInt(@intCast(1)));
    for ([_]u32{ 2, 999 }) |invalid| {
        try fixture.reject(error.InvalidValueId, &.{ first, reference, TestFunctions.instruction(integer, .{ .call = .{ .callee = @fromBackingInt(@intCast(invalid)), .args = &.{@fromBackingInt(@intCast(0))} } }) }, @fromBackingInt(@intCast(2)));
        try fixture.reject(error.InvalidValueId, &.{ first, reference, TestFunctions.instruction(integer, .{ .call = .{ .callee = @fromBackingInt(@intCast(1)), .args = &.{@fromBackingInt(@intCast(invalid))} } }) }, @fromBackingInt(@intCast(2)));
    }
    for ([_][]const ValueId{ &.{}, &.{ @fromBackingInt(@intCast(0)), @fromBackingInt(@intCast(0)) } }) |args| {
        try fixture.reject(error.InvalidCall, &.{ first, reference, TestFunctions.instruction(integer, .{ .call = .{ .callee = @fromBackingInt(@intCast(1)), .args = args } }) }, @fromBackingInt(@intCast(2)));
    }
    try fixture.reject(error.OperandTypeMismatch, &.{ first, reference, TestFunctions.instruction(integer, .{ .call = .{ .callee = @fromBackingInt(@intCast(1)), .args = &.{@fromBackingInt(@intCast(1))} } }) }, @fromBackingInt(@intCast(2)));
    try fixture.reject(error.InvalidCall, &.{ first, reference, TestFunctions.instruction(fixture.boolean, .{ .call = .{ .callee = @fromBackingInt(@intCast(1)), .args = &.{@fromBackingInt(@intCast(0))} } }) }, @fromBackingInt(@intCast(2)));
    try fixture.ir.setFunctionBody(fixture.function, &.{ first, reference, TestFunctions.instruction(integer, .{ .call = .{ .callee = @fromBackingInt(@intCast(1)), .args = &.{@fromBackingInt(@intCast(0))} } }) }, @fromBackingInt(@intCast(2)));
    const instructions = @constCast(fixture.ir.getFunction(fixture.function).?.body.?.instructions);
    const original = instructions[2];
    instructions[2].op.call.callee = @fromBackingInt(@intCast(2));
    try std.testing.expectError(error.InvalidValueId, fixture.ir.verify());
    instructions[2] = original;
    @constCast(instructions[2].op.call.args)[0] = @fromBackingInt(@intCast(2));
    try std.testing.expectError(error.InvalidValueId, fixture.ir.verify());
    @constCast(instructions[2].op.call.args)[0] = @fromBackingInt(@intCast(0));
    try fixture.ir.verify();
}

fn testFunctionAllocationFailures(allocator: std.mem.Allocator) !void {
    var fixture = try TestFunctions.init(allocator);
    defer fixture.ir.deinit();
    const ir = &fixture.ir;
    const first = TestFunctions.instruction(fixture.integer, .{ .parameter = 0 });
    ir.setFunctionBody(fixture.function, &.{first}, @fromBackingInt(@intCast(0))) catch |err| {
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
            TestFunctions.instruction(fixture.integer, .{ .call = .{ .callee = @fromBackingInt(@intCast(1)), .args = &.{@fromBackingInt(@intCast(0))} } }),
            TestFunctions.instruction(fixture.integer, .{ .call = .{ .callee = @fromBackingInt(@intCast(1)), .args = &.{@fromBackingInt(@intCast(2))} } }),
        };
        ir.setFunctionBody(function, &instructions, @fromBackingInt(@intCast(3))) catch |err| {
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
    try ir.setFunctionBody(reused, &.{first}, @fromBackingInt(@intCast(0)));
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
    try ir.setFunctionBody(leaf, &.{.{ .ty = void_type, .span = span, .op = .{ .constant = void_value } }}, @fromBackingInt(@intCast(0)));
    try ir.setFunctionBody(identity, &.{.{ .ty = signature, .span = span, .op = .{ .parameter = 0 } }}, @fromBackingInt(@intCast(0)));
    try ir.setFunctionBody(call, &.{
        .{ .ty = signature, .span = span, .op = .{ .parameter = 0 } },
        .{ .ty = void_type, .span = span, .op = .{ .call = .{ .callee = @fromBackingInt(@intCast(0)), .args = &.{} } } },
    }, @fromBackingInt(@intCast(1)));
    try ir.verify();
}

test "body insertion checks referenced signatures and verification checks pools first" {
    var fixture = try TestFunctions.init(std.testing.allocator);
    defer fixture.ir.deinit();
    const ir = &fixture.ir;
    const other = try ir.addFunction("other", fixture.signature, test_span);
    const reference = TestFunctions.instruction(fixture.signature, .{ .function_ref = other });
    ir.functions.items[@backingInt(other)].ty = fixture.integer;
    try fixture.reject(error.InvalidFunctionType, &.{reference}, @fromBackingInt(@intCast(0)));
    ir.functions.items[@backingInt(other)].ty = @fromBackingInt(@intCast(999));
    try fixture.reject(error.InvalidTypeId, &.{reference}, @fromBackingInt(@intCast(0)));
    ir.functions.items[@backingInt(other)].ty = fixture.signature;
    const signature = &ir.types.items[@backingInt(fixture.signature)].function;
    signature.result = @fromBackingInt(@intCast(999));
    try fixture.reject(error.InvalidTypeId, &.{reference}, @fromBackingInt(@intCast(0)));
    try std.testing.expectError(error.InvalidTypeId, ir.verify());
    signature.result = fixture.integer;
    ir.constants.items[@backingInt(fixture.number)].ty = @fromBackingInt(@intCast(999));
    try std.testing.expectError(error.InvalidTypeId, ir.verify());
    ir.constants.items[@backingInt(fixture.number)].ty = fixture.integer;
    try std.testing.expectError(error.MissingFunctionBody, ir.verify());
}

test "function ownership and body publication survive every allocation failure" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testFunctionAllocationFailures, .{});
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
    const fields = @constCast(ir.types.items[@backingInt(ids[0])].record.fields);
    const saved = fields[0].ty;
    fields[0].ty = ids[0];
    try std.testing.expectError(error.CyclicRecordLayout, ir.verify());
    fields[0].ty = @fromBackingInt(@intCast(std.math.maxInt(u32)));
    try std.testing.expectError(error.InvalidTypeId, ir.verify());
    fields[0].ty = saved;
    try ir.verify();
}

test "managed array IR owns layouts and rejects forged record equality and payloads" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const span: Span = .{ .start = 0, .end = 0 };
    const string = try ir.internType(.string);
    const boolean = try ir.internType(.boolean);
    const unit = try ir.internType(.void);
    const record = try ir.internType(.{ .record = .{ .identity = 0, .fields = &.{} } });
    try ir.setRecordFields(record, &.{.{ .name = "s", .ty = string, .span = span }});
    const tagged = try ir.internType(.{ .record = .{ .identity = 1, .fields = &.{}, .tagged = true } });
    const array = try ir.internType(.{ .array = record });
    const strings = try ir.internType(.{ .array = string });
    _ = try ir.internType(.{ .array = array });
    _ = try ir.internType(.{ .array = tagged });
    const signature = try ir.internType(.{ .function = .{ .params = &.{ record, array }, .result = boolean } });
    const function = try ir.addFunction("contains", signature, span);
    var instructions = [_]Instruction{
        .{ .ty = record, .span = span, .op = .{ .parameter = 0 } },
        .{ .ty = array, .span = span, .op = .{ .parameter = 1 } },
        .{ .ty = boolean, .span = span, .op = .{ .array_contains = .{ .value = @fromBackingInt(@intCast(0)), .array = @fromBackingInt(@intCast(1)) } } },
    };
    try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(function, &instructions, @fromBackingInt(@intCast(2))));
    instructions[2] = .{ .ty = array, .span = span, .op = .{ .array_literal = &.{@fromBackingInt(@intCast(1))} } };
    try std.testing.expectError(error.OperandTypeMismatch, ir.setFunctionBody(function, &instructions, @fromBackingInt(@intCast(2))));
    // Whole-pool verification still checks an array element's nominal graph.
    ir.types.items[@backingInt(array)] = .{ .array = unit };
    try std.testing.expectError(error.InvalidArrayElement, ir.verify());
    ir.types.items[@backingInt(array)] = .{ .array = record };
    const fields = @constCast(ir.types.items[@backingInt(record)].record.fields);
    fields[0].ty = strings;
    ir.clearFunctions();
    try ir.verify();
    // Arrays must not hide a cycle in an otherwise by-value layout DAG.
    fields[0].ty = array;
    try std.testing.expectError(error.CyclicRecordLayout, ir.verify());
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

test "range values require integer elements exact visible operands and immutable projections" {
    var t = try TestFunctions.init(std.testing.allocator);
    defer t.ir.deinit();
    const range = try t.ir.internType(.{ .range = t.integer });
    try std.testing.expectEqual(range, try t.ir.internType(.{ .range = t.integer }));
    try std.testing.expectError(error.InvalidTypeId, t.ir.internType(.{ .range = t.boolean }));
    try std.testing.expectError(error.InvalidTypeId, t.ir.internType(.{ .range = @fromBackingInt(@intCast(999)) }));
    _ = try t.ir.internType(.{ .array = range });
    try std.testing.expectError(error.NonConstantType, t.ir.internConstant(.{ .ty = range, .value = .void }));
    try std.testing.expect(!isManaged(t.ir.getType(range).?));
    try std.testing.expect(!isRecordField(t.ir.getType(range).?));
    try std.testing.expect(!isUnionPayload(t.ir.getType(range).?));
    const make = TestFunctions.instruction;
    const good = [_]Instruction{
        make(t.integer, .{ .parameter = 0 }),
        make(range, .{ .range_init = .{ .start = @fromBackingInt(@intCast(0)), .end = @fromBackingInt(@intCast(0)), .inclusive = true } }),
        make(t.integer, .{ .range_start = @fromBackingInt(@intCast(1)) }),
        make(t.integer, .{ .range_end = @fromBackingInt(@intCast(1)) }),
        make(t.boolean, .{ .range_inclusive = @fromBackingInt(@intCast(1)) }),
    };
    try t.ir.setFunctionBody(t.function, &good, @fromBackingInt(@intCast(3)));
    const owned = @constCast(t.ir.getFunction(t.function).?.body.?.instructions);
    for ([_]u32{ 1, 99 }) |bad| {
        owned[1].op.range_init.start = @fromBackingInt(@intCast(bad));
        try std.testing.expectError(error.InvalidValueId, t.ir.verify());
        owned[1] = good[1];
        owned[1].op.range_init.end = @fromBackingInt(@intCast(bad));
        try std.testing.expectError(error.InvalidValueId, t.ir.verify());
        owned[1] = good[1];
    }
    for ([_]usize{ 2, 3, 4 }) |i| {
        const invalid: Operation = switch (i) {
            2 => .{ .range_start = @fromBackingInt(@intCast(0)) },
            3 => .{ .range_end = @fromBackingInt(@intCast(0)) },
            else => .{ .range_inclusive = @fromBackingInt(@intCast(0)) },
        };
        owned[i].op = invalid;
        try std.testing.expectError(error.InvalidOperation, t.ir.verify());
        owned[i] = good[i];
        owned[i].ty = range;
        try std.testing.expectError(error.OperandTypeMismatch, t.ir.verify());
        owned[i] = good[i];
    }
    owned[1].op.range_init.start = @fromBackingInt(@intCast(4));
    try std.testing.expectError(error.InvalidValueId, t.ir.verify());
    owned[1] = good[1];
    owned[0].ty = t.unsigned;
    const saved_signature = t.ir.functions.items[0].ty;
    t.ir.functions.items[0].ty = try t.ir.internType(.{ .function = .{ .params = &.{t.unsigned}, .result = t.integer } });
    try std.testing.expectError(error.OperandTypeMismatch, t.ir.verify());
    t.ir.functions.items[0].ty = saved_signature;
    owned[0] = good[0];
    owned[1].ty = t.integer;
    try std.testing.expectError(error.InvalidOperation, t.ir.verify());
    owned[1] = good[1];
    owned[1].span.end = 11;
    try std.testing.expectError(error.InvalidSourceSpan, t.ir.verify());
    owned[1] = good[1];
    try t.ir.verify();
}

test "iteration IR verifies cursor types limits visible values and nonescaping capabilities" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    ir.source_len = 100;
    const string = try ir.internType(.string);
    const rune = try ir.internType(.rune);
    const integer = try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = native_pointer_bits } });
    const unsigned = try ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = native_pointer_bits } });
    const unit = try ir.internType(.void);
    const signature = try ir.internType(.{ .function = .{ .params = &.{ string, integer }, .result = rune } });
    const function = try ir.addFunction("cursor", signature, .{ .start = 0, .end = 0 });
    const make = TestFunctions.instruction;
    const good = [_]Instruction{
        make(string, .{ .parameter = 0 }),
        make(integer, .{ .parameter = 1 }),
        make(rune, .{ .string_decode = .{ .array = @fromBackingInt(0), .index = @fromBackingInt(1) } }),
        make(integer, .{ .string_next = .{ .array = @fromBackingInt(0), .index = @fromBackingInt(1) } }),
        make(integer, .{ .iteration_len = @fromBackingInt(0) }),
        make(string, .{ .local_slot = @fromBackingInt(0) }),
        make(unit, .{ .borrow_reserve = @fromBackingInt(5) }),
        make(unit, .{ .borrow_start = .{ .reservation = @fromBackingInt(6), .index = null } }),
        make(string, .{ .borrow_read = @fromBackingInt(7) }),
        make(rune, .{ .string_decode = .{ .array = @fromBackingInt(8), .index = @fromBackingInt(1) } }),
    };
    try ir.setFunctionBody(function, &good, @fromBackingInt(9));
    const owned = @constCast(ir.getFunction(function).?.body.?.instructions);
    for ([_]usize{ 2, 3, 4 }) |i| {
        for ([_]TypeId{ string, unsigned, unit }) |bad_type| {
            owned[i].ty = bad_type;
            try std.testing.expectError(error.OperandTypeMismatch, ir.verify());
            owned[i] = good[i];
        }
        if (i == 4) {
            owned[i].op.iteration_len = @fromBackingInt(1);
            try std.testing.expectError(error.InvalidOperation, ir.verify());
            owned[i] = good[i];
        } else {
            owned[i].op = if (i == 2) .{ .string_decode = .{ .array = @fromBackingInt(1), .index = @fromBackingInt(1) } } else .{ .string_next = .{ .array = @fromBackingInt(1), .index = @fromBackingInt(1) } };
            try std.testing.expectError(error.InvalidOperation, ir.verify());
            owned[i] = good[i];
            owned[i].op = if (i == 2) .{ .string_decode = .{ .array = @fromBackingInt(0), .index = @fromBackingInt(0) } } else .{ .string_next = .{ .array = @fromBackingInt(0), .index = @fromBackingInt(0) } };
            try std.testing.expectError(error.OperandTypeMismatch, ir.verify());
            owned[i] = good[i];
        }
    }
    for ([_]u32{ 5, 6, 7, 9, 99 }) |bad| {
        owned[9].op.string_decode.array = @fromBackingInt(bad);
        try std.testing.expectError(if (bad == 6 or bad == 7) error.InvalidOperation else error.InvalidValueId, ir.verify());
        owned[9] = good[9];
        owned[9].op.string_decode.index = @fromBackingInt(bad);
        try std.testing.expectError(if (bad == 6 or bad == 7) error.OperandTypeMismatch else error.InvalidValueId, ir.verify());
        owned[9] = good[9];
    }
    try ir.verify();
}

test "range operands and projections cannot escape private branch regions" {
    var t = try TestFunctions.init(std.testing.allocator);
    defer t.ir.deinit();
    const range = try t.ir.internType(.{ .range = t.integer });
    const truth = try t.ir.internConstant(.{ .ty = t.boolean, .value = .{ .boolean = true } });
    const make = TestFunctions.instruction;
    const good = [_]Instruction{
        make(t.boolean, .{ .constant = truth }),
        make(t.integer, .{ .parameter = 0 }),
        make(range, .{ .range_init = .{ .start = @fromBackingInt(@intCast(1)), .end = @fromBackingInt(@intCast(1)), .inclusive = false } }),
        make(range, .{ .range_init = .{ .start = @fromBackingInt(@intCast(1)), .end = @fromBackingInt(@intCast(1)), .inclusive = true } }),
        make(range, .{ .branch = .{ .condition = @fromBackingInt(@intCast(0)), .then_start = 2, .else_start = 3, .then_value = @fromBackingInt(@intCast(2)), .else_value = @fromBackingInt(@intCast(3)) } }),
        make(t.integer, .{ .range_end = @fromBackingInt(@intCast(4)) }),
    };
    try t.ir.setFunctionBody(t.function, &good, @fromBackingInt(@intCast(5)));
    const owned = @constCast(t.ir.getFunction(t.function).?.body.?.instructions);
    for ([_]Operation{
        .{ .range_start = @fromBackingInt(@intCast(2)) },
        .{ .range_end = @fromBackingInt(@intCast(3)) },
        .{ .range_inclusive = @fromBackingInt(@intCast(2)) },
        .{ .range_init = .{ .start = @fromBackingInt(@intCast(2)), .end = @fromBackingInt(@intCast(1)), .inclusive = false } },
    }) |op| {
        owned[5] = make(if (op == .range_init) range else t.integer, op);
        try std.testing.expectError(error.InvalidValueId, t.ir.verify());
    }
    owned[5] = good[5];
    try t.ir.verify();
}
