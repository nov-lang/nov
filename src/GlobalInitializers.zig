const std = @import("std");
const Ast = @import("Ast.zig");
const Module = @import("Module.zig");
const Sema = @import("Sema.zig");
const Ir = @import("Ir.zig");
const Dependencies = @import("Dependencies.zig");
const BodyChecker = @import("BodyChecker.zig");

const Ref = struct { module: *Module, id: Module.DeclId };
const Task = struct { ref: Ref, finish: bool = false, span: ?Module.Span = null };
const State = enum { unvisited, visiting, resolved, failed };

/// Check global data before any ordinary function body. The explicit worklist
/// also orders references across source boundaries without recursive AST walks.
pub fn analyze(root: *Module, sema: *Sema) Sema.Error!void {
    if (sema.compilation == null) {
        var dependencies = try Dependencies.analyze(root);
        defer dependencies.deinit();
        if (root.hasErrors()) return;
    }
    const count = sema.entries.len;
    const states = try sema.allocator.alloc(State, count);
    defer sema.allocator.free(states);
    @memset(states, .unvisited);
    var tasks: std.ArrayList(Task) = .empty;
    defer tasks.deinit(sema.allocator);
    var nodes: std.ArrayList(Ast.Node.Index) = .empty;
    defer nodes.deinit(sema.allocator);
    for (0..count) |index| {
        const ref: Ref = if (sema.compilation) |comp| blk: {
            const source_ref = comp.declarations.items[index];
            break :blk .{ .module = comp.getModule(source_ref.source).?, .id = source_ref.local };
        } else .{ .module = root, .id = @fromBackingInt(@intCast(index)) };
        if (!ordinary(sema, ref)) continue;
        try tasks.append(sema.allocator, .{ .ref = ref });
        while (tasks.pop()) |task| {
            const current = task.ref;
            const module = current.module;
            const id = sema.entryIndex(module, current.id);
            if (!ordinary(sema, current)) continue;
            const declaration = module.getDeclaration(current.id).?;
            const full = module.source.ast.decl(declaration.node).ast;
            if (task.finish) {
                if (states[id] == .failed) continue;
                if (if (sema.compilation) |comp| comp.hasErrors() else module.hasErrors()) {
                    sema.entries[id].status = .failed;
                    states[id] = .failed;
                    continue;
                }
                const diagnostics_start = module.diagnostics.items.len;
                const ty = try BodyChecker.checkGlobalInitializer(module, sema, current.id);
                try sema.syncEntries(module);
                if (ty) |value_type| {
                    if (Ir.isArrayElement(sema.ir.getType(value_type).?) and module.diagnostics.items.len == diagnostics_start) {
                        sema.entries[id].type_id = value_type;
                        sema.entries[id].status = .global_checked;
                        // Mutable literals are storage, never inline constants.
                        sema.entries[id].constant = null;
                        states[id] = .resolved;
                        try sema.global_order.append(sema.allocator, id);
                        continue;
                    }
                }
                if (module.diagnostics.items.len == diagnostics_start) try module.diagnostics.append(module.allocator, .{ .tag = .unsupported_global_initializer, .span = declaration.name_span });
                sema.entries[id].status = .failed;
                states[id] = .failed;
                continue;
            }
            switch (states[id]) {
                .resolved, .failed => continue,
                .visiting => {
                    try module.diagnostics.append(module.allocator, .{ .tag = .value_cycle, .span = task.span orelse declaration.name_span, .related = declaration.name_span });
                    sema.entries[id].status = .failed;
                    states[id] = .failed;
                    continue;
                },
                .unvisited => {},
            }
            states[id] = .visiting;
            try tasks.append(sema.allocator, .{ .ref = current, .finish = true });
            nodes.clearRetainingCapacity();
            if (full.init_node != 0) try nodes.append(sema.allocator, full.init_node);
            while (nodes.pop()) |node| {
                const ast = &module.source.ast;
                const tag = ast.nodes.items(.tag)[node];
                const data = ast.nodes.items(.data)[node];
                const reference: ?Ref = if (module.compilation) |comp| blk: {
                    const target = comp.reference(module, node) orelse break :blk null;
                    break :blk .{ .module = comp.getModule(target.source).?, .id = target.local };
                } else if (tag == .identifier) blk: {
                    const target = module.lookup(ast.tokenSlice(ast.firstToken(node))) orelse break :blk null;
                    break :blk .{ .module = module, .id = target };
                } else null;
                if (reference) |target| {
                    if (ordinary(sema, target)) try tasks.append(sema.allocator, .{ .ref = target, .span = module.tokenSpan(ast.firstToken(node)) });
                    continue;
                }
                switch (tag) {
                    .function_expr, .number_literal, .char_literal, .string_literal, .enum_literal, .identifier => {},
                    .struct_init, .container_decl_arg => for (ast.fullStructInit(node).?.fields) |field| {
                        const value = if (ast.nodes.items(.tag)[field] == .container_field) ast.nodes.items(.data)[field].rhs else field;
                        if (value != 0) try nodes.append(sema.allocator, value);
                    },
                    .array_init, .array_init_comma => try nodes.appendSlice(sema.allocator, ast.extra_data[data.lhs..data.rhs]),
                    .array_init_two, .array_init_two_comma => {
                        if (data.lhs != 0) try nodes.append(sema.allocator, data.lhs);
                        if (data.rhs != 0) try nodes.append(sema.allocator, data.rhs);
                    },
                    .call, .call_one, .call_comma, .call_one_comma => {
                        const args = ast.fullCall(node).?.ast.args;
                        try nodes.appendSlice(sema.allocator, args[@min(@as(usize, 1), args.len)..]);
                    },
                    .block, .block_two, .block_newline, .block_two_newline => {
                        var buffer: [2]Ast.Node.Index = undefined;
                        try nodes.appendSlice(sema.allocator, ast.fullBlock(&buffer, node).?.ast.statements);
                    },
                    .field_access => try nodes.append(sema.allocator, data.lhs),
                    .slice => {
                        const slice = ast.slice(node).ast;
                        try nodes.append(sema.allocator, slice.sliced);
                        if (slice.start != 0) try nodes.append(sema.allocator, slice.start);
                        if (slice.end != 0) try nodes.append(sema.allocator, slice.end);
                    },
                    .negation, .bool_not, .bit_not => try nodes.append(sema.allocator, data.rhs),
                    .add, .sub, .mul, .div, .mod, .equal_equal, .bang_equal, .less_than, .greater_than, .less_or_equal, .greater_or_equal, .shl, .shr, .bit_and, .bit_xor, .bit_or, .bool_and, .bool_or, .range, .array_access => {
                        if (data.lhs != 0) try nodes.append(sema.allocator, data.lhs);
                        if (data.rhs != 0) try nodes.append(sema.allocator, data.rhs);
                    },
                    // Node payloads also contain token and extra-data indices.
                    // Never interpret an unknown tag's payload as child nodes.
                    else => {
                        try module.diagnostics.append(module.allocator, .{ .tag = .unsupported_dependency_expression, .span = module.tokenSpan(ast.firstToken(node)) });
                        states[id] = .failed;
                        sema.entries[id].status = .failed;
                    },
                }
            }
        }
    }
}

fn ordinary(sema: *const Sema, ref: Ref) bool {
    const declaration = ref.module.getDeclaration(ref.id).?;
    if (declaration.container != null) return false;
    const full = ref.module.source.ast.decl(declaration.node).ast;
    const entry = sema.entries[sema.entryIndex(ref.module, ref.id)];
    return entry.status == .deferred or (entry.status == .constant_checked and full.is_mutable);
}

/// Allocate every identity before lowering initializers or ordinary bodies.
pub fn allocate(module: *Module, sema: *Sema, id: Module.DeclId) Sema.Error!void {
    const index = sema.entryIndex(module, id);
    const entry = sema.entries[index];
    if (entry.status != .global_checked) return;
    const declaration = module.getDeclaration(id).?;
    const signature = try sema.ir.internType(.{ .function = .{ .params = &.{}, .result = entry.type_id.? } });
    const name = try std.fmt.allocPrint(sema.allocator, "$global{d}", .{index});
    defer sema.allocator.free(name);
    const initializer = try sema.ir.addFunction(name, signature, declaration.name_span);
    _ = try sema.ir.addGlobal(declaration.name, entry.type_id.?, declaration.name_span, initializer);
}

pub fn lower(module: *Module, sema: *Sema, types: []const ?Ir.TypeId, constants: []const ?Ir.ConstId, functions: []const ?Ir.FunctionId, id: Module.DeclId) Sema.Error!bool {
    const declaration = module.getDeclaration(id).?;
    for (sema.ir.globals.items) |global| {
        if (global.span.source == declaration.name_span.source and global.span.start == declaration.name_span.start) {
            return BodyChecker.lowerGlobalInitializer(module, &sema.ir, types, constants, functions, id, global.initializer);
        }
    }
    return error.InvalidGlobal;
}

const Data = union(enum) {
    scalar: Ir.Constant.Value,
    function: Ir.FunctionId,
    aggregate: []const Data,
    tagged: struct { index: u32, payload: ?*const Data },
};

/// Startup may allocate/copy known data, but must not compute unknown values or
/// invoke user code. Fold scalar operations here, including through aggregate
/// projections and imported globals, before publishing executable IR.
pub fn validateData(root: *Module, sema: *Sema) Sema.Error!void {
    if (if (sema.compilation) |comp| comp.hasErrors() else root.hasErrors()) return;
    var arena = std.heap.ArenaAllocator.init(sema.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const globals = try allocator.alloc(?Data, sema.ir.globals.items.len);
    @memset(globals, null);
    for (sema.ir.globals.items, 0..) |global, global_index| {
        const body = sema.ir.getFunction(global.initializer).?.body.?;
        const values = try allocator.alloc(Data, body.instructions.len);
        var evaluator: Evaluator = .{ .allocator = allocator, .ir = &sema.ir, .globals = globals, .values = values, .instructions = body.instructions };
        var skip_until: usize = 0;
        for (@constCast(body.instructions), 0..) |*instruction, index| {
            if (index < skip_until) continue;
            for (body.instructions[index..], index..) |merge, end| {
                if (merge.op != .short_circuit or merge.op.short_circuit.rhs_start != index or end == index) continue;
                const short = merge.op.short_circuit;
                const lhs = values[@backingInt(short.lhs)];
                if (lhs == .scalar and lhs.scalar == .boolean and
                    (if (short.kind == .@"and") !lhs.scalar.boolean else lhs.scalar.boolean))
                {
                    skip_until = end;
                    break;
                }
            }
            if (index < skip_until) continue;
            const value = evaluator.evaluate(instruction.*) catch |err| switch (err) {
                error.InvalidData => {
                    const owner = if (sema.compilation) |comp| comp.getModule(instruction.span.source).? else root;
                    try owner.diagnostics.append(owner.allocator, .{ .tag = .unsupported_global_initializer, .span = instruction.span });
                    return;
                },
                error.OutOfMemory => return error.OutOfMemory,
            };
            values[index] = value;
            if (value == .scalar and instruction.op != .short_circuit) {
                // Interning validates exact integer widths as well as the host
                // evaluation width. Never leave a trapping operation at startup.
                const constant = sema.ir.internConstant(.{ .ty = instruction.ty, .value = value.scalar }) catch |err| switch (err) {
                    error.IntegerOutOfRange, error.FloatOutOfRange => {
                        const owner = if (sema.compilation) |comp| comp.getModule(instruction.span.source).? else root;
                        try owner.diagnostics.append(owner.allocator, .{ .tag = .literal_out_of_range, .span = instruction.span });
                        return;
                    },
                    else => return err,
                };
                instruction.op = .{ .constant = constant };
            }
        }
        globals[global_index] = values[@backingInt(body.result)];
    }
}

const Evaluator = struct {
    allocator: std.mem.Allocator,
    ir: *Ir,
    globals: []const ?Data,
    values: []const Data,
    instructions: []const Ir.Instruction,

    fn get(self: Evaluator, id: Ir.ValueId) Data {
        return self.values[@backingInt(id)];
    }

    fn scalar(self: Evaluator, id: Ir.ValueId) error{InvalidData}!Ir.Constant.Value {
        return switch (self.get(id)) {
            .scalar => |value| value,
            else => error.InvalidData,
        };
    }

    fn aggregate(self: Evaluator, ids: []const Ir.ValueId) std.mem.Allocator.Error!Data {
        const result = try self.allocator.alloc(Data, ids.len);
        for (result, ids) |*value, id| value.* = self.get(id);
        return .{ .aggregate = result };
    }

    fn evaluate(self: Evaluator, instruction: Ir.Instruction) (std.mem.Allocator.Error || error{InvalidData})!Data {
        const value: Ir.Constant.Value = switch (instruction.op) {
            .constant => |id| self.ir.getConstant(id).?.value,
            .function_ref => |id| return .{ .function = id },
            .closure_init => |closure| {
                if (closure.captures.len != 0) return error.InvalidData;
                return .{ .function = closure.function };
            },
            .global_slot => |id| return self.globals[@backingInt(id)] orelse error.InvalidData,
            .local_load, .array_clone => |id| return self.get(id),
            .record_init, .array_literal => |ids| return self.aggregate(ids),
            .field_read => |read| {
                const base = self.get(read.base);
                if (base != .aggregate or read.index >= base.aggregate.len) return error.InvalidData;
                return base.aggregate[read.index];
            },
            .union_init => |init| {
                const payload = if (init.payload) |id| blk: {
                    const data = try self.allocator.create(Data);
                    data.* = self.get(id);
                    break :blk data;
                } else null;
                return .{ .tagged = .{ .index = init.index, .payload = payload } };
            },
            .union_tag => |read| blk: {
                const base = self.get(read.base);
                if (base != .tagged) return error.InvalidData;
                break :blk .{ .boolean = base.tagged.index == read.index };
            },
            .union_payload => |read| {
                const base = self.get(read.base);
                if (base != .tagged or base.tagged.index != read.index) return error.InvalidData;
                return (base.tagged.payload orelse return error.InvalidData).*;
            },
            .range_init => |range| {
                const data = try self.allocator.alloc(Data, 3);
                data[0] = self.get(range.start);
                data[1] = self.get(range.end);
                data[2] = .{ .scalar = .{ .boolean = range.inclusive } };
                return .{ .aggregate = data };
            },
            .range_start, .range_end, .range_inclusive => |id| {
                const base = self.get(id);
                if (base != .aggregate or base.aggregate.len != 3) return error.InvalidData;
                return base.aggregate[
                    switch (instruction.op) {
                        .range_start => @as(usize, 0),
                        .range_end => 1,
                        else => 2,
                    }
                ];
            },
            .array_index => |read| {
                const base = self.get(read.array);
                const raw_index = try integer(try self.scalar(read.index));
                if (base == .aggregate) {
                    const index = try normalizeBound(raw_index, base.aggregate.len);
                    if (index == base.aggregate.len) return error.InvalidData;
                    return base.aggregate[index];
                }
                if (base != .scalar or base.scalar != .string) return error.InvalidData;
                const len = std.unicode.utf8CountCodepoints(base.scalar.string) catch return error.InvalidData;
                const index = try normalizeBound(raw_index, len);
                if (index == len) return error.InvalidData;
                var iterator = std.unicode.Utf8View.initUnchecked(base.scalar.string).iterator();
                var offset: i512 = 0;
                while (iterator.nextCodepoint()) |codepoint| : (offset += 1) {
                    if (offset == index) return .{ .scalar = .{ .rune = codepoint } };
                }
                return error.InvalidData;
            },
            .string_slice => |slice| blk: {
                const base = try self.scalar(slice.base);
                if (base != .string) return error.InvalidData;
                const len = std.unicode.utf8CountCodepoints(base.string) catch return error.InvalidData;
                const start = if (slice.start) |id| try normalizeBound(try integer(try self.scalar(id)), len) else 0;
                const end = if (slice.end) |id| try normalizeBound(try integer(try self.scalar(id)), len) else len;
                const inclusive = slice.end != null and slice.inclusive;
                if (inclusive and end == len) return error.InvalidData;
                const exclusive_end = end + @intFromBool(inclusive);
                if (exclusive_end < start) return error.InvalidData;
                var iterator = std.unicode.Utf8View.initUnchecked(base.string).iterator();
                var offset: i512 = 0;
                var first: usize = 0;
                var last: usize = 0;
                while (offset <= exclusive_end) : (offset += 1) {
                    if (offset == start) first = iterator.i;
                    if (offset == exclusive_end) {
                        last = iterator.i;
                        break;
                    }
                    _ = iterator.nextCodepoint() orelse return error.InvalidData;
                }
                break :blk .{ .string = base.string[first..last] };
            },
            .array_len => |id| blk: {
                const base = self.get(id);
                const len = if (base == .aggregate) base.aggregate.len else if (base == .scalar and base.scalar == .string)
                    std.unicode.utf8CountCodepoints(base.scalar.string) catch return error.InvalidData
                else
                    return error.InvalidData;
                break :blk try integerValue(self.ir.getType(instruction.ty).?.integer, len);
            },
            .unary => |unary| blk: {
                const operand = try self.scalar(unary.operand);
                if (unary.op == .bool_not) {
                    if (operand != .boolean) return error.InvalidData;
                    break :blk .{ .boolean = !operand.boolean };
                }
                if (operand == .float_bits and unary.op == .negate) {
                    const bits = self.ir.getType(instruction.ty).?.float;
                    break :blk .{ .float_bits = operand.float_bits ^ (@as(u64, 1) << @as(u6, @intCast(bits - 1))) };
                }
                const n = try integer(operand);
                const ty = self.ir.getType(instruction.ty).?.integer;
                const result = if (unary.op == .negate) -n else if (ty.signedness == .unsigned)
                    n ^ ((@as(i512, 1) << @as(u9, @intCast(ty.bits))) - 1)
                else
                    ~n;
                break :blk try integerValue(ty, result);
            },
            .binary => |binary| try self.evaluateBinary(instruction.ty, binary.op, try self.scalar(binary.lhs), try self.scalar(binary.rhs), self.ir.getType(self.instructions[@backingInt(binary.lhs)].ty).?),
            .short_circuit => |short| blk: {
                const lhs = try self.scalar(short.lhs);
                if (lhs != .boolean) return error.InvalidData;
                if (if (short.kind == .@"and") !lhs.boolean else lhs.boolean) break :blk lhs;
                const rhs = try self.scalar(short.rhs);
                if (rhs != .boolean) return error.InvalidData;
                break :blk .{ .boolean = if (short.kind == .@"and") lhs.boolean and rhs.boolean else lhs.boolean or rhs.boolean };
            },
            // An expression checker accepting an operation does not make that
            // operation legal in a data initializer. Keep this allowlist closed.
            else => return error.InvalidData,
        };
        return .{ .scalar = value };
    }

    // Use the wider evaluation integer so even the minimum native signed
    // index can be normalized without negation overflow.
    fn normalizeBound(bound: i512, len: usize) error{InvalidData}!usize {
        const normalized = if (bound < 0) bound + len else bound;
        if (normalized < 0 or normalized > len) return error.InvalidData;
        return @intCast(normalized);
    }

    fn integer(value: Ir.Constant.Value) error{InvalidData}!i512 {
        return switch (value) {
            .signed_integer => |n| n,
            .unsigned_integer => |n| n,
            .rune => |n| n,
            .enum_tag => |n| n,
            else => error.InvalidData,
        };
    }

    fn integerValue(ty: Ir.Type.Integer, n: i512) error{InvalidData}!Ir.Constant.Value {
        const bits: u9 = @intCast(ty.bits - @as(u16, if (ty.signedness == .signed) 1 else 0));
        const limit = @as(i512, 1) << bits;
        if (n >= limit or n < (if (ty.signedness == .signed) -limit else @as(i512, 0))) return error.InvalidData;
        return if (ty.signedness == .signed) .{ .signed_integer = @intCast(n) } else .{ .unsigned_integer = @intCast(n) };
    }

    fn evaluateBinary(self: Evaluator, result_type: Ir.TypeId, op: Ir.BinaryOp, lhs: Ir.Constant.Value, rhs: Ir.Constant.Value, operand_type: Ir.Type) (std.mem.Allocator.Error || error{InvalidData})!Ir.Constant.Value {
        if (lhs == .string and rhs == .string) {
            if (op == .add) return .{ .string = try std.mem.concat(self.allocator, u8, &.{ lhs.string, rhs.string }) };
            const order = std.mem.order(u8, lhs.string, rhs.string);
            return comparison(op, order == .eq, order == .lt);
        }
        if (lhs == .boolean and rhs == .boolean) return comparison(op, lhs.boolean == rhs.boolean, !lhs.boolean and rhs.boolean);
        if (lhs == .void and rhs == .void) return comparison(op, true, false);
        if (lhs == .float_bits and rhs == .float_bits) {
            const a: f64 = if (operand_type.float == 32) @as(f32, @bitCast(@as(u32, @intCast(lhs.float_bits)))) else @bitCast(lhs.float_bits);
            const b: f64 = if (operand_type.float == 32) @as(f32, @bitCast(@as(u32, @intCast(rhs.float_bits)))) else @bitCast(rhs.float_bits);
            if (self.ir.getType(result_type).? == .boolean) return .{ .boolean = switch (op) {
                .eq => a == b,
                .ne => a != b,
                .lt => a < b,
                .le => a <= b,
                .gt => a > b,
                .ge => a >= b,
                else => return error.InvalidData,
            } };
            if ((op == .div or op == .mod) and b == 0) return error.InvalidData;
            const n = switch (op) {
                .add => a + b,
                .sub => a - b,
                .mul => a * b,
                .div => a / b,
                .mod => @mod(a, b),
                else => return error.InvalidData,
            };
            if (!std.math.isFinite(n)) return error.InvalidData;
            if (operand_type.float == 32) {
                const narrow: f32 = @floatCast(n);
                if (!std.math.isFinite(narrow)) return error.InvalidData;
                return .{ .float_bits = @as(u32, @bitCast(narrow)) };
            }
            return .{ .float_bits = @bitCast(n) };
        }
        const a = try integer(lhs);
        const b = try integer(rhs);
        if (self.ir.getType(result_type).? == .boolean) return comparison(op, a == b, a < b);
        if ((op == .div or op == .mod) and b == 0) return error.InvalidData;
        const n = switch (op) {
            .add => a + b,
            .sub => a - b,
            .mul => a * b,
            .div => @divTrunc(a, b),
            .mod => @rem(a, b),
            .bit_and => a & b,
            .bit_or => a | b,
            .bit_xor => a ^ b,
            else => return error.InvalidData,
        };
        // The signed minimum divided by -1 must fail even for remainder.
        if ((op == .div or op == .mod) and b == -1) _ = try integerValue(operand_type.integer, -a);
        return integerValue(self.ir.getType(result_type).?.integer, n);
    }

    fn comparison(op: Ir.BinaryOp, equal: bool, less: bool) error{InvalidData}!Ir.Constant.Value {
        return .{ .boolean = switch (op) {
            .eq => equal,
            .ne => !equal,
            .lt => less,
            .le => less or equal,
            .gt => !less and !equal,
            .ge => !less,
            else => return error.InvalidData,
        } };
    }
};
