//! Fixed-point, instruction-CFG borrow validity. No source/token ordering is
//! used: regions supply edges, and stores are uses through RHS evaluation.
const std = @import("std");
const Ir = @import("Ir.zig");
const Module = @import("Module.zig");
const Lowering = @import("Lowering.zig");
const TestAllocations = @import("TestAllocations.zig");

pub fn check(allocator: std.mem.Allocator, body: Ir.Body) Ir.VerifyError!?Ir.BorrowConflict {
    return checkWithIr(allocator, body, null);
}

fn mayCall(body: Ir.Body, instruction: Ir.Instruction, ir: ?*const Ir) bool {
    if (instruction.op == .call) return true;
    const metadata = ir orelse return false;
    const operand: Ir.ValueId = switch (instruction.op) {
        .binary => |binary| if (!binary.enum_tag and (binary.op == .eq or binary.op == .ne)) binary.lhs else return false,
        .array_contains => |contains| contains.value,
        else => return false,
    };
    if (@backingInt(operand) >= body.instructions.len) return false;
    return metadata.equalityMayCall(body.instructions[@backingInt(operand)].ty);
}

pub fn checkWithIr(allocator: std.mem.Allocator, body: Ir.Body, ir: ?*const Ir) Ir.VerifyError!?Ir.BorrowConflict {
    var has_borrows = false;
    for (body.instructions) |instruction| if (instruction.op == .borrow_reserve or instruction.op == .reference_parameter) {
        has_borrows = true;
        break;
    };
    if (!has_borrows) return null;
    const n = body.instructions.len;
    const edges = try allocator.alloc([2]usize, n);
    defer allocator.free(edges);
    for (edges, 0..) |*edge, i| edge.* = .{ i + 1, n };
    // Outer regions first, then inner regions refine their own boundary edges.
    var end = n;
    while (end > 0) {
        end -= 1;
        switch (body.instructions[end].op) {
            .branch => |b| {
                edges[b.then_start - 1] = .{ if (b.then_start < b.else_start) b.then_start else end, b.else_start };
                if (b.then_start < b.else_start) edges[b.else_start - 1] = .{ end, n };
            },
            .short_circuit => |s| edges[s.rhs_start - 1] = .{ s.rhs_start, end },
            .loop => |l| {
                const control_end = l.else_start orelse end;
                edges[l.body_start - 1] = .{ l.body_start, if (l.unconditional) n else control_end };
                edges[(l.continuation_start orelse control_end) - 1] = .{ l.continuation_start orelse l.condition_start, n };
                if (l.continuation_start != null) edges[control_end - 1] = .{ l.condition_start, n };
            },
            else => {},
        }
    }
    for (body.instructions, 0..) |instruction, i| {
        switch (instruction.op) {
            .return_value, .panic => edges[i] = .{ n, n },

            .call => |c| if (c.noreturn) {
                edges[i] = .{ n, n };
            },
            .loop_break, .loop_continue => {
                const owner = Ir.enclosingLoop(body, i) orelse return error.InvalidRegion;
                const l = body.instructions[owner].op.loop;
                edges[i] = .{ if (instruction.op == .loop_break) owner else if (i < l.body_start or (l.continuation_start != null and i >= l.continuation_start.?)) l.condition_start else l.continuation_start orelse l.condition_start, n };
            },
            // Terminal merges are unreachable; their children carry the exits.
            .branch => |b| if (b.returns) {
                edges[i] = .{ n, n };
            },
            .loop => |l| if (l.returns) {
                edges[i] = .{ n, n };
            },
            // Assertions keep region-specific success edges. Failure exits,
            // so it adds no continuing-path liveness or successor.
            else => {},
        }
    }
    for (body.instructions) |instruction| if (instruction.op == .reference_slot or instruction.op == .reference_parameter or instruction.op == .reference_argument)
        return checkReferences(allocator, body, edges, ir);
    const live = try allocator.alloc(bool, n + 1);
    defer allocator.free(live);
    const active = try allocator.alloc(bool, n + 1);
    defer allocator.free(active);
    for (body.instructions, 0..) |reservation, origin| {
        if (reservation.op != .borrow_reserve) continue;
        @memset(live, false);
        @memset(active, false);
        // Backwards may-liveness, with a fresh declaration killing the previous
        // iteration's capability. Backedges converge without a lexical cutoff.
        var changed = true;
        while (changed) {
            changed = false;
            var i = n;
            while (i > 0) {
                i -= 1;
                if (i == origin) continue;
                const op = body.instructions[i].op;
                const use = switch (op) {
                    .borrow_start => |b| @backingInt(b.reservation) == origin,
                    .borrow_read => |b| @backingInt(body.instructions[@backingInt(b)].op.borrow_start.reservation) == origin,
                    .borrow_store => |b| @backingInt(body.instructions[@backingInt(b.borrow)].op.borrow_start.reservation) == origin,
                    .array_clear => |c| switch (c.target) {
                        .borrow => |b| @backingInt(body.instructions[@backingInt(b)].op.borrow_start.reservation) == origin,
                        .slot => false,
                    },
                    else => false,
                };
                if (!live[i] and (use or live[edges[i][0]] or live[edges[i][1]])) {
                    live[i] = true;
                    changed = true;
                }
            }
        }
        // Forward reachability from the reservation avoids treating accesses
        // before activation (or in mutually exclusive siblings) as conflicts.
        for (edges[origin]) |next| active[next] = true;
        changed = true;
        while (changed) {
            changed = false;
            for (edges, 0..) |edge, i| {
                if (!active[i] or i == origin) continue;
                for (edge) |next| if (!active[next]) {
                    active[next] = true;
                    changed = true;
                };
            }
        }
        for (body.instructions, 0..) |instruction, i| {
            if (i == origin or !active[i] or !live[i]) continue;
            const owner = reservation.op.borrow_reserve;
            // Environments hide cell accesses from the caller's ordinary operands.
            // Until effect summaries exist, a call may touch any captured owner.
            if (mayCall(body, instruction, ir) and body.instructions[@backingInt(owner)].boxed)
                return .{ .access = instruction.span, .origin = reservation.span };
            if (instruction.op == .closure_init) {
                for (instruction.op.closure_init.captures) |capture| if (capture == owner)
                    return .{ .access = instruction.span, .origin = reservation.span };
            }
            const slot: ?Ir.ValueId = switch (instruction.op) {
                .local_load, .borrow_reserve => |s| s,
                .local_store => |s| s.slot,
                .field_store => |s| s.slot,
                .array_clear => |c| switch (c.target) {
                    .slot => |s| s,
                    .borrow => null,
                },
                else => null,
            };
            if (slot != null and slot.? == reservation.op.borrow_reserve)
                return .{ .access = instruction.span, .origin = reservation.span };
        }
    }
    return null;
}

fn testEqualityHookLoans(allocator: std.mem.Allocator) !void {
    var ir = Ir.init(allocator);
    defer ir.deinit();
    const span: Ir.Span = .{ .start = 0, .end = 0 };
    const boolean = try ir.internType(.boolean);
    const integer = try ir.internType(.{ .integer = .{ .bits = 32, .signedness = .signed } });
    const record = try ir.internType(.{ .record = .{ .identity = 0, .fields = &.{} } });
    const array = try ir.internType(.{ .array = record });
    const nested = try ir.internType(.{ .record = .{ .identity = 1, .tagged = true, .fields = &.{} } });
    try ir.setRecordFields(nested, &.{
        .{ .name = "items", .ty = array, .span = span },
        .{ .name = "other", .ty = integer, .span = span },
    });
    const plain = try ir.internType(.{ .record = .{ .identity = 2, .fields = &.{} } });
    const integers = try ir.internType(.{ .array = integer });
    const enumeration = try ir.internType(.{ .enumeration = .{ .identity = 0, .name = "E", .tags = &.{.{ .name = "a", .span = span }} } });
    const enum_array = try ir.internType(.{ .array = enumeration });
    const sig = try ir.internType(.{ .function = .{ .params = &.{ record, record }, .result = boolean } });
    const hook = try ir.addFunction("equal", sig, span);
    const enum_sig = try ir.internType(.{ .function = .{ .params = &.{ enumeration, enumeration }, .result = boolean } });
    const enum_hook = try ir.addFunction("equalenum", enum_sig, span);
    for ([_]bool{ false, true }) |hooked| {
        try ir.setTypeEquality(record, if (hooked) hook else null);
        try ir.setTypeEquality(enumeration, if (hooked) enum_hook else null);
        for ([_]Ir.TypeId{ record, array, nested, plain, integer, integers, enumeration, enum_array }) |ty| {
            const calls = hooked and ty != plain and ty != integer and ty != integers;
            try std.testing.expectEqual(calls, ir.equalityMayCall(ty));
            for ([_]bool{ false, true }) |references| {
                for ([_]bool{ false, true }) |global| {
                    for ([_]Ir.Operation{
                        .{ .binary = .{ .op = .eq, .lhs = @fromBackingInt(0), .rhs = @fromBackingInt(0) } },
                        .{ .binary = .{ .op = .ne, .lhs = @fromBackingInt(0), .rhs = @fromBackingInt(0) } },
                        .{ .array_contains = .{ .value = @fromBackingInt(0), .array = @fromBackingInt(0) } },
                        .{ .binary = .{ .op = .eq, .lhs = @fromBackingInt(0), .rhs = @fromBackingInt(0), .enum_tag = true } },
                        .{ .binary = .{ .op = .ne, .lhs = @fromBackingInt(0), .rhs = @fromBackingInt(0), .enum_tag = true } },
                    }) |op| {
                        const raw = op == .binary and op.binary.enum_tag;
                        if (raw and ty != enumeration) continue;
                        const call_effect = calls and !raw;
                        const constant: Ir.Operation = .{ .constant = @fromBackingInt(0) };
                        var code = [_]Ir.Instruction{
                            .{ .ty = ty, .span = span, .op = constant },
                            .{ .ty = ty, .span = span, .op = if (global) .{ .global_slot = @fromBackingInt(0) } else .{ .local_slot = @fromBackingInt(0) }, .boxed = true },
                            .{ .ty = ty, .span = .{ .start = 2, .end = 3 }, .op = .{ .borrow_reserve = @fromBackingInt(1) } },
                            .{ .ty = ty, .span = span, .op = .{ .borrow_start = .{ .reservation = @fromBackingInt(2), .index = @fromBackingInt(0) } } },
                            .{ .ty = ty, .span = span, .op = if (references) .{ .reference_slot = @fromBackingInt(3) } else constant },
                            .{ .ty = boolean, .span = .{ .start = 5, .end = 6 }, .op = op },
                            .{ .ty = ty, .span = span, .op = if (references) .{ .reference_load = @fromBackingInt(4) } else constant },
                            .{ .ty = ty, .span = span, .op = .{ .borrow_read = @fromBackingInt(if (references) 6 else 3) } },
                        };
                        const body: Ir.Body = .{ .instructions = &code, .result = @fromBackingInt(7) };
                        try std.testing.expect(try check(allocator, body) == null);
                        const conflict = try checkWithIr(allocator, body, &ir);
                        try std.testing.expectEqual(call_effect, conflict != null);
                        if (conflict) |found| {
                            try std.testing.expectEqual(code[5].span, found.access);
                            try std.testing.expectEqual(code[2].span, found.origin);
                        }
                        // Globals become boxed in Ir.borrowConflict; locals do not.
                        code[1].boxed = false;
                        try std.testing.expect(try checkWithIr(allocator, body, &ir) == null);
                        try std.testing.expectEqual(call_effect and global, try ir.borrowConflict(body) != null);
                        code[1].boxed = true;
                        code[7].op = constant;
                        code[6].op = constant;
                        try std.testing.expect(try ir.borrowConflict(body) == null);
                    }
                }
            }
        }
    }
    ir.clearFunctions();
    try std.testing.expect(!ir.equalityMayCall(nested));
    try std.testing.expect(!ir.equalityMayCall(@fromBackingInt(999)));
    // A forged cycle must terminate without hiding a reachable hook.
    const fields = @constCast(ir.types.items[@backingInt(nested)].record.fields);
    fields[0].ty = nested;
    try std.testing.expect(!ir.equalityMayCall(nested));
    fields[1].ty = array;
    const restored_hook = try ir.addFunction("restored", sig, span);
    try ir.setTypeEquality(record, restored_hook);
    try std.testing.expect(ir.equalityMayCall(nested));
}

test "equality hooks protect boxed owner loans in both CFG paths globals and every OOM" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testEqualityHookLoans, .{});
}

test "equality hooks reject captured owner replacement through explicit and iteration loans" {
    const declaration = "let S=struct{f:()->bool,@[operator(.equal)]let eq:(a:S,b:S)->bool=a.f()}\n";
    for ([_][]const u8{
        "loop &x in xs {@assert(f())\n@println(x.*)}",
        "loop &x in xs {@assert(a==a)\n@println(x.*)}",
        "loop &x in xs {@assert(a!=a)\n@println(x.*)}",
        "loop &x in xs {@assert([a]==[a])\n@println(x.*)}",
        "let choices=[a]\nloop &x in xs {@assert(a in choices)\n@println(x.*)}",
        "let p=&xs[0]\n@assert(a==a)\n@println(p.*)",
        "let choices=[a]\nlet p=&xs[0]\n@assert(a in choices)\n@println(p.*)",
    }) |operation| {
        const source = try std.mem.concat(std.testing.allocator, u8, &.{ declaration, "let main:()={let mut xs=[1,2]\nlet f=()->bool{xs=[9]\ntrue}\nlet a=S.[f=f]\n", operation, "}" });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "equality-loans.nov", source);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        defer result.deinit();
        try std.testing.expect(module.hasErrors());
        try std.testing.expectEqual(Module.Diagnostic.Tag.borrow_conflict, module.diagnostics.items[0].tag);
        try std.testing.expect(!result.lowering_complete);
    }
}

test "equality without hooks preserves boxed loans and hooks respect last uses" {
    for ([_][]const u8{
        "let S=struct{f:()->bool,@[operator(.equal)]let eq:(a:S,b:S)->bool=a.f()}\n" ++
            "let main:()={let mut xs=[1,2]\nlet f=()->bool{xs=[9]\ntrue}\nlet a=S.[f=f]\n" ++
            "loop &x in xs {@assert(1==1)\n@assert(1!=2)\n@assert(1 in [1,2])\n@assert([1,2]==[1,2])\n@assert(.[n:1]==.[n:1])\n@assert([1] in [[1],[2]])\n@println(x.*)}\n" ++
            "let p=&xs[0]\n@println(p.*)\n@assert(a==a)}",
        "let S=struct{f:()->bool,@[operator(.equal)]let eq:(a:S,b:S)->bool=a.f()}\n" ++
            "let main:()={let mut xs=[1,2]\nlet f=()->bool{true}\nlet a=S.[f=f]\n" ++
            "loop &x in xs {@assert(a==a)\n@println(x.*)}}",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "equality-safe-loans.nov", source);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        defer result.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(result.lowering_complete);
        try result.ir.verify();
    }
}

fn testIterationContinuationLoans(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "continuation-loans.nov",
        \\let f:()={let mut items=[1,2]
        \\let mut other=0
        \\let p=&other
        \\loop &item in items : {item.* +=p.*
        \\    continue} {item.* +=1
        \\    continue}
        \\@println(p.*)
        \\items+=[3]}
    );
    defer module.deinit();
    var result = try Lowering.lower(&module);
    defer result.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(result.lowering_complete);
    try result.ir.verify();
    var regions: usize = 0;
    for (result.ir.functions.items) |function| {
        const body = function.body orelse continue;
        try std.testing.expectEqual(@as(?Ir.BorrowConflict, null), try check(allocator, body));
        for (body.instructions, 0..) |instruction, i| {
            if (instruction.op != .loop) continue;
            const loop = instruction.op.loop;
            const continuation = loop.continuation_start.?;
            var body_loans: usize = 0;
            var continuation_loans: usize = 0;
            for (body.instructions[loop.body_start..i], loop.body_start..) |operation, index| {
                if (operation.op != .borrow_start) continue;
                if (index < continuation) body_loans += 1 else continuation_loans += 1;
            }
            try std.testing.expectEqual(@as(usize, 1), body_loans);
            try std.testing.expectEqual(@as(usize, 1), continuation_loans);
            regions += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 1), regions);
}

test "iteration continuation loans follow body and continuation continue edges under OOM" {
    try testIterationContinuationLoans(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testIterationContinuationLoans, .{});
}

fn testClearLoans(allocator: std.mem.Allocator) !void {
    // Exercise both the raw-loan fast path and reference-slot dataflow. The
    // clear through the capability is its final use, not an owner access.
    for ([_]bool{ false, true }) |references| {
        const ty: Ir.TypeId = @fromBackingInt(0);
        const span: Ir.Span = .{ .source = @fromBackingInt(0), .start = 0, .end = 1 };
        const constant: Ir.Operation = .{ .constant = @fromBackingInt(0) };
        var instructions = [_]Ir.Instruction{
            .{ .ty = ty, .span = span, .op = constant },
            .{ .ty = ty, .span = span, .op = .{ .local_slot = @fromBackingInt(0) } },
            .{ .ty = ty, .span = .{ .source = span.source, .start = 2, .end = 3 }, .op = .{ .borrow_reserve = @fromBackingInt(1) } },
            .{ .ty = ty, .span = span, .op = .{ .borrow_start = .{ .reservation = @fromBackingInt(2), .index = @fromBackingInt(0) } } },
            .{ .ty = ty, .span = span, .op = if (references) .{ .reference_slot = @fromBackingInt(3) } else constant },
            .{ .ty = ty, .span = .{ .source = span.source, .start = 5, .end = 6 }, .op = .{ .array_clear = .{ .target = .{ .slot = @fromBackingInt(1) } } } },
            .{ .ty = ty, .span = span, .op = if (references) .{ .reference_load = @fromBackingInt(4) } else constant },
            .{ .ty = ty, .span = span, .op = .{ .array_clear = .{ .target = .{ .borrow = @fromBackingInt(if (references) 6 else 3) } } } },
            .{ .ty = ty, .span = span, .op = .{ .array_clear = .{ .target = .{ .slot = @fromBackingInt(1) } } } },
        };
        const body: Ir.Body = .{ .instructions = &instructions, .result = @fromBackingInt(8) };
        const conflict = (try check(allocator, body)).?;
        try std.testing.expectEqual(instructions[5].span, conflict.access);
        try std.testing.expectEqual(instructions[2].span, conflict.origin);
        instructions[5].op = constant;
        try std.testing.expectEqual(@as(?Ir.BorrowConflict, null), try check(allocator, body));
    }
}

fn testClearArgument(allocator: std.mem.Allocator) !void {
    const ty: Ir.TypeId = @fromBackingInt(0);
    const span: Ir.Span = .{ .source = @fromBackingInt(0), .start = 0, .end = 1 };
    var instructions = [_]Ir.Instruction{
        .{ .ty = ty, .span = span, .op = .{ .reference_parameter = .{ .index = 0, .ty = ty } } },
        .{ .ty = ty, .span = .{ .source = span.source, .start = 1, .end = 2 }, .op = .{ .reference_argument = .{ .borrow = @fromBackingInt(0), .ty = ty } } },
        .{ .ty = ty, .span = .{ .source = span.source, .start = 2, .end = 3 }, .op = .{ .array_clear = .{ .target = .{ .borrow = @fromBackingInt(0) } } } },
        .{ .ty = ty, .span = span, .op = .{ .call = .{ .callee = @fromBackingInt(0), .args = &.{@fromBackingInt(1)} } } },
    };
    const body: Ir.Body = .{ .instructions = &instructions, .result = @fromBackingInt(3) };
    const conflict = (try check(allocator, body)).?;
    try std.testing.expectEqual(instructions[2].span, conflict.access);
    try std.testing.expectEqual(instructions[1].span, conflict.origin);
    instructions[2].op = .{ .constant = @fromBackingInt(0) };
    try std.testing.expectEqual(@as(?Ir.BorrowConflict, null), try check(allocator, body));
}

test "clear protects pending external reference argument snapshots under OOM" {
    try testClearArgument(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testClearArgument, .{});
}

test "clear keeps capabilities live and conflicts with live element loans in both paths under OOM" {
    try testClearLoans(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testClearLoans, .{});
}

test "clear rejects live element loans and pending reference argument snapshots" {
    for ([_][]const u8{
        "let f:()={let mut a=[1]\nlet p=&a[0]\n@clear(a)\n@println(p.*)}",
        "let consume:(p:&mut []int,n:int)={}\nlet f:(p:&mut []int)={consume(p,{@clear(p.*)\n0})}",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "clear-conflict.nov", source);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        defer result.deinit();
        try std.testing.expect(module.hasErrors());
        try std.testing.expectEqual(Module.Diagnostic.Tag.borrow_conflict, module.diagnostics.items[0].tag);
        try std.testing.expect(!result.lowering_complete);
    }
}

const Bits = struct {
    words: []u64,

    fn has(self: Bits, bit: usize) bool {
        return self.words[bit / 64] & (@as(u64, 1) << @intCast(bit % 64)) != 0;
    }

    fn set(self: Bits, bit: usize) void {
        self.words[bit / 64] |= @as(u64, 1) << @intCast(bit % 64);
    }

    fn unset(self: Bits, bit: usize) void {
        self.words[bit / 64] &= ~(@as(u64, 1) << @intCast(bit % 64));
    }

    fn merge(self: Bits, other: Bits) bool {
        var changed = false;
        for (self.words, other.words) |*word, source| {
            const next = word.* | source;
            changed = changed or next != word.*;
            word.* = next;
        }
        return changed;
    }
};

const BitRows = struct {
    data: []u64,
    width: usize,

    fn init(allocator: std.mem.Allocator, rows: usize, bits: usize) !BitRows {
        const width = (std.math.add(usize, bits, 63) catch return error.OutOfMemory) / 64;
        const size = std.math.mul(usize, rows, width) catch return error.OutOfMemory;
        const data = try allocator.alloc(u64, size);
        @memset(data, 0);
        return .{ .data = data, .width = width };
    }

    fn row(self: BitRows, index: usize) Bits {
        return .{ .words = self.data[index * self.width ..][0..self.width] };
    }
};

// Forward reaching targets and backward SSA/slot liveness are separate: pushing
// a snapshot's union of origins backwards through a rebind loses path precision.
fn checkReferences(allocator: std.mem.Allocator, body: Ir.Body, edges: []const [2]usize, ir: ?*const Ir) Ir.VerifyError!?Ir.BorrowConflict {
    const n = body.instructions.len;
    const slot_ids = try allocator.alloc(usize, n);
    defer allocator.free(slot_ids);
    const origin_ids = try allocator.alloc(usize, n);
    defer allocator.free(origin_ids);
    @memset(slot_ids, n);
    @memset(origin_ids, n);
    var slot_count: usize = 0;
    var origin_count: usize = 0;
    for (body.instructions, 0..) |instruction, i| switch (instruction.op) {
        .reference_slot => {
            slot_ids[i] = slot_count;
            slot_count += 1;
        },
        .borrow_reserve, .reference_parameter => {
            origin_ids[i] = origin_count;
            origin_count += 1;
        },
        else => {},
    };
    const reach_rows = std.math.mul(usize, n, slot_count) catch return error.OutOfMemory;
    const reaching = try BitRows.init(allocator, reach_rows, origin_count);
    defer allocator.free(reaching.data);
    const origins = try BitRows.init(allocator, n, origin_count);
    defer allocator.free(origins.data);
    for (body.instructions, 0..) |instruction, i| switch (instruction.op) {
        .borrow_reserve, .reference_parameter => origins.row(i).set(origin_ids[i]),
        .borrow_start => |b| origins.row(i).set(origin_ids[@backingInt(b.reservation)]),
        else => {},
    };
    const reachable = try allocator.alloc(bool, n);
    defer allocator.free(reachable);
    @memset(reachable, false);
    reachable[0] = true;
    var changed = true;
    while (changed) {
        changed = false;
        for (body.instructions, 0..) |instruction, i| {
            if (!reachable[i]) continue;
            const definition: ?struct { slot: usize, borrow: Ir.ValueId } = switch (instruction.op) {
                .reference_slot => |b| .{ .slot = slot_ids[i], .borrow = b },
                .reference_rebind => |b| .{ .slot = slot_ids[@backingInt(b.slot)], .borrow = b.borrow },
                else => null,
            };
            if (instruction.op == .reference_argument) {
                changed = origins.row(i).merge(origins.row(@backingInt(instruction.op.reference_argument.borrow))) or changed;
            }
            if (instruction.op == .reference_load) {
                const slot = slot_ids[@backingInt(instruction.op.reference_load)];
                changed = origins.row(i).merge(reaching.row(i * slot_count + slot)) or changed;
            }
            for (edges[i]) |next| {
                if (next == n) continue;
                if (!reachable[next]) {
                    reachable[next] = true;
                    changed = true;
                }
                for (0..slot_count) |slot| {
                    const source = if (definition != null and definition.?.slot == slot)
                        origins.row(@backingInt(definition.?.borrow))
                    else
                        reaching.row(i * slot_count + slot);
                    changed = reaching.row(next * slot_count + slot).merge(source) or changed;
                }
            }
        }
    }

    // Value IDs share a bitset: slots are mutable definitions, other capability
    // IDs are SSA definitions. Both kill at definition, including on backedges.
    const live = try BitRows.init(allocator, n + 1, n);
    defer allocator.free(live.data);
    const scratch = try BitRows.init(allocator, 1, n);
    defer allocator.free(scratch.data);
    changed = true;
    while (changed) {
        changed = false;
        var i = n;
        while (i > 0) {
            i -= 1;
            if (!reachable[i]) continue;
            const bits = scratch.row(0);
            @memset(bits.words, 0);
            for (edges[i]) |next| _ = bits.merge(live.row(next));
            bits.unset(i);
            switch (body.instructions[i].op) {
                .borrow_start => |b| bits.set(@backingInt(b.reservation)),
                .reference_slot => |b| bits.set(@backingInt(b)),
                .reference_rebind => |b| {
                    bits.unset(@backingInt(b.slot));
                    bits.set(@backingInt(b.borrow));
                },
                .reference_load => |slot| bits.set(@backingInt(slot)),
                .borrow_read => |b| bits.set(@backingInt(b)),
                .borrow_store => |b| bits.set(@backingInt(b.borrow)),
                .array_clear => |c| switch (c.target) {
                    .borrow => |b| bits.set(@backingInt(b)),
                    .slot => {},
                },
                .reference_argument => |b| bits.set(@backingInt(b.borrow)),
                .call => |call| for (call.args) |arg| {
                    if (body.instructions[@backingInt(arg)].op == .reference_argument) bits.set(@backingInt(arg));
                },
                else => {},
            }
            changed = live.row(i).merge(bits) or changed;
        }
    }

    // An evaluated reference argument is an exclusive snapshot until its
    // call consumes it. This also protects external parameter origins, which
    // have no local owner slot for the owner-access pass below.
    for (body.instructions, 0..) |instruction, i| {
        if (!reachable[i]) continue;
        const accessed = switch (instruction.op) {
            .borrow_read => |borrow| borrow,
            .borrow_store => |store| store.borrow,
            .array_clear => |c| switch (c.target) {
                .borrow => |b| b,
                .slot => continue,
            },
            .reference_argument => |arg| arg.borrow,
            else => continue,
        };
        for (body.instructions, 0..) |pending, p| {
            if (pending.op != .reference_argument or !live.row(i).has(p)) continue;
            const left = origins.row(@backingInt(accessed));
            const right = origins.row(p);
            for (left.words, right.words) |l, r| {
                if (l & r != 0) return .{ .access = instruction.span, .origin = pending.span };
            }
        }
    }

    // Formal parameters have distinct external origins. Forwarding the same
    // parameter or overlapping dynamic snapshots twice still aliases.
    for (body.instructions, 0..) |instruction, i| {
        if (!reachable[i] or instruction.op != .call) continue;
        const args = instruction.op.call.args;
        for (args, 0..) |arg, a| {
            if (body.instructions[@backingInt(arg)].op != .reference_argument) continue;
            const left = origins.row(@backingInt(arg));
            for (args[0..a]) |other| {
                if (body.instructions[@backingInt(other)].op != .reference_argument) continue;
                const right = origins.row(@backingInt(other));
                for (left.words, right.words) |l, r| {
                    if (l & r != 0) return .{ .access = instruction.span, .origin = body.instructions[@backingInt(other)].span };
                }
            }
        }
    }

    const active = try allocator.alloc(bool, n + 1);
    defer allocator.free(active);
    for (body.instructions, 0..) |reservation, origin| {
        if (reservation.op != .borrow_reserve) continue;
        @memset(active, false);
        for (edges[origin]) |next| active[next] = true;
        changed = true;
        while (changed) {
            changed = false;
            for (edges, 0..) |edge, i| {
                if (!active[i] or i == origin) continue;
                for (edge) |next| if (!active[next]) {
                    active[next] = true;
                    changed = true;
                };
            }
        }
        const loan = origin_ids[origin];
        const owner = reservation.op.borrow_reserve;
        for (body.instructions, 0..) |instruction, i| {
            if (i == origin or !reachable[i] or !active[i]) continue;
            const access: ?Ir.ValueId = switch (instruction.op) {
                .local_load, .borrow_reserve => |s| s,
                .local_store => |s| s.slot,
                .field_store => |s| s.slot,
                .array_clear => |c| switch (c.target) {
                    .slot => |s| s,
                    .borrow => null,
                },
                else => null,
            };
            var touches = access != null and access.? == owner;
            if (mayCall(body, instruction, ir) and body.instructions[@backingInt(owner)].boxed) touches = true;
            if (instruction.op == .closure_init) {
                for (instruction.op.closure_init.captures) |capture| {
                    if (capture == owner) touches = true;
                }
            }
            if (!touches) continue;
            for (body.instructions, 0..) |capability, cap| {
                if (!live.row(i).has(cap)) continue;
                const targets = if (capability.op == .reference_slot)
                    reaching.row(i * slot_count + slot_ids[cap])
                else
                    origins.row(cap);
                if (targets.has(loan)) return .{ .access = instruction.span, .origin = reservation.span };
            }
        }
    }
    return null;
}
