//! Fixed-point, instruction-CFG borrow validity. No source/token ordering is
//! used: regions supply edges, and stores are uses through RHS evaluation.
const std = @import("std");
const Ir = @import("Ir.zig");

pub fn check(allocator: std.mem.Allocator, body: Ir.Body) Ir.VerifyError!?Ir.BorrowConflict {
    var has_borrows = false;
    for (body.instructions) |instruction| if (instruction.op == .borrow_reserve) {
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
    for (body.instructions) |instruction| if (instruction.op == .reference_slot)
        return checkReferences(allocator, body, edges);
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
            if (instruction.op == .call and body.instructions[@backingInt(owner)].boxed)
                return .{ .access = instruction.span, .origin = reservation.span };
            if (instruction.op == .closure_init) {
                for (instruction.op.closure_init.captures) |capture| if (capture == owner)
                    return .{ .access = instruction.span, .origin = reservation.span };
            }
            const slot: ?Ir.ValueId = switch (instruction.op) {
                .local_load, .borrow_reserve => |s| s,
                .local_store => |s| s.slot,
                .field_store => |s| s.slot,
                else => null,
            };
            if (slot != null and slot.? == reservation.op.borrow_reserve)
                return .{ .access = instruction.span, .origin = reservation.span };
        }
    }
    return null;
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
fn checkReferences(allocator: std.mem.Allocator, body: Ir.Body, edges: []const [2]usize) Ir.VerifyError!?Ir.BorrowConflict {
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
        .borrow_reserve => {
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
        .borrow_reserve => origins.row(i).set(origin_ids[i]),
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
                else => {},
            }
            changed = live.row(i).merge(bits) or changed;
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
                else => null,
            };
            var touches = access != null and access.? == owner;
            if (instruction.op == .call and body.instructions[@backingInt(owner)].boxed) touches = true;
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
