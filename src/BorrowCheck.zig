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
