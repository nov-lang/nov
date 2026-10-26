const std = @import("std");

pub fn checkAllAllocationFailures(
    backing_allocator: std.mem.Allocator,
    comptime test_fn: anytype,
    extra_args: anytype,
) !void {
    // SafeAllocator remaps can depend on bucket state. Force resize/remap
    // fallbacks through counted allocations so every sweep has the same sites.
    var fixed = std.testing.FailingAllocator.init(backing_allocator, .{ .resize_fail_index = 0 });
    try std.testing.checkAllAllocationFailures(fixed.allocator(), test_fn, extra_args);
}

test "allocation failure sweeps include growing and shrinking ownership transfers" {
    try checkAllAllocationFailures(std.testing.allocator, struct {
        fn run(allocator: std.mem.Allocator, value: u32) !void {
            var list: std.ArrayList(u32) = .empty;
            defer list.deinit(allocator);
            try list.ensureTotalCapacity(allocator, 64);
            try list.append(allocator, value);
            const owned = try list.toOwnedSlice(allocator);
            defer allocator.free(owned);
            try std.testing.expectEqualSlices(u32, &.{value}, owned);
        }
    }.run, .{@as(u32, 42)});
}
