//! Opt-in C11 runtime text for managed scalar-array backing.
//! Scalar-only C emission stays unchanged.
pub const source = @embedFile("runtime/gc.h");

test "GC template is embedded without changing scalar emission" {
    const std = @import("std");
    const CEmitter = @import("CEmitter.zig");
    const Ir = @import("Ir.zig");
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const c = try CEmitter.emit(std.testing.allocator, &ir, null);
    defer std.testing.allocator.free(c);
    try std.testing.expect(std.mem.indexOf(u8, source, "nov_gc_collect") != null);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_gc_") == null);
}
