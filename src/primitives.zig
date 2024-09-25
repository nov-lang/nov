const std = @import("std");

pub const names = std.StaticStringMap(void).initComptime(.{
    .{"bool"},
    .{"int"},
    // .{"uint"},
    .{"float"},
    .{"string"},
    .{"false"},
    .{"true"},
    // .{"void"},
    // .{"null"},
    // .{"undefined"},
    // .{"anytype"},
});

pub fn isPrimitive(name: []const u8) bool {
    return names.has(name);
}
