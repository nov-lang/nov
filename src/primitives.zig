const std = @import("std");

/// Does not include arrays and maps
pub const names = std.StaticStringMap(void).initComptime(.{
    .{"bool"},
    // .{"c_int"},
    // .{"c_long"},
    // .{"c_longdouble"},
    // .{"c_longlong"},
    // .{"c_char"},
    // .{"c_short"},
    // .{"c_uint"},
    // .{"c_ulong"},
    // .{"c_ulonglong"},
    // .{"c_ushort"},
    .{"f16"},
    .{"f32"},
    .{"f64"},
    .{"f80"},
    .{"f128"},
    .{"float"},
    .{"false"},
    .{"i8"},
    .{"i16"},
    .{"i32"},
    .{"i64"},
    .{"i128"},
    .{"int"},
    .{"rune"}, // unicode code point, u21
    .{"string"},
    .{"true"},
    .{"type"},
    .{"u8"},
    .{"u16"},
    .{"u32"},
    .{"u64"},
    .{"u128"},
    .{"uint"},
    // .{"undefined"},
    .{"void"},
    .{"voidptr"},
});

pub fn isPrimitive(name: []const u8) bool {
    return names.has(name);
}
