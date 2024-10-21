const std = @import("std");

tag: Tag,
/// Info about the builtin call's ability to take advantage of a result location pointer.
needs_mem_loc: MemLocRequirement = .never,
/// `true` if the builtin call can be the left-hand side of an expression (assigned to).
allows_lvalue: bool = false,
/// `true` if builtin call is not available outside function scope
illegal_outside_function: bool = false,
/// The number of parameters to this builtin function. `null` means variable number
/// of parameters.
param_count: ?u8,

// TODO: missing a lot see README
pub const Tag = enum {
    import,
    print,
    println,
    max,
    min,
    type_info,
    type_of,
    @"unreachable",
};

pub const MemLocRequirement = enum {
    /// The builtin never needs a memory location.
    never,
    /// The builtin always needs a memory location.
    always,
    /// The builtin forwards the question to argument at index 0.
    forward0,
    /// The builtin forwards the question to argument at index 1.
    forward1,
};

pub const list = std.StaticStringMap(@This()).initComptime(.{
    .{
        "@import",
        .{
            .tag = .import,
            .param_count = 1,
        },
        "@print",
        .{
            .tag = .print,
            .param_count = null,
        },
        "@println",
        .{
            .tag = .println,
            .param_count = null,
        },
        "@max",
        .{
            .tag = .max,
            .param_count = null,
        },
        "@min",
        .{
            .tag = .min,
            .param_count = null,
        },
        "@typeInfo",
        .{
            .tag = .type_info,
            .param_count = 1,
        },
        "@TypeOf",
        .{
            .tag = .type_of,
            .param_count = 1,
        },
        "@unreachable",
        .{
            .tag = .@"unreachable",
            .param_count = 0,
        },
    },
});

comptime {
    std.debug.assert(@sizeOf(@This()) <= @sizeOf(u64));
}
