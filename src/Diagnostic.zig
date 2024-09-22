const std = @import("std");

allocator: std.mem.Allocator,
panic_mode: bool = false,
last_error: ?Error = null,
error_prefix: ?[]const u8 = null,

pub const Error = enum {};
