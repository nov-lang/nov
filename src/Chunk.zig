const std = @import("std");
const Value = @import("value.zig").Value;

const Chunk = @This();

code: std.ArrayListUnmanaged(u32),
lines: std.ArrayListUnmanaged(usize), // use RLE to save memory
constants: std.ArrayListUnmanaged(Value),
allocator: std.mem.Allocator,

// TODO: reduce the number of opcodes
// TODO: add higher-level opcodes
pub const OpCode = enum(u8) {
    constant,
    equal,
    greater,
    less,
    add,
    sub,
    mul,
    div,
    mod,
    not,
    negation,
    pop,
    get_local,
    set_local,
    get_global,
    set_global,
    define_global,
    print,
    jump,
    jump_if_false,
    loop,
    @"return",
};

pub fn init(allocator: std.mem.Allocator) Chunk {
    return .{
        .code = .{},
        .lines = .{},
        .constants = .{},
        .allocator = allocator,
    };
}

pub fn deinit(self: *Chunk) void {
    self.code.clearAndFree(self.allocator);
    self.lines.clearAndFree(self.allocator);
    self.constants.clearAndFree(self.allocator);
}

pub fn write(self: *Chunk, code: u32, line: usize) !void {
    try self.code.append(self.allocator, code);
    try self.lines.append(self.allocator, line);
}

pub fn addConstant(self: *Chunk, value: Value) !u24 {
    try self.constants.append(self.allocator, value);
    return @intCast(self.constants.items.len - 1);
}
