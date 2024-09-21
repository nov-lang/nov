const std = @import("std");
const Value = @import("value.zig").Value;

const Chunk = @This();

code: std.ArrayListUnmanaged(u8),
lines: std.ArrayListUnmanaged(usize), // use std.MultiArrayList with code?
constants: std.ArrayListUnmanaged(Value),
allocator: std.mem.Allocator,

// TODO: reduce the number of opcodes
// TODO: add higher-level opcodes
pub const OpCode = enum(u8) {
    constant,
    equal,
    greater,
    less,
    @"or",
    add,
    sub,
    mul,
    div,
    not,
    negation,
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

pub fn write(self: *Chunk, byte: u8, line: usize) !void {
    try self.code.append(self.allocator, byte);
    try self.lines.append(self.allocator, line);
}

pub fn addConstant(self: *Chunk, value: Value) !usize {
    try self.constants.append(self.allocator, value);
    return self.constants.items.len - 1;
}
