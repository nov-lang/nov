const std = @import("std");
const Chunk = @import("Chunk.zig");
pub const print = std.debug.print;

pub fn disassembleChunk(chunk: *const Chunk, name: []const u8) void {
    print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
}

pub fn disassembleInstruction(chunk: *const Chunk, offset: usize) usize {
    print("{d:0>4} ", .{offset});
    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        print("   | ", .{});
    } else {
        print("{d:0>4} ", .{chunk.lines.items[offset]});
    }

    const op = chunk.code.items[offset];
    switch (op) {
        @intFromEnum(Chunk.OpCode.constant) => return constantInstruction("OP_CONSTANT", chunk, offset),
        @intFromEnum(Chunk.OpCode.equal) => return simpleInstruction("OP_EQUAL", offset),
        @intFromEnum(Chunk.OpCode.greater) => return simpleInstruction("OP_GREATER", offset),
        @intFromEnum(Chunk.OpCode.less) => return simpleInstruction("OP_LESS", offset),
        @intFromEnum(Chunk.OpCode.@"or") => return simpleInstruction("OP_OR", offset),
        @intFromEnum(Chunk.OpCode.add) => return simpleInstruction("OP_ADD", offset),
        @intFromEnum(Chunk.OpCode.sub) => return simpleInstruction("OP_SUBTRACT", offset),
        @intFromEnum(Chunk.OpCode.mul) => return simpleInstruction("OP_MULTIPLY", offset),
        @intFromEnum(Chunk.OpCode.div) => return simpleInstruction("OP_DIVIDE", offset),
        @intFromEnum(Chunk.OpCode.mod) => return simpleInstruction("OP_MODULO", offset),
        @intFromEnum(Chunk.OpCode.not) => return simpleInstruction("OP_NOT", offset),
        @intFromEnum(Chunk.OpCode.negation) => return simpleInstruction("OP_NEGATE", offset),
        @intFromEnum(Chunk.OpCode.pop) => return simpleInstruction("OP_POP", offset),
        @intFromEnum(Chunk.OpCode.get_global) => return constantInstruction("OP_GET_GLOBAL", chunk, offset),
        @intFromEnum(Chunk.OpCode.set_global) => return constantInstruction("OP_SET_GLOBAL", chunk, offset),
        @intFromEnum(Chunk.OpCode.define_global) => return constantInstruction("OP_DEFINE_GLOBAL", chunk, offset),
        @intFromEnum(Chunk.OpCode.print) => return simpleInstruction("OP_PRINT", offset),
        @intFromEnum(Chunk.OpCode.@"return") => return simpleInstruction("OP_RETURN", offset),
        else => {
            print("Unknown opcode: {}\n", .{op});
            return offset + 1;
        }
    }
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, chunk: *const Chunk, offset: usize) usize {
    const constant = chunk.code.items[offset + 1];
    print("{s: <16} {d} '{}'\n", .{name, constant, chunk.constants.items[constant]});
    return offset + 2;
}
