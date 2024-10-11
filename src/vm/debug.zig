const std = @import("std");
const Chunk = @import("Chunk.zig");
pub const print = std.debug.print;

pub fn disassembleChunk(chunk: *const Chunk, name: []const u8) void {
    print("\x1b[2m", .{}); // dim
    print("== {s} ==\n", .{name});
    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
    print("\x1b[m", .{});
}

pub fn disassembleInstruction(chunk: *const Chunk, offset: usize) usize {
    print("{d:0>4} ", .{offset});
    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        print("   | ", .{});
    } else {
        print("{d:0>4} ", .{chunk.lines.items[offset]});
    }

    const code = chunk.code.items[offset];
    const instruction: Chunk.OpCode = @enumFromInt(@as(u8, @truncate(code)));
    switch (instruction) {
        .constant => return constantInstruction("OP_CONSTANT", chunk, offset),
        .equal => return simpleInstruction("OP_EQUAL", offset),
        .greater => return simpleInstruction("OP_GREATER", offset),
        .less => return simpleInstruction("OP_LESS", offset),
        .add => return simpleInstruction("OP_ADD", offset),
        .sub => return simpleInstruction("OP_SUBTRACT", offset),
        .mul => return simpleInstruction("OP_MULTIPLY", offset),
        .div => return simpleInstruction("OP_DIVIDE", offset),
        .mod => return simpleInstruction("OP_MODULO", offset),
        .not => return simpleInstruction("OP_NOT", offset),
        .negation => return simpleInstruction("OP_NEGATE", offset),
        .pop => return simpleInstruction("OP_POP", offset),
        .get_local => return byteInstruction("OP_GET_LOCAL", chunk, offset),
        .set_local => return byteInstruction("OP_SET_LOCAL", chunk, offset),
        .get_global => return constantInstruction("OP_GET_GLOBAL", chunk, offset),
        .set_global => return constantInstruction("OP_SET_GLOBAL", chunk, offset),
        .define_global => return constantInstruction("OP_DEFINE_GLOBAL", chunk, offset),
        .print => return simpleInstruction("OP_PRINT", offset),
        .jump => return jumpInstruction("OP_JUMP", true, chunk, offset),
        .jump_if_false => return jumpInstruction("OP_JUMP_IF_FALSE", true, chunk, offset),
        .loop => return jumpInstruction("OP_LOOP", false, chunk, offset),
        .@"return" => return simpleInstruction("OP_RETURN", offset),
    }
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, chunk: *const Chunk, offset: usize) usize {
    const constant: u24 = @intCast(chunk.code.items[offset] >> 8);
    const value = chunk.constants.items[constant];
    print("{s: <16} {d:0>4} '{}'\n", .{ name, constant, value });
    return offset + 1;
}

fn byteInstruction(name: []const u8, chunk: *const Chunk, offset: usize) usize {
    const slot: u24 = @intCast(chunk.code.items[offset] >> 8);
    print("{s: <16} {d:0>4}\n", .{ name, slot });
    return offset + 1;
}

fn jumpInstruction(name: []const u8, forward: bool, chunk: *const Chunk, offset: usize) usize {
    const jump: u24 = @intCast(chunk.code.items[offset] >> 8);
    if (forward) {
        print("{s: <16} {d:0>4} -> {d}\n", .{ name, offset, offset + 1 + jump });
    } else {
        print("{s: <16} {d:0>4} -> {d}\n", .{ name, offset, offset + 1 - jump });
    }
    return offset + 1;
}
