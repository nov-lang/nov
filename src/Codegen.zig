const std = @import("std");
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const _value = @import("value.zig");
const Value = _value.Value;
const Chunk = @import("Chunk.zig");
const isPrimitive = @import("primitives.zig").isPrimitive;

const Codegen = @This();

ast: *const Ast,
allocator: std.mem.Allocator,
arena: std.mem.Allocator,
chunk: Chunk,

pub fn generate(allocator: std.mem.Allocator, ast: Ast) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var codegen: Codegen = .{
        .allocator = allocator,
        .arena = arena.allocator(),
        .ast = &ast,
        .chunk = Chunk.init(allocator),
    };

    try codegen.generateNode(0);
}

const Nodegen = *const fn (*Codegen, Node.Index) anyerror!void;
const generators = std.enums.directEnumArray(Node.Tag, Nodegen, 0, .{
    // .negation = generateNegation,
    .int_literal = generateIntLiteral,
});

fn generateNode(self: *Codegen, node: Node.Index) !void {
    // synchronize

    if (generators[@intFromEnum(self.ast.nodes.items(.tag)[node])]) |gen| {
        return gen(self, node);
    }
}

fn emit(self: *Codegen, code: u32) !void {
    return self.chunk.write(code, self.previous.line);
}

fn emitOpCode(self: *Codegen, code: Chunk.OpCode) !void {
    return self.emit(@intFromEnum(code));
}

fn emitCodeArg(self: *Codegen, code: Chunk.OpCode, arg: u24) !void {
    return self.emit(@as(u32, @intFromEnum(code)) | (@as(u32, @intCast(arg)) << 8));
}

fn makeConstant(self: *Codegen, value: Value) !u24 {
    return self.chunk.addConstant(value);
}

fn emitConstant(self: *Codegen, value: Value) !void {
    return self.emitCodeArg(.constant, self.makeConstant(value));
}

// fn generateNegation(self: *Codegen, node: Node.Index) !void {
//     const operand = self.ast.nodes.items(.negation)[node];
//     try self.generateNode(operand);
//     try self.chunk.write(Chunk.OpCode.negation, self.ast.nodes.items(.line)[node]);
// }

fn generateIntLiteral(self: *Codegen, node: Node.Index) !void {
    const literal = self.ast.nodes.items(.main_token)[node];
    const value = Value.create(try std.fmt.parseInt(i64, literal, 0));
    try self.emitConstant(value);
}
