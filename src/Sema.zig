const std = @import("std");
const builtin = @import("builtin");
const Ast = @import("Ast.zig");
const Nir = @import("Nir.zig");
pub const Error = std.mem.Allocator.Error;

// TODO: rename NirGen or something?
const Sema = @This();

ast: Ast,
allocator: std.mem.Allocator,
instructions: std.MultiArrayList(Nir.Inst),
string_bytes: std.ArrayListUnmanaged(u8),
extra: std.ArrayListUnmanaged(u32),
scope: Scope,

pub fn generate(allocator: std.mem.Allocator, ast: Ast) Error!Nir {
    std.debug.assert(ast.errors.len == 0);

    var sema: Sema = .{
        .ast = ast,
        .allocator = allocator,
        .instructions = .{},
        .string_bytes = .{},
        .extra = .{},
        .scope = .root,
    };

    // index 0 is reserved for an empty string
    try sema.string_bytes.append(allocator, 0);

    try sema.instructions.ensureTotalCapacity(allocator, ast.nodes.len);

    for (ast.rootStmts()) |stmt| {
        const ref = try sema.expr(stmt);
        std.log.debug("{}: {}", .{ ast.nodes.items(.tag)[stmt], ref });
    }

    return .{
        .instructions = sema.instructions.toOwnedSlice(),
        .string_bytes = try sema.string_bytes.toOwnedSlice(allocator),
        .extra = try sema.extra.toOwnedSlice(allocator),
    };
}

const Scope = enum(u8) {
    root = 0,
    container = 1,
    _,
};

fn expr(self: *Sema, node: Ast.Node.Index) Error!Nir.Inst.Ref {
    switch (self.ast.nodes.items(.tag)[node]) {
        .root => unreachable,

        .add => return self.simpleBinOp(node, .add),
        .sub => return self.simpleBinOp(node, .sub),
        .mul => return self.simpleBinOp(node, .mul),

        .int_literal,
        .float_literal,
        => return self.numberLiteral(node, .positive),

        else => unreachable,
    }
}

// TODO
fn rvalue(self: *Sema, raw_result: Nir.Inst.Ref, src_node: Ast.Node.Index) Error!Nir.Inst.Ref {
    _ = self;
    _ = src_node;
    return raw_result;
}

fn simpleBinOp(self: *Sema, node: Ast.Node.Index, op: Nir.Inst.Tag) Error!Nir.Inst.Ref {
    const node_data = self.ast.nodes.items(.data)[node];
    const lhs = try self.expr(node_data.lhs);
    const rhs = try self.expr(node_data.rhs);
    const result = try self.addPlNode(op, node, Nir.Inst.Bin{ .lhs = lhs, .rhs = rhs });
    return self.rvalue(result, node);
}

const Sign = enum { negative, positive };

fn numberLiteral(self: *Sema, node: Ast.Node.Index, sign: Sign) Error!Nir.Inst.Ref {
    const token = self.ast.nodes.items(.main_token)[node];
    const bytes = self.ast.tokenSlice(token);
    const result = switch (std.zig.parseNumberLiteral(bytes)) {
        .int => |num| switch (num) {
            0 => if (sign == .positive) .zero else unreachable, // TODO: error -0
            1 => {
                const result: Nir.Inst.Ref = switch (sign) {
                    .positive => .one,
                    .negative => .negative_one,
                };
                return self.rvalue(result, node);
            },
            else => try self.add(.{
                .tag = .int,
                .data = .{ .int = num },
            }),
        },
        .big_int => |base| big: {
            var big_int = try std.math.big.int.Managed.init(self.allocator);
            defer big_int.deinit();
            const prefix_offset: usize = if (base == .decimal) 0 else 2;
            big_int.setString(@intFromEnum(base), bytes[prefix_offset..]) catch |err| switch (err) {
                error.InvalidCharacter => unreachable, // caught in parseNumberLiteral
                error.InvalidBase => unreachable, // we only pass 16, 8, 2, see above
                error.OutOfMemory => return error.OutOfMemory,
            };

            const limbs = big_int.limbs[0..big_int.len()];
            std.debug.assert(big_int.isPositive());
            // TODO
            _ = limbs;
            if (false) break :big 0;
            unreachable;
        },
        .float => {
            // TODO: should be handled somewhere else since nov allows 1. or .1 as float but zig doesn't
            const unsigned_float_number = std.fmt.parseFloat(f64, bytes) catch |err| switch (err) {
                error.InvalidCharacter => unreachable, // validated by tokenizer
            };
            const float_number = switch (sign) {
                .negative => -unsigned_float_number,
                .positive => unsigned_float_number,
            };
            const result = try self.add(.{
                .tag = .float,
                .data = .{ .float = float_number },
            });
            return self.rvalue(result, node);
        },
        .failure => |err| {
            // TODO:
            std.log.err("{}", .{err});
            std.process.exit(1);
        },
    };

    if (sign == .positive) {
        return self.rvalue(result, node);
    } else {
        // TODO
        // const negated = try self.addUnNode(.negate, result, node);
        // return self.rvalue(negated, node);
        unreachable;
    }
}

// TODO: used in addPlNode
// fn nodeIndexToRelative(self: *Sema, node_index: Ast.Node.Index) i32 {
//     return @as(i32, @bitCast(node_index)) - @as(i32, @bitCast(self.decl_node_index));
// }

fn addPlNode(
    self: *Sema,
    tag: Nir.Inst.Tag,
    src_node: Ast.Node.Index,
    extra: anytype,
) Error!Nir.Inst.Ref {
    const payload_index = try self.addExtra(extra);
    return self.add(.{
        .tag = tag,
        .data = .{
            .pl_node = .{
                .src_node = @bitCast(src_node), //self.nodeIndexToRelative(src_node),
                .payload_index = payload_index,
            },
        },
    });
}

fn addExtra(self: *Sema, extra: anytype) Error!u32 {
    const fields = std.meta.fields(@TypeOf(extra));
    try self.extra.ensureUnusedCapacity(self.allocator, fields.len);
    return self.addExtraAssumeCapacity(extra);
}

fn addExtraAssumeCapacity(self: *Sema, extra: anytype) u32 {
    const fields = std.meta.fields(@TypeOf(extra));
    const extra_index = self.extra.items.len;
    self.extra.items.len += fields.len;
    self.setExtra(extra_index, extra);
    return @intCast(extra_index);
}

fn setExtra(self: *Sema, index: usize, extra: anytype) void {
    const fields = std.meta.fields(@TypeOf(extra));
    var i = index;
    inline for (fields) |field| {
        self.extra.items[i] = switch (field.type) {
            u32 => @field(extra, field.name),

            Nir.Inst.Ref,
            Nir.Inst.Index,
            // Nir.Inst.Declaration.Name,
            // Nir.NullTerminatedString,
            => @intFromEnum(@field(extra, field.name)),

            i32,
            // Nir.Inst.Call.Flags,
            // Nir.Inst.BuiltinCall.Flags,
            // Nir.Inst.SwitchBlock.Bits,
            // Nir.Inst.SwitchBlockErrUnion.Bits,
            // Nir.Inst.FuncFancy.Bits,
            // Nir.Inst.Declaration.Flags,
            => @bitCast(@field(extra, field.name)),

            else => @compileError("bad field type"),
        };
        i += 1;
    }
}

fn add(self: *Sema, inst: Nir.Inst) Error!Nir.Inst.Ref {
    return (try self.addAsIndex(inst)).toRef();
}

fn addAsIndex(self: *Sema, inst: Nir.Inst) Error!Nir.Inst.Index {
    const new_index: Nir.Inst.Index = @enumFromInt(self.instructions.len);
    try self.instructions.append(self.allocator, inst);
    return new_index;
}
