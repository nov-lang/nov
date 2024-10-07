const std = @import("std");
const builtin = @import("builtin");
const Ast = @import("Ast.zig");

const Nir = @This();

instructions: std.MultiArrayList(Inst).Slice,
string_bytes: []u8,
extra: []u32,

pub const Inst = struct {
    tag: Tag,
    data: Data,

    // TODO: doc comments
    pub const Tag = enum(u8) {
        add,
        sub,
        mul,
        int,
        float,
    };

    // pub const data_tags = std.enums.directEnumArray(Tag, Data.FieldEnum, 0, .{
    //     .add = .pl_node,
    //     .sub = .pl_node,
    //     .mul = .pl_node,
    //     .shl = .pl_node,
    //     .shr = .pl_node,
    // });

    // TODO: this is stinky
    pub const Index = enum(u32) {
        root = 0,
        ref_start_index = 8, // KEEP IN SYNC WITH Ref
        _,

        pub fn toRef(i: Index) Ref {
            return @enumFromInt(@intFromEnum(Index.ref_start_index) + @intFromEnum(i));
        }
    };

    pub const Ref = enum(u32) {
        int_type,
        uint_type,
        float_type,
        bool_type,
        string_type,
        // type_type,
        // @"()_type",
        zero,
        one,
        negative_one,

        none = std.math.maxInt(u32),

        _,
    };

    pub const Data = union {
        pl_node: struct {
            /// Offset from Decl AST node index.
            /// `Tag` determines which kind of AST node this points to.
            src_node: i32,
            /// index into extra.
            /// `Tag` determines what lives there.
            payload_index: u32,

            // pub fn src(self: @This()) LazySrcLoc {
            //     return LazySrcLoc.nodeOffset(self.src_node);
            // }
        },
        int: u64, // TODO: store in Value (u32, or ditch nan-boxing)
        float: f64,

        // comptime {
        //     if (!std.debug.runtime_safety) {
        //         std.debug.assert(@sizeOf(Data) == 8);
        //     }
        // }

        //     pub const FieldEnum = enum {
        //         pl_node,
        //     };

        //     comptime {
        //         for (std.meta.fieldNames(Data), std.meta.fieldNames(FieldEnum)) |f1, f2| {
        //             std.debug.assert(std.mem.eql(u8, f1, f2));
        //         }
        //     }
    };

    /// The meaning of these operands depends on the corresponding `Tag`.
    pub const Bin = struct {
        lhs: Ref,
        rhs: Ref,
    };
};

pub const NullTerminatedString = enum(u32) {
    empty = 0,
    _,
};

/// Given an index into `string_bytes` returns the null-terminated string found there.
pub fn nullTerminatedString(self: Nir, index: NullTerminatedString) [:0]const u8 {
    const slice = self.string_bytes[@intFromEnum(index)..];
    return slice[0..std.mem.indexOfScalar(u8, slice, 0).? :0];
}

fn ExtraData(comptime T: type) type {
    return struct { data: T, end: usize };
}

pub fn extraData(self: Nir, comptime T: type, index: usize) ExtraData(T) {
    const fields = @typeInfo(T).Struct.fields;
    var i: usize = index;
    var result: T = undefined;
    inline for (fields) |field| {
        @field(result, field.name) = switch (field.type) {
            u32 => self.extra[i],

            Inst.Ref,
            Inst.Index,
            // Inst.Declaration.Name,
            NullTerminatedString,
            => @enumFromInt(self.extra[i]),

            i32,
            // Inst.Call.Flags,
            // Inst.BuiltinCall.Flags,
            // Inst.SwitchBlock.Bits,
            // Inst.SwitchBlockErrUnion.Bits,
            // Inst.FuncFancy.Bits,
            // Inst.Declaration.Flags,
            => @bitCast(self.extra[i]),

            else => @compileError("bad field type"),
        };
        i += 1;
    }
    return .{
        .data = result,
        .end = i,
    };
}

const Type = enum {
    int,
};

pub fn typeOf(self: *const Nir, inst: Inst) Type {
    _ = self;
    _ = inst;
    return .int;
    // if (inst.toInterned()) |ip_index| {
    // } else {
    // }
}
