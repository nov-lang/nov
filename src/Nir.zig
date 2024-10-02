const std = @import("std");
const builtin = @import("builtin");

// TODO: wrong way to start I think

const Nir = @This();

instructions: std.MultiArrayList(Inst).Slice,
// string_bytes: []u8,
// extra: []u32,

pub const Inst = struct {
    tag: Tag,
    data: Data,

    pub const Tag = enum(u8) {
        /// Arithmetic addition.
        /// Uses the `pl_node` union field. Payload is `Bin`.
        add,
        /// Arithmetic subtraction.
        /// Uses the `pl_node` union field. Payload is `Bin`.
        sub,
        /// Arithmetic multiplication.
        /// Uses the `pl_node` union field. Payload is `Bin`.
        mul,
        // TODO: div
        // TODO: mod
        /// Integer shift-left. Zeroes are shifted in from the right hand side.
        /// Uses the `pl_node` union field. Payload is `Bin`.
        shl,
        /// Integer shift-right. Arithmetic or logical depending on the signedness of
        /// the integer type.
        /// Uses the `pl_node` union field. Payload is `Bin`.
        shr,
    };

    pub const data_tags = std.enums.directEnumArray(Tag, Data.FieldEnum, 0, .{
        .add = .pl_node,
        .sub = .pl_node,
        .mul = .pl_node,
        .shl = .pl_node,
        .shr = .pl_node,
    });

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

        comptime {
            if (!std.debug.runtime_safety) {
                std.debug.assert(@sizeOf(Data) == 8);
            }
        }

        pub const FieldEnum = enum {
            pl_node,
        };

        comptime {
            for (std.meta.fieldNames(Data), std.meta.fieldNames(FieldEnum)) |f1, f2| {
                std.debug.assert(std.mem.eql(u8, f1, f2));
            }
        }
    };
};
