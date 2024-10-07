// TODO: zig/src/Air.zig

// % cat test.zig && zig build-obj --verbose-air test.zig -OReleaseSmall
// export fn add(a: u32, b: u32) u32 {
//     var c: u32 = 42;
//     c += 1;
//     return c + a + b;
// }
// # Begin Function AIR: test.add:
// # Total AIR+Liveness bytes: 263B
// # AIR Instructions:         11 (99B)
// # AIR Extra Data:           13 (52B)
// # Liveness tomb_bits:       8B
// # Liveness Extra Data:      0 (0B)
// # Liveness special table:   0 (0B)
//   %0 = arg(u32, 0)
//   %1 = arg(u32, 1)
//   %2 = alloc(*u32)
//   %3!= store(%2, <u32, 42>)
//   %4 = load(u32, %2)
//   %5 = add(%4!, <u32, 1>)
//   %6!= store(%2, %5!)
//   %7 = load(u32, %2!)
//   %8 = add(%7!, %0!)
//   %9 = add(%8!, %1!)
//   %10!= ret(%9!)
// # End Function AIR: test.add

const std = @import("std");

const Air = @This();

instructions: std.MultiArrayList(Inst).Slice,

pub const Inst = struct {
    tag: Tag,
    data: Data,

    // TODO: doc comments
    pub const Tag = enum(u8) {
        /// Float or integer addition. For integers, wrapping is undefined behavior.
        /// Both operands are guaranteed to be the same type, and the result type
        /// is the same as both operands.
        /// Uses the `bin_op` field.
        add,
        /// Allocates stack local memory.
        /// Uses the `ty` field.
        alloc,
        /// Write a value to a pointer. LHS is pointer, RHS is value.
        /// Result type is always ().
        /// Uses the `bin_op` field.
        /// The value to store may be undefined, in which case the destination
        /// memory region has undefined bytes after this instruction is
        /// evaluated. In such case ignoring this instruction is legal
        /// lowering.
        store,
        /// Read a value from a pointer.
        /// Uses the `ty_op` field.
        load,
        /// Return a value from a function.
        /// Result type is always noreturn; no instructions in a block follow this one.
        /// Uses the `un_op` field.
        /// Triggers `resolveTypeLayout` on the return type.
        ret,
    };

    pub const Data = union {
        no_op: void,
        // un_op: Ref,

        bin_op: struct {
            lhs: Ref,
            rhs: Ref,
        },
        ty: Type,
        // arg: struct {
        //     ty: Ref,
        //     src_index: u32,
        // },
        ty_op: struct {
            ty: Ref,
            // Index into a different array.
            payload: u32,
        },

        comptime {
            if (!std.debug.runtime_safety) {
                std.debug.assert(@sizeOf(Data) == 8);
            }
        }
    };

    // TODO: very wrong
    pub const Ref = union(enum) {
        type: Type,
        value: Value,
        index: usize,
    };
};

// TODO: use InternPool
const Type = SimpleType;
const Value = union {
    simple: SimpleValue,
    int: u64,
};

const SimpleType = enum {
    int,
    float,
    bool,
    string,
    type,
    undefined,
    @"()",
};

const SimpleValue = enum {
    undefined,
    true,
    false,
    @"()",
};
