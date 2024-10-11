const std = @import("std");
const builtin = @import("builtin");
const Ast = @import("Ast.zig");
const BigIntConst = std.math.big.int.Const;
const BigIntMutable = std.math.big.int.Mutable;

const Nir = @This();

instructions: std.MultiArrayList(Inst).Slice,
/// In order to store references to strings in fewer bytes, we copy all
/// string bytes into here. String bytes can be null. It is up to whomever
/// is referencing the data here whether they want to store both index and length,
/// thus allowing null bytes, or store only index, and use null-termination. The
/// `string_bytes` array is agnostic to either usage.
/// Index 0 is reserved for special cases.
string_bytes: []u8,
/// The meaning of this data is determined by `Inst.Tag` value.
/// The first few indexes are reserved. See `ExtraIndex` for the values.
extra: []u32,

/// The data stored at byte offset 0 when NIR is stored in a file.
pub const Header = extern struct {
    instructions_len: u32,
    string_bytes_len: u32,
    extra_len: u32,
    __unused: u32 = 0,
    stat_inode: std.fs.File.INode,
    stat_size: u64,
    stat_mtime: i128,
};

pub const ExtraIndex = enum(u32) {
    /// If this is 0, no compile errors. Otherwise there is a `CompileErrors`
    /// payload at this index.
    compile_errors,
    /// If this is 0, this file contains no imports. Otherwise there is a `Imports`
    /// payload at this index.
    imports,

    _,
};

fn ExtraData(comptime T: type) type {
    return struct { data: T, end: usize };
}

/// Returns the requested data, as well as the new index which is at the start of the
/// trailers for the object.
pub fn extraData(self: Nir, comptime T: type, index: usize) ExtraData(T) {
    const fields = @typeInfo(T).Struct.fields;
    comptime var i: usize = index;
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

pub const NullTerminatedString = enum(u32) {
    empty = 0,
    _,
};

/// Given an index into `string_bytes` returns the null-terminated string found there.
pub fn nullTerminatedString(self: Nir, index: NullTerminatedString) [:0]const u8 {
    const slice = self.string_bytes[@intFromEnum(index)..];
    return slice[0..std.mem.indexOfScalar(u8, slice, 0).? :0];
}

pub fn refSlice(self: Nir, start: usize, len: usize) []Inst.Ref {
    return @ptrCast(self.extra[start..][0..len]);
}

pub fn bodySlice(self: Nir, start: usize, len: usize) []Inst.Index {
    return @ptrCast(self.extra[start..][0..len]);
}

pub fn hasCompileErrors(self: Nir) bool {
    return self.extra[@intFromEnum(ExtraIndex.compile_errors)] != 0;
}

pub fn deinit(self: *Nir, allocator: std.mem.Allocator) void {
    self.instructions.deinit(allocator);
    allocator.free(self.string_bytes);
    allocator.free(self.extra);
    self.* = undefined;
}

/// These are untyped instructions generated from an Abstract Syntax Tree.
/// The data here is immutable because it is possible to have multiple
/// analyses on the same NIR happening at the same time.
pub const Inst = struct {
    tag: Tag,
    data: Data,

    // TODO: doc comments
    pub const Tag = enum(u8) {
        add,
        sub,
        mul,
        /// Integer literal that fits in a u64. Uses the `int` union field.
        int,
        /// Arbitrary sized integer literal. Uses the `str` union field.
        int_big,
        /// A float literal that fits in a f64. Uses the float union value.
        float,
        /// A float literal that fits in a f128. Uses the `pl_node` union value.
        /// Payload is `Float128`.
        float128,
    };

    pub const data_tags = std.enums.directEnumArray(Tag, Data.FieldEnum, 0, .{
        .add = .pl_node,
        .sub = .pl_node,
        .mul = .pl_node,

        .int = .int,
        .int_big = .str,
        .float = .float,
        .float128 = .pl_node,
    });

    /// Rarer instructions are here; ones that do not fit in the 8-bit `Tag` enum.
    /// `noreturn` instructions may not go here; they must be part of the main `Tag` enum.
    // TODO: needed?
    pub const Extended = enum(u16) {
        pub const InstData = struct {
            opcode: Extended,
            small: u16,
            operand: u32,
        };
    };

    /// The position of a NIR instruction within the `Nir` instructions array.
    pub const Index = enum(u32) {
        root = 0,
        ref_start_index = static_len,
        _,

        pub const static_len = @intFromEnum(Ref.__last_ref);

        pub fn toRef(i: Index) Ref {
            return @enumFromInt(@intFromEnum(Index.ref_start_index) + @intFromEnum(i));
        }

        pub fn toOptional(i: Index) OptionalIndex {
            return @enumFromInt(@intFromEnum(i));
        }
    };

    pub const OptionalIndex = enum(u32) {
        root = 0,
        ref_start_index = Index.static_len,
        none = std.math.maxInt(u32),
        _,

        pub fn unwrap(oi: OptionalIndex) ?Index {
            return if (oi == .none) null else @enumFromInt(@intFromEnum(oi));
        }
    };

    /// A reference to NIR instruction, or to an InternPool index, or neither.
    ///
    /// If the integer tag value is < InternPool.static_len, then it
    /// corresponds to an InternPool index. Otherwise, this refers to a ZIR
    /// instruction.
    ///
    /// The tag type is specified so that it is safe to bitcast between `[]u32`
    /// and `[]Ref`.
    pub const Ref = enum(u32) {
        u8_type,
        i8_type,
        u16_type,
        i16_type,
        u32_type,
        i32_type,
        u64_type,
        i64_type,
        u128_type,
        i128_type,
        int_type,
        uint_type,
        // c_char_type,
        // c_short_type,
        // c_ushort_type,
        // c_int_type,
        // c_uint_type,
        // c_long_type,
        // c_ulong_type,
        // c_longlong_type,
        // c_ulonglong_type,
        // c_longdouble_type,
        // f16_type,
        f32_type,
        f64_type,
        f128_type,
        float_type,
        anyptr_type,
        bool_type,
        // type_type,
        // @"()_type",
        // void_type,
        string_type,
        zero,
        one,
        negative_one,
        // void_value, // @"()_value",
        unreachable_value,
        bool_true,
        bool_false,

        __last_ref,

        /// This Ref does not correspond to any NIR instruction or constant
        /// value and may instead be used as a sentinel to indicate null.
        none = std.math.maxInt(u32),

        _,

        pub fn toIndex(inst: Ref) ?Index {
            std.debug.assert(inst != .none);
            const ref_int = @intFromEnum(inst);
            if (ref_int >= @intFromEnum(Index.ref_start_index)) {
                return @enumFromInt(ref_int - @intFromEnum(Index.ref_start_index));
            } else {
                return null;
            }
        }

        pub fn toIndexAllowNone(inst: Ref) ?Index {
            if (inst == .none) return null;
            return toIndex(inst);
        }
    };

    // TODO: sort
    /// All instructions have an 8-byte payload, which is contained within
    /// this union. `Tag` determines which union field is active, as well as
    /// how to interpret the data within.
    pub const Data = union {
        /// Used for `Tag.extended`. The extended opcode determines the meaning
        /// of the `small` and `operand` fields.
        extended: Extended.InstData,
        /// Used for unary operators, with an AST node source location.
        un_node: struct {
            /// Offset from Decl AST node index.
            src_node: i32,
            /// The meaning of this operand depends on the corresponding `Tag`.
            operand: Ref,
        },
        /// Used for unary operators, with a token source location.
        un_tok: struct {
            /// Offset from Decl AST token index.
            src_tok: Ast.TokenIndex,
            /// The meaning of this operand depends on the corresponding `Tag`.
            operand: Ref,
        },
        pl_node: struct {
            /// Offset from Decl AST node index.
            /// `Tag` determines which kind of AST node this points to.
            src_node: i32,
            /// index into extra.
            /// `Tag` determines what lives there.
            payload_index: u32,
        },
        pl_tok: struct {
            /// Offset from Decl AST token index.
            src_tok: Ast.TokenIndex,
            /// index into extra.
            /// `Tag` determines what lives there.
            payload_index: u32,
        },
        bin: Bin,
        /// For strings which may contain null bytes.
        str: struct {
            /// Offset into `string_bytes`.
            start: NullTerminatedString,
            /// Number of bytes in the string.
            len: u32,

            pub fn get(self: @This(), code: Nir) []const u8 {
                return code.string_bytes[@intFromEnum(self.start)..][0..self.len];
            }
        },
        str_tok: struct {
            /// Offset into `string_bytes`. Null-terminated.
            start: NullTerminatedString,
            /// Offset from Decl AST token index.
            src_tok: u32,

            pub fn get(self: @This(), code: Nir) [:0]const u8 {
                return code.nullTerminatedString(self.start);
            }
        },
        /// Offset from Decl AST token index.
        tok: Ast.TokenIndex,
        /// Offset from Decl AST node index.
        node: i32,
        int: u64,
        float: f64,
        ptr_type: struct {
            flags: packed struct {
                is_allowzero: bool,
                is_mutable: bool,
                is_volatile: bool,
                has_sentinel: bool,
                has_align: bool,
                has_addrspace: bool,
                has_bit_range: bool,
                _: u1 = undefined,
            },
            size: std.builtin.Type.Pointer.Size,
            /// Index into extra. See `PtrType`.
            payload_index: u32,
        },
        int_type: struct {
            /// Offset from Decl AST node index.
            /// `Tag` determines which kind of AST node this points to.
            src_node: i32,
            signedness: std.builtin.Signedness,
            bit_count: u16,
        },
        @"unreachable": struct {
            /// Offset from Decl AST node index.
            /// `Tag` determines which kind of AST node this points to.
            src_node: i32,
        },
        @"break": struct {
            operand: Ref,
            /// Index of a `Break` payload.
            payload_index: u32,
        },
        dbg_stmt: LineColumn,
        /// Used for unary operators which reference an inst,
        /// with an AST node source location.
        inst_node: struct {
            /// Offset from Decl AST node index.
            src_node: i32,
            /// The meaning of this operand depends on the corresponding `Tag`.
            inst: Index,
        },
        str_op: struct {
            /// Offset into `string_bytes`. Null-terminated.
            str: NullTerminatedString,
            operand: Ref,

            pub fn getStr(self: @This(), code: Nir) [:0]const u8 {
                return code.nullTerminatedString(self.str);
            }
        },
        @"defer": struct {
            index: u32,
            len: u32,
        },
        defer_err_code: struct {
            err_code: Ref,
            payload_index: u32,
        },
        save_err_ret_index: struct {
            operand: Ref, // If error type (or .none), save new trace index
        },
        elem_val_imm: struct {
            /// The indexable value being accessed.
            operand: Ref,
            /// The index being accessed.
            idx: u32,
        },
        declaration: struct {
            /// This node provides a new absolute baseline node for all instructions within this struct.
            src_node: Ast.Node.Index,
            /// index into extra to a `Declaration` payload.
            payload_index: u32,
        },

        // Make sure we don't accidentally add a field to make this union
        // bigger than expected. Note that in Debug builds, Zig is allowed
        // to insert a secret field for safety checks.
        comptime {
            if (!std.debug.runtime_safety) {
                std.debug.assert(@sizeOf(Data) == 8);
            }
        }

        /// TODO this has to be kept in sync with `Data` which we want to be an untagged
        /// union. There is some kind of language awkwardness here and it has to do with
        /// deserializing an untagged union (in this case `Data`) from a file, and trying
        /// to preserve the hidden safety field.
        pub const FieldEnum = enum {
            extended,
            un_node,
            un_tok,
            pl_node,
            pl_tok,
            bin,
            str,
            str_tok,
            tok,
            node,
            int,
            float,
            ptr_type,
            int_type,
            @"unreachable",
            @"break",
            dbg_stmt,
            inst_node,
            str_op,
            @"defer",
            defer_err_code,
            save_err_ret_index,
            elem_val_imm,
            declaration,
        };

        comptime {
            for (std.meta.fieldNames(Data), std.meta.fieldNames(FieldEnum)) |f1, f2| {
                std.debug.assert(std.mem.eql(u8, f1, f2));
            }
        }
    };

    /// The meaning of these operands depends on the corresponding `Tag`.
    pub const Bin = struct {
        lhs: Ref,
        rhs: Ref,
    };

    /// This data is stored inside extra, with trailing operands according to `body_len`.
    /// Each operand is an `Index`.
    pub const Block = struct {
        body_len: u32,
    };

    /// Trailing: `CompileErrors.Item` for each `items_len`.
    pub const CompileErrors = struct {
        items_len: u32,

        /// Trailing: `note_payload_index: u32` for each `notes_len`.
        /// It's a payload index of another `Item`.
        pub const Item = struct {
            /// null terminated string index
            msg: NullTerminatedString,
            node: Ast.Node.Index,
            /// If node is 0 then this will be populated.
            token: Ast.TokenIndex,
            /// Can be used in combination with `token`.
            byte_offset: u32,
            /// 0 or a payload index of a `Block`, each is a payload
            /// index of another `Item`.
            notes: u32,

            pub fn notesLen(item: Item, code: Nir) u32 {
                if (item.notes == 0) return 0;
                const block = code.extraData(Block, item.notes);
                return block.data.body_len;
            }
        };
    };

    pub const LineColumn = struct {
        line: u32,
        column: u32,
    };
};
