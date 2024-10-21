// Originally based on https://github.com/ziglang/zig/blob/master/lib/std/zig/Zir.zig
// See https://github.com/ziglang/zig/blob/master/LICENSE for additional LICENSE details

const std = @import("std");
const builtin = @import("builtin");
const Ast = @import("Ast.zig");

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
            Inst.Declaration.Name,
            NullTerminatedString,
            => @enumFromInt(self.extra[i]),

            i32,
            // Inst.Call.Flags,
            // Inst.BuiltinCall.Flags,
            // Inst.SwitchBlock.Bits,
            // Inst.SwitchBlockErrUnion.Bits,
            // Inst.FuncFancy.Bits,
            Inst.Declaration.Flags,
            => @bitCast(self.extra[i]),

            else => @compileError("extraData: bad field type: " ++ @typeName(field.type)),
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
        div,
        mod_rem, // %
        /// Integer literal that fits in a u64. Uses the `int` union field.
        int,
        /// Arbitrary sized integer literal. Uses the `str` union field.
        int_big,
        /// A float literal that fits in a f64. Uses the float union value.
        float,
        /// A float literal that fits in a f128. Uses the `pl_node` union value.
        /// Payload is `Float128`.
        float128,

        negate,

        dbg_stmt,
        break_inline,

        declaration,

        extended,

        pub fn isNoReturn(tag: Tag) bool {
            return switch (tag) {
                // .@"break",
                // .break_inline,
                // .condbr,
                // .condbr_inline,
                // .compile_error,
                // .ret_node,
                // .ret_load,
                // .ret_implicit,
                // .ret_err_value,
                // .repeat,
                // .repeat_inline,
                // .panic,
                // .trap,
                // .check_comptime_control_flow,
                // .switch_continue,
                // => true,
                else => false,
            };
        }
    };

    pub const data_tags = std.enums.directEnumArray(Tag, Data.FieldEnum, 0, .{
        .add = .pl_node,
        .sub = .pl_node,
        .mul = .pl_node,
        .div = .pl_node,
        .mod_rem = .pl_node,

        .int = .int,
        .int_big = .str,
        .float = .float,
        .float128 = .pl_node,

        .negate = .un_node,
        .dbg_stmt = .dbg_stmt,
        .break_inline = .@"break",
        .declaration = .declaration,
        .extended = .extended,
    });

    /// Rarer instructions are here; ones that do not fit in the 8-bit `Tag` enum.
    /// `noreturn` instructions may not go here; they must be part of the main `Tag` enum.
    // TODO: needed?
    pub const Extended = enum(u16) {
        /// Declares a global variable.
        /// `operand` is payload index to `ExtendedVar`.
        /// `small` is `ExtendedVar.Small`.
        variable,
        /// A struct type definition. Contains references to ZIR instructions for
        /// the field types, defaults, and alignments.
        /// `operand` is payload index to `StructDecl`.
        /// `small` is `StructDecl.Small`.
        struct_decl,

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
        // f80_type,
        f128_type,
        float_type,
        anyptr_type,
        bool_type,
        type_type,
        void_type,
        voidptr_type,
        string_type,
        zero,
        one,
        negative_one,
        void_value,
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

    pub const Break = struct {
        pub const no_src_node = std.math.maxInt(i32);

        operand_src_node: i32,
        block_inst: Index,
    };

    /// The meaning of these operands depends on the corresponding `Tag`.
    pub const Bin = struct {
        lhs: Ref,
        rhs: Ref,
    };

    // TODO
    /// Trailing:
    /// 0. lib_name: NullTerminatedString, // null terminated string index, if has_lib_name is set
    /// 1. align: Ref, // if has_align is set
    /// 2. init: Ref // if has_init is set
    /// The source node is obtained from the containing `block_inline`.
    pub const ExtendedVar = struct {
        var_type: Ref,

        pub const Small = packed struct {
            has_lib_name: bool,
            has_align: bool,
            has_init: bool,
            is_extern: bool,
            is_const: bool,
            is_threadlocal: bool,
            _: u10 = undefined,
        };
    };

    /// This data is stored inside extra, with trailing operands according to `body_len`.
    /// Each operand is an `Index`.
    pub const Block = struct {
        body_len: u32,
    };

    // TODO!
    /// Trailing:
    /// 0. doc_comment: u32          // if `has_doc_comment`; null-terminated string index
    /// 1. align_body_len: u32       // if `has_align_linksection_addrspace`; 0 means no `align`
    /// 2. linksection_body_len: u32 // if `has_align_linksection_addrspace`; 0 means no `linksection`
    /// 3. addrspace_body_len: u32   // if `has_align_linksection_addrspace`; 0 means no `addrspace`
    /// 4. value_body_inst: Zir.Inst.Index
    ///    - for each `value_body_len`
    ///    - body to be exited via `break_inline` to this `declaration` instruction
    /// 5. align_body_inst: Zir.Inst.Index
    ///    - for each `align_body_len`
    ///    - body to be exited via `break_inline` to this `declaration` instruction
    /// 6. linksection_body_inst: Zir.Inst.Index
    ///    - for each `linksection_body_len`
    ///    - body to be exited via `break_inline` to this `declaration` instruction
    /// 7. addrspace_body_inst: Zir.Inst.Index
    ///    - for each `addrspace_body_len`
    ///    - body to be exited via `break_inline` to this `declaration` instruction
    pub const Declaration = struct {
        // These fields should be concatenated and reinterpreted as a `std.zig.SrcHash`.
        src_hash_0: u32,
        src_hash_1: u32,
        src_hash_2: u32,
        src_hash_3: u32,
        /// The name of this `Decl`. Also indicates whether it is a test, comptime block, etc.
        name: Name,
        src_line: u32,
        flags: Flags,

        pub const Flags = packed struct(u32) {
            value_body_len: u28,
            is_pub: bool,
            is_export: bool,
            has_doc_comment: bool,
            has_align_linksection_addrspace: bool,
        };

        pub const Name = enum(u32) {
            @"comptime" = std.math.maxInt(u32),
            @"usingnamespace" = std.math.maxInt(u32) - 1,
            unnamed_test = std.math.maxInt(u32) - 2,
            /// In this case, `has_doc_comment` will be true, and the doc
            /// comment body is the identifier name.
            decltest = std.math.maxInt(u32) - 3,
            /// Other values are `NullTerminatedString` values, i.e. index into
            /// `string_bytes`. If the byte referenced is 0, the decl is a named
            /// test, and the actual name begins at the following byte.
            _,

            pub fn isNamedTest(name: Name, nir: Nir) bool {
                return switch (name) {
                    .@"comptime", .@"usingnamespace", .unnamed_test, .decltest => false,
                    _ => nir.string_bytes[@intFromEnum(name)] == 0,
                };
            }
            pub fn toString(name: Name, nir: Nir) ?NullTerminatedString {
                switch (name) {
                    .@"comptime", .@"usingnamespace", .unnamed_test, .decltest => return null,
                    _ => {},
                }
                const idx: u32 = @intFromEnum(name);
                if (nir.string_bytes[idx] == 0) {
                    // Named test
                    return @enumFromInt(idx + 1);
                }
                return @enumFromInt(idx);
            }
        };

        pub const Bodies = struct {
            value_body: []const Index,
            align_body: ?[]const Index,
            linksection_body: ?[]const Index,
            addrspace_body: ?[]const Index,
        };

        pub fn getBodies(declaration: Declaration, extra_end: u32, nir: Nir) Bodies {
            var extra_index: u32 = extra_end;
            extra_index += @intFromBool(declaration.flags.has_doc_comment);
            const value_body_len = declaration.flags.value_body_len;
            const align_body_len, const linksection_body_len, const addrspace_body_len = lens: {
                if (!declaration.flags.has_align_linksection_addrspace) {
                    break :lens .{ 0, 0, 0 };
                }
                const lens = nir.extra[extra_index..][0..3].*;
                extra_index += 3;
                break :lens lens;
            };
            return .{
                .value_body = b: {
                    defer extra_index += value_body_len;
                    break :b nir.bodySlice(extra_index, value_body_len);
                },
                .align_body = if (align_body_len == 0) null else b: {
                    defer extra_index += align_body_len;
                    break :b nir.bodySlice(extra_index, align_body_len);
                },
                .linksection_body = if (linksection_body_len == 0) null else b: {
                    defer extra_index += linksection_body_len;
                    break :b nir.bodySlice(extra_index, linksection_body_len);
                },
                .addrspace_body = if (addrspace_body_len == 0) null else b: {
                    defer extra_index += addrspace_body_len;
                    break :b nir.bodySlice(extra_index, addrspace_body_len);
                },
            };
        }
    };

    /// A f128 value, broken up into 4 u32 parts.
    pub const Float128 = struct {
        piece0: u32,
        piece1: u32,
        piece2: u32,
        piece3: u32,

        pub fn get(self: Float128) f128 {
            const int_bits = @as(u128, self.piece0) |
                (@as(u128, self.piece1) << 32) |
                (@as(u128, self.piece2) << 64) |
                (@as(u128, self.piece3) << 96);
            return @as(f128, @bitCast(int_bits));
        }
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

    /// Trailing: for each `imports_len` there is an Item
    pub const Imports = struct {
        imports_len: u32,

        pub const Item = struct {
            /// null terminated string index
            name: NullTerminatedString,
            /// points to the import name
            token: Ast.TokenIndex,
        };
    };

    pub const LineColumn = struct {
        line: u32,
        column: u32,
    };

    // TODO
    /// Trailing:
    /// 0. captures_len: u32 // if has_captures_len
    /// 1. fields_len: u32, // if has_fields_len
    /// 2. decls_len: u32, // if has_decls_len
    /// 3. capture: Capture // for every captures_len
    /// 4. backing_int_body_len: u32, // if has_backing_int
    /// 5. backing_int_ref: Ref, // if has_backing_int and backing_int_body_len is 0
    /// 6. backing_int_body_inst: Inst, // if has_backing_int and backing_int_body_len is > 0
    /// 7. decl: Index, // for every decls_len; points to a `declaration` instruction
    /// 8. flags: u32 // for every 8 fields
    ///    - sets of 4 bits:
    ///      0b000X: whether corresponding field has an align expression
    ///      0b00X0: whether corresponding field has a default expression
    ///      0b0X00: whether corresponding field is comptime
    ///      0bX000: whether corresponding field has a type expression
    /// 9. fields: { // for every fields_len
    ///        field_name: u32, // if !is_tuple
    ///        doc_comment: NullTerminatedString, // .empty if no doc comment
    ///        field_type: Ref, // if corresponding bit is not set. none means anytype.
    ///        field_type_body_len: u32, // if corresponding bit is set
    ///        align_body_len: u32, // if corresponding bit is set
    ///        init_body_len: u32, // if corresponding bit is set
    ///    }
    /// 10. bodies: { // for every fields_len
    ///        field_type_body_inst: Inst, // for each field_type_body_len
    ///        align_body_inst: Inst, // for each align_body_len
    ///        init_body_inst: Inst, // for each init_body_len
    ///    }
    pub const StructDecl = struct {
        // These fields should be concatenated and reinterpreted as a `std.zig.SrcHash`.
        // This hash contains the source of all fields, and any specified attributes (`extern`, backing type, etc).
        fields_hash_0: u32,
        fields_hash_1: u32,
        fields_hash_2: u32,
        fields_hash_3: u32,
        src_line: u32,
        /// This node provides a new absolute baseline node for all instructions within this struct.
        src_node: Ast.Node.Index,

        pub const Small = packed struct {
            has_captures_len: bool,
            has_fields_len: bool,
            has_decls_len: bool,
            has_backing_int: bool,
            known_non_opv: bool,
            known_comptime_only: bool,
            is_tuple: bool,
            name_strategy: NameStrategy,
            layout: std.builtin.Type.ContainerLayout,
            any_default_inits: bool,
            any_comptime_fields: bool,
            any_aligned_fields: bool,
            _: u2 = undefined,
        };
    };

    /// Represents a single value being captured in a type declaration's closure.
    pub const Capture = packed struct(u32) {
        tag: enum(u3) {
            /// `data` is a `u16` index into the parent closure.
            nested,
            /// `data` is a `Zir.Inst.Index` to an instruction whose value is being captured.
            instruction,
            /// `data` is a `Zir.Inst.Index` to an instruction representing an alloc whose contents is being captured.
            instruction_load,
            /// `data` is a `NullTerminatedString` to a decl name.
            decl_val,
            /// `data` is a `NullTerminatedString` to a decl name.
            decl_ref,
        },
        data: u29,
        pub const Unwrapped = union(enum) {
            nested: u16,
            instruction: Nir.Inst.Index,
            instruction_load: Nir.Inst.Index,
            decl_val: NullTerminatedString,
            decl_ref: NullTerminatedString,
        };
        pub fn wrap(cap: Unwrapped) Capture {
            return switch (cap) {
                .nested => |idx| .{
                    .tag = .nested,
                    .data = idx,
                },
                .instruction => |inst| .{
                    .tag = .instruction,
                    .data = @intCast(@intFromEnum(inst)),
                },
                .instruction_load => |inst| .{
                    .tag = .instruction_load,
                    .data = @intCast(@intFromEnum(inst)),
                },
                .decl_val => |str| .{
                    .tag = .decl_val,
                    .data = @intCast(@intFromEnum(str)),
                },
                .decl_ref => |str| .{
                    .tag = .decl_ref,
                    .data = @intCast(@intFromEnum(str)),
                },
            };
        }
        pub fn unwrap(cap: Capture) Unwrapped {
            return switch (cap.tag) {
                .nested => .{ .nested = @intCast(cap.data) },
                .instruction => .{ .instruction = @enumFromInt(cap.data) },
                .instruction_load => .{ .instruction_load = @enumFromInt(cap.data) },
                .decl_val => .{ .decl_val = @enumFromInt(cap.data) },
                .decl_ref => .{ .decl_ref = @enumFromInt(cap.data) },
            };
        }
    };

    pub const NameStrategy = enum(u2) {
        /// Use the same name as the parent declaration name.
        /// e.g. `const Foo = struct {...};`.
        parent,
        /// Use the name of the currently executing comptime function call,
        /// with the current parameters. e.g. `ArrayList(i32)`.
        func,
        /// Create an anonymous name for this declaration.
        /// Like this: "ParentDeclName_struct_69"
        anon,
        /// Use the name specified in the next `dbg_var_{val,ptr}` instruction.
        dbg_var,
    };
};
