//! C11 backend for verified typed IR.
//!
//! Emits one translation unit with externally visible `nov_`-prefixed functions
//! and function-pointer typedefs. IR values become ordered const locals;
//! local slots and conditional merges use mutable temporaries, so
//! the IR's callee-first, left-to-right order is preserved without relying on C
//! argument evaluation order.
//!
//! Arithmetic uses unsigned 32/64-bit working types, avoiding signed overflow
//! and narrow integer promotions. Conversion back to signed relies on the
//! modular unsigned-to-signed conversion provided by GCC, Clang and MSVC.
//! Widths other than 8/16/32/64 remain unsupported.
const std = @import("std");
const Ir = @import("Ir.zig");

pub const Error = Ir.VerifyError || std.mem.Allocator.Error || error{
    UnsupportedIntegerWidth,
    MissingEntrypoint,
    InvalidEntrypoint,
};

/// Describes the first construct the backend rejected. `span` indexes the source
/// the IR was lowered from, which this backend never needs to read.
pub const Diagnostic = struct {
    reason: Reason,
    function: ?Ir.FunctionId,
    span: Ir.Span,

    pub const Reason = enum {
        integer_width_unsupported,
        entrypoint_missing,
        entrypoint_signature,

        pub fn message(reason: Reason) []const u8 {
            return switch (reason) {
                .integer_width_unsupported => "C backend only supports 8, 16, 32 and 64-bit integers",
                .entrypoint_missing => "program requires a root function 'main' with type () -> void",
                .entrypoint_signature => "program entrypoint 'main' must have type () -> void",
            };
        }
    };
};

const Origin = struct { function: Ir.FunctionId, span: Ir.Span };
const WriteError = error{WriteFailed};

// Canonical IR deliberately forgets alias spelling. Guard every translation unit
// rather than tracking source-level alias provenance through types and lowering.
const native_target_guard = std.fmt.comptimePrint(
    "#include <limits.h>\n_Static_assert(sizeof(void *) * CHAR_BIT == {d}, \"Nov requires the native target pointer width\");\n",
    .{Ir.native_pointer_bits},
);

/// Verifies `ir`, then returns owned C source. Nothing is returned on failure.
/// When `diagnostic` is non-null it describes unsupported-feature failures.
pub fn emit(allocator: std.mem.Allocator, ir: *const Ir, diagnostic: ?*Diagnostic) Error![]u8 {
    try ir.verify();
    const origins = try allocator.alloc(?Origin, ir.types.items.len);
    defer allocator.free(origins);
    @memset(origins, null);
    try analyze(ir, origins, diagnostic);

    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    write(allocator, ir, origins, &out.writer) catch |err| switch (err) {
        error.WriteFailed, error.OutOfMemory => return error.OutOfMemory,
    };
    return out.toOwnedSlice();
}

/// Emits a program, leaving library emission through `emit` unchanged.
pub fn emitProgram(allocator: std.mem.Allocator, ir: *const Ir, diagnostic: ?*Diagnostic) Error![]u8 {
    try ir.verify();
    for (ir.functions.items, 0..) |function, i| {
        if (!std.mem.eql(u8, function.name, "main")) continue;
        const signature = ir.types.items[@intFromEnum(function.ty)].function;
        if (signature.params.len != 0 or ir.types.items[@intFromEnum(signature.result)] != .void) {
            return fail(diagnostic, .entrypoint_signature, .{ .function = @enumFromInt(i), .span = function.span });
        }
        const library = try emit(allocator, ir, diagnostic);
        defer allocator.free(library);
        return std.fmt.allocPrint(allocator, "{s}\nint main(void) {{\n    nov_main();\n{s}{s}    return 0;\n}}\n", .{ library, if (usesPrinting(ir)) "    nov_rt_check(fflush(stdout));\n" else "", if (usesManagedRuntime(ir)) "    nov_gc_shutdown(&nov_rt_heap);\n" else "" });
    }
    if (diagnostic) |d| d.* = .{ .reason = .entrypoint_missing, .function = null, .span = .{ .start = ir.source_len, .end = ir.source_len } };
    return error.MissingEntrypoint;
}

fn fail(diagnostic: ?*Diagnostic, reason: Diagnostic.Reason, origin: Origin) Error {
    if (diagnostic) |d| d.* = .{ .reason = reason, .function = origin.function, .span = origin.span };
    return switch (reason) {
        .integer_width_unsupported => error.UnsupportedIntegerWidth,
        .entrypoint_missing => error.MissingEntrypoint,
        .entrypoint_signature => error.InvalidEntrypoint,
    };
}

fn mark(origins: []?Origin, ty: Ir.TypeId, origin: Origin) void {
    const slot = &origins[@intFromEnum(ty)];
    if (slot.* == null) slot.* = origin;
}

/// Rejects unsupported operations and records which pool types are reachable
/// from function signatures and instructions. Type references only point to
/// lower indices, so one descending pass reaches every nested type.
fn analyze(ir: *const Ir, origins: []?Origin, diagnostic: ?*Diagnostic) Error!void {
    for (ir.functions.items, 0..) |function, f| {
        const id: Ir.FunctionId = @enumFromInt(f);
        mark(origins, function.ty, .{ .function = id, .span = function.span });
        for (function.body.?.instructions) |instruction| {
            const origin: Origin = .{ .function = id, .span = instruction.span };
            mark(origins, instruction.ty, origin);
        }
    }
    var i = origins.len;
    while (i > 0) {
        i -= 1;
        const origin = origins[i] orelse continue;
        switch (ir.types.items[i]) {
            .void, .boolean, .string => {},
            .array => |element| mark(origins, element, origin),
            .integer => |integer| switch (integer.bits) {
                8, 16, 32, 64 => {},
                else => return fail(diagnostic, .integer_width_unsupported, origin),
            },
            .function => |function| {
                for (function.params) |param| mark(origins, param, origin);
                mark(origins, function.result, origin);
            },
        }
    }
}

fn write(allocator: std.mem.Allocator, ir: *const Ir, origins: []const ?Origin, w: *std.Io.Writer) (WriteError || std.mem.Allocator.Error)!void {
    try w.writeAll("/* Generated by Nov. */\n#include <stdbool.h>\n#include <stdint.h>\n");
    try w.writeAll(native_target_guard);
    // Transitive signature types need declarations even when no function holds
    // an array value. Only direct managed values need the runtime/root frames.
    for (origins, ir.types.items) |origin, ty| {
        if ((origin != null and ty == .array) or usesManagedRuntime(ir)) {
            try w.writeAll("typedef struct { void *data; uintptr_t len, cap; } nov_array;\n");
            break;
        }
    }
    for (origins, ir.types.items) |origin, ty| {
        if (origin != null and ty == .string) {
            try w.writeAll("typedef struct { void *owner; const unsigned char *data; uintptr_t len; } nov_string;\n");
            break;
        }
    }
    try writeRuntime(ir, w);
    if (usesManagedRuntime(ir)) {
        try w.writeAll("#define NOV_GC_PANIC nov_rt_panic\n");
        try w.writeAll(@import("GcRuntime.zig").source);
        try w.writeByte('\n');
        try w.writeAll(@embedFile("runtime/array.h"));
        if (usesStringConcat(ir)) try w.writeAll(@embedFile("runtime/string.h"));
        try writeStringConversions(ir, w);
    }
    if (usesStringEquality(ir)) try w.writeAll(
        "#include <string.h>\n" ++
            "static bool nov_string_equal(nov_string a, nov_string b) {\n" ++
            "    return a.len == b.len && (!a.len || memcmp(a.data, b.data, (size_t)a.len) == 0);\n" ++
            "}\n",
    );
    for (ir.types.items, 0..) |_, t| {
        const element: Ir.TypeId = @enumFromInt(t);
        if (!usesArrayContains(ir, element)) continue;
        try w.print("static bool nov_array_contains_{d}(", .{t});
        try writeType(ir, w, element);
        try w.writeAll(" value, nov_array array) {\n    const ");
        try writeType(ir, w, element);
        try w.writeAll(" *data = array.data;\n    for (uintptr_t i = 0; i < array.len; ++i) {\n        if (value == data[i]) return true;\n    }\n    return false;\n}\n");
    }
    try writeArithmeticRuntime(ir, w);
    var first_typedef = true;
    for (origins, ir.types.items, 0..) |origin, ty, i| {
        if (origin == null or ty != .function) continue;
        if (first_typedef) try w.writeByte('\n');
        first_typedef = false;
        try w.writeAll("typedef ");
        try writeType(ir, w, ty.function.result);
        try w.print(" (*nov_fn_{d})(", .{i});
        try writeParamTypes(ir, w, ty.function.params);
        try w.writeAll(");\n");
    }
    if (ir.functions.items.len == 0) return;
    try w.writeByte('\n');
    for (ir.functions.items) |function| {
        try writeSignature(ir, w, function);
        try w.writeAll(";\n");
    }
    for (ir.functions.items) |function| {
        try w.writeByte('\n');
        try writeSignature(ir, w, function);
        try w.writeAll(" {\n");
        try writeBody(allocator, ir, w, function);
        try w.writeAll("}\n");
    }
}

fn needsRootFrame(ir: *const Ir, function: Ir.Function) bool {
    const signature = ir.getType(function.ty).?.function;
    for (signature.params) |param| if (Ir.isManaged(ir.getType(param).?)) return true;
    for (function.body.?.instructions) |instruction| if (Ir.isManaged(ir.getType(instruction.ty).?)) return true;
    return Ir.isManaged(ir.getType(signature.result).?);
}

fn usesManagedRuntime(ir: *const Ir) bool {
    for (ir.functions.items) |function| if (needsRootFrame(ir, function)) return true;
    return false;
}

fn usesStringEquality(ir: *const Ir) bool {
    for (ir.functions.items) |function| {
        const body = function.body.?;
        for (body.instructions) |instruction| {
            if (instruction.op != .binary) continue;
            const binary = instruction.op.binary;
            if ((binary.op == .eq or binary.op == .ne) and ir.getType(body.instructions[@intFromEnum(binary.lhs)].ty).? == .string) return true;
        }
    }
    return false;
}

fn writeStringConversions(ir: *const Ir, w: *std.Io.Writer) WriteError!void {
    for (ir.types.items, 0..) |ty, t| {
        if (ty != .integer) continue;
        var used = false;
        for (ir.functions.items) |function| {
            const body = function.body.?;
            for (body.instructions) |instruction| {
                if (instruction.op == .to_string and @intFromEnum(body.instructions[@intFromEnum(instruction.op.to_string)].ty) == t) used = true;
            }
        }
        if (!used) continue;
        try w.writeAll("#include <inttypes.h>\nstatic nov_string nov_to_string_");
        try w.print("{d}(", .{t});
        try writeType(ir, w, @enumFromInt(t));
        try w.print(" value) {{\n    char bytes[32];\n    int len = snprintf(bytes, sizeof bytes, \"%\" PRI{s}64, ({s}int64_t)value);\n", .{ if (ty.integer.signedness == .signed) "d" else "u", if (ty.integer.signedness == .signed) "" else "u" });
        try w.writeAll("    if (len < 0 || (size_t)len >= sizeof bytes) nov_rt_panic(\"integer formatting failed\");\n    return nov_string_concat((nov_string){0}, (nov_string){NULL, (const unsigned char *)bytes, (uintptr_t)len});\n}\n");
    }
}

fn usesStringConcat(ir: *const Ir) bool {
    for (ir.functions.items) |function| {
        for (function.body.?.instructions) |instruction| {
            if (instruction.op == .binary and ir.getType(instruction.ty).? == .string) return true;
            if (instruction.op == .to_string and ir.getType(function.body.?.instructions[@intFromEnum(instruction.op.to_string)].ty).? == .integer) return true;
        }
    }
    return false;
}

fn usesPrinting(ir: *const Ir) bool {
    for (ir.functions.items) |function| {
        for (function.body.?.instructions) |instruction| {
            if (instruction.op == .print) return true;
        }
    }
    return false;
}

fn writeRuntime(ir: *const Ir, w: *std.Io.Writer) WriteError!void {
    const printing = usesPrinting(ir);
    var panic_used = printing or usesManagedRuntime(ir);
    for (ir.functions.items) |function| {
        for (function.body.?.instructions) |instruction| {
            if (instruction.op == .binary and (instruction.op.binary.op == .div or instruction.op.binary.op == .mod)) panic_used = true;
        }
    }
    if (!panic_used) return;
    try w.writeAll(
        "#include <stdio.h>\n#include <stdlib.h>\n\n" ++
            "static _Noreturn void nov_rt_panic(const char *message) {\n" ++
            "    (void)fflush(stdout);\n    (void)fprintf(stderr, \"panic: %s\\n\", message);\n    exit(1);\n}\n",
    );
    if (printing) try w.writeAll(
        "#include <inttypes.h>\n\n" ++
            "#if defined(_WIN32)\n#include <io.h>\n#include <fcntl.h>\n#endif\n" ++
            "static void nov_rt_init_stdout(void) {\n" ++
            "#if defined(_WIN32)\n    static bool initialized;\n    if (!initialized) {\n        if (_setmode(_fileno(stdout), _O_BINARY) == -1) nov_rt_panic(\"stdout write failed\");\n        initialized = true;\n    }\n#endif\n}\n" ++
            "static void nov_rt_check(int result) {\n" ++
            "    if (result < 0) nov_rt_panic(\"stdout write failed\");\n}\n",
    );
}

fn workingBits(ty: Ir.Type) u16 {
    return if (ty.integer.bits == 64) 64 else 32;
}

// Emit only helpers actually referenced by instructions. The prefix cannot
// collide with mangled Nov names (whose underscores are doubled).
fn writeArithmeticRuntime(ir: *const Ir, w: *std.Io.Writer) WriteError!void {
    for (ir.types.items, 0..) |ty, t| {
        if (ty != .integer) continue;
        for ([_]Ir.BinaryOp{ .div, .mod }) |op| {
            var used = false;
            for (ir.functions.items) |function| {
                for (function.body.?.instructions) |instruction| {
                    if (@intFromEnum(instruction.ty) == t and instruction.op == .binary and instruction.op.binary.op == op) used = true;
                }
            }
            if (!used) continue;

            const bits = workingBits(ty);
            try w.writeAll("\nstatic ");
            try writeType(ir, w, @enumFromInt(t));
            try w.print(" nov_rt_{s}_{d}(", .{ @tagName(op), t });
            try writeType(ir, w, @enumFromInt(t));
            try w.writeAll(" a, ");
            try writeType(ir, w, @enumFromInt(t));
            try w.print(" b) {{\n    if (b == 0) nov_rt_panic(\"{s} by zero\");\n", .{if (op == .div) "division" else "remainder"});
            if (ty.integer.signedness == .signed) {
                // Unsigned subtraction also handles the magnitude of MIN.
                try w.print("    const uint{d}_t x = a < 0 ? UINT{d}_C(0) - (uint{d}_t)a : (uint{d}_t)a;\n", .{ bits, bits, bits, bits });
                try w.print("    const uint{d}_t y = b < 0 ? UINT{d}_C(0) - (uint{d}_t)b : (uint{d}_t)b;\n", .{ bits, bits, bits, bits });
                try w.print("    const uint{d}_t r = x {s} y;\n    return (", .{ bits, if (op == .div) "/" else "%" });
                try writeType(ir, w, @enumFromInt(t));
                try w.print(")({s} ? UINT{d}_C(0) - r : r);\n}}\n", .{ if (op == .div) "(a < 0) != (b < 0)" else "a < 0", bits });
            } else {
                try w.writeAll("    return (");
                try writeType(ir, w, @enumFromInt(t));
                try w.print(")((uint{d}_t)a {s} (uint{d}_t)b);\n}}\n", .{ bits, if (op == .div) "/" else "%", bits });
            }
        }
    }
}

/// Only called for types accepted by `analyze`.
fn writeType(ir: *const Ir, w: *std.Io.Writer, id: Ir.TypeId) WriteError!void {
    switch (ir.types.items[@intFromEnum(id)]) {
        .void => try w.writeAll("void"),
        .boolean => try w.writeAll("bool"),
        .integer => |integer| try w.print("{s}int{d}_t", .{ if (integer.signedness == .unsigned) "u" else "", integer.bits }),
        .function => try w.print("nov_fn_{d}", .{@intFromEnum(id)}),
        .array => try w.writeAll("nov_array"),
        .string => try w.writeAll("nov_string"),
    }
}

fn writeParamTypes(ir: *const Ir, w: *std.Io.Writer, params: []const Ir.TypeId) WriteError!void {
    if (params.len == 0) return w.writeAll("void");
    for (params, 0..) |param, i| {
        if (i != 0) try w.writeAll(", ");
        try writeType(ir, w, param);
    }
}

fn writeSignature(ir: *const Ir, w: *std.Io.Writer, function: Ir.Function) WriteError!void {
    const signature = ir.types.items[@intFromEnum(function.ty)].function;
    try writeType(ir, w, signature.result);
    try w.writeByte(' ');
    try writeFunctionName(w, function.name);
    try w.writeByte('(');
    if (signature.params.len == 0) try w.writeAll("void");
    for (signature.params, 0..) |param, i| {
        if (i != 0) try w.writeAll(", ");
        try writeType(ir, w, param);
        try w.print(" p{d}", .{i});
    }
    try w.writeByte(')');
}

/// Injective: `_` becomes `__` and any other non-alphanumeric byte `_XX`, so
/// Nov names can neither collide with each other nor with C keywords, libc or
/// the generated `nov_fn_N` typedefs.
fn writeFunctionName(w: *std.Io.Writer, name: []const u8) WriteError!void {
    try w.writeAll("nov_");
    for (name) |c| switch (c) {
        'a'...'z', 'A'...'Z', '0'...'9' => try w.writeByte(c),
        '_' => try w.writeAll("__"),
        else => try w.print("_{X:0>2}", .{c}),
    };
}

fn writeConstant(w: *std.Io.Writer, constant: Ir.Constant, ty: Ir.Type) WriteError!void {
    switch (constant.value) {
        .string => |bytes| {
            try w.writeAll("(nov_string){NULL, (const unsigned char *)\"");
            for (bytes) |byte| try w.print("\\{o:0>3}", .{byte});
            try w.print("\", {d}}}", .{bytes.len});
        },
        .void => unreachable, // void values have no C object
        .boolean => |value| try w.writeAll(if (value) "true" else "false"),
        .unsigned_integer => |value| try w.print("UINT{d}_C({d})", .{ ty.integer.bits, value }),
        .signed_integer => |value| {
            const integer = ty.integer;
            const minimum = -(@as(i128, 1) << @as(u7, @intCast(integer.bits - 1)));
            if (value == minimum) {
                // The magnitude of the minimum is not a valid positive literal.
                try w.print("(-INT{d}_MAX - 1)", .{integer.bits});
            } else if (value < 0) {
                try w.print("(-INT{d}_C({d}))", .{ integer.bits, -value });
            } else {
                try w.print("INT{d}_C({d})", .{ integer.bits, value });
            }
        },
    }
}

fn usesArrayContains(ir: *const Ir, element: Ir.TypeId) bool {
    for (ir.functions.items) |function| {
        const body = function.body.?;
        for (body.instructions) |instruction| {
            if (instruction.op == .array_contains and body.instructions[@intFromEnum(instruction.op.array_contains.value)].ty == element) return true;
        }
    }
    return false;
}

fn writeBody(allocator: std.mem.Allocator, ir: *const Ir, w: *std.Io.Writer, function: Ir.Function) (WriteError || std.mem.Allocator.Error)!void {
    const signature = ir.types.items[@intFromEnum(function.ty)].function;
    const body = function.body.?;
    const value_used = try allocator.alloc(bool, body.instructions.len);
    defer allocator.free(value_used);
    @memset(value_used, false);
    const param_used = try allocator.alloc(bool, signature.params.len);
    defer allocator.free(param_used);
    @memset(param_used, false);
    value_used[@intFromEnum(body.result)] = true;
    for (body.instructions) |instruction| switch (instruction.op) {
        .array_literal => |elements| for (elements) |value| {
            value_used[@intFromEnum(value)] = true;
        },
        .array_clone, .array_len, .to_string => |value| value_used[@intFromEnum(value)] = true,
        .array_index => |r| {
            value_used[@intFromEnum(r.array)] = true;
            value_used[@intFromEnum(r.index)] = true;
        },
        .array_contains => |r| {
            value_used[@intFromEnum(r.value)] = true;
            value_used[@intFromEnum(r.array)] = true;
        },
        .array_set => |r| {
            value_used[@intFromEnum(r.array)] = true;
            value_used[@intFromEnum(r.index)] = true;
            value_used[@intFromEnum(r.value)] = true;
        },
        .array_append => |r| {
            value_used[@intFromEnum(r.lhs)] = true;
            value_used[@intFromEnum(r.rhs)] = true;
        },
        .parameter => |index| param_used[index] = true,
        .local_slot => |value| value_used[@intFromEnum(value)] = true,
        .local_load => |slot| value_used[@intFromEnum(slot)] = true,
        .local_store => |store| {
            value_used[@intFromEnum(store.slot)] = true;
            value_used[@intFromEnum(store.value)] = true;
        },
        .unary => |unary| value_used[@intFromEnum(unary.operand)] = true,
        .binary => |binary| {
            value_used[@intFromEnum(binary.lhs)] = true;
            value_used[@intFromEnum(binary.rhs)] = true;
        },
        .call => |call| {
            value_used[@intFromEnum(call.callee)] = true;
            for (call.args) |arg| value_used[@intFromEnum(arg)] = true;
        },
        .print => |print| for (print.args) |arg| {
            value_used[@intFromEnum(arg)] = true;
        },
        .short_circuit => |region| {
            value_used[@intFromEnum(region.lhs)] = true;
            value_used[@intFromEnum(region.rhs)] = true;
        },
        .branch => |region| {
            value_used[@intFromEnum(region.condition)] = true;
            value_used[@intFromEnum(region.then_value)] = true;
            if (region.else_value) |value| value_used[@intFromEnum(value)] = true;
        },
        .return_value, .loop_break => |value| {
            if (value) |v| value_used[@intFromEnum(v)] = true;
        },
        .loop => |region| {
            value_used[@intFromEnum(region.condition)] = true;
            value_used[@intFromEnum(region.body)] = true;
            if (region.continuation) |value| value_used[@intFromEnum(value)] = true;
            if (region.else_value) |value| value_used[@intFromEnum(value)] = true;
        },
        .loop_continue, .constant, .function_ref => {},
    };

    const managed = needsRootFrame(ir, function);
    if (managed) {
        // The embedded internal API also serves library emission, whose host
        // owns shutdown. Keep strict C11 unused-function warnings portable.
        try w.writeAll("    (void)nov_gc_register_globals; (void)nov_gc_unregister_globals; (void)nov_gc_shutdown;\n    (void)nov_array_clone; (void)nov_array_index; (void)nov_array_append;\n");
        var roots: usize = 0;
        for (signature.params) |param| if (Ir.isManaged(ir.getType(param).?)) {
            roots += 1;
        };
        for (body.instructions, 0..) |instruction, i| {
            if (!Ir.isManaged(ir.getType(instruction.ty).?)) continue;
            try w.writeAll("    ");
            try writeType(ir, w, instruction.ty);
            try w.print(" v{d} = {{0}};\n", .{i});
            roots += 1;
        }
        // Stable function-scope void* fields, never casts from typed T** slots.
        if (roots != 0) {
            try w.writeAll("    void **nov_roots[] = {");
            for (signature.params, 0..) |param, p| if (Ir.isManaged(ir.getType(param).?)) {
                try w.print(" &p{d}.{s},", .{ p, if (ir.getType(param).? == .string) "owner" else "data" });
            };
            for (body.instructions, 0..) |instruction, i| if (Ir.isManaged(ir.getType(instruction.ty).?)) {
                try w.print(" &v{d}.{s},", .{ i, if (ir.getType(instruction.ty).? == .string) "owner" else "data" });
            };
            try w.print(" }};\n    nov_gc_roots nov_frame = {{0}};\n    nov_gc_push(&nov_rt_heap, &nov_frame, nov_roots, {d});\n", .{roots});
        } else {
            try w.writeAll("    nov_gc_roots nov_frame = {0};\n    nov_gc_push(&nov_rt_heap, &nov_frame, NULL, 0);\n");
        }
    }

    // Silence unused diagnostics instead of requiring non-standard attributes.
    for (param_used, 0..) |used, i| {
        if (!used) try w.print("    (void)p{d};\n", .{i});
    }
    for (body.instructions, 0..) |instruction, i| {
        // Open outer regions first when nested RHS regions share a start.
        var end = body.instructions.len;
        while (end > i) {
            end -= 1;
            const candidate = body.instructions[end];
            if (candidate.op == .loop) {
                const region = candidate.op.loop;
                if (region.condition_start == i) {
                    if (ir.getType(candidate.ty).? != .void and !Ir.isManaged(ir.getType(candidate.ty).?)) {
                        try w.writeAll("    ");
                        try writeType(ir, w, candidate.ty);
                        try w.print(" v{d};\n", .{end});
                    }
                    try w.print("    while (true) {{\n    goto nov_cond{d};\n    nov_cond{d}:;\n", .{ end, end });
                }
                if (region.body_start == i and !Ir.terminates(body, region.condition))
                    try w.print("    if (!v{d}) goto nov_{s}{d};\n", .{ @intFromEnum(region.condition), if (region.else_start != null) "else" else "exit", end });
                if (region.continuation_start != null and region.continuation_start.? == i)
                    try w.print("    goto nov_cont{d};\n    nov_cont{d}:;\n", .{ end, end });
                if (region.else_start != null and region.else_start.? == i)
                    try w.print("    }}\n    goto nov_else{d};\n    nov_else{d}:;\n    {{\n", .{ end, end });
            } else if (candidate.op == .branch) {
                const region = candidate.op.branch;
                const has_value = ir.types.items[@intFromEnum(candidate.ty)] != .void;
                if (region.then_start == i) {
                    if (has_value and !Ir.isManaged(ir.getType(candidate.ty).?)) {
                        try w.writeAll("    ");
                        try writeType(ir, w, candidate.ty);
                        try w.print(" v{d};\n", .{end});
                    }
                    try w.print("    if (v{d}) {{\n", .{@intFromEnum(region.condition)});
                }
                if (region.else_start == i) {
                    if (has_value and !Ir.terminates(body, region.then_value)) try w.print("    v{d} = v{d};\n", .{ end, @intFromEnum(region.then_value) });
                    try w.writeAll("    } else {\n");
                }
            } else if (candidate.op == .short_circuit and candidate.op.short_circuit.rhs_start == i) {
                const region = candidate.op.short_circuit;
                try w.print("    bool v{d} = v{d};\n    if ({s}v{d}) {{\n", .{ end, @intFromEnum(region.lhs), if (region.kind == .@"or") "!" else "", @intFromEnum(region.lhs) });
            }
        }
        if (instruction.op == .loop) {
            if (instruction.op.loop.else_value) |value| {
                if (ir.getType(instruction.ty).? != .void and !Ir.terminates(body, value))
                    try w.print("    v{d} = v{d};\n", .{ i, @intFromEnum(value) });
            }
            try w.print("    }}\n    goto nov_exit{d};\n    nov_exit{d}:;\n", .{ i, i });
            if (ir.getType(instruction.ty).? != .void and !value_used[i]) try w.print("    (void)v{d};\n", .{i});
            continue;
        }
        if (instruction.op == .loop_break or instruction.op == .loop_continue) {
            const owner = Ir.enclosingLoop(body, i).?;
            const loop = body.instructions[owner].op.loop;
            if (instruction.op == .loop_break) {
                if (instruction.op.loop_break) |value| {
                    if (ir.getType(body.instructions[owner].ty).? != .void)
                        try w.print("    v{d} = v{d};\n", .{ owner, @intFromEnum(value) });
                }
                try w.print("    goto nov_exit{d};\n", .{owner});
            } else {
                const from_body = i >= loop.body_start and i < (loop.continuation_start orelse loop.else_start orelse owner);
                try w.print("    goto nov_{s}{d};\n", .{ if (from_body and loop.continuation != null) "cont" else "cond", owner });
            }
            continue;
        }
        if (instruction.op == .branch) {
            if (ir.types.items[@intFromEnum(instruction.ty)] != .void and !Ir.terminates(body, instruction.op.branch.else_value.?)) {
                try w.print("    v{d} = v{d};\n", .{ i, @intFromEnum(instruction.op.branch.else_value.?) });
            }
            try w.writeAll("    }\n");
            if (ir.types.items[@intFromEnum(instruction.ty)] != .void and !value_used[i]) try w.print("    (void)v{d};\n", .{i});
            continue;
        }
        if (instruction.op == .short_circuit) {
            if (!Ir.terminates(body, instruction.op.short_circuit.rhs)) try w.print("    v{d} = v{d};\n", .{ i, @intFromEnum(instruction.op.short_circuit.rhs) });
            try w.writeAll("    }\n");
            if (!value_used[i]) try w.print("    (void)v{d};\n", .{i});
            continue;
        }
        const ty = ir.types.items[@intFromEnum(instruction.ty)];
        if (instruction.op == .return_value) {
            if (managed) try w.writeAll("    nov_gc_pop(&nov_rt_heap, &nov_frame);\n");
            if (instruction.op.return_value) |value| {
                if (ir.types.items[@intFromEnum(body.instructions[@intFromEnum(value)].ty)] != .void) {
                    try w.print("    return v{d};\n", .{@intFromEnum(value)});
                } else try w.writeAll("    return;\n");
            } else try w.writeAll("    return;\n");
            continue;
        }
        if (instruction.op == .local_store) {
            const store = instruction.op.local_store;
            try w.print("    v{d} = v{d};\n", .{ @intFromEnum(store.slot), @intFromEnum(store.value) });
            continue;
        }
        if (ty == .void) {
            // Void values have no C object; emit their effects as statements.
            if (instruction.op == .print) try writePrint(ir, w, body, instruction.op.print);
            if (instruction.op == .call) {
                try w.writeAll("    ");
                try writeCall(w, instruction.op.call);
                try w.writeAll(";\n");
            }
            continue;
        }
        if (ty == .string) {
            try w.print("    v{d} = ", .{i});
            switch (instruction.op) {
                .constant => |id| try writeConstant(w, ir.getConstant(id).?, ty),
                .parameter => |index| try w.print("p{d}", .{index}),
                .local_slot, .local_load => |value| try w.print("v{d}", .{@intFromEnum(value)}),
                .to_string => |value| {
                    const source_ty = body.instructions[@intFromEnum(value)].ty;
                    switch (ir.getType(source_ty).?) {
                        .string => try w.print("v{d}", .{@intFromEnum(value)}),
                        .boolean => try w.print("(v{d} ? (nov_string){{NULL, (const unsigned char *)\"true\", 4}} : (nov_string){{NULL, (const unsigned char *)\"false\", 5}})", .{@intFromEnum(value)}),
                        .integer => try w.print("nov_to_string_{d}(v{d})", .{ @intFromEnum(source_ty), @intFromEnum(value) }),
                        else => unreachable,
                    }
                },
                .binary => |b| try w.print("nov_string_concat(v{d}, v{d})", .{ @intFromEnum(b.lhs), @intFromEnum(b.rhs) }),
                .call => |call| try writeCall(w, call),
                else => unreachable,
            }
            try w.writeAll(";\n");
            if (!value_used[i] or instruction.op == .local_slot) try w.print("    (void)v{d};\n", .{i});
            continue;
        }
        if (ty == .array) {
            try writeArrayValue(ir, w, instruction, i);
            if (!value_used[i] or instruction.op == .local_slot) try w.print("    (void)v{d};\n", .{i});
            continue;
        }
        try w.writeAll(if (instruction.op == .local_slot) "    " else "    const ");
        try writeType(ir, w, instruction.ty);
        try w.print(" v{d} = ", .{i});
        switch (instruction.op) {
            .array_len => |value| try w.print("v{d}.len", .{@intFromEnum(value)}),
            .array_index => |r| {
                try w.writeAll("((");
                try writeType(ir, w, instruction.ty);
                try w.print(" *)v{d}.data)[nov_array_index(v{d}, v{d})]", .{ @intFromEnum(r.array), @intFromEnum(r.array), @intFromEnum(r.index) });
            },
            .array_contains => |r| try w.print("nov_array_contains_{d}(v{d}, v{d})", .{ @intFromEnum(body.instructions[@intFromEnum(r.value)].ty), @intFromEnum(r.value), @intFromEnum(r.array) }),
            .array_literal, .array_clone, .array_set, .array_append, .to_string => unreachable,
            .constant => |id| try writeConstant(w, ir.constants.items[@intFromEnum(id)], ty),
            .parameter => |index| try w.print("p{d}", .{index}),
            .local_slot, .local_load => |value| try w.print("v{d}", .{@intFromEnum(value)}),
            .function_ref => |id| try writeFunctionName(w, ir.functions.items[@intFromEnum(id)].name),
            .unary => |unary| switch (unary.op) {
                .bool_not => try w.print("!v{d}", .{@intFromEnum(unary.operand)}),
                .bit_not => {
                    // Operands may be promoted to int, so convert back explicitly.
                    try w.writeByte('(');
                    try writeType(ir, w, instruction.ty);
                    try w.print(")~v{d}", .{@intFromEnum(unary.operand)});
                },
                .negate => {
                    try w.writeByte('(');
                    try writeType(ir, w, instruction.ty);
                    try w.print(")(UINT{d}_C(0) - (uint{d}_t)v{d})", .{ workingBits(ty), workingBits(ty), @intFromEnum(unary.operand) });
                },
            },
            .binary => |binary| arithmetic: {
                const lhs = @intFromEnum(binary.lhs);
                const rhs = @intFromEnum(binary.rhs);
                if (ir.getType(body.instructions[lhs].ty).? == .string) {
                    if (binary.op == .ne) try w.writeByte('!');
                    try w.print("nov_string_equal(v{d}, v{d})", .{ lhs, rhs });
                    break :arithmetic;
                }
                switch (binary.op) {
                    .add, .sub, .mul => {
                        try w.writeByte('(');
                        try writeType(ir, w, instruction.ty);
                        const op = switch (binary.op) {
                            .add => "+",
                            .sub => "-",
                            .mul => "*",
                            else => unreachable,
                        };
                        try w.print(")((uint{d}_t)v{d} {s} (uint{d}_t)v{d})", .{ workingBits(ty), lhs, op, workingBits(ty), rhs });
                        break :arithmetic;
                    },
                    .div, .mod => {
                        try w.print("nov_rt_{s}_{d}(v{d}, v{d})", .{ @tagName(binary.op), @intFromEnum(instruction.ty), lhs, rhs });
                        break :arithmetic;
                    },
                    else => {},
                }
                const operator = switch (binary.op) {
                    .eq => "==",
                    .ne => "!=",
                    .lt => "<",
                    .le => "<=",
                    .gt => ">",
                    .ge => ">=",
                    .add, .sub, .mul, .div, .mod => unreachable,
                };
                try w.print("v{d} {s} v{d}", .{ @intFromEnum(binary.lhs), operator, @intFromEnum(binary.rhs) });
            },
            .call => |call| try writeCall(w, call),
            .loop, .loop_break, .loop_continue, .return_value, .print, .short_circuit, .branch, .local_store => unreachable,
        }
        try w.writeAll(";\n");
        if (!value_used[i] or instruction.op == .local_slot) try w.print("    (void)v{d};\n", .{i});
    }
    const result_type = ir.types.items[@intFromEnum(signature.result)];
    if (managed and !Ir.terminates(body, body.result)) try w.writeAll("    nov_gc_pop(&nov_rt_heap, &nov_frame);\n");
    if (result_type != .void and !Ir.terminates(body, body.result)) try w.print("    return v{d};\n", .{@intFromEnum(body.result)});
}

fn writeElementLayout(ir: *const Ir, w: *std.Io.Writer, element: Ir.TypeId) WriteError!void {
    try w.writeAll("sizeof(");
    try writeType(ir, w, element);
    try w.writeAll("), _Alignof(");
    try writeType(ir, w, element);
    try w.writeByte(')');
}

fn writeArrayValue(ir: *const Ir, w: *std.Io.Writer, instruction: Ir.Instruction, i: usize) WriteError!void {
    const element = ir.getType(instruction.ty).?.array;
    try w.print("    v{d} = ", .{i});
    switch (instruction.op) {
        .array_literal => |elements| {
            try w.print("nov_array_new({d}, {d}, ", .{ elements.len, elements.len });
            try writeElementLayout(ir, w, element);
            try w.writeAll(");\n");
            for (elements, 0..) |value, index| {
                try w.writeAll("    ((");
                try writeType(ir, w, element);
                try w.print(" *)v{d}.data)[{d}] = v{d};\n", .{ i, index, @intFromEnum(value) });
            }
            return;
        },
        .array_clone => |value| {
            try w.print("nov_array_clone(v{d}, ", .{@intFromEnum(value)});
            try writeElementLayout(ir, w, element);
            try w.writeByte(')');
        },
        .array_set => |r| {
            try w.print("nov_array_clone(v{d}, ", .{@intFromEnum(r.array)});
            try writeElementLayout(ir, w, element);
            try w.writeAll(");\n    ((");
            try writeType(ir, w, element);
            try w.print(" *)v{d}.data)[nov_array_index(v{d}, v{d})] = v{d};\n", .{ i, i, @intFromEnum(r.index), @intFromEnum(r.value) });
            return;
        },
        .array_append => |r| {
            try w.print("nov_array_append(v{d}, v{d}, ", .{ @intFromEnum(r.lhs), @intFromEnum(r.rhs) });
            try writeElementLayout(ir, w, element);
            try w.writeByte(')');
        },
        .parameter => |index| try w.print("p{d}", .{index}),
        .local_slot, .local_load => |value| try w.print("v{d}", .{@intFromEnum(value)}),
        .call => |call| try writeCall(w, call),
        else => unreachable,
    }
    try w.writeAll(";\n");
}

fn writePrint(ir: *const Ir, w: *std.Io.Writer, body: Ir.Body, print: anytype) WriteError!void {
    // Single-string print uses fwrite directly, and library emission has no
    // final-flush wrapper to reference the shared scalar/flush check helper.
    try w.writeAll("    (void)nov_rt_check;\n    nov_rt_init_stdout();\n");
    if (print.args.len == 0 and !print.newline) try w.writeAll("    nov_rt_check(fputs(\"\", stdout));\n");
    for (print.args, 0..) |arg, i| {
        if (i != 0) try w.writeAll("    nov_rt_check(fputc(' ', stdout));\n");
        const ty = ir.types.items[@intFromEnum(body.instructions[@intFromEnum(arg)].ty)];
        switch (ty) {
            .string => try w.print("    if (v{d}.len && fwrite(v{d}.data, 1, (size_t)v{d}.len, stdout) != (size_t)v{d}.len) nov_rt_panic(\"stdout write failed\");\n", .{ @intFromEnum(arg), @intFromEnum(arg), @intFromEnum(arg), @intFromEnum(arg) }),
            .boolean => try w.print("    nov_rt_check(fputs(v{d} ? \"true\" : \"false\", stdout));\n", .{@intFromEnum(arg)}),
            .integer => |integer| try w.print("    nov_rt_check(fprintf(stdout, \"%\" PRI{s}MAX, ({s}intmax_t)v{d}));\n", .{ if (integer.signedness == .signed) "d" else "u", if (integer.signedness == .signed) "" else "u", @intFromEnum(arg) }),
            else => unreachable,
        }
    }
    if (print.newline) try w.writeAll("    nov_rt_check(fputc('\\n', stdout));\n");
}

fn writeCall(w: *std.Io.Writer, call: anytype) WriteError!void {
    try w.print("v{d}(", .{@intFromEnum(call.callee)});
    for (call.args, 0..) |arg, i| {
        if (i != 0) try w.writeAll(", ");
        try w.print("v{d}", .{@intFromEnum(arg)});
    }
    try w.writeByte(')');
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const Module = @import("Module.zig");
const Lowering = @import("Lowering.zig");

const test_gpa = std.testing.allocator;

/// Lowers `source`, which must be accepted by the frontend, and emits C.
fn emitSource(source: []const u8, diagnostic: ?*Diagnostic) ![]u8 {
    var module = try Module.init(test_gpa, "test.nov", source);
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    if (module.hasErrors()) {
        for (module.ast.errors) |err| std.debug.print("parse error: {any}\n", .{err});
        for (module.diagnostics.items) |err| std.debug.print("{s} at {d}: {s}\n", .{ @tagName(err.tag), err.span.start, module.sourceSlice(err.span) orelse "<invalid span>" });
    }
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    return emit(test_gpa, &sema.ir, diagnostic);
}

test "nested callback signatures declare array types without managed runtime" {
    for ([_][]const u8{
        "let ignore: (callback: () -> []int) -> void = {}\nlet main: () -> void = {}",
        "let ignore: (callback: ([]bool) -> void) -> void = {}\nlet main: () -> void = {}",
        "let identity: (callback: ([]u8) -> () -> []int) -> ([]u8) -> () -> []int = callback\nlet main: () -> void = {}",
        "let ignore: (callback: (([]int) -> void) -> (() -> []bool)) -> void = {}\nlet main: () -> void = {}",
    }) |source| {
        var module = try Module.init(test_gpa, "callbacks.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        for ([_]bool{ false, true }) |program| {
            const c = if (program) try emitProgram(test_gpa, &sema.ir, null) else try emit(test_gpa, &sema.ir, null);
            defer test_gpa.free(c);
            try compileAndRun(c, if (program) "typedef int nonempty_translation_unit;" else "void nov_main(void); int main(void) { nov_main(); return 0; }");
            const array_decl = std.mem.indexOf(u8, c, "} nov_array;") orelse return error.TestUnexpectedResult;
            const callback_decl = std.mem.indexOf(u8, c, " (*nov_fn_") orelse return error.TestUnexpectedResult;
            try std.testing.expect(array_decl < callback_decl);
            try std.testing.expect(std.mem.indexOf(u8, c, "nov_gc_") == null);
            try std.testing.expect(std.mem.indexOf(u8, c, "nov_rt_") == null);
            try std.testing.expect(std.mem.indexOf(u8, c, "nov_frame") == null);
        }
    }
}

test "unreachable pooled array and callback types do not change scalar emission" {
    var module = try Module.init(test_gpa, "unused-types.nov", "let main: () -> void = {}");
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    const baseline = try emitProgram(test_gpa, &sema.ir, null);
    defer test_gpa.free(baseline);
    const boolean = try sema.ir.internType(.boolean);
    const array = try sema.ir.internType(.{ .array = boolean });
    const callback = try sema.ir.internType(.{ .function = .{ .params = &.{array}, .result = array } });
    _ = try sema.ir.internType(.{ .function = .{ .params = &.{callback}, .result = callback } });
    const c = try emitProgram(test_gpa, &sema.ir, null);
    defer test_gpa.free(c);
    try std.testing.expectEqualStrings(baseline, c);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_array") == null);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_gc_") == null);
    try compileAndRun(c, "typedef int nonempty_translation_unit;");
}

const array_example =
    \\let make: (n: int) -> []int = {@print(n)
    \\    [n, n + 10]}
    \\let combine: (first: []int, second: []int) -> []int = {
    \\    let mut result: []int = first
    \\    result[0] = 99
    \\    @println(first[0], second[-1])
    \\    result += second
    \\    return result}
    \\let main: () -> void = {
    \\    let mut numbers: []int = [1, 2, 3]
    \\    let original: []int = numbers
    \\    numbers[-1] = 30
    \\    numbers += numbers
    \\    @println(original[-1], numbers[2], numbers[-1], numbers.len)
    \\    let joined: []int = combine(make(4), make(5))
    \\    @println(joined[0], joined[-1], joined.len)
    \\    let flags: []bool = [true, false]
    \\    @println(flags[-1], flags[0], flags.len)
    \\    numbers = []
    \\    @println(numbers.len)}
;

fn arrayTestProgram(source: []const u8) ![]u8 {
    const terminated = try test_gpa.dupeZ(u8, source);
    defer test_gpa.free(terminated);
    const formatted = try @import("Formatter.zig").format(test_gpa, terminated);
    defer test_gpa.free(formatted);
    const formatted_z = try test_gpa.dupeZ(u8, formatted);
    defer test_gpa.free(formatted_z);
    const again = try @import("Formatter.zig").format(test_gpa, formatted_z);
    defer test_gpa.free(again);
    try std.testing.expectEqualStrings(formatted, again);
    const original = try emitSource(source, null);
    defer test_gpa.free(original);
    const library = try emitSource(formatted, null);
    defer test_gpa.free(library);
    try std.testing.expectEqualStrings(original, library);
    return collectingTestProgram(library);
}

fn collectingTestProgram(library: []const u8) ![]u8 {
    return std.mem.concat(test_gpa, u8, &.{
        library,
        "\nint main(void) { nov_rt_heap.collect_every_allocation = 1; nov_main(); " ++
            "if (nov_rt_heap.frames) return 70; nov_gc_collect(&nov_rt_heap); " ++
            "if (nov_rt_heap.live_objects) return 71; nov_gc_shutdown(&nov_rt_heap); return 0; }\n",
    });
}

const string_example =
    \\let greeting: string = "hé\0"
    \\let identity: (s: string) -> string = s
    \\let effect: (s: string) -> string = {@print(s)
    \\    s + "!"}
    \\let choose: (b: bool) -> string = {if b {return "early" + "!"}
    \\    "late"}
    \\let main: () -> void = {
    \\    let mut value = greeting + "😀"
    \\    let shared = identity(value)
    \\    value = "new"
    \\    @println(shared, shared.len, value)
    \\    @println(effect("A"), effect("B"))
    \\    let chosen = if false {"skip" + "!"} else {shared}
    \\    @println(chosen, choose(true), choose(false))
    \\    let mut count: int = 0
    \\    loop count < 3 {
    \\        value += "x"
    \\        count += 1
    \\        if count == 1 {continue}
    \\        if count == 3 {break}
    \\    }
    \\    @println(value, "".len, "" + "", "\{literal\}", "\xC3\xA9")
    \\    let a = [true, false]
    \\    @println(a.len, shared)
    \\}
;
const string_output = "hé\x00😀 8 new\nABA! B!\nhé\x00😀 early! late\nnewxxx 0  {literal} é\n2 hé\x00😀\n";

test "immutable strings share UTF8 bytes across calls replacements control flow and GC" {
    const c = try arrayTestProgram(string_example);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, string_output, "");
    try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, string_output, "", "-O0");
}

const match_pipes =
    \\let fibo: (n: int) -> int = match n {
    \\    0 | 1 => n
    \\    _ => fibo(n - 1) + fibo(n - 2)
    \\}
    \\let fizz: (n: uint) -> string = match n % 3, n % 5 {
    \\    0, 0 => "FizzBuzz"
    \\    0, _ => "Fizz"
    \\    _, 0 => "Buzz"
    \\    _ => "number"
    \\}
    \\let identity: (n: int) -> int = n
    \\let callee: () -> (int) -> int = {@print("C")
    \\    identity}
    \\let argument: () -> int = {@print("A")
    \\    7}
    \\let early: (n: int) -> string = match n {
    \\    -1 | 0 => {return "early" + "!"}
    \\    _ => "late" + "!"
    \\}
    \\let terminal: (b: bool) -> int = match b {
    \\    true => {return 1}
    \\    _ => {return 2}
    \\}
    \\let pipeExit: () -> int = {return 12} |> callee()
    \\let calleeExit: () -> int = argument() |> {return 13}
    \\let subjectExit: () -> int = match {return 14}, argument() {_ => 99}
    \\let otherExit: () -> int = match argument(), {return 15} {_ => 99}
    \\let main: () = {
    \\    10 |> fibo |> @println
    \\    @println(fizz(15), fizz(3), fizz(5), fizz(1))
    \\    argument() |> callee() |> @println
    \\    let callback = identity
    \\    8 |> callback |> @println
    \\    @println(pipeExit(), calleeExit(), subjectExit(), otherExit())
    \\    true |> @print
    \\    "!" |> @println
    \\    let mut n: int = 0
    \\    let selected = match {n += 1
    \\        @print("L")
    \\        n}, {n *= 2
    \\        @print("R")
    \\        n} {
    \\        1 | 2, 2 => {@print("Y")
    \\            "chosen" + "!"}
    \\        1, _ => {@print("BAD")
    \\            "bad"}
    \\        _ => "bad"
    \\    }
    \\    @println(selected, n)
    \\    let text = "a" + "\x00b"
    \\    @println(match text {"a\x00b" | "other" => true
    \\        _ => false})
    \\    @println(match "a" + "b", "c" + "d" {"ab", "cd" => "rooted" + "!"
    \\        _ => "bad"})
    \\    @println(match false {true => "wrong"
    \\        _ => "right"})
    \\    @println(early(-1), early(2), terminal(true), terminal(false))
    \\    let a: []int = match true {
    \\        true => [1, 2]
    \\        _ => [3]
    \\    }
    \\    let mut copy = a
    \\    copy[0] = 9
    \\    @println(a[0], copy[0], a.len)
    \\    let picked = match true {true => identity
    \\        _ => identity}
    \\    6 |> picked |> @println
    \\    let inferred = match false {true => 1
    \\        _ => n}
    \\    @println(inferred)
    \\    let result = loop {
    \\        match n {2 => {n += 1
    \\            continue}
    \\            _ => {break n}}
    \\    }
    \\    @println(result)
    \\    @println(match true, false {true, false => "tuple"
    \\        _, _ => "bad"})
    \\}
;
const match_pipes_output = "55\nFizzBuzz Fizz Buzz number\nCA7\n8\nCA12 13 14 15\ntrue!\nLRYchosen! 2\ntrue\nrooted!\nright\nearly! late! 1 2\n1 9 2\n6\n2\n3\ntuple\n";

test "match and pipes preserve recursion effects exits exact results formatting and forced GC" {
    const c = try arrayTestProgram(match_pipes);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, match_pipes_output, "", mode);
}

test "match allocates only in the selected prong and reports runtime OOM" {
    for ([_]bool{ false, true }) |allocate| {
        const c = try arrayTestProgram(if (allocate)
            "let main: () = match true {true => @println(\"a\" + \"b\")\n_ => @println(\"ok\")}"
        else
            "let main: () = match true {true => @println(\"ok\")\n_ => @println(\"a\" + \"b\")}");
        defer test_gpa.free(c);
        const exhausted = try std.mem.concat(test_gpa, u8, &.{
            "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n" ++
                "#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n",
            c,
        });
        defer test_gpa.free(exhausted);
        for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", if (allocate) 1 else 0, if (allocate) "" else "ok\n", if (allocate) "panic: GC allocation failed\n" else "", mode);
    }
}

test "match and pipes own metadata after source release" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "match.nov", match_pipes);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        break :blk result;
    };
    defer sema.deinit();
    const c = try emitProgram(test_gpa, &sema.ir, null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, match_pipes_output, "");
}

test "match and pipes clean up every compiler allocation failure" {
    try std.testing.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{match_pipes});
}

const array_membership =
    \\let left: () -> i16 = {@print("L")
    \\    2}
    \\let right: () -> []i16 = {@print("R")
    \\    [1, 2, -3]}
    \\let early_left: () -> bool = {return true} in right()
    \\let early_right: () -> bool = left() in {return false}
    \\let main: () -> void = {
    \\    let mut a: []i16 = [-3, 2, 7]
    \\    let original = a
    \\    @println(-3 in a, 2 in a, 7 in a, 4 in a)
    \\    @println(true in [false, true], false in [true], true in [], false in [])
    \\    @println(left() in right())
    \\    @println(early_left(), early_right())
    \\    let mut n: i16 = 1
    \\    @println({n += 1
    \\        n} in {n = 9
    \\        [2, n]}, n)
    \\    @println({a = [9]
    \\        9} in a, 9 in original, -3 in original)
    \\    @println(false and left() in right(), true or left() in right())
    \\    let found = loop {let skipped = {break true} in right()
    \\    }
    \\    let other = loop {let skipped = left() in {break false}
    \\    }
    \\    @println(found, other)
    \\    let mut steps: int = 0
    \\    loop steps < 3 : steps += 1 {
    \\        let skipped = {if steps == 0 {continue}
    \\            2} in [2, steps]
    \\        loop {let stop = true in {break}
    \\        }
    \\        @println(skipped)
    \\        if steps == 1 {break}
    \\    }
    \\    @println(steps)
    \\    let mut checks: int = 0
    \\    loop {checks += 1
    \\        true in {if checks == 1 {continue}
    \\            if checks == 3 {break}
    \\            [true]}} : {let skipped = {continue} in [true]
    \\    } {@print("C")}
    \\    @println(checks)
    \\}
;
const array_membership_output = "true true true false\ntrue false false false\nLRtrue\nLtrue false\ntrue 9\ntrue false true\nfalse true\nLtrue false\ntrue\n1\nC3\n";

test "array membership preserves exact equality effects controls formatting and forced GC" {
    const c = try arrayTestProgram(array_membership);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, array_membership_output, "", mode);
}

test "array membership supports every fixed and native scalar element type" {
    var source = std.Io.Writer.Allocating.init(test_gpa);
    defer source.deinit();
    try source.writer.writeAll("let main: () -> void = {\n");
    for ([_][]const u8{ "i8", "i16", "i32", "i64", "int", "u8", "u16", "u32", "u64", "uint", "bool" }, 0..) |name, i| {
        const boolean = std.mem.eql(u8, name, "bool");
        const negative = name[0] == 'i';
        const value: []const u8 = if (boolean) "true" else if (negative) "-1" else "255";
        try source.writer.print("let n{d}: {s} = {s}\nlet a{d}: []{s} = [{s}]\n@println(n{d} in a{d}, n{d} in [], {s} in a{d})\n", .{ i, name, value, i, name, value, i, i, i, if (boolean) "false" else "0", i });
    }
    try source.writer.writeAll("}\n");
    const c = try arrayTestProgram(source.written());
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "true false false\n" ** 11, "", mode);
}

test "array membership scan allocates nothing ignores capacity and stops at first match" {
    const library = try emitSource("let contains: (n: i16, a: []i16) -> bool = n in a\nlet flag: (n: bool, a: []bool) -> bool = n in a", null);
    defer test_gpa.free(library);
    try std.testing.expect(std.mem.indexOf(u8, library, "memcmp") == null);
    const prefix = "static bool ";
    const first = std.mem.indexOf(u8, library, prefix ++ "nov_array_contains_").? + prefix.len;
    const integer_helper = library[first .. first + std.mem.indexOfScalar(u8, library[first..], '(').?];
    const second = std.mem.indexOfPos(u8, library, first, prefix ++ "nov_array_contains_").? + prefix.len;
    const boolean_helper = library[second .. second + std.mem.indexOfScalar(u8, library[second..], '(').?];
    const main = try std.fmt.allocPrint(
        test_gpa,
        "\nint main(void) {{\n" ++
            "int16_t data[] = {{-7, 2, 99}}; bool flags[] = {{false, true}};\n" ++
            "nov_array empty = {{NULL, 0, 10}};\n" ++
            "if ({s}(1, empty)) return 1;\n" ++
            "if ({s}(99, (nov_array){{data, 2, 3}})) return 2;\n" ++
            "if (!{s}(-7, (nov_array){{data, UINTPTR_MAX, UINTPTR_MAX}})) return 3;\n" ++
            "if (!{s}(2, (nov_array){{data, 2, 2}})) return 4;\n" ++
            "if (!{s}(true, (nov_array){{flags, 2, 2}})) return 5;\n" ++
            "if (nov_rt_heap.frames || nov_rt_heap.live_objects) return 6;\n" ++
            "nov_gc_shutdown(&nov_rt_heap); return 0; }}\n",
        .{ integer_helper, integer_helper, integer_helper, integer_helper, boolean_helper },
    );
    defer test_gpa.free(main);
    const c = try std.mem.concat(test_gpa, u8, &.{
        "#include <stdlib.h>\nstatic void *no_alloc(size_t n) { (void)n; abort(); }\n#define NOV_GC_MALLOC no_alloc\n#define NOV_GC_FREE free\n",
        library,
        main,
    });
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "", "", mode);
}

test "array membership helpers are demand only including nested callback signatures" {
    for ([_][]const u8{
        "let f: (callback: ([]bool) -> bool) -> void = {}",
        "let f: (n: int) -> bool = n == 1",
        "let f: (a: []i8) -> []i8 = a",
        "let f: () -> bool = {return true} in [true]",
    }) |source| {
        const c = try emitSource(source, null);
        defer test_gpa.free(c);
        try std.testing.expect(std.mem.indexOf(u8, c, "nov_array_contains_") == null);
        try compileAndRun(c, "int main(void) { return 0; }");
    }
}

test "array membership owned lowering emits after freeing source" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "source-free.nov", array_membership);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        break :blk result;
    };
    defer sema.deinit();
    try sema.ir.verify();
    const c = try emitProgram(test_gpa, &sema.ir, null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, array_membership_output, "");
}

test "array membership compiler ownership survives every allocation failure" {
    try std.testing.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{array_membership});
}

const range_program =
    \\let accept: (n: uint) -> void = @println(n)
    \\let signed: (n: int) -> void = @println(n)
    \\let count: (a: i8, b: i8) -> uint = {
    \\    let mut total: uint = 0
    \\    loop _ in a..b {total += 1}
    \\    total
    \\}
    \\let early: () -> string = {
    \\    loop i in 0..5 {if i == 2 {return "ret" + "urn"}}
    \\    "unreachable result"
    \\}
    \\let mark: (s: string, n: uint) -> uint = {@print(s)
    \\    n}
    \\let main: () = {
    \\    loop i in 0..3 {accept(i)}
    \\    loop i in -2..1 {signed(i)}
    \\    @println(count(-2, 2), count(2, 2), count(3, -2))
    \\    let mut hi: uint = 4
    \\    let mut calls: uint = 0
    \\    loop i in mark("S", 0)..{calls += 1
    \\        mark("E", hi)} {
    \\        hi = 0
    \\        @print(i)
    \\    }
    \\    @println("", calls, hi)
    \\    let mut total: uint = 0
    \\    loop i in 0..3 {
    \\        let text = "iteration={i}"
    \\        let values: []uint = [i, i + 1]
    \\        loop j in 0..5 {
    \\            if j == 1 {continue}
    \\            if j == 3 {break}
    \\            total += 1
    \\            let other = text + "!"
    \\            if values[0] == i {@print(other, " ")}
    \\        }
    \\        if i == 1 {continue}
    \\        @print(i)
    \\    }
    \\    @println("", total, early())
    \\    loop _ in 4..4 {@println(false)}
    \\}
;
const range_output = "0\n1\n2\n-2\n-1\n0\n4 0 0\nSE0123 1 0\niteration=0!  iteration=0!  0iteration=1!  iteration=1!  iteration=2!  iteration=2!  2 6 return\n";

test "ranges preserve defaults snapshots controls fresh values formatting and GC roots" {
    const c = try arrayTestProgram(range_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, range_output, "", mode);
}

test "ranges are finite at every integer maximum and accept grouped minimum bounds" {
    var source = std.Io.Writer.Allocating.init(test_gpa);
    defer source.deinit();
    var output = std.Io.Writer.Allocating.init(test_gpa);
    defer output.deinit();
    try source.writer.writeAll("let main: () = {\n");
    for ([_][]const u8{ "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "int", "uint" }) |name| {
        const bits: u7 = if (std.mem.eql(u8, name, "int") or std.mem.eql(u8, name, "uint")) @intCast(Ir.native_pointer_bits) else try std.fmt.parseInt(u7, name[1..], 10);
        const signed = name[0] == 'i';
        const min: i128 = if (signed) -(@as(i128, 1) << @intCast(bits - 1)) else 0;
        const max: i128 = (@as(i128, 1) << @intCast(bits - @as(u7, @intFromBool(signed)))) - 1;
        try source.writer.print("{{let a: {s} = {d}\nlet b: {s} = {d}\nloop i in a..{d} {{@println(i)}}\nloop i in {d}..b {{@println(i)}}\nloop _ in b..b {{@println(false)}}\n}}\n", .{ name, min, name, max, min + 2, max - 2 });
        try output.writer.print("{d}\n{d}\n{d}\n{d}\n", .{ min, min + 1, max - 2, max - 1 });
    }
    const min: i128 = -(@as(i128, 1) << (Ir.native_pointer_bits - 1));
    const max: u128 = (@as(u128, 1) << Ir.native_pointer_bits) - 1;
    try source.writer.print("loop i in -{{{{{d}}}}}..{d} {{@println(i)}}\nloop i in {d}..{d} {{@println(i)}}\n}}\n", .{ -min, min + 1, max - 1, max });
    try output.writer.print("{d}\n{d}\n", .{ min, max - 1 });
    const terminated = try test_gpa.dupeZ(u8, source.written());
    defer test_gpa.free(terminated);
    const formatted = try @import("Formatter.zig").format(test_gpa, terminated);
    defer test_gpa.free(formatted);
    const c = try emitSource(source.written(), null);
    defer test_gpa.free(c);
    const formatted_c = try emitSource(formatted, null);
    defer test_gpa.free(formatted_c);
    try std.testing.expectEqualStrings(c, formatted_c);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_rt_heap") == null);
    for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(c, "void nov_main(void); int main(void) {nov_main(); return 0;}", 0, output.written(), "", mode);
}

test "range endpoint controls belong to the enclosing scope and snapshots precede effects" {
    const source =
        \\let mark: (n: uint) -> uint = {@print("end")
        \\    n}
        \\let first: () -> uint = {
        \\    loop i in {return 17}..mark(1) {@println(i)}
        \\}
        \\let second: () -> uint = {
        \\    let zero: uint = 0
        \\    loop i in zero..{return 18} {@println(i)}
        \\}
        \\let outer: () -> uint = loop {
        \\    loop i in {break 42}..mark(1) {@println(i)}
        \\}
        \\let main: () = {
        \\    let zero: uint = 0
        \\    let two: uint = 2
        \\    let mut tries: uint = 0
        \\    loop tries < 3 : tries += 1 {
        \\        loop i in {if tries == 0 {continue}
        \\            zero}..{if tries == 2 {break}
        \\            two} {@print(i)}
        \\    }
        \\    @println("", tries, first(), second(), outer())
        \\    let mut lo: uint = 1
        \\    let mut hi: uint = 4
        \\    let mut calls: uint = 0
        \\    loop __range_counter in {calls += 1
        \\        lo}..{calls += 1
        \\        lo = 9
        \\        hi} {lo = 99
        \\        hi = 0
        \\        @print(__range_counter)}
        \\    @println("", calls, lo, hi)
        \\    let text = "managed" + " result"
        \\    @println(text)
        \\}
    ;
    const c = try arrayTestProgram(source);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "01 2 17 18 42\n123 2 99 0\nmanaged result\n", "", mode);
}

const terminal_range_bounds =
    \\let first: () -> int = loop _ in {return 7}..10 {}
    \\let last: () -> int = loop _ in 0..{return 8} {}
    \\let outer: () -> int = loop {loop _ in {break 9}..10 {}}
    \\let main: () -> void = {
    \\    let text = "a" + "b"
    \\    @println(first(), last(), outer(), text)
    \\    let mut n: int = 0
    \\    loop n < 3 : n += 1 {loop _ in {continue}..10 {}}
    \\    @println(n)
    \\}
;

test "range endpoint controls do not require an inferred iteration type" {
    const c = try arrayTestProgram(terminal_range_bounds);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "7 8 9 ab\n3\n", "", mode);
}

test "range lowering and formatting survive every allocation failure" {
    for ([_][]const u8{ range_program, terminal_range_bounds }) |source|
        try std.testing.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{source});
}

test "range allocating bodies report runtime OOM" {
    const c = try arrayTestProgram("let main: () = loop i in 0..1 {@println(\"{i}\")}\n");
    defer test_gpa.free(c);
    const exhausted = try std.mem.concat(test_gpa, u8, &.{
        "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n",
        c,
    });
    defer test_gpa.free(exhausted);
    for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", 1, "", "panic: GC allocation failed\n", mode);
}

const string_interpolation =
    \\let prefix: string = "é\0"
    \\let fizz: (n: int) -> string = match n % 3, n % 5 {
    \\    0, 0 => "FizzBuzz"
    \\    0, _ => "Fizz"
    \\    _, 0 => "Buzz"
    \\    _ => "{n}"
    \\}
    \\let identity: (s: string) -> string = "{s}"
    \\let early: (n: int) -> string = {if n == 0 {return "{n}"}
    \\    "number={n}"}
    \\let main: () -> void = {
    \\    let mut n: int = 0
    \\    let yes = true
    \\    let no = false
    \\    let mut s = prefix + "😀"
    \\    let shared = identity(s)
    \\    @println("A{yes}:{no}:{n}:\{{s}\}:Z", shared.len, "{true}/{false}")
    \\    s = "other"
    \\    @println(shared == prefix + "😀", "{shared}" == shared, "{shared}".len)
    \\    let empty = ""
    \\    @println("{empty}{empty}" == "", "{prefix}{shared}\0".len)
    \\    loop n < 16 : n += 1 {
    \\        if n == 0 {continue}
    \\        let text = fizz(n)
    \\        @println("[{n}]={text}")
    \\    }
    \\    let found = loop : {} {break "{n}"}
    \\    @println(found, early(0), early(9), shared)
    \\}
;
const string_interpolation_output = "Atrue:false:0:{é\x00😀}:Z 7 true/false\ntrue true 7\ntrue 11\n[1]=1\n[2]=2\n[3]=Fizz\n[4]=4\n[5]=Buzz\n[6]=Fizz\n[7]=7\n[8]=8\n[9]=Fizz\n[10]=Buzz\n[11]=11\n[12]=Fizz\n[13]=13\n[14]=14\n[15]=FizzBuzz\n16 0 number=9 é\x00😀\n";

test "identifier interpolation preserves chunks formatting roots loops matches and returns" {
    const c = try arrayTestProgram(string_interpolation);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, string_interpolation_output, "", mode);
}

test "interpolation decimal formatting is exact at every supported integer boundary" {
    var source = std.Io.Writer.Allocating.init(test_gpa);
    defer source.deinit();
    var output = std.Io.Writer.Allocating.init(test_gpa);
    defer output.deinit();
    try source.writer.writeAll("let main: () -> void = {\n");
    for ([_][]const u8{ "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "int", "uint" }, 0..) |name, i| {
        const bits: u7 = if (name[0] == 'i' and name[1] == 'n' or name[0] == 'u' and name[1] == 'i') @intCast(Ir.native_pointer_bits) else try std.fmt.parseInt(u7, name[1..], 10);
        const signed = name[0] == 'i';
        const min: i128 = if (signed) -(@as(i128, 1) << @intCast(bits - 1)) else 0;
        const max: i128 = (@as(i128, 1) << @intCast(bits - @as(u7, @intFromBool(signed)))) - 1;
        try source.writer.print("let a{d}: {s} = {d}\nlet b{d}: {s} = {d}\n@println(\"{{a{d}}}:{{b{d}}}\")\n", .{ i, name, min, i, name, max, i, i });
        try output.writer.print("{d}:{d}\n", .{ min, max });
    }
    try source.writer.writeAll("}\n");
    const c = try arrayTestProgram(source.written());
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, output.written(), "", mode);
}

test "interpolation escapes and inserted strings never become syntax or normalize" {
    const c = try arrayTestProgram(
        \\let main: () -> void = {
        \\    let n: int = -7
        \\    let raw = "\{n\}"
        \\    let composed = "é"
        \\    let decomposed = "e\u{301}"
        \\    @println("\x7braw\u{7d}\{{n}\}\\{n}:{raw}", "{composed}" == "{decomposed}")
        \\}
    );
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, "{raw}{-7}\\-7:{n} false\n", "");
}

test "interpolation allocations panic on exhaustion while bool and string sharing allocate nothing" {
    for ([_]struct { source: []const u8, output: []const u8 = "", allocates: bool = true }{
        .{ .source = "let main: () -> void = {let n: int = 1\n@println(\"{n}\")}" },
        .{ .source = "let main: () -> void = {let b = true\n@println(\"value={b}\")}" },
        .{ .source = "let main: () -> void = {let s = \"é\\0\"\n@println(\"{s}\", \"{false}\")}", .output = "é\x00 false\n", .allocates = false },
    }) |case| {
        const c = try arrayTestProgram(case.source);
        defer test_gpa.free(c);
        const exhausted = try std.mem.concat(test_gpa, u8, &.{
            "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n",
            c,
        });
        defer test_gpa.free(exhausted);
        for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", if (case.allocates) 1 else 0, case.output, if (case.allocates) "panic: GC allocation failed\n" else "", mode);
    }
}

test "interpolation without printing needs only demanded conversion helpers" {
    const library = try emitSource("let b: (n: bool) -> string = \"{n}\"\nlet s: (text: string) -> string = \"{text}\"\nlet i: (n: int) -> bool = \"{n}\" == \"-42\"", null);
    defer test_gpa.free(library);
    try std.testing.expect(std.mem.indexOf(u8, library, "nov_rt_init_stdout") == null);
    const combined = try std.mem.concat(test_gpa, u8, &.{
        library,
        "\nint main(void) {\n" ++
            "nov_string a = nov_b(true), b = nov_b(false);\n" ++
            "nov_string input = {NULL, (const unsigned char *)\"a\\0b\", 3};\n" ++
            "nov_string shared = nov_s(input);\n" ++
            "nov_string allocated = nov_string_concat(input, input);\n" ++
            "nov_string alias = nov_s(allocated);\n" ++
            "if (alias.owner != allocated.owner || alias.data != allocated.data || alias.len != allocated.len) return 2;\n" ++
            "if (a.len != 4 || b.len != 5 || shared.data != input.data || shared.owner != input.owner || !nov_i(-42) || nov_i(42)) return 1;\n" ++
            "nov_gc_shutdown(&nov_rt_heap); return 0; }",
    });
    defer test_gpa.free(combined);
    try compileAndRun(combined, "typedef int nonempty_translation_unit;");
    for ([_][]const u8{ "let f: (n: bool) -> string = \"{n}\"", "let f: (s: string) -> string = \"{s}\"" }) |source| {
        const c = try emitSource(source, null);
        defer test_gpa.free(c);
        try std.testing.expect(std.mem.indexOf(u8, c, "nov_to_string_") == null);
        try std.testing.expect(std.mem.indexOf(u8, c, "nov_string_concat") == null);
        try compileAndRun(c, "int main(void) {return 0;}");
    }
}

const string_equality =
    \\let literal: string = "hé\0😀"
    \\let make: () -> string = "hé\0" + "😀"
    \\let mark: (s: string) -> string = {@print(s)
    \\    s + "!"}
    \\let early: () -> bool = make() == {return true
    \\}
    \\let main: () -> void = {
    \\    let a = make()
    \\    let b = make()
    \\    @println(a == b, a == literal, a != b, literal != "hé\0😁")
    \\    @println("" == "" + "", "" != "x", "a\0b" == "a\x00b", "a\0b" != "a\0c")
    \\    @println("a\0" != "a", "ab" != "abc", "ab" != "cb", "a" != "A")
    \\    @println("é" == "\u{e9}", "é" != "e\u{301}", "😀" == "\u{1f600}")
    \\    let mut s = "old" + "!"
    \\    let snapshot = s == {s = "new" + "!"
    \\        "old" + "!"}
    \\    @println(snapshot, s == "new!")
    \\    @println(mark("L") != mark("R"), early())
    \\    let skipped = false and mark("BAD") == "BAD!"
    \\    @println(skipped)
    \\    let mut i: int = 0
    \\    loop i < 3 {
    \\        i += 1
    \\        let result = make() == {if i == 1 {continue}
    \\            if i == 2 {break}
    \\            mark("BAD")}
    \\        @println(result)
    \\    }
    \\    @println(i)
    \\}
;
const string_equality_output = "true true false true\ntrue true true true\ntrue true true true\ntrue true true\ntrue true\nLRtrue true\nfalse\n2\n";

test "string equality compares bytes and preserves effects terminal paths formatting and GC roots" {
    const c = try arrayTestProgram(string_equality);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, string_equality_output, "", mode);
}

test "string equality library is allocation free and handles null empty views" {
    const library = try emitSource("let equal: (a: string, b: string) -> bool = a == b\nlet different: (a: string, b: string) -> bool = a != b", null);
    defer test_gpa.free(library);
    try std.testing.expect(std.mem.indexOf(u8, library, "nov_string_concat") == null);
    const c = try std.mem.concat(test_gpa, u8, &.{
        "#include <stdlib.h>\nstatic void *no_alloc(size_t n) { (void)n; abort(); }\n#define NOV_GC_MALLOC no_alloc\n#define NOV_GC_FREE free\n",
        library,
        "\nint main(void) {\n" ++
            "nov_string a = {NULL, (const unsigned char *)\"a\\0b\", 3};\n" ++
            "nov_string b = {NULL, (const unsigned char *)\"a\\0c\", 3};\n" ++
            "nov_string empty = {NULL, NULL, 0};\n" ++
            "if (!nov_equal(empty, empty) || nov_different(empty, empty)) return 1;\n" ++
            "if (nov_equal(empty, a) || !nov_different(a, empty)) return 2;\n" ++
            "if (nov_equal(a, b) || !nov_different(a, b) || !nov_equal(a, a)) return 3;\n" ++
            "if (nov_rt_heap.frames || nov_rt_heap.live_objects) return 4;\n" ++
            "nov_gc_shutdown(&nov_rt_heap); return 0; }\n",
    });
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "", "", mode);
}

test "string equality helper is emitted only on demand" {
    for ([_][]const u8{
        "let f: (callback: (string) -> bool) -> void = {}",
        "let f: (s: string) -> string = s",
        "let main: () -> void = @println(\"a\" + \"b\")",
    }) |source| {
        const library = try emitSource(source, null);
        defer test_gpa.free(library);
        try std.testing.expect(std.mem.indexOf(u8, library, "nov_string_equal") == null);
        try compileAndRun(library, "int main(void) { return 0; }");
    }
}

const string_compound =
    \\let id: (s: string) -> string = s
    \\let early: (b: bool) -> string = {
    \\    let mut s: string = "old" + "!"
    \\    s += if b {s = "returned"
    \\        return s} else {"tail"}
    \\    s
    \\}
    \\let main: () -> void = {
    \\    let mut s = "hé" + "\0"
    \\    let original = id(s)
    \\    s += s
    \\    @println(s, s.len, original, original.len)
    \\    s += {s = "replacement" + "!"
    \\        id("😀" + "\0")}
    \\    @println(s, original)
    \\    let mut n: int = 0
    \\    loop n < 4 {
    \\        n += 1
    \\        s += {
    \\            if n == 1 {s = "continue" + "!"
    \\                continue}
    \\            if n == 3 {s = "break" + "!"
    \\                break}
    \\            "tail" + "!"
    \\        }
    \\        @println(s)
    \\    }
    \\    @println(s, n, early(true), early(false), original)
    \\    let mut empty = ""
    \\    empty += ""
    \\    empty += "é"
    \\    empty += ""
    \\    @println(empty, empty.len)
    \\}
;
const string_compound_output = "hé\x00hé\x00 8 hé\x00 4\nhé\x00hé\x00😀\x00 hé\x00\ncontinue!tail!\nbreak! 3 returned old!tail hé\x00\né 2\n";

test "string compounds capture before RHS and preserve aliases terminal effects and GC roots" {
    const c = try arrayTestProgram(string_compound);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, string_compound_output, "", mode);
}

test "string parameters returns and assignments share backing without allocation" {
    const library = try emitSource("let id: (s: string) -> string = {let mut x = s\nx = s\nx}\nlet make: () -> string = \"a\" + \"b\"", null);
    defer test_gpa.free(library);
    const c = try std.mem.concat(test_gpa, u8, &.{
        library,
        "\nint main(void) { nov_string s = nov_make(); nov_string t = nov_id(s); if (s.owner != t.owner || s.data != t.data || s.len != t.len) return 1; if (nov_rt_heap.live_objects != 1) return 2; nov_gc_shutdown(&nov_rt_heap); return 0; }\n",
    });
    defer test_gpa.free(c);
    try compileAndRun(c, "typedef int nonempty_translation_unit;");
}

test "single string print libraries compile strictly without a program wrapper" {
    for ([_][]const u8{ "\"text\"", "\"\"", "\"a\\0b\"" }, [_][]const u8{ "text", "", "a\x00b" }) |literal, output| {
        const source = try std.fmt.allocPrint(test_gpa, "let main: () -> void = @print({s})", .{literal});
        defer test_gpa.free(source);
        const library = try emitSource(source, null);
        defer test_gpa.free(library);
        try std.testing.expect(std.mem.indexOf(u8, library, "int main(") == null);
        try std.testing.expect(std.mem.indexOf(u8, library, "nov_rt_check(fflush(stdout))") == null);
        try compileAndRunExpected(library, "#include <stdio.h>\nvoid nov_main(void);\nint main(void) { nov_main(); return fflush(stdout) == EOF; }\n", 0, output, "");
    }
}

test "string output preserves NUL and checks fwrite and final flush failures" {
    var module = try Module.init(test_gpa, "stdout.nov", "let main: () -> void = @print(\"a\\0b\")");
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    const program = try emitProgram(test_gpa, &sema.ir, null);
    defer test_gpa.free(program);
    try compileAndRunExpected(program, "typedef int nonempty_translation_unit;", 0, "a\x00b", "");
    for ([_][]const u8{
        "#include <stdio.h>\nstatic size_t fail_write(const void *s, size_t n, size_t c, FILE *f) { (void)s; (void)n; (void)c; (void)f; return 0; }\n#define fwrite fail_write\n",
        "#include <stdio.h>\nstatic int fail_flush(FILE *f) { if (f == stdout) return EOF; return fflush(f); }\n#define fflush fail_flush\n",
    }, [_][]const u8{ "", "a\x00b" }) |prefix, output| {
        const injected = try std.mem.concat(test_gpa, u8, &.{ prefix, program });
        defer test_gpa.free(injected);
        try compileAndRunExpected(injected, "typedef int nonempty_translation_unit;", 1, output, "panic: stdout write failed\n");
    }
}

test "string callback signatures need declarations but not a managed runtime" {
    const library = try emitSource("let f: (callback: (string) -> string) -> void = {}", null);
    defer test_gpa.free(library);
    try std.testing.expect(std.mem.indexOf(u8, library, "} nov_string;") != null);
    try std.testing.expect(std.mem.indexOf(u8, library, "nov_gc_") == null);
    try compileAndRun(library, "int main(void) { return 0; }");
}

test "generated string programs compile for Windows with binary UTF8 output" {
    const zig_exe = std.testing.environ.getAlloc(test_gpa, "NOV_TEST_ZIG") catch try test_gpa.dupe(u8, "zig");
    defer test_gpa.free(zig_exe);
    for ([_][]const u8{ string_example, string_interpolation }) |source| {
        var module = try Module.init(test_gpa, "windows.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        const program = try emitProgram(test_gpa, &sema.ir, null);
        defer test_gpa.free(program);
        try std.testing.expect(std.mem.indexOf(u8, program, "_setmode(_fileno(stdout), _O_BINARY)") != null);
        var tmp = std.testing.tmpDir(.{});
        defer tmp.cleanup();
        try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "strings.c", .data = program });
        const compiled = try std.process.run(test_gpa, std.testing.io, .{
            .argv = &.{ zig_exe, "cc", "-target", "x86_64-windows-gnu", "-std=c11", "-Wall", "-Wextra", "-Wpedantic", "-Werror", "-c", "strings.c", "-o", "strings.o" },
            .cwd = .{ .dir = tmp.dir },
        });
        defer test_gpa.free(compiled.stdout);
        defer test_gpa.free(compiled.stderr);
        try expectSuccess(compiled, "Windows string C compilation");
    }
}

test "lowered strings emit after module and source storage are freed" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "source-free.nov", "let text: string = \"a\\0é\"\nlet main: () -> void = {let n: int = -42\n@println(\"{text}!\", text == \"a\\0\" + \"é\", \"{n}\")}");
        defer module.deinit();
        break :blk try Lowering.lower(&module);
    };
    defer sema.deinit();
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
    const program = try emitProgram(test_gpa, &sema.ir, null);
    defer test_gpa.free(program);
    try compileAndRunExpected(program, "typedef int nonempty_translation_unit;", 0, "a\x00é! true -42\n", "");
}

test "string compiler ownership survives every allocation failure" {
    for ([_][]const u8{ string_example, string_compound, string_equality, string_interpolation }) |source| try std.testing.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{source});
}

test "string length overflow panics before touching invalid backing in both modes" {
    for ([_][]const u8{
        "let f: (a: string, b: string) -> string = a + b",
        "let f: (a: string, b: string) -> string = \"{a}{b}\"",
        "let f: (a: string, b: string) -> string = {let mut s = a\ns += b\ns}",
    }) |source| {
        const library = try emitSource(source, null);
        defer test_gpa.free(library);
        const c = try std.mem.concat(test_gpa, u8, &.{
            library,
            "\nint main(void) { (void)nov_f((nov_string){NULL, NULL, UINTPTR_MAX}, (nov_string){NULL, NULL, 1}); return 99; }\n",
        });
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "", "panic: string size overflow\n", mode);
    }
}

const inferred_example =
    \\let seed: int = 7
    \\let mark: (n: int) -> int = {@println(n)
    \\    n}
    \\let make: (n: int) -> []int = [n, n + 1]
    \\let main: () -> void = {
    \\    let mut value = mark(seed)
    \\    let saved = value
    \\    value += 1
    \\    let callback = mark
    \\    let result = callback(value)
    \\    let ready = true
    \\    let mut values = make(result)
    \\    let original = values
    \\    values[0] += 1
    \\    values += [result]
    \\    @println(1 + {let inner = result
    \\        inner})
    \\    @println(saved, value, original[0], values[0], values.len, ready)
    \\}
;
const inferred_output = "7\n8\n9\n7 8 8 9 3 true\n";

test "inferred locals preserve evaluation order snapshots copies and GC roots" {
    const c = try arrayTestProgram(inferred_example);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, inferred_output, "", mode);
}

const inferred_arrays =
    \\let make: (n: int) -> []int = [n, n + 1]
    \\let mark: (n: int) -> int = {@println(n)
    \\    make(n)[0]}
    \\let early: (stop: bool) -> int = {
    \\    let a = [{if stop {return 9}
    \\        mark(3)}, mark(4)]
    \\    a[-1]}
    \\let main: () -> void = {
    \\    let mut count: int = 0
    \\    let mut a = [0, mark(7), {count += 1
    \\        mark(8)}]
    \\    let original = a
    \\    a[0] += {let temporary = [0, make(9)[0]]
    \\        temporary[-1]}
    \\    a += [count]
    \\    @println(original[0], original[1], original[2], count, a[0], a.len)
    \\    let flags = [true, false]
    \\    @println(flags[0], flags[-1], flags.len)
    \\    @println(early(true), early(false))
    \\    loop count < 4 {
    \\        count += 1
    \\        let values = [{if count == 3 {continue}
    \\            if count == 4 {break}
    \\            mark(count)}, mark(20)]
    \\        @println(values[0], values[-1])
    \\    }
    \\    let skipped = false and {let unused = [mark(99)]
    \\        unused[0] == 99}
    \\    @println(count, skipped)
    \\}
;
const inferred_arrays_output = "7\n8\n0 7 8 1 9 4\ntrue false 2\n3\n4\n9 4\n2\n20\n2 20\n4 false\n";

test "inferred array elements execute once in order with allocation control exits and GC" {
    const c = try arrayTestProgram(inferred_arrays);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, inferred_arrays_output, "", mode);
}

test "scalar arrays copy grow and return with every allocation collecting" {
    const c = try arrayTestProgram(array_example);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode| {
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "3 30 30 6\n454 15\n99 15 4\nfalse true 2\n0\n", "", mode);
    }
}

test "array element compound arithmetic wraps at every approved width" {
    var source = std.Io.Writer.Allocating.init(test_gpa);
    defer source.deinit();
    var output = std.Io.Writer.Allocating.init(test_gpa);
    defer output.deinit();
    try source.writer.writeAll("let main: () -> void = {\n");
    for ([_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "int", "uint" }) |name| {
        const signed = name[0] == 'i';
        const bits = if (std.mem.eql(u8, name, "int") or std.mem.eql(u8, name, "uint")) Ir.native_pointer_bits else try std.fmt.parseInt(u16, name[1..], 10);
        const maximum = (@as(u128, 1) << @intCast(bits - @as(u16, if (signed) 1 else 0))) - 1;
        try source.writer.print("{{let mut a: []{s} = [{d}]\na[-1] += 1\n@println(a[0])\na[0] -= 1\n@println(a[0])\na[0] *= 2\n@println(a[0])\n", .{ name, maximum });
        if (signed) {
            try output.writer.print("-{d}\n{d}\n-2\n", .{ maximum + 1, maximum });
            try source.writer.print("a[0] = -{d}\na[0] /= -1\n@println(a[0])\na[0] %= -1\n@println(a[0])\n", .{maximum + 1});
            try output.writer.print("-{d}\n0\n", .{maximum + 1});
            try source.writer.writeAll("a[0] = -7\na[0] /= 3\n@println(a[0])\na[0] = -7\na[0] %= 3\n@println(a[0])}\n");
            try output.writer.writeAll("-2\n-1\n");
        } else {
            try output.writer.print("0\n{d}\n{d}\n", .{ maximum, maximum - 1 });
            try source.writer.writeAll("a[0] = 7\na[0] /= 3\n@println(a[0])\na[0] = 7\na[0] %= 3\n@println(a[0])}\n");
            try output.writer.writeAll("2\n1\n");
        }
    }
    try source.writer.writeAll("}");
    const c = try arrayTestProgram(source.written());
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, output.written(), "", mode);
}

const array_compound_effects =
    \\let early: (c: bool) -> int = {
    \\    let mut a: []int = [10]
    \\    a[0] += if c {a = [88]
    \\        return a[0]} else {2}
    \\    a[0]}
    \\let index_return: () -> int = {
    \\    let mut a: []int = []
    \\    a[{return 7}] += { @print(false)
    \\        1}
    \\}
    \\let main: () -> void = {
    \\    let mut a: []int = [10, 20]
    \\    let copy: []int = a
    \\    let mut count: int = 0
    \\    a[{count += 1
    \\        @print(count)
    \\        a = [90]
    \\        0 - 1}] += {let scratch: []int = [3]
    \\        @print(a[0])
    \\        a = []
    \\        scratch[0]}
    \\    @println(a[0], a[-1], a.len, copy[-1], count)
    \\    a[-2] += {a[-1] = 99
    \\        a[0] = 80
    \\        2}
    \\    @println(a[0], a[-1])
    \\    a[-1] = {a = [77]
    \\        5}
    \\    @println(a[0], a[-1], a.len)
    \\    let mut n: int = 0
    \\    loop n < 4 {
    \\        n += 1
    \\        a[0] *= if n == 1 {a = [6, 7]
    \\            continue} else if n == 3 {a[0] = 9
    \\            break} else {let temp: []int = [2]
    \\            temp[0]}
    \\    }
    \\    @println(n, a[0], a[-1])
    \\    loop true {a[{break}] += 1}
    \\    loop n < 5 {n += 1
    \\        a[{continue}] -= 1}
    \\    @println(a[0], n, early(true), early(false), index_return())
    \\}
;

const array_compound_output = "19010 23 2 20 1\n12 23\n12 5 2\n3 9 7\n9 5 88 12 7\n";

test "array compound stores share one pre-index snapshot and skip stores on terminal paths" {
    const c = try arrayTestProgram(array_compound_effects);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, array_compound_output, "", mode);
}

test "array compound bounds precede RHS while plain replacement bounds follow RHS" {
    for ([_][]const u8{ "0", "1", "-2", if (Ir.native_pointer_bits == 64) "-9223372036854775808" else "-2147483648" }) |index| {
        for ([_][]const u8{ "=", "+=", "-=", "*=", "/=", "%=" }) |op| {
            const source = try std.fmt.allocPrint(test_gpa, "let main: () -> void = {{let mut a: []int = {s}\na[{{@print(true)\n0 + {s}}}] {s} {{@print(false)\nreturn}}}}", .{ if (std.mem.eql(u8, index, "0")) "[]" else "[1]", index, op });
            defer test_gpa.free(source);
            const c = try arrayTestProgram(source);
            defer test_gpa.free(c);
            const plain = std.mem.eql(u8, op, "=");
            for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", if (plain) 0 else 1, if (plain) "truefalse" else "true", if (plain) "" else "panic: array index out of bounds\n", mode);
        }
    }
    const plain = try arrayTestProgram("let main: () -> void = {let mut a: []int = []\na[0] = {@print(true)\n1}}");
    defer test_gpa.free(plain);
    try compileAndRunExpected(plain, "typedef int nonempty_translation_unit;", 1, "true", "panic: array index out of bounds\n");
}

test "array compound division and remainder by zero panic after RHS effects" {
    for ([_][]const u8{ "/=", "%=" }) |op| {
        const source = try std.fmt.allocPrint(test_gpa, "let main: () -> void = {{let mut a: []i8 = [7]\na[-1] {s} {{a = []\n@print(true)\n0}}}}", .{op});
        defer test_gpa.free(source);
        const c = try arrayTestProgram(source);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "true", if (std.mem.eql(u8, op, "/=")) "panic: division by zero\n" else "panic: remainder by zero\n", mode);
    }
}

test "arrays support every approved integer element type" {
    var source = std.Io.Writer.Allocating.init(test_gpa);
    defer source.deinit();
    try source.writer.writeAll("let main: () -> void = {\n");
    for ([_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "int", "uint" }) |name| {
        try source.writer.print("{{let mut a: []{s} = [0, 1,]\na[0] = 2\na += []\n@println(a[0], a[-1], a.len)}}\n", .{name});
    }
    try source.writer.writeAll("}");
    const c = try arrayTestProgram(source.written());
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, "2 1 2\n" ** 10, "");
}

test "array evaluation snapshots arguments and evaluates allocating bases and indices once" {
    const c = try arrayTestProgram(
        \\let make: () -> []int = {@print(true)
        \\    [10, 20]}
        \\let index: () -> int = {let temporary: []int = [0]
        \\    @print(false)
        \\    temporary[0]}
        \\let pair: (a: []int, b: []int) -> int = a[0] + b[0]
        \\let main: () -> void = {
        \\    @println(make()[index()])
        \\    let mut a: []int = [1]
        \\    @println(pair(a, {a[0] = 99
        \\        [2]}), a[0])
        \\    a[index()] = {let scratch: []bool = [true]
        \\        @print(scratch[0])
        \\        5}
        \\    @println(a[0])
        \\}
    );
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, "truefalse10\n3 99\nfalsetrue5\n", "");
}

test "array roots survive nested loops conditional merges early returns and argument allocations" {
    const c = try arrayTestProgram(
        \\let choose: (a: []int, c: bool) -> []int = {
        \\    let mut n: int = 0
        \\    loop {let checks: []bool = [n < 4]
        \\        checks[0]} {
        \\        let fresh: []int = if c {a} else {[8]}
        \\        n += 1
        \\        if n == 1 {continue}
        \\        loop true {
        \\            let scratch: []bool = [true]
        \\            if scratch[0] {break}
        \\        }
        \\        if n == 3 {return fresh}
        \\    }
        \\    []
        \\}
        \\let pair: (a: []int, b: []int) -> int = a[-1] + b[-1]
        \\let main: () -> void = {
        \\    let mut a: []int = [1, 2]
        \\    let mut b: []int = []
        \\    b = a
        \\    a[0] = 7
        \\    b[1] = 9
        \\    @println(a[0], a[1], b[0], b[1])
        \\    @println(pair(choose(a, true), choose(b, false)))
        \\    let result: []int = choose(a, true)
        \\    a = []
        \\    @println(result[-1], a.len)
        \\    let skipped: bool = false and {let unused: []int = [99]
        \\        unused[0] == 99}
        \\    @println(skipped)
        \\    loop {let stop: []bool = [true]
        \\        if stop[0] {break}
        \\        false} {}
        \\}
    );
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, "7 2 1 9\n10\n2 0\nfalse\n", "");
}

test "array negative bounds and minimum native index panic identically in both modes" {
    for ([_][]const u8{ "0", "1", "-2", if (Ir.native_pointer_bits == 64) "-9223372036854775808" else "-2147483648" }) |index| {
        const source = try std.fmt.allocPrint(test_gpa, "let main: () -> void = {{let a: []bool = {s}\n@println(a[{s}])}}", .{ if (std.mem.eql(u8, index, "0")) "[]" else "[true]", index });
        defer test_gpa.free(source);
        const c = try arrayTestProgram(source);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "", "panic: array index out of bounds\n", mode);
    }
}

test "array capacity and append length overflow panic before accessing backing" {
    const library = try emitSource("let anchor: (unused: []bool) -> void = {}", null);
    defer test_gpa.free(library);
    for ([_][]const u8{
        "(void)nov_array_new(UINTPTR_MAX, UINTPTR_MAX, sizeof(uint64_t), _Alignof(uint64_t));",
        "(void)nov_array_append((nov_array){NULL, UINTPTR_MAX, UINTPTR_MAX}, (nov_array){NULL, 1, 1}, 1, 1);",
    }) |operation| {
        const c = try std.fmt.allocPrint(test_gpa, "{s}\nint main(void) {{{s} return 99;}}\n", .{ library, operation });
        defer test_gpa.free(c);
        try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 1, "", "panic: array size overflow\n");
    }
}

test "array allocation failure retries preserve roots and shutdown frees backing" {
    for ([_][]const u8{ array_example, array_compound_effects, inferred_example, inferred_arrays, string_example, string_compound, string_equality, unconditional_loops, else_loops, array_membership, string_interpolation }, [_][]const u8{ "3 30 30 6\n454 15\n99 15 4\nfalse true 2\n0\n", array_compound_output, inferred_output, inferred_arrays_output, string_output, string_compound_output, string_equality_output, unconditional_output, else_loop_output, array_membership_output, string_interpolation_output }) |source, output| {
        const c = try arrayTestProgram(source);
        defer test_gpa.free(c);
        const injected = try std.mem.concat(test_gpa, u8, &.{
            "#include <stdlib.h>\n#include <assert.h>\nstatic unsigned attempts, outstanding;\n" ++
                "static void *test_malloc(size_t n) { void *p; if (++attempts % 2) return NULL; p = malloc(n); if (p) ++outstanding; return p; }\n" ++
                "static void test_free(void *p) { assert(outstanding); --outstanding; free(p); }\n" ++
                "#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE test_free\n#define main test_main\n",
            c,
            "\n#undef main\nint main(void) { int result = test_main(); assert(!outstanding); return result; }\n",
        });
        defer test_gpa.free(injected);
        for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(injected, "typedef int nonempty_translation_unit;", 0, output, "", mode);
        const exhausted = try std.mem.concat(test_gpa, u8, &.{
            "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n" ++
                "#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n",
            c,
        });
        defer test_gpa.free(exhausted);
        for ([_][]const u8{ "-O0", "-O2" }) |mode| try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", 1, if (std.mem.eql(u8, source, inferred_example)) "7\n8\n" else if (std.mem.eql(u8, source, inferred_arrays)) "7\n" else if (std.mem.eql(u8, source, else_loops)) "false true false\n42 42 5\nexhausted 5 3 3\ncondition continuation body\n10 11 12 14\n3 7\n31 32 3\ntrue\n" else "", "panic: GC allocation failed\n", mode);
    }
}

fn testArrayEmissionAllocationFailures(allocator: std.mem.Allocator, source: []const u8) !void {
    var module = try Module.init(allocator, "arrays.nov", source);
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    if (module.hasErrors()) {
        for (module.diagnostics.items) |err| std.debug.print("{s} at {d}: {s}\n", .{ @tagName(err.tag), err.span.start, module.sourceSlice(err.span) orelse "<invalid span>" });
    }
    try std.testing.expect(!module.hasErrors());
    const c = try emitProgram(allocator, &sema.ir, null);
    defer allocator.free(c);
}

test "array compiler ownership survives every allocation failure" {
    for ([_][]const u8{ array_example, array_compound_effects, inferred_example, inferred_arrays }) |source| try std.testing.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{source});
}

const continuation_loops =
    \\let early: () -> int = {
    \\    loop true : {@println("bad return continuation")} {return 7}
    \\    0
    \\}
    \\let cont_return: () -> int = {
    \\    loop true : {return 8} {continue}
    \\    0
    \\}
    \\let main: () -> void = {
    \\    let mut checks: int = 0
    \\    let mut steps: int = 0
    \\    let mut bodies: int = 0
    \\    loop {checks += 1
    \\        if checks > 10 {return}
    \\        let skipped = checks == 1 and {continue}
    \\        checks < 5} : {steps += 1
    \\        if steps > 10 {return}
    \\        let skipped = steps == 1 and {continue}
    \\        @print(steps)} {
    \\        bodies += 1
    \\        let mut inner: int = 0
    \\        loop inner < 2 : inner += 1 {
    \\            if inner == 0 {continue}
    \\            break
    \\        }
    \\        let mut rhs: int = 0
    \\        rhs += if bodies == 1 {continue} else {10}
    \\        @print(rhs)
    \\    }
    \\    @println(checks, steps, bodies)
    \\    loop true : if true {@println("bad break continuation")} {break}
    \\    @println(early(), cont_return())
    \\    let mut n: int = 0
    \\    loop true : {n += 1
    \\        if n > 10 {return}
    \\        if n == 2 {break}} {continue}
    \\    @println(n)
    \\    loop {n += 1
    \\        if n > 10 {return}
    \\        if n == 3 {continue}
    \\        if n == 4 {break}
    \\        true} : {@println("bad condition continuation")} {@println("bad condition body")}
    \\    @println(n)
    \\}
;

const continuation_output = "1021035 3 3\n7 8\n2\n4\n";

test "continuation loops route nested branch short circuit and RHS controls in strict C11" {
    const c = try arrayTestProgram(continuation_loops);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, continuation_output, "", mode);
}

const else_loops =
    \\let plus: (n: int) -> int = n + 1
    \\let search: (end: int, needle: int) -> bool = {
    \\    let mut n: int = 0
    \\    loop n < end : n += 1 {if n == needle {break true}} else {false}
    \\}
    \\let early: (where: int) -> int = {
    \\    loop {if where == 0 {return 10}
    \\        true} : {if where == 2 {return 12}} {
    \\        if where == 1 {return 11}
    \\        if where == 2 {continue}
    \\        break
    \\    } else {return 99}
    \\    13
    \\}
    \\let fallback_return: () -> int = loop false {} else {return 14}
    \\let choose: (take: bool) -> []int = {
    \\    let mut a: []int = [3, 4]
    \\    let mut result = loop take {break a} else {a}
    \\    a[0] = 9
    \\    a += [5]
    \\    result[-1] = 8
    \\    @println(a[0], a[-1], a.len, result[0], result[-1])
    \\    return loop false {} else {result}
    \\}
    \\let join: (a: string, b: string, c: string) -> string = a + b + c
    \\let main: () -> void = {
    \\    @println(search(0, 0), search(3, 2), search(2, 2))
    \\    let n: i8 = 42
    \\    let from_else = loop false {break 1} else {n}
    \\    let from_break = loop true {break n} else {2}
    \\    let callable = loop false {break plus} else {plus}
    \\    @println(from_else, from_break, callable(4))
    \\    let mut checks: int = 0
    \\    let mut steps: int = 0
    \\    let mut bodies: int = 0
    \\    let exhaustion = loop {checks += 1
    \\        if checks == 1 {continue}
    \\        checks < 5} : {steps += 1
    \\        if steps == 1 {continue}} {
    \\        bodies += 1
    \\        continue
    \\    } else {"exhausted"}
    \\    @println(exhaustion, checks, steps, bodies)
    \\    let a = loop {break "condition"
    \\        } : {@println("bad continuation")} {} else {"bad else"}
    \\    let b = loop true : {break "continuation"} {continue} else {"bad else"}
    \\    let c = loop true {break "body"} else {"bad else"}
    \\    @println(a, b, c)
    \\    @println(early(0), early(1), early(2), fallback_return())
    \\    let mut outer: int = 0
    \\    loop outer < 3 : outer += 1 {
    \\        loop false {} else {continue}
    \\    }
    \\    let result: int = loop : {@println("bad outer continuation")} {
    \\        loop false {} else {break 7}
    \\    }
    \\    @println(outer, result)
    \\    let mut phase: int = 0
    \\    let condition_parent: int = loop {phase += 1
    \\        if phase == 1 {loop false {} else {continue}}
    \\        if phase == 3 {loop false {} else {break 31}}
    \\        true} : {if phase == 2 {loop false {} else {continue}}} {} else {0}
    \\    let continuation_parent: int = loop phase < 4 : {
    \\        loop false {} else {break 32}
    \\    } {} else {0}
    \\    @println(condition_parent, continuation_parent, phase)
    \\    let nested = loop false {} else {loop true {break true} else {false}}
    \\    @println(nested)
    \\    let x = choose(true)
    \\    let y = choose(false)
    \\    let pressure: []int = [99]
    \\    @println(x[0], x[-1], y[0], y[-1], pressure.len)
    \\    let mut text = "a" + "b"
    \\    let shared = loop false {} else {text}
    \\    text += "c"
    \\    @println(join("L" + "!", loop false {} else {shared + "!"}, "R" + "!"), text, shared)
    \\}
;
const else_loop_output = "false true false\n42 42 5\nexhausted 5 3 3\ncondition continuation body\n10 11 12 14\n3 7\n31 32 3\ntrue\n9 5 3 3 8\n9 5 3 3 8\n3 8 3 8 1\nL!ab!R! abc ab\n";

test "break operands infer owner target controls before their eventual values" {
    for ([_][]const u8{
        "loop false {} else {break n}",
        "{break n}",
        "1 + {break n}",
        "identity({break n})",
        "{let inner = loop false {break wide} else {wide}\nbreak n}",
    }) |operand| {
        const source = try std.fmt.allocPrint(
            test_gpa,
            "let identity: (n: i8) -> i8 = n\nlet main: () -> void = {{let n: i8 = 7\nlet wide: i16 = 8\nlet x = loop true {{if false {{break 1}}\nbreak {s}}} else {{2}}\n@println(x)}}",
            .{operand},
        );
        defer test_gpa.free(source);
        const c = try emitSource(source, null);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O2" }) |mode|
            try compileAndRunMode(c, "void nov_main(void);\nint main(void) { nov_main(); return 0; }", 0, "7\n", "", mode);
    }
}

test "conditional loop else results exhaust break return and target parent controls" {
    const c = try arrayTestProgram(else_loops);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, else_loop_output, "", mode);
}

const value_loops =
    \\let plus: (n: int) -> int = n + 1
    \\let join: (a: string, b: string, c: string) -> string = a + b + c
    \\let build_array: () -> []int = {
    \\    let mut source: []int = [3, 4]
    \\    let result: []int = loop : source += [99] {
    \\        break {let snapshot = source
    \\            source[0] = 8
    \\            source += [5]
    \\            snapshot}
    \\    }
    \\    source[0] = 9
    \\    @println(source[0], source.len)
    \\    return loop {break result}
    \\}
    \\let array_rhs: () -> []int = {
    \\    let mut source: []int = [11, 12]
    \\    let result: []int = loop {source += {break source}}
    \\    source[0] = 99
    \\    source += [13]
    \\    @println(source[0], source.len)
    \\    result
    \\}
    \\let text: () -> string = {
    \\    let mut source = "a" + "b"
    \\    let result: string = loop {break source + {source = "c" + "d"
    \\        "e" + "f"}}
    \\    @println(source)
    \\    return loop {break result}
    \\}
    \\let main: () -> void = {
    \\    let flag: bool = loop {break true}
    \\    let number: int = loop {break loop {break 42}}
    \\    let callable: (int) -> int = loop {break plus}
    \\    @println(flag, number, callable(4))
    \\    let mut source: []int = [1, 2]
    \\    let mut copy: []int = loop {if flag {break source} else {break [99]}}
    \\    source[0] = 7
    \\    copy[-1] = 8
    \\    @println(source[0], source[-1], copy[0], copy[-1])
    \\    let result = build_array()
    \\    let message = text()
    \\    let shared: string = loop {break message}
    \\    let pressure: []int = [10, 20]
    \\    @println(result[0], result[1], result[-1], result.len, pressure.len)
    \\    @println(message, shared)
    \\    let in_rhs: int = 1 + loop {break 2}
    \\    let short = false or loop {break true}
    \\    @println(in_rhs, short)
    \\    let from_rhs = array_rhs()
    \\    @println(from_rhs[0], from_rhs[-1], from_rhs.len)
    \\    let mut ticks: int = 0
    \\    let from_cont: string = loop : {ticks += 1
    \\        let fresh = "x" + "y"
    \\        if ticks > 10 {return}
    \\        if ticks == 1 {continue}
    \\        break fresh + "z"} {continue}
    \\    let cont_array: []int = loop : if true {break [21, 22]} {continue}
    \\    @println(from_cont, ticks, cont_array[-1])
    \\    let mut attempts: int = 0
    \\    let nested = join("L" + "!", loop : {
    \\        break loop : {break "nest" + "ed"} {
    \\            attempts += 1
    \\            if attempts > 10 {return}
    \\            continue
    \\        }
    \\    } {
    \\        attempts += 1
    \\        if attempts > 10 {return}
    \\        continue
    \\    }, "R" + "!")
    \\    @println(nested, attempts)
    \\}
;

const value_loop_output = "true 42 5\n7 2 1 8\n9 3\ncd\n3 4 4 2 2\nabef abef\n3 true\n99 3\n11 12 2\nxyz 2 22\nL!nestedR! 2\n";

test "value loops merge scalar function and managed results with every allocation collecting" {
    const c = try arrayTestProgram(value_loops);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, value_loop_output, "", mode);
}

test "loop exhaustion strings share backing without allocation and terminal fallbacks pop roots" {
    const library = try emitSource(
        \\let make: () -> string = "a" + "b"
        \\let share: (s: string) -> string = loop false {} else {s}
        \\let early: (s: string) -> string = loop false {} else {return s}
    , null);
    defer test_gpa.free(library);
    const c = try std.mem.concat(test_gpa, u8, &.{
        library,
        "\nint main(void) { nov_rt_heap.collect_every_allocation = 1; nov_string s = nov_make(); nov_string t = nov_share(s); " ++
            "nov_string u = nov_early(t); if (!s.owner || s.owner != t.owner || s.owner != u.owner || s.data != u.data) return 1; " ++
            "if (nov_rt_heap.live_objects != 1 || nov_rt_heap.frames) return 2; nov_gc_shutdown(&nov_rt_heap); return 0; }\n",
    });
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "", "", mode);
}

test "value loop string breaks share backing without allocating" {
    const library = try emitSource(
        \\let make: () -> string = loop {break "a" + "b"}
        \\let share: (s: string) -> string = loop : if true {break s} {continue}
    , null);
    defer test_gpa.free(library);
    const c = try std.mem.concat(test_gpa, u8, &.{
        library,
        "\nint main(void) { nov_rt_heap.collect_every_allocation = 1; nov_string s = nov_make(); nov_string t = nov_share(s); " ++
            "if (!s.owner || s.owner != t.owner || s.data != t.data || s.len != t.len) return 1; " ++
            "if (nov_rt_heap.live_objects != 1 || nov_rt_heap.frames) return 2; " ++
            "nov_gc_collect(&nov_rt_heap); if (nov_rt_heap.live_objects) return 3; nov_gc_shutdown(&nov_rt_heap); return 0; }\n",
    });
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O2" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "", "", mode);
}

test "loop continuations and value results execute before formatting" {
    for ([_][]const u8{ continuation_loops, value_loops, else_loops }, [_][]const u8{ continuation_output, value_loop_output, else_loop_output }) |source, output| {
        const library = try emitSource(source, null);
        defer test_gpa.free(library);
        const c = try collectingTestProgram(library);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O2" }) |mode|
            try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, output, "", mode);
    }
}

test "continuation and value loop lowering and emission survive every allocation failure" {
    for ([_][]const u8{ continuation_loops, value_loops, else_loops }) |source|
        try std.testing.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{source});
}

const unconditional_loops =
    \\let build: (early: bool) -> string = {
    \\    let mut text = ""
    \\    let mut values: []int = []
    \\    let mut n: int = 0
    \\    loop {
    \\        let fresh = "a" + "b"
    \\        values += [n]
    \\        n += 1
    \\        loop n < 3 {loop {break}
    \\            break}
    \\        if n == 1 {continue}
    \\        text += fresh
    \\        if early {return text}
    \\        if n == 3 {break}
    \\    }
    \\    @println(values.len, values[-1])
    \\    text
    \\}
    \\let exit: () -> void = loop {return}
    \\let dead: () -> void = {loop {continue}
    \\    @println("not called")}
    \\let main: () -> void = {exit()
    \\    @println(build(true))
    \\    @println(build(false))
    \\    loop {true} {loop {break}
    \\        break}}
;
const unconditional_output = "ab\n3 2\nabab\n";

test "unconditional loops execute finite controls returns and managed roots after formatting" {
    const formatted = try @import("Formatter.zig").format(test_gpa, unconditional_loops);
    defer test_gpa.free(formatted);
    for ([_][]const u8{ unconditional_loops, formatted }) |source| {
        const c = try arrayTestProgram(source);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O2" }) |mode|
            try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, unconditional_output, "", mode);
    }
}

test "condition loops repeat current reads and scope initialization with innermost controls" {
    const c = try emitSource(
        \\let mark: (n: int) -> int = {@print(n)
        \\    n}
        \\let out: () -> void = {
        \\    let mut n: int = 0
        \\    loop false {@println(mark(99))}
        \\    loop mark(n) < 4 and {n += 1
        \\        true} {
        \\        let mut fresh: int = mark(n)
        \\        fresh += 10
        \\        if n == 2 {continue}
        \\        let mut j: int = 0
        \\        loop j < 3 {
        \\            j += 1
        \\            if j == 1 {continue}
        \\            if j == 3 {break}
        \\            @print(j)
        \\        }
        \\        @println(fresh)
        \\        if n == 3 {break}
        \\    }
        \\    @println(n)
        \\    loop n < 5 {n += 1}
        \\    @println(n)
        \\    loop false {let fresh: int = 99
        \\        @println(fresh)}
        \\}
    , null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "void nov_out(void); int main(void) { nov_out(); return 0; }", 0, "01211\n1223213\n3\n5\n", "");
}

test "loop conditions execute once per attempt including zero and final false" {
    const c = try emitSource(
        \\let out: () -> void = {
        \\    let mut n: int = 0
        \\    let mut attempts: int = 0
        \\    loop {attempts += 1
        \\        @print(attempts)
        \\        n < 2} {n += 1
        \\        @print(n)}
        \\    @println(attempts, n)
        \\    loop {attempts += 1
        \\        false} {let bad: int = 1 / 0
        \\        @println(bad)}
        \\    @println(attempts)
        \\}
    , null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "void nov_out(void); int main(void) { nov_out(); return 0; }", 0, "112233 2\n4\n", "");
}

test "loop exits compose with value branches assignments calls short circuit and returns" {
    const c = try emitSource(
        \\let sum: (a: int, b: int) -> int = a + b
        \\let early: (stop: bool) -> int = {
        \\    let mut n: int = 0
        \\    loop n < 4 {
        \\        n += 1
        \\        let b: bool = false and {break}
        \\        let d: bool = true or {continue}
        \\        if b or !d {return 99}
        \\        n += if n == 1 {continue} else {0}
        \\        let value: int = if n == 2 {if stop {return n} else {continue}} else {n}
        \\        @println(value)
        \\        let ignored: int = sum({if n == 3 {break}
        \\            n}, {return 88})
        \\    }
        \\    n
        \\}
        \\let condition: () -> int = {loop {return 7} {}
        \\    99}
        \\let control: () -> void = {
        \\    let mut n: int = 0
        \\    loop {n += 1
        \\        if n == 1 {continue}
        \\        if n == 3 {break}
        \\        true} {@println(n)}
        \\    @println(n)
        \\}
        \\let out: () -> void = {@println(early(true), early(false), condition())
        \\    control()}
    , null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "void nov_out(void); int main(void) { nov_out(); return 0; }", 0, "3\n2 3 7\n2\n3\n", "");
}

test "loop conditions short circuit panics and skipped bodies stay conditional" {
    const c = try emitSource(
        \\let f: (take: bool) -> void = {
        \\    let mut n: int = 0
        \\    loop n < 2 and {n += 1
        \\        if take {let bad: int = 1 / 0
        \\            @println(bad)}
        \\        true} {
        \\        @println(n)
        \\    }
        \\    loop false {let bad: int = 1 / 0
        \\        @println(bad)}
        \\}
        \\let bodypanic: () -> void = {loop true {let bad: int = 1 % 0
        \\    @println(bad)}}
    , null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "#include <stdbool.h>\nvoid nov_f(bool); int main(void) { nov_f(false); return 0; }", 0, "1\n2\n", "");
    try compileAndRunExpected(c, "#include <stdbool.h>\nvoid nov_f(bool); int main(void) { nov_f(true); return 0; }", 1, "", "panic: division by zero\n");
    try compileAndRunExpected(c, "void nov_bodypanic(void); int main(void) { nov_bodypanic(); return 0; }", 1, "", "panic: remainder by zero\n");
}

test "mutable locals snapshot reads and preserve conditional sequential effects" {
    const c = try emitSource(
        \\let mark: (n: int) -> int = {@print(n)
        \\    n}
        \\let id: (n: int) -> int = n
        \\let next: (n: int) -> int = n + 1
        \\let out: () -> void = {
        \\    let mut n: int = mark(1)
        \\    let saved: int = n
        \\    @println(n, {n = mark(2)
        \\        n}, n, saved)
        \\    n += {n = mark(10)
        \\        mark(3)}
        \\    @println(n)
        \\    if true {n *= 2} else {n = 99}
        \\    if false {n = 99} else if true {n -= 1}
        \\    @println(n)
        \\    let mut b: bool = false
        \\    @println(false and {n = 99
        \\        true}, true or {n = 99
        \\        false}, true and {n += 1
        \\        b = true
        \\        b}, b, n)
        \\    let mut callback: (int) -> int = id
        \\    @println(callback({callback = next
        \\        20}), callback(20))
        \\    {let mut local: int = 30
        \\        local += 1
        \\        n = local}
        \\    {let mut local: int = 40
        \\        n += local}
        \\    @println(n)
        \\    let unit: void = {n = 8}
        \\    let mut unused: int = 0
        \\    unused = 1
        \\    @println(n)
        \\}
    , null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "void nov_out(void); int main(void) { nov_out(); return 0; }", 0, "121 2 2 1\n1035\n9\nfalse true true true 10\n20 21\n71\n8\n", "");
}

test "compound assignments reuse wrapping division and remainder semantics" {
    const c = try emitSource(
        \\let out: () -> void = {
        \\    let mut s: i8 = 127
        \\    s += 1
        \\    @println(s)
        \\    s /= -1
        \\    @println(s)
        \\    s %= -1
        \\    s -= 1
        \\    s *= 3
        \\    @println(s)
        \\    let mut u: u8 = 255
        \\    u += 1
        \\    u -= 1
        \\    u *= 2
        \\    u /= 2
        \\    u %= 10
        \\    @println(u)
        \\}
        \\let divide: () -> void = {let mut n: int = 1
        \\    n /= {@print(true)
        \\        0}}
        \\let remainder: () -> void = {let mut n: int = 1
        \\    n %= 0}
    , null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "void nov_out(void); int main(void) { nov_out(); return 0; }", 0, "-128\n-128\n-3\n7\n", "");
    try compileAndRunExpected(c, "void nov_divide(void); int main(void) { nov_divide(); return 0; }", 1, "true", "panic: division by zero\n");
    try compileAndRunExpected(c, "void nov_remainder(void); int main(void) { nov_remainder(); return 0; }", 1, "", "panic: remainder by zero\n");
}

test "assignment and initializer returns skip stores and subsequent effects" {
    const c = try emitSource(
        \\let early: (c: bool) -> int = {
        \\    let mut n: int = 1
        \\    n += if c {n = 4
        \\        return n} else {n = 8
        \\        2}
        \\    return n
        \\}
        \\let abort: () -> int = {let mut n: int = 1
        \\    n = {return n}}
        \\let init: () -> int = {let mut n: int = {return 7}
        \\}
        \\let short: (c: bool) -> int = {let mut n: int = 1
        \\    let b: bool = c and {n += 1
        \\        return n}
        \\    if b {n = 99}
        \\    n}
        \\let out: () -> void = @println(early(true), early(false), abort(), init(), short(true), short(false))
    , null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "void nov_out(void); int main(void) { nov_out(); return 0; }", 0, "4 3 1 7 2 1\n", "");
}

fn testLoopAllocationFailures(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "loops.nov",
        \\let f: (stop: bool) -> int = {let mut n: int = 0
        \\    loop n < 3 and {if stop {return n} else {true}} {
        \\        let mut fresh: int = n
        \\        n += 1
        \\        loop fresh < 2 {fresh += 1
        \\            if stop {continue} else {break}}
        \\        if stop {break} else {continue}
        \\    }
        \\    n}
        \\let main: () -> void = @println(f(true), f(false))
    );
    var result = Lowering.lower(&module) catch |err| {
        module.deinit();
        return err;
    };
    defer result.deinit();
    const failed = module.hasErrors();
    module.deinit();
    try std.testing.expect(!failed);
    try std.testing.expect(result.lowering_complete);
    try result.ir.verify();
    const c = try emitProgram(allocator, &result.ir, null);
    defer allocator.free(c);
}

test "loop source IR and C emission ownership survive every allocation failure" {
    try std.testing.checkAllAllocationFailures(test_gpa, testLoopAllocationFailures, .{});
    try std.testing.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{unconditional_loops});
}

fn testMutableAllocationFailures(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "mutable.nov",
        \\let id: (n: int) -> int = n
        \\let f: (c: bool) -> int = {let mut n: int = id(1)
        \\    let mut callback: (int) -> int = id
        \\    let b: bool = c and {n += callback(n)
        \\        true}
        \\    n += if b {return n} else {callback = id
        \\        callback(n)}
        \\    n}
        \\let main: () -> void = @println(f(true), f(false))
    );
    var result = Lowering.lower(&module) catch |err| {
        module.deinit();
        return err;
    };
    defer result.deinit();
    const failed = module.hasErrors();
    module.deinit();
    try std.testing.expect(!failed);
    try std.testing.expect(result.lowering_complete);
    try result.ir.verify();
    const c = try emitProgram(allocator, &result.ir, null);
    defer allocator.free(c);
}

test "mutable slot ownership lowering and emission survive every allocation failure" {
    try std.testing.checkAllAllocationFailures(test_gpa, testMutableAllocationFailures, .{});
}

test "early returns preserve effects fallbacks nested merges and function values" {
    const c = try emitSource(
        \\let mark: (n: int) -> int = {@print(n)
        \\    n}
        \\let add: (a: int, b: int) -> int = a + b
        \\let fallback: (c: bool) -> int = {
        \\    @print(mark(1))
        \\    if c {{return mark(2)}}
        \\    mark(3)
        \\}
        \\let mixed: (c: bool) -> int = if c {return mark(4)} else {mark(5)}
        \\let reverse: (c: bool) -> int = if c {mark(6)} else {return mark(7)}
        \\let all: (c: bool) -> int = if c {if c {return mark(8)} else {return mark(9)}} else {return mark(10)}
        \\let abortargs: () -> int = add(mark(11), {return mark(12)})
        \\let abortprint: () -> int = @println(mark(13), {return mark(14)}, mark(15))
        \\let abortinit: () -> int = {let unused: bool = {return mark(16)}
        \\}
        \\let choose: () -> (int) -> int = return mark
        \\let unit: (c: bool) -> void = {if c {return @print(true)}
        \\    @print(false)}
        \\let out: () -> void = {
        \\    @println(fallback(true), fallback(false))
        \\    @println(mixed(true), mixed(false), reverse(true), reverse(false))
        \\    @println(all(true), all(false), abortargs(), abortprint(), abortinit())
        \\    @println(choose()(17))
        \\    unit(true)
        \\    unit(false)
        \\    return @println()
        \\}
    , null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "void nov_out(void); int main(void) { nov_out(); return 0; }", 0, "1121132 3\n45674 5 6 7\n81011121314168 10 12 14 16\n1717\ntruefalse\n", "");
}

test "returning short circuit regions skip effects and merge continuing booleans" {
    const c = try emitSource(
        \\let mark: (n: int) -> int = {@print(n)
        \\    n}
        \\let a: (c: bool) -> int = {let b: bool = c and {return mark(1)}
        \\    if b {return 2}
        \\    mark(3)}
        \\let o: (c: bool) -> int = {let b: bool = c or {return mark(4)}
        \\    if b {return mark(5)}
        \\    6}
        \\let nested: (c: bool) -> bool = c and {if c {return false} else {true or {return false}}}
        \\let out: () -> void = {
        \\    @println(a(true), a(false), o(true), o(false))
        \\    @println(nested(true), nested(false))
        \\    return
        \\}
    , null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "void nov_out(void); int main(void) { nov_out(); return 0; }", 0, "13541 3 5 4\nfalse false\n", "");
}

test "returns skip fallback panics but evaluate returned expressions" {
    const c = try emitSource(
        \\let divide: (n: int) -> int = 1 / n
        \\let early: (c: bool) -> int = {if c {return 42}
        \\    divide(0)}
        \\let panic: () -> int = return {@print(true)
        \\    divide(0)}
        \\let skip: () -> void = @println(early(true), false and {return}, true or {return})
    , null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "void nov_skip(void); int main(void) { nov_skip(); return 0; }", 0, "42 false true\n", "");
    try compileAndRunExpected(c, "#include <stdint.h>\n#include <stdbool.h>\nint64_t nov_early(bool); int main(void) { (void)nov_early(false); return 0; }", 1, "", "panic: division by zero\n");
    try compileAndRunExpected(c, "#include <stdint.h>\nint64_t nov_panic(void); int main(void) { (void)nov_panic(); return 0; }", 1, "true", "panic: division by zero\n");
}

fn testReturnAllocationFailures(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "return.nov",
        \\let f: (c: bool) -> int = {
        \\    let b: bool = c and {if c {return 1} else {return 2}}
        \\    if b {return 3}
        \\    return 4
        \\}
        \\let main: () -> void = {if true {return @println(f(true))}
        \\    @println(f(false))}
    );
    var result = Lowering.lower(&module) catch |err| {
        module.deinit();
        return err;
    };
    defer result.deinit();
    const failed = module.hasErrors();
    module.deinit();
    try std.testing.expect(!failed);
    try std.testing.expect(result.lowering_complete);
    try result.ir.verify();
    const c = try emitProgram(allocator, &result.ir, null);
    defer allocator.free(c);
}

test "return ownership and C emission survive every allocation failure" {
    try std.testing.checkAllAllocationFailures(test_gpa, testReturnAllocationFailures, .{});
}

test "if branches execute selected effects once with nesting and function results" {
    const c = try emitSource(
        \\let mark: (n: int, value: bool) -> bool = {@print(n)
        \\    value}
        \\let id: (n: int) -> int = n
        \\let other: (n: int) -> int = n + 1
        \\let out: () -> void = {
        \\    let outer: int = 10
        \\    @println(if mark(1, true) {let local: int = outer + 1
        \\        @print(local)
        \\        local} else {let local: int = outer + 2
        \\        @print(local)
        \\        local}, if mark(2, false) {@print(false)
        \\        outer} else {@print(true)
        \\        outer + 2})
        \\    @println(if mark(3, false) {false} else if mark(4, true) {true} else {false})
        \\    @println(if mark(5, true) {if mark(6, false) {false} else {true}} else {false})
        \\    @println(false and {if mark(7, true) {true} else {false}}, if true {false or mark(8, true)} else {false})
        \\    let callback: (int) -> int = if false {id} else {other}
        \\    @println(callback(20))
        \\    if false {@print(false)}
        \\    if true {@print(true)}
        \\    if true {} else {}
        \\    let unused: int = if false {outer} else {outer}
        \\    @println(if true {outer} else {outer})
        \\}
    , null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "void nov_out(void); int main(void) { nov_out(); return 0; }", 0, "1112true11 12\n34true\n56true\n8false true\n21\ntrue10\n", "");
}

test "if skips unselected panics and propagates taken branch panics" {
    const c = try emitSource(
        \\let divide: (x: int) -> int = 1 / x
        \\let remainder: (x: int) -> int = 1 % x
        \\let skip: () -> void = {
        \\    @println(if true {1} else {divide(0)}, if false {remainder(0)} else {2})
        \\    if false {let unused: int = divide(0)
        \\    }
        \\}
        \\let takethen: () -> void = if true {@print(true)
        \\    let unused: int = divide(0)
        \\} else {}
        \\let takeelse: () -> void = if false {} else if false {} else {@print(false)
        \\    let unused: int = remainder(0)
        \\}
    , null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "void nov_skip(void); int main(void) { nov_skip(); return 0; }", 0, "1 2\n", "");
    try compileAndRunExpected(c, "void nov_takethen(void); int main(void) { nov_takethen(); return 0; }", 1, "true", "panic: division by zero\n");
    try compileAndRunExpected(c, "void nov_takeelse(void); int main(void) { nov_takeelse(); return 0; }", 1, "false", "panic: remainder by zero\n");
}

test "short circuit truth tables nested effects captures and argument order" {
    const c = try emitSource(
        \\let mark: (n: int, value: bool) -> bool = {
        \\    @print(n)
        \\    value
        \\}
        \\let choose: () -> (int, bool) -> bool = {
        \\    @print(true)
        \\    mark
        \\}
        \\let captures: (x: bool) -> bool = {
        \\    let outer: bool = x
        \\    outer and {outer or outer}
        \\}
        \\let out: () -> void = {
        \\    @println(true and true, true and false, false and true, false and false, true or true, true or false, false or true, false or false)
        \\    @println(mark(1, false) and mark(2, true), mark(3, true) and mark(4, false), mark(5, true) or mark(6, false), mark(7, false) or mark(8, true))
        \\    @println(mark(9, true) and {mark(10, false) or mark(11, true)})
        \\    @println({mark(12, false) and mark(13, true)} or mark(14, false))
        \\    @println(captures(false), captures(true))
        \\    @println(false and {let skipped: bool = choose()(15, true)
        \\        @print(skipped)
        \\        skipped})
        \\    @println(true and {let taken: bool = choose()(16, true)
        \\        @print(taken)
        \\        taken and true})
        \\    let unused: bool = false or mark(17, true)
        \\    @println(true or false and mark(18, true))
        \\    @println({true or false} == {false or true}, !{true and false})
        \\}
    , null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "void nov_out(void); int main(void) { nov_out(); return 0; }", 0, "true false false false true true true false\n134578false false true true\n91011true\n1214false\nfalse true\nfalse\ntrue16truetrue\n17true\ntrue true\n", "");
}

test "short circuit skips RHS panics and propagates taken RHS panics" {
    const c = try emitSource(
        \\let divide: (x: int) -> bool = 1 / x == 0
        \\let remainder: (x: int) -> bool = 1 % x == 0
        \\let skip: () -> void = @println(false and divide(0), true or remainder(0))
        \\let takeand: () -> bool = true and {@print(true)
        \\    divide(0)}
        \\let takeor: () -> bool = false or {@print(false)
        \\    remainder(0)}
    , null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "void nov_skip(void); int main(void) { nov_skip(); return 0; }", 0, "false true\n", "");
    try compileAndRunExpected(c, "#include <stdbool.h>\nbool nov_takeand(void); int main(void) { (void)nov_takeand(); return 0; }", 1, "true", "panic: division by zero\n");
    try compileAndRunExpected(c, "#include <stdbool.h>\nbool nov_takeor(void); int main(void) { (void)nov_takeor(); return 0; }", 1, "false", "panic: remainder by zero\n");
}

fn testShortCircuitAllocationFailures(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "short.nov",
        \\let id: (x: bool) -> bool = x
        \\let main: () -> void = {
        \\    let outer: bool = true
        \\    @println(outer and {let callback: (bool) -> bool = id
        \\        let inner: bool = callback(false) or outer
        \\        @println(inner)
        \\        if inner {outer} else if outer {id(false)} else {inner and outer}}, false or id(true))
        \\    if outer {let unit: void = @println(true)
        \\    }
        \\}
    );
    var result = Lowering.lower(&module) catch |err| {
        module.deinit();
        return err;
    };
    defer result.deinit();
    const failed = module.hasErrors();
    module.deinit();
    try std.testing.expect(!failed);
    try result.ir.verify();
    const c = try emitProgram(allocator, &result.ir, null);
    defer allocator.free(c);
}

test "nested conditional ownership and emission survive every allocation failure" {
    try std.testing.checkAllAllocationFailures(test_gpa, testShortCircuitAllocationFailures, .{});
}

test "local initializers statements and nested results execute in source order" {
    const c = try emitSource(
        \\let mark: (value: int) -> int = {
        \\    @print(value)
        \\    value
        \\}
        \\let add: (a: int, b: int) -> int = a + b
        \\let choose: () -> (int, int) -> int = {
        \\    @print(mark(8))
        \\    add
        \\}
        \\let out: () -> void = {
        \\    let unused: int = mark(1)
        \\    let first: int = mark(2)
        \\    let next: int = {
        \\        let scoped: int = mark(3)
        \\        @print(scoped)
        \\        scoped + first
        \\    }
        \\    {let scoped: int = mark(4)
        \\    }
        \\    let callback: (int, int) -> int = add
        \\    @println(first, first, next, callback(mark(5), mark(6)))
        \\    @println({@print(mark(7))
        \\        let last: bool = true
        \\        last})
        \\    let unit: void = @println()
        \\    @println(choose()(mark(9), mark(10)))
        \\    @println({let left: int = mark(11)
        \\        left} + mark(12))
        \\}
    , null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "void nov_out(void); int main(void) { nov_out(); return 0; }", 0, "12334562 2 5 11\n77true\n\n8891019\n111223\n", "");
}

test "native aliases lower arithmetic indirect calls comparisons and printing" {
    const c = try emitSource(
        "let negative: int = -42\nlet count: uint = 3\n" ++
            "let add: (x: int) -> int = x + 2\n" ++
            "let apply: (f: (int) -> int, x: int) -> int = f(x)\n" ++
            "let unsigned: (x: uint) -> uint = {x * 4 - 1} / 2\n" ++
            "let out: () -> void = @println(apply(add, negative), unsigned(count), count > 0)",
        null,
    );
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, native_target_guard) != null);
    try compileAndRunExpected(c, "void nov_out(void); int main(void) { nov_out(); return 0; }", 0, "-40 5 true\n", "");
}

test "emitted C rejects a mismatched native pointer width" {
    const c = try emitSource("let identity: (x: int) -> int = x", null);
    defer test_gpa.free(c);
    const start = std.mem.indexOf(u8, c, native_target_guard).?;
    const wrong_guard = try std.fmt.allocPrint(test_gpa, "#include <limits.h>\n_Static_assert(sizeof(void *) * CHAR_BIT == {d}, \"Nov requires the native target pointer width\");\n", .{Ir.native_pointer_bits + 1});
    defer test_gpa.free(wrong_guard);
    const wrong_c = try std.fmt.allocPrint(test_gpa, "{s}{s}{s}", .{ c[0..start], wrong_guard, c[start + native_target_guard.len ..] });
    defer test_gpa.free(wrong_c);
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const io = std.testing.io;
    try tmp.dir.writeFile(io, .{ .sub_path = "wrong.c", .data = wrong_c });
    const zig_exe = std.testing.environ.getAlloc(test_gpa, "NOV_TEST_ZIG") catch null;
    defer if (zig_exe) |path| test_gpa.free(path);
    const compiled = try std.process.run(test_gpa, io, .{
        .argv = if (zig_exe) |path| &.{ path, "cc", "-std=c11", "-fsyntax-only", "wrong.c" } else &.{ "cc", "-std=c11", "-fsyntax-only", "wrong.c" },
        .cwd = .{ .dir = tmp.dir },
    });
    defer test_gpa.free(compiled.stdout);
    defer test_gpa.free(compiled.stderr);
    try std.testing.expect(compiled.term == .exited and compiled.term.exited != 0);
    try std.testing.expect(std.mem.indexOf(u8, compiled.stderr, "Nov requires the native target pointer width") != null);
}

test "printing formats all integer widths boolean spacing and empty calls" {
    const c = try emitSource(
        "let empty: () -> void = @print()\nlet line: () -> void = @println()\n" ++
            "let bools: () -> void = @print(true, false)\n" ++
            "let out: (a: i8, b: u8, c: i16, d: u16, e: i32, f: u32, g: i64, h: u64) -> void = @println(a, b, c, d, e, f, g, h)",
        null,
    );
    defer test_gpa.free(c);
    try compileAndRunExpected(c,
        \\#include <stdint.h>
        \\void nov_empty(void); void nov_line(void); void nov_bools(void);
        \\void nov_out(int8_t,uint8_t,int16_t,uint16_t,int32_t,uint32_t,int64_t,uint64_t);
        \\int main(void) {
        \\ nov_empty(); nov_bools(); nov_line();
        \\ nov_out(INT8_MIN,UINT8_MAX,INT16_MIN,UINT16_MAX,INT32_MIN,UINT32_MAX,INT64_MIN,UINT64_MAX);
        \\ nov_out(INT8_MAX,0,INT16_MAX,0,INT32_MAX,0,INT64_MAX,0);
        \\ return 0;
        \\}
    , 0, "true false\n-128 255 -32768 65535 -2147483648 4294967295 -9223372036854775808 18446744073709551615\n127 0 32767 0 2147483647 0 9223372036854775807 0\n", "");
}

test "printing evaluates all arguments left to right before writing" {
    const c = try emitSource("let out: (a: () -> i32, b: () -> i32) -> void = @println(a(), b())", null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c,
        \\#include <stdint.h>
        \\#include <stdio.h>
        \\void nov_out(int32_t (*a)(void), int32_t (*b)(void));
        \\static int32_t first(void) { (void)fputs("A",stdout); return 1; }
        \\static int32_t second(void) { (void)fputs("B",stdout); return 2; }
        \\int main(void) { nov_out(first,second); return 0; }
    , 0, "AB1 2\n", "");
}

test "printing runtime is on demand and shared with arithmetic panics" {
    const plain = try emitSource("let main: () -> void = {}", null);
    defer test_gpa.free(plain);
    try std.testing.expect(std.mem.indexOf(u8, plain, "nov_rt_") == null);
    const c = try emitSource("let out: () -> void = @println(true)\nlet div: (x: i32) -> i32 = x / 0", null);
    defer test_gpa.free(c);
    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, c, "static _Noreturn void nov_rt_panic"));
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_rt_check(fflush(stdout))") == null);
    try compileAndRunExpected(c, "#include <stdint.h>\nvoid nov_out(void); int32_t nov_div(int32_t);\nint main(void) { nov_out(); return (int)nov_div(1); }\n", 1, "true\n", "panic: division by zero\n");
}

fn testPrintingEmissionAllocationFailures(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "print.nov", "let main: () -> void = @println(true, false)");
    defer module.deinit();
    var result = try Lowering.lower(&module);
    defer result.deinit();
    const c = try emitProgram(allocator, &result.ir, null);
    defer allocator.free(c);
}

test "printing program emission survives every allocation failure" {
    try std.testing.checkAllAllocationFailures(test_gpa, testPrintingEmissionAllocationFailures, .{});
}

test "printing rejects unsupported backend integer widths" {
    var diagnostic: Diagnostic = undefined;
    try std.testing.expectError(error.UnsupportedIntegerWidth, emitSource("let out: (x: i128) -> void = @println(x)", &diagnostic));
    try std.testing.expectEqual(Diagnostic.Reason.integer_width_unsupported, diagnostic.reason);
}

test "stdout failures panic including the program final flush" {
    var module = try Module.init(test_gpa, "print.nov", "let n: i32 = 42\nlet main: () -> void = @println(true, n)");
    defer module.deinit();
    var result = try Lowering.lower(&module);
    defer result.deinit();
    const program = try emitProgram(test_gpa, &result.ir, null);
    defer test_gpa.free(program);
    try std.testing.expect(std.mem.indexOf(u8, program, "nov_rt_check(fflush(stdout));") != null);
    try compileAndRunExpected(program, "typedef int nonempty_translation_unit;\n", 0, "true 42\n", "");
    // Replace one libc entry at a time. The wrappers fail only stdout, keeping
    // stderr reporting real and exercising panic's best-effort flush too.
    const Case = struct { prefix: []const u8, stdout: []const u8 };
    for ([_]Case{
        .{ .prefix = "#include <stdio.h>\nstatic int fail_puts(const char *s, FILE *f) { (void)s; (void)f; return EOF; }\n#define fputs fail_puts\n", .stdout = "" },
        .{ .prefix = "#include <stdio.h>\nstatic int fail_putc(int c, FILE *f) { (void)c; (void)f; return EOF; }\n#define fputc fail_putc\n", .stdout = "true" },
        .{ .prefix = "#include <stdio.h>\n#include <stdarg.h>\nstatic int fail_printf(FILE *f, const char *fmt, ...) { if (f == stdout) return -1; va_list args; va_start(args,fmt); int n = vfprintf(f,fmt,args); va_end(args); return n; }\n#define fprintf fail_printf\n", .stdout = "true " },
        .{ .prefix = "#include <stdio.h>\nstatic int fail_flush(FILE *f) { if (f == stdout) return EOF; return fflush(f); }\n#define fflush fail_flush\n", .stdout = "true 42\n" },
    }) |case| {
        const injected = try std.mem.concat(test_gpa, u8, &.{ case.prefix, program });
        defer test_gpa.free(injected);
        try compileAndRunExpected(injected, "typedef int nonempty_translation_unit;\n", 1, case.stdout, "panic: stdout write failed\n");
    }
    if (@import("builtin").os.tag == .linux) {
        // Exercise real libc errors too: unbuffered writes fail immediately,
        // while buffered writes succeed until emitProgram's final flush.
        for ([_][]const u8{ "_IONBF", "_IOFBF" }) |mode| {
            const full = try std.fmt.allocPrint(
                test_gpa,
                "#define main nov_test_entry\n{s}\n#undef main\n" ++
                    "int main(void) {{ static char buffer[BUFSIZ]; if (freopen(\"/dev/full\", \"w\", stdout) == NULL) return 77; " ++
                    "if (setvbuf(stdout, buffer, {s}, sizeof buffer) != 0) return 78; return nov_test_entry(); }}\n",
                .{ program, mode },
            );
            defer test_gpa.free(full);
            try compileAndRunExpected(full, "typedef int nonempty_translation_unit;\n", 1, "", "panic: stdout write failed\n");
        }
    }
    // A newline is itself a checked write, including zero-argument println.
    var line_module = try Module.init(test_gpa, "line.nov", "let main: () -> void = @println()");
    defer line_module.deinit();
    var line_result = try Lowering.lower(&line_module);
    defer line_result.deinit();
    const line_program = try emitProgram(test_gpa, &line_result.ir, null);
    defer test_gpa.free(line_program);
    const injected_line = try std.mem.concat(test_gpa, u8, &.{ "#include <stdio.h>\nstatic int fail_putc(int c, FILE *f) { (void)c; (void)f; return EOF; }\n#define fputc fail_putc\n", line_program });
    defer test_gpa.free(injected_line);
    try compileAndRunExpected(injected_line, "typedef int nonempty_translation_unit;\n", 1, "", "panic: stdout write failed\n");
}

test "program entrypoint is separate from library emission and propagates panic" {
    for ([_]bool{ false, true }) |panic| {
        const source = if (panic)
            "let divide: () -> i32 = 1 / 0\nlet sink: (x: i32) -> void = {}\nlet main: () -> void = sink(divide())"
        else
            "let helper: () -> void = {}\nlet main: () -> void = helper()";
        var module = try Module.init(test_gpa, "program.nov", source);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        defer result.deinit();
        try std.testing.expect(!module.hasErrors());
        const library = try emit(test_gpa, &result.ir, null);
        defer test_gpa.free(library);
        try std.testing.expect(std.mem.indexOf(u8, library, "int main(void)") == null);
        const program = try emitProgram(test_gpa, &result.ir, null);
        defer test_gpa.free(program);
        try std.testing.expect(std.mem.startsWith(u8, program, library));
        try compileAndRunExpected(program, "typedef int nonempty_translation_unit;\n", if (panic) 1 else 0, "", if (panic) "panic: division by zero\n" else "");
    }
}

test "program entrypoint validation reports missing and wrong signatures" {
    const cases = [_]struct { source: []const u8, expected: Error }{
        .{ .source = "", .expected = error.MissingEntrypoint },
        .{ .source = "let other: () -> void = {}", .expected = error.MissingEntrypoint },
        .{ .source = "let main: () -> bool = true", .expected = error.InvalidEntrypoint },
        .{ .source = "let main: (x: bool) -> void = {}", .expected = error.InvalidEntrypoint },
    };
    for (cases) |case| {
        var module = try Module.init(test_gpa, "program.nov", case.source);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        defer result.deinit();
        try std.testing.expect(!module.hasErrors());
        var diagnostic: Diagnostic = undefined;
        try std.testing.expectError(case.expected, emitProgram(test_gpa, &result.ir, &diagnostic));
        try std.testing.expect(diagnostic.span.end <= case.source.len);
    }
}

test "empty module emits only the prelude" {
    const c = try emitSource("", null);
    defer test_gpa.free(c);
    try std.testing.expectEqualStrings("/* Generated by Nov. */\n#include <stdbool.h>\n#include <stdint.h>\n" ++ native_target_guard, c);
}

test "comparison and call functions emit ordered const locals" {
    const c = try emitSource(
        "let less: (x: i32, y: i32) -> bool = x < y\n" ++
            "let id: (x: i32) -> i32 = x\n" ++
            "let apply: (callback: (i32) -> i32, value: i32) -> i32 = callback(value)\n" ++
            "let main: () -> i32 = apply(id, 7)\n",
        null,
    );
    defer test_gpa.free(c);
    try std.testing.expectEqualStrings(
        \\/* Generated by Nov. */
        \\#include <stdbool.h>
        \\#include <stdint.h>
        \\
    ++ native_target_guard ++
        \\
        \\typedef bool (*nov_fn_2)(int32_t, int32_t);
        \\typedef int32_t (*nov_fn_3)(int32_t);
        \\typedef int32_t (*nov_fn_4)(nov_fn_3, int32_t);
        \\typedef int32_t (*nov_fn_5)(void);
        \\
        \\bool nov_less(int32_t p0, int32_t p1);
        \\int32_t nov_id(int32_t p0);
        \\int32_t nov_apply(nov_fn_3 p0, int32_t p1);
        \\int32_t nov_main(void);
        \\
        \\bool nov_less(int32_t p0, int32_t p1) {
        \\    const int32_t v0 = p0;
        \\    const int32_t v1 = p1;
        \\    const bool v2 = v0 < v1;
        \\    return v2;
        \\}
        \\
        \\int32_t nov_id(int32_t p0) {
        \\    const int32_t v0 = p0;
        \\    return v0;
        \\}
        \\
        \\int32_t nov_apply(nov_fn_3 p0, int32_t p1) {
        \\    const nov_fn_3 v0 = p0;
        \\    const int32_t v1 = p1;
        \\    const int32_t v2 = v0(v1);
        \\    return v2;
        \\}
        \\
        \\int32_t nov_main(void) {
        \\    const nov_fn_4 v0 = nov_apply;
        \\    const nov_fn_3 v1 = nov_id;
        \\    const int32_t v2 = INT32_C(7);
        \\    const int32_t v3 = v0(v1, v2);
        \\    return v3;
        \\}
        \\
    , c);
}

test "void calls are statements and unused values and parameters are silenced" {
    const c = try emitSource(
        "let empty: () = {}\n" ++
            "let call: () = empty()\n" ++
            "let first: (a: bool, b: bool) -> bool = a\n",
        null,
    );
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "void nov_empty(void) {\n}\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, c, " = nov_empty;\n    v0();\n}\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, c, "    (void)p1;\n    const bool v0 = p0;\n    return v0;\n") != null);
}

test "bit and boolean negation convert explicitly back to the operand type" {
    const c = try emitSource(
        "let bits: (x: u8) -> u8 = ~x\nlet flip: (x: bool) -> bool = !x\n",
        null,
    );
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "const uint8_t v1 = (uint8_t)~v0;") != null);
    try std.testing.expect(std.mem.indexOf(u8, c, "const bool v1 = !v0;") != null);
}

test "integer constants cover every supported width without invalid literals" {
    const c = try emitSource(
        "let a: () -> i8 = -128\nlet b: () -> i16 = -32768\nlet c: () -> i32 = -2147483648\n" ++
            "let d: () -> i64 = -9223372036854775808\nlet e: () -> i64 = -5\nlet f: () -> u64 = 18446744073709551615\n" ++
            "let g: () -> bool = false\n",
        null,
    );
    defer test_gpa.free(c);
    for ([_][]const u8{
        "= (-INT8_MAX - 1);",
        "= (-INT16_MAX - 1);",
        "= (-INT32_MAX - 1);",
        "= (-INT64_MAX - 1);",
        "= (-INT64_C(5));",
        "= UINT64_C(18446744073709551615);",
        "= false;",
    }) |expected| {
        try std.testing.expect(std.mem.indexOf(u8, c, expected) != null);
    }
}

test "function names are mangled injectively" {
    var out = std.Io.Writer.Allocating.init(test_gpa);
    defer out.deinit();
    try writeFunctionName(&out.writer, "int");
    try out.writer.writeByte(' ');
    try writeFunctionName(&out.writer, "a_b");
    try out.writer.writeByte(' ');
    try writeFunctionName(&out.writer, "a__b");
    try out.writer.writeByte(' ');
    try writeFunctionName(&out.writer, "a_41");
    try out.writer.writeByte(' ');
    try writeFunctionName(&out.writer, "A");
    try out.writer.writeByte(' ');
    try writeFunctionName(&out.writer, "\xc3\xa9");
    try std.testing.expectEqualStrings("nov_int nov_a__b nov_a____b nov_a__41 nov_A nov__C3_A9", out.written());
}

test "unsupported integer widths are rejected wherever they are reachable" {
    var diagnostic: Diagnostic = undefined;
    try std.testing.expectError(error.UnsupportedIntegerWidth, emitSource("let wide: (x: i128) -> i128 = x", &diagnostic));
    try std.testing.expectEqual(Diagnostic.Reason.integer_width_unsupported, diagnostic.reason);
    try std.testing.expectEqual(@as(u32, 0), @intFromEnum(diagnostic.function.?));
    // Nested inside a function type that only appears as a parameter.
    try std.testing.expectError(error.UnsupportedIntegerWidth, emitSource("let f: (g: (u128) -> bool) -> bool = true", null));
}

test "non-byte-sized integer pool entries do not matter unless reachable" {
    var ir = Ir.init(test_gpa);
    defer ir.deinit();
    const odd = try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 7 } });
    const void_type = try ir.internType(.void);
    const function_type = try ir.internType(.{ .function = .{ .params = &.{}, .result = void_type } });
    const function = try ir.addFunction("f", function_type, .{ .start = 0, .end = 0 });
    const unit = try ir.internConstant(.{ .ty = void_type, .value = .void });
    try ir.setFunctionBody(function, &.{.{ .ty = void_type, .span = .{ .start = 0, .end = 0 }, .op = .{ .constant = unit } }}, @enumFromInt(0));
    const c = try emit(test_gpa, &ir, null);
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "void nov_f(void) {\n}\n") != null);

    const odd_function_type = try ir.internType(.{ .function = .{ .params = &.{odd}, .result = void_type } });
    const odd_function = try ir.addFunction("g", odd_function_type, .{ .start = 0, .end = 0 });
    try ir.setFunctionBody(odd_function, &.{.{ .ty = void_type, .span = .{ .start = 0, .end = 0 }, .op = .{ .constant = unit } }}, @enumFromInt(0));
    var diagnostic: Diagnostic = undefined;
    try std.testing.expectError(error.UnsupportedIntegerWidth, emit(test_gpa, &ir, &diagnostic));
    try std.testing.expectEqual(@as(u32, 1), @intFromEnum(diagnostic.function.?));
}

test "unverified IR is rejected before emission" {
    var ir = Ir.init(test_gpa);
    defer ir.deinit();
    const void_type = try ir.internType(.void);
    const function_type = try ir.internType(.{ .function = .{ .params = &.{}, .result = void_type } });
    _ = try ir.addFunction("f", function_type, .{ .start = 0, .end = 0 });
    try std.testing.expectError(error.MissingFunctionBody, emit(test_gpa, &ir, null));
}

test "emission survives allocation failures" {
    const Case = struct {
        fn run(allocator: std.mem.Allocator, source: []const u8) !void {
            var module = try Module.init(allocator, "oom.nov", source);
            defer module.deinit();
            var sema = try Lowering.lower(&module);
            defer sema.deinit();
            const c = try emit(allocator, &sema.ir, null);
            allocator.free(c);
        }
    };
    try std.testing.checkAllAllocationFailures(test_gpa, Case.run, .{
        "let id: (x: i32) -> i32 = x\nlet unused: (a: bool, b: bool) -> bool = a\nlet quotient: (a: i32, b: i32) -> i32 = a / b",
    });
}

// Compiles generated C with strict warnings and runs it, so these tests prove the
// emitted text is valid C11 rather than merely matching expected strings. This is
// a test harness with its own `main`, not a Nov entrypoint or CLI.

const harness_program =
    \\#include <stdbool.h>
    \\#include <stdint.h>
    \\
    \\typedef int32_t (*unary_fn)(int32_t);
    \\void nov_empty(void);
    \\void nov_call(void);
    \\int32_t nov_id(int32_t);
    \\int32_t nov_apply(unary_fn, int32_t);
    \\unary_fn nov_factory(void);
    \\int32_t nov_forward(int32_t);
    \\int32_t nov_indirect(void);
    \\int32_t nov_via__apply(void);
    \\int32_t nov_keep__first(int32_t, int32_t);
    \\bool nov_less(int32_t, int32_t);
    \\bool nov_ge8(uint8_t, uint8_t);
    \\bool nov_same(bool, bool);
    \\bool nov_flip(bool);
    \\uint8_t nov_bits8(uint8_t);
    \\int8_t nov_bits__i8(int8_t);
    \\uint16_t nov_bits16(uint16_t);
    \\uint64_t nov_bits64(uint64_t);
    \\int8_t nov_min8(void);
    \\int32_t nov_min32(void);
    \\int64_t nov_min64(void);
    \\int64_t nov_neg5(void);
    \\uint64_t nov_max64(void);
    \\bool nov_truth(void);
    \\
    \\#define CHECK(condition) do { if (!(condition)) return __LINE__; } while (0)
    \\
    \\int main(void) {
    \\    nov_empty();
    \\    nov_call();
    \\    CHECK(nov_id(5) == 5);
    \\    CHECK(nov_apply(nov_id, 9) == 9);
    \\    CHECK(nov_factory() == nov_id);
    \\    CHECK(nov_forward(3) == 3);
    \\    CHECK(nov_indirect() == 7);
    \\    CHECK(nov_via__apply() == 5);
    \\    CHECK(nov_keep__first(4, 8) == 4);
    \\    CHECK(nov_less(1, 2) && !nov_less(2, 2) && !nov_less(3, 2));
    \\    CHECK(nov_ge8(255, 1) && nov_ge8(7, 7) && !nov_ge8(0, 1));
    \\    CHECK(nov_same(true, true) && !nov_same(true, false));
    \\    CHECK(nov_flip(false) && !nov_flip(true));
    \\    CHECK(nov_bits8(0x0F) == 0xF0);
    \\    CHECK(nov_bits__i8(0) == -1 && nov_bits__i8(-128) == 127);
    \\    CHECK(nov_bits16(0x00FF) == 0xFF00);
    \\    CHECK(nov_bits64(0) == UINT64_MAX);
    \\    CHECK(nov_min8() == INT8_MIN);
    \\    CHECK(nov_min32() == INT32_MIN);
    \\    CHECK(nov_min64() == INT64_MIN);
    \\    CHECK(nov_neg5() == -5);
    \\    CHECK(nov_max64() == UINT64_MAX);
    \\    CHECK(nov_truth());
    \\    return 0;
    \\}
    \\
;

const nov_program =
    \\let empty: () = {}
    \\let call: () = empty()
    \\let id: (x: i32) -> i32 = x
    \\let apply: (callback: (i32) -> i32, value: i32) -> i32 = callback(value)
    \\let factory: () -> (i32) -> i32 = id
    \\let forward: (x: i32) -> i32 = later(x)
    \\let later: (x: i32) -> i32 = x
    \\let indirect: () -> i32 = factory()(7)
    \\let via_apply: () -> i32 = apply(id, 5)
    \\let keep_first: (a: i32, b: i32) -> i32 = a
    \\let less: (x: i32, y: i32) -> bool = x < y
    \\let ge8: (x: u8, y: u8) -> bool = x >= y
    \\let same: (a: bool, b: bool) -> bool = a == b
    \\let flip: (x: bool) -> bool = !x
    \\let bits8: (x: u8) -> u8 = ~x
    \\let bits_i8: (x: i8) -> i8 = ~x
    \\let bits16: (x: u16) -> u16 = ~x
    \\let bits64: (x: u64) -> u64 = ~x
    \\let min8: () -> i8 = -128
    \\let min32: () -> i32 = -2147483648
    \\let min64: () -> i64 = -9223372036854775808
    \\let neg5: () -> i64 = -5
    \\let max64: () -> u64 = 18446744073709551615
    \\let truth: () -> bool = true
    \\
;

fn expectSuccess(result: std.process.RunResult, what: []const u8) !void {
    const ok = switch (result.term) {
        .exited => |code| code == 0,
        else => false,
    };
    if (!ok) std.debug.print("{s} failed: {any}\n{s}\n{s}\n", .{ what, result.term, result.stdout, result.stderr });
    try std.testing.expect(ok);
}

fn compileAndRun(c_source: []const u8, harness: []const u8) !void {
    return compileAndRunExpected(c_source, harness, 0, "", "");
}

fn compileAndRunExpected(c_source: []const u8, harness: []const u8, exit_code: u8, stdout: []const u8, stderr: []const u8) !void {
    return compileAndRunMode(c_source, harness, exit_code, stdout, stderr, "-O2");
}

fn compileAndRunMode(c_source: []const u8, harness: []const u8, exit_code: u8, stdout: []const u8, stderr: []const u8, mode: []const u8) !void {
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "generated.c", .data = c_source });
    try tmp.dir.writeFile(io, .{ .sub_path = "harness.c", .data = harness });

    // `zig build test` provides the pinned toolchain's `zig cc`; otherwise use `cc`.
    const zig_exe = std.testing.environ.getAlloc(test_gpa, "NOV_TEST_ZIG") catch null;
    defer if (zig_exe) |path| test_gpa.free(path);
    const program = "program" ++ (if (@import("builtin").os.tag == .windows) ".exe" else "");
    const flags = [_][]const u8{ "-std=c11", mode, "-fsanitize=undefined", "-fno-sanitize-recover=all", "-Wall", "-Wextra", "-Wpedantic", "-Werror", "generated.c", "harness.c", "-o", program };
    var argv: std.ArrayList([]const u8) = .empty;
    defer argv.deinit(test_gpa);
    if (zig_exe) |path| {
        try argv.appendSlice(test_gpa, &.{ path, "cc" });
    } else {
        try argv.append(test_gpa, "cc");
    }
    try argv.appendSlice(test_gpa, &flags);

    const compiled = try std.process.run(test_gpa, io, .{ .argv = argv.items, .cwd = .{ .dir = tmp.dir }, .timeout = .{ .duration = .{ .raw = .fromSeconds(60), .clock = .awake } } });
    defer test_gpa.free(compiled.stdout);
    defer test_gpa.free(compiled.stderr);
    try expectSuccess(compiled, "C compilation");

    const dir_path = try tmp.dir.realPathFileAlloc(io, ".", test_gpa);
    defer test_gpa.free(dir_path);
    const program_path = try std.fs.path.join(test_gpa, &.{ dir_path, program });
    defer test_gpa.free(program_path);
    const ran = try std.process.run(test_gpa, io, .{ .argv = &.{program_path}, .cwd = .{ .dir = tmp.dir }, .timeout = .{ .duration = .{ .raw = .fromSeconds(10), .clock = .awake } } });
    defer test_gpa.free(ran.stdout);
    defer test_gpa.free(ran.stderr);
    // Check panic termination and both streams as well as successful harnesses.
    try std.testing.expectEqual(std.process.Child.Term{ .exited = exit_code }, ran.term);
    try std.testing.expectEqualStrings(stdout, ran.stdout);
    try std.testing.expectEqualStrings(stderr, ran.stderr);
}

test "generated C compiles with strict warnings and behaves correctly" {
    const c = try emitSource(nov_program, null);
    defer test_gpa.free(c);
    try compileAndRun(c, harness_program);
}

test "fixed width arithmetic boundaries and zero panics in generated C" {
    for ([_]u16{ 8, 16, 32, 64 }) |bits| {
        for ([_]bool{ false, true }) |signed| {
            var source = std.Io.Writer.Allocating.init(test_gpa);
            defer source.deinit();
            const prefix = if (signed) "i" else "u";
            for ([_][]const u8{ "+", "-", "*", "/", "%" }, [_][]const u8{ "add", "sub", "mul", "div", "mod" }) |op, name| {
                try source.writer.print("let {s}: (a: {s}{d}, b: {s}{d}) -> {s}{d} = a {s} b\n", .{ name, prefix, bits, prefix, bits, prefix, bits, op });
            }
            if (signed) try source.writer.print("let neg: (a: i{d}) -> i{d} = -a\n", .{ bits, bits });
            const c = try emitSource(source.written(), null);
            defer test_gpa.free(c);
            var harness = std.Io.Writer.Allocating.init(test_gpa);
            defer harness.deinit();
            try harness.writer.writeAll("#include <stdint.h>\n#include <stdio.h>\n#include <stdlib.h>\n");
            for ([_][]const u8{ "add", "sub", "mul", "div", "mod" }) |name| {
                try harness.writer.print("{s}int{d}_t nov_{s}({s}int{d}_t, {s}int{d}_t);\n", .{ if (signed) "" else "u", bits, name, if (signed) "" else "u", bits, if (signed) "" else "u", bits });
            }
            if (signed) try harness.writer.print("int{d}_t nov_neg(int{d}_t);\n", .{ bits, bits });
            try harness.writer.writeAll("#define CHECK(x) do { if (!(x)) abort(); } while (0)\nint main(void) {\n");
            if (signed) {
                try harness.writer.print("const int{d}_t lo = INT{d}_MIN, hi = INT{d}_MAX;\n", .{ bits, bits, bits });
                try harness.writer.writeAll("CHECK(nov_add(hi,1)==lo); CHECK(nov_sub(lo,1)==hi); CHECK(nov_mul(lo,-1)==lo); CHECK(nov_mul(hi,hi)==1); CHECK(nov_neg(lo)==lo); CHECK(nov_neg(1)==-1); CHECK(nov_div(lo,-1)==lo); CHECK(nov_mod(lo,-1)==0); CHECK(nov_div(-7,3)==-2); CHECK(nov_div(7,-3)==-2); CHECK(nov_div(-7,-3)==2); CHECK(nov_mod(-7,3)==-1); CHECK(nov_mod(7,-3)==1); CHECK(nov_mod(-7,-3)==-1); CHECK(nov_div(lo,1)==lo);\n");
            } else {
                try harness.writer.print("const uint{d}_t hi = UINT{d}_MAX;\n", .{ bits, bits });
                try harness.writer.writeAll("CHECK(nov_add(hi,1)==0); CHECK(nov_sub(0,1)==hi); CHECK(nov_mul(hi,hi)==1); CHECK(nov_div(hi,hi)==1); CHECK(nov_mod(hi,hi)==0); CHECK(nov_div(7,3)==2); CHECK(nov_mod(7,3)==1);\n");
            }
            try harness.writer.writeAll("CHECK(nov_mul(0,0)==0); CHECK(nov_div(0,1)==0); CHECK(nov_mod(0,1)==0); return 0; }\n");
            try compileAndRun(c, harness.written());
            for ([_][]const u8{ "div", "mod" }) |name| {
                const panic_harness = try std.fmt.allocPrint(test_gpa, "#include <stdint.h>\n#include <stdio.h>\n{s}int{d}_t nov_{s}({s}int{d}_t, {s}int{d}_t);\nint main(void) {{ (void)fputs(\"buffered\", stdout); return (int)nov_{s}(1,0); }}\n", .{ if (signed) "" else "u", bits, name, if (signed) "" else "u", bits, if (signed) "" else "u", bits, name });
                defer test_gpa.free(panic_harness);
                try compileAndRunExpected(c, panic_harness, 1, "buffered", if (std.mem.eql(u8, name, "div")) "panic: division by zero\n" else "panic: remainder by zero\n");
            }
        }
    }
}
