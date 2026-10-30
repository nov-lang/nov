//! C11 backend for verified typed IR.
//!
//! Emits one translation unit with mangled internal function names and explicit
//! exact-name C exports, plus function-pointer typedefs. IR values become ordered const locals;
//! local slots and conditional merges use mutable temporaries, so
//! the IR's callee-first, left-to-right order is preserved without relying on C
//! argument evaluation order.
//!
//! Arithmetic uses unsigned 32/64-bit working types, avoiding signed overflow
//! and narrow integer promotions. Conversion back to signed relies on the
//! modular unsigned-to-signed conversion provided by GCC, Clang and MSVC.
//! Widths other than 8/16/32/64 remain unsupported.
const std = @import("std");
const TestAllocations = @import("TestAllocations.zig");
const builtin = @import("builtin");
const Ir = @import("Ir.zig");
const Module = @import("Module.zig");
const Lowering = @import("Lowering.zig");
const GcRuntime = @import("GcRuntime.zig");
const Formatter = @import("Formatter.zig");
const Diagnostics = @import("Diagnostics.zig");
const Compilation = @import("Compilation.zig");
const Source = @import("Source.zig");

pub const Error = Ir.VerifyError || std.mem.Allocator.Error || error{
    UnsupportedIntegerWidth,
    MissingEntrypoint,
    InvalidEntrypoint,
    InvalidTestEntrypoint,
    ExportSymbolCollision,
    ExternSymbolCollision,
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
        entrypoint_external,
        test_entrypoint,
        export_symbol_collision,
        extern_symbol_collision,

        pub fn message(reason: Reason) []const u8 {
            return switch (reason) {
                .integer_width_unsupported => "C backend only supports 8, 16, 32 and 64-bit integers",
                .entrypoint_missing => "program requires a root function 'main' with type () -> void",
                .entrypoint_signature => "program entrypoint 'main' must have type () -> void",
                .test_entrypoint => "test entrypoints must be distinct test functions with type () -> void and a Nov body",
                .entrypoint_external => "program entrypoint 'main' must have a Nov body, not an extern declaration",
                .export_symbol_collision => "export symbol conflicts with a generated function name; choose another export name",
                .extern_symbol_collision => "extern symbol conflicts with a generated function name; choose another extern name",
            };
        }
    };
};

const Origin = struct { function: Ir.FunctionId, span: Ir.Span };
const WriteError = error{ WriteFailed, OutOfMemory };

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
    try analyze(allocator, ir, origins, diagnostic);

    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    write(allocator, ir, origins, &out.writer) catch |err| switch (err) {
        error.WriteFailed, error.OutOfMemory => return error.OutOfMemory,
    };
    return out.toOwnedSlice();
}

/// Emits a program, leaving library emission through `emit` unchanged.
pub fn emitProgram(allocator: std.mem.Allocator, ir: *const Ir, diagnostic: ?*Diagnostic) Error![]u8 {
    for (ir.functions.items, 0..) |function, i| {
        if (@backingInt(function.span.source) == 0 and std.mem.eql(u8, function.name, "main"))
            return emitProgramRoot(allocator, ir, @fromBackingInt(@intCast(i)), .{ .start = ir.source_len, .end = ir.source_len }, diagnostic);
    }
    return emitProgramRoot(allocator, ir, null, .{ .start = ir.source_len, .end = ir.source_len }, diagnostic);
}

/// The caller supplies the root declaration's FunctionId. An imported function
/// named main must never implicitly become the process entrypoint.
pub fn emitProgramRoot(allocator: std.mem.Allocator, ir: *const Ir, entry: ?Ir.FunctionId, missing_span: Ir.Span, diagnostic: ?*Diagnostic) Error![]u8 {
    try ir.verify();
    const id = entry orelse {
        if (diagnostic) |d| d.* = .{ .reason = .entrypoint_missing, .function = null, .span = missing_span };
        return error.MissingEntrypoint;
    };
    const function = ir.getFunction(id) orelse return error.InvalidFunctionId;
    if (function.extern_name != null)
        return fail(diagnostic, .entrypoint_external, .{ .function = id, .span = function.span });
    const signature = ir.types.items[@backingInt(function.ty)].function;
    if (function.is_test or signature.params.len != 0 or ir.types.items[@backingInt(signature.result)] != .void)
        return fail(diagnostic, .entrypoint_signature, .{ .function = id, .span = function.span });
    const library = try emit(allocator, ir, diagnostic);
    defer allocator.free(library);
    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    out.writer.print("{s}\nint main(void) {{\n    ", .{library}) catch return error.OutOfMemory;
    writeScopedFunctionName(&out.writer, function) catch return error.OutOfMemory;
    out.writer.print("();\n{s}{s}    return 0;\n}}\n", .{ if (usesPrinting(ir)) "    nov_rt_check(fflush(stdout));\n" else "", if (usesManagedRuntime(ir)) "    nov_gc_shutdown(&nov_rt_heap);\n" else "" }) catch return error.OutOfMemory;
    return out.toOwnedSlice();
}

/// Emits a one-test-per-process driver without requiring or invoking Nov main.
pub fn emitTestProgram(allocator: std.mem.Allocator, ir: *const Ir, tests: []const Ir.FunctionId, diagnostic: ?*Diagnostic) Error![]u8 {
    try ir.verify();
    for (tests, 0..) |id, index| {
        const function = ir.getFunction(id) orelse return fail(diagnostic, .test_entrypoint, .{ .function = id, .span = .{ .start = 0, .end = 0 } });
        const signature = ir.getType(function.ty).?.function;
        if (!function.is_test or function.body == null or function.noreturn or function.export_name != null or function.extern_name != null or signature.params.len != 0 or ir.getType(signature.result).? != .void)
            return fail(diagnostic, .test_entrypoint, .{ .function = id, .span = function.span });
        for (tests[0..index]) |previous| if (previous == id)
            return fail(diagnostic, .test_entrypoint, .{ .function = id, .span = function.span });
    }
    const library = try emit(allocator, ir, diagnostic);
    defer allocator.free(library);
    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    const w = &out.writer;
    w.print("{s}\n#include <string.h>\nint main(int argc, char **argv) {{\n    (void)argv;\n    if (argc != 2) return 2;\n", .{library}) catch return error.OutOfMemory;
    for (tests, 0..) |id, index| {
        w.print("    {s}if (strcmp(argv[1], \"{d}\") == 0) {{\n        ", .{ if (index == 0) "" else "else ", index }) catch return error.OutOfMemory;
        writeScopedFunctionName(w, ir.getFunction(id).?) catch return error.OutOfMemory;
        w.writeAll("();\n    }\n") catch return error.OutOfMemory;
    }
    w.writeAll(if (tests.len == 0) "    return 2;\n" else "    else return 2;\n") catch return error.OutOfMemory;
    w.print("{s}{s}    return 0;\n}}\n", .{ if (usesPrinting(ir)) "    nov_rt_check(fflush(stdout));\n" else "", if (usesManagedRuntime(ir)) "    nov_gc_shutdown(&nov_rt_heap);\n" else "" }) catch return error.OutOfMemory;
    return out.toOwnedSlice();
}

fn fail(diagnostic: ?*Diagnostic, reason: Diagnostic.Reason, origin: Origin) Error {
    if (diagnostic) |d| d.* = .{ .reason = reason, .function = origin.function, .span = origin.span };
    return switch (reason) {
        .test_entrypoint => error.InvalidTestEntrypoint,
        .integer_width_unsupported => error.UnsupportedIntegerWidth,
        .entrypoint_missing => error.MissingEntrypoint,
        .entrypoint_signature, .entrypoint_external => error.InvalidEntrypoint,
        .export_symbol_collision => error.ExportSymbolCollision,
        .extern_symbol_collision => error.ExternSymbolCollision,
    };
}

const TypeReachability = struct {
    origins: []?Origin,
    pending: []Ir.TypeId,
    len: usize = 0,

    fn mark(self: *TypeReachability, ty: Ir.TypeId, origin: Origin) void {
        const slot = &self.origins[@backingInt(ty)];
        if (slot.* != null) return;
        slot.* = origin;
        // Each type is queued once, including nominal edges in either pool direction.
        self.pending[self.len] = ty;
        self.len += 1;
    }
};

/// Rejects unsupported widths and marks runtime types reachable from signatures
/// and instructions, including nominal edges in either pool-index direction.
fn analyze(allocator: std.mem.Allocator, ir: *const Ir, origins: []?Origin, diagnostic: ?*Diagnostic) Error!void {
    const pending = try allocator.alloc(Ir.TypeId, ir.types.items.len);
    defer allocator.free(pending);
    var reachable: TypeReachability = .{ .origins = origins, .pending = pending };
    for (ir.functions.items, 0..) |function, f| {
        const id: Ir.FunctionId = @fromBackingInt(@intCast(f));
        if (function.export_name orelse function.extern_name) |symbol| {
            for (ir.functions.items) |other| {
                if (other.export_name != null or other.extern_name != null) continue;
                var generated = std.Io.Writer.Allocating.init(allocator);
                defer generated.deinit();
                writeScopedFunctionName(&generated.writer, other) catch return error.OutOfMemory;
                if (std.mem.eql(u8, symbol, generated.written())) return fail(diagnostic, if (function.extern_name != null) .extern_symbol_collision else .export_symbol_collision, .{ .function = id, .span = function.span });
            }
        }
        reachable.mark(function.ty, .{ .function = id, .span = function.span });
        const body = function.body orelse continue;
        for (body.instructions) |instruction| {
            const origin: Origin = .{ .function = id, .span = instruction.span };
            reachable.mark(instruction.ty, origin);
        }
    }
    while (reachable.len != 0) {
        reachable.len -= 1;
        const id = pending[reachable.len];
        const origin = origins[@backingInt(id)].?;
        switch (ir.getType(id).?) {
            .void, .boolean, .rune, .string, .enumeration, .float => {},
            .record => |record| for (record.fields) |field| {
                reachable.mark(field.ty, .{ .function = origin.function, .span = field.span });
            },
            .range, .array => |element| reachable.mark(element, origin),
            .integer => |integer| switch (integer.bits) {
                8, 16, 32, 64 => {},
                else => return fail(diagnostic, .integer_width_unsupported, origin),
            },
            .function => |function| {
                for (function.params) |param| reachable.mark(param, origin);
                reachable.mark(function.result, origin);
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
    try writeFloatRuntime(allocator, ir, origins, w);
    try writeRuneRuntime(ir, w);
    if (usesManagedRuntime(ir)) {
        try w.writeAll("#define NOV_GC_PANIC nov_rt_panic\n");
        try w.writeAll(GcRuntime.source);
        try w.writeByte('\n');
        try w.writeAll(@embedFile("runtime/array.h"));
        if (usesStringConcat(ir)) try w.writeAll(@embedFile("runtime/string.h"));
        try writeStringConversions(ir, w);
    }
    try writeStringScalarRuntime(ir, w);
    if (usesStringEquality(ir)) try w.writeAll(
        "#include <string.h>\n" ++
            "static bool nov_string_equal(nov_string a, nov_string b) {\n" ++
            "    return a.len == b.len && (!a.len || memcmp(a.data, b.data, (size_t)a.len) == 0);\n" ++
            "}\n",
    );
    for (ir.types.items, 0..) |_, t| {
        const element: Ir.TypeId = @fromBackingInt(@intCast(t));
        if (!usesArrayContains(ir, element)) continue;
        try w.print("static bool nov_array_contains_{d}(", .{t});
        try writeType(ir, w, element);
        try w.writeAll(" value, nov_array array) {\n    const ");
        try writeType(ir, w, element);
        try w.writeAll(" *data = array.data;\n    for (uintptr_t i = 0; i < array.len; ++i) {\n        if (");
        try w.writeAll(if (ir.getType(element).? == .string) "nov_string_equal(value, data[i])" else "value == data[i]");
        try w.writeAll(") return true;\n    }\n    return false;\n}\n");
    }
    try writeArithmeticRuntime(ir, w);
    // Nominal placeholders may precede dependencies in either pool direction.
    const declared = try allocator.alloc(bool, ir.types.items.len);
    defer allocator.free(declared);
    @memset(declared, false);
    const root_counts = try allocator.alloc(usize, ir.types.items.len);
    defer allocator.free(root_counts);
    for (ir.types.items, root_counts) |ty, *count| count.* = @intFromBool(ty == .string or ty == .array or (ty == .function and usesClosures(ir)));
    var remaining: usize = 0;
    for (origins, ir.types.items) |origin, ty| {
        if (origin != null and ty == .record) remaining += 1;
    }
    while (remaining != 0) {
        for (origins, ir.types.items, 0..) |origin, ty, i| {
            if (origin == null or ty != .record or declared[i]) continue;
            var ready = true;
            for (ty.record.fields) |field| {
                if (ir.getType(field.ty).? == .record and !declared[@backingInt(field.ty)]) ready = false;
            }
            if (!ready) continue;
            declared[i] = true;
            remaining -= 1;
            for (ty.record.fields) |field| root_counts[i] = std.math.add(usize, root_counts[i], root_counts[@backingInt(field.ty)]) catch return error.OutOfMemory;
            try w.print("typedef struct {{\n", .{});
            if (ty.record.tagged) try w.writeAll("    uint32_t tag;\n");
            for (ty.record.fields, 0..) |field, f| {
                if (ir.getType(field.ty).? == .void) continue;
                try w.writeAll("    ");
                try writeType(ir, w, field.ty);
                try w.print(" f{d};\n", .{f});
            }
            if (ty.record.fields.len == 0) try w.writeAll("    unsigned char empty;\n");
            try w.print("}} nov_record_{d};\n", .{i});
        }
    }
    for (origins, ir.types.items, 0..) |origin, ty, i| {
        if (origin == null or ty != .range) continue;
        try w.writeAll("typedef struct { ");
        try writeType(ir, w, ty.range);
        try w.print(" start, end; bool inclusive; }} nov_range_{d};\n", .{i});
    }
    // Forward declarations permit higher-order signatures without recursive C typedefs.
    if (usesClosures(ir)) {
        for (origins, ir.types.items, 0..) |origin, ty, i| {
            if (origin != null and ty == .function) try w.print("typedef struct nov_fn_{d} nov_fn_{d};\n", .{ i, i });
        }
        for (origins, ir.types.items, 0..) |origin, ty, i| {
            if (origin == null or ty != .function) continue;
            try w.print("struct nov_fn_{d} {{ ", .{i});
            try writeType(ir, w, ty.function.result);
            try w.writeAll(" (*code)(void *");
            for (ty.function.params) |param| {
                try w.writeAll(", ");
                try writeType(ir, w, param);
            }
            try w.writeAll("); void *env; };\n");
        }
    }
    // Array element descriptors can refer to ordinary function-pointer types.
    var first_typedef = true;
    for (origins, ir.types.items, 0..) |origin, ty, i| {
        if (origin == null or ty != .function or usesClosures(ir)) continue;
        if (first_typedef) try w.writeByte('\n');
        first_typedef = false;
        try w.writeAll("typedef ");
        try writeType(ir, w, ty.function.result);
        try w.print(" (*nov_fn_{d})(", .{i});
        try writeParamTypes(ir, w, ty.function.params);
        try w.writeAll(");\n");
    }
    const copy_types = try allocator.alloc(bool, ir.types.items.len);
    defer allocator.free(copy_types);
    try markCopyTypes(allocator, ir, copy_types);
    for (ir.types.items, 0..) |_, t| {
        const element: Ir.TypeId = @fromBackingInt(@intCast(t));
        var copy_element = false;
        for (ir.types.items, copy_types) |ty, copied| {
            if (copied and ty == .array and ty.array == element) copy_element = true;
        }
        if (!usesElementLayout(ir, element) and !copy_element) continue;
        const count = root_counts[t];
        if (count != 0) {
            try w.print("static const size_t nov_element_offsets_{d}[] = {{", .{t});
            try writeElementOffsets(ir, root_counts, w, element);
            try w.writeAll(" };\n");
        }
        try w.print("static const nov_gc_descriptor nov_element_{d} = {{ sizeof(", .{t});
        try writeType(ir, w, element);
        try w.writeAll("), _Alignof(");
        try writeType(ir, w, element);
        try w.print("), {d}, ", .{count});
        if (count == 0) try w.writeAll("NULL") else try w.print("nov_element_offsets_{d}", .{t});
        try w.writeAll(" };\n");
    }
    try writeCopyHelpers(ir, copy_types, root_counts, w);
    if (ir.functions.items.len == 0) return;
    try w.writeByte('\n');
    for (ir.functions.items) |function| {
        try writeSignature(ir, w, function);
        try w.writeAll(";\n");
    }
    try writeAggregateFormatting(allocator, ir, root_counts, w);
    if (usesClosures(ir)) {
        try w.writeAll("static const size_t nov_env_offset[] = {0};\nstatic const nov_gc_descriptor nov_env_element = {sizeof(void *), _Alignof(void *), 1, nov_env_offset};\n");
        for (ir.functions.items, 0..) |function, f| {
            if (function.is_closure) continue;
            var referenced = false;
            for (ir.functions.items) |caller| {
                if (caller.body) |body| for (body.instructions) |instruction| {
                    if (instruction.op == .function_ref and @backingInt(instruction.op.function_ref) == f) {
                        referenced = true;
                        break;
                    }
                };
            }
            if (!referenced) continue;
            const signature = ir.getType(function.ty).?.function;
            try w.writeAll("static inline ");
            try writeType(ir, w, signature.result);
            try w.print(" nov_adapter_{d}(void *nov_env", .{f});
            for (signature.params, 0..) |param, p| {
                try w.writeAll(", ");
                try writeType(ir, w, param);
                try w.print(" p{d}", .{p});
            }
            try w.writeAll(") { (void)nov_env; ");
            if (ir.getType(signature.result).? != .void) try w.writeAll("return ");
            try writeScopedFunctionName(w, function);
            try w.writeByte('(');
            for (signature.params, 0..) |_, p| {
                if (p != 0) try w.writeAll(", ");
                try w.print("p{d}", .{p});
            }
            try w.writeAll("); }\n");
        }
    }
    for (ir.functions.items) |function| {
        if (function.body == null) continue;
        try w.writeByte('\n');
        try writeSignature(ir, w, function);
        try w.writeAll(" {\n");
        try writeBody(allocator, ir, root_counts, w, function);
        try w.writeAll("}\n");
    }
}

fn usesElementLayout(ir: *const Ir, element: Ir.TypeId) bool {
    for (ir.functions.items) |function| {
        const body = function.body orelse continue;
        for (body.instructions) |instruction| switch (instruction.op) {
            .array_literal, .array_set, .array_append => if (ir.getType(instruction.ty).?.array == element) return true,
            .array_clone => {},
            .local_slot => if (instruction.boxed and instruction.ty == element) return true,
            .closure_init => |closure| {
                for (ir.getFunction(closure.function).?.captures) |capture| if (!capture.mutable and capture.ty == element and ir.getType(element).? != .void) return true;
            },
            else => {},
        };
    }
    return false;
}

fn writeElementOffsets(ir: *const Ir, root_counts: []const usize, w: *std.Io.Writer, id: Ir.TypeId) WriteError!void {
    const Frame = struct { ty: Ir.TypeId, next: usize = 0 };
    var stack: [Ir.max_record_depth + 1]Frame = undefined;
    stack[0] = .{ .ty = id };
    var len: usize = 1;
    while (len != 0) {
        const frame = &stack[len - 1];
        const ty = ir.getType(frame.ty).?;
        if (ty == .record) {
            if (frame.next == ty.record.fields.len) {
                len -= 1;
            } else {
                const field = ty.record.fields[frame.next];
                frame.next += 1;
                if (root_counts[@backingInt(field.ty)] == 0) continue;
                stack[len] = .{ .ty = field.ty };
                len += 1;
            }
        } else {
            std.debug.assert(ty == .string or ty == .array or ty == .function);
            try w.writeAll(" offsetof(");
            try writeType(ir, w, id);
            try w.writeAll(", ");
            for (stack[0 .. len - 1]) |parent| try w.print("f{d}.", .{parent.next - 1});
            try w.print("{s}),", .{if (ty == .string) "owner" else if (ty == .function) "env" else "data"});
            len -= 1;
        }
    }
}

fn writeRoots(ir: *const Ir, root_counts: []const usize, w: *std.Io.Writer, id: Ir.TypeId, prefix: []const u8, index: usize) WriteError!void {
    if (root_counts[@backingInt(id)] == 0) return;
    const Frame = struct { ty: Ir.TypeId, next: usize = 0 };
    var stack: [Ir.max_record_depth + 1]Frame = undefined;
    stack[0] = .{ .ty = id };
    var len: usize = 1;
    while (len != 0) {
        const frame = &stack[len - 1];
        const ty = ir.getType(frame.ty).?;
        if (ty == .record) {
            if (frame.next == ty.record.fields.len) {
                len -= 1;
            } else {
                const field = ty.record.fields[frame.next];
                frame.next += 1;
                if (root_counts[@backingInt(field.ty)] == 0) continue;
                stack[len] = .{ .ty = field.ty };
                len += 1;
            }
        } else {
            if (managedType(ir, ty)) {
                try w.print(" &{s}{d}", .{ prefix, index });
                for (stack[0 .. len - 1]) |parent| try w.print(".f{d}", .{parent.next - 1});
                try w.print(".{s},", .{if (ty == .string) "owner" else if (ty == .function) "env" else "data"});
            }
            len -= 1;
        }
    }
}

fn usesClosures(ir: *const Ir) bool {
    for (ir.functions.items) |function| if (function.is_closure) return true;
    return false;
}

fn managedType(ir: *const Ir, ty: Ir.Type) bool {
    return Ir.isManaged(ty) or (ty == .function and usesClosures(ir));
}

fn needsRootFrame(ir: *const Ir, function: Ir.Function) bool {
    const body = function.body orelse return false;
    if (function.is_closure) return true;
    for (body.instructions) |instruction| if (instruction.boxed) return true;
    const signature = ir.getType(function.ty).?.function;
    for (signature.params) |param| if (managedType(ir, ir.getType(param).?)) return true;
    for (body.instructions) |instruction| if (managedType(ir, ir.getType(instruction.ty).?)) return true;
    return managedType(ir, ir.getType(signature.result).?);
}

fn usesManagedRuntime(ir: *const Ir) bool {
    for (ir.functions.items) |function| if (needsRootFrame(ir, function)) return true;
    return false;
}

fn usesStringIndex(ir: *const Ir) bool {
    for (ir.functions.items) |function| {
        const body = function.body orelse continue;
        for (body.instructions) |instruction| {
            if (instruction.op == .array_index and ir.getType(body.instructions[@backingInt(instruction.op.array_index.array)].ty).? == .string) return true;
        }
    }
    return false;
}

fn usesStringSlice(ir: *const Ir) bool {
    for (ir.functions.items) |function| {
        const body = function.body orelse continue;
        for (body.instructions) |instruction| if (instruction.op == .string_slice) return true;
    }
    return false;
}

fn usesStringLength(ir: *const Ir) bool {
    for (ir.functions.items) |function| {
        const body = function.body orelse continue;
        for (body.instructions) |instruction| {
            if (instruction.op == .array_len and ir.getType(body.instructions[@backingInt(instruction.op.array_len)].ty).? == .string) return true;
        }
    }
    return false;
}

fn usesStringIteration(ir: *const Ir) bool {
    for (ir.functions.items) |function| {
        const body = function.body orelse continue;
        for (body.instructions) |instruction| {
            if (instruction.op == .string_decode or instruction.op == .string_next) return true;
        }
    }
    return false;
}

fn writeStringScalarRuntime(ir: *const Ir, w: *std.Io.Writer) WriteError!void {
    const indexing = usesStringIndex(ir);
    const slicing = usesStringSlice(ir);
    const iteration = usesStringIteration(ir);
    if (!indexing and !slicing and !usesStringLength(ir) and !iteration) return;
    // The representation keeps byte lengths; only language-facing offsets count scalars.
    try w.writeAll(
        \\static uint32_t nov_string_decode(nov_string text, uintptr_t *offset) {
        \\    if (*offset >= text.len) nov_rt_panic("invalid UTF-8 string");
        \\    const unsigned char lead = text.data[(*offset)++];
        \\    if (lead < 0x80u) return lead;
        \\    uint32_t value, minimum;
        \\    uintptr_t extra;
        \\    if (lead >= 0xc2u && lead <= 0xdfu) { value = lead & 0x1fu; minimum = 0x80u; extra = 1; }
        \\    else if (lead >= 0xe0u && lead <= 0xefu) { value = lead & 0x0fu; minimum = 0x800u; extra = 2; }
        \\    else if (lead >= 0xf0u && lead <= 0xf4u) { value = lead & 0x07u; minimum = 0x10000u; extra = 3; }
        \\    else { nov_rt_panic("invalid UTF-8 string"); return 0; }
        \\    if (extra > text.len - *offset) nov_rt_panic("invalid UTF-8 string");
        \\    for (uintptr_t i = 0; i < extra; ++i) {
        \\        const unsigned char next = text.data[(*offset)++];
        \\        if ((next & 0xc0u) != 0x80u) nov_rt_panic("invalid UTF-8 string");
        \\        value = (value << 6) | (next & 0x3fu);
        \\    }
        \\    if (value < minimum || value > 0x10ffffu || (value >= 0xd800u && value <= 0xdfffu)) nov_rt_panic("invalid UTF-8 string");
        \\    return value;
        \\}
        \\
    );
    if (iteration) try w.writeAll(
        \\static uint32_t nov_string_cursor_rune(nov_string text, intptr_t cursor) {
        \\    if (cursor < 0) nov_rt_panic("invalid UTF-8 cursor");
        \\    uintptr_t offset = (uintptr_t)cursor;
        \\    return nov_string_decode(text, &offset);
        \\}
        \\static intptr_t nov_string_cursor_next(nov_string text, intptr_t cursor) {
        \\    if (cursor < 0) nov_rt_panic("invalid UTF-8 cursor");
        \\    uintptr_t offset = (uintptr_t)cursor;
        \\    (void)nov_string_decode(text, &offset);
        \\    if (offset > INTPTR_MAX) nov_rt_panic("iterable too long");
        \\    return (intptr_t)offset;
        \\}
        \\
    );
    if (indexing or slicing or usesStringLength(ir)) try w.writeAll(
        \\static uintptr_t nov_string_scalar_len(nov_string text) {
        \\    uintptr_t offset = 0, count = 0;
        \\    while (offset < text.len) { (void)nov_string_decode(text, &offset); ++count; }
        \\    return count;
        \\}
        \\
    );
    if (slicing) try w.writeAll(
        \\static uintptr_t nov_string_bound(intptr_t bound, uintptr_t count) {
        \\    if (bound < 0) {
        \\        const uintptr_t distance = (uintptr_t)(-(bound + 1)) + 1;
        \\        if (distance > count) nov_rt_panic("string slice out of bounds");
        \\        return count - distance;
        \\    }
        \\    if ((uintptr_t)bound > count) nov_rt_panic("string slice out of bounds");
        \\    return (uintptr_t)bound;
        \\}
        \\static nov_string nov_string_slice(nov_string text, intptr_t start, intptr_t end, bool has_end, bool inclusive) {
        \\    const uintptr_t count = nov_string_scalar_len(text);
        \\    const uintptr_t first = nov_string_bound(start, count);
        \\    uintptr_t last = has_end ? nov_string_bound(end, count) : count;
        \\    if (has_end && inclusive) {
        \\        if (last == count) nov_rt_panic("string slice out of bounds");
        \\        ++last;
        \\    }
        \\    if (first > last) nov_rt_panic("string slice out of bounds");
        \\    uintptr_t offset = 0, scalar = 0;
        \\    while (scalar < first) { (void)nov_string_decode(text, &offset); ++scalar; }
        \\    const uintptr_t begin = offset;
        \\    while (scalar < last) { (void)nov_string_decode(text, &offset); ++scalar; }
        \\    return (nov_string){text.owner, begin ? text.data + begin : text.data, offset - begin};
        \\}
        \\
    );
    if (indexing) try w.writeAll(
        \\static uint32_t nov_string_rune_at(nov_string text, intptr_t index) {
        \\    uintptr_t target;
        \\    if (index < 0) {
        \\        const uintptr_t distance = (uintptr_t)(-(index + 1)) + 1;
        \\        const uintptr_t count = nov_string_scalar_len(text);
        \\        if (distance > count) nov_rt_panic("string index out of bounds");
        \\        target = count - distance;
        \\    } else target = (uintptr_t)index;
        \\    uintptr_t offset = 0;
        \\    while (offset < text.len) {
        \\        const uint32_t value = nov_string_decode(text, &offset);
        \\        if (target == 0) return value;
        \\        --target;
        \\    }
        \\    nov_rt_panic("string index out of bounds");
        \\    return 0;
        \\}
        \\
    );
}

fn usesStringEquality(ir: *const Ir) bool {
    for (ir.types.items, 0..) |ty, t| {
        if (ty == .string and usesArrayContains(ir, @fromBackingInt(@intCast(t)))) return true;
    }
    for (ir.functions.items) |function| {
        const body = function.body orelse continue;
        for (body.instructions) |instruction| {
            if (instruction.op != .binary) continue;
            const binary = instruction.op.binary;
            if ((binary.op == .eq or binary.op == .ne) and ir.getType(body.instructions[@backingInt(binary.lhs)].ty).? == .string) return true;
        }
    }
    return false;
}

fn writeRuneRuntime(ir: *const Ir, w: *std.Io.Writer) WriteError!void {
    var printing = false;
    var conversion = false;
    for (ir.functions.items) |function| {
        const body = function.body orelse continue;
        for (body.instructions) |instruction| switch (instruction.op) {
            .print => |p| {
                for (p.args) |arg| {
                    if (ir.getType(body.instructions[@backingInt(arg)].ty).? == .rune) printing = true;
                }
            },
            .to_string => |value| {
                if (ir.getType(body.instructions[@backingInt(value)].ty).? == .rune) conversion = true;
            },
            else => {},
        };
    }
    if (!printing and !conversion) return;
    try w.writeAll(
        \\static uintptr_t nov_rt_encode_rune(uint32_t value, unsigned char bytes[4]) {
        \\    if (value <= UINT32_C(0x7f)) {
        \\        bytes[0] = (unsigned char)value;
        \\        return 1;
        \\    }
        \\    if (value <= UINT32_C(0x7ff)) {
        \\        bytes[0] = (unsigned char)(0xc0u | (value >> 6));
        \\        bytes[1] = (unsigned char)(0x80u | (value & 0x3fu));
        \\        return 2;
        \\    }
        \\    if (value <= UINT32_C(0xffff)) {
        \\        bytes[0] = (unsigned char)(0xe0u | (value >> 12));
        \\        bytes[1] = (unsigned char)(0x80u | ((value >> 6) & 0x3fu));
        \\        bytes[2] = (unsigned char)(0x80u | (value & 0x3fu));
        \\        return 3;
        \\    }
        \\    bytes[0] = (unsigned char)(0xf0u | (value >> 18));
        \\    bytes[1] = (unsigned char)(0x80u | ((value >> 12) & 0x3fu));
        \\    bytes[2] = (unsigned char)(0x80u | ((value >> 6) & 0x3fu));
        \\    bytes[3] = (unsigned char)(0x80u | (value & 0x3fu));
        \\    return 4;
        \\}
        \\
    );
    if (printing) try w.writeAll(
        \\static void nov_rt_print_rune(uint32_t value) {
        \\    unsigned char bytes[4];
        \\    uintptr_t len = nov_rt_encode_rune(value, bytes);
        \\    if (fwrite(bytes, 1, (size_t)len, stdout) != (size_t)len) nov_rt_panic("stdout write failed");
        \\}
        \\
    );
}

fn writeStringConversions(ir: *const Ir, w: *std.Io.Writer) WriteError!void {
    for (ir.types.items, 0..) |ty, t| {
        if (ty != .integer and ty != .rune and ty != .float) continue;
        var used = false;
        for (ir.functions.items) |function| {
            const body = function.body orelse continue;
            for (body.instructions) |instruction| {
                if (instruction.op == .to_string and @backingInt(body.instructions[@backingInt(instruction.op.to_string)].ty) == t) used = true;
            }
        }
        if (!used) continue;
        if (ty == .rune) {
            try w.print("static nov_string nov_to_string_{d}(uint32_t value) {{\n", .{t});
            try w.writeAll("    unsigned char bytes[4];\n    uintptr_t len = nov_rt_encode_rune(value, bytes);\n    return nov_string_concat((nov_string){0}, (nov_string){NULL, bytes, len});\n}\n");
            continue;
        }
        if (ty == .float) {
            try w.print("static nov_string nov_to_string_{d}(", .{t});
            try writeType(ir, w, @fromBackingInt(@intCast(t)));
            try w.print(" value) {{\n    char bytes[32];\n    int len = nov_rt_float_format_{d}(bytes, value);\n", .{ty.float});
            try w.writeAll("    return nov_string_concat((nov_string){0}, (nov_string){NULL, (const unsigned char *)bytes, (uintptr_t)len});\n}\n");
            continue;
        }
        try w.writeAll("#include <inttypes.h>\nstatic nov_string nov_to_string_");
        try w.print("{d}(", .{t});
        try writeType(ir, w, @fromBackingInt(@intCast(t)));
        try w.print(" value) {{\n    char bytes[32];\n    int len = snprintf(bytes, sizeof bytes, \"%\" PRI{s}64, ({s}int64_t)value);\n", .{ if (ty.integer.signedness == .signed) "d" else "u", if (ty.integer.signedness == .signed) "" else "u" });
        try w.writeAll("    if (len < 0 || (size_t)len >= sizeof bytes) nov_rt_panic(\"integer formatting failed\");\n    return nov_string_concat((nov_string){0}, (nov_string){NULL, (const unsigned char *)bytes, (uintptr_t)len});\n}\n");
    }
}

fn usesStringConcat(ir: *const Ir) bool {
    for (ir.functions.items) |function| {
        const body = function.body orelse continue;
        for (body.instructions) |instruction| {
            if (instruction.op == .binary and ir.getType(instruction.ty).? == .string) return true;
            if (instruction.op == .to_string) {
                const ty = ir.getType(body.instructions[@backingInt(instruction.op.to_string)].ty).?;
                if (ty == .integer or ty == .rune or ty == .float or aggregateFormattingType(ty)) return true;
            }
        }
    }
    return false;
}

fn usesPrinting(ir: *const Ir) bool {
    for (ir.functions.items) |function| {
        const body = function.body orelse continue;
        for (body.instructions) |instruction| {
            if (instruction.op == .print) return true;
        }
    }
    return false;
}

fn writeRuntime(ir: *const Ir, w: *std.Io.Writer) WriteError!void {
    const printing = usesPrinting(ir);
    var panic_used = printing or usesManagedRuntime(ir);
    var source_panic = false;
    var assertion_used = false;
    for (ir.functions.items) |function| {
        const body = function.body orelse continue;
        for (body.instructions) |instruction| {
            if (instruction.op == .binary and ir.getType(instruction.ty).? == .integer and (instruction.op.binary.op == .div or instruction.op.binary.op == .mod)) panic_used = true;
            if (instruction.op == .panic) source_panic = true;
            if (instruction.op == .assertion) assertion_used = true;
        }
    }
    if (!panic_used and !source_panic and !assertion_used) return;
    try w.writeAll("#include <stdio.h>\n#include <stdlib.h>\n\n");
    if (source_panic) try w.writeAll(
        "static _Noreturn void nov_rt_panic_bytes(const unsigned char *message, uintptr_t len) {\n" ++
            "    (void)fflush(stdout);\n" ++
            "    (void)fwrite(\"panic: \", 1, 7, stderr);\n" ++
            "    if (len) (void)fwrite(message, 1, (size_t)len, stderr);\n" ++
            "    (void)fputc('\\n', stderr);\n    exit(1);\n}\n",
    );
    if (panic_used) try w.writeAll(
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

// Float constants cross the C parser as integers, preserving even NaN payloads.
// Classification is bit-based so spelling is shared by every output path and
// does not depend on libc's NaN sign/payload formatting or math.h macros.
fn writeFloatRuntime(allocator: std.mem.Allocator, ir: *const Ir, origins: []const ?Origin, w: *std.Io.Writer) (WriteError || std.mem.Allocator.Error)!void {
    var reachable = false;
    for (origins, ir.types.items) |origin, ty| {
        if (origin != null and ty == .float) reachable = true;
    }
    if (!reachable) return;
    try w.writeAll(
        \\#include <float.h>
        \\#include <limits.h>
        \\#if defined(__FAST_MATH__) || (defined(__FINITE_MATH_ONLY__) && __FINITE_MATH_ONLY__)
        \\#error "Nov floats require IEEE semantics without fast-math"
        \\#endif
        \\_Static_assert(FLT_RADIX == 2 && FLT_EVAL_METHOD == 0, "Nov requires binary floats evaluated at their storage width");
        \\_Static_assert(sizeof(float) * CHAR_BIT == 32 && FLT_MANT_DIG == 24 && FLT_MIN_EXP == -125 && FLT_MAX_EXP == 128, "Nov f32 requires IEEE binary32");
        \\_Static_assert(sizeof(double) * CHAR_BIT == 64 && DBL_MANT_DIG == 53 && DBL_MIN_EXP == -1021 && DBL_MAX_EXP == 1024, "Nov f64 requires IEEE binary64");
        \\_Static_assert(FLT_HAS_SUBNORM == 1 && DBL_HAS_SUBNORM == 1, "Nov floats require subnormals");
        \\#if defined(__clang__)
        \\#pragma clang fp contract(off)
        \\#elif defined(__GNUC__)
        \\#pragma GCC optimize ("fp-contract=off")
        \\#else
        \\#pragma STDC FP_CONTRACT OFF
        \\#endif
        \\
    );
    const formatted = try allocator.alloc(bool, ir.types.items.len);
    defer allocator.free(formatted);
    const converted = try allocator.alloc(bool, ir.types.items.len);
    defer allocator.free(converted);
    _ = markFormattingTypes(ir, formatted, converted);
    for ([_]u16{ 32, 64 }) |bits| {
        var constants = false;
        var formatting = false;
        for (ir.types.items, formatted) |ty, used| {
            if (ty == .float and ty.float == bits and used) formatting = true;
        }
        for (ir.functions.items) |function| {
            const body = function.body orelse continue;
            for (body.instructions) |instruction| {
                const ty = ir.getType(instruction.ty).?;
                if (instruction.op == .constant and ty == .float and ty.float == bits) constants = true;
                if (instruction.op == .to_string) {
                    const source_ty = ir.getType(body.instructions[@backingInt(instruction.op.to_string)].ty).?;
                    if (source_ty == .float and source_ty.float == bits) formatting = true;
                }
                if (instruction.op == .print) for (instruction.op.print.args) |arg| {
                    const source_ty = ir.getType(body.instructions[@backingInt(arg)].ty).?;
                    if (source_ty == .float and source_ty.float == bits) formatting = true;
                };
            }
        }
        if (!constants and !formatting) continue;
        try w.writeAll("#include <string.h>\n");
        const c_type = if (bits == 32) "float" else "double";
        if (constants) try w.print("static {s} nov_rt_float_bits_{d}(uint{d}_t bits) {{ {s} value; memcpy(&value, &bits, sizeof value); return value; }}\n", .{ c_type, bits, bits, c_type });
        if (formatting) {
            try w.writeAll("#include <stdio.h>\n");
            try w.print("static int nov_rt_float_format_{d}(char bytes[32], {s} value) {{\n    uint{d}_t bits; memcpy(&bits, &value, sizeof bits);\n", .{ bits, c_type, bits });
            try w.print("    uint{d}_t magnitude = bits & UINT{d}_C({d});\n    const char *special = NULL;\n", .{ bits, bits, if (bits == 32) @as(u64, 0x7fffffff) else @as(u64, 0x7fffffffffffffff) });
            const infinity: u64 = if (bits == 32) 0x7f800000 else 0x7ff0000000000000;
            try w.print("    if (magnitude > UINT{d}_C({d})) special = \"nan\";\n    else if (magnitude == UINT{d}_C({d})) special = bits != magnitude ? \"-inf\" : \"inf\";\n    else if (!magnitude && bits) special = \"-0\";\n", .{ bits, infinity, bits, infinity });
            try w.print("    int len = special ? snprintf(bytes, 32, \"%s\", special) : snprintf(bytes, 32, \"%.{d}g\", (double)value);\n", .{if (bits == 32) @as(u8, 9) else @as(u8, 17)});
            try w.writeAll("    if (len < 0 || len >= 32) nov_rt_panic(\"float formatting failed\");\n    return len;\n}\n");
        }
    }
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
                const body = function.body orelse continue;
                for (body.instructions) |instruction| {
                    if (@backingInt(instruction.ty) == t and instruction.op == .binary and instruction.op.binary.op == op) used = true;
                }
            }
            if (!used) continue;

            const bits = workingBits(ty);
            try w.writeAll("\nstatic ");
            try writeType(ir, w, @fromBackingInt(@intCast(t)));
            try w.print(" nov_rt_{s}_{d}(", .{ @tagName(op), t });
            try writeType(ir, w, @fromBackingInt(@intCast(t)));
            try w.writeAll(" a, ");
            try writeType(ir, w, @fromBackingInt(@intCast(t)));
            try w.print(" b) {{\n    if (b == 0) nov_rt_panic(\"{s} by zero\");\n", .{if (op == .div) "division" else "remainder"});
            if (ty.integer.signedness == .signed) {
                // Unsigned subtraction also handles the magnitude of MIN.
                try w.print("    const uint{d}_t x = a < 0 ? UINT{d}_C(0) - (uint{d}_t)a : (uint{d}_t)a;\n", .{ bits, bits, bits, bits });
                try w.print("    const uint{d}_t y = b < 0 ? UINT{d}_C(0) - (uint{d}_t)b : (uint{d}_t)b;\n", .{ bits, bits, bits, bits });
                try w.print("    const uint{d}_t r = x {s} y;\n    return (", .{ bits, if (op == .div) "/" else "%" });
                try writeType(ir, w, @fromBackingInt(@intCast(t)));
                try w.print(")({s} ? UINT{d}_C(0) - r : r);\n}}\n", .{ if (op == .div) "(a < 0) != (b < 0)" else "a < 0", bits });
            } else {
                try w.writeAll("    return (");
                try writeType(ir, w, @fromBackingInt(@intCast(t)));
                try w.print(")((uint{d}_t)a {s} (uint{d}_t)b);\n}}\n", .{ bits, if (op == .div) "/" else "%", bits });
            }
        }
    }
}

/// Only called for types accepted by `analyze`.
fn writeType(ir: *const Ir, w: *std.Io.Writer, id: Ir.TypeId) WriteError!void {
    switch (ir.types.items[@backingInt(id)]) {
        .void => try w.writeAll("void"),
        .boolean => try w.writeAll("bool"),
        .float => |bits| try w.writeAll(if (bits == 32) "float" else "double"),
        .enumeration, .rune => try w.writeAll("uint32_t"),
        .integer => |integer| try w.print("{s}int{d}_t", .{ if (integer.signedness == .unsigned) "u" else "", integer.bits }),
        .function => try w.print("nov_fn_{d}", .{@backingInt(id)}),
        .range => try w.print("nov_range_{d}", .{@backingInt(id)}),
        .array => try w.writeAll("nov_array"),
        .string => try w.writeAll("nov_string"),
        .record => try w.print("nov_record_{d}", .{@backingInt(id)}),
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
    if (function.noreturn) try w.writeAll("_Noreturn ");
    const signature = ir.types.items[@backingInt(function.ty)].function;
    try writeType(ir, w, signature.result);
    try w.writeByte(' ');
    try writeScopedFunctionName(w, function);
    try w.writeByte('(');
    if (function.is_closure) try w.writeAll("void *nov_env") else if (signature.params.len == 0) try w.writeAll("void");
    for (signature.params, 0..) |param, i| {
        if (i != 0 or function.is_closure) try w.writeAll(", ");
        try writeType(ir, w, param);
        try w.print(" p{d}", .{i});
    }
    try w.writeByte(')');
}

/// Injective: `_` becomes `__` and any other non-alphanumeric byte `_XX`, so
/// Nov names can neither collide with each other nor with C keywords, libc or
/// the generated `nov_fn_N` typedefs.
fn sameFunction(a: Ir.Function, b: Ir.Function) bool {
    // Verified extern/export aliases can denote one C function under distinct
    // Nov names, including recursive calls through a bodyless declaration.
    if (a.extern_name orelse a.export_name) |a_symbol| {
        if (b.extern_name orelse b.export_name) |b_symbol| return std.mem.eql(u8, a_symbol, b_symbol);
    }
    return a.span.source == b.span.source and std.mem.eql(u8, a.name, b.name);
}

fn writeScopedFunctionName(w: *std.Io.Writer, function: Ir.Function) WriteError!void {
    if (function.extern_name orelse function.export_name) |symbol| return w.writeAll(symbol);
    // The single underscore separator cannot occur inside an encoded Nov name.
    if (@backingInt(function.span.source) != 0) try w.print("nov_M{d}_", .{@backingInt(function.span.source)});
    try writeFunctionName(w, function.name);
}

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
        .float_bits => |bits| try w.print("nov_rt_float_bits_{d}(UINT{d}_C({d}))", .{ ty.float, ty.float, bits }),
        .enum_tag, .rune => |value| try w.print("UINT32_C({d})", .{value}),
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
        const body = function.body orelse continue;
        for (body.instructions) |instruction| {
            if (instruction.op == .array_contains and body.instructions[@backingInt(instruction.op.array_contains.value)].ty == element) return true;
        }
    }
    return false;
}

fn writeBody(allocator: std.mem.Allocator, ir: *const Ir, root_counts: []const usize, w: *std.Io.Writer, function: Ir.Function) (WriteError || std.mem.Allocator.Error)!void {
    const signature = ir.types.items[@backingInt(function.ty)].function;
    const body = function.body.?;
    const transfers = try ArrayTransfers.init(allocator, ir, body);
    defer transfers.deinit(allocator);
    const value_used = try allocator.alloc(bool, body.instructions.len);
    defer allocator.free(value_used);
    @memset(value_used, false);
    const param_used = try allocator.alloc(bool, signature.params.len);
    defer allocator.free(param_used);
    @memset(param_used, false);
    value_used[@backingInt(body.result)] = true;
    for (body.instructions) |instruction| switch (instruction.op) {
        .array_literal, .record_init => |elements| for (elements) |value| {
            value_used[@backingInt(value)] = true;
        },
        .union_init => |init_value| {
            if (init_value.payload) |payload| value_used[@backingInt(payload)] = true;
        },
        .union_tag, .union_payload => |read| value_used[@backingInt(read.base)] = true,
        .field_read => |read| value_used[@backingInt(read.base)] = true,
        .range_init => |r| {
            value_used[@backingInt(r.start)] = true;
            value_used[@backingInt(r.end)] = true;
        },
        .range_start, .range_end, .range_inclusive, .array_clone, .array_len, .iteration_len, .to_string => |value| value_used[@backingInt(value)] = true,
        .string_slice => |r| {
            value_used[@backingInt(r.base)] = true;
            if (r.start) |value| value_used[@backingInt(value)] = true;
            if (r.end) |value| value_used[@backingInt(value)] = true;
        },
        .array_index => |r| {
            value_used[@backingInt(r.array)] = true;
            value_used[@backingInt(r.index)] = true;
        },
        .array_contains => |r| {
            value_used[@backingInt(r.value)] = true;
            value_used[@backingInt(r.array)] = true;
        },
        .array_set => |r| {
            value_used[@backingInt(r.array)] = true;
            value_used[@backingInt(r.index)] = true;
            value_used[@backingInt(r.value)] = true;
        },
        .array_append => |r| {
            value_used[@backingInt(r.lhs)] = true;
            value_used[@backingInt(r.rhs)] = true;
        },
        .borrow_reserve => |slot| value_used[@backingInt(slot)] = true,
        .borrow_start => |start| if (start.index) |index| {
            value_used[@backingInt(index)] = true;
        },
        .string_decode, .string_next => |r| {
            value_used[@backingInt(r.array)] = true;
            value_used[@backingInt(r.index)] = true;
        },
        .borrow_read => {},
        .borrow_store => |store| value_used[@backingInt(store.value)] = true,
        .parameter => |index| param_used[index] = true,
        .local_slot => |value| value_used[@backingInt(value)] = true,
        .local_load => |slot| value_used[@backingInt(slot)] = true,
        .field_store => |store| {
            value_used[@backingInt(store.slot)] = true;
            value_used[@backingInt(store.value)] = true;
        },
        .local_store => |store| {
            value_used[@backingInt(store.slot)] = true;
            value_used[@backingInt(store.value)] = true;
        },
        .unary => |unary| value_used[@backingInt(unary.operand)] = true,
        .binary => |binary| {
            value_used[@backingInt(binary.lhs)] = true;
            value_used[@backingInt(binary.rhs)] = true;
        },
        .call => |call| {
            if (!call.noreturn or sameFunction(ir.functions.items[@backingInt(body.instructions[@backingInt(call.callee)].op.function_ref)], function)) value_used[@backingInt(call.callee)] = true;
            for (call.args) |arg| value_used[@backingInt(arg)] = true;
        },
        .print => |print| for (print.args) |arg| {
            value_used[@backingInt(arg)] = true;
        },
        .short_circuit => |region| {
            value_used[@backingInt(region.lhs)] = true;
            value_used[@backingInt(region.rhs)] = true;
        },
        .branch => |region| {
            value_used[@backingInt(region.condition)] = true;
            value_used[@backingInt(region.then_value)] = true;
            if (region.else_value) |value| value_used[@backingInt(value)] = true;
        },
        .assertion => |assertion| {
            value_used[@backingInt(assertion.condition)] = true;
            if (assertion.message) |message| value_used[@backingInt(message)] = true;
        },
        .panic => |value| value_used[@backingInt(value)] = true,
        .return_value, .loop_break => |value| {
            if (value) |v| value_used[@backingInt(v)] = true;
        },
        .loop => |region| {
            if (!region.unconditional) value_used[@backingInt(region.condition)] = true;
            value_used[@backingInt(region.body)] = true;
            if (region.continuation) |value| value_used[@backingInt(value)] = true;
            if (region.else_value) |value| value_used[@backingInt(value)] = true;
        },
        .capture_value, .capture_cell, .loop_continue, .constant, .function_ref => {},
        .closure_specialize => |closure| {
            value_used[@backingInt(closure.environment)] = true;
        },
        .closure_init => |closure| for (closure.captures) |value| {
            value_used[@backingInt(value)] = true;
        },
    };

    const managed = needsRootFrame(ir, function);
    if (usesClosures(ir)) try w.writeAll("    (void)nov_env_element;\n");
    if (managed) {
        // The embedded internal API also serves library emission, whose host
        // owns shutdown. Keep strict C11 unused-function warnings portable.
        try w.writeAll("    (void)nov_gc_register_globals; (void)nov_gc_unregister_globals; (void)nov_gc_shutdown; (void)nov_gc_pop; (void)nov_gc_alloc;\n    (void)nov_array_clone; (void)nov_array_index; (void)nov_array_append;\n");
        var roots: usize = @intFromBool(function.is_closure);
        if (function.is_closure) try w.writeAll("    (void)nov_env;\n");
        for (body.instructions, 0..) |instruction, i| {
            if (!instruction.boxed) continue;
            try w.print("    void *nov_cell_{d} = NULL;\n#define v{d} (*(", .{ i, i });
            try writeType(ir, w, instruction.ty);
            try w.print(" *)nov_cell_{d})\n", .{i});
            roots += 1;
        }
        for (signature.params) |param| if (managedType(ir, ir.getType(param).?)) {
            roots = std.math.add(usize, roots, root_counts[@backingInt(param)]) catch return error.OutOfMemory;
        };
        for (body.instructions, 0..) |instruction, i| {
            if (instruction.boxed or !managedType(ir, ir.getType(instruction.ty).?)) continue;
            try w.writeAll("    ");
            try writeType(ir, w, instruction.ty);
            try w.print(" v{d} = {{0}};\n", .{i});
            roots = std.math.add(usize, roots, root_counts[@backingInt(instruction.ty)]) catch return error.OutOfMemory;
        }
        // Stable function-scope void* fields, never casts from typed T** slots.
        if (roots != 0) {
            try w.writeAll("    void **nov_roots[] = {");
            if (function.is_closure) try w.writeAll(" &nov_env,");
            for (body.instructions, 0..) |instruction, i| if (instruction.boxed) {
                try w.print(" &nov_cell_{d},", .{i});
            };
            for (signature.params, 0..) |param, p| if (managedType(ir, ir.getType(param).?)) {
                try writeRoots(ir, root_counts, w, param, "p", p);
            };
            for (body.instructions, 0..) |instruction, i| if (!instruction.boxed and managedType(ir, ir.getType(instruction.ty).?)) {
                try writeRoots(ir, root_counts, w, instruction.ty, "v", i);
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
                    if (ir.getType(candidate.ty).? != .void and !managedType(ir, ir.getType(candidate.ty).?)) {
                        try w.writeAll("    ");
                        try writeType(ir, w, candidate.ty);
                        try w.print(" v{d};\n", .{end});
                    }
                    try w.print("    while (true) {{\n    goto nov_cond{d};\n    nov_cond{d}:;\n", .{ end, end });
                }
                if (region.body_start == i and !region.unconditional and !Ir.terminates(body, region.condition))
                    try w.print("    if (!v{d}) goto nov_{s}{d};\n", .{ @backingInt(region.condition), if (region.else_start != null) "else" else "exit", end });
                if (region.continuation_start != null and region.continuation_start.? == i)
                    try w.print("    goto nov_cont{d};\n    nov_cont{d}:;\n", .{ end, end });
                if (region.else_start != null and region.else_start.? == i)
                    try w.print("    }}\n    goto nov_else{d};\n    nov_else{d}:;\n    {{\n", .{ end, end });
            } else if (candidate.op == .branch) {
                const region = candidate.op.branch;
                const has_value = ir.types.items[@backingInt(candidate.ty)] != .void;
                if (region.then_start == i) {
                    if (has_value and !managedType(ir, ir.getType(candidate.ty).?)) {
                        try w.writeAll("    ");
                        try writeType(ir, w, candidate.ty);
                        try w.print(" v{d};\n", .{end});
                    }
                    try w.print("    if (v{d}) {{\n", .{@backingInt(region.condition)});
                }
                if (region.else_start == i) {
                    if (has_value and !Ir.terminates(body, region.then_value)) try w.print("    v{d} = v{d};\n", .{ end, @backingInt(region.then_value) });
                    try w.writeAll("    } else {\n");
                }
            } else if (candidate.op == .short_circuit and candidate.op.short_circuit.rhs_start == i) {
                const region = candidate.op.short_circuit;
                try w.print("    bool v{d} = v{d};\n    if ({s}v{d}) {{\n", .{ end, @backingInt(region.lhs), if (region.kind == .@"or") "!" else "", @backingInt(region.lhs) });
            }
        }
        if (instruction.op == .loop) {
            if (instruction.op.loop.else_value) |value| {
                if (ir.getType(instruction.ty).? != .void and !Ir.terminates(body, value))
                    try w.print("    v{d} = v{d};\n", .{ i, @backingInt(value) });
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
                        try w.print("    v{d} = v{d};\n", .{ owner, @backingInt(value) });
                }
                try w.print("    goto nov_exit{d};\n", .{owner});
            } else {
                const from_body = i >= loop.body_start and i < (loop.continuation_start orelse loop.else_start orelse owner);
                try w.print("    goto nov_{s}{d};\n", .{ if (from_body and loop.continuation != null) "cont" else "cond", owner });
            }
            continue;
        }
        if (instruction.op == .branch) {
            if (ir.types.items[@backingInt(instruction.ty)] != .void and !Ir.terminates(body, instruction.op.branch.else_value.?)) {
                try w.print("    v{d} = v{d};\n", .{ i, @backingInt(instruction.op.branch.else_value.?) });
            }
            try w.writeAll("    }\n");
            if (ir.types.items[@backingInt(instruction.ty)] != .void and !value_used[i]) try w.print("    (void)v{d};\n", .{i});
            continue;
        }
        if (instruction.op == .short_circuit) {
            if (!Ir.terminates(body, instruction.op.short_circuit.rhs)) try w.print("    v{d} = v{d};\n", .{ i, @backingInt(instruction.op.short_circuit.rhs) });
            try w.writeAll("    }\n");
            if (!value_used[i]) try w.print("    (void)v{d};\n", .{i});
            continue;
        }
        const ty = ir.types.items[@backingInt(instruction.ty)];
        if (instruction.op == .call and instruction.op.call.noreturn) {
            const call = instruction.op.call;
            const target = body.instructions[@backingInt(call.callee)].op.function_ref;
            try w.writeAll("    ");
            if (sameFunction(ir.functions.items[@backingInt(target)], function)) {
                // Volatile indirection avoids C compilers rejecting valid Nov
                // recursion under -Winfinite-recursion. C11 has no noreturn
                // pointer qualifier, so explicitly represent the absent successor.
                try w.writeAll("{ ");
                try writeType(ir, w, body.instructions[@backingInt(call.callee)].ty);
                if (usesClosures(ir)) try w.print(" volatile nov_target = v{d};\n    nov_target.code(nov_target.env", .{@backingInt(call.callee)}) else try w.print(" volatile nov_target = v{d};\n    nov_target(", .{@backingInt(call.callee)});
                for (call.args, 0..) |arg, index| {
                    if (index != 0 or (usesClosures(ir) and sameFunction(ir.functions.items[@backingInt(target)], function))) try w.writeAll(", ");
                    try w.print("v{d}", .{@backingInt(arg)});
                }
                try w.writeAll(");\n    for (;;) {}\n    }\n");
            } else {
                try writeScopedFunctionName(w, ir.functions.items[@backingInt(target)]);
                try w.writeByte('(');
                for (call.args, 0..) |arg, index| {
                    if (index != 0 or (usesClosures(ir) and sameFunction(ir.functions.items[@backingInt(target)], function))) try w.writeAll(", ");
                    try w.print("v{d}", .{@backingInt(arg)});
                }
                try w.writeAll(");\n");
            }
            continue;
        }
        if (instruction.op == .assertion) {
            const assertion = instruction.op.assertion;
            try w.print("    if (!v{d}) {{\n        (void)fflush(stdout);\n        (void)fwrite(\"", .{@backingInt(assertion.condition)});
            for (assertion.location) |byte| try w.print("\\{o:0>3}", .{byte});
            try w.print(": assertion failed\", 1, {d}, stderr);\n", .{assertion.location.len + ": assertion failed".len});
            if (assertion.message) |message| {
                const value = @backingInt(message);
                try w.print("        (void)fwrite(\": \", 1, 2, stderr);\n        if (v{d}.len) (void)fwrite(v{d}.data, 1, (size_t)v{d}.len, stderr);\n", .{ value, value, value });
            }
            try w.writeAll("        (void)fputc('\\n', stderr);\n        exit(1);\n    }\n");
            continue;
        }
        if (instruction.op == .panic) {
            const value = @backingInt(instruction.op.panic);
            try w.print("    nov_rt_panic_bytes(v{d}.data, v{d}.len);\n", .{ value, value });
            continue;
        }
        if (instruction.op == .return_value) {
            if (managed) try w.writeAll("    nov_gc_pop(&nov_rt_heap, &nov_frame);\n");
            if (instruction.op.return_value) |value| {
                if (ir.types.items[@backingInt(body.instructions[@backingInt(value)].ty)] != .void) {
                    try w.print("    return v{d};\n", .{@backingInt(value)});
                } else try w.writeAll("    return;\n");
            } else try w.writeAll("    return;\n");
            continue;
        }
        if (instruction.op == .borrow_reserve) continue;
        if (instruction.op == .borrow_start) {
            const start = instruction.op.borrow_start;
            const slot = body.instructions[@backingInt(start.reservation)].op.borrow_reserve;
            if (start.index) |index| try w.print("    const uintptr_t b{d} = nov_array_index(v{d}, v{d});\n    (void)b{d};\n", .{ i, @backingInt(slot), @backingInt(index), i });
            continue;
        }
        if (instruction.op == .borrow_store) {
            const store = instruction.op.borrow_store;
            const slot = Ir.borrowOwner(body, store.borrow);
            const owner = ir.getType(body.instructions[@backingInt(slot)].ty).?;
            if (body.instructions[@backingInt(store.borrow)].op.borrow_start.index == null) {
                try w.print("    v{d}", .{@backingInt(slot)});
            } else {
                try w.writeAll("    ((");
                try writeType(ir, w, owner.array);
                try w.print(" *)v{d}.data)[b{d}]", .{ @backingInt(slot), @backingInt(store.borrow) });
            }
            for (store.path) |index| try w.print(".f{d}", .{index});
            try w.print(" = v{d};\n", .{@backingInt(store.value)});
            continue;
        }
        if (instruction.op == .field_store) {
            const store = instruction.op.field_store;
            try w.print("    v{d}", .{@backingInt(store.slot)});
            for (store.path) |index| try w.print(".f{d}", .{index});
            try w.print(".f{d} = v{d};\n", .{ store.index, @backingInt(store.value) });
            continue;
        }
        if (instruction.op == .local_store) {
            const store = instruction.op.local_store;
            try w.print("    v{d} = v{d};\n", .{ @backingInt(store.slot), @backingInt(store.value) });
            continue;
        }
        if (instruction.op == .array_clone and ty == .record) {
            try w.print("    v{d} = nov_copy_{d}(v{d});\n", .{ i, @backingInt(instruction.ty), @backingInt(instruction.op.array_clone) });
            continue;
        }
        if (instruction.op == .union_payload) {
            const read = instruction.op.union_payload;
            try w.print("    if (v{d}.tag != {d}) nov_rt_panic(\"inactive union payload\");\n", .{ @backingInt(read.base), read.index });
        }
        if (instruction.boxed) {
            if (instruction.op == .local_slot) {
                try w.print("    nov_cell_{d} = nov_gc_alloc(&nov_rt_heap, &nov_element_{d});\n    v{d} = v{d};\n", .{ i, @backingInt(instruction.ty), i, @backingInt(instruction.op.local_slot) });
            } else {
                try w.print("    nov_cell_{d} = ((void **)nov_env)[{d}];\n", .{ i, instruction.op.capture_cell });
            }
            try w.print("    (void)nov_cell_{d};\n", .{i});
            continue;
        }
        if (instruction.op == .capture_value) {
            if (ty == .void) continue;
            if (managedType(ir, ty)) try w.print("    v{d} = ", .{i}) else {
                try w.writeAll("    const ");
                try writeType(ir, w, instruction.ty);
                try w.print(" v{d} = ", .{i});
            }
            try w.writeAll("*(");
            try writeType(ir, w, instruction.ty);
            try w.print(" *)((void **)nov_env)[{d}];\n    (void)v{d};\n", .{ instruction.op.capture_value, i });
            continue;
        }
        if (instruction.op == .closure_specialize) {
            const closure = instruction.op.closure_specialize;
            try w.print("    v{d}.code = ", .{i});
            try writeScopedFunctionName(w, ir.getFunction(closure.function).?);
            try w.print(";\n    v{d}.env = v{d}.env;\n    (void)v{d};\n", .{ i, @backingInt(closure.environment), i });
            continue;
        }
        if (instruction.op == .closure_init) {
            const closure = instruction.op.closure_init;
            const target = ir.getFunction(closure.function).?;
            try w.print("    v{d}.code = ", .{i});
            try writeScopedFunctionName(w, target);
            try w.writeAll(";\n");
            if (closure.captures.len != 0) {
                try w.print("    v{d}.env = nov_gc_alloc_repeated(&nov_rt_heap, &nov_env_element, {d});\n", .{ i, closure.captures.len });
                for (closure.captures, target.captures, 0..) |value, capture, c| {
                    if (capture.mutable) {
                        try w.print("    ((void **)v{d}.env)[{d}] = nov_cell_{d};\n", .{ i, c, @backingInt(value) });
                    } else if (ir.getType(capture.ty).? != .void) {
                        try w.print("    ((void **)v{d}.env)[{d}] = nov_gc_alloc(&nov_rt_heap, &nov_element_{d});\n    *(", .{ i, c, @backingInt(capture.ty) });
                        try writeType(ir, w, capture.ty);
                        try w.print(" *)((void **)v{d}.env)[{d}] = v{d};\n", .{ i, c, @backingInt(value) });
                    }
                }
            }
            try w.print("    (void)v{d};\n", .{i});
            continue;
        }
        if (ty == .void) {
            // Void values have no C object; emit their effects as statements.
            if (instruction.op == .print) try writePrint(ir, w, body, instruction.op.print);
            if (instruction.op == .call) {
                try w.writeAll("    ");
                try writeCall(ir, w, instruction.op.call);
                try w.writeAll(";\n");
            }
            continue;
        }
        if (ty == .string) {
            try w.print("    v{d} = ", .{i});
            switch (instruction.op) {
                .field_read, .union_payload => |read| try w.print("v{d}.f{d}", .{ @backingInt(read.base), read.index }),
                .borrow_read => |borrow| try writeBorrowRead(ir, w, body, borrow, instruction.ty),
                .constant => |id| try writeConstant(w, ir.getConstant(id).?, ty),
                .parameter => |index| try w.print("p{d}", .{index}),
                .local_slot, .local_load => |value| try w.print("v{d}", .{@backingInt(value)}),
                .to_string => |value| {
                    const source_ty = body.instructions[@backingInt(value)].ty;
                    switch (ir.getType(source_ty).?) {
                        .string => try w.print("v{d}", .{@backingInt(value)}),
                        .boolean => try w.print("(v{d} ? (nov_string){{NULL, (const unsigned char *)\"true\", 4}} : (nov_string){{NULL, (const unsigned char *)\"false\", 5}})", .{@backingInt(value)}),
                        .integer, .float, .rune, .array, .record, .enumeration => try w.print("nov_to_string_{d}(v{d})", .{ @backingInt(source_ty), @backingInt(value) }),
                        else => unreachable,
                    }
                },
                .string_slice => |r| {
                    try w.print("nov_string_slice(v{d}, ", .{@backingInt(r.base)});
                    if (r.start) |value| try w.print("v{d}", .{@backingInt(value)}) else try w.writeAll("0");
                    try w.writeAll(", ");
                    if (r.end) |value| try w.print("v{d}", .{@backingInt(value)}) else try w.writeAll("0");
                    try w.print(", {s}, {s})", .{ if (r.end != null) "true" else "false", if (r.inclusive) "true" else "false" });
                },
                .binary => |b| try w.print("nov_string_concat(v{d}, v{d})", .{ @backingInt(b.lhs), @backingInt(b.rhs) }),
                .array_index => |r| {
                    try w.writeAll("((nov_string *)");
                    try w.print("v{d}.data)[nov_array_index(v{d}, v{d})]", .{ @backingInt(r.array), @backingInt(r.array), @backingInt(r.index) });
                },
                .call => |call| try writeCall(ir, w, call),
                else => unreachable,
            }
            try w.writeAll(";\n");
            if (!value_used[i] or instruction.op == .local_slot) try w.print("    (void)v{d};\n", .{i});
            continue;
        }
        if (ty == .array) {
            try writeArrayValue(ir, w, body, transfers, instruction, i);
            if (!value_used[i] or instruction.op == .local_slot) try w.print("    (void)v{d};\n", .{i});
            continue;
        }
        if (ty == .record or (ty == .function and usesClosures(ir))) {
            try w.print("    v{d} = ", .{i});
        } else {
            try w.writeAll(if (instruction.op == .local_slot) "    " else "    const ");
            try writeType(ir, w, instruction.ty);
            try w.print(" v{d} = ", .{i});
        }
        switch (instruction.op) {
            .range_init => |r| {
                try w.writeByte('(');
                try writeType(ir, w, instruction.ty);
                try w.print("){{v{d}, v{d}, {s}}}", .{ @backingInt(r.start), @backingInt(r.end), if (r.inclusive) "true" else "false" });
            },
            .range_start => |value| try w.print("v{d}.start", .{@backingInt(value)}),
            .range_end => |value| try w.print("v{d}.end", .{@backingInt(value)}),
            .range_inclusive => |value| try w.print("v{d}.inclusive", .{@backingInt(value)}),
            .union_init => |init_value| {
                try w.writeByte('(');
                try writeType(ir, w, instruction.ty);
                try w.print("){{ .tag = {d}", .{init_value.index});
                if (init_value.payload) |payload| try w.print(", .f{d} = v{d}", .{ init_value.index, @backingInt(payload) });
                try w.writeAll(" }");
            },
            .union_tag => |read| try w.print("(v{d}.tag == {d})", .{ @backingInt(read.base), read.index }),
            .union_payload => |read| try w.print("v{d}.f{d}", .{ @backingInt(read.base), read.index }),
            .record_init => |values| {
                try w.writeByte('(');
                try writeType(ir, w, instruction.ty);
                try w.writeAll("){ ");
                for (values, 0..) |value, f| {
                    if (f != 0) try w.writeAll(", ");
                    try w.print("v{d}", .{@backingInt(value)});
                }
                if (values.len == 0) try w.writeByte('0');
                try w.writeAll(" }");
            },
            .borrow_read => |borrow| try writeBorrowRead(ir, w, body, borrow, instruction.ty),
            .borrow_reserve, .borrow_start, .borrow_store => unreachable,
            .field_read => |read| try w.print("v{d}.f{d}", .{ @backingInt(read.base), read.index }),
            .iteration_len => |value| try w.print("(v{d}.len > INTPTR_MAX ? (nov_rt_panic(\"iterable too long\"), (intptr_t)0) : (intptr_t)v{d}.len)", .{ @backingInt(value), @backingInt(value) }),
            .string_decode, .string_next => |r| try w.print("nov_string_cursor_{s}(v{d}, v{d})", .{ if (instruction.op == .string_decode) "rune" else "next", @backingInt(r.array), @backingInt(r.index) }),
            .array_len => |value| {
                if (ir.getType(body.instructions[@backingInt(value)].ty).? == .string)
                    try w.print("nov_string_scalar_len(v{d})", .{@backingInt(value)})
                else
                    try w.print("v{d}.len", .{@backingInt(value)});
            },
            .array_index => |r| {
                if (ir.getType(body.instructions[@backingInt(r.array)].ty).? == .string) {
                    try w.print("nov_string_rune_at(v{d}, v{d})", .{ @backingInt(r.array), @backingInt(r.index) });
                } else {
                    try w.writeAll("((");
                    try writeType(ir, w, instruction.ty);
                    try w.print(" *)v{d}.data)[nov_array_index(v{d}, v{d})]", .{ @backingInt(r.array), @backingInt(r.array), @backingInt(r.index) });
                }
            },
            .array_contains => |r| try w.print("nov_array_contains_{d}(v{d}, v{d})", .{ @backingInt(body.instructions[@backingInt(r.value)].ty), @backingInt(r.value), @backingInt(r.array) }),
            .array_literal, .array_clone, .array_set, .array_append, .to_string, .field_store => unreachable,
            .constant => |id| try writeConstant(w, ir.constants.items[@backingInt(id)], ty),
            .parameter => |index| try w.print("p{d}", .{index}),
            .local_slot, .local_load => |value| try w.print("v{d}", .{@backingInt(value)}),
            .function_ref => |id| {
                if (usesClosures(ir)) {
                    try w.print("(nov_fn_{d}){{nov_adapter_{d}, NULL}}", .{ @backingInt(instruction.ty), @backingInt(id) });
                } else try writeScopedFunctionName(w, ir.functions.items[@backingInt(id)]);
            },
            .capture_value, .capture_cell, .closure_init, .closure_specialize => unreachable,
            .unary => |unary| switch (unary.op) {
                .bool_not => try w.print("!v{d}", .{@backingInt(unary.operand)}),
                .bit_not => {
                    // Operands may be promoted to int, so convert back explicitly.
                    try w.writeByte('(');
                    try writeType(ir, w, instruction.ty);
                    try w.print(")~v{d}", .{@backingInt(unary.operand)});
                },
                .negate => negate: {
                    if (ty == .float) {
                        try w.print("({s})(-v{d})", .{ if (ty.float == 32) "float" else "double", @backingInt(unary.operand) });
                        break :negate;
                    }
                    try w.writeByte('(');
                    try writeType(ir, w, instruction.ty);
                    try w.print(")(UINT{d}_C(0) - (uint{d}_t)v{d})", .{ workingBits(ty), workingBits(ty), @backingInt(unary.operand) });
                },
            },
            .binary => |binary| arithmetic: {
                const lhs = @backingInt(binary.lhs);
                const rhs = @backingInt(binary.rhs);
                if (ir.getType(body.instructions[lhs].ty).? == .string) {
                    if (binary.op == .ne) try w.writeByte('!');
                    try w.print("nov_string_equal(v{d}, v{d})", .{ lhs, rhs });
                    break :arithmetic;
                }
                if (ty == .float) {
                    const operator = switch (binary.op) {
                        .add => "+",
                        .sub => "-",
                        .mul => "*",
                        .div => "/",
                        else => unreachable,
                    };
                    try w.print("({s})(v{d} {s} v{d})", .{ if (ty.float == 32) "float" else "double", lhs, operator, rhs });
                    break :arithmetic;
                }
                switch (binary.op) {
                    .add, .sub, .mul, .bit_and, .bit_or, .bit_xor => {
                        try w.writeByte('(');
                        try writeType(ir, w, instruction.ty);
                        const op = switch (binary.op) {
                            .add => "+",
                            .sub => "-",
                            .mul => "*",
                            .bit_and => "&",
                            .bit_or => "|",
                            .bit_xor => "^",
                            else => unreachable,
                        };
                        try w.print(")((uint{d}_t)v{d} {s} (uint{d}_t)v{d})", .{ workingBits(ty), lhs, op, workingBits(ty), rhs });
                        break :arithmetic;
                    },
                    .div, .mod => {
                        try w.print("nov_rt_{s}_{d}(v{d}, v{d})", .{ @tagName(binary.op), @backingInt(instruction.ty), lhs, rhs });
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
                    .add, .sub, .mul, .div, .mod, .bit_and, .bit_or, .bit_xor => unreachable,
                };
                try w.print("v{d} {s} v{d}", .{ @backingInt(binary.lhs), operator, @backingInt(binary.rhs) });
            },
            .call => |call| try writeCall(ir, w, call),
            .string_slice, .assertion, .panic, .loop, .loop_break, .loop_continue, .return_value, .print, .short_circuit, .branch, .local_store => unreachable,
        }
        try w.writeAll(";\n");
        if (!value_used[i] or instruction.op == .local_slot) try w.print("    (void)v{d};\n", .{i});
    }
    const result_type = ir.types.items[@backingInt(signature.result)];
    if (managed and !Ir.terminates(body, body.result)) try w.writeAll("    nov_gc_pop(&nov_rt_heap, &nov_frame);\n");
    if (result_type != .void and !Ir.terminates(body, body.result)) try w.print("    return v{d};\n", .{@backingInt(body.result)});
    for (body.instructions, 0..) |instruction, i| if (instruction.boxed) {
        try w.print("#undef v{d}\n", .{i});
    };
}

fn markCopyTypes(allocator: std.mem.Allocator, ir: *const Ir, copied: []bool) std.mem.Allocator.Error!void {
    @memset(copied, false);
    for (ir.functions.items) |function| {
        const body = function.body orelse continue;
        const transfers = try ArrayTransfers.init(allocator, ir, body);
        defer transfers.deinit(allocator);
        for (body.instructions, 0..) |instruction, i| switch (instruction.op) {
            .array_clone => copied[@backingInt(instruction.ty)] = true,
            .array_set => |update| {
                if (!transfers.canTake(body, update.array, i)) copied[@backingInt(instruction.ty)] = true;
                const element = ir.getType(instruction.ty).?.array;
                if (try ir.needsArrayCopy(element) and !transfers.canTake(body, update.value, i)) copied[@backingInt(element)] = true;
            },
            .array_append => |append| {
                const element = ir.getType(instruction.ty).?.array;
                if (try ir.needsArrayCopy(element) and (!transfers.canTake(body, append.lhs, i) or !transfers.canTake(body, append.rhs, i))) copied[@backingInt(element)] = true;
            },
            .array_literal => |elements| {
                const element = ir.getType(instruction.ty).?.array;
                if (try ir.needsArrayCopy(element)) for (elements) |value| {
                    if (!transfers.canTake(body, value, i)) copied[@backingInt(element)] = true;
                };
            },
            else => {},
        };
    }
    const formatted = try allocator.alloc(bool, ir.types.items.len);
    defer allocator.free(formatted);
    const converted = try allocator.alloc(bool, ir.types.items.len);
    defer allocator.free(converted);
    _ = markFormattingTypes(ir, formatted, converted);
    for (formatted, 0..) |used, t| {
        const id: Ir.TypeId = @fromBackingInt(@intCast(t));
        if (used and formatHook(ir, id) != null and try ir.needsArrayCopy(id)) copied[t] = true;
    }
    var changed = true;
    while (changed) {
        changed = false;
        for (ir.types.items, copied) |ty, copy| {
            if (!copy) continue;
            if (ty == .array) {
                const element = @backingInt(ty.array);
                if (!copied[element] and try ir.needsArrayCopy(ty.array)) {
                    copied[element] = true;
                    changed = true;
                }
            } else if (ty == .record) {
                for (ty.record.fields) |field| {
                    const f = @backingInt(field.ty);
                    if (!copied[f] and try ir.needsArrayCopy(field.ty)) {
                        copied[f] = true;
                        changed = true;
                    }
                }
            }
        }
    }
}

// Unproven values retain eager recursive copies for independent value storage.
// Fresh-temp transfers below do not introduce shared backing or COW.
fn writeCopyHelpers(ir: *const Ir, copy_types: []const bool, counts: []const usize, w: *std.Io.Writer) WriteError!void {
    for (ir.types.items, 0..) |_, i| {
        const id: Ir.TypeId = @fromBackingInt(@intCast(i));
        if (!copy_types[i]) continue;
        try w.writeAll("static inline ");
        try writeType(ir, w, id);
        try w.print(" nov_copy_{d}(", .{i});
        try writeType(ir, w, id);
        try w.writeAll(" p0);\n");
    }
    for (ir.types.items, 0..) |ty, i| {
        const id: Ir.TypeId = @fromBackingInt(@intCast(i));
        if (!copy_types[i]) continue;
        try w.writeAll("static inline ");
        try writeType(ir, w, id);
        try w.print(" nov_copy_{d}(", .{i});
        try writeType(ir, w, id);
        try w.writeAll(" p0) {\n    ");
        try writeType(ir, w, id);
        try w.writeAll(" v0 = p0;\n    void **roots[] = {");
        try writeRoots(ir, counts, w, id, "p", 0);
        try writeRoots(ir, counts, w, id, "v", 0);
        try w.print(" }};\n    nov_gc_roots frame = {{0}};\n    nov_gc_push(&nov_rt_heap, &frame, roots, {d});\n", .{counts[i] * 2});
        if (ty == .array) {
            try w.print("    v0 = nov_array_clone(p0, &nov_element_{d});\n", .{@backingInt(ty.array)});
            if (try ir.needsArrayCopy(ty.array)) {
                try w.writeAll("    for (uintptr_t j = 0; j < v0.len; ++j) {\n        ");
                try writeType(ir, w, ty.array);
                try w.writeAll(" *data = v0.data;\n");
                try w.print("        data[j] = nov_copy_{d}(data[j]);\n    }}\n", .{@backingInt(ty.array)});
            }
        } else {
            for (ty.record.fields, 0..) |field, f| {
                if (!try ir.needsArrayCopy(field.ty)) continue;
                if (ty.record.tagged) try w.print("    if (p0.tag == {d}) ", .{f}) else try w.writeAll("    ");
                try w.print("v0.f{d} = nov_copy_{d}(p0.f{d});\n", .{ f, @backingInt(field.ty), f });
            }
        }
        try w.writeAll("    nov_gc_pop(&nov_rt_heap, &frame);\n    return v0;\n}\n");
    }
}

fn writeElementLayout(_: *const Ir, w: *std.Io.Writer, element: Ir.TypeId) WriteError!void {
    try w.print("&nov_element_{d}", .{@backingInt(element)});
}

fn writeBorrowRead(ir: *const Ir, w: *std.Io.Writer, body: Ir.Body, borrow: Ir.ValueId, ty: Ir.TypeId) WriteError!void {
    const slot = Ir.borrowOwner(body, borrow);
    if (body.instructions[@backingInt(borrow)].op.borrow_start.index == null) {
        try w.print("v{d}", .{@backingInt(slot)});
    } else {
        try w.writeAll("((");
        try writeType(ir, w, ty);
        try w.print(" *)v{d}.data)[b{d}]", .{ @backingInt(slot), @backingInt(borrow) });
    }
}

// This is a backend proof, not a consuming convention imposed on arbitrary IR.
// Count every reference (including projections, captures, region yields and the
// body result), and recognize only constructors whose recursive backing is known.
const ArrayTransfers = struct {
    ir: *const Ir,
    uses: []usize,
    detached: []bool,

    fn count(uses: []usize, value: anytype) void {
        const T = @TypeOf(value);
        if (T == Ir.ValueId) {
            uses[@backingInt(value)] += 1;
            return;
        }
        switch (@typeInfo(T)) {
            .@"struct" => |info| inline for (info.field_names) |name| count(uses, @field(value, name)),
            .@"union" => switch (value) {
                inline else => |payload| count(uses, payload),
            },
            .optional => if (value) |payload| count(uses, payload),
            .pointer => |info| if (info.size == .slice and info.child == Ir.ValueId) {
                for (value) |operand| count(uses, operand);
            },
            else => {},
        }
    }

    fn init(allocator: std.mem.Allocator, ir: *const Ir, body: Ir.Body) std.mem.Allocator.Error!ArrayTransfers {
        const uses = try allocator.alloc(usize, body.instructions.len);
        errdefer allocator.free(uses);
        @memset(uses, 0);
        const detached = try allocator.alloc(bool, body.instructions.len);
        errdefer allocator.free(detached);
        @memset(detached, false);
        count(uses, body.result);
        for (body.instructions) |instruction| count(uses, instruction.op);
        const self: ArrayTransfers = .{ .ir = ir, .uses = uses, .detached = detached };
        for (body.instructions, 0..) |instruction, i| {
            detached[i] = switch (instruction.op) {
                // These operations either copy or use this same proof for inputs.
                .array_clone, .array_literal, .array_set, .array_append => true,
                .record_init => |fields| blk: {
                    for (fields) |field| {
                        if (try ir.needsArrayCopy(body.instructions[@backingInt(field)].ty) and !self.canTake(body, field, i)) break :blk false;
                    }
                    break :blk true;
                },
                .union_init => |init_value| if (init_value.payload) |payload|
                    !try ir.needsArrayCopy(body.instructions[@backingInt(payload)].ty) or self.canTake(body, payload, i)
                else
                    true,
                // Calls, merges, storage reads and projections are deliberately
                // not ownership evidence, even if lowering normally detaches them.
                else => false,
            };
        }
        return self;
    }

    fn deinit(self: ArrayTransfers, allocator: std.mem.Allocator) void {
        allocator.free(self.uses);
        allocator.free(self.detached);
    }

    fn canTake(self: ArrayTransfers, body: Ir.Body, value: Ir.ValueId, consumer: usize) bool {
        const producer = @backingInt(value);
        if (producer >= consumer or !self.detached[producer]) return false;
        if (self.uses[producer] != 1) {
            // Only an indexed update may consume an observed temporary. Do not
            // extend the consuming convention of constructors or append here.
            const update = body.instructions[consumer].op;
            if (update != .array_set or update.array_set.array != value) return false;
            if (!sameConditionalRegion(body, producer, consumer)) return false;
            var observations: usize = 0;
            for (body.instructions, 0..) |instruction, observer| {
                const observes = switch (instruction.op) {
                    .array_len, .iteration_len => |base| base == value,
                    .array_index => |read| read.array == value and switch (self.ir.getType(instruction.ty).?) {
                        .integer, .float, .boolean, .rune, .enumeration, .string => true,
                        // In particular, never let a projection carry mutable
                        // backing (or a closure environment) past the update.
                        else => false,
                    },
                    else => false,
                };
                if (!observes) continue;
                if (observer <= producer or observer >= consumer or
                    !sameLoopRegion(body, producer, observer) or
                    !sameConditionalRegion(body, producer, observer)) return false;
                observations += 1;
            }
            // All other references, including aliases, captures, calls, yields
            // and the body result, are still counted as consuming/escaping uses.
            if (self.uses[producer] != observations + 1) return false;
        }
        return sameLoopRegion(body, producer, consumer);
    }

    fn sameConditionalRegion(body: Ir.Body, a: usize, b: usize) bool {
        for (body.instructions, 0..) |instruction, end| switch (instruction.op) {
            .branch => |branch| {
                for ([_]usize{ branch.then_start, branch.else_start }, [_]usize{ branch.else_start, end }) |start, stop| {
                    if ((a >= start and a < stop) != (b >= start and b < stop)) return false;
                }
            },
            .short_circuit => |region| {
                if ((a >= region.rhs_start and a < end) != (b >= region.rhs_start and b < end)) return false;
            },
            else => {},
        };
        return true;
    }

    fn sameLoopRegion(body: Ir.Body, producer: usize, consumer: usize) bool {
        // A single static use can execute repeatedly. Never adopt an outer
        // temporary inside a loop, or across its separately executed regions.
        for (body.instructions, 0..) |instruction, end| {
            if (instruction.op != .loop) continue;
            const loop = instruction.op.loop;
            const boundaries = [_]usize{ loop.condition_start, loop.body_start, loop.continuation_start orelse loop.else_start orelse @intCast(end), loop.else_start orelse @intCast(end), end };
            for (boundaries[0 .. boundaries.len - 1], boundaries[1..]) |start, stop| {
                if ((producer >= start and producer < stop) != (consumer >= start and consumer < stop)) return false;
            }
        }
        return true;
    }
};

fn writeArrayValue(ir: *const Ir, w: *std.Io.Writer, body: Ir.Body, transfers: ArrayTransfers, instruction: Ir.Instruction, i: usize) WriteError!void {
    const element = ir.getType(instruction.ty).?.array;
    try w.print("    v{d} = ", .{i});
    switch (instruction.op) {
        .array_index => |r| try w.print("((nov_array *)v{d}.data)[nov_array_index(v{d}, v{d})]", .{ @backingInt(r.array), @backingInt(r.array), @backingInt(r.index) }),
        .field_read, .union_payload => |read| try w.print("v{d}.f{d}", .{ @backingInt(read.base), read.index }),
        .borrow_read => |borrow| try writeBorrowRead(ir, w, body, borrow, instruction.ty),
        .array_literal => |elements| {
            try w.print("nov_array_new({d}, {d}, ", .{ elements.len, elements.len });
            try writeElementLayout(ir, w, element);
            try w.writeAll(");\n");
            for (elements, 0..) |value, index| {
                try w.writeAll("    ((");
                try writeType(ir, w, element);
                if (try ir.needsArrayCopy(element) and !transfers.canTake(body, value, i)) {
                    try w.print(" *)v{d}.data)[{d}] = nov_copy_{d}(v{d});\n", .{ i, index, @backingInt(element), @backingInt(value) });
                } else try w.print(" *)v{d}.data)[{d}] = v{d};\n", .{ i, index, @backingInt(value) });
            }
            return;
        },
        .array_clone => |value| {
            try w.print("nov_copy_{d}(v{d})", .{ @backingInt(instruction.ty), @backingInt(value) });
        },
        .array_set => |r| {
            if (transfers.canTake(body, r.array, i)) try w.print("v{d}", .{@backingInt(r.array)}) else try w.print("nov_copy_{d}(v{d})", .{ @backingInt(instruction.ty), @backingInt(r.array) });
            try w.writeAll(";\n    ((");
            try writeType(ir, w, element);
            try w.print(" *)v{d}.data)[nov_array_index(v{d}, v{d})] = ", .{ i, i, @backingInt(r.index) });
            if (try ir.needsArrayCopy(element) and !transfers.canTake(body, r.value, i)) try w.print("nov_copy_{d}(v{d});\n", .{ @backingInt(element), @backingInt(r.value) }) else try w.print("v{d};\n", .{@backingInt(r.value)});
            return;
        },
        .array_append => |r| {
            try w.print("nov_array_append(v{d}, v{d}, ", .{ @backingInt(r.lhs), @backingInt(r.rhs) });
            try writeElementLayout(ir, w, element);
            try w.writeByte(')');
        },
        .parameter => |index| try w.print("p{d}", .{index}),
        .local_slot, .local_load => |value| try w.print("v{d}", .{@backingInt(value)}),
        .call => |call| try writeCall(ir, w, call),
        else => unreachable,
    }
    try w.writeAll(";\n");
    if (instruction.op == .array_append and try ir.needsArrayCopy(element)) {
        const append = instruction.op.array_append;
        // The runtime has already allocated a fresh outer buffer. Detach only
        // the portions whose inputs cannot transfer; never clone that buffer.
        for ([_]Ir.ValueId{ append.lhs, append.rhs }, 0..) |value, side| {
            if (transfers.canTake(body, value, i)) continue;
            try w.print("    for (uintptr_t j = {s}", .{if (side == 0) "0" else ""});
            if (side == 1) try w.print("v{d}.len", .{@backingInt(append.lhs)});
            try w.print("; j < v{d}.len; ++j) {{\n        ", .{if (side == 0) @backingInt(append.lhs) else i});
            try writeType(ir, w, element);
            try w.print(" *data = v{d}.data;\n        data[j] = nov_copy_{d}(data[j]);\n    }}\n", .{ i, @backingInt(element) });
        }
    }
}

test "floats direct IR exact bits IEEE operations comparisons and separate C ABI O0 O3" {
    var ir = Ir.init(test_gpa);
    defer ir.deinit();
    const span: Ir.Span = .{ .start = 0, .end = 0 };
    const boolean = try ir.internType(.boolean);
    var harness = std.Io.Writer.Allocating.init(test_gpa);
    defer harness.deinit();
    var checks = std.Io.Writer.Allocating.init(test_gpa);
    defer checks.deinit();
    try harness.writer.writeAll("#include <stdint.h>\n#include <string.h>\n#include <assert.h>\n#include <math.h>\n");
    for ([_]u16{ 32, 64 }) |bits| {
        const ty = try ir.internType(.{ .float = bits });
        const c_type = if (bits == 32) "float" else "double";
        const patterns: []const u64 = if (bits == 32)
            &.{ 0, 0x80000000, 1, 0x007fffff, 0x00800000, 0x3f800001, 0x7f7fffff, 0x7f800000, 0xff800000, 0x7fc12345, 0xffc54321 }
        else
            &.{ 0, 0x8000000000000000, 1, 0x000fffffffffffff, 0x0010000000000000, 0x3ff0000000000001, 0x7fefffffffffffff, 0x7ff0000000000000, 0xfff0000000000000, 0x7ff8123456789abc, 0xfff8abcdef123456 };
        const getter = try ir.internType(.{ .function = .{ .params = &.{}, .result = ty } });
        for (patterns, 0..) |pattern, i| {
            const name = try std.fmt.allocPrint(test_gpa, "float{d}constant{d}", .{ bits, i });
            defer test_gpa.free(name);
            const function = try ir.addFunction(name, getter, span);
            try ir.setExportName(function, name);
            const value = try ir.internConstant(.{ .ty = ty, .value = .{ .float_bits = pattern } });
            try ir.setFunctionBody(function, &.{.{ .ty = ty, .span = span, .op = .{ .constant = value } }}, @fromBackingInt(0));
            try harness.writer.print("extern {s} {s}(void);\n", .{ c_type, name });
            try checks.writer.print("{{ {s} value = {s}(); uint{d}_t bits; memcpy(&bits, &value, sizeof bits); assert(bits == UINT{d}_C({d})); }}\n", .{ c_type, name, bits, bits, pattern });
        }
        const unary = try ir.internType(.{ .function = .{ .params = &.{ty}, .result = ty } });
        const binary = try ir.internType(.{ .function = .{ .params = &.{ ty, ty }, .result = ty } });
        const comparison = try ir.internType(.{ .function = .{ .params = &.{ ty, ty }, .result = boolean } });
        const parameter: Ir.Instruction = .{ .ty = ty, .span = span, .op = .{ .parameter = 0 } };
        const other: Ir.Instruction = .{ .ty = ty, .span = span, .op = .{ .parameter = 1 } };
        for ([_]Ir.BinaryOp{ .add, .sub, .mul, .div, .eq, .ne, .lt, .le, .gt, .ge }) |op| {
            const compare = switch (op) {
                .eq, .ne, .lt, .le, .gt, .ge => true,
                else => false,
            };
            const name = try std.fmt.allocPrint(test_gpa, "float{d}{s}", .{ bits, @tagName(op) });
            defer test_gpa.free(name);
            const function = try ir.addFunction(name, if (compare) comparison else binary, span);
            try ir.setExportName(function, name);
            try ir.setFunctionBody(function, &.{ parameter, other, .{ .ty = if (compare) boolean else ty, .span = span, .op = .{ .binary = .{ .op = op, .lhs = @fromBackingInt(0), .rhs = @fromBackingInt(1) } } } }, @fromBackingInt(2));
            try harness.writer.print("extern {s} {s}({s}, {s});\n", .{ if (compare) "_Bool" else c_type, name, c_type, c_type });
            if (compare) {
                try checks.writer.print("assert({s}(({s})NAN, 1) == {d});\n", .{ name, c_type, @intFromBool(op == .ne) });
                try checks.writer.print("assert({s}(0, -0.0) == {d});\n", .{ name, @intFromBool(op == .eq or op == .le or op == .ge) });
            }
        }
        const negate_name = try std.fmt.allocPrint(test_gpa, "float{d}negate", .{bits});
        defer test_gpa.free(negate_name);
        const negate = try ir.addFunction(negate_name, unary, span);
        try ir.setExportName(negate, negate_name);
        try ir.setFunctionBody(negate, &.{ parameter, .{ .ty = ty, .span = span, .op = .{ .unary = .{ .op = .negate, .operand = @fromBackingInt(0) } } } }, @fromBackingInt(1));
        try harness.writer.print("extern {s} {s}({s});\n", .{ c_type, negate_name, c_type });
        const foreign_name = try std.fmt.allocPrint(test_gpa, "provider{d}", .{bits});
        defer test_gpa.free(foreign_name);
        const foreign = try ir.addFunction(foreign_name, unary, span);
        try ir.setExternName(foreign, foreign_name);
        try harness.writer.print("{s} {s}({s} value) {{ return value; }}\n", .{ c_type, foreign_name, c_type });
        const forward_name = try std.fmt.allocPrint(test_gpa, "float{d}forward", .{bits});
        defer test_gpa.free(forward_name);
        const forward = try ir.addFunction(forward_name, unary, span);
        try ir.setExportName(forward, forward_name);
        try ir.setFunctionBody(forward, &.{ parameter, .{ .ty = unary, .span = span, .op = .{ .function_ref = foreign } }, .{ .ty = ty, .span = span, .op = .{ .call = .{ .callee = @fromBackingInt(1), .args = &.{@fromBackingInt(0)} } } } }, @fromBackingInt(2));
        try harness.writer.print("extern {s} {s}({s});\n", .{ c_type, forward_name, c_type });
        try checks.writer.print("assert(signbit({s}(0))); assert(!signbit({s}(-0.0)));\nassert(signbit({s}(-0.0)));\n", .{ negate_name, negate_name, forward_name });
        try checks.writer.print("assert(isnan(float{d}div(0, 0))); assert(isinf(float{d}div(1, 0))); assert(float{d}div(-1, 0) < 0);\n", .{ bits, bits, bits });
        try checks.writer.print("assert(float{d}add(1, 2) == 3); assert(float{d}sub(1, 2) == -1); assert(float{d}mul(2, 3) == 6);\n", .{ bits, bits, bits });
        const large = if (bits == 32) "0x1p24f" else "0x1p53";
        try checks.writer.print("assert(float{d}sub(float{d}add({s}, 1), {s}) == 0);\n", .{ bits, bits, large, large });
    }
    try harness.writer.writeAll("int main(void) {\n");
    try harness.writer.writeAll(checks.written());
    try harness.writer.writeAll("return 0; }\n");
    const c = try emit(test_gpa, &ir, null);
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_rt_panic") == null);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_rt_div_") == null);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_gc_") == null);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_rt_float_format_") == null);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, harness.written(), 0, "", "", mode);
}

fn emitFloatFormatting(allocator: std.mem.Allocator) ![]u8 {
    var ir = Ir.init(allocator);
    defer ir.deinit();
    const span: Ir.Span = .{ .start = 0, .end = 0 };
    const unit = try ir.internType(.void);
    const string = try ir.internType(.string);
    const signature = try ir.internType(.{ .function = .{ .params = &.{}, .result = unit } });
    for ([_]u16{ 32, 64 }) |bits| {
        const ty = try ir.internType(.{ .float = bits });
        const array = try ir.internType(.{ .array = ty });
        const record = try ir.internType(.{ .record = .{ .identity = bits, .fields = &.{} } });
        try ir.setRecordFields(record, &.{.{ .name = "value", .ty = ty, .span = span }});
        const tagged = try ir.internType(.{ .record = .{ .identity = bits + 1, .tagged = true, .fields = &.{} } });
        try ir.setRecordFields(tagged, &.{.{ .name = "some", .ty = record, .span = span }});
        try ir.setTypeFormatting(record, "R", null);
        try ir.setTypeFormatting(tagged, "U", null);
        const patterns: []const u64 = if (bits == 32) &.{ 0x3dcccccd, 0x80000000, 1, 0x7f800000, 0xff800000, 0xffc12345 } else &.{ 0x3fb999999999999a, 0x8000000000000000, 1, 0x7ff0000000000000, 0xfff0000000000000, 0xfff8123456789abc };
        var instructions: std.ArrayList(Ir.Instruction) = .empty;
        defer instructions.deinit(allocator);
        var values: [6]Ir.ValueId = undefined;
        for (patterns, 0..) |pattern, i| {
            const constant = try ir.internConstant(.{ .ty = ty, .value = .{ .float_bits = pattern } });
            values[i] = @fromBackingInt(@intCast(instructions.items.len));
            try instructions.append(allocator, .{ .ty = ty, .span = span, .op = .{ .constant = constant } });
        }
        try instructions.append(allocator, .{ .ty = unit, .span = span, .op = .{ .print = .{ .args = &values, .newline = true } } });
        const array_value: Ir.ValueId = @fromBackingInt(@intCast(instructions.items.len));
        try instructions.append(allocator, .{ .ty = array, .span = span, .op = .{ .array_literal = &values } });
        try instructions.append(allocator, .{ .ty = unit, .span = span, .op = .{ .print = .{ .args = &.{array_value}, .newline = true } } });
        const record_value: Ir.ValueId = @fromBackingInt(@intCast(instructions.items.len));
        try instructions.append(allocator, .{ .ty = record, .span = span, .op = .{ .record_init = &.{values[0]} } });
        const union_value: Ir.ValueId = @fromBackingInt(@intCast(instructions.items.len));
        try instructions.append(allocator, .{ .ty = tagged, .span = span, .op = .{ .union_init = .{ .index = 0, .payload = record_value } } });
        try instructions.append(allocator, .{ .ty = unit, .span = span, .op = .{ .print = .{ .args = &.{union_value}, .newline = true } } });
        var strings: [6]Ir.ValueId = undefined;
        for (values, 0..) |value, i| {
            strings[i] = @fromBackingInt(@intCast(instructions.items.len));
            try instructions.append(allocator, .{ .ty = string, .span = span, .op = .{ .to_string = value } });
        }
        try instructions.append(allocator, .{ .ty = unit, .span = span, .op = .{ .print = .{ .args = &strings, .newline = true } } });
        const converted: Ir.ValueId = @fromBackingInt(@intCast(instructions.items.len));
        try instructions.append(allocator, .{ .ty = string, .span = span, .op = .{ .to_string = array_value } });
        try instructions.append(allocator, .{ .ty = unit, .span = span, .op = .{ .print = .{ .args = &.{converted}, .newline = true } } });
        const function = try ir.addFunction(if (bits == 32) "show32" else "show64", signature, span);
        try ir.setFunctionBody(function, instructions.items, @fromBackingInt(@intCast(instructions.items.len - 1)));
        const convert_ty = try ir.internType(.{ .function = .{ .params = &.{ty}, .result = string } });
        const convert = try ir.addFunction(if (bits == 32) "convert32" else "convert64", convert_ty, span);
        try ir.setFunctionBody(convert, &.{ .{ .ty = ty, .span = span, .op = .{ .parameter = 0 } }, .{ .ty = string, .span = span, .op = .{ .to_string = @fromBackingInt(0) } } }, @fromBackingInt(1));
    }
    return emit(allocator, &ir, null);
}

fn testFloatFormattingAllocations(allocator: std.mem.Allocator) !void {
    const c = try emitFloatFormatting(allocator);
    defer allocator.free(c);
}

test "floats direct IR formatting shared print interpolation aggregates source free forced GC O0 O3 OOM" {
    // Every temporary input and the IR itself have been freed before C execution.
    const library = try emitFloatFormatting(test_gpa);
    defer test_gpa.free(library);
    try std.testing.expect(std.mem.indexOf(u8, library, "_Alignof(float), 0, NULL") != null);
    try std.testing.expect(std.mem.indexOf(u8, library, "_Alignof(double), 0, NULL") != null);
    const output32 = "0.100000001 -0 1.40129846e-45 inf -inf nan\n";
    const array32 = "[0.100000001, -0, 1.40129846e-45, inf, -inf, nan]\n";
    const output64 = "0.10000000000000001 -0 4.9406564584124654e-324 inf -inf nan\n";
    const array64 = "[0.10000000000000001, -0, 4.9406564584124654e-324, inf, -inf, nan]\n";
    const expected = output32 ++ array32 ++ "U.some[ value: 0.100000001 ]\n" ++ output32 ++ array32 ++ output64 ++ array64 ++ "U.some[ value: 0.10000000000000001 ]\n" ++ output64 ++ array64;
    const c = try std.mem.concat(test_gpa, u8, &.{ library, "\nint main(void) { nov_rt_heap.collect_every_allocation = 1; nov_show32(); nov_show64(); if (nov_rt_heap.frames) return 70; nov_gc_collect(&nov_rt_heap); if (nov_rt_heap.live_objects) return 71; nov_gc_shutdown(&nov_rt_heap); return 0; }\n" });
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty;", 0, expected, "", mode);
    const oom = try std.mem.concat(test_gpa, u8, &.{ "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", library, "\nint main(void) { nov_rt_heap.collect_every_allocation = 1; (void)nov_convert32(1); return 0; }\n" });
    defer test_gpa.free(oom);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(oom, "typedef int nonempty;", 1, "", "panic: GC allocation failed\n", mode);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testFloatFormattingAllocations, .{});
}

const float_program =
    \\let divide32:(x:f32,y:f32)->f32=x/y
    \\let divide64:(x:f64,y:f64)->f64=x/y
    \\let R=struct{x:f32,y:f64,}
    \\let U=union{none,some:R,}
    \\let main:()={
    \\    let zero:f32=0
    \\    let nan=divide32(zero,zero)
    \\    let inf=divide32(1,zero)
    \\    let minus:f32=-0.0
    \\    @println(nan,inf,-inf,minus,divide64(0,0),divide64(1,0),divide64(-1,0),@as(f64,-0.0))
    \\    @println(nan==nan,nan!=nan,nan<zero,nan<=zero,nan>zero,nan>=zero,minus==zero)
    \\    let mut a:[]f32=[nan,minus,0.1]
    \\    @println(nan in a,zero in a,inf in a)
    \\    a[1]+=1.5
    \\    a[2]*=2
    \\    a+=[inf]
    \\    let mut r=R[.x=0.25,.y=0.5]
    \\    loop i in 0..2 {loop j in 0..2 {r.x+=0.25
    \\        r.y*=2.0}}
    \\    @println(a,r,U.some[r])
    \\    @println("{nan} {inf} {-inf} {minus} {a} {r}")
    \\    let mut sum:f64=0
    \\    loop x in a {if x==x {sum+=0.5}}
    \\    @println(sum,if true {@as(f32,0.1)} else {@as(f32,0.2)},loop {break @as(f64,0.1)})
    \\    let NAN:f32=0.1
    \\    let INFINITY:f64=0.1
    \\    let EOF:f32=2
    \\    @println(NAN,INFINITY,EOF)
    \\}
;

fn emitFloatSourceFree(allocator: std.mem.Allocator) ![]u8 {
    var lowered = blk: {
        const source = try allocator.dupe(u8, float_program);
        defer allocator.free(source);
        var module = try Module.init(allocator, "floats.nov", source);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        if (module.hasErrors()) for (module.diagnostics.items) |diagnostic| {
            std.debug.print("{s} at {d}: {s}\n", .{ @tagName(diagnostic.tag), diagnostic.span.start, module.sourceSlice(diagnostic.span) orelse "<invalid>" });
        };
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(result.lowering_complete);
        break :blk result;
    };
    defer lowered.deinit();
    return emit(allocator, &lowered.ir, null);
}

fn testFloatSourceFreeAllocations(allocator: std.mem.Allocator) !void {
    const c = try emitFloatSourceFree(allocator);
    defer allocator.free(c);
}

test "floats source literals mutation loops membership nesting interpolation macros source free forced GC O0 O3" {
    const library = try emitFloatSourceFree(test_gpa);
    defer test_gpa.free(library);
    const c = try collectingTestProgram(library);
    defer test_gpa.free(c);
    const expected = "nan inf -inf -0 nan inf -inf -0\n" ++
        "false true false false false false true\nfalse true false\n" ++
        "[nan, 1.5, 0.200000003, inf] R[ x: 1.25, y: 8 ] U.some[ x: 1.25, y: 8 ]\n" ++
        "nan inf -inf -0 [nan, 1.5, 0.200000003, inf] R[ x: 1.25, y: 8 ]\n" ++
        "1.5 0.100000001 0.10000000000000001\n0.100000001 0.10000000000000001 2\n";
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty;", 0, expected, "", mode);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testFloatSourceFreeAllocations, .{});
}

test "floats source literal rounding exact bits native type and C extern export signatures O0 O3" {
    const library = try emitSource(
        \\@[extern("provider32")]
        \\let provider32:(x:f32)->f32
        \\@[extern("provider64")]
        \\let provider64:(x:f64)->f64
        \\@[export("round32")]
        \\let round32:()->f32=provider32(16777217)
        \\@[export("round64")]
        \\let round64:()->f64=provider64(9007199254740993)
        \\@[export("tie32")]
        \\let tie32:()->f32=1.000000059604644775390625
        \\@[export("above32")]
        \\let above32:()->f32=1.000000059604644775390626
        \\@[export("tie64")]
        \\let tie64:()->f64=1.00000000000000011102230246251565404236316680908203125
        \\@[export("sub32")]
        \\let sub32:()->f32=1.401298464324817070923729583289916131280e-45
        \\@[export("sub64")]
        \\let sub64:()->f64=4.9406564584124654e-324
        \\@[export("negative32")]
        \\let negative32:()->f32=-0.0
        \\@[export("negative64")]
        \\let negative64:()->f64=-0.0
        \\@[export("contract32")]
        \\let contract32:(x:f32,y:f32)->f32=x*y-1
        \\@[export("contract64")]
        \\let contract64:(x:f64,y:f64)->f64=x*y-1
        \\@[export("native")]
        \\let native:(x:float)->float=x
    , null);
    defer test_gpa.free(library);
    const harness =
        \\#include <assert.h>
        \\#include <stdint.h>
        \\#include <string.h>
        \\float provider32(float x) { return x; }
        \\double provider64(double x) { return x; }
        \\extern float round32(void), tie32(void), above32(void), sub32(void), negative32(void);
        \\extern double round64(void), tie64(void), sub64(void), negative64(void);
        \\extern float contract32(float, float);
        \\extern double contract64(double, double);
        \\static uint32_t bits32(float x) { uint32_t b; memcpy(&b, &x, sizeof b); return b; }
        \\static uint64_t bits64(double x) { uint64_t b; memcpy(&b, &x, sizeof b); return b; }
        \\int main(void) {
        \\    assert(contract32(0x1.000002p0f, 0x1.fffffcp-1f) == 0);
        \\    assert(contract64(0x1.0000000000001p0, 0x1.ffffffffffffep-1) == 0);
        \\    assert(bits32(round32()) == UINT32_C(0x4b800000));
        \\    assert(bits64(round64()) == UINT64_C(0x4340000000000000));
        \\    assert(bits32(tie32()) == UINT32_C(0x3f800000));
        \\    assert(bits32(above32()) == UINT32_C(0x3f800001));
        \\    assert(bits64(tie64()) == UINT64_C(0x3ff0000000000000));
        \\    assert(bits32(sub32()) == 1); assert(bits64(sub64()) == 1);
        \\    assert(bits32(negative32()) == UINT32_C(0x80000000));
        \\    assert(bits64(negative64()) == UINT64_C(0x8000000000000000));
        \\    return 0;
        \\}
    ;
    const native_signature = if (Ir.native_float_bits == 32) "float native(float p0)" else "double native(double p0)";
    try std.testing.expect(std.mem.indexOf(u8, library, native_signature) != null);
    try std.testing.expect(std.mem.indexOf(u8, library, "nov_rt_panic") == null);
    try std.testing.expect(std.mem.indexOf(u8, library, "nov_rt_div_") == null);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(library, harness, 0, "", "", mode);
}

test "floats scalar print demand only and transitive metadata do not create GC roots O0 O3" {
    const c = try emitSource(
        \\let R=struct{x:f32,y:f64,}
        \\let U=union{none,some:R,}
        \\let ignore:(callback:([]U)->f64)={}
        \\let show:(x:f32)=@println(x)
    , null);
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_rt_float_format_32") != null);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_rt_float_format_64") == null);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_rt_float_bits_") == null);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_gc_") == null);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_to_string_") == null);
    try std.testing.expect(std.mem.indexOf(u8, c, "float f0;") != null);
    try std.testing.expect(std.mem.indexOf(u8, c, "double f1;") != null);
    const harness = "void nov_show(float); int main(void) { nov_show(-0.0f); nov_show(0.1f); return 0; }\n";
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, harness, 0, "-0\n0.100000001\n", "", mode);
}

test "floats foreign signatures reject float header macros before C emission" {
    var ir = Ir.init(test_gpa);
    defer ir.deinit();
    const ty = try ir.internType(.{ .float = 32 });
    const signature = try ir.internType(.{ .function = .{ .params = &.{ty}, .result = ty } });
    const function = try ir.addFunction("identity", signature, .{ .start = 0, .end = 0 });
    for ([_][]const u8{ "FLT_RADIX", "FLT_MAX", "DBL_MANT_DIG", "LDBL_MAX", "DECIMAL_DIG", "FLT_DECIMAL_DIG", "DBL_DECIMAL_DIG", "LDBL_DECIMAL_DIG" }) |name| {
        try std.testing.expectError(error.InvalidExternName, ir.setExternName(function, name));
        try std.testing.expectError(error.InvalidExportName, ir.setExportName(function, name));
    }
}

fn testFloatContraction(compiler: []const []const u8) !void {
    var probe_argv: std.ArrayList([]const u8) = .empty;
    defer probe_argv.deinit(test_gpa);
    try probe_argv.appendSlice(test_gpa, compiler);
    try probe_argv.append(test_gpa, "--version");
    const probe = std.process.run(test_gpa, std.testing.io, .{ .argv = probe_argv.items, .timeout = .{ .duration = .{ .raw = .fromSeconds(10), .clock = .awake } } }) catch |err| switch (err) {
        error.FileNotFound => return error.SkipZigTest,
        else => return err,
    };
    defer test_gpa.free(probe.stdout);
    defer test_gpa.free(probe.stderr);
    try expectSuccess(probe, "C compiler discovery");
    const c = try emitSource(
        \\@[export("contract32")]
        \\let contract32:(x:f32,y:f32)->f32=x*y-1
        \\@[export("contract64")]
        \\let contract64:(x:f64,y:f64)->f64=x*y-1
        \\let show:(x:f32,y:f64)=@println(x,y)
    , null);
    defer test_gpa.free(c);
    const harness =
        \\#include <assert.h>
        \\extern float contract32(float, float);
        \\extern double contract64(double, double);
        \\extern void nov_show(float, double);
        \\int main(void) {
        \\    assert(contract32(0x1.000002p0f, 0x1.fffffcp-1f) == 0);
        \\    assert(contract64(0x1.0000000000001p0, 0x1.ffffffffffffep-1) == 0);
        \\    nov_show(-0.0f, 0.1);
        \\    return 0;
        \\}
    ;
    for ([_][]const u8{ "-O0", "-O3" }) |mode| {
        try compileAndRunCompiler(c, harness, 0, "-0 0.10000000000000001\n", "", mode, compiler, &.{});
        if ((builtin.cpu.arch == .x86_64 or builtin.cpu.arch == .x86) and std.Target.x86.featureSetHas(builtin.cpu.features, .fma)) {
            // Enable hardware fusion without overriding source pragmas:
            // emitted library code must still round each operation.
            try compileAndRunCompiler(c, harness, 0, "-0 0.10000000000000001\n", "", mode, compiler, &.{"-mfma"});
        }
    }
}

test "floats GCC strict library contraction and printing O0 O3 with FMA when available" {
    try testFloatContraction(&.{"gcc"});
}

test "floats clang strict library contraction and printing O0 O3 with FMA when available" {
    const zig_exe = std.testing.environ.getAlloc(test_gpa, "NOV_TEST_ZIG") catch null;
    defer if (zig_exe) |path| test_gpa.free(path);
    try testFloatContraction(if (zig_exe) |path| &.{ path, "cc" } else &.{"clang"});
}

test "floats foreign signatures preserve stdio macro collision rejection" {
    var ir = Ir.init(test_gpa);
    defer ir.deinit();
    const ty = try ir.internType(.{ .float = 32 });
    const signature = try ir.internType(.{ .function = .{ .params = &.{ty}, .result = ty } });
    const function = try ir.addFunction("identity", signature, .{ .start = 0, .end = 0 });
    for ([_][]const u8{ "stdin", "stdout", "stderr", "EOF", "FILE", "BUFSIZ", "FOPEN_MAX" }) |name| {
        try std.testing.expectError(error.InvalidExternName, ir.setExternName(function, name));
        try std.testing.expectError(error.InvalidExportName, ir.setExportName(function, name));
    }
}

test "default aggregate formatting direct IR nested GC C11 O0 O3 and invalid tags" {
    var ir = Ir.init(test_gpa);
    defer ir.deinit();
    const span: Ir.Span = .{ .start = 0, .end = 0 };
    const unit = try ir.internType(.void);
    const string = try ir.internType(.string);
    const integer = try ir.internType(.{ .integer = .{ .bits = 64, .signedness = .signed } });
    const rune = try ir.internType(.rune);
    const boolean = try ir.internType(.boolean);
    const enumeration = try ir.internType(.{ .enumeration = .{ .identity = 0, .name = "Kind", .tags = &.{ .{ .name = "first", .span = span }, .{ .name = "second", .span = span } } } });
    const leaf = try ir.internType(.{ .record = .{ .identity = 1, .fields = &.{} } });
    try ir.setRecordFields(leaf, &.{
        .{ .name = "text", .ty = string, .span = span },
        .{ .name = "number", .ty = integer, .span = span },
        .{ .name = "rune", .ty = rune, .span = span },
        .{ .name = "kind", .ty = enumeration, .span = span },
        .{ .name = "yes", .ty = boolean, .span = span },
    });
    const array = try ir.internType(.{ .array = leaf });
    const nested = try ir.internType(.{ .array = array });
    const payload = try ir.internType(.{ .record = .{ .identity = 2, .fields = &.{} } });
    try ir.setRecordFields(payload, &.{
        .{ .name = "items", .ty = nested, .span = span },
        .{ .name = "empty", .ty = array, .span = span },
    });
    const root = try ir.internType(.{ .record = .{ .identity = 3, .tagged = true, .fields = &.{} } });
    try ir.setRecordFields(root, &.{
        .{ .name = "none", .ty = unit, .span = span },
        .{ .name = "some", .ty = payload, .span = span },
    });
    try ir.setTypeFormatting(enumeration, "Kind", null);
    try ir.setTypeFormatting(leaf, "Leaf", null);
    try ir.setTypeFormatting(payload, "Payload", null);
    try ir.setTypeFormatting(root, "Message", null);
    const show_ty = try ir.internType(.{ .function = .{ .params = &.{root}, .result = unit } });
    const convert_ty = try ir.internType(.{ .function = .{ .params = &.{root}, .result = string } });
    const show = try ir.addFunction("show", show_ty, span);
    const convert = try ir.addFunction("convert", convert_ty, span);
    const parameter: Ir.Instruction = .{ .ty = root, .span = span, .op = .{ .parameter = 0 } };
    try ir.setFunctionBody(show, &.{ parameter, .{ .ty = unit, .span = span, .op = .{ .print = .{ .args = &.{@fromBackingInt(0)}, .newline = true } } } }, @fromBackingInt(1));
    const prefix = try ir.internConstant(.{ .ty = string, .value = .{ .string = "<" } });
    const suffix = try ir.internConstant(.{ .ty = string, .value = .{ .string = ">" } });
    // The same IR shape used by interpolation: the structural conversion must
    // stay rooted through both subsequent concatenation allocations.
    try ir.setFunctionBody(convert, &.{
        parameter,
        .{ .ty = string, .span = span, .op = .{ .to_string = @fromBackingInt(0) } },
        .{ .ty = string, .span = span, .op = .{ .constant = prefix } },
        .{ .ty = string, .span = span, .op = .{ .binary = .{ .op = .add, .lhs = @fromBackingInt(2), .rhs = @fromBackingInt(1) } } },
        .{ .ty = string, .span = span, .op = .{ .constant = suffix } },
        .{ .ty = string, .span = span, .op = .{ .binary = .{ .op = .add, .lhs = @fromBackingInt(3), .rhs = @fromBackingInt(4) } } },
    }, @fromBackingInt(5));
    const c = try emit(test_gpa, &ir, null);
    defer test_gpa.free(c);
    const setup = try std.fmt.allocPrint(test_gpa,
        \\int main(void) {{
        \\    nov_rt_heap.collect_every_allocation = 1;
        \\    nov_string text = {{0}}; nov_array leaves = {{0}}, nested = {{0}};
        \\    void **roots[] = {{&text.owner, &leaves.data, &nested.data}};
        \\    nov_gc_roots frame = {{0}}; nov_gc_push(&nov_rt_heap, &frame, roots, 3);
        \\    nov_gc_descriptor bytes = {{4, 1, 0, NULL}};
        \\    text.owner = nov_gc_alloc(&nov_rt_heap, &bytes);
        \\    memcpy(text.owner, "h\xc3\xa9\0", 4); text.data = text.owner; text.len = 4;
        \\    size_t offsets[] = {{offsetof(nov_record_{d}, f0) + offsetof(nov_string, owner)}};
        \\    nov_gc_descriptor element = {{sizeof(nov_record_{d}), _Alignof(nov_record_{d}), 1, offsets}};
        \\    leaves = nov_array_new(1, 1, &element);
        \\    ((nov_record_{d} *)leaves.data)[0] = (nov_record_{d}){{text, -7, 0x1f600, 1, true}};
        \\    size_t nested_offsets[] = {{offsetof(nov_array, data)}};
        \\    nov_gc_descriptor nested_element = {{sizeof(nov_array), _Alignof(nov_array), 1, nested_offsets}};
        \\    nested = nov_array_new(1, 1, &nested_element);
        \\    ((nov_array *)nested.data)[0] = leaves;
        \\    nov_record_{d} value = {{0}};
        \\    value.tag = 1; value.f1.f0 = nested;
        \\
    , .{ @backingInt(leaf), @backingInt(leaf), @backingInt(leaf), @backingInt(leaf), @backingInt(leaf), @backingInt(root) });
    defer test_gpa.free(setup);
    const expected = "Message.some[ items: [[Leaf[ text: hé\x00, number: -7, rune: 😀, kind: Kind.second, yes: true ]]], empty: [] ]\n";
    const success = try std.mem.concat(test_gpa, u8, &.{
        c, setup,
        \\    size_t before_print = nov_rt_heap.live_objects;
        \\    nov_show(value);
        \\    if (nov_rt_heap.live_objects != before_print) return 4;
        \\    nov_string result = nov_convert(value);
        \\    if (fwrite(result.data, 1, (size_t)result.len, stdout) != result.len) return 2;
        \\    putchar('\n');
        \\    value.tag = 0;
        \\    nov_show(value);
        \\    nov_gc_pop(&nov_rt_heap, &frame); nov_gc_collect(&nov_rt_heap);
        \\    if (nov_rt_heap.live_objects) return 3;
        \\    nov_gc_shutdown(&nov_rt_heap); return 0;
        \\}
        \\
    });
    defer test_gpa.free(success);
    const output = try std.mem.concat(test_gpa, u8, &.{ expected, "<", expected[0 .. expected.len - 1], ">\nMessage.none\n" });
    defer test_gpa.free(output);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(success, "typedef int nonempty;", 0, output, "", mode);
    const exhausted = try std.mem.concat(test_gpa, u8, &.{
        "#include <stdlib.h>\nstatic int blocked;\nstatic void *limited_malloc(size_t n) { return blocked ? NULL : malloc(n); }\n#define NOV_GC_MALLOC limited_malloc\n#define NOV_GC_FREE free\n",
        c,
        setup,
        "blocked = 1; nov_show(value); (void)nov_convert(value); return 0; }\n",
    });
    defer test_gpa.free(exhausted);
    try compileAndRunExpected(exhausted, "typedef int nonempty;", 1, expected, "panic: GC allocation failed\n");
    for ([_]bool{ false, true }) |bad_enum| {
        const corrupt = if (bad_enum) try std.fmt.allocPrint(test_gpa, "((nov_record_{d} *)leaves.data)[0].f3 = 99;\n", .{@backingInt(leaf)}) else try test_gpa.dupe(u8, "value.tag = 99;\n");
        defer test_gpa.free(corrupt);
        const bad = try std.mem.concat(test_gpa, u8, &.{ c, setup, corrupt, "(void)nov_convert(value); return 0; }\n" });
        defer test_gpa.free(bad);
        try compileAndRunExpected(bad, "typedef int nonempty;", 1, "", if (bad_enum) "panic: invalid enum tag\n" else "panic: invalid union tag\n");
    }

    const observer_ty = try ir.internType(.{ .function = .{ .params = &.{integer}, .result = integer } });
    const observer = try ir.addFunction("observe", observer_ty, span);
    try ir.setExternName(observer, "observe");
    const hook_ty = try ir.internType(.{ .function = .{ .params = &.{leaf}, .result = string } });
    const hook = try ir.addFunction("leaf_hook", hook_ty, span);
    const hook_prefix = try ir.internConstant(.{ .ty = string, .value = .{ .string = "hook" } });
    try ir.setFunctionBody(hook, &.{
        .{ .ty = leaf, .span = span, .op = .{ .parameter = 0 } },
        .{ .ty = integer, .span = span, .op = .{ .field_read = .{ .base = @fromBackingInt(0), .index = 1 } } },
        .{ .ty = observer_ty, .span = span, .op = .{ .function_ref = observer } },
        .{ .ty = integer, .span = span, .op = .{ .call = .{ .callee = @fromBackingInt(2), .args = &.{@fromBackingInt(1)} } } },
        .{ .ty = string, .span = span, .op = .{ .to_string = @fromBackingInt(3) } },
        .{ .ty = string, .span = span, .op = .{ .constant = hook_prefix } },
        .{ .ty = string, .span = span, .op = .{ .binary = .{ .op = .add, .lhs = @fromBackingInt(5), .rhs = @fromBackingInt(4) } } },
    }, @fromBackingInt(6));
    try ir.setTypeFormatting(leaf, "Leaf", hook);
    const hooked = try emit(test_gpa, &ir, null);
    defer test_gpa.free(hooked);
    const more = try std.fmt.allocPrint(test_gpa,
        \\nov_array more = nov_array_new(2, 2, &element);
        \\((nov_record_{d} *)more.data)[0] = ((nov_record_{d} *)leaves.data)[0];
        \\((nov_record_{d} *)more.data)[1] = ((nov_record_{d} *)leaves.data)[0];
        \\((nov_record_{d} *)more.data)[1].f1 = 8;
        \\leaves = more; ((nov_array *)nested.data)[0] = leaves;
        \\
    , .{ @backingInt(leaf), @backingInt(leaf), @backingInt(leaf), @backingInt(leaf), @backingInt(leaf) });
    defer test_gpa.free(more);
    const callbacks = try std.mem.concat(test_gpa, u8, &.{
        hooked,
        \\static int calls;
        \\int64_t observe(int64_t n) { if (n != (calls % 2 ? 8 : -7)) abort(); return ++calls; }
        \\
        ,
        setup,
        more,
        \\nov_show(value);
        \\nov_string result = nov_convert(value);
        \\if (calls != 4) return 5;
        \\if (fwrite(result.data, 1, (size_t)result.len, stdout) != result.len) return 6;
        \\putchar('\n');
        \\nov_gc_pop(&nov_rt_heap, &frame); nov_gc_collect(&nov_rt_heap);
        \\if (nov_rt_heap.live_objects) return 7;
        \\nov_gc_shutdown(&nov_rt_heap); return 0; }
        \\
    });
    defer test_gpa.free(callbacks);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(callbacks, "typedef int nonempty;", 0, "Message.some[ items: [[hook1, hook2]], empty: [] ]\n<Message.some[ items: [[hook3, hook4]], empty: [] ]>\n", "", mode);

    const payload_hook_ty = try ir.internType(.{ .function = .{ .params = &.{payload}, .result = string } });
    const payload_hook = try ir.addFunction("payload_hook", payload_hook_ty, span);
    const payload_body = try test_gpa.dupe(Ir.Instruction, ir.getFunction(hook).?.body.?.instructions);
    defer test_gpa.free(payload_body);
    payload_body[0].ty = payload;
    payload_body[1].op = .{ .constant = try ir.internConstant(.{ .ty = integer, .value = .{ .signed_integer = -7 } }) };
    payload_body[5].op = .{ .constant = try ir.internConstant(.{ .ty = string, .value = .{ .string = "payload" } }) };
    try ir.setFunctionBody(payload_hook, payload_body, @fromBackingInt(6));
    try ir.setTypeFormatting(payload, "Payload", payload_hook);
    const overridden = try emit(test_gpa, &ir, null);
    defer test_gpa.free(overridden);
    const payload_callbacks = try std.mem.concat(test_gpa, u8, &.{
        overridden,
        "static int calls;\nint64_t observe(int64_t n) { if (n != -7) abort(); return ++calls; }\n",
        setup,
        \\nov_show(value);
        \\nov_string result = nov_convert(value);
        \\if (calls != 2) return 8;
        \\if (fwrite(result.data, 1, (size_t)result.len, stdout) != result.len) return 9;
        \\putchar('\n');
        \\nov_gc_pop(&nov_rt_heap, &frame); nov_gc_collect(&nov_rt_heap);
        \\if (nov_rt_heap.live_objects) return 10;
        \\nov_gc_shutdown(&nov_rt_heap); return 0; }
        \\
    });
    defer test_gpa.free(payload_callbacks);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(payload_callbacks, "typedef int nonempty;", 0, "Message.some[ payload1 ]\n<Message.some[ payload2 ]>\n", "", mode);
}

test "default aggregate printing shared type DAG empty arrays validates without expanding values" {
    var source = std.Io.Writer.Allocating.init(test_gpa);
    defer source.deinit();
    try source.writer.writeAll("let R0=struct{value:int,}\n");
    for (1..48) |i| try source.writer.print("let R{d}=struct{{a:R{d},b:R{d},}}\n", .{ i, i - 1, i - 1 });
    try source.writer.writeAll("let main:()={let empty:[]R47=[]\n@println(empty)\n@println(\"{empty}\")}\n");
    // Exercise frontend registration, body publication, verification, and helper
    // discovery without materializing or compiling the exponentially large C layout.
    const c = try emitSource(source.written(), null);
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_format_") != null);
    try std.testing.expect(c.len < 100_000);
}

test "default aggregate formatting allocating hooks share mutable closure state source free GC" {
    const source =
        \\let Counter=struct{callbacks:[]()->int,}
        \\let hook:(value:Counter)->string="C{value.callbacks[0]()}"
        \\let show:(values:[]Counter)=@println(values.len)
        \\let convert:(values:[]Counter)->string="{values.len}"
        \\let main:()={
        \\ let mut count:int=0
        \\ let tick=()->int{count+=1
        \\ count}
        \\ let counter=Counter[.callbacks=[tick]]
        \\ let values=[counter,counter]
        \\ show(values)
        \\ @println(convert(values))
        \\ @println(count)
        \\}
    ;
    var lowered = blk: {
        var module = try Module.init(test_gpa, "default-hooks.nov", source);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        if (module.hasErrors()) {
            var out = std.Io.Writer.Allocating.init(test_gpa);
            defer out.deinit();
            try Diagnostics.renderModule(&module, &out.writer);
            std.debug.print("{s}", .{out.written()});
        }
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(result.lowering_complete);
        break :blk result;
    };
    defer lowered.deinit();
    const ir = &lowered.ir;
    var hook: ?Ir.FunctionId = null;
    for (ir.functions.items, 0..) |function, i| {
        if (std.mem.eql(u8, function.name, "hook")) hook = @fromBackingInt(@intCast(i));
    }
    const nominal = ir.getType(ir.getFunction(hook.?).?.ty).?.function.params[0];
    try ir.setTypeFormatting(nominal, "Counter", hook);
    // Keep frontend registration out of this backend test. Publish aggregate
    // print/interpolation operands only after installing the hook contract.
    for (ir.functions.items) |function| {
        const show = std.mem.eql(u8, function.name, "show");
        const convert = std.mem.eql(u8, function.name, "convert");
        if (!show and !convert) continue;
        const body = function.body.?;
        var parameter: ?Ir.ValueId = null;
        for (body.instructions, 0..) |instruction, i| {
            if (instruction.op == .parameter and instruction.op.parameter == 0) parameter = @fromBackingInt(@intCast(i));
        }
        var changed = false;
        for (@constCast(body.instructions)) |*instruction| {
            if (show and instruction.op == .print) {
                @constCast(instruction.op.print.args)[0] = parameter.?;
                changed = true;
            }
            if (convert and instruction.op == .to_string) {
                instruction.op.to_string = parameter.?;
                changed = true;
            }
        }
        try std.testing.expect(changed);
    }
    const library = try emit(test_gpa, ir, null);
    defer test_gpa.free(library);
    const c = try collectingTestProgram(library);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty;", 0, "[C1, C2]\n[C3, C4]\n4\n", "", mode);
}

test "default aggregate formatting named empty scalar void enum and hook only demand" {
    var ir = Ir.init(test_gpa);
    defer ir.deinit();
    const span: Ir.Span = .{ .start = 0, .end = 0 };
    const unit = try ir.internType(.void);
    const integer = try ir.internType(.{ .integer = .{ .bits = 64, .signedness = .signed } });
    const empty = try ir.internType(.{ .record = .{ .identity = 0, .fields = &.{} } });
    const message = try ir.internType(.{ .record = .{ .identity = 1, .tagged = true, .fields = &.{} } });
    try ir.setRecordFields(message, &.{
        .{ .name = "none", .ty = unit, .span = span },
        .{ .name = "number", .ty = integer, .span = span },
        .{ .name = "empty", .ty = empty, .span = span },
    });
    const color = try ir.internType(.{ .enumeration = .{ .identity = 2, .name = "Color", .tags = &.{.{ .name = "red", .span = span }} } });
    try ir.setTypeFormatting(empty, "Empty", null);
    try ir.setTypeFormatting(message, "Message", null);
    try ir.setTypeFormatting(color, "Color", null);
    const show_ty = try ir.internType(.{ .function = .{ .params = &.{ empty, message, color }, .result = unit } });
    const show = try ir.addFunction("show", show_ty, span);
    try ir.setFunctionBody(show, &.{
        .{ .ty = empty, .span = span, .op = .{ .parameter = 0 } },
        .{ .ty = message, .span = span, .op = .{ .parameter = 1 } },
        .{ .ty = color, .span = span, .op = .{ .parameter = 2 } },
        .{ .ty = unit, .span = span, .op = .{ .print = .{ .args = &.{ @fromBackingInt(0), @fromBackingInt(1), @fromBackingInt(2) }, .newline = true } } },
    }, @fromBackingInt(3));
    const c = try emit(test_gpa, &ir, null);
    defer test_gpa.free(c);
    const driver = try std.fmt.allocPrint(test_gpa, "int main(void) {{ nov_record_{d} empty = {{0}}; nov_record_{d} message = {{0}};\n" ++
        "message.tag = 1; message.f1 = 7; nov_show(empty, message, 0);\n" ++
        "message.tag = 0; nov_show(empty, message, 0);\n" ++
        "message.tag = 2; nov_show(empty, message, 0); nov_gc_shutdown(&nov_rt_heap); return 0; }}\n", .{ @backingInt(empty), @backingInt(message) });
    defer test_gpa.free(driver);
    const executable = try std.mem.concat(test_gpa, u8, &.{ c, driver });
    defer test_gpa.free(executable);
    try compileAndRunExpected(executable, "typedef int nonempty;", 0, "Empty[] Message.number[7] Color.red\nEmpty[] Message.none Color.red\nEmpty[] Message.empty[] Color.red\n", "");

    // No structural helper is demanded beneath this overriding root hook.
    ir.clearFunctions();
    const string = try ir.internType(.string);
    const signature = try ir.internType(.{ .function = .{ .params = &.{empty}, .result = string } });
    const hook = try ir.addFunction("hook", signature, span);
    const text = try ir.internConstant(.{ .ty = string, .value = .{ .string = "custom" } });
    try ir.setFunctionBody(hook, &.{.{ .ty = string, .span = span, .op = .{ .constant = text } }}, @fromBackingInt(0));
    try ir.setTypeFormatting(empty, "Empty", hook);
    const convert = try ir.addFunction("convert", signature, span);
    try ir.setFunctionBody(convert, &.{
        .{ .ty = empty, .span = span, .op = .{ .parameter = 0 } },
        .{ .ty = string, .span = span, .op = .{ .to_string = @fromBackingInt(0) } },
    }, @fromBackingInt(1));
    const hooked = try emit(test_gpa, &ir, null);
    defer test_gpa.free(hooked);
    const hook_driver = try std.fmt.allocPrint(test_gpa, "int main(void) {{ nov_rt_heap.collect_every_allocation = 1; nov_string text = nov_convert((nov_record_{d}){{0}}); if (text.len != 6 || memcmp(text.data, \"custom\", 6)) return 1; nov_gc_shutdown(&nov_rt_heap); return 0; }}\n", .{@backingInt(empty)});
    defer test_gpa.free(hook_driver);
    const hook_executable = try std.mem.concat(test_gpa, u8, &.{ hooked, hook_driver });
    defer test_gpa.free(hook_executable);
    try compileAndRun(hook_executable, "typedef int nonempty;");
}

fn formatHook(ir: *const Ir, ty: Ir.TypeId) ?Ir.FunctionId {
    return if (ir.getTypeFormatting(ty)) |entry| entry.formatter else null;
}

fn formatName(ir: *const Ir, ty: Ir.TypeId) []const u8 {
    if (ir.getTypeFormatting(ty)) |entry| return entry.display_name;
    const value = ir.getType(ty).?;
    return if (value == .enumeration) value.enumeration.name else "anonymous";
}

fn aggregateFormattingType(ty: Ir.Type) bool {
    return ty == .array or ty == .record or ty == .enumeration;
}

fn markFormatType(ir: *const Ir, used: []bool, id: Ir.TypeId) void {
    const i = @backingInt(id);
    if (used[i]) return;
    used[i] = true;
    if (formatHook(ir, id) != null) return;
    switch (ir.getType(id).?) {
        .array => |element| markFormatType(ir, used, element),
        .record => |record| for (record.fields) |field| {
            if (ir.getType(field.ty).? != .void) markFormatType(ir, used, field.ty);
        },
        else => {},
    }
}

// Encode names as C byte escapes, including non-ASCII identifiers and NUL.
fn writeFormatLiteral(w: *std.Io.Writer, bytes: []const u8) WriteError!void {
    try w.writeAll("    nov_format_bytes(sink, (const unsigned char *)\"");
    for (bytes) |byte| try w.print("\\{o:0>3}", .{byte});
    try w.print("\", {d});\n", .{bytes.len});
}

fn markFormattingTypes(ir: *const Ir, used: []bool, converted: []bool) bool {
    @memset(used, false);
    @memset(converted, false);
    var demanded = false;
    for (ir.functions.items) |function| {
        const body = function.body orelse continue;
        for (body.instructions) |instruction| switch (instruction.op) {
            .print => |p| for (p.args) |arg| {
                const id = body.instructions[@backingInt(arg)].ty;
                if (aggregateFormattingType(ir.getType(id).?)) {
                    markFormatType(ir, used, id);
                    demanded = true;
                }
            },
            .to_string => |value| {
                const id = body.instructions[@backingInt(value)].ty;
                if (aggregateFormattingType(ir.getType(id).?)) {
                    markFormatType(ir, used, id);
                    converted[@backingInt(id)] = true;
                    demanded = true;
                }
            },
            else => {},
        };
    }
    return demanded;
}

fn writeAggregateFormatting(allocator: std.mem.Allocator, ir: *const Ir, counts: []const usize, w: *std.Io.Writer) (WriteError || std.mem.Allocator.Error)!void {
    const used = try allocator.alloc(bool, ir.types.items.len);
    defer allocator.free(used);
    const converted = try allocator.alloc(bool, ir.types.items.len);
    defer allocator.free(converted);
    if (!markFormattingTypes(ir, used, converted)) return;
    const conversion = std.mem.indexOfScalar(bool, converted, true) != null;
    try w.writeAll("#include <stdio.h>\n#include <inttypes.h>\n");
    try w.writeAll(if (conversion) "typedef struct { bool streaming; nov_string output; } nov_format_sink;\n" else "typedef struct { bool streaming; } nov_format_sink;\n");
    if (conversion) try w.writeAll(
        \\static void nov_format_string(nov_format_sink *sink, nov_string chunk) {
        \\    if (sink->streaming) {
        \\        if (chunk.len && fwrite(chunk.data, 1, (size_t)chunk.len, stdout) != (size_t)chunk.len) nov_rt_panic("stdout write failed");
        \\    } else {
        \\        void **roots[] = {&chunk.owner};
        \\        nov_gc_roots frame = {0};
        \\        nov_gc_push(&nov_rt_heap, &frame, roots, 1);
        \\        sink->output = nov_string_concat(sink->output, chunk);
        \\        nov_gc_pop(&nov_rt_heap, &frame);
        \\    }
        \\}
        \\
    );
    var bytes_needed = !conversion;
    for (ir.types.items, used, 0..) |ty, needed, t| {
        if (needed and ty != .string and formatHook(ir, @fromBackingInt(@intCast(t))) == null) bytes_needed = true;
    }
    if (bytes_needed) {
        try w.writeAll("static void nov_format_bytes(nov_format_sink *sink, const unsigned char *data, uintptr_t len) {\n");
        if (conversion) {
            try w.writeAll("    nov_format_string(sink, (nov_string){NULL, data, len});\n");
        } else {
            try w.writeAll("    (void)sink;\n    if (len && fwrite(data, 1, (size_t)len, stdout) != (size_t)len) nov_rt_panic(\"stdout write failed\");\n");
        }
        try w.writeAll("}\n");
    }
    for (used, 0..) |needed, t| {
        if (!needed) continue;
        try w.print("static void nov_format_{d}(nov_format_sink *, ", .{t});
        try writeType(ir, w, @fromBackingInt(@intCast(t)));
        try w.writeAll(", bool);\n");
    }
    for (ir.types.items, used, 0..) |ty, needed, t| {
        if (!needed) continue;
        try w.print("static void nov_format_{d}(nov_format_sink *sink, ", .{t});
        try writeType(ir, w, @fromBackingInt(@intCast(t)));
        try w.writeAll(" value, bool named) {\n    (void)value; (void)named;\n");
        const id: Ir.TypeId = @fromBackingInt(@intCast(t));
        if (formatHook(ir, id)) |hook| {
            // Validate tags before calling user code, including overriding hooks.
            if (ty == .enumeration) try w.print("    if (value >= {d}) nov_rt_panic(\"invalid enum tag\");\n", .{ty.enumeration.tags.len});
            if (ty == .record and ty.record.tagged) try w.print("    if (value.tag >= {d}) nov_rt_panic(\"invalid union tag\");\n", .{ty.record.fields.len});
            const snapshot = try ir.needsArrayCopy(id);
            if (snapshot) {
                // Match frontend call value semantics, retaining the detached
                // receiver while the hook and its callees allocate.
                try w.writeAll("    ");
                try writeType(ir, w, id);
                try w.writeAll(" v0 = value;\n    void **roots[] = {");
                try writeRoots(ir, counts, w, id, "v", 0);
                try w.print(" }};\n    nov_gc_roots frame = {{0}};\n    nov_gc_push(&nov_rt_heap, &frame, roots, {d});\n    v0 = nov_copy_{d}(v0);\n", .{ counts[t], t });
            }
            try w.writeAll("    nov_string chunk = ");
            try writeScopedFunctionName(w, ir.getFunction(hook).?);
            try w.writeAll(if (snapshot) "(v0);\n" else "(value);\n");
            try w.writeAll(if (conversion) "    nov_format_string(sink, chunk);\n" else "    nov_format_bytes(sink, chunk.data, chunk.len);\n");
            if (snapshot) try w.writeAll("    nov_gc_pop(&nov_rt_heap, &frame);\n");
        } else switch (ty) {
            .string => try w.writeAll(if (conversion) "    nov_format_string(sink, value);\n" else "    nov_format_bytes(sink, value.data, value.len);\n"),
            .boolean => try w.writeAll("    nov_format_bytes(sink, (const unsigned char *)(value ? \"true\" : \"false\"), value ? 4 : 5);\n"),
            .integer => |integer| {
                try w.print("    char bytes[32];\n    int len = snprintf(bytes, sizeof bytes, \"%\" PRI{s}MAX, ({s}intmax_t)value);\n", .{ if (integer.signedness == .signed) "d" else "u", if (integer.signedness == .signed) "" else "u" });
                try w.writeAll("    if (len < 0 || (size_t)len >= sizeof bytes) nov_rt_panic(\"integer formatting failed\");\n    nov_format_bytes(sink, (const unsigned char *)bytes, (uintptr_t)len);\n");
            },
            .float => |bits| {
                try w.print("    char bytes[32];\n    int len = nov_rt_float_format_{d}(bytes, value);\n", .{bits});
                try w.writeAll("    nov_format_bytes(sink, (const unsigned char *)bytes, (uintptr_t)len);\n");
            },
            .rune => try w.writeAll(
                \\    unsigned char bytes[4];
                \\    uintptr_t len;
                \\    if (value <= 0x7f) { bytes[0] = (unsigned char)value; len = 1; }
                \\    else if (value <= 0x7ff) { bytes[0] = (unsigned char)(0xc0u | (value >> 6)); bytes[1] = (unsigned char)(0x80u | (value & 63u)); len = 2; }
                \\    else if (value <= 0xffff) { bytes[0] = (unsigned char)(0xe0u | (value >> 12)); bytes[1] = (unsigned char)(0x80u | ((value >> 6) & 63u)); bytes[2] = (unsigned char)(0x80u | (value & 63u)); len = 3; }
                \\    else { bytes[0] = (unsigned char)(0xf0u | (value >> 18)); bytes[1] = (unsigned char)(0x80u | ((value >> 12) & 63u)); bytes[2] = (unsigned char)(0x80u | ((value >> 6) & 63u)); bytes[3] = (unsigned char)(0x80u | (value & 63u)); len = 4; }
                \\    nov_format_bytes(sink, bytes, len);
                \\
            ),
            .array => |element| {
                try writeFormatLiteral(w, "[");
                try w.writeAll("    for (uintptr_t i = 0; i < value.len; ++i) {\n    if (i) {\n");
                try writeFormatLiteral(w, ", ");
                try w.print("    }}\n    nov_format_{d}(sink, ((const ", .{@backingInt(element)});
                try writeType(ir, w, element);
                try w.writeAll(" *)value.data)[i], true);\n    }\n");
                try writeFormatLiteral(w, "]");
            },
            .enumeration => |enumeration| {
                try w.writeAll("    switch (value) {\n");
                for (enumeration.tags, 0..) |tag, i| {
                    try w.print("    case {d}:\n", .{i});
                    try writeFormatLiteral(w, formatName(ir, id));
                    try writeFormatLiteral(w, ".");
                    try writeFormatLiteral(w, tag.name);
                    try w.writeAll("    break;\n");
                }
                try w.writeAll("    default: nov_rt_panic(\"invalid enum tag\");\n    }\n");
            },
            .record => |record| {
                if (record.tagged) {
                    try w.writeAll("    switch (value.tag) {\n");
                    for (record.fields, 0..) |field, f| {
                        try w.print("    case {d}:\n", .{f});
                        try writeFormatLiteral(w, formatName(ir, id));
                        try writeFormatLiteral(w, ".");
                        try writeFormatLiteral(w, field.name);
                        const payload = ir.getType(field.ty).?;
                        if (payload != .void) {
                            const flatten = payload == .record and !payload.record.tagged and formatHook(ir, field.ty) == null;
                            const hooked_record = payload == .record and !payload.record.tagged and !flatten;
                            if (!flatten) try writeFormatLiteral(w, if (hooked_record) "[ " else "[");
                            try w.print("    nov_format_{d}(sink, value.f{d}, {s});\n", .{ @backingInt(field.ty), f, if (flatten) "false" else "true" });
                            if (!flatten) try writeFormatLiteral(w, if (hooked_record) " ]" else "]");
                        }
                        try w.writeAll("    break;\n");
                    }
                    try w.writeAll("    default: nov_rt_panic(\"invalid union tag\");\n    }\n");
                } else {
                    try w.writeAll("    if (named) {\n");
                    try writeFormatLiteral(w, formatName(ir, id));
                    try w.writeAll("    }\n");
                    try writeFormatLiteral(w, if (record.fields.len == 0) "[" else "[ ");
                    for (record.fields, 0..) |field, f| {
                        if (f != 0) try writeFormatLiteral(w, ", ");
                        try writeFormatLiteral(w, field.name);
                        try writeFormatLiteral(w, ": ");
                        try w.print("    nov_format_{d}(sink, value.f{d}, true);\n", .{ @backingInt(field.ty), f });
                    }
                    try writeFormatLiteral(w, if (record.fields.len == 0) "]" else " ]");
                }
            },
            else => unreachable,
        }
        try w.writeAll("}\n");
        if (converted[t]) {
            try w.print("static nov_string nov_to_string_{d}(", .{t});
            try writeType(ir, w, @fromBackingInt(@intCast(t)));
            try w.print(" value) {{\n    nov_format_sink sink = {{0}};\n    void **roots[] = {{&sink.output.owner}};\n    nov_gc_roots frame = {{0}};\n    nov_gc_push(&nov_rt_heap, &frame, roots, 1);\n    nov_format_{d}(&sink, value, true);\n", .{t});
            try w.writeAll("    nov_string result = sink.output;\n    nov_gc_pop(&nov_rt_heap, &frame);\n    return result;\n}\n");
        }
    }
}

fn writePrint(ir: *const Ir, w: *std.Io.Writer, body: Ir.Body, print: anytype) WriteError!void {
    // Single-string print uses fwrite directly, and library emission has no
    // final-flush wrapper to reference the shared scalar/flush check helper.
    try w.writeAll("    (void)nov_rt_check;\n    nov_rt_init_stdout();\n");
    if (print.args.len == 0 and !print.newline) try w.writeAll("    nov_rt_check(fputs(\"\", stdout));\n");
    for (print.args, 0..) |arg, i| {
        if (i != 0) try w.writeAll("    nov_rt_check(fputc(' ', stdout));\n");
        const ty = ir.types.items[@backingInt(body.instructions[@backingInt(arg)].ty)];
        switch (ty) {
            .array, .record, .enumeration => try w.print("    {{ nov_format_sink sink = {{0}}; sink.streaming = true; nov_format_{d}(&sink, v{d}, true); }}\n", .{ @backingInt(body.instructions[@backingInt(arg)].ty), @backingInt(arg) }),
            .float => |bits| try w.print("    {{ char bytes[32]; int len = nov_rt_float_format_{d}(bytes, v{d}); if (fwrite(bytes, 1, (size_t)len, stdout) != (size_t)len) nov_rt_panic(\"stdout write failed\"); }}\n", .{ bits, @backingInt(arg) }),
            .rune => try w.print("    nov_rt_print_rune(v{d});\n", .{@backingInt(arg)}),
            .string => try w.print("    if (v{d}.len && fwrite(v{d}.data, 1, (size_t)v{d}.len, stdout) != (size_t)v{d}.len) nov_rt_panic(\"stdout write failed\");\n", .{ @backingInt(arg), @backingInt(arg), @backingInt(arg), @backingInt(arg) }),
            .boolean => try w.print("    nov_rt_check(fputs(v{d} ? \"true\" : \"false\", stdout));\n", .{@backingInt(arg)}),
            .integer => |integer| try w.print("    nov_rt_check(fprintf(stdout, \"%\" PRI{s}MAX, ({s}intmax_t)v{d}));\n", .{ if (integer.signedness == .signed) "d" else "u", if (integer.signedness == .signed) "" else "u", @backingInt(arg) }),
            else => unreachable,
        }
    }
    if (print.newline) try w.writeAll("    nov_rt_check(fputc('\\n', stdout));\n");
}

fn writeCall(ir: *const Ir, w: *std.Io.Writer, call: anytype) WriteError!void {
    if (usesClosures(ir)) try w.print("v{d}.code(v{d}.env", .{ @backingInt(call.callee), @backingInt(call.callee) }) else try w.print("v{d}(", .{@backingInt(call.callee)});
    for (call.args, 0..) |arg, i| {
        if (i != 0 or usesClosures(ir)) try w.writeAll(", ");
        try w.print("v{d}", .{@backingInt(arg)});
    }
    try w.writeByte(')');
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const rune_program =
    \\let Rune = rune
    \\let seed: Rune = 'é'
    \\let R = struct { value: Rune, }
    \\let Option: (T:type)->type = union { none, some:T, }
    \\let Result: (T:type,E:type)->type = union { ok:T, err:E, }
    \\let identity: (r: Rune)->Rune = r
    \\let effect: (s:string,r:Rune)->Rune = {@print(s)
    \\r}
    \\let consume: (a:rune,b:rune)->string = "{a}/{b}"
    \\let main: () = {
    \\@println('A',seed,'😀','\u{10ffff}','\0','\n','\t','\'','\\')
    \\let call=identity
    \\@println(call(seed), seed=='é', seed!='😀', "hello"[0])
    \\let mut r=R[.value=seed]
    \\let saved=r
    \\r.value='😀'
    \\let opt:Option(rune)=.some[r.value]
    \\let res:Result(rune,string)=.ok[saved.value]
    \\@println(match opt {
    \\.some=>|v| v
    \\.none=>'x'
    \\},match res {
    \\.ok=>|v| v
    \\.err=>'x'
    \\})
    \\let mut a:[]Rune=['A',seed,'😀']
    \\let mut copy=a
    \\copy[0]='Z'
    \\a+=['\u{10ffff}']
    \\@println(a[0],copy[0],a[1],a[2],a[-1],'é' in a,'x' in a)
    \\let whole=&a
    \\whole[0]='B'
    \\let snapshot=whole
    \\whole+=['C']
    \\@println(snapshot.len,whole.len,whole[0],whole[-1])
    \\let element=&a[1]
    \\let old=element
    \\element='Q'
    \\@println(old,element)
    \\@println(consume(effect("L",'é'),effect("R",'😀')))
    \\let marker="I"
    \\let face='😀'
    \\let last='\u{10ffff}'
    \\let text="prefix:{effect(marker,face)}:"+"{seed}:\0:{last}"
    \\@println(text)
    \\}
;

const rune_output = "A é 😀 \xf4\x8f\xbf\xbf \x00 \n \t ' \\\né true true h\n😀 é\nA Z é 😀 \xf4\x8f\xbf\xbf true false\n4 5 B C\né Q\nLRé/😀\nIprefix:😀:é:\x00:\xf4\x8f\xbf\xbf\n";

test "rune UTF8 storage aliases calls arrays borrows interpolation source free GC C11 O0 O3" {
    {
        var module = try Module.init(test_gpa, "rune.nov", rune_program);
        defer module.deinit();
        if (module.hasErrors()) {
            var out = std.Io.Writer.Allocating.init(test_gpa);
            defer out.deinit();
            try Diagnostics.renderModule(&module, &out.writer);
            std.debug.print("{s}", .{out.written()});
        }
        try std.testing.expect(!module.hasErrors());
    }
    const c = try arrayTestProgram(rune_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, rune_output, "", mode);
}

fn testRuneSourceFree(allocator: std.mem.Allocator) !void {
    var result = blk: {
        const source = try allocator.dupe(u8, rune_program);
        defer allocator.free(source);
        var module = try Module.init(allocator, "rune.nov", source);
        defer module.deinit();
        var lowered = try Lowering.lower(&module);
        errdefer lowered.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(lowered.lowering_complete);
        break :blk lowered;
    };
    defer result.deinit();
    try result.ir.verify();
    const c = try emitProgram(allocator, &result.ir, null);
    defer allocator.free(c);
}

test "rune compiler ownership survives source release and every allocation failure" {
    try testRuneSourceFree(test_gpa);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testRuneSourceFree, .{});
}

test "rune UTF8 encoder boundaries agree for direct output and allocated conversion" {
    var source = std.Io.Writer.Allocating.init(test_gpa);
    defer source.deinit();
    var expected = std.Io.Writer.Allocating.init(test_gpa);
    defer expected.deinit();
    try source.writer.writeAll("let main:()={\n");
    for ([_]u21{ 0, 0x7f, 0x80, 0x7ff, 0x800, 0xd7ff, 0xe000, 0xffff, 0x10000, 0x10ffff }) |value| {
        try source.writer.print("let r{x}='\\u{{{x}}}'\n@print(r{x})\n@print(\"{{r{x}}}\")\n", .{ value, value, value, value });
        var bytes: [4]u8 = undefined;
        const len = try std.unicode.utf8Encode(value, &bytes);
        try expected.writer.writeAll(bytes[0..len]);
        try expected.writer.writeAll(bytes[0..len]);
    }
    try source.writer.writeAll("}\n");
    const c = try arrayTestProgram(source.written());
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, expected.written(), "", mode);
}

test "rune direct printing is allocation free and helpers are demand only" {
    const c = try emitSource("let main:()=@print('A','é','€','😀','\\u{10ffff}','\\0')", null);
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_gc_") == null);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_string") == null);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_rt_encode_rune") != null);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "void nov_main(void); int main(void) {nov_main(); return 0;}", 0, "A é € 😀 \xf4\x8f\xbf\xbf \x00", "", mode);
    const scalar = try emitSource("let f:(r:rune)->rune=r", null);
    defer test_gpa.free(scalar);
    try std.testing.expect(std.mem.indexOf(u8, scalar, "nov_rt_") == null);
    try std.testing.expect(std.mem.indexOf(u8, scalar, "nov_string") == null);
    try compileAndRun(scalar, "int main(void) {return 0;}");
    const conversion = try emitSource("let f:(r:rune)->string=\"{r}\"", null);
    defer test_gpa.free(conversion);
    try std.testing.expect(std.mem.indexOf(u8, conversion, "nov_string_concat") != null);
    try std.testing.expect(std.mem.indexOf(u8, conversion, "nov_rt_print_rune") == null);
    try compileAndRun(conversion, "int main(void) {return 0;}");
}

test "rune interpolation allocation failure panics but direct print does not allocate" {
    const library = try emitSource("let main:()={@print('é')\nlet r='😀'\n@println(\"{r}\")}", null);
    defer test_gpa.free(library);
    const c = try std.mem.concat(test_gpa, u8, &.{ "#include <stdlib.h>\nstatic void *test_malloc(size_t n) {(void)n; return NULL;}\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", library, "\nint main(void) {nov_main(); return 0;}\n" });
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 1, "é", "panic: GC allocation failed\n");
}

test "rune stdout write and final flush failures use the existing panic contract" {
    var module = try Module.init(test_gpa, "rune.nov", "let main:()=@print('é')");
    defer module.deinit();
    var lowered = try Lowering.lower(&module);
    defer lowered.deinit();
    const c = try emitProgram(test_gpa, &lowered.ir, null);
    defer test_gpa.free(c);
    for ([_]struct { prefix: []const u8, output: []const u8 }{
        .{ .prefix = "#include <stdio.h>\nstatic size_t fail_write(const void *p,size_t s,size_t n,FILE *f) {if(f==stdout)return 0; return fwrite(p,s,n,f);}\n#define fwrite fail_write\n", .output = "" },
        .{ .prefix = "#include <stdio.h>\nstatic int fail_flush(FILE *f) {if(f==stdout)return EOF; return fflush(f);}\n#define fflush fail_flush\n", .output = "é" },
    }) |case| {
        const injected = try std.mem.concat(test_gpa, u8, &.{ case.prefix, c });
        defer test_gpa.free(injected);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(injected, "typedef int nonempty_translation_unit;", 1, case.output, "panic: stdout write failed\n", mode);
    }
}

const test_gpa = std.testing.allocator;

const type_reachability_cases = [_]struct { source: []const u8, output: []const u8 = "" }{
    .{ .source =
    \\let Row=struct{values:[][]string,}
    \\let snapshot:(row:Row)->Row=row
    \\let main:()={}
    },
    .{ .source =
    \\let Outer=struct{rows:[][][]Row,}
    \\let Row=struct{leaf:Leaf,}
    \\let Leaf=struct{value:string,}
    \\let snapshot:(row:Outer)->Outer=row
    \\let main:()={}
    },
    .{ .source =
    \\let Outer=struct{callbacks:[][]([][][]Leaf)->[][][]string,}
    \\let Leaf=struct{value:string,}
    \\let snapshot:(row:Outer)->Outer=row
    \\let main:()={}
    },
    .{ .source =
    \\let Outer=struct{rows:[][]Box(Box(string)),}
    \\let Box:(T:type)->type=struct{values:[]T,}
    \\let snapshot:(row:Outer)->Outer=row
    \\let main:()={}
    },
    .{ .source =
    \\let Outer=struct{callbacks:[][]([][][]Leaf)->[][][]string,}
    \\let Leaf=struct{value:string,}
    \\let snapshot:(row:Outer)->Outer=row
    \\let make:(flag:bool)->()->bool=()->bool {flag}
    \\let main:()={}
    },
    .{ .source =
    \\let Outer=struct{rows:[][]Box(Box(string)),}
    \\let Box:(T:type)->type=struct{values:[]T,}
    \\let snapshot:(row:Outer)->Outer=row
    \\let make:(row:Outer)->()->Outer=()->Outer {snapshot(row)}
    \\let main:()={let mut row=Outer[.rows=[[Box(Box(string))[.values=[Box(string)[.values=["reachable"+"!"]]]]]]]
    \\ let captured=make(row)
    \\ row.rows=[]
    \\ let copy=captured()
    \\ @println(copy.rows[0][0].values[0].values[0])}
    , .output = "reachable!\n" },
};

fn emitSourceFreeTypeReachability(allocator: std.mem.Allocator, source: []const u8) ![]u8 {
    var lowered = blk: {
        const with_unused = try std.mem.concat(allocator, u8, &.{ source, "\nlet Unused=struct{wide:[][][]i128,}\n" });
        defer allocator.free(with_unused);
        var module = try Module.init(allocator, "type-reachability.nov", with_unused);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(result.lowering_complete);
        break :blk result;
    };
    defer lowered.deinit();
    const c = try emit(allocator, &lowered.ir, null);
    errdefer allocator.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "} nov_string;") != null);
    try std.testing.expect(std.mem.indexOf(u8, c, "int128_t") == null);
    return c;
}

fn testTypeReachabilityAllocationFailures(allocator: std.mem.Allocator, source: []const u8) !void {
    allocator.free(try emitSourceFreeTypeReachability(allocator, source));
}

test "type reachability completes nested nominal signature generic and closure edges source free C11 O0 O3 forced GC" {
    for (type_reachability_cases) |case| {
        const library = try emitSourceFreeTypeReachability(test_gpa, case.source);
        defer test_gpa.free(library);
        const c = try collectingTestProgram(library);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, case.output, "", mode);
    }
}

test "type reachability transitive metadata survives every compiler allocation failure" {
    for (type_reachability_cases) |case|
        try TestAllocations.checkAllAllocationFailures(test_gpa, testTypeReachabilityAllocationFailures, .{case.source});
}

test "type reachability nested declarations compile as C11 for Windows and aarch64 Linux" {
    if (Ir.native_pointer_bits != 64) return error.SkipZigTest;
    const zig_exe = std.testing.environ.getAlloc(test_gpa, "NOV_TEST_ZIG") catch return error.SkipZigTest;
    defer test_gpa.free(zig_exe);
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    for (type_reachability_cases) |case| {
        const c = try emitSourceFreeTypeReachability(test_gpa, case.source);
        defer test_gpa.free(c);
        try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "reachable.c", .data = c });
        for ([_][]const u8{ "x86_64-windows-gnu", "aarch64-linux-gnu" }) |target| for ([_][]const u8{ "-O0", "-O3" }) |mode| {
            const compiled = try std.process.run(test_gpa, std.testing.io, .{
                .argv = &.{ zig_exe, "cc", "-target", target, "-std=c11", mode, "-Wall", "-Wextra", "-Wpedantic", "-Werror", "-c", "reachable.c", "-o", "reachable.o" },
                .cwd = .{ .dir = tmp.dir },
                .timeout = .{ .duration = .{ .raw = .fromSeconds(60), .clock = .awake } },
            });
            defer test_gpa.free(compiled.stdout);
            defer test_gpa.free(compiled.stderr);
            try expectSuccess(compiled, "cross-target C compilation");
        };
    }
}

test "type reachability checks demanded transitive wide fields without emitting unused layouts" {
    for ([_][]const u8{
        "let Row=struct{values:[][][]i128,}\nlet snapshot:(row:Row)->Row=row\nlet main:()={}",
        "let Row=struct{values:[][]([][]u128)->void,}\nlet snapshot:(row:Row)->Row=row\nlet main:()={}",
        "let Row=struct{box:Box(i128),}\nlet Box:(T:type)->type=struct{values:[][]T,}\nlet snapshot:(row:Row)->Row=row\nlet main:()={}",
    }) |source| {
        var module = try Module.init(test_gpa, "reachable-width.nov", source);
        defer module.deinit();
        var lowered = try Lowering.lower(&module);
        defer lowered.deinit();
        try std.testing.expect(!module.hasErrors());
        var diagnostic: Diagnostic = undefined;
        try std.testing.expectError(error.UnsupportedIntegerWidth, emit(test_gpa, &lowered.ir, &diagnostic));
        try std.testing.expectEqualStrings("values", module.sourceSlice(.{ .start = diagnostic.span.start, .end = diagnostic.span.end }).?);
    }
}

test "type reachability follows source free range edges discovered through nominal nested arrays" {
    for ([_]u16{ 32, 128 }) |bits| {
        var ir = Ir.init(test_gpa);
        defer ir.deinit();
        const span: Ir.Span = .{ .start = 0, .end = 0 };
        const record = try ir.internType(.{ .record = .{ .identity = 0, .fields = &.{} } });
        const integer = try ir.internType(.{ .integer = .{ .bits = bits, .signedness = .signed } });
        const range = try ir.internType(.{ .range = integer });
        var nested = range;
        for (0..3) |_| nested = try ir.internType(.{ .array = nested });
        try ir.setRecordFields(record, &.{.{ .name = "values", .ty = nested, .span = span }});
        const signature = try ir.internType(.{ .function = .{ .params = &.{record}, .result = record } });
        const function = try ir.addFunction("snapshot", signature, span);
        try ir.setFunctionBody(function, &.{.{ .ty = record, .span = span, .op = .{ .parameter = 0 } }}, @fromBackingInt(0));
        if (bits == 128) {
            try std.testing.expectError(error.UnsupportedIntegerWidth, emit(test_gpa, &ir, null));
        } else {
            const library = try emit(test_gpa, &ir, null);
            defer test_gpa.free(library);
            const c = try std.mem.concat(test_gpa, u8, &.{ library, "\nint main(void) {nov_rt_heap.collect_every_allocation=1;nov_record_0 row=nov_snapshot((nov_record_0){0});if(row.f0.len)return 1;nov_gc_collect(&nov_rt_heap);nov_gc_shutdown(&nov_rt_heap);return 0;}\n" });
            defer test_gpa.free(c);
            for ([_][]const u8{ "-O0", "-O3" }) |mode|
                try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "", "", mode);
        }
    }
}

const storable_arrays_program =
    \\let E=enum{first,second,}
    \\let Box:(T:type)->type=struct{value:T,}
    \\let Row=struct{values:[][]string,}
    \\let U=union{row:Row,text:string,none,}
    \\let inc:(n:int)->int=n+1
    \\let apply:(f:(int)->int,n:int)->int=f(n)
    \\let callbacks:()->[](int)->int=[inc]
    \\let snapshot:(a:[][]string)->[][]string=a
    \\let main:()={
    \\ let mut kinds:[]E=[.first,.second]
    \\ let oldKinds=kinds
    \\ kinds[0]=.second
    \\ kinds+=[.first]
    \\ loop &kind in kinds {kind=.second}
    \\ let found=E.first in oldKinds
    \\ let absent=E.first in kinds
    \\ if oldKinds[0]!=E.first or !found or absent {@panic("enum equality")}
    \\ let empty:[][]string=[[],[]]
    \\ let mut rows=[[],["a"+"b"]]
    \\ let old=snapshot(rows)
    \\ rows[0]=["new"+"!"]
    \\ loop &row in rows {row+=["tail"+"!"]}
    \\ if old[0].len!=0 or old[1][0]!="ab" or rows[0][1]!="tail!" {@panic("nested snapshot")}
    \\ let mut projected=rows[1]
    \\ projected[0]="changed"
    \\ if rows[1][0]!="ab" {@panic("projection copy")}
    \\ let boxed=Box([][]string)[.value=rows]
    \\ rows+=[["third"]]
    \\ if boxed.value.len!=2 or empty[0].len!=0 {@panic("generic array field")}
    \\ let mut unions:[]U=[.row[Row[.values=rows]],.text["text"+"!"],.none[]]
    \\ let saved=unions
    \\ unions[0]=.none[]
    \\ unions+=[.row[Row[.values=old]]]
    \\ loop &u in unions {u=.none[]}
    \\ let mut extracted=match saved[0] {.row=>|r|r.values
    \\ _=>[]}
    \\ extracted[0]=["different"]
    \\ if match saved[0] {.row=>|r|r.values[0][0]!="new!"
    \\ _=>true} {@panic("union active payload snapshot")}
    \\ let mut ordinary=callbacks()
    \\ ordinary+=[inc]
    \\ ordinary[0]=inc
    \\ if apply(ordinary[1],3)!=4 {@panic("ordinary function array")}
    \\ let mut counter:int=1
    \\ let frozen=rows
    \\ let f=()->int {counter+=1
    \\ if frozen[0][0]!="new!" {@panic("capture snapshot")}
    \\ counter}
    \\ let mut functions=[f]
    \\ let shared=functions
    \\ functions+=[f]
    \\ rows[0]=["rebound"]
    \\ if functions[0]()!=2 or shared[0]()!=3 {@panic("closure shared cell")}
    \\ loop &callback in functions {callback=f}
    \\ loop callback in functions {let result=callback()
    \\ if result<4 {@panic("closure iteration")}}
    \\ let start:int=1
    \\ let mut ranges=[start..3,start..=2]
    \\ let bounds=ranges
    \\ ranges[0]=start..1
    \\ ranges+=[start..2]
    \\ let mut total:int=0
    \\ loop range in bounds {loop n in range {total+=n}}
    \\ loop &range in ranges {range=start..2}
    \\ if total!=6 {@panic("range value bounds")}
    \\ @println("storable arrays ok")
    \\}
;

test "storable arrays recursive snapshots enum union function closure range forced GC all modes" {
    {
        var module = try Module.init(test_gpa, "storable.nov", storable_arrays_program);
        defer module.deinit();
        if (!module.hasErrors()) {
            var lowered = try Lowering.lower(&module);
            defer lowered.deinit();
        }
        if (module.hasErrors()) {
            var out = std.Io.Writer.Allocating.init(test_gpa);
            defer out.deinit();
            try Diagnostics.renderModule(&module, &out.writer);
            std.debug.print("{s}", .{out.written()});
        }
        try std.testing.expect(!module.hasErrors());
    }
    const c = try arrayTestProgram(storable_arrays_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O1", "-O2", "-O3", "-Os" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "storable arrays ok\n", "", mode);
}

test "storable arrays source free and every compiler allocation failure" {
    try testClosureOwnership(test_gpa, storable_arrays_program);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testClosureOwnership, .{storable_arrays_program});
}

test "storable arrays precise backing descriptors survive source release" {
    var lowered = blk: {
        var module = try Module.init(test_gpa, "storable.nov", storable_arrays_program);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        break :blk result;
    };
    defer lowered.deinit();
    const library = try emit(test_gpa, &lowered.ir, null);
    defer test_gpa.free(library);
    var checks = std.Io.Writer.Allocating.init(test_gpa);
    defer checks.deinit();
    try checks.writer.writeAll("int main(void) {nov_rt_heap.collect_every_allocation=1;nov_main();\n");
    for (lowered.ir.types.items, 0..) |ty, i| {
        const id: Ir.TypeId = @fromBackingInt(@intCast(i));
        if (!usesElementLayout(&lowered.ir, id)) continue;
        const roots: usize = switch (ty) {
            .enumeration, .range => 0,
            .array, .function => 1,
            .record => if (ty.record.tagged) 2 else continue,
            else => continue,
        };
        try checks.writer.print("if(nov_element_{d}.pointer_count!={d})return 10;\n", .{ i, roots });
        if (ty == .array or ty == .function) {
            try checks.writer.print("if(nov_element_{d}.pointer_offsets[0]!=offsetof(", .{i});
            try writeType(&lowered.ir, &checks.writer, id);
            try checks.writer.print(",{s}))return 11;\n", .{if (ty == .array) "data" else "env"});
        }
    }
    try checks.writer.writeAll("nov_gc_collect(&nov_rt_heap);nov_gc_shutdown(&nov_rt_heap);return 0;}\n");
    const c = try std.mem.concat(test_gpa, u8, &.{ library, checks.written() });
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "storable arrays ok\n", "", mode);
}

test "storable arrays recursive runtime allocation failures" {
    for ([_][]const u8{
        "let main:()={let a:[][]int=[[1]]\nlet b=a\n@println(b[0][0],a[0][0])}",
        "let R=struct{a:[][]int,}\nlet U=union{some:R,none,}\nlet main:()={let a:[]U=[.some[R[.a=[[1]]]],.none[]]\nlet b=a\n@println(match b[0] {.some=>|r|r.a[0][0]\n.none=>0},a.len)}",
        "let main:()={let mut n:int=1\nlet f=()->int {n+=1\nn}\nlet a=[f]\nlet b=a\n@println(b[0](),a[0]())}",
    }) |source| {
        const library = try emitSource(source, null);
        defer test_gpa.free(library);
        for (0..3) |budget| {
            const prefix = try std.fmt.allocPrint(test_gpa, "#include <stdlib.h>\nstatic size_t count;\nstatic void *test_malloc(size_t n) {{if(count++ >= {d})return NULL;return malloc(n);}}\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", .{budget});
            defer test_gpa.free(prefix);
            const c = try std.mem.concat(test_gpa, u8, &.{ prefix, library, "\nint main(void) {nov_rt_heap.collect_every_allocation=1;nov_main();return 0;}\n" });
            defer test_gpa.free(c);
            for ([_][]const u8{ "-O0", "-O3" }) |mode|
                try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "", "panic: GC allocation failed\n", mode);
        }
    }
}

test "storable arrays forged reused nested and active union payloads detach recursive backing" {
    for ([_]struct { source: []const u8, path: []const u8 }{
        .{ .source = "let main:()={let seed:[]int=[1]\nlet mut a=[seed,seed]\na[-1]=seed\na+=a\n@println(a.len,seed[0],a[0][0])}", .path = "data" },
        .{ .source = "let R=struct{a:[]int,}\nlet U=union{some:R,none,}\nlet main:()={let seed:U=.some[R[.a=[1]]]\nlet mut a=[seed,seed]\na[-1]=seed\na+=a\n@println(a.len,match seed {.some=>|r|r.a[0]\n.none=>0},match a[0] {.some=>|r|r.a[0]\n.none=>0})}", .path = "f0.f0.data" },
    }) |case| {
        var module = try Module.init(test_gpa, "forged-storable.nov", case.source);
        defer module.deinit();
        var lowered = try Lowering.lower(&module);
        defer lowered.deinit();
        try std.testing.expect(!module.hasErrors());
        const body = lowered.ir.functions.items[0].body.?;
        for (@constCast(body.instructions)) |*instruction| switch (instruction.op) {
            .array_literal => |elements| if (elements.len == 2) {
                @constCast(elements)[1] = elements[0];
            },
            .array_set => |*update| {
                update.array = body.instructions[@backingInt(update.array)].op.array_clone;
                update.value = body.instructions[@backingInt(update.value)].op.array_clone;
            },
            .array_append => |*append| append.rhs = append.lhs,
            else => {},
        };
        try lowered.ir.verify();
        const transfers = try ArrayTransfers.init(test_gpa, &lowered.ir, body);
        defer transfers.deinit(test_gpa);
        var instrumented = try emit(test_gpa, &lowered.ir, null);
        defer test_gpa.free(instrumented);
        var checked: usize = 0;
        for (body.instructions, 0..) |instruction, i| {
            const ty = lowered.ir.getType(instruction.ty).?;
            if (ty != .array or !try lowered.ir.needsArrayCopy(ty.array)) continue;
            switch (instruction.op) {
                .array_literal, .array_set, .array_append => {},
                else => continue,
            }
            var operation = std.Io.Writer.Allocating.init(test_gpa);
            defer operation.deinit();
            try writeArrayValue(&lowered.ir, &operation.writer, body, transfers, instruction, i);
            var checks = std.Io.Writer.Allocating.init(test_gpa);
            defer checks.deinit();
            try checks.writer.writeAll(operation.written());
            try checks.writer.writeAll("    { ");
            try writeType(&lowered.ir, &checks.writer, ty.array);
            try checks.writer.print(" *items=v{d}.data;\n", .{i});
            try checks.writer.print("    for(uintptr_t j=0;j<v{d}.len;++j) for(uintptr_t k=0;k<j;++k) assert(items[j].{s}!=items[k].{s});\n", .{ i, case.path, case.path });
            switch (instruction.op) {
                .array_literal => |elements| for (elements, 0..) |value, j| {
                    try checks.writer.print("    assert(items[{d}].{s}!=v{d}.{s});\n", .{ j, case.path, @backingInt(value), case.path });
                },
                .array_set => |update| {
                    try checks.writer.print("    assert(v{d}.data!=v{d}.data);assert(items[1].{s}!=v{d}.{s});\n", .{ i, @backingInt(update.array), case.path, @backingInt(update.value), case.path });
                },
                .array_append => |append| {
                    try checks.writer.writeAll("    ");
                    try writeType(&lowered.ir, &checks.writer, ty.array);
                    try checks.writer.print(" *original=v{d}.data;\n", .{@backingInt(append.lhs)});
                    try checks.writer.print("    for(uintptr_t j=0;j<v{d}.len;++j) assert(items[j].{s}!=original[j%v{d}.len].{s});\n", .{ i, case.path, @backingInt(append.lhs), case.path });
                },
                else => unreachable,
            }
            try checks.writer.writeAll("    }\n");
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, instrumented, operation.written()));
            const replaced = try std.mem.replaceOwned(u8, test_gpa, instrumented, operation.written(), checks.written());
            test_gpa.free(instrumented);
            instrumented = replaced;
            checked += 1;
        }
        try std.testing.expectEqual(@as(usize, 3), checked);
        const prefixed = try std.mem.concat(test_gpa, u8, &.{ "#include <assert.h>\n", instrumented });
        defer test_gpa.free(prefixed);
        const c = try collectingTestProgram(prefixed);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "4 1 1\n", "", mode);
    }
}

test "storable arrays individual elements retain nominal printing contracts" {
    const source =
        \\let E=enum{first,second,
        \\ @[stringify]
        \\ let toString:(e:E)->string=if e==E.first {"first"} else {"second"}
        \\}
        \\let Alias=E
        \\let R=struct{s:string,
        \\ @[stringify]
        \\ let toString:(r:R)->string=r.s+"!"
        \\}
        \\let U=union{some:R,none,
        \\ @[stringify]
        \\ let toString:(u:U)->string=match u {.some=>|r|"{r}"
        \\ .none=>"none"}
        \\}
        \\let main:()={let a:[]Alias=[.first,.second]
        \\ let r=[R[.s="record"]]
        \\ let u:[]U=[.some[r[0]],.none[]]
        \\ @println(a[0],r[0],u[0],u[1],a[0]==E.first,Alias.second in a)}
    ;
    var module = try Module.init(test_gpa, "print-storable.nov", source);
    defer module.deinit();
    if (module.hasErrors()) {
        var out = std.Io.Writer.Allocating.init(test_gpa);
        defer out.deinit();
        try Diagnostics.renderModule(&module, &out.writer);
        std.debug.print("{s}", .{out.written()});
    }
    try std.testing.expect(!module.hasErrors());
    const c = try arrayTestProgram(source);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "first record! record! none true true\n", "", mode);
}

test "storable arrays ordinary higher order functions without closure representation" {
    const c = try arrayTestProgram(
        \\let inc:(n:int)->int=n+1
        \\let identity:(f:(int)->int)->(int)->int=f
        \\let main:()={let mut functions=[identity]
        \\ let copy=functions
        \\ functions+=[identity]
        \\ functions[0]=identity
        \\ loop &f in functions {f=identity}
        \\ @println(copy[0](inc)(4),functions[1](inc)(8))}
    );
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "5 9\n", "", mode);
}

const closures_program =
    \\let inferred=(value:int)->int {value+3}
    \\let apply: (f: (int)->int, n:int)->int = f(n)
    \\let ordinary: (n:int)->int = n+1
    \\let make: (seed:int)->()->int = {
    \\ let mut n=seed
    \\ let inc=()->int {
    \\ n+=1
    \\ n
    \\ }
    \\ let shared=inc
    \\ @println(shared())
    \\ inc
    \\}
    \\let main:()={
    \\ let double=(value:int)->int {value+value}
    \\ let declared:(value:int)->int=value+4
    \\ @println(apply(double,3),apply(ordinary,3),inferred(2),declared(2))
    \\ let f=make(10)
    \\ @println(f(),f())
    \\ let mut a:[]int=[1,2]
    \\ let snapshot=a
    \\ let read=()->int {snapshot[0]}
    \\ let write=() {a[0]=9}
    \\ let other=()->int {a[0]}
    \\ write()
    \\ @println(read(),other(),a[0])
    \\ let nested=()->()->int {()->int {a[0]}}
    \\ let inner=nested()
    \\ a[0]=7
    \\ @println(inner())
    \\}
;

test "closures snapshots shared escaping cells nested functions and forced GC" {
    const c = try arrayTestProgram(closures_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "/* separate translation unit */\nint nov_test_dummy;\n", 0, "6 4 5 6\n11\n12 13\n1 9 9\n7\n", "", mode);
}

const constructor_shorthand_program =
    \\let Box:(T:type)->type=struct{value:T,}
    \\let Alias=Box([]int)
    \\let R=struct{value:[]int,label:string,n:int=9,}
    \\let label="module"
    \\let number:int=7
    \\let N=struct{number:int,}
    \\let M=struct{n:int,}
    \\let borrow:(a:[]int)->R={let value=&a
    \\ R[.value,.label]}
    \\let factory:()->()->R={let value:[]int=[4]
    \\ let text="captured"+"!"
    \\ ()->R {if true {R[.value,.label=text]} else {R[.value,.label]}}}
    \\let main:()={
    \\ let mut value:[]int=[1]
    \\ let change=()->string {value[0]=2
    \\ "changed"+"!"}
    \\ let first=R[.value,.label=change()]
    \\ let second=R[.label=change(),.value]
    \\ value[0]=3
    \\ @println(first.value[0],second.value[0],value[0],first.n,first.label)
    \\ let mut box:Alias=Box([]int)[.value]
    \\ value[0]=8
    \\ @println(box.value[0],value[0],N[.number].number)
    \\ let f=factory()
    \\ let r=f()
    \\ @println(r.value[0],r.label,r.n)
    \\ let b=borrow(value)
    \\ value[0]=10
    \\ @println(b.value[0],value[0],b.label)
    \\ let mut n:int=1
    \\ let nested=()->()->int {()->int {M[.n].n}}
    \\ let inner=nested()
    \\ n=5
    \\ @println(inner())
    \\}
;

test "constructor shorthand runtime snapshots captures generic aliases borrows order and GC" {
    const c = try arrayTestProgram(constructor_shorthand_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "1 2 3 9 changed!\n3 8 7\n4 captured! 9\n8 10 module\n5\n", "", mode);
    try testClosureOwnership(test_gpa, constructor_shorthand_program);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testClosureOwnership, .{constructor_shorthand_program});
}

test "constructor shorthand keeps contextual and qualified union enum payload grammar" {
    const c = try arrayTestProgram(
        \\let K=enum{ready,}
        \\let U=union{item:K,none,}
        \\let main:()={
        \\ let ready:int=7
        \\ let factory=()->U {U.item[.ready]}
        \\ let a:U=.item[.ready]
        \\ let b=factory()
        \\ @println(match a {.item=>|k|k==K.ready
        \\ .none=>false}, match b {.item=>|k|k==K.ready
        \\ .none=>false},ready)
        \\}
    );
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "true true 7\n", "", mode);
}

test "constructor shorthand runtime OOM uses ordinary array copy failure" {
    const source = "let R=struct{value:[]int,}\nlet main:()={let value:[]int=[1]\n@println(\"before\")\nlet r=R[.value]\n@println(r.value[0])\n@println(value[0])}";
    const program = try arrayTestProgram(source);
    defer test_gpa.free(program);
    const c = try std.mem.concat(test_gpa, u8, &.{ "#include <stdlib.h>\nstatic size_t count;\nstatic void *test_malloc(size_t n) {if(count++ >= 1)return NULL;return malloc(n);}\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", program });
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 1, "before\n", "panic: GC allocation failed\n");
}

const closure_types_program =
    \\let K=enum{first,second,}
    \\let R=struct{s:string,n:int,}
    \\let U=union{none,some:R,}
    \\let identity:(n:int)->int=n
    \\let factory:(s:string, r:R, u:U, a:[]R, flag:bool, rune_value:rune, kind:K, callback:(int)->int)->()->string={
    \\ let bounds=0..2
    \\ ()->string {
    \\  loop i in bounds {@print(i)}
    \\  @println(flag,rune_value,kind==K.second,callback(r.n),a[0].s)
    \\  match u {
    \\   .none=>s
    \\   .some=>|v| s+v.s
    \\  }
    \\ }
    \\}
    \\let main=(){
    \\ let f=factory("a"+"b", R[.s="r"+"!",.n=7], .some[R[.s="u"+"!",.n=8]], [R[.s="array"+"!",.n=9]], true, 'é', K.second, identity)
    \\ @println(f())
    \\ let mut s="old"+"!"
    \\ let snapshot=s
    \\ let nested=()->()->string {()->string {snapshot}}
    \\ s="new"+"!"
    \\ let inner=nested()
    \\ @println(inner(),s)
    \\ let mut target=()->int {1}
    \\ let invoke=()->int {target()}
    \\ let saved=target
    \\ target=()->int {2}
    \\ @println(invoke(),saved())
    \\ let branch=(b:bool)->string {if b {snapshot} else {snapshot+"?"}}
    \\ @println(branch(true),branch(false))
    \\}
;

test "closures capture supported local types ranges function cells and private branches" {
    const c = try arrayTestProgram(closure_types_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "01true é true 7 array!\nabu!\nold! new!\n2 1\nold! old!?\n", "", mode);
}

const backtick_program =
    \\@[noreturn]
    \\let halt:()=@panic("stop")
    \\let forward:(x:`T)->`T=halt()
    \\let choose:(flag:bool,x:`T)->`U=if flag {halt()} else {x}
    \\let finish:()->`T=return
    \\let loopValue:(x:`T)->`U=loop {break x}
    \\let makeVoid:(x:`T)->()->`U=()->`U{return}
    \\let makeBox:(x:`T)->Box(`T)=Box(`T)[.value=x]
    \\let getBox:(x:Box(`T))->`T=x.value
    \\let add:(x:`a,y:`b)->`c=x+y
    \\let first:(items:[]`T)->`T=items[0]
    \\let identity:(value:`T)->`T=value
    \\let make:(value:`T)->()->`U=()->`U{return value}
    \\let Box:(T:type)->type=struct{value:T,}
    \\let unwrap:(x:Box(`T))->`U=x.value
    \\let invert:(x:`T)->`U=!x
    \\let size:(x:`T)->`U=x.len
    \\let G:(T:type)->type=struct{let id:(x:`U)->`U=x
    \\}
    \\let wrap:(x:`T)->`U=G(uint).id(x)
    \\let cell:(value:`T)->(`T)->`T={let mut saved:`T=value
    \\(next:`T)->`T{saved=next
    \\saved}}
    \\let step:(x:uint)->uint={@println("arg",x)
    \\x}
    \\let main:()={
    \\let fresh=getBox(makeBox(true))
    \\let done=makeVoid(true)
    \\done()
    \\let n:uint=add(step(1),step(2))
    \\@println(n,add("ab","cd"))
    \\let mut a:[][]uint=[[1,2],[3]]
    \\let b=identity(a)
    \\let snapshot=make(a)
    \\let mut row=a[0]
    \\row[0]=9
    \\a[0]=row
    \\let c=snapshot()
    \\@println(first(first(b)),first(first(c)),a[0][0])
    \\let next=cell("old")
    \\let alias=next
    \\@println(next("new"),alias("last"))
    \\let box=Box(string)[.value="field"]
    \\@println(unwrap(box),invert(true),size("abc"),size([true,false]),wrap("ok"))
    \\@println(fresh,if false {forward(true)} else {true},choose(false,"live"),loopValue("break"))
    \\finish()}
;

fn emitBacktickSourceFree(allocator: std.mem.Allocator) ![]u8 {
    return emitBacktickProgramSourceFree(allocator, backtick_program);
}

fn emitBacktickProgramSourceFree(allocator: std.mem.Allocator, source: []const u8) ![]u8 {
    var lowered = blk: {
        var module = try Module.init(allocator, "backtick.nov", source);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        if (module.hasErrors()) std.debug.print("backtick execution diagnostics: {any} {any}\n", .{ module.source.ast.errors, module.diagnostics.items });
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(result.lowering_complete);
        break :blk result;
    };
    defer lowered.deinit();
    try lowered.ir.verify();
    return emit(allocator, &lowered.ir, null);
}

fn testBacktickSourceFree(allocator: std.mem.Allocator) !void {
    const c = try emitBacktickSourceFree(allocator);
    defer allocator.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_env_element") != null);
}

test "backtick source free exact functions closures recursive copies LTR and forced GC C11" {
    const library = try emitBacktickSourceFree(test_gpa);
    defer test_gpa.free(library);
    const c = try collectingTestProgram(library);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "arg 1\narg 2\n3 abcd\n1 1 9\nnew last\nfield false 3 2 ok\ntrue true live break\n", "", mode);
}

test "backtick source free compiler and emitter every allocation failure" {
    try testBacktickSourceFree(test_gpa);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testBacktickSourceFree, .{});
}

const contextual_backtick_program =
    \\let identity:(x:`T)->`T=x
    \\let alias=identity
    \\let apply:(f:(`T)->`U,x:`T)->`U=f(x)
    \\let reverse:(x:`T,f:(`T)->`U)->`U=f(x)
    \\let make:(x:`T)->()->`T=()->`T{x}
    \\let result:()->(uint)->uint=identity
    \\let NS=struct{let id:(x:`T)->`T=x
    \\}
    \\let G:(T:type)->type=struct{let pick:(x:`U,y:T)->`U=x
    \\}
    \\let main:()={
    \\let mut order:uint=0
    \\let callee=()->((uint)->uint,uint)->uint{order=order*10+1
    \\@as(((uint)->uint,uint)->uint,apply)}
    \\let callback=()->(uint)->uint{order=order*10+2
    \\alias}
    \\let arg=()->uint{order=order*10+3
    \\42}
    \\let a=callee()(callback(),arg())
    \\@println(a,order)
    \\let cb:(uint)->uint=identity
    \\let cast=@as((uint)->uint,alias)
    \\let mut slot:(uint)->uint=NS.id
    \\slot=identity
    \\let branch:(uint)->uint=if true {alias} else {NS.id}
    \\let funcs:[](uint)->uint=[identity,alias]
    \\let method:(uint,bool)->uint=G(bool).pick
    \\let b:uint=apply(identity,42)
    \\let c:uint=reverse(42,identity)
    \\@println(cb(a),cast(b),slot(c),branch(4),funcs[1](5),method(6,true),result()(7))
    \\let factory:([][]string)->()->[][]string=make
    \\let mut rows:[][]string=[["old"+"!"]]
    \\let saved=factory(rows)
    \\rows[0]=["new"+"!"]
    \\let copy=saved()
    \\let through=apply(make,"captured"+"!")
    \\@println(copy[0][0],rows[0][0],through())}
;

fn testContextualBacktickSourceFree(allocator: std.mem.Allocator) !void {
    allocator.free(try emitBacktickProgramSourceFree(allocator, contextual_backtick_program));
}

test "backtick contextual values source free canonical callbacks LTR once captures forced GC C11 O0 O3" {
    const library = try emitBacktickProgramSourceFree(test_gpa, contextual_backtick_program);
    defer test_gpa.free(library);
    const c = try collectingTestProgram(library);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "42 123\n42 42 42 4 5 6 7\nold! new! captured!\n", "", mode);
}

test "backtick contextual values source free compiler emitter every allocation failure" {
    try testContextualBacktickSourceFree(test_gpa);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testContextualBacktickSourceFree, .{});
}

test "backtick contextual returned closure exact runtime budgets and forced GC" {
    const library = try emitBacktickProgramSourceFree(test_gpa,
        \\let make:(x:`T)->()->`T=()->`T{x}
        \\let main:()={let factory:(string)->()->string=make
        \\let saved=factory("alive"+"!")
        \\@println(saved())}
    );
    defer test_gpa.free(library);
    const program = try collectingTestProgram(library);
    defer test_gpa.free(program);
    for (0..4) |budget| {
        const prefix = try std.fmt.allocPrint(test_gpa, "#include <stdlib.h>\n#include <assert.h>\nstatic unsigned allocations, outstanding;\n" ++
            "static void *test_malloc(size_t n) {{ if (allocations == {d}) return NULL; void *p = malloc(n); if (p) {{ ++allocations; ++outstanding; }} return p; }}\n" ++
            "static void test_free(void *p) {{ assert(outstanding); --outstanding; free(p); }}\n" ++
            "#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE test_free\n#define main test_main\n", .{budget});
        defer test_gpa.free(prefix);
        const counted = try std.mem.concat(test_gpa, u8, &.{ prefix, program, "\n#undef main\nint main(void) { int result = test_main(); assert(!outstanding); assert(allocations == 3); return result; }\n" });
        defer test_gpa.free(counted);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(counted, "typedef int nonempty_translation_unit;", if (budget == 3) 0 else 1, if (budget == 3) "alive!\n" else "", if (budget == 3) "" else "panic: GC allocation failed\n", mode);
    }
}

fn emitBacktickImportsSourceFree(allocator: std.mem.Allocator) ![]u8 {
    var sema = blk: {
        var comp = Compilation.init(allocator);
        defer comp.deinit();
        const root = try comp.addSource("/app/main.nov",
            \\let api=@import("api.nov")
            \\let alias=api.alias
            \\let method=api.NS.id
            \\let forward:(x:`T)->`T=api.stop()
            \\let staticForward:(x:`T)->`T=api.NS.stop()
            \\let main:()={let cb:(uint,uint)->uint=alias
            \\let n:uint=cb(1,2)
            \\let ns:(string)->string=method
            \\let decorated:(string)->string=api.decorate
            \\@println(n,api.alias("a","b"),ns("namespace"),decorated("ok"),if false {forward(true)} else {true},if false {staticForward(true)} else {true})}
        );
        const api = try comp.addSource("/app/api.nov",
            \\let prefix="library:"
            \\@[public]
            \\@[noreturn]
            \\let stop:()=@panic("stop")
            \\@[public]
            \\let add:(x:`a,y:`b)->`c=x+y
            \\@[public]
            \\let alias=add
            \\@[public]
            \\let decorate:(x:`T)->string=prefix+x
            \\@[public]
            \\let NS=struct{
            \\@[public]
            \\let id:(x:`T)->`T=x
            \\@[public]
            \\@[noreturn]
            \\let stop:()=@panic("static")
            \\}
        );
        const module = comp.getModule(root).?;
        const local = module.lookup("api").?;
        try comp.imports.append(allocator, .{ .declaration = comp.declaration_ids.get(.{ .source = root, .local = local }).?, .target = api, .named = false, .span = module.getDeclaration(local).?.name_span });
        comp.setRoot(root);
        var result = try Lowering.lowerCompilation(&comp);
        errdefer result.deinit();
        try std.testing.expect(!comp.hasErrors());
        try std.testing.expect(result.lowering_complete);
        break :blk result;
    };
    defer sema.deinit();
    try sema.ir.verify();
    return emit(allocator, &sema.ir, null);
}

fn testBacktickImportAllocations(allocator: std.mem.Allocator) !void {
    allocator.free(try emitBacktickImportsSourceFree(allocator));
}

test "backtick imported aliases and namespace functions execute after graph release with forced GC" {
    const library = try emitBacktickImportsSourceFree(test_gpa);
    defer test_gpa.free(library);
    const c = try collectingTestProgram(library);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "3 ab namespace library:ok true true\n", "", mode);
}

test "backtick imported functions own source free IR under every allocation failure" {
    try testBacktickImportAllocations(test_gpa);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testBacktickImportAllocations, .{});
}

fn testClosureOwnership(allocator: std.mem.Allocator, source: []const u8) !void {
    var lowered = blk: {
        var module = try Module.init(allocator, "closures.nov", source);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(result.lowering_complete);
        break :blk result;
    };
    defer lowered.deinit();
    try lowered.ir.verify();
    const c = try emitProgram(allocator, &lowered.ir, null);
    defer allocator.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_env_element") != null);
}

const generic_closure_block_probe_program =
    \\let id:(x:`A)->`A=x
    \\let alias=id
    \\let apply:(callback:(`A)->`B,input:`A)->`B=callback(input)
    \\let main:()={
    \\ @println(id({let f=(x:`T)->`T{x}
    \\  f(true)}))
    \\ @println(alias({let f:(x:`T)->`T=x
    \\  f(false)}))
    \\ @println(id({let saved:[]bool=[true]
    \\  let f=(x:`T)->`T{if saved[0] {x} else {x}}
    \\  let g=(x:`U)->`U{f(x)}
    \\  {g(true)}}))
    \\ @println(apply({let f=(x:`T)->`T{x}
    \\  f},true),apply({let f:(x:`T)->`T=x
    \\  {f}},false))
    \\ @println(id({let f=(x:`T)->`T{x}
    \\  let bound:(bool)->bool=f
    \\  let copied=bound
    \\  copied})(true))
    \\ @println(id(loop {let f=(x:`T)->`T{x}
    \\  loop {break}
    \\  break f(false)}))
    \\ @println(id({let f=(x:bool)->bool{x}
    \\  f(true)}))
    \\ @println(id({let f=(x:`T)->`T{x}
    \\  f(false)}))
    \\ let f:int=9
    \\ @println(f)
    \\}
;

test "generic closures block probes infer temporary templates callbacks and loop results with source free GC" {
    const c = try arrayTestProgram(generic_closure_block_probe_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "true\nfalse\ntrue\ntrue false\ntrue\nfalse\ntrue\nfalse\n9\n", "", mode);
}

test "generic closures block probes source free compiler allocation rollback" {
    try testClosureOwnership(test_gpa, generic_closure_block_probe_program);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testClosureOwnership, .{generic_closure_block_probe_program});
}

const generic_closure_review_program =
    \\let id:(x:`A)->`A=x
    \\let apply:(callback:(`A)->`A,input:`A)->`A=callback(input)
    \\let R=struct{
    \\ let helper:(x:bool)->int=1
    \\ let method:(self:R,input:`A)->`U={
    \\  let f=(x:`B)->`C{helper(x)}
    \\  f(input)}
    \\}
    \\let Box:(Element:type)->type=struct{
    \\ let Alias=@This()
    \\ let helper:(x:Element)->int=2
    \\ let method:(self:Alias,input:`A)->`U={
    \\  let f=(x:`B)->`C{let nested=(value:`D)->`E{helper(value)}
    \\   nested(x)}
    \\  f(input)}
    \\}
    \\let main:()={
    \\ @println(id({(x:`T)->`T{x}(true)}))
    \\ @println(id({apply((x:`T)->`T{x},false)}))
    \\ @println(R[].method(true),Box(bool)[].method(false))
    \\ {let gone:int=7
    \\  @print(gone)}
    \\ let uncaptured:[][]int=[[7]]
    \\ let f=(x:`T)->`T{let gone:int=9
    \\  let later:int=10
    \\  {let sibling:int=1
    \\  }
    \\  {let sibling:int=2
    \\  }
    \\  x}
    \\ let later:int=11
    \\ @println(f(true),f(@as(int,3)))
    \\}
;

test "generic closures review argument blocks method scopes lexical definitions and source free GC" {
    const c = try arrayTestProgram(generic_closure_review_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "true\nfalse\n1 2\n7true 3\n", "", mode);
}

test "generic closures review lexical conflicts do not become runtime captures" {
    var module = try Module.init(test_gpa, "lexical-capture-separation.nov", generic_closure_review_program);
    defer module.deinit();
    var lowered = try Lowering.lower(&module);
    defer lowered.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(lowered.lowering_complete);
    var carriers: usize = 0;
    for (lowered.ir.functions.items) |function| {
        if (!std.mem.startsWith(u8, function.name, "$template")) continue;
        carriers += 1;
        try std.testing.expectEqual(@as(usize, 0), function.captures.len);
    }
    try std.testing.expect(carriers != 0);
    try lowered.ir.verify();
}

test "generic closures review argument blocks and method scopes allocation rollback" {
    try testClosureOwnership(test_gpa, generic_closure_review_program);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testClosureOwnership, .{generic_closure_review_program});
}

const generic_closures_program =
    \\let Receiver=struct{value:int,
    \\ let through:(self:Receiver,callback:(`T)->`T,input:`T)->`T=callback(input)
    \\}
    \\let reverse:(input:`T,callback:(`T)->`T)->`T=callback(input)
    \\let outer:(input:`A)->`A={
    \\ let forward=(value:`B)->`B{value}
    \\ let local:(value:`C)->`C={let ordinary=(item:`C)->`C{item}
    \\  ordinary(forward(value))}
    \\ local(input)
    \\}
    \\let apply:(callback:(`T)->`T,input:`T)->`T=callback(input)
    \\let make:(initial:int)->(bool)->bool={
    \\ let mut count=initial
    \\ let frozen:[]int=[initial]
    \\ let local:(value:`T)->`T={count+=1
    \\  @print(count,frozen[0])
    \\  value}
    \\ let first:(bool)->bool=local
    \\ let again:(bool)->bool=local
    \\ @println(local(@as(int,7)),first(true),again(false))
    \\ first
    \\}
    \\let main:()={
    \\ let a=make(10)
    \\ let b=make(20)
    \\ let copy=a
    \\ @println(copy(true),b(false),a(true))
    \\ let identity=(value:`T)->`T{value}
    \\ @println(identity(true),identity(@as(int,42)),apply(identity,false))
    \\ @println(outer(true),outer(@as(int,9)))
    \\ @println((item:`T)->`T{item}(false),apply((item:`T)->`T{item},true))
    \\ @println(Receiver[.value=1].through(identity,true),reverse(false,identity))
    \\ @println((left:`T,right:`U)->`T{left}({@print("L")
    \\  true},{@print("R")
    \\  @as(int,1)}))
    \\}
;

test "generic closures canonical code shares one creation snapshot and verifies forged environments" {
    var module = try Module.init(test_gpa, "generic-capture-ir.nov", "let main:()={let frozen:[]int=[1]\nlet mut n:int=0\nlet f=(x:`T)->`T{n+=frozen[0]\nx}\nlet a:(bool)->bool=f\nlet b:(bool)->bool=f\n@println(f(true),f(@as(int,1)),a(false),b(true))}");
    defer module.deinit();
    var lowered = try Lowering.lower(&module);
    defer lowered.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(lowered.lowering_complete);
    try lowered.ir.verify();
    try std.testing.expectEqual(@as(usize, 2), lowered.ir.local_generic_instances.items.len);
    var carriers: usize = 0;
    var specialized: usize = 0;
    for (lowered.ir.functions.items) |function| {
        if (function.environment_layout != null) specialized += 1;
        if (std.mem.startsWith(u8, function.name, "$template")) carriers += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), carriers);
    try std.testing.expectEqual(@as(usize, 2), specialized);
    var snapshots: usize = 0;
    var forged: usize = 0;
    for (lowered.ir.functions.items) |function| if (function.body) |body| {
        for (@constCast(body.instructions), 0..) |*instruction, index| {
            if (instruction.op == .array_clone and !function.is_closure) snapshots += 1;
            if (instruction.op != .closure_specialize) continue;
            const saved = instruction.*;
            instruction.op.closure_specialize.environment = @fromBackingInt(@intCast(index));
            try std.testing.expectError(error.InvalidValueId, lowered.ir.verify());
            instruction.* = saved;
            instruction.op.closure_specialize.function = @fromBackingInt(std.math.maxInt(u32));
            try std.testing.expectError(error.InvalidFunctionId, lowered.ir.verify());
            instruction.* = saved;
            const target = &lowered.ir.functions.items[@backingInt(saved.op.closure_specialize.function)];
            const layout = target.environment_layout;
            target.environment_layout = null;
            try std.testing.expectError(error.InvalidOperation, lowered.ir.verify());
            target.environment_layout = layout;
            const carrier = body.instructions[@backingInt(saved.op.closure_specialize.environment)].op.closure_init;
            instruction.op = .{ .closure_init = .{ .function = saved.op.closure_specialize.function, .captures = carrier.captures } };
            try std.testing.expectError(error.InvalidOperation, lowered.ir.verify());
            instruction.* = saved;
            forged += 1;
        }
    };
    try std.testing.expectEqual(@as(usize, 1), snapshots);
    try std.testing.expectEqual(@as(usize, 4), forged);
    try lowered.ir.verify();
}

test "generic closures loop instances snapshots deep copies and transitive ordinary closures" {
    const c = try arrayTestProgram(
        \\let main:()={
        \\ let mut first:(bool)->int=(b:bool)->int{0}
        \\ let mut last:(bool)->int=(b:bool)->int{0}
        \\ loop i in @as(int,0)..2 {
        \\  let mut count=i
        \\  let arrays:[][]int=[[i]]
        \\  let generic=(x:`T)->int{count+=1
        \\   let changed=arrays
        \\   let mut row=changed[0]
        \\   row[0]=99
        \\   count+arrays[0][0]}
        \\  let ordinary=(b:bool)->int{if b {generic(true)} else {generic(@as(int,3))}}
        \\  if i==0 {first=ordinary} else {last=ordinary}
        \\ }
        \\ @println(first(true),first(false),last(true),last(false))
        \\ let frozen:[]int=[4]
        \\ let read=(x:`T)->int{frozen[0]}
        \\ let mutate=(items:[]int){let mut copy=items
        \\  copy[0]=9}
        \\ mutate(frozen)
        \\ @println(read(true),read(@as(int,0)))
        \\ let mut calls:int=0
        \\ let callback=(b:bool)->bool{calls+=1
        \\  b}
        \\ let invoke=(x:`T)->`T{let called=callback(true)
        \\  x}
        \\ @println(invoke(true),invoke(@as(int,1)),calls)
        \\}
    );
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "1 2 3 4\n4 4\ntrue 1 2\n", "", mode);
}

test "generic closures allocate at creation even unused and never recapture on specialization or copy" {
    const cases = [_]struct { source: []const u8, limit: usize, output: []const u8, fails: bool }{
        .{ .source = "let main:()={let frozen:int=1\nlet unused=(x:`T)->`T{@print(frozen)\nx}\n@println(@as(int,7))}", .limit = 1, .output = "", .fails = true },
        .{ .source = "let main:()={let frozen:int=1\nlet unused=(x:`T)->`T{@print(frozen)\nx}\n@println(@as(int,7))}", .limit = 2, .output = "7\n", .fails = false },
        .{ .source = "let main:()={let mut n:int=1\nlet frozen:int=1\nlet f=(x:`T)->`T{n+=frozen\nx}\nlet a:(bool)->bool=f\nlet b=a\n@println(b(true),f(@as(int,7)),n)}", .limit = 3, .output = "true 7 3\n", .fails = false },
    };
    for (cases) |case| {
        const library = try emitSource(case.source, null);
        defer test_gpa.free(library);
        const prefix = try std.fmt.allocPrint(test_gpa, "#include <stdlib.h>\nstatic size_t count;\nstatic void *test_malloc(size_t n) {{if(count++ >= {d}) return NULL; return malloc(n);}}\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", .{case.limit});
        defer test_gpa.free(prefix);
        const program = try collectingTestProgram(library);
        defer test_gpa.free(program);
        const c = try std.mem.concat(test_gpa, u8, &.{ prefix, program });
        defer test_gpa.free(c);
        try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", if (case.fails) 1 else 0, case.output, if (case.fails) "panic: GC allocation failed\n" else "");
    }
}

test "generic closures source free environment ownership and allocation rollback" {
    const source = "let main:()={let mut n:int=1\nlet f=(x:`T)->`T{n+=1\nx}\nlet b:(bool)->bool=f\n@println(f(@as(int,2)),b(true))}";
    try testClosureOwnership(test_gpa, source);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testClosureOwnership, .{source});
}

test "generic closures creation environments direct calls contextual callbacks and GC" {
    const c = try arrayTestProgram(generic_closures_program);
    defer test_gpa.free(c);
    try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "11 1012 1013 107 true false\n21 2022 2023 207 true false\n14 1024 2015 10true false true\ntrue 42 false\ntrue 9\nfalse true\ntrue false\nLRtrue\n", "", "-O0");
}

test "closures own source free metadata and survive every compiler allocation failure" {
    for ([_][]const u8{ closures_program, closure_types_program }) |source| {
        try testClosureOwnership(test_gpa, source);
        try TestAllocations.checkAllAllocationFailures(test_gpa, testClosureOwnership, .{source});
    }
}

test "closures runtime OOM checks cells environments snapshots and allocation free copies" {
    const allocating = [_][]const u8{
        "let main:()={let mut n:int=1\nlet f=()->int {n}\n@println(f())}",
        "let main:()={let n:int=1\nlet f=()->int {n}\n@println(f())}",
        "let main:()={let a:[]int=[1]\nlet f=()->int {a[0]}\n@println(f())}",
    };
    for (allocating) |source| {
        const library = try arrayTestProgram(source);
        defer test_gpa.free(library);
        // Fail at successive allocation sites, including the immutable capture
        // box after the environment has already been rooted.
        for (0..3) |limit| {
            const prefix = try std.fmt.allocPrint(test_gpa, "#include <stdlib.h>\nstatic size_t count;\nstatic void *test_malloc(size_t n) {{if(count++ >= {d}) return NULL; return malloc(n);}}\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", .{limit});
            defer test_gpa.free(prefix);
            const c = try std.mem.concat(test_gpa, u8, &.{ prefix, library });
            defer test_gpa.free(c);
            const needed: usize = if (std.mem.indexOf(u8, source, "[]int") != null) 5 else 2;
            try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", if (limit < needed) 1 else 0, if (limit < needed) "" else "1\n", if (limit < needed) "panic: GC allocation failed\n" else "");
        }
    }
    const library = try emitSource("let take:(f:()->string)->()->string=f\nlet main:()={let s=\"shared\"\nlet f=()->string {s}\nlet g=take(f)\nlet h=g\n@println(h())}", null);
    defer test_gpa.free(library);
    const c = try std.mem.concat(test_gpa, u8, &.{ "#include <stdlib.h>\nstatic size_t count;\nstatic void *test_malloc(size_t n) {if(count++ >= 2) return NULL; return malloc(n);}\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", library, "\nint main(void){nov_rt_heap.collect_every_allocation=1;nov_main();nov_gc_collect(&nov_rt_heap);if(nov_rt_heap.frames || nov_rt_heap.live_objects)return 70;nov_gc_shutdown(&nov_rt_heap);return 0;}\n" });
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, "shared\n", "");
}

test "closures support scoped borrows reference iteration and per iteration cells" {
    const c = try arrayTestProgram(
        \\let main=(){
        \\ let inspect=(input:int)->int {let view=&input
        \\  view}
        \\ @println(inspect(4))
        \\ let visit=(items:[]int){loop &element in items {@print(element)}}
        \\ visit([1,2])
        \\ @println()
        \\ let mut values:[]int=[1,2]
        \\ let change=(){loop &item in values {item+=1}}
        \\ change()
        \\ @println(values[0],values[1])
        \\ let frozen=values
        \\ let read=()->int {
        \\  let view=&frozen[0]
        \\  view
        \\ }
        \\ let read_loop=(){loop &item in frozen {@print(item)}}
        \\ @println(read())
        \\ read_loop()
        \\ @println()
        \\ let mut first:()->int=()->int {0}
        \\ let mut last:()->int=()->int {0}
        \\ loop i in @as(int,0)..2 {
        \\  let mut value=i
        \\  let next=()->int {value+=1
        \\   value}
        \\  if i==0 {first=next} else {last=next}
        \\ }
        \\ @println(first(),first(),last(),last())
        \\}
    );
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "4\n12\n2 3\n2\n23\n1 2 2 3\n", "", mode);
}

test "closures evaluate callees before arguments and retain replaced environments" {
    const c = try arrayTestProgram(
        \\let apply=(callback:(int)->int,input:int)->int {callback(input)}
        \\let main=(){
        \\ @println(apply((value:int)->int {value+1},2))
        \\ @println(() -> int {7}())
        \\ let saved="saved"+"!"
        \\ let mut target=(suffix:string)->string {saved+suffix}
        \\ @println(target({target=(suffix:string)->string {"wrong"}
        \\  "?"+"!"}))
        \\ @println(target("ignored"))
        \\ let mut n:int=1
        \\ let outer=()->(int)->int {@print("C")
        \\  (add:int)->int {n+add}}
        \\ @println(outer()({@print("A")
        \\  n=5
        \\  @as(int,2)}))
        \\}
    );
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "3\n7\nsaved!?!\nwrong\nCA7\n", "", mode);
}

test "closures trace shared function cycles and keep propagation scoped" {
    const c = try arrayTestProgram(
        \\let R=union{ok:int,err:string,}
        \\let main=(){
        \\ let mut next:()->int=()->int {0}
        \\ next=()->int {if false {next()} else {7}}
        \\ @println(next())
        \\ let message="failure"+"!"
        \\ let choose=(success:bool)->R {if success {.ok[1]} else {.err[message]}}
        \\ let transform=(success:bool)->R {
        \\  let value=choose(success).!
        \\  .ok[value+1]
        \\ }
        \\ loop success in [true,false] {
        \\  match transform(success) {
        \\   .ok=>|value| @println(value)
        \\   .err=>|error| @println(error)
        \\  }
        \\ }
        \\}
    );
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "7\n2\nfailure!\n", "", mode);
}

test "closures reject forged capture indices kinds sources and cell flags" {
    var module = try Module.init(test_gpa, "captures.nov", "let main=(){let mut n:int=1\nlet frozen:int=2\nlet f=()->int {n+frozen}\n@println(f())}");
    defer module.deinit();
    var lowered = try Lowering.lower(&module);
    defer lowered.deinit();
    try std.testing.expect(!module.hasErrors());
    try lowered.ir.verify();
    var checked: usize = 0;
    for (lowered.ir.functions.items) |function| {
        const instructions = @constCast(function.body.?.instructions);
        for (instructions) |*instruction| {
            const saved = instruction.*;
            switch (instruction.op) {
                .capture_value, .capture_cell => {
                    if (instruction.op == .capture_value) instruction.op.capture_value = std.math.maxInt(u32) else instruction.op.capture_cell = std.math.maxInt(u32);
                    try std.testing.expectError(error.InvalidOperation, emit(test_gpa, &lowered.ir, null));
                    instruction.* = saved;
                    instruction.op = if (saved.op == .capture_value) .{ .capture_cell = saved.op.capture_value } else .{ .capture_value = saved.op.capture_cell };
                    try std.testing.expectError(error.InvalidOperation, emit(test_gpa, &lowered.ir, null));
                    checked += 1;
                },
                .closure_init => {
                    instruction.op.closure_init.captures = &.{};
                    try std.testing.expectError(error.InvalidOperation, emit(test_gpa, &lowered.ir, null));
                    checked += 1;
                },
                .local_slot => if (instruction.boxed) {
                    instruction.boxed = false;
                    try std.testing.expectError(error.InvalidOperation, emit(test_gpa, &lowered.ir, null));
                    checked += 1;
                },
                else => {},
            }
            instruction.* = saved;
        }
    }
    try std.testing.expectEqual(@as(usize, 4), checked);
    try lowered.ir.verify();
}

test "inferred function declarations preserve noreturn and nested return scopes" {
    const c = try arrayTestProgram(
        \\@[export]
        \\let exported=(n:int)->int {n+1}
        \\@[noreturn]
        \\let stop=(){
        \\ let inner=()->int {return 7}
        \\ let named:(n:int)->int={return n}
        \\ @println(named(inner()))
        \\ @panic("done")
        \\}
        \\let main=(){@println(exported(2))
        \\ stop()}
    );
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "3\n7\n", "panic: done\n", mode);
}

const generic_union_program =
    \\let Result: (T: type, E: type) -> type = union { ok: T, err: E, }
    \\let Apply = Result
    \\let Count = int
    \\let R = Apply(Count, string)
    \\let TextResult = Result(string, string)
    \\let Reversed = union { err: string, ok: bool, }
    \\let source: (success: bool) -> R = {
    \\    @print("P")
    \\    if success { .ok[7] } else { .err["er" + "ror"] }
    \\}
    \\let transform: (success: bool) -> TextResult = {
    \\    let n = source(success).!
    \\    @print("S")
    \\    return .ok["value:{n}"]
    \\}
    \\let boolean: (success: bool) -> Reversed = {
    \\    let n = source(success).!
    \\    .ok[n == 7]
    \\}
    \\let describe: (r: TextResult) -> string = match r {
    \\    .ok => |s| "ok:" + s
    \\    .err => |s| "err:" + s
    \\}
    \\let describeBool: (r: Reversed) -> string = match r {
    \\    .ok => |b| "bool:{b}"
    \\    .err => |s| "err:" + s
    \\}
    \\let main: () = {
    \\    @println(describe(transform(true)), describe(transform(false)))
    \\    @println(describeBool(boolean(true)), describeBool(boolean(false)))
    \\    let mut item = Result(string, string).ok["a" + "b"]
    \\    let copy: TextResult = item
    \\    item = .err["c" + "d"]
    \\    @println(describe(copy), describe(item))
    \\}
;
const enum_program =
    \\@[public]
    \\let Kind=enum{not_found,permission_denied,io,
    \\    @[public]
    \\    @[stringify]
    \\    let toString:(self:Kind)->string=match self{Alias.not_found=>"not_found"
    \\        Kind.permission_denied | .io=>"other"}
    \\    let eql:(a:Kind,b:Kind)->bool=a==b
    \\}
    \\let Alias=Kind
    \\let initial=Alias.not_found
    \\let Result:(T:type,E:type)->type=union{ok:T,err:E,}
    \\let Error=struct{kind:Alias=.not_found,message:string,valid:bool=true,code:uint=7,}
    \\let R=Result(uint,Alias)
    \\let S=Result(uint,Error)
    \\let make:()->R=.err[.not_found]
    \\let pass:()->R={let n=make().!
    \\    .ok[n]}
    \\let structured:()->S=.err[Error[.message="de"+"tail"]]
    \\let copy:(e:Error)->Error=e
    \\let main:()={
    \\    let mut k:Kind=.io
    \\    let saved=k
    \\    k=initial
    \\    let e=copy(Error[.message="hel"+"lo"])
    \\    @println(k,"{saved}",Kind.eql(k,.not_found),saved!=k,e.kind,e.message,e.valid,e.code)
    \\    @println(match pass(){.ok=>|n|"{n}"
    \\        .err=>|kind|"{kind}"})
    \\    @println(match structured(){.ok=>|n|"{n}"
    \\        .err=>|err|"{err.kind}:{err.message}"})
    \\}
;

const instance_program =
    \\let text:(r:R)->string="WRONG"
    \\let Alias=R
    \\let R=struct{s:string,
    \\    let next:(value:Alias,n:int,other:R)->R={@println("method",value.s,n,other.s)
    \\        R[.s=value.s+other.s]}
    \\    let text:(value:R)->string=value.s
    \\    let replace:(value:R)->R={let mut copy=value
    \\        copy=R[.s="changed"]
    \\        copy}
    \\}
    \\let K=enum{x,y,
    \\    let eql:(left:K,right:K)->bool=left==right
    \\}
    \\let U=union{record:R,empty,
    \\    let text:(value:U)->string=match value{.record=>|r|r.text()
    \\        .empty=>"empty"}
    \\}
    \\let make:(label:string)->R={@println(label)
    \\    R[.s=label+"!"]}
    \\let number:()->int={@println("arg")
    \\    7}
    \\let main:()={
    \\    let original=make("receiver")
    \\    let saved=original
    \\    @println(original.next(number(),make("right")).text(),saved.text(),original.replace().text(),original.text())
    \\    @println(make("nested").next(number(),make("last")).text())
    \\    let k=K.x
    \\    @println(k.eql(.x),K.y.eql(k))
    \\    let u=U.record[make("payload")]
    \\    @println(u.text(),match u{.record=>|record|record.next(number(),make("capture")).text()
    \\        .empty=>"bad"})
    \\}
;
const instance_output = "receiver\narg\nright\nmethod receiver! 7 right!\nreceiver!right! receiver! changed receiver!\nnested\narg\nlast\nmethod nested! 7 last!\nnested!last!\ntrue false\npayload\narg\ncapture\nmethod payload! 7 capture!\npayload! payload!capture!\n";

test "instance calls preserve once only receiver argument order copies captures and forced GC" {
    const c = try arrayTestProgram(instance_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, instance_output, "", mode);
}

test "instance calls skip later effects on receiver return panic propagation and terminal args" {
    const prelude =
        \\let R=struct{s:string="ok",
        \\    let use:(r:R,n:int)->string=r.s
        \\    let skip:(r:R,n:int,m:int)->string=r.s
        \\    @[noreturn]
        \\    let stop:(r:R)=@panic(r.s+"!")
        \\}
        \\@[noreturn]
        \\let relay:(r:R)=r.stop()
        \\let Result:(T:type,E:type)->type=union{ok:T,err:E,}
        \\let RR=Result(R,string)
        \\let RS=Result(string,string)
        \\let failed:()->RR=.err["error"]
        \\let arg:()->int={@print("BAD")
        \\    1}
        \\let receiver:()->R={@print("receiver")
        \\    R[]}
        \\let propagated:()->RS=.ok[failed().!.use(arg())]
        \\let pass:(r:R)->R=r
        \\let returned:()->string=pass({return "returned"}).use(arg())
        \\let argument:()->string=receiver().use({return "argument"})
        \\let main:()={
        \\    @println(returned())
        \\    @println(argument())
        \\    @println(match propagated(){.ok=>|s|s
        \\        .err=>|e|e})
    ;
    const Case = struct { tail: []const u8, output: []const u8, message: []const u8 };
    for ([_]Case{
        .{ .tail = "R[.s=\"st\"+\"op\"].stop()\n}", .output = "returned\nreceiverargument\nerror\n", .message = "panic: stop!\n" },
        .{ .tail = "pass({@print(\"panic\")\n@panic(\"receiver\")}).use(arg())\n}", .output = "returned\nreceiverargument\nerror\npanic", .message = "panic: receiver\n" },
        .{ .tail = "receiver().skip(@panic(\"argument\"),arg())\n}", .output = "returned\nreceiverargument\nerror\nreceiver", .message = "panic: argument\n" },
    }) |case| {
        const source = try std.mem.concat(test_gpa, u8, &.{ prelude, "\n", case.tail });
        defer test_gpa.free(source);
        errdefer std.debug.print("terminal instance source: {s}\n", .{source});
        var module = try Module.init(test_gpa, "terminal-instance.nov", source);
        defer module.deinit();
        if (module.hasErrors()) {
            var output = std.Io.Writer.Allocating.init(test_gpa);
            defer output.deinit();
            try Diagnostics.renderModule(&module, &output.writer);
            std.debug.print("{s}", .{output.written()});
        }
        const c = try arrayTestProgram(source);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, case.output, case.message, mode);
    }
}

const generic_instance_program =
    \\let Box:(T:type)->type=struct{value:T,
    \\let map:(object:Box(T),convert:(T)->`U)->Box(`U)=Box(`U)[.value=convert(object.value)]
    \\let keep:(object:Box(T),change:()->`U)->T={let ignored=change()
    \\object.value}
    \\}
    \\let Plain=struct{
    \\let apply:(object:Plain,x:`T)->`T=x
    \\let produce:(object:Plain,f:()->`T)->`T=f()
    \\}
    \\let identity:(x:`T)->`T=x
    \\let make:()->`T=7
    \\let main:()={let mut r=Box([][]string)[.value=[["old"+"!"]]]
    \\let mut count:int=0
    \\let receiver=()->Box([][]string){count+=1
    \\@println("receiver",count)
    \\r}
    \\let change=()->bool {@println("argument")
    \\r.value[0]=["new"+"!"]
    \\true}
    \\let read=()->string {r.value[0][0]}
    \\let saved=receiver().keep(change)
    \\@println(saved[0][0],read(),count)
    \\let mapped=Box(bool)[.value=true].map(identity)
    \\let static=Box(bool).map(mapped,identity)
    \\@println(mapped.value,static.value)
    \\let plain=Plain[]
    \\let n:uint=plain.produce(make)
    \\@println(plain.apply(true),n)
    \\}
;

test "backtick instance generated C receiver order recursive snapshots shared closure cells contextual callbacks source free OOM forced GC" {
    const c = try arrayTestProgram(generic_instance_program);
    defer test_gpa.free(c);
    try testClosureOwnership(test_gpa, generic_instance_program);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testClosureOwnership, .{generic_instance_program});
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "receiver 1\nargument\nold! new! 1\ntrue true\ntrue 7\n", "", mode);
}

test "backtick instance terminal operands generated C both branches return before unreachable constraints or specialization" {
    const source =
        \\let R=struct{
        \\let f:(object:R,a:`T,b:`T,c:`T)->`T=missing
        \\let first:(object:R,x:`T,y:`T)->`T=missing
        \\}
        \\let f:(a:`T,b:`T,c:`T)->`T=missing
        \\let receiver:()->R={@print("receiver ")
        \\R[]}
        \\let prefix:()->bool={@print("prefix ")
        \\true}
        \\let choose:(flag:bool)->uint=receiver().f(prefix(),if flag{return 1}else{return 2},unknown)
        \\let first:(flag:bool)->uint=receiver().first(if flag{return 3}else{return 4},true)
        \\let static:(flag:bool)->uint=R.f(receiver(),prefix(),if flag{return 5}else{return 6},"wrong")
        \\let direct:(flag:bool)->uint=f(prefix(),if flag{return 7}else{return 8},unknown)
        \\let main:()={@println(choose(true),choose(false))
        \\@println(first(true),first(false))
        \\@println(static(true),static(false))
        \\@println(direct(true),direct(false))}
    ;
    const c = try arrayTestProgram(source);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "receiver prefix receiver prefix 1 2\nreceiver receiver 3 4\nreceiver prefix receiver prefix 5 6\nprefix prefix 7 8\n", "", mode);
}

const generic_flow_program =
    \\let W=struct{let f:(self:W,a:`T,b:`T,c:`T)->`T=missing
    \\}
    \\let C=struct{let f:(self:C,a:bool,b:bool,c:bool)->bool=a
    \\}
    \\let Normal=struct{let f:(self:Normal,a:`T,b:`T,c:`T)->`T=a
    \\}
    \\let matched:(flag:bool)->uint=W[].f(true,match flag{true=>{return 1}
    \\false=>{return 2}},unknown)
    \\let exhaustion:()->uint=W[].f(true,loop false{}else{return 3},unknown)
    \\let concrete:(flag:bool)->uint=C[].f(true,match flag{true=>{return 4}
    \\false=>{return 5}},unknown)
    \\let parent:()->bool=Normal[].f(true,loop{loop false{}else{break true}},true)
    \\let main:()={let a=matched(true)
    \\let b=matched(false)
    \\let c=exhaustion()
    \\let saved=()->uint{a+b+c}
    \\@println(a,b,c,saved(),concrete(true),concrete(false),parent())}
;

test "backtick instance abstract control flow matches exhaustion and parent loop scopes source free OOM forced GC" {
    try testClosureOwnership(test_gpa, generic_flow_program);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testClosureOwnership, .{generic_flow_program});
    const c = try arrayTestProgram(generic_flow_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "1 2 3 6 4 5 true\n", "", mode);
}

test "backtick instance factory signatures preserve terminal execution source free OOM forced GC" {
    const prelude =
        \\let R=struct{
        \\let f:(object:R,a:`T,b:`T,c:`T)->`T=missing
        \\@[noreturn]
        \\let stop:(object:R)=@panic("factory")
        \\}
        \\let make:()->R={@print("made ")
        \\R[]}
        \\let Factory=struct{let make:()->R={@print("made ")
        \\R[]}
        \\}
        \\let main:()={let label="owned"
        \\let keep=()->string{label}
        \\R[].f(true,
    ;
    for ([_][]const u8{ "make().stop()", "{let r=make()\nr.stop()}", "Factory.make().stop()" }) |argument| {
        const source = try std.mem.concat(test_gpa, u8, &.{ prelude, argument, ",unknown)}" });
        defer test_gpa.free(source);
        try testClosureOwnership(test_gpa, source);
        try TestAllocations.checkAllAllocationFailures(test_gpa, testClosureOwnership, .{source});
        const c = try arrayTestProgram(source);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "made ", "panic: factory\n", mode);
    }
}

test "backtick instance generated C terminal argument skips later effects and noreturn methods terminate" {
    const prelude =
        \\let R=struct{
        \\let skip:(object:R,x:`T,y:`T)->`T=x
        \\@[noreturn]
        \\let stop:(object:R,x:`T)=@panic("stop")
        \\}
        \\let later:()->bool={@println("BAD")
        \\true}
    ;
    for ([_]struct { tail: []const u8, message: []const u8 }{
        .{ .tail = "let main:()={R[].skip(@panic(\"argument\"),later())\n}", .message = "panic: argument\n" },
        .{ .tail = "@[noreturn]\nlet main:()=R[].stop(true)", .message = "panic: stop\n" },
        .{ .tail = "@[noreturn]\nlet main:()=R[].skip(if true{@panic(\"branch\")}else{@panic(\"other\")},unknown)", .message = "panic: branch\n" },
        .{ .tail = "@[noreturn]\nlet main:()=R[].skip({let r=R[]\nr.stop(true)},unknown)", .message = "panic: stop\n" },
    }) |case| {
        const source = try std.mem.concat(test_gpa, u8, &.{ prelude, "\n", case.tail });
        defer test_gpa.free(source);
        const c = try arrayTestProgram(source);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "", case.message, mode);
    }
}

test "instance receiver snapshots survive later binding replacement" {
    const source =
        \\let R=struct{s:string,
        \\    let join:(r:R,other:R)->string=r.s+other.s
        \\}
        \\let main:()={let mut r=R[.s="old"+"!"]
        \\    @println(r.join({r=R[.s="new"+"!"]
        \\        r}),r.s)}
    ;
    const c = try arrayTestProgram(source);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "old!new! new!\n", "", mode);
}

test "instance calls runtime allocation failures occur at receiver args and method in order" {
    for ([_][]const u8{
        "let main:()=@println(R[.s=\"a\"+\"b\"].join(arg()))",
        "let main:()=@println(R[.s=\"ab\"].join(\"c\"+\"d\"))",
        "let main:()=@println(R[.s=\"ab\"].join(\"cd\"))",
    }) |body| {
        const source = try std.mem.concat(test_gpa, u8, &.{
            "let R=struct{s:string,\nlet join:(r:R,s:string)->string=r.s+s\n}\nlet arg:()->string={@print(\"BAD\")\n\"cd\"}\n", body,
        });
        defer test_gpa.free(source);
        const program = try arrayTestProgram(source);
        defer test_gpa.free(program);
        const exhausted = try std.mem.concat(test_gpa, u8, &.{ "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", program });
        defer test_gpa.free(exhausted);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", 1, "", "panic: GC allocation failed\n", mode);
    }
}

test "instance calls source free ownership and every allocation failure" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "methods.nov", instance_program);
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
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, instance_output, "");
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{instance_program});
}

test "enum values methods defaults records generic payloads propagation and forced GC" {
    var module = try Module.init(test_gpa, "enums.nov", enum_program);
    defer module.deinit();
    if (module.hasErrors()) {
        var output = std.Io.Writer.Allocating.init(test_gpa);
        defer output.deinit();
        try Diagnostics.renderModule(&module, &output.writer);
        std.debug.print("{s}", .{output.written()});
    }
    const c = try arrayTestProgram(enum_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "not_found other true true not_found hello true 7\nnot_found\nnot_found:detail\n", "", mode);
}

test "qualified enum match patterns and mixed alternatives execute with exact alias identity" {
    const c = try emitSource(
        "let K=enum{x,y,z,}\nlet Alias=K\n" ++
            "let qualified:(k:K)->bool=match k{K.x=>true\nAlias.y=>false\nK.z=>true}\n" ++
            "let mixed:(k:Alias)->bool=match k{Alias.x | .y=>true\nK.z=>false}\n" ++
            "let main:()=@println(qualified(.x),qualified(.y),qualified(.z),mixed(.x),mixed(.y),mixed(.z))",
        null,
    );
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "void nov_main(void); int main(void) { nov_main(); return 0; }", 0, "true false true true true false\n", "", mode);
}

test "enum source-free emission and every compiler allocation failure" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "enum-owned.nov", enum_program);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        break :blk result;
    };
    defer sema.deinit();
    const c = try emitProgram(test_gpa, &sema.ir, null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, "not_found other true true not_found hello true 7\nnot_found\nnot_found:detail\n", "");
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{enum_program});
}

test "root enum scalars need no GC owners or integer operations" {
    const library = try emitSource("let K=enum{x,y,}\nlet choose:(k:K)->K=match k{.x=>.y\n.y=>.x}\nlet main:()={let a=K.x\nlet b=choose(a)\n@println(b==.y)}", null);
    defer test_gpa.free(library);
    try std.testing.expect(std.mem.indexOf(u8, library, "nov_gc_") == null);
    try std.testing.expect(std.mem.indexOf(u8, library, "nov_record_") == null);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(library, "void nov_main(void); int main(void) { nov_main(); return 0; }", 0, "true\n", "", mode);
}

test "transitive enum record leaves are reachable through placeholders in every field order" {
    for ([_][]const u8{
        "k:K,b:bool,n:uint,s:string,",
        "s:string,k:K,b:bool,n:uint,",
        "n:uint,s:string,b:bool,k:K,",
        "b:bool,n:uint,k:K,s:string,",
    }) |fields| {
        const source = try std.fmt.allocPrint(test_gpa, "let U=union{{ok:R,}}\nlet R=struct{{{s}}}\nlet K=enum{{x,y,}}\nlet unused:(cb:(U)->void)={{}}\nlet main:()={{}}", .{fields});
        defer test_gpa.free(source);
        const c = try emitSource(source, null);
        defer test_gpa.free(c);
        try std.testing.expect(std.mem.indexOf(u8, c, "nov_string;") != null);
        try std.testing.expect(std.mem.indexOf(u8, c, "nov_gc_") == null);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "void nov_main(void); int main(void) { nov_main(); return 0; }", 0, "", "", mode);
    }
    for ([_][]const u8{ "i128", "u128" }) |width| {
        const source = try std.fmt.allocPrint(test_gpa, "let U=union{{ok:R,}}\nlet R=struct{{k:K,b:bool,s:string,n:{s},}}\nlet K=enum{{x,}}\nlet unused:(cb:(U)->void)={{}}\nlet main:()={{}}", .{width});
        defer test_gpa.free(source);
        var module = try Module.init(test_gpa, "enum-width.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        var diagnostic: Diagnostic = undefined;
        try std.testing.expectError(error.UnsupportedIntegerWidth, emit(test_gpa, &sema.ir, &diagnostic));
        try std.testing.expectEqualStrings("n", module.sourceSlice(.{ .start = diagnostic.span.start, .end = diagnostic.span.end }).?);
    }
}

const record_payload_program =
    \\let Result:(T:type,E:type)->type=union{ok:T,err:E,}
    \\let Apply=Result
    \\let U=union{ok:File,err:Error,none,}
    \\let File=struct{name:string,extra:string,n:int,valid:bool=true,}
    \\let Error=struct{message:string,detail:string,code:i8,}
    \\let Alias=File
    \\let Input=Apply(Alias,Error)
    \\let Output=Result(bool,Error)
    \\let text:(s:string)->string={@print(s)
    \\    s+"!"}
    \\let make:(good:bool)->Input={@print("M")
    \\    if good {.ok[Alias[.extra=text("b"),.name=text("a"),.n=7]]} else {.err[Error[.message="er"+"ror",.detail="de"+"tail",.code=-8]]}}
    \\let use:(r:Alias,later:string)->string="{r.name}:{r.extra}:{r.n}:{r.valid}:"+later
    \\let run:(good:bool)->Output={let file=make(good).!
    \\    @println(use(file,"x"+"y"))
    \\    .ok[file.valid]}
    \\let show:(r:Output)->string=match r{.ok=>|b|"{b}"
    \\    .err=>|e|"{e.message}:{e.detail}:{e.code}"}
    \\let copy:(u:U)->U=u
    \\let main:()={@println(show(run(true)),show(run(false)))
    \\    let mut u=U.ok[File[.name="c"+"d",.extra="e"+"f",.n=9]]
    \\    let saved=copy(u)
    \\    loop i in 0..4 {u=if i == 0 {saved} else if i == 1 {.err[Error[.message="g"+"h",.detail="i"+"j",.code=2]]} else {.none[]}
    \\        let value:U=loop{break u}
    \\        @println(match value{.ok=>|f|use(f,"k"+"l")
    \\            .err=>|e|e.message+e.detail
    \\            .none=>"none"})}
    \\    @println(match saved{.ok=>|f|use(f,"m"+"n")
    \\        .err=>@panic("wrong error")
    \\        .none=>@panic("wrong none")})}
;
const record_payload_output = "Mbaa!:b!:7:true:xy\nMtrue error:detail:-8\ncd:ef:9:true:kl\nghij\nnone\nnone\ncd:ef:9:true:mn\n";

test "record payloads preserve layout order copies effects propagation captures and forced GC" {
    const c = try arrayTestProgram(record_payload_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, record_payload_output, "", mode);
}

test "record payload inactive storage stays zero when loop temporaries change variants" {
    const library = try emitSource(
        "let U=union{ok:R,err:R,none,}\nlet R=struct{s:string,t:string,n:int,}\n" ++
            "let make:(which:uint)->U={let mut result:U=.none[]\nloop i in 0..which {result=if i == 0 {.ok[R[.s=\"a\"+\"b\",.t=\"c\"+\"d\",.n=1]]} else if i == 1 {.err[R[.s=\"e\"+\"f\",.t=\"g\"+\"h\",.n=2]]} else {.none[]}}\nresult}\nlet copy:(u:U)->U=u",
        null,
    );
    defer test_gpa.free(library);
    const c = try std.mem.concat(test_gpa, u8, &.{ library, "\nstatic int empty(nov_record_1 r) { return !r.f0.owner && !r.f0.data && !r.f0.len && !r.f1.owner && !r.f1.data && !r.f1.len && !r.f2; }\n" ++
        "int main(void) { nov_rt_heap.collect_every_allocation = 1; " ++
        "for (uintptr_t i=0; i<4; ++i) { nov_record_0 a=nov_make(i), b=nov_copy(a); " ++
        "if (a.tag != (i==1 ? 0u : i==2 ? 1u : 2u)) return 1; " ++
        "if ((a.tag != 0 && !empty(a.f0)) || (a.tag != 1 && !empty(a.f1))) return 2; " ++
        "if (a.tag == 0 && (a.f0.f0.owner != b.f0.f0.owner || a.f0.f1.owner != b.f0.f1.owner || a.f0.f0.data != b.f0.f0.data)) return 3; " ++
        "if (a.tag == 1 && (a.f1.f0.owner != b.f1.f0.owner || a.f1.f1.owner != b.f1.f1.owner)) return 4; " ++
        "if (nov_rt_heap.frames) return 5; nov_gc_collect(&nov_rt_heap); if (nov_rt_heap.live_objects) return 6; " ++
        "} nov_gc_shutdown(&nov_rt_heap); return 0; }\n" });
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "", "", mode);
}

test "record payload width checking follows reverse dependencies and reports leaf source" {
    for ([_][]const u8{ "i128", "u128" }) |width| {
        for ([_]bool{ false, true }) |generic| {
            const source = try std.fmt.allocPrint(test_gpa, "{s}\nlet R=struct{{wide:{s},s:string,}}\nlet f:(cb:(U)->void)={{}}\nlet main:()={{}}", .{ if (generic) "let G:(T:type)->type=union{ok:T,}\nlet U=G(R)" else "let U=union{ok:R,}", width });
            defer test_gpa.free(source);
            var module = try Module.init(test_gpa, "record-width.nov", source);
            defer module.deinit();
            var sema = try Lowering.lower(&module);
            defer sema.deinit();
            try std.testing.expect(!module.hasErrors());
            var diagnostic: Diagnostic = undefined;
            try std.testing.expectError(error.UnsupportedIntegerWidth, emit(test_gpa, &sema.ir, &diagnostic));
            try std.testing.expectEqualStrings("wide", module.sourceSlice(.{ .start = diagnostic.span.start, .end = diagnostic.span.end }).?);
        }
        const unused = try std.fmt.allocPrint(test_gpa, "let U=union{{ok:R,}}\nlet R=struct{{wide:{s},}}\nlet main:()={{}}", .{width});
        defer test_gpa.free(unused);
        const c = try emitSource(unused, null);
        defer test_gpa.free(c);
        try std.testing.expect(std.mem.indexOf(u8, c, "nov_record_") == null);
    }
}

const option_propagation_program =
    \\let O:(T:type)->type=union{none,some:T,}
    \\let Text=union{some:string,none,}
    \\let Item=struct{s:string,}
    \\let Items=union{some:Item,none,}
    \\let make:(yes:bool)->O(int)={@print("M")
    \\if yes{.some[7]}else{.none[]}}
    \\let later:()->int={@print("BAD")
    \\1}
    \\let use:(a:int,b:int)->int=a+b
    \\let rhs:()->Text=.some["{make(false).!+later()}"]
    \\let args:()->Text=.some["{use(make(false).!,later())}"]
    \\let inLoop:()->Text={loop i in 0..3{let n=make(false).!
    \\@println(n,later())}
    \\.some["BAD"]}
    \\let returned:()->Text={return .some["{make(true).!}"]}
    \\let nested:()->Text=.some[returned().!+"!"]
    \\let unit:(o:O(void))->Text={o.!
    \\.some["unit"]}
    \\let records:(o:O(Item))->Items={let mut item=o.!
    \\let copy=item
    \\item=Item[.s="new"+"!"]
    \\.some[Item[.s=copy.s+item.s]]}
    \\let show:(o:Text)=@println(match o{.some=>|s|s
    \\.none=>"none"})
    \\let main:()={show(rhs())
    \\show(args())
    \\show(inLoop())
    \\show(nested())
    \\show(unit(O(void).some[]))
    \\show(unit(O(void).none[]))
    \\let item=records(O(Item).some[Item[.s="old"+"!"]])
    \\let noise="collect"+"!"
    \\@println(match item{.some=>|r|r.s
    \\.none=>"BAD"},noise)
    \\@println(match records(O(Item).none[]){.some=>"BAD"
    \\.none=>"none"})}
;
const option_propagation_output = "Mnone\nMnone\nMnone\nM7!\nunit\nnone\nold!new! collect!\nnone\n";

test "Option propagation once only early returns calls loops records void and forced GC" {
    const c = try arrayTestProgram(option_propagation_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, option_propagation_output, "", mode);
}

test "Option propagation source free IR and every compiler allocation failure" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "option-owned.nov", option_propagation_program);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        break :blk result;
    };
    defer sema.deinit();
    try sema.ir.verify();
    const library = try emit(test_gpa, &sema.ir, null);
    defer test_gpa.free(library);
    const c = try collectingTestProgram(library);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, option_propagation_output, "", mode);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{option_propagation_program});
}

test "Option propagation none skips allocations and some reports runtime OOM" {
    for ([_]bool{ false, true }) |allocate| {
        const source = try std.fmt.allocPrint(test_gpa, "let O:(T:type)->type=union{{some:T,none,}}\nlet make:()->O(string)={s}\nlet run:()->O(string)=.some[make().!+\"!\"]\nlet main:()=@println(match run(){{.some=>|s|s\n.none=>\"none\"}})", .{if (allocate) ".some[\"a\"+\"b\"]" else ".none[]"});
        defer test_gpa.free(source);
        const program = try arrayTestProgram(source);
        defer test_gpa.free(program);
        const exhausted = try std.mem.concat(test_gpa, u8, &.{ "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", program });
        defer test_gpa.free(exhausted);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", if (allocate) 1 else 0, if (allocate) "" else "none\n", if (allocate) "panic: GC allocation failed\n" else "", mode);
    }
}

test "record payload propagation allocates only evaluated strings and handles OOM" {
    for ([_]bool{ false, true }) |allocate| {
        const source = try std.fmt.allocPrint(test_gpa, "let G:(T:type,E:type)->type=union{{ok:T,err:E,}}\nlet R=struct{{s:string,}}\nlet make:()->G(int,R)=.err[R[.s={s}]]\nlet run:()->G(bool,R)=.ok[make().! == 0]\nlet main:()=@println(match run(){{.ok=>@panic(\"bad\")\n.err=>|r|r.s}})", .{if (allocate) "\"e\"+\"rr\"" else "\"err\""});
        defer test_gpa.free(source);
        const library = try emitSource(source, null);
        defer test_gpa.free(library);
        const program = try collectingTestProgram(library);
        defer test_gpa.free(program);
        const exhausted = try std.mem.concat(test_gpa, u8, &.{ "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", program });
        defer test_gpa.free(exhausted);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", if (allocate) 1 else 0, if (allocate) "" else "err\n", if (allocate) "panic: GC allocation failed\n" else "", mode);
    }
}

test "record payload captures stay rooted through terminal panic and skip later effects" {
    const source =
        \\let G:(T:type,E:type)->type=union{ok:T,err:E,}
        \\let R=struct{s:string,t:string,}
        \\let fail:()->G(R,R)={@print("F")
        \\    return .err[R[.s="pa"+"nic",.t="mes"+"sage"]]}
        \\let later:()->string={@print("BAD")
        \\    "later"}
        \\let use:(r:R,s:string)->bool=r.s == s
        \\let run:()->G(bool,R)=.ok[use(fail().!,later())]
        \\let suffix:()->string="x"+"y"
        \\let main:()=match run(){.ok=>@panic("bad success")
        \\    .err=>|r|@panic("{r.s}:{r.t}:"+suffix())}
    ;

    const c = try arrayTestProgram(source);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "F", "panic: panic:message:xy\n", mode);
}

test "record payload IR owns source and emission survives every allocation failure" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "record-payload.nov", record_payload_program);
        defer module.deinit();

        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        break :blk result;
    };
    defer sema.deinit();
    try sema.ir.verify();
    const library = try emit(test_gpa, &sema.ir, null);
    defer test_gpa.free(library);
    const c = try collectingTestProgram(library);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, record_payload_output, "");
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{"let G:(T:type)->type=union{ok:T,}\nlet R=struct{s:string,}\nlet main:()={let u=G(R).ok[R[.s=\"a\"+\"b\"]]\n@println(match u{.ok=>|r|r.s})}"});
}

test "printing uses annotated format but ignores ordinary toString" {
    const source =
        \\let Custom=struct{value:int,
        \\@[stringify]
        \\let format:(self:Custom)->string="custom:{self.value}"
        \\let toString:(self:Custom)->string="ordinary custom"
        \\}
        \\let Plain=struct{value:int,
        \\let toString:(self:Plain)->string="ordinary plain"
        \\}
        \\let main:()={let custom=Custom[.value=7]
        \\let plain=Plain[.value=8]
        \\@println(custom,plain)
        \\@println("{custom} {plain}")
        \\@println([custom],[plain])
        \\@println(custom.format(),custom.toString(),plain.toString())}
    ;
    const c = try arrayTestProgram(source);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "custom:7 Plain[ value: 8 ]\n" ++
        "custom:7 Plain[ value: 8 ]\n" ++
        "[custom:7] [Plain[ value: 8 ]]\n" ++
        "custom:7 ordinary custom ordinary plain\n", "", mode);
}

const record_interpolation_program =
    \\let Alias=R
    \\let Text=string
    \\let Leaf=struct{s:string,
    \\    @[stringify]
    \\    let toString:(l:Leaf)->string={@print("L")
    \\        "leaf:"+l.s}
    \\}
    \\let R=struct{s:string,t:string,
    \\    @[stringify]
    \\    let toString:(r:Alias)->Text={@print("R")
    \\        let leaf=Leaf[.s=r.s+"!"]
    \\        "{leaf}/{r.t}"}
    \\}
    \\let make:()->Alias={@print("M")
    \\    R[.s="a"+"b",.t="c"+"d"]}
    \\let scalar:()->bool={@print("S")
    \\    true}
    \\let U=union{item:string,none,
    \\    @[stringify]
    \\    let toString:(u:U)->string={@print("U")
    \\        match u{.item=>|s|"union:"+s
    \\            .none=>"none"}}
    \\}
    \\let UnionAlias=U
    \\let main:()={let r=make()
    \\    @println("<{r}>|{r}|{make()}|{scalar()}")
    \\    let u=UnionAlias.item["x"+"y"]
    \\    @println("{u}:{@as(i8, -8)}:{false}")
    \\    @println(r)}
;
const record_interpolation_output = "MRLRLMRLS<leaf:ab!/cd>|leaf:ab!/cd|leaf:ab!/cd|true\nUunion:xy:-8:false\nRLleaf:ab!/cd\n";

test "record interpolation reuses print conversions with exact aliases nested allocations and ordered effects" {
    const c = try arrayTestProgram(record_interpolation_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, record_interpolation_output, "", mode);
}

test "record interpolation captures errors for panic and keeps nested payload owners rooted" {
    const source =
        \\let G:(T:type,E:type)->type=union{ok:T,err:E,}
        \\let Error=struct{message:string,detail:string,
        \\    @[stringify]
        \\    let toString:(err:Error)->string={@print("C")
        \\        "{err.message}: {err.detail}"}
        \\}
        \\let fail:()->G(int,Error)=.err[Error[.message="not "+"found",.detail="fi"+"le"]]
        \\let main:()=match fail(){.ok=>@panic("unexpected")
        \\    .err=>|err|@panic("Error: {err}")}
    ;
    const c = try arrayTestProgram(source);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "C", "panic: Error: not found: file\n", mode);
}

test "record interpolation skips converters and later placeholders on propagated or terminal paths" {
    const source =
        \\let G:(T:type,E:type)->type=union{ok:T,err:E,}
        \\let R=struct{s:string,
        \\    @[stringify]
        \\    let toString:(r:R)->string={@print("BAD-CONVERT")
        \\        r.s}
        \\}
        \\let fail:()->G(R,string)={@print("F")
        \\    .err["e"+"rr"]}
        \\let later:()->string={@print("BAD-LATER")
        \\    "later"}
        \\let run:()->G(string,string)=.ok["prefix:{fail().!}:{later()}"]
        \\@[noreturn]
        \\let stop:()=@panic("stop")
        \\let terminal:()->string="{stop()}{missing}"
        \\let main:()={@println(match run(){.ok=>|s|s
        \\    .err=>|s|s})
        \\    @print(terminal())}
    ;
    const c = try arrayTestProgram(source);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "Ferr\n", "panic: stop\n", mode);
}

test "record interpolation survives source release and every compiler allocation failure" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "record-interpolation.nov", record_interpolation_program);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        break :blk result;
    };
    defer sema.deinit();
    try sema.ir.verify();
    const library = try emit(test_gpa, &sema.ir, null);
    defer test_gpa.free(library);
    const c = try collectingTestProgram(library);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, record_interpolation_output, "");
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{record_interpolation_program});
}

test "record interpolation conversion allocations fail safely and skipped conversions allocate nothing" {
    for ([_]bool{ false, true }) |convert| {
        const source = try std.fmt.allocPrint(test_gpa, "let R=struct{{s:string,\n@[stringify]\nlet toString:(r:R)->string=r.s+\"!\"\n}}\nlet main:()={{let r=R[.s=\"ok\"]\n@println(if {s} {{\"{{r}}\"}} else {{\"skip\"}})}}", .{if (convert) "true" else "false"});
        defer test_gpa.free(source);
        const library = try emitSource(source, null);
        defer test_gpa.free(library);
        const program = try collectingTestProgram(library);
        defer test_gpa.free(program);
        const exhausted = try std.mem.concat(test_gpa, u8, &.{ "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", program });
        defer test_gpa.free(exhausted);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", if (convert) 1 else 0, if (convert) "" else "skip\n", if (convert) "panic: GC allocation failed\n" else "", mode);
    }
}

const generic_struct_program =
    \\let Box:(T:type)->type=struct{value:T,
    \\let Self=@This()
    \\let get:(self:Self)->T=self.value
    \\let forward:(self:Self)->T=sibling(self)
    \\let sibling:(self:Self)->T=self.get()
    \\let make:(value:T)->Self=Self[.value=value]
    \\let wrap:(self:Self)->Box(Self)=Box(Self)[.value=self]
    \\@[stringify]
    \\let toString:(self:Self)->string="<{self.value}>"
    \\let unused:()->T=unknown
    \\}
    \\let Pair:(A:type,B:type)->type=struct{first:A,second:B,}
    \\let Default:(T:type)->type=struct{value:T=7,}
    \\let Kind=enum{first,second,}
    \\let Flags:(T:type)->type=struct{value:T=.second,enabled:bool=true,text:string="default",}
    \\let R:(T:type,E:type)->type=union{ok:T,err:E,}
    \\let O:(T:type)->type=union{some:T,none,}
    \\let Template=Box
    \\let IntBox=Template(int)
    \\let Alias=IntBox
    \\let Nested=struct{box:Box(Box(string)),}
    \\let option:(x:O(Box(string)))->O(Box(int))={let value=x.!
    \\.some[Box(int)[.value=4]]}
    \\let result:(x:R(Box(string),Box(string)))->R(Box(int),Box(string))={let value=x.!
    \\.ok[Box(int)[.value=5]]}
    \\let main:()={
    \\let mut a=Box(int)[.value=1]
    \\let saved:Alias=a
    \\a.value+=2
    \\let wrapped=a.wrap()
    \\let mut nested=Nested[.box=Box(Box(string))[.value=Box(string)[.value="a"+"b"]]]
    \\let original=nested
    \\nested.box.value.value+="!"
    \\nested.box.value=Box(string).make("new"+"!")
    \\let pair=Pair(string,int)[.second=wrapped.value.forward(),.first=nested.box.value.get()]
    \\let reverse=Pair(int,string)[.first=pair.second,.second=pair.first]
    \\let flags=Flags(Kind)[]
    \\@println(saved.value,a.get(),pair.first,reverse.first,original.box,Default(int)[].value,flags.value==Kind.second,flags.enabled,flags.text)
    \\let mut arr:[]Box(string)=[Box(string)[.value="x"+"y"],nested.box.value]
    \\let copy=arr
    \\arr[0]=Box(string)[.value="z"+"!"]
    \\arr += [Box(string)[.value="tail"+"!"]]
    \\let ok:O(Box(string))=.some[copy[0]]
    \\let err:R(Box(string),Box(string))=.err[copy[1]]
    \\@println(copy[0],arr[0],arr[-1],match option(ok){.some=>|v|v.value
    \\.none=>0},match result(err){.ok=>|v|"BAD"
    \\.err=>|v|v.value})
    \\}
;
const generic_struct_output = "1 3 new! 3 <<ab>> 7 true true default\n<xy> <z!> <tail!> 4 new!\n";

test "generic structs specialize nested constructors methods defaults arrays and propagation under forced GC" {
    const c = try arrayTestProgram(generic_struct_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, generic_struct_output, "", mode);
}

test "generic structs own source free IR and survive every allocation failure" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "generic-struct.nov", generic_struct_program);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(result.lowering_complete);
        break :blk result;
    };
    defer sema.deinit();
    try sema.ir.verify();
    const c = try emitProgram(test_gpa, &sema.ir, null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, generic_struct_output, "");
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{generic_struct_program});
}

fn testImportedGenericStructs(allocator: std.mem.Allocator) ![]u8 {
    var sema = blk: {
        var comp = Compilation.init(allocator);
        defer comp.deinit();
        const root = try comp.addSource("/app/main.nov",
            \\let api=@import("api.nov")
            \\let library=@import("box.nov")
            \\let Box=api.Box
            \\let B=Box(int)
            \\let Alias=B
            \\let Kind=enum{wrong,}
            \\let prefix="WRONG"
            \\let helper:()->string="WRONG"
            \\let main:()={let value:int=3
            \\let x:Alias=library.Box(int)[.value]
            \\let y:api.Box(int)=B.make(x.value)
            \\let z=Box(string).make("a"+"b")
            \\@println(x,y,z,z.message(),x.kind==library.Kind.good)
            \\}
        );
        const api = try comp.addSource("/lib/api.nov",
            \\let implementation=@import("box.nov")
            \\@[public]
            \\let Box=implementation.Box
        );
        const library = try comp.addSource("/lib/box.nov",
            \\let prefix="library:"
            \\let helper:()->string="helper"
            \\@[public]
            \\let Kind=enum{good,bad,}
            \\@[public]
            \\let Box:(T:type)->type=struct{value:T,kind:Kind=.good,
            \\let Self=@This()
            \\@[public]
            \\let make:(value:T)->Self=Self[.value]
            \\@[public]
            \\@[stringify]
            \\let toString:(self:Self)->string="<{self.value}>"
            \\@[public]
            \\let message:(self:Self)->string=prefix+helper()
            \\}
        );
        for ([_]struct { source: Source.Id, name: []const u8, target: Source.Id }{
            .{ .source = root, .name = "api", .target = api },
            .{ .source = root, .name = "library", .target = library },
            .{ .source = api, .name = "implementation", .target = library },
        }) |edge| {
            const module = comp.getModule(edge.source).?;
            const local = module.lookup(edge.name).?;
            try comp.imports.append(allocator, .{ .declaration = comp.declaration_ids.get(.{ .source = edge.source, .local = local }).?, .target = edge.target, .named = false, .span = module.getDeclaration(local).?.name_span });
        }
        comp.setRoot(root);
        var result = try Lowering.lowerCompilation(&comp);
        errdefer result.deinit();
        if (comp.hasErrors()) for (comp.modules.items) |module| std.debug.print("imported struct {s}: {any}\n", .{ module.source.path, module.diagnostics.items });
        try std.testing.expect(!comp.hasErrors());
        try std.testing.expect(result.lowering_complete);
        var instances: usize = 0;
        for (result.ir.types.items) |ty| if (ty == .record and ty.record.type_args.len != 0) {
            try std.testing.expectEqual(library, ty.record.source);
            instances += 1;
        };
        try std.testing.expectEqual(@as(usize, 2), instances);
        break :blk result;
    };
    defer sema.deinit();
    try sema.ir.verify();
    return emit(allocator, &sema.ir, null);
}

fn testImportedGenericStructAllocations(allocator: std.mem.Allocator) !void {
    allocator.free(try testImportedGenericStructs(allocator));
}

test "constructor shorthand imported generic aliases retain canonical identity and defining scope after graph release" {
    const library = try testImportedGenericStructs(test_gpa);
    defer test_gpa.free(library);
    const c = try collectingTestProgram(library);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "<3> <3> <ab> library:helper true\n", "", mode);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testImportedGenericStructAllocations, .{});
}

test "generic structs method worklist handles concrete typed locals void and noreturn methods" {
    const c = try arrayTestProgram(
        \\let Box:(T:type)->type=struct{value:T,
        \\let Self=@This()
        \\let earlier:(value:T)->T={let x:T=value
        \\x}
        \\let next:(self:Self)->Box(bool)=Box(bool).make(true)
        \\let make:(value:T)->Self=Self[.value=earlier(value)]
        \\let touch:(self:Self)->void=@print(self.value)
        \\@[noreturn]
        \\let stop:()->void=@panic("stop")
        \\}
        \\let main:()={let x=Box(int).make(3)
        \\x.touch()
        \\let y=x.next()
        \\y.touch()
        \\let stop:()->void=Box(string).stop
        \\@println("done")
        \\}
    );
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "3truedone\n", "", mode);
}

const generic_method_program =
    \\let Result:(T:type,E:type)->type=union{
    \\ok:T,err:E,
    \\let Self=@This()
    \\let unwrapOr:(self:Self,fallback:T)->T=match self{.ok=>|v|v
    \\.err=>fallback}
    \\let unwrapOrElse:(self:@This(),fallback:(E)->T)->T=match self{.ok=>|v|v
    \\.err=>|e|fallback(e)}
    \\let forward:(self:Self,fallback:T)->T=Self.sibling(self,fallback)
    \\let sibling:(self:Self,fallback:T)->T=unwrapOr(self,fallback)
    \\let recur:(self:Self,fallback:T,n:int)->T=if n == 0 {self.forward(fallback)} else {recur(self,fallback,n - 1)}
    \\@[stringify]
    \\let toString:(self:Self)->string=match self{.ok=>"ok"
    \\.err=>"err"}
    \\}
    \\let Count=int
    \\let R=Result(Count,string)
    \\let Alias=R
    \\let S=Result(string,int)
    \\let errorInt:(e:string)->int={@print(e)
    \\41}
    \\let errorString:(e:int)->string={@print(e)
    \\"fall"+"back"}
    \\let make:()->R={@print("R")
    \\.err["E"]}
    \\let makeFallback:()->int={@print("F")
    \\7}
    \\let main:()={
    \\let ok:R=.ok[3]
    \\let err:Alias=.err["E"]
    \\let text:S=.err[5]
    \\let cb:(string)->int=errorInt
    \\let method:(R,int)->int=Alias.unwrapOr
    \\@println(ok.unwrapOr(9)," ",R.unwrapOr(err,8)," ",method(ok,0))
    \\@println(ok.unwrapOrElse(cb)," ",err.unwrapOrElse(cb))
    \\@println(text.unwrapOrElse(errorString))
    \\@println(make().unwrapOr(makeFallback()))
    \\@println(err.recur(6,3)," ",ok," ",text)
    \\}
;

const generic_method_output = "3   8   3\nE3   41\n5fallback\nRF7\n6   ok   err\n";

test "generic union ordinary methods specialize callbacks siblings recursion and printable signatures" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "generic-method.nov", generic_method_program);
        defer module.deinit();
        if (module.hasErrors()) std.debug.print("parse {any} collection {any}\n", .{ module.source.ast.errors, module.diagnostics.items });
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        if (module.hasErrors()) std.debug.print("generic methods: {any}\n", .{module.diagnostics.items});
        try std.testing.expect(result.lowering_complete);
        break :blk result;
    };
    defer sema.deinit();
    try sema.ir.verify();
    const c = try emitProgram(test_gpa, &sema.ir, null);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, generic_method_output, "", mode);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{generic_method_program});
}

test "generic methods preserve formatter identity and roots under forced collection" {
    const c = try arrayTestProgram(generic_method_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, generic_method_output, "", mode);
}

test "generic methods root allocated record payload copies across later arguments" {
    const source =
        \\let G:(T:type,E:type)->type=union{ok:T,err:E,
        \\let unwrapOr:(self:@This(),fallback:T)->T=match self{.ok=>|v|v
        \\.err=>fallback}
        \\}
        \\let Pair=struct{s:string,n:int,}
        \\let R=G(Pair,string)
        \\let S=G(string,bool)
        \\let make:()->R=.ok[Pair[.s="a"+"b",.n=7]]
        \\let later:()->Pair=Pair[.s="c"+"d",.n=9]
        \\let consume:(p:Pair,s:string)=@println(p.s,p.n,s)
        \\let main:()={
        \\let mut original=make()
        \\let copy=original
        \\original=.err["discard"+"ed"]
        \\loop i in 0..3 {consume(copy.unwrapOr(later()),"e"+"f")
        \\let text:S=.ok["g"+"h"]
        \\@println(text.unwrapOr("i"+"j"),i)}
        \\}
    ;
    const c = try arrayTestProgram(source);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "ab 7 ef\ngh 0\nab 7 ef\ngh 1\nab 7 ef\ngh 2\n", "", mode);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{source});
}

test "generic method terminal branches and noreturn methods retain void annotations" {
    const source =
        \\let G:(T:type)->type=union{ok:T,err:string,
        \\let Self=@This()
        \\let get:(self:Self)->T=match self{.ok=>|v|{return v}
        \\.err=>|e|@panic(e)}
        \\@[noreturn]
        \\let halt:(self:Self,message:string)->void=@panic(message)
        \\let stop:(self:Self)->T={Self.halt(self,"stopped")}
        \\}
        \\let R=G(int)
        \\let main:()={let r:R=.ok[7]
        \\@println(r.get())
        \\r.halt("done")}
    ;
    const c = try arrayTestProgram(source);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "7\n", "panic: done\n", mode);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{source});
}

test "generic callback methods allocate only in selected ordinary bodies and report runtime OOM" {
    for ([_]bool{ false, true }) |allocate| {
        const source = try std.fmt.allocPrint(test_gpa, "let G:(T:type,E:type)->type=union{{ok:T,err:E,\nlet unwrapOrElse:(self:@This(),fallback:(E)->T)->T=match self{{.ok=>|v|v\n.err=>|e|fallback(e)}}\n}}\nlet R=G(string,string)\nlet cb:(e:string)->string={s}\nlet main:()={{let r:R=.err[\"err\"]\n@println(r.unwrapOrElse(cb))}}", .{if (allocate) "e+\"!\"" else "e"});
        defer test_gpa.free(source);
        const library = try emitSource(source, null);
        defer test_gpa.free(library);
        const program = try collectingTestProgram(library);
        defer test_gpa.free(program);
        const exhausted = try std.mem.concat(test_gpa, u8, &.{ "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", program });
        defer test_gpa.free(exhausted);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", if (allocate) 1 else 0, if (allocate) "" else "err\n", if (allocate) "panic: GC allocation failed\n" else "", mode);
    }
}

const demand_void_result_program =
    \\@[public]
    \\let Result:(T:type,E:type)->type=union{ok:T,err:E,
    \\let Self=@This()
    \\@[public]
    \\let unwrapOr:(self:Self,fallback:T)->T=match self{.ok=>|v|v
    \\.err=>fallback}
    \\@[public]
    \\let unwrapOrElse:(self:Self,fallback:(E)->T)->T=match self{.ok=>|v|v
    \\.err=>|e|fallback(e)}
    \\}
    \\let Unit=Result(void,string)
    \\let Empty=Result(int,void)
    \\let Both=Result(void,void)
    \\let unit:(b:bool)->Unit=if b{.ok[]}else{.err["err"+"!"]}
    \\let run:(b:bool)->Result(int,string)={let v:void=unit(b).!
    \\.ok[2]}
    \\let makeEmpty:(b:bool)->Empty=if b{.ok[5]}else{.err[]}
    \\let remap:(b:bool)->Result(bool,void)=.ok[makeEmpty(b).! == 5]
    \\let both:()->Both=.err[]
    \\let main:()={
    \\@println(match run(true){.ok=>|v|v
    \\.err=>0})
    \\@println(match run(false){.ok=>"bad"
    \\.err=>|e|e})
    \\@println(makeEmpty(true).unwrapOr(9),makeEmpty(false).unwrapOr(9))
    \\@println(match remap(false){.ok=>"bad"
    \\.err=>"empty"},match both(){.ok=>"bad"
    \\.err=>"both"})}
;

test "demand generic void Results retain source free ownership formatter roots and OOM safety" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "demand-void-result.nov", demand_void_result_program);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(result.lowering_complete);
        try std.testing.expectEqual(@as(usize, 7), result.ir.functions.items.len);
        break :blk result;
    };
    defer sema.deinit();
    try sema.ir.verify();
    const library = try emit(test_gpa, &sema.ir, null);
    defer test_gpa.free(library);
    const program = try collectingTestProgram(library);
    defer test_gpa.free(program);
    const formatted = try arrayTestProgram(demand_void_result_program);
    defer test_gpa.free(formatted);
    try std.testing.expectEqualStrings(program, formatted);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(program, "typedef int nonempty_translation_unit;", 0, "2\nerr!\n5 9\nempty both\n", "", mode);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{demand_void_result_program});
}

test "demand generic method body worklist discovers new containers callbacks and earlier siblings" {
    const source =
        \\let G:(T:type,E:type)->type=union{ok:T,err:E,
        \\let Self=@This()
        \\let identity:(self:Self,fallback:T)->T=match self{.ok=>|v|v
        \\.err=>fallback}
        \\let get:(self:Self,fallback:T)->T=identity(self,fallback)
        \\@[stringify]
        \\let toString:(self:Self)->string={
        \\let flag:G(bool,E)=.ok[true]
        \\let cb:(G(bool,E),bool)->bool=G(bool,E).get
        \\if cb(flag,false){"yes"+"!"}else{"no"}}
        \\let unused:(x:void)->T=missing
        \\let expand:()->G((T)->T)=G((T)->T).expand()
        \\}
        \\let R=G(int,string)
        \\let main:()={let r:R=.ok[1]
        \\@println("{r}")}
    ;
    var sema = blk: {
        var module = try Module.init(test_gpa, "demand-closure.nov", source);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(result.lowering_complete);
        try std.testing.expectEqual(@as(usize, 4), result.ir.functions.items.len);
        break :blk result;
    };
    defer sema.deinit();
    try sema.ir.verify();
    const library = try emit(test_gpa, &sema.ir, null);
    defer test_gpa.free(library);
    const program = try collectingTestProgram(library);
    defer test_gpa.free(program);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(program, "typedef int nonempty_translation_unit;", 0, "yes!\n", "", mode);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{source});
}

const generic_union_output = "PSPok:value:7 err:error\nPPbool:true err:error\nok:ab err:cd\n";

test "generic unions and structural propagation preserve identities effects and forced GC" {
    const c = try arrayTestProgram(generic_union_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, generic_union_output, "", mode);
}

test "generic propagation preserves once only evaluation short circuit returns and void payloads" {
    const source =
        \\let G:(T:type,E:type)->type=union{ok:T,err:E,}
        \\let R=G(int,string)
        \\let read:(b:bool)->R={@print("R")
        \\    if b {.ok[5]} else {.err["e"+"!"]}}
        \\let rhs:()->int={@print("X")
        \\    1}
        \\let sum:(b:bool)->R=.ok[read(b).!+rhs()]
        \\let lazy:(b:bool)->G(bool,string)=.ok[b and read(false).! == 0]
        \\let unit:(b:bool)->G(void,string)=if b {.ok[]} else {.err["void"+"!"]}
        \\let fromUnit:(b:bool)->R={let v:void=unit(b).!
        \\    .ok[2]}
        \\let emptyError:()->G(int,void)=.err[]
        \\let remap:()->G(bool,void)=.ok[emptyError().! == 0]
        \\let terminal:()->R=R.ok[{return .ok[9]}].!
        \\let main:()={
        \\    @println(match sum(false){.ok=>|v|"{v}"
        \\        .err=>|s|s},match sum(true){.ok=>|v|"{v}"
        \\        .err=>|s|s})
        \\    @println(match lazy(false){.ok=>|b|"{b}"
        \\        .err=>|s|s},match lazy(true){.ok=>|b|"{b}"
        \\        .err=>|s|s})
        \\    @println(match fromUnit(true){.ok=>|v|"{v}"
        \\        .err=>|s|s},match fromUnit(false){.ok=>|v|"{v}"
        \\        .err=>|s|s})
        \\    @println(match remap(){.ok=>"bad"
        \\        .err=>"empty"},match terminal(){.ok=>|v|"{v}"
        \\        .err=>|s|s})
        \\}
    ;
    const c = try arrayTestProgram(source);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "RRXe! 6\nRfalse e!\n2 void!\nempty 9\n", "", mode);
}

test "generic success string stays rooted across later allocating arguments and loops" {
    const source =
        \\let G:(T:type,E:type)->type=union{err:E,ok:T,}
        \\let Input=G(string,string)
        \\let Output=G(int,string)
        \\let make:()->Input=.ok["a"+"b"]
        \\let use:(s:string,other:string)->string=s+other
        \\let run:()->Output={
        \\    let s=make().!
        \\    let n:int=loop{let copy=use(s,"c"+"d")
        \\        @println(copy,s)
        \\        break 3}
        \\    .ok[n]
        \\}
        \\let main:()=@println(match run(){.ok=>|v|"{v}"
        \\    .err=>|e|e})
    ;
    const c = try arrayTestProgram(source);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "abcd ab\n3\n", "", mode);
}

test "generic error propagation itself allocates nothing and payload OOM remains checked" {
    for ([_]bool{ false, true }) |allocate| {
        const source = try std.fmt.allocPrint(test_gpa, "let G:(T:type,E:type)->type=union{{ok:T,err:E,}}\nlet make:()->G(int,string)=.err[{s}]\nlet run:()->G(bool,string)=.ok[make().! == 0]\nlet main:()=@println(match run(){{.ok=>\"bad\"\n.err=>|e|e}})", .{if (allocate) "\"e\"+\"rr\"" else "\"err\""});
        defer test_gpa.free(source);
        const library = try emitSource(source, null);
        defer test_gpa.free(library);
        const program = try collectingTestProgram(library);
        defer test_gpa.free(program);
        const exhausted = try std.mem.concat(test_gpa, u8, &.{ "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", program });
        defer test_gpa.free(exhausted);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", if (allocate) 1 else 0, if (allocate) "" else "err\n", if (allocate) "panic: GC allocation failed\n" else "", mode);
    }
}

test "generic union instantiation and propagation own source and survive OOM" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "generic-union.nov", generic_union_program);
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
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, generic_union_output, "");
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{generic_union_program});
}

const noreturn_program =
    \\@[noreturn]
    \\let recursive:()=recursive()
    \\@[noreturn]
    \\let stop:(s:string)->void=@panic(s)
    \\@[noreturn]
    \\let spin:()=loop {loop {break}
    \\    continue}
    \\let A=struct{
    \\    @[noreturn]
    \\    @[public]
    \\    let halt:(s:string)=stop(s)
    \\}
    \\let side:()->int={@print("bad")
    \\    1}
    \\let arg:()->int={@print("A")
    \\    7}
    \\let f:(a:int,b:int,c:int)->int=a+b+c
    \\let indirect:(g:()->void)={g()
    \\    @print("I")}
    \\let normal:()={}
    \\@[noreturn]
    \\let choose:(b:bool)=if b {A.halt("hé\0"+"!")} else {spin()}
    \\let main:()={indirect(normal)
    \\    @println(f(arg(), choose(true), side()))}
;

test "noreturn metadata owns source survives OOM formatting and GC with C11 prototypes" {
    const formatted = try Formatter.format(test_gpa, noreturn_program);
    defer test_gpa.free(formatted);
    for ([_][]const u8{ noreturn_program, formatted }) |source| {
        const c = try arrayTestProgram(source);
        defer test_gpa.free(c);
        try std.testing.expect(std.mem.indexOf(u8, c, "_Noreturn void nov_stop(") != null);
        try std.testing.expect(std.mem.indexOf(u8, c, "_Noreturn void nov_spin(") != null);
        try std.testing.expect(std.mem.indexOf(u8, c, "typedef _Noreturn") == null);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "IA", "panic: hé\x00!\n", mode);
    }
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{noreturn_program});
}

test "terminal loop conditions skip body continuation exhaustion and preserve nested effects" {
    const prelude = "@[noreturn]\nlet stop:()=@panic(\"stop\")\n";
    const Case = struct { body: []const u8, stdout: []const u8 = "", stderr: []const u8 = "panic: stop\n", code: u8 = 1, annotation: []const u8 = "@[noreturn]\n" };
    for ([_]Case{
        .{ .body = "loop @panic(\"stop\") {}" },
        .{ .body = "loop stop() {}" },
        .{ .body = "loop {@print(\"C\")\nstop()} : @print(\"bad cont\") {@print(\"bad body\")\nbreak} else {@print(\"bad else\")}", .stdout = "C" },
        .{ .body = "loop {if true {@print(\"T\")\nstop()} else {@print(\"bad branch\")\n@panic(\"wrong\")}} {@print(\"bad body\")}", .stdout = "T" },
        .{ .body = "loop {@print(\"O\")\nloop {@print(\"I\")\nstop()} {@print(\"bad inner\")}} {@print(\"bad outer\")}", .stdout = "OI" },
        .{ .body = "loop {loop {break} {@print(\"bad inner\")}\n@print(\"C\")\nstop()} {@print(\"bad outer\")}", .stdout = "C" },
        .{ .body = "{loop {return} {@print(\"bad body\")}\n}", .annotation = "", .stderr = "", .code = 0 },
        .{ .body = "{loop {break} {@print(\"bad body\")} else {@print(\"bad else\")}\n@println(\"tail\")}", .annotation = "", .stdout = "tail\n", .stderr = "", .code = 0 },
        .{ .body = "{loop {if true {break} else {stop()}} {@print(\"bad body\")}\n@println(\"tail\")}", .annotation = "", .stdout = "tail\n", .stderr = "", .code = 0 },
        .{ .body = "{let mut n:int=0\nloop {n+=1\nif n<3 {continue} else {@print(n)\nstop()}} {break}}", .stdout = "3" },
        .{ .body = "{let mut n:int=0\nloop {n+=1\nif n<3 {continue}\nbreak} {@print(\"bad body\")}\n@println(n)}", .annotation = "", .stdout = "3\n", .stderr = "", .code = 0 },
        .{ .body = "loop {let mut n:int=0\nloop {n+=1\nif n<3 {continue}\nbreak} {@print(\"bad inner\")}\n@print(n)\nstop()} {@print(\"bad outer\")}", .stdout = "3" },
    }) |case| {
        const source = try std.mem.concat(test_gpa, u8, &.{ prelude, case.annotation, "let main:()=", case.body });
        defer test_gpa.free(source);
        const c = try arrayTestProgram(source);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(c, "typedef int nonempty_translation_unit;", case.code, case.stdout, case.stderr, mode);
    }
}

test "direct noreturn calls are terminal in operands branches and argument paths" {
    const prelude =
        \\@[noreturn]
        \\let stop:()=@panic("stop")
        \\@[noreturn]
        \\let take:(s:string)=@panic(s)
        \\let side:()->int={@print("bad")
        \\    9}
        \\let arg:()->int={@print("A")
        \\    1}
        \\let f:(a:int,b:int,c:int)->int=a+b+c
        \\let returning:()->string=take({return "returned"})
        \\let main:()=
    ;
    for ([_][]const u8{
        "@println(stop()+side())",                            "@println(-stop())",                "@println(stop()==stop())",
        "@println(arg()+stop(), side())",                     "@println(f(arg(),stop(),side()))", "@println(if false {side()} else {stop()})",
        "@println(match true {true=>stop()\nfalse=>side()})", "@println(true and stop(),side())", "@println(stop()(side()))",
        "@panic(stop())",                                     "@println(\"{stop()}\")",           "{@print(returning())\nstop()}",
        "@println(take(stop()),side())",
    }) |body| {
        const source = try std.mem.concat(test_gpa, u8, &.{ prelude, body });
        defer test_gpa.free(source);
        const c = try arrayTestProgram(source);
        defer test_gpa.free(c);
        const output: []const u8 = if (std.mem.indexOf(u8, body, "arg()") != null) "A" else if (std.mem.indexOf(u8, body, "returning()") != null) "returned" else "";
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, output, "panic: stop\n", mode);
    }
}

const panic_program =
    \\let message: () -> string = {@print("M")
    \\    "hé\0" + "!"}
    \\let stop: (b: bool) -> int = match b {
    \\    true => @panic("{message()}")
    \\    false => @panic("wrong")
    \\}
    \\let main: () = @println("before", stop(true), "after")
;

test "panic prints UTF8 and embedded NUL once with allocating interpolation and forced GC" {
    const c = try arrayTestProgram(panic_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "M", "panic: hé\x00!\n", mode);
}

test "panic literal empty and NUL messages need no allocation" {
    for ([_][]const u8{ "", "hé\\0!" }, [_][]const u8{ "panic: \n", "panic: hé\x00!\n" }) |message, expected| {
        const source = try std.fmt.allocPrint(test_gpa, "let main: () = @panic(\"{s}\")", .{message});
        defer test_gpa.free(source);
        var module = try Module.init(test_gpa, "panic.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        const c = try emitProgram(test_gpa, &sema.ir, null);
        defer test_gpa.free(c);
        const exhausted = try std.mem.concat(test_gpa, u8, &.{ "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", c });
        defer test_gpa.free(exhausted);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", 1, "", expected, mode);
    }
}

test "panic branches operands and terminal arguments preserve selected effects" {
    const prelude =
        \\let U = union {ok: int, err: string,}
        \\let side: () -> int = {@print("bad")
        \\    9}
        \\let arg: () -> int = {@print("A")
        \\    1}
        \\let f: (a: int, b: int, c: int) -> int = a + b + c
        \\let pick: (b: bool) -> int = if b {@panic("stop")} else {arg()}
        \\let both: (b: bool) -> string = if b {@panic("stop")} else {@panic("other")}
        \\let returning: () -> string = @panic({return "returned"})
        \\let breaking: () -> int = loop {@panic({break 7})}
        \\let continuing: () -> int = {let mut n: int = 0
        \\    loop n < 2 {n += 1
        \\        @panic({continue})}
        \\    n}
        \\let main: () =
    ;
    const Case = struct { body: []const u8, stdout: []const u8 = "", stderr: []const u8 = "panic: stop\n", code: u8 = 1 };
    for ([_]Case{
        .{ .body = "@println(pick(false), returning(), breaking(), continuing())", .stdout = "A1 returned 7 2\n", .stderr = "", .code = 0 },
        .{ .body = "@println(pick(true), side())" },
        .{ .body = "@println(both(true), side())" },
        .{ .body = "@println(match U.err[\"stop\"] {.ok => |n| n\n.err => |e| @panic(e)}, side())" },
        .{ .body = "@println(match true {true => @panic(\"stop\")\nfalse => @panic(\"other\")}, side())" },
        .{ .body = "@println(@panic(\"stop\") + side())" },
        .{ .body = "@println(-@panic(\"stop\"))" },
        .{ .body = "@println(@panic(\"stop\") + @panic(\"other\"))" },
        .{ .body = "@println(@panic(\"stop\") == @panic(\"other\"))" },
        .{ .body = "@println(if false {@panic(\"wrong\")} else {arg()})", .stdout = "A1\n", .stderr = "", .code = 0 },
        .{ .body = "@println(match false {true => @panic(\"wrong\")\nfalse => arg()})", .stdout = "A1\n", .stderr = "", .code = 0 },
        .{ .body = "@println(match U.ok[7] {.ok => |n| n\n.err => |e| @panic(e)})", .stdout = "7\n", .stderr = "", .code = 0 },
        .{ .body = "@println(arg() + @panic(\"stop\"), side())", .stdout = "A" },
        .{ .body = "@println(f(arg(), @panic(\"stop\"), side()))", .stdout = "A" },
        .{ .body = "@println(@panic(\"stop\")(side()))" },
        .{ .body = "@println(@panic(@panic(\"stop\")), side())" },
        .{ .body = "@println(if @panic(\"stop\") {side()} else {side()})" },
        .{ .body = "@println(true and @panic(\"stop\"), side())" },
        .{ .body = "@println(false or @panic(\"stop\"), side())" },
        .{ .body = "@println(false and @panic(\"stop\"), true or @panic(\"stop\"))", .stdout = "false true\n", .stderr = "", .code = 0 },
        .{ .body = "@println(loop false {} else {@panic(\"stop\")})" },
        .{ .body = "loop {break @panic(\"stop\")}" },
    }) |case| {
        const source = try std.mem.concat(test_gpa, u8, &.{ prelude, case.body });
        defer test_gpa.free(source);
        errdefer std.debug.print("panic expression: {s}\n", .{case.body});
        const c = try arrayTestProgram(source);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(c, "typedef int nonempty_translation_unit;", case.code, case.stdout, case.stderr, mode);
    }
}

test "panic and noreturn source free IR emission and lowering survive every allocation failure" {
    for ([_][]const u8{ panic_program, noreturn_program }, [_][]const u8{ "M", "IA" }) |source, stdout| {
        var sema = blk: {
            var module = try Module.init(test_gpa, "panic.nov", source);
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
        try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 1, stdout, "panic: hé\x00!\n");
        try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{source});
    }
}

test "panic allocation OOM keeps existing runtime diagnostics and flushed effects" {
    const c = try arrayTestProgram(panic_program);
    defer test_gpa.free(c);
    const exhausted = try std.mem.concat(test_gpa, u8, &.{ "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", c });
    defer test_gpa.free(exhausted);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", 1, "M", "panic: GC allocation failed\n", mode);
}

const union_program =
    \\let Alias = U
    \\@[public]
    \\let U = union {
    \\    ok: int, err: string, none, flag: bool,
    \\    @[public]
    \\    @[stringify]
    \\    let toString: (u: Alias) -> string = describe(u)
    \\    let describe: (u: U) -> string = match u {
    \\        .ok => |v| "n:{v}"
    \\        .err => |s| s + "!"
    \\        .none => |unit| {let v: void = unit
    \\            "none"}
    \\        .flag => |b| "b:{b}"
    \\    }
    \\}
    \\let id: (u: U) -> Alias = u
    \\let make: () -> string = {@print("S")
    \\    "hé\0" + "x"}
    \\let choose: (u: U) -> int = match u {
    \\    .ok => |n| n
    \\    _ => -1
    \\}
    \\let main: () = {
    \\    let mut u: Alias = .err[make()]
    \\    let copy = id(u)
    \\    let number = choose(.ok[7])
    \\    u = .none[]
    \\    @println(copy, u, U.flag[true], Alias.ok[number])
    \\    let s = match copy {
    \\        .err => |s| {u = .err["new" + "value"]
    \\            s}
    \\        _ => "unexpected"
    \\    }
    \\    @println(s, copy, u)
    \\    @println(if false {U.err[make()]} else {.ok[3]})
    \\    @println(choose(.none[]))
    \\}
;
const union_output = "Shé\x00x! none b:true n:7\nhé\x00x hé\x00x! newvalue!\nn:3\n-1\n";

test "unions preserve nominal copies private captures methods evaluation and forced GC" {
    const c = try arrayTestProgram(union_program);
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, ".f1.owner") != null);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, union_output, "", mode);
}

test "union copies share immutable owners and zero inactive fields" {
    const library = try emitSource("let U=union{s:string,n:int,none,}\nlet make:()->U=U.s[\"a\"+\"b\"]\nlet copy:(u:U)->U=u\nlet none:()->U=.none[]", null);
    defer test_gpa.free(library);
    const c = try std.mem.concat(test_gpa, u8, &.{ library, "\nint main(void) { nov_rt_heap.collect_every_allocation = 1; " ++
        "nov_record_0 a=nov_make(), b=nov_copy(a), n=nov_none(); " ++
        "if (!a.f0.owner || a.f0.owner != b.f0.owner || a.f0.data != b.f0.data || n.f0.owner || n.f0.data || n.f0.len || n.f1) return 1; " ++
        "if (nov_rt_heap.frames || nov_rt_heap.live_objects != 1) return 2; nov_gc_shutdown(&nov_rt_heap); return 0; }\n" });
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "", "", mode);
}

test "union extraction checks the active tag even in forged well typed IR" {
    var module = try Module.init(test_gpa, "checked-union.nov", "let U=union{ok:int,none,}\nlet get:(u:U)->int=match u{.ok=>|v|v\n_=>0}\nlet main:()=@println(get(.none[]))");
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    var changed = false;
    for (sema.ir.functions.items) |f| for (@constCast(f.body.?.instructions)) |*instruction| {
        if (instruction.op == .union_tag) {
            instruction.op.union_tag.index = 1;
            changed = true;
        }
    };
    try std.testing.expect(changed);
    try sema.ir.verify();
    const c = try emitProgram(test_gpa, &sema.ir, null);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "", "panic: inactive union payload\n", mode);
}

test "union payload integer storage preserves every backend width and boundary" {
    const source = "let U=union{a:i8,b:u8,c:i16,d:u16,e:i32,f:u32,g:i64,h:u64,}\n" ++
        "let show:(u:U)=match u{.a=>|v|@print(v)\n.b=>|v|@print(v)\n.c=>|v|@print(v)\n.d=>|v|@print(v)\n.e=>|v|@print(v)\n.f=>|v|@print(v)\n.g=>|v|@print(v)\n.h=>|v|@print(v)}\n" ++
        "let main:()={show(.a[-128])\n@print(\" \")\nshow(.b[255])\n@print(\" \")\nshow(.c[-32768])\n@print(\" \")\nshow(.d[65535])\n@print(\" \")\nshow(.e[-2147483648])\n@print(\" \")\nshow(.f[4294967295])\n@print(\" \")\nshow(.g[-9223372036854775808])\n@print(\" \")\nshow(.h[18446744073709551615])\n@println()}";
    const c = try arrayTestProgram(source);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "-128 255 -32768 65535 -2147483648 4294967295 -9223372036854775808 18446744073709551615\n", "", mode);
}

test "union payload allocation failure panics but skipped payloads allocate nothing" {
    for ([_]struct { body: []const u8, allocates: bool }{
        .{ .body = "@println(match U.none[] {.s=>|s|s\n.none=>\"ok\"})", .allocates = false },
        .{ .body = "let u:U=if true {.none[]} else {.s[\"a\"+\"b\"]}\n@println(match u {.s=>|s|s\n.none=>\"ok\"})", .allocates = false },
        .{ .body = "let u=U.s[\"a\"+\"b\"]\n@println(match u {.s=>|s|s\n.none=>\"ok\"})", .allocates = true },
    }) |case| {
        const source = try std.fmt.allocPrint(test_gpa, "let U=union{{s:string,none,}}\nlet main:()={{{s}\n}}", .{case.body});
        defer test_gpa.free(source);
        const library = try emitSource(source, null);
        defer test_gpa.free(library);
        const program = try collectingTestProgram(library);
        defer test_gpa.free(program);
        const exhausted = try std.mem.concat(test_gpa, u8, &.{ "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", program });
        defer test_gpa.free(exhausted);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", if (case.allocates) 1 else 0, if (case.allocates) "" else "ok\n", if (case.allocates) "panic: GC allocation failed\n" else "", mode);
    }
}

test "union semantic 128 bit payloads retain reachable C width diagnostics" {
    for ([_][]const u8{ "i128", "u128" }) |width| {
        const source = try std.fmt.allocPrint(test_gpa, "let Wide={s}\nlet Alias=U\nlet U=union{{value:Wide,none,}}\nlet f:(u:Alias)={{}}\nlet main:()={{}}", .{width});
        defer test_gpa.free(source);
        var module = try Module.init(test_gpa, "wide-union.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(sema.lowering_complete);
        var diagnostic: Diagnostic = undefined;
        try std.testing.expectError(error.UnsupportedIntegerWidth, emitProgram(test_gpa, &sema.ir, &diagnostic));
        try std.testing.expectEqual(Diagnostic.Reason.integer_width_unsupported, diagnostic.reason);
        try std.testing.expectEqualStrings("value", module.sourceSlice(.{ .start = diagnostic.span.start, .end = diagnostic.span.end }).?);
        const unused = try std.fmt.allocPrint(test_gpa, "let U=union{{value:{s},none,}}\nlet main:()={{}}", .{width});
        defer test_gpa.free(unused);
        const c = try emitSource(unused, null);
        defer test_gpa.free(c);
        try std.testing.expect(std.mem.indexOf(u8, c, "nov_record_") == null);
    }
}

test "union lowering and emission own source and survive every allocation failure" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "unions.nov", union_program);
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
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, union_output, "");
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{union_program});
}

const field_mutation =
    \\let K = enum { first, second, }
    \\let Alias = R
    \\let R = struct { x: i8 = 10, y: int = 0, s: string = "hé\0", b: bool = false, k: K = .first, len: uint = 0, }
    \\let copy: (r: R) -> R = {let mut local = r
    \\    local.x = 99
    \\    local.s += "copy"
    \\    local}
    \\let early: () -> R = {let mut r = R[]
    \\    r.x += {r.y = 7
    \\        return r}}
    \\let O = union { some: i8, none, }
    \\let propagate: (o: O) -> O = {let mut r = R[]
    \\    r.x += o.!
    \\    .some[r.x]}
    \\let main: () = {
    \\    let mut r: Alias = R[]
    \\    let saved = r
    \\    r.x += {r.y = 7
    \\        r.x = 90
    \\        @print("A")
    \\        3}
    \\    @println(r.x, r.y, saved.x)
    \\    r.x *= {r = R[.x = 100, .y = 8, .s = "new" + "!"]
    \\        2}
    \\    r.x -= 1
    \\    r.x /= 5
    \\    r.x %= 3
    \\    r.b = true
    \\    r.k = .second
    \\    r.len = 42
    \\    @println(r.x, r.y, r.s, r.b, r.k == K.second, r.len)
    \\    r.s = "hé\0" + "old"
    \\    let old = r
    \\    r.s += {r.s = "discard" + "!"
    \\        r.y = 9
    \\        "tail" + "\0"}
    \\    @println(r.s, old.s, r.y, copy(r).x, r.x)
    \\    r.s += {r = R[.s = "whole" + "record", .y = 11]
    \\        "!" + "!"}
    \\    @println(r.s, r.y)
    \\    let mut n: int = 0
    \\    loop n < 3 {
    \\        n += 1
    \\        r.x += if n == 1 {continue} else {if n == 3 {break} else {2}}
    \\    }
    \\    @println(r.x, n, early().x, early().y)
    \\    @println(match propagate(.none[]) {.some => false
    \\        .none => true}, match propagate(.some[2]) {.some => |v| v
    \\        .none => 0})
    \\}
;
const field_mutation_output = "A13 7 10\n2 8 new! true true 42\nhé\x00oldtail\x00 hé\x00old 9 99 2\nhé\x00oldtail\x00!! 11\n12 3 10 7\ntrue 12\n";

test "field mutation snapshots old fields but stores into current slots in strict C11" {
    const c = try arrayTestProgram(field_mutation);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, field_mutation_output, "", mode);
}

test "field mutation terminal RHS panics before pending arithmetic or stores" {
    for ([_][]const u8{ "=", "+=", "-=", "*=", "/=", "%=" }) |op| {
        for ([_][]const u8{ "@panic(\"stop\")", "die()", "{let a:[]i8=[]\na[0]}" }) |rhs| {
            const source = try std.fmt.allocPrint(test_gpa, "let R=struct{{x:i8=7,}}\n@[noreturn]\nlet die:()=@panic(\"stop\")\nlet main:()={{let mut r=R[]\nr.x {s} {{@print(\"R\")\n{s}}}}}", .{ op, rhs });
            defer test_gpa.free(source);
            const c = try arrayTestProgram(source);
            defer test_gpa.free(c);
            for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "R", if (rhs[0] == '{') "panic: array index out of bounds\n" else "panic: stop\n", mode);
        }
    }
    for ([_][]const u8{ "/=", "%=" }) |op| {
        const source = try std.fmt.allocPrint(test_gpa, "let R=struct{{x:i8=7,}}\nlet main:()={{let mut r=R[]\nr.x {s} {{r=R[.x=99]\n@print(\"R\")\n0}}}}", .{op});
        defer test_gpa.free(source);
        const c = try arrayTestProgram(source);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "R", if (std.mem.eql(u8, op, "/=")) "panic: division by zero\n" else "panic: remainder by zero\n", mode);
    }
}

test "field mutation owns source-free IR and survives every compiler allocation failure" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "fields.nov", field_mutation);
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
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, field_mutation_output, "");
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{field_mutation});
}

const record_program =
    \\let Alias = R
    \\@[public]
    \\let R = struct {
    \\    s: string = "hé\0",
    \\    n: int = 0,
    \\    b: bool = false,
    \\    @[public]
    \\    @[stringify]
    \\    let toString: (self: Alias) -> string = {
    \\        @print("T")
    \\        "{self.s}:{self.n}:{self.b}"
    \\    }
    \\    let switch: (value: R) -> Alias = value
    \\}
    \\let num: () -> int = {@print("N")
    \\    5}
    \\let text: () -> string = {@print("S")
    \\    "hé\0" + "!"}
    \\let main: () = {
    \\    let mut item = Alias[ .n = num(), .s = text(), .b = true ]
    \\    let copy = item
    \\    item = R[ .s = "replacement" ]
    \\    @println(copy, R[ .s = text() ], item)
    \\    let chosen = if false { R[ .s = text() ] } else { R.switch(copy) }
    \\    @println(chosen)
    \\    @println(R.toString(R[]))
    \\}
;
const record_output = "NSTSTThé\x00!:5:true hé\x00!:0:false replacement:0:false\nThé\x00!:5:true\nThé\x00:0:false\n";

test "nominal records preserve copies aliases effects field roots and formatting" {
    const c = try arrayTestProgram(record_program);
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, ".f0.owner") != null);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, record_output, "", mode);
}

test "record fields defaults methods and C output survive source release and every allocation failure" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "records.nov", record_program);
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
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, record_output, "");
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{record_program});
}

test "record parameters returns and assignments share immutable string backing" {
    const library = try emitSource("let R = struct { s: string, n: int = 7, }\nlet make: () -> R = R[.s=\"a\"+\"b\"]\nlet share: (r: R) -> R = {let mut copy=r\ncopy=r\ncopy}\n", null);
    defer test_gpa.free(library);
    const c = try std.mem.concat(test_gpa, u8, &.{ library, "\nint main(void) { nov_rt_heap.collect_every_allocation = 1; " ++
        "nov_record_0 a = nov_make(); nov_record_0 b = nov_share(a); " ++
        "if (!a.f0.owner || a.f0.owner != b.f0.owner || a.f0.data != b.f0.data || b.f1 != 7) return 1; " ++
        "b.f1 = 9; if (a.f1 != 7 || nov_rt_heap.live_objects != 1 || nov_rt_heap.frames) return 2; " ++
        "nov_gc_shutdown(&nov_rt_heap); return 0; }\n" });
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "", "", mode);
}

test "record 128 bit fields type check but reachable layouts retain backend width diagnostics" {
    for ([_][]const u8{
        "let R=struct{x:i128,}\nlet f:(r:R)= {}\nlet main:()={}",
        "let Wide=u128\nlet Alias=R\nlet R=struct{x:Wide,}\nlet f:(r:Alias)= {}\nlet main:()={}",
        "let Wide=u128\nlet Alias=R\nlet R=struct{x:Wide=340282366920938463463374607431768211455,}\nlet f:(r:Alias)= {}\nlet main:()={}",
    }) |source| {
        var module = try Module.init(test_gpa, "wide-record.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(sema.lowering_complete);
        try sema.ir.verify();
        var diagnostic: Diagnostic = undefined;
        try std.testing.expectError(error.UnsupportedIntegerWidth, emitProgram(test_gpa, &sema.ir, &diagnostic));
        try std.testing.expectEqual(Diagnostic.Reason.integer_width_unsupported, diagnostic.reason);
        try std.testing.expect(diagnostic.function != null);
        try std.testing.expectEqualStrings("x", source[diagnostic.span.start..diagnostic.span.end]);
    }
}

test "unused record 128 bit fields leave emitted C unchanged" {
    const narrow = try emitSource("let Wide=i64\nlet Alias=R\nlet R=struct{x:Wide,}\nlet main:()={}", null);
    defer test_gpa.free(narrow);
    for ([_][]const u8{ "i128", "u128" }) |width| {
        const source = try std.fmt.allocPrint(test_gpa, "let Wide={s}\nlet Alias=R\nlet R=struct{{x:Wide,}}\nlet main:()={{}}", .{width});
        defer test_gpa.free(source);
        const c = try emitSource(source, null);
        defer test_gpa.free(c);
        try std.testing.expectEqualStrings(narrow, c);
        try std.testing.expect(std.mem.indexOf(u8, c, "nov_record_") == null);
    }
}

test "record scalar fields cover every supported integer width and literal limits" {
    const source =
        \\let R = struct {
        \\ a: i8 = -128, b: u8 = 255,
        \\ c: i16 = -32768, d: u16 = 65535,
        \\ e: i32 = -2147483648, f: u32 = 4294967295,
        \\ g: i64 = -9223372036854775808, h: u64 = 18446744073709551615,
        \\ i: int = -1, j: uint = 1,
        \\}
        \\let main: () = { let r = R[]
        \\ @println(r.a, r.b, r.c, r.d, r.e, r.f, r.g, r.h, r.i, r.j)
        \\}
    ;
    const c = try arrayTestProgram(source);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "-128 255 -32768 65535 -2147483648 4294967295 -9223372036854775808 18446744073709551615 -1 1\n", "", mode);
}

test "record scoped forward methods override module homonyms without global printing overloads" {
    const source =
        \\let Alias=R
        \\let first: (r: R) -> string = "wrong"
        \\let R=struct { s: Text="right",
        \\ @[stringify]
        \\ let toString: (self: Alias) -> Text = first(self)
        \\ let first: (self: R) -> string = second(self)
        \\ let second: (self: R) -> string = self.s+"!"
        \\}
        \\let Text=string
        \\let main:()={ @println(Alias[], Alias.first(R[])) }
    ;
    const c = try arrayTestProgram(source);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "right! right!\n", "", mode);
}

test "record constructors and allocating toString report OOM while skipped constructors allocate nothing" {
    for ([_]struct { body: []const u8, allocates: bool }{
        .{ .body = "let r = if true { R[] } else { R[.s=\"a\"+\"b\"] }\n@println(r.s)", .allocates = false },
        .{ .body = "let r=R[.s=\"a\"+\"b\"]\n@println(r.s)", .allocates = true },
        .{ .body = "@println(R[])", .allocates = true },
    }) |case| {
        const source = try std.fmt.allocPrint(test_gpa, "let R=struct{{s:string=\"ok\",\n@[stringify]\nlet toString:(self:R)->string=self.s+\"!\"\n}}\nlet main:()={{{s}\n}}", .{case.body});
        defer test_gpa.free(source);
        const library = try emitSource(source, null);
        defer test_gpa.free(library);
        const program = try collectingTestProgram(library);
        defer test_gpa.free(program);
        const exhausted = try std.mem.concat(test_gpa, u8, &.{ "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", program });
        defer test_gpa.free(exhausted);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", if (case.allocates) 1 else 0, if (case.allocates) "" else "ok\n", if (case.allocates) "panic: GC allocation failed\n" else "", mode);
    }
}

test "record string field roots cover branch and loop merges early returns and allocating arguments" {
    const source =
        \\let R = struct { s: string,
        \\ @[stringify]
        \\ let toString: (self: R) -> string = self.s + "!"
        \\}
        \\let make: (r: R, text: string) -> R = { @println(text)
        \\ return r }
        \\let main: () = {
        \\ let a = if true { R[.s="a"+"b"] } else { R[.s="unused"+"x"] }
        \\ let b: R = loop { break a }
        \\ @println(make(b, "allocate"+"!"), R[.s="c"+"d"])
        \\}
    ;
    const c = try arrayTestProgram(source);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "allocate!\nab! cd!\n", "", mode);
}

/// Lowers `source`, which must be accepted by the frontend, and emits C.
fn emitSource(source: []const u8, diagnostic: ?*Diagnostic) ![]u8 {
    var module = try Module.init(test_gpa, "test.nov", source);
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    if (module.hasErrors()) {
        for (module.source.ast.errors) |err| std.debug.print("parse error: {any}\n", .{err});
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
    const terminated = try test_gpa.dupeSentinel(u8, source, 0);
    defer test_gpa.free(terminated);
    const formatted = try Formatter.format(test_gpa, terminated);
    defer test_gpa.free(formatted);
    const formatted_z = try test_gpa.dupeSentinel(u8, formatted, 0);
    defer test_gpa.free(formatted_z);
    const again = try Formatter.format(test_gpa, formatted_z);
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
const string_output = "hé\x00😀 4 new\nABA! B!\nhé\x00😀 early! late\nnewxxx 0  {literal} é\n2 hé\x00😀\n";

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
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, match_pipes_output, "", mode);
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
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", if (allocate) 1 else 0, if (allocate) "" else "ok\n", if (allocate) "panic: GC allocation failed\n" else "", mode);
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
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{match_pipes});
}

const boolean_match_program =
    \\let Flag = bool
    \\let identity: (n: int) -> int = n
    \\let subject: (b: Flag) -> Flag = {@print("S")
    \\    b}
    \\let select: (b: Flag) -> string = match subject(b) {
    \\    true => {@print("T")
    \\        "yes" + "!"}
    \\    false => {@print("F")
    \\        "no" + "!"}
    \\}
    \\let early: (b: bool) -> int = match b {
    \\    false => {return 4}
    \\    true => {return 5}
    \\}
    \\let mixed: (b: bool) -> int = match b {
    \\    true => {return 6}
    \\    false => 7
    \\}
    \\let subjectExit: () -> int = match {return 8} {
    \\    true => 99
    \\    false => 100
    \\}
    \\let main: () = {
    \\    @println(select(true), select(false))
    \\    @println(early(false), early(true), mixed(true), mixed(false), subjectExit())
    \\    let mut count: int = 0
    \\    let value = @as(int, match {count += 1
    \\        true} {true => 1
    \\        true | false => 2})
    \\    @println(value, count)
    \\    @println(@as(int, match subject(false) {true | false => 3}))
    \\    @println(@as(int, match subject(true) {_ | true => 9}))
    \\    @println(@as(int, match false {true => 10
    \\        false | _ => 11}))
    \\    let text = "root" + "ed"
    \\    @println(match false {true => "bad"
    \\        false => text}, "later" + "!")
    \\    let original: []int = [12, 13]
    \\    let mut chosen = match false {true => []
    \\        false => original}
    \\    chosen[0] = 99
    \\    let later: []int = [14]
    \\    @println(original[0], chosen[0], chosen.len, later[0])
    \\    let callback = match true {true => identity
    \\        false => identity}
    \\    @println(callback(15))
    \\    let inferred = match false {true => 16
    \\        false => count}
    \\    @println(inferred)
    \\    let result = loop {
    \\        loop false {} else {
    \\            match count == 1 {
    \\                true => {count += 1
    \\                    continue}
    \\                false => {break count}
    \\            }
    \\        }
    \\    }
    \\    @println(result)
    \\    let nested: int = loop {
    \\        match loop false {} else {break 17} {
    \\            true => @println("bad")
    \\            false => @println("bad")
    \\        }
    \\    }
    \\    @println(nested)
    \\    loop {
    \\        match {if count == 2 {count += 1
    \\                continue}
    \\            break} {true => @println("bad")
    \\            false => @println("bad")}
    \\    }
    \\    @println(count)
    \\}
;
const boolean_match_output = "STSFyes! no!\n4 5 6 7 8\n1 1\nS3\nS9\n11\nrooted later!\n12 99 2 14\n15\n1\n2\n17\n3\n";

test "exhaustive boolean match preserves effects terminal paths managed results formatting and forced GC" {
    const c = try arrayTestProgram(boolean_match_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, boolean_match_output, "", mode);
}

test "exhaustive boolean match owns metadata after source release" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "boolean-match.nov", boolean_match_program);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        break :blk result;
    };
    defer sema.deinit();
    const c = try emitProgram(test_gpa, &sema.ir, null);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, boolean_match_output, "", mode);
}

test "exhaustive boolean match cleans up every compiler allocation failure" {
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{boolean_match_program});
}

test "exhaustive boolean match allocates only in selected prong and reports runtime OOM" {
    for ([_]bool{ false, true }) |allocate| {
        const c = try arrayTestProgram(if (allocate)
            "let main: () = match false {true => @println(\"ok\")\nfalse => @println(\"a\" + \"b\")}"
        else
            "let main: () = match false {true => @println(\"a\" + \"b\")\nfalse => @println(\"ok\")}");
        defer test_gpa.free(c);
        const exhausted = try std.mem.concat(test_gpa, u8, &.{
            "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n" ++
                "#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n",
            c,
        });
        defer test_gpa.free(exhausted);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", if (allocate) 1 else 0, if (allocate) "" else "ok\n", if (allocate) "panic: GC allocation failed\n" else "", mode);
    }
}

const bitwise_program =
    \\let left: () -> i8 = {@print("L")
    \\    let a: []i8 = [6]
    \\    a[0]}
    \\let right: () -> i8 = {@print("R")
    \\    let s = "alloc" + "ated"
    \\    let a: []i8 = [3]
    \\    if s == "allocated" {a[0]} else {0}}
    \\let early_left: () -> i8 = {return 7} & right()
    \\let early_right: () -> i8 = left() ^ {return 8}
    \\let factory: () -> (i8) -> i8 = {@print("F")
    \\    let s = "a" + "b"
    \\    if s == "ab" {identity} else {identity}}
    \\let identity: (n: i8) -> i8 = n
    \\let main: () -> void = {
    \\    @println(left() & right(), left() | right(), left() ^ right())
    \\    @println(early_left(), early_right())
    \\    @println(factory()(left() | right()))
    \\    let mut n: i8 = 1
    \\    @println({n = 6
    \\        n} & {n = 3
    \\        n}, n)
    \\    let chosen = loop {let skipped = {break 5 | n} & right()
    \\    }
    \\    let other: i8 = loop {let skipped = left() ^ {break 9}
    \\    }
    \\    @println(chosen, other)
    \\    let mut steps: int = 0
    \\    loop steps < 3 : steps += 1 {
    \\        let skipped: i8 = {if steps == 0 {continue}
    \\            6} & {if steps == 2 {break}
    \\            right()}
    \\        @println(skipped)
    \\    }
    \\    @println(steps)
    \\    let typed: i8 = 7
    \\    @println(match typed {0 | 1 => false
    \\        _ => true})
    \\    let a: i8 = 1 | 2 & 4
    \\    let b: i8 = 7 ^ 3 & 2
    \\    @println(a, b, ~typed & 3 == 0, 1 | typed ^ 2 == 5)
    \\}
;
const bitwise_output = "LRLRLR2 7 5\nL7 8\nFLR7\n2 3\nL7 9\nR2\n2\ntrue\n0 0 true true\n";

test "bitwise binaries preserve effects controls precedence formatting and forced GC" {
    const c = try arrayTestProgram(bitwise_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, bitwise_output, "", mode);
}

test "bitwise binary scalar emission needs no runtime helpers" {
    const c = try emitSource("let f: (a: i8, b: i8) -> i8 = a & b | a ^ b\nlet g: (a: u64, b: u64) -> u64 = a ^ b", null);
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_rt_") == null);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_gc_") == null);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_panic") == null);
    try compileAndRun(c, "int main(void) { return 0; }");
}

test "bitwise binaries support all fixed and native integer boundaries without helpers" {
    var source = std.Io.Writer.Allocating.init(test_gpa);
    defer source.deinit();
    var output = std.Io.Writer.Allocating.init(test_gpa);
    defer output.deinit();
    try source.writer.writeAll("let main: () -> void = {\n");
    for ([_][]const u8{ "i8", "i16", "i32", "i64", "int", "u8", "u16", "u32", "u64", "uint" }, 0..) |name, i| {
        const signed = name[0] == 'i';
        const bits = if (std.mem.eql(u8, name, "int") or std.mem.eql(u8, name, "uint")) Ir.native_pointer_bits else try std.fmt.parseInt(u16, name[1..], 10);
        const sign: u128 = @as(u128, 1) << @intCast(bits - 1);
        const max = if (signed) sign - 1 else (sign * 2) - 1;
        const min: i128 = if (signed) -@as(i128, @intCast(sign)) else 0;
        try source.writer.print("let lo{d}: {s} = {d}\nlet hi{d}: {s} = {d}\nlet sign{d}: {s} = {d}\n", .{ i, name, min, i, name, max, i, name, if (signed) min else @as(i128, @intCast(sign)) });
        try source.writer.print("@println(lo{d} & hi{d}, lo{d} | hi{d}, hi{d} ^ hi{d}, ~hi{d} & hi{d}, sign{d} & sign{d}, sign{d} | hi{d})\n", .{ i, i, i, i, i, i, i, i, i, i, i, i });
        try source.writer.print("let p{d}: {s} = 6\n@println(p{d} & 3, 3 | p{d}, p{d} ^ 3)\n", .{ i, name, i, i, i });
        try output.writer.print("0 {d} 0 0 {d} {d}\n2 7 5\n", .{ if (signed) @as(i128, -1) else @as(i128, @intCast(max)), if (signed) min else @as(i128, @intCast(sign)), if (signed) @as(i128, -1) else @as(i128, @intCast(max)) });
    }
    try source.writer.writeAll("}\n");
    const c = try emitSource(source.written(), null);
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_rt_div_") == null);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_rt_mod_") == null);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_gc_") == null);
    try std.testing.expect(std.mem.indexOf(u8, c, "uint32_t)") != null);
    try std.testing.expect(std.mem.indexOf(u8, c, "uint64_t)") != null);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "void nov_main(void); int main(void) { nov_main(); return 0; }", 0, output.written(), "", mode);
}

test "bitwise binary owned lowering emits after source release" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "source-free.nov", bitwise_program);
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
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, bitwise_output, "");
}

test "bitwise binaries survive every compiler allocation failure" {
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{bitwise_program});
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
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, array_membership_output, "", mode);
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
    const expected = try std.mem.join(test_gpa, "", &@as([11][]const u8, @splat("true false false\n")));
    defer test_gpa.free(expected);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, expected, "", mode);
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
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "", "", mode);
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
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{array_membership});
}

const append_rhs_cases = [_]struct { source: []const u8, allocations: usize, output: []const u8 }{
    .{ .source = "let main:()={let mut a:[]int=[1]\na+=a\n@println(a[0],a[1])}", .allocations = 3, .output = "1 1\n" },
    .{ .source = "let main:()={let mut a:[]int=[1]\nlet b:[]int=[2]\na+=b\n@println(a[0],a[1],b[0])}", .allocations = 4, .output = "1 2 2\n" },
    .{ .source = "let L=struct{a:[]int,b:[]int,}\nlet T=struct{leaf:L,}\nlet main:()={let mut a=[T[.leaf=L[.a=[1],.b=[2]]]]\nlet b=[T[.leaf=L[.a=[3],.b=[4]]]]\na+=b\n@println(a[0].leaf.a[0],a[1].leaf.b[0],b[0].leaf.a[0])}", .allocations = 12, .output = "1 4 3\n" },
    .{ .source = "let L=struct{a:[]int,}\nlet T=struct{leaf:L,}\nlet main:()={let mut a=T[.leaf=L[.a=[1]]]\nlet b=T[.leaf=L[.a=[2]]]\nlet r=&b\na.leaf.a+=r.leaf.a\n@println(a.leaf.a[0],a.leaf.a[1],r.leaf.a[0])}", .allocations = 4, .output = "1 2 2\n" },
    .{ .source = "let L=struct{a:[]int,}\nlet main:()={let mut a=L[.a=[1]]\nlet r=&a\nr.a+=r.a\n@println(r.a.len,r.a[0],r.a[1])}", .allocations = 3, .output = "2 1 1\n" },
    .{ .source = "let L=struct{a:[]int,}\nlet main:()={let mut a=[L[.a=[1]]]\nlet r=&a[0]\nr.a+=r.a\n@println(r.a.len,r.a[0],r.a[1])}", .allocations = 4, .output = "2 1 1\n" },
    .{ .source = "let main:()={let a:[]int=[2]\nlet r=&a\nlet mut b:[]int=[1]\nb+=r\n@println(b[0],b[1],r[0])}", .allocations = 4, .output = "1 2 2\n" },
    .{ .source = "let L=struct{a:[]int,}\nlet main:()={let mut rows=[L[.a=[2]]]\nlet mut a:[]int=[1]\na+=rows[{rows=[L[.a=[9]]]\n0}].a\n@println(a[0],a[1],rows[0].a[0])}", .allocations = 9, .output = "1 2 9\n" },
    .{ .source = "let L=struct{a:[]int,}\nlet select:(a:[]L,n:int)->L=a[n]\nlet main:()={let mut rows=[L[.a=[2]]]\nlet mut a:[]int=[1]\na+=select(rows,{rows=[L[.a=[9]]]\n0}).a\n@println(a[0],a[1],rows[0].a[0])}", .allocations = 10, .output = "1 2 9\n" },
    .{ .source = "let L=struct{a:[]int,}\nlet main:()={let mut rows=[L[.a=[2]]]\nlet change=(){rows=[L[.a=[9]]]}\nlet index=()->int {change()\n0}\nlet mut a:[]int=[1]\na+=rows[index()].a\n@println(a[0],a[1],rows[0].a[0])}", .allocations = 13, .output = "1 2 9\n" },
    .{ .source = "let main:()={let mut a:[]int=[1]\nlet append=(){a+=a}\nlet same=append\nsame()\n@println(a[0],a[1])}", .allocations = 5, .output = "1 1\n" },
};

test "append RHS projection exact allocations and every runtime OOM under forced GC values and nested projections" {
    try testAppendRhsRuntimeOom(0, 4);
}

test "append RHS projection exact allocations and every runtime OOM under forced GC references" {
    try testAppendRhsRuntimeOom(4, 7);
}

test "append RHS projection exact allocations and every runtime OOM under forced GC effects and closures" {
    try testAppendRhsRuntimeOom(7, 11);
}

fn testAppendRhsRuntimeOom(start: usize, end: usize) !void {
    for (append_rhs_cases[start..end], start..) |case, case_index| {
        errdefer std.debug.print("append RHS case {d}: {s}\n", .{ case_index, case.source });
        const program = try arrayTestProgram(case.source);
        defer test_gpa.free(program);
        for (0..case.allocations + 1) |budget| {
            const prefix = try std.fmt.allocPrint(test_gpa, "#include <stdlib.h>\n#include <assert.h>\nstatic unsigned allocations, outstanding;\n" ++
                "static void *test_malloc(size_t n) {{ if (allocations == {d}) return NULL; void *p = malloc(n); if (p) {{ ++allocations; ++outstanding; }} return p; }}\n" ++
                "static void test_free(void *p) {{ assert(outstanding); --outstanding; free(p); }}\n" ++
                "#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE test_free\n#define main test_main\n", .{budget});
            defer test_gpa.free(prefix);
            const suffix = try std.fmt.allocPrint(test_gpa, "\n#undef main\nint main(void) {{ int result = test_main(); assert(!outstanding); assert(allocations == {d}); return result; }}\n", .{case.allocations});
            defer test_gpa.free(suffix);
            const counted = try std.mem.concat(test_gpa, u8, &.{ prefix, program, suffix });
            defer test_gpa.free(counted);
            const success = budget == case.allocations;
            for ([_][]const u8{ "-O0", "-O3" }) |mode|
                try compileAndRunMode(counted, "typedef int nonempty_translation_unit;", if (success) 0 else 1, if (success) case.output else "", if (success) "" else "panic: GC allocation failed\n", mode);
        }
    }
}

test "append RHS projection preserves lhs snapshots effects once and terminal evaluation" {
    const program = try arrayTestProgram(
        \\let L=struct{a:[]int,}
        \\let stop:()->int={let mut a:[]int=[1]
        \\a+={@print("R")
        \\return 7}}
        \\let main:()={let mut a=L[.a=[1]]
        \\let change=()->[]int {a.a=[9]
        \\@print("C")
        \\a.a}
        \\a.a+=change()
        \\@println(a.a[0],a.a[1],stop())
        \\let mut steps:int=0
        \\loop steps<3 : steps+=1 {
        \\let mut b:[]int=[2]
        \\b+={if steps==0 {continue}
        \\if steps==2 {break}
        \\b}
        \\@println(b[0],b[1])}
        \\@println(steps)
        \\let mut rows=[L[.a=[3]]]
        \\let mut b:[]int=[1]
        \\b+=rows[{@print("I")
        \\rows=[L[.a=[8]]]
        \\0}].a
        \\@println(b[0],b[1],rows[0].a[0])}
    );
    defer test_gpa.free(program);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(program, "typedef int nonempty_translation_unit;", 0, "CR1 9 7\n2 2\n2\nI1 3 8\n", "", mode);
}

fn testAppendRhsOwnership(allocator: std.mem.Allocator, source: []const u8) !void {
    var sema = blk: {
        var module = try Module.init(allocator, "append-rhs.nov", source);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        break :blk result;
    };
    defer sema.deinit();
    try sema.ir.verify();
    const c = try emitProgram(allocator, &sema.ir, null);
    defer allocator.free(c);
}

test "append RHS projection source free ownership and compiler allocation failures" {
    for (append_rhs_cases) |case| {
        try testAppendRhsOwnership(test_gpa, case.source);
        try TestAllocations.checkAllAllocationFailures(test_gpa, testAppendRhsOwnership, .{case.source});
    }
}

test "append RHS projection OOM preserves snapshot before index side effects" {
    const program = try arrayTestProgram(
        \\let L=struct{a:[]int,}
        \\let main:()={let mut rows=[L[.a=[2]]]
        \\let mut a:[]int=[1]
        \\a+=rows[{@print("I")
        \\rows=[L[.a=[9]]]
        \\0}].a
        \\@println(a[0],a[1],rows[0].a[0])}
    );
    defer test_gpa.free(program);
    for ([_]usize{ 0, 3, 4, 5, 6, 8, 9 }, [_][]const u8{ "", "", "", "", "I", "I", "I1 2 9\n" }) |budget, output| {
        const prefix = try std.fmt.allocPrint(test_gpa, "#include <stdlib.h>\nstatic unsigned allocations;\n" ++
            "static void *test_malloc(size_t n) {{ return ++allocations > {d} ? NULL : malloc(n); }}\n" ++
            "#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", .{budget});
        defer test_gpa.free(prefix);
        const counted = try std.mem.concat(test_gpa, u8, &.{ prefix, program });
        defer test_gpa.free(counted);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(counted, "typedef int nonempty_translation_unit;", if (budget == 9) 0 else 1, output, if (budget == 9) "" else "panic: GC allocation failed\n", mode);
    }
}

const readonly_membership_cases = [_]struct { source: []const u8, allocations: usize, output: []const u8 }{
    .{
        .source = "let main:()={let a:[]i16=[-3,2,7]\nloop _ in 0..4 {@println(2 in a,9 in a)}}",
        .allocations = 1,
        .output = "true false\ntrue false\ntrue false\ntrue false\n",
    },
    .{
        .source = "let L=struct{a:[]int,b:[]int,}\nlet T=struct{leaf:L,}\nlet main:()={let t=T[.leaf=L[.a=[1],.b=[2]]]\nloop _ in 0..4 {@println(1 in t.leaf.a,3 in t.leaf.b)}}",
        .allocations = 2,
        .output = "true false\ntrue false\ntrue false\ntrue false\n",
    },
    .{
        .source = "let L=struct{a:[]int,}\nlet main:()={let t=L[.a=[1]]\nlet r=&t\nloop _ in 0..4 {@println(1 in r.a,2 in r.a)}}",
        .allocations = 1,
        .output = "true false\ntrue false\ntrue false\ntrue false\n",
    },
    .{
        .source = "let main:()={let a:[]u8=[]\nlet n:u8=255\n@println(n in a,n in [],true in [],'😀' in [],\"\" in [])}",
        .allocations = 0,
        .output = "false false false false false\n",
    },
    .{
        .source = "let main:()={let n:i8=-128\n@println(n in [-128,127],true in [false,true],false in [true])}",
        .allocations = 3,
        .output = "true true false\n",
    },
    .{
        .source = "let main:()={let a=[\"hé\"+\"\\0😀\",\"\"]\nlet r=['😀','\\0']\nlet b=[false,true]\nloop _ in 0..4 {@println(\"hé\\0😀\" in a,\"hé\" in a,'😀' in r,'x' in r,true in b)}}",
        .allocations = 4,
        .output = "true false true false true\ntrue false true false true\ntrue false true false true\ntrue false true false true\n",
    },
    .{
        // The entire record array is detached before the index replaces it.
        .source = "let L=struct{a:[]int,b:[]int,}\nlet main:()={let mut rows=[L[.a=[1],.b=[2]]]\nlet found=1 in rows[{rows=[L[.a=[9],.b=[8]]]\n0}].a\n@println(found,9 in rows[0].a)}",
        .allocations = 9,
        .output = "true true\n",
    },
    .{
        // Both closures share the mutable cell. The index must use the old base.
        .source = "let L=struct{a:[]int,}\nlet main:()={let mut rows=[L[.a=[1]]]\nlet change=(){rows=[L[.a=[9]]]}\nlet index=()->int {change()\n0}\nlet found=1 in rows[index()].a\n@println(found,9 in rows[0].a)}",
        .allocations = 10,
        .output = "true true\n",
    },
    .{
        // A computed base must still snapshot its argument before later effects.
        .source = "let L=struct{a:[]int,}\nlet select:(a:[]L,n:int)->L=a[n]\nlet main:()={let mut rows=[L[.a=[1]]]\nlet found=1 in select(rows,{rows=[L[.a=[9]]]\n0}).a\n@println(found,9 in rows[0].a)}",
        .allocations = 7,
        .output = "true true\n",
    },
    .{
        .source = "let main:()={let a:[]int=[1]\nlet r=&a\nloop _ in 0..4 {@println(1 in r,2 in r)}}",
        .allocations = 1,
        .output = "true false\ntrue false\ntrue false\ntrue false\n",
    },
    .{
        .source = "let L=struct{a:[]int,}\nlet stop:()->bool={let rows=[L[.a=[1]]]\n1 in rows[{return true}].a}\nlet main:()=@println(stop())",
        .allocations = 4,
        .output = "true\n",
    },
};

test "readonly membership exact allocation counts and budgets preserve snapshots under forced GC" {
    for (readonly_membership_cases, 0..) |case, case_index| {
        errdefer std.debug.print("readonly membership case {d}: {s}\n", .{ case_index, case.source });
        const program = try arrayTestProgram(case.source);
        defer test_gpa.free(program);
        for (0..case.allocations + 1) |budget| {
            const prefix = try std.fmt.allocPrint(test_gpa, "#include <stdlib.h>\n#include <assert.h>\nstatic unsigned allocations, outstanding;\n" ++
                "static void *test_malloc(size_t n) {{ if (allocations == {d}) return NULL; void *p = malloc(n); if (p) {{ ++allocations; ++outstanding; }} return p; }}\n" ++
                "static void test_free(void *p) {{ assert(outstanding); --outstanding; free(p); }}\n" ++
                "#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE test_free\n#define main test_main\n", .{budget});
            defer test_gpa.free(prefix);
            const suffix = try std.fmt.allocPrint(test_gpa, "\n#undef main\nint main(void) {{ int result = test_main(); assert(!outstanding); assert(allocations == {d}); return result; }}\n", .{case.allocations});
            defer test_gpa.free(suffix);
            const counted = try std.mem.concat(test_gpa, u8, &.{ prefix, program, suffix });
            defer test_gpa.free(counted);
            const success = budget == case.allocations;
            for ([_][]const u8{ "-O0", "-O3" }) |mode|
                try compileAndRunMode(counted, "typedef int nonempty_translation_unit;", if (success) 0 else 1, if (success) case.output else "", if (success) "" else "panic: GC allocation failed\n", mode);
        }
    }
}

test "readonly membership snapshots keep recursively independent backing" {
    for (readonly_membership_cases[6..8]) |case| {
        var module = try Module.init(test_gpa, "membership-backing.nov", case.source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        try sema.ir.verify();
        const body = sema.ir.functions.items[0].body.?;
        const transfers = try ArrayTransfers.init(test_gpa, &sema.ir, body);
        defer transfers.deinit(test_gpa);
        var library = try emit(test_gpa, &sema.ir, null);
        defer test_gpa.free(library);
        var checked: usize = 0;
        for (body.instructions, 0..) |instruction, i| {
            if (instruction.op != .array_clone) continue;
            const ty = sema.ir.getType(instruction.ty).?;
            if (ty != .array or sema.ir.getType(ty.array).? != .record) continue;
            checked += 1;
            const source = @backingInt(instruction.op.array_clone);
            var operation = std.Io.Writer.Allocating.init(test_gpa);
            defer operation.deinit();
            try writeArrayValue(&sema.ir, &operation.writer, body, transfers, instruction, i);
            var checks = std.Io.Writer.Allocating.init(test_gpa);
            defer checks.deinit();
            try checks.writer.writeAll(operation.written());
            try checks.writer.print("    assert(v{d}.data != v{d}.data);\n", .{ i, source });
            for (sema.ir.getType(ty.array).?.record.fields, 0..) |_, field|
                try checks.writer.print("    assert(((nov_record_{d} *)v{d}.data)[0].f{d}.data != ((nov_record_{d} *)v{d}.data)[0].f{d}.data);\n", .{ @backingInt(ty.array), i, field, @backingInt(ty.array), source, field });
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, library, operation.written()));
            const replaced = try std.mem.replaceOwned(u8, test_gpa, library, operation.written(), checks.written());
            test_gpa.free(library);
            library = replaced;
        }
        try std.testing.expectEqual(@as(usize, 1), checked);
        const prefixed = try std.mem.concat(test_gpa, u8, &.{ "#include <assert.h>\n", library });
        defer test_gpa.free(prefixed);
        const program = try collectingTestProgram(prefixed);
        defer test_gpa.free(program);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(program, "typedef int nonempty_translation_unit;", 0, case.output, "", mode);
    }
}

test "readonly membership orders lhs snapshot index effects and runtime OOM" {
    const program = try arrayTestProgram(
        \\let L=struct{a:[]int,b:[]int,}
        \\let main:()={let mut rows=[L[.a=[1],.b=[2]]]
        \\let found={@print("L")
        \\1} in rows[{@print("I")
        \\rows=[L[.a=[9],.b=[8]]]
        \\0}].a
        \\@println(found,9 in rows[0].a)}
    );
    defer test_gpa.free(program);
    for ([_]usize{ 0, 3, 6, 9 }, [_][]const u8{ "", "L", "LI", "LItrue true\n" }) |budget, output| {
        const prefix = try std.fmt.allocPrint(test_gpa, "#include <stdlib.h>\nstatic unsigned allocations;\n" ++
            "static void *test_malloc(size_t n) {{ return ++allocations > {d} ? NULL : malloc(n); }}\n" ++
            "#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", .{budget});
        defer test_gpa.free(prefix);
        const c = try std.mem.concat(test_gpa, u8, &.{ prefix, program });
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(c, "typedef int nonempty_translation_unit;", if (budget == 9) 0 else 1, output, if (budget == 9) "" else "panic: GC allocation failed\n", mode);
    }
}

test "readonly membership source free IR and every compiler allocation failure" {
    for (readonly_membership_cases, 0..) |case, case_index| {
        errdefer std.debug.print("readonly membership ownership case {d}: {s}\n", .{ case_index, case.source });
        try TestAllocations.checkAllAllocationFailures(test_gpa, testCopyElisionOwnership, .{case.source});
    }
}

const string_byte_program =
    \\let Box:(T:type)->type=struct{value:T,}
    \\let identity:(s:string)->string=s
    \\let base:()->string={@print("B")
    \\    "a"+"é\0Z"}
    \\let index:()->int={let garbage="index"+" allocation"
    \\    @print("I")
    \\    return -2}
    \\let accept:(b:rune)->rune=b
    \\let gcIndex:(i:int)->int={let garbage="collect"+" index"
    \\    i}
    \\let terminalIndex:()->u8="x"[{return 8}]
    \\let main:()={
    \\    let text="aé€😀\0Z"
    \\    @println(text[0],text[1],text[2],text[3],text[4],text[5],text.len)
    \\    @println(text[-1],text[-2],text[-3],text[-4],text[-5],text[-6])
    \\    let inferred=text[1]
    \\    let bytes:[]rune=[text[2],inferred]
    \\    @println(accept(inferred),inferred=='é',text[2]=='€',bytes[0])
    \\    @println(base()[index()])
    \\    let mut owner="o"+"ld"
    \\    @println(owner[{owner="n"+"ew"
    \\        let garbage="collect"+" now"
    \\        0}],owner)
    \\    let array=["a"+"b","c"+"d"]
    \\    let box=Box(string)[.value="e"+"f"]
    \\    let chosen=if true {"i"+"j"}else{"k"+"l"}
    \\    @println(array[-1][gcIndex(-1)],box.value[gcIndex(0)],identity("g"+"h")[gcIndex(1)],chosen[gcIndex(-2)])
    \\    let r=&owner
    \\    @println(r[0],r[-1],terminalIndex())
    \\}
;
const string_byte_output = "a é € 😀 \x00 Z 6\nZ \x00 😀 € é a\né true true €\nBI\x00\no new\nd e h i\nn w 8\n";

test "string scalar indexing exact types effects snapshots UTF8 NUL and forced GC" {
    const c = try arrayTestProgram(string_byte_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, string_byte_output, "", mode);
}

test "string scalar indexing bounds panic at both optimization levels" {
    const min = try std.fmt.allocPrint(test_gpa, "{d}", .{std.math.minInt(isize)});
    defer test_gpa.free(min);
    const max = try std.fmt.allocPrint(test_gpa, "{d}", .{std.math.maxInt(isize)});
    defer test_gpa.free(max);
    for ([_]struct { text: []const u8, index: []const u8 }{
        .{ .text = "abc", .index = "3" },
        .{ .text = "é€😀", .index = "3" },
        .{ .text = "é€😀", .index = "-4" },
        .{ .text = "abc", .index = "-4" },
        .{ .text = "abc", .index = min },
        .{ .text = "abc", .index = max },
        .{ .text = "", .index = "0" },
        .{ .text = "", .index = "-1" },
    }) |case| {
        const source = try std.fmt.allocPrint(test_gpa, "let main:()=@println(\"{s}\"[{s}])", .{ case.text, case.index });
        defer test_gpa.free(source);
        const c = try arrayTestProgram(source);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "", "panic: string index out of bounds\n", mode);
    }
}

const string_slice_program =
    \\let base:()->string={@print("B")
    \\    "aé€"+"😀\0z"}
    \\let lower:()->int={@print("L")
    \\    1}
    \\let upper:()->int={@print("U")
    \\    return -2}
    \\let main:()={
    \\    let view=base()[lower()..=upper()]
    \\    @println(view,view.len,view[0],view[-2])
    \\    @println(view[..2],view[2..],view[..][1..3])
    \\    @println(""[..].len,view[4..].len,view[..0].len,view[-4..-4].len)
    \\    let mut text="aé"+"€😀"
    \\    let mut start:int=1
    \\    let snapshot=text[{text="re"+"bound"
    \\        start}..{start=0
    \\        @as(int,-1)}]
    \\    @println(snapshot,text,start)
    \\    let values=[view,snapshot]
    \\    @println(values[0][..1],values[1][..])
    \\}
;
const string_slice_output = "BLUé€😀\x00 4 é 😀\né€ 😀\x00 €😀\n0 0 0 0\né€ rebound 0\né é€\n";

test "string slicing Unicode views order rebind forced GC source free and OOM" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "slicing.nov", string_slice_program);
        defer module.deinit();
        errdefer std.debug.print("slice parse: {any}\n", .{module.source.ast.errors});
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        errdefer std.debug.print("slice diagnostics: {any} parse: {any}\n", .{ module.diagnostics.items, module.source.ast.errors });
        try std.testing.expect(!module.hasErrors());
        break :blk result;
    };
    defer sema.deinit();
    try sema.ir.verify();
    const c = try arrayTestProgram(string_slice_program);
    defer test_gpa.free(c);
    const source_free = try emitProgram(test_gpa, &sema.ir, null);
    defer test_gpa.free(source_free);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| {
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, string_slice_output, "", mode);
        try compileAndRunMode(source_free, "typedef int nonempty_translation_unit;", 0, string_slice_output, "", mode);
    }
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{string_slice_program});
}

test "string slicing rejects private region values slots and borrow capabilities" {
    var module = try Module.init(test_gpa, "invalid-slice-ir.nov", "let f:(flag:bool,n:int)->string={let mut s=if flag {\"a\"}else{\"b\"}\nlet r=&s\nr[n..n]}");
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    const instructions = @constCast(sema.ir.functions.items[0].body.?.instructions);
    var read: usize = 0;
    var private: Ir.ValueId = undefined;
    for (instructions, 0..) |instruction, i| {
        if (instruction.op == .string_slice) read = i;
        if (instruction.op == .branch) private = instruction.op.branch.then_value;
    }
    const saved = instructions[read];
    for (0..3) |operand| {
        instructions[read] = saved;
        switch (operand) {
            0 => instructions[read].op.string_slice.base = private,
            1 => instructions[read].op.string_slice.start = private,
            2 => instructions[read].op.string_slice.end = private,
            else => unreachable,
        }
        try std.testing.expectError(error.InvalidValueId, emit(test_gpa, &sema.ir, null));
        for (instructions, 0..) |instruction, i| {
            if (instruction.op != .local_slot and instruction.op != .borrow_reserve and instruction.op != .borrow_start) continue;
            instructions[read] = saved;
            const value: Ir.ValueId = @fromBackingInt(@intCast(i));
            switch (operand) {
                0 => instructions[read].op.string_slice.base = value,
                1 => instructions[read].op.string_slice.start = value,
                2 => instructions[read].op.string_slice.end = value,
                else => unreachable,
            }
            try std.testing.expectError(if (instruction.op == .local_slot) error.InvalidValueId else if (operand == 0) error.InvalidOperation else error.OperandTypeMismatch, emit(test_gpa, &sema.ir, null));
        }
    }
    instructions[read] = saved;
    try sema.ir.verify();
}

test "string slicing managed subviews retain original owner across frame release and GC" {
    const c = try emitSource("let make:()->string={let s=\"aé\"+\"€😀z\"\ns[1..-1]}\nlet cut:(s:string)->string=s[1..]\nlet empty:(s:string)->string=s[0..0]", null);
    defer test_gpa.free(c);
    const harness =
        \\int main(void) {
        \\    nov_rt_heap.collect_every_allocation = 1;
        \\    nov_string view = nov_make();
        \\    if (!view.owner || view.len != 9 || view.data == view.owner) return 1;
        \\    void *owner = view.owner;
        \\    void **roots[] = {&view.owner}; nov_gc_roots frame = {0};
        \\    nov_gc_push(&nov_rt_heap, &frame, roots, 1);
        \\    view = nov_cut(view);
        \\    if (view.owner != owner || view.len != 7) return 2;
        \\    nov_gc_collect(&nov_rt_heap);
        \\    if (nov_rt_heap.live_objects != 1 || view.data[0] != 0xe2) return 3;
        \\    view = nov_empty(view);
        \\    if (view.owner != owner || view.len != 0) return 4;
        \\    nov_gc_collect(&nov_rt_heap);
        \\    if (nov_rt_heap.live_objects != 1) return 5;
        \\    nov_gc_pop(&nov_rt_heap, &frame);
        \\    nov_gc_collect(&nov_rt_heap);
        \\    if (nov_rt_heap.live_objects) return 6;
        \\    nov_gc_shutdown(&nov_rt_heap); return 0;
        \\}
    ;
    const program = try std.mem.concat(test_gpa, u8, &.{ c, harness });
    defer test_gpa.free(program);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(program, "typedef int nonempty_translation_unit;", 0, "", "", mode);
}

test "string slicing invalid normalized bounds panic in all modes" {
    for ([_][]const u8{
        "\"é€😀\"[..=3]",
        "\"é€😀\"[2..=0]",
        "\"é€😀\"[..=-4]",
        "\"\"[..=0]",
        "\"é€😀\"[..=9223372036854775807]",
        "\"é€😀\"[..=-9223372036854775808]",
        "\"é€😀\"[4..]",
        "\"é€😀\"[..4]",
        "\"é€😀\"[-4..]",
        "\"é€😀\"[..-4]",
        "\"é€😀\"[2..1]",
        "\"é€😀\"[-1..-2]",
        "\"\"[1..]",
        "\"\"[..-1]",
        "\"é€😀\"[-9223372036854775808..]",
        "\"é€😀\"[..-9223372036854775808]",
    }) |expression| {
        const source = try std.fmt.allocPrint(test_gpa, "let main:()=@println({s})", .{expression});
        defer test_gpa.free(source);
        const c = try arrayTestProgram(source);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "", "panic: string slice out of bounds\n", mode);
    }
}

test "string slicing shares owner byte offsets permits NULL empty and allocates nothing" {
    const c = try emitSource("let cut:(s:string,a:int,b:int)->string=s[a..b]\nlet all:(s:string)->string=s[..]\nlet tail:(s:string,a:int)->string=s[a..]\nlet inclusive:(s:string,a:int,b:int)->string=s[a..=b]\nlet inclusive_all:(s:string)->string=s[..=]", null);
    defer test_gpa.free(c);
    const program = try std.mem.concat(test_gpa, u8, &.{
        "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n",
        c,
        \\int main(void) {
        \\    static const unsigned char bytes[] = "a\xc3\xa9\xe2\x82\xac\xf0\x9f\x98\x80\0z";
        \\    const nov_string text = {NULL,bytes,sizeof bytes - 1};
        \\    nov_string v = nov_cut(text,1,-1);
        \\    if (v.owner != text.owner || v.data != bytes+1 || v.len != 10) return 1;
        \\    v = nov_cut(v,1,3);
        \\    if (v.data != bytes+3 || v.len != 7) return 2;
        \\    v = nov_all((nov_string){0});
        \\    if (v.data || v.owner || v.len) return 3;
        \\    v = nov_tail((nov_string){0},0);
        \\    if (v.data || v.owner || v.len) return 4;
        \\    v = nov_cut((nov_string){0},0,0);
        \\    if (v.data || v.owner || v.len) return 5;
        \\    v = nov_cut(text,6,6);
        \\    if (v.data != bytes+sizeof bytes-1 || v.len) return 6;
        \\    v = nov_inclusive(text,1,-2);
        \\    if (v.data != bytes+1 || v.len != 10 || v.owner != text.owner) return 7;
        \\    v = nov_inclusive(text,2,1);
        \\    if (v.data != bytes+3 || v.len) return 8;
        \\    v = nov_inclusive__all((nov_string){0});
        \\    if (v.data || v.owner || v.len) return 9;
        \\    nov_gc_shutdown(&nov_rt_heap); return 0;
        \\}
        ,
    });
    defer test_gpa.free(program);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(program, "typedef int nonempty_translation_unit;", 0, "", "", mode);
}

test "string scalar indexing helper is opt in and allocation free" {
    for ([_][]const u8{
        "let main:()={}",
        "let f:(s:string)->string=s",
        "let f:(a:[]string)->string=a[0]",
        "let f:(a:[]u8)->u8=a[0]",
    }) |source| {
        const c = try emitSource(source, null);
        defer test_gpa.free(c);
        try std.testing.expect(std.mem.indexOf(u8, c, "nov_string_rune_at") == null);
        try compileAndRun(c, "int main(void) { return 0; }");
    }
    const c = try emitSource("let at:(s:string,i:int)->rune=s[i]", null);
    defer test_gpa.free(c);
    const program = try std.mem.concat(test_gpa, u8, &.{
        "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n",
        c,
        "\nint main(void) { " ++
            "if (nov_at((nov_string){NULL, (const unsigned char *)\"a\\0b\", 3}, -2) != 0) return 1; " ++
            "nov_gc_shutdown(&nov_rt_heap); return 0; }\n",
    });
    defer test_gpa.free(program);
    try compileAndRun(program, "typedef int nonempty_translation_unit;");
}

test "string scalar length is demand only and counts scalars not graphemes or bytes" {
    const c = try arrayTestProgram(
        \\let base:()->string={@print("B")
        \\    "é"+"😀"}
        \\let main:()={
        \\    @println("".len,"\0".len,"e\u{301}".len,"é€😀".len,base().len)
        \\    let bytes:[]u8=[195,169]
        \\    @println(bytes.len,bytes[0])
        \\}
    );
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_string_rune_at") == null);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "B0 1 2 3 2\n2 195\n", "", mode);
}

test "string scalar decoder accepts every UTF8 boundary without allocating" {
    const c = try emitSource("let at:(s:string,i:int)->rune=s[i]\nlet size:(s:string)->uint=s.len", null);
    defer test_gpa.free(c);
    const harness =
        \\int main(void) {
        \\    static const unsigned char bytes[] = {0,0x7f,0xc2,0x80,0xdf,0xbf,0xe0,0xa0,0x80,0xed,0x9f,0xbf,0xee,0x80,0x80,0xef,0xbf,0xbf,0xf0,0x90,0x80,0x80,0xf4,0x8f,0xbf,0xbf};
        \\    static const uint32_t expected[] = {0,0x7f,0x80,0x7ff,0x800,0xd7ff,0xe000,0xffff,0x10000,0x10ffff};
        \\    const nov_string text = {NULL, bytes, sizeof bytes};
        \\    if (nov_size(text) != 10) return 1;
        \\    if (nov_size((nov_string){0}) != 0) return 2;
        \\    for (intptr_t i = 0; i < 10; ++i) {
        \\        if (nov_at(text, i) != expected[i]) return 3;
        \\        if (nov_at(text, i - 10) != expected[i]) return 4;
        \\    }
        \\    nov_gc_shutdown(&nov_rt_heap); return 0;
        \\}
    ;
    const program = try std.mem.concat(test_gpa, u8, &.{
        "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n",
        c,
        harness,
    });
    defer test_gpa.free(program);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(program, "typedef int nonempty_translation_unit;", 0, "", "", mode);
}

test "string scalar decoder rejects malformed truncated overlong surrogate and out of range UTF8" {
    const c = try emitSource("let at:(s:string,i:int)->rune=s[i]\nlet size:(s:string)->uint=s.len", null);
    defer test_gpa.free(c);
    for ([_][]const u8{
        "0x80",                "0xc0,0x80",           "0xc1,0xbf",      "0xc2",                "0xc2,0x41",
        "0xe0,0x80,0x80",      "0xe2,0x82",           "0xed,0xa0,0x80", "0xf0,0x80,0x80,0x80", "0xf0,0x90,0x80",
        "0xf4,0x90,0x80,0x80", "0xf5,0x80,0x80,0x80", "0xff",
    }) |bytes| {
        for ([_][]const u8{ "nov_at(text, 0)", "nov_at(text, -1)", "nov_size(text)" }) |operation| {
            const harness = try std.fmt.allocPrint(test_gpa, "int main(void) {{ const unsigned char bytes[] = {{{s}}}; const nov_string text = {{NULL,bytes,sizeof bytes}}; (void){s}; return 2; }}", .{ bytes, operation });
            defer test_gpa.free(harness);
            const program = try std.mem.concat(test_gpa, u8, &.{ c, harness });
            defer test_gpa.free(program);
            for ([_][]const u8{ "-O0", "-O3" }) |mode|
                try compileAndRunMode(program, "typedef int nonempty_translation_unit;", 1, "", "panic: invalid UTF-8 string\n", mode);
        }
    }
}

test "string scalar indexing source free emission and compiler OOM sweep" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "source-free.nov", string_byte_program);
        defer module.deinit();
        errdefer std.debug.print("string byte diagnostics: {any} parse: {any}\n", .{ module.diagnostics.items, module.source.ast.errors });
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        break :blk result;
    };
    defer sema.deinit();
    try sema.ir.verify();
    const c = try emitProgram(test_gpa, &sema.ir, null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, string_byte_output, "");
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{string_byte_program});
}

test "string scalar indexing rejects escaped regions slots and borrow capabilities before C emission" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "malformed-byte.nov", "let f:(flag:bool)->rune={let mut s=if flag {\"a\"}else{\"b\"}\nlet r=&s\nr[0]}");
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        break :blk result;
    };
    defer sema.deinit();
    const instructions = @constCast(sema.ir.functions.items[0].body.?.instructions);
    var read: usize = 0;
    var private: Ir.ValueId = undefined;
    for (instructions, 0..) |instruction, i| {
        if (instruction.op == .array_index) read = i;
        if (instruction.op == .branch) private = instruction.op.branch.then_value;
    }
    const saved = instructions[read];
    instructions[read].op.array_index.array = private;
    try std.testing.expectError(error.InvalidValueId, emit(test_gpa, &sema.ir, null));
    instructions[read] = saved;
    for (instructions, 0..) |instruction, i| {
        if (instruction.op != .local_slot and instruction.op != .borrow_reserve and instruction.op != .borrow_start) continue;
        instructions[read].op.array_index.array = @fromBackingInt(@intCast(i));
        try std.testing.expectError(if (instruction.op == .local_slot) error.InvalidValueId else error.InvalidOperation, emit(test_gpa, &sema.ir, null));
        instructions[read] = saved;
        instructions[read].op.array_index.index = @fromBackingInt(@intCast(i));
        try std.testing.expectError(if (instruction.op == .local_slot) error.InvalidValueId else error.OperandTypeMismatch, emit(test_gpa, &sema.ir, null));
        instructions[read] = saved;
    }
    try sema.ir.verify();
}

test "string scalar indexing terminal base skips index calls" {
    const library = try emitSource("let index:()->int={@print(\"wrong\")\n0}\nlet main:()=@println(@panic(\"base\")[index()])", null);
    defer test_gpa.free(library);
    const c = try collectingTestProgram(library);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "", "panic: base\n", mode);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_string_rune_at") == null);
}

fn importedStringByteProgram(allocator: std.mem.Allocator) ![]u8 {
    var sema = blk: {
        var comp = Compilation.init(allocator);
        defer comp.deinit();
        const root = try comp.addSource("/main.nov", "let api=@import(\"api.nov\")\nlet Alias=api.Text\nlet main:()={let s:Alias=api.make()\n@println(s[0],s[-1],api.make()[1])}");
        const api = try comp.addSource("/api.nov", "@[public]\nlet Text=string\n@[public]\nlet make:()->Text=\"é\"+\"!\"");
        const module = comp.getModule(root).?;
        const local = module.lookup("api").?;
        try comp.imports.append(allocator, .{ .declaration = comp.declaration_ids.get(.{ .source = root, .local = local }).?, .target = api, .named = false, .span = module.getDeclaration(local).?.name_span });
        comp.setRoot(root);
        var result = try Lowering.lowerCompilation(&comp);
        errdefer result.deinit();
        try std.testing.expect(!comp.hasErrors());
        break :blk result;
    };
    defer sema.deinit();
    try sema.ir.verify();
    return emit(allocator, &sema.ir, null);
}

fn importedStringByteAllocations(allocator: std.mem.Allocator) !void {
    allocator.free(try importedStringByteProgram(allocator));
}

test "string scalar indexing imported aliases survive graph release GC and OOM" {
    const library = try importedStringByteProgram(test_gpa);
    defer test_gpa.free(library);
    const c = try collectingTestProgram(library);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "é ! !\n", "", mode);
    try TestAllocations.checkAllAllocationFailures(test_gpa, importedStringByteAllocations, .{});
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

const iteration_program =
    \\let R=struct{n:int,s:string,}
    \\let Nested=struct{inner:R,}
    \\let sum:(a:[]int)->int={let mut n:int=0
    \\    loop &x in a {n+=x}
    \\    n}
    \\let produce:()->[]int={@print("once:")
    \\    [@as(int,1),2,3]}
    \\let first:()->string=loop r in "😀é" {return "{r}"} else {"empty"}
    \\let main:()={
    \\    loop x in produce() {@print(x)}
    \\    @println("")
    \\    let empty:[]int=[]
    \\    loop x in empty {@print(x)} else {@println("empty")}
    \\    let mut a:[]int=[1,2,3]
    \\    let before=a
    \\    loop &x in a {x*=2
    \\        if x==4 {continue}
    \\        x+=1}
    \\    loop x in a {@print(x)}
    \\    @print(":")
    \\    loop x in before {@print(x)}
    \\    @println("")
    \\    loop &x in a {x=9
    \\        break}
    \\    @println(a[0],a[1],a[2])
    \\    if sum(a)!=20 {@panic("parameter loan")}
    \\    let flags=[true,false]
    \\    let mut seen:uint=0
    \\    loop flag in flags {if flag {seen+=1}}
    \\    if seen!=1 {@panic("bool iteration")}
    \\    let mut runes=['é','😀']
    \\    loop &r in runes {r='€'}
    \\    loop r in runes {if r!='€' {@panic("rune iteration")}}
    \\    let mut nested=[Nested[.inner=R[.n=1,.s="nested"+"!"]]]
    \\    loop &r in nested {r.inner.n+=2
    \\        r.inner.s+="?"
    \\        let copy=r.inner
    \\        r.inner=R[.n=copy.n*2,.s=copy.s+"."]}
    \\    if nested[0].inner.n!=6 {@panic("nested integer write")}
    \\    if nested[0].inner.s!="nested!?." {@panic("nested string write")}
    \\    let mut records=[R[.n=1,.s="a"+"!"],R[.n=2,.s="b"+"!"]]
    \\    let originals=records
    \\    loop &r in records {r.n+=3
    \\        r.s+="?"}
    \\    loop &r in records {let old=r
    \\        r=R[.n=old.n*2,.s=old.s+"."]}
    \\    loop r in records {let mut copy=r
    \\        copy.n=99
    \\        @println(r.n,r.s,copy.n)}
    \\    loop r in originals {@println(r.n,r.s)}
    \\    let mut words=["a"+"b","é"+"😀"]
    \\    loop &s in words {s+="!"}
    \\    loop s in words {@println(s)}
    \\    let mut text="Aé€😀\0Z"+"!"
    \\    loop r in text {text="rebound"+"!"
    \\        @print(r)}
    \\    @println("",text)
    \\    loop _ in "" {@print("bad")} else {@println("empty string")}
    \\    loop r in "0é😀x"[-3..-1] {@print(r)}
    \\    @println("")
    \\    let choice=loop r in "é😀" {if r=='😀' {break r}} else {'?'}
    \\    @println(choice,first())
    \\    let picked=loop &x in a {if x==4 {break x}} else {0}
    \\    @println(picked)
    \\    loop &x in a {loop r in "é!" {if r=='!' {continue}
    \\        @print(x,r)}}
    \\    @println("")
    \\}
;
const iteration_output = "once:123\nempty\n347:123\n9 4 7\n8 a!?. 99\n10 b!?. 99\n1 a!\n2 b!\nab!\né😀!\nAé€😀\x00Z! rebound!\nempty string\né😀\n😀 😀\n4\n9 é4 é7 é\n";

test "iteration arrays strings scoped references snapshots Unicode NUL records and GC" {
    const c = try arrayTestProgram(iteration_program);
    defer test_gpa.free(c);
    var sema = source_free: {
        var module = try Module.init(test_gpa, "iteration.nov", iteration_program);
        defer module.deinit();
        var lowered = try Lowering.lower(&module);
        errdefer lowered.deinit();
        try std.testing.expect(!module.hasErrors());
        break :source_free lowered;
    };
    defer sema.deinit();
    const library = try emit(test_gpa, &sema.ir, null);
    defer test_gpa.free(library);
    const source_free = try collectingTestProgram(library);
    defer test_gpa.free(source_free);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| {
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, iteration_output, "", mode);
        try compileAndRunMode(source_free, "typedef int nonempty_translation_unit;", 0, iteration_output, "", mode);
    }
}

const iteration_source_cases = [_]struct { source: []const u8, allocations: usize, output: []const u8 }{
    .{
        .source = "let main:()={let a:[]int=[1,2,3]\nloop _ in 0..3 {loop x in a {@print(x)}}}",
        .allocations = 1,
        .output = "123123123",
    },
    .{
        .source = "let main:()={let mut a:[]int=[1,2,3]\nloop x in a {if x==2 {continue}\n@print(x,a[0])}}",
        .allocations = 1,
        .output = "1 13 1",
    },
    .{
        .source = "let main:()={let a:[]int=[]\n@println(loop x in a {break x} else {7})}",
        .allocations = 0,
        .output = "7\n",
    },
    .{
        .source = "let scan:(a:[]int)={loop x in a {@print(x)}}\nlet main:()=scan([1,2])",
        .allocations = 1,
        .output = "12",
    },
    .{
        // A later loan forces a parameter slot before the loop is lowered.
        .source = "let scan:(a:[]int)={loop x in a {@print(x)}\nlet r=&a\n@print(r.len)}\nlet main:()=scan([1,2])",
        .allocations = 1,
        .output = "122",
    },
    .{
        .source = "let scan:(a:[]int)={let mut b=a\nloop x in b {@print(x)}\n@print(a[0],b[0])}\nlet main:()=scan([1,2])",
        .allocations = 2,
        .output = "121 1",
    },
    .{
        .source = "let R=struct{n:int,a:[]int,b:[]int,}\nlet main:()={let rows=[R[.n=1,.a=[2],.b=[3]]]\nloop _ in 0..3 {loop r in rows {@print(r.n)}}}",
        .allocations = 3,
        .output = "111",
    },
    .{
        // Borrowed and computed bases must still take their ordinary snapshot.
        .source = "let main:()={let a:[]int=[1,2]\nlet r=&a\nloop x in r {@print(x)}}",
        .allocations = 2,
        .output = "12",
    },
    .{
        .source = "let main:()={let a:[]int=[1,2]\nloop x in {a} {@print(x)}}",
        .allocations = 2,
        .output = "12",
    },
};

test "iteration source exact allocation counts budgets and forced GC" {
    for (iteration_source_cases, 0..) |case, case_index| {
        errdefer std.debug.print("iteration source case {d}: {s}\n", .{ case_index, case.source });
        const program = try arrayTestProgram(case.source);
        defer test_gpa.free(program);
        for (0..case.allocations + 1) |budget| {
            const prefix = try std.fmt.allocPrint(test_gpa, "#include <stdlib.h>\n#include <assert.h>\nstatic unsigned allocations, outstanding;\n" ++
                "static void *test_malloc(size_t n) {{ if (allocations == {d}) return NULL; void *p = malloc(n); if (p) {{ ++allocations; ++outstanding; }} return p; }}\n" ++
                "static void test_free(void *p) {{ assert(outstanding); --outstanding; free(p); }}\n" ++
                "#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE test_free\n#define main test_main\n", .{budget});
            defer test_gpa.free(prefix);
            const suffix = try std.fmt.allocPrint(test_gpa, "\n#undef main\nint main(void) {{ int result = test_main(); assert(!outstanding); assert(allocations == {d}); return result; }}\n", .{case.allocations});
            defer test_gpa.free(suffix);
            const counted = try std.mem.concat(test_gpa, u8, &.{ prefix, program, suffix });
            defer test_gpa.free(counted);
            const success = budget == case.allocations;
            for ([_][]const u8{ "-O0", "-O3" }) |mode|
                try compileAndRunMode(counted, "typedef int nonempty_translation_unit;", if (success) 0 else 1, if (success) case.output else "", if (success) "" else "panic: GC allocation failed\n", mode);
        }
    }
}

const iteration_source_values =
    \\let R=struct{a:[]int,}
    \\let change:(r:R)->R={let mut copy=r
    \\    copy.a[0]=8
    \\    copy}
    \\let make:()->[]R={@print("S")
    \\    [R[.a=[4]]]}
    \\let early:()->int=loop x in {return 7} {}
    \\let main:()={let mut rows=[R[.a=[1]],R[.a=[2]]]
    \\    let mut saved=R[.a=[]]
    \\    let mut stored=R[.a=[]]
    \\    let mut captured:()->int=()->int {0}
    \\    loop r in rows {saved=change(r)
    \\        stored=r
    \\        captured=()->int {r.a[0]}
    \\        @println(r.a[0],saved.a[0])}
    \\    rows[1]=R[.a=[9]]
    \\    saved.a[0]=6
    \\    stored.a[0]=5
    \\    @println(rows[0].a[0],rows[1].a[0],saved.a[0],stored.a[0],captured())
    \\    loop r in make() {@print(r.a[0])}
    \\    @println(early())
    \\    let a:[]int=[1,2,3]
    \\    @println(loop x in a {if x==2 {break x}} else {9})
    \\    let exhausted:int=loop x in a {loop y in a {if y==1 {continue}
    \\        if y==2 {break}}
    \\        if x==3 {continue}} else {5}
    \\    @println(exhausted)
    \\}
;
const iteration_source_values_output = "1 8\n2 8\n1 9 6 5 2\nS47\n2\n5\n";

test "iteration source copied records passed stored captured controls and source free GC" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "iteration-source.nov", iteration_source_values);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        if (module.hasErrors()) for (module.diagnostics.items) |diagnostic| {
            std.debug.print("iteration source diagnostic: {any}\n", .{diagnostic});
        };
        try std.testing.expect(!module.hasErrors());
        break :blk result;
    };
    defer sema.deinit();
    try sema.ir.verify();
    var library = try emit(test_gpa, &sema.ir, null);
    defer test_gpa.free(library);
    var checked: usize = 0;
    for (sema.ir.functions.items) |function| {
        if (!std.mem.eql(u8, function.name, "main")) continue;
        const body = function.body.?;
        for (body.instructions, 0..) |instruction, i| {
            if (instruction.op != .array_clone or sema.ir.getType(instruction.ty).? != .record) continue;
            const source = @backingInt(instruction.op.array_clone);
            if (body.instructions[source].op != .array_index) continue;
            // A passed, stored or captured item must recursively detach even
            // though its loop source descriptor now shares the owner backing.
            const operation = try std.fmt.allocPrint(test_gpa, "    v{d} = nov_copy_{d}(v{d});\n", .{ i, @backingInt(instruction.ty), source });
            defer test_gpa.free(operation);
            const checks = try std.fmt.allocPrint(test_gpa, "{s}    assert(v{d}.f0.data != v{d}.f0.data);\n", .{ operation, i, source });
            defer test_gpa.free(checks);
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, library, operation));
            const replaced = try std.mem.replaceOwned(u8, test_gpa, library, operation, checks);
            test_gpa.free(library);
            library = replaced;
            checked += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 3), checked);
    const prefixed = try std.mem.concat(test_gpa, u8, &.{ "#include <assert.h>\n", library });
    defer test_gpa.free(prefixed);
    const program = try collectingTestProgram(prefixed);
    defer test_gpa.free(program);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(program, "typedef int nonempty_translation_unit;", 0, iteration_source_values_output, "", mode);
}

test "iteration call protection permits disjoint unboxed methods and completed loop calls" {
    const source =
        \\let R=struct{n:int=0,
        \\    let invoke:(self:R,cb:())={let run=(){cb()}
        \\        run()}
        \\    @[stringify]
        \\    let toString:(self:R)->string="R"
        \\}
        \\let G:(T:type)->type=struct{value:T,
        \\    let invoke:(self:@This(),cb:())=cb()
        \\}
        \\let main:()={let mut a:[]int=[1,2,3]
        \\    let mut b:[]int=[0]
        \\    let changeB=(){let loan=&b[0]
        \\        loan+=1}
        \\    let r=R[]
        \\    let g=G(int)[.value=0]
        \\    loop x in a {r.invoke(changeB)
        \\        g.invoke((){@print(x)})
        \\        let text="{r}"
        \\        @print(text)}
        \\    if b[0]!=3 {@panic("disjoint owner")}
        \\    let mut c:[]int=[4,5,6]
        \\    let changeC=(){let loan=&c[1]
        \\        loan=9}
        \\    loop x in c {@print(x)} else {r.invoke(changeC)}
        \\    @print(c[1])
        \\    loop _ in c {break} else {r.invoke(changeC)}
        \\    r.invoke(changeC)
        \\    if c[1]!=9 {@panic("completed reservation")}
        \\}
    ;
    const program = try arrayTestProgram(source);
    defer test_gpa.free(program);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(program, "typedef int nonempty_translation_unit;", 0, "1R2R3R4569", "", mode);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testCopyElisionOwnership, .{source});
}

test "iteration call protection permits late captures after completed loops and computed snapshots" {
    const source =
        \\let after:()={let mut a:[]int=[1,2,3]
        \\    loop x in a {@print(x)}
        \\    let f=(){let loan=&a[2]
        \\        loan=9}
        \\    f()
        \\    @print(a[2])}
        \\let main:()={after()
        \\    let run=(){let mut a:[]int=[1,2,3]
        \\        loop x in a {@print(x)}
        \\        let f=(){let loan=&a[2]
        \\            loan=9}
        \\        f()
        \\        @print(a[2])}
        \\    run()
        \\    let mut a:[]int=[1,2,3]
        \\    let mut f:()=(){}
        \\    loop x in {a} {f()
        \\        f=(){let loan=&a[2]
        \\            loan=9}
        \\        @print(x)}
        \\    @print(a[2])
        \\}
    ;
    const program = try arrayTestProgram(source);
    defer test_gpa.free(program);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(program, "typedef int nonempty_translation_unit;", 0, "123912391239", "", mode);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testCopyElisionOwnership, .{source});
}

test "iteration source source free IR survives every compiler allocation failure" {
    for (iteration_source_cases) |case|
        try TestAllocations.checkAllAllocationFailures(test_gpa, testCopyElisionOwnership, .{case.source});
    try TestAllocations.checkAllAllocationFailures(test_gpa, testCopyElisionOwnership, .{iteration_source_values});
}

test "iteration forward string decoder is linear allocation free and checks malformed input" {
    const library = try emitSource("let f:(s:string)=loop r in s {@print(r)}", null);
    defer test_gpa.free(library);
    try std.testing.expect(std.mem.indexOf(u8, library, "nov_string_scalar_len") == null);
    try std.testing.expect(std.mem.indexOf(u8, library, "nov_string_rune_at") == null);
    try std.testing.expect(std.mem.indexOf(u8, library, "nov_string_cursor_next") != null);
    const success = try std.mem.concat(test_gpa, u8, &.{ library, "\nint main(void) { nov_f((nov_string){0}); nov_f((nov_string){NULL,(const unsigned char *)\"A\\0\\303\\251\\360\\237\\230\\200\",8}); " ++
        "if (nov_rt_heap.live_objects) return 8; nov_gc_shutdown(&nov_rt_heap); return 0; }\n" });
    defer test_gpa.free(success);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(success, "typedef int nonempty_translation_unit;", 0, "A\x00é😀", "", mode);
    for ([_][]const u8{ "\\200", "\\300\\200", "\\355\\240\\200", "\\364\\220\\200\\200", "\\360\\237\\230" }) |bytes| {
        const c = try std.fmt.allocPrint(test_gpa, "{s}\nint main(void) {{static const unsigned char bytes[]=\"{s}\"; nov_f((nov_string){{NULL,bytes,sizeof bytes-1}});return 99;}}\n", .{ library, bytes });
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "", "panic: invalid UTF-8 string\n", mode);
    }
}

test "iteration backend rejects sibling cursors and escaped element loans" {
    var module = try Module.init(test_gpa, "bad-iteration-ir.nov", "let f:(flag:bool)={let mut a:[]int=[1]\nlet s=if flag {\"é\"} else {\"😀\"}\nloop r in s {@print(r)}\nloop &x in a {x+=1}\n@println(\"\")} ");
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    const instructions = @constCast(sema.ir.functions.items[0].body.?.instructions);
    var private: Ir.ValueId = undefined;
    var index: Ir.ValueId = undefined;
    var element: Ir.ValueId = undefined;
    for (instructions, 0..) |instruction, i| switch (instruction.op) {
        .branch => |b| {
            private = b.then_value;
        },
        .string_decode => index = @fromBackingInt(@intCast(i)),
        .borrow_start => element = @fromBackingInt(@intCast(i)),
        else => {},
    };
    const cursor = instructions[@backingInt(index)];
    instructions[@backingInt(index)].op.string_decode.array = private;
    try std.testing.expectError(error.InvalidValueId, emit(test_gpa, &sema.ir, null));
    instructions[@backingInt(index)] = cursor;
    const last = instructions[instructions.len - 1];
    instructions[instructions.len - 1].op = .{ .borrow_read = element };
    try std.testing.expectError(error.InvalidValueId, emit(test_gpa, &sema.ir, null));
    instructions[instructions.len - 1] = last;
    try sema.ir.verify();
}

test "iteration loop effects preserve return propagation exhaustion and outer control targets" {
    const source =
        \\let O:(T:type)->type=union{some:T,none,}
        \\let get:(yes:bool)->O(int)=if yes {.some[1]} else {.none[]}
        \\let refs:(yes:bool)->O(int)={let mut a:[]int=[1,2]
        \\    loop &x in a {x+=get(yes).!}
        \\    .some[a[0]+a[1]]}
        \\let values:(yes:bool)->O(string)={loop r in "é😀" {let n=get(yes).!
        \\    if n==1 {return .some["{r}"]}}
        \\    .some["empty"]}
        \\let early:()->int=loop x in {return 7} {}
        \\let copied:()->string={let a=["a"+"b"]
        \\    loop &s in a {return s} else {"empty"}}
        \\let main:()={@println(early(),copied())
        \\    @println(match refs(true){.some=>|n|n
        \\        .none=>0},match refs(false){.some=>|n|n
        \\        .none=>0})
        \\    @println(match values(true){.some=>|s|s
        \\        .none=>"none"},match values(false){.some=>|s|s
        \\        .none=>"none"})
        \\    let mut n:int=0
        \\    loop n<3 : n+=1 {loop x in {continue} {@print("bad")}}
        \\    @println(n)
        \\    let value:int=loop {loop _ in "" {} else {break 8}}
        \\    @println(value)
        \\    let mut a:[]int=[1,2]
        \\    loop &x in a {let mut b:[]int=[3]
        \\        loop &y in b {y+=x
        \\            x+=y}}
        \\}
    ;
    const c = try arrayTestProgram(source);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "7 ab\n5 0\né none\n3\n8\n", "", mode);
}

test "iteration compiler allocation failure rollback covers copies and references" {
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{
        "let main:()={let mut a=[\"a\"+\"b\",\"é\"]\nloop &s in a {s+=\"!\"}\nloop s in a {loop r in s {@print(r)}}}",
    });
}

test "ranges preserve defaults snapshots controls fresh values formatting and GC roots" {
    const c = try arrayTestProgram(range_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, range_output, "", mode);
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
    const terminated = try test_gpa.dupeSentinel(u8, source.written(), 0);
    defer test_gpa.free(terminated);
    const formatted = try Formatter.format(test_gpa, terminated);
    defer test_gpa.free(formatted);
    const c = try emitSource(source.written(), null);
    defer test_gpa.free(c);
    const formatted_c = try emitSource(formatted, null);
    defer test_gpa.free(formatted_c);
    try std.testing.expectEqualStrings(c, formatted_c);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_rt_heap") == null);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "void nov_main(void); int main(void) {nov_main(); return 0;}", 0, output.written(), "", mode);
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
    for ([_]bool{ false, true }) |inclusive| {
        const bounds = try std.mem.replaceOwned(u8, test_gpa, source, "..", if (inclusive) "..=" else "..");
        defer test_gpa.free(bounds);
        const c = try arrayTestProgram(bounds);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, if (inclusive) "012 2 17 18 42\n1234 2 99 0\nmanaged result\n" else "01 2 17 18 42\n123 2 99 0\nmanaged result\n", "", mode);
    }
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
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "7 8 9 ab\n3\n", "", mode);
}

test "range lowering and formatting survive every allocation failure" {
    for ([_][]const u8{ range_program, terminal_range_bounds }) |source|
        try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{source});
}

test "range allocating bodies report runtime OOM" {
    for ([_][]const u8{
        "let main: () = loop i in 0..1 {@println(\"{i}\")}\n",
        "let main: () = loop i in 0..=0 {@println(\"{i}\")}\n",
    }) |source| {
        const c = try arrayTestProgram(source);
        defer test_gpa.free(c);
        const exhausted = try std.mem.concat(test_gpa, u8, &.{
            "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n",
            c,
        });
        defer test_gpa.free(exhausted);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", 1, "", "panic: GC allocation failed\n", mode);
    }
}

const inclusive_range_program =
    \\let mark: (s: string, n: uint) -> uint = {@print(s)
    \\    n}
    \\let early: () -> uint = {loop i in 0..=3 {if i == 1 {return i}}
    \\    99}
    \\let main: () = {
    \\    let mut lo: uint = 0
    \\    let mut hi: uint = 2
    \\    loop i in mark("S", lo)..={lo = 9
    \\        mark("E", hi)} {
    \\        hi = 0
    \\        let text = "i={i}" + "!"
    \\        let a: []uint = [i, i + 1]
    \\        loop j in 0..=2 {
    \\            if j == 1 {continue}
    \\            if j == 2 {break}
    \\            @print(text, a[0])
    \\        }
    \\        continue
    \\    }
    \\    @println("", lo, hi, early())
    \\    loop _ in 2..=1 {@println(false)}
    \\    loop i in -1..=1 {@print(i)}
    \\    loop _ in 0..=0 {@println(" done")}
    \\}
;

test "inclusive ranges preserve snapshots controls fresh bindings formatting and GC roots" {
    const c = try arrayTestProgram(inclusive_range_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "SEi=0! 0i=1! 1i=2! 2 9 0 1\n-101 done\n", "", mode);
}

test "inclusive nested range guards and endpoint controls target the correct owner" {
    const source =
        \\let main: () = {
        \\    let lo: u8 = 254
        \\    let hi: u8 = 255
        \\    loop i in lo..=hi {
        \\        loop _ in hi..=hi {continue}
        \\        loop j in {if i == hi {continue}
        \\            i}..=hi {@println(i, j)
        \\            break}
        \\        @println("body", i)
        \\    }
        \\    let mut hits: uint = 0
        \\    loop i in lo..=hi {
        \\        loop _ in lo..={if i == hi {break}
        \\            hi} {hits += 1}
        \\    }
        \\    @println(hits)
        \\}
    ;
    const c = try emitSource(source, null);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "void nov_main(void); int main(void) {nov_main(); return 0;}", 0, "254 254\nbody 254\n2\n", "", mode);
}

test "inclusive range terminal bounds retain parent control without type hints" {
    const source = try std.mem.replaceOwned(u8, test_gpa, terminal_range_bounds, "..", "..=");
    defer test_gpa.free(source);
    const c = try arrayTestProgram(source);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "7 8 9 ab\n3\n", "", mode);
}

test "inclusive ranges visit all integer edge endpoints once including continue at MAX" {
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
        try source.writer.print("{{let a: {s} = {d}\nlet b: {s} = {d}\nloop i in a..={d} {{@println(i)}}\nloop i in {d}..=b {{@println(i)\ncontinue}}\nloop i in b..=b {{@println(i)\ncontinue}}\nloop _ in b..=a {{@println(false)}}\n}}\n", .{ name, min, name, max, min + 1, max - 1 });
        try output.writer.print("{d}\n{d}\n{d}\n{d}\n{d}\n", .{ min, min + 1, max - 1, max, max });
    }
    const min: i128 = -(@as(i128, 1) << (Ir.native_pointer_bits - 1));
    const max: u128 = (@as(u128, 1) << Ir.native_pointer_bits) - 1;
    try source.writer.print("loop i in -{{{{{d}}}}}..={d} {{@println(i)}}\nloop i in {d}..={d} {{@println(i)\ncontinue}}\nloop i in {d}..={d} {{@println(i)}}\n}}\n", .{ -min, min, max, max, max - 1, max });
    try output.writer.print("{d}\n{d}\n{d}\n{d}\n", .{ min, max, max - 1, max });
    const terminated = try test_gpa.dupeSentinel(u8, source.written(), 0);
    defer test_gpa.free(terminated);
    const formatted = try Formatter.format(test_gpa, terminated);
    defer test_gpa.free(formatted);
    const c = try emitSource(source.written(), null);
    defer test_gpa.free(c);
    const formatted_c = try emitSource(formatted, null);
    defer test_gpa.free(formatted_c);
    try std.testing.expectEqualStrings(c, formatted_c);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_rt_heap") == null);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "void nov_main(void); int main(void) {nov_main(); return 0;}", 0, output.written(), "", mode);
}

test "inclusive range C emission owns all data after source destruction" {
    var sema = owned: {
        var module = try Module.init(test_gpa, "inclusive.nov", inclusive_range_program);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(result.lowering_complete);
        break :owned result;
    };
    defer sema.deinit();
    try sema.ir.verify();
    const c = try emitProgram(test_gpa, &sema.ir, null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, "SEi=0! 0i=1! 1i=2! 2 9 0 1\n-101 done\n", "");
}

test "inclusive range compiler ownership survives every allocation failure" {
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{inclusive_range_program});
    const source = try std.mem.replaceOwned(u8, test_gpa, terminal_range_bounds, "..", "..=");
    defer test_gpa.free(source);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{source});
}

const range_else_program =
    \\let identity: (n: int) -> int = n
    \\let allocate: (n: int) -> []int = [n, n + 1]
    \\let search: (lo: uint, hi: uint, target: uint) -> bool = loop i in lo..hi {
    \\    if i == target {break true}
    \\} else {false}
    \\let early: () -> string = loop _ in 0..2 {return "return"} else {"wrong"}
    \\let outer: () -> int = loop {
    \\    loop _ in 2..1 {} else {break 42}
    \\}
    \\let terminal: () -> int = loop _ in 0..0 {} else {return 7}
    \\let parent: () -> int = loop {loop _ in {break 8}..10 {} else {break 9}}
    \\let main: () = {
    \\    @println(search(0, 3, 2), search(0, 2, 2), search(3, 1, 2), early(), outer(), terminal(), parent())
    \\    let a = loop i in 0..2 {if i == 0 {break 4}
    \\        break identity(5)} else {6}
    \\    let b = loop _ in 0..0 {break 1} else {identity(2)}
    \\    let index = loop i in 0..2 {if i == 0 {break 3}
    \\        break i} else {4}
    \\    let callback = loop _ in 0..0 {break identity} else {identity}
    \\    @println(a, b, index, callback(9))
    \\    let from_index = if true {0} else {loop i in 0..2 {break i} else {1}}
    \\    let inferred_outer = loop {loop _ in 1..0 {} else {break identity(12)}}
    \\    let empty_array = loop _ in 0..1 {break []} else {allocate(1)}
    \\    @println(from_index, inferred_outer, empty_array.len)
    \\    let mut source: []int = [10, 11]
    \\    let mut broken = loop _ in 0..1 {break source} else {allocate(99)}
    \\    let mut exhausted = loop _ in 2..1 {break allocate(99)} else {source}
    \\    broken[0] = 20
    \\    exhausted[0] = 30
    \\    source[0] = 40
    \\    let text = loop _ in 0..0 {break "bad"} else {"fall" + "back"}
    \\    let shared = text
    \\    let allocated = allocate(50)
    \\    @println(source[0], broken[0], exhausted[0], allocated[0], shared, text)
    \\    let subject = match loop _ in 0..1 {break 1} else {identity(2)} {1 => "one"
    \\        _ => "other"}
    \\    @println(subject)
    \\    let mut attempts: int = 0
    \\    loop attempts < 3 : attempts += 1 {
    \\        loop _ in 0..0 {} else {continue}
    \\    }
    \\    @println(attempts)
    \\    loop _ in 0..1 {} else {let i: int = 5
    \\        loop _ in 0..1 {break}
    \\        @println(i)}
    \\}
;

test "range else results infer exactly copy arrays share strings and preserve outer controls" {
    for ([_]bool{ false, true }) |inclusive| {
        // Keep zero-iteration ranges empty in both forms.
        const empty = try std.mem.replaceOwned(u8, test_gpa, range_else_program, "0..0", "1..0");
        defer test_gpa.free(empty);
        const source = try std.mem.replaceOwned(u8, test_gpa, empty, "..", if (inclusive) "..=" else "..");
        defer test_gpa.free(source);
        const c = try arrayTestProgram(source);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, if (inclusive) "true true false return 42 7 8\n4 2 3 9\n0 12 0\n40 20 30 50 fallback fallback\none\n3\n5\n" else "true false false return 42 7 8\n4 2 3 9\n0 12 0\n40 20 30 50 fallback fallback\none\n3\n5\n", "", mode);
    }
}

test "range else distinguishes inclusive MAX exhaustion continue source break and return" {
    var source = std.Io.Writer.Allocating.init(test_gpa);
    defer source.deinit();
    var output = std.Io.Writer.Allocating.init(test_gpa);
    defer output.deinit();
    try source.writer.writeAll("let main: () = {\n");
    for ([_][]const u8{ "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "int", "uint" }) |name| {
        const bits: u7 = if (std.mem.eql(u8, name, "int") or std.mem.eql(u8, name, "uint")) @intCast(Ir.native_pointer_bits) else try std.fmt.parseInt(u7, name[1..], 10);
        const max: i128 = (@as(i128, 1) << @intCast(bits - @as(u7, @intFromBool(name[0] == 'i')))) - 1;
        try source.writer.print("{{let hi: {s} = {d}\nlet mut hits: int = 0\nloop _ in hi..hi {{hits += 100}} else {{hits += 1}}\nloop _ in hi..=hi {{hits += 10\ncontinue}} else {{hits += 1}}\nloop _ in hi..=hi {{break}} else {{hits += 100}}\nlet found = loop i in hi..=hi {{break i}} else {{hi}}\n@println(hits, found)\n}}\n", .{ name, max });
        try output.writer.print("12 {d}\n", .{max});
    }
    try source.writer.writeAll("}\n");
    const c = try emitSource(source.written(), null);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "void nov_main(void); int main(void) {nov_main(); return 0;}", 0, output.written(), "", mode);
}

test "range else ownership and probes survive every allocation failure" {
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{range_else_program});
    const inclusive = try std.mem.replaceOwned(u8, test_gpa, range_else_program, "..", "..=");
    defer test_gpa.free(inclusive);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{inclusive});
}

test "range else emitted results outlive source and report allocation failure" {
    var sema = owned: {
        var module = try Module.init(test_gpa, "range-else.nov", range_else_program);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(result.lowering_complete);
        break :owned result;
    };
    defer sema.deinit();
    try sema.ir.verify();
    const c = try emitProgram(test_gpa, &sema.ir, null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, "true false false return 42 7 8\n4 2 3 9\n0 12 0\n40 20 30 50 fallback fallback\none\n3\n5\n", "");
    for ([_][]const u8{
        "let main: () = @println(loop _ in 0..0 {break \"unused\"} else {\"a\" + \"b\"})",
        "let main: () = {let a: []bool = loop _ in 1..=0 {break [true]} else {[false]}\n@println(a[0])}",
    }) |source| {
        const generated = try arrayTestProgram(source);
        defer test_gpa.free(generated);
        const exhausted = try std.mem.concat(test_gpa, u8, &.{ "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", generated });
        defer test_gpa.free(exhausted);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", 1, "", "panic: GC allocation failed\n", mode);
    }
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
const string_interpolation_output = "Atrue:false:0:{é\x00😀}:Z 3 true/false\ntrue true 3\ntrue 6\n[1]=1\n[2]=2\n[3]=Fizz\n[4]=4\n[5]=Buzz\n[6]=Fizz\n[7]=7\n[8]=8\n[9]=Fizz\n[10]=Buzz\n[11]=11\n[12]=Fizz\n[13]=13\n[14]=14\n[15]=FizzBuzz\n16 0 number=9 é\x00😀\n";

test "identifier interpolation preserves chunks formatting roots loops matches and returns" {
    const c = try arrayTestProgram(string_interpolation);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, string_interpolation_output, "", mode);
}

const inferred_global_program =
    \\let enabled = true
    \\let disabled = @as(Flag, @as(bool, false))
    \\let prefix = "hé\0"
    \\let suffix = @as(string, "😀")
    \\let count = @as(Count, @as(int, -42))
    \\let Count = int
    \\let Flag = bool
    \\let choose: (b: Flag) -> string = match b {
    \\    true => prefix
    \\    false => suffix
    \\}
    \\let main: () -> void = {
    \\    @println(enabled, disabled, count < 0, count == -42)
    \\    @println("{prefix}{suffix}:{count}:{enabled}", choose(disabled), choose(enabled))
    \\}
;

const annotated_global_program =
    \\let inferred_bool = @as(Flag, true)
    \\let annotated_bool: Flag = @as(bool, @as(Flag, true))
    \\let inferred_int = @as(Count, -42)
    \\let annotated_int: Count = @as(int, @as(Count, -42))
    \\let inferred_string = @as(Text, "hé\0")
    \\let annotated_string: Text = @as(string, @as(Text, "hé\0"))
    \\let Flag = bool
    \\let Count = int
    \\let Text = string
    \\let main: () -> void = {
    \\    @println(inferred_bool, annotated_bool, inferred_int, annotated_int)
    \\    @println("{inferred_string}:{annotated_string}")
    \\}
;

test "annotated scalar ascriptions match inferred runtime values formatting and forced GC" {
    const c = try arrayTestProgram(annotated_global_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "true true -42 -42\nhé\x00:hé\x00\n", "", mode);
}

test "annotated scalar ascriptions survive every compiler allocation failure" {
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{annotated_global_program});
}

test "inferred global scalars inline into printing interpolation and boolean matches" {
    const c = try arrayTestProgram(inferred_global_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "true false true true\nhé\x00😀:-42:true 😀 hé\x00\n", "", mode);
}

test "inferred global scalars survive every compiler allocation failure" {
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{inferred_global_program});
}

test "inferred global scalars preserve all native and fixed integer boundaries" {
    var source = std.Io.Writer.Allocating.init(test_gpa);
    defer source.deinit();
    var output = std.Io.Writer.Allocating.init(test_gpa);
    defer output.deinit();
    const names = [_][]const u8{ "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "int", "uint" };
    for (names) |name| {
        const bits: u7 = if (std.mem.eql(u8, name, "int") or std.mem.eql(u8, name, "uint")) @intCast(Ir.native_pointer_bits) else try std.fmt.parseInt(u7, name[1..], 10);
        const signed = name[0] == 'i';
        const min: i128 = if (signed) -(@as(i128, 1) << @intCast(bits - 1)) else 0;
        const max: i128 = (@as(i128, 1) << @intCast(bits - @as(u7, @intFromBool(signed)))) - 1;
        try source.writer.print("let min_{s} = @as({s}, {d})\nlet max_{s} = @as({s}, {d})\n", .{ name, name, min, name, name, max });
        try output.writer.print("{d} {d}\n", .{ min, max });
    }
    try source.writer.writeAll("let main: () -> void = {\n");
    for (names) |name| try source.writer.print("@println(min_{s}, max_{s})\n", .{ name, name });
    try source.writer.writeAll("}\n");
    const c = try emitSource(source.written(), null);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "void nov_main(void); int main(void) { nov_main(); return 0; }", 0, output.written(), "", mode);
}

test "inferred global scalars emit literal strings after releasing source" {
    var module = try Module.init(test_gpa, "owned-global.nov", "let text = \"hé\\0\\{ok\\}😀\"\nlet wrapped: string = @as(string, \"literal\")\nlet main: () -> void = @println(text, wrapped)");
    var sema = Lowering.lower(&module) catch |err| {
        module.deinit();
        return err;
    };
    const failed = module.hasErrors();
    module.deinit();
    defer sema.deinit();
    try std.testing.expect(!failed);
    try std.testing.expect(sema.lowering_complete);
    const c = try emitProgram(test_gpa, &sema.ir, null);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "hé\x00{ok}😀 literal\n", "", mode);
}

test "inferred global scalars retain the existing 128 bit backend rejection" {
    try std.testing.expectError(error.UnsupportedIntegerWidth, emitSource("let n = @as(i128, -170141183460469231731687303715884105728)\nlet f: () -> i128 = n", null));
    try std.testing.expectError(error.UnsupportedIntegerWidth, emitSource("let n = @as(u128, 340282366920938463463374607431768211455)\nlet f: () -> u128 = n", null));
}

const type_alias_program =
    \\let Count = Forward
    \\let Forward = uint
    \\let Counts = []Count
    \\let Callback = (Count) -> Count
    \\let Text = string
    \\let Unit = void
    \\let Flag = bool
    \\let increment: (value: Count) -> Count = {@print("E")
    \\    value + 1}
    \\let apply: (callback: Callback, values: Counts) -> Count = callback(values[0])
    \\let main: () -> Unit = {
    \\    let mut values: Counts = [7]
    \\    let mut copy = @as(Counts, values)
    \\    copy[0] = 9
    \\    let callback = @as(Callback, increment)
    \\    let text: Text = "alias" + "es"
    \\    @println(apply(callback, values), copy[0], "{@as(Count, 10)}", @as(Flag, true), text)
    \\}
;

test "type aliases preserve callbacks array copies effects formatting and forced GC" {
    const c = try arrayTestProgram(type_alias_program);
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_Count") == null);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_Callback") == null);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "E8 9 10 true aliases\n", "", mode);
}

test "type aliases retain existing backend width diagnostics" {
    try std.testing.expectError(error.UnsupportedIntegerWidth, emitSource("let Wide = i128\nlet f: (x: Wide) -> Wide = x", null));
    // An unused alias creates no runtime object or backend requirement.
    const c = try emitSource("let Wide = i128\nlet f: () -> int = 1", null);
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_Wide") == null);
}

test "type aliases compiler ownership survives every allocation failure" {
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{type_alias_program});
}

test "type aliases preserve every supported integer boundary" {
    var source = std.Io.Writer.Allocating.init(test_gpa);
    defer source.deinit();
    var output = std.Io.Writer.Allocating.init(test_gpa);
    defer output.deinit();
    const names = [_][]const u8{ "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "int", "uint" };
    for (names) |name| try source.writer.print("let Alias_{s} = {s}\n", .{ name, name });
    try source.writer.writeAll("let main: () -> void = {\n");
    for (names) |name| {
        const bits: u7 = if (std.mem.eql(u8, name, "int") or std.mem.eql(u8, name, "uint")) @intCast(Ir.native_pointer_bits) else try std.fmt.parseInt(u7, name[1..], 10);
        const signed = name[0] == 'i';
        const min: i128 = if (signed) -(@as(i128, 1) << @intCast(bits - 1)) else 0;
        const max: i128 = (@as(i128, 1) << @intCast(bits - @as(u7, @intFromBool(signed)))) - 1;
        try source.writer.print("@println(@as(Alias_{s}, {d}), @as(Alias_{s}, {d}))\n", .{ name, min, name, max });
        try output.writer.print("{d} {d}\n", .{ min, max });
    }
    try source.writer.writeAll("}\n");
    const c = try emitSource(source.written(), null);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "void nov_main(void); int main(void) { nov_main(); return 0; }", 0, output.written(), "", mode);
}

const ascription_program =
    \\let id: (n: int) -> int = {@print("E")
    \\    n}
    \\let early: () -> int = @as(bool, {return 7})
    \\let main: () -> void = {
    \\    @println("{@as(int, 1)}", @as(uint, 1))
    \\    let f = @as((int) -> int, id)
    \\    @println(@as(int, f(2)), 3 |> @as((int) -> int, id), early())
    \\    let mut a = @as([]int, [1, 2])
    \\    let mut b = @as([]int, a)
    \\    b[0] = 9
    \\    @println(a[0], b[0], @as([]int, []).len)
    \\    let text = "hé" + "!"
    \\    @println("{@as(string, text)}", @as(bool, true))
    \\    @println(@as(int, match true {true => 4
    \\        _ => 5}), @as(int, loop {break 6}))
    \\    loop {
    \\        @as(void, {break})
    \\    }
    \\    let mut n = @as(int, 0)
    \\    loop n < 1 : n += 1 {@as(void, {continue})}
    \\    @as(void, @println(n))
    \\}
;

test "ascription preserves exact values effects array copies terminals formatting and forced GC" {
    const c = try arrayTestProgram(ascription_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "1 1\nEE2 3 7\n1 9 0\nhé! true\n4 6\n1\n", "", mode);
}

test "ascription compiler ownership survives every allocation failure" {
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{ascription_program});
}

test "ascribed interpolation prints every supported integer boundary" {
    var source = std.Io.Writer.Allocating.init(test_gpa);
    defer source.deinit();
    var output = std.Io.Writer.Allocating.init(test_gpa);
    defer output.deinit();
    try source.writer.writeAll("let main: () -> void = {\n");
    for ([_][]const u8{ "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "int", "uint" }) |name| {
        const bits: u7 = if (std.mem.eql(u8, name, "int") or std.mem.eql(u8, name, "uint")) @intCast(Ir.native_pointer_bits) else try std.fmt.parseInt(u7, name[1..], 10);
        const signed = name[0] == 'i';
        const min: i128 = if (signed) -(@as(i128, 1) << @intCast(bits - 1)) else 0;
        const max: i128 = (@as(i128, 1) << @intCast(bits - @as(u7, @intFromBool(signed)))) - 1;
        try source.writer.print("@println(\"{{@as({s}, {d})}}:{{@as({s}, {d})}}\")\n", .{ name, min, name, max });
        try output.writer.print("{d}:{d}\n", .{ min, max });
    }
    try source.writer.writeAll("}\n");
    const c = try arrayTestProgram(source.written());
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, output.written(), "", mode);
}

const quoted_interpolation_program =
    \\let greet:(name:string)->string="Hello {name}!"
    \\let main:()={
    \\@println("{"a"}","{greet("Ada")}","{'{'}{'}'}", "{'é'}")
    \\@println("Nested: {"hello {"world"}"}","{"\u{1f600}\0"}")
    \\@println("Escapes: {"\""} {"\\"}")
    \\let mut count=0
    \\let next=(text:string)->string{count+=1
    \\"{count}:{text}"}
    \\@println("{next("a")}/{next("nested {next("b")}")}",count)
    \\}
;

fn emitQuotedInterpolationSourceFree(allocator: std.mem.Allocator) ![]u8 {
    var sema = blk: {
        var module = try Module.init(allocator, "quoted-interpolation.nov", quoted_interpolation_program);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(result.lowering_complete);
        break :blk result;
    };
    defer sema.deinit();
    return emit(allocator, &sema.ir, null);
}

fn testQuotedInterpolationOwnership(allocator: std.mem.Allocator) !void {
    allocator.free(try emitQuotedInterpolationSourceFree(allocator));
}

test "quoted interpolation owns nested source and survives every compiler allocation failure" {
    try testQuotedInterpolationOwnership(test_gpa);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testQuotedInterpolationOwnership, .{});
}

test "quoted interpolation executes nested escapes runes and ordered calls with forced GC O0 O3" {
    const library = try emitQuotedInterpolationSourceFree(test_gpa);
    defer test_gpa.free(library);
    const c = try collectingTestProgram(library);
    defer test_gpa.free(c);
    const output = "a Hello Ada! {} é\nNested: hello world 😀\x00\nEscapes: \" \\\n1:a/3:nested 2:b 3\n";
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty;", 0, output, "", mode);
}

test "expression interpolation reuses calls scopes ordering and rooted temporaries" {
    const c = try arrayTestProgram(
        \\let echo: (n: int) -> string = {@print(n)
        \\    if n == 0 {return "zero"}
        \\    "{n}" + "!"}
        \\let argument: (n: int) -> int = {@print("A")
        \\    n}
        \\let factory: () -> (int) -> string = {@print("C")
        \\    echo}
        \\let join: (a: string, b: string) -> string = a + b
        \\let main: () -> void = {
        \\    let n: int = 2
        \\    let bytes = "é\0😀"
        \\    @println("{factory()(argument(1))}/{echo(argument(n + 1))}:{bytes}", echo(4))
        \\    @println("{join(echo(5), echo(6))}", "{ n + 1 }", "{bytes.len}")
        \\    let a = [true, false]
        \\    @println("{a[0]}:{true and false}")
        \\    let zero: int = 0
        \\    loop i in zero..2 {
        \\        loop j in zero..1 {
        \\            @println("{echo(i + j)}")
        \\        }
        \\    }
        \\}
    );
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "CA1A341!/3!:é\x00😀 4!\n565!6! 3 3\ntrue:false\n0zero\n11!\n", "", mode);
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
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, output.written(), "", mode);
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
        .{ .source = "let make: (n: int) -> string = \"{n}\"\nlet main: () -> void = @println(\"{make(42)}\")" },
        .{ .source = "let yes: () -> bool = true\nlet main: () -> void = @println(\"{yes()}\")", .output = "true\n", .allocates = false },
        .{ .source = "let main: () -> void = {let s = \"é\\0\"\n@println(\"{s}\", \"{false}\")}", .output = "é\x00 false\n", .allocates = false },
    }) |case| {
        const c = try arrayTestProgram(case.source);
        defer test_gpa.free(c);
        const exhausted = try std.mem.concat(test_gpa, u8, &.{
            "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n",
            c,
        });
        defer test_gpa.free(exhausted);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", if (case.allocates) 1 else 0, case.output, if (case.allocates) "panic: GC allocation failed\n" else "", mode);
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
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, string_equality_output, "", mode);
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
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "", "", mode);
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
const string_compound_output = "hé\x00hé\x00 6 hé\x00 3\nhé\x00hé\x00😀\x00 hé\x00\ncontinue!tail!\nbreak! 3 returned old!tail hé\x00\né 1\n";

test "string compounds capture before RHS and preserve aliases terminal effects and GC roots" {
    const c = try arrayTestProgram(string_compound);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, string_compound_output, "", mode);
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

test "generated string and rune programs compile for Windows with binary UTF8 output" {
    const zig_exe = std.testing.environ.getAlloc(test_gpa, "NOV_TEST_ZIG") catch try test_gpa.dupe(u8, "zig");
    defer test_gpa.free(zig_exe);
    for ([_][]const u8{ string_example, string_interpolation, rune_program, closures_program, closure_types_program }) |source| {
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
    for ([_][]const u8{ string_example, string_compound, string_equality, string_interpolation }) |source| try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{source});
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
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "", "panic: string size overflow\n", mode);
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
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, inferred_output, "", mode);
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
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, inferred_arrays_output, "", mode);
}

test "scalar arrays copy grow and return with every allocation collecting" {
    const c = try arrayTestProgram(array_example);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| {
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
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, output.written(), "", mode);
}

test "struct field compound arithmetic wraps at every approved width" {
    var source = std.Io.Writer.Allocating.init(test_gpa);
    defer source.deinit();
    var output = std.Io.Writer.Allocating.init(test_gpa);
    defer output.deinit();
    for ([_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "int", "uint" }) |name| try source.writer.print("let R_{s}=struct{{x:{s},}}\n", .{ name, name });
    try source.writer.writeAll("let main: () -> void = {\n");
    for ([_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "int", "uint" }) |name| {
        const signed = name[0] == 'i';
        const bits = if (std.mem.eql(u8, name, "int") or std.mem.eql(u8, name, "uint")) Ir.native_pointer_bits else try std.fmt.parseInt(u16, name[1..], 10);
        const maximum = (@as(u128, 1) << @intCast(bits - @as(u16, if (signed) 1 else 0))) - 1;
        try source.writer.print("{{let mut a = R_{s}[.x = {d}]\na.x += 1\n@println(a.x)\na.x -= 1\n@println(a.x)\na.x *= 2\n@println(a.x)\n", .{ name, maximum });
        if (signed) {
            try output.writer.print("-{d}\n{d}\n-2\n", .{ maximum + 1, maximum });
            try source.writer.print("a.x = -{d}\na.x /= -1\n@println(a.x)\na.x %= -1\n@println(a.x)\n", .{maximum + 1});
            try output.writer.print("-{d}\n0\n", .{maximum + 1});
            try source.writer.writeAll("a.x = -7\na.x /= 3\n@println(a.x)\na.x = -7\na.x %= 3\n@println(a.x)}\n");
            try output.writer.writeAll("-2\n-1\n");
        } else {
            try output.writer.print("0\n{d}\n{d}\n", .{ maximum, maximum - 1 });
            try source.writer.writeAll("a.x = 7\na.x /= 3\n@println(a.x)\na.x = 7\na.x %= 3\n@println(a.x)}\n");
            try output.writer.writeAll("2\n1\n");
        }
    }
    try source.writer.writeAll("}");
    const c = try arrayTestProgram(source.written());
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, output.written(), "", mode);
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
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, array_compound_output, "", mode);
}

test "array compound bounds precede RHS while plain replacement bounds follow RHS" {
    for ([_][]const u8{ "0", "1", "-2", if (Ir.native_pointer_bits == 64) "-9223372036854775808" else "-2147483648" }) |index| {
        for ([_][]const u8{ "=", "+=", "-=", "*=", "/=", "%=" }) |op| {
            const source = try std.fmt.allocPrint(test_gpa, "let main: () -> void = {{let mut a: []int = {s}\na[{{@print(true)\n0 + {s}}}] {s} {{@print(false)\nreturn}}}}", .{ if (std.mem.eql(u8, index, "0")) "[]" else "[1]", index, op });
            defer test_gpa.free(source);
            const c = try arrayTestProgram(source);
            defer test_gpa.free(c);
            const plain = std.mem.eql(u8, op, "=");
            for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", if (plain) 0 else 1, if (plain) "truefalse" else "true", if (plain) "" else "panic: array index out of bounds\n", mode);
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
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "true", if (std.mem.eql(u8, op, "/=")) "panic: division by zero\n" else "panic: remainder by zero\n", mode);
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
    const expected = try std.mem.join(test_gpa, "", &@as([10][]const u8, @splat("2 1 2\n")));
    defer test_gpa.free(expected);
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, expected, "");
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
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "", "panic: array index out of bounds\n", mode);
    }
}

test "array capacity and append length overflow panic before accessing backing" {
    const library = try emitSource("let anchor: (unused: []bool) -> void = {}", null);
    defer test_gpa.free(library);
    for ([_][]const u8{
        "(void)nov_array_new(UINTPTR_MAX, UINTPTR_MAX, &(nov_gc_descriptor){sizeof(uint64_t), _Alignof(uint64_t), 0, NULL});",
        "(void)nov_array_append((nov_array){NULL, UINTPTR_MAX, UINTPTR_MAX}, (nov_array){NULL, 1, 1}, &(nov_gc_descriptor){1, 1, 0, NULL});",
    }) |operation| {
        const c = try std.fmt.allocPrint(test_gpa, "{s}\nint main(void) {{{s} return 99;}}\n", .{ library, operation });
        defer test_gpa.free(c);
        try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 1, "", "panic: array size overflow\n");
    }
}

test "array allocation failure retries preserve roots and shutdown frees backing" {
    for ([_][]const u8{ array_example, array_compound_effects, inferred_example, inferred_arrays, string_example, string_compound, string_equality, unconditional_loops, else_loops, array_membership, string_interpolation, record_program, field_mutation, managed_array_program, array_fields_program }, [_][]const u8{ "3 30 30 6\n454 15\n99 15 4\nfalse true 2\n0\n", array_compound_output, inferred_output, inferred_arrays_output, string_output, string_compound_output, string_equality_output, unconditional_output, else_loop_output, array_membership_output, string_interpolation_output, record_output, field_mutation_output, managed_array_output, array_fields_output }) |source, output| {
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
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(injected, "typedef int nonempty_translation_unit;", 0, output, "", mode);
        const exhausted = try std.mem.concat(test_gpa, u8, &.{
            "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n" ++
                "#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n",
            c,
        });
        defer test_gpa.free(exhausted);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", 1, if (std.mem.eql(u8, source, inferred_example)) "7\n8\n" else if (std.mem.eql(u8, source, inferred_arrays)) "7\n" else if (std.mem.eql(u8, source, else_loops)) "false true false\n42 42 5\nexhausted 5 3 3\ncondition continuation body\n10 11 12 14\n3 7\n31 32 3\ntrue\n" else if (std.mem.eql(u8, source, record_program)) "NS" else if (std.mem.eql(u8, source, field_mutation)) "A13 7 10\n" else "", "panic: GC allocation failed\n", mode);
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
    for ([_][]const u8{ array_example, array_compound_effects, inferred_example, inferred_arrays }) |source| try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{source});
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
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
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
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(c, "void nov_main(void);\nint main(void) { nov_main(); return 0; }", 0, "7\n", "", mode);
    }
}

test "conditional loop else results exhaust break return and target parent controls" {
    const c = try arrayTestProgram(else_loops);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
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
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
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
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
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
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "", "", mode);
}

test "loop continuations and value results execute before formatting" {
    for ([_][]const u8{ continuation_loops, value_loops, else_loops }, [_][]const u8{ continuation_output, value_loop_output, else_loop_output }) |source, output| {
        const library = try emitSource(source, null);
        defer test_gpa.free(library);
        const c = try collectingTestProgram(library);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, output, "", mode);
    }
}

test "continuation and value loop lowering and emission survive every allocation failure" {
    for ([_][]const u8{ continuation_loops, value_loops, else_loops }) |source|
        try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{source});
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
    \\let dead: () -> void = loop {continue}
    \\let main: () -> void = {exit()
    \\    @println(build(true))
    \\    @println(build(false))
    \\    loop {true} {loop {break}
    \\        break}}
;
const unconditional_output = "ab\n3 2\nabab\n";

test "unconditional loops execute finite controls returns and managed roots after formatting" {
    const formatted = try Formatter.format(test_gpa, unconditional_loops);
    defer test_gpa.free(formatted);
    for ([_][]const u8{ unconditional_loops, formatted }) |source| {
        const c = try arrayTestProgram(source);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
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
        \\let condition: () -> int = loop {return 7} {}
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
    try TestAllocations.checkAllAllocationFailures(test_gpa, testLoopAllocationFailures, .{});
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{unconditional_loops});
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
    try TestAllocations.checkAllAllocationFailures(test_gpa, testMutableAllocationFailures, .{});
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
    try TestAllocations.checkAllAllocationFailures(test_gpa, testReturnAllocationFailures, .{});
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
    try TestAllocations.checkAllAllocationFailures(test_gpa, testShortCircuitAllocationFailures, .{});
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
    try TestAllocations.checkAllAllocationFailures(test_gpa, testPrintingEmissionAllocationFailures, .{});
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
    // Fortified libc may define fprintf as a macro; enable fortification here
    // so replacing that entry also exercises removal of the libc definition.
    const Case = struct { prefix: []const u8, stdout: []const u8 };
    for ([_]Case{
        .{ .prefix = "#include <stdio.h>\nstatic int fail_puts(const char *s, FILE *f) { (void)s; (void)f; return EOF; }\n#define fputs fail_puts\n", .stdout = "" },
        .{ .prefix = "#include <stdio.h>\nstatic int fail_putc(int c, FILE *f) { (void)c; (void)f; return EOF; }\n#define fputc fail_putc\n", .stdout = "true" },
        .{ .prefix = "#include <stdio.h>\n#include <stdarg.h>\nstatic int fail_printf(FILE *f, const char *fmt, ...) { if (f == stdout) return -1; va_list args; va_start(args,fmt); int n = vfprintf(f,fmt,args); va_end(args); return n; }\n#undef fprintf\n#define fprintf fail_printf\n", .stdout = "true " },
        .{ .prefix = "#include <stdio.h>\nstatic int fail_flush(FILE *f) { if (f == stdout) return EOF; return fflush(f); }\n#define fflush fail_flush\n", .stdout = "true 42\n" },
    }) |case| {
        const injected = try std.mem.concat(test_gpa, u8, &.{ "#ifndef _FORTIFY_SOURCE\n#define _FORTIFY_SOURCE 2\n#endif\n", case.prefix, program });
        defer test_gpa.free(injected);
        try compileAndRunExpected(injected, "typedef int nonempty_translation_unit;\n", 1, case.stdout, "panic: stdout write failed\n");
    }
    if (builtin.os.tag == .linux) {
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

test "source-qualified functions share pools without entrypoint or C symbol collisions" {
    var ir = Ir.init(test_gpa);
    defer ir.deinit();
    try ir.source_lengths.appendSlice(test_gpa, &.{ 1, 1 });
    const unit = try ir.internType(.void);
    const signature = try ir.internType(.{ .function = .{ .params = &.{}, .result = unit } });
    const imported_span: Ir.Span = .{ .source = @fromBackingInt(@intCast(1)), .start = 0, .end = 1 };
    const root_span: Ir.Span = .{ .start = 0, .end = 1 };
    const imported = try ir.addFunction("main", signature, imported_span);
    try ir.setFunctionBody(imported, &.{.{ .ty = unit, .span = imported_span, .op = .{ .return_value = null } }}, @fromBackingInt(@intCast(0)));
    try std.testing.expectError(error.MissingEntrypoint, emitProgram(test_gpa, &ir, null));
    const root = try ir.addFunction("main", signature, root_span);
    try ir.setFunctionBody(root, &.{.{ .ty = unit, .span = root_span, .op = .{ .return_value = null } }}, @fromBackingInt(@intCast(0)));
    const source = try emitProgramRoot(test_gpa, &ir, root, root_span, null);
    defer test_gpa.free(source);
    try std.testing.expect(std.mem.indexOf(u8, source, "void nov_M1_nov_main(void)") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "int main(void) {\n    nov_main();") != null);
    try compileAndRun(source, "typedef int nonempty_translation_unit;");
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
    try std.testing.expectEqual(@as(u32, 0), @backingInt(diagnostic.function.?));
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
    try ir.setFunctionBody(function, &.{.{ .ty = void_type, .span = .{ .start = 0, .end = 0 }, .op = .{ .constant = unit } }}, @fromBackingInt(@intCast(0)));
    const c = try emit(test_gpa, &ir, null);
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "void nov_f(void) {\n}\n") != null);

    const odd_function_type = try ir.internType(.{ .function = .{ .params = &.{odd}, .result = void_type } });
    const odd_function = try ir.addFunction("g", odd_function_type, .{ .start = 0, .end = 0 });
    try ir.setFunctionBody(odd_function, &.{.{ .ty = void_type, .span = .{ .start = 0, .end = 0 }, .op = .{ .constant = unit } }}, @fromBackingInt(@intCast(0)));
    var diagnostic: Diagnostic = undefined;
    try std.testing.expectError(error.UnsupportedIntegerWidth, emit(test_gpa, &ir, &diagnostic));
    try std.testing.expectEqual(@as(u32, 1), @backingInt(diagnostic.function.?));
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
    try TestAllocations.checkAllAllocationFailures(test_gpa, Case.run, .{
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
    return compileAndRunMode(c_source, harness, exit_code, stdout, stderr, "-O3");
}

fn compileAndRunMode(c_source: []const u8, harness: []const u8, exit_code: u8, stdout: []const u8, stderr: []const u8, mode: []const u8) !void {
    // `zig build test` provides the pinned toolchain's `zig cc`; otherwise use `cc`.
    const zig_exe = std.testing.environ.getAlloc(test_gpa, "NOV_TEST_ZIG") catch null;
    defer if (zig_exe) |path| test_gpa.free(path);
    return compileAndRunCompiler(c_source, harness, exit_code, stdout, stderr, mode, if (zig_exe) |path| &.{ path, "cc" } else &.{"cc"}, &.{});
}

fn compileAndRunCompiler(c_source: []const u8, harness: []const u8, exit_code: u8, stdout: []const u8, stderr: []const u8, mode: []const u8, compiler: []const []const u8, extra_flags: []const []const u8) !void {
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "generated.c", .data = c_source });
    try tmp.dir.writeFile(io, .{ .sub_path = "harness.c", .data = harness });

    const program = "program" ++ (if (builtin.os.tag == .windows) ".exe" else "");
    const flags = [_][]const u8{ "-std=c11", mode, "-fsanitize=undefined", "-fno-sanitize-recover=all", "-Wall", "-Wextra", "-Wpedantic", "-Werror", "generated.c", "harness.c", "-o", program };
    var argv: std.ArrayList([]const u8) = .empty;
    defer argv.deinit(test_gpa);
    try argv.appendSlice(test_gpa, compiler);
    try argv.appendSlice(test_gpa, extra_flags);
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

test "assert eager message evaluation exact bytes source ownership and terminal operands" {
    const cases = [_]struct { source: []const u8, code: u8 = 0, output: []const u8, diagnostic: []const u8 = "" }{
        .{ .source = "let condition:()->bool={@print(\"C\")\ntrue}\nlet message:()->string={@print(\"M\")\n\"unused\"+\" message\"}\nlet main:()={@assert(condition(),message())\n@print(\"T\")}", .output = "CMT" },
        .{ .source = "let main:()={@print(\"before\")\n@assert(false,\"hé\\0\"+\"!\")}", .code = 1, .output = "before", .diagnostic = "test.nov:2:1: assertion failed: hé\x00!\n" },
        .{ .source = "let main:()={\nlet mut n:int=0\nlet r=&n\n@assert(false,\"{r}\")\nn=1\n}", .code = 1, .output = "", .diagnostic = "test.nov:4:1: assertion failed: 0\n" },
        .{ .source = "let main:()={\nlet mut s=\"a\"+\"b\"\nlet r=&s\n@assert(false,{@print(r)\n\"message:\"+r})\ns=\"after\"\n}", .code = 1, .output = "ab", .diagnostic = "test.nov:4:1: assertion failed: message:ab\n" },
        .{ .source = "let f:(flag:bool)={let mut n:int=0\nlet r=&n\nif flag {n=1\n@assert(true)} else {@print(r)}\n@print(n)}\nlet main:()={f(true)\nf(false)}", .output = "100" },
        .{ .source = "let main:()=@assert(false)", .code = 1, .output = "", .diagnostic = "test.nov:1:13: assertion failed\n" },
        .{ .source = "let main:()={@assert({return},@panic(\"unreached\"))}", .output = "" },
        .{ .source = "let main:()=@assert(false,{@print(\"M\")\nreturn})", .output = "M" },
        .{ .source = "let main:()={let mut n:int=0\nlet r=&n\n@assert(r==0,{n=1\n\"snapshot\"})\n@assert(n==1)}", .output = "" },
        .{ .source = "let main:()={let mut n:int=0\nloop n<3 {let r=&n\n@assert(r<3,\"iteration\")\nr+=1}\n@assert(n==3)\nif true {@assert(true)} else {@assert(false)}}", .output = "" },
    };
    for (cases) |case| {
        var lowered = blk: {
            var module = try Module.init(test_gpa, "test.nov", case.source);
            defer module.deinit();
            var result = try Lowering.lower(&module);
            errdefer result.deinit();
            try std.testing.expect(!module.hasErrors());
            break :blk result;
        };
        defer lowered.deinit();
        const c = try emitProgram(test_gpa, &lowered.ir, null);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", case.code, case.output, case.diagnostic, mode);
    }
}

test "assert and test driver emission survive allocation failures and source release" {
    try TestAllocations.checkAllAllocationFailures(test_gpa, testAssertionEmissionAllocations, .{});
}

fn testAssertionEmissionAllocations(allocator: std.mem.Allocator) !void {
    var lowered = blk: {
        var module = try Module.init(allocator, "assert.nov", "@[test]\nlet check:()=@assert(true,\"owned\"+\" message\")");
        defer module.deinit();
        module.tests_enabled = true;
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        break :blk result;
    };
    defer lowered.deinit();
    const c = try emitTestProgram(allocator, &lowered.ir, &.{@fromBackingInt(0)}, null);
    defer allocator.free(c);
}

test "test executable selects exactly one test and rejects invalid selections" {
    var ir = Ir.init(test_gpa);
    defer ir.deinit();
    const unit = try ir.internType(.void);
    const signature = try ir.internType(.{ .function = .{ .params = &.{}, .result = unit } });
    const value = try ir.internConstant(.{ .ty = unit, .value = .void });
    const id = try ir.addFunction("check", signature, .{ .start = 0, .end = 0 });
    ir.functions.items[@backingInt(id)].is_test = true;
    try ir.setFunctionBody(id, &.{.{ .ty = unit, .span = .{ .start = 0, .end = 0 }, .op = .{ .constant = value } }}, @fromBackingInt(0));
    try std.testing.expectError(error.InvalidEntrypoint, emitProgramRoot(test_gpa, &ir, id, .{ .start = 0, .end = 0 }, null));
    try std.testing.expectError(error.InvalidTestEntrypoint, emitTestProgram(test_gpa, &ir, &.{ id, id }, null));
    try std.testing.expectError(error.InvalidTestEntrypoint, emitTestProgram(test_gpa, &ir, &.{@fromBackingInt(999)}, null));
    const string = try ir.internType(.string);
    const text = try ir.internConstant(.{ .ty = string, .value = .{ .string = "once" } });
    const second = try ir.addFunction("second", signature, .{ .start = 0, .end = 0 });
    ir.functions.items[@backingInt(second)].is_test = true;
    try ir.setFunctionBody(second, &.{
        .{ .ty = string, .span = .{ .start = 0, .end = 0 }, .op = .{ .constant = text } },
        .{ .ty = unit, .span = .{ .start = 0, .end = 0 }, .op = .{ .print = .{ .args = &.{@fromBackingInt(0)}, .newline = false } } },
    }, @fromBackingInt(1));
    const c = try emitTestProgram(test_gpa, &ir, &.{ id, second }, null);
    defer test_gpa.free(c);
    const renamed = try std.mem.concat(test_gpa, u8, &.{ "#define main test_driver\n", c, "\n#undef main\nint main(void) { char *good[] = {\"driver\", \"0\"}; char *bad[] = {\"driver\", \"00\"}; char *second[] = {\"driver\", \"1\"}; if (test_driver(2, good) || test_driver(2, second)) return 1; if (test_driver(2, bad) != 2 || test_driver(1, good) != 2) return 1; return 0; }\n" });
    defer test_gpa.free(renamed);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(renamed, "typedef int nonempty_translation_unit;", 0, "once", "", mode);
    const empty = try emitTestProgram(test_gpa, &ir, &.{}, null);
    defer test_gpa.free(empty);
    try compileAndRunExpected(empty, "typedef int nonempty_translation_unit;", 2, "", "");
}

fn externTestProgram(source: []const u8) ![]u8 {
    const terminated = try test_gpa.dupeSentinel(u8, source, 0);
    defer test_gpa.free(terminated);
    const formatted = try Formatter.format(test_gpa, terminated);
    defer test_gpa.free(formatted);
    const original = try emitSource(source, null);
    defer test_gpa.free(original);
    const library = try emitSource(formatted, null);
    defer test_gpa.free(library);
    try std.testing.expectEqualStrings(original, library);
    var module = try Module.init(test_gpa, "extern.nov", formatted);
    defer module.deinit();
    var result = try Lowering.lower(&module);
    defer result.deinit();
    try std.testing.expect(!module.hasErrors());
    return emitProgram(test_gpa, &result.ir, null);
}

const extern_program =
    \\@[extern("foreign_add")]
    \\let add: (x: i32, y: i32) -> i32
    \\@[extern("foreign_add")]
    \\let alias: (x: i32, y: i32) -> i32
    \\@[extern]
    \\let foreign_unit: ()
    \\@[extern]
    \\let abs: (x: i32) -> i32
    \\@[extern]
    \\let putchar: (x: i32) -> i32
    \\let callee: () -> (i32, i32) -> i32 = {@print("C")
    \\    add}
    \\let arg: (label: string, n: i32) -> i32 = {@print(label)
    \\    n}
    \\let main: () = {
    \\    foreign_unit()
    \\    let f: (i32, i32) -> i32 = add
    \\    @println(callee()(arg("L", 20), arg("R", 22)))
    \\    @println(f(1, 2), alias(3, 4), abs(-9))
    \\    if putchar(65) != 65 { @panic("putchar letter failed") }
    \\    if putchar(10) != 10 { @panic("putchar newline failed") }
    \\}
;

const extern_provider =
    \\#include <stdint.h>
    \\#include <stdio.h>
    \\int32_t foreign_add(int32_t x, int32_t y) { return x + y; }
    \\void foreign_unit(void) { (void)putchar('U'); }
;

test "extern raw symbols libc function values aliases and ordered callee arguments link separate TU" {
    const c = try externTestProgram(extern_program);
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "int32_t foreign_add(int32_t p0, int32_t p1);") != null);
    try std.testing.expect(std.mem.indexOf(u8, c, "void foreign_unit(void);") != null);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_add") == null);
    try std.testing.expect(std.mem.indexOf(u8, c, "foreign_add(int32_t p0, int32_t p1) {") == null);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, extern_provider, 0, "UCLR42\n3 7 9\nA\n", "", mode);
}

test "extern custom scalar providers preserve every supported ABI width" {
    var source = std.Io.Writer.Allocating.init(test_gpa);
    defer source.deinit();
    var provider = std.Io.Writer.Allocating.init(test_gpa);
    defer provider.deinit();
    try provider.writer.writeAll("#include <stdint.h>\n#include <stdbool.h>\n");
    const types = [_][]const u8{ "bool", "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "int", "uint" };
    const c_types = [_][]const u8{ "bool", "int8_t", "uint8_t", "int16_t", "uint16_t", "int32_t", "uint32_t", "int64_t", "uint64_t", "intptr_t", "uintptr_t" };
    const values = [_][]const u8{ "true", "-128", "255", "-32768", "65535", "-2147483648", "4294967295", "-9223372036854775808", "18446744073709551615", "-7", "9" };
    for (types, c_types) |ty, c_ty| {
        try source.writer.print("@[extern(\"provider_{s}\")]\nlet identity_{s}: (x: {s}) -> {s}\n", .{ ty, ty, ty, ty });
        try provider.writer.print("{s} provider_{s}({s} x) {{ return x; }}\n", .{ c_ty, ty, c_ty });
    }
    try source.writer.writeAll("let main: () = {\n");
    for (types, values) |ty, value| try source.writer.print("if identity_{s}({s}) != {s} {{ @panic(\"ABI mismatch\") }}\n", .{ ty, value, value });
    try source.writer.writeAll("}\n");
    const c = try externTestProgram(source.written());
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, provider.written(), 0, "", "", mode);
}

test "extern noreturn prototype terminates before later argument effects" {
    const source =
        \\@[extern("foreign_stop")]
        \\@[noreturn]
        \\let stop: ()
        \\let consume: (x: i32, y: i32) = {}
        \\let main: () = { @print("before")
        \\    consume({stop()}, {@print("BAD")
        \\        1}) }
    ;
    const c = try externTestProgram(source);
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "_Noreturn void foreign_stop(void);") != null);
    const provider = "#include <stdlib.h>\n_Noreturn void foreign_stop(void) { exit(7); }\n";
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, provider, 7, "before", "", mode);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{source});
}

test "extern matching export aliases share one definition" {
    const c = try externTestProgram(
        \\@[extern("shared")]
        \\let external: (x: i32) -> i32
        \\@[export("shared")]
        \\let definition: (x: i32) -> i32 = x
        \\let main: () = @println(external(42))
    );
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "42\n", "", mode);
}

test "extern noreturn export alias recursion compiles without being called" {
    const c = try emitSource(
        \\@[extern("foreign_spin")]
        \\@[noreturn]
        \\let recur: ()
        \\@[export("foreign_spin")]
        \\@[noreturn]
        \\let spin: () = recur()
    , null);
    defer test_gpa.free(c);
    const harness = "int main(void) { return 0; }";
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, harness, 0, "", "", mode);
}

test "extern ordinary export alias recursion through function values terminates" {
    const c = try emitSource(
        \\@[extern("foreign_countdown")]
        \\let recur: (n: i32) -> i32
        \\@[export("foreign_countdown")]
        \\let countdown: (n: i32) -> i32 = {
        \\    let f: (i32) -> i32 = recur
        \\    if n == 0 { 42 } else { f(n - 1) }
        \\}
    , null);
    defer test_gpa.free(c);
    const harness =
        \\#include <stdint.h>
        \\int32_t foreign_countdown(int32_t);
        \\int main(void) { return foreign_countdown(4) != 42; }
    ;
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, harness, 0, "", "", mode);
}

test "extern unused declarations need no link provider or runtime" {
    const c = try emitSource("@[extern]\nlet absent_provider: (x: i32) -> i32\n@[extern]\n@[noreturn]\nlet absent_stop: ()", null);
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "int32_t absent_provider(int32_t p0);") != null);
    try std.testing.expect(std.mem.indexOf(u8, c, "_Noreturn void absent_stop(void);") != null);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_rt_") == null);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_gc_") == null);
    try std.testing.expect(std.mem.indexOf(u8, c, " {\n") == null);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "int main(void) { return 0; }", 0, "", "", mode);
}

fn testExternSourceFree(allocator: std.mem.Allocator) !void {
    var result = blk: {
        const source = try allocator.dupe(u8, extern_program);
        defer allocator.free(source);
        var module = try Module.init(allocator, "extern.nov", source);
        defer module.deinit();
        var lowered = try Lowering.lower(&module);
        errdefer lowered.deinit();
        try std.testing.expect(!module.hasErrors());
        break :blk lowered;
    };
    defer result.deinit();
    try result.ir.verify();
    const c = try emitProgram(allocator, &result.ir, null);
    defer allocator.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "int32_t foreign_add(") != null);
}

test "extern source free ownership and every compiler allocation failure" {
    try TestAllocations.checkAllAllocationFailures(test_gpa, testExternSourceFree, .{});
}

test "extern generated collision diagnostics and allocation cleanup" {
    const source = "let helper: () = {}\n@[extern(\"nov_helper\")]\nlet foreign: ()";
    var diagnostic: Diagnostic = undefined;
    try std.testing.expectError(error.ExternSymbolCollision, emitSource(source, &diagnostic));
    try std.testing.expectEqual(Diagnostic.Reason.extern_symbol_collision, diagnostic.reason);
    try std.testing.expectEqualStrings("foreign", source[diagnostic.span.start..diagnostic.span.end]);
    try TestAllocations.checkAllAllocationFailures(test_gpa, struct {
        fn run(allocator: std.mem.Allocator) !void {
            var module = try Module.init(allocator, "collision.nov", source);
            defer module.deinit();
            var result = try Lowering.lower(&module);
            defer result.deinit();
            try std.testing.expect(!module.hasErrors());
            const c = emit(allocator, &result.ir, null) catch |err| {
                if (err == error.OutOfMemory) return err;
                try std.testing.expectEqual(error.ExternSymbolCollision, err);
                return;
            };
            defer allocator.free(c);
            return error.TestUnexpectedResult;
        }
    }.run, .{});
}

test "extern root main is invalid as a program but emits a library prototype" {
    var module = try Module.init(test_gpa, "entry.nov", "@[extern(\"foreign_main\")]\nlet main: ()");
    defer module.deinit();
    var result = try Lowering.lower(&module);
    defer result.deinit();
    try std.testing.expect(!module.hasErrors());
    var diagnostic: Diagnostic = undefined;
    try std.testing.expectError(error.InvalidEntrypoint, emitProgram(test_gpa, &result.ir, &diagnostic));
    try std.testing.expectEqual(Diagnostic.Reason.entrypoint_external, diagnostic.reason);
    const c = try emit(test_gpa, &result.ir, null);
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "void foreign_main(void);") != null);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "int main(void) { return 0; }", 0, "", "", mode);
}

test "export exact default and custom names link from a separate C translation unit" {
    const source =
        \\let offset: (x: i32) -> i32 = x + 1
        \\@[export]
        \\let add: (a: i32, b: i32) -> i32 = offset(a) + b
        \\@[public]
        \\@[export("nov_fibonacci")]
        \\let fib: (n: uint) -> uint = if n < 2 { n } else { fib(n - 1) + fib(n - 2) }
        \\@[export]
        \\let invoke: (a: i32) -> i32 = {let f: (i32, i32) -> i32 = add
        \\f(a, 2)}
        \\@[export]
        \\let unit: () = {}
        \\let main: () = {}
    ;
    const c = try emitSource(source, null);
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "int32_t add(") != null);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_add(") == null);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_fib(") == null);
    const harness =
        \\#include <stdint.h>
        \\int32_t add(int32_t, int32_t);
        \\uintptr_t nov_fibonacci(uintptr_t);
        \\int32_t invoke(int32_t);
        \\void unit(void);
        \\int main(void) {
        \\    unit();
        \\    return add(20, 21) != 42 || invoke(39) != 42 || nov_fibonacci(10) != 55;
        \\}
    ;
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, harness, 0, "", "", mode);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{source});
}

test "export scalar ABI uses exact integer widths bool and native pointer widths" {
    var source = std.Io.Writer.Allocating.init(test_gpa);
    defer source.deinit();
    var harness = std.Io.Writer.Allocating.init(test_gpa);
    defer harness.deinit();
    try harness.writer.writeAll("#include <stdint.h>\n#include <stdbool.h>\n");
    const types = [_][]const u8{ "bool", "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "int", "uint" };
    const c_types = [_][]const u8{ "bool", "int8_t", "uint8_t", "int16_t", "uint16_t", "int32_t", "uint32_t", "int64_t", "uint64_t", "intptr_t", "uintptr_t" };
    const values = [_][]const u8{ "true", "INT8_MIN", "UINT8_MAX", "INT16_MIN", "UINT16_MAX", "INT32_MIN", "UINT32_MAX", "INT64_MIN", "UINT64_MAX", "INTPTR_MIN", "UINTPTR_MAX" };
    for (types, c_types) |ty, c_ty| {
        try source.writer.print("@[export]\nlet identity_{s}: (x: {s}) -> {s} = x\n", .{ ty, ty, ty });
        try harness.writer.print("{s} identity_{s}({s});\n", .{ c_ty, ty, c_ty });
    }
    try harness.writer.writeAll("int main(void) {\n");
    for (types, values, 1..) |ty, value, code| try harness.writer.print("if (identity_{s}({s}) != {s}) return {d};\n", .{ ty, value, value, code });
    try harness.writer.writeAll("return 0;\n}\n");
    const c = try emitSource(source.written(), null);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, harness.written(), 0, "", "", mode);
}

test "export noreturn annotation preserves panic and terminal call behavior in C" {
    const c = try emitSource("@[export]\n@[noreturn]\nlet stop: () = @panic(\"foreign call\")", null);
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "_Noreturn void stop(void)") != null);
    const harness = "_Noreturn void stop(void);\nint main(void) { stop(); }\n";
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, harness, 1, "", "panic: foreign call\n", mode);
}

test "export collision with generated private symbol reports the exporting source" {
    const source = "let helper: () -> i32 = 1\n@[export(\"nov_helper\")]\nlet exposed: () -> i32 = 2";
    var diagnostic: Diagnostic = undefined;
    try std.testing.expectError(error.ExportSymbolCollision, emitSource(source, &diagnostic));
    try std.testing.expectEqual(Diagnostic.Reason.export_symbol_collision, diagnostic.reason);
    try std.testing.expectEqualStrings("exposed", source[diagnostic.span.start..diagnostic.span.end]);
    try TestAllocations.checkAllAllocationFailures(test_gpa, struct {
        fn run(allocator: std.mem.Allocator) !void {
            var module = try Module.init(allocator, "collision.nov", source);
            defer module.deinit();
            var result = try Lowering.lower(&module);
            defer result.deinit();
            try std.testing.expect(!module.hasErrors());
            const emitted = emit(allocator, &result.ir, null) catch |err| {
                if (err == error.OutOfMemory) return err;
                try std.testing.expectEqual(error.ExportSymbolCollision, err);
                return;
            };
            defer allocator.free(emitted);
            return error.TestUnexpectedResult;
        }
    }.run, .{});
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

test "cross source generic reexports retain defining scope under forced GC after graph release" {
    var sema = blk: {
        var comp = Compilation.init(test_gpa);
        defer comp.deinit();
        const root = try comp.addSource("/app/main.nov",
            \\let std=@import("std")
            \\let Result=std.Result
            \\let R=Result(string,string)
            \\let prefix="WRONG"
            \\let helper:()->string="WRONG"
            \\let main:()={let mut r:R=.ok["a"+"b"]
            \\    let saved=r
            \\    r=.err["error"]
            \\    @println(saved.unwrapOr("fallback"),r.unwrapOr("fall"+"back"),saved.message())}
        );
        const entry = try comp.addSource("/lib/std.nov",
            \\let result=@import("result.nov")
            \\@[public]
            \\let Result=result.Result
            \\let main:()=@println("WRONG MAIN")
        );
        const library = try comp.addSource("/lib/result.nov",
            \\let prefix="library:"
            \\let helper:()->string="helper"
            \\@[public]
            \\let Result:(T:type,E:type)->type=union{ok:T,err:E,
            \\let Self=@This()
            \\@[public]
            \\let unwrapOr:(self:Self,fallback:T)->T=match self{.ok=>|v|v
            \\.err=>fallback}
            \\@[public]
            \\let message:(self:Self)->string=prefix+helper()
            \\}
        );
        for ([_]struct { source: Source.Id, name: []const u8, target: Source.Id, named: bool }{
            .{ .source = root, .name = "std", .target = entry, .named = true },
            .{ .source = entry, .name = "result", .target = library, .named = false },
        }) |edge| {
            const module = comp.getModule(edge.source).?;
            const local = module.lookup(edge.name).?;
            try comp.imports.append(test_gpa, .{ .declaration = comp.declaration_ids.get(.{ .source = edge.source, .local = local }).?, .target = edge.target, .named = edge.named, .span = module.getDeclaration(local).?.name_span });
        }
        comp.setRoot(root);
        var result = try Lowering.lowerCompilation(&comp);
        errdefer result.deinit();
        try std.testing.expect(!comp.hasErrors());
        try std.testing.expect(result.lowering_complete);
        break :blk result;
    };
    defer sema.deinit();
    const library = try emit(test_gpa, &sema.ir, null);
    defer test_gpa.free(library);
    const program = try collectingTestProgram(library);
    defer test_gpa.free(program);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(program, "typedef int nonempty_translation_unit;", 0, "ab fallback library:helper\n", "", mode);
}

const nested_record_program =
    \\let Alias=Deep
    \\let Deep=struct{box:Box,
    \\@[stringify]
    \\let toString:(r:Deep)->string="{r.box.inner.s}:{r.box.inner.n}:{r.box.other.s}"
    \\}
    \\let Box=struct{inner:Leaf,other:Leaf,}
    \\let Leaf=struct{s:string,n:int=1,}
    \\let R:(T:type,E:type)->type=union{ok:T,err:E,}
    \\let O:(T:type)->type=union{some:T,none,}
    \\let text:(s:string)->string={@print(s)
    \\s+"!"}
    \\let leaf:(s:string)->Leaf=Leaf[.s=s+"!"]
    \\let deep:(s:string)->Alias=Alias[.box=Box[.inner=leaf(s),.other=leaf("other")]]
    \\let identity:(r:Deep)->Deep=r
    \\let invoke:(cb:(Deep)->Deep,r:Deep)->Deep=cb(r)
    \\let result:(ok:bool)->R(Deep,Deep)=if ok{.ok[deep("ok")]}else{.err[deep("err")]}
    \\let propagate:(ok:bool)->R(Deep,Deep)={let mut r=result(ok).!
    \\r.box.inner.s="new"+"!"
    \\.ok[r]}
    \\let option:(o:O(Deep))->O(Deep)={let mut r=o.!
    \\r.box.inner.n=8
    \\.some[r]}
    \\let terminal:()={let ignored=Deep[.box={return}]
    \\}
    \\let main:()={
    \\let mut r=Deep[.box=Box[.other=Leaf[.s=text("b")],.inner=Leaf[.s=text("a")]]]
    \\let saved=r
    \\let called=invoke(identity,r)
    \\let merged=if true{r}else{deep("BAD")}
    \\let looped:Deep=loop{break r}
    \\r.box.inner.n += {r=deep("replaced")
    \\5}
    \\r.box.inner.s="live"+"!"
    \\@println(r,saved,called,merged,looped)
    \\r.box.inner=leaf("whole")
    \\let copied=r.box
    \\r.box.other.s="changed"+"!"
    \\@println(r,copied.inner.s,copied.other.s)
    \\@println(match propagate(true){.ok=>|v|"{v}"
    \\.err=>|v|"{v}"},match propagate(false){.ok=>|v|"{v}"
    \\.err=>|v|"{v}"})
    \\@println(match option(.some[saved]){.some=>|v|"{v}"
    \\.none=>"none"},match option(.none[]){.some=>"BAD"
    \\.none=>"none"},saved)
    \\if false{let skipped=deep("BAD")
    \\@println(skipped)}
    \\terminal()
    \\}
;

test "nested records preserve recursive copies current slot paths captures and roots under forced GC" {
    const c = try arrayTestProgram(nested_record_program);
    defer test_gpa.free(c);
    const output = "balive!:6:other! a!:1:b! a!:1:b! a!:1:b! a!:1:b!\n" ++
        "whole!:1:changed! whole! other!\nnew!:1:other! err!:1:other!\na!:8:b! none a!:1:b!\n";
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, output, "", mode);
}

test "nested records emission and path ownership survive every allocation failure" {
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{nested_record_program});
}

test "managed array record widths retain semantic limits and report exact backend leaf spans" {
    for ([_][]const u8{
        "let Outer=struct{inner:Inner,}\nlet Inner=struct{wide:i128,}\nlet f:(a:[]Outer)={}",
        "let f:(a:[]Outer)={}\nlet Outer=struct{inner:Inner,}\nlet Inner=struct{wide:u128,}",
        "let Outer=struct{inner:Inner,}\nlet Inner=struct{wide:i128,}\nlet f:(cb:([]Outer)->[]Outer)={}",
    }) |source| {
        var module = try Module.init(test_gpa, "array-width.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        var diagnostic: Diagnostic = undefined;
        try std.testing.expectError(error.UnsupportedIntegerWidth, emit(test_gpa, &sema.ir, &diagnostic));
        try std.testing.expectEqualStrings("wide", module.sourceSlice(diagnostic.span).?);
    }
}

test "nested records callback widths report transitive exact leaf spans" {
    var module = try Module.init(test_gpa, "nested-width.nov", "let Outer=struct{left:Middle,right:Middle,}\nlet Middle=struct{leaf:Leaf,}\nlet Leaf=struct{wide:i128,}\nlet U=union{ok:Outer,none,}\nlet f:(cb:(U)->void)={}");
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    var diagnostic: Diagnostic = undefined;
    try std.testing.expectError(error.UnsupportedIntegerWidth, emit(test_gpa, &sema.ir, &diagnostic));
    try std.testing.expectEqualStrings("wide", module.sourceSlice(diagnostic.span).?);
}

test "nested records imported forward diamond identities survive graph release and forced GC" {
    var sema = blk: {
        var comp = Compilation.init(test_gpa);
        defer comp.deinit();
        const root = try comp.addSource("/main.nov", "let left=@import(\"left.nov\")\nlet right=@import(\"right.nov\")\n" ++
            "let Root=struct{left:left.Box,right:right.Box,}\n" ++
            "let main:()={let mut r=Root[.left=left.make(),.right=right.make()]\nlet saved=r\nr.left.inner.s=\"changed\"+\"!\"\n@println(r.left.inner.s,saved.left.inner.s,r.right.inner.s)\n" ++
            "let mut a=[left.make(),right.make()]\nlet old=a\nlet mut v=a[0]\nv.inner.s=\"array\"+\"!\"\na[-1]=v\n@println(old[-1].inner.s,a[-1].inner.s)}");
        const left = try comp.addSource("/left.nov", "let shared=@import(\"shared.nov\")\n@[public]\nlet Box=shared.Box\n@[public]\nlet make:()->Box=shared.make()");
        const right = try comp.addSource("/right.nov", "let shared=@import(\"shared.nov\")\n@[public]\nlet Box=shared.Box\n@[public]\nlet make:()->Box=shared.make()");
        const shared = try comp.addSource("/shared.nov", "@[public]\nlet Box=struct{inner:Alias,}\nlet Alias=Leaf\nlet Leaf=struct{s:string,}\n@[public]\nlet make:()->Box=Box[.inner=Alias[.s=\"sha\"+\"red\"]]");
        for ([_]struct { source: Source.Id, name: []const u8, target: Source.Id }{
            .{ .source = root, .name = "left", .target = left },
            .{ .source = root, .name = "right", .target = right },
            .{ .source = left, .name = "shared", .target = shared },
            .{ .source = right, .name = "shared", .target = shared },
        }) |edge| {
            const module = comp.getModule(edge.source).?;
            const local = module.lookup(edge.name).?;
            try comp.imports.append(test_gpa, .{ .declaration = comp.declaration_ids.get(.{ .source = edge.source, .local = local }).?, .target = edge.target, .named = false, .span = module.getDeclaration(local).?.name_span });
        }
        comp.setRoot(root);
        var result = try Lowering.lowerCompilation(&comp);
        errdefer result.deinit();
        if (comp.hasErrors()) {
            var output = std.Io.Writer.Allocating.init(test_gpa);
            defer output.deinit();
            try Diagnostics.renderCompilation(&comp, &output.writer);
            std.debug.print("{s}", .{output.written()});
        }
        try std.testing.expect(result.lowering_complete);
        const record = result.ir.getType(result.entries[2].alias_type.?).?.record;
        try std.testing.expectEqual(record.fields[0].ty, record.fields[1].ty);
        break :blk result;
    };
    defer sema.deinit();
    const library = try emit(test_gpa, &sema.ir, null);
    defer test_gpa.free(library);
    const c = try collectingTestProgram(library);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "changed! shared shared\nshared array!\n", "", mode);
}

test "nested records deepest supported declarations compile in dependency order" {
    var source = std.Io.Writer.Allocating.init(test_gpa);
    defer source.deinit();
    for (0..Ir.max_record_depth - 1) |i| try source.writer.print("let R{d}=struct{{next:R{d},}}\n", .{ i, i + 1 });
    try source.writer.print("let R{d}=struct{{s:string,}}\nlet f:(r:R0)={{}}\nlet main:()={{}}", .{Ir.max_record_depth - 1});
    const library = try emitSource(source.written(), null);
    defer test_gpa.free(library);
    const c = try collectingTestProgram(library);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "", "", mode);
}

test "nested records propagation shares immutable strings and skips allocations on terminal paths" {
    for ([_]bool{ false, true }) |allocate| {
        const source = try std.fmt.allocPrint(test_gpa, "let R:(T:type,E:type)->type=union{{ok:T,err:E,}}\nlet Outer=struct{{middle:Middle,}}\nlet Middle=struct{{leaf:Leaf,}}\nlet Leaf=struct{{s:string,}}\n" ++
            "let make:()->R(int,Outer)=.err[Outer[.middle=Middle[.leaf=Leaf[.s={s}]]]]\n" ++
            "let run:()->R(bool,Outer)=.ok[make().! == 0]\nlet main:()=@println(match run(){{.ok=>\"BAD\"\n.err=>|r|r.middle.leaf.s}})", .{if (allocate) "\"e\"+\"rr\"" else "\"err\""});
        defer test_gpa.free(source);
        const library = try emitSource(source, null);
        defer test_gpa.free(library);
        const program = try collectingTestProgram(library);
        defer test_gpa.free(program);
        const c = try std.mem.concat(test_gpa, u8, &.{ "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", program });
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(c, "typedef int nonempty_translation_unit;", if (allocate) 1 else 0, if (allocate) "" else "err\n", if (allocate) "panic: GC allocation failed\n" else "", mode);
    }
}

const managed_array_program =
    \\let Kind=enum{first,second,}
    \\let Alias=Outer
    \\let Outer=struct{inner:Inner,other:string,k:Kind=.first,}
    \\let Inner=struct{s:string,n:int=1,}
    \\let O:(T:type)->type=union{some:T,none,}
    \\let R:(T:type,E:type)->type=union{ok:T,err:E,}
    \\let make:(s:string)->Alias=Alias[.inner=Inner[.s=s+"!"],.other="other"+"!"]
    \\let strings:()->[]string=["hé\0"+"😀","later"+"!","", "static"]
    \\let records:()->[]Outer=[make("a"),make("b")]
    \\let copy:(a:[]Outer)->[]Outer=a
    \\let identity:(a:[]string)->[]string=a
    \\let append:()->[]Outer={let mut a=records()
    \\a += [make("c")]
    \\a += []
    \\a}
    \\let terminal:()->[]string=["root"+"!",{return []},"BAD"+"!"]
    \\let main:()={
    \\let mut a=identity(strings())
    \\let b=a
    \\a[0]="changed"+"!"
    \\a += ["tail"+"!"]
    \\@println(b[0],a[0],a[-1],a.len,"hé\0😀" in b,"hé"+"\0😀" in b,"hé" in b,"" in b,"missing" in b,"" in [])
    \\a[0]={a=["discard"+"!"]
    \\"snapshot"+"!"}
    \\@println(a[0],a[-1],a.len,b[-1])
    \\let mut r=copy(append())
    \\let original=r
    \\let mut value=r[0]
    \\value.inner.s="value"+"!"
    \\value.inner.n=9
    \\value.k=.second
    \\r[-1]=value
    \\let mut first=r[0]
    \\first.inner.s="first"+"!"
    \\@println(original[0].inner.s,original[-1].inner.s,r[-1].inner.s,r[0].inner.s,first.inner.s,r[-1].inner.n,r[-1].k==Kind.second)
    \\r[0]={r=[make("discard")]
    \\make("stored")}
    \\@println(r[0].inner.s,r[-1].inner.s,r.len,original[0].other)
    \\let option:O(Outer)=.some[r[0]]
    \\let result:R(Outer,Outer)=.ok[original[0]]
    \\@println(match option{.some=>|v|v.inner.s
    \\.none=>"BAD"},match result{.ok=>|v|v.inner.s
    \\.err=>|v|v.other})
    \\let empty:[]Outer=[]
    \\let empty2:[]string=[]
    \\let again:[]Outer=[]
    \\@println(empty.len,empty2.len,again.len,terminal().len)
    \\}
;
const managed_array_output = "hé\x00😀 changed! tail! 5 true true false true false false\nsnapshot! tail! 5 static\na! c! value! a! first! 9 true\nstored! value! 3 other!\nstored! a!\n0 0 0 0\n";

test "managed arrays copy nested record values and share immutable bytes under forced GC" {
    const c = try arrayTestProgram(managed_array_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, managed_array_output, "", mode);
}

test "managed array literal and indexed stores preserve ordered effects snapshots and terminal paths" {
    const c = try arrayTestProgram(
        \\let A=struct{s:string,}
        \\let mark:(s:string)->string={@print(s)
        \\s+"!"}
        \\let record:(s:string)->A=A[.s=mark(s)]
        \\let main:()={
        \\let mut a=[mark("A"),mark("B")]
        \\a[{a=["discard"+"!"]
        \\@print("I")
        \\0}]={@print("R")
        \\mark("N")}
        \\@println("",a[0],a[-1],a.len)
        \\let mut r=[record("C"),record("D")]
        \\r[{r=[record("X")]
        \\@print("J")
        \\0}]={@print("S")
        \\record("E")}
        \\@println("",r[0].s,r[-1].s,r.len)
        \\let mut count:int=0
        \\loop count<3{count+=1
        \\let skipped=[mark("L"),{if count==1{continue}
        \\if count==2{break}
        \\mark("BAD")}]
        \\@println(skipped[0])}
        \\@println(count)
        \\}
    );
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "ABIRN N! B! 2\nCDXJSE E! D! 2\nLL2\n", "", mode);
}

const copy_elision_effects =
    \\let A=struct{a:[]int,}
    \\let use:(a:A,n:int)->int=a.a[0]+n
    \\let main:()={
    \\let mut a=A[.a=[1]]
    \\let frozen=a
    \\let snap=()->int {frozen.a[0]}
    \\let change=() {let r=&a
    \\r.a=[r.a[0]+1]}
    \\@println(use(a,{change()
    \\0}),snap(),a.a[0])
    \\let old=a.a[{change()
    \\0}]
    \\@println(old,a.a[0])
    \\a.a[0]={change()
    \\7}
    \\@println(a.a[0],frozen.a[0],snap())
    \\}
;

const copy_elision_cases = [_]struct { source: []const u8, allocations: usize, output: []const u8 }{
    .{
        .source = "let main:()={let a:[]int=[1]\nlet b=a\n@println(a[0],b[0])}",
        .allocations = 2,
        .output = "1 1\n",
    },
    .{
        .source = "let make:()->[]int={return [1]}\nlet use:(a:[]int)->int=a[0]\nlet main:()={let a=make()\n@println(a[0],use(make()))}",
        .allocations = 2,
        .output = "1 1\n",
    },
    .{
        .source = "let A=struct{a:[]int,b:[]int,}\nlet make:()->A=A[.a=[1],.b=[2]]\nlet use:(a:A)->int=a.a[0]+a.b[0]\nlet main:()=@println(use(make()))",
        .allocations = 2,
        .output = "3\n",
    },
    .{
        .source = "let A=struct{a:[]int,b:[]int,}\nlet main:()={let a=A[.a=[1],.b=[2]]\nlet n=a.a[0]\n@println(n,a.b.len,a.b[-1])}",
        .allocations = 2,
        .output = "1 1 2\n",
    },
    .{
        .source = "let make:()->[]int=if true {loop {break [1]}} else {[2]}\nlet main:()={let a=make()\n@println(a[0])}",
        .allocations = 1,
        .output = "1\n",
    },
    .{
        .source = "let main:()={let mut a:[]int=[2,3]\na[-1]+=4\n@println(a[0],a[1])}",
        .allocations = 2,
        .output = "2 7\n",
    },
    .{
        .source = "let main:()={let mut a:[]i8=[127]\na[0]+=1\nlet mut b:[]u64=[8]\nb[0]*=3\n@println(a[0],b[0])}",
        .allocations = 4,
        .output = "-128 24\n",
    },
    .{
        .source = "let L=struct{a:[]int,b:[]int,}\nlet T=struct{leaf:L,}\nlet main:()={let mut t=T[.leaf=L[.a=[1,2],.b=[3]]]\nt.leaf.a[-1]+={t.leaf.b=[9]\n4}\n@println(t.leaf.a[0],t.leaf.a[1],t.leaf.b[0])}",
        .allocations = 4,
        .output = "1 6 9\n",
    },
    .{
        .source = "let main:()={let mut a:[]int=[1,2]\na[{a=[8]\n0}]+={a=[9]\n4}\n@println(a[0],a[1])}",
        .allocations = 4,
        .output = "5 2\n",
    },
    .{
        .source = "let main:()={let mut a:[]u16=[12]\na[0]/=3\n@println(a[0])}",
        .allocations = 2,
        .output = "4\n",
    },
    .{
        .source = "let main:()={let mut a:[]int=[1]\nlet r=&a[0]\nr+=2\n@println(a[0])}",
        .allocations = 1,
        .output = "3\n",
    },
    .{
        .source = "let L=struct{a:[]int,}\nlet main:()={let mut a=L[.a=[1]]\nlet change=(){a.a=[9]}\na.a[0]+={change()\n2}\n@println(a.a[0])}",
        .allocations = 5,
        .output = "3\n",
    },
    // Includes immutable capture backing and its box, both environments, the
    // shared mutable cell, and snapshots preceding argument/index/RHS effects.
    .{ .source = copy_elision_effects, .allocations = 13, .output = "1 1 2\n2 3\n7 1 1\n" },
    // Targeted transfers retain value snapshots; append projects its final RHS
    // read and detaches unproven nested storage in the fresh result.
    .{
        .source = "let L=struct{a:[]int,}\nlet main:()={let a=[L[.a=[1]]]\n@println(a[0].a[0])}",
        .allocations = 2,
        .output = "1\n",
    },
    .{
        .source = "let L=struct{a:[]int,}\nlet main:()={let seed=L[.a=[1]]\nlet a=[seed,seed]\n@println(a[0].a[0],a[1].a[0])}",
        .allocations = 4,
        .output = "1 1\n",
    },
    .{
        .source = "let L=struct{a:[]int,}\nlet main:()={let mut a=[L[.a=[1]]]\na[-1]=L[.a=[2]]\n@println(a[0].a[0])}",
        .allocations = 5,
        .output = "2\n",
    },
    .{
        .source = "let L=struct{a:[]int,}\nlet main:()={let mut a=[L[.a=[1]]]\na += [L[.a=[2]]]\n@println(a[0].a[0],a[1].a[0])}",
        .allocations = 7,
        .output = "1 2\n",
    },
    .{
        .source = "let L=struct{a:[]int,}\nlet main:()={let mut a=[L[.a=[1]]]\na+=a\n@println(a[0].a[0],a[1].a[0])}",
        .allocations = 6,
        .output = "1 1\n",
    },
    .{
        .source = "let L=struct{a:[]int,}\nlet T=struct{leaf:L,items:[]L,}\nlet main:()={let mut a=[T[.leaf=L[.a=[1]],.items=[L[.a=[2]]]]]\na+=a\n@println(a.len)}",
        .allocations = 12,
        .output = "2\n",
    },
    .{
        .source = "let L=struct{a:[]int,}\nlet T=struct{leaf:L,items:[]L,}\nlet U=union{node:T,none,}\nlet main:()={let u=U.node[T[.leaf=L[.a=[1]],.items=[L[.a=[2]]]]]\nmatch u {\n.node=>|t|{let mut a=t.items\na+=a\n@println(a.len)}\n.none=>{}\n}}",
        .allocations = 12,
        .output = "2\n",
    },
};

test "copy elision allocation counts retain snapshots under forced GC" {
    for (copy_elision_cases) |case| {
        const program = try arrayTestProgram(case.source);
        defer test_gpa.free(program);
        const suffix = try std.fmt.allocPrint(
            test_gpa,
            "\n#undef main\nint main(void) {{ int result = test_main(); assert(!outstanding); printf(\"allocations: %u\\n\", allocations); return result; }}\n",
            .{},
        );
        defer test_gpa.free(suffix);
        const counted = try std.mem.concat(test_gpa, u8, &.{
            "#include <stdlib.h>\n#include <assert.h>\nstatic unsigned allocations, outstanding;\n" ++
                "static void *test_malloc(size_t n) { void *p = malloc(n); if (p) { ++allocations; ++outstanding; } return p; }\n" ++
                "static void test_free(void *p) { assert(outstanding); --outstanding; free(p); }\n" ++
                "#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE test_free\n#define main test_main\n",
            program,
            suffix,
        });
        defer test_gpa.free(counted);
        const expected = try std.fmt.allocPrint(test_gpa, "{s}allocations: {d}\n", .{ case.output, case.allocations });
        defer test_gpa.free(expected);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(counted, "typedef int nonempty_translation_unit;", 0, expected, "", mode);
    }
}

test "copy elision forged reused and live storage operands keep independent backing" {
    var module = try Module.init(test_gpa, "forged-transfers.nov", "let L=struct{a:[]int,}\nlet main:()={let seed=L[.a=[1]]\nlet mut a=[seed,seed]\na[-1]=seed\na+=a\n@println(a.len,seed.a[0],a[0].a[0])}");
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    const body = sema.ir.functions.items[0].body.?;
    for (@constCast(body.instructions)) |*instruction| switch (instruction.op) {
        .array_literal => |elements| if (elements.len == 2) {
            // Reusing the same detached record twice must clone both insertions.
            @constCast(elements)[1] = elements[0];
        },
        .array_set => |*update| {
            try std.testing.expect(body.instructions[@backingInt(update.array)].op == .array_clone);
            try std.testing.expect(body.instructions[@backingInt(update.value)].op == .array_clone);
            // Well-typed IR need not contain the snapshots lowering inserts.
            update.array = body.instructions[@backingInt(update.array)].op.array_clone;
            update.value = body.instructions[@backingInt(update.value)].op.array_clone;
        },
        .array_append => |*append| append.rhs = append.lhs,
        else => {},
    };
    try sema.ir.verify();
    const transfers = try ArrayTransfers.init(test_gpa, &sema.ir, body);
    defer transfers.deinit(test_gpa);
    const library = try emit(test_gpa, &sema.ir, null);
    defer test_gpa.free(library);
    var instrumented = try test_gpa.dupe(u8, library);
    defer test_gpa.free(instrumented);
    var checked: usize = 0;
    for (body.instructions, 0..) |instruction, i| {
        const ty = sema.ir.getType(instruction.ty).?;
        if (ty != .array or sema.ir.getType(ty.array).? != .record) continue;
        var operation = std.Io.Writer.Allocating.init(test_gpa);
        defer operation.deinit();
        try writeArrayValue(&sema.ir, &operation.writer, body, transfers, instruction, i);
        var checks = std.Io.Writer.Allocating.init(test_gpa);
        defer checks.deinit();
        try checks.writer.writeAll(operation.written());
        switch (instruction.op) {
            .array_literal => |elements| {
                try std.testing.expect(!transfers.canTake(body, elements[0], i));
                for (elements, 0..) |value, j| try checks.writer.print("    assert(((nov_record_{d} *)v{d}.data)[{d}].f0.data != v{d}.f0.data);\n", .{ @backingInt(ty.array), i, j, @backingInt(value) });
            },
            .array_set => |update| {
                try std.testing.expect(!transfers.canTake(body, update.array, i));
                try std.testing.expect(!transfers.canTake(body, update.value, i));
                try checks.writer.print("    assert(v{d}.data != v{d}.data);\n    assert(((nov_record_{d} *)v{d}.data)[1].f0.data != v{d}.f0.data);\n", .{ i, @backingInt(update.array), @backingInt(ty.array), i, @backingInt(update.value) });
            },
            .array_append => |append| {
                try std.testing.expect(!transfers.canTake(body, append.lhs, i));
                try checks.writer.print("    assert(v{d}.data != v{d}.data);\n    for (uintptr_t j=0; j<v{d}.len; ++j) assert(((nov_record_{d} *)v{d}.data)[j].f0.data != ((nov_record_{d} *)v{d}.data)[j % v{d}.len].f0.data);\n", .{ i, @backingInt(append.lhs), i, @backingInt(ty.array), i, @backingInt(ty.array), @backingInt(append.lhs), @backingInt(append.lhs) });
            },
            else => continue,
        }
        checked += 1;
        try checks.writer.print("    for (uintptr_t j=0; j<v{d}.len; ++j) for (uintptr_t k=0; k<j; ++k) assert(((nov_record_{d} *)v{d}.data)[j].f0.data != ((nov_record_{d} *)v{d}.data)[k].f0.data);\n", .{ i, @backingInt(ty.array), i, @backingInt(ty.array), i });
        try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, instrumented, operation.written()));
        const replaced = try std.mem.replaceOwned(u8, test_gpa, instrumented, operation.written(), checks.written());
        test_gpa.free(instrumented);
        instrumented = replaced;
    }
    try std.testing.expectEqual(@as(usize, 3), checked);
    const prefixed = try std.mem.concat(test_gpa, u8, &.{ "#include <assert.h>\n", instrumented });
    defer test_gpa.free(prefixed);
    const c = try collectingTestProgram(prefixed);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "4 1 1\n", "", mode);
}

test "copy elision forged single static use inside loop cannot consume outer backing" {
    var module = try Module.init(test_gpa, "forged-loop-transfer.nov", "let main:()={let mut a:[]uint=[1]\nlet snapshot=a\nloop i in 0..2 {a[0]=i}\n@println(snapshot[0],a[0])}");
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    const body = sema.ir.functions.items[0].body.?;
    var outer: ?Ir.ValueId = null;
    var update_index: usize = 0;
    for (@constCast(body.instructions), 0..) |*instruction, i| switch (instruction.op) {
        .local_slot => |*value| if (body.instructions[@backingInt(value.*)].op == .array_clone and outer == null) {
            outer = value.*;
            // Leave the clone with only its use in the loop, not a slot use.
            value.* = body.instructions[@backingInt(value.*)].op.array_clone;
        },
        .array_set => |*update| {
            update.array = outer.?;
            update_index = i;
        },
        else => {},
    };
    try sema.ir.verify();
    const transfers = try ArrayTransfers.init(test_gpa, &sema.ir, body);
    defer transfers.deinit(test_gpa);
    try std.testing.expectEqual(@as(usize, 1), transfers.uses[@backingInt(outer.?)]);
    try std.testing.expect(transfers.detached[@backingInt(outer.?)]);
    try std.testing.expect(!transfers.canTake(body, outer.?, update_index));
    var operation = std.Io.Writer.Allocating.init(test_gpa);
    defer operation.deinit();
    try writeArrayValue(&sema.ir, &operation.writer, body, transfers, body.instructions[update_index], update_index);
    const replacement = try std.fmt.allocPrint(test_gpa, "{s}    assert(v{d}.data != v{d}.data);\n", .{ operation.written(), update_index, @backingInt(outer.?) });
    defer test_gpa.free(replacement);
    const library = try emit(test_gpa, &sema.ir, null);
    defer test_gpa.free(library);
    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, library, operation.written()));
    const instrumented = try std.mem.replaceOwned(u8, test_gpa, library, operation.written(), replacement);
    defer test_gpa.free(instrumented);
    const prefixed = try std.mem.concat(test_gpa, u8, &.{ "#include <assert.h>\n", instrumented });
    defer test_gpa.free(prefixed);
    const c = try collectingTestProgram(prefixed);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "1 1\n", "", mode);
}

// Instrument at the operation, not at a later language read which could hide
// sharing behind another snapshot. Also check the live source of moved clones.
fn testTransferTopologyRuntime(ir: *const Ir, take: bool, executions: usize, output: []const u8) !void {
    try ir.verify();
    const body = ir.functions.items[0].body.?;
    const transfers = try ArrayTransfers.init(test_gpa, ir, body);
    defer transfers.deinit(test_gpa);
    var instrumented = try emit(test_gpa, ir, null);
    defer test_gpa.free(instrumented);
    var updates: usize = 0;
    for (body.instructions, 0..) |instruction, i| {
        if (instruction.op != .array_set) continue;
        updates += 1;
        const base = instruction.op.array_set.array;
        try std.testing.expectEqual(take, transfers.canTake(body, base, i));
        if (take) try std.testing.expect(transfers.uses[@backingInt(base)] >= 1);
        var operation = std.Io.Writer.Allocating.init(test_gpa);
        defer operation.deinit();
        try writeArrayValue(ir, &operation.writer, body, transfers, instruction, i);
        var checks = std.Io.Writer.Allocating.init(test_gpa);
        defer checks.deinit();
        const element = ir.getType(instruction.ty).?.array;
        try checks.writer.print("    {{\n        uintptr_t selected = nov_array_index(v{d}, v{d});\n        ", .{ @backingInt(base), @backingInt(instruction.op.array_set.index) });
        try writeType(ir, &checks.writer, element);
        try checks.writer.writeAll(" before = ((");
        try writeType(ir, &checks.writer, element);
        try checks.writer.print(" *)v{d}.data)[selected];\n", .{@backingInt(base)});
        const source = if (take) body.instructions[@backingInt(base)].op.array_clone else base;
        if (take) {
            try checks.writer.writeAll("        ");
            try writeType(ir, &checks.writer, element);
            try checks.writer.writeAll(" source_before = ((");
            try writeType(ir, &checks.writer, element);
            try checks.writer.print(" *)v{d}.data)[selected];\n", .{@backingInt(source)});
        }
        try checks.writer.writeAll(operation.written());
        try checks.writer.print("        assert(v{d}.data {s} v{d}.data);\n        assert(v{d}.data != v{d}.data);\n        assert(((", .{ i, if (take) "==" else "!=", @backingInt(base), i, @backingInt(source) });
        try writeType(ir, &checks.writer, element);
        try checks.writer.print(" *)v{d}.data)[selected] == {s});\n", .{ @backingInt(source), if (take) "source_before" else "before" });
        if (take) {
            // The detached producer can change, but its live upstream storage
            // cannot. Its old value is still checked to make the write real.
            try checks.writer.writeAll("        assert(before != ((");
            try writeType(ir, &checks.writer, element);
            try checks.writer.print(" *)v{d}.data)[selected]);\n", .{i});
        }
        try checks.writer.writeAll("        ++test_transfer_updates;\n    }\n");
        try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, instrumented, operation.written()));
        const replaced = try std.mem.replaceOwned(u8, test_gpa, instrumented, operation.written(), checks.written());
        test_gpa.free(instrumented);
        instrumented = replaced;
    }
    try std.testing.expect(updates != 0);
    const driver = try std.fmt.allocPrint(test_gpa, "\nint main(void) {{ nov_rt_heap.collect_every_allocation = 1; nov_main(); assert(test_transfer_updates == {d}); " ++
        "assert(!nov_rt_heap.frames); nov_gc_collect(&nov_rt_heap); assert(!nov_rt_heap.live_objects); nov_gc_shutdown(&nov_rt_heap); return 0; }}\n", .{executions});
    defer test_gpa.free(driver);
    const c = try std.mem.concat(test_gpa, u8, &.{ "#include <assert.h>\nstatic unsigned test_transfer_updates;\n", instrumented, driver });
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, output, "", mode);
}

test "copy elision observed snapshots reject forged later reads and escaping projections" {
    for ([_][]const u8{
        "let main:()={let mut a:[]int=[1]\na[0]+=2\n@println(a[0],a.len)}",
        "let L=struct{a:[]int,}\nlet main:()={let mut a:[]L=[L[.a=[1]]]\nlet before=a[0]\na[0]=L[.a=[2]]\n@println(before.a[0],a[0].a[0])}",
    }, 0..) |source, case| {
        var module = try Module.init(test_gpa, "forged-observation.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        const body = sema.ir.functions.items[0].body.?;
        const instructions = @constCast(body.instructions);
        var consumer: usize = 0;
        for (instructions, 0..) |instruction, i| {
            if (instruction.op == .array_set) consumer = i;
        }
        var base = instructions[consumer].op.array_set.array;
        if (case == 0) {
            // The old-element read is safe, but a later scalar read is not.
            var changed = false;
            for (instructions[consumer + 1 ..]) |*instruction| {
                if (instruction.op == .array_index) {
                    instruction.op.array_index.array = base;
                    changed = true;
                    break;
                }
            }
            try std.testing.expect(changed);
        } else {
            // Reuse the earlier detached snapshot whose indexed projection
            // carries array backing out to another value boundary.
            for (instructions[0..consumer]) |instruction| {
                if (instruction.op == .array_index) {
                    base = instruction.op.array_index.array;
                    const load = &instructions[@backingInt(base)];
                    try std.testing.expect(load.op == .local_load);
                    const slot = load.op.local_load;
                    load.op = .{ .array_clone = instructions[@backingInt(slot)].op.local_slot };
                    break;
                }
            }
            instructions[consumer].op.array_set.array = base;
        }
        try sema.ir.verify();
        const transfers = try ArrayTransfers.init(test_gpa, &sema.ir, body);
        defer transfers.deinit(test_gpa);
        try std.testing.expect(transfers.uses[@backingInt(base)] > 1);
        try std.testing.expect(transfers.detached[@backingInt(base)]);
        try std.testing.expect(!transfers.canTake(body, base, consumer));
        try testCopyElisionEmissionRuntime(&sema.ir, if (case == 0) "1 1\n" else "1 2\n");
    }
}

fn testCopyElisionEmissionRuntime(ir: *const Ir, output: []const u8) !void {
    const library = try emit(test_gpa, ir, null);
    defer test_gpa.free(library);
    const c = try collectingTestProgram(library);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, output, "", mode);
}

test "copy elision observed snapshots respect branch short circuit and loop regions" {
    for ([_][]const u8{
        "if true {a[0]+=1}",
        "let ok=true and {a[0]+=1\ntrue}",
        "loop i in 0..2 {a[0]+=1}",
    }) |region| {
        const source = try std.fmt.allocPrint(test_gpa, "let main:()={{let mut a:[]uint=[1]\nlet mut snapshot=a\n{s}\n@println(snapshot[0],a[0])}}", .{region});
        defer test_gpa.free(source);
        var module = try Module.init(test_gpa, "observed-region.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        const body = sema.ir.functions.items[0].body.?;
        var outer: ?Ir.ValueId = null;
        var consumer: usize = 0;
        for (@constCast(body.instructions), 0..) |*instruction, i| switch (instruction.op) {
            .local_slot => |*value| if (body.instructions[@backingInt(value.*)].op == .array_clone and outer == null) {
                outer = value.*;
                value.* = body.instructions[@backingInt(value.*)].op.array_clone;
            },
            .array_index => |*read| if (outer != null and consumer == 0) {
                read.array = outer.?;
            },
            .array_set => |*update| {
                update.array = outer.?;
                consumer = i;
            },
            else => {},
        };
        try sema.ir.verify();
        const transfers = try ArrayTransfers.init(test_gpa, &sema.ir, body);
        defer transfers.deinit(test_gpa);
        try std.testing.expectEqual(@as(usize, 2), transfers.uses[@backingInt(outer.?)]);
        try std.testing.expect(!transfers.canTake(body, outer.?, consumer));
        try testTransferTopologyRuntime(&sema.ir, false, if (std.mem.startsWith(u8, region, "loop")) 2 else 1, "1 2\n");
    }
    for ([_][]const u8{
        "let main:()=if true {let mut a:[]int=[1]\na[0]+=1\n@println(a[0])}",
        "let main:()=loop i in 0..2 {let mut a:[]int=[1]\na[0]+=1\n@println(a[0])}",
    }, [_][]const u8{ "2\n", "2\n2\n" }, [_]usize{ 1, 2 }) |source, output, executions| {
        var module = try Module.init(test_gpa, "observed-local-region.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        try testTransferTopologyRuntime(&sema.ir, true, executions, output);
    }
}

test "copy elision compound updates adopt observed snapshots immediately" {
    for (copy_elision_cases[5..9], [_]usize{ 1, 2, 1, 1 }) |case, executions| {
        var sema = blk: {
            var module = try Module.init(test_gpa, "observed.nov", case.source);
            defer module.deinit();
            var result = try Lowering.lower(&module);
            errdefer result.deinit();
            try std.testing.expect(!module.hasErrors());
            break :blk result;
        };
        defer sema.deinit();
        try testTransferTopologyRuntime(&sema.ir, true, executions, case.output);
    }
}

test "copy elision topology rejects private sibling producers and keeps slot bridges independent" {
    const cases = [_]struct { source: []const u8, runtime: bool }{
        .{
            // The condition is reconstructed on every check, including exhaustion.
            .source = "let main:()={let owner:[]int=[1]\nlet mut bridge:[]int=[0]\nlet mut n:int=0\nloop {let snapshot=owner\nbridge=snapshot\nn<2} {bridge[0]=9\nn+=1}\n@println(owner[0],n)}",
            .runtime = true,
        },
        .{
            .source = "let main:()={let owner:[]int=[1]\nlet mut bridge:[]int=[0]\nlet mut n:int=0\nloop n<2 : {bridge[0]=9\nn+=1} {let snapshot=owner\nbridge=snapshot}\n@println(owner[0],n)}",
            .runtime = true,
        },
        .{
            .source = "let main:()={let mut a:[]int=[1]\nif true {let snapshot=a\n} else {a[0]=9}\n}",
            .runtime = false,
        },
        .{
            .source = "let main:()={let mut a:[]int=[1]\nlet ignored=true and {let snapshot=a\ntrue}\na[0]=9\n}",
            .runtime = false,
        },
    };
    for (cases) |case| {
        var module = try Module.init(test_gpa, "private-transfer-topology.nov", case.source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        const body = sema.ir.functions.items[0].body.?;
        var producer: ?Ir.ValueId = null;
        var producer_slot: usize = 0;
        var consumer: usize = 0;
        for (body.instructions, 0..) |instruction, i| switch (instruction.op) {
            .local_slot => |value| if (body.instructions[@backingInt(value)].op == .array_clone and producer == null) {
                producer = value;
                producer_slot = i;
            },
            .array_set => consumer = i,
            else => {},
        };
        const instructions = @constCast(body.instructions);
        const original_base = instructions[consumer].op.array_set.array;
        // Give the private fresh producer exactly one static use in its sibling.
        instructions[producer_slot].op.local_slot = instructions[@backingInt(producer.?)].op.array_clone;
        instructions[consumer].op.array_set.array = producer.?;
        const transfers = try ArrayTransfers.init(test_gpa, &sema.ir, body);
        defer transfers.deinit(test_gpa);
        try std.testing.expectEqual(@as(usize, 1), transfers.uses[@backingInt(producer.?)]);
        try std.testing.expect(transfers.detached[@backingInt(producer.?)]);
        if (case.runtime) try std.testing.expect(!transfers.canTake(body, producer.?, consumer));
        try std.testing.expectError(error.InvalidValueId, sema.ir.verify());
        try std.testing.expectError(error.InvalidValueId, emit(test_gpa, &sema.ir, null));
        if (case.runtime) {
            // Legal communication across siblings goes through storage. Forge
            // away the usual read snapshot and require the operation to detach.
            instructions[producer_slot].op.local_slot = producer.?;
            instructions[consumer].op.array_set.array = instructions[@backingInt(original_base)].op.array_clone;
            try testTransferTopologyRuntime(&sema.ir, false, 2, "1 2\n");
        }
    }
}

test "copy elision topology nested loop reuse and same region repeated execution" {
    for ([_]bool{ false, true }) |same_region| {
        const source = if (same_region)
            "let main:()={loop _ in 0..3 {let mut a:[]int=[1]\na[0]=9\n@println(a[0])}}"
        else
            "let main:()={let mut a:[]int=[1]\nloop _ in 0..2 {let snapshot=a\nloop _ in 0..2 {a[0]=9}\n}\n@println(a[0])}";
        var module = try Module.init(test_gpa, "loop-transfer-topology.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        const body = sema.ir.functions.items[0].body.?;
        if (!same_region) {
            var producer: ?Ir.ValueId = null;
            for (@constCast(body.instructions)) |*instruction| switch (instruction.op) {
                .local_slot => |*value| if (body.instructions[@backingInt(value.*)].op == .array_clone and producer == null) {
                    producer = value.*;
                    value.* = body.instructions[@backingInt(value.*)].op.array_clone;
                },
                .array_set => |*update| update.array = producer.?,
                else => {},
            };
            const transfers = try ArrayTransfers.init(test_gpa, &sema.ir, body);
            defer transfers.deinit(test_gpa);
            try std.testing.expectEqual(@as(usize, 1), transfers.uses[@backingInt(producer.?)]);
            try std.testing.expect(transfers.detached[@backingInt(producer.?)]);
        }
        try testTransferTopologyRuntime(&sema.ir, same_region, if (same_region) 3 else 4, if (same_region) "9\n9\n9\n" else "9\n");
    }
}

test "copy elision topology branch and short circuit consumers retain static ownership" {
    const cases = [_]struct { source: []const u8, take: bool, executions: usize, output: []const u8 }{
        .{
            .source = "let main:()={let mut a:[]int=[1]\nlet snapshot=a\nif true {a[0]=9}\n@println(a[0])}",
            .take = true,
            .executions = 1,
            .output = "9\n",
        },
        .{
            .source = "let main:()={let mut a:[]int=[1]\nlet snapshot=a\nlet ignored=true and {a[0]=9\ntrue}\n@println(a[0])}",
            .take = true,
            .executions = 1,
            .output = "9\n",
        },
        .{
            .source = "let main:()={let mut a:[]int=[1]\nlet snapshot=a\nif false {a[0]=9}\n@println(a[0])}",
            .take = true,
            .executions = 0,
            .output = "1\n",
        },
        .{
            .source = "let main:()={let mut a:[]int=[1]\nlet snapshot=a\nlet ignored=false and {a[0]=9\ntrue}\n@println(a[0])}",
            .take = true,
            .executions = 0,
            .output = "1\n",
        },
        .{
            .source = "let main:()={let mut a:[]int=[1]\nlet snapshot=a\nlet ignored=true or {a[0]=9\ntrue}\n@println(a[0])}",
            .take = true,
            .executions = 0,
            .output = "1\n",
        },
        .{
            .source = "let main:()={let mut a:[]int=[1]\nlet snapshot=a\nlet ignored=false or {if true {a[0]=9}\ntrue}\n@println(a[0])}",
            .take = true,
            .executions = 1,
            .output = "9\n",
        },
        .{
            // Both mutually exclusive references count. No path-sensitive move
            // policy is inferred, and both prongs execute across the loop.
            .source = "let main:()={loop flag in [true,false] {let mut a:[]int=[1]\nlet snapshot=a\nif flag {a[0]=9} else {a[0]=8}\n@println(a[0])}}",
            .take = false,
            .executions = 2,
            .output = "9\n8\n",
        },
    };
    for (cases) |case| {
        var module = try Module.init(test_gpa, "branch-transfer-topology.nov", case.source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        const body = sema.ir.functions.items[0].body.?;
        var producer: ?Ir.ValueId = null;
        for (@constCast(body.instructions)) |*instruction| switch (instruction.op) {
            .local_slot => |*value| if (body.instructions[@backingInt(value.*)].op == .array_clone and producer == null) {
                producer = value.*;
                value.* = body.instructions[@backingInt(value.*)].op.array_clone;
            },
            .array_set => |*update| update.array = producer.?,
            else => {},
        };
        const transfers = try ArrayTransfers.init(test_gpa, &sema.ir, body);
        defer transfers.deinit(test_gpa);
        try std.testing.expectEqual(@as(usize, if (case.take) 1 else 2), transfers.uses[@backingInt(producer.?)]);
        try std.testing.expect(transfers.detached[@backingInt(producer.?)]);
        try testTransferTopologyRuntime(&sema.ir, case.take, case.executions, case.output);
    }
}

test "copy elision targeted transfers exhaust exact runtime budgets" {
    for (copy_elision_cases[5..]) |case| {
        // This fixture prints between allocations; its failure prefixes are
        // checked separately below rather than assuming empty output.
        if (std.mem.eql(u8, case.source, copy_elision_effects)) continue;
        const program = try arrayTestProgram(case.source);
        defer test_gpa.free(program);
        // Every remaining allocation must still fail cleanly, including partial
        // nested records and the final append buffer, with collection forced.
        for (0..case.allocations + 1) |budget| {
            const prefix = try std.fmt.allocPrint(test_gpa, "#include <stdlib.h>\nstatic unsigned allocations;\n" ++
                "static void *test_malloc(size_t n) {{ return ++allocations > {d} ? NULL : malloc(n); }}\n" ++
                "#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", .{budget});
            defer test_gpa.free(prefix);
            const c = try std.mem.concat(test_gpa, u8, &.{ prefix, program });
            defer test_gpa.free(c);
            const success = budget == case.allocations;
            try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", if (success) 0 else 1, if (success) case.output else "", if (success) "" else "panic: GC allocation failed\n");
        }
    }
}

test "copy elision runtime OOM preserves required snapshots before effects" {
    for ([_]struct { source: []const u8, budget: usize }{
        .{ .source = copy_elision_cases[0].source, .budget = 1 },
        .{ .source = copy_elision_cases[1].source, .budget = 1 },
        .{ .source = copy_elision_cases[2].source, .budget = 1 },
        .{ .source = copy_elision_effects, .budget = 1 },
        .{ .source = copy_elision_effects, .budget = 2 },
        .{ .source = copy_elision_effects, .budget = 7 },
    }) |case| {
        const program = try arrayTestProgram(case.source);
        defer test_gpa.free(program);
        const prefix = try std.fmt.allocPrint(
            test_gpa,
            "#include <stdlib.h>\nstatic unsigned allocations;\n" ++
                "static void *test_malloc(size_t n) {{ return ++allocations > {d} ? NULL : malloc(n); }}\n" ++
                "#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n",
            .{case.budget},
        );
        defer test_gpa.free(prefix);
        const exhausted = try std.mem.concat(test_gpa, u8, &.{ prefix, program });
        defer test_gpa.free(exhausted);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", 1, "", "panic: GC allocation failed\n", mode);
    }
}

fn testCopyElisionOwnership(allocator: std.mem.Allocator, source: []const u8) !void {
    var sema = blk: {
        var module = try Module.init(allocator, "copy-elision.nov", source);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        break :blk result;
    };
    defer sema.deinit();
    try sema.ir.verify();
    const c = try emitProgram(allocator, &sema.ir, null);
    defer allocator.free(c);
}

test "copy elision owns source free IR through every compiler allocation failure" {
    for (copy_elision_cases) |case|
        try TestAllocations.checkAllAllocationFailures(test_gpa, testCopyElisionOwnership, .{case.source});
}

const array_fields_program =
    \\let Box:(T:type)->type=struct{value:T,}
    \\let Leaf=struct{values:[]int,text:string,}
    \\let Tree=struct{leaf:Leaf,records:[]Leaf,}
    \\let U=union{some:Tree,none,}
    \\let leaf:(n:int)->Leaf=Leaf[.values=[n,n+1],.text="shared"+"!"]
    \\let tree:()->Tree=Tree[.leaf=leaf(1),.records=[leaf(3),leaf(5)]]
    \\let identity:(t:Tree)->Tree=t
    \\let main:()={
    \\let mut a=tree()
    \\let saved=identity(a)
    \\let snapshot=()->Tree {a}
    \\let frozen=saved
    \\let immutable=()->Tree {frozen}
    \\let change=() {a.leaf.values[0]+=10}
    \\let same=change
    \\change()
    \\same()
    \\let mut read=snapshot()
    \\read.leaf.values[0]=90
    \\a.leaf.values += [9]
    \\a.leaf.values[1]={a.leaf=leaf(7)
    \\8}
    \\let mut records=a.records
    \\let mut first=records[0]
    \\first.values[0]=30
    \\records[0]=first
    \\records += [leaf(9)]
    \\a.records=records
    \\let boxed=Box(Tree)[.value=a]
    \\let mut branch=if true {boxed.value} else {tree()}
    \\branch.leaf.values[0]=40
    \\let mut looped=loop {break saved}
    \\looped.leaf.values[0]=50
    \\let u=U.some[saved]
    \\let mut payload=match u {.some=>|v|v
    \\.none=>tree()}
    \\payload.leaf.values[0]=60
    \\@println(immutable().leaf.values[0],snapshot().leaf.values[0],read.leaf.values[0],a.leaf.values[0],a.leaf.values[1],a.leaf.values.len)
    \\@println(saved.records[0].values[0],a.records[0].values[0],a.records[-1].values[0],boxed.value.leaf.values[0],branch.leaf.values[0],looped.leaf.values[0],payload.leaf.values[0])
    \\}
;
const array_fields_output = "1 21 90 21 8 3\n3 30 9 21 40 50 60\n";

test "array fields recursively detach all value boundaries under forced GC" {
    const c = try arrayTestProgram(array_fields_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, array_fields_output, "", mode);
}

test "array fields two managed union alternatives copy capture and match under forced GC" {
    const c = try arrayTestProgram(
        \\let Left=struct{values:[]int,text:string,}
        \\let Right=struct{items:[]Left,extra:[]string,}
        \\let U=union{left:Left,right:Right,none,}
        \\let identity:(u:U)->U={let copy=u
        \\return copy}
        \\let make:(which:uint)->U=if which==0 {.left[Left[.values=[1,2],.text="left"+"!"]]} else if which==1 {
        \\.right[Right[.items=[Left[.values=[3,4],.text="right"+"!"]],.extra=["extra"+"!"]]]
        \\} else {.none[]}
        \\let capture:(u:U)->()->U={let frozen=identity(u)
        \\()->U {frozen}}
        \\let show:(u:U)={match u {
        \\.left=>|p|{let mut changed=p
        \\changed.values[-1]=99
        \\@println("L",p.values[0],p.values[-1],p.text,changed.values[-1])}
        \\.right=>|p|{let mut changed=p
        \\let mut item=changed.items[0]
        \\item.values[-1]=88
        \\changed.items[0]=item
        \\changed.extra[0]="changed"+"!"
        \\@println("R",p.items[0].values[0],p.items[0].values[-1],p.items[0].text,p.extra[0],changed.items[0].values[-1])}
        \\.none=>@println("N")
        \\}}
        \\let main:()={loop i in 0..3 {
        \\let mut original=make(i)
        \\let copy=identity(original)
        \\let read=capture(copy)
        \\original=.none[]
        \\show(identity(read()))
        \\show(copy)
        \\show(original)
        \\}}
    );
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "L 1 2 left! 99\nL 1 2 left! 99\nN\nR 3 4 right! extra! 88\nR 3 4 right! extra! 88\nN\nN\nN\nN\n", "", mode);
}

test "array fields negative indexed nested store snapshots array but keeps latest siblings" {
    const c = try arrayTestProgram(
        \\let Inner=struct{values:[]int,sibling:string,}
        \\let Outer=struct{inner:Inner,sibling:string,}
        \\let make:(n:int,s:string)->Outer=Outer[.inner=Inner[.values=[n,n+1,n+2],.sibling=s+" inner"],.sibling=s+" outer"]
        \\let main:()={let mut owner=make(1,"old")
        \\let saved=owner
        \\owner.inner.values[{owner=make(10,"index")
        \\owner.inner.values=[10]
        \\owner.inner.sibling="index"+" sibling"
        \\let negative:int=-1
        \\@print("I")
        \\negative}]={owner=make(20,"rhs")
        \\owner.inner.values=[20,21]
        \\owner.inner.sibling="latest"+" inner"
        \\owner.sibling="latest"+" outer"
        \\@print("R")
        \\9}
        \\@println(owner.inner.values[0],owner.inner.values[1],owner.inner.values[-1],owner.inner.values.len,owner.inner.sibling,owner.sibling)
        \\@println(saved.inner.values[0],saved.inner.values[-1],saved.inner.sibling,saved.sibling)
        \\}
    );
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "IR1 2 9 3 latest inner latest outer\n1 3 old inner old outer\n", "", mode);
}

test "array fields transitive escaping immutable snapshots and shared mutable cells after frame release" {
    const c = try arrayTestProgram(
        \\let Leaf=struct{values:[]int,text:string,}
        \\let Record=struct{leaf:Leaf,items:[]Leaf,}
        \\let make:(n:int)->Record=Record[.leaf=Leaf[.values=[n,n+1],.text="leaf"+"!"],.items=[Leaf[.values=[n+2],.text="item"+"!"]]]
        \\let factory:(seed:int)->()->()->Record={
        \\let immutable=make(seed)
        \\let mut mutable=immutable
        \\let middle=()->()->Record {
        \\()->Record {
        \\mutable.leaf.values[0]+=1
        \\let mut item=mutable.items[0]
        \\item.values[0]+=10
        \\mutable.items[0]=item
        \\let mut ordinary=mutable
        \\ordinary.leaf.values[0]=999
        \\let mut detached=ordinary.items[0]
        \\detached.values[0]=999
        \\ordinary.items[0]=detached
        \\@println(immutable.leaf.values[0],immutable.items[0].values[0],mutable.leaf.values[0],mutable.items[0].values[0],immutable.leaf.text,mutable.items[0].text)
        \\immutable
        \\}}
        \\mutable.leaf.values[0]+=100
        \\middle}
        \\let escape:(seed:int)->()->Record={let middle=factory(seed)
        \\middle()}
        \\let main:()={let middle=factory(1)
        \\let first=middle()
        \\let second=middle()
        \\let shared=first
        \\let mut result=first()
        \\result.leaf.values[0]=700
        \\let again=second()
        \\let third=shared()
        \\let escaped=escape(5)
        \\let other=escaped()
        \\@println(result.leaf.values[0],again.leaf.values[0],third.leaf.values[0],other.leaf.values[0])
        \\}
    );
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "1 3 102 13 leaf! item!\n1 3 103 23 leaf! item!\n1 3 104 33 leaf! item!\n5 7 106 17 leaf! item!\n700 1 1 5\n", "", mode);
}

test "array fields record element snapshots precede index and append effects" {
    const c = try arrayTestProgram(
        \\let A=struct{a:[]int,}
        \\let main:()={
        \\let mut records=[A[.a=[1]]]
        \\let snapshot=records[{let loan=&records[0]
        \\loan.a=[9]
        \\0}]
        \\@println(snapshot.a[0],records[0].a[0])
        \\records += [{let loan=&records[0]
        \\loan.a=[8]
        \\A[.a=[2]]}]
        \\@println(records[0].a[0],records[1].a[0])
        \\}
    );
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "1 9\n9 2\n", "", mode);
}

test "array fields whole record and record element loans preserve ordinary copies" {
    const c = try arrayTestProgram(
        \\let A=struct{a:[]int,}
        \\let main:()={
        \\let mut owner=A[.a=[1]]
        \\let loan=&owner
        \\let mut detached=loan
        \\loan.a += [2]
        \\detached.a[0]=9
        \\@println(loan.a[0],loan.a.len,detached.a[0],detached.a.len)
        \\let mut records=[A[.a=[3]]]
        \\let element=&records[0]
        \\let mut old=element
        \\element.a += [4]
        \\old.a[0]=8
        \\@println(element.a[0],element.a.len,old.a[0],old.a.len)
        \\}
    );
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "1 2 9 1\n3 2 8 1\n", "", mode);
}

test "array fields insertion replacement and append backing pointers detach at operation boundaries" {
    const source =
        \\let Leaf=struct{values:[]int,text:string,}
        \\let Tree=struct{leaf:Leaf,records:[]Leaf,}
        \\let Wrapped=union{node:Tree,none,}
        \\let main:()={
        \\let seed=Leaf[.values=[1,2],.text="shared"+"!"]
        \\let tree=Tree[.leaf=seed,.records=[seed,seed]]
        \\let wrapped=Wrapped.node[tree]
        \\match wrapped {.node=>|snapshot|{
        \\let mut inserted=[snapshot,snapshot]
        \\inserted[-1]=tree
        \\inserted += [tree,tree]
        \\let rhs=inserted
        \\inserted += rhs
        \\inserted += inserted
        \\@println(inserted.len,inserted[0].leaf.values[0],inserted[-1].records[-1].text)
        \\}
        \\.none=>{}
        \\}
        \\}
    ;
    var module = try Module.init(test_gpa, "array-insertion-pointers.nov", source);
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    const body = sema.ir.functions.items[0].body.?;
    var tree_id: ?Ir.TypeId = null;
    for (body.instructions) |instruction| {
        const ty = sema.ir.getType(instruction.ty).?;
        if (ty == .record and std.mem.eql(u8, ty.record.fields[0].name, "leaf")) tree_id = instruction.ty;
    }
    try std.testing.expect(tree_id != null);
    const tree = tree_id.?;
    const leaf = sema.ir.getType(tree).?.record.fields[0].ty;
    const checker = try std.fmt.allocPrint(test_gpa, "static void test_leaf_independent(nov_record_{d} a, nov_record_{d} b) {{\n" ++
        "assert(a.f0.data != b.f0.data); assert(a.f0.len == b.f0.len);\n" ++
        "assert(a.f1.owner == b.f1.owner); assert(a.f1.data == b.f1.data); assert(a.f1.len == b.f1.len);\n}}\n" ++
        "static void test_tree_independent(nov_record_{d} a, nov_record_{d} b) {{\n" ++
        "test_leaf_independent(a.f0, b.f0); assert(a.f1.data != b.f1.data); assert(a.f1.len == b.f1.len);\n" ++
        "nov_record_{d} *x = a.f1.data, *y = b.f1.data;\n" ++
        "for (uintptr_t i = 0; i < a.f1.len; ++i) {{\n" ++
        "test_leaf_independent(a.f0, x[i]); test_leaf_independent(b.f0, y[i]);\n" ++
        "for (uintptr_t j = 0; j < b.f1.len; ++j) test_leaf_independent(x[i], y[j]);\n" ++
        "for (uintptr_t j = 0; j < i; ++j) {{test_leaf_independent(x[i], x[j]); test_leaf_independent(y[i], y[j]);}}\n" ++
        "}}\n}}\n" ++
        "static void test_tree_transferred(nov_record_{d} a, nov_record_{d} b) {{ assert(a.f0.f0.data == b.f0.f0.data); assert(a.f1.data == b.f1.data); }}\nvoid nov_main(void);", .{ @backingInt(leaf), @backingInt(leaf), @backingInt(tree), @backingInt(tree), @backingInt(leaf), @backingInt(tree), @backingInt(tree) });
    defer test_gpa.free(checker);
    const library = try emit(test_gpa, &sema.ir, null);
    defer test_gpa.free(library);
    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, library, "void nov_main(void);"));
    var instrumented = try std.mem.replaceOwned(u8, test_gpa, library, "void nov_main(void);", checker);
    defer test_gpa.free(instrumented);
    var literals: usize = 0;
    var replacements: usize = 0;
    var appends: usize = 0;
    for (body.instructions, 0..) |instruction, i| {
        const ty = sema.ir.getType(instruction.ty).?;
        if (ty != .array or ty.array != tree) continue;
        switch (instruction.op) {
            .array_literal, .array_set, .array_append => {},
            else => continue,
        }
        // Assert immediately after the operation, before any ordinary read or
        // later boundary copy could conceal shared insertion backing.
        var operation = std.Io.Writer.Allocating.init(test_gpa);
        defer operation.deinit();
        const transfers = try ArrayTransfers.init(test_gpa, &sema.ir, body);
        defer transfers.deinit(test_gpa);
        try writeArrayValue(&sema.ir, &operation.writer, body, transfers, instruction, i);
        var checks = std.Io.Writer.Allocating.init(test_gpa);
        defer checks.deinit();
        try checks.writer.writeAll(operation.written());
        switch (instruction.op) {
            .array_literal => |elements| {
                literals += 1;
                for (elements, 0..) |value, index| {
                    try checks.writer.print("    test_tree_{s}(((nov_record_{d} *)v{d}.data)[{d}], v{d});\n", .{ if (transfers.canTake(body, value, i)) "transferred" else "independent", @backingInt(tree), i, index, @backingInt(value) });
                }
            },
            .array_set => |update| {
                replacements += 1;
                try checks.writer.print("    assert(v{d}.data {s} v{d}.data);\n", .{ i, if (transfers.canTake(body, update.array, i)) "==" else "!=", @backingInt(update.array) });
                try checks.writer.print("    test_tree_{s}(((nov_record_{d} *)v{d}.data)[nov_array_index(v{d}, v{d})], v{d});\n", .{ if (transfers.canTake(body, update.value, i)) "transferred" else "independent", @backingInt(tree), i, i, @backingInt(update.index), @backingInt(update.value) });
                // A transferred base is the output itself after the write.
                if (!transfers.canTake(body, update.array, i)) try checks.writer.print("    for (uintptr_t j = 0; j < v{d}.len; ++j) if (j != nov_array_index(v{d}, v{d})) test_tree_independent(((nov_record_{d} *)v{d}.data)[j], ((nov_record_{d} *)v{d}.data)[j]);\n", .{ i, i, @backingInt(update.index), @backingInt(tree), i, @backingInt(tree), @backingInt(update.array) });
            },
            .array_append => |append| {
                appends += 1;
                // Projected storage is not a detached temporary, even though
                // the fresh append result is. Only the literal can transfer.
                try std.testing.expectEqual(body.instructions[@backingInt(append.rhs)].op == .array_literal, transfers.canTake(body, append.rhs, i));
                try checks.writer.print("    assert(v{d}.data != v{d}.data && v{d}.data != v{d}.data);\n", .{ i, @backingInt(append.lhs), i, @backingInt(append.rhs) });
                for ([_]Ir.ValueId{ append.lhs, append.rhs }, 0..) |value, side| {
                    try checks.writer.print("    for (uintptr_t j = 0; j < v{d}.len; ++j) test_tree_{s}(((nov_record_{d} *)v{d}.data)[j + ", .{ @backingInt(value), if (transfers.canTake(body, value, i)) "transferred" else "independent", @backingInt(tree), i });
                    if (side == 0) try checks.writer.writeByte('0') else try checks.writer.print("v{d}.len", .{@backingInt(append.lhs)});
                    try checks.writer.print("], ((nov_record_{d} *)v{d}.data)[j]);\n", .{ @backingInt(tree), @backingInt(value) });
                }
            },
            else => unreachable,
        }
        try checks.writer.print("    for (uintptr_t j = 0; j < v{d}.len; ++j) for (uintptr_t k = 0; k < j; ++k) test_tree_independent(((nov_record_{d} *)v{d}.data)[j], ((nov_record_{d} *)v{d}.data)[k]);\n", .{ i, @backingInt(tree), i, @backingInt(tree), i });
        try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, instrumented, operation.written()));
        const replaced = try std.mem.replaceOwned(u8, test_gpa, instrumented, operation.written(), checks.written());
        test_gpa.free(instrumented);
        instrumented = replaced;
    }
    try std.testing.expectEqual(@as(usize, 2), literals);
    try std.testing.expectEqual(@as(usize, 1), replacements);
    try std.testing.expectEqual(@as(usize, 3), appends);
    const prefixed = try std.mem.concat(test_gpa, u8, &.{ "#include <assert.h>\n", instrumented });
    defer test_gpa.free(prefixed);
    const c = try collectingTestProgram(prefixed);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "16 1 shared!\n", "", mode);
}

test "array fields backing pointers are recursively independent and strings share" {
    var module = try Module.init(test_gpa, "array-fields.nov", array_fields_program);
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    var tree_id: Ir.TypeId = undefined;
    for (sema.ir.functions.items) |function| {
        if (std.mem.eql(u8, function.name, "tree")) tree_id = sema.ir.getType(function.ty).?.function.result;
    }
    const leaf_id = sema.ir.getType(tree_id).?.record.fields[0].ty;
    const library = try emit(test_gpa, &sema.ir, null);
    defer test_gpa.free(library);
    const driver = try std.fmt.allocPrint(test_gpa, "#include <assert.h>\nint main(void) {{\n" ++
        "nov_rt_heap.collect_every_allocation = 1;\n" ++
        "nov_record_{d} a = nov_tree();\n" ++
        "void **roots[] = {{ &a.f0.f0.data, &a.f0.f1.owner, &a.f1.data }};\n" ++
        "nov_gc_roots frame = {{0}}; nov_gc_push(&nov_rt_heap, &frame, roots, 3);\n" ++
        "nov_record_{d} b = nov_identity(a);\n" ++
        "assert(a.f0.f0.data != b.f0.f0.data); assert(a.f1.data != b.f1.data);\n" ++
        "assert(a.f0.f1.owner == b.f0.f1.owner);\n" ++
        "nov_record_{d} *x = a.f1.data, *y = b.f1.data;\n" ++
        "for (uintptr_t i = 0; i < a.f1.len; ++i) {{ assert(x[i].f0.data != y[i].f0.data); assert(x[i].f1.owner == y[i].f1.owner); }}\n" ++
        "nov_gc_pop(&nov_rt_heap, &frame); nov_gc_collect(&nov_rt_heap); assert(!nov_rt_heap.live_objects); nov_gc_shutdown(&nov_rt_heap); return 0; }}\n", .{ @backingInt(tree_id), @backingInt(tree_id), @backingInt(leaf_id) });
    defer test_gpa.free(driver);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| {
        const c = try std.mem.concat(test_gpa, u8, &.{ library, driver });
        defer test_gpa.free(c);
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "", "", mode);
    }
}

test "array fields reject recursive layouts and unstable borrowed paths" {
    for ([_][]const u8{
        "let A=struct{a:[]A,}\nlet main:()={}",
        "let A=struct{b:[]B,}\nlet B=struct{a:A,}\nlet main:()={}",
        "let A:(T:type)->type=struct{a:[]A(T),}\nlet main:(a:A(int))={}",
        "let A=struct{a:[][]A,}\nlet main:()={}",
        "let A=struct{a:[]int,}\nlet main:()={let mut a=A[.a=[1]]\nlet r=&a.a\n}",
        "let A=struct{a:[]int,}\nlet main:()={let mut a=A[.a=[1]]\nlet r=&a\nr.a[0]=2}",
    }) |source| {
        var module = try Module.init(test_gpa, "rejected-array-fields.nov", source);
        defer module.deinit();
        try std.testing.expect(!module.hasErrors());
        var sema = Lowering.lower(&module) catch |err| switch (err) {
            error.InvalidModule => {
                try std.testing.expect(module.hasErrors());
                continue;
            },
            else => return err,
        };
        defer sema.deinit();
        try std.testing.expect(module.hasErrors());
    }
}

test "array fields source-free IR and compiler allocation failures" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "array-fields.nov", array_fields_program);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        for (module.diagnostics.items) |err| std.debug.print("{s} at {d}: {s}\n", .{ @tagName(err.tag), err.span.start, module.sourceSlice(err.span) orelse "<invalid>" });
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        break :blk result;
    };
    defer sema.deinit();
    try sema.ir.verify();
    const c = try emitProgram(test_gpa, &sema.ir, null);
    defer test_gpa.free(c);
    try compileAndRunExpected(c, "typedef int nonempty_translation_unit;", 0, array_fields_output, "");
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{array_fields_program});
}

test "managed array compiler metadata survives every allocation failure" {
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{managed_array_program});
}

test "nested records root enumeration prunes large scalar diamonds beside managed roots" {
    var source = std.Io.Writer.Allocating.init(test_gpa);
    defer source.deinit();
    try source.writer.writeAll("let R0=struct{n:int,}\n");
    for (1..31) |i| try source.writer.print("let R{d}=struct{{left:R{d},right:R{d},}}\n", .{ i, i - 1, i - 1 });
    try source.writer.writeAll("let Mixed=struct{scalar:R30,text:string,}\nlet f:(r:R30,s:string)={let copy=r\n}\nlet g:(r:Mixed)={}\nlet h:(a:[]Mixed)->[]Mixed=a");
    const c = try emitSource(source.written(), null);
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "void **nov_roots[] = { &p1.owner, };") != null);
    try std.testing.expect(std.mem.indexOf(u8, c, "void **nov_roots[] = { &p0.f1.owner, };") != null);
    try std.testing.expect(c.len < 32 * 1024);
    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, c, "offsetof(nov_record_"));
}

const integer_borrow_program =
    \\let R=struct{n:int=0,}
    \\let main:()={
    \\let mut x:uint=1
    \\let earlierx=x
    \\let y=&x
    \\y+=1
    \\y*=6
    \\y-=2
    \\y/=2
    \\y%=3
    \\@println(x,earlierx)
    \\let mut a=[R[]]
    \\let r=&a[0]
    \\let z=&x
    \\z+={r.n+=3
    \\2}
    \\@println(x,a[0].n)
    \\let mut small:i8=127
    \\let s=&small
    \\s+=1
    \\@println(small)
    \\let mut byte:u8=0
    \\let b=&byte
    \\b-=1
    \\b*=2
    \\@println(byte)
    \\loop i in 0..3 {let t=&x
    \\t+=i}
    \\@println(x)
    \\let last=&x
    \\let truth=true and {last+=1
    \\true}
    \\@println(x,truth)
    \\}
;

test "static integer borrowing direct slots snapshots mixed loans C11 O0 O3" {
    const c = try arrayTestProgram(integer_borrow_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "2 1\n4 3\n-128\n254\n7\n8 true\n", "", mode);
}

test "static integer borrowing source free emission and every allocation failure" {
    var result = blk: {
        var module = try Module.init(test_gpa, "integer-borrow.nov", integer_borrow_program);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        break :blk result;
    };
    defer result.deinit();
    try result.ir.verify();
    const c = try emitProgram(test_gpa, &result.ir, null);
    defer test_gpa.free(c);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{integer_borrow_program});
}

test "static integer borrowing backend preserves semantic 128 bit rejection" {
    for ([_][]const u8{ "i128", "u128" }) |name| {
        const source = try std.fmt.allocPrint(test_gpa, "let main:()={{let mut x:{s}=1\nlet y=&x\ny+=1\n@println(x)}}", .{name});
        defer test_gpa.free(source);
        var diagnostic: Diagnostic = undefined;
        try std.testing.expectError(error.UnsupportedIntegerWidth, emitSource(source, &diagnostic));
        try std.testing.expectEqual(Diagnostic.Reason.integer_width_unsupported, diagnostic.reason);
    }
}

test "static integer borrowing arithmetic failures terminal RHS and backend width policy" {
    const cases = [_]struct { source: []const u8, output: []const u8 = "", code: u8 = 0, message: []const u8 = "" }{
        .{ .source = "let main:()={let mut x:uint=1\nlet y=&x\ny/=0}", .code = 1, .message = "panic: division by zero\n" },
        .{ .source = "let main:()={let mut x:int=1\nlet y=&x\ny%=0}", .code = 1, .message = "panic: remainder by zero\n" },
        .{ .source = "let main:()={let mut x:i8=-128\nlet y=&x\ny/=-1\n@println(x)}", .output = "-128\n" },
        .{ .source = "let main:()={let mut x:i8=-128\nlet y=&x\ny%=-1\n@println(x)}", .output = "0\n" },
        .{ .source = "let main:()={let mut x:uint=1\nlet y=&x\ny+={@print(\"R\")\nreturn}}", .output = "R" },
        .{ .source = "let main:()={let mut x:uint=1\nlet y=&x\ny+={@panic(\"stop\")}}", .code = 1, .message = "panic: stop\n" },
        .{ .source = "let main:()={let mut x:uint=1\nlet y=&x\nloop {y+={break}}\n@println(x)}", .output = "1\n" },
        .{ .source = "let main:()={let mut x:uint=1\nlet y=&x\nloop i in 0..2 {y+={continue}}\n@println(x)}", .output = "1\n" },
    };
    for (cases) |case| {
        errdefer std.debug.print("integer borrow runtime: {s}\n", .{case.source});
        const c = try emitSource(case.source, null);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(c, "void nov_main(void); int main(void) { nov_main(); return 0; }", case.code, case.output, case.message, mode);
    }
    for ([_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "int", "uint" }) |name| {
        const bits: u7 = if (std.mem.eql(u8, name, "int") or std.mem.eql(u8, name, "uint")) Ir.native_pointer_bits else try std.fmt.parseInt(u7, name[1..], 10);
        const signed = name[0] == 'i';
        const maximum = (@as(i128, 1) << (if (signed) bits - 1 else bits)) - 1;
        const minimum = if (signed) -(@as(i128, 1) << (bits - 1)) else @as(i128, 0);
        const source = try std.fmt.allocPrint(test_gpa, "let main:()={{let mut x:{s}=6\nlet y=&x\ny+=2\ny*=3\ny-=4\ny/=2\ny%=3\nlet mut edge:{s}={d}\nlet e=&edge\ne+=1\n@println(x,edge==@as({s},{d}))}}", .{ name, name, maximum, name, minimum });
        defer test_gpa.free(source);
        const c = try emitSource(source, null);
        defer test_gpa.free(c);
        try std.testing.expect(std.mem.indexOf(u8, c, "nov_rt_heap") == null);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(c, "void nov_main(void); int main(void) { nov_main(); return 0; }", 0, "1 true\n", "", mode);
    }
}

const reusable_range_program =
    \\let bound: (x: uint) -> uint = {@print(x)
    \\x}
    \\let main: () = {
    \\let mut range = bound(0)..bound(3)
    \\let copy = range
    \\loop i in range {@print(i)
    \\range = 8..=9}
    \\loop i in copy {@print(i)}
    \\loop i in range {@print(i)}
    \\loop i in if true {0..2} else {0..=2} {@print(i)}
    \\let chosen = loop {break 4..=5}
    \\loop i in chosen {@print(i)}
    \\loop i in { @print("S")
    \\copy } {@print(i)
    \\continue}
    \\@println()
    \\}
;
test "reusable range values snapshot copy branches and loop results O0 O3" {
    const c = try arrayTestProgram(reusable_range_program);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "03012012890145S012\n", "", mode);
}

test "reusable range source free emission and every compiler allocation failure" {
    var sema = blk: {
        var module = try Module.init(test_gpa, "range-values.nov", reusable_range_program);
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
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{reusable_range_program});
}

test "reusable range runtime boundaries per-loop effects and terminal controls" {
    const cases = [_]struct { source: []const u8, output: []const u8, code: u8, message: []const u8 }{
        .{ .source = "let main: () = {let range=0..10\nloop i in range {@print(\"{i} \")}\n@println()}", .output = "0 1 2 3 4 5 6 7 8 9 \n", .code = 0, .message = "" },
        .{ .source = "let main: () = {let r=@as(u8, 255)..=@as(u8, 255)\nloop i in r {@print(i)\ncontinue}\nloop i in r {@print(i)}\nlet s=@as(i8, -128)..=@as(i8, -127)\nloop i in s {@print(i)}\nlet empty=3..3\nloop _ in empty {@print(\"BAD\")}\nlet reversed=4..=2\nloop _ in reversed {@print(\"BAD\")}\n@println()}", .output = "255255-128-127\n", .code = 0, .message = "" },
        .{ .source = "let main: () = {let r=0..2\nloop _ in 0..2 {loop i in {@print(\"S\")\nr} {@print(i)}}\nlet copy=loop _ in 0..0 {} else {r}\nloop i in copy {@print(i)\nbreak}\nloop i in r {@print(i)}\n@println()}", .output = "S01S01001\n", .code = 0, .message = "" },
        .{ .source = "let end: () -> uint = {@print(\"BAD\")\n1}\nlet main: () = {let r={@print(\"R\")\nreturn}..end()\n}", .output = "R", .code = 0, .message = "" },
        .{ .source = "let main: () = {let r={@print(\"P\")\n@panic(\"stop\")}..{@print(\"BAD\")\n@as(uint, 1)}\n}", .output = "P", .code = 1, .message = "panic: stop\n" },
        .{ .source = "let main: () = {let r=0..3\nloop _ in 0..1 {loop i in {break} {@print(i)}}\n@print(\"\")\n@println()}", .output = "\n", .code = 0, .message = "" },
    };
    for (cases) |case| {
        errdefer std.debug.print("range source: {s}\n", .{case.source});
        const c = try arrayTestProgram(case.source);
        defer test_gpa.free(c);
        for ([_][]const u8{ "-O0", "-O3" }) |mode|
            try compileAndRunMode(c, "typedef int nonempty_translation_unit;", case.code, case.output, case.message, mode);
    }
}

test "reusable range storage does not need a managed runtime" {
    const c = try emitSource("let main: () = {let mut r=0..2\nlet copy=r\nloop i in r {r=8..=9\n@print(i)}\nloop i in copy {@print(i)}}", null);
    defer test_gpa.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_gc_") == null);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "void nov_main(void); int main(void) { nov_main(); return 0; }", 0, "0101", "", mode);
}

const static_borrow_program =
    \\let Name=struct{text:string,}
    \\let G:(T:type)->type=struct{name:T,score:uint=1,enabled:bool=false,small:i8=2,}
    \\let Person=G(Name)
    \\let pick:()->int={@print("I")
    \\@as(int,-1)}
    \\let main:()={
    \\    let mut people=[Person[.name=Name[.text="A"+"da"]]]
    \\    let original=people
    \\    let changed=&people[pick()]
    \\    let saved=changed.name
    \\    changed.name.text+=" Lovelace"
    \\    changed.score+=1
    \\    changed.enabled=true
    \\    changed.small*=3
    \\    let noise="collect"+"!"
    \\    @println(changed.name.text,changed.score,changed.enabled,changed.small,saved.text,noise)
    \\    @println(people[0].name.text,people[0].score,original[0].name.text,original[0].score)
    \\    let again=&people[0]
    \\    again.name=Name[.text="new"+"!"]
    \\    @println(people[0].name.text)
    \\    loop i in 0..3 {let each=&people[0]
    \\        each.score+=1
    \\        @println(i,people[0].score)}
    \\}
;

test "static borrowing owner indexed updates copies once index source free GC C11 O0 O3 and OOM" {
    var result = blk: {
        var module = try Module.init(test_gpa, "borrow-runtime.nov", static_borrow_program);
        defer module.deinit();
        if (module.hasErrors()) {
            var out = std.Io.Writer.Allocating.init(test_gpa);
            defer out.deinit();
            try Diagnostics.renderModule(&module, &out.writer);
            std.debug.print("{s}", .{out.written()});
        }
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        if (module.hasErrors()) for (module.diagnostics.items) |d| std.debug.print("{s}: {s}\n", .{ @tagName(d.tag), module.sourceSlice(d.span).? });
        try std.testing.expect(result.lowering_complete);
        break :blk result;
    };
    defer result.deinit();
    try result.ir.verify();
    const library = try emit(test_gpa, &result.ir, null);
    defer test_gpa.free(library);
    const c = try collectingTestProgram(library);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "IAda Lovelace 2 true 6 Ada collect!\nAda Lovelace 2 Ada 1\nnew!\n0 3\n1 4\n2 5\n", "", mode);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{static_borrow_program});
}

test "static borrowing checks bounds once before subsequent effects" {
    const source =
        \\let R=struct{x:int=0,}
        \\let main:()={let mut a=[R[]]
        \\let r=&a[{@print("index")
        \\@as(int,-2)}]
        \\@print("BAD")
        \\r.x=1}
    ;
    const c = try arrayTestProgram(source);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 1, "index", "panic: array index out of bounds\n", mode);
}

test "static borrowing string RHS runtime allocation failure follows activation" {
    const library = try emitSource("let R=struct{s:string,}\nlet main:()={let mut a=[R[.s=\"old\"]]\nlet r=&a[0]\n@print(\"borrowed\")\nr.s+=\"!\"}", null);
    defer test_gpa.free(library);
    const c = try collectingTestProgram(library);
    defer test_gpa.free(c);
    const exhausted = try std.mem.concat(test_gpa, u8, &.{
        "#include <stdlib.h>\nstatic unsigned allocations;\nstatic void *test_malloc(size_t n) { return ++allocations > 1 ? NULL : malloc(n); }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", c,
    });
    defer test_gpa.free(exhausted);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", 1, "borrowed", "panic: GC allocation failed\n", mode);
}

const borrow_snapshot_program =
    \\let R=struct{x:int=0,s:string="old",}
    \\let main:()={
    \\    let mut a=[R[],R[.x=1]]
    \\    let before=a
    \\    let value=a[{let r=&a[0]
    \\        r.x=9
    \\        r.s="new"+"!"
    \\        @print("I")
    \\        0}]
    \\    @println(value.x,value.s,a[0].x,a[0].s,before[0].x,before[0].s)
    \\    a=[R[],R[.x=1]]
    \\    a+={let r=&a[0]
    \\        r.x=9
    \\        @print("A")
    \\        [R[.x=2]]}
    \\    @println(a[0].x,a[1].x,a[2].x)
    \\    a=[R[],R[.x=1]]
    \\    a[{let r=&a[0]
    \\        r.x=9
    \\        @print("J")
    \\        1}]={let r=&a[0]
    \\        r.x=8
    \\        @print("S")
    \\        R[.x=2]}
    \\    @println(a[0].x,a[1].x)
    \\    a=[R[],R[.x=1]]
    \\    a[1]={let r=&a[0]
    \\        r.x=9
    \\        @print("R")
    \\        R[.x=2]}
    \\    @println(a[0].x,a[1].x)
    \\}
;

test "static borrowing normal array snapshots precede index update and append effects" {
    const library = try emitSource(borrow_snapshot_program, null);
    defer test_gpa.free(library);
    const c = try collectingTestProgram(library);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "I0 old 9 new! 0 old\nA0 1 2\nJS0 2\nR0 2\n", "", mode);
}

const borrow_snapshot_terminal_program =
    \\let R=struct{x:int=0,}
    \\let read:(stop:bool)={let mut a=[R[]]
    \\    let value=a[{if stop {let r=&a[0]
    \\        r.x=9
    \\        @println("exit-index",a[0].x)
    \\        return}
    \\        let r=&a[0]
    \\        r.x=7
    \\        0}]
    \\    @println("read",value.x,a[0].x)}
    \\let append:(stop:bool)={let mut a=[R[]]
    \\    a+=if stop {let r=&a[0]
    \\        r.x=9
    \\        @println("exit-append",a[0].x)
    \\        return} else {let r=&a[0]
    \\        r.x=7
    \\        [R[]]}
    \\    @println("append",a[0].x,a.len)}
    \\let update:(stop:bool)={let mut a=[R[],R[]]
    \\    a[1]=if stop {let r=&a[0]
    \\        r.x=9
    \\        @println("exit-update",a[0].x)
    \\        return} else {let r=&a[0]
    \\        r.x=7
    \\        R[.x=2]}
    \\    @println("update",a[0].x,a[1].x)}
    \\let main:()={read(true)
    \\    read(false)
    \\    append(true)
    \\    append(false)
    \\    update(true)
    \\    update(false)}
;

test "static borrowing snapshots preserve terminal index append and update branches" {
    const library = try emitSource(borrow_snapshot_terminal_program, null);
    defer test_gpa.free(library);
    const c = try collectingTestProgram(library);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "exit-index 9\nread 0 7\nexit-append 9\nappend 0 2\nexit-update 9\nupdate 0 2\n", "", mode);
}

test "static borrowing normal snapshot OOM precedes index and RHS effects" {
    const source = "let R=struct{x:int=0,}\nlet main:()={let mut a=[R[]]\n@print(\"base\")\nlet value=a[{let r=&a[0]\nr.x=9\n@print(\"BAD\")\n0}]\n@println(value.x)}";
    const library = try emitSource(source, null);
    defer test_gpa.free(library);
    const c = try collectingTestProgram(library);
    defer test_gpa.free(c);
    const exhausted = try std.mem.concat(test_gpa, u8, &.{
        "#include <stdlib.h>\nstatic unsigned allocations;\nstatic void *test_malloc(size_t n) { return ++allocations > 1 ? NULL : malloc(n); }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", c,
    });
    defer test_gpa.free(exhausted);
    for ([_][]const u8{ "-O0", "-O3" }) |mode| try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", 1, "base", "panic: GC allocation failed\n", mode);
}

test "static borrowing snapshot lowering and emission survive every allocation failure" {
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{borrow_snapshot_program});
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{borrow_snapshot_terminal_program});
}

const general_borrow_program =
    \\let E=enum{a,b,}
    \\let R=struct{s:string,n:int=1,
    \\let change:(self:R)->R={let mut copy=self
    \\copy.n=99
    \\copy}
    \\}
    \\let O:(T:type)->type=union{some:T,none,}
    \\let inc:(x:int)->int=x+1
    \\let dec:(x:int)->int=x-1
    \\let read:(x:string)->string={let r=&x
    \\return r}
    \\let readArray:(x:[]int)->[]int={let r=&x
    \\return r}
    \\let option:(x:O(string))->O(string)={let r=&x
    \\.some[r.!+"!"]}
    \\let main:()={
    \\let mut b=false
    \\let rb=&b
    \\rb=true
    \\@println(rb,b)
    \\let mut e:E=.a
    \\let re=&e
    \\re=.b
    \\@println(re==.b,e==.b)
    \\let mut s="a"+"b"
    \\let rs=&s
    \\let old=rs
    \\rs+="c"
    \\@println(read(rs),old,s)
    \\let mut r=R[.s="old"+"!"]
    \\let rr=&r
    \\let saved=rr
    \\rr.s+="!"
    \\rr.n+=2
    \\@println(rr.change().n,rr.n,saved.s)
    \\rr=R[.s="new"+"!"]
    \\@println(r.s,r.n)
    \\let mut o:O(string)=.none[]
    \\let ro=&o
    \\ro=.some["yes"+"!"]
    \\@println(match option(ro){.some=>|v|v
    \\.none=>"bad"})
    \\let mut a:[]int=[1,2]
    \\let ra=&a
    \\let copy=ra
    \\ra+=[3]
    \\ra[0]+=4
    \\@println(readArray(ra)[0],copy[0],a.len)
    \\let mut flags=[false]
    \\let rf=&flags[0]
    \\rf=true
    \\@println(rf,flags[0])
    \\let mut nums:[]int=[5]
    \\let rn=&nums[0]
    \\rn*=2
    \\rn=inc(rn)
    \\@println(nums[0])
    \\let mut strings=["a"+"b"]
    \\let rt=&strings[0]
    \\let before=rt
    \\rt+="!"
    \\@println(rt,before,strings[0])
    \\let mut records=[R[.s="one"]]
    \\let item=&records[0]
    \\let snapshot=item
    \\item=R[.s="two"]
    \\@println(snapshot.s,records[0].s)
    \\let mut range=@as(int,0)..2
    \\let rg=&range
    \\rg=@as(int,1)..3
    \\loop i in rg {@print(i)}
    \\@println("")
    \\let mut f=inc
    \\let fun=&f
    \\@println(fun(5))
    \\fun=dec
    \\@println(f(5))
    \\let fixed=true
    \\let readonly=&fixed
    \\@println(readonly)
    \\let fixedArray=["keep"+"!"]
    \\let elem=&fixedArray[0]
    \\@println(read(elem))
    \\}
;

test "general borrowing runtime type matrix copied reads source free GC C11 O0 O3 OOM" {
    var result = blk: {
        var module = try Module.init(test_gpa, "general-borrow.nov", general_borrow_program);
        defer module.deinit();
        if (module.hasErrors()) {
            var out = std.Io.Writer.Allocating.init(test_gpa);
            defer out.deinit();
            try Diagnostics.renderModule(&module, &out.writer);
            std.debug.print("{s}", .{out.written()});
        }
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        if (module.hasErrors()) {
            var out = std.Io.Writer.Allocating.init(test_gpa);
            defer out.deinit();
            try Diagnostics.renderModule(&module, &out.writer);
            std.debug.print("{s}", .{out.written()});
        }
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(result.lowering_complete);
        break :blk result;
    };
    defer result.deinit();
    try result.ir.verify();
    const library = try emit(test_gpa, &result.ir, null);
    defer test_gpa.free(library);
    try std.testing.expect(std.mem.indexOf(u8, library, "generation") == null);
    const c = try collectingTestProgram(library);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "true true\ntrue true\nabc ab abc\n99 3 old!\nnew! 1\nyes!!\n5 1 3\ntrue true\n11\nab! ab ab!\none two\n12\n6\n4\ntrue\nkeep!\n", "", mode);
    try TestAllocations.checkAllAllocationFailures(test_gpa, testArrayEmissionAllocationFailures, .{general_borrow_program});
}

test "general borrowing snapshots before alias index RHS effects and readonly CFG slots" {
    const source =
        \\let use:(x:[]int)->[]int={let r=&x
        \\if true {let copy=r
        \\return copy} else {return r}}
        \\let read:(x:bool)->bool={if x {let r=&x
        \\if r {return r}}
        \\return x}
        \\let main:()={let mut a:[]int=[1,2]
        \\let r=&a
        \\let mut independent=r
        \\independent[0]=99
        \\r[0]={r=[7,8]
        \\5}
        \\r+={r=[9]
        \\[3]}
        \\let before=r[{r[0]=44
        \\0}]
        \\let saved=use(r)
        \\r=[8]
        \\@println(before,saved[0],saved[1],saved[2],independent[0],a[0],read(true),read(false))
        \\let fixed="safe"+"!"
        \\if false {let unused=&fixed
        \\@println(unused)}
        \\@println(fixed)
        \\}
    ;
    const c = try arrayTestProgram(source);
    defer test_gpa.free(c);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(c, "typedef int nonempty_translation_unit;", 0, "5 44 2 3 99 8 true false\nsafe!\n", "", mode);
}

test "general borrowing runtime allocation failure does not need reference validity machinery" {
    const c = try arrayTestProgram("let main:()={let mut s=\"a\"\nlet r=&s\nr+=\"b\"\n@println(r)}");
    defer test_gpa.free(c);
    const exhausted = try std.mem.concat(test_gpa, u8, &.{ "#include <stdlib.h>\nstatic void *test_malloc(size_t n) { (void)n; return NULL; }\n#define NOV_GC_MALLOC test_malloc\n#define NOV_GC_FREE free\n", c });
    defer test_gpa.free(exhausted);
    for ([_][]const u8{ "-O0", "-O3" }) |mode|
        try compileAndRunMode(exhausted, "typedef int nonempty_translation_unit;", 1, "", "panic: GC allocation failed\n", mode);
}
