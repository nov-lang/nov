//! C11 backend foundation for verified typed IR.
//!
//! Emits one translation unit with externally visible `nov_`-prefixed functions
//! and function-pointer typedefs. IR values become ordered locals (only a
//! conditional merge needs a mutable temporary), so
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
        return std.fmt.allocPrint(allocator, "{s}\nint main(void) {{\n    nov_main();\n{s}    return 0;\n}}\n", .{ library, if (usesPrinting(ir)) "    nov_rt_check(fflush(stdout));\n" else "" });
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
            .void, .boolean => {},
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
    try writeRuntime(ir, w);
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
    var panic_used = printing;
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
        .parameter => |index| param_used[index] = true,
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
        .constant, .function_ref => {},
    };

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
            if (candidate.op == .branch) {
                const region = candidate.op.branch;
                const has_value = ir.types.items[@intFromEnum(candidate.ty)] != .void;
                if (region.then_start == i) {
                    if (has_value) {
                        try w.writeAll("    ");
                        try writeType(ir, w, candidate.ty);
                        try w.print(" v{d};\n", .{end});
                    }
                    try w.print("    if (v{d}) {{\n", .{@intFromEnum(region.condition)});
                }
                if (region.else_start == i) {
                    if (has_value) try w.print("    v{d} = v{d};\n", .{ end, @intFromEnum(region.then_value) });
                    try w.writeAll("    } else {\n");
                }
            } else if (candidate.op == .short_circuit and candidate.op.short_circuit.rhs_start == i) {
                const region = candidate.op.short_circuit;
                try w.print("    bool v{d} = v{d};\n    if ({s}v{d}) {{\n", .{ end, @intFromEnum(region.lhs), if (region.kind == .@"or") "!" else "", @intFromEnum(region.lhs) });
            }
        }
        if (instruction.op == .branch) {
            if (ir.types.items[@intFromEnum(instruction.ty)] != .void) {
                try w.print("    v{d} = v{d};\n", .{ i, @intFromEnum(instruction.op.branch.else_value.?) });
            }
            try w.writeAll("    }\n");
            if (ir.types.items[@intFromEnum(instruction.ty)] != .void and !value_used[i]) try w.print("    (void)v{d};\n", .{i});
            continue;
        }
        if (instruction.op == .short_circuit) {
            try w.print("    v{d} = v{d};\n    }}\n", .{ i, @intFromEnum(instruction.op.short_circuit.rhs) });
            if (!value_used[i]) try w.print("    (void)v{d};\n", .{i});
            continue;
        }
        const ty = ir.types.items[@intFromEnum(instruction.ty)];
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
        try w.writeAll("    const ");
        try writeType(ir, w, instruction.ty);
        try w.print(" v{d} = ", .{i});
        switch (instruction.op) {
            .constant => |id| try writeConstant(w, ir.constants.items[@intFromEnum(id)], ty),
            .parameter => |index| try w.print("p{d}", .{index}),
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
            .print, .short_circuit, .branch => unreachable,
        }
        try w.writeAll(";\n");
        if (!value_used[i]) try w.print("    (void)v{d};\n", .{i});
    }
    const result_type = ir.types.items[@intFromEnum(signature.result)];
    if (result_type != .void) try w.print("    return v{d};\n", .{@intFromEnum(body.result)});
}

fn writePrint(ir: *const Ir, w: *std.Io.Writer, body: Ir.Body, print: anytype) WriteError!void {
    if (print.args.len == 0 and !print.newline) try w.writeAll("    nov_rt_check(fputs(\"\", stdout));\n");
    for (print.args, 0..) |arg, i| {
        if (i != 0) try w.writeAll("    nov_rt_check(fputc(' ', stdout));\n");
        const ty = ir.types.items[@intFromEnum(body.instructions[@intFromEnum(arg)].ty)];
        switch (ty) {
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
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    return emit(test_gpa, &sema.ir, diagnostic);
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
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "generated.c", .data = c_source });
    try tmp.dir.writeFile(io, .{ .sub_path = "harness.c", .data = harness });

    // `zig build test` provides the pinned toolchain's `zig cc`; otherwise use `cc`.
    const zig_exe = std.testing.environ.getAlloc(test_gpa, "NOV_TEST_ZIG") catch null;
    defer if (zig_exe) |path| test_gpa.free(path);
    const program = "program" ++ (if (@import("builtin").os.tag == .windows) ".exe" else "");
    const flags = [_][]const u8{ "-std=c11", "-O2", "-fsanitize=undefined", "-fno-sanitize-recover=all", "-Wall", "-Wextra", "-Wpedantic", "-Werror", "generated.c", "harness.c", "-o", program };
    var argv: std.ArrayList([]const u8) = .empty;
    defer argv.deinit(test_gpa);
    if (zig_exe) |path| {
        try argv.appendSlice(test_gpa, &.{ path, "cc" });
    } else {
        try argv.append(test_gpa, "cc");
    }
    try argv.appendSlice(test_gpa, &flags);

    const compiled = try std.process.run(test_gpa, io, .{ .argv = argv.items, .cwd = .{ .dir = tmp.dir } });
    defer test_gpa.free(compiled.stdout);
    defer test_gpa.free(compiled.stderr);
    try expectSuccess(compiled, "C compilation");

    const dir_path = try tmp.dir.realPathFileAlloc(io, ".", test_gpa);
    defer test_gpa.free(dir_path);
    const program_path = try std.fs.path.join(test_gpa, &.{ dir_path, program });
    defer test_gpa.free(program_path);
    const ran = try std.process.run(test_gpa, io, .{ .argv = &.{program_path}, .cwd = .{ .dir = tmp.dir } });
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
