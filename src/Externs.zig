const std = @import("std");
const CEmitter = @import("CEmitter.zig");
const Compilation = @import("Compilation.zig");
const Ir = @import("Ir.zig");
const Lowering = @import("Lowering.zig");
const Module = @import("Module.zig");
const Sema = @import("Sema.zig");
const Source = @import("Source.zig");
const TestAllocations = @import("TestAllocations.zig");

const span: Ir.Span = .{ .start = 0, .end = 1 };
const zero: Ir.ValueId = @fromBackingInt(0);

const Fixture = struct {
    ir: Ir,
    integer: Ir.TypeId,
    unit: Ir.TypeId,
    signature: Ir.TypeId,
    function: Ir.FunctionId,

    fn init(allocator: std.mem.Allocator) !Fixture {
        var ir = Ir.init(allocator);
        errdefer ir.deinit();
        ir.source_len = 1;
        const integer = try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 32 } });
        const unit = try ir.internType(.void);
        const signature = try ir.internType(.{ .function = .{ .params = &.{integer}, .result = integer } });
        const function = try ir.addFunction("identity", signature, span);
        return .{ .ir = ir, .integer = integer, .unit = unit, .signature = signature, .function = function };
    }

    fn body(self: *Fixture, function: Ir.FunctionId) !void {
        try self.ir.setFunctionBody(function, &.{.{ .ty = self.integer, .span = span, .op = .{ .parameter = 0 } }}, zero);
    }
};

fn ownership(allocator: std.mem.Allocator) !void {
    var f = try Fixture.init(allocator);
    defer f.ir.deinit();
    const ir = &f.ir;
    try std.testing.expect(ir.getFunction(f.function).?.extern_name == null);
    const name = try allocator.dupe(u8, "foreign_identity");
    defer allocator.free(name);
    ir.setExternName(f.function, name) catch |err| {
        try std.testing.expect(ir.getFunction(f.function).?.extern_name == null);
        try std.testing.expectError(error.MissingFunctionBody, ir.verify());
        return err;
    };
    @memset(name, 'x');
    try std.testing.expectEqualStrings("foreign_identity", ir.getFunction(f.function).?.extern_name.?);
    try std.testing.expectEqualStrings("identity", ir.getFunction(f.function).?.name);
    for ([_]bool{ true, false }) |self_alias| {
        const old = ir.getFunction(f.function).?;
        ir.setExternName(f.function, if (self_alias) old.extern_name.? else "replacement") catch |err| {
            const retained = ir.getFunction(f.function).?;
            try std.testing.expectEqual(old.extern_name.?.ptr, retained.extern_name.?.ptr);
            try std.testing.expectEqualStrings("foreign_identity", retained.extern_name.?);
            try std.testing.expectEqual(old.name.ptr, retained.name.ptr);
            try std.testing.expect(retained.body == null);
            try ir.verify();
            return err;
        };
    }
    try std.testing.expectEqualStrings("replacement", ir.getFunction(f.function).?.extern_name.?);
    try std.testing.expect(ir.getFunction(f.function).?.body == null);
    try ir.verify();
    const capacity = ir.functions.capacity;
    ir.clearFunctions();
    try std.testing.expectEqual(capacity, ir.functions.capacity);
    try std.testing.expectEqual(@as(usize, 0), ir.functions.items.len);
    try ir.verify();
    const reused = try ir.addFunction("identity", f.signature, span);
    try std.testing.expect(ir.getFunction(reused).?.extern_name == null);
    try ir.setExternName(reused, "replacement");
    try ir.verify();
}

test "extern IR owns copies self aliases replacements and clearFunctions under OOM" {
    try ownership(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, ownership, .{});
}

test "extern IR setter and verifier reject reserved names but permit libc functions" {
    var f = try Fixture.init(std.testing.allocator);
    defer f.ir.deinit();
    const ir = &f.ir;
    for ([_][]const u8{ "abs", "putchar", "malloc", "sin", "sqrt", "time", "signal", "exit", "memcpy", "custom_func", "nov_add", "INTERFACE", "PRIVATE", "p", "v", "p0x" }) |name| {
        try ir.setExternName(f.function, name);
        try ir.verify();
    }
    const original = ir.functions.items[0];
    defer ir.functions.items[0] = original;
    for ([_][]const u8{ "", "_private", "9bad", "a-b", "a\x00b", "é", "main", "while", "bool", "true", "NULL", "INT64_MAX", "INT_LEAST8_MAX", "INT_LEAST64_MIN", "UINT_FAST32_MAX", "INT_FAST16_MIN", "UINT8_C", "PRId64", "SCNuMAX", "uint_fast32_t", "FILE", "EOF", "size_t", "stdout", "p0", "p123", "v0", "v12", "nov_frame", "nov_roots", "nov_target", "nov_rt_x", "nov_gc_x", "nov_fn_x", "nov_record_x", "nov_range_x", "nov_stringFoo", "nov_arrayFoo", "nov_M0", "nov_element_0", "nov_to_string_0", "NOV_GC_H" }) |name| {
        try std.testing.expectError(error.InvalidExternName, ir.setExternName(f.function, name));
        try std.testing.expectEqual(original.extern_name.?.ptr, ir.functions.items[0].extern_name.?.ptr);
        ir.functions.items[0].extern_name = name;
        const result = ir.verify();
        ir.functions.items[0] = original;
        try std.testing.expectError(error.InvalidExternName, result);
    }
    try std.testing.expectError(error.InvalidFunctionId, ir.setExternName(@fromBackingInt(999), "valid"));
    try ir.verify();
}

test "extern IR accepts exactly foreign scalar widths and void results without bodies" {
    var f = try Fixture.init(std.testing.allocator);
    defer f.ir.deinit();
    const ir = &f.ir;
    ir.clearFunctions();
    const boolean = try ir.internType(.boolean);
    for ([_]std.builtin.Signedness{ .signed, .unsigned }) |signedness| {
        for ([_]u16{ 8, 16, 32, 64, Ir.native_pointer_bits }) |bits| {
            const integer = try ir.internType(.{ .integer = .{ .signedness = signedness, .bits = bits } });
            for ([_]Ir.TypeId{ boolean, integer }) |scalar| {
                for ([_]Ir.TypeId{ scalar, f.unit }) |result| {
                    const ty = try ir.internType(.{ .function = .{ .params = &.{scalar}, .result = result } });
                    const function = try ir.addFunction("foreign", ty, span);
                    try ir.setExternName(function, "foreign_scalar");
                    try ir.verify();
                    try std.testing.expect(ir.getFunction(function).?.body == null);
                    ir.clearFunctions();
                }
            }
        }
    }
    const ty = try ir.internType(.{ .function = .{ .params = &.{}, .result = f.unit } });
    const function = try ir.addFunction("stop", ty, span);
    ir.functions.items[@backingInt(function)].noreturn = true;
    try ir.setExternName(function, "foreign_stop");
    try ir.verify();
}

fn rejectSignature(f: *Fixture, ty: Ir.TypeId, expected: anyerror) !void {
    const original = f.ir.functions.items[0];
    defer f.ir.functions.items[0] = original;
    f.ir.functions.items[0].ty = ty;
    try std.testing.expectError(expected, f.ir.setExternName(f.function, "invalid_signature"));
    try std.testing.expect(f.ir.functions.items[0].extern_name == null);
    f.ir.functions.items[0].extern_name = "forged";
    try std.testing.expectError(expected, f.ir.verify());
}

test "extern IR rejects aggregate function and unsupported integer signatures even when forged" {
    var f = try Fixture.init(std.testing.allocator);
    defer f.ir.deinit();
    const ir = &f.ir;
    const string = try ir.internType(.string);
    const array = try ir.internType(.{ .array = f.integer });
    const range = try ir.internType(.{ .range = f.integer });
    const record = try ir.internType(.{ .record = .{ .identity = 0, .fields = &.{} } });
    const tagged = try ir.internType(.{ .record = .{ .identity = 1, .fields = &.{}, .tagged = true } });
    const enumeration = try ir.internType(.{ .enumeration = .{ .identity = 2, .name = "E", .tags = &.{.{ .name = "tag", .span = span }} } });
    for ([_]Ir.TypeId{ string, array, range, record, tagged, enumeration, f.signature }) |bad| {
        for ([_]bool{ false, true }) |as_result| {
            const ty = try ir.internType(.{ .function = .{ .params = if (as_result) &.{} else &.{bad}, .result = if (as_result) bad else f.integer } });
            try rejectSignature(&f, ty, error.InvalidExternSignature);
        }
    }
    for ([_]std.builtin.Signedness{ .signed, .unsigned }) |signedness| {
        for ([_]u16{ 1, 7, 24, 65, 128 }) |bits| {
            const bad = try ir.internType(.{ .integer = .{ .signedness = signedness, .bits = bits } });
            for ([_]bool{ false, true }) |as_result| {
                const ty = try ir.internType(.{ .function = .{ .params = if (as_result) &.{} else &.{bad}, .result = if (as_result) bad else f.integer } });
                try rejectSignature(&f, ty, error.InvalidExternSignature);
            }
        }
    }
}

test "extern IR rejects bodies dual foreign metadata malformed ids signatures and noreturn" {
    var f = try Fixture.init(std.testing.allocator);
    defer f.ir.deinit();
    const ir = &f.ir;
    try ir.setExternName(f.function, "foreign_identity");
    try std.testing.expectError(error.ExternFunctionBody, f.body(f.function));
    try std.testing.expectError(error.InvalidForeignFunction, ir.setExportName(f.function, "exported"));
    const original = ir.functions.items[0];
    defer ir.functions.items[0] = original;
    const instructions = [_]Ir.Instruction{.{ .ty = f.integer, .span = span, .op = .{ .parameter = 0 } }};
    ir.functions.items[0].body = .{ .instructions = &instructions, .result = zero };
    try std.testing.expectError(error.ExternFunctionBody, ir.verify());
    ir.functions.items[0] = original;
    ir.functions.items[0].export_name = "exported";
    try std.testing.expectError(error.InvalidForeignFunction, ir.verify());
    ir.functions.items[0] = original;
    ir.functions.items[0].noreturn = true;
    try std.testing.expectError(error.InvalidReturn, ir.setExternName(f.function, "replacement"));
    try std.testing.expectError(error.InvalidReturn, ir.verify());
    ir.functions.items[0] = original;
    ir.functions.items[0].ty = @fromBackingInt(999);
    try std.testing.expectError(error.InvalidTypeId, ir.setExternName(f.function, "replacement"));
    try std.testing.expectError(error.InvalidTypeId, ir.verify());
    ir.functions.items[0] = original;
    ir.functions.items[0].ty = f.integer;
    try std.testing.expectError(error.InvalidFunctionType, ir.setExternName(f.function, "replacement"));
    try std.testing.expectError(error.InvalidFunctionType, ir.verify());
    ir.functions.items[0] = original;
    const ty = try ir.internType(.{ .function = .{ .params = &.{ f.integer, f.integer }, .result = f.integer } });
    ir.functions.items[0].ty = ty;
    const signature = &ir.types.items[@backingInt(ty)].function;
    const result = signature.result;
    signature.result = @fromBackingInt(999);
    try std.testing.expectError(error.InvalidTypeId, ir.setExternName(f.function, "replacement"));
    try std.testing.expectError(error.InvalidTypeId, ir.verify());
    signature.result = result;
    const params = @constCast(signature.params);
    const param = params[0];
    params[0] = @fromBackingInt(999);
    try std.testing.expectError(error.InvalidTypeId, ir.setExternName(f.function, "replacement"));
    try std.testing.expectError(error.InvalidTypeId, ir.verify());
    params[0] = f.unit;
    try std.testing.expectError(error.VoidParameter, ir.setExternName(f.function, "replacement"));
    try std.testing.expectError(error.VoidParameter, ir.verify());
    params[0] = param;
    ir.functions.items[0] = original;
    try std.testing.expectEqual(original.extern_name.?.ptr, ir.functions.items[0].extern_name.?.ptr);
    try ir.verify();

    var defined = try Fixture.init(std.testing.allocator);
    defer defined.ir.deinit();
    try defined.body(defined.function);
    const body = defined.ir.getFunction(defined.function).?.body.?;
    try std.testing.expectError(error.ExternFunctionBody, defined.ir.setExternName(defined.function, "replacement"));
    try std.testing.expect(defined.ir.getFunction(defined.function).?.extern_name == null);
    try std.testing.expectEqual(body.instructions.ptr, defined.ir.getFunction(defined.function).?.body.?.instructions.ptr);
    try defined.ir.setExportName(defined.function, "exported");
    try std.testing.expectError(error.InvalidForeignFunction, defined.ir.setExternName(defined.function, "replacement"));
    try defined.ir.verify();
}

fn foreignName(ir: *Ir, id: Ir.FunctionId, exported: bool, name: []const u8) !void {
    if (exported) try ir.setExportName(id, name) else try ir.setExternName(id, name);
}

test "extern IR shared symbols require canonical signatures and noreturn in both orders" {
    for ([_]bool{ false, true }) |first_export| {
        for ([_]bool{ false, true }) |second_export| {
            for (0..6) |mismatch| {
                var f = try Fixture.init(std.testing.allocator);
                defer f.ir.deinit();
                const ir = &f.ir;
                const boolean = try ir.internType(.boolean);
                const unit_ty = try ir.internType(.{ .function = .{ .params = &.{}, .result = f.unit } });
                if (mismatch >= 4) ir.functions.items[0].ty = unit_ty;
                if (mismatch == 5) ir.functions.items[0].noreturn = true;
                const other_ty = switch (mismatch) {
                    0 => try ir.internType(.{ .function = .{ .params = &.{f.integer}, .result = f.integer } }),
                    1 => try ir.internType(.{ .function = .{ .params = &.{boolean}, .result = f.integer } }),
                    2 => try ir.internType(.{ .function = .{ .params = &.{f.integer}, .result = boolean } }),
                    3 => try ir.internType(.{ .function = .{ .params = &.{ f.integer, f.integer }, .result = f.integer } }),
                    else => unit_ty,
                };
                try ir.source_lengths.appendSlice(std.testing.allocator, &.{ 1, 1 });
                const other = try ir.addFunction("identity", other_ty, .{ .source = @fromBackingInt(1), .start = 0, .end = 1 });
                if (mismatch == 4) ir.functions.items[@backingInt(other)].noreturn = true;
                try foreignName(ir, f.function, first_export, "shared_symbol");
                const expected: ?anyerror = if (first_export and second_export) error.DuplicateExportName else if (mismatch != 0) error.ConflictingForeignSignature else null;
                if (expected) |err| {
                    try std.testing.expectError(err, foreignName(ir, other, second_export, "shared_symbol"));
                    try std.testing.expect(ir.getFunction(other).?.extern_name == null);
                    try std.testing.expect(ir.getFunction(other).?.export_name == null);
                    const old = ir.functions.items[@backingInt(other)];
                    if (second_export) ir.functions.items[@backingInt(other)].export_name = "shared_symbol" else ir.functions.items[@backingInt(other)].extern_name = "shared_symbol";
                    const verified = ir.verify();
                    ir.functions.items[@backingInt(other)] = old;
                    try std.testing.expectError(err, verified);
                } else {
                    try foreignName(ir, other, second_export, "shared_symbol");
                    if (first_export) try f.body(f.function);
                    if (second_export) try f.body(other);
                    try ir.verify();
                    const a = ir.getFunction(f.function).?;
                    const b = ir.getFunction(other).?;
                    try std.testing.expect((a.extern_name orelse a.export_name.?).ptr != (b.extern_name orelse b.export_name.?).ptr);
                }
            }
        }
    }
}

const Case = struct {
    library: []const u8,
    root: []const u8 = "let main: () = {}",
    named: bool = false,
    rejected: bool = false,
    diagnostic: ?Module.Diagnostic.Tag = null,
    symbol: []const u8 = "foreign_identity",
    extern_count: usize = 1,
    export_count: usize = 0,
    references: usize = 0,
};

const Lowered = struct {
    sema: Sema,
    root: Source.Id,
    library: Source.Id,
    entry: ?Ir.FunctionId,
};

// Everything used by emission below must outlive this compilation and its ASTs.
fn lowerCase(allocator: std.mem.Allocator, case: Case) !Lowered {
    var comp = Compilation.init(allocator);
    defer comp.deinit();
    _ = try comp.addSource("/externs/padding.nov", "");
    const text = try std.mem.concat(allocator, u8, &.{ if (case.named) "let lib = @import(\"library\")\n" else "let lib = @import(\"library.nov\")\n", case.root });
    defer allocator.free(text);
    const root = try comp.addSource("/externs/main.nov", text);
    const library = try comp.addSource("/externs/library.nov", case.library);
    const module = comp.getModule(root).?;
    const local = module.lookup("lib").?;
    const declaration = comp.declaration_ids.get(.{ .source = root, .local = local }).?;
    try comp.imports.append(allocator, .{ .declaration = declaration, .target = library, .named = case.named, .span = module.getDeclaration(local).?.name_span });
    comp.setRoot(root);
    for (comp.modules.items) |source| {
        try std.testing.expectEqual(@as(usize, 0), source.source.ast.errors.len);
        try std.testing.expectEqual(@as(usize, 0), source.diagnostics.items.len);
    }
    if (comp.getModule(library).?.lookup("hidden") != null) {
        try std.testing.expect(comp.lookupNamespace(root, declaration, "hidden") == null);
        try std.testing.expect(comp.lookupNamespace(library, declaration, "hidden") != null);
    }
    var sema = Lowering.lowerCompilation(&comp) catch |err| {
        if (err == error.OutOfMemory) for (comp.modules.items) |source| try std.testing.expectEqual(@as(usize, 0), source.diagnostics.items.len);
        return err;
    };
    errdefer sema.deinit();
    if (comp.hasErrors() != case.rejected) {
        std.debug.print("extern namespace case: {s}\n", .{case.root});
        for (comp.modules.items) |source| for (source.diagnostics.items) |d| std.debug.print("{s}: {t}\n", .{ source.source.path, d.tag });
    }
    try std.testing.expectEqual(case.rejected, comp.hasErrors());
    try std.testing.expectEqual(!case.rejected, sema.lowering_complete);
    var entry: ?Ir.FunctionId = null;
    if (case.rejected) {
        try std.testing.expectEqual(@as(usize, 0), sema.ir.functions.items.len);
        var found = false;
        for (comp.modules.items) |source| for (source.diagnostics.items) |d| {
            try std.testing.expectEqual(source.source.id, d.span.source);
            try std.testing.expect((comp.sourceSlice(d.span) orelse return error.TestUnexpectedResult).len != 0);
            if (d.related) |related| try std.testing.expect((comp.sourceSlice(related) orelse return error.TestUnexpectedResult).len != 0);
            if (case.diagnostic) |tag| {
                if (tag == d.tag) {
                    found = true;
                    if (tag == .conflicting_foreign_signature or tag == .duplicate_export_name) {
                        try std.testing.expect(d.related != null);
                        try std.testing.expect(d.related.?.source != d.span.source);
                    }
                }
            } else if (d.span.source == root) found = true;
        };
        try std.testing.expect(found);
    } else {
        entry = sema.getCompilationEntry(comp.root_main.?).?.function.?;
        try std.testing.expectEqual(root, sema.ir.getFunction(entry.?).?.span.source);
        try std.testing.expect(@backingInt(root) != 0 and @backingInt(library) != 0);
        for (sema.ir.functions.items) |function| {
            try std.testing.expect(function.span.source == root or function.span.source == library);
            try std.testing.expect(comp.sourceSlice(function.span) != null);
            if (function.body) |body| for (body.instructions) |instruction| try std.testing.expect(comp.sourceSlice(instruction.span) != null);
        }
    }
    try sema.ir.verify();
    return .{ .sema = sema, .root = root, .library = library, .entry = entry };
}

fn count(text: []const u8, needle: []const u8, expected: usize) !void {
    try std.testing.expectEqual(expected, std.mem.count(u8, text, needle));
}

fn checkEmission(allocator: std.mem.Allocator, case: Case, text: []const u8) !void {
    const prototype = try std.fmt.allocPrint(allocator, "int32_t {s}(int32_t p0);\n", .{case.symbol});
    defer allocator.free(prototype);
    const definition = try std.fmt.allocPrint(allocator, "int32_t {s}(int32_t p0) {{\n", .{case.symbol});
    defer allocator.free(definition);
    const reference = try std.fmt.allocPrint(allocator, " = {s};", .{case.symbol});
    defer allocator.free(reference);
    // Compatible aliases may each emit a redundant, legal C declaration.
    try count(text, prototype, case.extern_count + case.export_count);
    try count(text, definition, case.export_count);
    try count(text, reference, case.references);
}

fn checkCase(allocator: std.mem.Allocator, case: Case) !void {
    var result = try lowerCase(allocator, case);
    defer result.sema.deinit();
    try result.sema.ir.verify();
    if (case.rejected) return;
    var externs: usize = 0;
    var exports: usize = 0;
    var first_name: ?[]const u8 = null;
    for (result.sema.ir.functions.items) |function| {
        try std.testing.expect(function.span.end <= result.sema.ir.source_lengths.items[@backingInt(function.span.source)]);
        if (function.body) |body| for (body.instructions) |instruction| {
            try std.testing.expect(instruction.span.end <= result.sema.ir.source_lengths.items[@backingInt(instruction.span.source)]);
        };
        if (function.extern_name) |name| {
            externs += 1;
            try std.testing.expect(function.body == null);
            try std.testing.expect(function.export_name == null);
            try std.testing.expectEqualStrings(case.symbol, name);
            if (first_name) |first| try std.testing.expect(first.ptr != name.ptr);
            first_name = name;
            if (std.mem.eql(u8, function.name, "hidden")) try std.testing.expect(!function.public);
            if (std.mem.eql(u8, function.name, "visible")) try std.testing.expect(function.public);
        }
        if (function.export_name) |name| {
            exports += 1;
            try std.testing.expectEqualStrings(case.symbol, name);
            try std.testing.expect(function.body != null);
            if (first_name) |first| try std.testing.expect(first.ptr != name.ptr);
            first_name = name;
        }
    }
    try std.testing.expectEqual(case.extern_count, externs);
    try std.testing.expectEqual(case.export_count, exports);
    const c = try CEmitter.emit(allocator, &result.sema.ir, null);
    defer allocator.free(c);
    try checkEmission(allocator, case, c);
    const program = try CEmitter.emitProgramRoot(allocator, &result.sema.ir, result.entry, .{ .source = result.root, .start = 0, .end = 0 }, null);
    defer allocator.free(program);
    try checkEmission(allocator, case, program);
    const main_call = try std.fmt.allocPrint(allocator, "int main(void) {{\n    nov_M{d}_nov_main();", .{@backingInt(result.root)});
    defer allocator.free(main_call);
    try count(program, main_call, 1);
}

const hidden_library =
    \\@[extern("foreign_identity")]
    \\let hidden: (x: i32) -> i32
    \\@[public]
    \\let wrapper: (x: i32) -> i32 = hidden(x)
    \\@[public]
    \\let Box: (T: type) -> type = struct {
    \\    value: T,
    \\    @[public]
    \\    let get: (self: Box(T)) -> i32 = hidden(7)
    \\}
;

const visible_library = "@[public]\n@[extern(\"foreign_identity\")]\nlet visible: (x: i32) -> i32";
const external_root = "@[extern(\"foreign_identity\")]\nlet local: (x: i32) -> i32\nlet main: () = @println(local(1), lib.visible(2))";
const exported_root = "@[export(\"foreign_identity\")]\nlet local: (x: i32) -> i32 = x\nlet main: () = @println(lib.visible(2))";

test "extern namespace file and named imports keep public independent and defining scopes intact" {
    for ([_]bool{ false, true }) |named| {
        try checkCase(std.testing.allocator, .{ .library = hidden_library, .named = named, .root = "let Box = lib.Box(i32)\nlet hidden: (x: bool) -> bool = x\nlet main: () = { let box = Box[.value=3]\n@println(lib.wrapper(2), box.get()) }", .references = 2 });
        try checkCase(std.testing.allocator, .{ .library = visible_library, .named = named, .root = "let main: () = { let f: (i32) -> i32 = lib.visible\n@println(f(2), lib.visible(3)) }", .references = 2 });
        for ([_][]const u8{
            "let main: () = @println(lib.hidden(1))",
            "let main: () = { let f: (i32) -> i32 = lib.hidden\n@println(f(1)) }",
        }) |root| try checkCase(std.testing.allocator, .{ .library = hidden_library, .named = named, .root = root, .rejected = true });
    }
}

test "extern namespace canonical aliases and extern export aliases own names after sources die" {
    for ([_]bool{ false, true }) |named| {
        try checkCase(std.testing.allocator, .{ .library = "let Alias = i32\n@[public]\n@[extern(\"foreign_identity\")]\nlet visible: (x: Alias) -> Alias", .named = named, .root = external_root, .extern_count = 2, .references = 2 });
        try checkCase(std.testing.allocator, .{ .library = visible_library, .named = named, .root = exported_root, .export_count = 1, .references = 1 });
        try checkCase(std.testing.allocator, .{ .library = "@[public]\n@[export(\"foreign_identity\")]\nlet visible: (x: i32) -> i32 = x", .named = named, .root = external_root, .export_count = 1, .references = 2 });
    }
}

fn checkSemanticCase(allocator: std.mem.Allocator, case: Case) !void {
    var result = try lowerCase(allocator, case);
    defer result.sema.deinit();
    try result.sema.ir.verify();
}

test "extern namespace conflicting signatures noreturn and duplicate exports reject in both source orders under OOM" {
    const external = "@[extern(\"shared_symbol\")]\nlet foreign: (x: i32) -> i32";
    const exported = "@[export(\"shared_symbol\")]\nlet definition: (x: i32) -> i32 = x";
    for ([_]bool{ false, true }) |named| {
        for ([_]struct { first: []const u8, second: []const u8, tag: Module.Diagnostic.Tag = .conflicting_foreign_signature }{
            .{ .first = external, .second = "@[extern(\"shared_symbol\")]\nlet other: (x: u32) -> i32" },
            .{ .first = external, .second = "@[extern(\"shared_symbol\")]\nlet other: (x: i32) -> u32" },
            .{ .first = external, .second = "@[extern(\"shared_symbol\")]\nlet other: (x: i32, y: i32) -> i32" },
            .{ .first = exported, .second = "@[extern(\"shared_symbol\")]\nlet other: ()" },
            .{ .first = "@[noreturn]\n@[extern(\"shared_symbol\")]\nlet stop: ()", .second = "@[extern(\"shared_symbol\")]\nlet other: ()" },
            .{ .first = exported, .second = "@[export(\"shared_symbol\")]\nlet other: (x: i32) -> i32 = x", .tag = .duplicate_export_name },
        }) |conflict| {
            for ([_]bool{ false, true }) |reverse| {
                const root = try std.mem.concat(std.testing.allocator, u8, &.{ if (reverse) conflict.second else conflict.first, "\nlet main: () = {}" });
                defer std.testing.allocator.free(root);
                const case: Case = .{ .library = if (reverse) conflict.first else conflict.second, .root = root, .named = named, .rejected = true, .diagnostic = conflict.tag };
                try checkSemanticCase(std.testing.allocator, case);
                try TestAllocations.checkAllAllocationFailures(std.testing.allocator, checkSemanticCase, .{case});
            }
        }
    }
}

test "extern namespace matching noreturn aliases retain canonical types and source spans" {
    const case: Case = .{
        .library = "@[noreturn]\n@[extern(\"foreign_stop\")]\nlet hidden: ()",
        .root = "@[noreturn]\n@[extern(\"foreign_stop\")]\nlet local: ()\nlet main: () = {}",
    };
    var result = try lowerCase(std.testing.allocator, case);
    defer result.sema.deinit();
    var ty: ?Ir.TypeId = null;
    var names: usize = 0;
    for (result.sema.ir.functions.items) |function| {
        if (function.extern_name) |name| {
            names += 1;
            try std.testing.expectEqualStrings("foreign_stop", name);
            try std.testing.expect(function.noreturn);
            try std.testing.expect(function.body == null);
            if (ty) |previous| try std.testing.expectEqual(previous, function.ty);
            ty = function.ty;
        }
    }
    try std.testing.expectEqual(@as(usize, 2), names);
    const c = try CEmitter.emit(std.testing.allocator, &result.sema.ir, null);
    defer std.testing.allocator.free(c);
    try count(c, "_Noreturn void foreign_stop(void);", 2);
    const original = result.sema.ir.functions.items[0].span;
    defer result.sema.ir.functions.items[0].span = original;
    result.sema.ir.functions.items[0].span.source = @fromBackingInt(999);
    try std.testing.expectError(error.InvalidSourceSpan, result.sema.ir.verify());
    result.sema.ir.functions.items[0].span = original;
    result.sema.ir.functions.items[0].span.end = result.sema.ir.source_lengths.items[@backingInt(original.source)] + 1;
    try std.testing.expectError(error.InvalidSourceSpan, result.sema.ir.verify());
}

test "extern namespace source free emission and rejected graphs survive every OOM" {
    for ([_]bool{ false, true }) |named| {
        for ([_]Case{
            .{ .library = hidden_library, .named = named, .root = "let Box = lib.Box(i32)\nlet main: () = { let box = Box[.value=1]\n@println(lib.wrapper(2), box.get()) }", .references = 2 },
            .{ .library = visible_library, .named = named, .root = external_root, .extern_count = 2, .references = 2 },
            .{ .library = visible_library, .named = named, .root = exported_root, .export_count = 1, .references = 1 },
            .{ .library = "@[public]\n@[export(\"foreign_identity\")]\nlet visible: (x: i32) -> i32 = x", .named = named, .root = external_root, .export_count = 1, .references = 2 },
            .{ .library = hidden_library, .named = named, .root = "let main: () = @println(lib.hidden(1))", .rejected = true },
            .{ .library = "@[extern(\"foreign_identity\")]\nlet hidden: (x: string) -> string", .named = named, .rejected = true, .diagnostic = .invalid_extern_signature },
            .{ .library = visible_library, .named = named, .root = "@[extern(\"foreign_identity\")]\nlet local: (x: bool) -> i32\nlet main: () = {}", .rejected = true, .diagnostic = .conflicting_foreign_signature },
        }) |case| {
            try checkCase(std.testing.allocator, case);
            try TestAllocations.checkAllAllocationFailures(std.testing.allocator, checkCase, .{case});
        }
    }
}
