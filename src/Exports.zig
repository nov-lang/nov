const std = @import("std");
const CEmitter = @import("CEmitter.zig");
const Compilation = @import("Compilation.zig");
const Ir = @import("Ir.zig");
const Lowering = @import("Lowering.zig");
const Module = @import("Module.zig");
const Sema = @import("Sema.zig");
const Source = @import("Source.zig");
const TestAllocations = @import("TestAllocations.zig");

const Export = struct {
    name: []const u8,
    symbol: []const u8,
    signature: []const u8,
    public: bool = false,
};

const Case = struct {
    library: []const u8,
    root: []const u8 = "let main: () = {}",
    named: bool = false,
    exports: []const Export = &.{},
    rejected: bool = false,
    diagnostic: ?Module.Diagnostic.Tag = null,
};

const Lowered = struct {
    sema: Sema,
    entry: ?Ir.FunctionId,
    root: Source.Id,
    library: Source.Id,
};

// No source text, AST, declaration table or import edge survives this helper.
fn lowerCase(allocator: std.mem.Allocator, case: Case) !Lowered {
    var comp = Compilation.init(allocator);
    defer comp.deinit();
    _ = try comp.addSource("/exports/padding.nov", "");
    const root_text = try std.mem.concat(allocator, u8, &.{
        if (case.named) "let lib = @import(\"library\")\n" else "let lib = @import(\"library.nov\")\n",
        case.root,
    });
    defer allocator.free(root_text);
    const root = try comp.addSource("/exports/main.nov", root_text);
    const library_id = try comp.addSource("/exports/library.nov", case.library);
    try std.testing.expect(@backingInt(root) != 0);
    try std.testing.expect(@backingInt(library_id) != 0);
    const module = comp.getModule(root).?;
    const local = module.lookup("lib").?;
    const declaration = comp.declaration_ids.get(.{ .source = root, .local = local }).?;
    try comp.imports.append(allocator, .{
        .declaration = declaration,
        .target = library_id,
        .named = case.named,
        .span = module.getDeclaration(local).?.name_span,
    });
    comp.setRoot(root);
    for (comp.modules.items) |source| {
        try std.testing.expectEqual(@as(usize, 0), source.source.ast.errors.len);
        try std.testing.expectEqual(@as(usize, 0), source.diagnostics.items.len);
    }
    for (case.exports) |exported| {
        try std.testing.expectEqual(exported.public, comp.lookupNamespace(root, declaration, exported.name) != null);
        try std.testing.expect(comp.lookupNamespace(library_id, declaration, exported.name) != null);
    }
    var sema = Lowering.lowerCompilation(&comp) catch |err| {
        if (err == error.OutOfMemory) for (comp.modules.items) |source| {
            try std.testing.expectEqual(@as(usize, 0), source.diagnostics.items.len);
        };
        return err;
    };
    errdefer sema.deinit();
    if (comp.hasErrors() != case.rejected) {
        std.debug.print("export namespace case: {s}\n", .{case.root});
        for (comp.modules.items) |source| for (source.diagnostics.items) |d| {
            std.debug.print("{s}: {t}: {s}\n", .{ source.source.path, d.tag, comp.sourceSlice(d.span) orelse "?" });
        };
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
            if (d.related) |span| try std.testing.expect(comp.sourceSlice(span) != null);
            if (case.diagnostic) |tag| {
                if (d.tag == tag) found = true;
            } else if (d.span.source == root) found = true;
        };
        try std.testing.expect(found);
    } else {
        entry = sema.getCompilationEntry(comp.root_main.?).?.function.?;
        try std.testing.expectEqual(root, sema.ir.getFunction(entry.?).?.span.source);
        try sema.ir.verify();
    }
    return .{ .sema = sema, .entry = entry, .root = root, .library = library_id };
}

fn expectCount(text: []const u8, needle: []const u8, count: usize) !void {
    try std.testing.expectEqual(count, std.mem.count(u8, text, needle));
}

fn checkExports(allocator: std.mem.Allocator, result: *const Lowered, case: Case, c: []const u8) !void {
    var export_count: usize = 0;
    for (result.sema.ir.functions.items) |function| {
        if (function.export_name != null) export_count += 1;
    }
    try std.testing.expectEqual(case.exports.len, export_count);
    for (case.exports) |exported| {
        var found: usize = 0;
        for (result.sema.ir.functions.items) |function| {
            if (function.span.source != result.library or !std.mem.eql(u8, function.name, exported.name)) continue;
            found += 1;
            try std.testing.expectEqualStrings(exported.symbol, function.export_name.?);
            try std.testing.expectEqual(exported.public, function.public);
        }
        try std.testing.expectEqual(@as(usize, 1), found);
        const prototype = try std.fmt.allocPrint(allocator, "{s};\n", .{exported.signature});
        defer allocator.free(prototype);
        const definition = try std.fmt.allocPrint(allocator, "{s} {{\n", .{exported.signature});
        defer allocator.free(definition);
        try expectCount(c, prototype, 1);
        try expectCount(c, definition, 1);
    }
}

fn checkCase(allocator: std.mem.Allocator, case: Case) !void {
    var result = try lowerCase(allocator, case);
    defer result.sema.deinit();
    try result.sema.ir.verify();
    if (case.rejected) return;
    const c = try CEmitter.emit(allocator, &result.sema.ir, null);
    defer allocator.free(c);
    try checkExports(allocator, &result, case, c);
    const program = try CEmitter.emitProgramRoot(allocator, &result.sema.ir, result.entry, .{ .source = result.root, .start = 0, .end = 0 }, null);
    defer allocator.free(program);
    try checkExports(allocator, &result, case, program);
    const entry_call = try std.fmt.allocPrint(allocator, "int main(void) {{\n    nov_M{d}_nov_main();", .{@backingInt(result.root)});
    defer allocator.free(entry_call);
    try expectCount(program, entry_call, 1);
}

const library =
    \\@[export]
    \\let hidden_exact: (x: i32) -> i32 = x
    \\@[export("external_custom")]
    \\let hiddenCustom: (x: bool) -> bool = x
    \\@[public]
    \\@[export("external_visible")]
    \\let visible: (x: i32) -> i32 = hidden_exact(x)
    \\@[public]
    \\let wrapper: (x: bool) -> bool = hiddenCustom(x)
    \\@[public]
    \\let publicOnly: (x: i32) -> i32 = x
    \\@[export]
    \\let unused_exact: (x: i32) -> i32 = x
    \\@[export("external_unused")]
    \\let unusedCustom: (x: bool) -> bool = x
;

const exports = [_]Export{
    .{ .name = "hidden_exact", .symbol = "hidden_exact", .signature = "int32_t hidden_exact(int32_t p0)" },
    .{ .name = "hiddenCustom", .symbol = "external_custom", .signature = "bool external_custom(bool p0)" },
    .{ .name = "visible", .symbol = "external_visible", .signature = "int32_t external_visible(int32_t p0)", .public = true },
    .{ .name = "unused_exact", .symbol = "unused_exact", .signature = "int32_t unused_exact(int32_t p0)" },
    .{ .name = "unusedCustom", .symbol = "external_unused", .signature = "bool external_unused(bool p0)" },
};

const accepted_root = "let main: () = @println(lib.visible(7), lib.wrapper(true), lib.publicOnly(8))";

test "export namespace file and named imports keep export separate from public" {
    for ([_]bool{ false, true }) |named| {
        try checkCase(std.testing.allocator, .{ .library = library, .exports = &exports, .named = named, .root = accepted_root });
        for ([_][]const u8{
            "let main: () = @println(lib.hidden_exact(1))",
            "let main: () = @println(lib.hiddenCustom(true))",
            "let main: () = { let f = lib.hidden_exact\n@println(f(1)) }",
        }) |root| {
            try checkCase(std.testing.allocator, .{ .library = library, .exports = &exports, .named = named, .root = root, .rejected = true });
        }
    }
}

test "export namespace unreferenced private exports emit once and public only stays mangled" {
    const allocator = std.testing.allocator;
    for ([_]bool{ false, true }) |named| {
        const case: Case = .{ .library = library, .exports = &exports, .named = named };
        try checkCase(allocator, case);
        var result = try lowerCase(allocator, case);
        defer result.sema.deinit();
        const c = try CEmitter.emit(allocator, &result.sema.ir, null);
        defer allocator.free(c);
        for ([_][]const u8{ "nov_hidden__exact", "nov_hiddenCustom", "nov_visible", "nov_unused__exact", "nov_unusedCustom", "external_custom_", "external_visible_", "external_unused_" }) |wrong| {
            try expectCount(c, wrong, 0);
        }
        const mangled = try std.fmt.allocPrint(allocator, "int32_t nov_M{d}_nov_publicOnly(int32_t p0)", .{@backingInt(result.library)});
        defer allocator.free(mangled);
        try expectCount(c, mangled, 2);
        try expectCount(c, "int32_t publicOnly(", 0);
        for (result.sema.ir.functions.items) |function| {
            if (std.mem.eql(u8, function.name, "publicOnly")) {
                try std.testing.expect(function.public);
                try std.testing.expect(function.export_name == null);
            }
        }
    }
}

const recursive_library =
    \\@[export("external_recur")]
    \\let recurse: (n: i32) -> i32 = if n == 0 { 0 } else { recurse(n - 1) }
    \\@[public]
    \\let wrapper: (n: i32) -> i32 = {
    \\    let f: (i32) -> i32 = recurse
    \\    f(n) + recurse(n)
    \\}
;

const recursive_exports = [_]Export{
    .{ .name = "recurse", .symbol = "external_recur", .signature = "int32_t external_recur(int32_t p0)" },
};

test "export namespace recursion internal calls and function values use raw symbols" {
    const allocator = std.testing.allocator;
    for ([_]bool{ false, true }) |named| {
        const case: Case = .{
            .library = recursive_library,
            .exports = &recursive_exports,
            .named = named,
            .root = "let main: () = @println(lib.wrapper(2))",
        };
        try checkCase(allocator, case);
        var result = try lowerCase(allocator, case);
        defer result.sema.deinit();
        const c = try CEmitter.emit(allocator, &result.sema.ir, null);
        defer allocator.free(c);
        // Calls go through function-reference temporaries, including recursion.
        try expectCount(c, "external_recur(", 2);
        try expectCount(c, " = external_recur;", 3);
        try expectCount(c, "nov_recurse", 0);
        try expectCount(c, "external_recur_", 0);
    }
}

test "export namespace accepted and rejected graphs survive allocation failure sweeps" {
    for ([_]bool{ false, true }) |named| {
        for ([_]Case{
            .{ .library = library, .exports = &exports, .named = named, .root = accepted_root },
            .{ .library = recursive_library, .exports = &recursive_exports, .named = named, .root = "let main: () = @println(lib.wrapper(2))" },
            .{ .library = library, .exports = &exports, .named = named, .root = "let main: () = @println(lib.hidden_exact(1))", .rejected = true },
            .{ .library = "@[export(\"invalid_abi\")]\nlet unused: (x: string) -> string = x", .named = named, .rejected = true, .diagnostic = .invalid_export_signature },
        }) |case| {
            try checkCase(std.testing.allocator, case);
            try TestAllocations.checkAllAllocationFailures(std.testing.allocator, checkCase, .{case});
        }
    }
}
