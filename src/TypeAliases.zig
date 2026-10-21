const std = @import("std");
const Module = @import("Module.zig");
const Dependencies = @import("Dependencies.zig");
const Sema = @import("Sema.zig");
const Lowering = @import("Lowering.zig");

const Diagnostics = @import("Diagnostics.zig");
const Formatter = @import("Formatter.zig");
const TypeResolver = @import("TypeResolver.zig");

const canonical_program =
    \\let Count = Later
    \\let Later = uint
    \\let Counts = []Count
    \\let Callback = (Count) -> Count
    \\let Higher = (Callback, []Count) -> Callback
    \\let Named = (value: uint) -> uint
    \\let Flag = bool
    \\let Unit = void
    \\let Text = string
    \\let global: Count = 7
    \\let id: (value: Count) -> Count = value
    \\let use: (cb: Callback, values: Counts) -> Count = cb(values[0])
    \\let main: () -> Unit = {
    \\    let f: Callback = @as(Callback, id)
    \\    let values: Counts = @as([]Count, [global])
    \\    @println(use(f, values), "{@as(Count, 8)}", @as(Flag, true), @as(Text, "ok"))
    \\}
;

test "type aliases canonicalize forward chains nested components and named signatures" {
    var module = try Module.init(std.testing.allocator, "aliases.nov", canonical_program);
    defer module.deinit();
    var dependencies = try Dependencies.analyze(&module);
    defer dependencies.deinit();
    try std.testing.expect(!module.hasErrors());
    for (module.declarations.items, 0..) |decl, i| {
        if (decl.is_type_alias) try std.testing.expectEqual(@as(usize, 0), dependencies.getEdges(@enumFromInt(i)).?.len);
    }
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    const count = sema.getEntry(module.lookup("Count").?).?;
    try std.testing.expectEqual(Sema.Status.type_alias, count.status);
    try std.testing.expect(count.type_id == null and count.constant == null and count.function == null);
    try std.testing.expectEqual(count.alias_type, sema.getEntry(module.lookup("Later").?).?.alias_type);
    try std.testing.expectEqual(count.alias_type, sema.getEntry(module.lookup("global").?).?.type_id);
    try std.testing.expectEqual(sema.getEntry(module.lookup("Callback").?).?.alias_type, sema.getEntry(module.lookup("Named").?).?.alias_type);
    try std.testing.expectEqual(@as(usize, 3), sema.ir.functions.items.len);
    try std.testing.expectEqual(module.lookup("main").?, @as(Module.DeclId, @enumFromInt(12)));
    try sema.ir.verify();
}

const Failure = struct { source: []const u8, tag: Module.Diagnostic.Tag, text: ?[]const u8 = null };
const failures = [_]Failure{
    .{ .source = "let Count = uint\nlet main: () = @println(Count)", .tag = .not_a_value, .text = "Count" },
    .{ .source = "let Fn = (int) -> int\nlet main: () = Fn(1)", .tag = .not_a_value },
    .{ .source = "let Count = u8\nlet main: () = @println(@as(Count, 256))", .tag = .literal_out_of_range },
    .{ .source = "let Count = uint\nlet f: (x: int) -> Count = @as(Count, x)", .tag = .expression_type_mismatch },
    .{ .source = "let Value: int = 1\nlet Alias = Value\nlet x: Alias = 2", .tag = .not_a_type, .text = "Value" },
    .{ .source = "let Value = 1\nlet Alias = Value\nlet x: Alias = 2", .tag = .not_a_type },
    .{ .source = "let f: (x: int) -> int = x\nlet Alias = f\nlet x: Alias = 1", .tag = .not_a_type },
    .{ .source = "let A = Missing\nlet x: A = 1", .tag = .unknown_type, .text = "Missing" },
    .{ .source = "let A = []Missing", .tag = .unknown_type },
    .{ .source = "let A = (Missing) -> int", .tag = .unknown_type },
    .{ .source = "let A = []string", .tag = .unsupported_type },
    .{ .source = "let mut Count = uint\nlet x: Count = 1", .tag = .not_a_type },
    .{ .source = "let Count = uint\nlet f: (Count: int) -> int = Count", .tag = .unsupported_shadowing },
    .{ .source = "let Count = uint\nlet main: () = {let Count: int = 1\n}", .tag = .unsupported_shadowing },
    .{ .source = "let main: () = {let Local = int\n@println(@as(Local, 1))}", .tag = .unsupported_body_expression },
    .{ .source = "let uint = int", .tag = .unsupported_shadowing },
};

test "type aliases reject runtime uses value aliases shadowing and invalid types" {
    for (failures) |case| {
        var module = try Module.init(std.testing.allocator, "bad-alias.nov", case.source);
        defer module.deinit();
        if (module.ast.errors.len != 0) std.debug.print("syntax failure in {s}\n", .{case.source});
        try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(module.hasErrors());
        try std.testing.expect(!sema.lowering_complete);
        try std.testing.expectEqual(@as(usize, 0), sema.ir.functions.items.len);
        var found = false;
        for (module.diagnostics.items) |diagnostic| if (diagnostic.tag == case.tag) {
            if (case.text) |text| try std.testing.expectEqualStrings(text, module.sourceSlice(diagnostic.span).?);
            found = true;
        };
        if (!found) std.debug.print("missing {t} in {s}\n", .{ case.tag, case.source });
        try std.testing.expect(found);
    }
}

test "type aliases share the module namespace with functions and values" {
    for ([_][]const u8{
        "let Count = uint\nlet Count: () -> int = 1",
        "let Count: () -> int = 1\nlet Count = uint",
        "let Count = uint\nlet Count: int = 1",
        "let Count = uint\nlet Count = int",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "duplicate-alias.nov", source);
        defer module.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        const diagnostic = module.diagnostics.items[0];
        try std.testing.expectEqual(Module.Diagnostic.Tag.duplicate_name, diagnostic.tag);
        try std.testing.expectEqualStrings("Count", module.sourceSlice(diagnostic.span).?);
        try std.testing.expectEqualStrings("Count", module.sourceSlice(diagnostic.related.?).?);
        try std.testing.expectError(error.InvalidModule, Lowering.lower(&module));
    }
}

test "type alias classification preserves runtime globals functions and literal arrays" {
    var module = try Module.init(std.testing.allocator, "values.nov", "let N = Other\nlet Other = 1\nlet F = f\nlet f: () -> int = 1\nlet Empty = []\nlet Array = [1]\nlet Unknown = Missing\nlet True = true");
    defer module.deinit();
    try std.testing.expect(!module.hasErrors());
    for (module.declarations.items) |decl| try std.testing.expect(!decl.is_type_alias);
    var sema = try Sema.analyzeAnnotations(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expectEqual(Sema.Status.deferred, sema.getEntry(module.lookup("N").?).?.status);
    try std.testing.expectEqual(Sema.Status.signature_only, sema.getEntry(module.lookup("f").?).?.status);
}

test "type alias cycles are distinct from eager value cycles and retain related source notes" {
    for ([_][]const u8{
        "let A = A\nlet x: A = 1",
        "let A = B\nlet B = A\nlet main: () = @println(@as(A, 1))",
        "let A = []B\nlet B = A",
        "let A = (B) -> int\nlet B = (int) -> A",
        "let A = () -> B\nlet B = []A",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "cycle.nov", source);
        defer module.deinit();
        var dependencies = try Dependencies.analyze(&module);
        defer dependencies.deinit();
        try std.testing.expect(!module.hasErrors());
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!sema.lowering_complete);
        try std.testing.expectEqual(@as(usize, 0), sema.ir.functions.items.len);
        for (module.diagnostics.items) |diagnostic| {
            try std.testing.expectEqual(Module.Diagnostic.Tag.type_alias_cycle, diagnostic.tag);
            try std.testing.expect(diagnostic.related != null);
            try std.testing.expectEqualStrings(module.sourceSlice(diagnostic.span).?, module.sourceSlice(diagnostic.related.?).?);
        }
        try std.testing.expect(module.diagnostics.items.len != 0);
        var output = std.Io.Writer.Allocating.init(std.testing.allocator);
        defer output.deinit();
        try Diagnostics.renderModule(&module, &output.writer);
        try std.testing.expect(std.mem.indexOf(u8, output.written(), "note: declaration in this cycle") != null);
    }
}

test "type alias diamonds share canonical identities without false cycles" {
    var module = try Module.init(std.testing.allocator, "diamond.nov", "let A = (B, C) -> D\nlet B = D\nlet C = D\nlet D = int\nlet E = (int, int) -> int");
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try std.testing.expectEqual(sema.entries[0].alias_type, sema.entries[4].alias_type);
}

test "type alias expansion has an iterative explicit bound" {
    for ([_]usize{ TypeResolver.max_alias_depth - 1, TypeResolver.max_alias_depth + 1 }) |count| {
        var source = std.Io.Writer.Allocating.init(std.testing.allocator);
        defer source.deinit();
        for (0..count) |i| try source.writer.print("let A_{d} = A_{d}\n", .{ i, i + 1 });
        try source.writer.print("let A_{d} = int\nlet value: A_0 = 1", .{count});
        var module = try Module.init(std.testing.allocator, "depth.nov", source.written());
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expectEqual(count >= TypeResolver.max_alias_depth, module.hasErrors());
        for (module.diagnostics.items) |diagnostic| try std.testing.expectEqual(Module.Diagnostic.Tag.type_alias_limit, diagnostic.tag);
    }
}

fn allocationTest(allocator: std.mem.Allocator, source: []const u8) !void {
    var module = try Module.init(allocator, "oom-alias.nov", source);
    defer module.deinit();
    var sema = Lowering.lower(&module) catch |err| {
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    try sema.ir.verify();
}

test "type alias resolution and diagnostics roll back on every allocation failure" {
    for ([_][]const u8{
        "let Count = Later\nlet Later = uint\nlet Fn = (Count) -> []Count\nlet main: () = {let a = @as([]Count, [1])\n@println(a[0])}",
        "let A = () -> B\nlet B = []A",
        "let A = []Missing",
        "let A = uint\nlet main: () = @println(A)",
    }) |source| try std.testing.checkAllAllocationFailures(std.testing.allocator, allocationTest, .{source});
}

test "type alias IR survives source destruction and formatter preservation" {
    var sema = blk: {
        var module = try Module.init(std.testing.allocator, "owned-alias.nov", canonical_program);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        break :blk result;
    };
    defer sema.deinit();
    try sema.ir.verify();
    try std.testing.expect(sema.lowering_complete);
    const formatted = try Formatter.format(std.testing.allocator, canonical_program);
    defer std.testing.allocator.free(formatted);
    const terminated = try std.testing.allocator.dupeZ(u8, formatted);
    defer std.testing.allocator.free(terminated);
    const again = try Formatter.format(std.testing.allocator, terminated);
    defer std.testing.allocator.free(again);
    try std.testing.expectEqualStrings(formatted, again);
    var module = try Module.init(std.testing.allocator, "formatted-alias.nov", formatted);
    defer module.deinit();
    var result = try Lowering.lower(&module);
    defer result.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expectEqual(sema.ir.functions.items.len, result.ir.functions.items.len);
}
