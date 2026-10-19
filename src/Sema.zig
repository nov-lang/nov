const std = @import("std");
const Ast = @import("Ast.zig");
const Module = @import("Module.zig");
const Ir = @import("Ir.zig");
const TypeResolver = @import("TypeResolver.zig");
const Literal = @import("Literal.zig");
const BodyChecker = @import("BodyChecker.zig");
const Sema = @This();

allocator: std.mem.Allocator,
ir: Ir,
entries: []Entry,

pub const Status = enum { deferred, signature_only, constant_checked, body_checked, failed };
pub const Entry = struct {
    type_id: ?Ir.TypeId = null,
    constant: ?Ir.ConstId = null,
    status: Status = .deferred,
};
pub const Error = Ir.InternError || error{InvalidModule};

/// A partial semantic pass: resolves explicit annotations and literal constants.
/// Deferred entries and signatures are not checked bodies or executable IR.
/// Owns all output storage; OOM rolls back diagnostics appended to the module.
pub fn analyzeAnnotations(module: *Module) Error!Sema {
    if (module.hasErrors()) return error.InvalidModule;
    const diagnostics_start = module.diagnostics.items.len;
    errdefer module.diagnostics.shrinkRetainingCapacity(diagnostics_start);
    var result: Sema = .{
        .allocator = module.allocator,
        .ir = Ir.init(module.allocator),
        .entries = try module.allocator.alloc(Entry, module.declarations.items.len),
    };
    errdefer result.deinit();
    @memset(result.entries, .{});
    for (module.declarations.items, 0..) |declaration, i| {
        const full = module.ast.decl(declaration.node);
        if (full.ast.type_node == 0) continue;
        const type_id = (try TypeResolver.resolve(module, &result.ir, full.ast.type_node)) orelse {
            result.entries[i].status = .failed;
            continue;
        };
        result.entries[i].type_id = type_id;
        if (module.ast.fullFnProto(full.ast.type_node) != null) {
            result.entries[i].status = .signature_only;
        } else if (full.ast.init_node != 0) {
            try result.checkLiteral(module, i, full.ast.init_node, module.tokenSpan(module.ast.firstToken(full.ast.type_node)));
        }
    }
    return result;
}

/// Checks the bounded expression-body subset after collecting all annotations.
/// This does not resolve global initializers or produce executable operations.
pub fn analyzeFunctions(module: *Module) Error!Sema {
    const diagnostics_start = module.diagnostics.items.len;
    errdefer module.diagnostics.shrinkRetainingCapacity(diagnostics_start);
    var result = try analyzeAnnotations(module);
    errdefer result.deinit();
    if (module.hasErrors()) return result;
    const types = try result.allocator.alloc(?Ir.TypeId, result.entries.len);
    defer result.allocator.free(types);
    for (result.entries, types) |entry, *ty| ty.* = entry.type_id;
    for (result.entries, 0..) |*entry, i| {
        if (entry.status != .signature_only) continue;
        entry.status = if (try BodyChecker.check(module, &result.ir, types, @enumFromInt(i))) .body_checked else .failed;
    }
    return result;
}

pub fn deinit(self: *Sema) void {
    self.ir.deinit();
    self.allocator.free(self.entries);
    self.* = undefined;
}

/// Entries use the source module's declaration numbering, but borrow no AST data.
pub fn getEntry(self: *const Sema, id: Module.DeclId) ?Entry {
    const index = @intFromEnum(id);
    if (index >= self.entries.len) return null;
    return self.entries[index];
}

fn checkLiteral(self: *Sema, module: *Module, index: usize, initializer: Ast.Node.Index, annotation: Module.Span) Error!void {
    switch (try Literal.check(module, &self.ir, initializer, self.entries[index].type_id.?, annotation)) {
        .not_literal => {},
        .failed => self.entries[index].status = .failed,
        .constant => |constant| {
            self.entries[index].constant = constant;
            self.entries[index].status = .constant_checked;
        },
    }
}

test "explicit annotations check literals and preserve integer boundaries" {
    var module = try Module.init(std.testing.allocator, "literals.nov", "let small: i8 = -128\nlet byte: u8 = 0xff\nlet binary: i32 = 0b10\nlet octal: i32 = 0o10\nlet separated: i32 = 1_000\n" ++
        "let minimum: i128 = -170141183460469231731687303715884105728\nlet maximum: u128 = 340282366920938463463374607431768211455\n" ++
        "let yes: bool = {true}\nlet no: bool = false\nlet grouped: i32 = {{42}}");
    defer module.deinit();
    var sema = try analyzeAnnotations(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    for (sema.entries) |entry| try std.testing.expectEqual(Status.constant_checked, entry.status);
    const small = sema.ir.getConstant(sema.getEntry(module.lookup("small").?).?.constant.?).?;
    try std.testing.expectEqual(@as(i128, -128), small.value.signed_integer);
    try std.testing.expectEqual(@as(u128, 255), sema.ir.getConstant(sema.getEntry(module.lookup("byte").?).?.constant.?).?.value.unsigned_integer);
    try std.testing.expectEqual(@as(i128, 1000), sema.ir.getConstant(sema.getEntry(module.lookup("separated").?).?.constant.?).?.value.signed_integer);
    try std.testing.expectEqual(std.math.minInt(i128), sema.ir.getConstant(sema.getEntry(module.lookup("minimum").?).?.constant.?).?.value.signed_integer);
    try std.testing.expectEqual(std.math.maxInt(u128), sema.ir.getConstant(sema.getEntry(module.lookup("maximum").?).?.constant.?).?.value.unsigned_integer);
    try std.testing.expect(sema.ir.getConstant(sema.getEntry(module.lookup("yes").?).?.constant.?).?.value.boolean);
    try std.testing.expect(sema.getEntry(@enumFromInt(999)) == null);
    try sema.ir.verify();
}

test "range and type errors have literal and annotation locations" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag, literal: []const u8 };
    const cases = [_]Case{
        .{ .source = "let x: i8 = 128", .tag = .literal_out_of_range, .literal = "128" },
        .{ .source = "let x: i8 = -129", .tag = .literal_out_of_range, .literal = "-129" },
        .{ .source = "let x: u8 = 256", .tag = .literal_out_of_range, .literal = "256" },
        .{ .source = "let x: u8 = -1", .tag = .literal_out_of_range, .literal = "-1" },
        .{ .source = "let x: u128 = 0x100000000000000000000000000000000", .tag = .literal_out_of_range, .literal = "0x100000000000000000000000000000000" },
        .{ .source = "let x: i128 = -0x80000000000000000000000000000001", .tag = .literal_out_of_range, .literal = "-0x80000000000000000000000000000001" },
        .{ .source = "let x: i128 = 170141183460469231731687303715884105728", .tag = .literal_out_of_range, .literal = "170141183460469231731687303715884105728" },
        .{ .source = "let x: i128 = -170141183460469231731687303715884105729", .tag = .literal_out_of_range, .literal = "-170141183460469231731687303715884105729" },
        .{ .source = "let x: u128 = 340282366920938463463374607431768211456", .tag = .literal_out_of_range, .literal = "340282366920938463463374607431768211456" },
        .{ .source = "let x: bool = 340282366920938463463374607431768211456", .tag = .literal_type_mismatch, .literal = "340282366920938463463374607431768211456" },
        .{ .source = "let x: void = 340282366920938463463374607431768211456", .tag = .literal_type_mismatch, .literal = "340282366920938463463374607431768211456" },
        .{ .source = "let x: bool = 1", .tag = .literal_type_mismatch, .literal = "1" },
        .{ .source = "let x: i32 = true", .tag = .literal_type_mismatch, .literal = "true" },
        .{ .source = "let x: void = 1", .tag = .literal_type_mismatch, .literal = "1" },
    };
    for (cases) |case| {
        var module = try Module.init(std.testing.allocator, "invalid.nov", case.source);
        defer module.deinit();
        var sema = try analyzeAnnotations(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        const diagnostic = module.diagnostics.items[0];
        try std.testing.expectEqual(case.tag, diagnostic.tag);
        try std.testing.expectEqualStrings(case.literal, module.sourceSlice(diagnostic.span).?);
        try std.testing.expect(diagnostic.related != null);
        try std.testing.expectEqual(Status.failed, sema.entries[0].status);
        try std.testing.expect(sema.entries[0].constant == null);
        try sema.ir.verify();
    }
}

test "large integer spellings canonicalize across bases and separators" {
    var source = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer source.deinit();
    const maximum = std.math.maxInt(u128);
    try source.writer.print("let decimal: u128 = {d}\nlet hex: u128 = 0x{x}\nlet binary: u128 = 0b{b}\nlet octal: u128 = 0o{o}\n", .{ maximum, maximum, maximum, maximum });
    try source.writer.writeAll("let separated: u128 = 0xFFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF\nlet minimum: i128 = -0x8000_0000_0000_0000_0000_0000_0000_0000");
    var module = try Module.init(std.testing.allocator, "bases.nov", source.written());
    defer module.deinit();
    var sema = try analyzeAnnotations(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    for (sema.entries[0..5]) |entry| {
        try std.testing.expectEqual(Status.constant_checked, entry.status);
        try std.testing.expectEqual(sema.entries[0].constant, entry.constant);
    }
    try std.testing.expectEqual(std.math.minInt(i128), sema.ir.getConstant(sema.entries[5].constant.?).?.value.signed_integer);
    try sema.ir.verify();
}

test "signatures are resolved but bodies and unannotated expressions stay deferred" {
    var module = try Module.init(std.testing.allocator, "deferred.nov", "let add: (a: i32, b: i32) -> i32 = missing(a, b)\nlet main: () = {}\nlet pending = 123\nlet expression: i32 = 1 + 2\nlet storage: i32");
    defer module.deinit();
    var sema = try analyzeAnnotations(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    const add = sema.getEntry(module.lookup("add").?).?;
    try std.testing.expectEqual(Status.signature_only, add.status);
    const signature = sema.ir.getType(add.type_id.?).?.function;
    try std.testing.expectEqual(@as(usize, 2), signature.params.len);
    try std.testing.expectEqual(signature.result, signature.params[0]);
    try std.testing.expectEqual(Status.signature_only, sema.getEntry(module.lookup("main").?).?.status);
    const pending = sema.getEntry(module.lookup("pending").?).?;
    try std.testing.expectEqual(Status.deferred, pending.status);
    try std.testing.expect(pending.type_id == null);
    try std.testing.expectEqual(Status.deferred, sema.getEntry(module.lookup("expression").?).?.status);
    try std.testing.expectEqual(Status.deferred, sema.getEntry(module.lookup("storage").?).?.status);
}

test "invalid annotations and unsupported literals never become checked constants" {
    for ([_][]const u8{
        "let x: Unknown = 1",
        "let x: int = 1",
        "let f: (x: i32, x: i32) -> i32 = 1",
        "let x: i32 = 1e",
        "let x: i32 = 1.0",
        "let x: i32 = 'a'",
        "let x: bool = \"true\"",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "unsupported.nov", source);
        defer module.deinit();
        try std.testing.expect(!module.hasErrors());
        var sema = try analyzeAnnotations(&module);
        defer sema.deinit();
        try std.testing.expect(module.hasErrors());
        try std.testing.expectEqual(Status.failed, sema.entries[0].status);
        try std.testing.expect(sema.entries[0].constant == null);
        try sema.ir.verify();
    }
}

test "empty labeled and multistatement blocks stay deferred" {
    var module = try Module.init(std.testing.allocator, "blocks.nov", "let empty: void = {}\nlet labeled: i32 = label: {42}\nlet many: i32 = {1\n2}");
    defer module.deinit();
    var sema = try analyzeAnnotations(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    for (sema.entries) |entry| {
        try std.testing.expectEqual(Status.deferred, entry.status);
        try std.testing.expect(entry.constant == null);
    }
}

test "typed results retain no borrowed module storage" {
    var module = try Module.init(std.testing.allocator, "owned.nov", "let x: i32 = 42\nlet f: (i32) -> bool = unknown");
    var sema = analyzeAnnotations(&module) catch |err| {
        module.deinit();
        return err;
    };
    module.deinit();
    defer sema.deinit();
    try std.testing.expectEqual(@as(i128, 42), sema.ir.getConstant(sema.entries[0].constant.?).?.value.signed_integer);
    try std.testing.expectEqual(@as(usize, 1), sema.ir.getType(sema.entries[1].type_id.?).?.function.params.len);
    try sema.ir.verify();
}

test "annotation analysis refuses modules with syntax or collection errors" {
    for ([_][]const u8{ "let broken =", "let a = 1\nlet a = 2" }) |source| {
        var module = try Module.init(std.testing.allocator, "invalid.nov", source);
        defer module.deinit();
        const count = module.diagnostics.items.len;
        try std.testing.expectError(error.InvalidModule, analyzeAnnotations(&module));
        try std.testing.expectEqual(count, module.diagnostics.items.len);
    }
}

test "literal diagnostics render the actual value and annotation" {
    var module = try Module.init(std.testing.allocator, "range.nov", "let value: i8 = -129");
    defer module.deinit();
    var sema = try analyzeAnnotations(&module);
    defer sema.deinit();
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try @import("Diagnostics.zig").renderModule(&module, &output.writer);
    try std.testing.expectEqualStrings(
        "range.nov:1:17: error: integer literal does not fit the annotated type\nlet value: i8 = -129\n                ^~~~\n" ++
            "range.nov:1:12: note: type annotation here\nlet value: i8 = -129\n           ^~\n",
        output.written(),
    );
}

fn testAllocationFailures(allocator: std.mem.Allocator, source: []const u8) !void {
    var module = try Module.init(allocator, "oom.nov", source);
    defer module.deinit();
    var sema = analyzeAnnotations(&module) catch |err| {
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    try sema.ir.verify();
}

test "annotation analysis owns its storage and rolls back diagnostics on allocation failure" {
    for ([_][]const u8{
        "",
        "let x: i32 = 42\nlet y: bool = false",
        "let f: (a: i32, callback: () -> bool) -> i32 = a",
        "let x: i8 = 128\nlet bad: Unknown = 1",
        "let f: (x: void, x: i32) -> bool = true",
    }) |source| {
        try std.testing.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}
