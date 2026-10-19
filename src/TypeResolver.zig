const std = @import("std");
const Ast = @import("Ast.zig");
const Module = @import("Module.zig");
const Ir = @import("Ir.zig");
const primitives = @import("primitives.zig");

pub const Error = Ir.InternError;

/// Resolves an explicit, nonzero type node in a syntax-valid AST, without
/// evaluating aliases, values or bodies.
/// Semantic failures append source diagnostics and return null. Child types and
/// diagnostics may remain after OOM; callers needing a transaction must own and
/// discard the output IR and roll back diagnostics, as Sema does.
pub fn resolve(module: *Module, ir: *Ir, node: Ast.Node.Index) Error!?Ir.TypeId {
    return resolveDepth(module, ir, node, 0);
}

// Count all explicit type nodes on a path, including the root and leaf.
const max_type_depth = 128;

fn diagnose(module: *Module, node: Ast.Node.Index, tag: Module.Diagnostic.Tag) Error!?Ir.TypeId {
    try module.diagnostics.append(module.allocator, .{
        .tag = tag,
        .span = module.tokenSpan(module.ast.firstToken(node)),
    });
    return null;
}

fn resolveDepth(module: *Module, ir: *Ir, node: Ast.Node.Index, depth: usize) Error!?Ir.TypeId {
    if (depth >= max_type_depth) return diagnose(module, node, .type_nesting_limit);
    const ast = &module.ast;
    if (ast.fullFnProto(node)) |proto| {
        var params: std.ArrayList(Ir.TypeId) = .empty;
        defer params.deinit(module.allocator);
        var names: std.StringHashMapUnmanaged(Module.Span) = .empty;
        defer names.deinit(module.allocator);
        var valid = true;
        var iterator = proto.iterate(ast);
        while (iterator.next()) |param| {
            if (param.name_token) |token| {
                const span = module.tokenSpan(token);
                const name = ast.tokenSlice(token);
                if (std.mem.startsWith(u8, name, "@\"")) {
                    try module.diagnostics.append(module.allocator, .{ .tag = .unsupported_name, .span = span });
                    valid = false;
                } else {
                    const entry = try names.getOrPut(module.allocator, name);
                    if (entry.found_existing) {
                        try module.diagnostics.append(module.allocator, .{
                            .tag = .duplicate_parameter,
                            .span = span,
                            .related = entry.value_ptr.*,
                        });
                        valid = false;
                    } else {
                        entry.value_ptr.* = span;
                    }
                }
            }
            if (try resolveDepth(module, ir, param.type_expr, depth + 1)) |ty| {
                if (ir.getType(ty).? == .void) {
                    _ = try diagnose(module, param.type_expr, .invalid_parameter_type);
                    valid = false;
                } else {
                    try params.append(module.allocator, ty);
                }
            } else {
                valid = false;
            }
        }
        const result = if (proto.ast.return_type) |return_node|
            try resolveDepth(module, ir, return_node, depth + 1)
        else
            try ir.internType(.void);
        if (!valid or result == null) return null;
        return try ir.internType(.{ .function = .{ .params = params.items, .result = result.? } });
    }
    if (ast.nodes.items(.tag)[node] != .identifier) return diagnose(module, node, .unsupported_type);
    const name = ast.tokenSlice(ast.firstToken(node));
    // Until aliasing/shadowing rules exist, a module binding is never a type.
    if (module.lookup(name) != null) return diagnose(module, node, .unsupported_type);
    if (std.mem.eql(u8, name, "true") or std.mem.eql(u8, name, "false")) return diagnose(module, node, .not_a_type);
    if (std.mem.eql(u8, name, "bool")) return try ir.internType(.boolean);
    if (std.mem.eql(u8, name, "void")) return try ir.internType(.void);
    inline for (.{ "i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128" }) |integer| {
        if (std.mem.eql(u8, name, integer)) return try ir.internType(.{ .integer = .{
            .signedness = if (integer[0] == 'i') .signed else .unsigned,
            .bits = comptime std.fmt.parseInt(u16, integer[1..], 10) catch unreachable,
        } });
    }
    return diagnose(module, node, if (primitives.isPrimitive(name)) .unsupported_type else .unknown_type);
}

fn testTypeNode(module: *const Module, name: []const u8) Ast.Node.Index {
    return module.ast.decl(module.getDeclaration(module.lookup(name).?).?.node).ast.type_node;
}

fn testResolve(allocator: std.mem.Allocator, source: []const u8) !void {
    var module = try Module.init(allocator, "types.nov", source);
    defer module.deinit();
    var ir = Ir.init(allocator);
    defer ir.deinit();
    try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
    for (module.declarations.items) |declaration| {
        const node = module.ast.decl(declaration.node).ast.type_node;
        if (node != 0) _ = try resolve(&module, &ir, node);
    }
    try ir.verify();
}

test "explicit primitive types have canonical identities and widths" {
    var module = try Module.init(std.testing.allocator, "primitives.nov",
        \\let a: bool
        \\let b: void
        \\let c: i8
        \\let d: i16
        \\let e: i32
        \\let f: i64
        \\let g: i128
        \\let h: u8
        \\let i: u16
        \\let j: u32
        \\let k: u64
        \\let l: u128
    );
    defer module.deinit();
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    try std.testing.expect(!module.hasErrors());
    const expected = [_]Ir.Type{
        .boolean,                                                 .void,
        .{ .integer = .{ .signedness = .signed, .bits = 8 } },    .{ .integer = .{ .signedness = .signed, .bits = 16 } },
        .{ .integer = .{ .signedness = .signed, .bits = 32 } },   .{ .integer = .{ .signedness = .signed, .bits = 64 } },
        .{ .integer = .{ .signedness = .signed, .bits = 128 } },  .{ .integer = .{ .signedness = .unsigned, .bits = 8 } },
        .{ .integer = .{ .signedness = .unsigned, .bits = 16 } }, .{ .integer = .{ .signedness = .unsigned, .bits = 32 } },
        .{ .integer = .{ .signedness = .unsigned, .bits = 64 } }, .{ .integer = .{ .signedness = .unsigned, .bits = 128 } },
    };
    for (module.declarations.items, expected) |declaration, ty| {
        const node = module.ast.decl(declaration.node).ast.type_node;
        const id = (try resolve(&module, &ir, node)).?;
        try std.testing.expectEqual(try ir.internType(ty), id);
        try std.testing.expectEqual(id, (try resolve(&module, &ir, node)).?);
    }
    try std.testing.expect(!module.hasErrors());
    try ir.verify();
}

test "function signatures canonicalize names omission and nested types without bodies" {
    var module = try Module.init(std.testing.allocator, "functions.nov",
        \\let zero: () = unknown_body()
        \\let zero_explicit: () -> void
        \\let one: (i32) -> bool
        \\let named: (value: i32) -> bool
        \\let many: (i32, flag: bool, u64) -> i128
        \\let nested: (callback: (x: i32) -> bool) -> ()
        \\let nested_again: ((i32) -> bool) -> () -> void
    );
    defer module.deinit();
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    try std.testing.expect(!module.hasErrors());
    var ids: [7]Ir.TypeId = undefined;
    for (module.declarations.items, &ids) |declaration, *id| {
        id.* = (try resolve(&module, &ir, module.ast.decl(declaration.node).ast.type_node)).?;
    }
    try std.testing.expectEqual(ids[0], ids[1]);
    try std.testing.expectEqual(ids[2], ids[3]);
    try std.testing.expectEqual(ids[5], ids[6]);
    const zero = ir.getType(ids[0]).?.function;
    try std.testing.expectEqual(@as(usize, 0), zero.params.len);
    try std.testing.expect(ir.getType(zero.result).? == .void);
    const one = ir.getType(ids[2]).?.function;
    try std.testing.expectEqual(@as(usize, 1), one.params.len);
    try std.testing.expectEqual(try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 32 } }), one.params[0]);
    try std.testing.expect(ir.getType(one.result).? == .boolean);
    const many = ir.getType(ids[4]).?.function;
    try std.testing.expectEqualSlices(Ir.TypeId, &.{
        one.params[0],
        try ir.internType(.boolean),
        try ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = 64 } }),
    }, many.params);
    try std.testing.expectEqual(try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 128 } }), many.result);
    const nested = ir.getType(ids[5]).?.function;
    try std.testing.expectEqualSlices(Ir.TypeId, &.{ids[2]}, nested.params);
    try std.testing.expectEqual(ids[0], nested.result);
    try std.testing.expect(!module.hasErrors());
    // Resolution has already freed its temporary parameter lists.
    try std.testing.expectEqualSlices(Ir.TypeId, &.{ids[2]}, ir.getType(ids[5]).?.function.params);
    try ir.verify();
}

test "unsupported unknown and non-type forms have source diagnostics" {
    const Case = struct { spelling: []const u8, tag: Module.Diagnostic.Tag, span: []const u8 };
    const cases = [_]Case{
        .{ .spelling = "Missing", .tag = .unknown_type, .span = "Missing" },
        .{ .spelling = "alias", .tag = .unsupported_type, .span = "alias" },
        .{ .spelling = "true", .tag = .not_a_type, .span = "true" },
        .{ .spelling = "false", .tag = .not_a_type, .span = "false" },
        .{ .spelling = "*i32", .tag = .unsupported_type, .span = "*" },
        .{ .spelling = "42", .tag = .unsupported_type, .span = "42" },
        .{ .spelling = "i7", .tag = .unknown_type, .span = "i7" },
    };
    for (cases) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let alias = i32\nlet value: {s}", .{case.spelling});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "bad.nov", source);
        defer module.deinit();
        var ir = Ir.init(std.testing.allocator);
        defer ir.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expectEqual(@as(?Ir.TypeId, null), try resolve(&module, &ir, testTypeNode(&module, "value")));
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        const diagnostic = module.diagnostics.items[0];
        try std.testing.expectEqual(case.tag, diagnostic.tag);
        try std.testing.expectEqualStrings(case.span, module.sourceSlice(diagnostic.span).?);
        try std.testing.expect(diagnostic.related == null);
    }
    for ([_][]const u8{ "int", "uint", "float", "f16", "f32", "f64", "f80", "f128", "string", "rune", "type" }) |name| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let value: {s}", .{name});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "unsupported.nov", source);
        defer module.deinit();
        var ir = Ir.init(std.testing.allocator);
        defer ir.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expectEqual(@as(?Ir.TypeId, null), try resolve(&module, &ir, testTypeNode(&module, "value")));
        try std.testing.expectEqual(Module.Diagnostic.Tag.unsupported_type, module.diagnostics.items[0].tag);
        try std.testing.expectEqualStrings(name, module.sourceSlice(module.diagnostics.items[0].span).?);
        try std.testing.expectEqual(@as(usize, 0), ir.types.items.len);
    }
}

test "malformed signatures collect duplicate void unknown and result failures" {
    var module = try Module.init(std.testing.allocator, "parameters.nov", "let value: (x: Missing, x: void, x: bool, void, (a: void)) -> Absent");
    defer module.deinit();
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expectEqual(@as(?Ir.TypeId, null), try resolve(&module, &ir, testTypeNode(&module, "value")));
    const expected = [_]Module.Diagnostic.Tag{
        .unknown_type,        .duplicate_parameter,    .invalid_parameter_type,
        .duplicate_parameter, .invalid_parameter_type, .invalid_parameter_type,
        .unknown_type,
    };
    try std.testing.expectEqual(expected.len, module.diagnostics.items.len);
    const first_name = module.tokenSpan(module.ast.fullFnProto(testTypeNode(&module, "value")).?.lparen + 1);
    for (module.diagnostics.items, expected) |diagnostic, tag| {
        try std.testing.expectEqual(tag, diagnostic.tag);
        if (tag == .duplicate_parameter) {
            try std.testing.expectEqual(first_name, diagnostic.related.?);
            try std.testing.expectEqualStrings("x", module.sourceSlice(diagnostic.span).?);
            try std.testing.expect(first_name.start < diagnostic.span.start);
        } else if (tag == .invalid_parameter_type) {
            try std.testing.expectEqualStrings("void", module.sourceSlice(diagnostic.span).?);
        }
    }
    for (ir.types.items) |ty| try std.testing.expect(ty != .function);
    try ir.verify();
}

test "individual parameter and result failures never intern a successful signature" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let value: (@\"x\": i32)", .tag = .unsupported_name },
        .{ .source = "let value: (callback: (@\"x\": i32))", .tag = .unsupported_name },
        .{ .source = "let value: (void)", .tag = .invalid_parameter_type },
        .{ .source = "let value: (x: void)", .tag = .invalid_parameter_type },
        .{ .source = "let value: (Missing)", .tag = .unknown_type },
        .{ .source = "let value: (i32) -> Missing", .tag = .unknown_type },
        .{ .source = "let value: (i32) -> int", .tag = .unsupported_type },
        .{ .source = "let value: (true)", .tag = .not_a_type },
        .{ .source = "let value: ((void))", .tag = .invalid_parameter_type },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "invalid-signature.nov", case.source);
        defer module.deinit();
        var ir = Ir.init(std.testing.allocator);
        defer ir.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expectEqual(@as(?Ir.TypeId, null), try resolve(&module, &ir, testTypeNode(&module, "value")));
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        for (ir.types.items) |ty| try std.testing.expect(ty != .function);
        try ir.verify();
    }
}

test "duplicate names alone invalidate signatures but nested scopes are independent" {
    for ([_][]const u8{ "let value: (x: i32, x: bool)", "let value: (x: (x: i32), y: (x: i32))" }, 0..) |source, i| {
        var module = try Module.init(std.testing.allocator, "names.nov", source);
        defer module.deinit();
        var ir = Ir.init(std.testing.allocator);
        defer ir.deinit();
        try std.testing.expect(!module.hasErrors());
        const result = try resolve(&module, &ir, testTypeNode(&module, "value"));
        try std.testing.expectEqual(i == 1, result != null);
        try std.testing.expectEqual(@as(usize, if (i == 0) 1 else 0), module.diagnostics.items.len);
        try ir.verify();
    }
}

test "recursive type depth is bounded for parameter and result paths" {
    for ([_]bool{ false, true }) |parameter_path| {
        for ([_]usize{ 127, 128, 256 }) |count| {
            var source = std.Io.Writer.Allocating.init(std.testing.allocator);
            defer source.deinit();
            try source.writer.writeAll("let value: ");
            for (0..count) |_| try source.writer.writeAll(if (parameter_path) "(" else "() -> ");
            try source.writer.writeAll("bool");
            if (parameter_path) for (0..count) |_| try source.writer.writeAll(")");
            var module = try Module.init(std.testing.allocator, "depth.nov", source.written());
            defer module.deinit();
            var ir = Ir.init(std.testing.allocator);
            defer ir.deinit();
            try std.testing.expect(!module.hasErrors());
            const result = try resolve(&module, &ir, testTypeNode(&module, "value"));
            try std.testing.expectEqual(count == 127, result != null);
            if (count >= 128) {
                try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
                const diagnostic = module.diagnostics.items[0];
                try std.testing.expectEqual(Module.Diagnostic.Tag.type_nesting_limit, diagnostic.tag);
                const expected_start = "let value: ".len + 128 * @as(usize, if (parameter_path) 1 else "() -> ".len);
                try std.testing.expectEqual(expected_start, diagnostic.span.start);
            }
            try ir.verify();
        }
    }
}

test "module IR temporary signatures and diagnostics clean up on every allocation failure" {
    for ([_][]const u8{
        "let value: ()",
        "let value: (x: i32, bool, u128) -> i64\nlet again: (i32, bool, u128) -> i64",
        "let value: (callback: (x: i32) -> bool) -> (u8) -> void",
        "let value: (x: Missing, x: void, x: bool, void) -> Absent",
        "let value: (x: i32, x: bool)",
        "let value: int",
    }) |source| {
        try std.testing.checkAllAllocationFailures(std.testing.allocator, testResolve, .{source});
    }
}
