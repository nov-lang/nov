const std = @import("std");
const Ast = @import("Ast.zig");
const Module = @import("Module.zig");
const Ir = @import("Ir.zig");

pub const Result = union(enum) { not_literal, failed, constant: Ir.ConstId };
pub const Error = Ir.InternError;

/// Borrows a valid AST and caller-owned IR; performs no body checking or numeric
/// defaulting. Invalid literals append source diagnostics and return failed.
/// On allocation failure, interned constants and diagnostics may remain: the
/// caller owns rollback as well as the lifetime of both module and IR storage.
pub fn check(module: *Module, ir: *Ir, node: Ast.Node.Index, expected: Ir.TypeId, annotation: ?Module.Span) Error!Result {
    const ast = &module.ast;
    var current = node;
    while (true) {
        const span = module.tokenSpan(ast.nodes.items(.main_token)[current]);
        switch (ast.nodes.items(.tag)[current]) {
            .block, .block_two, .block_newline, .block_two_newline => {
                var buffer: [2]Ast.Node.Index = undefined;
                const block = ast.fullBlock(&buffer, current).?;
                // Only unlabeled, single-expression braces are grouping.
                if (ast.firstToken(current) != block.ast.lbrace or block.ast.statements.len != 1) return .not_literal;
                if (ast.nodes.items(.tag)[block.ast.statements[0]] == .decl) return .not_literal;
                current = block.ast.statements[0];
            },
            .number_literal => return checkNumber(module, ir, current, expected, false, span, annotation),
            .negation => {
                const original_operand = ast.nodes.items(.data)[current].rhs;
                var operand = original_operand;

                while (true) {
                    var buffer: [2]Ast.Node.Index = undefined;
                    const block = ast.fullBlock(&buffer, operand) orelse break;
                    if (ast.firstToken(operand) != block.ast.lbrace or block.ast.statements.len != 1) return .not_literal;
                    if (ast.nodes.items(.tag)[block.ast.statements[0]] == .decl) return .not_literal;
                    operand = block.ast.statements[0];
                }
                if (ast.nodes.items(.tag)[operand] != .number_literal) return .not_literal;
                const end = module.tokenSpan(ast.lastToken(original_operand)).end;
                return checkNumber(module, ir, operand, expected, true, .{ .start = span.start, .end = end }, annotation);
            },
            .identifier => {
                const name = module.sourceSlice(span).?;
                // Collected bindings take precedence over builtin booleans.
                if (module.lookup(name) != null) return .not_literal;
                if (std.mem.eql(u8, name, "true") or std.mem.eql(u8, name, "false")) {
                    return internLiteral(module, ir, expected, .{ .boolean = std.mem.eql(u8, name, "true") }, span, annotation);
                }
                return .not_literal;
            },
            .char_literal, .string_literal => return failLiteral(module, .unsupported_literal, span, annotation),
            else => return .not_literal,
        }
    }
}

fn failLiteral(module: *Module, tag: Module.Diagnostic.Tag, span: Module.Span, annotation: ?Module.Span) std.mem.Allocator.Error!Result {
    try module.diagnostics.append(module.allocator, .{
        .tag = tag,
        .span = span,
        .related = if (tag == .literal_out_of_range or tag == .literal_type_mismatch) annotation else null,
    });
    return .failed;
}

fn internLiteral(module: *Module, ir: *Ir, expected: Ir.TypeId, value: Ir.Constant.Value, span: Module.Span, annotation: ?Module.Span) Error!Result {
    const constant = ir.internConstant(.{ .ty = expected, .value = value }) catch |err| switch (err) {
        error.IntegerOutOfRange => return failLiteral(module, .literal_out_of_range, span, annotation),
        error.ConstantTypeMismatch, error.NonConstantType => return failLiteral(module, .literal_type_mismatch, span, annotation),
        else => return err,
    };
    return .{ .constant = constant };
}

fn checkNumber(module: *Module, ir: *Ir, node: Ast.Node.Index, expected: Ir.TypeId, negative: bool, span: Module.Span, annotation: ?Module.Span) Error!Result {
    const bytes = module.ast.tokenSlice(module.ast.nodes.items(.main_token)[node]);
    switch (std.zig.parseNumberLiteral(bytes)) {
        .float => return failLiteral(module, .unsupported_literal, span, annotation),
        .failure => return failLiteral(module, .invalid_literal, span, annotation),
        else => {},
    }
    const ty = ir.getType(expected) orelse return error.InvalidTypeId;
    if (ty != .integer) return failLiteral(module, .literal_type_mismatch, span, annotation);
    const magnitude = std.fmt.parseInt(u128, bytes, 0) catch |err| switch (err) {
        error.Overflow => return failLiteral(module, .literal_out_of_range, span, annotation),
        error.InvalidCharacter => return failLiteral(module, .invalid_literal, span, annotation),
    };
    switch (ty.integer.signedness) {
        .signed => {
            const minimum_magnitude = @as(u128, 1) << 127;
            if (magnitude > (if (negative) minimum_magnitude else minimum_magnitude - 1)) {
                return failLiteral(module, .literal_out_of_range, span, annotation);
            }
            const value: i128 = if (negative)
                (if (magnitude == minimum_magnitude) std.math.minInt(i128) else -@as(i128, @intCast(magnitude)))
            else
                @intCast(magnitude);
            return internLiteral(module, ir, expected, .{ .signed_integer = value }, span, annotation);
        },
        .unsigned => {
            if (negative and magnitude != 0) return failLiteral(module, .literal_out_of_range, span, annotation);
            return internLiteral(module, ir, expected, .{ .unsigned_integer = magnitude }, span, annotation);
        },
    }
}

fn testNode(module: *const Module, index: usize) Ast.Node.Index {
    return module.ast.decl(module.declarations.items[index].node).ast.init_node;
}

const test_i8: Ir.Type = .{ .integer = .{ .signedness = .signed, .bits = 8 } };
const test_u8: Ir.Type = .{ .integer = .{ .signedness = .unsigned, .bits = 8 } };
const test_i128: Ir.Type = .{ .integer = .{ .signedness = .signed, .bits = 128 } };
const test_u128: Ir.Type = .{ .integer = .{ .signedness = .unsigned, .bits = 128 } };

test "integer boundaries bases separators and grouping preserve typed constants" {
    const Case = struct { expression: []const u8, ty: Ir.Type, value: Ir.Constant.Value };
    for ([_]Case{
        .{ .expression = "-128", .ty = test_i8, .value = .{ .signed_integer = -128 } },
        .{ .expression = "-{{128}}", .ty = test_i8, .value = .{ .signed_integer = -128 } },
        .{ .expression = "127", .ty = test_i8, .value = .{ .signed_integer = 127 } },
        .{ .expression = "0xff", .ty = test_u8, .value = .{ .unsigned_integer = 255 } },
        .{ .expression = "0b10", .ty = test_i8, .value = .{ .signed_integer = 2 } },
        .{ .expression = "0o10", .ty = test_i8, .value = .{ .signed_integer = 8 } },
        .{ .expression = "1_000", .ty = test_i128, .value = .{ .signed_integer = 1000 } },
        .{ .expression = "-170141183460469231731687303715884105728", .ty = test_i128, .value = .{ .signed_integer = std.math.minInt(i128) } },
        .{ .expression = "170141183460469231731687303715884105727", .ty = test_i128, .value = .{ .signed_integer = std.math.maxInt(i128) } },
        .{ .expression = "340282366920938463463374607431768211455", .ty = test_u128, .value = .{ .unsigned_integer = std.math.maxInt(u128) } },
        .{ .expression = "-0", .ty = test_u8, .value = .{ .unsigned_integer = 0 } },
        .{ .expression = "{{42}}", .ty = test_i8, .value = .{ .signed_integer = 42 } },
        .{ .expression = "{\n42\n}", .ty = test_i8, .value = .{ .signed_integer = 42 } },
        .{ .expression = "{true}", .ty = .boolean, .value = .{ .boolean = true } },
        .{ .expression = "false", .ty = .boolean, .value = .{ .boolean = false } },
    }) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let x = {s}", .{case.expression});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "literal.nov", source);
        defer module.deinit();
        try std.testing.expect(!module.hasErrors());
        var ir = Ir.init(std.testing.allocator);
        defer ir.deinit();
        const expected = try ir.internType(case.ty);
        const result = try check(&module, &ir, testNode(&module, 0), expected, null);
        try std.testing.expect(result == .constant);
        try std.testing.expectEqualDeep(Ir.Constant{ .ty = expected, .value = case.value }, ir.getConstant(result.constant).?);
        try std.testing.expectEqual(result.constant, (try check(&module, &ir, testNode(&module, 0), expected, null)).constant);
        try std.testing.expect(!module.hasErrors());
        try ir.verify();
    }
}

test "invalid literals fail with source spans and only optional type notes" {
    const Case = struct { expression: []const u8, ty: Ir.Type = test_i8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .expression = "128", .tag = .literal_out_of_range },
        .{ .expression = "-129", .tag = .literal_out_of_range },
        .{ .expression = "256", .ty = test_u8, .tag = .literal_out_of_range },
        .{ .expression = "-1", .ty = test_u8, .tag = .literal_out_of_range },
        .{ .expression = "170141183460469231731687303715884105728", .ty = test_i128, .tag = .literal_out_of_range },
        .{ .expression = "-170141183460469231731687303715884105729", .ty = test_i128, .tag = .literal_out_of_range },
        .{ .expression = "340282366920938463463374607431768211456", .ty = test_u128, .tag = .literal_out_of_range },
        .{ .expression = "0x100000000000000000000000000000000", .ty = test_u128, .tag = .literal_out_of_range },
        .{ .expression = "340282366920938463463374607431768211456", .ty = .boolean, .tag = .literal_type_mismatch },
        .{ .expression = "-340282366920938463463374607431768211456", .ty = .boolean, .tag = .literal_type_mismatch },
        .{ .expression = "340282366920938463463374607431768211456", .ty = .void, .tag = .literal_type_mismatch },
        .{ .expression = "true", .tag = .literal_type_mismatch },
        .{ .expression = "false", .ty = .void, .tag = .literal_type_mismatch },
        .{ .expression = "1e", .ty = .boolean, .tag = .invalid_literal },
        .{ .expression = "0b102", .tag = .invalid_literal },
        .{ .expression = "1.0", .tag = .unsupported_literal },
        .{ .expression = "-1.0", .tag = .unsupported_literal },
        .{ .expression = "'a'", .tag = .unsupported_literal },
        .{ .expression = "\"true\"", .tag = .unsupported_literal },
    }) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let x: Expected = {s}", .{case.expression});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "invalid.nov", source);
        defer module.deinit();
        try std.testing.expect(!module.hasErrors());
        var ir = Ir.init(std.testing.allocator);
        defer ir.deinit();
        const expected = try ir.internType(case.ty);
        const full = module.ast.decl(module.declarations.items[0].node);
        const annotation = module.tokenSpan(module.ast.firstToken(full.ast.type_node));
        for ([_]?Module.Span{ annotation, null }, 0..) |note, i| {
            try std.testing.expectEqual(Result.failed, try check(&module, &ir, full.ast.init_node, expected, note));
            try std.testing.expectEqual(i + 1, module.diagnostics.items.len);
            const diagnostic = module.diagnostics.items[i];
            try std.testing.expectEqual(case.tag, diagnostic.tag);
            try std.testing.expectEqualStrings(case.expression, module.sourceSlice(diagnostic.span).?);
            const has_type_note = case.tag == .literal_type_mismatch or case.tag == .literal_out_of_range;
            try std.testing.expectEqualDeep(if (has_type_note) note else null, diagnostic.related);
        }
        try std.testing.expectEqual(@as(usize, 0), ir.constants.items.len);
        try ir.verify();
    }
}

test "nonliteral blocks expressions and shadowed booleans stay untouched" {
    for ([_][]const u8{
        "let x = {}",
        "let x = label: {42}",
        "let x = {1\n2}",
        "let x = {\nlet local = 1\n}",
        "let x = 1 + 2",
        "let x = missing",
        "let x = call()",
        "let x = -missing",
        "let x = -{missing}",
        "let x = true\nlet true = 1",
        "let x = false\nlet false = 1",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "nonliteral.nov", source);
        defer module.deinit();

        try std.testing.expect(!module.hasErrors());
        var ir = Ir.init(std.testing.allocator);
        defer ir.deinit();
        const expected = try ir.internType(test_i8);
        try std.testing.expectEqual(Result.not_literal, try check(&module, &ir, testNode(&module, 0), expected, null));
        try std.testing.expect(!module.hasErrors());
        try std.testing.expectEqual(@as(usize, 0), ir.constants.items.len);
    }
}

test "large spellings canonicalize across bases and constants outlive the module" {
    var source = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer source.deinit();
    const maximum = std.math.maxInt(u128);
    try source.writer.print("let decimal = {d}\nlet hex = 0x{x}\nlet binary = 0b{b}\nlet octal = 0o{o}\n", .{ maximum, maximum, maximum, maximum });
    try source.writer.writeAll("let separated = 0xFFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF");
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const expected = try ir.internType(test_u128);
    const constant = blk: {
        var module = try Module.init(std.testing.allocator, "bases.nov", source.written());
        defer module.deinit();
        try std.testing.expect(!module.hasErrors());
        const first = (try check(&module, &ir, testNode(&module, 0), expected, null)).constant;
        for (1..module.declarations.items.len) |i| {
            try std.testing.expectEqual(first, (try check(&module, &ir, testNode(&module, i), expected, null)).constant);
        }
        break :blk first;
    };
    try std.testing.expectEqual(maximum, ir.getConstant(constant).?.value.unsigned_integer);
    try ir.verify();
}

fn testAllocationFailures(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "oom.nov", "let valid = 42\nlet invalid = 128\nlet mismatch = true\nlet unsupported = \"text\"");
    defer module.deinit();
    var ir = Ir.init(allocator);
    defer ir.deinit();
    const expected = try ir.internType(test_i8);
    // The outer owner cleans up both pools even if earlier checks succeeded.
    for (0..module.declarations.items.len) |i| {
        _ = try check(&module, &ir, testNode(&module, i), expected, null);
    }
    try std.testing.expectEqual(@as(usize, 1), ir.constants.items.len);
    try std.testing.expectEqual(@as(usize, 3), module.diagnostics.items.len);
    try ir.verify();
}

test "caller cleanup handles every allocation failure after constants or diagnostics" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{});
}
