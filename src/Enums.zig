//! Regression coverage for bounded nominal, unnumbered enums.
const std = @import("std");
const Module = @import("Module.zig");
const Lowering = @import("Lowering.zig");
const Ir = @import("Ir.zig");
const Formatter = @import("Formatter.zig");
const Diagnostics = @import("Diagnostics.zig");

const program =
    \\let Alias=Kind
    \\let Kind=enum{not_found,permission_denied,io,}
    \\let Other=enum{not_found,permission_denied,io,}
    \\let Result:(T:type,E:type)->type=union{ok:T,err:E,}
    \\let A=Result(uint,Alias)
    \\let B=Result(uint,Kind)
    \\let C=Result(uint,Other)
    \\let global=Alias.not_found
    \\let contextual:Kind=.io
    \\let ErrorRecord=struct{kind:Alias=.not_found,other:Kind=Kind.io,}
    \\let f:(k:Alias)->Kind=match k{Alias.not_found=>.io
    \\    Kind.permission_denied | .io=>global}
    \\let make:()->A=.err[.not_found]
    \\let use:(r:B)->A={let n=r.!
    \\    .ok[n]}
;

test "enum aliases globals generic arguments defaults and source-free names preserve identity" {
    var result = blk: {
        var module = try Module.init(std.testing.allocator, "enum.nov", program);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        errdefer result.deinit();
        if (module.hasErrors()) std.debug.print("enum diagnostics: {any}\n", .{module.diagnostics.items});
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(result.lowering_complete);
        const a = result.getEntry(module.lookup("A").?).?.alias_type.?;
        try std.testing.expectEqual(a, result.getEntry(module.lookup("B").?).?.alias_type.?);
        try std.testing.expect(a != result.getEntry(module.lookup("C").?).?.alias_type.?);
        try std.testing.expectEqual(result.getEntry(module.lookup("Kind").?).?.alias_type, result.getEntry(module.lookup("Alias").?).?.alias_type);
        break :blk result;
    };
    defer result.deinit();
    try result.ir.verify();
    var enums: usize = 0;
    for (result.ir.types.items) |ty| if (ty == .enumeration) {
        enums += 1;
        try std.testing.expectEqualStrings("not_found", ty.enumeration.tags[0].name);
        try std.testing.expect(!Ir.isManaged(ty));
    };
    try std.testing.expectEqual(@as(usize, 2), enums);
    const formatted = try Formatter.format(std.testing.allocator, program);
    defer std.testing.allocator.free(formatted);
    var module = try Module.init(std.testing.allocator, "formatted.nov", formatted);
    defer module.deinit();
    var again = try Lowering.lower(&module);
    defer again.deinit();
    try std.testing.expect(!module.hasErrors());
    try again.ir.verify();
    try std.testing.checkAllAllocationFailures(std.testing.allocator, allocationTest, .{program});
}

test "qualified enum patterns accept aliases mixed alternatives and exhaustive qualified prongs" {
    const source = "let K=enum{x,y,z,}\nlet Alias=K\n" ++
        "let qualified:(k:K)->bool=match k{K.x=>true\nAlias.y=>false\nK.z=>true}\n" ++
        "let mixed:(k:Alias)->bool=match k{Alias.x | .y=>true\nK.z=>false}\n" ++
        "let overlap:(k:K)->bool=match k{K.x=>true\n.x | Alias.y=>false\n.z=>true}";
    var module = try Module.init(std.testing.allocator, "qualified-patterns.nov", source);
    defer module.deinit();
    var result = try Lowering.lower(&module);
    defer result.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(result.lowering_complete);
    try result.ir.verify();
    try std.testing.checkAllAllocationFailures(std.testing.allocator, allocationTest, .{source});
}

test "covered enum prong notes identify the relevant tag and alternative source span" {
    const Case = struct { prongs: []const u8, previous: []const u8 };
    for ([_]Case{
        .{ .prongs = ".x=>true\n.y=>false\n.y=>true\n.z=>false", .previous = ".y" },
        .{ .prongs = ".y=>false\n.y=>true\n.z=>false", .previous = ".y" },
        .{ .prongs = "K.x=>true\nAlias.y=>false\nK.y=>true\nK.z=>false", .previous = "Alias.y" },
        .{ .prongs = "K.y=>false\n.y=>true\nK.z=>false", .previous = "K.y" },
        .{ .prongs = ".x | K.y=>true\n.y=>false\n.z=>false", .previous = "K.y" },
        .{ .prongs = ".y | Alias.z=>true\nK.z | .y=>false\n.x=>true", .previous = "Alias.z" },
        .{ .prongs = "_=>true\nK.y=>false", .previous = "_" },
        .{ .prongs = "K.x=>true\n.y | .z=>false\n_=>true", .previous = "K.x" },
    }) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let K=enum{{x,y,z,}}\nlet Alias=K\nlet f:(k:K)->bool=match k{{{s}}}", .{case.prongs});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "covered-enum.nov", source);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        defer result.deinit();
        try std.testing.expect(!result.lowering_complete);
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        const diagnostic = module.diagnostics.items[0];
        try std.testing.expectEqual(Module.Diagnostic.Tag.match_covered_enum_prong, diagnostic.tag);
        const related = diagnostic.related.?;
        const start = std.mem.indexOf(u8, source, case.previous).?;
        try std.testing.expectEqual(start, related.start);
        try std.testing.expectEqual(start + case.previous.len, related.end);
        try std.testing.expectEqualStrings(case.previous, module.sourceSlice(related).?);
        var output = std.Io.Writer.Allocating.init(std.testing.allocator);
        defer output.deinit();
        try Diagnostics.renderModule(&module, &output.writer);
        try std.testing.expect(std.mem.indexOf(u8, output.written(), "note: earlier pattern covers this variant") != null);
        try std.testing.checkAllAllocationFailures(std.testing.allocator, allocationTest, .{source});
    }
}

fn allocationTest(allocator: std.mem.Allocator, source: []const u8) !void {
    var module = try Module.init(allocator, "enum-oom.nov", source);
    defer module.deinit();
    var result = Lowering.lower(&module) catch |err| {
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer result.deinit();
    try result.ir.verify();
    if (module.hasErrors()) {
        try std.testing.expect(!result.lowering_complete);
        try std.testing.expectEqual(@as(usize, 0), result.ir.functions.items.len);
    }
}

test "enums reject inexact types arithmetic ordering conversions captures and invalid declarations" {
    const prefix = "let Kind=enum{not_found,permission_denied,io,}\nlet Other=enum{not_found,}\n";
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let f:()->Kind=Other.not_found", .tag = .literal_type_mismatch },
        .{ .source = "let f:(k:Kind,o:Other)->bool=k==o", .tag = .expression_type_mismatch },
        .{ .source = "let f:(k:Kind)->bool=k==0", .tag = .literal_type_mismatch },
        .{ .source = "let f:(k:Kind)->bool=k<.io", .tag = .operator_not_supported },
        .{ .source = "let f:(k:Kind)->Kind=k+.io", .tag = .operator_not_supported },
        .{ .source = "let f:(k:Kind)->Kind=~k", .tag = .operator_not_supported },
        .{ .source = "let f:(k:Kind)->uint=@as(uint,k)", .tag = .expression_type_mismatch },
        .{ .source = "let f:()->Kind=@as(Kind,0)", .tag = .literal_type_mismatch },
        .{ .source = "let f:()->Kind=Kind.not_found[]", .tag = .unsupported_type },
        .{ .source = "let f:()->Kind=.unknown", .tag = .unknown_field },
        .{ .source = "let f:()->Kind=Kind.unknown", .tag = .unknown_field },
        .{ .source = "let f:()={let k=.not_found\n}", .tag = .enum_needs_type },
        .{ .source = "let f:(k:Kind)->bool=match k{.not_found=>true}", .tag = .match_missing_variants },
        .{ .source = "let f:(k:Kind)->bool=match k{.not_found=>true\n.not_found=>false\n_=>false}", .tag = .match_covered_enum_prong },
        .{ .source = "let f:(k:Kind)->bool=match k{.not_found | .io=>true\n.io=>false\n_=>false}", .tag = .match_covered_enum_prong },
        .{ .source = "let f:(k:Kind)->bool=match k{_=>true\n.io=>false}", .tag = .match_covered_enum_prong },
        .{ .source = "let f:(k:Kind)->bool=match k{.unknown=>true\n_=>false}", .tag = .unknown_field },
        .{ .source = "let f:(k:Kind)->bool=match k{Kind.unknown=>true\n_=>false}", .tag = .unknown_field },
        .{ .source = "let Alias=Other\nlet f:(k:Kind)->bool=match k{Alias.not_found=>true\n_=>false}", .tag = .literal_type_mismatch },
        .{ .source = "let f:(k:Kind)->bool=match k{.not_found | Other.not_found=>true\n_=>false}", .tag = .literal_type_mismatch },
        .{ .source = "let f:(k:Kind)->bool=match k{.not_found=>true\n.permission_denied | Other.not_found=>false}", .tag = .literal_type_mismatch },
        .{ .source = "let f:(k:Kind)->bool=match k{k.not_found=>true\n_=>false}", .tag = .unsupported_match_pattern },
        .{ .source = "let R=struct{x:Kind,}\nlet f:(k:Kind,r:R)->bool=match k{r.x=>true\n_=>false}", .tag = .unsupported_match_pattern },
        .{ .source = "let K=enum{x,\nlet method:()->Kind=.io\n}\nlet f:(k:Kind)->bool=match k{K.method=>true\n_=>false}", .tag = .unsupported_match_pattern },
        .{ .source = "let f:(k:Kind)->bool=match k{.not_found=>|x|true\n_=>false}", .tag = .invalid_match_capture },
        .{ .source = "let f:(k:Kind)->bool=match k{.not_found=>true\n_=>1}", .tag = .literal_type_mismatch },
        .{ .source = "let f:(Kind:Kind)={}", .tag = .unsupported_shadowing },
        .{ .source = "let f:()={let Kind:Kind=.io\n}", .tag = .unsupported_shadowing },
        .{ .source = "let Bad=enum{x,x,}", .tag = .duplicate_field },
        .{ .source = "let Bad=enum{x=5,y,}", .tag = .unsupported_enum_value },
        .{ .source = "let Bad=enum{x:uint,}", .tag = .unsupported_enum_value },
        .{ .source = "let Bad=enum{}", .tag = .empty_enum },
        .{ .source = "let Bad=enum{x,\nlet x:()={}\n}", .tag = .duplicate_field },
        .{ .source = "let Bad=enum{x,\nlet constant:Kind=.io\n}", .tag = .unsupported_declaration },
        .{ .source = "let mut Bad=enum{x,}", .tag = .unsupported_type },
        .{ .source = "let uint=enum{x,}", .tag = .unsupported_shadowing },
        .{ .source = "let Bad:(T:type)->type=enum{x,}", .tag = .unsupported_type },
        .{ .source = "let f:(k:Kind)=@println(k)", .tag = .invalid_to_string },
        .{ .source = "let f:(k:Kind)->string=\"{k}\"", .tag = .invalid_to_string },
        .{ .source = "let Bad=struct{k:Kind=0,}", .tag = .literal_type_mismatch },
        .{ .source = "let Bad=struct{k:Kind=.unknown,}", .tag = .unknown_field },
        .{ .source = "let Bad=struct{k:Kind=Other.not_found,}", .tag = .literal_type_mismatch },
        .{ .source = "let Bad=enum{x,\nlet toString:(x:Bad)->bool=true\n}\nlet f:(b:Bad)=@println(b)", .tag = .invalid_to_string },
        .{ .source = "let G:(T:type,E:type)->type=union{ok:T,err:E,}\nlet f:(r:G(uint,Kind))->G(uint,Other)=.ok[r.!]", .tag = .propagation_error_mismatch },
    }) |case| {
        const source = try std.mem.concat(std.testing.allocator, u8, &.{ prefix, case.source });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "bad-enum.nov", source);
        defer module.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
        var result = try Lowering.lower(&module);
        defer result.deinit();
        var found = false;
        for (module.diagnostics.items) |d| if (d.tag == case.tag) {
            found = true;
        };
        if (!found) std.debug.print("missing {t} for {s}: {any}\n", .{ case.tag, case.source, module.diagnostics.items });
        try std.testing.expect(found);
        try std.testing.expect(!result.lowering_complete);
        try std.testing.expectEqual(@as(usize, 0), result.ir.functions.items.len);
        try std.testing.checkAllAllocationFailures(std.testing.allocator, allocationTest, .{source});
    }
}

fn irAllocationTest(allocator: std.mem.Allocator) !void {
    var ir = Ir.init(allocator);
    defer ir.deinit();
    ir.source_len = 1;
    const tags = [_]Ir.EnumTag{.{ .name = "not_found", .span = .{ .start = 0, .end = 1 } }};
    const ty = try ir.internType(.{ .enumeration = .{ .identity = 7, .name = "Kind", .tags = &tags } });
    const c = try ir.internConstant(.{ .ty = ty, .value = .{ .enum_tag = 0 } });
    const signature = try ir.internType(.{ .function = .{ .params = &.{ty}, .result = ty } });
    const f = try ir.addFunction("identity", signature, .{ .start = 0, .end = 1 });
    try ir.setFunctionBody(f, &.{.{ .ty = ty, .span = .{ .start = 0, .end = 1 }, .op = .{ .constant = c } }}, @enumFromInt(0));
    try ir.verify();
}

test "enum IR validates owned nominal metadata constants operations and allocation rollback" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, irAllocationTest, .{});
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    ir.source_len = 1;
    var name = [_]u8{'K'};
    var tag_name = [_]u8{'x'};
    var tags = [_]Ir.EnumTag{.{ .name = &tag_name, .span = .{ .start = 0, .end = 1 } }};
    const ty = try ir.internType(.{ .enumeration = .{ .identity = 0, .name = &name, .tags = &tags } });
    name[0] = 'Z';
    tag_name[0] = 'z';
    try std.testing.expectEqualStrings("K", ir.getType(ty).?.enumeration.name);
    try std.testing.expectEqualStrings("x", ir.getType(ty).?.enumeration.tags[0].name);
    tags[0].name = "x";
    const enum_type: Ir.Type = .{ .enumeration = .{ .identity = 0, .name = "K", .tags = &tags } };
    try std.testing.expectEqual(ty, try ir.internType(enum_type));
    const other = try ir.internType(.{ .enumeration = .{ .identity = 1, .name = "Other", .tags = &tags } });
    try std.testing.expect(ty != other);
    const count = ir.types.items.len;
    try std.testing.expectError(error.InvalidTypeId, ir.internType(.{ .enumeration = .{ .identity = 2, .name = "Empty", .tags = &.{} } }));
    try std.testing.expectError(error.InvalidTypeId, ir.internType(.{ .enumeration = .{ .identity = 2, .name = "Dup", .tags = &.{ tags[0], tags[0] } } }));
    tags[0].span.end = 2;
    try std.testing.expectError(error.InvalidTypeId, ir.internType(.{ .enumeration = .{ .identity = 2, .name = "Span", .tags = &tags } }));
    tags[0].span.end = 1;
    try std.testing.expectError(error.InvalidTypeId, ir.internType(.{ .enumeration = .{ .identity = 0, .name = "Forged", .tags = &tags } }));
    try std.testing.expectEqual(count, ir.types.items.len);
    for ([_]Ir.Constant.Value{ .{ .unsigned_integer = 0 }, .{ .signed_integer = 0 }, .{ .boolean = false }, .{ .enum_tag = 1 } }) |value|
        try std.testing.expectError(error.ConstantTypeMismatch, ir.internConstant(.{ .ty = ty, .value = value }));
    const c = try ir.internConstant(.{ .ty = ty, .value = .{ .enum_tag = 0 } });
    const integer = try ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = 32 } });
    try std.testing.expectError(error.ConstantTypeMismatch, ir.internConstant(.{ .ty = integer, .value = .{ .enum_tag = 0 } }));
    const boolean = try ir.internType(.boolean);
    const signature = try ir.internType(.{ .function = .{ .params = &.{ ty, other }, .result = boolean } });
    const f = try ir.addFunction("compare", signature, .{ .start = 0, .end = 1 });
    var body = [_]Ir.Instruction{
        .{ .ty = ty, .span = .{ .start = 0, .end = 1 }, .op = .{ .parameter = 0 } },
        .{ .ty = ty, .span = .{ .start = 0, .end = 1 }, .op = .{ .constant = c } },
        .{ .ty = boolean, .span = .{ .start = 0, .end = 1 }, .op = .{ .binary = .{ .op = .lt, .lhs = @enumFromInt(0), .rhs = @enumFromInt(1) } } },
    };
    try std.testing.expectError(error.InvalidOperation, ir.setFunctionBody(f, &body, @enumFromInt(2)));
    body[2].op.binary.op = .eq;
    body[1] = .{ .ty = other, .span = .{ .start = 0, .end = 1 }, .op = .{ .parameter = 1 } };
    try std.testing.expectError(error.OperandTypeMismatch, ir.setFunctionBody(f, &body, @enumFromInt(2)));
    body[1] = .{ .ty = ty, .span = .{ .start = 0, .end = 1 }, .op = .{ .constant = c } };
    try ir.setFunctionBody(f, &body, @enumFromInt(2));
    try ir.verify();
    ir.constants.items[@intFromEnum(c)].value.enum_tag = 1;
    try std.testing.expectError(error.ConstantTypeMismatch, ir.verify());
    ir.constants.items[@intFromEnum(c)].value.enum_tag = 0;
    ir.types.items[@intFromEnum(other)].enumeration.identity = 0;
    try std.testing.expectError(error.NonCanonicalType, ir.verify());
    ir.types.items[@intFromEnum(other)].enumeration.identity = 1;
}

test "enum declarations share the namespace and unsupported backing syntax diagnoses" {
    for ([_][]const u8{ "let K=enum{x,}\nlet K=enum{y,}", "let K=enum{x,}\nlet K:uint=1", "let K=enum{x,\nlet f:()={}\nlet f:()={}\n}" }) |source| {
        var module = try Module.init(std.testing.allocator, "duplicate-enum.nov", source);
        defer module.deinit();
        try std.testing.expectEqual(Module.Diagnostic.Tag.duplicate_name, module.diagnostics.items[0].tag);
    }
    for ([_][]const u8{ "let K=enum(u32){x,}", "let K=enum(int){x,}" }) |source| {
        var module = try Module.init(std.testing.allocator, "enum-backing.nov", source);
        defer module.deinit();
        try std.testing.expect(module.ast.errors.len != 0);
        var output = std.Io.Writer.Allocating.init(std.testing.allocator);
        defer output.deinit();
        try Diagnostics.renderModule(&module, &output.writer);
        try std.testing.expect(std.mem.indexOf(u8, output.written(), "explicit enum backing types are unsupported") != null);
    }
}
