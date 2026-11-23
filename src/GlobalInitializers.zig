const std = @import("std");
const Ast = @import("Ast.zig");
const Module = @import("Module.zig");
const Sema = @import("Sema.zig");
const Ir = @import("Ir.zig");
const Dependencies = @import("Dependencies.zig");
const BodyChecker = @import("BodyChecker.zig");
const Lowering = @import("Lowering.zig");
const TestAllocations = @import("TestAllocations.zig");

test "len builtin global known data folds Unicode scalars including NUL" {
    var module = try Module.init(std.testing.allocator, "global-len.nov",
        \\let n:uint=@len(text)
        \\let count:uint=@len(items)
        \\let items:[]int=[1,2]
        \\let text="é\0😀"
        \\let main:()=@println(n,count)
    );
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    errdefer std.debug.print("global len: {any}\n", .{module.diagnostics.items});
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
    var lengths: usize = 0;
    for (sema.ir.globals.items) |global| {
        if (!std.mem.eql(u8, global.name, "n") and !std.mem.eql(u8, global.name, "count")) continue;
        for (sema.ir.getFunction(global.initializer).?.body.?.instructions) |instruction| {
            try std.testing.expect(instruction.op != .array_len);
            if (instruction.op == .constant) {
                const value = sema.ir.getConstant(instruction.op.constant).?.value;
                if (value == .unsigned_integer and value.unsigned_integer == @as(u128, if (std.mem.eql(u8, global.name, "n")) 3 else 2)) lengths += 1;
            }
        }
    }
    try std.testing.expectEqual(@as(usize, 2), lengths);
}

fn testExtremumGlobalData(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "global-extremum.nov",
        \\let small:i16=@min(later,@as(i16,-7),@as(i16,4))
        \\let large:u8=@max([@as(u8,2),@as(u8,255),@as(u8,9)])
        \\let middle:u16=@min(@as(u16,9),@as(u16,3),@as(u16,7))
        \\let zero:f32=@max(@as(f32,-0.0),@as(f32,0.0))
        \\let single:i16=@min([later])
        \\let later:i16=12
        \\let main:()={}
    );
    defer module.deinit();
    try expectParsed(&module);
    var result = try Lowering.lower(&module);
    defer result.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(result.lowering_complete);
    try result.ir.verify();
    for (result.ir.globals.items) |global| {
        const body = result.ir.getFunction(global.initializer).?.body.?;
        const instruction = body.instructions[@backingInt(body.result)];
        try std.testing.expectEqual(global.ty, instruction.ty);
        try std.testing.expect(instruction.op == .constant);
        const value = result.ir.getConstant(instruction.op.constant).?.value;
        if (std.mem.eql(u8, global.name, "small")) try std.testing.expectEqual(@as(i128, -7), value.signed_integer);
        if (std.mem.eql(u8, global.name, "large")) try std.testing.expectEqual(@as(u128, 255), value.unsigned_integer);
        if (std.mem.eql(u8, global.name, "middle")) try std.testing.expectEqual(@as(u128, 3), value.unsigned_integer);
        if (std.mem.eql(u8, global.name, "zero")) try std.testing.expectEqual(@as(u64, 0x80000000), value.float_bits);
        if (std.mem.eql(u8, global.name, "single")) try std.testing.expectEqual(@as(i128, 12), value.signed_integer);
    }
}

test "extremum globals preserve scalar and array known data under OOM" {
    try testExtremumGlobalData(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testExtremumGlobalData, .{});
}

test "extremum global evaluator preserves NaN payloads and signed zero ties" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    for ([_]u16{ 32, 64 }) |bits| {
        const ty = try ir.internType(.{ .float = bits });
        const nan: u64 = if (bits == 32) 0x7fc00017 else 0x7ff8000000000017;
        const negative_zero: u64 = if (bits == 32) 0x80000000 else 0x8000000000000000;
        const one: u64 = if (bits == 32) 0x3f800000 else 0x3ff0000000000000;
        for ([_][2]u64{ .{ nan, one }, .{ one, nan }, .{ negative_zero, 0 }, .{ 0, negative_zero } }) |pair| {
            const data = [_]Data{ .{ .scalar = .{ .float_bits = pair[0] } }, .{ .scalar = .{ .float_bits = pair[1] } } };
            const values = [_]Data{ data[0], data[1], .{ .aggregate = &data } };
            const evaluator: Evaluator = .{ .allocator = std.testing.allocator, .ir = &ir, .globals = &.{}, .values = &values, .instructions = &.{} };
            for ([_]bool{ false, true }) |maximum| {
                for ([_]bool{ false, true }) |array| {
                    const actual = try evaluator.evaluate(.{ .ty = ty, .span = .{ .start = 0, .end = 0 }, .op = .{ .extremum = .{
                        .maximum = maximum,
                        .array = array,
                        .args = if (array) &.{@fromBackingInt(2)} else &.{ @fromBackingInt(0), @fromBackingInt(1) },
                    } } });
                    try std.testing.expectEqual(pair[0], actual.scalar.float_bits);
                }
            }
        }
    }
}

test "extremum globals reject empty arrays runtime calls and reached factories" {
    for ([_][]const u8{
        "@min(@as([]int,[]))",
        "@max(@as([]int,[]))",
        "@min(f(),1)",
        "@max([@default(S).value])",
    }) |expression| {
        const source = try std.mem.concat(std.testing.allocator, u8, &.{
            "let S=struct{value:int=0,\n@[default]\nlet make:()->S=S.[value=9]\n}\nlet f:()->int=1\nlet value=", expression,
        });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "global-extremum-invalid.nov", source);
        defer module.deinit();
        try expectParsed(&module);
        var result = try Lowering.lower(&module);
        defer result.deinit();
        try std.testing.expect(module.hasErrors());
        try std.testing.expect(!result.lowering_complete);
    }
}

test "extremum globals reject reached hooks without field fallback" {
    for ([_][]const u8{ "@min(S.[value=1],S.[value=2])", "@max([S.[value=1],S.[value=2]])", "true and @min(S.[value=1],S.[value=2]).value==1" }) |expression| {
        const source = try std.mem.concat(std.testing.allocator, u8, &.{
            "let S=struct{value:int,\n@[operator(.less_than)]\nlet before:(a:S,b:S)->bool=a.value<b.value\n}\nlet value=", expression,
        });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "global-extremum-hook.nov", source);
        defer module.deinit();
        try expectParsed(&module);
        var result = try Lowering.lower(&module);
        defer result.deinit();
        try std.testing.expect(module.hasErrors());
        try std.testing.expectEqual(Module.Diagnostic.Tag.unsupported_global_initializer, module.diagnostics.items[0].tag);
        try std.testing.expect(!result.lowering_complete);
    }
}

test "extremum globals skip unreachable hooks and singleton comparisons" {
    var module = try Module.init(std.testing.allocator, "global-extremum-hook-skip.nov",
        \\let S=struct{value:int,
        \\@[operator(.less_than)]
        \\let before:(a:S,b:S)->bool=a.value<b.value
        \\}
        \\let single=@min([S.[value=7]]).value
        \\let no=false and @min(S.[value=1],S.[value=2]).value==1
        \\let yes=true or @max([S.[value=1],S.[value=2]]).value==2
        \\let Alias=@TypeOf(@min(S.[value=1],S.[value=2]))
        \\let value:Alias=S.[value=3]
        \\let main:()={}
    );
    defer module.deinit();
    try expectParsed(&module);
    var result = try Lowering.lower(&module);
    defer result.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(result.lowering_complete);
    try result.ir.verify();
    for (result.ir.globals.items) |global| {
        if (!std.mem.eql(u8, global.name, "single")) continue;
        const body = result.ir.getFunction(global.initializer).?.body.?;
        const instruction = body.instructions[@backingInt(body.result)];
        try std.testing.expect(instruction.op == .constant);
        try std.testing.expectEqual(@as(i128, 7), result.ir.getConstant(instruction.op.constant).?.value.signed_integer);
    }
}

test "extremum globals isolate queries and skip unreachable factories" {
    var module = try Module.init(std.testing.allocator, "global-extremum-query.nov",
        \\let S=struct{value:int=0,
        \\@[default]
        \\let make:()->S=S.[value=9]
        \\}
        \\let Alias=@TypeOf(@min(@default(S).value,1))
        \\let no=false and @min(@default(S).value,1)==1
        \\let yes=true or @max(@default(S).value,1)==1
        \\let value:Alias=3
        \\let main:()={}
    );
    defer module.deinit();
    try expectParsed(&module);
    var result = try Lowering.lower(&module);
    defer result.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(result.lowering_complete);
    try result.ir.verify();
    for (result.ir.functions.items) |function| try std.testing.expect(!std.mem.eql(u8, function.name, "make"));
}

fn testArraySliceEvaluation(allocator: std.mem.Allocator) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var ir = Ir.init(allocator);
    defer ir.deinit();
    const integer = try ir.internType(.{ .integer = .{ .bits = Ir.native_pointer_bits, .signedness = .signed } });
    const array = try ir.internType(.{ .array = integer });
    const nested = [_]Data{ .{ .scalar = .{ .string = "managed" } }, .{ .function = @fromBackingInt(7) } };
    const payload: Data = .{ .aggregate = &nested };
    const elements = [_]Data{ .{ .scalar = .{ .signed_integer = 1 } }, payload, .{ .tagged = .{ .index = 2, .payload = &payload } } };
    var values = [_]Data{ .{ .aggregate = &elements }, .{ .scalar = .{ .signed_integer = -2 } }, .{ .scalar = .{ .signed_integer = -1 } } };
    const instructions = [_]Ir.Instruction{.{ .ty = array, .span = .{ .start = 0, .end = 0 }, .op = undefined }};
    const evaluator: Evaluator = .{ .allocator = arena.allocator(), .ir = &ir, .globals = &.{}, .values = &values, .instructions = &instructions };
    var instruction: Ir.Instruction = .{ .ty = array, .span = .{ .start = 0, .end = 0 }, .op = .{ .array_slice = .{ .base = @fromBackingInt(0), .start = @fromBackingInt(1), .end = @fromBackingInt(2), .inclusive = true } } };
    const result = try evaluator.evaluate(instruction);
    try std.testing.expectEqual(@as(usize, 2), result.aggregate.len);
    try std.testing.expect(result.aggregate.ptr != elements[1..].ptr);
    try std.testing.expectEqualDeep(elements[1..], result.aggregate);
    for ([_]Data{
        .{ .scalar = .{ .boolean = true } },       .{ .scalar = .{ .string = "1" } },
        .{ .scalar = .{ .unsigned_integer = 1 } }, .{ .scalar = .{ .rune = 1 } },
        .{ .scalar = .{ .enum_tag = 1 } },         .{ .scalar = .{ .float_bits = 0 } },
        .{ .scalar = .void },                      payload,
        elements[2],                               nested[1],
    }) |invalid| {
        for ([_]bool{ false, true }) |end| {
            values[1] = invalid;
            instruction.op.array_slice.start = if (end) null else @fromBackingInt(1);
            instruction.op.array_slice.end = if (end) @fromBackingInt(1) else null;
            try std.testing.expectError(error.InvalidData, evaluator.evaluate(instruction));
        }
    }
    for ([_][2]i128{ .{ -4, 0 }, .{ 0, 4 }, .{ 2, 1 }, .{ std.math.minInt(i128), 0 }, .{ 0, std.math.maxInt(i128) } }) |bounds| {
        values[1] = .{ .scalar = .{ .signed_integer = bounds[0] } };
        values[2] = .{ .scalar = .{ .signed_integer = bounds[1] } };
        instruction.op.array_slice.start = @fromBackingInt(1);
        instruction.op.array_slice.end = @fromBackingInt(2);
        instruction.op.array_slice.inclusive = false;
        try std.testing.expectError(error.InvalidData, evaluator.evaluate(instruction));
    }
}

test "array slice evaluator copies Data and rejects invalid scalar bounds under OOM" {
    try testArraySliceEvaluation(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testArraySliceEvaluation, .{});
}

fn testArraySliceData(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "global-array-slice.nov",
        \\let tail=items[first..=-1]
        \\let count=@len(tail)
        \\let first:int=-2
        \\let items=[1,2,3]
        \\let all=items[..=]
        \\let empty=items[3..3]
        \\let none:[]str=[]
        \\let R=struct{rows:[][]str,}
        \\let U=union{some:R,none,}
        \\let rows=[["a","b"],["c"]]
        \\let record=R.[rows=rows[..]]
        \\let wrapped=U.some[record]
        \\let wrappers=[wrapped]
        \\let managed=wrappers[..]
        \\let nested=record.rows[-2..=0][0][1..]
        \\let flag_values=[true,false]
        \\let flags=flag_values[..]
        \\let float_values=[1.0,2.0]
        \\let floats=float_values[..]
        \\let rune_values=['é','x']
        \\let runes=rune_values[..]
        \\let E=enum{one,two,}
        \\let enum_values=[E.one,E.two]
        \\let enums=enum_values[..]
        \\let callback:()->int=1
        \\let callback_values=[callback]
        \\let callbacks=callback_values[..]
        \\let range_values=[0..2,3..=4]
        \\let ranges=range_values[..]
        \\let Alias=@TypeOf(items[{1/0}..])
        \\let queried:Alias=items[..]
        \\let no=false and @len(items[{1/0}..])==0
        \\let yes=true or @len(items[..=3])==0
        \\let proof_tail=tail==[2,3]
        \\let proof_all=all==items
        \\let proof_empty=@len(empty)==0
        \\let proof_none=@len(none[..=])==0
        \\let proof_nested=nested==["b"]
        \\let proof_managed=managed==[wrapped]
        \\let proof_flags=flags[0]
        \\let proof_floats=floats[1]==2.0
        \\let proof_runes=runes[0]=='é'
        \\let proof_enums=enums[1]==E.two
        \\let proof_callbacks=@len(callbacks)==1
        \\let proof_ranges=@len(ranges[1])==2
        \\let proof_no=!no
        \\let proof_yes=yes
        \\let main:()={}
    );
    defer module.deinit();
    try expectParsed(&module);
    var result = try Lowering.lower(&module);
    defer result.deinit();
    if (module.hasErrors()) std.debug.print("array slice globals: {any}\n", .{module.diagnostics.items});
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(result.lowering_complete);
    try result.ir.verify();
    var proofs: usize = 0;
    var slices: usize = 0;
    for (result.ir.globals.items) |global| {
        const body = result.ir.getFunction(global.initializer).?.body.?;
        for (body.instructions) |instruction| {
            try std.testing.expect(instruction.op != .call);
            if (instruction.op == .array_slice) slices += 1;
        }
        if (std.mem.eql(u8, global.name, "count") or std.mem.startsWith(u8, global.name, "proof_")) {
            const instruction = body.instructions[@backingInt(body.result)];
            try std.testing.expect(instruction.op == .constant);
            const value = result.ir.getConstant(instruction.op.constant).?.value;
            if (std.mem.eql(u8, global.name, "count")) {
                try std.testing.expect(value == .unsigned_integer and value.unsigned_integer == 2);
            } else {
                try std.testing.expect(value == .boolean and value.boolean);
                proofs += 1;
            }
        }
    }
    try std.testing.expectEqual(@as(usize, 14), proofs);
    try std.testing.expect(slices >= 15);
}

test "array slice globals preserve known managed data queries and skipped bounds under OOM" {
    try testArraySliceData(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testArraySliceData, .{});
}

fn testArraySliceRejection(allocator: std.mem.Allocator, source: []const u8, tag: Module.Diagnostic.Tag) !void {
    var module = try Module.init(allocator, "global-array-slice-invalid.nov", source);
    defer module.deinit();
    try expectParsed(&module);
    var result = try Lowering.lower(&module);
    defer result.deinit();
    try std.testing.expect(module.hasErrors());
    try std.testing.expectEqual(tag, module.diagnostics.items[0].tag);
    try std.testing.expect(!result.lowering_complete);
}

test "array slice globals reject reached invalid bounds and user calls under OOM" {
    for ([_][]const u8{
        "items[-4..]",                              "items[..4]", "items[2..1]", "items[..=3]", "items[..=-4]",
        "items[@as(int,-9223372036854775807)-1..]",
    }) |expression| {
        const source = try std.mem.concat(std.testing.allocator, u8, &.{ "let items=[1,2,3]\nlet f:()->int=0\nlet value=", expression });
        defer std.testing.allocator.free(source);
        try testArraySliceRejection(std.testing.allocator, source, .unsupported_global_initializer);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testArraySliceRejection, .{ source, .unsupported_global_initializer });
    }
    const call_source = "let items=[1,2,3]\nlet f:()->int=0\nlet value=items[f()..]";
    try testArraySliceRejection(std.testing.allocator, call_source, .unsupported_dependency_expression);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testArraySliceRejection, .{ call_source, .unsupported_dependency_expression });
}

fn testArrayConcatenationData(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "global-array-concat.nov",
        \\let rows=first+[[2]]
        \\let first=[[1]]
        \\let count=@len(rows)
        \\let empty:[]u8=[]+[]
        \\let main:()={@assert(count==2 and rows[0][0]==1 and rows[1][0]==2)}
    );
    defer module.deinit();
    var result = try Lowering.lower(&module);
    defer result.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(result.lowering_complete);
    try result.ir.verify();
    var counts: usize = 0;
    for (result.ir.globals.items) |global| {
        if (!std.mem.eql(u8, global.name, "count")) continue;
        const body = result.ir.getFunction(global.initializer).?.body.?;
        const instruction = body.instructions[@backingInt(body.result)];
        try std.testing.expect(instruction.op == .constant);
        const value = result.ir.getConstant(instruction.op.constant).?.value;
        try std.testing.expect(value == .unsigned_integer and value.unsigned_integer == 2);
        counts += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), counts);
}

test "array concatenation validates global known data and folds lengths under OOM" {
    try testArrayConcatenationData(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testArrayConcatenationData, .{});
}

fn expectParsed(module: *const Module) !void {
    if (module.source.ast.errors.len != 0) {
        std.debug.print("global initializer parse errors: {any}\nsource:\n{s}\n", .{ module.source.ast.errors, module.source.ast.source });
    }
    try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
}

fn testStructuralEqualityData(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "global-equality.nov",
        \\let E=enum{one,two,}
        \\let R=struct{items:[][]int,kind:E,}
        \\let U=union{none,some:R,number:int,}
        \\let r=R.[items=[[1,2],[]],kind=.one]
        \\let u=U.some[r]
        \\let pass_array=[[1,2],[]]==[[1,2],[]]
        \\let pass_length=[1]!=[1,2]
        \\let pass_element=[[1]]!=[[2]]
        \\let pass_record=r==R.[items=[[1,2],[]],kind=.one]
        \\let pass_field=r!=R.[items=[[1,2],[]],kind=.two]
        \\let pass_union=u==U.some[r]
        \\let pass_payload=u!=U.some[R.[items=[],kind=.one]]
        \\let pass_tag=U.number[1]!=u
        \\let pass_void=U.none[]==U.none[]
        \\let pass_enum=E.one!=E.two
        \\let pass_zero=[0.0]==[-0.0]
        \\let pass_float=[1.0]!=[2.0]
        \\let pass_f32=[@as(f32,0.0)]==[@as(f32,-0.0)]
        \\let pass_short=false and {[1]==[2]} or true
        \\let main:()={}
    );
    defer module.deinit();
    try expectParsed(&module);
    var result = try Lowering.lower(&module);
    defer result.deinit();
    if (module.hasErrors()) std.debug.print("global equality: {any}\n", .{module.diagnostics.items});
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(result.lowering_complete);
    try result.ir.verify();
    var count: usize = 0;
    for (result.ir.globals.items) |global| {
        if (!std.mem.startsWith(u8, global.name, "pass_")) continue;
        const body = result.ir.getFunction(global.initializer).?.body.?;
        const instruction = body.instructions[@backingInt(body.result)];
        // Short-circuit merge instructions intentionally remain in the IR.
        if (instruction.op == .short_circuit) continue;
        try std.testing.expect(instruction.op == .constant);
        const value = result.ir.getConstant(instruction.op.constant).?.value;
        try std.testing.expect(value == .boolean and value.boolean);
        count += 1;
    }
    try std.testing.expectEqual(@as(usize, 13), count);
}

test "global structural equality known data under OOM" {
    try testStructuralEqualityData(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testStructuralEqualityData, .{});
}

test "global structural equality rejects exact type mismatches in frontend" {
    for ([_][:0]const u8{
        "let A=struct{x:int,}\nlet B=struct{x:int,}\nlet value=A.[x=1]==B.[x=1]",
        "let E=enum{one,}\nlet F=enum{one,}\nlet value=E.one==F.one",
        "let value=[@as(i8,1)]==[@as(i16,1)]",
        "let value=[@as(f32,1.0)]==[@as(f64,1.0)]",
        "let a=0..1\nlet b=0..1\nlet value=a==b",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "global-equality-types.nov", source);
        defer module.deinit();
        try expectParsed(&module);
        var result = try Lowering.lower(&module);
        defer result.deinit();
        try std.testing.expect(module.hasErrors());
        try std.testing.expect(module.diagnostics.items.len != 0);
        for (module.diagnostics.items) |diagnostic| {
            try std.testing.expect(diagnostic.tag != .unsupported_global_initializer);
            try std.testing.expect(diagnostic.tag != .unsupported_dependency_expression);
        }
        try std.testing.expect(!result.lowering_complete);
    }
}

test "global structural equality safely skips unreachable hooks" {
    var module = try Module.init(std.testing.allocator, "global-equality-skipped-hooks.nov",
        \\let S=struct{x:int,
        \\@[operator(.equal)]
        \\let eq:(a:S,b:S)->bool=false
        \\}
        \\let U=union{none,some:S,}
        \\let P=struct{first:int,item:S,}
        \\let empty:[]S=[]
        \\let pass_empty=empty==empty
        \\let pass_length=empty!=[S.[x=1]]
        \\let pass_tag=U.none[]!=U.some[S.[x=1]]
        \\let pass_void=U.none[]==U.none[]
        \\let pass_field=P.[first=1,item=S.[x=1]]!=P.[first=2,item=S.[x=1]]
        \\let pass_short=true or {S.[x=1]==S.[x=1]}
        \\let main:()={}
    );
    defer module.deinit();
    try expectParsed(&module);
    var result = try Lowering.lower(&module);
    defer result.deinit();
    if (module.hasErrors()) std.debug.print("global skipped hooks: {any}\n", .{module.diagnostics.items});
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(result.lowering_complete);
    try result.ir.verify();
    var count: usize = 0;
    for (result.ir.globals.items) |global| {
        if (!std.mem.startsWith(u8, global.name, "pass_") or std.mem.eql(u8, global.name, "pass_short")) continue;
        const body = result.ir.getFunction(global.initializer).?.body.?;
        const instruction = body.instructions[@backingInt(body.result)];
        try std.testing.expect(instruction.op == .constant);
        const value = result.ir.getConstant(instruction.op.constant).?.value;
        try std.testing.expect(value == .boolean and value.boolean);
        count += 1;
    }
    try std.testing.expectEqual(@as(usize, 5), count);
}

test "global structural equality membership evaluates known aggregates directly" {
    // The legacy dependency scanner rejects global `in` before data validation.
    // Exercise the evaluator directly rather than mistaking that for hook rejection.
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const span: Ir.Span = .{ .start = 0, .end = 0 };
    const boolean = try ir.internType(.boolean);
    const unit = try ir.internType(.void);
    const integer = try ir.internType(.{ .integer = .{ .bits = 32, .signedness = .signed } });
    const array = try ir.internType(.{ .array = integer });
    const record = try ir.internType(.{ .record = .{ .identity = 0, .fields = &.{} } });
    try ir.setRecordFields(record, &.{.{ .name = "items", .ty = array, .span = span }});
    const tagged = try ir.internType(.{ .record = .{ .identity = 1, .tagged = true, .fields = &.{} } });
    try ir.setRecordFields(tagged, &.{ .{ .name = "none", .ty = unit, .span = span }, .{ .name = "some", .ty = record, .span = span } });
    const one: Data = .{ .scalar = .{ .signed_integer = 1 } };
    const items: Data = .{ .aggregate = &.{one} };
    const row: Data = .{ .aggregate = &.{items} };
    const some: Data = .{ .tagged = .{ .index = 1, .payload = &row } };
    const none: Data = .{ .tagged = .{ .index = 0, .payload = null } };
    for ([_]struct { ty: Ir.TypeId, value: Data, other: Data }{
        .{ .ty = array, .value = items, .other = .{ .aggregate = &.{} } },
        .{ .ty = record, .value = row, .other = .{ .aggregate = &.{.{ .aggregate = &.{} }} } },
        .{ .ty = tagged, .value = some, .other = none },
        .{ .ty = tagged, .value = none, .other = some },
    }) |case| {
        const container = try ir.internType(.{ .array = case.ty });
        const instructions = [_]Ir.Instruction{
            .{ .ty = case.ty, .span = span, .op = undefined },
            .{ .ty = container, .span = span, .op = undefined },
        };
        var values = [_]Data{ case.value, .{ .aggregate = &.{ case.other, case.value } } };
        const evaluator: Evaluator = .{ .allocator = std.testing.allocator, .ir = &ir, .globals = &.{}, .values = &values, .instructions = &instructions };
        const contains: Ir.Instruction = .{ .ty = boolean, .span = span, .op = .{ .array_contains = .{ .value = @fromBackingInt(0), .array = @fromBackingInt(1) } } };
        try std.testing.expect((try evaluator.evaluate(contains)).scalar.boolean);
        values[1] = .{ .aggregate = &.{case.other} };
        try std.testing.expect(!(try evaluator.evaluate(contains)).scalar.boolean);
        values[1] = .{ .aggregate = &.{} };
        try std.testing.expect(!(try evaluator.evaluate(contains)).scalar.boolean);
        if (case.ty == record) {
            const signature = try ir.internType(.{ .function = .{ .params = &.{ record, record }, .result = boolean } });
            const hook = try ir.addFunction("equal", signature, span);
            try ir.setTypeEquality(record, hook);
            // Empty scans need no comparison; nonempty scans must not bypass the hook.
            try std.testing.expect(!(try evaluator.evaluate(contains)).scalar.boolean);
            values[1] = .{ .aggregate = &.{case.value} };
            try std.testing.expectError(error.InvalidData, evaluator.evaluate(contains));
            try ir.setTypeEquality(record, null);
        }
    }
}

test "global structural float equality uses IEEE NaN and signed zero semantics" {
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const boolean = try ir.internType(.boolean);
    for ([_]u16{ 32, 64 }) |bits| {
        const float = try ir.internType(.{ .float = bits });
        const array = try ir.internType(.{ .array = float });
        var values = [_]Data{ undefined, undefined, undefined };
        const instructions = [_]Ir.Instruction{
            .{ .ty = array, .span = .{ .start = 0, .end = 0 }, .op = undefined },
            .{ .ty = array, .span = .{ .start = 0, .end = 0 }, .op = undefined },
            .{ .ty = float, .span = .{ .start = 0, .end = 0 }, .op = undefined },
        };
        const evaluator: Evaluator = .{ .allocator = std.testing.allocator, .ir = &ir, .globals = &.{}, .values = &values, .instructions = &instructions };
        for ([_]struct { lhs: u64, rhs: u64, expected: bool }{
            .{ .lhs = if (bits == 32) 0x7fc00000 else 0x7ff8000000000000, .rhs = if (bits == 32) 0x7fc00000 else 0x7ff8000000000000, .expected = false },
            .{ .lhs = 0, .rhs = if (bits == 32) 0x80000000 else 0x8000000000000000, .expected = true },
            .{ .lhs = if (bits == 32) 0x3f800000 else 0x3ff0000000000000, .rhs = 0, .expected = false },
        }) |case| {
            const lhs: Data = .{ .scalar = .{ .float_bits = case.lhs } };
            const rhs: Data = .{ .scalar = .{ .float_bits = case.rhs } };
            values[0] = .{ .aggregate = &.{lhs} };
            values[1] = .{ .aggregate = &.{rhs} };
            values[2] = lhs;
            for ([_]Ir.BinaryOp{ .eq, .ne }) |op| {
                const actual = try evaluator.evaluate(.{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .binary = .{ .op = op, .lhs = @fromBackingInt(0), .rhs = @fromBackingInt(1) } } });
                try std.testing.expectEqual(if (op == .eq) case.expected else !case.expected, actual.scalar.boolean);
            }
            const member = try evaluator.evaluate(.{ .ty = boolean, .span = .{ .start = 0, .end = 0 }, .op = .{ .array_contains = .{ .value = @fromBackingInt(2), .array = @fromBackingInt(1) } } });
            try std.testing.expectEqual(case.expected, member.scalar.boolean);
        }
    }
}

test "global structural equality never invokes nominal hooks" {
    for ([_][]const u8{
        "S.[x=1]==S.[x=1]",
        "S.[x=1]!=S.[x=2]",
        "[S.[x=1]]==[S.[x=1]]",
        "P.[item=S.[x=1]]==P.[item=S.[x=1]]",
        "U.some[S.[x=1]]==U.some[S.[x=1]]",
        "E.one==E.one",
        "V.none[]==V.none[]",
    }) |expression| {
        const source = try std.mem.concat(std.testing.allocator, u8, &.{
            "let S=struct{x:int,\n@[operator(.equal)]\nlet eq:(a:S,b:S)->bool=true\n}\n",
            "let E=enum{one,\n@[operator(.equal)]\nlet eq:(a:E,b:E)->bool=true\n}\n",
            "let V=union{none,\n@[operator(.equal)]\nlet eq:(a:V,b:V)->bool=true\n}\n",
            "let P=struct{item:S,}\nlet U=union{none,some:S,}\nlet value=",
            expression,
        });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "global-equality-hook.nov", source);
        defer module.deinit();
        try expectParsed(&module);
        var result = try Lowering.lower(&module);
        defer result.deinit();
        try std.testing.expect(module.hasErrors());
        if (module.diagnostics.items[0].tag != .unsupported_global_initializer) std.debug.print("global hook expression {s}: {any}\n", .{ expression, module.diagnostics.items });
        try std.testing.expectEqual(Module.Diagnostic.Tag.unsupported_global_initializer, module.diagnostics.items[0].tag);
        try std.testing.expect(!result.lowering_complete);
    }
}

fn testDefaultGlobalData(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "global-default-data.nov",
        \\let Alias=u8
        \\let R=struct{items:[][]int=[[1]],number:u8=7,}
        \\let Box:(T:type)->type=struct{items:[]T=[],}
        \\let byte:Alias=@default(Alias)
        \\let flag:bool=@default(bool)
        \\let text:str=@default(str)
        \\let scalar:f32=@default(f32)
        \\let items:[][]int=@default([][]int)
        \\let range:..u8=@default(..u8)
        \\let record:R=@default(R)
        \\let second:R=@default(R)
        \\let copy:R=record
        \\let box:Box(u8)=@default(Box(u8))
        \\let queried:@TypeOf(@default(R))=@default(@TypeOf(record))
        \\let main:()={}
    );
    defer module.deinit();
    try expectParsed(&module);
    var result = try Lowering.lower(&module);
    defer result.deinit();
    if (module.hasErrors()) std.debug.print("global defaults: {any}\n", .{module.diagnostics.items});
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(result.lowering_complete);
    try result.ir.verify();
    var records: usize = 0;
    for (result.ir.globals.items) |global| {
        const body = result.ir.getFunction(global.initializer).?.body.?;
        try std.testing.expectEqual(global.ty, body.instructions[@backingInt(body.result)].ty);
        for (body.instructions) |instruction| {
            try std.testing.expect(instruction.op != .call);
            if (instruction.op == .record_init) records += 1;
        }
    }
    try std.testing.expect(records >= 3);
}

test "global defaults preserve exact data types and managed construction under OOM" {
    try testDefaultGlobalData(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testDefaultGlobalData, .{});
}

test "global defaults reject reached custom factories without struct fallback" {
    for ([_][]const u8{
        "@default(S)",
        "@default(S).value==0",
        "true and @default(S).value==0",
    }) |expression| {
        const source = try std.mem.concat(std.testing.allocator, u8, &.{
            "let S=struct{value:int=0,\n@[default]\nlet make:()->S=S.[value=9]\n}\nlet value=", expression,
        });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "global-default-factory.nov", source);
        defer module.deinit();
        try expectParsed(&module);
        var result = try Lowering.lower(&module);
        defer result.deinit();
        try std.testing.expect(module.hasErrors());
        try std.testing.expectEqual(Module.Diagnostic.Tag.unsupported_global_initializer, module.diagnostics.items[0].tag);
        try std.testing.expect(!result.lowering_complete);
    }
}

test "global default type query does not publish a custom factory" {
    var module = try Module.init(std.testing.allocator, "global-default-type-only.nov",
        \\let S=struct{value:int=0,
        \\@[default]
        \\let make:()->S=S.[value=9]
        \\}
        \\let Alias=@TypeOf(@default(S))
        \\let value:Alias=S.[value=1]
        \\let main:()={}
    );
    defer module.deinit();
    try expectParsed(&module);
    var result = try Lowering.lower(&module);
    defer result.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(result.lowering_complete);
    try result.ir.verify();
    for (result.ir.functions.items) |function| {
        try std.testing.expect(!std.mem.eql(u8, function.name, "make"));
        if (function.body) |body| for (body.instructions) |instruction| {
            try std.testing.expect(instruction.op != .call);
        };
    }
}

test "global defaults skip unreachable factories and do not publish type queries" {
    var module = try Module.init(std.testing.allocator, "global-default-query.nov",
        \\let S=struct{value:int=0,
        \\@[default]
        \\let make:()->S=S.[value=9]
        \\}
        \\let Alias=@TypeOf(@default(S))
        \\let no=false and @default(S).value==9
        \\let yes=true or @default(S).value==9
        \\let value:Alias=S.[value=1]
        \\let main:()={}
    );
    defer module.deinit();
    try expectParsed(&module);
    var result = try Lowering.lower(&module);
    defer result.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(result.lowering_complete);
    try result.ir.verify();
    for (result.ir.globals.items) |global| try std.testing.expect(!std.mem.eql(u8, global.name, "Alias"));
}

const Ref = struct { module: *Module, id: Module.DeclId };
const Task = struct { ref: Ref, finish: bool = false, span: ?Module.Span = null };
const State = enum { unvisited, visiting, resolved, failed };

/// Check global data before any ordinary function body. The explicit worklist
/// also orders references across source boundaries without recursive AST walks.
pub fn analyze(root: *Module, sema: *Sema) Sema.Error!void {
    if (sema.compilation == null) {
        var dependencies = try Dependencies.analyze(root);
        defer dependencies.deinit();
        if (root.hasErrors()) return;
    }
    const count = sema.entries.len;
    const states = try sema.allocator.alloc(State, count);
    defer sema.allocator.free(states);
    @memset(states, .unvisited);
    var tasks: std.ArrayList(Task) = .empty;
    defer tasks.deinit(sema.allocator);
    var nodes: std.ArrayList(Ast.Node.Index) = .empty;
    defer nodes.deinit(sema.allocator);
    for (0..count) |index| {
        const ref: Ref = if (sema.compilation) |comp| blk: {
            const source_ref = comp.declarations.items[index];
            break :blk .{ .module = comp.getModule(source_ref.source).?, .id = source_ref.local };
        } else .{ .module = root, .id = @fromBackingInt(@intCast(index)) };
        if (!ordinary(sema, ref)) continue;
        try tasks.append(sema.allocator, .{ .ref = ref });
        while (tasks.pop()) |task| {
            const current = task.ref;
            const module = current.module;
            const id = sema.entryIndex(module, current.id);
            if (!ordinary(sema, current)) continue;
            const declaration = module.getDeclaration(current.id).?;
            const full = module.source.ast.decl(declaration.node).ast;
            if (task.finish) {
                if (states[id] == .failed) continue;
                if (if (sema.compilation) |comp| comp.hasErrors() else module.hasErrors()) {
                    sema.entries[id].status = .failed;
                    states[id] = .failed;
                    continue;
                }
                const diagnostics_start = module.diagnostics.items.len;
                const ty = try BodyChecker.checkGlobalInitializer(module, sema, current.id);
                try sema.syncEntries(module);
                if (ty) |value_type| {
                    if (Ir.isArrayElement(sema.ir.getType(value_type).?) and module.diagnostics.items.len == diagnostics_start) {
                        sema.entries[id].type_id = value_type;
                        sema.entries[id].status = .global_checked;
                        // Mutable literals are storage, never inline constants.
                        sema.entries[id].constant = null;
                        states[id] = .resolved;
                        try sema.global_order.append(sema.allocator, id);
                        continue;
                    }
                }
                if (module.diagnostics.items.len == diagnostics_start) try module.diagnostics.append(module.allocator, .{ .tag = .unsupported_global_initializer, .span = declaration.name_span });
                sema.entries[id].status = .failed;
                states[id] = .failed;
                continue;
            }
            switch (states[id]) {
                .resolved, .failed => continue,
                .visiting => {
                    try module.diagnostics.append(module.allocator, .{ .tag = .value_cycle, .span = task.span orelse declaration.name_span, .related = declaration.name_span });
                    sema.entries[id].status = .failed;
                    states[id] = .failed;
                    continue;
                },
                .unvisited => {},
            }
            states[id] = .visiting;
            try tasks.append(sema.allocator, .{ .ref = current, .finish = true });
            nodes.clearRetainingCapacity();
            if (full.init_node != 0) try nodes.append(sema.allocator, full.init_node);
            while (nodes.pop()) |node| {
                const ast = &module.source.ast;
                const tag = ast.nodes.items(.tag)[node];
                const data = ast.nodes.items(.data)[node];
                const reference: ?Ref = if (module.compilation) |comp| blk: {
                    const target = comp.reference(module, node) orelse break :blk null;
                    break :blk .{ .module = comp.getModule(target.source).?, .id = target.local };
                } else if (tag == .identifier) blk: {
                    const target = module.lookup(ast.tokenSlice(ast.firstToken(node))) orelse break :blk null;
                    break :blk .{ .module = module, .id = target };
                } else null;
                if (reference) |target| {
                    if (ordinary(sema, target)) try tasks.append(sema.allocator, .{ .ref = target, .span = module.tokenSpan(ast.firstToken(node)) });
                    continue;
                }
                switch (tag) {
                    .function_expr, .number_literal, .char_literal, .string_literal, .enum_literal, .identifier => {},
                    .struct_init, .container_decl_arg => for (ast.fullStructInit(node).?.fields) |field| {
                        const value = if (ast.nodes.items(.tag)[field] == .container_field) ast.nodes.items(.data)[field].rhs else field;
                        if (value != 0) try nodes.append(sema.allocator, value);
                    },
                    .array_init, .array_init_comma => try nodes.appendSlice(sema.allocator, ast.extra_data[data.lhs..data.rhs]),
                    .array_init_two, .array_init_two_comma => {
                        if (data.lhs != 0) try nodes.append(sema.allocator, data.lhs);
                        if (data.rhs != 0) try nodes.append(sema.allocator, data.rhs);
                    },
                    .call, .call_one, .call_comma, .call_one_comma => {
                        const call = ast.fullCall(node).?.ast;
                        const type_operand = ast.nodes.items(.tag)[call.fn_expr] == .builtin_literal and
                            (std.mem.eql(u8, ast.tokenSlice(ast.firstToken(call.fn_expr)), "@as") or
                                std.mem.eql(u8, ast.tokenSlice(ast.firstToken(call.fn_expr)), "@default"));
                        // Type queries inside this operand are unevaluated, including
                        // custom default factories. Only eager operands order globals.
                        try nodes.appendSlice(sema.allocator, call.args[if (type_operand) @min(@as(usize, 1), call.args.len) else 0..]);
                    },
                    .block, .block_two, .block_newline, .block_two_newline => {
                        var buffer: [2]Ast.Node.Index = undefined;
                        try nodes.appendSlice(sema.allocator, ast.fullBlock(&buffer, node).?.ast.statements);
                    },
                    .field_access => try nodes.append(sema.allocator, data.lhs),
                    .slice => {
                        const slice = ast.slice(node).ast;
                        try nodes.append(sema.allocator, slice.sliced);
                        if (slice.start != 0) try nodes.append(sema.allocator, slice.start);
                        if (slice.end != 0) try nodes.append(sema.allocator, slice.end);
                    },
                    .negation, .bool_not, .bit_not => try nodes.append(sema.allocator, data.rhs),
                    .add, .sub, .mul, .div, .mod, .equal_equal, .bang_equal, .less_than, .greater_than, .less_or_equal, .greater_or_equal, .shl, .shr, .bit_and, .bit_xor, .bit_or, .bool_and, .bool_or, .range, .array_access => {
                        if (data.lhs != 0) try nodes.append(sema.allocator, data.lhs);
                        if (data.rhs != 0) try nodes.append(sema.allocator, data.rhs);
                    },
                    // Node payloads also contain token and extra-data indices.
                    // Never interpret an unknown tag's payload as child nodes.
                    else => {
                        try module.diagnostics.append(module.allocator, .{ .tag = .unsupported_dependency_expression, .span = module.tokenSpan(ast.firstToken(node)) });
                        states[id] = .failed;
                        sema.entries[id].status = .failed;
                    },
                }
            }
        }
    }
}

fn ordinary(sema: *const Sema, ref: Ref) bool {
    const declaration = ref.module.getDeclaration(ref.id).?;
    if (declaration.container != null) return false;
    const full = ref.module.source.ast.decl(declaration.node).ast;
    const entry = sema.entries[sema.entryIndex(ref.module, ref.id)];
    return entry.status == .deferred or (entry.status == .constant_checked and full.is_mutable);
}

/// Allocate every identity before lowering initializers or ordinary bodies.
pub fn allocate(module: *Module, sema: *Sema, id: Module.DeclId) Sema.Error!void {
    const index = sema.entryIndex(module, id);
    const entry = sema.entries[index];
    if (entry.status != .global_checked) return;
    const declaration = module.getDeclaration(id).?;
    const signature = try sema.ir.internType(.{ .function = .{ .params = &.{}, .result = entry.type_id.? } });
    const name = try std.fmt.allocPrint(sema.allocator, "$global{d}", .{index});
    defer sema.allocator.free(name);
    const initializer = try sema.ir.addFunction(name, signature, declaration.name_span);
    _ = try sema.ir.addGlobal(declaration.name, entry.type_id.?, declaration.name_span, initializer);
}

pub fn lower(module: *Module, sema: *Sema, types: []const ?Ir.TypeId, constants: []const ?Ir.ConstId, functions: []const ?Ir.FunctionId, id: Module.DeclId) Sema.Error!bool {
    const declaration = module.getDeclaration(id).?;
    for (sema.ir.globals.items) |global| {
        if (global.span.source == declaration.name_span.source and global.span.start == declaration.name_span.start) {
            return BodyChecker.lowerGlobalInitializer(module, &sema.ir, types, constants, functions, id, global.initializer);
        }
    }
    return error.InvalidGlobal;
}

const Data = union(enum) {
    scalar: Ir.Constant.Value,
    function: Ir.FunctionId,
    aggregate: []const Data,
    tagged: struct { index: u32, payload: ?*const Data },
};

/// Startup may allocate/copy known data, but must not compute unknown values or
/// invoke user code. Fold scalar operations here, including through aggregate
/// projections and imported globals, before publishing executable IR.
pub fn validateData(root: *Module, sema: *Sema) Sema.Error!void {
    if (if (sema.compilation) |comp| comp.hasErrors() else root.hasErrors()) return;
    var arena = std.heap.ArenaAllocator.init(sema.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const globals = try allocator.alloc(?Data, sema.ir.globals.items.len);
    @memset(globals, null);
    for (sema.ir.globals.items, 0..) |global, global_index| {
        const body = sema.ir.getFunction(global.initializer).?.body.?;
        const values = try allocator.alloc(Data, body.instructions.len);
        var evaluator: Evaluator = .{ .allocator = allocator, .ir = &sema.ir, .globals = globals, .values = values, .instructions = body.instructions };
        var skip_until: usize = 0;
        for (@constCast(body.instructions), 0..) |*instruction, index| {
            if (index < skip_until) continue;
            for (body.instructions[index..], index..) |merge, end| {
                if (merge.op != .short_circuit or merge.op.short_circuit.rhs_start != index or end == index) continue;
                const short = merge.op.short_circuit;
                const lhs = values[@backingInt(short.lhs)];
                if (lhs == .scalar and lhs.scalar == .boolean and
                    (if (short.kind == .@"and") !lhs.scalar.boolean else lhs.scalar.boolean))
                {
                    skip_until = end;
                    break;
                }
            }
            if (index < skip_until) continue;
            const value = evaluator.evaluate(instruction.*) catch |err| switch (err) {
                error.InvalidData => {
                    const owner = if (sema.compilation) |comp| comp.getModule(instruction.span.source).? else root;
                    try owner.diagnostics.append(owner.allocator, .{ .tag = .unsupported_global_initializer, .span = instruction.span });
                    return;
                },
                error.OutOfMemory => return error.OutOfMemory,
            };
            values[index] = value;
            if (value == .scalar and instruction.op != .short_circuit) {
                // Interning validates exact integer widths as well as the host
                // evaluation width. Never leave a trapping operation at startup.
                const constant = sema.ir.internConstant(.{ .ty = instruction.ty, .value = value.scalar }) catch |err| switch (err) {
                    error.IntegerOutOfRange, error.FloatOutOfRange => {
                        const owner = if (sema.compilation) |comp| comp.getModule(instruction.span.source).? else root;
                        try owner.diagnostics.append(owner.allocator, .{ .tag = .literal_out_of_range, .span = instruction.span });
                        return;
                    },
                    else => return err,
                };
                if (instruction.op == .extremum) sema.ir.allocator.free(instruction.op.extremum.args);
                instruction.op = .{ .constant = constant };
            }
        }
        globals[global_index] = values[@backingInt(body.result)];
    }
}

const Evaluator = struct {
    allocator: std.mem.Allocator,
    ir: *Ir,
    globals: []const ?Data,
    values: []const Data,
    instructions: []const Ir.Instruction,

    fn get(self: Evaluator, id: Ir.ValueId) Data {
        return self.values[@backingInt(id)];
    }

    fn scalar(self: Evaluator, id: Ir.ValueId) error{InvalidData}!Ir.Constant.Value {
        return switch (self.get(id)) {
            .scalar => |value| value,
            else => error.InvalidData,
        };
    }

    fn aggregate(self: Evaluator, ids: []const Ir.ValueId) std.mem.Allocator.Error!Data {
        const result = try self.allocator.alloc(Data, ids.len);
        for (result, ids) |*value, id| value.* = self.get(id);
        return .{ .aggregate = result };
    }

    fn evaluate(self: Evaluator, instruction: Ir.Instruction) (std.mem.Allocator.Error || error{InvalidData})!Data {
        const value: Ir.Constant.Value = switch (instruction.op) {
            .constant => |id| self.ir.getConstant(id).?.value,
            .function_ref => |id| return .{ .function = id },
            .closure_init => |closure| {
                if (closure.captures.len != 0) return error.InvalidData;
                return .{ .function = closure.function };
            },
            .global_slot => |id| return self.globals[@backingInt(id)] orelse error.InvalidData,
            .local_load, .array_clone => |id| return self.get(id),
            .record_init, .array_literal => |ids| return self.aggregate(ids),
            .array_append => |append| {
                const lhs = self.get(append.lhs);
                const rhs = self.get(append.rhs);
                if (lhs != .aggregate or rhs != .aggregate) return error.InvalidData;
                const len = std.math.add(usize, lhs.aggregate.len, rhs.aggregate.len) catch return error.InvalidData;
                const data = try self.allocator.alloc(Data, len);
                @memcpy(data[0..lhs.aggregate.len], lhs.aggregate);
                @memcpy(data[lhs.aggregate.len..], rhs.aggregate);
                return .{ .aggregate = data };
            },
            .field_read => |read| {
                const base = self.get(read.base);
                if (base != .aggregate or read.index >= base.aggregate.len) return error.InvalidData;
                return base.aggregate[read.index];
            },
            .union_init => |init| {
                const payload = if (init.payload) |id| blk: {
                    const data = try self.allocator.create(Data);
                    data.* = self.get(id);
                    break :blk data;
                } else null;
                return .{ .tagged = .{ .index = init.index, .payload = payload } };
            },
            .union_tag => |read| blk: {
                const base = self.get(read.base);
                if (base != .tagged) return error.InvalidData;
                break :blk .{ .boolean = base.tagged.index == read.index };
            },
            .union_payload => |read| {
                const base = self.get(read.base);
                if (base != .tagged or base.tagged.index != read.index) return error.InvalidData;
                return (base.tagged.payload orelse return error.InvalidData).*;
            },
            .range_init => |range| {
                const data = try self.allocator.alloc(Data, 3);
                data[0] = self.get(range.start);
                data[1] = self.get(range.end);
                data[2] = .{ .scalar = .{ .boolean = range.inclusive } };
                return .{ .aggregate = data };
            },
            .range_start, .range_end, .range_inclusive => |id| {
                const base = self.get(id);
                if (base != .aggregate or base.aggregate.len != 3) return error.InvalidData;
                return base.aggregate[
                    switch (instruction.op) {
                        .range_start => @as(usize, 0),
                        .range_end => 1,
                        else => 2,
                    }
                ];
            },
            .array_index => |read| {
                const base = self.get(read.array);
                const raw_index = try integer(try self.scalar(read.index));
                if (base == .aggregate) {
                    const index = try normalizeBound(raw_index, base.aggregate.len);
                    if (index == base.aggregate.len) return error.InvalidData;
                    return base.aggregate[index];
                }
                if (base != .scalar or base.scalar != .string) return error.InvalidData;
                const len = std.unicode.utf8CountCodepoints(base.scalar.string) catch return error.InvalidData;
                const index = try normalizeBound(raw_index, len);
                if (index == len) return error.InvalidData;
                var iterator = std.unicode.Utf8View.initUnchecked(base.scalar.string).iterator();
                var offset: i512 = 0;
                while (iterator.nextCodepoint()) |codepoint| : (offset += 1) {
                    if (offset == index) return .{ .scalar = .{ .rune = codepoint } };
                }
                return error.InvalidData;
            },
            .array_slice => |slice| {
                const base = self.get(slice.base);
                if (base != .aggregate) return error.InvalidData;
                const len = base.aggregate.len;
                const first = if (slice.start) |id| try self.arraySliceBound(id, len) else 0;
                const end = if (slice.end) |id| try self.arraySliceBound(id, len) else len;
                const inclusive = slice.end != null and slice.inclusive;
                if (inclusive and end == len) return error.InvalidData;
                const last = end + @intFromBool(inclusive);
                if (first > last) return error.InvalidData;
                // Data is immutable arena-owned evaluation state. Executable IR
                // retains the slice operation, which recursively copies storage.
                return .{ .aggregate = try self.allocator.dupe(Data, base.aggregate[first..last]) };
            },
            .string_slice => |slice| blk: {
                const base = try self.scalar(slice.base);
                if (base != .string) return error.InvalidData;
                const len = std.unicode.utf8CountCodepoints(base.string) catch return error.InvalidData;
                const start = if (slice.start) |id| try normalizeBound(try integer(try self.scalar(id)), len) else 0;
                const end = if (slice.end) |id| try normalizeBound(try integer(try self.scalar(id)), len) else len;
                const inclusive = slice.end != null and slice.inclusive;
                if (inclusive and end == len) return error.InvalidData;
                const exclusive_end = end + @intFromBool(inclusive);
                if (exclusive_end < start) return error.InvalidData;
                var iterator = std.unicode.Utf8View.initUnchecked(base.string).iterator();
                var offset: i512 = 0;
                var first: usize = 0;
                var last: usize = 0;
                while (offset <= exclusive_end) : (offset += 1) {
                    if (offset == start) first = iterator.i;
                    if (offset == exclusive_end) {
                        last = iterator.i;
                        break;
                    }
                    _ = iterator.nextCodepoint() orelse return error.InvalidData;
                }
                break :blk .{ .string = base.string[first..last] };
            },
            .array_len => |id| blk: {
                const base = self.get(id);
                const operand_type = self.ir.getType(self.instructions[@backingInt(id)].ty).?;
                if (operand_type == .range) {
                    if (base != .aggregate or base.aggregate.len != 3) return error.InvalidData;
                    const start = base.aggregate[0];
                    const end = base.aggregate[1];
                    const inclusive = base.aggregate[2];
                    if (start != .scalar or end != .scalar or inclusive != .scalar or inclusive.scalar != .boolean) return error.InvalidData;
                    const first = try integer(start.scalar);
                    const last = try integer(end.scalar);
                    const count: i512 = if (last < first) 0 else last - first + @intFromBool(inclusive.scalar.boolean);
                    break :blk try integerValue(self.ir.getType(instruction.ty).?.integer, count);
                }
                const len = if (base == .aggregate) base.aggregate.len else if (base == .scalar and base.scalar == .string)
                    std.unicode.utf8CountCodepoints(base.scalar.string) catch return error.InvalidData
                else
                    return error.InvalidData;
                break :blk try integerValue(self.ir.getType(instruction.ty).?.integer, len);
            },
            .unary => |unary| blk: {
                const operand = try self.scalar(unary.operand);
                if (unary.op == .bool_not) {
                    if (operand != .boolean) return error.InvalidData;
                    break :blk .{ .boolean = !operand.boolean };
                }
                if (operand == .float_bits and unary.op == .negate) {
                    const bits = self.ir.getType(instruction.ty).?.float;
                    break :blk .{ .float_bits = operand.float_bits ^ (@as(u64, 1) << @as(u6, @intCast(bits - 1))) };
                }
                const n = try integer(operand);
                const ty = self.ir.getType(instruction.ty).?.integer;
                const result = if (unary.op == .negate) -n else if (ty.signedness == .unsigned)
                    n ^ ((@as(i512, 1) << @as(u9, @intCast(ty.bits))) - 1)
                else
                    ~n;
                break :blk try integerValue(ty, result);
            },
            .binary => |binary| blk: {
                const ty = self.instructions[@backingInt(binary.lhs)].ty;
                if (ty != self.instructions[@backingInt(binary.rhs)].ty) return error.InvalidData;
                if (binary.enum_tag) break :blk try self.evaluateBinary(instruction.ty, binary.op, try self.scalar(binary.lhs), try self.scalar(binary.rhs), self.ir.getType(ty).?);
                if (binary.op == .eq or binary.op == .ne) {
                    const equal = try self.dataEqual(ty, self.get(binary.lhs), self.get(binary.rhs));
                    break :blk .{ .boolean = if (binary.op == .eq) equal else !equal };
                }
                break :blk try self.evaluateBinary(instruction.ty, binary.op, try self.scalar(binary.lhs), try self.scalar(binary.rhs), self.ir.getType(ty).?);
            },
            .array_contains => |contains| blk: {
                const ty = self.instructions[@backingInt(contains.value)].ty;
                const array_type = self.ir.getType(self.instructions[@backingInt(contains.array)].ty).?;
                if (array_type != .array or array_type.array != ty or !self.ir.hasValueEquality(ty)) return error.InvalidData;
                const array = self.get(contains.array);
                if (array != .aggregate) return error.InvalidData;
                for (array.aggregate) |item| {
                    if (try self.dataEqual(ty, self.get(contains.value), item)) break :blk .{ .boolean = true };
                }
                break :blk .{ .boolean = false };
            },
            .extremum => |extremum| {
                var best: Data = undefined;
                if (extremum.array) {
                    if (extremum.args.len != 1) return error.InvalidData;
                    const array = self.get(extremum.args[0]);
                    if (array != .aggregate or array.aggregate.len == 0) return error.InvalidData;
                    best = array.aggregate[0];
                    for (array.aggregate[1..]) |candidate| {
                        if (extremum.comparator != null) return error.InvalidData;
                        if (try self.extremumLess(instruction.ty, if (extremum.maximum) best else candidate, if (extremum.maximum) candidate else best)) best = candidate;
                    }
                } else {
                    if (extremum.args.len < 2) return error.InvalidData;
                    best = self.get(extremum.args[0]);
                    for (extremum.args[1..]) |id| {
                        if (extremum.comparator != null) return error.InvalidData;
                        const candidate = self.get(id);
                        if (try self.extremumLess(instruction.ty, if (extremum.maximum) best else candidate, if (extremum.maximum) candidate else best)) best = candidate;
                    }
                }
                return best;
            },
            .short_circuit => |short| blk: {
                const lhs = try self.scalar(short.lhs);
                if (lhs != .boolean) return error.InvalidData;
                if (if (short.kind == .@"and") !lhs.boolean else lhs.boolean) break :blk lhs;
                const rhs = try self.scalar(short.rhs);
                if (rhs != .boolean) return error.InvalidData;
                break :blk .{ .boolean = if (short.kind == .@"and") lhs.boolean and rhs.boolean else lhs.boolean or rhs.boolean };
            },
            // An expression checker accepting an operation does not make that
            // operation legal in a data initializer. Keep this allowlist closed.
            else => return error.InvalidData,
        };
        return .{ .scalar = value };
    }

    // Compare without rebuilding the selected value: ties, NaNs and signed
    // zeros retain the first candidate's exact representation.
    fn extremumLess(self: Evaluator, id: Ir.TypeId, lhs: Data, rhs: Data) error{InvalidData}!bool {
        if (lhs != .scalar or rhs != .scalar) return error.InvalidData;
        const ty = self.ir.getType(id) orelse return error.InvalidData;
        switch (ty) {
            .integer, .rune => return try integer(lhs.scalar) < try integer(rhs.scalar),
            .float => |bits| {
                if (lhs.scalar != .float_bits or rhs.scalar != .float_bits) return error.InvalidData;
                if (bits == 32) {
                    const a: f32 = @bitCast(@as(u32, @intCast(lhs.scalar.float_bits)));
                    const b: f32 = @bitCast(@as(u32, @intCast(rhs.scalar.float_bits)));
                    return a < b;
                }
                if (bits != 64) return error.InvalidData;
                const a: f64 = @bitCast(lhs.scalar.float_bits);
                const b: f64 = @bitCast(rhs.scalar.float_bits);
                return a < b;
            },
            else => return error.InvalidData,
        }
    }

    fn dataEqual(self: Evaluator, id: Ir.TypeId, lhs: Data, rhs: Data) (std.mem.Allocator.Error || error{InvalidData})!bool {
        if (!self.ir.hasValueEquality(id)) return error.InvalidData;
        // Test before inspecting data: even identical tags or empty nominal
        // records must use their hook. Children are visited only as needed.
        if (self.ir.getTypeEquality(id)) |entry| {
            if (entry.comparator != null) return error.InvalidData;
        }
        const ty = self.ir.getType(id) orelse return error.InvalidData;
        switch (ty) {
            .array => |child| {
                if (lhs != .aggregate or rhs != .aggregate) return error.InvalidData;
                if (lhs.aggregate.len != rhs.aggregate.len) return false;
                for (lhs.aggregate, rhs.aggregate) |a, b| {
                    if (!try self.dataEqual(child, a, b)) return false;
                }
                return true;
            },
            .record => |record| {
                if (record.tagged) {
                    if (lhs != .tagged or rhs != .tagged) return error.InvalidData;
                    if (lhs.tagged.index >= record.fields.len or rhs.tagged.index >= record.fields.len) return error.InvalidData;
                    if (lhs.tagged.index != rhs.tagged.index) return false;
                    const child = record.fields[lhs.tagged.index].ty;
                    if (self.ir.getType(child).? == .void) return true;
                    return self.dataEqual(child, (lhs.tagged.payload orelse return error.InvalidData).*, (rhs.tagged.payload orelse return error.InvalidData).*);
                }
                if (lhs != .aggregate or rhs != .aggregate or lhs.aggregate.len != record.fields.len or rhs.aggregate.len != record.fields.len) return error.InvalidData;
                for (record.fields, lhs.aggregate, rhs.aggregate) |field, a, b| {
                    if (!try self.dataEqual(field.ty, a, b)) return false;
                }
                return true;
            },
            else => {},
        }
        if (lhs != .scalar or rhs != .scalar or std.meta.activeTag(lhs.scalar) != std.meta.activeTag(rhs.scalar)) return error.InvalidData;
        const a = lhs.scalar;
        const b = rhs.scalar;
        return switch (ty) {
            .void => if (a == .void) true else error.InvalidData,
            .boolean => if (a == .boolean) a.boolean == b.boolean else error.InvalidData,
            .string => if (a == .string) std.mem.eql(u8, a.string, b.string) else error.InvalidData,
            .integer, .rune, .enumeration => try integer(a) == try integer(b),
            .float => |bits| blk: {
                if (a != .float_bits) return error.InvalidData;
                // Value equality, not bit equality: NaNs differ and signed zeros agree.
                if (bits == 32) {
                    const x: f32 = @bitCast(@as(u32, @intCast(a.float_bits)));
                    const y: f32 = @bitCast(@as(u32, @intCast(b.float_bits)));
                    break :blk x == y;
                }
                const x: f64 = @bitCast(a.float_bits);
                const y: f64 = @bitCast(b.float_bits);
                break :blk x == y;
            },
            else => error.InvalidData,
        };
    }

    fn arraySliceBound(self: Evaluator, id: Ir.ValueId, len: usize) error{InvalidData}!usize {
        const value = try self.scalar(id);
        if (value != .signed_integer) return error.InvalidData;
        return normalizeBound(value.signed_integer, len);
    }

    // Use the wider evaluation integer so even the minimum native signed
    // index can be normalized without negation overflow.
    fn normalizeBound(bound: i512, len: usize) error{InvalidData}!usize {
        const normalized = if (bound < 0) bound + len else bound;
        if (normalized < 0 or normalized > len) return error.InvalidData;
        return @intCast(normalized);
    }

    fn integer(value: Ir.Constant.Value) error{InvalidData}!i512 {
        return switch (value) {
            .signed_integer => |n| n,
            .unsigned_integer => |n| n,
            .rune => |n| n,
            .enum_tag => |n| n,
            else => error.InvalidData,
        };
    }

    fn integerValue(ty: Ir.Type.Integer, n: i512) error{InvalidData}!Ir.Constant.Value {
        const bits: u9 = @intCast(ty.bits - @as(u16, if (ty.signedness == .signed) 1 else 0));
        const limit = @as(i512, 1) << bits;
        if (n >= limit or n < (if (ty.signedness == .signed) -limit else @as(i512, 0))) return error.InvalidData;
        return if (ty.signedness == .signed) .{ .signed_integer = @intCast(n) } else .{ .unsigned_integer = @intCast(n) };
    }

    fn evaluateBinary(self: Evaluator, result_type: Ir.TypeId, op: Ir.BinaryOp, lhs: Ir.Constant.Value, rhs: Ir.Constant.Value, operand_type: Ir.Type) (std.mem.Allocator.Error || error{InvalidData})!Ir.Constant.Value {
        if (lhs == .string and rhs == .string) {
            if (op == .add) return .{ .string = try std.mem.concat(self.allocator, u8, &.{ lhs.string, rhs.string }) };
            const order = std.mem.order(u8, lhs.string, rhs.string);
            return comparison(op, order == .eq, order == .lt);
        }
        if (lhs == .boolean and rhs == .boolean) return comparison(op, lhs.boolean == rhs.boolean, !lhs.boolean and rhs.boolean);
        if (lhs == .void and rhs == .void) return comparison(op, true, false);
        if (lhs == .float_bits and rhs == .float_bits) {
            const a: f64 = if (operand_type.float == 32) @as(f32, @bitCast(@as(u32, @intCast(lhs.float_bits)))) else @bitCast(lhs.float_bits);
            const b: f64 = if (operand_type.float == 32) @as(f32, @bitCast(@as(u32, @intCast(rhs.float_bits)))) else @bitCast(rhs.float_bits);
            if (self.ir.getType(result_type).? == .boolean) return .{ .boolean = switch (op) {
                .eq => a == b,
                .ne => a != b,
                .lt => a < b,
                .le => a <= b,
                .gt => a > b,
                .ge => a >= b,
                else => return error.InvalidData,
            } };
            if ((op == .div or op == .mod) and b == 0) return error.InvalidData;
            const n = switch (op) {
                .add => a + b,
                .sub => a - b,
                .mul => a * b,
                .div => a / b,
                .mod => @mod(a, b),
                else => return error.InvalidData,
            };
            if (!std.math.isFinite(n)) return error.InvalidData;
            if (operand_type.float == 32) {
                const narrow: f32 = @floatCast(n);
                if (!std.math.isFinite(narrow)) return error.InvalidData;
                return .{ .float_bits = @as(u32, @bitCast(narrow)) };
            }
            return .{ .float_bits = @bitCast(n) };
        }
        const a = try integer(lhs);
        const b = try integer(rhs);
        if (self.ir.getType(result_type).? == .boolean) return comparison(op, a == b, a < b);
        if ((op == .div or op == .mod) and b == 0) return error.InvalidData;
        const n = switch (op) {
            .add => a + b,
            .sub => a - b,
            .mul => a * b,
            .div => @divTrunc(a, b),
            .mod => @rem(a, b),
            .bit_and => a & b,
            .bit_or => a | b,
            .bit_xor => a ^ b,
            else => return error.InvalidData,
        };
        // The signed minimum divided by -1 must fail even for remainder.
        if ((op == .div or op == .mod) and b == -1) _ = try integerValue(operand_type.integer, -a);
        return integerValue(self.ir.getType(result_type).?.integer, n);
    }

    fn comparison(op: Ir.BinaryOp, equal: bool, less: bool) error{InvalidData}!Ir.Constant.Value {
        return .{ .boolean = switch (op) {
            .eq => equal,
            .ne => !equal,
            .lt => less,
            .le => less or equal,
            .gt => !less and !equal,
            .ge => !less,
            else => return error.InvalidData,
        } };
    }
};
