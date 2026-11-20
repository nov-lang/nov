const std = @import("std");
const builtin = @import("builtin");

const nov_version: std.SemanticVersion = .{ .major = 0, .minor = 0, .patch = 0 };

pub fn build(b: *std.Build) void {
    comptime {
        const current_zig = builtin.zig_version;
        const min_zig = std.SemanticVersion.parse("0.17.0") catch unreachable;
        if (current_zig.order(min_zig) == .lt) {
            @compileError(std.fmt.comptimePrint(
                \\Your zig version ({}) does not meet the minimum required version ({})
            , .{ current_zig, min_zig }));
        }
    }

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const strip = b.option(bool, "strip", "Omit debug information");
    const build_options = b.addOptions();
    build_options.addOption(std.SemanticVersion, "version", nov_version);

    const exe = makeExe(b, .{
        .target = target,
        .optimize = optimize,
        .build_options = build_options,
        .strip = strip,
    });
    b.installArtifact(exe);
    const install_lib = b.addInstallDirectory(.{
        .source_dir = b.path("lib"),
        .install_dir = .lib,
        .install_subdir = "nov",
    });
    b.getInstallStep().dependOn(&install_lib.step);

    const run_cmd = b.addRunFile(.{ .relative = .{ .base = .install_bin, .sub_path = exe.out_filename } });
    run_cmd.step.dependOn(b.getInstallStep());
    run_cmd.addPassthruArgs();
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const release = b.step("release", "Make an upstream binary release");
    release.dependOn(&install_lib.step);
    const release_targets = [_]std.Target.Query{
        .{ .cpu_arch = .aarch64, .os_tag = .macos },
        .{ .cpu_arch = .aarch64, .os_tag = .linux },
        .{ .cpu_arch = .x86_64, .os_tag = .linux },
        .{ .cpu_arch = .x86_64, .os_tag = .windows },
    };
    for (release_targets) |target_query| {
        const rel_target = b.resolveTargetQuery(target_query);
        const rel_exe = makeExe(b, .{
            .target = rel_target,
            .optimize = .ReleaseSafe,
            .build_options = build_options,
            .strip = true,
        });
        rel_exe.root_module.strip = true;
        const install = b.addInstallArtifact(rel_exe, .{});
        install.dest_sub_path = b.fmt("{s}-{s}", .{
            target_query.zigTriple(b.allocator) catch unreachable,
            rel_exe.name,
        });
        release.dependOn(&install.step);
    }

    const test_filter = b.option([]const u8, "test-filter", "Run matching unit tests");
    const tests = b.addTest(.{ .root_module = exe.root_module, .filters = if (test_filter) |filter| &.{filter} else &.{} });
    const unit_step = b.step("test-unit", "Run unit and generated-C tests");
    const run_tests = b.addRunArtifact(tests);
    unit_step.dependOn(&run_tests.step);
    // Generated-C tests compile with the pinned toolchain's `zig cc`.
    run_tests.setEnvironmentVariable("NOV_TEST_ZIG", b.graph.zig_exe);
    run_tests.setCwd(b.path("."));
    run_tests.setEnvironmentVariable("NOV_TEST_LIB_DIR", "lib");
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_tests.step);
    const cli_step = b.step("test-cli", "Run CLI integration tests");
    test_step.dependOn(cli_step);
    addCliTests(b, exe, cli_step);
    addImportCliTests(b, exe, cli_step);
    addGcTests(b, test_step);

    const fmt_step = b.step("fmt", "Format all source files");
    fmt_step.dependOn(&b.addFmt(.{ .paths = b.pathList(&.{ "build.zig", "src" }) }).step);
}

fn addGcTests(b: *std.Build, test_step: *std.Build.Step) void {
    const compile = b.addSystemCommand(&.{ b.graph.zig_exe, "cc", "-std=c11", "-Wall", "-Wextra", "-Wpedantic", "-Werror", "-O3", "-fsanitize=undefined", "-fno-sanitize-recover=all" });
    compile.addFileArg(b.path("src/runtime/gc_test.c"));
    compile.addArg("-o");
    const binary = compile.addOutputFileArg("gc-test");
    const cases = [_]struct { arg: ?[]const u8, message: []const u8 }{
        .{ .arg = null, .message = "" },
        .{ .arg = "oom", .message = "GC allocation failed" },
        .{ .arg = "double", .message = "GC roots already registered" },
        .{ .arg = "pop", .message = "GC root frames must pop in LIFO order" },
        .{ .arg = "interior", .message = "GC invalid managed pointer" },
        .{ .arg = "foreign", .message = "GC invalid managed pointer" },
        .{ .arg = "descriptor", .message = "GC invalid pointer offset" },
        .{ .arg = "alignment", .message = "GC invalid descriptor" },
        .{ .arg = "stride", .message = "GC invalid pointer offset" },
        .{ .arg = "shutdown", .message = "GC shutdown with registered roots" },
        .{ .arg = "unregister", .message = "GC globals not registered" },
    };
    for (cases) |case| {
        const run = std.Build.Step.Run.create(b, b.fmt("GC runtime: {s}", .{case.arg orelse "stress"}));
        run.addFileArg(binary);
        if (case.arg) |arg| run.addArg(arg);
        run.expectExitCode(if (case.arg == null) 0 else 1);
        run.expectStdOutEqual("");
        run.expectStdErrEqual(if (case.arg == null) "" else b.fmt("panic: {s}\n", .{case.message}));
        test_step.dependOn(&run.step);
    }
}

fn addCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const mutable_method_tests = b.step("test-cli-mutable-methods", "Run mutable method receiver CLI tests");
    addMutableMethodCliTests(b, exe, mutable_method_tests);
    test_step.dependOn(mutable_method_tests);
    const global_tests = b.step("test-cli-globals", "Run global storage and data-only initializer CLI tests");
    addGlobalCliTests(b, exe, global_tests);
    test_step.dependOn(global_tests);
    const positional_field_tests = b.step("test-cli-positional-fields", "Run positional anonymous struct field CLI tests");
    addPositionalFieldCliTests(b, exe, positional_field_tests);
    test_step.dependOn(positional_field_tests);
    const anonymous_struct_tests = b.step("test-cli-anonymous-structs", "Run breaking struct constructor and anonymous value CLI tests");
    addAnonymousStructCliTests(b, exe, anonymous_struct_tests);
    test_step.dependOn(anonymous_struct_tests);
    const export_tests = b.step("test-cli-exports", "Run export CLI and separate-TU C ABI tests");
    addExportCliTests(b, exe, export_tests);
    test_step.dependOn(export_tests);
    const extern_tests = b.step("test-cli-extern", "Run extern CLI and separate-TU C ABI tests");
    addExternCliTests(b, exe, extern_tests);
    test_step.dependOn(extern_tests);
    const test_selection_tests = b.step("test-cli-test-selection", "Run test filtering and timeout CLI tests");
    addTestSelectionCliTests(b, exe, test_selection_tests);
    test_step.dependOn(test_selection_tests);
    const language_tests = b.step("test-cli-language-tests", "Run test command and assertion language CLI tests");
    addLanguageTestCliTests(b, exe, language_tests);
    test_step.dependOn(language_tests);
    const range_type_tests = b.step("test-cli-range-types", "Run source range type, printing and standard-library conversion CLI tests");
    addArrayValueHelperCliTests(b, exe, range_type_tests, "range-types.nov", @embedFile("examples/range-types.nov"), "..u8\n0..10\n-10..=20\n1..5\nValues: [1, 2, 3, 4]\n", "PASS range_types_aliases_fields_and_callbacks\nPASS range_printing_and_nested_values\nPASS empty_descending_and_inclusive_ranges\nPASS integer_widths_and_extreme_bounds\nPASS evaluation_snapshots_queries_and_independent_outputs\n5 passed; 0 failed; 5 total\n");
    test_step.dependOn(range_type_tests);
    const trivial_tests = b.step("test-cli-trivial", "Run standard-library trivial helper CLI tests");
    addArrayValueHelperCliTests(b, exe, trivial_tests, "trivial.nov", @embedFile("examples/trivial.nov"), "Swapped: 2 1\nIdentity: 2\nConstant: 2\nFlipped: 7\n", "PASS swap_scalars\nPASS swap_aggregates_preserves_independence\nPASS identity_values_and_copies\nPASS constant_snapshots_and_shared_closures\nPASS flip_types_argument_order_and_shared_state\nPASS noop_and_unevaluated_queries\n6 passed; 0 failed; 6 total\n");
    test_step.dependOn(trivial_tests);
    const mixed_operator_tests = b.step("test-cli-mixed-operators", "Run exact mixed-signature operator overload CLI tests");
    addMixedOperatorCliTests(b, exe, mixed_operator_tests);
    test_step.dependOn(mixed_operator_tests);
    const operator_tests = b.step("test-cli-operators", "Run nominal operator overload CLI tests");
    addOperatorCliTests(b, exe, operator_tests);
    test_step.dependOn(operator_tests);
    const equality_tests = b.step("test-cli-equality", "Run structural equality and operator override CLI tests");
    addEqualityCliTests(b, exe, equality_tests);
    test_step.dependOn(equality_tests);
    const concat_tests = b.step("test-cli-array-concat", "Run array concatenation CLI tests");
    addArrayConcatCliTests(b, exe, concat_tests);
    test_step.dependOn(concat_tests);
    const clear_tests = b.step("test-cli-clear", "Run array clear CLI tests");
    addClearCliTests(b, exe, clear_tests);
    test_step.dependOn(clear_tests);
    const len_tests = b.step("test-cli-len", "Run length builtin and removed length property CLI tests");
    addLenCliTests(b, exe, len_tests);
    test_step.dependOn(len_tests);
    const range_length_tests = b.step("test-cli-range-length", "Run finite range length CLI tests");
    addRangeLengthCliTests(b, exe, range_length_tests);
    test_step.dependOn(range_length_tests);
    const string_byte_tests = b.step("test-cli-string-bytes", "Run immutable string scalar indexing CLI tests");
    addStringByteCliTests(b, exe, string_byte_tests);
    test_step.dependOn(string_byte_tests);
    const string_slice_tests = b.step("test-cli-string-slices", "Run immutable scalar string slicing CLI tests");
    addStringSliceCliTests(b, exe, string_slice_tests);
    test_step.dependOn(string_slice_tests);
    const str_tests = b.step("test-cli-str", "Run renamed str type CLI tests");
    addStrCliTests(b, exe, str_tests);
    test_step.dependOn(str_tests);
    const integer_parsing_tests = b.step("test-cli-integer-parsing", "Run strict decimal integer parsing CLI tests");
    addIntegerParsingCliTests(b, exe, integer_parsing_tests);
    test_step.dependOn(integer_parsing_tests);
    const string_search_tests = b.step("test-cli-string-search", "Run standard-library string search CLI tests");
    addStringSearchCliTests(b, exe, string_search_tests);
    test_step.dependOn(string_search_tests);
    const string_split_tests = b.step("test-cli-string-split", "Run standard-library string split CLI tests");
    addStringSplitCliTests(b, exe, string_split_tests);
    test_step.dependOn(string_split_tests);
    const string_conversion_tests = b.step("test-cli-string-conversions", "Run UTF-8 string and byte conversion CLI tests");
    addStringConversionCliTests(b, exe, string_conversion_tests);
    test_step.dependOn(string_conversion_tests);
    const string_transform_tests = b.step("test-cli-string-transform", "Run standard-library string trim, replace and repeat CLI tests");
    addStringTransformCliTests(b, exe, string_transform_tests);
    test_step.dependOn(string_transform_tests);
    const collection_helper_tests = b.step("test-cli-collection-helpers", "Run string join and array selection CLI tests");
    addCollectionHelperCliTests(b, exe, collection_helper_tests);
    test_step.dependOn(collection_helper_tests);
    const rune_tests = b.step("test-cli-runes", "Run Unicode rune CLI tests");
    addRuneCliTests(b, exe, rune_tests);
    test_step.dependOn(rune_tests);
    const iteration_tests = b.step("test-cli-iteration", "Run array and Unicode string iteration CLI tests");
    addIterationCliTests(b, exe, iteration_tests);
    test_step.dependOn(iteration_tests);
    const continuation_tests = b.step("test-cli-iteration-continuations", "Run iteration continuation CLI tests");
    addIterationContinuationCliTests(b, exe, continuation_tests);
    test_step.dependOn(continuation_tests);
    const zip_iteration_tests = b.step("test-cli-zip-iteration", "Run zip iteration CLI and strict generated-C tests");
    addZipIterationCliTests(b, exe, zip_iteration_tests);
    test_step.dependOn(zip_iteration_tests);
    const closure_tests = b.step("test-cli-closures", "Run function expression and closure CLI tests");
    addClosureCliTests(b, exe, closure_tests);
    test_step.dependOn(closure_tests);
    const inferred_closure_tests = b.step("test-cli-inferred-closures", "Run inferred closure CLI tests");
    addInferredClosureCliTests(b, exe, inferred_closure_tests);
    test_step.dependOn(inferred_closure_tests);
    const array_field_tests = b.step("test-cli-array-fields", "Run array-valued struct field CLI tests");
    addArrayFieldCliTests(b, exe, array_field_tests);
    test_step.dependOn(array_field_tests);
    const storable_array_tests = b.step("test-cli-storable-arrays", "Run storable array CLI tests");
    addStorableArrayCliTests(b, exe, storable_array_tests);
    test_step.dependOn(storable_array_tests);
    const storable_field_tests = b.step("test-cli-storable-fields", "Run finite storable struct field CLI tests");
    addStorableFieldCliTests(b, exe, storable_field_tests);
    test_step.dependOn(storable_field_tests);
    const union_array_tests = b.step("test-cli-union-arrays", "Run direct union array payload CLI tests");
    addUnionArrayCliTests(b, exe, union_array_tests);
    test_step.dependOn(union_array_tests);
    const nested_union_tests = b.step("test-cli-nested-unions", "Run finite acyclic nested union CLI tests");
    addNestedUnionCliTests(b, exe, nested_union_tests);
    test_step.dependOn(nested_union_tests);
    const value_payload_tests = b.step("test-cli-value-payloads", "Run function and range union payload CLI tests");
    addValuePayloadCliTests(b, exe, value_payload_tests);
    test_step.dependOn(value_payload_tests);
    const printing_tests = b.step("test-cli-printing", "Run type and aggregate printing CLI tests");
    addPrintingCliTests(b, exe, printing_tests);
    test_step.dependOn(printing_tests);
    const array_helper_tests = b.step("test-cli-array-helpers", "Run standard-library array helper CLI tests");
    addArrayHelperCliTests(b, exe, array_helper_tests);
    test_step.dependOn(array_helper_tests);
    const array_flat_map_tests = b.step("test-cli-array-flat-map", "Run standard-library array flatMap CLI tests");
    addArrayValueHelperCliTests(b, exe, array_flat_map_tests, "array-flat-map.nov", @embedFile("examples/array-flat-map.nov"), "Expanded: [1, 1, 2, 2, 3, 3]\n", "PASS order_empty_results_and_callback_count\nPASS flatten_one_layer_and_recursive_independence\nPASS storable_outputs_and_shared_closures\nPASS snapshots_inference_and_queries\n4 passed; 0 failed; 4 total\n");
    test_step.dependOn(array_flat_map_tests);
    const array_reverse_tests = b.step("test-cli-array-reverse", "Run standard-library array reverse CLI tests");
    addArrayValueHelperCliTests(b, exe, array_reverse_tests, "array-reverse.nov", @embedFile("examples/array-reverse.nov"), "Reversed: [3, 2, 1]\nOriginal: [1, 2, 3]\n", "PASS order_empty_and_singleton\nPASS recursive_storage_independence\nPASS storable_types_and_shared_closure_cells\nPASS inferred_callback_and_unevaluated_query\n4 passed; 0 failed; 4 total\n");
    test_step.dependOn(array_reverse_tests);
    const array_sort_tests = b.step("test-cli-array-sort", "Run standard-library stable array sort CLI tests");
    addArraySortCliTests(b, exe, array_sort_tests);
    test_step.dependOn(array_sort_tests);
    const array_default_tests = b.step("test-cli-array-defaults", "Run array-valued struct default CLI tests");
    addArrayDefaultCliTests(b, exe, array_default_tests);
    test_step.dependOn(array_default_tests);
    const last_use_tests = b.step("test-cli-last-use", "Run last-use value independence CLI tests");
    addLastUseCliTests(b, exe, last_use_tests);
    test_step.dependOn(last_use_tests);
    const array_search_tests = b.step("test-cli-array-search", "Run standard-library array search and traversal CLI tests");
    addArraySearchCliTests(b, exe, array_search_tests);
    test_step.dependOn(array_search_tests);
    const container_map_tests = b.step("test-cli-container-map", "Run standard-library container mapping CLI tests");
    addContainerMapCliTests(b, exe, container_map_tests);
    test_step.dependOn(container_map_tests);
    const constructor_shorthand_tests = b.step("test-cli-constructor-shorthand", "Run constructor field shorthand CLI tests");
    addConstructorShorthandCliTests(b, exe, constructor_shorthand_tests);
    test_step.dependOn(constructor_shorthand_tests);
    addFieldMutationCliTests(b, exe, test_step, fixtures);
    addGenericStructCliTests(b, exe, test_step, fixtures);
    const quoted_interpolation_tests = b.step("test-cli-quoted-interpolation", "Run quoted interpolation CLI tests");
    addQuotedInterpolationCliTests(b, exe, quoted_interpolation_tests);
    test_step.dependOn(quoted_interpolation_tests);
    const float_tests = b.step("test-cli-floats", "Run floating-point CLI tests");
    addFloatCliTests(b, exe, float_tests);
    test_step.dependOn(float_tests);
    const type_of_tests = b.step("test-cli-type-of", "Run @TypeOf CLI tests");
    addTypeOfCliTests(b, exe, type_of_tests);
    test_step.dependOn(type_of_tests);
    const type_of_expression_tests = b.step("test-cli-type-of-expressions", "Run unevaluated @TypeOf expression CLI tests");
    addTypeOfExpressionCliTests(b, exe, type_of_expression_tests);
    test_step.dependOn(type_of_expression_tests);
    const integer_default_tests = b.step("test-cli-integer-defaults", "Run integer default inference CLI tests");
    addIntegerDefaultCliTests(b, exe, integer_default_tests);
    test_step.dependOn(integer_default_tests);
    const generic_function_tests = b.step("test-cli-generic-functions", "Run direct generic function CLI tests");
    addGenericFunctionCliTests(b, exe, generic_function_tests);
    test_step.dependOn(generic_function_tests);
    const generic_closure_tests = b.step("test-cli-generic-closures", "Run generic closure and local template CLI tests");
    addGenericClosureCliTests(b, exe, generic_closure_tests);
    test_step.dependOn(generic_closure_tests);
    const generic_method_tests = b.step("test-cli-generic-methods", "Run inferred generic instance method CLI tests");
    addGenericMethodCliTests(b, exe, generic_method_tests);
    test_step.dependOn(generic_method_tests);
    addRangeValueCliTests(b, exe, test_step, fixtures);
    const reference_tests = b.step("test-cli-references", "Run explicit local reference CLI tests");
    addReferenceCliTests(b, exe, reference_tests);
    const reference_rebinding_tests = b.step("test-cli-reference-rebinding", "Run CFG reference rebinding CLI tests");
    addReferenceRebindingCliTests(b, exe, reference_rebinding_tests);
    const reference_parameter_tests = b.step("test-cli-reference-parameters", "Run safe reference parameter CLI tests");
    addReferenceParameterCliTests(b, exe, reference_parameter_tests);
    test_step.dependOn(reference_parameter_tests);
    test_step.dependOn(reference_rebinding_tests);
    addBorrowCliTests(b, exe, reference_tests, fixtures);
    test_step.dependOn(reference_tests);
    _ = fixtures.add("library-result-source.nov", @embedFile("lib/result.nov"));
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    _ = fixtures.add("library-result.nov",
        \\let standard = @import("std")
        \\let Result = standard.Result
        \\
        \\let R = Result(uint, str)
        \\let eager: () -> uint = { @print("E")
        \\    9 }
        \\let recover: (error: str) -> uint = { @print("F")
        \\    7 }
        \\let text: (error: uint) -> str = "fallback:{error}"
        \\let unit: () -> Result(void, str) = .ok[]
        \\let fromUnit: () -> R = { unit().!
        \\    .ok[3] }
        \\let main: () = {
        \\    let good: R = .ok[1]
        \\    let bad: R = .err["missing"]
        \\    @println(good.unwrapOr(eager()), bad.unwrapOr(9), good.unwrapOrElse(recover), bad.unwrapOrElse(recover))
        \\    @println(Result(str, uint).err[2].unwrapOrElse(text))
        \\    @println(fromUnit().unwrapOr(0), match Result(uint, void).err[] {
        \\        .ok => |value| value
        \\        .err => @as(uint, 4)
        \\    })
        \\}
        \\
    );
    _ = fixtures.add("valid.nov", "let answer = 1 + 2 * 3\n");
    _ = fixtures.add("another-valid.nov", "let text = \"hello\"\n");
    _ = fixtures.add("no-final-newline.nov", "let answer = 42");
    _ = fixtures.add("syntax-only.nov", "let name = absent\nlet name = 42\n");
    _ = fixtures.add("invalid.nov", "let broken =");
    _ = fixtures.add("unsupported-loop.nov", "let value = loop {} else {}\n");
    _ = fixtures.add("arrays.nov", "let values = [[\n], [1,], [1, 2, 3,\n],]");
    _ = fixtures.add("unclosed-array.nov", "let values = [1");
    _ = fixtures.add("needs-format.nov", "let answer=1+2*3\n");
    _ = fixtures.add("let-only.nov", "let number: i32 = 42\n");
    _ = fixtures.add("print-program.nov", "let number: i32 = -42\nlet answer: () -> i32 = number\nlet main: () -> void = @println(true, answer())\n");
    _ = fixtures.add("checkout.nov", @embedFile("examples/checkout.nov"));
    _ = fixtures.add("fibo.nov", @embedFile("examples/fibo.nov"));
    _ = fixtures.add("fizzbuzz.nov", @embedFile("examples/fizzbuzz.nov"));
    _ = fixtures.add("range-search.nov", @embedFile("examples/range-search.nov"));
    _ = fixtures.add("bitwise.nov", "let main: () -> void = {let n: i8 = -128\nlet m: u64 = 18446744073709551615\n@println(n | 127, n & 127, n ^ -1, m ^ m)}\n");
    _ = fixtures.add("strings.nov", @embedFile("examples/strings.nov"));
    _ = fixtures.add("interpolation.nov", @embedFile("examples/interpolation.nov"));
    _ = fixtures.add("type-aliases.nov", @embedFile("examples/type-aliases.nov"));
    _ = fixtures.add("type.nov", @embedFile("examples/type.nov"));
    _ = fixtures.add("enums.nov", "let Kind=enum{not_found,permission_denied,io,\n@[stringify]\nlet toString:(k:Kind)->str=match k{.not_found=>\"not_found\"\n.permission_denied | .io=>\"other\"}\n}\nlet Result:(T:type,E:type)->type=union{ok:T,err:E,}\nlet R=Result(uint,Kind)\nlet Error=struct{kind:Kind=.not_found,message:str,}\nlet fail:()->R=.err[.not_found]\nlet pass:()->R={let n=fail().!\n.ok[n]}\nlet main:()={let e=Error.[message=\"not \"+\"found\"]\n@println(e.kind,e.message,Kind.io!=.not_found,match pass(){.ok=>|n|\"{n}\"\n.err=>|k|\"{k}\"})}");
    _ = fixtures.add("instance-methods.nov", "let K=enum{x,y,\nlet eql:(a:K,b:K)->bool=a==b\n}\nlet Alias=R\nlet R=struct{s:str,\nlet text:(r:Alias)->str=r.s\n}\nlet main:()={let k=K.x\n@println(Alias.[s=\"hel\"+\"lo\"].text(),k.eql(.x),K.y.eql(k))}");
    _ = fixtures.add("instance-signature.nov", "let R=struct{let bad:()->bool=true\n}\nlet main:()=R.[].bad()");
    _ = fixtures.add("enum-missing.nov", "let K=enum{x,y,}\nlet f:(k:K)->bool=match k{.x=>true}");
    _ = fixtures.add("enum-numbered.nov", "let K=enum{x=7,y,}");
    _ = fixtures.add("enum-backing.nov", "let K=enum(u8){x,}");
    _ = fixtures.add("unions.nov", "let Alias=U\nlet U=union{ok:int,err:str,none,}\nlet describe:(u:Alias)->str=match u{.ok=>|v|\"ok:{v}\"\n.err=>|s|s\n.none=>\"none\"}\nlet main:()={@println(describe(U.ok[7]),describe(.err[\"a\"+\"b\"]),describe(.none[]))}");
    _ = fixtures.add("union-missing.nov", "let U=union{ok:int,none,}\nlet f:(u:U)->int=match u{.ok=>|n|n}");
    _ = fixtures.add("union-duplicate.nov", "let U=union{ok:int,none,}\nlet f:(u:U)->int=match u{.ok=>0\n.ok=>1\n_=>2}");
    _ = fixtures.add("union-capture.nov", "let U=union{ok:int,none,}\nlet f:(u:U)->int=match u{.ok=>|n|n\n_=>n}");
    _ = fixtures.add("generic-unions.nov", "let G:(T:type,E:type)->type=union{err:E,ok:T,}\nlet R=G(int,str)\nlet Text=G(str,str)\nlet make:(b:bool)->R={@print(\"M\")\nif b{.ok[7]}else{.err[\"b\"+\"ad\"]}}\nlet wrap:(b:bool)->Text={let n=make(b).!\n.ok[\"{n}\"]}\nlet show:(r:Text)->str=match r{.ok=>|s|s\n.err=>|s|s}\nlet main:()=@println(show(wrap(true)),show(wrap(false)))");
    _ = fixtures.add("record-payloads.nov", "let G:(T:type,Failure:type)->type=union{ok:T,err:Failure,}\nlet U=union{ok:R,err:E,}\nlet R=struct{s:str,}\nlet E=struct{message:str,}\nlet make:(b:bool)->G(R,E)=if b{.ok[R.[s=\"o\"+\"k\"]]}else{.err[E.[message=\"e\"+\"rr\"]]}\nlet wrap:(b:bool)->G(bool,E)={let r=make(b).!\n.ok[r.s == \"ok\"]}\nlet show:(r:G(bool,E))->str=match r{.ok=>|b|\"{b}\"\n.err=>|e|e.message}\nlet main:()=@println(show(wrap(true)),show(wrap(false)),match U.ok[R.[s=\"record\"]]{.ok=>|r|r.s\n.err=>|e|e.message})");
    _ = fixtures.add("record-interpolation.nov", "let G:(T:type,E:type)->type=union{ok:T,err:E,}\nlet Error=struct{s:str,\n@[stringify]\nlet toString:(err:Error)->str={@print(\"C\")\nerr.s+\"!\"}\n}\nlet fail:()->G(int,Error)=.err[Error.[s=\"not \"+\"found\"]]\nlet main:()=match fail(){.ok=>@panic(\"bad\")\n.err=>|err|@panic(\"Error: {err}\")}");
    _ = fixtures.add("record-interpolation-missing.nov", "let R=struct{s:str,}\nlet f:(r:R)->str=\"{r}\"");
    _ = fixtures.add("wide-record-payload.nov", "let U=union{ok:R,}\nlet R=struct{wide:i128,}\nlet f:(u:U)={}\nlet main:()={}");
    _ = fixtures.add("generic-cycle.nov", "let G:(T:type)->type=union{ok:G(T),}\nlet R=G(int)");
    _ = fixtures.add("generic-methods.nov", "let Result:(T:type,E:type)->type=union{ok:T,err:E,\nlet Self=@This()\nlet unwrapOr:(self:Self,fallback:T)->T=match self{.ok=>|v|v\n.err=>fallback}\nlet unwrapOrElse:(self:Self,fallback:(E)->T)->T=match self{.ok=>|v|v\n.err=>|e|fallback(e)}\n}\nlet R=Result(int,str)\nlet cb:(e:str)->int={@print(e)\n7}\nlet main:()={let r:R=.err[\"E\"]\n@println(R.unwrapOr(r,3),r.unwrapOrElse(cb))}");
    _ = fixtures.add("generic-method.nov", "let G:(T:type)->type=union{ok:T,\nlet m=1\n}");
    _ = fixtures.add("propagation-error.nov", "let G:(T:type,E:type)->type=union{ok:T,err:E,}\nlet f:(r:G(int,str))->G(int,bool)=.ok[r.!]");
    _ = fixtures.add("wide-generic.nov", "let G:(T:type)->type=union{ok:T,err:str,}\nlet f:()->G(i128)=.ok[170141183460469231731687303715884105727]\nlet main:()={}");
    _ = fixtures.add("wide-union.nov", "let U=union{ok:i128,none,}\nlet f:(u:U)={}\nlet main:()={}");
    _ = fixtures.add("wide-record.nov", "let R=struct{x:i128,}\nlet f:(r:R)= {}\nlet main:()={}\n");
    _ = fixtures.add("wide-record-alias.nov", "let Wide=u128\nlet Alias=R\nlet R=struct{x:Wide,}\nlet f:(r:Alias)= {}\nlet main:()={}\n");
    _ = fixtures.add("unused-wide-record.nov", "let R=struct{x:i128,}\nlet main:()={}\n");
    _ = fixtures.add("wide-record-bounds.nov", "let R=struct{x:i128=170141183460469231731687303715884105728,}\n");
    _ = fixtures.add("wide-record-array.nov", "let R=struct{x:[][]u128,}\nlet f:(r:R)={}\nlet main:()={}\n");
    _ = fixtures.add("boolean-match.nov",
        \\let Flag = bool
        \\let choose: (b: Flag) -> int = match b {
        \\    true => 1
        \\    false => 2
        \\}
        \\let main: () -> void = {
        \\    @println(
        \\        choose(true),
        \\        choose(false),
        \\        @as(
        \\            int,
        \\            match false {
        \\                true | false => 3
        \\            }
        \\        )
        \\    )
        \\}
        \\
    );
    _ = fixtures.add("annotated-globals.nov",
        \\let inferred_bool = @as(Flag, true)
        \\let annotated_bool: Flag = @as(bool, @as(Flag, true))
        \\let inferred_int = @as(Count, -42)
        \\let annotated_int: Count = @as(int, @as(Count, -42))
        \\let inferred_string = @as(Text, "hé\0")
        \\let annotated_string: Text = @as(str, @as(Text, "hé\0"))
        \\let Flag = bool
        \\let Count = int
        \\let Text = str
        \\let main: () -> void = {
        \\    @println(inferred_bool, annotated_bool, inferred_int, annotated_int)
        \\    @println("{inferred_string}:{annotated_string}")
        \\}
        \\
    );
    _ = fixtures.add("annotated-global-mismatch.nov", "let n: uint = @as(int, 1)\n");
    _ = fixtures.add("annotated-global-nesting.nov", "let n: uint = @as(uint, @as(int, 1))\n");
    _ = fixtures.add("annotated-global-bounds.nov", "let n: u8 = @as(u8, 256)\n");
    _ = fixtures.add("annotated-global-interpolation.nov", "let s: str = @as(str, \"{true}\")\n");
    _ = fixtures.add("global-untyped.nov", "let n = 1\n");
    _ = fixtures.add("global-bounds.nov", "let n = @as(u8, 256)\n");
    _ = fixtures.add("global-call.nov", "let n = @as(int, missing())\n");
    _ = fixtures.add("global-unknown.nov", "let n = @as(int, missing)\n");
    _ = fixtures.add("global-type.nov", "let n = @as(Missing, 1)\n");
    _ = fixtures.add("global-nesting.nov", "let n = @as(int, @as(uint, 1))\n");
    _ = fixtures.add("global-block.nov", "let n = @as(int, {1})\n");
    _ = fixtures.add("global-interpolation.nov", "let n = @as(str, \"{true}\")\n");
    _ = fixtures.add("alias-value.nov", "let Count = uint\nlet f: (n: uint) = {}\nlet main: () = f(Count)\n");
    _ = fixtures.add("alias-cycle.nov", "let A = []B\nlet B = A\n");
    _ = fixtures.add("ascription.nov", "let main: () -> void = {\n    @println(\"{@as(int, 1)}\", @as(uint, 1))\n    let a = @as([]int, [2])\n    @println(a[0])\n}\n");
    _ = fixtures.add("interpolation-format.nov", "let main: () -> void = {let n: int = 1\n@println(\"{n:02}\")}\n");
    _ = fixtures.add("interpolation-name.nov", "let main: () -> void = @println(\"{missing}\")\n");
    _ = fixtures.add("interpolation-global.nov", "let n: int = 1\nlet text: str = \"{n}\"\n");
    _ = fixtures.add("inferred-locals.nov", @embedFile("examples/inferred-locals.nov"));
    _ = fixtures.add("inferred-integer.nov", "let main: () -> void = {let n = 1\n@println(n)}\n");
    _ = fixtures.add("inferred-empty-array.nov", "let main: () -> void = {let a = []\n}\n");
    _ = fixtures.add("scalar-arrays.nov", @embedFile("examples/arrays.nov"));
    _ = fixtures.add("membership-default.nov", "let main: () -> void = @println(1 in [1, 2])\n");
    _ = fixtures.add("membership-width.nov", "let f: (n: i16, a: []i8) -> bool = n in a\n");
    _ = fixtures.add("membership-string.nov", "let main: () -> void = @println(\"a\" in \"abc\")\n");
    _ = fixtures.add("array-bounds.nov", "let main: () -> void = {let a: []bool = [true]\n@println(a[-2])}\n");
    _ = fixtures.add("array-compound.nov", "let main: () -> void = {\n    let mut a: []i8 = [127, 10]\n    a[0] += 1\n    a[0] -= 1\n    a[0] *= 2\n    a[0] /= -1\n    a[0] %= 3\n    a[-1] += {\n        a = []\n        5\n    }\n    @println(a[0], a[-1], @len(a))\n}\n");
    _ = fixtures.add("array-bool-compound.nov", "let main: () -> void = {let mut a: []bool = [true]\na[0] += false}\n");
    _ = fixtures.add("array-compound-bounds.nov", "let main: () -> void = {let mut a: []int = []\na[-1] += {@print(true)\nreturn}}\n");
    _ = fixtures.add("array-length-write.nov", "let main: () -> void = {let mut a: []int = []\na.len = 1}\n");
    _ = fixtures.add("if-program.nov", "let divide: (n: int) -> int = 1 / n\nlet main: () -> void = {\nlet n: int = if false {divide(0)} else if true {42} else {divide(0)}\nif n == 42 {@println(n)}\n}\n");
    _ = fixtures.add("return-program.nov", "let f: (c: bool) -> int = {if c {{return 42}}\n1 / 0}\nlet main: () -> void = {let b: bool = false and {return}\n@println(f(true), b)\nreturn}\n");
    _ = fixtures.add("mutable-program.nov", "let f: (c: bool) -> int = {let mut n: int = 1\nn += if c {return 42} else {2}\nn}\nlet main: () -> void = {let mut n: int = 1\nif true {n += 2}\nlet b: bool = false and {n = 99\ntrue}\n@println(n, b, f(true), f(false))}\n");
    _ = fixtures.add("loop-values.nov", @embedFile("examples/loop-values.nov"));
    _ = fixtures.add("inclusive-range.nov",
        \\let main: () = {
        \\    loop i in 0..=2 {
        \\        @println(i)
        \\    }
        \\    let max: u64 = 18446744073709551615
        \\    loop i in max..=max {
        \\        @println(i)
        \\        continue
        \\    }
        \\    loop _ in 2..=1 {
        \\        @println(false)
        \\    }
        \\}
        \\
    );
    _ = fixtures.add("loop-program.nov", "let f: () -> int = {let mut n: int = 0\nloop {if n == 5 {break}\nn += 1\nloop false {continue}\nif n == 2 {continue}\nif n == 4 {break}\n@println(n)}\nn}\nlet main: () -> void = @println(f())\n");
    _ = fixtures.add("else-break-outside.nov", "let main: () -> void = loop false {} else {break}\n");
    _ = fixtures.add("else-mismatch.nov", "let main: () -> void = {let x: bool = loop true {break true} else {1}\n}\n");
    _ = fixtures.add("break-outside.nov", "let main: () -> void = {break}\n");
    _ = fixtures.add("value-break.nov", "let main: () -> void = loop true {break 1}\n");
    _ = fixtures.add("immutable-assignment.nov", "let main: () -> void = {let n: int = 1\nn = 2}\n");
    _ = fixtures.add("return-invalid.nov", "let main: () -> void = return true\n");
    _ = fixtures.add("return-unreachable.nov", "let main: () -> void = {return @println(true)\n@println(false)}\n");
    _ = fixtures.add("if-invalid.nov", "let main: () -> void = if true {} else {@println(missing)}\n");
    _ = fixtures.add("source-panic.nov", "let message: () -> str = {@print(\"before\")\n\"hé\\0\" + \"!\"}\n@[noreturn]\nlet stop: (s: str) -> void = @panic(s)\nlet main: () = stop(\"{message()}\")\n");
    _ = fixtures.add("panic-program.nov", "let zero: i32 = 0\nlet divide: () -> i32 = 1 / zero\nlet main: () -> void = @println(divide())\n");

    const Case = struct {
        name: []const u8,
        args: []const []const u8,
        exit_code: u8,
        stdout: []const u8 = "",
        stderr_match: ?[]const u8 = null,
        lib_dir: bool = false,
    };
    const cases = [_]Case{
        .{ .name = "enum semantic", .args = &.{ "check", "enums.nov", "--semantic" }, .exit_code = 0 },
        .{ .name = "enum missing variant", .args = &.{ "check", "enum-missing.nov", "--semantic" }, .exit_code = 1, .stderr_match = "match must cover every variant" },
        .{ .name = "enum numbered unsupported", .args = &.{ "check", "enum-numbered.nov", "--semantic" }, .exit_code = 1, .stderr_match = "explicit enum values" },
        .{ .name = "enum backing unsupported", .args = &.{ "check", "enum-backing.nov", "--semantic" }, .exit_code = 1, .stderr_match = "explicit enum backing types are unsupported" },
        .{ .name = "record interpolation semantic", .args = &.{ "check", "record-interpolation.nov", "--semantic" }, .exit_code = 0 },
        .{ .name = "record interpolation default formatting", .args = &.{ "check", "record-interpolation-missing.nov", "--semantic" }, .exit_code = 0 },
        .{ .name = "record payload semantic", .args = &.{ "check", "record-payloads.nov", "--semantic" }, .exit_code = 0 },
        .{ .name = "record payload 128 bit semantic", .args = &.{ "check", "wide-record-payload.nov", "--semantic" }, .exit_code = 0 },
        .{ .name = "record payload backend width", .args = &.{ "emit-c", "wide-record-payload.nov" }, .exit_code = 1, .stderr_match = "C backend only supports 8, 16, 32 and 64-bit integers" },
        .{ .name = "generic union semantic", .args = &.{ "check", "generic-unions.nov", "--semantic" }, .exit_code = 0 },
        .{ .name = "library Result semantic", .args = &.{ "check", "library-result.nov", "--semantic" }, .exit_code = 0, .lib_dir = true },
        .{ .name = "library Result format", .args = &.{ "fmt", "--check", "library-result-source.nov" }, .exit_code = 0 },
        .{ .name = "generic methods semantic", .args = &.{ "check", "generic-methods.nov", "--semantic" }, .exit_code = 0 },
        .{ .name = "generic cycle", .args = &.{ "check", "generic-cycle.nov", "--semantic" }, .exit_code = 1, .stderr_match = "cyclic generic container instantiation" },
        .{ .name = "generic method diagnostic", .args = &.{ "check", "generic-method.nov", "--semantic" }, .exit_code = 1, .stderr_match = "generic container members require an ordinary function signature" },
        .{ .name = "propagation exact error", .args = &.{ "check", "propagation-error.nov", "--semantic" }, .exit_code = 1, .stderr_match = ".! error payload must exactly match" },
        .{ .name = "generic 128 bit semantic", .args = &.{ "check", "wide-generic.nov", "--semantic" }, .exit_code = 0 },
        .{ .name = "generic backend width", .args = &.{ "emit-c", "wide-generic.nov" }, .exit_code = 1, .stderr_match = "C backend only supports 8, 16, 32 and 64-bit integers" },
        .{ .name = "union semantic", .args = &.{ "check", "unions.nov", "--semantic" }, .exit_code = 0 },
        .{ .name = "union incomplete", .args = &.{ "check", "union-missing.nov", "--semantic" }, .exit_code = 1, .stderr_match = "union match must cover every variant" },
        .{ .name = "union duplicate", .args = &.{ "check", "union-duplicate.nov", "--semantic" }, .exit_code = 1, .stderr_match = "union match prong is already covered" },
        .{ .name = "union private capture", .args = &.{ "check", "union-capture.nov", "--semantic" }, .exit_code = 1, .stderr_match = "unknown name 'n'" },
        .{ .name = "union backend width", .args = &.{ "emit-c", "wide-union.nov" }, .exit_code = 1, .stderr_match = "C backend only supports 8, 16, 32 and 64-bit integers" },
        .{ .name = "syntax check rejects lib-dir", .args = &.{ "check", "valid.nov", "--lib-dir", "library" }, .exit_code = 1, .stderr_match = "--lib-dir requires semantic check" },
        .{ .name = "fmt rejects lib-dir", .args = &.{ "fmt", "valid.nov", "--lib-dir", "library" }, .exit_code = 1, .stderr_match = "--lib-dir requires semantic check" },
        .{ .name = "semantic check rejects empty lib-dir", .args = &.{ "check", "--semantic", "valid.nov", "--lib-dir", "" }, .exit_code = 1, .stderr_match = "--lib-dir requires a nonempty path" },
        .{ .name = "help", .args = &.{"--help"}, .exit_code = 0, .stderr_match = "Usage: nov" },
        .{ .name = "version", .args = &.{"--version"}, .exit_code = 0, .stdout = b.fmt("nov {f}\n", .{nov_version}) },
        .{ .name = "no arguments", .args = &.{}, .exit_code = 1, .stderr_match = "Usage: nov" },
        .{ .name = "unknown command", .args = &.{"unknown-command"}, .exit_code = 1, .stderr_match = "unknown command" },
        .{ .name = "implicit build missing source", .args = &.{"missing.nov"}, .exit_code = 1, .stderr_match = "missing.nov: error: FileNotFound" },
        .{ .name = "implicit build invalid source", .args = &.{"invalid.nov"}, .exit_code = 1, .stderr_match = "invalid.nov:1:13: error:" },
        .{ .name = "implicit build multiple files", .args = &.{ "valid.nov", "another-valid.nov" }, .exit_code = 1, .stderr_match = "exactly one file" },
        .{ .name = "implicit build rejects semantic", .args = &.{ "valid.nov", "--semantic" }, .exit_code = 1, .stderr_match = "--semantic requires" },
        .{ .name = "implicit build rejects check", .args = &.{ "valid.nov", "--check" }, .exit_code = 1, .stderr_match = "--check requires" },
        .{ .name = "build conflicting modes", .args = &.{ "build", "valid.nov", "--debug", "--release" }, .exit_code = 1, .stderr_match = "mutually exclusive" },
        .{ .name = "run conflicting modes", .args = &.{ "run", "valid.nov", "--release", "--debug" }, .exit_code = 1, .stderr_match = "mutually exclusive" },
        .{ .name = "implicit build conflicting modes", .args = &.{ "valid.nov", "--debug", "--release" }, .exit_code = 1, .stderr_match = "mutually exclusive" },
        .{ .name = "check without files", .args = &.{"check"}, .exit_code = 1, .stderr_match = "error:" },
        .{ .name = "fmt without files", .args = &.{"fmt"}, .exit_code = 1, .stderr_match = "error:" },
        .{ .name = "build without files", .args = &.{"build"}, .exit_code = 1, .stderr_match = "exactly one file" },
        .{ .name = "run without files", .args = &.{"run"}, .exit_code = 1, .stderr_match = "exactly one file" },
        .{ .name = "build multiple files", .args = &.{ "build", "valid.nov", "another-valid.nov" }, .exit_code = 1, .stderr_match = "exactly one file" },
        .{ .name = "run arguments deferred", .args = &.{ "run", "valid.nov", "argument" }, .exit_code = 1, .stderr_match = "program arguments are not supported" },
        .{ .name = "run rejects output", .args = &.{ "run", "valid.nov", "-o", "out" }, .exit_code = 1, .stderr_match = "--output requires the build command" },
        .{ .name = "emit-c rejects cc", .args = &.{ "emit-c", "valid.nov", "--cc", "cc" }, .exit_code = 1, .stderr_match = "--cc requires" },
        .{ .name = "build rejects semantic", .args = &.{ "build", "valid.nov", "--semantic" }, .exit_code = 1, .stderr_match = "--semantic requires" },
        .{ .name = "build rejects check", .args = &.{ "build", "valid.nov", "--check" }, .exit_code = 1, .stderr_match = "--check requires" },
        .{ .name = "build empty cc", .args = &.{ "build", "valid.nov", "--cc", "" }, .exit_code = 1, .stderr_match = "nonempty executable" },
        .{ .name = "build empty output", .args = &.{ "build", "valid.nov", "-o", "" }, .exit_code = 1, .stderr_match = "nonempty path" },
        .{ .name = "build invalid source", .args = &.{ "build", "invalid.nov" }, .exit_code = 1, .stderr_match = "invalid.nov:1:13: error:" },
        .{ .name = "run invalid source", .args = &.{ "run", "invalid.nov" }, .exit_code = 1, .stderr_match = "invalid.nov:1:13: error:" },
        .{ .name = "build missing main", .args = &.{ "build", "let-only.nov" }, .exit_code = 1, .stderr_match = "error:" },
        .{ .name = "unknown option", .args = &.{"--unknown-option"}, .exit_code = 1, .stderr_match = "--unknown-option" },
        .{ .name = "check rejects --check", .args = &.{ "check", "--check", "valid.nov" }, .exit_code = 1, .stderr_match = "--check" },
        .{ .name = "--check requires fmt", .args = &.{"--check"}, .exit_code = 1, .stderr_match = "--check" },
        .{ .name = "check valid syntax", .args = &.{ "check", "valid.nov" }, .exit_code = 0 },
        .{ .name = "check annotated scalar globals", .args = &.{ "check", "--semantic", "annotated-globals.nov" }, .exit_code = 0 },
        .{ .name = "fmt annotated scalar globals", .args = &.{ "fmt", "--check", "annotated-globals.nov" }, .exit_code = 0 },
        .{ .name = "annotated global exact declaration type", .args = &.{ "emit-c", "annotated-global-mismatch.nov" }, .exit_code = 1, .stderr_match = "expression does not match the expected type" },
        .{ .name = "annotated global exact nested types", .args = &.{ "check", "--semantic", "annotated-global-nesting.nov" }, .exit_code = 1, .stderr_match = "expression does not match the expected type" },
        .{ .name = "annotated global literal bounds", .args = &.{ "check", "--semantic", "annotated-global-bounds.nov" }, .exit_code = 1, .stderr_match = "integer literal does not fit" },
        .{ .name = "annotated global interpolation diagnostic", .args = &.{ "emit-c", "annotated-global-interpolation.nov" }, .exit_code = 1, .stderr_match = "move interpolation into a function" },
        .{ .name = "global integer defaults to int", .args = &.{ "check", "--semantic", "global-untyped.nov" }, .exit_code = 0 },
        .{ .name = "global ascription bounds", .args = &.{ "check", "--semantic", "global-bounds.nov" }, .exit_code = 1, .stderr_match = "integer literal does not fit" },
        .{ .name = "global ascription rejects eager calls", .args = &.{ "check", "--semantic", "global-call.nov" }, .exit_code = 1, .stderr_match = "dependency analysis for this expression" },
        .{ .name = "global ascription unknown value", .args = &.{ "check", "--semantic", "global-unknown.nov" }, .exit_code = 1, .stderr_match = "unknown name 'missing'" },
        .{ .name = "global ascription unknown type", .args = &.{ "check", "--semantic", "global-type.nov" }, .exit_code = 1, .stderr_match = "unknown type 'Missing'" },
        .{ .name = "global ascription exact nesting", .args = &.{ "emit-c", "global-nesting.nov" }, .exit_code = 1, .stderr_match = "expression does not match the expected type" },
        .{ .name = "global ascription accepts known-data blocks", .args = &.{ "check", "--semantic", "global-block.nov" }, .exit_code = 0 },
        .{ .name = "global ascription rejects interpolation", .args = &.{ "check", "--semantic", "global-interpolation.nov" }, .exit_code = 1, .stderr_match = "move interpolation into a function" },
        .{ .name = "check wide record", .args = &.{ "check", "--semantic", "wide-record.nov" }, .exit_code = 0 },
        .{ .name = "reject reachable wide record backend", .args = &.{ "emit-c", "wide-record.nov" }, .exit_code = 1, .stderr_match = "C backend only supports 8, 16, 32 and 64-bit integers" },
        .{ .name = "check wide record aliases", .args = &.{ "check", "--semantic", "wide-record-alias.nov" }, .exit_code = 0 },
        .{ .name = "reject reachable wide record alias backend", .args = &.{ "emit-c", "wide-record-alias.nov" }, .exit_code = 1, .stderr_match = "C backend only supports 8, 16, 32 and 64-bit integers" },
        .{ .name = "run unused wide record", .args = &.{ "run", "unused-wide-record.nov" }, .exit_code = 0 },
        .{ .name = "wide record literal bounds", .args = &.{ "check", "--semantic", "wide-record-bounds.nov" }, .exit_code = 1, .stderr_match = "integer literal does not fit the annotated type" },
        .{ .name = "check nested wide record arrays", .args = &.{ "check", "--semantic", "wide-record-array.nov" }, .exit_code = 0 },
        .{ .name = "reject reachable nested wide record array backend", .args = &.{ "emit-c", "wide-record-array.nov" }, .exit_code = 1, .stderr_match = "C backend only supports 8, 16, 32 and 64-bit integers" },
        .{ .name = "check nominal structs", .args = &.{ "check", "--semantic", "type.nov" }, .exit_code = 0 },
        .{ .name = "fmt nominal structs", .args = &.{ "fmt", "--check", "type.nov" }, .exit_code = 0 },
        .{ .name = "check type aliases", .args = &.{ "check", "--semantic", "type-aliases.nov" }, .exit_code = 0 },
        .{ .name = "fmt type aliases", .args = &.{ "fmt", "--check", "type-aliases.nov" }, .exit_code = 0 },
        .{ .name = "reject alias value", .args = &.{ "emit-c", "alias-value.nov" }, .exit_code = 1, .stderr_match = "expected a runtime value, found a type alias" },
        .{ .name = "reject alias cycle", .args = &.{ "check", "--semantic", "alias-cycle.nov" }, .exit_code = 1, .stderr_match = "cyclic type alias dependency" },
        .{ .name = "check exhaustive boolean match", .args = &.{ "check", "--semantic", "boolean-match.nov" }, .exit_code = 0 },
        .{ .name = "fmt exhaustive boolean match", .args = &.{ "fmt", "--check", "boolean-match.nov" }, .exit_code = 0 },
        .{ .name = "check ascription", .args = &.{ "check", "--semantic", "ascription.nov" }, .exit_code = 0 },
        .{ .name = "fmt ascription", .args = &.{ "fmt", "--check", "ascription.nov" }, .exit_code = 0 },
        .{ .name = "check interpolation", .args = &.{ "check", "--semantic", "interpolation.nov" }, .exit_code = 0 },
        .{ .name = "fmt interpolation", .args = &.{ "fmt", "--check", "interpolation.nov" }, .exit_code = 0 },
        .{ .name = "reject interpolation formats", .args = &.{ "emit-c", "interpolation-format.nov" }, .exit_code = 1, .stderr_match = "expected {expression}; blocks, control flow, comments and format options are unsupported" },
        .{ .name = "reject interpolation unknown names", .args = &.{ "emit-c", "interpolation-name.nov" }, .exit_code = 1, .stderr_match = "unknown name 'missing'" },
        .{ .name = "reject global interpolation", .args = &.{ "check", "--semantic", "interpolation-global.nov" }, .exit_code = 1, .stderr_match = "global strings require literal initializers; move interpolation into a function" },
        .{ .name = "check unchanged fibo", .args = &.{ "check", "--semantic", "fibo.nov" }, .exit_code = 0 },
        .{ .name = "check fizzbuzz", .args = &.{ "check", "--semantic", "fizzbuzz.nov" }, .exit_code = 0 },
        .{ .name = "fmt fizzbuzz", .args = &.{ "fmt", "--check", "fizzbuzz.nov" }, .exit_code = 0 },
        .{ .name = "fmt unchanged fibo", .args = &.{ "fmt", "--check", "fibo.nov" }, .exit_code = 0 },
        .{ .name = "check inferred locals", .args = &.{ "check", "--semantic", "inferred-locals.nov" }, .exit_code = 0 },
        .{ .name = "fmt inferred locals", .args = &.{ "fmt", "--check", "inferred-locals.nov" }, .exit_code = 0 },
        .{ .name = "check inferred integer default", .args = &.{ "check", "--semantic", "inferred-integer.nov" }, .exit_code = 0 },
        .{ .name = "reject inferred empty array", .args = &.{ "check", "--semantic", "inferred-empty-array.nov" }, .exit_code = 1, .stderr_match = "array literal needs an expected []T type" },
        .{ .name = "check array compounds", .args = &.{ "check", "--semantic", "array-compound.nov" }, .exit_code = 0 },
        .{ .name = "fmt array compounds", .args = &.{ "fmt", "--check", "array-compound.nov" }, .exit_code = 0 },
        .{ .name = "reject bool array compounds", .args = &.{ "emit-c", "array-bool-compound.nov" }, .exit_code = 1, .stderr_match = "operator is not supported" },
        .{ .name = "check scalar arrays", .args = &.{ "check", "--semantic", "scalar-arrays.nov" }, .exit_code = 0 },
        .{ .name = "check membership integer defaults", .args = &.{ "check", "--semantic", "membership-default.nov" }, .exit_code = 0 },
        .{ .name = "reject membership widening", .args = &.{ "emit-c", "membership-width.nov" }, .exit_code = 1, .stderr_match = "expression does not match the expected type" },
        .{ .name = "reject string membership", .args = &.{ "emit-c", "membership-string.nov" }, .exit_code = 1, .stderr_match = "operator is not supported for this operand type" },
        .{ .name = "reject legacy array length writes", .args = &.{ "emit-c", "array-length-write.nov" }, .exit_code = 1, .stderr_match = "assignment target must be a mutable local" },
        .{ .name = "check range search", .args = &.{ "check", "--semantic", "range-search.nov" }, .exit_code = 0 },
        .{ .name = "fmt range search", .args = &.{ "fmt", "--check", "range-search.nov" }, .exit_code = 0 },
        .{ .name = "check inclusive ranges", .args = &.{ "check", "--semantic", "inclusive-range.nov" }, .exit_code = 0 },
        .{ .name = "fmt inclusive ranges", .args = &.{ "fmt", "--check", "inclusive-range.nov" }, .exit_code = 0 },
        .{ .name = "check semantic loops", .args = &.{ "check", "--semantic", "loop-program.nov" }, .exit_code = 0 },
        .{ .name = "check break outside loop", .args = &.{ "check", "--semantic", "break-outside.nov" }, .exit_code = 1, .stderr_match = "break and continue require an enclosing loop" },
        .{ .name = "emit rejects conditional value break", .args = &.{ "emit-c", "value-break.nov" }, .exit_code = 1, .stderr_match = "conditional loops cannot break with a value" },
        .{ .name = "reject else break outside loop", .args = &.{ "check", "--semantic", "else-break-outside.nov" }, .exit_code = 1, .stderr_match = "break and continue require an enclosing loop" },
        .{ .name = "reject mismatched else result", .args = &.{ "emit-c", "else-mismatch.nov" }, .exit_code = 1, .stderr_match = "literal does not match the annotated type" },
        .{ .name = "check loop values and continuations", .args = &.{ "check", "--semantic", "loop-values.nov" }, .exit_code = 0 },
        .{ .name = "fmt loop values and continuations", .args = &.{ "fmt", "--check", "loop-values.nov" }, .exit_code = 0 },
        .{ .name = "fmt condition loops detects changes", .args = &.{ "fmt", "--check", "loop-program.nov" }, .exit_code = 1, .stderr_match = "file is not formatted" },
        .{ .name = "fmt value break checks syntax only", .args = &.{ "fmt", "--check", "value-break.nov" }, .exit_code = 1, .stderr_match = "file is not formatted" },
        .{ .name = "fmt scalar arrays example", .args = &.{ "fmt", "--check", "scalar-arrays.nov" }, .exit_code = 0 },
        .{ .name = "fmt length writes checks syntax only", .args = &.{ "fmt", "--check", "array-length-write.nov" }, .exit_code = 1, .stderr_match = "file is not formatted" },
        .{ .name = "fmt executable example", .args = &.{ "fmt", "--check", "checkout.nov" }, .exit_code = 0 },
        .{ .name = "check semantic mutable locals", .args = &.{ "check", "--semantic", "mutable-program.nov" }, .exit_code = 0 },
        .{ .name = "emit rejects immutable assignment", .args = &.{ "emit-c", "immutable-assignment.nov" }, .exit_code = 1, .stderr_match = "immutable-assignment.nov:2:1: error: assignment requires a mutable local" },
        .{ .name = "check semantic returns", .args = &.{ "check", "--semantic", "return-program.nov" }, .exit_code = 0 },
        .{ .name = "check return signature diagnostic", .args = &.{ "check", "--semantic", "return-invalid.nov" }, .exit_code = 1, .stderr_match = "return-invalid.nov:1:31: error: literal does not match the annotated type" },
        .{ .name = "emit rejects unreachable statement", .args = &.{ "emit-c", "return-unreachable.nov" }, .exit_code = 1, .stderr_match = "return-unreachable.nov:2:1: error: unreachable statement after terminal control flow" },
        .{ .name = "check semantic if expressions", .args = &.{ "check", "--semantic", "if-program.nov" }, .exit_code = 0 },
        .{ .name = "check semantic unselected if branch", .args = &.{ "check", "--semantic", "if-invalid.nov" }, .exit_code = 1, .stderr_match = "unknown name" },
        .{ .name = "check remains syntax only", .args = &.{ "check", "syntax-only.nov" }, .exit_code = 0 },
        .{ .name = "check array separators", .args = &.{ "check", "arrays.nov" }, .exit_code = 0 },
        .{ .name = "check unclosed array", .args = &.{ "check", "unclosed-array.nov" }, .exit_code = 1, .stderr_match = "unclosed-array.nov:1:16: error: expected ']', found 'EOF'" },
        .{ .name = "check without final newline", .args = &.{ "check", "no-final-newline.nov" }, .exit_code = 0 },
        .{ .name = "fmt check without final newline", .args = &.{ "fmt", "--check", "no-final-newline.nov" }, .exit_code = 1, .stderr_match = "no-final-newline.nov" },
        .{ .name = "check invalid syntax", .args = &.{ "check", "invalid.nov" }, .exit_code = 1, .stderr_match = "invalid.nov:1:13: error:" },
        .{ .name = "check unsupported loop", .args = &.{ "check", "unsupported-loop.nov" }, .exit_code = 1, .stderr_match = "unsupported-loop.nov:1:13: error: unsupported loop form" },
        .{ .name = "fmt unsupported loop", .args = &.{ "fmt", "--check", "unsupported-loop.nov" }, .exit_code = 1, .stderr_match = "unsupported-loop.nov:1:13: error: unsupported loop form" },
        .{ .name = "check missing source", .args = &.{ "check", "missing.nov" }, .exit_code = 1, .stderr_match = "missing.nov: error: FileNotFound" },
        .{ .name = "check instance methods", .args = &.{ "check", "--semantic", "instance-methods.nov" }, .exit_code = 0 },
        .{ .name = "reject instance receiver signature", .args = &.{ "emit-c", "instance-signature.nov" }, .exit_code = 1, .stderr_match = "instance method requires a first parameter of the exact receiver container type" },
        .{ .name = "check multiple valid files", .args = &.{ "check", "valid.nov", "another-valid.nov" }, .exit_code = 0 },
        .{ .name = "check mixed files", .args = &.{ "check", "valid.nov", "invalid.nov", "another-valid.nov" }, .exit_code = 1, .stderr_match = "invalid.nov:1:13: error:" },
        .{ .name = "fmt rejects mixed stdin", .args = &.{ "fmt", "-", "valid.nov" }, .exit_code = 1, .stderr_match = "requires sole input" },
        .{ .name = "fmt rejects duplicate stdin", .args = &.{ "fmt", "-", "-" }, .exit_code = 1, .stderr_match = "requires sole input" },
        .{ .name = "fmt rejects stdin exclusions", .args = &.{ "fmt", "-", "--exclude", "valid.nov" }, .exit_code = 1, .stderr_match = "cannot use --exclude" },
        .{ .name = "fmt rejects empty exclusion", .args = &.{ "fmt", "valid.nov", "--exclude", "" }, .exit_code = 1, .stderr_match = "nonempty path" },
        .{ .name = "check rejects exclusion", .args = &.{ "check", "valid.nov", "--exclude", "valid.nov" }, .exit_code = 1, .stderr_match = "--exclude requires" },
        .{ .name = "fmt literal exclusions repeat", .args = &.{ "fmt", "--check", "needs-format.nov", "invalid.nov", "valid.nov", "--exclude", "needs-format.nov", "--exclude", "invalid.nov" }, .exit_code = 0 },
        .{ .name = "fmt check formatted", .args = &.{ "fmt", "--check", "valid.nov" }, .exit_code = 0 },
        .{ .name = "fmt check needs formatting", .args = &.{ "fmt", "--check", "needs-format.nov" }, .exit_code = 1, .stderr_match = "needs-format.nov" },
    };
    for (cases) |case| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI: {s}", .{case.name});
        run.setCwd(fixtures.getDirectory());
        run.addArgs(case.args);
        if (case.lib_dir) {
            run.addArg("--lib-dir");
            run.addDirectoryArg(library);
        }
        run.expectExitCode(case.exit_code);
        run.expectStdOutEqual(case.stdout);
        if (case.stderr_match) |expected| {
            run.expectStdErrMatch(expected);
        } else {
            run.expectStdErrEqual("");
        }
        test_step.dependOn(&run.step);
    }

    for ([_][]const u8{ "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "check", "fmt", "emit-c" }) |command| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI: {s} rejects {s}", .{ command, mode });
            run.addArgs(&.{ command, "valid.nov", mode });
            run.expectExitCode(1);
            run.expectStdOutEqual("");
            run.expectStdErrMatch("require the build, run, or test command");
            test_step.dependOn(&run.step);
        }
    }

    const stdin_cases = [_]struct { source: []const u8, check: bool = false, output: []const u8 = "", diagnostic: ?[]const u8 = null, code: u8 = 0 }{
        .{ .source = "let f:(r:R)->str=r.next(1).text()", .output = "let f: (r: R) -> str = r.next(1).text()\n" },
        .{ .source = "let K=enum{not_found,io,}\nlet E=struct{k:K=.not_found,}\nlet f:()->K=K.io", .output = "let K = enum {\n    not_found,\n    io,\n}\nlet E = struct {\n    k: K = .not_found,\n}\nlet f: () -> K = K.io\n" },
        .{ .source = "let U=union{ok:R,}\nlet R=struct{s:str,}\nlet f:()->U=.ok[R.[s=\"ok\"]]", .output = "let U = union {\n    ok: R,\n}\nlet R = struct {\n    s: str,\n}\nlet f: () -> U = .ok[R.[ s = \"ok\" ]]\n" },
        .{ .source = "let G:(T:type)->type=union{ok:T,err:str,}\nlet R=G(int)\nlet f:(r:R)->R=.ok[r.!]", .output = "let G: (T: type) -> type = union {\n    ok: T,\n    err: str,\n}\nlet R = G(int)\nlet f: (r: R) -> R = .ok[r.!]\n" },
        .{ .source = "let U=union{ok:int,none,}\nlet f:(u:U)->int=match u{.ok=>|v|v\n.none=>0}", .output = "let U = union {\n    ok: int,\n    none,\n}\nlet f: (u: U) -> int = match u {\n    .ok => |v| v\n    .none => 0\n}\n" },
        .{ .source = "let standard=@import(\"std\")\nlet file=@import(\"./a/../file.nov\")", .output = "let standard = @import(\"std\")\nlet file = @import(\"./a/../file.nov\")\n" },
        .{ .source = "let x=1", .output = "let x = 1\n" },
        .{ .source = "let x = 1\n", .output = "let x = 1\n" },
        .{ .source = "" },
        .{ .source = "let x=1", .check = true, .diagnostic = "<stdin>: error: input is not formatted", .code = 1 },
        .{ .source = "let x = 1\n", .check = true },
        .{ .source = "let x=", .diagnostic = "<stdin>:1:7: error:", .code = 1 },
        .{ .source = "let f:()->void={a[-1]+=1\na[0]-=2\na[0]*=3\na[0]/=4\na[0]%=5}", .output = "let f: () -> void = {\n    a[-1] += 1\n    a[0] -= 2\n    a[0] *= 3\n    a[0] /= 4\n    a[0] %= 5\n}\n" },
        .{ .source = "let f:()->void={let mut s=\"é\"\ns+=\"\\0\"}", .output = "let f: () -> void = {\n    let mut s = \"é\"\n    s += \"\\0\"\n}\n" },
        .{ .source = "let x:[]int=[1]", .output = "let x: []int = [1]\n" },
        .{ .source = "let x = [1]\n", .check = true },
        .{ .source = "let x=[1]", .check = true, .diagnostic = "<stdin>: error: input is not formatted", .code = 1 },
        .{ .source = "let x:[]f16=[]", .diagnostic = "<stdin>: error: formatting this syntax is not supported yet", .code = 1 },
    };
    for (stdin_cases, 0..) |case, index| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI: fmt stdin {d}", .{index});
        run.addArgs(&.{ "fmt", "-" });
        if (case.check) run.addArg("--check");
        run.setStdIn(.{ .bytes = case.source });
        run.expectExitCode(case.code);
        run.expectStdOutEqual(case.output);
        if (case.diagnostic) |message| run.expectStdErrMatch(message) else run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }

    // An isolated PATH exercises real `zig cc` discovery, independent of host CC.
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        const library_run = b.addRunArtifact(exe);
        library_run.step.name = b.fmt("CLI: library Result ({s})", .{mode});
        library_run.setCwd(fixtures.getDirectory());
        library_run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        library_run.setEnvironmentVariable("NOV_CC", "");
        library_run.setEnvironmentVariable("CC", "");
        library_run.addArgs(&.{ "run", "library-result.nov", "--lib-dir" });
        library_run.addDirectoryArg(library);
        if (mode.len != 0) library_run.addArg(mode);
        library_run.expectExitCode(0);
        library_run.expectStdOutEqual("EF1 9 1 7\nfallback:2\n3 4\n");
        library_run.expectStdErrEqual("");
        test_step.dependOn(&library_run.step);
        const generic_method_run = b.addRunArtifact(exe);
        generic_method_run.step.name = b.fmt("CLI: generic methods ({s})", .{mode});
        generic_method_run.setCwd(fixtures.getDirectory());
        generic_method_run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        generic_method_run.setEnvironmentVariable("NOV_CC", "");
        generic_method_run.setEnvironmentVariable("CC", "");
        generic_method_run.addArgs(&.{ "run", "generic-methods.nov" });
        if (mode.len != 0) generic_method_run.addArg(mode);
        generic_method_run.expectExitCode(0);
        generic_method_run.expectStdOutEqual("E3 7\n");
        generic_method_run.expectStdErrEqual("");
        test_step.dependOn(&generic_method_run.step);

        const instance_run = b.addRunArtifact(exe);
        instance_run.step.name = b.fmt("CLI: instance methods ({s})", .{mode});
        instance_run.setCwd(fixtures.getDirectory());
        instance_run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        instance_run.setEnvironmentVariable("NOV_CC", "");
        instance_run.setEnvironmentVariable("CC", "");
        instance_run.addArgs(&.{ "run", "instance-methods.nov" });
        if (mode.len != 0) instance_run.addArg(mode);
        instance_run.expectExitCode(0);
        instance_run.expectStdOutEqual("hello true false\n");
        instance_run.expectStdErrEqual("");
        test_step.dependOn(&instance_run.step);

        const enums_run = b.addRunArtifact(exe);
        enums_run.step.name = b.fmt("CLI: enums and Result propagation ({s})", .{mode});
        enums_run.setCwd(fixtures.getDirectory());
        enums_run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        enums_run.setEnvironmentVariable("NOV_CC", "");
        enums_run.setEnvironmentVariable("CC", "");
        enums_run.addArgs(&.{ "run", "enums.nov" });
        if (mode.len != 0) enums_run.addArg(mode);
        enums_run.expectExitCode(0);
        enums_run.expectStdOutEqual("not_found not found true not_found\n");
        enums_run.expectStdErrEqual("");
        test_step.dependOn(&enums_run.step);

        const record_interpolation_run = b.addRunArtifact(exe);
        record_interpolation_run.step.name = b.fmt("CLI: record interpolation panic ({s})", .{mode});
        record_interpolation_run.setCwd(fixtures.getDirectory());
        record_interpolation_run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        record_interpolation_run.setEnvironmentVariable("NOV_CC", "");
        record_interpolation_run.setEnvironmentVariable("CC", "");
        record_interpolation_run.addArgs(&.{ "run", "record-interpolation.nov" });
        if (mode.len != 0) record_interpolation_run.addArg(mode);
        record_interpolation_run.expectExitCode(1);
        record_interpolation_run.expectStdOutEqual("C");
        record_interpolation_run.expectStdErrEqual("panic: Error: not found!\n");
        test_step.dependOn(&record_interpolation_run.step);

        const payload_run = b.addRunArtifact(exe);
        payload_run.step.name = b.fmt("CLI: record payloads and propagation ({s})", .{mode});
        payload_run.setCwd(fixtures.getDirectory());
        payload_run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        payload_run.setEnvironmentVariable("NOV_CC", "");
        payload_run.setEnvironmentVariable("CC", "");
        payload_run.addArgs(&.{ "run", "record-payloads.nov" });
        if (mode.len != 0) payload_run.addArg(mode);
        payload_run.expectExitCode(0);
        payload_run.expectStdOutEqual("true err record\n");
        payload_run.expectStdErrEqual("");
        test_step.dependOn(&payload_run.step);

        const generic_run = b.addRunArtifact(exe);
        generic_run.step.name = b.fmt("CLI: generic unions and propagation ({s})", .{mode});
        generic_run.setCwd(fixtures.getDirectory());
        generic_run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        generic_run.setEnvironmentVariable("NOV_CC", "");
        generic_run.setEnvironmentVariable("CC", "");
        generic_run.addArgs(&.{ "run", "generic-unions.nov" });
        if (mode.len != 0) generic_run.addArg(mode);
        generic_run.expectExitCode(0);
        generic_run.expectStdOutEqual("MM7 bad\n");
        generic_run.expectStdErrEqual("");
        test_step.dependOn(&generic_run.step);

        const generic_build = b.addRunArtifact(exe);
        generic_build.step.name = b.fmt("CLI: build generic unions ({s})", .{mode});
        generic_build.setCwd(fixtures.getDirectory());
        generic_build.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        generic_build.setEnvironmentVariable("NOV_CC", "");
        generic_build.setEnvironmentVariable("CC", "");
        generic_build.addArgs(&.{ "build", "generic-unions.nov" });
        if (mode.len != 0) generic_build.addArg(mode);
        generic_build.addArg("-o");
        const generic_program = generic_build.addOutputFileArg(if (exe.root_module.resolved_target.?.result.os.tag == .windows) "generic.exe" else "generic");
        generic_build.expectExitCode(0);
        generic_build.expectStdOutEqual("");
        generic_build.expectStdErrEqual("");
        const generic_execute = std.Build.Step.Run.create(b, b.fmt("CLI: execute generic unions ({s})", .{mode}));
        generic_execute.addFileArg(generic_program);
        generic_execute.expectExitCode(0);
        generic_execute.expectStdOutEqual("MM7 bad\n");
        generic_execute.expectStdErrEqual("");
        test_step.dependOn(&generic_execute.step);

        const source_panic = b.addRunArtifact(exe);
        source_panic.step.name = b.fmt("CLI: source panic ({s})", .{mode});
        source_panic.setCwd(fixtures.getDirectory());
        source_panic.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        source_panic.setEnvironmentVariable("NOV_CC", "");
        source_panic.setEnvironmentVariable("CC", "");
        source_panic.addArgs(&.{ "run", "source-panic.nov" });
        if (mode.len != 0) source_panic.addArg(mode);
        source_panic.expectExitCode(1);
        source_panic.expectStdOutEqual("before");
        source_panic.expectStdErrEqual("panic: hé\x00!\n");
        test_step.dependOn(&source_panic.step);

        const union_run = b.addRunArtifact(exe);
        union_run.step.name = b.fmt("CLI: nominal unions ({s})", .{mode});
        union_run.setCwd(fixtures.getDirectory());
        union_run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        union_run.setEnvironmentVariable("NOV_CC", "");
        union_run.setEnvironmentVariable("CC", "");
        union_run.addArgs(&.{ "run", "unions.nov" });
        if (mode.len != 0) union_run.addArg(mode);
        union_run.expectExitCode(0);
        union_run.expectStdOutEqual("ok:7 ab none\n");
        union_run.expectStdErrEqual("");
        test_step.dependOn(&union_run.step);

        const fibo = b.addRunArtifact(exe);
        fibo.step.name = b.fmt("CLI: unchanged fibo ({s})", .{mode});
        fibo.setCwd(fixtures.getDirectory());
        fibo.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        fibo.setEnvironmentVariable("NOV_CC", "");
        fibo.setEnvironmentVariable("CC", "");
        fibo.addArgs(&.{ "run", "fibo.nov" });
        if (mode.len != 0) fibo.addArg(mode);
        fibo.expectExitCode(0);
        fibo.expectStdOutEqual("55\n");
        fibo.expectStdErrEqual("");
        test_step.dependOn(&fibo.step);

        const fizzbuzz = b.addRunArtifact(exe);
        fizzbuzz.step.name = b.fmt("CLI: fizzbuzz ({s})", .{mode});
        fizzbuzz.setCwd(fixtures.getDirectory());
        fizzbuzz.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        fizzbuzz.setEnvironmentVariable("NOV_CC", "");
        fizzbuzz.setEnvironmentVariable("CC", "");
        fizzbuzz.addArgs(&.{ "run", "fizzbuzz.nov" });
        if (mode.len != 0) fizzbuzz.addArg(mode);
        fizzbuzz.expectExitCode(0);
        fizzbuzz.expectStdOutEqual("1\n2\nFizz\n4\nBuzz\nFizz\n7\n8\nFizz\nBuzz\n11\nFizz\n13\n14\nFizzBuzz\n");
        fizzbuzz.expectStdErrEqual("");
        test_step.dependOn(&fizzbuzz.step);

        const bits = b.addRunArtifact(exe);
        bits.step.name = b.fmt("CLI: bitwise ({s})", .{mode});
        bits.setCwd(fixtures.getDirectory());
        bits.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        bits.setEnvironmentVariable("NOV_CC", "");
        bits.setEnvironmentVariable("CC", "");
        bits.addArgs(&.{ "run", "bitwise.nov" });
        if (mode.len != 0) bits.addArg(mode);
        bits.expectExitCode(0);
        bits.expectStdOutEqual("-1 0 127 0\n");
        bits.expectStdErrEqual("");
        test_step.dependOn(&bits.step);

        const bits_build = b.addRunArtifact(exe);
        bits_build.step.name = b.fmt("CLI: build bitwise ({s})", .{mode});
        bits_build.setCwd(fixtures.getDirectory());
        bits_build.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        bits_build.setEnvironmentVariable("NOV_CC", "");
        bits_build.setEnvironmentVariable("CC", "");
        bits_build.addArgs(&.{ "build", "bitwise.nov" });
        if (mode.len != 0) bits_build.addArg(mode);
        bits_build.addArg("-o");
        const bits_program = bits_build.addOutputFileArg(if (exe.root_module.resolved_target.?.result.os.tag == .windows) "bits.exe" else "bits");
        bits_build.expectExitCode(0);
        bits_build.expectStdOutEqual("");
        bits_build.expectStdErrEqual("");
        const bits_run = std.Build.Step.Run.create(b, b.fmt("CLI: execute bitwise ({s})", .{mode}));
        bits_run.addFileArg(bits_program);
        bits_run.expectExitCode(0);
        bits_run.expectStdOutEqual("-1 0 127 0\n");
        bits_run.expectStdErrEqual("");
        test_step.dependOn(&bits_run.step);

        const record_run = b.addRunArtifact(exe);
        record_run.step.name = b.fmt("CLI: nominal structs ({s})", .{mode});
        record_run.setCwd(fixtures.getDirectory());
        record_run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        record_run.setEnvironmentVariable("NOV_CC", "");
        record_run.setEnvironmentVariable("CC", "");
        record_run.addArgs(&.{ "run", "type.nov" });
        if (mode.len != 0) record_run.addArg(mode);
        record_run.expectExitCode(0);
        record_run.expectStdOutEqual("x: 5, y: hello. z: true\n");
        record_run.expectStdErrEqual("");
        test_step.dependOn(&record_run.step);
        const record_build = b.addRunArtifact(exe);
        record_build.step.name = b.fmt("CLI: build nominal structs ({s})", .{mode});
        record_build.setCwd(fixtures.getDirectory());
        record_build.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        record_build.setEnvironmentVariable("NOV_CC", "");
        record_build.setEnvironmentVariable("CC", "");
        record_build.addArgs(&.{ "build", "type.nov" });
        if (mode.len != 0) record_build.addArg(mode);
        record_build.addArg("-o");
        const record_binary = record_build.addOutputFileArg(if (exe.root_module.resolved_target.?.result.os.tag == .windows) "record.exe" else "record");
        record_build.expectExitCode(0);
        record_build.expectStdOutEqual("");
        record_build.expectStdErrEqual("");
        const record_execute = std.Build.Step.Run.create(b, b.fmt("CLI: execute nominal structs ({s})", .{mode}));
        record_execute.addFileArg(record_binary);
        record_execute.expectExitCode(0);
        record_execute.expectStdOutEqual("x: 5, y: hello. z: true\n");
        record_execute.expectStdErrEqual("");
        test_step.dependOn(&record_execute.step);

        const search_output = "true false false\nFound 5\nNot found\nExhausted at MAX\n";
        const search = b.addRunArtifact(exe);
        search.step.name = b.fmt("CLI: range search ({s})", .{mode});
        search.setCwd(fixtures.getDirectory());
        search.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        search.setEnvironmentVariable("NOV_CC", "");
        search.setEnvironmentVariable("CC", "");
        search.addArgs(&.{ "run", "range-search.nov" });
        if (mode.len != 0) search.addArg(mode);
        search.expectExitCode(0);
        search.expectStdOutEqual(search_output);
        search.expectStdErrEqual("");
        test_step.dependOn(&search.step);

        const search_build = b.addRunArtifact(exe);
        search_build.step.name = b.fmt("CLI: build range search ({s})", .{mode});
        search_build.setCwd(fixtures.getDirectory());
        search_build.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        search_build.setEnvironmentVariable("NOV_CC", "");
        search_build.setEnvironmentVariable("CC", "");
        search_build.addArgs(&.{ "build", "range-search.nov" });
        if (mode.len != 0) search_build.addArg(mode);
        search_build.addArg("-o");
        const search_program = search_build.addOutputFileArg(if (exe.root_module.resolved_target.?.result.os.tag == .windows) "search.exe" else "search");
        search_build.expectExitCode(0);
        search_build.expectStdOutEqual("");
        search_build.expectStdErrEqual("");
        const search_run = std.Build.Step.Run.create(b, b.fmt("CLI: execute range search ({s})", .{mode}));
        search_run.addFileArg(search_program);
        search_run.expectExitCode(0);
        search_run.expectStdOutEqual(search_output);
        search_run.expectStdErrEqual("");
        test_step.dependOn(&search_run.step);

        const inclusive = b.addRunArtifact(exe);
        inclusive.step.name = b.fmt("CLI: inclusive ranges ({s})", .{mode});
        inclusive.setCwd(fixtures.getDirectory());
        inclusive.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        inclusive.setEnvironmentVariable("NOV_CC", "");
        inclusive.setEnvironmentVariable("CC", "");
        inclusive.addArgs(&.{ "run", "inclusive-range.nov" });
        if (mode.len != 0) inclusive.addArg(mode);
        inclusive.expectExitCode(0);
        inclusive.expectStdOutEqual("0\n1\n2\n18446744073709551615\n");
        inclusive.expectStdErrEqual("");
        test_step.dependOn(&inclusive.step);

        const inclusive_build = b.addRunArtifact(exe);
        inclusive_build.step.name = b.fmt("CLI: build inclusive ranges ({s})", .{mode});
        inclusive_build.setCwd(fixtures.getDirectory());
        inclusive_build.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        inclusive_build.setEnvironmentVariable("NOV_CC", "");
        inclusive_build.setEnvironmentVariable("CC", "");
        inclusive_build.addArgs(&.{ "build", "inclusive-range.nov" });
        if (mode.len != 0) inclusive_build.addArg(mode);
        inclusive_build.addArg("-o");
        const inclusive_program = inclusive_build.addOutputFileArg(if (exe.root_module.resolved_target.?.result.os.tag == .windows) "inclusive.exe" else "inclusive");
        inclusive_build.expectExitCode(0);
        inclusive_build.expectStdOutEqual("");
        inclusive_build.expectStdErrEqual("");
        const inclusive_run = std.Build.Step.Run.create(b, b.fmt("CLI: execute inclusive ranges ({s})", .{mode}));
        inclusive_run.addFileArg(inclusive_program);
        inclusive_run.expectExitCode(0);
        inclusive_run.expectStdOutEqual("0\n1\n2\n18446744073709551615\n");
        inclusive_run.expectStdErrEqual("");
        test_step.dependOn(&inclusive_run.step);

        const value_loops = b.addRunArtifact(exe);
        value_loops.step.name = b.fmt("CLI: loop values and continuations ({s})", .{mode});
        value_loops.setCwd(fixtures.getDirectory());
        value_loops.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        value_loops.setEnvironmentVariable("NOV_CC", "");
        value_loops.setEnvironmentVariable("CC", "");
        value_loops.addArgs(&.{ "run", "loop-values.nov" });
        if (mode.len != 0) value_loops.addArg(mode);
        value_loops.expectExitCode(0);
        value_loops.expectStdOutEqual("First multiple: found 28 42\nBounded search: true false false 28 -1\n");
        value_loops.expectStdErrEqual("");
        test_step.dependOn(&value_loops.step);

        const interpolation_output = "1: 1\n2: 2\n3: Fizz\n4: 4\n5: Buzz\n6: Fizz\n7: 7\n8: 8\n9: Fizz\n10: Buzz\n11: 11\n12: Fizz\n13: 13\n14: 14\n15: FizzBuzz\n{literal}: true: é\x00😀 3\n";
        const aliases = b.addRunArtifact(exe);
        aliases.step.name = b.fmt("CLI: type aliases ({s})", .{mode});
        aliases.setCwd(fixtures.getDirectory());
        aliases.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        aliases.setEnvironmentVariable("NOV_CC", "");
        aliases.setEnvironmentVariable("CC", "");
        aliases.addArgs(&.{ "run", "type-aliases.nov" });
        if (mode.len != 0) aliases.addArg(mode);
        aliases.expectExitCode(0);
        aliases.expectStdOutEqual("Count: 8 8\n");
        aliases.expectStdErrEqual("");
        test_step.dependOn(&aliases.step);

        const annotated_globals = b.addRunArtifact(exe);
        annotated_globals.step.name = b.fmt("CLI: annotated scalar globals ({s})", .{mode});
        annotated_globals.setCwd(fixtures.getDirectory());
        annotated_globals.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        annotated_globals.setEnvironmentVariable("NOV_CC", "");
        annotated_globals.setEnvironmentVariable("CC", "");
        annotated_globals.addArgs(&.{ "run", "annotated-globals.nov" });
        if (mode.len != 0) annotated_globals.addArg(mode);
        annotated_globals.expectExitCode(0);
        annotated_globals.expectStdOutEqual("true true -42 -42\nhé\x00:hé\x00\n");
        annotated_globals.expectStdErrEqual("");
        test_step.dependOn(&annotated_globals.step);

        const boolean_match = b.addRunArtifact(exe);
        boolean_match.step.name = b.fmt("CLI: exhaustive boolean match ({s})", .{mode});
        boolean_match.setCwd(fixtures.getDirectory());
        boolean_match.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        boolean_match.setEnvironmentVariable("NOV_CC", "");
        boolean_match.setEnvironmentVariable("CC", "");
        boolean_match.addArgs(&.{ "run", "boolean-match.nov" });
        if (mode.len != 0) boolean_match.addArg(mode);
        boolean_match.expectExitCode(0);
        boolean_match.expectStdOutEqual("1 2 3\n");
        boolean_match.expectStdErrEqual("");
        test_step.dependOn(&boolean_match.step);

        const ascription = b.addRunArtifact(exe);
        ascription.step.name = b.fmt("CLI: ascription ({s})", .{mode});
        ascription.setCwd(fixtures.getDirectory());
        ascription.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        ascription.setEnvironmentVariable("NOV_CC", "");
        ascription.setEnvironmentVariable("CC", "");
        ascription.addArgs(&.{ "run", "ascription.nov" });
        if (mode.len != 0) ascription.addArg(mode);
        ascription.expectExitCode(0);
        ascription.expectStdOutEqual("1 1\n2\n");
        ascription.expectStdErrEqual("");
        test_step.dependOn(&ascription.step);

        const interpolation = b.addRunArtifact(exe);
        interpolation.step.name = b.fmt("CLI: interpolation ({s})", .{mode});
        interpolation.setCwd(fixtures.getDirectory());
        interpolation.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        interpolation.setEnvironmentVariable("NOV_CC", "");
        interpolation.setEnvironmentVariable("CC", "");
        interpolation.addArgs(&.{ "run", "interpolation.nov" });
        if (mode.len != 0) interpolation.addArg(mode);
        interpolation.expectExitCode(0);
        interpolation.expectStdOutEqual(interpolation_output);
        interpolation.expectStdErrEqual("");
        test_step.dependOn(&interpolation.step);
        const interpolation_build = b.addRunArtifact(exe);
        interpolation_build.step.name = b.fmt("CLI: build interpolation ({s})", .{mode});
        interpolation_build.setCwd(fixtures.getDirectory());
        interpolation_build.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        interpolation_build.setEnvironmentVariable("NOV_CC", "");
        interpolation_build.setEnvironmentVariable("CC", "");
        interpolation_build.addArgs(&.{ "build", "interpolation.nov" });
        if (mode.len != 0) interpolation_build.addArg(mode);
        interpolation_build.addArg("-o");
        const interpolation_program = interpolation_build.addOutputFileArg(if (exe.root_module.resolved_target.?.result.os.tag == .windows) "interpolation.exe" else "interpolation");
        interpolation_build.expectExitCode(0);
        interpolation_build.expectStdOutEqual("");
        interpolation_build.expectStdErrEqual("");
        const interpolation_run = std.Build.Step.Run.create(b, b.fmt("CLI: execute interpolation ({s})", .{mode}));
        interpolation_run.addFileArg(interpolation_program);
        interpolation_run.expectExitCode(0);
        interpolation_run.expectStdOutEqual(interpolation_output);
        interpolation_run.expectStdErrEqual("");
        test_step.dependOn(&interpolation_run.step);

        const strings = b.addRunArtifact(exe);
        strings.step.name = b.fmt("CLI: UTF8 strings and NUL ({s})", .{mode});
        strings.setCwd(fixtures.getDirectory());
        strings.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        strings.setEnvironmentVariable("NOV_CC", "");
        strings.setEnvironmentVariable("CC", "");
        strings.addArgs(&.{ "run", "strings.nov" });
        if (mode.len != 0) strings.addArg(mode);
        strings.expectExitCode(0);
        strings.expectStdOutEqual("Hello, 世界! 10\n[new binding]!\n[Hello, 世界!]\n{UTF-8} 1\na\x00b 3\nSame greeting: true true\nNUL bytes: true true\nUTF-8 equality: true false\n");
        strings.expectStdErrEqual("");
        test_step.dependOn(&strings.step);

        const inferred = b.addRunArtifact(exe);
        inferred.step.name = b.fmt("CLI: inferred locals ({s})", .{mode});
        inferred.setCwd(fixtures.getDirectory());
        inferred.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        inferred.setEnvironmentVariable("NOV_CC", "");
        inferred.setEnvironmentVariable("CC", "");
        inferred.addArgs(&.{ "run", "inferred-locals.nov" });
        if (mode.len != 0) inferred.addArg(mode);
        inferred.expectExitCode(0);
        inferred.expectStdOutEqual("7\n8\n9\n7 8 8 9 3 true\n");
        inferred.expectStdErrEqual("");
        test_step.dependOn(&inferred.step);

        for ([_]bool{ false, true }) |bounds| {
            const arrays = b.addRunArtifact(exe);
            arrays.step.name = b.fmt("CLI: scalar arrays bounds={any} ({s})", .{ bounds, mode });
            arrays.setCwd(fixtures.getDirectory());
            arrays.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            arrays.setEnvironmentVariable("NOV_CC", "");
            arrays.setEnvironmentVariable("CC", "");
            arrays.addArgs(&.{ "run", if (bounds) "array-bounds.nov" else "scalar-arrays.nov" });
            if (mode.len != 0) arrays.addArg(mode);
            arrays.expectExitCode(if (bounds) 1 else 0);
            arrays.expectStdOutEqual(if (bounds) "" else "3 30 30 6\n454 15\n99 15 4\nfalse true 2\n0\ntrue false true true\ntrue false false false\n");
            arrays.expectStdErrEqual(if (bounds) "panic: array index out of bounds\n" else "");
            test_step.dependOn(&arrays.step);
        }
        for ([_]bool{ false, true }) |bounds| {
            const compound = b.addRunArtifact(exe);
            compound.step.name = b.fmt("CLI: array compounds bounds={any} ({s})", .{ bounds, mode });
            compound.setCwd(fixtures.getDirectory());
            compound.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            compound.setEnvironmentVariable("NOV_CC", "");
            compound.setEnvironmentVariable("CC", "");
            compound.addArgs(&.{ "run", if (bounds) "array-compound-bounds.nov" else "array-compound.nov" });
            if (mode.len != 0) compound.addArg(mode);
            compound.expectExitCode(if (bounds) 1 else 0);
            compound.expectStdOutEqual(if (bounds) "" else "2 15 2\n");
            compound.expectStdErrEqual(if (bounds) "panic: array index out of bounds\n" else "");
            test_step.dependOn(&compound.step);
        }
        const loops = b.addRunArtifact(exe);
        loops.step.name = b.fmt("CLI: condition loops ({s})", .{mode});
        loops.setCwd(fixtures.getDirectory());
        loops.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        loops.setEnvironmentVariable("NOV_CC", "");
        loops.setEnvironmentVariable("CC", "");
        loops.addArgs(&.{ "run", "loop-program.nov" });
        if (mode.len != 0) loops.addArg(mode);
        loops.expectExitCode(0);
        loops.expectStdOutEqual("1\n3\n4\n");
        loops.expectStdErrEqual("");
        test_step.dependOn(&loops.step);
        const loop_build = b.addRunArtifact(exe);
        loop_build.step.name = b.fmt("CLI: build condition loops ({s})", .{mode});
        loop_build.setCwd(fixtures.getDirectory());
        loop_build.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        loop_build.setEnvironmentVariable("NOV_CC", "");
        loop_build.setEnvironmentVariable("CC", "");
        loop_build.addArgs(&.{ "build", "loop-program.nov" });
        if (mode.len != 0) loop_build.addArg(mode);
        loop_build.addArg("-o");
        const loop_program = loop_build.addOutputFileArg(if (exe.root_module.resolved_target.?.result.os.tag == .windows) "loops.exe" else "loops");
        loop_build.expectExitCode(0);
        loop_build.expectStdOutEqual("");
        loop_build.expectStdErrEqual("");
        const loop_run = std.Build.Step.Run.create(b, b.fmt("CLI: execute condition loops ({s})", .{mode}));
        loop_run.addFileArg(loop_program);
        loop_run.expectExitCode(0);
        loop_run.expectStdOutEqual("1\n3\n4\n");
        loop_run.expectStdErrEqual("");
        test_step.dependOn(&loop_run.step);
        const mutable = b.addRunArtifact(exe);
        mutable.step.name = b.fmt("CLI: mutable locals ({s})", .{mode});
        mutable.setCwd(fixtures.getDirectory());
        mutable.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        mutable.setEnvironmentVariable("NOV_CC", "");
        mutable.setEnvironmentVariable("CC", "");
        mutable.addArgs(&.{ "run", "mutable-program.nov" });
        if (mode.len != 0) mutable.addArg(mode);
        mutable.expectExitCode(0);
        mutable.expectStdOutEqual("3 false 42 3\n");
        mutable.expectStdErrEqual("");
        test_step.dependOn(&mutable.step);
        const returns = b.addRunArtifact(exe);
        returns.step.name = b.fmt("CLI: early returns ({s})", .{mode});
        returns.setCwd(fixtures.getDirectory());
        returns.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        returns.setEnvironmentVariable("NOV_CC", "");
        returns.setEnvironmentVariable("CC", "");
        returns.addArgs(&.{ "run", "return-program.nov" });
        if (mode.len != 0) returns.addArg(mode);
        returns.expectExitCode(0);
        returns.expectStdOutEqual("42 false\n");
        returns.expectStdErrEqual("");
        test_step.dependOn(&returns.step);
        const conditional = b.addRunArtifact(exe);
        conditional.step.name = b.fmt("CLI: if expressions ({s})", .{mode});
        conditional.setCwd(fixtures.getDirectory());
        conditional.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        conditional.setEnvironmentVariable("NOV_CC", "");
        conditional.setEnvironmentVariable("CC", "");
        conditional.addArgs(&.{ "run", "if-program.nov" });
        if (mode.len != 0) conditional.addArg(mode);
        conditional.expectExitCode(0);
        conditional.expectStdOutEqual("42\n");
        conditional.expectStdErrEqual("");
        test_step.dependOn(&conditional.step);
        const checkout = b.addRunArtifact(exe);
        checkout.step.name = b.fmt("CLI: native integer checkout ({s})", .{mode});
        checkout.setCwd(fixtures.getDirectory());
        checkout.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        checkout.setEnvironmentVariable("NOV_CC", "");
        checkout.setEnvironmentVariable("CC", "");
        checkout.addArgs(&.{ "run", "checkout.nov" });
        if (mode.len != 0) checkout.addArg(mode);
        checkout.expectExitCode(0);
        checkout.expectStdOutEqual("3 6750\n");
        checkout.expectStdErrEqual("");
        test_step.dependOn(&checkout.step);
        for ([_]bool{ false, true }) |panic| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI: run {s} ({s})", .{ if (panic) "panic" else "printed program", mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ "run", if (panic) "panic-program.nov" else "print-program.nov" });
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(if (panic) 1 else 0);
            run.expectStdOutEqual(if (panic) "" else "true -42\n");
            run.expectStdErrEqual(if (panic) "panic: division by zero\n" else "");
            test_step.dependOn(&run.step);
        }
        for ([_]bool{ false, true }) |implicit| {
            for ([_]bool{ false, true }) |panic| {
                for ([_][]const u8{ "-o", "--output" }) |option| {
                    const compile = b.addRunArtifact(exe);
                    compile.step.name = b.fmt("CLI: {s} {s} with {s} ({s})", .{ if (implicit) "implicit build" else "build", if (panic) "panic" else "printed program", option, mode });
                    compile.setCwd(fixtures.getDirectory());
                    compile.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                    compile.setEnvironmentVariable("NOV_CC", "");
                    compile.setEnvironmentVariable("CC", "");
                    if (!implicit) compile.addArg("build");
                    if (mode.len != 0) compile.addArg(mode);
                    // Exercise literal --cc paths as well as fallback discovery.
                    if (implicit) compile.addArgs(&.{ "--cc", "/nov-missing/explicit" });
                    compile.addArgs(&.{ if (panic) "panic-program.nov" else "./print-program.nov", option });
                    const program = compile.addOutputFileArg(if (exe.root_module.resolved_target.?.result.os.tag == .windows) "printed program.exe" else "printed program");
                    compile.expectExitCode(0);
                    compile.expectStdOutEqual("");
                    compile.expectStdErrEqual("");
                    const execute = std.Build.Step.Run.create(b, b.fmt("CLI: execute {s}", .{compile.step.name}));
                    execute.addFileArg(program);
                    execute.expectExitCode(if (panic) 1 else 0);
                    execute.expectStdOutEqual(if (panic) "" else "true -42\n");
                    execute.expectStdErrEqual(if (panic) "panic: division by zero\n" else "");
                    test_step.dependOn(&execute.step);
                }
            }
        }
    }
}

fn addInferredClosureCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    _ = fixtures.add("inferred-closures.nov", @embedFile("examples/inferred-closures.nov"));
    for ([_][]const u8{ "fmt", "check" }) |command| {
        const check = b.addRunArtifact(exe);
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ command, if (std.mem.eql(u8, command, "fmt")) "--check" else "--semantic", "inferred-closures.nov" });
        if (!std.mem.eql(u8, command, "fmt")) {
            check.addArg("--lib-dir");
            check.addDirectoryArg(library);
        }
        check.expectExitCode(0);
        check.expectStdOutEqual("");
        check.expectStdErrEqual("");
        test_step.dependOn(&check.step);
    }
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test" }) |command| {
            const run = b.addRunArtifact(exe);
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ command, "inferred-closures.nov" });
            if (mode.len != 0) run.addArg(mode);
            run.addArg("--lib-dir");
            run.addDirectoryArg(library);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (std.mem.eql(u8, command, "test")) "PASS inferred_callbacks\n1 passed; 0 failed; 1 total\n" else "Doubled: [2, 4, 6]\nSelected: [4, 6]; total: 10\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
}

fn addClosureCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("closures.nov", @embedFile("examples/closures.nov"));
    _ = fixtures.add("factory.nov", "@[public]\nlet counter=()->()->uint {let mut n:uint=0\n()->uint {n+=1\nn}}\n");
    _ = fixtures.add("imported.nov", "let api=@import(\"factory.nov\")\nlet main=(){let next=api.counter()\n@println(next(),next())}\n");
    _ = fixtures.add("excluded.nov", "@[test]\nlet ignored = (bad: Missing) {unknown()}\nlet main = () {let f = () -> int {7}\n@assert(f() == 7)}\n");
    for ([_][]const u8{ "fmt", "semantic" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI closures: example {s}", .{mode});
        run.setCwd(fixtures.getDirectory());
        run.addArgs(if (std.mem.eql(u8, mode, "fmt")) &.{ "fmt", "--check", "closures.nov" } else &.{ "check", "--semantic", "closures.nov" });
        run.expectExitCode(0);
        run.expectStdOutEqual("");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test", "excluded", "imported" }) |command| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI closures: {s} ({s})", .{ command, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            const excluded = std.mem.eql(u8, command, "excluded");
            const tests = std.mem.eql(u8, command, "test");
            const imported = std.mem.eql(u8, command, "imported");
            run.addArgs(&.{ if (tests) "test" else "run", if (excluded) "excluded.nov" else if (imported) "imported.nov" else "closures.nov" });
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (excluded) "" else if (imported) "1 2\n" else if (tests)
                "PASS escaping_snapshot\nPASS shared_counter\nPASS outer_updates\nPASS array_copies\nPASS generic_method\nPASS nested_capture\n6 passed; 0 failed; 6 total\n"
            else
                "5 + 3 = 8\nShared counter: 1 2 3\nSnapshot: 1 Current: 2\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "borrow", .source = "let main=(){let mut n:int=1\nlet view=&n\nlet f=()->int {view.*}\n}\n", .diagnostic = "local references require" },
        .{ .name = "recursion", .source = "let main=(){let f=(n:int)->int {f(n)}\n}\n", .diagnostic = "unknown name 'f'" },
        .{ .name = "parameter", .source = "let main=(){let f=(x){x}\n}\n", .diagnostic = "cannot infer every generic function type variable or inferred closure signature" },
        .{ .name = "foreign", .source = "@[export]\nlet callback=(f:()->int)->int {f()}\n", .diagnostic = "export ABI supports only" },
        .{ .name = "loan-call", .source = "let main=(){let mut n:int=0\nlet f=()->int {n}\nlet view=&n\n@println(f(),view.*)}\n", .diagnostic = "owner access or aliased reference arguments conflict with a live borrow" },
    }) |case| {
        const source = b.fmt("invalid-{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI closures: reject {s}", .{case.name});
        run.setCwd(fixtures.getDirectory());
        run.addArgs(&.{ "check", "--semantic", source });
        run.expectExitCode(1);
        run.expectStdOutEqual("");
        run.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&run.step);
    }
}

fn addMixedOperatorCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    addArrayValueHelperCliTests(b, exe, test_step, "mixed-operators.nov", @embedFile("examples/mixed-operators.nov"), "Scaled: Vector[ x: 2, y: 4 ]\nDot: 5\nScalar on left: Vector[ x: 2, y: 4 ]\n", "PASS mixed_signatures_and_distinct_results\nPASS mixed_equality_preserves_structural_equality\nPASS mixed_ordering_resolves_reversed_signatures\nPASS compound_results_and_snapshots\nPASS generic_containers_functions_and_callbacks\nPASS function_and_void_results\nPASS unique_parameter_context_and_unevaluated_queries\n7 passed; 0 failed; 7 total\n");
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    _ = fixtures.add("units.nov",
        \\@[public]
        \\let Value=struct{n:int,
        \\@[public]
        \\@[operator(.add)]
        \\let plus:(a:Value,b:int)->Value=.[n=a.n+b]
        \\@[public]
        \\@[operator(.add)]
        \\let text:(a:int,b:Value)->str="{a}:{b.n}"
        \\@[public]
        \\@[operator(.mul)]
        \\let product:(a:Value,b:Value)->int=a.n*b.n
        \\}
        \\@[public]
        \\let Secret=struct{n:int,
        \\@[operator(.mul)]
        \\let product:(a:int,b:Secret)->int=a*b.n
        \\}
        \\
    );
    _ = fixtures.add("imported.nov",
        \\let units=@import("units.nov")
        \\let Alias=units.Value
        \\let add:(a:`A,b:`B)->`C=a+b
        \\let main:()={let value=Alias.[n=3]
        \\@assert(add(value,2)==Alias.[n=5])
        \\@assert(add(2,value)=="2:3")
        \\@assert(value*value==9)
        \\@println("imported mixed operators")}
        \\
    );
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.setCwd(fixtures.getDirectory());
        run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        run.setEnvironmentVariable("NOV_CC", "");
        run.setEnvironmentVariable("CC", "");
        run.addArgs(&.{ "run", "imported.nov", "--lib-dir" });
        run.addDirectoryArg(library);
        if (mode.len != 0) run.addArg(mode);
        run.expectExitCode(0);
        run.expectStdOutEqual("imported mixed operators\n");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    for ([_]struct { source: []const u8, diagnostic: []const u8 }{
        .{ .source = "let S=struct{\n@[operator(.add)]\nlet first:(a:S,b:int)->S=a\n@[operator(.add)]\nlet second:(a:S,b:int)->int=1\n}\nlet main:()={}", .diagnostic = "duplicate" },
        .{ .source = "let Alias=int\nlet S=struct{\n@[operator(.add)]\nlet first:(a:S,b:Alias)->int=1\n@[operator(.add)]\nlet second:(a:S,b:int)->int=2\n}\nlet main:()={}", .diagnostic = "duplicate" },
        .{ .source = "let A=struct{\n@[operator(.add)]\nlet first:(a:A,b:B)->int=1\n}\nlet B=struct{\n@[operator(.add)]\nlet second:(a:A,b:B)->bool=true\n}\nlet main:()={let value:int=A.[]+B.[]\n}", .diagnostic = "ambiguous" },
        .{ .source = "let S=struct{\n@[operator(.mul)]\nlet dot:(a:S,b:S)->int=1\n}\nlet main:()={let mut value=S.[]\nvalue*=S.[]\n}", .diagnostic = "expected type" },
        .{ .source = "let S=struct{\n@[operator(.add)]\nlet add:(a:S,b:u8)->S=a\n}\nlet main:()={let n:u16=1\nlet value=S.[]+n\n}", .diagnostic = "not supported" },
        .{ .source = "let units=@import(\"units.nov\")\nlet main:()={let value=2*units.Secret.[n=3]\n}", .diagnostic = "imported methods require @[public]" },
        .{ .source = "let S=struct{\n@[operator(.add)]\nlet add:(a:S,b:int)->int=b\n}\nlet main:()={let value=1+S.[]\n}", .diagnostic = "not supported" },
        .{ .source = "let S=struct{\n@[operator(.equal)]\nlet equal:(a:S,b:int)->bool=true\n}\nlet main:()=@println(1 in [S.[]])", .diagnostic = "literal does not match the annotated type" },
        .{ .source = "let Box:(T:type)->type=struct{\n@[operator(.add)]\nlet first:(a:@This(),b:T)->int=1\n@[operator(.add)]\nlet second:(a:@This(),b:int)->int=2\n}\nlet main:()={let value=Box(int).[]\n}", .diagnostic = "duplicate" },
    }, 0..) |case, i| {
        const path = b.fmt("bad-mixed-operator-{d}.nov", .{i});
        _ = fixtures.add(path, case.source);
        for ([_]bool{ false, true }) |emit| {
            const reject = b.addRunArtifact(exe);
            reject.setCwd(fixtures.getDirectory());
            reject.addArgs(if (emit) &.{ "emit-c", path } else &.{ "check", "--semantic", path });
            reject.addArg("--lib-dir");
            reject.addDirectoryArg(library);
            reject.expectExitCode(1);
            reject.expectStdOutEqual("");
            reject.expectStdErrMatch("error:");
            reject.expectStdErrMatch(case.diagnostic);
            test_step.dependOn(&reject.step);
        }
    }
}

fn addOperatorCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    addArrayValueHelperCliTests(b, exe, test_step, "operators.nov", @embedFile("examples/operators.nov"), "Sum: 17\nNegative: -5\nOrdering: true false\nCompound: 17\n", "PASS arithmetic_negation_and_bitwise\nPASS derived_ordering_and_sort\nPASS compound_local_field_element_and_reference_targets\nPASS snapshots_and_compound_rhs_replacement\nPASS generic_hooks_inferred_callbacks_and_queries\nPASS enum_union_and_contextual_literals\nPASS operand_evaluation_precedes_reversed_ordering\n7 passed; 0 failed; 7 total\n");
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    _ = fixtures.add("models.nov",
        \\let std=@import("std")
        \\@[public]
        \\let Value=struct{n:int,
        \\@[public]
        \\@[operator(std.builtin.Operator.add)]
        \\let plus:(a:Value,b:Value)->Value=.[n=a.n+b.n]
        \\@[public]
        \\@[operator(.less_than)]
        \\let less:(a:Value,b:Value)->bool=a.n<b.n
        \\@[operator(.neg)]
        \\let neg:(a:Value)->Value=.[n= -a.n]
        \\}
        \\
    );
    _ = fixtures.add("imported.nov",
        \\let models=@import("models.nov")
        \\let Alias=models.Value
        \\let add:(a:`T,b:`T)->`T=a+b
        \\let main:()={let a=Alias.[n=1]
        \\let b=Alias.[n=2]
        \\let sum=add(a,b)
        \\@assert(sum.n==3 and a<b and b>=a)
        \\@println("imported operators")}
        \\
    );
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.setCwd(fixtures.getDirectory());
        run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        run.setEnvironmentVariable("NOV_CC", "");
        run.setEnvironmentVariable("CC", "");
        run.addArgs(&.{ "run", "imported.nov", "--lib-dir" });
        run.addDirectoryArg(library);
        if (mode.len != 0) run.addArg(mode);
        run.expectExitCode(0);
        run.expectStdOutEqual("imported operators\n");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    for ([_][]const u8{
        "let S=struct{\n@[operator(.add)]\nlet bad:(a:S)->S=a\n}\nlet main:()={}",
        "let S=struct{\n@[operator(.neg)]\nlet bad:(a:S,b:S)->S=a\n}\nlet main:()={}",
        "let S=struct{\n@[operator(.less_than)]\nlet bad:(a:S,b:S)->S=a\n}\nlet main:()={}",
        "let S=struct{\n@[operator(.mul)]\nlet bad:(a:int,b:int)->int=1\n}\nlet main:()={}",
        "let S=struct{}\nlet main:()={let x=S.[]+S.[]\n}",
        "let models=@import(\"models.nov\")\nlet main:()={let x= -models.Value.[n=1]\n}",
        "let models=@import(\"models.nov\")\nlet global=models.Value.[n=1]+models.Value.[n=2]\nlet main:()={}",
        "let A=struct{\n@[operator(.add)]\nlet plus:(a:A,b:A)->A=a\n}\nlet B=struct{}\nlet main:()={let x=A.[]+B.[]\n}",
        "let S=struct{f:()->bool,\n@[operator(.add)]\nlet plus:(a:S,b:S)->S=if a.f(){a}else{b}\n}\nlet main:()={let mut xs=[1,2]\nlet change=()->bool{xs=[9]\ntrue}\nlet a=S.[f=change]\nloop &x in xs{let z=a+a\n@println(x.*)}}",
    }, 0..) |source, i| {
        const path = b.fmt("bad-operator-{d}.nov", .{i});
        _ = fixtures.add(path, source);
        for ([_]bool{ false, true }) |emit| {
            const reject = b.addRunArtifact(exe);
            reject.setCwd(fixtures.getDirectory());
            reject.addArgs(if (emit) &.{ "emit-c", path } else &.{ "check", "--semantic", path });
            reject.addArg("--lib-dir");
            reject.addDirectoryArg(library);
            reject.expectExitCode(1);
            reject.expectStdOutEqual("");
            reject.expectStdErrMatch("error:");
            test_step.dependOn(&reject.step);
        }
    }
}

fn addEqualityCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    addArrayValueHelperCliTests(b, exe, test_step, "equality.nov", @embedFile("examples/equality.nov"), "Arrays: true\nSame user: true\nMembership: true\n", "PASS arrays_nested_values_and_membership\nPASS records_unions_enums_and_anonymous_values\nPASS overrides_apply_recursively_and_inequality_negates\nPASS enum_and_union_overrides\nPASS ieee_float_equality\nPASS hook_order_shortcircuit_snapshots_and_queries\n6 passed; 0 failed; 6 total\n");
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    _ = fixtures.add("models.nov",
        \\let std=@import("std")
        \\@[public]
        \\let User=struct{id:uint,name:str,
        \\@[public]
        \\@[operator(std.builtin.Operator.equal)]
        \\let same:(a:User,b:User)->bool=a.id==b.id
        \\}
        \\@[public]
        \\let Box:(T:type)->type=struct{value:T,
        \\@[public]
        \\@[operator(.equal)]
        \\let same:(a:@This(),b:@This())->bool=a.value==b.value
        \\}
        \\@[public]
        \\let Secret=struct{id:int,
        \\@[operator(.equal)]
        \\let same:(a:Secret,b:Secret)->bool=a.id==b.id
        \\}
        \\
    );
    _ = fixtures.add("imported.nov",
        \\let models=@import("models.nov")
        \\let Alias=models.User
        \\let equal:(a:`T,b:`T)->bool=a==b
        \\let main:()={let a=Alias.[id=7,name="Ada"]
        \\let b=models.User.[id=7,name="Grace"]
        \\@assert(equal(a,b) and b in [a])
        \\let first=models.Box([]int).[value=[1,2]]
        \\let second=models.Box([]int).[value=[1,2]]
        \\@assert(first==second and first!=models.Box([]int).[value=[2]])
        \\@println("imported equality")}
        \\
    );
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.setCwd(fixtures.getDirectory());
        run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        run.setEnvironmentVariable("NOV_CC", "");
        run.setEnvironmentVariable("CC", "");
        run.addArgs(&.{ "run", "imported.nov", "--lib-dir" });
        run.addDirectoryArg(library);
        if (mode.len != 0) run.addArg(mode);
        run.expectExitCode(0);
        run.expectStdOutEqual("imported equality\n");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    for ([_][]const u8{
        "let main:()={let a:[]u8=[1]\nlet b:[]u16=[1]\n@println(a==b)}",
        "let main:()={let f=()->int{1}\n@println([f]==[f])}",
        "let main:()={let mut n=1\nlet p=&n\n@println(p==p)}",
        "let main:()=@println([0..1]==[0..1])",
        "let Inner=struct{f:()->int,}\nlet Outer=struct{x:Inner,}\nlet query:(a:Outer)->@TypeOf(a==a)=true\nlet main:()={}",
        "let S=struct{n:int,\n@[operator(.equal)]\nlet bad:(a:S)->bool=true\n}\nlet main:()=@println(S.[n=1]==S.[n=1])",
        "let S=struct{n:int,\n@[operator(.equal)]\nlet bad:(a:S,b:S)->int=1\n}\nlet main:()=@println(S.[n=1]==S.[n=1])",
        "let S=struct{n:int,\n@[operator(.equal)]\nlet bad:(a:S)->int=1\n}\nlet main:()={}",
        "let Fake=enum{equal,}\nlet S=struct{\n@[operator(Fake.equal)]\nlet bad:(a:S,b:S)->bool=true\n}\nlet main:()={}",
        "let S=struct{\n@[operator(.index)]\nlet bad:(a:S,b:S)->S=S.[]\n}\nlet main:()={}",
        "let models=@import(\"models.nov\")\nlet main:()=@println(models.Secret.[id=1]==models.Secret.[id=1])",
        "let S=struct{f:()->bool,\n@[operator(.equal)]\nlet same:(a:S,b:S)->bool=a.f()\n}\nlet main:()={let mut xs=[1,2]\nlet change=()->bool{xs=[9]\ntrue}\nlet a=S.[f=change]\nloop &x in xs{@assert(a==a)\n@println(x.*)}}",
    }, 0..) |source, i| {
        const path = b.fmt("bad-equality-{d}.nov", .{i});
        _ = fixtures.add(path, source);
        for ([_]bool{ false, true }) |emit| {
            const reject = b.addRunArtifact(exe);
            reject.setCwd(fixtures.getDirectory());
            reject.addArgs(if (emit) &.{ "emit-c", path } else &.{ "check", "--semantic", path });
            reject.addArg("--lib-dir");
            reject.addDirectoryArg(library);
            reject.expectExitCode(1);
            reject.expectStdOutEqual("");
            reject.expectStdErrMatch("error:");
            test_step.dependOn(&reject.step);
        }
    }
}

fn addArrayConcatCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("array-concat.nov", @embedFile("examples/array-concat.nov"));
    const check = b.addRunArtifact(exe);
    check.setCwd(fixtures.getDirectory());
    check.addArgs(&.{ "check", "--semantic", "array-concat.nov" });
    check.expectExitCode(0);
    check.expectStdOutEqual("");
    check.expectStdErrEqual("");
    test_step.dependOn(&check.step);
    const fmt = b.addRunArtifact(exe);
    fmt.setCwd(fixtures.getDirectory());
    fmt.addArgs(&.{ "fmt", "--check", "array-concat.nov" });
    fmt.expectExitCode(0);
    fmt.expectStdOutEqual("");
    fmt.expectStdErrEqual("");
    test_step.dependOn(&fmt.step);
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test" }) |command| {
            const run = b.addRunArtifact(exe);
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ command, "array-concat.nov" });
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (std.mem.eql(u8, command, "run"))
                "Combined: [Ada, Grace, Linus]\nNumbers: [1, 2, 3]\n"
            else
                "PASS empty_chained_and_contextual_arrays\nPASS nested_storage_is_independent\nPASS storable_element_types\nPASS operands_snapshot_before_later_effects\nPASS generics_callbacks_and_unevaluated_queries\n5 passed; 0 failed; 5 total\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    for ([_][]const u8{
        "let f:(a:[]u8,b:[]u16)->[]u8=a+b\nlet main:()={}",
        "let f:(a:[]int)->[]int=a+1\nlet main:()={}",
        "let f:()={let a=[]+[]\n}\nlet main:()={}",
        "let f:(a:[]int)={let q:@TypeOf(a+[true])=[]\n}\nlet main:()={}",
        "let mut a=[1]\nlet bad=a+[2]\nlet main:()={}",
    }, 0..) |source, i| {
        const path = b.fmt("bad-concat-{d}.nov", .{i});
        _ = fixtures.add(path, source);
        for ([_]bool{ false, true }) |emit| {
            const reject = b.addRunArtifact(exe);
            reject.setCwd(fixtures.getDirectory());
            reject.addArgs(if (emit) &.{ "emit-c", path } else &.{ "check", "--semantic", path });
            reject.expectExitCode(1);
            reject.expectStdOutEqual("");
            reject.expectStdErrMatch("error:");
            test_step.dependOn(&reject.step);
        }
    }
}

fn addClearCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("clear.nov", @embedFile("examples/array-clear.nov"));
    const check = b.addRunArtifact(exe);
    check.setCwd(fixtures.getDirectory());
    check.addArgs(&.{ "check", "--semantic", "clear.nov" });
    check.expectExitCode(0);
    check.expectStdOutEqual("");
    check.expectStdErrEqual("");
    test_step.dependOn(&check.step);
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.setCwd(fixtures.getDirectory());
        run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        run.setEnvironmentVariable("NOV_CC", "");
        run.setEnvironmentVariable("CC", "");
        run.addArgs(&.{ "run", "clear.nov" });
        if (mode.len != 0) run.addArg(mode);
        run.expectExitCode(0);
        run.expectStdOutEqual("Cleared: [], saved: [Ada, Grace]\nNext batch: [Linus]\nNumbers: []\n");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
        const tests = b.addRunArtifact(exe);
        tests.setCwd(fixtures.getDirectory());
        tests.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        tests.setEnvironmentVariable("NOV_CC", "");
        tests.setEnvironmentVariable("CC", "");
        tests.addArgs(&.{ "test", "clear.nov" });
        if (mode.len != 0) tests.addArg(mode);
        tests.expectExitCode(0);
        test_step.dependOn(&tests.step);
    }
}

fn addLenCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("len.nov",
        \\let R = struct {len: uint = 7,}
        \\let count = @len("é😀\0")
        \\let empty = @len(@as([]int, []))
        \\let main: () = {
        \\    let mut calls: int = 0
        \\    let make = () -> str {calls += 1
        \\        "é😀\0"}
        \\    let mut rows = [[1, 2], [3]]
        \\    let alias = &rows
        \\    @assert(@len(alias.*) == 2 and @len(alias.*[0]) == 2)
        \\    @assert(@len(make()) == count and calls == 1)
        \\    @assert(@len("é😀\0"[1..]) == 2 and empty == 0)
        \\    @assert("{@len("é😀")}" == "2")
        \\    let mut record = R.[]
        \\    record.len = 9
        \\    @println(count, empty, calls, record.len)
        \\}
        \\
    );
    const check = b.addRunArtifact(exe);
    check.step.name = "CLI len: semantic check";
    check.setCwd(fixtures.getDirectory());
    check.addArgs(&.{ "check", "--semantic", "len.nov" });
    check.expectExitCode(0);
    check.expectStdOutEqual("");
    check.expectStdErrEqual("");
    test_step.dependOn(&check.step);
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI len: run ({s})", .{mode});
        run.setCwd(fixtures.getDirectory());
        run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        run.setEnvironmentVariable("NOV_CC", "");
        run.setEnvironmentVariable("CC", "");
        run.addArgs(&.{ "run", "len.nov" });
        if (mode.len != 0) run.addArg(mode);
        run.expectExitCode(0);
        run.expectStdOutEqual("3 0 1 9\n");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "array-property", .source = "let main:()={let a=[1]\n@println(a.len)}", .diagnostic = "unknown container field" },
        .{ .name = "string-property", .source = "let main:()=@println(\"é\".len)", .diagnostic = "unknown container field" },
        .{ .name = "array-property-write", .source = "let main:()={let mut a=[1]\na.len=2}", .diagnostic = "assignment target must be a mutable local" },
        .{ .name = "string-property-write", .source = "let main:()={let mut s=\"a\"\ns.len=2}", .diagnostic = "assignment target must be a mutable local" },
        .{ .name = "array-builtin-write", .source = "let main:()={let mut a=[1]\n@len(a)=2}", .diagnostic = "assignment target must be a mutable local" },
        .{ .name = "string-builtin-write", .source = "let main:()={let mut s=\"a\"\n@len(s)=2}", .diagnostic = "assignment target must be a mutable local" },
    }) |case| {
        const source = b.fmt("{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        for ([_]bool{ false, true }) |emit| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI len: reject {s} ({s})", .{ case.name, if (emit) "emit-c" else "check" });
            run.setCwd(fixtures.getDirectory());
            run.addArgs(if (emit) &.{ "emit-c", source } else &.{ "check", "--semantic", source });
            run.expectExitCode(1);
            run.expectStdOutEqual("");
            run.expectStdErrMatch(case.diagnostic);
            test_step.dependOn(&run.step);
        }
    }
}

fn addRangeLengthCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("range-length.nov", @embedFile("examples/range-length.nov"));
    _ = fixtures.add("counts.nov",
        \\@[public]
        \\let exclusive: uint = @len(2..5)
        \\@[public]
        \\let inclusive: uint = @len(-3..=2)
        \\@[public]
        \\let narrow: uint = @len(@as(u8, 0)..=@as(u8, 255))
        \\@[public]
        \\let reversed: uint = @len(5..=2)
        \\@[public]
        \\let stored = @as(i8, -128)..=@as(i8, 127)
        \\@[public]
        \\let stored_count: uint = @len(stored)
        \\
    );
    _ = fixtures.add("imported.nov",
        \\let counts = @import("counts.nov")
        \\let folded: uint = counts.exclusive + counts.inclusive + counts.narrow + counts.reversed + counts.stored_count
        \\let main: () = @println(counts.exclusive, counts.inclusive, counts.narrow, counts.reversed, counts.stored_count, folded)
        \\
    );
    const native_max = std.math.maxInt(usize);
    const native_min = std.math.minInt(isize);
    const native_signed_max = std.math.maxInt(isize);
    // Match the host executable's pointer width, not an optional build target.
    _ = fixtures.add("boundaries.nov", b.fmt(
        \\let main: () = {{
        \\    let lo: int = {d}
        \\    let hi: int = {d}
        \\    let top: uint = {d}
        \\    @assert(@len(lo..hi) == top)
        \\    @assert(@len(lo..0) == @as(uint, {d}))
        \\    @assert(@len(hi..=lo) == 0)
        \\    @assert(@len(top..=top) == 1)
        \\    @println(@len(lo..hi), @len(0..top))
        \\}}
        \\
    , .{ native_min, native_signed_max, native_max, @as(u128, 1) << (@bitSizeOf(usize) - 1) }));
    for ([_]struct { source: []const u8, stdout: []const u8 }{
        .{ .source = "range-length.nov", .stdout = b.fmt("Exclusive: 3 Inclusive: 4\nEmpty: 0 Reversed: 0\nCross zero: 6\nFull u8: 256\nStored generic: 4 Type: u{d}\n", .{@bitSizeOf(usize)}) },
        .{ .source = "imported.nov", .stdout = "3 6 256 0 256 521\n" },
        .{ .source = "boundaries.nov", .stdout = b.fmt("{d} {d}\n", .{ native_max, native_max }) },
    }) |case| {
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI range length: semantic {s}", .{case.source});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", case.source });
        check.expectExitCode(0);
        check.expectStdOutEqual("");
        check.expectStdErrEqual("");
        test_step.dependOn(&check.step);
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            for ([_][]const u8{ "run", "test" }) |command| {
                if (std.mem.eql(u8, command, "test") and !std.mem.eql(u8, case.source, "range-length.nov")) continue;
                const run = b.addRunArtifact(exe);
                run.step.name = b.fmt("CLI range length: {s} {s} ({s})", .{ command, case.source, mode });
                run.setCwd(fixtures.getDirectory());
                run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                run.setEnvironmentVariable("NOV_CC", "");
                run.setEnvironmentVariable("CC", "");
                run.addArgs(&.{ command, case.source });
                if (mode.len != 0) run.addArg(mode);
                run.expectExitCode(0);
                run.expectStdOutEqual(if (std.mem.eql(u8, command, "run")) case.stdout else "PASS exclusive_inclusive_empty_and_reversed\nPASS full_narrow_domains_and_native_result\nPASS stored_passed_returned_and_generic\nPASS endpoints_once_left_to_right\nPASS type_queries_do_not_evaluate\n5 passed; 0 failed; 5 total\n");
                run.expectStdErrEqual("");
                test_step.dependOn(&run.step);
            }
        }
    }
    for ([_]struct { name: []const u8, source: []const u8 }{
        .{ .name = "native-unsigned", .source = b.fmt("let main:()={{let hi:uint={d}\n@println(@len(0..=hi))}}", .{native_max}) },
        .{ .name = "native-signed", .source = b.fmt("let main:()={{let lo:int={d}\nlet hi:int={d}\n@println(@len(lo..=hi))}}", .{ native_min, native_signed_max }) },
        .{ .name = "u64-full", .source = "let main:()={let lo:u64=0\nlet hi:u64=18446744073709551615\n@println(@len(lo..=hi))}" },
        .{ .name = "i64-full", .source = "let main:()={let lo:i64=-9223372036854775808\nlet hi:i64=9223372036854775807\n@println(@len(lo..=hi))}" },
    }) |case| {
        const source = b.fmt("overflow-{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI range length: overflow {s} ({s})", .{ case.name, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ "run", source });
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(1);
            run.expectStdOutEqual("");
            run.expectStdErrEqual("panic: range length overflow\n");
            test_step.dependOn(&run.step);
        }
    }
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "unbounded", .source = "let main:()=@println(@len(0..))", .diagnostic = "expected expression" },
        .{ .name = "nonrange", .source = "let main:()=@println(@len(42))", .diagnostic = "@len" },
        .{ .name = "record", .source = "let R=struct{len:uint=7,}\nlet main:()=@println(@len(R.[]))", .diagnostic = "@len" },
        .{ .name = "range-property", .source = "let main:()={let r=0..3\n@println(r.len)}", .diagnostic = "error:" },
        .{ .name = "global-unsigned-overflow", .source = b.fmt("let count:uint=@len(@as(uint,0)..=@as(uint,{d}))\nlet main:()=@println(count)", .{native_max}), .diagnostic = "error:" },
        .{ .name = "global-signed-overflow", .source = b.fmt("let count:uint=@len(@as(int,{d})..=@as(int,{d}))\nlet main:()=@println(count)", .{ native_min, native_signed_max }), .diagnostic = "error:" },
        .{ .name = "global-u64-overflow", .source = "let count:uint=@len(@as(u64,0)..=@as(u64,18446744073709551615))\nlet main:()=@println(count)", .diagnostic = "error:" },
        .{ .name = "global-i64-overflow", .source = "let count:uint=@len(@as(i64,-9223372036854775808)..=@as(i64,9223372036854775807))\nlet main:()=@println(count)", .diagnostic = "error:" },
    }) |case| {
        const source = b.fmt("reject-{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        for ([_]bool{ false, true }) |emit| {
            const check = b.addRunArtifact(exe);
            check.step.name = b.fmt("CLI range length: reject {s} ({s})", .{ case.name, if (emit) "emit-c" else "check" });
            check.setCwd(fixtures.getDirectory());
            check.addArgs(if (emit) &.{ "emit-c", source } else &.{ "check", "--semantic", source });
            check.expectExitCode(1);
            check.expectStdOutEqual("");
            check.expectStdErrMatch(case.diagnostic);
            test_step.dependOn(&check.step);
        }
    }
}

fn addStringByteCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("string-bytes.nov", @embedFile("examples/string-bytes.nov"));
    _ = fixtures.add("excluded.nov", "@[test]\nlet excluded: () = @panic(\"test body ran\")\n" ++
        "let main: () = {let mut text = \"é😀\\0\"\nlet alias = &text\n@assert(alias.*[0] == 'é')\n@assert(alias.*[-2] == '😀')\n@assert(alias.*[-1] == '\\0')\n@assert(@len(alias.*) == 3)}\n");
    for ([_][]const u8{ "fmt", "semantic" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI string bytes: example {s}", .{mode});
        run.setCwd(fixtures.getDirectory());
        run.addArgs(if (std.mem.eql(u8, mode, "fmt")) &.{ "fmt", "--check", "string-bytes.nov" } else &.{ "check", "--semantic", "string-bytes.nov" });
        run.expectExitCode(0);
        run.expectStdOutEqual("");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test", "excluded" }) |command| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI string bytes: {s} ({s})", .{ command, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            const excluded = std.mem.eql(u8, command, "excluded");
            const tests = std.mem.eql(u8, command, "test");
            run.addArgs(&.{ if (tests) "test" else "run", if (excluded) "excluded.nov" else "string-bytes.nov" });
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (excluded) "" else if (tests) "PASS ascii_negative\nPASS utf8_nul\nPASS original_snapshot\n3 passed; 0 failed; 3 total\n" else "hello hello world h d\nh d\né 😀 \x00 3\nRune: é\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    for ([_]struct { name: []const u8, source: []const u8 }{
        .{ .name = "positive", .source = "let main: () = {let text = \"abc\"\n@println(text[3])}\n" },
        .{ .name = "too-negative", .source = "let main: () = {let text = \"abc\"\n@println(text[-4])}\n" },
        .{ .name = "empty", .source = "let main: () = {let text = \"\"\n@println(text[0])}\n" },
        .{ .name = "utf8-positive", .source = "let main: () = {let text = \"é😀\\0\"\n@println(text[3])}\n" },
        .{ .name = "utf8-too-negative", .source = "let main: () = {let text = \"é😀\\0\"\n@println(text[-4])}\n" },
    }) |case| {
        const source = b.fmt("bounds-{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        for ([_][]const u8{ "--debug", "--release" }) |mode| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI string bytes: bounds {s} ({s})", .{ case.name, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ "run", source, mode });
            run.expectExitCode(1);
            run.expectStdOutEqual("");
            run.expectStdErrEqual("panic: string index out of bounds\n");
            test_step.dependOn(&run.step);
        }
    }
    const mismatch = "expression does not match the expected type";
    const assignment = "assignment target must be a mutable local, its struct field path, or its scalar-array element";
    const borrow = "local references require &local or &localArray[index] and explicit p.* access";
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "result-uint", .source = "let byte: (text: str) -> uint = text[0]\n", .diagnostic = mismatch },
        .{ .name = "result-u8", .source = "let byte: (text: str) -> u8 = text[0]\n", .diagnostic = mismatch },
        .{ .name = "index-uint", .source = "let scalar: (text: str, index: uint) -> rune = text[index]\n", .diagnostic = mismatch },
        .{ .name = "equality-integer", .source = "let main: () = @assert(\"a\"[0] == 97)\n", .diagnostic = "literal does not match the annotated type" },
        .{ .name = "write", .source = "let main: () = {let mut text = \"a\"\ntext[0] = 'b'}\n", .diagnostic = assignment },
        .{ .name = "compound", .source = "let main: () = {let mut text = \"a\"\ntext[0] += 1}\n", .diagnostic = assignment },
        .{ .name = "alias-write", .source = "let main: () = {let mut text = \"a\"\nlet alias = &text\nalias.*[0] = 'b'}\n", .diagnostic = assignment },
        .{ .name = "borrow", .source = "let main: () = {let text = \"a\"\nlet byte = &text[0]\n}\n", .diagnostic = borrow },
        .{ .name = "mutable-borrow", .source = "let main: () = {let mut text = \"a\"\nlet byte = &text[0]\n}\n", .diagnostic = borrow },
        .{ .name = "alias-borrow", .source = "let main: () = {let mut text = \"a\"\nlet alias = &text\nlet byte = &alias.*[0]\n}\n", .diagnostic = borrow },
    }) |case| {
        const source = b.fmt("invalid-{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI string bytes: reject {s}", .{case.name});
        run.setCwd(fixtures.getDirectory());
        run.addArgs(&.{ "check", "--semantic", source });
        run.expectExitCode(1);
        run.expectStdOutEqual("");
        run.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&run.step);
    }
}

fn addStringSliceCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("string-slices.nov", @embedFile("examples/string-slices.nov"));
    _ = fixtures.add("excluded.nov", "@[test]\nlet excluded: () = @panic(\"test body ran\")\n" ++
        "let main: () = {let mut text = \"é😀\\0\"\nlet alias = &text\n@assert(alias.*[1..-1] == \"😀\")\n@assert(@len(alias.*[..]) == 3)}\n");
    for ([_][]const u8{ "fmt", "semantic" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI string slices: example {s}", .{mode});
        run.setCwd(fixtures.getDirectory());
        run.addArgs(if (std.mem.eql(u8, mode, "fmt")) &.{ "fmt", "--check", "string-slices.nov" } else &.{ "check", "--semantic", "string-slices.nov" });
        run.expectExitCode(0);
        run.expectStdOutEqual("");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test", "excluded" }) |command| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI string slices: {s} ({s})", .{ command, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            const excluded = std.mem.eql(u8, command, "excluded");
            const tests = std.mem.eql(u8, command, "test");
            run.addArgs(&.{ if (tests) "test" else "run", if (excluded) "excluded.nov" else "string-slices.nov" });
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (excluded) "" else if (tests)
                "PASS half_open_and_omitted\nPASS empty_boundaries\nPASS unicode_and_nul\nPASS nested_views\nPASS immutable_snapshots\nPASS inclusive_bounds\nPASS retained_owner\n7 passed; 0 failed; 7 total\n"
            else
                "é😀 hé llo hé😀llo\né😀 hé😀llo! é😀llo\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    for ([_]struct { name: []const u8, slice: []const u8, text: []const u8 = "é😀\\0", setup: []const u8 = "" }{
        .{ .name = "inclusive-end-at-length", .slice = "[..=3]" },
        .{ .name = "inclusive-empty", .text = "", .slice = "[..=0]" },
        .{ .name = "inclusive-reversed", .slice = "[2..=0]" },
        .{ .name = "inclusive-negative", .slice = "[..=-4]" },
        .{ .name = "inclusive-minimum", .slice = "[..=minimum]", .setup = b.fmt("let minimum: int = {d} - 1\n", .{std.math.minInt(isize) + 1}) },
        .{ .name = "inclusive-maximum", .slice = "[..=maximum]", .setup = b.fmt("let maximum: int = {d}\n", .{std.math.maxInt(isize)}) },
        .{ .name = "start-positive", .slice = "[4..]" },
        .{ .name = "end-positive", .slice = "[..4]" },
        .{ .name = "start-negative", .slice = "[-4..]" },
        .{ .name = "end-negative", .slice = "[..-4]" },
        .{ .name = "reversed", .slice = "[2..1]" },
        .{ .name = "normalized-reversed", .slice = "[-1..-2]" },
        .{ .name = "mixed-reversed", .slice = "[2..-2]" },
        .{ .name = "empty-positive", .text = "", .slice = "[..1]" },
        .{ .name = "empty-negative", .text = "", .slice = "[-1..]" },
        .{ .name = "minimum-int", .slice = "[minimum..]", .setup = b.fmt("let minimum: int = {d} - 1\n", .{std.math.minInt(isize) + 1}) },
    }) |case| {
        const source = b.fmt("bounds-{s}.nov", .{case.name});
        _ = fixtures.add(source, b.fmt("let main: () = {{let text = \"{s}\"\n{s}@println(text{s})}}\n", .{ case.text, case.setup, case.slice }));
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI string slices: bounds {s} ({s})", .{ case.name, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ "run", source });
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(1);
            run.expectStdOutEqual("");
            run.expectStdErrEqual("panic: string slice out of bounds\n");
            test_step.dependOn(&run.step);
        }
    }
    const mismatch = "expression does not match the expected type";
    const literal = "literal does not match the annotated type";

    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "start-uint", .source = "let f: (s: str, n: uint) -> str = s[n..]\n", .diagnostic = mismatch },
        .{ .name = "end-uint", .source = "let f: (s: str, n: uint) -> str = s[..n]\n", .diagnostic = mismatch },
        .{ .name = "start-i8", .source = "let f: (s: str, n: i8) -> str = s[n..]\n", .diagnostic = mismatch },
        .{ .name = "end-u8", .source = "let f: (s: str, n: u8) -> str = s[..n]\n", .diagnostic = mismatch },
        .{ .name = "start-rune", .source = "let f: (s: str) -> str = s['a'..]\n", .diagnostic = literal },
        .{ .name = "end-bool", .source = "let f: (s: str) -> str = s[..true]\n", .diagnostic = literal },
        .{ .name = "result-rune", .source = "let f: (s: str) -> rune = s[..]\n", .diagnostic = mismatch },
        .{ .name = "inclusive-end-uint", .source = "let f: (s: str, n: uint) -> str = s[..=n]\n", .diagnostic = mismatch },
        .{ .name = "inclusive-start-uint", .source = "let f: (s: str, n: uint) -> str = s[n..=]\n", .diagnostic = mismatch },
        .{ .name = "inclusive-array", .source = "let f: (s: []u8) -> str = s[..=1]\n", .diagnostic = "slicing requires a string value" },
        .{ .name = "array", .source = "let f: (s: []u8) -> str = s[..]\n", .diagnostic = "slicing requires a string value" },
        .{ .name = "write", .source = "let main: () = {let mut s = \"abc\"\ns[..] = \"x\"}\n", .diagnostic = "assignment target must be a mutable local, its struct field path, or its scalar-array element" },
    }) |case| {
        const source = b.fmt("invalid-{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI string slices: reject {s}", .{case.name});
        run.setCwd(fixtures.getDirectory());
        run.addArgs(&.{ "check", "--semantic", source });
        run.expectExitCode(1);
        run.expectStdOutEqual("");
        run.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&run.step);
    }
}

fn addRuneCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    _ = fixtures.add("runes.nov", @embedFile("examples/runes.nov"));
    _ = fixtures.add("generic.nov",
        \\let Box: (T: type) -> type = struct {
        \\    value: T,
        \\    let Self = @This()
        \\    let get: (self: Self) -> T = self.value
        \\}
        \\let main: () = {
        \\    let mut box = Box(rune).[ value = 'é' ]
        \\    let copy = box
        \\    box.value = '😀'
        \\    @println(copy.get(), box.get())
        \\    let zero = '\0'
        \\    let maximum = '\u{10FFFF}'
        \\    @print(zero, "{zero}{maximum}")
        \\}
        \\
    );
    for ([_][]const u8{ "fmt", "semantic" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI runes: example {s}", .{mode});
        run.setCwd(fixtures.getDirectory());
        run.addArgs(if (std.mem.eql(u8, mode, "fmt")) &.{ "fmt", "--check", "runes.nov" } else &.{ "check", "--semantic", "runes.nov" });
        if (!std.mem.eql(u8, mode, "fmt")) {
            run.addArg("--lib-dir");
            run.addDirectoryArg(library);
        }
        run.expectExitCode(0);
        run.expectStdOutEqual("");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test", "generic" }) |command| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI runes: {s} ({s})", .{ command, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            const generic = std.mem.eql(u8, command, "generic");
            const tests = std.mem.eql(u8, command, "test");
            run.addArgs(&.{ if (tests) "test" else "run", if (generic) "generic.nov" else "runes.nov" });
            if (mode.len != 0) run.addArg(mode);
            run.addArg("--lib-dir");
            run.addDirectoryArg(library);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (generic) "é 😀\n\x00 \x00\xf4\x8f\xbf\xbf" else if (tests)
                "PASS scalars_and_equality\nPASS interpolation_utf8\nPASS copies_and_borrows\nPASS optional_rune\n4 passed; 0 failed; 4 total\n"
            else
                "a é 😀\nRunes: aé😀\nIndexed: é 😀\nRune: h\na é\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    for ([_]struct { name: []const u8, source: []const u8 }{
        .{ .name = "integer-to-rune", .source = "let value: rune = 97\n" },
        .{ .name = "rune-to-integer", .source = "let value: u32 = 'a'\n" },
        .{ .name = "string-to-rune", .source = "let value: rune = \"a\"\n" },
        .{ .name = "rune-to-string", .source = "let value: str = 'a'\n" },
        .{ .name = "argument", .source = "let f: (x: rune) = {}\nlet main: () = f(97)\n" },
        .{ .name = "return", .source = "let f: () -> rune = 97\n" },
        .{ .name = "array", .source = "let main: () = {let values: []rune = [97]}\n" },
        .{ .name = "assignment", .source = "let main: () = {let mut r = 'a'\nr = 97}\n" },
        .{ .name = "as-rune", .source = "let main: () = {let r = @as(rune, 97)}\n" },
        .{ .name = "as-integer", .source = "let main: () = {let r = @as(u32, 'a')}\n" },
        .{ .name = "index-to-integer", .source = "let f: () -> u8 = \"hello\"[0]\n" },
        .{ .name = "equality-integer", .source = "let main: () = @assert('a' == 97)\n" },
        .{ .name = "add", .source = "let main: () = @println('a' + 'b')\n" },
        .{ .name = "order", .source = "let main: () = @println('a' < 'b')\n" },
        .{ .name = "bitwise", .source = "let main: () = @println('a' | 'b')\n" },
        .{ .name = "range", .source = "let main: () = {loop r in 'a'..'z' {}}\n" },
        .{ .name = "invalid-inline-rune", .source = "let main: () = @println(\"{'ab'}\")\n" },
        .{ .name = "empty", .source = "let value = ''\n" },
        .{ .name = "multiple", .source = "let value = 'ab'\n" },
        .{ .name = "combining", .source = "let value = 'é'\n" },
        .{ .name = "invalid-utf8", .source = "let value = '\xff'\n" },
        .{ .name = "surrogate", .source = "let value = '\\u{D800}'\n" },
        .{ .name = "out-of-range", .source = "let value = '\\u{110000}'\n" },
    }) |case| {
        const source = b.fmt("invalid-{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI runes: reject {s}", .{case.name});
        run.setCwd(fixtures.getDirectory());
        run.addArgs(&.{ "check", "--semantic", source });
        run.expectExitCode(1);
        run.expectStdOutEqual("");
        run.expectStdErrMatch(b.fmt("{s}:", .{source}));
        run.expectStdErrMatch("error:");
        test_step.dependOn(&run.step);
    }
    for ([_][]const u8{ "extern", "export" }) |abi| {
        for ([_][]const u8{ "parameter", "return" }) |position| {
            const source = b.fmt("{s}-{s}.nov", .{ abi, position });
            const parameter = std.mem.eql(u8, position, "parameter");
            _ = fixtures.add(source, b.fmt("@[{s}]\nlet f: {s}{s}\n", .{
                abi,
                if (parameter) "(value: rune)" else "() -> rune",
                if (std.mem.eql(u8, abi, "extern")) "" else if (parameter) " = {}" else " = 'a'",
            }));
            for ([_][]const u8{ "check", "emit-c" }) |command| {
                const run = b.addRunArtifact(exe);
                run.step.name = b.fmt("CLI runes: reject {s} {s}", .{ command, source });
                run.setCwd(fixtures.getDirectory());
                run.addArgs(&.{ command, if (std.mem.eql(u8, command, "check")) "--semantic" else "--library", source });
                run.expectExitCode(1);
                run.expectStdOutEqual("");
                run.expectStdErrMatch(b.fmt("{s}:2:5: error:", .{source}));
                run.expectStdErrMatch(b.fmt("{s} ABI supports only", .{abi}));
                test_step.dependOn(&run.step);
            }
        }
    }
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "array-positive", .source = "let main: () = {let values: []rune = ['é']\n@println(values[1])}\n", .diagnostic = "panic: array index out of bounds\n" },
        .{ .name = "array-negative", .source = "let main: () = {let values: []rune = ['é']\n@println(values[-2])}\n", .diagnostic = "panic: array index out of bounds\n" },
        .{ .name = "string-scalar", .source = "let main: () = @println(\"é😀\"[2])\n", .diagnostic = "panic: string index out of bounds\n" },
    }) |case| {
        const source = b.fmt("bounds-{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI runes: bounds {s} ({s})", .{ case.name, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ "run", source });
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(1);
            run.expectStdOutEqual("");
            run.expectStdErrEqual(case.diagnostic);
            test_step.dependOn(&run.step);
        }
    }
}

fn addTestSelectionCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("filtered.nov",
        \\@[test]
        \\let ignored:(value:Missing)=unknown
        \\@[test]
        \\let alpha_fast:()=@println("A")
        \\@[test]
        \\let beta_fast:()=@println("B")
        \\
    );
    _ = fixtures.add("timeout.nov", "@[test]\nlet hang:()=loop{}\n@[test]\nlet fast:()=@println(\"fast\")\n");
    _ = fixtures.add("syntax.nov", "@[test]\nlet ignored:()={let =}\n");
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_]struct { path: []const u8, args: []const []const u8, output: []const u8, code: u8 = 0 }{
            .{ .path = "filtered.nov", .args = &.{ "--filter", "fast", "--timeout-ms", "1000" }, .output = "A\nPASS alpha_fast\nB\nPASS beta_fast\n2 passed; 0 failed; 2 total\n" },
            .{ .path = "filtered.nov", .args = &.{ "--filter", "ALPHA" }, .output = "0 passed; 0 failed; 0 total\n" },
            .{ .path = "timeout.nov", .args = &.{ "--filter", "fast" }, .output = "fast\nPASS fast\n1 passed; 0 failed; 1 total\n" },
            .{ .path = "timeout.nov", .args = &.{ "--timeout-ms", "250" }, .output = "FAIL hang (timeout after 250 ms)\nfast\nPASS fast\n1 passed; 1 failed; 2 total\n", .code = 1 },
        }) |case| {
            const run = b.addRunArtifact(exe);
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ "test", case.path });
            run.addArgs(case.args);
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(case.code);
            run.expectStdOutEqual(case.output);
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    for ([_]struct { args: []const []const u8, diagnostic: []const u8 }{
        .{ .args = &.{ "test", "filtered.nov", "--filter", "ignored" }, .diagnostic = "error:" },
        .{ .args = &.{ "test", "syntax.nov", "--filter", "not-present" }, .diagnostic = "error:" },
        .{ .args = &.{ "run", "timeout.nov", "--filter", "fast" }, .diagnostic = "--filter requires the test command" },
        .{ .args = &.{ "check", "timeout.nov", "--timeout-ms", "100" }, .diagnostic = "--timeout-ms requires the test command" },
    }) |case| {
        const reject = b.addRunArtifact(exe);
        reject.setCwd(fixtures.getDirectory());
        reject.addArgs(case.args);
        reject.expectExitCode(1);
        reject.expectStdOutEqual("");
        reject.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&reject.step);
    }
    for ([_][]const u8{ "", "0", "-1", "1.5", "4294967296" }) |raw| {
        const reject = b.addRunArtifact(exe);
        reject.setCwd(fixtures.getDirectory());
        reject.addArgs(&.{ "test", "timeout.nov", "--timeout-ms", raw });
        reject.expectExitCode(1);
        reject.expectStdOutEqual("");
        reject.expectStdErrMatch("--timeout-ms requires a positive decimal integer");
        test_step.dependOn(&reject.step);
    }
}

fn addLanguageTestCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    _ = fixtures.add("tests.nov", @embedFile("examples/tests.nov"));
    _ = fixtures.add("imported.nov", "@[public]\n@[export(\"test_helper\")]\nlet helper: () -> i32 = 42\n" ++
        "@[test]\nlet panic_test: () = @panic(\"imported test ran\")\n" ++
        "@[test]\nlet ignored: (x: Missing) -> Missing = {let local = @import(\"missing.nov\")\nunknown}\n");
    _ = fixtures.add("ordered.nov", "let api = @import(\"imported.nov\")\n" ++
        "@[test]\nlet zebra: () = @assert(api.helper() == 42)\n" ++
        "let main: () = @panic(\"main ran\")\n" ++
        "@[test]\nlet alpha: () = {}\n");
    _ = fixtures.add("excluded.nov", "let api = @import(\"imported.nov\")\n" ++
        "let G: (T: type) -> type = struct { value: T, }\n" ++
        "@[test]\nlet ignored: (x: Missing) -> G(Missing) = {let local = @import(\"absent.nov\")\n@assert(false)\nunknown}\n" ++
        "let main: () = @println(api.helper())\n");
    _ = fixtures.add("syntax-error.nov", "@[test]\nlet broken: () = {let x = }\n");
    _ = fixtures.add("empty.nov", "let helper: () -> i32 = 42\n");
    _ = fixtures.add("failures.nov", "@[extern]\nlet exit: (status: i32)\n" ++
        "@[test]\nlet before: () = {}\n" ++
        "@[test]\nlet panic_test: () = @panic(\"intentional panic\")\n" ++
        "@[test]\nlet assertion: () = @assert(false, \"intentional assertion\")\n" ++
        "@[test]\nlet no_message: () = @assert(false)\n" ++
        "@[test]\nlet nonzero: () = exit(7)\n" ++
        "@[test]\nlet after: () = {}\n");
    _ = fixtures.add("eager.nov", "let condition: () -> bool = {@print(\"C\")\ntrue}\n" ++
        "let message: () -> str = {@print(\"M\")\n\"unused\"}\n" ++
        "@[test]\nlet eager: () = {let unit: void = @assert(condition(), message())\n@assert(true)\n@print(\"T\")}\n");
    _ = fixtures.add("isolation.nov", "@[extern]\nlet rand: () -> i32\n@[extern]\nlet srand: (seed: u32)\n" ++
        "@[test]\nlet first: () = {let initial = rand()\nsrand(1)\n@assert(rand() == initial)}\n" ++
        "@[test]\nlet second: () = {let initial = rand()\nsrand(1)\n@assert(rand() == initial)}\n");
    const Case = struct {
        args: []const []const u8,
        output: []const u8 = "",
        diagnostic: ?[]const u8 = null,
        code: u8 = 0,
    };
    const cases = [_]Case{
        .{ .args = &.{"test"}, .code = 1, .diagnostic = "test requires exactly one file" },
        .{ .args = &.{ "test", "empty.nov", "empty.nov" }, .code = 1, .diagnostic = "test requires exactly one file" },
        .{ .args = &.{ "test", "empty.nov", "extra" }, .code = 1, .diagnostic = "test requires exactly one file" },
        .{ .args = &.{ "test", "empty.nov", "--debug", "--release" }, .code = 1, .diagnostic = "--debug and --release are mutually exclusive" },
        .{ .args = &.{ "test", "empty.nov", "-o", "out" }, .code = 1, .diagnostic = "--output requires the build command" },
        .{ .args = &.{ "test", "empty.nov", "--library" }, .code = 1, .diagnostic = "--library requires the emit-c command" },
        .{ .args = &.{ "test", "empty.nov", "--check" }, .code = 1, .diagnostic = "--check requires the fmt command" },
        .{ .args = &.{ "test", "empty.nov", "--exclude", "x" }, .code = 1, .diagnostic = "--exclude requires the fmt command" },
        .{ .args = &.{ "test", "empty.nov", "--semantic" }, .code = 1, .diagnostic = "--semantic requires the check command" },
        .{ .args = &.{ "test", "empty.nov", "--cc", "" }, .code = 1, .diagnostic = "--cc requires a nonempty executable name or path" },
        .{ .args = &.{ "test", "empty.nov", "--lib-dir", "" }, .code = 1, .diagnostic = "--lib-dir requires a nonempty path" },
        .{ .args = &.{ "test", "missing.nov" }, .code = 1, .diagnostic = "FileNotFound" },
        .{ .args = &.{ "check", "excluded.nov" } },
        .{ .args = &.{ "check", "--semantic", "excluded.nov" } },
        .{ .args = &.{ "check", "syntax-error.nov" }, .code = 1, .diagnostic = "error:" },
        .{ .args = &.{ "test", "syntax-error.nov" }, .code = 1, .diagnostic = "error:" },
        .{ .args = &.{ "fmt", "--check", "tests.nov" } },
    };
    for (cases) |case| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI language tests: {s} ({d} arguments)", .{ case.args[0], case.args.len });
        run.setCwd(fixtures.getDirectory());
        run.addArgs(case.args);
        run.expectExitCode(case.code);
        run.expectStdOutEqual(case.output);
        if (case.diagnostic) |message| run.expectStdErrMatch(message) else run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    const invalid = [_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "signature", .source = "@[test]\nlet bad: (x: i32) = {}\n", .diagnostic = "test function must have type () -> void" },
        .{ .name = "return-type", .source = "@[test]\nlet bad: () -> i32 = 1\n", .diagnostic = "test function must have type () -> void" },
        .{ .name = "call", .source = "@[test]\nlet hidden: () = {}\nlet helper: () = hidden()\n", .diagnostic = "test functions cannot be referenced or called" },
        .{ .name = "extract", .source = "@[test]\nlet hidden: () = {}\nlet helper: () = {let f = hidden\n}\n", .diagnostic = "test functions cannot be referenced or called" },
        .{ .name = "test-call", .source = "@[test]\nlet hidden: () = {}\n@[test]\nlet caller: () = hidden()\n", .diagnostic = "test functions cannot be referenced or called" },
        .{ .name = "public-reference", .source = "let api = @import(\"public-test.nov\")\n@[test]\nlet caller: () = api.hidden()\n", .diagnostic = "test functions cannot be referenced or called" },
        .{ .name = "condition", .source = "@[test]\nlet bad: () = @assert(1)\n", .diagnostic = "literal does not match the annotated type" },
        .{ .name = "message", .source = "@[test]\nlet bad: () = @assert(true, 1)\n", .diagnostic = "literal does not match the annotated type" },
        .{ .name = "arity-zero", .source = "@[test]\nlet bad: () = @assert()\n", .diagnostic = "error:" },
        .{ .name = "arity-three", .source = "@[test]\nlet bad: () = @assert(true, \"x\", \"y\")\n", .diagnostic = "error:" },
        .{ .name = "compile-error", .source = "@[test]\nlet before: () = @println(\"must not run\")\n@[test]\nlet bad: () = missing()\n", .diagnostic = "unknown name 'missing'" },
        .{ .name = "width", .source = "@[test]\nlet bad: () = {let n: i128 = 1\n@assert(n == 1)}\n", .diagnostic = "C backend only supports 8, 16, 32 and 64-bit integers" },
    };
    _ = fixtures.add("public-test.nov", "@[public]\n@[test]\nlet hidden: () = {}\n");
    _ = fixtures.add("public-normal.nov", "let api = @import(\"public-test.nov\")\nlet helper: () = api.hidden()\n");
    for (invalid) |case| {
        const source = b.fmt("invalid-{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI language tests: reject {s}", .{case.name});
        run.setCwd(fixtures.getDirectory());
        run.addArgs(&.{ "test", source });
        run.expectExitCode(1);
        run.expectStdOutEqual("");
        run.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&run.step);
        if (std.mem.eql(u8, case.name, "call") or std.mem.eql(u8, case.name, "extract") or std.mem.eql(u8, case.name, "public-reference")) {
            const normal = b.addRunArtifact(exe);
            normal.step.name = b.fmt("CLI language tests: normal code rejects {s}", .{case.name});
            normal.setCwd(fixtures.getDirectory());
            normal.addArgs(&.{ "check", "--semantic", if (std.mem.eql(u8, case.name, "public-reference")) "public-normal.nov" else source });
            normal.expectExitCode(1);
            normal.expectStdOutEqual("");
            normal.expectStdErrMatch(case.diagnostic);
            test_step.dependOn(&normal.step);
        }
    }
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        const runtime_cases = [_]Case{
            .{ .args = &.{ "test", "tests.nov" }, .output = "PASS addition\nPASS array_copies\nPASS option_value\n3 passed; 0 failed; 3 total\n" },
            .{ .args = &.{ "test", "ordered.nov" }, .output = "PASS zebra\nPASS alpha\n2 passed; 0 failed; 2 total\n" },
            .{ .args = &.{ "test", "empty.nov" }, .output = "0 passed; 0 failed; 0 total\n" },
            .{ .args = &.{ "test", "eager.nov" }, .output = "CMTPASS eager\n1 passed; 0 failed; 1 total\n" },
            .{ .args = &.{ "test", "isolation.nov" }, .output = "PASS first\nPASS second\n2 passed; 0 failed; 2 total\n" },
            .{ .args = &.{ "test", "failures.nov" }, .code = 1, .output = "PASS before\nFAIL panic_test (exit 1)\nFAIL assertion (exit 1)\nFAIL no_message (exit 1)\nFAIL nonzero (exit 7)\nPASS after\n2 passed; 4 failed; 6 total\n", .diagnostic = "failures.nov:8:21: assertion failed: intentional assertion\n" },
            .{ .args = &.{ "run", "excluded.nov" }, .output = "42\n" },
        };
        for (runtime_cases) |case| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI language tests: {s} {s} {s}", .{ case.args[0], case.args[1], mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(case.args);
            if (mode.len != 0) run.addArg(mode);
            if (std.mem.eql(u8, case.args[1], "tests.nov")) run.addArgs(&.{ "--cc", "/nov-missing/explicit" });
            run.addArg("--lib-dir");
            run.addDirectoryArg(library);
            run.expectExitCode(case.code);
            run.expectStdOutEqual(case.output);
            if (case.diagnostic) |message| {
                run.expectStdErrMatch(message);
                run.expectStdErrMatch("panic: intentional panic");
                run.expectStdErrMatch("failures.nov:10:22: assertion failed\n");
            } else run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    const emit = b.addRunArtifact(exe);
    emit.setCwd(fixtures.getDirectory());
    emit.addArgs(&.{ "emit-c", "excluded.nov" });
    emit.expectExitCode(0);
    emit.expectStdErrEqual("");
    const emitted = emit.captureStdOut(.{});
    const inspect = b.addCheckFile(emitted, .{ .expected_matches = &.{"int main("} });
    test_step.dependOn(&inspect.step);
    const compile = b.addRunArtifact(exe);
    compile.setCwd(fixtures.getDirectory());
    compile.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
    compile.setEnvironmentVariable("NOV_CC", "");
    compile.setEnvironmentVariable("CC", "");
    compile.addArgs(&.{ "build", "excluded.nov", "-o" });
    const binary = compile.addOutputFileArg(if (exe.root_module.resolved_target.?.result.os.tag == .windows) "excluded.exe" else "excluded");
    compile.expectExitCode(0);
    compile.expectStdOutEqual("");
    compile.expectStdErrEqual("");
    const execute = std.Build.Step.Run.create(b, "CLI language tests: normal build excludes tests");
    execute.addFileArg(binary);
    execute.expectExitCode(0);
    execute.expectStdOutEqual("42\n");
    execute.expectStdErrEqual("");
    test_step.dependOn(&execute.step);
    const package = b.addWriteFiles();
    const installed_exe = package.addCopyFile(exe.getEmittedBin(), b.fmt("bin/{s}", .{exe.out_filename}));
    _ = package.addCopyDirectory(b.path("lib"), "lib/nov", .{});
    for ([_]bool{ false, true }) |override| {
        const installed = std.Build.Step.Run.create(b, b.fmt("CLI language tests: installed std override={}", .{override}));
        installed.addFileArg(installed_exe);
        installed.setCwd(fixtures.getDirectory());
        installed.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        installed.setEnvironmentVariable("NOV_CC", "");
        installed.setEnvironmentVariable("CC", "");
        installed.addArgs(&.{ "test", "tests.nov" });
        if (override) installed.addArgs(&.{ "--lib-dir", "missing-library" });
        installed.expectExitCode(if (override) 1 else 0);
        installed.expectStdOutEqual(if (override) "" else "PASS addition\nPASS array_copies\nPASS option_value\n3 passed; 0 failed; 3 total\n");
        if (override) installed.expectStdErrMatch("cannot load imported source: FileNotFound") else installed.expectStdErrEqual("");
        test_step.dependOn(&installed.step);
    }
}

fn addGenericClosureCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("generic-closures.nov", @embedFile("examples/generic-closures.nov"));
    _ = fixtures.add("factory.nov",
        \\let secret: int = 10
        \\@[public]
        \\let make: (initial: int) -> (bool) -> int = {
        \\    let mut count = initial
        \\    let local: (ignored: `T) -> int = {
        \\        count += 1
        \\        count + secret
        \\    }
        \\    @assert(local("warmup") == initial + 1 + secret)
        \\    let result: (bool) -> int = local
        \\    result
        \\}
        \\
    );
    _ = fixtures.add("imported.nov",
        \\let factory = @import("factory.nov")
        \\let secret: int = 1000
        \\let verify: () = {
        \\    let first = factory.make(1)
        \\    let second = factory.make(20)
        \\    let copy = first
        \\    @assert(first(true) == 13 and copy(false) == 14)
        \\    @assert(second(true) == 32 and first(false) == 15)
        \\}
        \\@[test]
        \\let imported_local_factory: () = verify()
        \\let main: () = verify()
        \\
    );
    for ([_][]const u8{ "generic-closures.nov", "factory.nov", "imported.nov" }) |source| {
        for ([_][]const u8{ "fmt", "semantic" }) |command| {
            const check = b.addRunArtifact(exe);
            check.step.name = b.fmt("CLI generic closures {s}: {s}", .{ command, source });
            check.setCwd(fixtures.getDirectory());
            check.addArgs(if (std.mem.eql(u8, command, "fmt")) &.{ "fmt", "--check", source } else &.{ "check", "--semantic", source });
            check.expectExitCode(0);
            check.expectStdOutEqual("");
            check.expectStdErrEqual("");
            test_step.dependOn(&check.step);
        }
    }
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "generic-closures.nov", "imported.nov" }) |source| {
            for ([_][]const u8{ "run", "test" }) |command| {
                const run = b.addRunArtifact(exe);
                run.step.name = b.fmt("CLI generic closures {s}: {s} ({s})", .{ command, source, mode });
                run.setCwd(fixtures.getDirectory());
                run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                run.setEnvironmentVariable("NOV_CC", "");
                run.setEnvironmentVariable("CC", "");
                run.addArgs(&.{ command, source });
                if (mode.len != 0) run.addArg(mode);
                const imported = std.mem.eql(u8, source, "imported.nov");
                const tests = std.mem.eql(u8, command, "test");
                run.expectExitCode(0);
                run.expectStdOutEqual(if (tests)
                    (if (imported) "PASS imported_local_factory\n1 passed; 0 failed; 1 total\n" else "PASS direct_and_contextual\nPASS callback_inference\nPASS shared_mutable_capture\nPASS recursive_array_snapshots\nPASS loop_instances_and_escape\nPASS nested_template_substitution\n6 passed; 0 failed; 6 total\n")
                else if (imported)
                    ""
                else
                    "generic closures: all six checks passed\n");
                run.expectStdErrEqual("");
                test_step.dependOn(&run.step);
            }
        }
    }
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "unspecialized-escape", .source = "let main:()={let leaked={let f=(x:`T)->`T{x}\nf}\n}", .diagnostic = "cannot infer every generic function type variable" },
        .{ .name = "local-recursion", .source = "let main:()={let f:(x:`T)->`T=f(x)\n@print(f(true))}", .diagnostic = "unknown name" },
        .{ .name = "closure-recursion", .source = "let main:()={let f=(x:`T)->`T{f(x)}\n@print(f(true))}", .diagnostic = "unknown name" },
        .{ .name = "integer-default", .source = "let main:()={let f:(x:`T)->`T=x\nlet value=f(1)\n@print()}", .diagnostic = "" },
        .{ .name = "conflict", .source = "let main:()={let f=(x:`T,y:`T)->`T{x}\n@print(f(true,@as(int,1)))}", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "callback-conflict", .source = "let main:()={let f=(x:`T)->`T{x}\nlet cb:(uint)->int=f\n}", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "scope", .source = "let main:()={let f=(x:`T)->`T{{let hidden:int=1\n}\nx}\n@print(f(true))\n@print(hidden)}", .diagnostic = "unknown name" },
    }) |case| {
        const source = b.fmt("reject-{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI generic closures check: {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source });
        check.expectExitCode(if (case.diagnostic.len == 0) 0 else 1);
        check.expectStdOutEqual("");
        if (case.diagnostic.len == 0) check.expectStdErrEqual("") else check.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&check.step);
    }
}

fn addQuotedInterpolationCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("quoted-interpolation.nov", @embedFile("examples/quoted-interpolation.nov"));
    for ([_][]const u8{ "fmt", "semantic" }) |mode| {
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI quoted interpolation: {s}", .{mode});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(if (std.mem.eql(u8, mode, "fmt")) &.{ "fmt", "--check", "quoted-interpolation.nov" } else &.{ "check", "--semantic", "quoted-interpolation.nov" });
        check.expectExitCode(0);
        check.expectStdOutEqual("");
        check.expectStdErrEqual("");
        test_step.dependOn(&check.step);
    }
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test" }) |command| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI quoted interpolation: {s} {s}", .{ command, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ command, "quoted-interpolation.nov" });
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (std.mem.eql(u8, command, "run")) "a\nHello Ada!\nBraces: {} and é\nNested: hello world\nEscapes: \" \\\n" else "PASS quoted_literals_and_nested_interpolation\nPASS quoted_arguments_evaluate_once_in_order\n2 passed; 0 failed; 2 total\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
}

fn addFloatCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    _ = fixtures.add("floats.nov", @embedFile("examples/floats.nov"));
    for ([_][]const u8{ "fmt", "semantic" }) |mode| {
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI floats: {s}", .{mode});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(if (std.mem.eql(u8, mode, "fmt")) &.{ "fmt", "--check", "floats.nov" } else &.{ "check", "--semantic", "floats.nov" });
        if (!std.mem.eql(u8, mode, "fmt")) {
            check.addArg("--lib-dir");
            check.addDirectoryArg(library);
        }
        check.expectExitCode(0);
        check.expectStdOutEqual("");
        check.expectStdErrEqual("");
        test_step.dependOn(&check.step);
    }
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test" }) |command| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI floats: {s} {s}", .{ command, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ command, "floats.nov", "--lib-dir" });
            run.addDirectoryArg(library);
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (std.mem.eql(u8, command, "run")) "Weighted mean: 2.5\nValues: [1, 0.25, 0.75]\nSpecial: inf -inf nan -0\n" else "PASS inference_and_target_rounding\nPASS ieee_comparisons_and_membership\nPASS mutation_copies_and_generic_containers\n3 passed; 0 failed; 3 total\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "integer-variable", .source = "let main:()={let x=1\nlet y:f64=x\n}", .diagnostic = "expression does not match" },
        .{ .name = "float-width", .source = "let main:()={let x:f32=1\nlet y:f64=x\n}", .diagnostic = "expression does not match" },
        .{ .name = "integer-context", .source = "let x:int=1.5", .diagnostic = "literal does not match" },
        .{ .name = "overflow", .source = "let x:f32=3.5e38", .diagnostic = "float literal does not fit" },
        .{ .name = "default-overflow", .source = "let x=1e400", .diagnostic = "float literal does not fit" },
        .{ .name = "modulo", .source = "let main:()={let x=1.5%1.0\n}", .diagnostic = "operator is not supported" },
        .{ .name = "bitwise", .source = "let main:()={let x=1.5&1.0\n}", .diagnostic = "operator is not supported" },
        .{ .name = "range", .source = "let main:()={let x=1.5..2.5\n}", .diagnostic = "literal does not match" },
        .{ .name = "index", .source = "let main:()={let a=[1]\n@println(a[0.0])}", .diagnostic = "literal does not match" },
        .{ .name = "f16", .source = "let x:f16=1.0", .diagnostic = "this type is not supported" },
        .{ .name = "f80", .source = "let x:f80=1.0", .diagnostic = "this type is not supported" },
        .{ .name = "f128", .source = "let x:f128=1.0", .diagnostic = "this type is not supported" },
        .{ .name = "foreign-macro", .source = "@[export(\"FLT_RADIX\")]\nlet f:(x:f32)->f32=x", .diagnostic = "export name must" },
    }) |case| {
        const source = b.fmt("invalid-float-{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI floats reject: {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source });
        check.expectExitCode(1);
        check.expectStdOutEqual("");
        check.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&check.step);
    }
}

fn addTypeOfExpressionCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    for ([_][]const u8{ "fmt", "semantic" }) |mode| {
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI TypeOf expressions: {s}", .{mode});
        check.addArgs(if (std.mem.eql(u8, mode, "fmt")) &.{ "fmt", "--check" } else &.{ "check", "--semantic" });
        check.addFileArg(b.path("examples/type-of-expressions.nov"));
        if (!std.mem.eql(u8, mode, "fmt")) {
            check.addArg("--lib-dir");
            check.addDirectoryArg(b.path("lib"));
        }
        check.expectExitCode(0);
        check.expectStdOutEqual("");
        check.expectStdErrEqual("");
        test_step.dependOn(&check.step);
    }
    const package = b.addWriteFiles();
    const installed_exe = package.addCopyFile(exe.getEmittedBin(), b.fmt("bin/{s}", .{exe.out_filename}));
    _ = package.addCopyDirectory(b.path("lib"), "lib/nov", .{});
    const source = package.add("type-of-expressions.nov", @embedFile("examples/type-of-expressions.nov"));
    for ([_]bool{ false, true }) |installed| {
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            for ([_][]const u8{ "run", "test" }) |command| {
                const run = if (installed) std.Build.Step.Run.create(b, "CLI TypeOf expressions: relocated install") else b.addRunArtifact(exe);
                run.step.name = b.fmt("CLI TypeOf expressions: {s} ({s}, installed={})", .{ command, mode, installed });
                if (installed) run.addFileArg(installed_exe);
                run.setCwd(package.getDirectory());
                run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                run.setEnvironmentVariable("NOV_CC", "");
                run.setEnvironmentVariable("CC", "");
                run.addArg(command);
                run.addFileArg(source);
                if (mode.len != 0) run.addArg(mode);
                if (!installed) {
                    run.addArg("--lib-dir");
                    run.addDirectoryArg(b.path("lib"));
                }
                run.expectExitCode(0);
                run.expectStdOutEqual(if (std.mem.eql(u8, command, "test"))
                    "PASS literals_calls_fields_and_indices\nPASS arrays_slices_blocks_if_and_match\nPASS operands_do_not_execute_or_store\nPASS references_borrows_and_type_only_captures\nPASS closures_and_inferred_generic_calls\n5 passed; 0 failed; 5 total\n"
                else
                    b.fmt("Literals: i{0d} f64 rune\nExpressions: i{0d} i{0d} str\nGeneric: []str\n", .{@bitSizeOf(usize)}));
                run.expectStdErrEqual("");
                test_step.dependOn(&run.step);
            }
        }
    }
}

fn addTypeOfCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    for ([_][]const u8{ "fmt", "semantic" }) |mode| {
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI TypeOf: {s}", .{mode});
        check.addArgs(if (std.mem.eql(u8, mode, "fmt")) &.{ "fmt", "--check" } else &.{ "check", "--semantic" });
        check.addFileArg(b.path("examples/type-of.nov"));
        check.expectExitCode(0);
        check.expectStdOutEqual("");
        check.expectStdErrEqual("");
        test_step.dependOn(&check.step);
    }
    const fixtures = b.addWriteFiles();
    const library = fixtures.add("library/std.nov",
        \\@[public]
        \\let value: u8 = 7
        \\@[public]
        \\let identity: (item: u8) -> @TypeOf(item) = item
        \\@[public]
        \\let Alias = u8
        \\let secret: u8 = 9
        \\
    );
    _ = fixtures.add("imports.nov",
        \\let file = @import("library/std.nov")
        \\let named = @import("std")
        \\let alias = file
        \\let Count = @TypeOf(alias.value)
        \\let verify: () = {
        \\    let a: @TypeOf(file.value) = 1
        \\    let b: @TypeOf(named.value) = 2
        \\    let c: Count = 3
        \\    let callback: @TypeOf(alias.identity) = named.identity
        \\    @assert(a + b == c and callback(c) == 3)
        \\    @assert("{@TypeOf(alias.value)}" == "u8")
        \\}
        \\@[test]
        \\let imported_value_queries: () = verify()
        \\let main: () = {
        \\    verify()
        \\    @println(@TypeOf(file.value), @TypeOf(named.value), @TypeOf(alias.identity))
        \\}
        \\
    );
    const imports_check = b.addRunArtifact(exe);
    imports_check.step.name = "CLI TypeOf: file, named and module alias imports semantic";
    imports_check.setCwd(fixtures.getDirectory());
    imports_check.addArgs(&.{ "check", "--semantic", "imports.nov", "--lib-dir" });
    imports_check.addDirectoryArg(library.dirname());
    imports_check.expectExitCode(0);
    imports_check.expectStdOutEqual("");
    imports_check.expectStdErrEqual("");
    test_step.dependOn(&imports_check.step);
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test" }) |command| {
            for ([_]bool{ false, true }) |imports| {
                const run = b.addRunArtifact(exe);
                run.step.name = b.fmt("CLI TypeOf: {s} {s} ({s})", .{ if (imports) "imports" else "example", command, mode });
                run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                run.setEnvironmentVariable("NOV_CC", "");
                run.setEnvironmentVariable("CC", "");
                run.addArg(command);
                if (imports) {
                    run.setCwd(fixtures.getDirectory());
                    run.addArgs(&.{ "imports.nov", "--lib-dir" });
                    run.addDirectoryArg(library.dirname());
                } else run.addFileArg(b.path("examples/type-of.nov"));
                if (mode.len != 0) run.addArg(mode);
                run.expectExitCode(0);
                run.expectStdOutEqual(if (std.mem.eql(u8, command, "test"))
                    (if (imports) "PASS imported_value_queries\n1 passed; 0 failed; 1 total\n" else "PASS annotations_ascriptions_and_generic_results\nPASS mutable_bindings_and_type_only_outer_references\n2 passed; 0 failed; 2 total\n")
                else if (imports)
                    "u8 u8 (u8) -> u8\n"
                else
                    b.fmt("Types: i{0d} []i{0d} Box(i{0d}) (i{0d}) -> i{0d}\nValues: 42 7 [1, 2] 9\n", .{@bitSizeOf(usize)}));
                run.expectStdErrEqual("");
                test_step.dependOn(&run.step);
            }
        }
    }
    const invalid_operand = "@TypeOf requires a supported value expression; types, namespaces and unspecialized templates are not value operands";
    const not_a_value = "expected a runtime value, found a type alias";
    const wrong_arity = "argument count does not match the function signature";
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "local-alias", .source = "let main:()={let x=1\nlet T=@TypeOf(x)\n}\n", .diagnostic = not_a_value },
        .{ .name = "runtime-argument", .source = "let consume:(x:int)= {}\nlet main:()={let x=1\nconsume(@TypeOf(x))}\n", .diagnostic = not_a_value },
        .{ .name = "call-operand", .source = "let f:(value:int)->int=value\nlet main:()={let x:@TypeOf(f())=1\n}\n", .diagnostic = wrong_arity },
        .{ .name = "unknown", .source = "let main:()={let x:@TypeOf(missing)=1\n}\n", .diagnostic = "unknown name 'missing'" },
        .{ .name = "primitive", .source = "let main:()={let x:@TypeOf(int)=1\n}\n", .diagnostic = invalid_operand },
        .{ .name = "type-alias", .source = "let T=int\nlet main:()={let x:@TypeOf(T)=1\n}\n", .diagnostic = not_a_value },
        .{ .name = "namespace", .source = "let lib=@import(\"library/std.nov\")\nlet main:()={let x:@TypeOf(lib)=1\n}\n", .diagnostic = not_a_value },
        .{ .name = "imported-type", .source = "let lib=@import(\"library/std.nov\")\nlet main:()={let x:@TypeOf(lib.Alias)=1\n}\n", .diagnostic = not_a_value },
        .{ .name = "zero-arguments", .source = "let main:()={let x:@TypeOf()=1\n}\n", .diagnostic = wrong_arity },
        .{ .name = "two-arguments", .source = "let main:()={let x=1\nlet y:@TypeOf(x,x)=1\n}\n", .diagnostic = wrong_arity },
        .{ .name = "private-file-value", .source = "let lib=@import(\"library/std.nov\")\nlet main:()={let x:@TypeOf(lib.secret)=1\n}\n", .diagnostic = not_a_value },
        .{ .name = "private-named-value", .source = "let lib=@import(\"std\")\nlet main:()={let x:@TypeOf(lib.secret)=1\n}\n", .diagnostic = not_a_value },
        .{ .name = "expression-unknown", .source = "let main:()={let x:@TypeOf(1+missing)=1\n}\n", .diagnostic = "unknown name 'missing'" },
        .{ .name = "template", .source = "let identity:(x:`T)->`T=x\nlet main:()={let x:@TypeOf(identity)=1\n}\n", .diagnostic = not_a_value },
    }) |case| {
        const source = b.fmt("{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        for ([_]bool{ false, true }) |emit| {
            const check = b.addRunArtifact(exe);
            check.step.name = b.fmt("CLI TypeOf rejects: {s} ({s})", .{ case.name, if (emit) "emit-c" else "semantic" });
            check.setCwd(fixtures.getDirectory());
            check.addArgs(if (emit) &.{"emit-c"} else &.{ "check", "--semantic" });
            check.addArgs(&.{ source, "--lib-dir" });
            check.addDirectoryArg(library.dirname());
            check.expectExitCode(1);
            check.expectStdOutEqual("");
            check.expectStdErrMatch(b.fmt("error: {s}", .{case.diagnostic}));
            test_step.dependOn(&check.step);
        }
    }
}

fn addIntegerDefaultCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    for ([_][]const u8{ "fmt", "semantic" }) |mode| {
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI integer defaults: {s}", .{mode});
        check.addArgs(if (std.mem.eql(u8, mode, "fmt")) &.{ "fmt", "--check" } else &.{ "check", "--semantic" });
        check.addFileArg(b.path("examples/integer-defaults.nov"));
        check.expectExitCode(0);
        check.expectStdOutEqual("");
        check.expectStdErrEqual("");
        test_step.dependOn(&check.step);
    }
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test" }) |command| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI integer defaults: {s} ({s})", .{ command, mode });
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArg(command);
            run.addFileArg(b.path("examples/integer-defaults.nov"));
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (std.mem.eql(u8, command, "test"))
                "PASS defaults_and_control_flow\nPASS constraints_precede_defaulting\n2 passed; 0 failed; 2 total\n"
            else
                "Defaults: 42 -2 3 42\nArray: [1, 2, 3]\nContext: [1, 7] 1\nGeneric: 42 3 3\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    const fixtures = b.addWriteFiles();
    const native_overflow = b.fmt("{d}", .{@as(u128, 1) << (@bitSizeOf(usize) - 1)});
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "nonliteral-narrowing", .source = "let main:()={let value=1\nlet narrow:u8=value\n}\n", .diagnostic = "expression does not match the expected type" },
        .{ .name = "local-overflow", .source = b.fmt("let main:()={{let value={s}\n}}\n", .{native_overflow}), .diagnostic = "integer literal does not fit" },
        .{ .name = "global-overflow", .source = b.fmt("let value={s}\n", .{native_overflow}), .diagnostic = "integer literal does not fit" },
        .{ .name = "float", .source = "let main:()={let value=1.5\n}\n", .diagnostic = "" },
        .{ .name = "empty-array", .source = "let main:()={let values=[]\n}\n", .diagnostic = "array literal needs an expected []T type" },
        .{ .name = "explicit-wide", .source = b.fmt("let value:u64={s}\nlet main:()={{let local:u64={s}\n}}\n", .{ native_overflow, native_overflow }), .diagnostic = "" },
    }) |case| {
        const source = b.fmt("{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI integer defaults check: {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source });
        check.expectExitCode(if (case.diagnostic.len == 0) 0 else 1);
        check.expectStdOutEqual("");
        if (case.diagnostic.len == 0) check.expectStdErrEqual("") else check.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&check.step);
    }
}

fn addGenericFunctionCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("generic-functions.nov", @embedFile("examples/generic-functions.nov"));
    _ = fixtures.add("contextual-value.nov",
        \\let identity: (x: `T) -> `T = x
        \\let verify: () = {
        \\    let cb: (uint) -> uint = identity
        \\    @assert(cb(42) == 42)
        \\    let local: (value: `T) -> `T = value
        \\    let closure = (value: `U) -> `U {
        \\        value
        \\    }
        \\    let typed: (bool) -> bool = local
        \\    let captured: (str) -> str = closure
        \\    @assert(local(true) and closure(false) == false and typed(true))
        \\    @assert(captured("local") == "local")
        \\}
        \\@[test]
        \\let contextual_value: () = verify()
        \\let main: () = verify()
        \\
    );
    _ = fixtures.add("library.nov",
        \\let secret: uint = 9
        \\let hidden: (x: `T) -> `T = x
        \\@[public]
        \\let exposed = hidden
        \\@[public]
        \\let add: (x: `a, y: `b) -> `c = x + y
        \\@[public]
        \\let alias = add
        \\@[public]
        \\let read: (x: `T) -> uint = secret
        \\@[public]
        \\let Box: (T: type) -> type = struct {
        \\    value: T,
        \\}
        \\@[public]
        \\let identity: (x: `T) -> `T = x
        \\@[public]
        \\let NS = struct {
        \\    @[public]
        \\    let id: (x: `T) -> `T = x
        \\    let private: (x: `T) -> `T = x
        \\}
        \\@[public]
        \\let G: (T: type) -> type = struct {
        \\    let sibling: (x: `U, y: T) -> `U = x
        \\    @[public]
        \\    let call: (x: `U, y: T) -> `V = sibling(x, y)
        \\}
        \\; Uncalled templates are not specialized or body-checked.
        \\@[public]
        \\let unused: (x: `T) -> `T = absent
        \\
    );
    _ = fixtures.add("api.nov", "let library = @import(\"library.nov\")\n@[public]\nlet sum = library.alias\n@[public]\nlet Box = library.Box\n@[public]\nlet namespace = library.NS\n");
    _ = fixtures.add("imported.nov",
        \\let library = @import("library.nov")
        \\let same = @import("./library.nov")
        \\let api = @import("api.nov")
        \\let sum = api.sum
        \\let method = api.namespace.id
        \\let secret: uint = 100
        \\let apply: (f: (`T) -> `U, x: `T) -> `U = f(x)
        \\let verifyCallbacks: () = {
        \\    let cb: (str) -> str = library.identity
        \\    let alias: (uint, uint) -> uint = sum
        \\    let ns: (bool) -> bool = method
        \\    let static: (str, uint) -> str = library.G(uint).call
        \\    let exposed: (bool) -> bool = library.exposed
        \\    let read: (str) -> uint = library.read
        \\    @assert(cb("callback") == "callback" and alias(20, 22) == 42)
        \\    @assert(ns(true) and exposed(true) and read("scope") == 9)
        \\    @assert(static("static", 1) == "static")
        \\    @assert(@as((bool) -> bool, same.identity)(true))
        \\    @assert(apply(library.identity, "inferred") == "inferred")
        \\}
        \\@[test]
        \\let imported_callbacks: () = verifyCallbacks()
        \\let main: () = {
        \\    let a: uint = sum(1, 2)
        \\    let b = library.add(@as(uint, 3), 4)
        \\    let c: uint = same.alias(5, 6)
        \\    let box: api.Box(str) = library.identity(same.Box(str).[ value = "canonical" ])
        \\    @println(a, b, c, method(true), library.NS.id(false), box.value)
        \\    @println(library.read("scope"), library.G(uint).call("sibling", 1))
        \\    verifyCallbacks()
        \\    let cb: (bool) -> bool = api.namespace.id
        \\    @println(cb(true), apply(same.identity, @as(uint, 42)))
        \\}
        \\
    );
    for ([_][]const u8{ "generic-functions.nov", "contextual-value.nov", "library.nov", "api.nov", "imported.nov" }) |source| {
        const fmt = b.addRunArtifact(exe);
        fmt.step.name = b.fmt("CLI generic functions fmt: {s}", .{source});
        fmt.setCwd(fixtures.getDirectory());
        fmt.addArgs(&.{ "fmt", "--check", source });
        fmt.expectExitCode(0);
        fmt.expectStdOutEqual("");
        fmt.expectStdErrEqual("");
        test_step.dependOn(&fmt.step);
    }
    for ([_][]const u8{ "generic-functions.nov", "contextual-value.nov", "imported.nov" }) |source| {
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI generic functions semantic: {s}", .{source});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source });
        check.expectExitCode(0);
        check.expectStdOutEqual("");
        check.expectStdErrEqual("");
        test_step.dependOn(&check.step);
    }
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test", "imported", "imported-test", "contextual-value", "contextual-value-test" }) |command| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI generic functions: {s} ({s})", .{ command, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            const imported = std.mem.startsWith(u8, command, "imported");
            const contextual = std.mem.startsWith(u8, command, "contextual-value");
            const tests = std.mem.endsWith(u8, command, "test");
            run.addArgs(&.{ if (tests) "test" else "run", if (imported) "imported.nov" else if (contextual) "contextual-value.nov" else "generic-functions.nov" });
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (contextual)
                (if (tests) "PASS contextual_value\n1 passed; 0 failed; 1 total\n" else "")
            else if (tests and imported)
                "PASS imported_callbacks\n1 passed; 0 failed; 1 total\n"
            else if (tests)
                "PASS first_and_nested_arrays\nPASS exact_add_constraints\nPASS ordinary_callback\nPASS contextual_callback_values\nPASS bidirectional_generic_callbacks\nPASS stored_contextual_callbacks\nPASS nominal_box\nPASS returned_closures_and_array_copies\nPASS body_constraints\n9 passed; 0 failed; 9 total\n"
            else if (imported)
                "3 7 11 true false canonical\n9 sibling\ntrue 42\n"
            else
                "7 3 3 hello!\n5 boxed captured\n42 43 44\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    const emit = b.addRunArtifact(exe);
    emit.step.name = "CLI generic functions emit canonical specializations";
    emit.setCwd(fixtures.getDirectory());
    emit.addArgs(&.{ "emit-c", "imported.nov" });
    emit.expectExitCode(0);
    emit.expectStdErrEqual("");
    const generated = b.addWriteFiles();
    _ = generated.addCopyFile(emit.captureStdOut(.{}), "imported.c");
    const identity_tests = b.addTest(.{ .root_module = b.createModule(.{
        .root_source_file = generated.add("canonical.zig",
            \\const std = @import("std");
            \\const source = @embedFile("imported.c");
            \\test "aliases share demand-only canonical specializations" {
            \\    var lines = std.mem.splitScalar(u8, source, '\n');
            \\    var total: usize = 0;
            \\    var sums: usize = 0;
            \\    var ids: usize = 0;
            \\    var identities: usize = 0;
            \\    var hidden: usize = 0;
            \\    while (lines.next()) |line| {
            \\        if (!std.mem.endsWith(u8, line, " {") or std.mem.indexOf(u8, line, "_24generic") == null) continue;
            \\        total += 1;
            \\        if (std.mem.indexOf(u8, line, "_nov_add_24generic") != null) sums += 1;
            \\        if (std.mem.indexOf(u8, line, "_nov_id_24generic") != null) ids += 1;
            \\        if (std.mem.indexOf(u8, line, "_nov_identity_24generic") != null) identities += 1;
            \\        if (std.mem.indexOf(u8, line, "_nov_hidden_24generic") != null) hidden += 1;
            \\    }
            \\    try std.testing.expectEqual(@as(usize, 12), total);
            \\    try std.testing.expectEqual(@as(usize, 1), sums);
            \\    try std.testing.expectEqual(@as(usize, 1), ids);
            \\    try std.testing.expectEqual(@as(usize, 4), identities);
            \\    try std.testing.expectEqual(@as(usize, 1), hidden);
            \\    try std.testing.expect(std.mem.indexOf(u8, source, "_nov_unused") == null);
            \\}
            \\
        ),
        .target = exe.root_module.resolved_target.?,
        .optimize = .Debug,
    }) });
    const identity = b.addRunArtifact(identity_tests);
    identity.step.name = "CLI generic functions canonical identity and demand";
    test_step.dependOn(&identity.step);

    const prefix = "let add:(x:`a,y:`b)->`c=x+y\nlet id:(x:`T)->`T=x\n";
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "integer-default", .source = prefix ++ "let main:()={let x=add(1,2)\n}", .diagnostic = "" },
        .{ .name = "result-only-default", .source = "let f:(x:`T)->`U=1\nlet main:()={let x=f(true)\n}", .diagnostic = "" },
        .{ .name = "conflict", .source = prefix ++ "let main:()={let x=add(@as(uint,1),@as(int,2))\n}", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "result-conflict", .source = prefix ++ "let main:()={let x:int=add(@as(uint,1),2)\n}", .diagnostic = "no implicit conversions are available" },
        .{ .name = "string-conflict", .source = prefix ++ "let main:()={let x=add(\"s\",@as(uint,2))\n}", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "function-value", .source = prefix ++ "let main:()={let f=id\n}", .diagnostic = "cannot infer every generic function type variable" },
        .{ .name = "callback-default", .source = prefix ++ "let apply:(f:(`T)->`U,x:`T)->`U=f(x)\nlet main:()={let x=apply(id,42)\n}", .diagnostic = "" },
        .{ .name = "callback-result-default", .source = "let f:(x:`T)->`U=1\nlet apply:(cb:(`T)->`U,x:`T)->`U=cb(x)\nlet main:()={let value=apply(f,true)\n}", .diagnostic = "" },
        .{ .name = "callback-result-conflict", .source = prefix ++ "let main:()={let cb:(uint)->int=id\n}", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "callback-argument-conflict", .source = prefix ++ "let apply:(f:(`T)->`T,x:`T)->`T=f(x)\nlet main:()={let x:int=apply(id,@as(uint,42))\n}", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "callback-arity-conflict", .source = prefix ++ "let main:()={let cb:(uint,uint)->uint=id\n}", .diagnostic = "argument count does not match the function signature" },
        .{ .name = "callback-non-function-context", .source = prefix ++ "let main:()={let cb:uint=id\n}", .diagnostic = "an exact concrete function signature in value contexts" },
        .{ .name = "callback-private-function", .source = "let lib=@import(\"library.nov\")\nlet main:()={let cb:(bool)->bool=lib.hidden\n}", .diagnostic = "declaration needs a resolved type before use here" },
        .{ .name = "callback-private-method", .source = "let lib=@import(\"library.nov\")\nlet main:()={let cb:(bool)->bool=lib.NS.private\n}", .diagnostic = "declaration needs a resolved type before use here" },
        .{ .name = "callback-private-sibling", .source = "let lib=@import(\"library.nov\")\nlet main:()={let cb:(bool,uint)->bool=lib.G(uint).sibling\n}", .diagnostic = "declaration needs a resolved type before use here" },
        .{ .name = "module-generic-closure", .source = "let f=(x:`T)->`T{x}\nlet main:()={}", .diagnostic = "generic closures, local templates" },
        .{ .name = "unbound", .source = "let x:`T=true\nlet main:()={}", .diagnostic = "unbound backtick type variable" },
        .{ .name = "unbound-body", .source = "let f:(x:`T)->`T={let y:`U=x\ny}\nlet main:()={let x=f(true)\n}", .diagnostic = "unbound backtick type variable" },
        .{ .name = "unsupported-inference", .source = "let f:(x:`T)->`U=match x{true=>\"yes\"\nfalse=>\"no\"}\nlet main:()={let x=f(true)\n}", .diagnostic = "type-constraint inference for this generic function or inferred closure body expression is not supported yet" },
        .{ .name = "bound-method", .source = "let S=struct{let f:(self:S,x:`T)->`T=x\n}\nlet main:()={let s=S.[]\nlet f:(bool)->bool=s.f\n}", .diagnostic = "unknown container field, method, enum tag or union variant" },
        .{ .name = "private-function", .source = "let lib=@import(\"library.nov\")\nlet main:()={let x=lib.hidden(true)\n}", .diagnostic = "declaration needs a resolved type before use here" },
        .{ .name = "private-method", .source = "let lib=@import(\"library.nov\")\nlet main:()={let x=lib.NS.private(true)\n}", .diagnostic = "declaration needs a resolved type before use here" },
        .{ .name = "private-sibling", .source = "let lib=@import(\"library.nov\")\nlet main:()={let x=lib.G(uint).sibling(true,1)\n}", .diagnostic = "declaration needs a resolved type before use here" },
        .{ .name = "compiletime-proposal", .source = "let f:(#compiletime T:type,x:T)->T=x\nlet main:()={}", .diagnostic = "expected type expression, found '#'" },
    }) |case| {
        const source = b.fmt("reject-{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI generic functions check: {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source });
        check.expectExitCode(if (case.diagnostic.len == 0) 0 else 1);
        check.expectStdOutEqual("");
        if (case.diagnostic.len == 0) check.expectStdErrEqual("") else check.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&check.step);
    }
}

fn addMutableMethodCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const counter = "let C=struct{value:int,let add:(self:&mut C,n:int)={self.*.value+=n}\nlet echo:(self:&mut C,x:`T)->`T=x\nlet read:(self:&C)->int=self.*.value\nlet take:(self:C)->int=self.value\n}\n";
    _ = fixtures.add("mutable-methods.nov", @embedFile("examples/mutable-methods.nov"));
    _ = fixtures.add("receiver-order.nov", counter ++
        \\let Callable=struct{call:(int)->int,}
        \\let forward:(p:&mut C,n:int)={p.add(n)}
        \\let readonly:(p:&C)->int=p.take()
        \\let generic:(p:&mut C,x:`T)->`T=p.echo(x)
        \\let verify:()={let mut c=C.[value=1]
        \\c.add(2)
        \\C.add(&c,3)
        \\let exact:u8=c.echo(@as(u8,7))
        \\let query:@TypeOf(c.echo(exact))=exact
        \\@assert(c.read()==6 and query==7)
        \\{let p=&c
        \\p.add(0)
        \\forward(p,0)
        \\@assert(p.read()==6 and p.take()==6 and generic(p,exact)==7)
        \\let unused:@TypeOf(p.add({@print("BAD")
        \\1}))={}
        \\}
        \\let fixed=C.[value=6]
        \\let r=&fixed
        \\@assert(r.read()==6 and r.take()==6 and readonly(r)==6)
        \\let f=Callable.[call=(n:int)->int{n+1}]
        \\let pf=&f
        \\@assert(pf.*.call(41)==42)
        \\let mut items=[C.[value=10]]
        \\let mut count:int=0
        \\items[{count+=1
        \\@print("R")
        \\0}].add({@print("A")
        \\5})
        \\@assert(count==1 and items[0].value==15)
        \\@println(c.value,exact,items[0].value)}
        \\@[test]
        \\let mutable_receivers_and_order:()=verify()
        \\let main:()=verify()
        \\
    );
    _ = fixtures.add("library.nov",
        \\let secret:int=4
        \\@[public]
        \\let Box:(T:type)->type=struct{let Self=@This()
        \\value:T,
        \\@[public]
        \\let set:(self:&mut Self,value:T)={self.*.value=value}
        \\@[public]
        \\let echo:(self:&mut Self,value:`U)->`U=value
        \\@[public]
        \\let read:(self:&Self)->int=secret
        \\let hidden:(self:&mut Self)={}
        \\}
        \\
    );
    _ = fixtures.add("api.nov", "let lib=@import(\"library.nov\")\n@[public]\nlet Box=lib.Box\n");
    _ = fixtures.add("imported.nov",
        \\let api=@import("api.nov")
        \\let same=@import("./library.nov")
        \\let Alias=api.Box(u8)
        \\let forward:(p:&mut Alias,value:`T)->`T={p.set(3)
        \\p.echo(value)}
        \\let readonly:(p:&Alias)->int=p.read()
        \\let verify:()={let mut b=Alias.[value=1]
        \\b.set(2)
        \\same.Box(u8).set(&b,3)
        \\let p=&b
        \\p.set(3)
        \\let result:str=forward(p,"generic")
        \\@assert(p.echo(result)=="generic" and p.read()==4 and readonly(p)==4)
        \\let fixed=Alias.[value=7]
        \\let r=&fixed
        \\@assert(r.read()==4 and readonly(r)==4)}
        \\@[test]
        \\let imported_public_generic_receivers:()=verify()
        \\let main:()=verify()
        \\
    );
    const fmt = b.addRunArtifact(exe);
    fmt.setCwd(fixtures.getDirectory());
    fmt.addArgs(&.{ "fmt", "--check", "mutable-methods.nov" });
    fmt.expectExitCode(0);
    fmt.expectStdOutEqual("");
    fmt.expectStdErrEqual("");
    test_step.dependOn(&fmt.step);
    for ([_][]const u8{ "mutable-methods.nov", "receiver-order.nov", "imported.nov" }) |source| {
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI mutable methods semantic: {s}", .{source});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source });
        check.expectExitCode(0);
        check.expectStdOutEqual("");
        check.expectStdErrEqual("");
        test_step.dependOn(&check.step);
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            for ([_][]const u8{ "run", "test" }) |command| {
                const run = b.addRunArtifact(exe);
                run.step.name = b.fmt("CLI mutable methods: {s} {s} ({s})", .{ command, source, mode });
                run.setCwd(fixtures.getDirectory());
                run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                run.setEnvironmentVariable("NOV_CC", "");
                run.setEnvironmentVariable("CC", "");
                run.addArgs(&.{ command, source });
                if (mode.len != 0) run.addArg(mode);
                const tests = std.mem.eql(u8, command, "test");
                const imported = std.mem.eql(u8, source, "imported.nov");
                const example = std.mem.eql(u8, source, "mutable-methods.nov");
                run.expectExitCode(0);
                run.expectStdOutEqual(if (example)
                    (if (tests) "PASS local_static_and_value_receivers\nPASS explicit_dereference_and_reference_parameter\nPASS array_element_receiver_evaluates_once\nPASS receiver_target_precedes_argument_rebinding\nPASS generic_self_and_method_inference\nPASS type_queries_do_not_borrow_or_execute\nPASS callable_fields_have_no_implicit_receiver\nPASS reference_fields_read_write_and_call\nPASS reference_field_target_precedes_rhs_rebinding\n9 passed; 0 failed; 9 total\n" else "Counter: 9\nThrough reference: 10\nArray elements: 10 25\nGeneric: after 7\n")
                else if (imported)
                    (if (tests) "PASS imported_public_generic_receivers\n1 passed; 0 failed; 1 total\n" else "")
                else if (tests)
                    "RA6 7 15\nPASS mutable_receivers_and_order\n1 passed; 0 failed; 1 total\n"
                else
                    "RA6 7 15\n");
                run.expectStdErrEqual("");
                test_step.dependOn(&run.step);
            }
        }
    }
    const conflict = "owner access or aliased reference arguments conflict with a live borrow";
    const immutable = "assignment requires a mutable local";
    const unsupported = "local references require";
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "immutable", .source = counter ++ "let main:()={let c=C.[value=0]\nc.add(1)}", .diagnostic = immutable },
        .{ .name = "temporary", .source = counter ++ "let main:()={C.[value=0].add(1)}", .diagnostic = unsupported },
        .{ .name = "global", .source = counter ++ "let mut c=C.[value=0]\nlet main:()={c.add(1)}", .diagnostic = unsupported },
        .{ .name = "field-place", .source = counter ++ "let H=struct{c:C,}\nlet main:()={let mut h=H.[c=C.[value=0]]\nh.c.add(1)}", .diagnostic = unsupported },
        .{ .name = "readonly", .source = counter ++ "let f:(p:&C)={p.add(1)}\nlet main:()={}", .diagnostic = immutable },
        .{ .name = "readonly-local", .source = counter ++ "let main:()={let c=C.[value=0]\nlet p=&c\np.add(1)}", .diagnostic = immutable },
        .{ .name = "readonly-generic", .source = counter ++ "let f:(p:&C)={let result:int=p.echo(@as(int,1))\n}\nlet main:()={}", .diagnostic = immutable },
        .{ .name = "readonly-query", .source = counter ++ "let f:(p:&C)={let q:@TypeOf(p.add(1))={}\n}\nlet main:()={}", .diagnostic = immutable },
        .{ .name = "readonly-field-write", .source = counter ++ "let f:(p:&C)={p.value=1}\nlet main:()={}", .diagnostic = immutable },
        .{ .name = "readonly-field-query", .source = counter ++ "let f:(p:&C)={let q:@TypeOf({p.value+=1})={}\n}\nlet main:()={}", .diagnostic = immutable },
        .{ .name = "private-reference", .source = "let api=@import(\"api.nov\")\nlet Alias=api.Box(u8)\nlet f:(p:&mut Alias)={p.hidden()}\nlet main:()={}", .diagnostic = "body checking for this expression is not implemented yet" },
        .{ .name = "argument-read", .source = counter ++ "let main:()={let mut c=C.[value=0]\nc.add(c.value)}", .diagnostic = conflict },
        .{ .name = "argument-write", .source = counter ++ "let main:()={let mut c=C.[value=0]\nc.add({c.value=3\n1})}", .diagnostic = conflict },
        .{ .name = "live-borrow", .source = counter ++ "let main:()={let mut c=C.[value=0]\nlet p=&c\nc.add(1)\n@println(p.*.value)}", .diagnostic = conflict },
        .{ .name = "element-owner", .source = counter ++ "let main:()={let mut cs=[C.[value=0]]\ncs[0].add({cs=[C.[value=2]]\n1})}", .diagnostic = conflict },
        .{ .name = "captured-owner", .source = counter ++ "let main:()={let mut c=C.[value=0]\nlet run=()->int{c.value=3\n1}\nc.add(run())}", .diagnostic = conflict },
        .{ .name = "aliased-arguments", .source = "let S=struct{let pair:(self:&mut S,other:&mut S)={}\n}\nlet main:()={let mut s=S.[]\ns.pair(&s)}", .diagnostic = conflict },
        .{ .name = "private-instance", .source = "let lib=@import(\"library.nov\")\nlet main:()={let mut b=lib.Box(u8).[value=1]\nb.hidden()}", .diagnostic = "unknown container field, method, enum tag or union variant" },
        .{ .name = "private-static", .source = "let lib=@import(\"library.nov\")\nlet main:()={let mut b=lib.Box(u8).[value=1]\nlib.Box(u8).hidden(&b)}", .diagnostic = "declaration needs a resolved type before use here" },
        .{ .name = "callback-field-arity", .source = "let S=struct{call:(int)->int,}\nlet id:(n:int)->int=n\nlet main:()={let mut s=S.[call=id]\ns.call()}", .diagnostic = "argument count does not match the function signature" },
        .{ .name = "callback-field-type", .source = "let S=struct{call:(int)->int,}\nlet id:(n:int)->int=n\nlet main:()={let mut s=S.[call=id]\ns.call(true)}", .diagnostic = "literal does not match the annotated type" },
    }) |case| {
        _ = fixtures.add(b.fmt("reject-{s}.nov", .{case.name}), case.source);
        for ([_]bool{ false, true }) |emit_c| {
            const check = b.addRunArtifact(exe);
            check.step.name = b.fmt("CLI mutable methods reject: {s} ({s})", .{ case.name, if (emit_c) "emit-c" else "semantic" });
            check.setCwd(fixtures.getDirectory());
            check.addArgs(if (emit_c) &.{"emit-c"} else &.{ "check", "--semantic" });
            check.addArg(b.fmt("reject-{s}.nov", .{case.name}));
            check.expectExitCode(1);
            check.expectStdOutEqual("");
            check.expectStdErrMatch(b.fmt("error: {s}", .{case.diagnostic}));
            test_step.dependOn(&check.step);
        }
    }
}

fn addGenericMethodCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("generic-methods.nov", @embedFile("examples/generic-methods.nov"));
    _ = fixtures.add("library.nov",
        \\let secret: uint = 42
        \\let identity: (value: `T) -> `T = value
        \\@[public]
        \\let callback = identity
        \\let Hidden: (T: type) -> type = struct {
        \\    value: T,
        \\    @[public]
        \\    let map: (self: Hidden(T), convert: (T) -> `U) -> Hidden(`U) = Hidden(`U).[ value = convert(self.value) ]
        \\    let private: (self: Hidden(T), value: `U) -> `U = value
        \\    @[public]
        \\    let read: (self: Hidden(T), value: `U) -> uint = secret
        \\}
        \\@[public]
        \\let Alias = Hidden
        \\@[public]
        \\let make: () -> Hidden(bool) = Hidden(bool).[ value = true ]
        \\let Plain = struct {
        \\    @[public]
        \\    let apply: (self: Plain, value: `T) -> `T = value
        \\}
        \\@[public]
        \\let plain: () -> Plain = Plain.[]
        \\
    );
    _ = fixtures.add("api.nov", "let library = @import(\"library.nov\")\n@[public]\nlet Box = library.Alias\n@[public]\nlet callback = library.callback\n");
    _ = fixtures.add("imported.nov",
        \\let library = @import("library.nov")
        \\let same = @import("./library.nov")
        \\let api = @import("api.nov")
        \\let B = api.Box(bool)
        \\let secret: uint = 100
        \\let verify: () = {
        \\    let box = library.make()
        \\    let instance: B = box.map(api.callback)
        \\    let static: B = same.Alias(bool).map(box, library.callback)
        \\    let alias: B = B.map(box, same.callback)
        \\    @assert(instance.value and static.value and alias.value)
        \\    @assert(box.read(true) == 42)
        \\    @assert(library.plain().apply("plain") == "plain")
        \\}
        \\@[test]
        \\let imported_aliases_and_visibility: () = verify()
        \\let main: () = verify()
        \\
    );
    _ = fixtures.add("order.nov",
        \\let Box: (T: type) -> type = struct {
        \\    value: T,
        \\    let map: (self: Box(T), convert: (T) -> `U, other: `U) -> `U = convert(self.value)
        \\}
        \\let increment: (value: uint) -> uint = {
        \\    @print("C")
        \\    value + 1
        \\}
        \\let receiver: () -> Box(uint) = {
        \\    @print("R")
        \\    Box(uint).[ value = 41 ]
        \\}
        \\let callback: () -> (uint) -> uint = {
        \\    @print("A")
        \\    increment
        \\}
        \\let argument: () -> uint = {
        \\    @print("B")
        \\    0
        \\}
        \\let verify: () = {
        \\    @assert(receiver().map(callback(), argument()) == 42)
        \\    @println("!")
        \\    @assert(Box(uint).map(receiver(), callback(), argument()) == 42)
        \\    @println("!")
        \\}
        \\@[test]
        \\let receiver_once_before_arguments: () = verify()
        \\let main: () = verify()
        \\
    );
    for ([_][]const u8{ "generic-methods.nov", "library.nov", "api.nov", "imported.nov", "order.nov" }) |source| {
        const fmt = b.addRunArtifact(exe);
        fmt.step.name = b.fmt("CLI generic methods fmt: {s}", .{source});
        fmt.setCwd(fixtures.getDirectory());
        fmt.addArgs(&.{ "fmt", "--check", source });
        fmt.expectExitCode(0);
        fmt.expectStdOutEqual("");
        fmt.expectStdErrEqual("");
        test_step.dependOn(&fmt.step);
    }
    for ([_][]const u8{ "generic-methods.nov", "imported.nov", "order.nov" }) |source| {
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI generic methods semantic: {s}", .{source});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source });
        check.expectExitCode(0);
        check.expectStdOutEqual("");
        check.expectStdErrEqual("");
        test_step.dependOn(&check.step);
    }
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "generic-methods.nov", "imported.nov", "order.nov" }) |source| {
            for ([_][]const u8{ "run", "test" }) |command| {
                const run = b.addRunArtifact(exe);
                run.step.name = b.fmt("CLI generic methods: {s} {s} ({s})", .{ command, source, mode });
                run.setCwd(fixtures.getDirectory());
                run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                run.setEnvironmentVariable("NOV_CC", "");
                run.setEnvironmentVariable("CC", "");
                run.addArgs(&.{ command, source });
                if (mode.len != 0) run.addArg(mode);
                const tests = std.mem.eql(u8, command, "test");
                const imported = std.mem.eql(u8, source, "imported.nov");
                const order = std.mem.eql(u8, source, "order.nov");
                run.expectExitCode(0);
                run.expectStdOutEqual(if (order)
                    (if (tests) "RABC!\nRABC!\nPASS receiver_once_before_arguments\n1 passed; 0 failed; 1 total\n" else "RABC!\nRABC!\n")
                else if (imported)
                    (if (tests) "PASS imported_aliases_and_visibility\n1 passed; 0 failed; 1 total\n" else "")
                else if (tests)
                    "PASS receiver_and_callback\nPASS expected_and_argument_constraints\nPASS contextual_identity\nPASS plain_nominal_and_static\nPASS byvalue_receiver_snapshot\n5 passed; 0 failed; 5 total\n"
                else
                    "42 value:41\n41 41\nplain true\n");
                run.expectStdErrEqual("");
                test_step.dependOn(&run.step);
            }
        }
    }
    const emit = b.addRunArtifact(exe);
    emit.step.name = "CLI generic methods emit canonical specializations";
    emit.setCwd(fixtures.getDirectory());
    emit.addArgs(&.{ "emit-c", "imported.nov" });
    emit.expectExitCode(0);
    emit.expectStdErrEqual("");
    const generated = b.addWriteFiles();
    _ = generated.addCopyFile(emit.captureStdOut(.{}), "imported.c");
    const canonical_tests = b.addTest(.{ .root_module = b.createModule(.{
        .root_source_file = generated.add("canonical.zig",
            \\const std = @import("std");
            \\const source = @embedFile("imported.c");
            \\test "static instance and imported aliases share one map specialization" {
            \\    var lines = std.mem.splitScalar(u8, source, '\n');
            \\    var maps: usize = 0;
            \\    while (lines.next()) |line| {
            \\        if (std.mem.endsWith(u8, line, " {") and std.mem.indexOf(u8, line, "_nov_map_24generic") != null) maps += 1;
            \\    }
            \\    try std.testing.expectEqual(@as(usize, 1), maps);
            \\}
            \\
        ),
        .target = exe.root_module.resolved_target.?,
        .optimize = .Debug,
    }) });
    const canonical = b.addRunArtifact(canonical_tests);
    canonical.step.name = "CLI generic methods canonical identity";
    test_step.dependOn(&canonical.step);

    const box = "let Box:(T:type)->type=struct{value:T,let map:(self:Box(T),convert:(T)->`U)->Box(`U)=Box(`U).[value=convert(self.value)]\nlet choose:(self:Box(T),convert:(T)->`U,other:`U)->`U=convert(self.value)\n}\nlet id:(x:`T)->`T=x\n";
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "callback-input-conflict", .source = box ++ "let f:(x:int)->bool=true\nlet main:()={let x=Box(uint).[value=1].map(f)\n}", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "expected-result-conflict", .source = box ++ "let main:()={let x:Box(int)=Box(uint).[value=1].map(id)\n}", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "argument-result-conflict", .source = box ++ "let main:()={let x=Box(uint).[value=1].choose(id,@as(int,2))\n}", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "integer-default", .source = "let S=struct{let f:(self:S,x:`T)->`T=x\n}\nlet main:()={let x=S.[].f(1)\n}", .diagnostic = "" },
        .{ .name = "callback-result-default", .source = box ++ "let f:(x:uint)->`U=1\nlet main:()={let x=Box(uint).[value=1].choose(f,1)\n}", .diagnostic = "" },
        .{ .name = "private-instance", .source = "let lib=@import(\"library.nov\")\nlet main:()={let x=lib.make().private(true)\n}", .diagnostic = "unknown container field, method, enum tag or union variant" },
        .{ .name = "private-static", .source = "let lib=@import(\"library.nov\")\nlet main:()={let x=lib.Alias(bool).private(lib.make(),true)\n}", .diagnostic = "declaration needs a resolved type before use here" },
        .{ .name = "private-callback", .source = "let lib=@import(\"library.nov\")\nlet main:()={let x:lib.Alias(bool)=lib.make().map(lib.identity)\n}", .diagnostic = "declaration needs a resolved type before use here" },
        .{ .name = "wrong-self-type", .source = "let S=struct{let f:(self:uint,x:`T)->`T=x\n}\nlet main:()={let x=S.[].f(true)\n}", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "missing-self", .source = "let S=struct{let f:(x:`T)->`T=x\n}\nlet main:()={let x=S.[].f(true)\n}", .diagnostic = "argument count does not match the function signature" },
        .{ .name = "wrong-arity", .source = box ++ "let main:()={let x=Box(bool).[value=true].map(id,true)\n}", .diagnostic = "argument count does not match the function signature" },
        .{ .name = "callback-signature", .source = box ++ "let f:(x:uint,y:uint)->uint=x\nlet main:()={let x=Box(uint).[value=1].map(f)\n}", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "bound-method", .source = box ++ "let main:()={let box=Box(bool).[value=true]\nlet f:((bool)->bool)->Box(bool)=box.map\n}", .diagnostic = "unknown container field, method, enum tag or union variant" },
        .{ .name = "callable-field", .source = "let S=struct{f:(bool)->bool,}\nlet id:(x:bool)->bool=x\nlet main:()={let s=S.[f=id]\nlet x=s.f(true)\n}", .diagnostic = "" },
    }) |case| {
        const source = b.fmt("reject-{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI generic methods check: {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source });
        check.expectExitCode(if (case.diagnostic.len == 0) 0 else 1);
        check.expectStdOutEqual("");
        if (case.diagnostic.len == 0) check.expectStdErrEqual("") else check.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&check.step);
    }
}

fn addGenericStructCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step, fixtures: *std.Build.Step.WriteFile) void {
    _ = fixtures.add("generic-struct-library.nov",
        \\@[public]
        \\let Box: (T: type) -> type = struct {
        \\    value: T,
        \\
        \\    let Self = @This()
        \\
        \\    @[public]
        \\    let make: (value: T) -> Self = Self.[ value = value ]
        \\
        \\    @[public]
        \\    let get: (self: Self) -> T = self.value
        \\
        \\    @[public]
        \\    @[stringify]
        \\    let toString: (self: Self) -> str = "<{self.value}>"
        \\}
        \\
    );
    _ = fixtures.add("generic-struct-api.nov", "let library = @import(\"generic-struct-library.nov\")\n@[public]\nlet Box = library.Box\n");
    _ = fixtures.add("generic-struct-main.nov",
        \\let api = @import("generic-struct-api.nov")
        \\let library = @import("generic-struct-library.nov")
        \\let Box = api.Box
        \\let B = Box(str)
        \\
        \\let main: () = {
        \\    let mut x: B = library.Box(str).[ value = "a" + "b" ]
        \\    let copy = x
        \\    x.value += "!"
        \\    let nested = Box(B).[ value = x ]
        \\    let values: []Box(str) = [copy, B.make("new" + "!")]
        \\    @println(nested, values[0].get(), values[1])
        \\}
        \\
    );
    for ([_][]const u8{ "generic-struct-main.nov", "generic-struct-library.nov", "generic-struct-api.nov" }) |source| {
        const fmt = b.addRunArtifact(exe);
        fmt.step.name = b.fmt("CLI generic structs fmt: {s}", .{source});
        fmt.setCwd(fixtures.getDirectory());
        fmt.addArgs(&.{ "fmt", "--check", source });
        fmt.expectExitCode(0);
        fmt.expectStdOutEqual("");
        fmt.expectStdErrEqual("");
        test_step.dependOn(&fmt.step);
    }
    for ([_][]const u8{ "semantic", "default", "--debug", "--release" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI generic structs: {s}", .{mode});
        run.setCwd(fixtures.getDirectory());
        const semantic = std.mem.eql(u8, mode, "semantic");
        if (semantic) {
            run.addArgs(&.{ "check", "--semantic", "generic-struct-main.nov" });
        } else {
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ "run", "generic-struct-main.nov" });
            if (!std.mem.eql(u8, mode, "default")) run.addArg(mode);
        }
        run.expectExitCode(0);
        run.expectStdOutEqual(if (semantic) "" else "<<ab!>> ab <new!>\n");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    const Case = struct { name: []const u8, source: []const u8, diagnostic: []const u8 };
    for ([_]Case{
        .{ .name = "nonstorable-array-field", .source = "let G:(T:type)->type=struct{value:[][]T,}\nlet main:()={let x=G(void).[]\n}", .diagnostic = "this type is not supported in semantic analysis yet" },
        .{ .name = "default", .source = "let G:(T:type)->type=struct{value:T=1,}\nlet main:()={let x=G(bool).[]\n}", .diagnostic = "literal does not match" },
        .{ .name = "cycle", .source = "let G:(T:type)->type=struct{value:G(T),}\nlet main:()={let x=G(int).[]\n}", .diagnostic = "cyclic generic container instantiation" },
    }) |case| {
        const name = b.fmt("generic-struct-{s}.nov", .{case.name});
        _ = fixtures.add(name, case.source);
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI generic structs reject: {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", name });
        check.expectExitCode(1);
        check.expectStdOutEqual("");
        check.addCheck(.{ .expect_stderr_match = case.diagnostic });
        test_step.dependOn(&check.step);
    }
}

fn addUnionArrayCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    _ = fixtures.add("union-arrays.nov", @embedFile("examples/union-arrays.nov"));
    const package = b.addWriteFiles();
    const installed_exe = package.addCopyFile(exe.getEmittedBin(), b.fmt("bin/{s}", .{exe.out_filename}));
    _ = package.addCopyDirectory(b.path("lib"), "lib/nov", .{});
    _ = package.add("union-arrays.nov", @embedFile("examples/union-arrays.nov"));
    const expected_tests = "PASS binding_assignment_arguments_and_returns\nPASS match_and_success_error_propagation\nPASS nested_strings_and_arrays_of_unions\nPASS closure_snapshots_and_shared_cells\nPASS mapping_and_finding_arrays\nPASS inactive_payloads_and_storable_elements\n6 passed; 0 failed; 6 total\n";
    for ([_]bool{ false, true }) |installed| {
        for ([_][]const u8{ "fmt", "semantic", "run", "test" }) |command| {
            const execute = std.mem.eql(u8, command, "run") or std.mem.eql(u8, command, "test");
            const modes: []const []const u8 = if (execute) &.{ "", "--debug", "--release" } else &.{""};
            for (modes) |mode| {
                const run = if (installed) std.Build.Step.Run.create(b, "CLI union arrays: relocated install") else b.addRunArtifact(exe);
                run.step.name = b.fmt("CLI union arrays: {s} {s} (installed={})", .{ command, mode, installed });
                if (installed) run.addFileArg(installed_exe);
                run.setCwd(if (installed) package.getDirectory() else fixtures.getDirectory());
                run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                run.setEnvironmentVariable("NOV_CC", "");
                run.setEnvironmentVariable("CC", "");
                if (std.mem.eql(u8, command, "fmt")) {
                    run.addArgs(&.{ "fmt", "--check", "union-arrays.nov" });
                } else if (std.mem.eql(u8, command, "semantic")) {
                    run.addArgs(&.{ "check", "--semantic", "union-arrays.nov" });
                } else {
                    run.addArgs(&.{ command, "union-arrays.nov" });
                    if (mode.len != 0) run.addArg(mode);
                }
                if (!installed and !std.mem.eql(u8, command, "fmt")) {
                    run.addArg("--lib-dir");
                    run.addDirectoryArg(library);
                }
                run.expectExitCode(0);
                run.expectStdOutEqual(if (std.mem.eql(u8, command, "test")) expected_tests else if (std.mem.eql(u8, command, "run")) "Payloads: [Payload.numbers[[1, 2]], Payload.words[[[hello], []]], Payload.none]\nAdvanced: [4]\nError: [missing]\n" else "");
                run.expectStdErrEqual("");
                test_step.dependOn(&run.step);
            }
        }
    }
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "recursive", .source = "let U=union{items:[]U,none,}\nlet main:()={}", .diagnostic = "cyclic" },
        .{ .name = "mutually-recursive", .source = "let U=union{items:[]R,none,}\nlet R=struct{values:[]U,}\nlet main:()={}", .diagnostic = "cyclic" },
        .{ .name = "recursive-nested-array", .source = "let U=union{items:[][]U,none,}\nlet main:()={}", .diagnostic = "cyclic" },
        .{ .name = "recursive-generic", .source = "let G:(T:type)->type=union{items:[]G(T),none,}\nlet f:(x:G(int))={}", .diagnostic = "cyclic" },
        .{ .name = "nested-function-reference", .source = "let Inner=union{value:&()->int,none,}\nlet U=union{value:Inner,}\nlet f:(x:U)={}", .diagnostic = "not supported" },
        .{ .name = "direct-function-reference", .source = "let U=union{value:&()->int,}\nlet f:(x:U)={}", .diagnostic = "not supported" },
        .{ .name = "direct-range-reference", .source = "let U=union{value:&@TypeOf(1..3),}\nlet f:(x:U)={}", .diagnostic = "not supported" },
        .{ .name = "array-void", .source = "let U=union{items:[]void,}\nlet f:(x:U)={}", .diagnostic = "not supported" },
        .{ .name = "array-type", .source = "let U=union{items:[]type,}\nlet f:(x:U)={}", .diagnostic = "not supported" },
        .{ .name = "array-reference", .source = "let U=union{items:[]&int,}\nlet f:(x:U)={}", .diagnostic = "references cannot escape" },
        .{ .name = "reference-payload", .source = "let U=union{value:&int,}\nlet f:(x:U)={}", .diagnostic = "not supported" },
        .{ .name = "reference-escape", .source = "let bad:()->&int={let mut value=1\n&value}\nlet main:()={}", .diagnostic = "reference" },
        .{ .name = "wrong-array-payload", .source = "let U=union{items:[]int,}\nlet f:()->U=.items[[true]]", .diagnostic = "error:" },
        .{ .name = "propagation-error-type", .source = "let std=@import(\"std\")\nlet f:(x:std.Result([]int,[]str))->std.Result([]int,[]bool)=.ok[x.!]", .diagnostic = "error:" },
    }) |case| {
        const source = b.fmt("invalid-union-array-{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI union arrays: reject {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source, "--lib-dir" });
        check.addDirectoryArg(library);
        check.expectExitCode(1);
        check.expectStdOutEqual("");
        check.expectStdErrMatch(".nov:");
        check.expectStdErrMatch("error:");
        check.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&check.step);
    }
}

fn addNestedUnionCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    const aliases_source =
        \\let standard = @import("std")
        \\let option = @import("lib/./option.nov")
        \\let result = @import("lib/result.nov")
        \\let api = @import("api.nov")
        \\let verify: () = {
        \\    let value: api.Nested = .some[.ok[7]]
        \\    let canonical: standard.Option(result.Result(int, str)) = value
        \\    let direct: option.Option(standard.Result(int, str)) = canonical
        \\    @assert(direct.unwrapOr(.err["missing"]).unwrapOr(0) == 7)
        \\    let reverse: api.Reverse = .ok[.some[8]]
        \\    let same: result.Result(option.Option(int), str) = reverse
        \\    @assert(same.unwrapOr(.none[]).unwrapOr(0) == 8)
        \\}
        \\@[test]
        \\let imported_nested_aliases_are_canonical: () = verify()
        \\let main: () = verify()
        \\
    ;
    const api_source =
        \\let standard = @import("std")
        \\let option = @import("lib/option.nov")
        \\let result = @import("lib/./result.nov")
        \\@[public]
        \\let Nested = option.Option(standard.Result(int, str))
        \\@[public]
        \\let Reverse = result.Result(standard.Option(int), str)
        \\
    ;
    _ = fixtures.add("nested-unions.nov", @embedFile("examples/nested-unions.nov"));
    _ = fixtures.add("aliases.nov", aliases_source);
    _ = fixtures.add("api.nov", api_source);
    const package = b.addWriteFiles();
    const installed_exe = package.addCopyFile(exe.getEmittedBin(), b.fmt("bin/{s}", .{exe.out_filename}));
    _ = package.addCopyDirectory(b.path("lib"), "lib/nov", .{});
    _ = package.add("nested-unions.nov", @embedFile("examples/nested-unions.nov"));
    _ = package.add("aliases.nov", std.mem.replaceOwned(u8, b.allocator, aliases_source, "lib/", "lib/nov/") catch @panic("OOM"));
    _ = package.add("api.nov", std.mem.replaceOwned(u8, b.allocator, api_source, "lib/", "lib/nov/") catch @panic("OOM"));
    const expected_tests = "PASS contextual_constructors_and_single_layer_matches\nPASS propagation_preserves_each_family_layer\nPASS recursive_array_storage_is_independent\nPASS closure_snapshots_and_shared_cells\nPASS mapping_and_search_produce_nested_unions\nPASS nested_stringify_and_inactive_payloads\n6 passed; 0 failed; 6 total\n";
    for ([_]bool{ false, true }) |installed| {
        for ([_][]const u8{ "nested-unions.nov", "aliases.nov", "api.nov" }) |source| {
            for ([_][]const u8{ "fmt", "semantic", "run", "test" }) |command| {
                const execute = std.mem.eql(u8, command, "run") or std.mem.eql(u8, command, "test");
                if (execute and std.mem.eql(u8, source, "api.nov")) continue;
                const modes: []const []const u8 = if (execute) &.{ "", "--debug", "--release" } else &.{""};
                for (modes) |mode| {
                    const run = if (installed) std.Build.Step.Run.create(b, "CLI nested unions: relocated install") else b.addRunArtifact(exe);
                    run.step.name = b.fmt("CLI nested unions: {s} {s} {s} (installed={})", .{ command, source, mode, installed });
                    if (installed) run.addFileArg(installed_exe);
                    run.setCwd(if (installed) package.getDirectory() else fixtures.getDirectory());
                    run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                    run.setEnvironmentVariable("NOV_CC", "");
                    run.setEnvironmentVariable("CC", "");
                    if (std.mem.eql(u8, command, "fmt")) {
                        run.addArgs(&.{ "fmt", "--check", source });
                    } else if (std.mem.eql(u8, command, "semantic")) {
                        run.addArgs(&.{ "check", "--semantic", source });
                    } else {
                        run.addArgs(&.{ command, source });
                        if (mode.len != 0) run.addArg(mode);
                    }
                    if (!installed and !std.mem.eql(u8, command, "fmt")) {
                        run.addArg("--lib-dir");
                        run.addDirectoryArg(library);
                    }
                    const example = std.mem.eql(u8, source, "nested-unions.nov");
                    const is_test = std.mem.eql(u8, command, "test");
                    run.expectExitCode(0);
                    run.expectStdOutEqual(if (execute) (if (example) (if (is_test) expected_tests else "Outer.inner[Inner.some[[[hello], []]]]\nOuter.none\nEnvelope.custom[Custom.label[ Label(nested) ]]\n") else if (is_test) "PASS imported_nested_aliases_are_canonical\n1 passed; 0 failed; 1 total\n" else "") else "");
                    run.expectStdErrEqual("");
                    test_step.dependOn(&run.step);
                }
            }
        }
    }
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "direct-cycle", .source = "let U=union{inner:U,none,}\nlet f:(x:U)={}", .diagnostic = "cyclic" },
        .{ .name = "nested-array-cycle", .source = "let U=union{inner:V,none,}\nlet V=union{items:[]U,none,}\nlet f:(x:U)={}", .diagnostic = "cyclic" },
        .{ .name = "generic-array-cycle", .source = "let U=union{inner:std.Option([]U),none,}\nlet f:(x:U)={}", .diagnostic = "cyclic" },
        .{ .name = "nominal-inner", .source = "let A=union{some:int,none,}\nlet B=union{some:int,none,}\nlet U=union{inner:A,}\nlet b:B=.some[1]\nlet f:()->U=.inner[b]", .diagnostic = "error:" },
        .{ .name = "nested-error-exact", .source = "let A=union{bad:int,}\nlet B=union{bad:int,}\nlet f:(x:std.Result(int,std.Option(A)))->std.Result(int,std.Option(B))=.ok[x.!]", .diagnostic = "error:" },
        .{ .name = "nested-error-element", .source = "let f:(x:std.Result(int,std.Option(int)))->std.Result(int,std.Option(bool))=.ok[x.!]", .diagnostic = "error:" },
        .{ .name = "option-to-result", .source = "let f:(x:std.Option(std.Result(int,str)))->std.Result(int,str)=x.!", .diagnostic = "error:" },
        .{ .name = "result-to-option", .source = "let f:(x:std.Result(std.Option(int),str))->std.Option(int)=x.!", .diagnostic = "error:" },
        .{ .name = "struct-union-cycle", .source = "let Inner=union{some:S,none,}\nlet S=struct{inner:Inner,}\nlet U=union{record:S,none,}\nlet f:(x:U)={}", .diagnostic = "cyclic" },
        .{ .name = "nested-function-reference", .source = "let Inner=union{value:&()->int,none,}\nlet U=union{inner:Inner,none,}\nlet f:(x:U)={}", .diagnostic = "not supported" },
        .{ .name = "nested-range-reference", .source = "let Inner=union{value:&@TypeOf(1..3),none,}\nlet U=union{inner:Inner,none,}\nlet f:(x:U)={}", .diagnostic = "not supported" },
        .{ .name = "nested-reference", .source = "let Inner=union{value:&int,none,}\nlet U=union{inner:Inner,none,}\nlet f:(x:U)={}", .diagnostic = "not supported" },
        .{ .name = "nested-reference-array", .source = "let Inner=union{items:[]&int,none,}\nlet U=union{inner:Inner,none,}\nlet f:(x:U)={}", .diagnostic = "references cannot escape" },
        .{ .name = "reference-escape", .source = "let bad:()->&int={let mut value=1\n&value}\nlet main:()={}", .diagnostic = "reference" },
    }) |case| {
        const source = b.fmt("invalid-nested-union-{s}.nov", .{case.name});
        _ = fixtures.add(source, b.fmt("let std=@import(\"std\")\n{s}\n", .{case.source}));
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI nested unions: reject {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source, "--lib-dir" });
        check.addDirectoryArg(library);
        check.expectExitCode(1);
        check.expectStdOutEqual("");
        check.expectStdErrMatch(".nov:");
        check.expectStdErrMatch("error:");
        check.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&check.step);
    }
}

fn addStorableArrayCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("storable-arrays.nov", @embedFile("examples/storable-arrays.nov"));
    for ([_][]const u8{ "fmt", "semantic" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI storable arrays: {s}", .{mode});
        run.setCwd(fixtures.getDirectory());
        run.addArgs(if (std.mem.eql(u8, mode, "fmt")) &.{ "fmt", "--check", "storable-arrays.nov" } else &.{ "check", "--semantic", "storable-arrays.nov" });
        run.expectExitCode(0);
        run.expectStdOutEqual("");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test" }) |command| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI storable arrays: {s} ({s})", .{ command, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ command, "storable-arrays.nov" });
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (std.mem.eql(u8, command, "test"))
                "PASS enum_membership_and_refs\nPASS nested_copies_and_iteration\nPASS union_active_payload_copies\nPASS ordinary_function_arrays\nPASS escaping_closures_share_cells\nPASS range_bounds_and_loops\n6 passed; 0 failed; 6 total\n"
            else
                "Rows: Ada Ada!\nBlue: true\nShared: 1 1 3\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    const unsupported_type = "this type is not supported in semantic analysis yet";
    const unsupported_operator = "operator is not supported for this operand type";
    const mismatch = "expression does not match the expected type";
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "void", .source = "let f:(a:[]void)={}", .diagnostic = unsupported_type },
        .{ .name = "type", .source = "let f:(a:[]type)={}", .diagnostic = unsupported_type },
        .{ .name = "nested-void", .source = "let f:(a:[][]void)={}", .diagnostic = unsupported_type },
        .{ .name = "function-membership", .source = "let f:(a:()->int)->bool=a in [a]", .diagnostic = unsupported_operator },
        .{ .name = "union-membership", .source = "let U=union{callback:()->int,none,}\nlet f:(a:U)->bool=a in [a]", .diagnostic = unsupported_operator },
        .{ .name = "record-membership", .source = "let R=struct{callback:()->int,}\nlet f:(a:R)->bool=a in [a]", .diagnostic = unsupported_operator },
        .{ .name = "nested-membership", .source = "let f:(a:[]()->int)->bool=a in [a]", .diagnostic = unsupported_operator },
        .{ .name = "range-membership", .source = "let f:(n:int)->bool={let r=n..n\nr in [r]}", .diagnostic = unsupported_operator },
        .{ .name = "function-equality", .source = "let f:(a:()->int)->bool=a==a", .diagnostic = unsupported_operator },
        .{ .name = "union-equality", .source = "let U=union{callback:()->int,none,}\nlet f:(a:U)->bool=a==a", .diagnostic = unsupported_operator },
        .{ .name = "record-equality", .source = "let R=struct{callback:()->int,}\nlet f:(a:R)->bool=a==a", .diagnostic = unsupported_operator },
        .{ .name = "nested-equality", .source = "let f:(a:[][]()->int)->bool=a==a", .diagnostic = unsupported_operator },
        .{ .name = "range-equality", .source = "let f:(n:int)->bool={let r=n..n\nr==r}", .diagnostic = unsupported_operator },
        .{ .name = "enum-nominal", .source = "let E=enum{x,}\nlet F=enum{x,}\nlet f:(a:E,b:F)={let x:[]E=[a,b]\n}", .diagnostic = mismatch },
        .{ .name = "enum-membership-nominal", .source = "let E=enum{x,}\nlet F=enum{x,}\nlet f:(a:E,b:F)->bool=a in [b]", .diagnostic = mismatch },
        .{ .name = "union-nominal", .source = "let U=union{none,}\nlet V=union{none,}\nlet f:(a:U,b:V)={let x:[]U=[a,b]\n}", .diagnostic = mismatch },
        .{ .name = "record-nominal", .source = "let R=struct{n:int,}\nlet S=struct{n:int,}\nlet f:(a:R,b:S)={let x:[]R=[a,b]\n}", .diagnostic = mismatch },
        .{ .name = "recursive-record", .source = "let R=struct{items:[][]R,}\nlet main:()={}", .diagnostic = "cyclic by-value struct layout" },
        .{ .name = "recursive-union", .source = "let U=union{items:[]U,none,}\nlet main:()={}", .diagnostic = "cyclic" },
        .{ .name = "recursive-generic", .source = "let G:(T:type)->type=struct{items:[]G(T),}\nlet f:(a:G(int))={}", .diagnostic = "cyclic generic container instantiation" },
    }) |case| {
        const source = b.fmt("invalid-storable-array-{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI storable arrays: reject {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source });
        check.expectExitCode(1);
        check.expectStdOutEqual("");
        check.expectStdErrMatch(b.fmt("{s}:", .{source}));
        check.expectStdErrMatch("error:");
        check.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&check.step);
    }
}

fn addGlobalCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("globals.nov", @embedFile("examples/globals.nov"));
    _ = fixtures.add("globals-state.nov", @embedFile("examples/globals-state.nov"));
    _ = fixtures.add("constants.nov", "@[public]\nlet fixed = 1\nlet hidden = 2\n");
    for ([_][]const u8{ "syntax", "semantic" }) |mode| {
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI globals: {s}", .{mode});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(if (std.mem.eql(u8, mode, "syntax")) &.{ "check", "globals.nov", "globals-state.nov" } else &.{ "check", "--semantic", "globals.nov" });
        check.expectExitCode(0);
        check.expectStdOutEqual("");
        check.expectStdErrEqual("");
        test_step.dependOn(&check.step);
    }
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test" }) |command| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI globals: {s} {s}", .{ command, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ command, "globals.nov" });
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (std.mem.eql(u8, command, "test"))
                "PASS live_global_reads\nPASS aggregate_copies_and_replacement\nPASS imported_storage_identity\n3 passed; 0 failed; 3 total\n"
            else
                "Initial: 8 true λ 1.5 Color.red\n" ++
                    "Live: 22 22 Snapshot: 20\n" ++
                    "Record: updated [1, 9, 3] Saved: [1, 2, 3]\n" ++
                    "Union: Message.values[[8, 9]]\n" ++
                    "Imported: 12 12 11 [12, 5] 7\n" ++
                    "Range: 6 Callback: 10\n" ++
                    "Replaced callback: 35\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "runtime-call", .source = "let f:()->int=7\nlet mut n=f()\nlet main:()={}", .diagnostic = "dependency analysis for this expression is not implemented yet" },
        .{ .name = "mutable-read", .source = "let mut n=1\nlet saved=n\nlet main:()={}", .diagnostic = "global initialization is unsupported" },
        .{ .name = "mutable-field-read", .source = "let s=@import(\"globals-state.nov\")\nlet saved=s.counter.value\nlet main:()={}", .diagnostic = "global initialization is unsupported" },
        .{ .name = "immutable-write", .source = "let n=1\nlet main:()={n=2}", .diagnostic = "assignment requires a mutable" },
        .{ .name = "imported-immutable-write", .source = "let s=@import(\"constants.nov\")\nlet main:()={s.fixed=2}", .diagnostic = "assignment requires a mutable" },
        .{ .name = "private-read", .source = "let s=@import(\"globals-state.nov\")\nlet main:()=@println(s.secret)", .diagnostic = "error:" },
        .{ .name = "private-write", .source = "let s=@import(\"globals-state.nov\")\nlet main:()={s.secret=2}", .diagnostic = "error:" },
        .{ .name = "borrow", .source = "let mut n=1\nlet main:()={let p=&n\n@println(p.*)}", .diagnostic = "nonlocal owners" },
        .{ .name = "borrow-array", .source = "let mut items=[1,2]\nlet main:()={let p=&items[0]\n@println(p.*)}", .diagnostic = "nonlocal owners" },
        .{ .name = "borrow-import", .source = "let s=@import(\"globals-state.nov\")\nlet main:()={let p=&s.count\n@println(p.*)}", .diagnostic = "nonlocal owners" },
    }) |case| {
        const source = b.fmt("invalid-global-{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        for ([_][]const u8{ "semantic", "emit-c" }) |command| {
            const check = b.addRunArtifact(exe);
            check.step.name = b.fmt("CLI globals: reject {s} ({s})", .{ case.name, command });
            check.setCwd(fixtures.getDirectory());
            if (std.mem.eql(u8, command, "semantic")) check.addArgs(&.{ "check", "--semantic" }) else check.addArg("emit-c");
            check.addArg(source);
            check.expectExitCode(1);
            check.expectStdOutEqual("");
            check.expectStdErrMatch(b.fmt("{s}:", .{source}));
            check.expectStdErrMatch(case.diagnostic);
            test_step.dependOn(&check.step);
        }
    }
}

fn addPrintingCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("printing.nov", @embedFile("examples/printing.nov"));
    const output = b.fmt(
        "Types: bool []rune Entry Message (i{d}) -> bool\nArray type: []i{d}\n" ++
            "Array: [1, 2, 3]\nNested: [[true, false], []]\n" ++
            "Struct: Entry[ name: Ada, scores: [1, 2, 3], color: Color.blue ]\n" ++
            "Unions: [Message.entry[ name: Ada, scores: [1, 2, 3], color: Color.blue ], Message.none]\n" ++
            "Enum: Color.red\nCustom: Label(custom)\nNested custom: [Label(custom)]\n",
        .{ @bitSizeOf(usize), @bitSizeOf(usize) },
    );
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test" }) |command| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI printing: {s} {s}", .{ command, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ command, "printing.nov" });
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (std.mem.eql(u8, command, "run")) output else "PASS aggregate_interpolation\nPASS hooks_and_types\n2 passed; 0 failed; 2 total\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
}

fn addArrayHelperCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    _ = fixtures.add("array-helpers.nov", @embedFile("examples/array-helpers.nov"));
    _ = fixtures.add("api.nov",
        \\@[public]
        \\let array = @import("std").array
        \\@[public]
        \\let map = array.map
        \\@[public]
        \\let filter = @import("lib/array.nov").filter
        \\@[public]
        \\let fold = array.fold
        \\
    );
    _ = fixtures.add("aliases.nov",
        \\let standard = @import("std")
        \\let array = standard.array
        \\let direct = @import("lib/array.nov")
        \\let api = @import("api.nov")
        \\let map = api.map
        \\let filter = api.filter
        \\let fold = api.fold
        \\let identity: (value: `T) -> `T = value
        \\let verify: () = {
        \\    let items: []int = [1, 2, 3]
        \\    let callable: ([]int, (int) -> int) -> []int = array.map
        \\    let select: ([]int, (int) -> bool) -> []int = direct.filter
        \\    let reduce: ([]int, int, (int, int) -> int) -> int = api.fold
        \\    let mapped = map(items, identity)
        \\    let copied = callable(mapped, identity)
        \\    let keep = (value: int) -> bool { value > 1 }
        \\    let selected = filter(copied, keep)
        \\    let again = select(selected, keep)
        \\    let subtract = (total: int, value: int) -> int { total - value }
        \\    @assert(fold(again, @as(int, 10), subtract) == 5)
        \\    @assert(reduce(items, @as(int, 10), subtract) == 4)
        \\    @assert(direct.map([true, false], identity)[0])
        \\    @assert(api.array.map(["public"], identity)[0] == "public")
        \\    let mut events: int = 0
        \\    let mut source: []int = [1, 2]
        \\    let getFilter = () -> ([]int, (int) -> bool) -> []int {
        \\        events = events * 10 + 1
        \\        array.filter
        \\    }
        \\    let frozen = getFilter()(
        \\        {
        \\            events = events * 10 + 2
        \\            source
        \\        },
        \\        {
        \\            events = events * 10 + 3
        \\            source[0] = 9
        \\            (value: int) -> bool {
        \\                events = events * 10 + value
        \\                source[1] = 8
        \\                true
        \\            }
        \\        }
        \\    )
        \\    @assert(events == 12312)
        \\    @assert(frozen[0] == 1 and frozen[1] == 2)
        \\    @assert(source[0] == 9 and source[1] == 8)
        \\    @println(copied[0], again[0], again[1])
        \\}
        \\@[test]
        \\let import_aliases_and_callable_generics: () = verify()
        \\let main: () = verify()
        \\
    );
    // Namespace-only modules need semantic checking, not output compilation.
    for ([_][]const u8{ "array-helpers.nov", "lib/std.nov", "lib/array.nov", "api.nov", "aliases.nov" }) |source| {
        for ([_]bool{ false, true }) |semantic| {
            if (!semantic and (std.mem.eql(u8, source, "api.nov") or std.mem.eql(u8, source, "aliases.nov"))) continue;
            const check = b.addRunArtifact(exe);
            check.step.name = b.fmt("CLI array helpers: {s} {s}", .{ if (semantic) "semantic" else "fmt", source });
            check.setCwd(fixtures.getDirectory());
            check.addArgs(if (semantic) &.{ "check", "--semantic", source } else &.{ "fmt", "--check", source });
            if (semantic) {
                check.addArg("--lib-dir");
                check.addDirectoryArg(library);
            }
            check.expectExitCode(0);
            check.expectStdOutEqual("");
            check.expectStdErrEqual("");
            test_step.dependOn(&check.step);
        }
    }
    const example_output = "Numbers: [1, 2, 3, 4]\nMapped: [2, 4, 6, 8]\nFiltered: [6, 8]\nFolded: 14\n";
    const example_tests = "PASS map_types_and_generic_callbacks\nPASS filter_order_and_exact_calls\nPASS fold_left_seed_and_empty\nPASS recursive_array_record_and_union_copies\nPASS function_elements_share_environments\nPASS argument_order_captures_and_snapshots\n6 passed; 0 failed; 6 total\n";
    const package = b.addWriteFiles();
    const installed_exe = package.addCopyFile(exe.getEmittedBin(), b.fmt("bin/{s}", .{exe.out_filename}));
    _ = package.addCopyDirectory(b.path("lib"), "lib/nov", .{});
    for ([_]bool{ false, true }) |installed| {
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            for ([_][]const u8{ "run", "test" }) |command| {
                for ([_][]const u8{ "array-helpers.nov", "aliases.nov" }) |source| {
                    const run = if (installed) std.Build.Step.Run.create(b, "CLI array helpers: relocated install") else b.addRunArtifact(exe);
                    run.step.name = b.fmt("CLI array helpers: {s} {s} {s} ({s}, installed={})", .{ command, source, mode, if (installed) "default library" else "explicit library", installed });
                    if (installed) run.addFileArg(installed_exe);
                    run.setCwd(fixtures.getDirectory());
                    run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                    run.setEnvironmentVariable("NOV_CC", "");
                    run.setEnvironmentVariable("CC", "");
                    run.addArgs(&.{ command, source });
                    if (mode.len != 0) run.addArg(mode);
                    if (!installed) {
                        run.addArg("--lib-dir");
                        run.addDirectoryArg(library);
                    }
                    const is_test = std.mem.eql(u8, command, "test");
                    const example = std.mem.eql(u8, source, "array-helpers.nov");
                    run.expectExitCode(0);
                    run.expectStdOutEqual(if (example) (if (is_test) example_tests else example_output) else if (is_test) "1 2 3\nPASS import_aliases_and_callable_generics\n1 passed; 0 failed; 1 total\n" else "1 2 3\n");
                    run.expectStdErrEqual("");
                    test_step.dependOn(&run.step);
                }
            }
        }
    }
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "map-parameter", .source = "std.array.map(@as([]int,[1]),(x:bool)->bool{x})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "map-arity", .source = "std.array.map(@as([]int,[1]),(x:int,y:int)->int{x+y})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "filter-result", .source = "std.array.filter(@as([]int,[1]),(x:int)->int{x})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "filter-parameter", .source = "std.array.filter(@as([]int,[1]),(x:bool)->bool{x})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "fold-result", .source = "std.array.fold(@as([]int,[1]),@as(int,0),(a:int,x:int)->bool{true})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "fold-arity", .source = "std.array.fold(@as([]int,[1]),@as(int,0),(x:int)->int{x})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "map-empty", .source = "std.array.map([],identity)", .diagnostic = "provide typed runtime arguments" },
        .{ .name = "filter-empty", .source = "std.array.filter([],(x:`T)->bool{true})", .diagnostic = "provide typed runtime arguments" },
        .{ .name = "fold-empty", .source = "std.array.fold([],@as(int,0),(a:int,x:`T)->int{a})", .diagnostic = "provide typed runtime arguments" },
        .{ .name = "fold-defaulted-initial", .source = "std.array.fold(@as([]int,[1]),0,(a:`U,x:int)->`U{a})", .diagnostic = "" },
        .{ .name = "map-void", .source = "std.array.map(@as([]int,[1]),(x:int)->void{})", .diagnostic = "this type is not supported in semantic analysis yet" },
    }) |case| {
        const source = b.fmt("invalid-array-helper-{s}.nov", .{case.name});
        _ = fixtures.add(source, b.fmt("let std=@import(\"std\")\nlet identity:(x:`T)->`T=x\nlet main:()={{let value={s}\n}}\n", .{case.source}));
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI array helpers: {s} {s}", .{ if (case.diagnostic.len == 0) "accept" else "reject", case.name });
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source, "--lib-dir" });
        check.addDirectoryArg(library);
        check.expectExitCode(if (case.diagnostic.len == 0) 0 else 1);
        check.expectStdOutEqual("");
        if (case.diagnostic.len == 0) {
            check.expectStdErrEqual("");
        } else {
            check.expectStdErrMatch(".nov:");
            check.expectStdErrMatch("error:");
            check.expectStdErrMatch(case.diagnostic);
        }
        test_step.dependOn(&check.step);
    }
    for ([_][]const u8{ "map", "filter", "fold" }) |helper| {
        const source = b.fmt("panic-array-helper-{s}.nov", .{helper});
        const callback = if (std.mem.eql(u8, helper, "fold")) "(total:int,value:int)->int" else if (std.mem.eql(u8, helper, "filter")) "(value:int)->bool" else "(value:int)->int";
        _ = fixtures.add(source, b.fmt("let std=@import(\"std\")\nlet verify:()={{let value=std.array.{s}(@as([]int,[1,2]),{s}{s}{{@print(value)\n@panic(\"{s} callback\")}})\n@println(\"unreachable\")}}\n@[test]\nlet callback_panic:()=verify()\nlet main:()=verify()\n", .{ helper, if (std.mem.eql(u8, helper, "fold")) "@as(int,0)," else "", callback, helper }));
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            for ([_][]const u8{ "run", "test" }) |command| {
                const run = b.addRunArtifact(exe);
                run.step.name = b.fmt("CLI array helpers: {s} {s} panic ({s})", .{ command, helper, mode });
                run.setCwd(fixtures.getDirectory());
                run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                run.setEnvironmentVariable("NOV_CC", "");
                run.setEnvironmentVariable("CC", "");
                run.addArgs(&.{ command, source, "--lib-dir" });
                run.addDirectoryArg(library);
                if (mode.len != 0) run.addArg(mode);
                run.expectExitCode(1);
                run.expectStdOutEqual(if (std.mem.eql(u8, command, "test")) "1FAIL callback_panic (exit 1)\n0 passed; 1 failed; 1 total\n" else "1");
                run.expectStdErrEqual(b.fmt("panic: {s} callback\n", .{helper}));
                test_step.dependOn(&run.step);
            }
        }
    }
}

fn addStrCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    _ = fixtures.add("str.nov",
        \\let std=@import("std")
        \\let Label=struct{value:str,
        \\@[stringify]
        \\let render:(self:Label)->str=self.value
        \\}
        \\let Keyed=struct{string:str,}
        \\let main:()={let mut value:str="a"
        \\let p=&value
        \\p.*+="b"
        \\let items:[]str=[p.*]
        \\@println(str,@TypeOf(value),@TypeOf(p),@TypeOf(items))
        \\@println(Label.[value=p.*],std.string.parseInt("42").unwrapOr(0))
        \\let keyed=Keyed.[string="string"]
        \\@println(keyed.string)}
        \\
    );
    _ = fixtures.add("alias.nov", "let string=str\nlet text:string=\"string\"\nlet main:()=@println(@TypeOf(text),text)\n");
    for ([_][]const u8{ "str.nov", "alias.nov" }) |source| {
        const check = b.addRunArtifact(exe);
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source, "--lib-dir" });
        check.addDirectoryArg(library);
        check.expectExitCode(0);
        check.expectStdOutEqual("");
        check.expectStdErrEqual("");
        test_step.dependOn(&check.step);
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            const run = b.addRunArtifact(exe);
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ "run", source, "--lib-dir" });
            run.addDirectoryArg(library);
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (std.mem.eql(u8, source, "str.nov")) "str str &mut str []str\nab 42\nstring\n" else "str string\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    for ([_][]const u8{ "let value:string=\"x\"", "let value:[]string=[]", "let f:(text:string)->str=text" }, 0..) |source, i| {
        const path = b.fmt("old-string-{d}.nov", .{i});
        _ = fixtures.add(path, source);
        for ([_]bool{ false, true }) |emit| {
            const reject = b.addRunArtifact(exe);
            reject.setCwd(fixtures.getDirectory());
            reject.addArgs(if (emit) &.{ "emit-c", path } else &.{ "check", "--semantic", path });
            reject.expectExitCode(1);
            reject.expectStdOutEqual("");
            reject.expectStdErrMatch("unknown type 'string'");
            test_step.dependOn(&reject.step);
        }
    }
}

fn addIntegerParsingCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    _ = fixtures.add("integer-parsing.nov", @embedFile("examples/integer-parsing.nov"));
    const aliases_source =
        \\let std=@import("std")
        \\let direct=@import("lib/string.nov")
        \\let other=@import("lib/./string.nov")
        \\let verify:()={let signed:(str)->std.Result(int,other.ParseIntError)=direct.parseInt
        \\let unsigned:(str)->std.Result(uint,direct.ParseIntError)=std.string.parseUint
        \\@assert(signed("-42").unwrapOr(0)==-42 and unsigned("+7").unwrapOr(0)==7)
        \\let result:std.Result(int,direct.ParseIntError)=other.parseInt("bad")
        \\@assert(match result{.ok=>false
        \\.err=>|error| error==.invalidInput})}
        \\let main:()=verify()
        \\@[test]
        \\let imported_aliases:()=verify()
        \\
    ;
    _ = fixtures.add("aliases.nov", aliases_source);
    const package = b.addWriteFiles();
    const installed_exe = package.addCopyFile(exe.getEmittedBin(), b.fmt("bin/{s}", .{exe.out_filename}));
    _ = package.addCopyDirectory(b.path("lib"), "lib/nov", .{});
    _ = package.add("integer-parsing.nov", @embedFile("examples/integer-parsing.nov"));
    _ = package.add("aliases.nov", std.mem.replaceOwned(u8, b.allocator, aliases_source, "lib/", "lib/nov/") catch @panic("OOM"));
    for ([_][]const u8{ "integer-parsing.nov", "aliases.nov", "lib/string.nov", "lib/std.nov" }) |source| {
        const check = b.addRunArtifact(exe);
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source, "--lib-dir" });
        check.addDirectoryArg(library);
        check.expectExitCode(0);
        check.expectStdOutEqual("");
        check.expectStdErrEqual("");
        test_step.dependOn(&check.step);
        if (std.mem.eql(u8, source, "aliases.nov")) continue;
        const fmt = b.addRunArtifact(exe);
        fmt.setCwd(fixtures.getDirectory());
        fmt.addArgs(&.{ "fmt", "--check", source });
        fmt.expectExitCode(0);
        fmt.expectStdOutEqual("");
        fmt.expectStdErrEqual("");
        test_step.dependOn(&fmt.step);
    }
    const output = b.fmt("Signed: Result(i{0d}, ParseIntError).ok[-42]\nUnsigned: Result(u{0d}, ParseIntError).ok[123]\nInvalid: Result(i{0d}, ParseIntError).err[ParseIntError.invalidInput]\nOverflow: Result(u{0d}, ParseIntError).err[ParseIntError.overflow]\n", .{@bitSizeOf(usize)});
    const tests = "PASS decimal_signs_and_leading_zeros\nPASS strict_ascii_input\nPASS native_signed_limits\nPASS native_unsigned_limits\nPASS propagation_callbacks_and_queries\n5 passed; 0 failed; 5 total\n";
    for ([_]bool{ false, true }) |installed| {
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            for ([_][]const u8{ "run", "test" }) |command| {
                for ([_][]const u8{ "integer-parsing.nov", "aliases.nov" }) |source| {
                    const run = if (installed) std.Build.Step.Run.create(b, "CLI integer parsing: relocated install") else b.addRunArtifact(exe);
                    if (installed) run.addFileArg(installed_exe);
                    run.setCwd(if (installed) package.getDirectory() else fixtures.getDirectory());
                    run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                    run.setEnvironmentVariable("NOV_CC", "");
                    run.setEnvironmentVariable("CC", "");
                    run.addArgs(&.{ command, source });
                    if (mode.len != 0) run.addArg(mode);
                    if (!installed) {
                        run.addArg("--lib-dir");
                        run.addDirectoryArg(library);
                    }
                    const is_test = std.mem.eql(u8, command, "test");
                    const example = std.mem.eql(u8, source, "integer-parsing.nov");
                    run.expectExitCode(0);
                    run.expectStdOutEqual(if (example) (if (is_test) tests else output) else if (is_test) "PASS imported_aliases\n1 passed; 0 failed; 1 total\n" else "");
                    run.expectStdErrEqual("");
                    test_step.dependOn(&run.step);
                }
            }
        }
    }
    for ([_][]const u8{
        "let main:()={let result=std.string.parseInt(42)\n}",
        "let main:()={let result=std.string.parseUint([49])\n}",
        "let main:()={let result:std.Result(uint,std.string.ParseIntError)=std.string.parseInt(\"1\")\n}",
        "let result=std.string.parseInt(\"1\")\nlet main:()={}",
    }, 0..) |source, i| {
        const path = b.fmt("bad-parse-{d}.nov", .{i});
        _ = fixtures.add(path, b.fmt("let std=@import(\"std\")\n{s}", .{source}));
        const reject = b.addRunArtifact(exe);
        reject.setCwd(fixtures.getDirectory());
        reject.addArgs(&.{ "check", "--semantic", path, "--lib-dir" });
        reject.addDirectoryArg(library);
        reject.expectExitCode(1);
        reject.expectStdOutEqual("");
        reject.expectStdErrMatch("error:");
        test_step.dependOn(&reject.step);
    }
}

fn addStringSearchCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    _ = fixtures.add("string-search.nov", @embedFile("examples/string-search.nov"));
    const aliases_source =
        \\let standard = @import("std")
        \\let direct = @import("lib/string.nov")
        \\let option = @import("lib/option.nov")
        \\let alternate = @import("lib/./option.nov")
        \\let other = @import("lib/./string.nov")
        \\let verify: () = {
        \\    let startsWith = direct.startsWith
        \\    let endsWith = other.endsWith
        \\    let find = other.find
        \\    let contains = direct.contains
        \\    let search: (str, str) -> option.Option(uint) = find
        \\    let locate: (str, str) -> alternate.Option(uint) = standard.string.find
        \\    let found: standard.Option(uint) = search("é😀z", "😀")
        \\    let index: standard.Option(uint) = locate("banana", "ana")
        \\    @assert(found.unwrapOr(99) == 1 and index.unwrapOr(99) == 1)
        \\    @assert(startsWith("é😀", "é") and endsWith("é😀", "😀"))
        \\    @assert(standard.string.startsWith("abc", "a") and other.contains("abc", "b"))
        \\    @assert(contains("a\0b", "\0") and direct.endsWith("abc", "c"))
        \\    @assert(direct.find("abc", "z").isNone())
        \\    @assert(search("", "").unwrapOr(99) == 0)
        \\}
        \\@[test]
        \\let aliases_and_canonical_option: () = verify()
        \\let main: () = verify()
        \\
    ;
    _ = fixtures.add("aliases.nov", aliases_source);
    for ([_][]const u8{ "string-search.nov", "lib/std.nov", "lib/string.nov", "aliases.nov" }) |source| {
        for ([_]bool{ false, true }) |semantic| {
            if (!semantic and std.mem.eql(u8, source, "aliases.nov")) continue;
            const check = b.addRunArtifact(exe);
            check.step.name = b.fmt("CLI string search: {s} {s}", .{ if (semantic) "semantic" else "fmt", source });
            check.setCwd(fixtures.getDirectory());
            check.addArgs(if (semantic) &.{ "check", "--semantic", source } else &.{ "fmt", "--check", source });
            if (semantic) {
                check.addArg("--lib-dir");
                check.addDirectoryArg(library);
            }
            check.expectExitCode(0);
            check.expectStdOutEqual("");
            check.expectStdErrEqual("");
            test_step.dependOn(&check.step);
        }
    }
    const example_output = "Prefix: true\nSuffix: true\nFirst: 1\nScalar index: 2\nContains: false\n";
    const example_tests = "PASS ascii_prefix_suffix_and_first_match\nPASS empty_and_oversized_patterns\nPASS unicode_scalars_nul_and_exact_matching\nPASS views_and_managed_owner_lifetimes\nPASS callable_and_argument_source_order\n5 passed; 0 failed; 5 total\n";
    const package = b.addWriteFiles();
    const installed_exe = package.addCopyFile(exe.getEmittedBin(), b.fmt("bin/{s}", .{exe.out_filename}));
    _ = package.addCopyDirectory(b.path("lib"), "lib/nov", .{});
    _ = package.add("string-search.nov", @embedFile("examples/string-search.nov"));
    _ = package.add("aliases.nov", std.mem.replaceOwned(u8, b.allocator, aliases_source, "lib/", "lib/nov/") catch @panic("OOM"));
    for ([_]bool{ false, true }) |installed| {
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            for ([_][]const u8{ "run", "test" }) |command| {
                for ([_][]const u8{ "string-search.nov", "aliases.nov" }) |source| {
                    const run = if (installed) std.Build.Step.Run.create(b, "CLI string search: relocated install") else b.addRunArtifact(exe);
                    run.step.name = b.fmt("CLI string search: {s} {s} {s} (installed={})", .{ command, source, mode, installed });
                    if (installed) run.addFileArg(installed_exe);
                    run.setCwd(if (installed) package.getDirectory() else fixtures.getDirectory());
                    run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                    run.setEnvironmentVariable("NOV_CC", "");
                    run.setEnvironmentVariable("CC", "");
                    run.addArgs(&.{ command, source });
                    if (mode.len != 0) run.addArg(mode);
                    if (!installed) {
                        run.addArg("--lib-dir");
                        run.addDirectoryArg(library);
                    }
                    const is_test = std.mem.eql(u8, command, "test");
                    const example = std.mem.eql(u8, source, "string-search.nov");
                    run.expectExitCode(0);
                    run.expectStdOutEqual(if (example) (if (is_test) example_tests else example_output) else if (is_test) "PASS aliases_and_canonical_option\n1 passed; 0 failed; 1 total\n" else "");
                    run.expectStdErrEqual("");
                    test_step.dependOn(&run.step);
                }
            }
        }
    }
    for ([_]struct { name: []const u8, expression: []const u8 }{
        .{ .name = "prefix-text", .expression = "std.string.startsWith(1,\"a\")" },
        .{ .name = "suffix-pattern", .expression = "std.string.endsWith(\"abc\",true)" },
        .{ .name = "find-pattern", .expression = "std.string.find(\"abc\",'a')" },
        .{ .name = "contains-text", .expression = "std.string.contains([1],\"a\")" },
    }) |case| {
        const source = b.fmt("invalid-string-search-{s}.nov", .{case.name});
        _ = fixtures.add(source, b.fmt("let std=@import(\"std\")\nlet main:()={{let result={s}\n}}\n", .{case.expression}));
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI string search: reject {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source, "--lib-dir" });
        check.addDirectoryArg(library);
        check.expectExitCode(1);
        check.expectStdOutEqual("");
        check.expectStdErrMatch(".nov:");
        check.expectStdErrMatch("error:");
        test_step.dependOn(&check.step);
    }
}

fn addStringConversionCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    _ = fixtures.add("string-conversions.nov", @embedFile("examples/string-conversions.nov"));
    const aliases_source =
        \\let standard = @import("std")
        \\let direct = @import("lib/string.nov")
        \\let alternate = @import("lib/./string.nov")
        \\let result = @import("lib/result.nov")
        \\let verify: () = {
        \\    let encode: (str) -> []u8 = direct.toBytes
        \\    let decode: ([]u8) -> standard.Result(str, alternate.Utf8Error) = alternate.fromBytes
        \\    let bytes = encode("Aé😀\0")
        \\    let text: result.Result(str, direct.Utf8Error) = decode(bytes)
        \\    @assert(text.unwrapOr("bad") == "Aé😀\0" and @len(bytes) == 8)
        \\    @assert(standard.string.fromBytes(direct.toBytes("é")).unwrapOr("") == "é")
        \\    @assert(alternate.fromBytes(standard.string.toBytes("😀")).unwrapOr("") == "😀")
        \\    @assert(@stringFromBytes(@stringToBytes("a\0é😀")) == "a\0é😀")
        \\    let empty: []u8 = @stringToBytes("")
        \\    @assert(@stringFromBytes(empty) == "")
        \\    match decode([0xFF]) {
        \\        .ok => @panic("invalid UTF-8 accepted")
        \\        .err => |error| {
        \\            let canonical: direct.Utf8Error = error
        \\            match canonical {
        \\                .invalidUtf8 => {}
        \\            }
        \\        }
        \\    }
        \\}
        \\@[test]
        \\let imported_aliases_and_canonical_types: () = verify()
        \\let main: () = verify()
        \\
    ;
    _ = fixtures.add("aliases.nov", aliases_source);
    for ([_][]const u8{ "string-conversions.nov", "aliases.nov", "lib/string.nov", "lib/std.nov" }) |source| {
        for ([_]bool{ false, true }) |semantic| {
            if (!semantic and std.mem.eql(u8, source, "aliases.nov")) continue;
            const check = b.addRunArtifact(exe);
            check.step.name = b.fmt("CLI string conversions: {s} {s}", .{ if (semantic) "semantic" else "fmt", source });
            check.setCwd(fixtures.getDirectory());
            check.addArgs(if (semantic) &.{ "check", "--semantic", source } else &.{ "fmt", "--check", source });
            if (semantic) {
                check.addArg("--lib-dir");
                check.addDirectoryArg(library);
            }
            check.expectExitCode(0);
            check.expectStdOutEqual("");
            check.expectStdErrEqual("");
            test_step.dependOn(&check.step);
        }
    }
    const example_output = "UTF-8 bytes: [65, 195, 169, 240, 159, 152, 128]\nRound trip: Aé😀\nIndependent snapshot: Aé😀\nInvalid UTF-8: Utf8Error.invalidUtf8\nNUL: 3 bytes, 3 scalars\n";
    const example_tests = "PASS empty_ascii_and_nul\nPASS valid_two_and_three_byte_boundaries\nPASS valid_four_byte_boundaries_and_state_reset\nPASS invalid_leads_and_stray_continuations\nPASS invalid_overlong_surrogate_and_out_of_range\nPASS invalid_continuations_and_truncation\nPASS exact_bytes_no_normalization_and_copy_isolation\nPASS source_scope_and_gc_churn\nPASS propagation_error_matching_and_first_class_wrappers\n9 passed; 0 failed; 9 total\n";
    const package = b.addWriteFiles();
    const installed_exe = package.addCopyFile(exe.getEmittedBin(), b.fmt("bin/{s}", .{exe.out_filename}));
    _ = package.addCopyDirectory(b.path("lib"), "lib/nov", .{});
    _ = package.add("string-conversions.nov", @embedFile("examples/string-conversions.nov"));
    _ = package.add("aliases.nov", std.mem.replaceOwned(u8, b.allocator, aliases_source, "lib/", "lib/nov/") catch @panic("OOM"));
    for ([_]bool{ false, true }) |installed| {
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            for ([_][]const u8{ "run", "test" }) |command| {
                for ([_][]const u8{ "string-conversions.nov", "aliases.nov" }) |source| {
                    const run = if (installed) std.Build.Step.Run.create(b, "CLI string conversions: relocated install") else b.addRunArtifact(exe);
                    run.step.name = b.fmt("CLI string conversions: {s} {s} {s} (installed={})", .{ command, source, mode, installed });
                    if (installed) run.addFileArg(installed_exe);
                    run.setCwd(if (installed) package.getDirectory() else fixtures.getDirectory());
                    run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                    run.setEnvironmentVariable("NOV_CC", "");
                    run.setEnvironmentVariable("CC", "");
                    run.addArgs(&.{ command, source });
                    if (mode.len != 0) run.addArg(mode);
                    if (!installed) {
                        run.addArg("--lib-dir");
                        run.addDirectoryArg(library);
                    }
                    const is_test = std.mem.eql(u8, command, "test");
                    const example = std.mem.eql(u8, source, "string-conversions.nov");
                    run.expectExitCode(0);
                    run.expectStdOutEqual(if (example) (if (is_test) example_tests else example_output) else if (is_test) "PASS imported_aliases_and_canonical_types\n1 passed; 0 failed; 1 total\n" else "");
                    run.expectStdErrEqual("");
                    test_step.dependOn(&run.step);
                }
            }
        }
    }
    for ([_][]const u8{ "[0xFF]", "[0xC0, 0x80]", "[0xED, 0xA0, 0x80]", "[0xF4, 0x90, 0x80, 0x80]", "[0xF0, 0x90, 0x80]" }, 0..) |bytes, index| {
        const source = b.fmt("panic-invalid-utf8-{d}.nov", .{index});
        _ = fixtures.add(source, b.fmt("let main:()={{let bytes:[]u8={s}\n@println(@stringFromBytes(bytes))\n}}\n", .{bytes}));
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI string conversions: invalid UTF-8 {d} {s}", .{ index, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ "run", source, "--lib-dir" });
            run.addDirectoryArg(library);
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(1);
            run.expectStdOutEqual("");
            run.expectStdErrEqual("panic: invalid UTF-8\n");
            test_step.dependOn(&run.step);
        }
    }
    for ([_]bool{ false, true }) |is_builtin| {
        for ([_]struct { name: []const u8, encode: bool, args: []const u8 }{
            .{ .name = "to-int", .encode = true, .args = "1" },
            .{ .name = "to-byte", .encode = true, .args = "@as(u8,65)" },
            .{ .name = "to-rune", .encode = true, .args = "'A'" },
            .{ .name = "to-array", .encode = true, .args = "[1]" },
            .{ .name = "to-missing", .encode = true, .args = "" },
            .{ .name = "to-extra", .encode = true, .args = "\"a\",\"b\"" },
            .{ .name = "from-string", .encode = false, .args = "\"abc\"" },
            .{ .name = "from-byte", .encode = false, .args = "@as(u8,65)" },
            .{ .name = "from-rune", .encode = false, .args = "'A'" },
            .{ .name = "from-int-array", .encode = false, .args = "[@as(int,65)]" },
            .{ .name = "from-uint-array", .encode = false, .args = "[@as(uint,65)]" },
            .{ .name = "from-rune-array", .encode = false, .args = "['A']" },
            .{ .name = "from-float-array", .encode = false, .args = "[1.0]" },
            .{ .name = "from-missing", .encode = false, .args = "" },
            .{ .name = "from-extra", .encode = false, .args = "[],[]" },
        }) |case| {
            const source = b.fmt("invalid-string-conversion-{s}-{s}.nov", .{ if (is_builtin) "builtin" else "wrapper", case.name });
            const callee = if (is_builtin) (if (case.encode) "@stringToBytes" else "@stringFromBytes") else if (case.encode) "std.string.toBytes" else "std.string.fromBytes";
            _ = fixtures.add(source, b.fmt("let std=@import(\"std\")\nlet main:()={{let value={s}({s})\n}}\n", .{ callee, case.args }));
            const check = b.addRunArtifact(exe);
            check.step.name = b.fmt("CLI string conversions: reject {s} {s}", .{ callee, case.name });
            check.setCwd(fixtures.getDirectory());
            check.addArgs(&.{ "check", "--semantic", source, "--lib-dir" });
            check.addDirectoryArg(library);
            check.expectExitCode(1);
            check.expectStdOutEqual("");
            check.expectStdErrMatch(".nov:");
            check.expectStdErrMatch("error:");
            test_step.dependOn(&check.step);
        }
    }
}

fn addStringTransformCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    const aliases_source =
        \\let standard = @import("std")
        \\let direct = @import("lib/string.nov")
        \\let alternate = @import("lib/./string.nov")
        \\let api = @import("api.nov")
        \\let verify: () = {
        \\    let trim: (str) -> str = direct.trim
        \\    let replace: (str, str, str) -> str = alternate.replace
        \\    let repeat: (str, uint) -> str = api.repeat
        \\    @assert(trim(" \u{3000}é😀\0\t") == "é😀\0")
        \\    @assert(replace("é\0é", "é", "😀") == "😀\0😀")
        \\    @assert(repeat("é\0", 2) == "é\0é\0")
        \\    @assert(standard.string.trim(" x ") == alternate.trim(" x "))
        \\    @assert(direct.replace("aaaaa", "aa", "X") == "XXa")
        \\    let maximum: uint = 0 - @as(uint, 1)
        \\    @assert(standard.string.repeat("", maximum) == "")
        \\}
        \\@[test]
        \\let imported_aliases_and_exact_types: () = verify()
        \\let main: () = verify()
        \\
    ;
    const api_source =
        \\let strings = @import("lib/string.nov")
        \\@[public]
        \\let repeat = strings.repeat
        \\
    ;
    const package = b.addWriteFiles();
    const installed_exe = package.addCopyFile(exe.getEmittedBin(), b.fmt("bin/{s}", .{exe.out_filename}));
    _ = package.addCopyDirectory(b.path("lib"), "lib/nov", .{});
    for ([_]struct { source: []const u8, contents: []const u8, stdout: []const u8, tests: []const u8 }{
        .{
            .source = "string-transform.nov",
            .contents = @embedFile("examples/string-transform.nov"),
            .stdout = "Trimmed: [é😀]\nReplaced: bXna\nRepeated: é😀é😀é😀\nNUL scalars: 3\n",
            .tests = "PASS trim_every_white_space_scalar\nPASS trim_property_boundaries_and_preserved_content\nPASS replace_nonoverlap_growth_deletion_and_boundaries\nPASS replace_exact_utf8_nul_case_and_normalization\nPASS repeat_count_boundaries_and_exact_content\nPASS eager_callee_and_arguments_once_left_to_right\nPASS retained_views_and_results_after_scope_and_gc_churn\n7 passed; 0 failed; 7 total\n",
        },
        .{ .source = "aliases.nov", .contents = aliases_source, .stdout = "", .tests = "PASS imported_aliases_and_exact_types\n1 passed; 0 failed; 1 total\n" },
    }) |case| {
        _ = fixtures.add(case.source, case.contents);
        _ = package.add(case.source, std.mem.replaceOwned(u8, b.allocator, case.contents, "lib/", "lib/nov/") catch @panic("OOM"));
        for ([_]bool{ false, true }) |installed| {
            for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
                for ([_][]const u8{ "run", "test" }) |command| {
                    const run = if (installed) std.Build.Step.Run.create(b, "CLI string transform: relocated install") else b.addRunArtifact(exe);
                    run.step.name = b.fmt("CLI string transform: {s} {s} {s} (installed={})", .{ command, case.source, mode, installed });
                    if (installed) run.addFileArg(installed_exe);
                    run.setCwd(if (installed) package.getDirectory() else fixtures.getDirectory());
                    run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                    run.setEnvironmentVariable("NOV_CC", "");
                    run.setEnvironmentVariable("CC", "");
                    run.addArgs(&.{ command, case.source });
                    if (mode.len != 0) run.addArg(mode);
                    if (!installed) {
                        run.addArg("--lib-dir");
                        run.addDirectoryArg(library);
                    }
                    run.expectExitCode(0);
                    run.expectStdOutEqual(if (std.mem.eql(u8, command, "test")) case.tests else case.stdout);
                    run.expectStdErrEqual("");
                    test_step.dependOn(&run.step);
                }
            }
        }
    }
    _ = fixtures.add("api.nov", api_source);
    _ = package.add("api.nov", std.mem.replaceOwned(u8, b.allocator, api_source, "lib/", "lib/nov/") catch @panic("OOM"));
    for ([_][]const u8{ "string-transform.nov", "aliases.nov", "api.nov", "lib/string.nov", "lib/std.nov" }) |source| {
        const check = b.addRunArtifact(exe);
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source, "--lib-dir" });
        check.addDirectoryArg(library);
        check.expectExitCode(0);
        check.expectStdOutEqual("");
        check.expectStdErrEqual("");
        test_step.dependOn(&check.step);
    }
    for ([_][]const u8{ "string-transform.nov", "lib/string.nov", "lib/std.nov" }) |source| {
        const check = b.addRunArtifact(exe);
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "fmt", "--check", source });
        check.expectExitCode(0);
        check.expectStdOutEqual("");
        check.expectStdErrEqual("");
        test_step.dependOn(&check.step);
    }
    for ([_]struct { name: []const u8, expression: []const u8 }{
        .{ .name = "trim-rune", .expression = "std.string.trim('x')" },
        .{ .name = "trim-bytes", .expression = "std.string.trim(@as([]u8,[120]))" },
        .{ .name = "trim-missing", .expression = "std.string.trim()" },
        .{ .name = "trim-extra", .expression = "std.string.trim(\"x\",\"y\")" },
        .{ .name = "replace-text", .expression = "std.string.replace(1,\"a\",\"b\")" },
        .{ .name = "replace-pattern", .expression = "std.string.replace(\"a\",'a',\"b\")" },
        .{ .name = "replace-replacement", .expression = "std.string.replace(\"a\",\"a\",true)" },
        .{ .name = "replace-missing", .expression = "std.string.replace(\"a\",\"a\")" },
        .{ .name = "replace-extra", .expression = "std.string.replace(\"a\",\"a\",\"b\",\"c\")" },
        .{ .name = "repeat-text", .expression = "std.string.repeat('x',2)" },
        .{ .name = "repeat-int", .expression = "std.string.repeat(\"x\",@as(int,2))" },
        .{ .name = "repeat-u8", .expression = "std.string.repeat(\"x\",@as(u8,2))" },
        .{ .name = "repeat-float", .expression = "std.string.repeat(\"x\",2.0)" },
        .{ .name = "repeat-negative", .expression = "std.string.repeat(\"x\",-1)" },
        .{ .name = "repeat-missing", .expression = "std.string.repeat(\"x\")" },
        .{ .name = "repeat-extra", .expression = "std.string.repeat(\"x\",2,3)" },
        .{ .name = "trim-function", .expression = "@as((str)->uint,std.string.trim)" },
        .{ .name = "replace-function", .expression = "@as((str,str,rune)->str,std.string.replace)" },
        .{ .name = "repeat-function", .expression = "@as((str,int)->str,std.string.repeat)" },
    }) |case| {
        const source = b.fmt("invalid-transform-{s}.nov", .{case.name});
        _ = fixtures.add(source, b.fmt("let std=@import(\"std\")\nlet main:()={{let value={s}\n}}\n", .{case.expression}));
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI string transform: reject {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source, "--lib-dir" });
        check.addDirectoryArg(library);
        check.expectExitCode(1);
        check.expectStdOutEqual("");
        check.expectStdErrMatch(".nov:");
        check.expectStdErrMatch("error:");
        test_step.dependOn(&check.step);
    }
    for ([_][]const u8{ "\"text\"", "\"\"" }, 0..) |text, index| {
        const source = b.fmt("empty-pattern-{d}.nov", .{index});
        _ = fixtures.add(source, b.fmt(
            "let std=@import(\"std\")\n" ++
                "let callee:()->(str,str,str)->str={{@print(\"C\")\nstd.string.replace}}\n" ++
                "let verify:()={{let value=callee()({{@print(\"T\")\n{s}}},{{@print(\"P\")\n\"\"}},{{@print(\"R\")\n\"replacement\"}})\n@println(\"result\",value)}}\n" ++
                "@[test]\nlet empty_pattern:()=verify()\nlet main:()=verify()\n",
            .{text},
        ));
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            for ([_][]const u8{ "run", "test" }) |command| {
                const run = b.addRunArtifact(exe);
                run.step.name = b.fmt("CLI string transform: empty pattern {d} {s} {s}", .{ index, command, mode });
                run.setCwd(fixtures.getDirectory());
                run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                run.setEnvironmentVariable("NOV_CC", "");
                run.setEnvironmentVariable("CC", "");
                run.addArgs(&.{ command, source, "--lib-dir" });
                run.addDirectoryArg(library);
                if (mode.len != 0) run.addArg(mode);
                const output = b.fmt("CTPRresult {s}\n", .{if (index == 0) "text" else ""});
                run.expectExitCode(0);
                run.expectStdOutEqual(if (std.mem.eql(u8, command, "test")) b.fmt("{s}PASS empty_pattern\n1 passed; 0 failed; 1 total\n", .{output}) else output);
                run.expectStdErrEqual("");
                test_step.dependOn(&run.step);
            }
        }
    }
}

fn addCollectionHelperCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    const aliases_source =
        \\let std = @import("std")
        \\let strings = @import("lib/string.nov")
        \\let arrays = @import("lib/array.nov")
        \\let alternate = @import("lib/./array.nov")
        \\let verify: () = {
        \\    let join: ([]str, str) -> str = strings.join
        \\    let take: ([]int, uint) -> []int = arrays.take
        \\    let drop: ([]int, uint) -> []int = alternate.drop
        \\    let findLast: ([]int, (int) -> bool) -> std.Option(int) = arrays.findLast
        \\    @assert(join(["é", "😀"], "\0") == "é\0😀")
        \\    @assert(take([1, 2, 3], 2)[-1] == 2)
        \\    @assert(drop([1, 2, 3], 2)[0] == 3)
        \\    @assert(findLast([1, 2, 3], (x) { x < 3 }).unwrapOr(0) == 2)
        \\}
        \\@[test]
        \\let imported_aliases: () = verify()
        \\let main: () = verify()
        \\
    ;
    const cases = [_]struct { source: []const u8, contents: []const u8, stdout: []const u8, tests: []const u8 }{
        .{
            .source = "string-join.nov",
            .contents = @embedFile("examples/string-join.nov"),
            .stdout = "Joined: red, green, blue\nEmpty fields: |red||blue|\nConcatenated: é😀é\nEmpty: []\n",
            .tests = "PASS empty_singleton_and_empty_separator\nPASS separators_and_empty_parts\nPASS exact_utf8_nul_and_no_normalization\nPASS retained_result_and_source_unaffected\nPASS eager_arguments_once_left_to_right\n5 passed; 0 failed; 5 total\n",
        },
        .{
            .source = "array-selection.nov",
            .contents = @embedFile("examples/array-selection.nov"),
            .stdout = "Numbers: [3, 1, 4, 2]\nLast above two: 4\nTake two: [3, 1]\nDrop two: [4, 2]\nTake oversized: [3, 1, 4, 2]\nDrop oversized: []\n",
            .tests = "PASS reverse_visit_order_and_shortcircuit\nPASS typed_empty_no_predicate_calls\nPASS take_drop_boundaries_and_order\nPASS generic_storable_families\nPASS recursive_copies_in_both_directions\nPASS predicate_mutations_preserve_snapshot\nPASS closures_share_capture_environments\nPASS eager_arguments_once_left_to_right\n8 passed; 0 failed; 8 total\n",
        },
        .{ .source = "aliases.nov", .contents = aliases_source, .stdout = "", .tests = "PASS imported_aliases\n1 passed; 0 failed; 1 total\n" },
    };
    const package = b.addWriteFiles();
    const installed_exe = package.addCopyFile(exe.getEmittedBin(), b.fmt("bin/{s}", .{exe.out_filename}));
    _ = package.addCopyDirectory(b.path("lib"), "lib/nov", .{});
    for (cases) |case| {
        _ = fixtures.add(case.source, case.contents);
        _ = package.add(case.source, std.mem.replaceOwned(u8, b.allocator, case.contents, "lib/", "lib/nov/") catch @panic("OOM"));
        for ([_]bool{ false, true }) |semantic| {
            if (!semantic and std.mem.eql(u8, case.source, "aliases.nov")) continue;
            const check = b.addRunArtifact(exe);
            check.step.name = b.fmt("CLI collection helpers: {s} {s}", .{ if (semantic) "semantic" else "fmt", case.source });
            check.setCwd(fixtures.getDirectory());
            check.addArgs(if (semantic) &.{ "check", "--semantic", case.source } else &.{ "fmt", "--check", case.source });
            if (semantic) {
                check.addArg("--lib-dir");
                check.addDirectoryArg(library);
            }
            check.expectExitCode(0);
            check.expectStdOutEqual("");
            check.expectStdErrEqual("");
            test_step.dependOn(&check.step);
        }
        for ([_]bool{ false, true }) |installed| {
            for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
                for ([_][]const u8{ "run", "test" }) |command| {
                    const run = if (installed) std.Build.Step.Run.create(b, "CLI collection helpers: relocated install") else b.addRunArtifact(exe);
                    run.step.name = b.fmt("CLI collection helpers: {s} {s} {s} (installed={})", .{ command, case.source, mode, installed });
                    if (installed) run.addFileArg(installed_exe);
                    run.setCwd(if (installed) package.getDirectory() else fixtures.getDirectory());
                    run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                    run.setEnvironmentVariable("NOV_CC", "");
                    run.setEnvironmentVariable("CC", "");
                    run.addArgs(&.{ command, case.source });
                    if (mode.len != 0) run.addArg(mode);
                    if (!installed) {
                        run.addArg("--lib-dir");
                        run.addDirectoryArg(library);
                    }
                    run.expectExitCode(0);
                    run.expectStdOutEqual(if (std.mem.eql(u8, command, "test")) case.tests else case.stdout);
                    run.expectStdErrEqual("");
                    test_step.dependOn(&run.step);
                }
            }
        }
    }
    for ([_]struct { name: []const u8, expression: []const u8 }{
        .{ .name = "join-element", .expression = "std.string.join([1],\",\")" },
        .{ .name = "join-separator", .expression = "std.string.join([\"a\"],',')" },
        .{ .name = "join-arity", .expression = "std.string.join([\"a\"])" },
        .{ .name = "last-predicate", .expression = "std.array.findLast([1],(x:int)->int{x})" },
        .{ .name = "last-parameter", .expression = "std.array.findLast([1],(x:bool)->bool{x})" },
        .{ .name = "last-array", .expression = "std.array.findLast(1,(x:int)->bool{true})" },
        .{ .name = "take-negative", .expression = "std.array.take([1],-1)" },
        .{ .name = "take-signed", .expression = "std.array.take([1],@as(int,1))" },
        .{ .name = "take-array", .expression = "std.array.take(\"abc\",1)" },
        .{ .name = "take-arity", .expression = "std.array.take([1])" },
        .{ .name = "drop-negative", .expression = "std.array.drop([1],-1)" },
        .{ .name = "drop-bool", .expression = "std.array.drop([1],true)" },
        .{ .name = "drop-extra", .expression = "std.array.drop([1],1,2)" },
    }) |case| {
        const source = b.fmt("invalid-collection-{s}.nov", .{case.name});
        _ = fixtures.add(source, b.fmt("let std=@import(\"std\")\nlet main:()={{let value={s}\n}}\n", .{case.expression}));
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI collection helpers: reject {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source, "--lib-dir" });
        check.addDirectoryArg(library);
        check.expectExitCode(1);
        check.expectStdOutEqual("");
        check.expectStdErrMatch(".nov:");
        check.expectStdErrMatch("error:");
        test_step.dependOn(&check.step);
    }
}

fn addStringSplitCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    _ = fixtures.add("string-split.nov", @embedFile("examples/string-split.nov"));
    const aliases_source =
        \\let standard = @import("std")
        \\let direct = @import("lib/string.nov")
        \\let alternate = @import("lib/./string.nov")
        \\let verify: () = {
        \\    let split: (str, str) -> []str = direct.split
        \\    let other = alternate.split
        \\    let fields = split("é😀x\0e\u{301}😀x", "😀x")
        \\    @assert(@len(fields) == 3 and fields[0] == "é" and fields[1] == "\0e\u{301}" and fields[2] == "")
        \\    @assert(other("aaaaa", "aa")[2] == "a")
        \\    @assert(standard.string.split("", ",")[0] == "")
        \\    @assert(direct.split("a::b", "::")[1] == "b")
        \\}
        \\@[test]
        \\let imported_and_local_aliases: () = verify()
        \\let main: () = verify()
        \\
    ;
    _ = fixtures.add("aliases.nov", aliases_source);
    for ([_][]const u8{ "string-split.nov", "aliases.nov", "lib/string.nov", "lib/std.nov" }) |source| {
        for ([_]bool{ false, true }) |semantic| {
            if (!semantic and std.mem.eql(u8, source, "aliases.nov")) continue;
            const check = b.addRunArtifact(exe);
            check.step.name = b.fmt("CLI string split: {s} {s}", .{ if (semantic) "semantic" else "fmt", source });
            check.setCwd(fixtures.getDirectory());
            check.addArgs(if (semantic) &.{ "check", "--semantic", source } else &.{ "fmt", "--check", source });
            if (semantic) {
                check.addArg("--lib-dir");
                check.addDirectoryArg(library);
            }
            check.expectExitCode(0);
            check.expectStdOutEqual("");
            check.expectStdErrEqual("");
            test_step.dependOn(&check.step);
        }
    }
    const package = b.addWriteFiles();
    const installed_exe = package.addCopyFile(exe.getEmittedBin(), b.fmt("bin/{s}", .{exe.out_filename}));
    _ = package.addCopyDirectory(b.path("lib"), "lib/nov", .{});
    _ = package.add("string-split.nov", @embedFile("examples/string-split.nov"));
    _ = package.add("aliases.nov", std.mem.replaceOwned(u8, b.allocator, aliases_source, "lib/", "lib/nov/") catch @panic("OOM"));
    for ([_]bool{ false, true }) |installed| {
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            for ([_][]const u8{ "run", "test" }) |command| {
                for ([_][]const u8{ "string-split.nov", "aliases.nov" }) |source| {
                    const run = if (installed) std.Build.Step.Run.create(b, "CLI string split: relocated install") else b.addRunArtifact(exe);
                    run.step.name = b.fmt("CLI string split: {s} {s} {s} (installed={})", .{ command, source, mode, installed });
                    if (installed) run.addFileArg(installed_exe);
                    run.setCwd(if (installed) package.getDirectory() else fixtures.getDirectory());
                    run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                    run.setEnvironmentVariable("NOV_CC", "");
                    run.setEnvironmentVariable("CC", "");
                    run.addArgs(&.{ command, source });
                    if (mode.len != 0) run.addArg(mode);
                    if (!installed) {
                        run.addArg("--lib-dir");
                        run.addDirectoryArg(library);
                    }
                    const is_test = std.mem.eql(u8, command, "test");
                    const example = std.mem.eql(u8, source, "string-split.nov");
                    run.expectExitCode(0);
                    run.expectStdOutEqual(if (example) (if (is_test)
                        "PASS normal_and_preserved_empty_fields\nPASS empty_text_no_match_and_oversized_separator\nPASS nonoverlap_unicode_nul_and_exact_matching\nPASS retained_managed_views_and_array_copies\nPASS callee_and_arguments_once_left_to_right\n5 passed; 0 failed; 5 total\n"
                    else
                        "Fields: 5\n[]\n[red]\n[]\n[blue]\n[]\nScalar lengths: 1 3\n") else if (is_test) "PASS imported_and_local_aliases\n1 passed; 0 failed; 1 total\n" else "");
                    run.expectStdErrEqual("");
                    test_step.dependOn(&run.step);
                }
            }
        }
    }
    for ([_][]const u8{ "nonempty", "empty" }) |name| {
        const source = b.fmt("empty-separator-{s}.nov", .{name});
        _ = fixtures.add(source, b.fmt(
            \\let std = @import("std")
            \\let main: () = {{
            \\    let callee = () -> (str, str) -> []str {{
            \\        @println("callee")
            \\        std.string.split
            \\    }}
            \\    let text = () -> str {{
            \\        @println("text")
            \\        "{s}"
            \\    }}
            \\    let separator = () -> str {{
            \\        @println("separator")
            \\        ""
            \\    }}
            \\    let fields = callee()(text(), separator())
            \\    @assert(@len(fields) == 1)
            \\    @println("fields", @len(fields), fields[0])
            \\}}
            \\
        , .{if (std.mem.eql(u8, name, "empty")) "" else "abc"}));
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI string split: empty separator {s} {s}", .{ name, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ "run", source, "--lib-dir" });
            run.addDirectoryArg(library);
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(b.fmt("callee\ntext\nseparator\nfields 1 {s}\n", .{if (std.mem.eql(u8, name, "empty")) "" else "abc"}));
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    for ([_]struct { name: []const u8, expression: []const u8 }{
        .{ .name = "text-int", .expression = "std.string.split(1,\",\")" },
        .{ .name = "text-array", .expression = "std.string.split([1],\",\")" },
        .{ .name = "separator-bool", .expression = "std.string.split(\"abc\",true)" },
        .{ .name = "separator-rune", .expression = "std.string.split(\"abc\",',')" },
        .{ .name = "no-args", .expression = "std.string.split()" },
        .{ .name = "missing-separator", .expression = "std.string.split(\"abc\")" },
        .{ .name = "extra-arg", .expression = "std.string.split(\"abc\",\",\",\"extra\")" },
    }) |case| {
        const source = b.fmt("invalid-string-split-{s}.nov", .{case.name});
        _ = fixtures.add(source, b.fmt("let std=@import(\"std\")\nlet main:()={{let result={s}\n}}\n", .{case.expression}));
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI string split: reject {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source, "--lib-dir" });
        check.addDirectoryArg(library);
        check.expectExitCode(1);
        check.expectStdOutEqual("");
        check.expectStdErrMatch(".nov:");
        check.expectStdErrMatch("error:");
        test_step.dependOn(&check.step);
    }
}

fn addLastUseCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("last-use.nov", @embedFile("examples/last-use.nov"));
    for ([_]bool{ false, true }) |semantic| {
        const check = b.addRunArtifact(exe);
        check.setCwd(fixtures.getDirectory());
        check.addArgs(if (semantic) &.{ "check", "--semantic", "last-use.nov" } else &.{ "fmt", "--check", "last-use.nov" });
        check.expectExitCode(0);
        check.expectStdOutEqual("");
        check.expectStdErrEqual("");
        test_step.dependOn(&check.step);
    }
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test" }) |command| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI last use: {s} {s}", .{ command, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ command, "last-use.nov" });
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (std.mem.eql(u8, command, "run")) "Forwarded: [1, 2, 3]\nSource: [[1, 2], [3]]\nSnapshot: [[9], [3]]\n" else "PASS final_use_chains_preserve_contents\nPASS later_uses_keep_independent_values\nPASS captured_owners_are_not_consumed\nPASS repeated_loop_reads_and_borrows_remain_valid\n4 passed; 0 failed; 4 total\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
}

fn addArrayDefaultCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    _ = fixtures.add("array-defaults.nov", @embedFile("examples/array-defaults.nov"));
    _ = fixtures.add("defaults.nov", "@[public]\nlet R=struct{values:[][]str=[[\"é\\0\"],[]],}\n@[public]\nlet Buffer:(T:type)->type=struct{values:[]T=[],}\n");
    _ = fixtures.add("imported.nov",
        \\let api=@import("defaults.nov")
        \\let alias=@import("./defaults.nov")
        \\let global=api.R.[]
        \\let Query=@TypeOf(alias.R.[])
        \\let main:()={let mut a:Query=.[]
        \\let b=api.R.[]
        \\a.values[0]=["changed"]
        \\let buffer=api.Buffer(Query).[]
        \\@assert(b.values[0][0]=="é\0" and global.values[0][0]=="é\0" and @len(buffer.values)==0)
        \\@println("imported defaults independent")}
    );
    for ([_][]const u8{ "array-defaults.nov", "imported.nov" }) |source| {
        const check = b.addRunArtifact(exe);
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source, "--lib-dir" });
        check.addDirectoryArg(library);
        check.expectExitCode(0);
        check.expectStdOutEqual("");
        check.expectStdErrEqual("");
        test_step.dependOn(&check.step);
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI array defaults: run {s} {s}", .{ source, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ "run", source, "--lib-dir" });
            run.addDirectoryArg(library);
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (std.mem.eql(u8, source, "imported.nov")) "imported defaults independent\n" else "First: [65] [changed, second] [[9], [], [3]]\nSecond: [] [first, second] [[1, 2], [], [3]]\nOverride: [custom]\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
            if (std.mem.eql(u8, source, "imported.nov")) continue;
            const tests = b.addRunArtifact(exe);
            tests.step.name = b.fmt("CLI array defaults: test {s}", .{mode});
            tests.setCwd(fixtures.getDirectory());
            tests.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            tests.setEnvironmentVariable("NOV_CC", "");
            tests.setEnvironmentVariable("CC", "");
            tests.addArgs(&.{ "test", source, "--lib-dir" });
            tests.addDirectoryArg(library);
            if (mode.len != 0) tests.addArg(mode);
            tests.expectExitCode(0);
            tests.expectStdOutEqual("PASS constructions_have_independent_arrays\nPASS explicit_overrides_and_value_copies\nPASS generic_empty_defaults_and_type_queries\nPASS globals_returns_and_closures\n4 passed; 0 failed; 4 total\n");
            tests.expectStdErrEqual("");
            test_step.dependOn(&tests.step);
        }
    }
    const format = b.addRunArtifact(exe);
    format.setCwd(fixtures.getDirectory());
    format.addArgs(&.{ "fmt", "--check", "array-defaults.nov" });
    format.expectExitCode(0);
    format.expectStdOutEqual("");
    format.expectStdErrEqual("");
    test_step.dependOn(&format.step);
}

fn addArrayValueHelperCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step, name: []const u8, source: []const u8, output: []const u8, tests: []const u8) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    _ = fixtures.add(name, source);
    const package = b.addWriteFiles();
    const installed_exe = package.addCopyFile(exe.getEmittedBin(), b.fmt("bin/{s}", .{exe.out_filename}));
    _ = package.addCopyDirectory(b.path("lib"), "lib/nov", .{});
    _ = package.add(name, source);
    const check = b.addRunArtifact(exe);
    check.setCwd(fixtures.getDirectory());
    check.addArgs(&.{ "check", "--semantic", name, "--lib-dir" });
    check.addDirectoryArg(library);
    check.expectExitCode(0);
    check.expectStdOutEqual("");
    check.expectStdErrEqual("");
    test_step.dependOn(&check.step);
    const fmt = b.addRunArtifact(exe);
    fmt.setCwd(fixtures.getDirectory());
    fmt.addArgs(&.{ "fmt", "--check", name, "lib/array.nov" });
    fmt.expectExitCode(0);
    fmt.expectStdOutEqual("");
    fmt.expectStdErrEqual("");
    test_step.dependOn(&fmt.step);
    for ([_]bool{ false, true }) |installed| {
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            for ([_][]const u8{ "run", "test" }) |command| {
                const run = if (installed) std.Build.Step.Run.create(b, "CLI array helper: relocated install") else b.addRunArtifact(exe);
                if (installed) run.addFileArg(installed_exe);
                run.setCwd(if (installed) package.getDirectory() else fixtures.getDirectory());
                run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                run.setEnvironmentVariable("NOV_CC", "");
                run.setEnvironmentVariable("CC", "");
                run.addArgs(&.{ command, name });
                if (mode.len != 0) run.addArg(mode);
                if (!installed) {
                    run.addArg("--lib-dir");
                    run.addDirectoryArg(library);
                }
                run.expectExitCode(0);
                run.expectStdOutEqual(if (std.mem.eql(u8, command, "run")) output else tests);
                run.expectStdErrEqual("");
                test_step.dependOn(&run.step);
            }
        }
    }
}

fn addArraySortCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    const aliases_source =
        \\let std = @import("std")
        \\let direct = @import("lib/array.nov")
        \\let alternate = @import("lib/./array.nov")
        \\let api = @import("api.nov")
        \\let sort = api.sort
        \\let less: (a: `T, b: `T) -> bool = a < b
        \\let verify: () = {
        \\    let concrete: ([]int, (int, int) -> bool) -> []int = sort
        \\    let items: []int = [3, 1, 2]
        \\    @assert(concrete(items, less)[0] == 1)
        \\    @assert(sort(items, less)[1] == 2)
        \\    @assert(direct.sort(items, less)[2] == 3)
        \\    @assert(alternate.sort(items, less)[0] == 1)
        \\    @assert(std.array.sort(@as([]i8, [2, 1]), less)[0] == 1)
        \\    @assert(items[0] == 3)
        \\}
        \\@[test]
        \\let imported_aliases: () = verify()
        \\let main: () = verify()
        \\
    ;
    const api_source =
        \\let arrays = @import("lib/array.nov")
        \\@[public]
        \\let sort = arrays.sort
        \\
    ;
    _ = fixtures.add("api.nov", api_source);
    const package = b.addWriteFiles();
    const installed_exe = package.addCopyFile(exe.getEmittedBin(), b.fmt("bin/{s}", .{exe.out_filename}));
    _ = package.addCopyDirectory(b.path("lib"), "lib/nov", .{});
    _ = package.add("api.nov", std.mem.replaceOwned(u8, b.allocator, api_source, "lib/", "lib/nov/") catch @panic("OOM"));
    for ([_]struct { source: []const u8, contents: []const u8, stdout: []const u8, tests: []const u8 }{
        .{
            .source = "array-sort.nov",
            .contents = @embedFile("examples/array-sort.nov"),
            .stdout = "Source: [3, 1, 2, 1]\nAscending: [1, 1, 2, 3]\nDescending: [3, 2, 1, 1]\nStable keys: Bo Ada Cy\nSource unchanged: [3, 1, 2, 1]\n",
            .tests = "PASS empty_and_singleton_no_comparisons\nPASS sorted_reverse_duplicates_and_descending\nPASS stable_struct_keys_and_all_equal\nPASS strings_runes_and_finite_floats\nPASS bool_integer_enum_union_and_range_elements\nPASS nested_arrays_and_records_copy_independently\nPASS function_elements_share_closure_captures\nPASS comparator_capture_and_comparison_order\nPASS arguments_once_left_to_right_and_source_snapshot\n9 passed; 0 failed; 9 total\n",
        },
        .{ .source = "aliases.nov", .contents = aliases_source, .stdout = "", .tests = "PASS imported_aliases\n1 passed; 0 failed; 1 total\n" },
    }) |case| {
        _ = fixtures.add(case.source, case.contents);
        _ = package.add(case.source, std.mem.replaceOwned(u8, b.allocator, case.contents, "lib/", "lib/nov/") catch @panic("OOM"));
        for ([_]bool{ false, true }) |installed| {
            for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
                for ([_][]const u8{ "run", "test" }) |command| {
                    const run = if (installed) std.Build.Step.Run.create(b, "CLI array sort: relocated install") else b.addRunArtifact(exe);
                    run.step.name = b.fmt("CLI array sort: {s} {s} {s} (installed={})", .{ command, case.source, mode, installed });
                    if (installed) run.addFileArg(installed_exe);
                    run.setCwd(if (installed) package.getDirectory() else fixtures.getDirectory());
                    run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                    run.setEnvironmentVariable("NOV_CC", "");
                    run.setEnvironmentVariable("CC", "");
                    run.addArgs(&.{ command, case.source });
                    if (mode.len != 0) run.addArg(mode);
                    if (!installed) {
                        run.addArg("--lib-dir");
                        run.addDirectoryArg(library);
                    }
                    run.expectExitCode(0);
                    run.expectStdOutEqual(if (std.mem.eql(u8, command, "test")) case.tests else case.stdout);
                    run.expectStdErrEqual("");
                    test_step.dependOn(&run.step);
                }
            }
        }
    }
    // Check both entry points into the std <-> array import cycle.
    for ([_][]const u8{ "array-sort.nov", "aliases.nov", "api.nov", "lib/std.nov", "lib/array.nov" }) |source| {
        const check = b.addRunArtifact(exe);
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source, "--lib-dir" });
        check.addDirectoryArg(library);
        check.expectExitCode(0);
        check.expectStdOutEqual("");
        check.expectStdErrEqual("");
        test_step.dependOn(&check.step);
    }
    for ([_]struct { name: []const u8, expression: []const u8, diagnostic: []const u8 }{
        .{ .name = "result", .expression = "std.array.sort(@as([]int,[1]),(a:int,b:int)->int{a})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "first-parameter", .expression = "std.array.sort(@as([]int,[1]),(a:bool,b:int)->bool{a})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "second-parameter", .expression = "std.array.sort(@as([]int,[1]),(a:int,b:bool)->bool{b})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "integer-width", .expression = "std.array.sort(@as([]i8,[1]),(a:i16,b:i16)->bool{a<b})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "float-width", .expression = "std.array.sort(@as([]f32,[1.0]),(a:f64,b:f64)->bool{a<b})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "nominal", .expression = "std.array.sort(@as([]A,[A.[1]]),(a:B,b:B)->bool{false})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "callback-arity", .expression = "std.array.sort(@as([]int,[1]),(a:int)->bool{true})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "missing", .expression = "std.array.sort(@as([]int,[1]))", .diagnostic = "argument" },
        .{ .name = "extra", .expression = "std.array.sort(@as([]int,[1]),less,less)", .diagnostic = "argument" },
        .{ .name = "not-array", .expression = "std.array.sort(1,less)", .diagnostic = "type" },
        .{ .name = "not-function", .expression = "std.array.sort(@as([]int,[1]),true)", .diagnostic = "type" },
    }) |case| {
        const source = b.fmt("invalid-sort-{s}.nov", .{case.name});
        _ = fixtures.add(source, b.fmt("let std=@import(\"std\")\nlet A=struct{{n:int,}}\nlet B=struct{{n:int,}}\nlet less:(a:int,b:int)->bool=a<b\nlet main:()={{let result={s}\n}}\n", .{case.expression}));
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI array sort: reject {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source, "--lib-dir" });
        check.addDirectoryArg(library);
        check.expectExitCode(1);
        check.expectStdOutEqual("");
        check.expectStdErrMatch(".nov:");
        check.expectStdErrMatch("error:");
        check.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&check.step);
    }
    _ = fixtures.add("panic-sort.nov",
        \\let std = @import("std")
        \\let verify: () = {
        \\    let result = std.array.sort(@as([]int, [2, 1]), (a: int, b: int) -> bool {
        \\        @print(a, b)
        \\        @panic("sort comparator")
        \\    })
        \\    @println("unreachable")
        \\}
        \\@[test]
        \\let comparator_panic: () = verify()
        \\let main: () = verify()
    );
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test" }) |command| {
            const run = b.addRunArtifact(exe);
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ command, "panic-sort.nov", "--lib-dir" });
            run.addDirectoryArg(library);
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(1);
            run.expectStdOutEqual(if (std.mem.eql(u8, command, "test")) "1 2FAIL comparator_panic (exit 1)\n0 passed; 1 failed; 1 total\n" else "1 2");
            run.expectStdErrEqual("panic: sort comparator\n");
            test_step.dependOn(&run.step);
        }
    }
}

fn addArraySearchCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    _ = fixtures.add("array-search.nov", @embedFile("examples/array-search.nov"));
    const api_source =
        \\@[public]
        \\let any = @import("std").array.any
        \\@[public]
        \\let all = @import("lib/array.nov").all
        \\@[public]
        \\let find = @import("std").array.find
        \\@[public]
        \\let findIndex = @import("lib/array.nov").findIndex
        \\@[public]
        \\let each = @import("std").array.each
        \\
    ;
    _ = fixtures.add("api.nov", api_source);
    const aliases_source =
        \\let standard = @import("std")
        \\let direct = @import("lib/array.nov")
        \\let option = @import("lib/option.nov")
        \\let alternate = @import("lib/./option.nov")
        \\let api = @import("api.nov")
        \\let find = api.find
        \\let verify: () = {
        \\    let items: []int = [1, 2, 3]
        \\    let predicate = (value: int) -> bool { value > 1 }
        \\    let search: ([]int, (int) -> bool) -> option.Option(int) = find
        \\    let locate: ([]int, (int) -> bool) -> alternate.Option(uint) = api.findIndex
        \\    let found: standard.Option(int) = search(items, predicate)
        \\    let index: standard.Option(uint) = locate(items, predicate)
        \\    @assert(found.unwrapOr(0) == 2 and index.unwrapOr(99) == 1)
        \\    @assert(api.any(items, predicate) and api.all(items, predicate) == false)
        \\    @assert(direct.find(items, predicate).unwrapOr(0) == 2)
        \\    @assert(direct.findIndex(items, predicate).unwrapOr(99) == 1)
        \\    let mut sum: int = 0
        \\    let visit: ([]int, (int) -> void) -> void = api.each
        \\    visit(items, (value: int) -> void { sum += value })
        \\    @assert(sum == 6)
        \\}
        \\@[test]
        \\let aliases_and_canonical_option: () = verify()
        \\let main: () = verify()
        \\
    ;
    _ = fixtures.add("aliases.nov", aliases_source);
    for ([_][]const u8{ "array-search.nov", "lib/std.nov", "lib/array.nov", "api.nov", "aliases.nov" }) |source| {
        for ([_]bool{ false, true }) |semantic| {
            if (!semantic and (std.mem.eql(u8, source, "api.nov") or std.mem.eql(u8, source, "aliases.nov"))) continue;
            const check = b.addRunArtifact(exe);
            check.step.name = b.fmt("CLI array search: {s} {s}", .{ if (semantic) "semantic" else "fmt", source });
            check.setCwd(fixtures.getDirectory());
            check.addArgs(if (semantic) &.{ "check", "--semantic", source } else &.{ "fmt", "--check", source });
            if (semantic) {
                check.addArg("--lib-dir");
                check.addDirectoryArg(library);
            }
            check.expectExitCode(0);
            check.expectStdOutEqual("");
            check.expectStdErrEqual("");
            test_step.dependOn(&check.step);
        }
    }
    const example_output = b.fmt("Any: true\nAll: false\nFirst: Option(i{d}).some[3]\nIndex: Option(u{d}).some[0]\nEach: 3 1 4 2\n", .{ @bitSizeOf(usize), @bitSizeOf(usize) });
    const example_tests = "PASS any_all_shortcircuit_order\nPASS first_match_index_and_none\nPASS empty_identities_and_each_order\nPASS find_payload_types_and_recursive_copies\nPASS function_elements_and_other_index_types\nPASS eager_arguments_and_mutation_snapshots\nPASS empty_arguments_are_still_eager\n7 passed; 0 failed; 7 total\n";
    const package = b.addWriteFiles();
    const installed_exe = package.addCopyFile(exe.getEmittedBin(), b.fmt("bin/{s}", .{exe.out_filename}));
    _ = package.addCopyDirectory(b.path("lib"), "lib/nov", .{});
    // Direct imports must resolve to the same library as the relocated executable.
    _ = package.add("array-search.nov", @embedFile("examples/array-search.nov"));
    _ = package.add("api.nov", std.mem.replaceOwned(u8, b.allocator, api_source, "lib/", "lib/nov/") catch @panic("OOM"));
    _ = package.add("aliases.nov", std.mem.replaceOwned(u8, b.allocator, aliases_source, "lib/", "lib/nov/") catch @panic("OOM"));
    for ([_]bool{ false, true }) |installed| {
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            for ([_][]const u8{ "run", "test" }) |command| {
                for ([_][]const u8{ "array-search.nov", "aliases.nov" }) |source| {
                    const run = if (installed) std.Build.Step.Run.create(b, "CLI array search: relocated install") else b.addRunArtifact(exe);
                    run.step.name = b.fmt("CLI array search: {s} {s} {s} (installed={})", .{ command, source, mode, installed });
                    if (installed) run.addFileArg(installed_exe);
                    run.setCwd(if (installed) package.getDirectory() else fixtures.getDirectory());
                    run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                    run.setEnvironmentVariable("NOV_CC", "");
                    run.setEnvironmentVariable("CC", "");
                    run.addArgs(&.{ command, source });
                    if (mode.len != 0) run.addArg(mode);
                    if (!installed) {
                        run.addArg("--lib-dir");
                        run.addDirectoryArg(library);
                    }
                    const is_test = std.mem.eql(u8, command, "test");
                    const example = std.mem.eql(u8, source, "array-search.nov");
                    run.expectExitCode(0);
                    run.expectStdOutEqual(if (example) (if (is_test) example_tests else example_output) else if (is_test) "PASS aliases_and_canonical_option\n1 passed; 0 failed; 1 total\n" else "");
                    run.expectStdErrEqual("");
                    test_step.dependOn(&run.step);
                }
            }
        }
    }
    for ([_]struct { name: []const u8, expression: []const u8, diagnostic: []const u8 }{
        .{ .name = "any-parameter", .expression = "std.array.any(@as([]int,[1]),(x:bool)->bool{x})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "all-result", .expression = "std.array.all(@as([]int,[1]),(x:int)->int{x})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "find-parameter", .expression = "std.array.find(@as([]int,[1]),(x:bool)->bool{x})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "index-result", .expression = "std.array.findIndex(@as([]int,[1]),(x:int)->int{x})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "each-nonvoid", .expression = "std.array.each(@as([]int,[1]),(x:int)->int{x})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "find-array-parameter", .expression = "std.array.find(@as([][]int,[[1]]),(x:int)->bool{true})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "find-union-reference-payload", .expression = "std.array.find(@as([]Payload,[.none[]]),yes)", .diagnostic = "not supported" },
        .{ .name = "find-function-parameter", .expression = "std.array.find([()->int{1}],(x:int)->bool{true})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "find-range-parameter", .expression = "std.array.find([1..3],(x:int)->bool{true})", .diagnostic = "expression does not match the expected type" },
        .{ .name = "any-empty", .expression = "std.array.any([],yes)", .diagnostic = "provide typed runtime arguments" },
        .{ .name = "all-empty", .expression = "std.array.all([],yes)", .diagnostic = "provide typed runtime arguments" },
        .{ .name = "find-empty", .expression = "std.array.find([],yes)", .diagnostic = "provide typed runtime arguments" },
        .{ .name = "index-empty", .expression = "std.array.findIndex([],yes)", .diagnostic = "provide typed runtime arguments" },
        .{ .name = "each-empty", .expression = "std.array.each([],(x:`T)->void{})", .diagnostic = "provide typed runtime arguments" },
    }) |case| {
        const source = b.fmt("invalid-array-search-{s}.nov", .{case.name});
        _ = fixtures.add(source, b.fmt("let std=@import(\"std\")\nlet Payload=union{{{s}none,}}\nlet yes:(x:`T)->bool=true\nlet main:()={{let result={s}\n}}\n", .{ if (std.mem.eql(u8, case.name, "find-union-reference-payload")) "callback:&()->int," else "", case.expression }));
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI array search: reject {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source, "--lib-dir" });
        check.addDirectoryArg(library);
        check.expectExitCode(1);
        check.expectStdOutEqual("");
        check.expectStdErrMatch(".nov:");
        check.expectStdErrMatch("error:");
        check.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&check.step);
    }
    for ([_][]const u8{ "any", "all", "find", "findIndex", "each" }) |helper| {
        const source = b.fmt("panic-array-search-{s}.nov", .{helper});
        const result_type = if (std.mem.eql(u8, helper, "each")) "void" else "bool";
        _ = fixtures.add(source, b.fmt("let std=@import(\"std\")\nlet verify:()={{{s}std.array.{s}(@as([]int,[1,2]),(value:int)->{s}{{@print(value)\n@panic(\"{s} callback\")}})\n@println(\"unreachable\")}}\n@[test]\nlet callback_panic:()=verify()\nlet main:()=verify()\n", .{ if (std.mem.eql(u8, helper, "each")) "" else "let result=", helper, result_type, helper }));
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            for ([_][]const u8{ "run", "test" }) |command| {
                const run = b.addRunArtifact(exe);
                run.step.name = b.fmt("CLI array search: {s} {s} panic ({s})", .{ command, helper, mode });
                run.setCwd(fixtures.getDirectory());
                run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                run.setEnvironmentVariable("NOV_CC", "");
                run.setEnvironmentVariable("CC", "");
                run.addArgs(&.{ command, source, "--lib-dir" });
                run.addDirectoryArg(library);
                if (mode.len != 0) run.addArg(mode);
                run.expectExitCode(1);
                run.expectStdOutEqual(if (std.mem.eql(u8, command, "test")) "1FAIL callback_panic (exit 1)\n0 passed; 1 failed; 1 total\n" else "1");
                run.expectStdErrEqual(b.fmt("panic: {s} callback\n", .{helper}));
                test_step.dependOn(&run.step);
            }
        }
    }
}

fn addContainerMapCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    _ = fixtures.add("container-map.nov", @embedFile("examples/container-map.nov"));
    const aliases_source =
        \\let standard = @import("std")
        \\let directOption = @import("lib/option.nov")
        \\let directResult = @import("lib/./result.nov")
        \\let api = @import("api.nov")
        \\let Option = directOption.Option
        \\let Result = standard.Result
        \\let verify: () = {
        \\    let some: api.IntOption = .some[4]
        \\    let mapped: standard.Option(str) = Option(int).map(some, (x) { "value:{x}" })
        \\    @assert(mapped.unwrapOr("") == "value:4")
        \\    @assert(some.map((x) { x + 1 }).unwrapOr(0) == 5)
        \\    let good: directResult.Result(int, str) = .ok[4]
        \\    let bad: api.IntResult = .err["bad"]
        \\    let text: standard.Result(str, str) = Result(int, str).map(good, (x) { "ok:{x}" })
        \\    @assert(text.unwrapOr("") == "ok:4")
        \\    let error: directResult.Result(int, int) = bad.mapErr((message) { 7 })
        \\    @assert(match error {
        \\        .ok => false
        \\        .err => |value| value == 7
        \\    })
        \\    let untouched: standard.Result(int, int) = Result(int, str).mapErr(good, (message) { 9 })
        \\    @assert(untouched.unwrapOr(0) == 4)
        \\}
        \\@[test]
        \\let imported_aliases_are_canonical: () = verify()
        \\let main: () = verify()
        \\
    ;
    const api_source =
        \\let option = @import("lib/./option.nov")
        \\let standard = @import("std")
        \\let Option = option.Option
        \\let Result = standard.Result
        \\@[public]
        \\let IntOption = Option(int)
        \\@[public]
        \\let IntResult = Result(int, str)
        \\
    ;
    _ = fixtures.add("aliases.nov", aliases_source);
    _ = fixtures.add("api.nov", api_source);
    for ([_][]const u8{ "container-map.nov", "aliases.nov", "api.nov", "lib/std.nov" }) |source| {
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI container map: semantic {s}", .{source});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source, "--lib-dir" });
        check.addDirectoryArg(library);
        check.expectExitCode(0);
        check.expectStdOutEqual("");
        check.expectStdErrEqual("");
        test_step.dependOn(&check.step);
    }
    const format = b.addRunArtifact(exe);
    format.step.name = "CLI container map: fmt example";
    format.setCwd(fixtures.getDirectory());
    format.addArgs(&.{ "fmt", "--check", "container-map.nov" });
    format.expectExitCode(0);
    format.expectStdOutEqual("");
    format.expectStdErrEqual("");
    test_step.dependOn(&format.step);

    const package = b.addWriteFiles();
    const installed_exe = package.addCopyFile(exe.getEmittedBin(), b.fmt("bin/{s}", .{exe.out_filename}));
    _ = package.addCopyDirectory(b.path("lib"), "lib/nov", .{});
    _ = package.add("container-map.nov", @embedFile("examples/container-map.nov"));
    _ = package.add("api.nov", std.mem.replaceOwned(u8, b.allocator, api_source, "lib/", "lib/nov/") catch @panic("OOM"));
    _ = package.add("aliases.nov", std.mem.replaceOwned(u8, b.allocator, aliases_source, "lib/", "lib/nov/") catch @panic("OOM"));
    const example_tests = "PASS option_variants_and_inferred_results\nPASS result_variants_and_shared_captures\nPASS eager_receiver_and_callback_construction\nPASS record_payloads_and_recursive_array_copies\nPASS inactive_callbacks_and_chained_maps\nPASS chained_container_operations\n6 passed; 0 failed; 6 total\n";
    for ([_]bool{ false, true }) |installed| {
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            for ([_][]const u8{ "run", "test" }) |command| {
                for ([_][]const u8{ "container-map.nov", "aliases.nov" }) |source| {
                    const run = if (installed) std.Build.Step.Run.create(b, "CLI container map: relocated install") else b.addRunArtifact(exe);
                    run.step.name = b.fmt("CLI container map: {s} {s} {s} (installed={})", .{ command, source, mode, installed });
                    if (installed) run.addFileArg(installed_exe);
                    run.setCwd(if (installed) package.getDirectory() else fixtures.getDirectory());
                    run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                    run.setEnvironmentVariable("NOV_CC", "");
                    run.setEnvironmentVariable("CC", "");
                    run.addArgs(&.{ command, source });
                    if (mode.len != 0) run.addArg(mode);
                    if (!installed) {
                        run.addArg("--lib-dir");
                        run.addDirectoryArg(library);
                    }
                    const example = std.mem.eql(u8, source, "container-map.nov");
                    const is_test = std.mem.eql(u8, command, "test");
                    run.expectExitCode(0);
                    run.expectStdOutEqual(if (example) (if (is_test) example_tests else "Option: 42\nNone: true\nResult: value:21\nerror:missing\n") else if (is_test) "PASS imported_aliases_are_canonical\n1 passed; 0 failed; 1 total\n" else "");
                    run.expectStdErrEqual("");
                    test_step.dependOn(&run.step);
                }
            }
        }
    }
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "option-parameter", .source = "let x:std.Option(int)=.some[1]\nlet y=x.map((v:bool)->int{1})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "result-parameter", .source = "let x:std.Result(int,str)=.ok[1]\nlet y=x.map((v:bool)->int{1})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "error-parameter", .source = "let x:std.Result(int,str)=.err[\"bad\"]\nlet y=x.mapErr((v:bool)->int{1})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "option-output-conflict", .source = "let x:std.Option(int)=.some[1]\nlet y:std.Option(bool)=x.map((v){v+1})", .diagnostic = "error:" },
        .{ .name = "result-output-conflict", .source = "let x:std.Result(int,str)=.ok[1]\nlet y:std.Result(bool,str)=x.map((v){v+1})", .diagnostic = "error:" },
        .{ .name = "error-output-conflict", .source = "let x:std.Result(int,str)=.err[\"bad\"]\nlet y:std.Result(int,bool)=x.mapErr((v){7})", .diagnostic = "error:" },
        .{ .name = "option-void-input", .source = "let x:std.Option(void)=.some[]\nlet y=x.map(()->int{1})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "result-void-input", .source = "let x:std.Result(void,str)=.ok[]\nlet y=x.map(()->int{1})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "error-void-input", .source = "let x:std.Result(int,void)=.err[]\nlet y=x.mapErr(()->int{1})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "option-void-parameter", .source = "let x:std.Option(void)=.some[]\nlet y=x.map((v:void)->int{1})", .diagnostic = "void is not a parameter type" },
        .{ .name = "result-void-parameter", .source = "let x:std.Result(void,str)=.ok[]\nlet y=x.map((v:void)->int{1})", .diagnostic = "void is not a parameter type" },
        .{ .name = "error-void-parameter", .source = "let x:std.Result(int,void)=.err[]\nlet y=x.mapErr((v:void)->int{1})", .diagnostic = "void is not a parameter type" },

        .{ .name = "error-void-success-propagation", .source = "let x:std.Result(void,str)=.ok[]\nlet y=x.mapErr((v){7})", .diagnostic = "[] for void" },
        .{ .name = "option-chain-value", .source = "let x:std.Option(int)=.some[1]\nlet y=x.andThen((v){v+1})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "result-chain-error", .source = "let x:std.Result(int,str)=.ok[1]\nlet y=x.andThen((v)->std.Result(int,bool){.ok[v]})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "result-chain-option", .source = "let x:std.Result(int,str)=.ok[1]\nlet y=x.andThen((v)->std.Option(int){.some[v]})", .diagnostic = "constraints require exactly identical types" },
        .{ .name = "option-void-output", .source = "let x:std.Option(int)=.some[1]\nlet y=x.map((v)->void{})", .diagnostic = "[] for void" },
        .{ .name = "result-void-output", .source = "let x:std.Result(int,str)=.ok[1]\nlet y=x.map((v)->void{})", .diagnostic = "[] for void" },
        .{ .name = "error-void-output", .source = "let x:std.Result(int,str)=.err[\"bad\"]\nlet y=x.mapErr((v)->void{})", .diagnostic = "[] for void" },
        .{ .name = "option-array-output-conflict", .source = "let x:std.Option(int)=.some[1]\nlet y:std.Option([]bool)=x.map((v){[v]})", .diagnostic = "error:" },
        .{ .name = "result-reference-output", .source = "let x:std.Result(int,str)=.ok[1]\nlet y=x.map((v)->&int{&v})", .diagnostic = "reference" },
        .{ .name = "error-reference-output", .source = "let x:std.Result(int,str)=.err[\"bad\"]\nlet y=x.mapErr((v)->&str{&v})", .diagnostic = "reference" },
    }) |case| {
        const source = b.fmt("invalid-container-map-{s}.nov", .{case.name});
        _ = fixtures.add(source, b.fmt("let std=@import(\"std\")\nlet main:()={{\n{s}\n}}\n", .{case.source}));
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI container map: reject {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source, "--lib-dir" });
        check.addDirectoryArg(library);
        check.expectExitCode(1);
        check.expectStdOutEqual("");
        check.expectStdErrMatch(".nov:");
        check.expectStdErrMatch("error:");
        check.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&check.step);
    }
    for ([_]struct { name: []const u8, receiver: []const u8, method: []const u8, parameter: []const u8, result: []const u8 = "int" }{
        .{ .name = "option", .receiver = "std.Option(int).some[1]", .method = "map", .parameter = "int" },
        .{ .name = "result", .receiver = "std.Result(int,str).ok[1]", .method = "map", .parameter = "int" },
        .{ .name = "error", .receiver = "std.Result(int,int).err[1]", .method = "mapErr", .parameter = "int" },
        .{ .name = "option-chain", .receiver = "std.Option(int).some[1]", .method = "andThen", .parameter = "int", .result = "std.Option(int)" },
        .{ .name = "result-chain", .receiver = "std.Result(int,str).ok[1]", .method = "andThen", .parameter = "int", .result = "std.Result(int,str)" },
    }) |case| {
        const source = b.fmt("panic-container-map-{s}.nov", .{case.name});
        _ = fixtures.add(source, b.fmt("let std=@import(\"std\")\nlet verify:()={{let result={s}.{s}((value:{s})->{s}{{@print(value)\n@panic(\"{s} callback\")}})\n@println(\"unreachable\")}}\n@[test]\nlet callback_panic:()=verify()\nlet main:()=verify()\n", .{ case.receiver, case.method, case.parameter, case.result, case.name }));
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            for ([_][]const u8{ "run", "test" }) |command| {
                const run = b.addRunArtifact(exe);
                run.step.name = b.fmt("CLI container map: {s} {s} panic ({s})", .{ command, case.name, mode });
                run.setCwd(fixtures.getDirectory());
                run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                run.setEnvironmentVariable("NOV_CC", "");
                run.setEnvironmentVariable("CC", "");
                run.addArgs(&.{ command, source, "--lib-dir" });
                run.addDirectoryArg(library);
                if (mode.len != 0) run.addArg(mode);
                run.expectExitCode(1);
                run.expectStdOutEqual(if (std.mem.eql(u8, command, "test")) "1FAIL callback_panic (exit 1)\n0 passed; 1 failed; 1 total\n" else "1");
                run.expectStdErrEqual(b.fmt("panic: {s} callback\n", .{case.name}));
                test_step.dependOn(&run.step);
            }
        }
    }
}

fn addArrayFieldCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("array-fields.nov", @embedFile("examples/array-fields.nov"));
    for ([_][]const u8{ "fmt", "semantic" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI array fields: {s}", .{mode});
        run.setCwd(fixtures.getDirectory());
        run.addArgs(if (std.mem.eql(u8, mode, "fmt")) &.{ "fmt", "--check", "array-fields.nov" } else &.{ "check", "--semantic", "array-fields.nov" });
        run.expectExitCode(0);
        run.expectStdOutEqual("");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test" }) |command| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI array fields: {s} ({s})", .{ command, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ command, "array-fields.nov" });
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (std.mem.eql(u8, command, "test"))
                "PASS field_mutations\nPASS recursive_copies\nPASS payload_and_control_flow\nPASS closure_copies\nPASS mutation_order_and_record_loans\n5 passed; 0 failed; 5 total\n"
            else
                "Saved: 2 2\nUpdated: 12 3\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    const unsupported_type = "this type is not supported in semantic analysis yet";
    const unsupported_borrow = "local references require &local or &localArray[index]";
    const unsupported_assignment = "assignment target must be a mutable local, its struct field path, or its scalar-array element";
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "nonstorable-field", .source = "let A=struct{a:[][]void,}\nlet main:()={}", .diagnostic = unsupported_type },
        .{ .name = "nonstorable-local", .source = "let main:()={let a:[][]type=[]\n}", .diagnostic = unsupported_type },
        .{ .name = "nonstorable-generic", .source = "let A:(T:type)->type=struct{a:[]T,}\nlet main:(a:A([]void))={}", .diagnostic = unsupported_type },
        .{ .name = "recursive", .source = "let A=struct{a:[]A,}\nlet main:()={}", .diagnostic = "cyclic by-value struct layout" },
        .{ .name = "mutually-recursive", .source = "let A=struct{b:[]B,}\nlet B=struct{a:A,}\nlet main:()={}", .diagnostic = "cyclic by-value struct layout" },
        .{ .name = "recursive-generic", .source = "let A:(T:type)->type=struct{a:[]A(T),}\nlet main:(a:A(int))={}", .diagnostic = "cyclic generic container instantiation" },
        .{ .name = "borrow-field", .source = "let A=struct{a:[]int,}\nlet main:()={let mut a=A.[a=[1]]\nlet r=&a.a\n}", .diagnostic = unsupported_borrow },
        .{ .name = "borrow-field-element", .source = "let A=struct{a:[]int,}\nlet main:()={let mut a=A.[a=[1]]\nlet r=&a.a[0]\n}", .diagnostic = unsupported_borrow },
        .{ .name = "borrowed-field-index", .source = "let A=struct{a:[]int,}\nlet main:()={let mut a=A.[a=[1]]\nlet r=&a\nr.*.a[0] = 2}", .diagnostic = unsupported_assignment },
    }) |case| {
        const source = b.fmt("invalid-array-field-{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI array fields: reject {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source });
        check.expectExitCode(1);
        check.expectStdOutEqual("");
        check.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&check.step);
    }
}

fn addPositionalFieldCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("positional-fields.nov", @embedFile("examples/positional-fields.nov"));
    _ = fixtures.add("shapes.nov",
        \\@[public]
        \\let Pair = @TypeOf(.[1, "a"])
        \\@[public]
        \\let make: () -> Pair = .[7, "imported"]
        \\
    );
    _ = fixtures.add("aliases.nov",
        \\let shapes = @import("./shapes.nov")
        \\@[public]
        \\let Pair = shapes.Pair
        \\
    );
    _ = fixtures.add("imported.nov",
        \\let shapes = @import("./shapes.nov")
        \\let aliases = @import("./aliases.nov")
        \\let exercise: () = {
        \\    let mut pair: aliases.Pair = shapes.make()
        \\    pair.0 += 1
        \\    @assert(pair.0 == 8 and pair.1 == "imported")
        \\    let local: @TypeOf(.[ 1, "a" ]) = pair
        \\    @assert(local.0 == 8 and local.1 == "imported")
        \\}
        \\let main: () = {
        \\    exercise()
        \\}
        \\@[test]
        \\let imported_projection: () = {
        \\    exercise()
        \\}
        \\
    );
    for ([_][]const u8{ "positional-fields.nov", "imported.nov" }) |source| {
        for ([_][]const u8{ "fmt", "semantic" }) |mode| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI positional fields: {s} {s}", .{ source, mode });
            run.setCwd(fixtures.getDirectory());
            run.addArgs(&.{ if (std.mem.eql(u8, mode, "fmt")) "fmt" else "check", if (std.mem.eql(u8, mode, "fmt")) "--check" else "--semantic", source });
            if (std.mem.eql(u8, mode, "semantic")) {
                run.addArg("--lib-dir");
                run.addDirectoryArg(b.path("lib"));
            }
            run.expectExitCode(0);
            run.expectStdOutEqual("");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            for ([_][]const u8{ "run", "test" }) |command| {
                // Exercise executable-relative installed stdlib discovery as well as imports.
                const run = b.addRunFile(.{ .relative = .{ .base = .install_bin, .sub_path = exe.out_filename } });
                run.step.dependOn(b.getInstallStep());
                run.step.name = b.fmt("CLI positional fields: {s} {s} ({s})", .{ source, command, mode });
                run.setCwd(fixtures.getDirectory());
                run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                run.setEnvironmentVariable("NOV_CC", "");
                run.setEnvironmentVariable("CC", "");
                run.addArgs(&.{ command, source });
                if (mode.len != 0) run.addArg(mode);
                run.expectExitCode(0);
                run.expectStdOutEqual(if (std.mem.eql(u8, source, "imported.nov"))
                    (if (std.mem.eql(u8, command, "test")) "PASS imported_projection\n1 passed; 0 failed; 1 total\n" else "")
                else if (std.mem.eql(u8, command, "test"))
                    "PASS projection_and_generic_map\nPASS independent_nested_snapshots\nPASS callable_snapshot_and_shared_environment\nPASS reusable_range_field\nPASS numeric_mutation_and_unevaluated_type\n5 passed; 0 failed; 5 total\n"
                else
                    "Positional fields: ok\n");
                run.expectStdErrEqual("");
                test_step.dependOn(&run.step);
            }
        }
    }
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 = "error:" }{
        .{ .name = "out-of-range", .source = "let main:()={let r=.[1,2]\n@println(r.2)}" },
        .{ .name = "huge", .source = "let main:()={let r=.[1,2]\n@println(r.999999999999999999999999999999999999999999999999999999999999999999)}" },
        .{ .name = "leading-zero", .source = "let main:()={let r=.[1,2]\n@println(r.01)}" },
        .{ .name = "hexadecimal", .source = "let main:()={let r=.[1,2]\n@println(r.0x1)}" },
        .{ .name = "separator", .source = "let main:()={let r=.[1,2]\n@println(r.0_1)}" },
        .{ .name = "readonly", .source = "let main:()={let r=.[1,2]\nr.0=3}", .diagnostic = "assignment requires a mutable local" },
        .{ .name = "readonly-reference", .source = "let main:()={let r=.[1,2]\nlet p=&r\np.*.0=3}" },
        .{ .name = "type-conflict", .source = "let main:()={let mut r=.[1,2]\nr.0=true}", .diagnostic = "literal does not match the annotated type" },
        .{ .name = "named-anonymous", .source = "let main:()={let r=.[a=1,b=2]\n@println(r.0)}" },
        .{ .name = "nominal", .source = "let R=struct{a:int,}\nlet main:()={let r=R.[a=1]\n@println(r.0)}" },
        .{ .name = "enum", .source = "let E=enum{a,b,}\nlet main:()={let r:E=.a\n@println(r.0)}" },
        .{ .name = "array", .source = "let main:()={let r=[1,2]\n@println(r.0)}" },
        .{ .name = "str", .source = "let main:()={let r=\"ab\"\n@println(r.0)}" },
        .{ .name = "tuple-bracket", .source = "let main:()={let r=.[1,2]\n@println(r[0])}" },
        .{ .name = "dynamic-index", .source = "let main:()={let r=.[1,2]\nlet i:int=0\n@println(r[i])}" },
        .{ .name = "field-borrow", .source = "let main:()={let mut r=.[1,2]\nlet p=&r.0\n}", .diagnostic = "local references require &local or &localArray[index]" },
        .{ .name = "element-borrow", .source = "let main:()={let mut r=.[[1],2]\nlet p=&r.0[0]\n}", .diagnostic = "local references require &local or &localArray[index]" },
        .{ .name = "readonly-reference-write", .source = "let main:()={let r=.[1,2]\nlet p=&r\np.0=3}", .diagnostic = "assignment requires a mutable local" },
    }) |case| {
        const source = b.fmt("invalid-positional-{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI positional fields: reject {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source });
        check.expectExitCode(1);
        check.expectStdOutEqual("");
        check.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&check.step);
    }
}

fn addAnonymousStructCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    for ([_][]const u8{ "fmt", "semantic" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI anonymous structs: {s}", .{mode});
        run.addArgs(if (std.mem.eql(u8, mode, "fmt")) &.{ "fmt", "--check" } else &.{ "check", "--semantic" });
        run.addFileArg(b.path("examples/anonymous-structs.nov"));
        if (std.mem.eql(u8, mode, "semantic")) {
            run.addArg("--lib-dir");
            run.addDirectoryArg(b.path("lib"));
        }
        run.expectExitCode(0);
        run.expectStdOutEqual("");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("shapes.nov",
        \\@[public]
        \\let Named = @TypeOf(.[a = 1, label = "a"])
        \\@[public]
        \\let Pair = @TypeOf(.[1, "a"])
        \\@[public]
        \\let make: () -> Named = .[a = 7, label = "module"]
        \\@[public]
        \\let Box: (T: type) -> type = struct { value: T, }
        \\
    );
    _ = fixtures.add("aliases.nov",
        \\let shapes = @import("./shapes.nov")
        \\@[public]
        \\let Named = shapes.Named
        \\@[public]
        \\let Pair = shapes.Pair
        \\
    );
    _ = fixtures.add("main.nov",
        \\let shapes = @import("./shapes.nov")
        \\let aliases = @import("./aliases.nov")
        \\let main: () = {
        \\    let local: aliases.Named = .[a = 1, label = "local"]
        \\    let imported: @TypeOf(local) = shapes.make()
        \\    let pair: aliases.Pair = .[2, "b"]
        \\    let boxed = shapes.Box(aliases.Named).[value = imported]
        \\    @println(local, boxed.value, pair)
        \\    @println(aliases.Named, aliases.Pair)
        \\}
        \\
    );
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test", "modules" }) |command| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI anonymous structs: {s} ({s})", .{ command, mode });
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            if (std.mem.eql(u8, command, "modules")) {
                run.setCwd(fixtures.getDirectory());
                run.addArgs(&.{ "run", "main.nov" });
                run.expectStdOutEqual(".[ a: 1, label: local ] .[ a: 7, label: module ] .[2, b]\nstruct{ a: i64, label: str } struct{ i64, str }\n");
            } else {
                run.addArg(command);
                run.addFileArg(b.path("examples/anonymous-structs.nov"));
                run.expectStdOutEqual(if (std.mem.eql(u8, command, "test"))
                    "PASS named_contextual_defaults\nPASS structural_shapes_across_scopes\nPASS inferred_positional_values\nPASS snapshots_and_closure_fields\nPASS generic_mapping\n5 passed; 0 failed; 5 total\n"
                else
                    ".[ a: 1 ] .[1, a] .[1, 2]\nstruct{ a: i64, label: str } struct{ i64, str }\nRectangle[ width: 3, height: 4 ] Empty[]\n");
            }
            run.addArg("--lib-dir");
            run.addDirectoryArg(b.path("lib"));
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "old-named", .source = "let S=struct{a:int,}\nlet main:()={let x=S[.a=1]\n}", .diagnostic = "error:" },
        .{ .name = "old-empty", .source = "let S=struct{}\nlet main:()={let x=S[]\n}", .diagnostic = "error:" },
        .{ .name = "old-contextual", .source = "let S=struct{a:int,}\nlet main:()={let x:S=[.a=1]\n}", .diagnostic = "error:" },
        .{ .name = "dotted-assignment", .source = "let S=struct{a:int,}\nlet main:()={let x=S.[.a=1]\n}", .diagnostic = "error:" },
        .{ .name = "dotted-shorthand", .source = "let S=struct{a:int,}\nlet main:()={let a:int=1\nlet x=S.[.a]\n}", .diagnostic = "error:" },
        .{ .name = "mixed-named-first", .source = "let main:()={let x=.[a=1,2]\n}", .diagnostic = "error:" },
        .{ .name = "mixed-positional-first", .source = "let main:()={let x=.[1,a=2]\n}", .diagnostic = "error:" },
        .{ .name = "ordered-names", .source = "let main:()={let value=.[b=true,a=1]\nlet x:@TypeOf(.[a=1,b=true])=value\n}", .diagnostic = "expected type" },
        .{ .name = "exact-field-types", .source = "let main:()={let x:@TypeOf(.[a=@as(u8,1)])=.[a=@as(u16,1)]\n}", .diagnostic = "expected type" },
        .{ .name = "nominal-distinct", .source = "let S=struct{a:int,}\nlet main:()={let x=.[a=1]\nlet y:S=x\n}", .diagnostic = "expected type" },
        .{ .name = "homogeneous-not-array", .source = "let main:()={let x=.[1,2]\nlet y:[]int=x\n}", .diagnostic = "expected type" },
        .{ .name = "computed-array-default", .source = "let S=struct{a:[]int=[1+2],}\nlet main:()={let x:S=.[]\n}", .diagnostic = "error:" },
    }) |case| {
        const source = b.fmt("invalid-anonymous-struct-{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI anonymous structs: reject {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source });
        check.expectExitCode(1);
        check.expectStdOutEqual("");
        check.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&check.step);
    }
}

fn addConstructorShorthandCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    for ([_][]const u8{ "fmt", "semantic" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI constructor shorthand: {s}", .{mode});
        run.addArgs(if (std.mem.eql(u8, mode, "fmt")) &.{ "fmt", "--check" } else &.{ "check", "--semantic" });
        run.addFileArg(b.path("examples/constructor-shorthand.nov"));
        run.expectExitCode(0);
        run.expectStdOutEqual("");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test" }) |command| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI constructor shorthand: {s} ({s})", .{ command, mode });
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArg(command);
            run.addFileArg(b.path("examples/constructor-shorthand.nov"));
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (std.mem.eql(u8, command, "test"))
                "PASS scoped_values\nPASS mixed_order_and_defaults\nPASS source_snapshots\nPASS closure_captures\nPASS aliases_and_payloads\n5 passed; 0 failed; 5 total\n"
            else
                "rectangle 6 4\nresized 9 4\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("constructor-order.nov",
        \\let mark: (label: str, value: int) -> int = { @print(label)
        \\    value }
        \\let R = struct { first: int = 30, width: int, last: int = 40, height: int, }
        \\let main: () = {
        \\    let mut width: int = 1
        \\    let r = R.[height = { width = mark("A", 2)
        \\        width }, width, last = mark("B", 4)]
        \\    @println(r.width, r.height, r.first, r.last)
        \\    let height: int = 5
        \\    let s = R.[width, height]
        \\    @println(s.first, s.last)
        \\}
        \\
    );
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI constructor shorthand: operand and default order ({s})", .{mode});
        run.setCwd(fixtures.getDirectory());
        run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        run.setEnvironmentVariable("NOV_CC", "");
        run.setEnvironmentVariable("CC", "");
        run.addArgs(&.{ "run", "constructor-order.nov" });
        if (mode.len != 0) run.addArg(mode);
        run.expectExitCode(0);
        run.expectStdOutEqual("AB2 2 30 4\n30 40\n");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "unknown-value", .source = "let R=struct{width:int=1,}\nlet main:()={let r=R.[width]\n}", .diagnostic = "unknown name 'width'" },
        .{ .name = "out-of-scope", .source = "let R=struct{width:int,}\nlet main:()={if true {let width:int=1\n}\nlet r=R.[width]\n}", .diagnostic = "unknown name 'width'" },
        .{ .name = "duplicate-explicit", .source = "let R=struct{width:int,}\nlet main:()={let width:int=1\nlet r=R.[width,width=2]\n}", .diagnostic = "duplicate container field" },
        .{ .name = "duplicate-shorthand", .source = "let R=struct{width:int,}\nlet main:()={let width:int=1\nlet r=R.[width=2,width]\n}", .diagnostic = "duplicate container field" },
        .{ .name = "unknown-field", .source = "let R=struct{height:int=1,}\nlet main:()={let width:int=1\nlet r=R.[width]\n}", .diagnostic = "unknown container field" },
        .{ .name = "wrong-type", .source = "let R=struct{width:int,}\nlet main:()={let width=true\nlet r=R.[width]\n}", .diagnostic = "expression does not match the expected type" },
        .{ .name = "integer-width", .source = "let R=struct{width:i8,}\nlet main:()={let width:i16=1\nlet r=R.[width]\n}", .diagnostic = "expression does not match the expected type" },
        .{ .name = "field-path", .source = "let R=struct{width:int,}\nlet main:()={let r=R.[.width.value]\n}", .diagnostic = "expression does not match the expected type" },
        .{ .name = "contextual-struct", .source = "let R=struct{width:int,}\nlet main:()={let width:int=1\nlet r:R=[.width]\n}", .diagnostic = "expression does not match the expected type" },
    }) |case| {
        const source = b.fmt("invalid-constructor-shorthand-{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI constructor shorthand: reject {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArg("check");
        check.addArg("--semantic");
        check.addArg(source);
        check.expectExitCode(1);
        check.expectStdOutEqual("");
        check.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&check.step);
    }
}

fn addFieldMutationCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step, fixtures: *std.Build.Step.WriteFile) void {
    _ = fixtures.add("field-library.nov", "@[public]\nlet K = enum { no, yes, }\n@[public]\nlet R = struct { x: int = 1, s: str = \"old\", b: bool = false, k: K = .no, len: uint = 0, }\n");
    _ = fixtures.add("field-mutation.nov",
        \\let library = @import("field-library.nov")
        \\let Alias = library.R
        \\let main: () = {
        \\    let mut r = Alias.[]
        \\    let copy = r
        \\    r.x += {
        \\        r = Alias.[ x = 99, len = 8 ]
        \\        2
        \\    }
        \\    r.s += "!"
        \\    r.b = true
        \\    r.k = .yes
        \\    @println(r.x, r.s, r.b, r.k == library.K.yes, r.len, copy.x, copy.s)
        \\}
        \\
    );
    _ = fixtures.add("struct-mutation-example.nov", @embedFile("examples/struct-mutation.nov"));
    const programs = [_]struct { source: []const u8, output: []const u8 }{
        .{ .source = "field-mutation.nov", .output = "3 old! true true 8 1 old\n" },
        .{ .source = "struct-mutation-example.nov", .output = "Counter updated 6 true true\nCounter 0 false true\n" },
    };
    for (programs) |program| for ([_][]const u8{ "semantic", "fmt", "default", "--debug", "--release" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI field mutation: {s} {s}", .{ program.source, mode });
        run.setCwd(fixtures.getDirectory());
        const semantic = std.mem.eql(u8, mode, "semantic");
        const fmt = std.mem.eql(u8, mode, "fmt");
        if (semantic) {
            run.addArgs(&.{ "check", "--semantic", program.source });
        } else if (fmt) {
            run.addArgs(&.{ "fmt", "--check", program.source });
        } else {
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ "run", program.source });
            if (!std.mem.eql(u8, mode, "default")) run.addArg(mode);
        }
        run.expectExitCode(0);
        run.expectStdOutEqual(if (semantic or fmt) "" else program.output);
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    };
}

fn addOptionCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step, fixtures: *std.Build.Step.WriteFile, library: std.Build.LazyPath) void {
    const imports = "let standard = @import(\"std\")\nlet direct = @import(\"library/option.nov\")\nlet alternate = @import(\"library/./option.nov\")\nlet O = standard.Option(int)\n";
    _ = fixtures.add("options.nov", imports ++
        \\let Record = struct {
        \\    value: str,
        \\}
        \\let viaFile: (value: O) -> direct.Option(int) = value
        \\let viaAlias: (value: alternate.Option(int)) -> O = value
        \\let make: (present: bool) -> O = {
        \\    @print(
        \\        if present {
        \\            "S"
        \\        } else {
        \\            "N"
        \\        }
        \\    )
        \\    if present {
        \\        .some[1]
        \\    } else {
        \\        .none[]
        \\    }
        \\}
        \\let eager: () -> int = {
        \\    @print("E")
        \\    9
        \\}
        \\let lazy: () -> int = {
        \\    @print("L")
        \\    7
        \\}
        \\let flag: () -> bool = true
        \\let text: () -> str = "lazy"
        \\let record: () -> Record = Record.[ value = "lazy record" ]
        \\let unitCallback: () -> void = @print("unit:")
        \\let unit: (value: standard.Option(void)) -> bool = match value {
        \\    .some => true
        \\    .none => false
        \\}
        \\let propagate: (value: O) -> standard.Option(str) = .some["value:{value.!}"]
        \\let main: () = {
        \\    @println(propagate(make(true)).unwrapOr("bad"), standard.Option(str).unwrapOr(propagate(make(false)), "none"))
        \\    @println(make(true).unwrapOr(eager()))
        \\    @println(O.unwrapOr(make(false), eager()))
        \\    @println(O.unwrapOrElse(make(true), lazy))
        \\    @println(make(false).unwrapOrElse(lazy))
        \\    @println(viaAlias(viaFile(O.some[3])).unwrapOr(0), viaFile(alternate.Option(int).none[]).unwrapOr(4))
        \\    let yes: standard.Option(bool) = .some[true]
        \\    let no: direct.Option(bool) = .none[]
        \\    @println(yes.unwrapOr(false), standard.Option(bool).unwrapOr(no, false), no.unwrapOrElse(flag), standard.Option(bool).unwrapOrElse(yes, flag))
        \\    let word: standard.Option(str) = .some["s" + "ome"]
        \\    let empty: direct.Option(str) = .none[]
        \\    @println(word.unwrapOr("bad"), standard.Option(str).unwrapOr(empty, "eager"), empty.unwrapOrElse(text), standard.Option(str).unwrapOrElse(word, text))
        \\    let item: standard.Option(Record) = .some[Record.[ value = "some record" ]]
        \\    let absent: direct.Option(Record) = .none[]
        \\    @println(item.unwrapOr(Record.[ value = "bad" ]).value, standard.Option(Record).unwrapOr(absent, Record.[ value = "eager record" ]).value)
        \\    @println(absent.unwrapOrElse(record).value, standard.Option(Record).unwrapOrElse(item, record).value)
        \\    @println(unit(standard.Option(void).some[]), unit(direct.Option(void).none[]))
        \\    @print("some:")
        \\    standard.Option(void).some[].unwrapOrElse(unitCallback)
        \\    @print("none:")
        \\    direct.Option(void).none[].unwrapOrElse(unitCallback)
        \\    @println("done")
        \\}
        \\
    );
    const output = "SNvalue:1 none\nSE1\nNE9\nS1\nNL7\n3 4\ntrue false true true\nsome eager lazy some\nsome record eager record\nlazy record some record\ntrue false\nsome:none:unit:done\n";
    _ = fixtures.add("options-example.nov", @embedFile("examples/options.nov"));
    _ = fixtures.add("variant-predicates.nov", imports ++
        \\let Record = struct {
        \\    text: str,
        \\}
        \\let R = standard.Result(Record, Record)
        \\let makeOption: (present: bool) -> O = {
        \\    @print("O")
        \\    if present {
        \\        .some[1]
        \\    } else {
        \\        .none[]
        \\    }
        \\}
        \\let makeResult: (success: bool) -> R = {
        \\    @print("R")
        \\    if success {
        \\        .ok[Record.[ text = "o" + "k" ]]
        \\    } else {
        \\        .err[Record.[ text = "e" + "rr" ]]
        \\    }
        \\}
        \\let main: () = {
        \\    let some = O.some[1]
        \\    let none: direct.Option(int) = .none[]
        \\    let predicate: (O) -> bool = alternate.Option(int).isSome
        \\    @println(predicate(some), some.isNone(), O.isSome(none), none.isNone())
        \\    @println(makeOption(true).isSome(), makeOption(false).isNone())
        \\    let ok = makeResult(true)
        \\    let err = makeResult(false)
        \\    @println(ok.isOk(), ok.isErr(), R.isOk(err), R.isErr(err))
        \\    @println(makeResult(false).isErr())
        \\    let item = standard.Option(Record).some[Record.[ text = "value" ]]
        \\    @println(item.isSome(), standard.Option(Record).none[].isNone())
        \\    @println(standard.Option(void).some[].isSome(), standard.Option(void).none[].isNone())
        \\    @println(standard.Result(void, void).ok[].isOk(), standard.Result(void, void).err[].isErr())
        \\    @println(standard.Option(bool).some[false].isSome(), standard.Result(bool, str).ok[false].isOk())
        \\    @println(ok.unwrapOr(Record.[ text = "fallback" ]).text, err.isErr())
        \\}
        \\
    );
    const Reject = struct { name: []const u8, source: []const u8, diagnostic: []const u8 = "expression does not match the expected type" };
    const rejects = [_]Reject{
        .{ .name = "payload", .source = "let f: () -> O = .some[@as(uint, 1)]\n" },
        .{ .name = "fallback", .source = "let f: (o: O) -> int = o.unwrapOr(@as(uint, 1))\n" },
        .{ .name = "void-fallback", .source = "let f: () -> void = standard.Option(void).unwrapOr(standard.Option(void).none[], @print(\"fallback\"))\n", .diagnostic = "void is not a parameter type" },
        .{ .name = "callback-argument", .source = "let cb: (value: int) -> int = value\nlet f: (o: O) -> int = o.unwrapOrElse(cb)\n" },
        .{ .name = "callback-return", .source = "let cb: () -> uint = 1\nlet f: (o: O) -> int = O.unwrapOrElse(o, cb)\n" },
        .{ .name = "distinct-payload", .source = "let f: (o: direct.Option(uint)) -> O = o\n" },
        .{ .name = "distinct-record", .source = "let A = struct { value: int, }\nlet B = struct { value: int, }\nlet f: (o: direct.Option(A)) -> standard.Option(B) = o\n" },
        .{ .name = "distinct-result", .source = "let f: (o: O) -> standard.Result(int, void) = o\n" },
        .{ .name = "missing-none", .source = "let f: (o: O) -> int = match o { .some => |value| value }\n", .diagnostic = "enum or union match must cover every variant or end with a wildcard (_)" },
        .{ .name = "propagation-family", .source = "let f: (o: O) -> standard.Result(int, void) = .ok[o.!]\n", .diagnostic = ".! requires an enclosing function returning the same union family" },
    };
    for (rejects) |case| {
        const name = b.fmt("option-{s}.nov", .{case.name});
        _ = fixtures.add(name, b.fmt("{s}{s}", .{ imports, case.source }));
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI Option: reject {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", name, "--lib-dir" });
        check.addDirectoryArg(library);
        check.expectExitCode(1);
        check.expectStdOutEqual("");
        check.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&check.step);
    }
    const Program = struct { source: []const u8, output: []const u8 };
    for ([_]Program{
        .{ .source = "options.nov", .output = output },
        .{ .source = "options-example.nov", .output = "Quotient: 21\nUsing fallback\nMissing: 0\n" },
        .{ .source = "variant-predicates.nov", .output = "true false false true\nOOtrue true\nRRtrue false false true\nRtrue\ntrue true\ntrue true\ntrue true\ntrue true\nok true\n" },
    }) |program| {
        for ([_][]const u8{ "semantic", "fmt", "default", "--debug", "--release" }) |mode| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI Option: {s} {s}", .{ program.source, mode });
            run.setCwd(fixtures.getDirectory());
            const semantic = std.mem.eql(u8, mode, "semantic");
            const fmt = std.mem.eql(u8, mode, "fmt");
            if (semantic) {
                run.addArgs(&.{ "check", "--semantic", program.source });
            } else if (fmt) {
                run.addArgs(&.{ "fmt", "--check", program.source });
            } else {
                run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                run.setEnvironmentVariable("NOV_CC", "");
                run.setEnvironmentVariable("CC", "");
                run.addArgs(&.{ "run", program.source });
                if (!std.mem.eql(u8, mode, "default")) run.addArg(mode);
            }
            if (!fmt) {
                run.addArg("--lib-dir");
                run.addDirectoryArg(library);
            }
            run.expectExitCode(0);
            run.expectStdOutEqual(if (semantic or fmt) "" else program.output);
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
}

fn addExportCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("exports.nov", @embedFile("examples/exports.nov"));

    _ = fixtures.add("default.nov", "@[export]\nlet exact: (x: i32) -> i32 = x\n");
    _ = fixtures.add("custom.nov", "@[export(\"custom_exact\")]\nlet renamed: (x: bool) -> bool = x\n");
    const library = fixtures.add("library/std.nov", "@[export]\nlet hidden: (x: i32) -> i32 = x\n" ++
        "@[export(\"custom_hidden\")]\nlet hiddenCustom: (x: bool) -> bool = x\n" ++
        "@[public]\n@[export(\"api_visible\")]\nlet visible: (x: i32) -> i32 = hidden(x)\n");
    for ([_][]const u8{ "file", "named" }) |kind| {
        const imported = if (std.mem.eql(u8, kind, "file")) "library/std.nov" else "std";
        for ([_][]const u8{ "hidden", "hiddenCustom", "visible" }) |member| {
            _ = fixtures.add(b.fmt("{s}-{s}.nov", .{ kind, member }), b.fmt(
                "let api = @import(\"{s}\")\nlet main: () = @println(api.{s}({s}))\n",
                .{ imported, member, if (std.mem.eql(u8, member, "hiddenCustom")) "true" else "7" },
            ));
        }
    }
    _ = fixtures.add("bad-name.nov", "@[export(\"not-a-name\")]\nlet f: () -> i32 = 1\n");
    _ = fixtures.add("bad-abi.nov", "@[export]\nlet f: (x: str) -> str = x\n");
    _ = fixtures.add("duplicate.nov", "@[export(\"same\")]\nlet f: () -> i32 = 1\n@[export(\"same\")]\nlet g: () -> i32 = 2\n");
    // Default names are exact too, so this collides with helper's generated name.
    _ = fixtures.add("collision.nov", "let helper: () -> i32 = 1\n@[export]\nlet nov_helper: () -> i32 = 2\n");
    const Case = struct { args: []const []const u8, diagnostic: ?[]const u8 = null };
    const cases = [_]Case{
        .{ .args = &.{ "check", "--semantic", "exports.nov" } },
        .{ .args = &.{ "fmt", "--check", "exports.nov" } },

        .{ .args = &.{ "check", "--semantic", "default.nov", "custom.nov" } },
        .{ .args = &.{ "fmt", "--check", "default.nov" } },
        .{ .args = &.{ "fmt", "--check", "custom.nov" } },
        .{ .args = &.{ "emit-c", "exports.nov" }, .diagnostic = "requires a root function 'main'" },
        .{ .args = &.{ "build", "exports.nov" }, .diagnostic = "requires a root function 'main'" },
        .{ .args = &.{ "run", "exports.nov" }, .diagnostic = "requires a root function 'main'" },
        .{ .args = &.{ "emit-c", "--library", "bad-name.nov" }, .diagnostic = "bad-name.nov:1:1: error: export name" },
        .{ .args = &.{ "emit-c", "--library", "bad-abi.nov" }, .diagnostic = "export ABI supports only" },
        .{ .args = &.{ "emit-c", "--library", "duplicate.nov" }, .diagnostic = "first export of this symbol here" },
        .{ .args = &.{ "emit-c", "--library", "collision.nov" }, .diagnostic = "export symbol conflicts with a generated function name" },
    };
    for (cases) |case| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI exports: {s} {s}", .{ case.args[0], case.args[case.args.len - 1] });
        run.setCwd(fixtures.getDirectory());
        run.addArgs(case.args);
        run.expectExitCode(if (case.diagnostic != null) 1 else 0);
        run.expectStdOutEqual("");
        if (case.diagnostic) |message| run.expectStdErrMatch(message) else run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    for ([_][]const u8{ "check", "fmt", "build", "run", "exports.nov" }) |command| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI exports: reject --library for {s}", .{command});
        run.setCwd(fixtures.getDirectory());
        run.addArgs(&.{ command, "--library" });
        if (!std.mem.eql(u8, command, "exports.nov")) run.addArg("exports.nov");
        run.expectExitCode(1);
        run.expectStdOutEqual("");
        run.expectStdErrEqual("error: --library requires the emit-c command\n");
        test_step.dependOn(&run.step);
    }
    for ([_][]const u8{ "file", "named" }) |kind| {
        for ([_][]const u8{ "hidden", "hiddenCustom", "visible" }) |member| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI exports: {s} import {s}", .{ kind, member });
            run.setCwd(fixtures.getDirectory());
            run.addArgs(&.{ "check", "--semantic", b.fmt("{s}-{s}.nov", .{ kind, member }), "--lib-dir" });
            run.addDirectoryArg(library.dirname());
            const visible = std.mem.eql(u8, member, "visible");
            run.expectExitCode(if (visible) 0 else 1);
            run.expectStdOutEqual("");
            if (visible) run.expectStdErrEqual("") else run.expectStdErrMatch(b.fmt("{s}-{s}.nov:2:", .{ kind, member }));
            test_step.dependOn(&run.step);
        }
    }
    const package = b.addWriteFiles();
    const installed_exe = package.addCopyFile(exe.getEmittedBin(), b.fmt("bin/{s}", .{exe.out_filename}));
    _ = package.addCopyFile(library, "lib/nov/std.nov");
    for ([_][]const u8{ "check", "emit-c" }) |command| {
        const installed = std.Build.Step.Run.create(b, b.fmt("CLI exports: installed default library {s}", .{command}));
        installed.addFileArg(installed_exe);
        installed.setCwd(fixtures.getDirectory());
        installed.addArg(command);
        installed.addArg(if (std.mem.eql(u8, command, "check")) "--semantic" else "--library");
        installed.addArg("named-visible.nov");
        installed.expectExitCode(0);
        installed.expectStdErrEqual("");
        if (std.mem.eql(u8, command, "check")) installed.expectStdOutEqual("") else _ = installed.captureStdOut(.{});
        test_step.dependOn(&installed.step);
    }
    const override = std.Build.Step.Run.create(b, "CLI exports: --library preserves explicit lib-dir override");
    override.addFileArg(installed_exe);
    override.setCwd(fixtures.getDirectory());
    override.addArgs(&.{ "emit-c", "--library", "named-visible.nov", "--lib-dir", "missing-library" });
    override.expectExitCode(1);
    override.expectStdOutEqual("");
    override.expectStdErrMatch("cannot load imported source: FileNotFound");
    test_step.dependOn(&override.step);
    const client = fixtures.add("client.c",
        \\#include <stdbool.h>
        \\#include <stdint.h>
        \\extern int32_t add_i32(int32_t, int32_t);
        \\extern uint64_t api_select_u64(bool, uint64_t, uint64_t);
        \\extern bool invert(bool);
        \\int main(void) {
        \\    if (add_i32(-40, 2) != -38) return 1;
        \\    if (api_select_u64(true, UINT64_MAX, 7) != UINT64_MAX) return 2;
        \\    if (api_select_u64(false, UINT64_MAX, 7) != 7) return 3;
        \\    if (!invert(false) || invert(true)) return 4;
        \\    return 0;
        \\}
        \\
    );
    const emit = b.addRunArtifact(exe);
    emit.step.name = "CLI exports: emit library without main";
    emit.setCwd(fixtures.getDirectory());
    emit.addArgs(&.{ "emit-c", "--library", "exports.nov", "--lib-dir" });
    emit.addDirectoryArg(library.dirname());
    emit.expectExitCode(0);
    emit.expectStdErrEqual("");
    const c_source = emit.captureStdOut(.{});
    const inspect = b.addCheckFile(c_source, .{ .expected_matches = &.{ "int32_t add_i32(", "uint64_t api_select_u64(", "bool invert(" } });
    inspect.setName("CLI exports: raw exact scalar symbols");
    test_step.dependOn(&inspect.step);
    for ([_][]const u8{ "-O0", "-O3" }) |optimization| {
        const compile = b.addSystemCommand(&.{ b.graph.zig_exe, "cc", "-x", "c", "-std=c11", "-Wall", "-Wextra", "-Werror", optimization });
        if (std.mem.eql(u8, optimization, "-O0")) compile.addArg("-g");
        compile.addFileArg(c_source);
        compile.addFileArg(client);
        compile.addArg("-o");
        const binary = compile.addOutputFileArg(if (exe.root_module.resolved_target.?.result.os.tag == .windows) "exports.exe" else "exports");
        const execute = std.Build.Step.Run.create(b, b.fmt("CLI exports: separate-TU C ABI {s}", .{optimization}));
        execute.addFileArg(binary);
        execute.expectExitCode(0);
        execute.expectStdOutEqual("");
        execute.expectStdErrEqual("");
        test_step.dependOn(&execute.step);
    }
    for ([_][]const u8{ "-O0", "-O3" }) |optimization| {
        const compile = b.addSystemCommand(&.{ b.graph.zig_exe, "cc", "-x", "c", "-std=c11", "-Wall", "-Wextra", "-Wpedantic", "-Werror", "-fsanitize=undefined", "-fno-sanitize-recover=all", optimization });
        compile.addFileArg(c_source);
        compile.addFileArg(b.path("examples/exports.c"));
        compile.addArg("-o");
        const binary = compile.addOutputFileArg(if (exe.root_module.resolved_target.?.result.os.tag == .windows) "example.exe" else "example");
        const execute = std.Build.Step.Run.create(b, b.fmt("CLI exports: checked-in example {s}", .{optimization}));
        execute.addFileArg(binary);
        execute.expectExitCode(0);
        execute.expectStdOutEqual("Sum: 42\nSelected: 42\nInverted: true\nArea: 42\nPerimeter: 26\n");
        execute.expectStdErrEqual("");
        test_step.dependOn(&execute.step);
    }
}

fn addExternCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("extern.nov", @embedFile("examples/extern.nov"));
    _ = fixtures.add("libc.nov", "@[extern]\nlet abs: (x: i32) -> i32\nlet main: () = @println(abs(-42))\n");
    _ = fixtures.add("library.nov", "@[extern]\nlet foreign_unit: ()\n@[extern(\"foreign_\\x73top\")]\n@[noreturn]\nlet stop: ()\n");
    var scalar_source: std.Io.Writer.Allocating = .init(b.allocator);
    for ([_][]const u8{ "bool", "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "int", "uint" }) |ty| {
        scalar_source.writer.print("@[extern]\nlet identity_{s}: (x: {s}) -> {s}\n", .{ ty, ty, ty }) catch @panic("OOM");
    }
    _ = fixtures.add("scalars.nov", scalar_source.written());
    for ([_][]const u8{ "extern.nov", "library.nov", "scalars.nov" }) |source| {
        for ([_][]const u8{ "semantic", "fmt" }) |mode| {
            const check = b.addRunArtifact(exe);
            check.step.name = b.fmt("CLI extern: {s} {s}", .{ mode, source });
            check.setCwd(fixtures.getDirectory());
            check.addArgs(if (std.mem.eql(u8, mode, "semantic")) &.{ "check", "--semantic", source } else &.{ "fmt", "--check", source });
            check.expectExitCode(0);
            check.expectStdOutEqual("");
            check.expectStdErrEqual("");
            test_step.dependOn(&check.step);
        }
    }
    const Rejection = struct { name: []const u8, source: []const u8, diagnostic: []const u8 };
    for ([_]Rejection{
        .{ .name = "body", .source = "@[extern]\nlet f: () = {}\n", .diagnostic = "prototype without a body" },
        .{ .name = "mut", .source = "@[extern]\nlet mut f: ()\n", .diagnostic = "immutable top-level function" },
        .{ .name = "missing-body", .source = "let f: ()\n", .diagnostic = "initializer" },
        .{ .name = "argument", .source = "@[extern(1)]\nlet f: ()\n", .diagnostic = "accepts no argument" },
        .{ .name = "arguments", .source = "@[extern(\"a\", \"b\")]\nlet f: ()\n", .diagnostic = "accepts no argument" },
        .{ .name = "duplicate", .source = "@[extern]\n@[extern]\nlet f: ()\n", .diagnostic = "cannot be repeated" },
        .{ .name = "abi-param", .source = "@[extern]\nlet f: (x: str)\n", .diagnostic = "extern ABI supports only" },
        .{ .name = "abi-result", .source = "@[extern]\nlet f: () -> str\n", .diagnostic = "extern ABI supports only" },
        .{ .name = "abi-wide", .source = "@[extern]\nlet f: (x: i128)\n", .diagnostic = "extern ABI supports only" },
        .{ .name = "name", .source = "@[extern(\"not-a-name\")]\nlet f: ()\n", .diagnostic = "extern name must be" },
        .{ .name = "export", .source = "@[extern]\n@[export]\nlet f: ()\n", .diagnostic = "cannot be combined" },
        .{ .name = "conflict", .source = "@[extern(\"same\")]\nlet f: (x: i32)\n@[extern(\"same\")]\nlet g: (x: bool)\n", .diagnostic = "same canonical signature" },
        .{ .name = "noreturn-result", .source = "@[extern]\n@[noreturn]\nlet f: () -> i32\n", .diagnostic = "must return void" },
        .{ .name = "collision", .source = "let helper: () = {}\n@[extern(\"nov_helper\")]\nlet f: ()\n", .diagnostic = "extern symbol conflicts" },
    }) |case| {
        const source = b.fmt("bad-{s}.nov", .{case.name});
        _ = fixtures.add(source, case.source);
        const reject = b.addRunArtifact(exe);
        reject.step.name = b.fmt("CLI extern: reject {s} without stdout", .{case.name});
        reject.setCwd(fixtures.getDirectory());
        reject.addArgs(&.{ "emit-c", "--library", source });
        reject.expectExitCode(1);
        reject.expectStdOutEqual("");
        reject.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&reject.step);
    }
    const library = fixtures.add("library/std.nov", "@[extern]\nlet abs: (x: i32) -> i32\n@[extern(\"c_hidden\")]\nlet hidden: (x: i32) -> i32\n@[public]\n@[extern(\"abs\")]\nlet visible: (x: i32) -> i32\n");
    for ([_][]const u8{ "file", "named" }) |kind| {
        for ([_][]const u8{ "abs", "hidden", "visible" }) |member| {
            const source = b.fmt("{s}-{s}.nov", .{ kind, member });
            _ = fixtures.add(source, b.fmt("let api = @import(\"{s}\")\nlet main: () = @println(api.{s}(-42))\n", .{ if (std.mem.eql(u8, kind, "file")) "library/std.nov" else "std", member }));
            const check = b.addRunArtifact(exe);
            check.step.name = b.fmt("CLI extern: {s} import {s}", .{ kind, member });
            check.setCwd(fixtures.getDirectory());
            check.addArgs(&.{ "check", "--semantic", source, "--lib-dir" });
            check.addDirectoryArg(library.dirname());
            const visible = std.mem.eql(u8, member, "visible");
            check.expectExitCode(if (visible) 0 else 1);
            check.expectStdOutEqual("");
            if (visible) check.expectStdErrEqual("") else check.expectStdErrMatch(b.fmt("{s}:2:", .{source}));
            test_step.dependOn(&check.step);
        }
    }
    const emit_library = b.addRunArtifact(exe);
    emit_library.setCwd(fixtures.getDirectory());
    emit_library.addArgs(&.{ "emit-c", "--library", "library.nov" });
    emit_library.expectExitCode(0);
    emit_library.expectStdErrEqual("");
    const library_c = emit_library.captureStdOut(.{});
    const inspect = b.addCheckFile(library_c, .{ .expected_matches = &.{ "void foreign_unit(void);", "_Noreturn void foreign_stop(void);" } });
    inspect.setName("CLI extern: exact and decoded library prototypes");
    test_step.dependOn(&inspect.step);
    // Linking with a separate C main also proves --library emits no process main.
    const client = fixtures.add("library-client.c", "int main(void) { return 0; }\n");
    const compile_library = b.addSystemCommand(&.{ b.graph.zig_exe, "cc", "-x", "c", "-std=c11", "-Wall", "-Wextra", "-Wpedantic", "-Werror" });
    compile_library.addFileArg(library_c);
    compile_library.addFileArg(client);
    compile_library.addArg("-o");
    _ = compile_library.addOutputFileArg("extern-library");
    test_step.dependOn(&compile_library.step);

    const emit = b.addRunArtifact(exe);
    emit.setCwd(fixtures.getDirectory());
    emit.addArgs(&.{ "emit-c", "extern.nov" });
    emit.expectExitCode(0);
    emit.expectStdErrEqual("");
    const c_source = emit.captureStdOut(.{});
    if (builtin.os.tag != .windows) {
        // Linker exit codes vary; the matching positive compile below checks the C itself.
        const missing = b.addSystemCommand(&.{ "sh", "-c", "if \"$@\"; then exit 1; else exit 0; fi", "extern-missing-provider", b.graph.zig_exe, "cc", "-x", "c", "-std=c11", "-O0" });
        missing.step.name = "CLI extern: missing provider fails to link";
        missing.addFileArg(c_source);
        missing.addArg("-o");
        _ = missing.addOutputFileArg("missing-provider");
        missing.expectExitCode(0);
        missing.expectStdOutEqual("");
        missing.expectStdErrMatch("c_max_i32");
        test_step.dependOn(&missing.step);
    }
    for ([_][]const u8{ "-O0", "-O3" }) |optimization| {
        const compile = b.addSystemCommand(&.{ b.graph.zig_exe, "cc", "-x", "c", "-std=c11", "-Wall", "-Wextra", "-Wpedantic", "-Werror", "-fsanitize=undefined", "-fno-sanitize-recover=all", optimization });
        compile.addFileArg(c_source);
        compile.addFileArg(b.path("examples/extern.c"));
        compile.addArg("-o");
        const binary = compile.addOutputFileArg(if (exe.root_module.resolved_target.?.result.os.tag == .windows) "extern.exe" else "extern");
        const execute = std.Build.Step.Run.create(b, b.fmt("CLI extern: checked-in example {s}", .{optimization}));
        execute.addFileArg(binary);
        execute.expectExitCode(0);
        execute.expectStdOutEqual("Abs: 42\nMaximum: 42\nEven: true\n");
        execute.expectStdErrEqual("");
        test_step.dependOn(&execute.step);
    }
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_]bool{ false, true }) |build_program| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI extern: libc {s} {s}", .{ if (build_program) "build" else "run", mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ if (build_program) "build" else "run", "libc.nov" });
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdErrEqual("");
            if (build_program) {
                run.addArg("-o");
                const binary = run.addOutputFileArg(if (exe.root_module.resolved_target.?.result.os.tag == .windows) "libc.exe" else "libc");
                run.expectStdOutEqual("");
                const execute = std.Build.Step.Run.create(b, b.fmt("CLI extern: execute libc build {s}", .{mode}));
                execute.addFileArg(binary);
                execute.expectExitCode(0);
                execute.expectStdOutEqual("42\n");
                execute.expectStdErrEqual("");
                test_step.dependOn(&execute.step);
            } else {
                run.expectStdOutEqual("42\n");
                test_step.dependOn(&run.step);
            }
        }
    }
}

fn addImportCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "library", .{});
    const program = fixtures.add("app/main.nov",
        \\let standard = @import("std")
        \\let left = @import("parts/left.nov")
        \\let right = @import("parts/right.nov")
        \\let R = standard.Result(uint, str)
        \\let recover: (error: str) -> uint = { @print(error)
        \\    left.value() }
        \\let main: () = {
        \\    let bad: R = .err["fallback:"]
        \\    @println(bad.unwrapOrElse(recover), right.value())
        \\}
        \\
    );
    _ = fixtures.add("app/parts/left.nov", "let shared = @import(\"../shared.nov\")\n@[public]\nlet value: () -> uint = shared.value()\n");
    _ = fixtures.add("app/parts/right.nov", "let shared = @import(\".././shared.nov\")\n@[public]\nlet value: () -> uint = shared.value()\n");
    _ = fixtures.add("app/shared.nov", "@[public]\nlet value: () -> uint = 7\nlet main: () = @println(\"wrong entry\")\n");
    _ = fixtures.add("relative.nov", "let left = @import(\"app/parts/left.nov\")\nlet right = @import(\"app/parts/right.nov\")\nlet main: () = @println(left.value(), right.value())\n");
    _ = fixtures.add("missing.nov", "let absent = @import(\"not-found.nov\")\nlet main: () = {}\n");
    _ = fixtures.add("cycle/a.nov", "let b = @import(\"b.nov\")\n");
    _ = fixtures.add("cycle/b.nov", "let a = @import(\"a.nov\")\n");
    _ = fixtures.add("self.nov",
        \\let self = @import("./self.nov")
        \\@[public]
        \\let value: () -> int = 7
        \\let main: () = @println(self.value())
        \\@[test]
        \\let self_import: () = @assert(self.value() == 7)
        \\
    );
    _ = fixtures.add("mutual/main.nov",
        \\let peer = @import("peer.nov")
        \\let standard = @import("std")
        \\let relative = @import("../library/./std.nov")
        \\@[public]
        \\let Option = standard.Option
        \\@[public]
        \\let base: () -> int = 7
        \\let verify: () -> int = {
        \\    let from_peer: Option(int) = peer.value()
        \\    let canonical: relative.Option(int) = from_peer
        \\    let reexported: peer.Option(int) = canonical
        \\    @assert(reexported.unwrapOr(0) == 8)
        \\    reexported.unwrapOr(0)
        \\}
        \\let main: () = @println(verify())
        \\@[test]
        \\let mutual_import: () = @assert(verify() == 8)
        \\
    );
    _ = fixtures.add("mutual/peer.nov",
        \\let root = @import("./main.nov")
        \\@[public]
        \\let Option = root.Option
        \\@[public]
        \\let value: () -> Option(int) = .some[root.base() + 1]
        \\let secret = 42
        \\let Hidden = int
        \\
    );
    _ = fixtures.add("mutual/private-value.nov", "let peer = @import(\"peer.nov\")\nlet main: () = @println(peer.secret)\n");
    _ = fixtures.add("mutual/private-type.nov", "let peer = @import(\"peer.nov\")\nlet use: (value: peer.Hidden) = {}\nlet main: () = {}\n");
    _ = fixtures.add("value-cycle/a.nov", "let peer = @import(\"b.nov\")\n@[public]\nlet value = peer.value\nlet main: () = @println(value)\n");
    _ = fixtures.add("value-cycle/b.nov", "let peer = @import(\"a.nov\")\n@[public]\nlet value = peer.value\n");
    _ = fixtures.add("type-cycle/a.nov", "let peer = @import(\"b.nov\")\n@[public]\nlet Alias = peer.Alias\nlet use: (value: Alias) = {}\nlet main: () = {}\n");
    _ = fixtures.add("type-cycle/b.nov", "let peer = @import(\"a.nov\")\n@[public]\nlet Alias = peer.Alias\n");
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "self.nov", "mutual/main.nov" }) |source| {
            for ([_][]const u8{ "run", "test" }) |command| {
                const run = b.addRunArtifact(exe);
                run.step.name = b.fmt("CLI imports: {s} {s} {s}", .{ command, source, mode });
                run.setCwd(fixtures.getDirectory());
                run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                run.setEnvironmentVariable("NOV_CC", "");
                run.setEnvironmentVariable("CC", "");
                run.addArgs(&.{ command, source });
                if (mode.len != 0) run.addArg(mode);
                run.addArg("--lib-dir");
                run.addDirectoryArg(library);
                run.expectExitCode(0);
                const self = std.mem.eql(u8, source, "self.nov");
                run.expectStdOutEqual(if (std.mem.eql(u8, command, "test"))
                    (if (self) "PASS self_import\n1 passed; 0 failed; 1 total\n" else "PASS mutual_import\n1 passed; 0 failed; 1 total\n")
                else if (self) "7\n" else "8\n");
                run.expectStdErrEqual("");
                test_step.dependOn(&run.step);
            }
        }
    }
    _ = fixtures.add("no-entry.nov", "let imported = @import(\"app/shared.nov\")\n");
    _ = fixtures.add("bad-entry.nov", "let imported = @import(\"app/shared.nov\")\nlet main = true\n");
    _ = fixtures.add("bad-import.nov", "let imported = @import(\"errors/body.nov\")\nlet main: () = {}\n");
    _ = fixtures.add("errors/body.nov", "let bad: () -> bool = absent\n");
    _ = fixtures.add("wide-import.nov", "let imported = @import(\"errors/wide.nov\")\nlet main: () = {}\n");
    _ = fixtures.add("errors/wide.nov", "let wide: () -> i128 = 1\n");
    _ = fixtures.add("hidden.nov", "let standard = @import(\"std\")\nlet value: bool = standard.hidden\n");
    const visibility = fixtures.add("visibility/std.nov", "let hidden = true\n");
    _ = fixtures.add("private/library.nov",
        \\let secret:int=7
        \\@[public]
        \\let Thing=struct{name:str,
        \\@[stringify]
        \\let toString:(self:Thing)->str=self.name
        \\let get:(self:Thing)->int=secret
        \\@[public]
        \\let report:(self:Thing)=@println(self,self.get())
        \\}
    );
    _ = fixtures.add("private/main.nov", "let lib=@import(\"library.nov\")\nlet Thing=lib.Thing\nlet main:()={let x=Thing.[name=\"private helper\"]\nx.report()}\n");
    _ = fixtures.add("private/global.nov", "let lib=@import(\"library.nov\")\nlet main:()=@println(lib.secret)\n");
    _ = fixtures.add("private/method.nov", "let lib=@import(\"library.nov\")\nlet main:()={let x=lib.Thing.[name=\"hidden\"]\n@println(x.get())}\n");
    _ = fixtures.add("private/print.nov", "let lib=@import(\"library.nov\")\nlet main:()=@println(lib.Thing.[name=\"hidden\"])\n");
    for ([_][]const u8{ "--debug", "--release" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI visibility: defining-file helpers {s}", .{mode});
        run.setCwd(fixtures.getDirectory());
        run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        run.setEnvironmentVariable("NOV_CC", "");
        run.setEnvironmentVariable("CC", "");
        run.addArgs(&.{ "run", "private/main.nov", mode });
        run.expectExitCode(0);
        run.expectStdOutEqual("private helper 7\n");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    addOptionCliTests(b, exe, test_step, fixtures, library);
    const Case = struct { name: []const u8, args: []const []const u8, code: u8, diagnostic: ?[]const u8 = null };
    const cases = [_]Case{
        .{ .name = "private top level", .args = &.{ "emit-c", "private/global.nov" }, .code = 1, .diagnostic = "global.nov:2:" },
        .{ .name = "private receiver method", .args = &.{ "emit-c", "private/method.nov" }, .code = 1, .diagnostic = "method.nov:3:" },
        .{ .name = "private converter", .args = &.{ "emit-c", "private/print.nov" }, .code = 1, .diagnostic = "print.nov:2:" },
        .{ .name = "relative diamond", .args = &.{ "check", "--semantic", "relative.nov" }, .code = 0 },
        .{ .name = "relative diamond and library callbacks", .args = &.{ "check", "--semantic", "app/main.nov" }, .code = 0 },
        .{ .name = "missing imports stay syntax only", .args = &.{ "check", "missing.nov" }, .code = 0 },
        .{ .name = "missing import", .args = &.{ "check", "--semantic", "missing.nov" }, .code = 1, .diagnostic = "cannot load imported source: FileNotFound" },
        .{ .name = "missing import buffers C", .args = &.{ "emit-c", "missing.nov" }, .code = 1, .diagnostic = "cannot load imported source" },
        .{ .name = "legal namespace cycle", .args = &.{ "check", "--semantic", "cycle/a.nov" }, .code = 0 },
        .{ .name = "cross-module value cycle", .args = &.{ "emit-c", "value-cycle/a.nov" }, .code = 1, .diagnostic = "cyclic value dependency" },
        .{ .name = "cross-module type alias cycle", .args = &.{ "emit-c", "type-cycle/a.nov" }, .code = 1, .diagnostic = "cyclic type alias dependency" },
        .{ .name = "cyclic module private value", .args = &.{ "emit-c", "mutual/private-value.nov" }, .code = 1, .diagnostic = "private-value.nov:2:" },
        .{ .name = "cyclic module private type", .args = &.{ "emit-c", "mutual/private-type.nov" }, .code = 1, .diagnostic = "private-type.nov:2:" },
        .{ .name = "imported main is not entry", .args = &.{ "emit-c", "no-entry.nov" }, .code = 1, .diagnostic = "no-entry.nov:1:1: error:" },
        .{ .name = "root entry signature", .args = &.{ "emit-c", "bad-entry.nov" }, .code = 1, .diagnostic = "bad-entry.nov:2:5: error:" },
        .{ .name = "imported body diagnostic", .args = &.{ "emit-c", "bad-import.nov" }, .code = 1, .diagnostic = "body.nov:1:23: error: unknown name 'absent'" },
        .{ .name = "imported backend diagnostic", .args = &.{ "emit-c", "wide-import.nov" }, .code = 1, .diagnostic = "wide.nov:1:" },
    };
    for (cases) |case| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI imports: {s}", .{case.name});
        run.stdio_limit = .limited(8192);
        run.setCwd(fixtures.getDirectory());
        run.addArgs(case.args);
        if (!(case.args.len == 2 and std.mem.eql(u8, case.args[0], "check"))) {
            run.addArg("--lib-dir");
            run.addDirectoryArg(library);
        }
        run.expectExitCode(case.code);
        run.expectStdOutEqual("");
        if (case.diagnostic) |message| run.expectStdErrMatch(message) else run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    for ([_][]const u8{ "root", "relative", "std", "result" }) |target| {
        const protected = b.addWriteFiles();
        const root_name = b.fmt("protect-{s}.nov", .{target});
        const root_source = "let imported = @import(\"imported.nov\")\nlet standard = @import(\"std\")\nlet R = standard.Result(uint, str)\nlet main: () = @println(imported.value())\n";
        const root_file = protected.add(root_name, root_source);
        const imported_source = "@[public]\nlet value: () -> uint = 7\n";
        const imported_file = protected.add("imported.nov", imported_source);
        const protected_library = protected.addCopyDirectory(b.path("lib"), "library", .{});
        const output_file = if (std.mem.eql(u8, target, "root")) root_file else if (std.mem.eql(u8, target, "relative")) imported_file else protected_library.path(b, b.fmt("{s}.nov", .{target}));
        const expected_source = if (std.mem.eql(u8, target, "root")) root_source else if (std.mem.eql(u8, target, "relative")) imported_source else if (std.mem.eql(u8, target, "std")) @embedFile("lib/std.nov") else @embedFile("lib/result.nov");
        const reject = b.addRunArtifact(exe);
        reject.step.name = b.fmt("CLI imports: protect {s} source from output overwrite", .{target});
        reject.setCwd(protected.getDirectory());
        reject.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        reject.setEnvironmentVariable("NOV_CC", "");
        reject.setEnvironmentVariable("CC", "");
        reject.addArgs(&.{ "build", root_name, "--lib-dir" });
        reject.addDirectoryArg(protected_library);
        reject.addArg("-o");
        reject.addFileArg(output_file);
        reject.expectExitCode(1);
        reject.expectStdOutEqual("");
        reject.expectStdErrEqual("error: output path refers to the input source file\n");
        const preserved = b.addCheckFile(output_file, .{ .expected_exact = expected_source });
        preserved.setName(b.fmt("CLI imports: {s} source remains unchanged", .{target}));
        preserved.step.dependOn(&reject.step);
        test_step.dependOn(&preserved.step);
    }

    const relative = b.addRunArtifact(exe);
    relative.step.name = "CLI imports: relative diamond run selects root main";
    relative.setCwd(fixtures.getDirectory());
    relative.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
    relative.setEnvironmentVariable("NOV_CC", "");
    relative.setEnvironmentVariable("CC", "");
    relative.addArgs(&.{ "run", "relative.nov" });
    relative.expectExitCode(0);
    relative.expectStdOutEqual("7 7\n");
    relative.expectStdErrEqual("");
    test_step.dependOn(&relative.step);

    const hidden = b.addRunArtifact(exe);
    hidden.step.name = "CLI imports: named module hides private declarations";
    hidden.setCwd(fixtures.getDirectory());
    hidden.addArgs(&.{ "check", "--semantic", "hidden.nov", "--lib-dir" });
    hidden.addDirectoryArg(visibility.dirname());
    hidden.expectExitCode(1);
    hidden.expectStdOutEqual("");
    hidden.expectStdErrMatch("hidden.nov:2:");
    test_step.dependOn(&hidden.step);

    for ([_][]const u8{ "--debug", "--release" }) |mode| {
        for ([_]bool{ false, true }) |build_program| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI imports: {s} {s}", .{ if (build_program) "build" else "run", mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ if (build_program) "build" else "run", "app/main.nov", mode, "--lib-dir" });
            run.addDirectoryArg(library);
            run.expectExitCode(0);
            run.expectStdErrEqual("");
            if (build_program) {
                run.addArg("-o");
                const binary = run.addOutputFileArg(if (exe.root_module.resolved_target.?.result.os.tag == .windows) "imports.exe" else "imports");
                run.expectStdOutEqual("");
                const execute = std.Build.Step.Run.create(b, b.fmt("CLI imports: execute built {s}", .{mode}));
                execute.addFileArg(binary);
                execute.expectExitCode(0);
                execute.expectStdOutEqual("fallback:7 7\n");
                execute.expectStdErrEqual("");
                test_step.dependOn(&execute.step);
            } else {
                run.expectStdOutEqual("fallback:7 7\n");
                test_step.dependOn(&run.step);
            }
        }
    }

    const emit = b.addRunArtifact(exe);
    emit.step.name = "CLI imports: emitted C compiles and runs";
    emit.setCwd(fixtures.getDirectory());
    emit.addArgs(&.{ "emit-c", "app/main.nov", "--lib-dir" });
    emit.addDirectoryArg(library);
    emit.expectExitCode(0);
    emit.expectStdErrEqual("");
    const c_source = emit.captureStdOut(.{});
    const compile = b.addSystemCommand(&.{ b.graph.zig_exe, "cc", "-x", "c", "-std=c11", "-Wall", "-Wextra", "-Werror" });
    compile.addFileArg(c_source);
    compile.addArg("-o");
    const binary = compile.addOutputFileArg(if (exe.root_module.resolved_target.?.result.os.tag == .windows) "emitted.exe" else "emitted");
    const execute = std.Build.Step.Run.create(b, "CLI imports: execute emitted C");
    execute.addFileArg(binary);
    execute.expectExitCode(0);
    execute.expectStdOutEqual("fallback:7 7\n");
    execute.expectStdErrEqual("");
    test_step.dependOn(&execute.step);

    // Recreate the installed layout in the cache, away from both source and cwd.
    const package = b.addWriteFiles();
    const installed_exe = package.addCopyFile(exe.getEmittedBin(), b.fmt("bin/{s}", .{exe.out_filename}));
    _ = package.addCopyDirectory(b.path("lib"), "lib/nov", .{});
    for ([_][]const u8{ "check", "emit-c", "build", "run" }) |command| {
        const installed = std.Build.Step.Run.create(b, b.fmt("CLI imports: relocated installed {s}", .{command}));
        installed.addFileArg(installed_exe);
        installed.setCwd(fixtures.getDirectory());
        installed.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        installed.setEnvironmentVariable("NOV_CC", "");
        installed.setEnvironmentVariable("CC", "");
        installed.addArg(command);
        if (std.mem.eql(u8, command, "check")) installed.addArg("--semantic");
        installed.addFileArg(program);
        if (std.mem.eql(u8, command, "build")) {
            installed.addArg("-o");
            _ = installed.addOutputFileArg(if (exe.root_module.resolved_target.?.result.os.tag == .windows) "installed-program.exe" else "installed-program");
        }
        installed.expectExitCode(0);
        installed.expectStdErrEqual("");
        if (std.mem.eql(u8, command, "emit-c")) {
            _ = installed.captureStdOut(.{});
        } else {
            installed.expectStdOutEqual(if (std.mem.eql(u8, command, "run")) "fallback:7 7\n" else "");
        }
        test_step.dependOn(&installed.step);
    }
    const override = std.Build.Step.Run.create(b, "CLI imports: explicit library overrides installed default");
    override.addFileArg(installed_exe);
    override.setCwd(fixtures.getDirectory());
    override.addArgs(&.{ "check", "--semantic", "app/main.nov", "--lib-dir", "missing-library" });
    override.expectExitCode(1);
    override.expectStdOutEqual("");
    override.expectStdErrMatch("cannot load imported source: FileNotFound");
    test_step.dependOn(&override.step);
}

const ExecutableOptions = struct {
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    build_options: *std.Build.Step.Options,
    strip: ?bool = null,
    lto: ?std.zig.LtoMode = null,
};

fn makeExe(b: *std.Build, options: ExecutableOptions) *std.Build.Step.Compile {
    const isocline = b.dependency("isocline", .{}).module("isocline");
    const clap = b.dependency("clap", .{}).module("clap");
    const known_folders = b.dependency("known_folders", .{}).module("known-folders");
    const axe = b.dependency("axe", .{}).module("axe");

    const exe_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = options.target,
        .optimize = options.optimize,
        .strip = options.strip,
        .link_libc = true,
    });

    const exe = b.addExecutable(.{
        .name = "nov",
        .root_module = exe_module,
    });

    exe.lto = options.lto;
    exe.root_module.addOptions("build_options", options.build_options);
    exe.root_module.addImport("isocline", isocline);
    exe.root_module.addImport("clap", clap);
    exe.root_module.addImport("axe", axe);
    exe.root_module.addImport("known-folders", known_folders);

    return exe;
}

fn addReferenceCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    for ([_][]const u8{ "fmt", "semantic" }) |mode| {
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI references: {s}", .{mode});
        check.addArgs(if (std.mem.eql(u8, mode, "fmt")) &.{ "fmt", "--check" } else &.{ "check", "--semantic" });
        check.addFileArg(b.path("examples/references.nov"));
        check.expectExitCode(0);
        check.expectStdOutEqual("");
        check.expectStdErrEqual("");
        test_step.dependOn(&check.step);
    }
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test" }) |command| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI references: {s} ({s})", .{ command, mode });
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArg(command);
            run.addFileArg(b.path("examples/references.nov"));
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (std.mem.eql(u8, command, "run"))
                "Rebound: 1 2\nReference: &mut u8\nArrays: [3, 4] [3, 5]\nReadonly: &[]u8 5\n"
            else
                "PASS readonly_and_downgrade\nPASS mutable_and_straightline_rebind\nPASS scalar_and_struct_pointees\nPASS arrays_indices_and_iteration\nPASS functions_ranges_and_unions\n5 passed; 0 failed; 5 total\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    const fixtures = b.addWriteFiles();
    const unsupported = "local references require";
    const immutable = "assignment requires a mutable local";
    for ([_]struct { name: []const u8, body: []const u8, diagnostic: []const u8 }{
        .{ .name = "immutable-binding-rebind", .body = "let p=&a\np=&b", .diagnostic = immutable },
        .{ .name = "readonly-owner-write", .body = "let fixed:u8=0\nlet mut p=&fixed\np.* = 1", .diagnostic = immutable },
        .{ .name = "downgraded-write", .body = "let p:&u8=&a\np.* = 1", .diagnostic = immutable },
        .{ .name = "readonly-rebound-write", .body = "let mut p:&u8=&a\np=&b\np.* = 1", .diagnostic = immutable },
        .{ .name = "mutable-upgrade", .body = "let fixed:u8=0\nlet p:&mut u8=&fixed", .diagnostic = "expression does not match the expected type" },
        .{ .name = "branch-inner-owner-escape", .body = "let mut p=&a\nif flag {let mut inner:u8=0\np=&inner}", .diagnostic = unsupported },
        .{ .name = "both-branches-inner-owner-escape", .body = "let mut p=&a\nif flag {let mut inner:u8=0\np=&inner} else {p=&b}", .diagnostic = unsupported },
        .{ .name = "loop-inner-owner-escape", .body = "let mut p=&a\nloop flag {let mut inner:u8=0\np=&inner}", .diagnostic = unsupported },
        .{ .name = "short-circuit-inner-owner-escape", .body = "let mut p=&a\nlet result=flag and {let mut inner:u8=0\np=&inner\ntrue}", .diagnostic = unsupported },
        .{ .name = "nested-scope-rebind", .body = "let mut p=&a\n{let mut inner:u8=0\np=&inner}\np.* = 1", .diagnostic = unsupported },
        .{ .name = "implicit-read", .body = "let p=&a\n@println(p)", .diagnostic = unsupported },
        .{ .name = "implicit-index", .body = "let values:[]u8=[1]\nlet p=&values\n@println(p[0])", .diagnostic = unsupported },
        .{ .name = "readonly-field-write", .body = "let value=Cell.[value=1]\nlet p=&value\np.value=2", .diagnostic = immutable },
        .{ .name = "implicit-call", .body = "let f=()->u8 {1}\nlet p=&f\n@println(p())", .diagnostic = unsupported },
        .{ .name = "copy-reference", .body = "let p=&a\nlet q=p", .diagnostic = unsupported },
        .{ .name = "pass-reference-to-value", .body = "let p=&a\nconsume(p)", .diagnostic = unsupported },
        .{ .name = "return-reference", .body = "let p=&a\nreturn p", .diagnostic = unsupported },
        .{ .name = "store-reference", .body = "let p=&a\nlet values=[p]", .diagnostic = unsupported },
        .{ .name = "capture-reference", .body = "let p=&a\nlet f=(){@println(p.*)}", .diagnostic = unsupported },
        .{ .name = "reborrow", .body = "let p=&a\nlet q=&p.*", .diagnostic = unsupported },
        .{ .name = "reference-to-reference", .body = "let p=&a\nlet q=&p", .diagnostic = unsupported },
        .{ .name = "typeof-dereference-mismatch", .body = "let p=&a\nlet x:@TypeOf(p.*)=true", .diagnostic = "literal does not match the annotated type" },
        .{ .name = "rebound-owner-conflict", .body = "let mut p=&a\np=&b\nb=2\np.* = 1", .diagnostic = "owner access or aliased reference arguments conflict with a live borrow" },
    }) |case| {
        const source = fixtures.add(b.fmt("reference-{s}.nov", .{case.name}), b.fmt("let Cell=struct{{value:u8,}}\nlet consume:(v:u8)={{}}\nlet verify:(flag:bool)={{let mut a:u8=0\nlet mut b:u8=0\n{s}\n}}\nlet main:()=verify(true)\n", .{case.body}));
        for ([_]bool{ false, true }) |emit| {
            const check = b.addRunArtifact(exe);
            check.step.name = b.fmt("CLI references reject: {s} ({s})", .{ case.name, if (emit) "emit-c" else "semantic" });
            check.addArgs(if (emit) &.{"emit-c"} else &.{ "check", "--semantic" });
            check.addFileArg(source);
            check.expectExitCode(1);
            check.expectStdOutEqual("");
            check.expectStdErrMatch(b.fmt("error: {s}", .{case.diagnostic}));
            test_step.dependOn(&check.step);
        }
    }
}

fn addReferenceParameterCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    for ([_][]const u8{ "fmt", "semantic", "", "--debug", "--release" }) |mode| {
        const format = std.mem.eql(u8, mode, "fmt");
        const semantic = std.mem.eql(u8, mode, "semantic");
        for ([_][]const u8{ "run", "test" }) |command| {
            if ((format or semantic) and std.mem.eql(u8, command, "test")) continue;
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI reference parameters: {s} ({s})", .{ command, mode });
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            if (format) {
                run.addArgs(&.{ "fmt", "--check" });
            } else if (semantic) {
                run.addArgs(&.{ "check", "--semantic" });
            } else {
                run.addArg(command);
            }
            run.addFileArg(b.path("examples/reference-parameters.nov"));
            if (!format and !semantic and mode.len != 0) run.addArg(mode);

            run.expectExitCode(0);
            run.expectStdOutEqual(if (format or semantic) "" else if (std.mem.eql(u8, command, "run"))
                "Swapped: 7 5 Sum: 12\nWords: [hello!, world]\n"
            else
                "PASS mutation_readonly_and_direct_borrows\nPASS generic_swap_and_methods\nPASS callbacks_closures_forwarding_recursion_and_pipes\nPASS managed_arrays_records_and_strings\nPASS argument_order_snapshots_actual_target\n5 passed; 0 failed; 5 total\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    const fixtures = b.addWriteFiles();
    const formatted = fixtures.add("reference-parameter-binding-call.nov",
        \\let increment: (p: &mut int) = {
        \\    p.* += 1
        \\}
        \\let main: () = {
        \\    let mut value: int = 1
        \\    let p = &value
        \\    increment(p)
        \\    @assert(value == 2)
        \\}
        \\
    );
    for ([_][]const u8{ "fmt", "semantic" }) |mode| {
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI reference parameters: binding call {s}", .{mode});
        check.addArgs(if (std.mem.eql(u8, mode, "fmt")) &.{ "fmt", "--check" } else &.{ "check", "--semantic" });
        check.addFileArg(formatted);
        check.expectExitCode(0);
        check.expectStdOutEqual("");
        check.expectStdErrEqual("");
        test_step.dependOn(&check.step);
    }
    const conflict = "owner access or aliased reference arguments conflict with a live borrow";
    const unsupported = "local references require";
    const escape = "references cannot escape";
    const mismatch = "expression does not match the expected type";
    const prefix = "let write:(p:&mut int,n:int)={p.* += n}\nlet pair:(p:&int,q:&int)->int=p.*+q.*\n";
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "alias-binding", .source = "let main:()={let mut a:int=1\nlet p=&a\nlet result=pair(p,p)\n}", .diagnostic = conflict },
        .{ .name = "alias-direct", .source = "let main:()={let a:int=1\nlet result=pair(&a,&a)\n}", .diagnostic = conflict },
        .{ .name = "alias-forwarding", .source = "let bad:(p:&int)->int=pair(p,p)\nlet main:()={}", .diagnostic = conflict },
        .{ .name = "alias-possible-owner", .source = "let bad:(flag:bool)={let a:int=1\nlet b:int=2\nlet mut p=&a\nif flag {p=&b}\nlet result=pair(p,&b)\n}\nlet main:()=bad(true)", .diagnostic = conflict },
        .{ .name = "alias-array-elements", .source = "let main:()={let a:[]int=[1,2]\nlet result=pair(&a[0],&a[1])\n}", .diagnostic = conflict },
        .{ .name = "mutable-alias", .source = "let two:(p:&mut int,q:&mut int)={p.* += q.*}\nlet main:()={let mut a:int=1\nlet p=&a\ntwo(p,p)}", .diagnostic = conflict },
        .{ .name = "indirect-alias", .source = "let main:()={let a:int=1\nlet f:(&int,&int)->int=pair\nlet result=f(&a,&a)\n}", .diagnostic = conflict },
        .{ .name = "boxed-captured-owner", .source = "let main:()={let mut a:int=1\nlet get=()->int {a}\nwrite(&a,1)\n@println(get())}", .diagnostic = conflict },
        .{ .name = "late-argument-mutation", .source = "let main:()={let mut a:int=1\nwrite(&a,{a=3\n2})}", .diagnostic = conflict },
        .{ .name = "late-after-rebind", .source = "let main:()={let mut a:int=1\nlet mut b:int=2\nlet mut p=&a\nwrite(p,{p=&b\na=3\n2})}", .diagnostic = conflict },
        .{ .name = "readonly-to-mutable", .source = "let main:()={let mut a:int=1\nlet p:&int=&a\nwrite(p,1)}", .diagnostic = mismatch },
        .{ .name = "different-pointee", .source = "let main:()={let mut a:u8=1\nwrite(&a,1)}", .diagnostic = mismatch },
        .{ .name = "signature-invariant", .source = "let main:()={let f:(&int,int)=write\n}", .diagnostic = mismatch },
        .{ .name = "parameter-rebind", .source = "let bad:(p:&mut int,q:&mut int)={p=q}\nlet main:()={}", .diagnostic = "assignment requires a mutable local" },
        .{ .name = "parameter-copy", .source = "let bad:(p:&int)={let q=p\n}\nlet main:()={}", .diagnostic = unsupported },
        .{ .name = "parameter-capture", .source = "let bad:(p:&int)={let f=()->int {p.*}\n}\nlet main:()={}", .diagnostic = unsupported },
        .{ .name = "parameter-return", .source = "let bad:(p:&int)->&int=p\nlet main:()={}", .diagnostic = escape },
        .{ .name = "parameter-array-storage", .source = "let bad:(p:&int)={let xs=[p]\n}\nlet main:()={}", .diagnostic = unsupported },
        .{ .name = "reference-field", .source = "let Bad=struct{p:&int,}\nlet main:()={}", .diagnostic = "this type is not supported in semantic analysis yet" },
        .{ .name = "parameter-reborrow", .source = "let bad:(p:&int)={let q=&p.*\n}\nlet main:()={}", .diagnostic = unsupported },
        .{ .name = "export", .source = "@[export]\nlet bad:(p:&int)={}\nlet main:()={}", .diagnostic = "export ABI supports only" },
        .{ .name = "extern", .source = "@[extern]\nlet bad:(p:&int)\nlet main:()={}", .diagnostic = "extern ABI supports only" },
    }) |case| {
        const source = fixtures.add(b.fmt("reference-parameter-{s}.nov", .{case.name}), b.fmt("{s}\n{s}\n", .{ prefix, case.source }));
        for ([_]bool{ false, true }) |emit| {
            const check = b.addRunArtifact(exe);
            check.step.name = b.fmt("CLI reference parameters reject: {s} ({s})", .{ case.name, if (emit) "emit-c" else "semantic" });
            check.addArgs(if (emit) &.{"emit-c"} else &.{ "check", "--semantic" });
            check.addFileArg(source);
            check.expectExitCode(1);
            check.expectStdOutEqual("");
            check.expectStdErrMatch(b.fmt("error: {s}", .{case.diagnostic}));
            test_step.dependOn(&check.step);
        }
    }
}

fn addReferenceRebindingCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    for ([_][]const u8{ "fmt", "semantic", "", "--debug", "--release" }) |mode| {
        const format = std.mem.eql(u8, mode, "fmt");
        const semantic = std.mem.eql(u8, mode, "semantic");
        for ([_][]const u8{ "run", "test" }) |command| {
            if ((format or semantic) and std.mem.eql(u8, command, "test")) continue;
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI reference rebinding: {s} ({s})", .{ command, mode });
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            if (format) {
                run.addArgs(&.{ "fmt", "--check" });
            } else if (semantic) {
                run.addArgs(&.{ "check", "--semantic" });
            } else {
                run.addArg(command);
            }
            run.addFileArg(b.path("examples/reference-rebinding.nov"));
            if (!format and !semantic and mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (format or semantic) "" else if (std.mem.eql(u8, command, "run"))
                "Chosen: 3 12\n"
            else
                "PASS branches_match_and_block\nPASS short_circuit_selection\nPASS zero_one_many_iterations\nPASS condition_continuation_and_exhaustion\nPASS break_and_continue_paths\nPASS readonly_targets\nPASS array_elements_different_owners\nPASS old_target_before_rhs_rebind\n8 passed; 0 failed; 8 total\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    const fixtures = b.addWriteFiles();
    const unsupported = "local references require";
    const conflict = "owner access or aliased reference arguments conflict with a live borrow";
    const mismatch = "expression does not match the expected type";
    for ([_]struct { name: []const u8, body: []const u8, diagnostic: []const u8 }{
        .{ .name = "unused-inner-owner", .body = "let mut p=&a\n{let mut inner:int=0\np=&inner}", .diagnostic = unsupported },
        .{ .name = "inner-array-owner", .body = "let mut xs:[]int=[1]\nlet mut p=&xs[0]\nif flag {let mut ys:[]int=[2]\np=&ys[0]}", .diagnostic = unsupported },
        .{ .name = "match-inner-owner", .body = "let mut p=&a\nmatch flag {true=>{let mut inner:int=0\np=&inner}\nfalse=>{p=&b}}", .diagnostic = unsupported },
        .{ .name = "possible-initial-owner", .body = "let mut p=&a\nif flag {p=&b}\na=3\np.* += 1", .diagnostic = conflict },
        .{ .name = "possible-new-owner", .body = "let mut p=&a\nif flag {p=&b}\nb=3\np.* += 1", .diagnostic = conflict },
        .{ .name = "zero-loop-initial-owner", .body = "let mut p=&a\nloop _ in 0..count {p=&b}\na=3\np.* += 1", .diagnostic = conflict },
        .{ .name = "short-circuit-possible-owner", .body = "let mut p=&a\nlet selected=flag and {p=&b\ntrue}\nb=3\np.* += 1", .diagnostic = conflict },
        .{ .name = "second-live-borrow", .body = "let q=&b\nlet mut p=&a\nif flag {p=&b}\np.* += 1\n@println(q.*)", .diagnostic = conflict },
        .{ .name = "compound-old-owner-access", .body = "let mut p=&a\np.* += {p=&b\na=3\n1}", .diagnostic = conflict },
        .{ .name = "mutable-to-readonly-owner", .body = "let fixed:int=0\nlet mut p=&a\nif flag {p=&fixed}", .diagnostic = mismatch },
        .{ .name = "different-child-type", .body = "let mut other:u8=0\nlet mut p=&a\nif flag {p=&other}", .diagnostic = mismatch },
        .{ .name = "readonly-stays-readonly", .body = "let mut p:&int=&a\nif flag {p=&b}\np.* = 3", .diagnostic = "assignment requires a mutable local" },
        .{ .name = "immutable-binding", .body = "let p=&a\nif flag {p=&b}", .diagnostic = "assignment requires a mutable local" },
        .{ .name = "exclusive-array-elements", .body = "let mut xs:[]int=[1,2]\nlet mut p=&xs[0]\np.* += {p=&xs[1]\n1}", .diagnostic = conflict },
        .{ .name = "conditional-exclusive-array", .body = "let mut xs:[]int=[1,2]\nlet mut ys:[]int=[3,4]\nlet mut p=&xs[0]\nif flag {p=&ys[0]}\nlet q=&xs[1]\np.* += 1\n@println(q.*)", .diagnostic = conflict },
    }) |case| {
        const source = fixtures.add(b.fmt("reference-rebinding-{s}.nov", .{case.name}), b.fmt("let verify:(flag:bool,count:int)={{let mut a:int=0\nlet mut b:int=0\n{s}\n}}\nlet main:()=verify(true,1)\n", .{case.body}));
        for ([_]bool{ false, true }) |emit| {
            const check = b.addRunArtifact(exe);
            check.step.name = b.fmt("CLI reference rebinding reject: {s} ({s})", .{ case.name, if (emit) "emit-c" else "semantic" });
            check.addArgs(if (emit) &.{"emit-c"} else &.{ "check", "--semantic" });
            check.addFileArg(source);
            check.expectExitCode(1);
            check.expectStdOutEqual("");
            check.expectStdErrMatch(b.fmt("error: {s}", .{case.diagnostic}));
            test_step.dependOn(&check.step);
        }
    }
}

fn addBorrowCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step, fixtures: *std.Build.Step.WriteFile) void {
    _ = fixtures.add("borrowed-elements.nov", @embedFile("examples/managed-arrays.nov"));
    _ = fixtures.add("borrowed-integer.nov",
        \\let main: () = {
        \\    let mut x: uint = 1
        \\    let earlierx = x
        \\    let y = &x
        \\    y.* += 1
        \\    @println(x, earlierx)
        \\}
        \\
    );
    _ = fixtures.add("local-borrows-example.nov", @embedFile("examples/local-borrows.nov"));
    const programs = [_]struct { source: []const u8, output: []const u8 }{
        .{ .source = "borrowed-integer.nov", .output = "2 1\n" },
        .{ .source = "local-borrows-example.nov", .output = "2 1\ntrue\nhello world hello world\ntrue\n5\n3 2 1\n1 2 \n9\n7\n" },
    };
    for (programs) |program| for ([_][]const u8{ "semantic", "fmt", "default", "--debug", "--release" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI local borrowing: {s} {s}", .{ program.source, mode });
        run.setCwd(fixtures.getDirectory());
        const semantic = std.mem.eql(u8, mode, "semantic");
        const fmt = std.mem.eql(u8, mode, "fmt");
        if (semantic) {
            run.addArgs(&.{ "check", "--semantic", program.source });
        } else if (fmt) {
            run.addArgs(&.{ "fmt", "--check", program.source });
        } else {
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ "run", program.source });
            if (!std.mem.eql(u8, mode, "default")) run.addArg(mode);
        }
        run.expectExitCode(0);
        run.expectStdOutEqual(if (semantic or fmt) "" else program.output);
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    };
    _ = fixtures.add("borrow-conflict.nov", "let R = struct { n: uint = 0, }\nlet main: () = {\nlet mut items = [R.[]]\nlet item = &items[0]\nitems += [R.[]]\nitem.*.n += 1\n}\n");
    for ([_][]const u8{ "semantic", "fmt", "default", "--debug", "--release" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI static borrowing: {s}", .{mode});
        run.setCwd(fixtures.getDirectory());
        const semantic = std.mem.eql(u8, mode, "semantic");
        const fmt = std.mem.eql(u8, mode, "fmt");
        if (semantic) {
            run.addArgs(&.{ "check", "--semantic", "borrowed-elements.nov" });
        } else if (fmt) {
            run.addArgs(&.{ "fmt", "--check", "borrowed-elements.nov" });
        } else {
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ "run", "borrowed-elements.nov" });
            if (!std.mem.eql(u8, mode, "default")) run.addArg(mode);
        }
        run.expectExitCode(0);
        run.expectStdOutEqual(if (semantic or fmt) "" else "Linus Edsger true false\nAda Lovelace 2\nAda 1\n");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    const reject = b.addRunArtifact(exe);
    reject.step.name = "CLI static borrowing: conflicting append";
    reject.setCwd(fixtures.getDirectory());
    reject.addArgs(&.{ "check", "--semantic", "borrow-conflict.nov" });
    reject.expectExitCode(1);
    reject.expectStdOutEqual("");
    reject.expectStdErrMatch("owner access or aliased reference arguments conflict with a live borrow");
    test_step.dependOn(&reject.step);
}

fn addIterationCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("iteration.nov", @embedFile("examples/iteration.nov"));
    for ([_][]const u8{ "fmt", "semantic" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI iteration: {s}", .{mode});
        run.setCwd(fixtures.getDirectory());
        run.addArgs(if (std.mem.eql(u8, mode, "fmt")) &.{ "fmt", "--check", "iteration.nov" } else &.{ "check", "--semantic", "iteration.nov" });
        run.expectExitCode(0);
        run.expectStdOutEqual("");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test" }) |command| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI iteration: {s} ({s})", .{ command, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ command, "iteration.nov" });
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (std.mem.eql(u8, command, "run"))
                "3\n5\n7\nOriginal: 2 4 6\naé😀\nFound: 5\n"
            else
                "PASS copied_elements\nPASS writable_references\nPASS readonly_references\nPASS unicode_and_once\nPASS exhaustion_and_breaks\n5 passed; 0 failed; 5 total\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    const conflict = "owner access or aliased reference arguments conflict with a live borrow";
    const immutable = "assignment requires a mutable local";
    const unsupported = "reference iteration requires a local array owner";
    const reborrow = "local references require";
    for ([_]struct { name: []const u8, body: []const u8, diagnostic: []const u8 }{
        .{ .name = "copy-assignment", .body = "let a:[]int=[1]\nloop x in a {x=2}", .diagnostic = immutable },
        .{ .name = "copy-field", .body = "let a=[R.[n=1]]\nloop x in a {x.n+=1}", .diagnostic = immutable },
        .{ .name = "source-element", .body = "let mut a:[]int=[1]\nloop x in a {a[0]=2}", .diagnostic = conflict },
        .{ .name = "source-compound", .body = "let mut a:[]int=[1]\nloop x in a {a[0]+=2}", .diagnostic = conflict },
        .{ .name = "source-replacement", .body = "let mut a:[]int=[1]\nloop x in a {a=[2]}", .diagnostic = conflict },
        .{ .name = "source-growth", .body = "let mut a:[]int=[1]\nloop x in a {a+=[2]}", .diagnostic = conflict },
        .{ .name = "new-loan", .body = "let mut a:[]int=[1]\nloop x in a {let r=&a[0]\nr.* = 2}", .diagnostic = conflict },
        .{ .name = "nested-exclusive", .body = "let mut a:[]int=[1]\nloop x in a {loop &y in a {}}", .diagnostic = conflict },
        .{ .name = "prior-loan", .body = "let mut a:[]int=[1]\nlet r=&a\nloop &x in a {@println(r.*[0])}", .diagnostic = conflict },
        .{ .name = "loan-source-read", .body = "let mut a:[]int=[1]\nloop &x in a {@println(a[0])}", .diagnostic = conflict },
        .{ .name = "loan-source-growth", .body = "let mut a:[]int=[1]\nloop &x in a {a+=[2]}", .diagnostic = conflict },
        .{ .name = "loan-new-loan", .body = "let mut a:[]int=[1]\nloop &x in a {let r=&a[0]\n}", .diagnostic = conflict },
        .{ .name = "reference-string", .body = "let a=\"é😀\"\nloop &x in a {}", .diagnostic = unsupported },
        .{ .name = "reference-range", .body = "loop &x in 0..2 {}", .diagnostic = unsupported },
        .{ .name = "reference-temporary", .body = "loop &x in [true] {}", .diagnostic = unsupported },
        .{ .name = "reference-reborrow", .body = "let a:[]int=[1]\nlet r=&a\nloop &x in r {}", .diagnostic = unsupported },
        .{ .name = "readonly-assignment", .body = "let a:[]int=[1]\nloop &x in a {x.* = 2}", .diagnostic = immutable },
        .{ .name = "readonly-compound", .body = "let a:[]int=[1]\nloop &x in a {x.* += 2}", .diagnostic = immutable },
        .{ .name = "readonly-field", .body = "let a=[R.[n=1]]\nloop &x in a {x.*.n += 1}", .diagnostic = immutable },
        .{ .name = "escape-return", .body = "let mut a:[]int=[1]\nloop &x in a {return &x.*}", .diagnostic = reborrow },
        .{ .name = "escape-array", .body = "let mut a:[]int=[1]\nloop &x in a {let escaped=[&x.*]\n}", .diagnostic = reborrow },
        .{ .name = "escape-binding", .body = "let mut a:[]int=[1]\nloop &x in a {let r=&x.*\n}", .diagnostic = reborrow },
        .{ .name = "escape-scope", .body = "let a:[]int=[1]\nloop &x in a {}\n@println(x)", .diagnostic = "unknown name" },
    }) |case| {
        const source = b.fmt("invalid-{s}.nov", .{case.name});
        _ = fixtures.add(source, b.fmt("let R=struct{{n:int,}}\nlet main:()={{\n{s}\n}}\n", .{case.body}));
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI iteration: reject {s}", .{case.name});
        run.setCwd(fixtures.getDirectory());
        run.addArgs(&.{ "check", "--semantic", source });
        run.expectExitCode(1);
        run.expectStdOutEqual("");
        run.expectStdErrMatch(b.fmt("{s}:", .{source}));
        run.expectStdErrMatch("error:");
        run.expectStdErrMatch(case.diagnostic);
        if (std.mem.eql(u8, case.diagnostic, conflict)) run.expectStdErrMatch("borrow originates here");
        test_step.dependOn(&run.step);
    }
}

fn addIterationContinuationCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const source = "iteration-continuations.nov";
    _ = fixtures.add(source,
        \\let R = union { ok: int, err: str, }
        \\let fail: () -> R = .err["continuation error"]
        \\let propagate: () -> R = {
        \\    loop x in [1] : { let n = fail().!
        \\        @println("BAD propagation", n)
        \\    } {}
        \\    .ok[0]
        \\}
        \\let early: () -> int = {
        \\    loop x in [1, 2] : @println("BAD return continuation") { return x }
        \\    0
        \\}
        \\let tailReturn: () -> int = loop x in [9] : { return x } {} else { @panic("BAD return else") }
        \\let main: () = {
        \\    let mut index: int = 0
        \\    loop item in [10, 20, 30] : index += 1 {
        \\        @println(index, item)
        \\        if item == 20 { continue }
        \\    } else { @println("array", index) }
        \\    loop r in "Aé😀\0Z" : @print(r) { continue }
        \\    @println("")
        \\    loop n in 2..=4 : @print(n) {} else { @println(" range") }
        \\    loop x, r, i in [7, 8], "é😀", 9.. : @println(x, r, i) { continue }
        \\    let mut a: []int = [1, 2]
        \\    loop &x in a : x.* += 10 { x.* *= 2 }
        \\    @println(a[0], a[1])
        \\    loop &x, i in a, @as(int, 0).. : {
        \\        x.* += i
        \\        continue
        \\    } { x.* += 1 }
        \\    @println(a[0], a[1])
        \\    let mut visits: int = 0
        \\    loop x in [1, 2, 3] : {
        \\        visits += 1
        \\        @print(x)
        \\        continue
        \\    } {
        \\        if visits > 3 { @panic("continuation repeated") }
        \\        continue
        \\    } else { @println(" continued", visits) }
        \\    loop x in [1, 2] : @println("BAD break continuation") { break } else { @println("BAD break else") }
        \\    @println("return", early(), tailReturn())
        \\    let picked: int = loop x in [4, 5] : { break x } {} else { 99 }
        \\    @println("picked", picked)
        \\    let terminal: int = loop x in [1] : { break 7 } {} else { @panic("not called") }
        \\    @println("terminal", terminal)
        \\    match propagate() { .ok => @panic("BAD success")
        \\        .err => |e| @println(e)
        \\    }
        \\    let empty: []int = []
        \\    loop x in empty : @println("BAD empty continuation") {} else { @println("empty") }
        \\    let mut last: int = 0
        \\    loop x in @as(u8, 255)..=255 : {
        \\        @println("max", x)
        \\        last += 1
        \\    } { continue }
        \\    loop x in @as(u64, 18446744073709551615)..=18446744073709551615 : {
        \\        @println("wide max", x)
        \\        continue
        \\    } { continue }
        \\    loop x, i in [6], @as(u64, 18446744073709551615).. : {
        \\        @println("open", x, i)
        \\        last += 1
        \\        continue
        \\    } { continue }
        \\    loop i, x in @as(i8, 127).., [8] : @println("signed", i, x) { continue }
        \\    @println("last", last)
        \\}
        \\@[test]
        \\let iteration_continuations: () = main()
        \\
    );
    const output = "0 10\n1 20\n2 30\narray 3\nAé😀\x00Z\n234 range\n7 é 9\n8 😀 10\n12 14\n13 16\n123 continued 3\nreturn 1 9\npicked 4\nterminal 7\ncontinuation error\nempty\nmax 255\nwide max 18446744073709551615\nopen 6 18446744073709551615\nsigned 127 8\nlast 2\n";
    const check = b.addRunArtifact(exe);
    check.setCwd(fixtures.getDirectory());
    check.addArgs(&.{ "check", "--semantic", source });
    check.expectExitCode(0);
    check.expectStdOutEqual("");
    check.expectStdErrEqual("");
    test_step.dependOn(&check.step);
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test" }) |command| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI iteration continuations: {s} {s}", .{ command, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ command, source });
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (std.mem.eql(u8, command, "run")) output else output ++ "PASS iteration_continuations\n1 passed; 0 failed; 1 total\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    _ = fixtures.add("continuation-overflow-exits.nov",
        \\let Option: (T: type) -> type = union { some: T, none, }
        \\let stop: () -> Option(int) = .none[]
        \\let returning: () -> int = {
        \\    loop x, i in [1, 2], @as(i8, 127).. : { return 9 } {}
        \\    0
        \\}
        \\let propagating: () -> Option(int) = {
        \\    loop x, i in [1, 2], @as(i8, 127).. : {
        \\        let n = stop().!
        \\        @println("BAD some", n)
        \\    } {}
        \\    .some[0]
        \\}
        \\let main: () = {
        \\    loop x, i in [1, 2], @as(i8, 127).. : { break } {} else { @panic("BAD break else") }
        \\    @println("done")
        \\    @println("return", returning())
        \\    match propagating() { .some => @panic("BAD some")
        \\        .none => @println("none")
        \\    }
        \\    loop x, i in [1, 2], @as(i8, 126).. : {
        \\        @println("final", x, i)
        \\        continue
        \\    } {} else { @println("exhausted") }
        \\}
        \\
    );
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI iteration continuations: overflow exits {s}", .{mode});
        run.setCwd(fixtures.getDirectory());
        run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        run.setEnvironmentVariable("NOV_CC", "");
        run.setEnvironmentVariable("CC", "");
        run.addArgs(&.{ "run", "continuation-overflow-exits.nov" });
        if (mode.len != 0) run.addArg(mode);
        run.expectExitCode(0);
        run.expectStdOutEqual("done\nreturn 9\nnone\nfinal 1 126\nfinal 2 127\nexhausted\n");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    for ([_]bool{ false, true }) |open_first| {
        for ([_]bool{ false, true }) |continues| {
            const invalid = b.fmt("continuation-overflow-{s}-{s}.nov", .{ if (open_first) "first" else "last", if (continues) "continue" else "fallthrough" });
            _ = fixtures.add(invalid, b.fmt("let main:()={{\nloop {s} : {{\n@println(\"cont\", x, i)\n{s}\n}} {{@println(\"body\", x, i)}} else {{@println(\"BAD else\")}}\n}}\n", .{
                if (open_first) "i,x in @as(i8,127)..,[1,2]" else "x,i in [1,2],@as(i8,127)..",
                if (continues) "continue" else "",
            }));
            for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
                const run = b.addRunArtifact(exe);
                run.step.name = b.fmt("CLI iteration continuations: delayed overflow {s} {s}", .{ invalid, mode });
                run.setCwd(fixtures.getDirectory());
                run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                run.setEnvironmentVariable("NOV_CC", "");
                run.setEnvironmentVariable("CC", "");
                run.addArgs(&.{ "run", invalid });
                if (mode.len != 0) run.addArg(mode);
                run.expectExitCode(1);
                run.expectStdOutEqual("body 1 127\ncont 1 127\n");
                run.expectStdErrMatch(b.fmt("{s}:2:1: assertion failed: open range iteration overflow\n", .{invalid}));
                test_step.dependOn(&run.step);
            }
        }
    }
    const conflict = "owner access or aliased reference arguments conflict with a live borrow";
    for ([_]struct { name: []const u8, body: []const u8, diagnostic: []const u8 }{
        .{ .name = "nonvoid", .body = "loop x in [1] : x {}", .diagnostic = "expression does not match the expected type" },
        .{ .name = "readonly", .body = "loop x in [1] : x = 2 {}", .diagnostic = "assignment requires a mutable local" },
        .{ .name = "body-local", .body = "loop x in [1] : @println(hidden) {let hidden=1\n}", .diagnostic = "unknown name" },
        .{ .name = "binding-in-else", .body = "loop x in [1] : @println(x) {} else {@println(x)}", .diagnostic = "unknown name" },
        .{ .name = "owner-mutation", .body = "let mut a=[1]\nloop x in a : a[0]=2 {}", .diagnostic = conflict },
        .{ .name = "owner-growth", .body = "let mut a=[1]\nloop x in a : a+=[2] {}", .diagnostic = conflict },
        .{ .name = "ref-conflict", .body = "let mut a=[1]\nloop &x in a : @println(a[0]) {}", .diagnostic = conflict },
        .{ .name = "new-loan", .body = "let mut a=[1]\nloop &x in a : {let r=&a[0]\nr.*=2} {}", .diagnostic = conflict },
        .{ .name = "readonly-ref", .body = "let a=[1]\nloop &x in a : x.*+=1 {}", .diagnostic = "assignment requires a mutable local" },
        .{ .name = "propagation-error", .body = "let f:(r:R)->S={loop x in [1] : {let n=r.!\n} {}\n.ok[0]}\n", .diagnostic = ".! error payload must exactly match" },
    }) |case| {
        const invalid = b.fmt("invalid-continuation-{s}.nov", .{case.name});
        _ = fixtures.add(invalid, b.fmt("let R=union{{ok:int,err:str,}}\nlet S=union{{ok:int,err:bool,}}\nlet main:()={{\n{s}\n}}\n", .{case.body}));
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI iteration continuations: reject {s}", .{case.name});
        run.setCwd(fixtures.getDirectory());
        run.addArgs(&.{ "check", "--semantic", invalid });
        run.expectExitCode(1);
        run.expectStdOutEqual("");
        run.expectStdErrMatch("error:");
        run.expectStdErrMatch(case.diagnostic);
        if (std.mem.eql(u8, case.diagnostic, conflict)) run.expectStdErrMatch("borrow originates here");
        test_step.dependOn(&run.step);
    }
}

fn addZipIterationCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    _ = fixtures.add("zip-iteration.nov", @embedFile("examples/zip-iteration.nov"));
    const output = "0 10\n1 20\n2 30\né a\na 😀\n";
    for ([_][]const u8{ "fmt", "semantic" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI zip iteration: {s}", .{mode});
        run.setCwd(fixtures.getDirectory());
        run.addArgs(if (std.mem.eql(u8, mode, "fmt")) &.{ "fmt", "--check", "zip-iteration.nov" } else &.{ "check", "--semantic", "zip-iteration.nov" });
        run.expectExitCode(0);
        run.expectStdOutEqual("");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
        for ([_][]const u8{ "run", "test" }) |command| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI zip iteration: {s} ({s})", .{ command, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ command, "zip-iteration.nov" });
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(0);
            run.expectStdOutEqual(if (std.mem.eql(u8, command, "run")) output else "PASS mixed_and_unicode\nPASS source_order\nPASS range_cardinalities\nPASS open_indices\nPASS reference_pairs\nPASS control_and_captures\n6 passed; 0 failed; 6 total\n");
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
    }
    // Reach the test bodies in ordinary emitted C as well as through `nov test`.
    const unannotated = std.mem.replaceOwned(u8, b.allocator, @embedFile("examples/zip-iteration.nov"), "@[test]\n", "") catch @panic("OOM");
    const demo = std.mem.replaceOwned(u8, b.allocator, unannotated, "let main:", "let demo:") catch @panic("OOM");
    _ = fixtures.add("zip-strict.nov", b.fmt("{s}\nlet main:()={{demo()\nmixed_and_unicode()\nsource_order()\nrange_cardinalities()\nopen_indices()\nreference_pairs()\ncontrol_and_captures()}}\n", .{demo}));
    const emit = b.addRunArtifact(exe);
    emit.step.name = "CLI zip iteration: emit C";
    emit.setCwd(fixtures.getDirectory());
    emit.addArgs(&.{ "emit-c", "zip-strict.nov" });
    emit.expectExitCode(0);
    emit.expectStdErrEqual("");
    const generated = emit.captureStdOut(.{});
    for ([_][]const u8{ "-O0", "-O3" }) |mode| {
        const compile = b.addSystemCommand(&.{ b.graph.zig_exe, "cc", "-x", "c", "-std=c11", mode, "-fsanitize=undefined", "-fno-sanitize-recover=all", "-Wall", "-Wextra", "-Wpedantic", "-Wunused-function", "-Wunused-variable", "-Wunused-but-set-variable", "-Werror" });
        compile.step.name = b.fmt("CLI zip iteration: strict C ({s})", .{mode});
        compile.addFileArg(generated);
        compile.addArg("-o");
        const binary = compile.addOutputFileArg(if (exe.root_module.resolved_target.?.result.os.tag == .windows) "zip.exe" else "zip");
        const run = std.Build.Step.Run.create(b, b.fmt("CLI zip iteration: strict C execution ({s})", .{mode}));
        run.addFileArg(binary);
        run.expectExitCode(0);
        run.expectStdOutEqual(output);
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    // Exact output proves preflight precedes both the body and exhaustion branch.
    for ([_]struct { name: []const u8, body: []const u8, stdout: []const u8 = "", panic: []const u8 = "zip iteration length mismatch" }{
        .{ .name = "arrays", .body = "loop a,b in [1],[2,3] {@println(\"BODY\")} else {@println(\"ELSE\")}" },
        .{ .name = "break", .body = "loop a,b in [1,2],[3] {@println(\"BODY\")\nbreak}" },
        .{ .name = "empty-first", .body = "let empty:[]int=[]\nloop a,b in empty,[1] {@println(\"BODY\")} else {@println(\"ELSE\")}" },
        .{ .name = "empty-last", .body = "loop a,b in [1],\"\" {break} else {@println(\"ELSE\")}" },
        .{ .name = "same-bytes-different-scalars", .body = "loop a,b in \"é\",\"ab\" {break}" },
        .{ .name = "scalar-not-byte-length", .body = "loop a,b in \"😀\",[1,2,3,4] {break}" },
        .{ .name = "later-source", .body = "loop a,b,c,i in [1],[2],[3,4],0.. {break}" },
        .{ .name = "effects-before-preflight", .body = "loop a,b,c in {@println(\"first\")\n[1]}, {@println(\"second\")\n[2,3]}, {@println(\"third\")\n0..1} {@println(\"BODY\")\nbreak}", .stdout = "first\nsecond\nthird\n" },
        .{ .name = "exclusive-range", .body = "loop a,b in 0..2,4..=6 {break}" },
        .{ .name = "reversed-range", .body = "loop a,b in 5..=3,0..=0 {break}" },
        .{ .name = "full-u64", .body = "loop a,b in @as(u64,0)..=18446744073709551615,0..0 {break}" },
        .{ .name = "full-i64", .body = "let minimum:i64=-9223372036854775807-1\nloop a,b in minimum..=9223372036854775807,\"\" {break}" },
        .{ .name = "full-versus-exclusive", .body = "loop a,b in @as(u64,0)..=18446744073709551615,@as(u64,0)..18446744073709551615 {break}" },
        .{ .name = "signed-exclusive-extremes", .body = "let minimum:i64=-9223372036854775807-1\nloop a,b in minimum..9223372036854775807,@as(u64,0)..=18446744073709551615 {break}" },
        .{ .name = "open-uint-overflow", .body = "loop a,i in [1,2],18446744073709551615.. {@println(a)\ncontinue}", .stdout = "1\n", .panic = "open range iteration overflow" },
        .{ .name = "open-signed-overflow", .body = "loop i,a in @as(i8,127)..,[1,2] {@println(a)\ncontinue}", .stdout = "1\n", .panic = "open range iteration overflow" },
    }) |case| {
        const source = b.fmt("runtime-{s}.nov", .{case.name});
        _ = fixtures.add(source, b.fmt("let main:()={{\n{s}\n}}\n", .{case.body}));
        for ([_][]const u8{ "", "--debug", "--release" }) |mode| {
            const run = b.addRunArtifact(exe);
            run.step.name = b.fmt("CLI zip iteration: panic {s} ({s})", .{ case.name, mode });
            run.setCwd(fixtures.getDirectory());
            run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
            run.setEnvironmentVariable("NOV_CC", "");
            run.setEnvironmentVariable("CC", "");
            run.addArgs(&.{ "run", source });
            if (mode.len != 0) run.addArg(mode);
            run.expectExitCode(1);
            run.expectStdOutEqual(case.stdout);
            run.expectStdErrMatch(b.fmt("assertion failed: {s}\n", .{case.panic}));
            test_step.dependOn(&run.step);
        }
    }
    const conflict = "owner access or aliased reference arguments conflict with a live borrow";
    for ([_]struct { name: []const u8, body: []const u8, diagnostic: []const u8 }{
        .{ .name = "few-sources", .body = "loop a,b in [1] {}", .diagnostic = "equal binding/source counts" },
        .{ .name = "few-bindings", .body = "loop a in [1],[2] {}", .diagnostic = "equal binding/source counts" },
        .{ .name = "all-open", .body = "loop a,b in 0..,-1.. {}", .diagnostic = "at least one bounded source" },
        .{ .name = "single-open", .body = "loop a in 0.. {}", .diagnostic = "at least one bounded source" },
        .{ .name = "alias-references", .body = "let mut a=[1]\nloop &x,&y in a,a {}", .diagnostic = conflict },
        .{ .name = "alias-copy-reference", .body = "let mut a=[1]\nloop x,&y in a,a {}", .diagnostic = conflict },
        .{ .name = "alias-reference-copy", .body = "let mut a=[1]\nloop &x,y in a,a {}", .diagnostic = conflict },
        .{ .name = "source-mutation", .body = "let mut a=[1]\nloop x,y in a,{a[0]=2\n[3]} {}", .diagnostic = conflict },
        .{ .name = "captured-source-mutation", .body = "let mut a=[1]\nlet mutate=()->[]int{a=[2]\na}\nloop x,y in a,mutate() {}", .diagnostic = conflict },
        .{ .name = "captured-owner", .body = "let mut a=[1]\nloop &x,i in a,0.. {let read=()->int{a[0]}\n@println(read())}", .diagnostic = conflict },
        .{ .name = "readonly", .body = "let a=[1]\nlet b=[2]\nloop &x,&y in a,b {x.*=3}", .diagnostic = "assignment requires a mutable local" },
        .{ .name = "reference-string", .body = "loop a,&b in [1],\"é\" {}", .diagnostic = "reference iteration requires a local array owner" },
        .{ .name = "reference-escape", .body = "let mut a=[1]\nloop &x,i in a,0.. {let escaped=&x.*\n}", .diagnostic = "local references require" },
        .{ .name = "binding-in-source", .body = "loop x,y in [1],x.. {}", .diagnostic = "unknown name" },
        .{ .name = "binding-in-else", .body = "loop x,y in [1],[2] {} else {@println(y)}", .diagnostic = "unknown name" },
        .{ .name = "binding-after-loop", .body = "loop x,y in [1],[2] {}\n@println(y)", .diagnostic = "unknown name" },
    }) |case| {
        const source = b.fmt("invalid-{s}.nov", .{case.name});
        _ = fixtures.add(source, b.fmt("let main:()={{\n{s}\n}}\n", .{case.body}));
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI zip iteration: reject {s}", .{case.name});
        run.setCwd(fixtures.getDirectory());
        run.addArgs(&.{ "check", "--semantic", source });
        run.expectExitCode(1);
        run.expectStdOutEqual("");
        run.expectStdErrMatch("error:");
        run.expectStdErrMatch(case.diagnostic);
        if (std.mem.eql(u8, case.diagnostic, conflict)) run.expectStdErrMatch("borrow originates here");
        test_step.dependOn(&run.step);
    }
}

fn addValuePayloadCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    const aliases_source =
        \\let standard = @import("std")
        \\let option = @import("lib/./option.nov")
        \\let result = @import("lib/result.nov")
        \\let api = @import("api.nov")
        \\let verify: () = {
        \\    let callback: api.Callback = (n) {
        \\        n + 1
        \\    }
        \\    let original: api.FunctionOption = .some[callback]
        \\    let canonical: standard.Option((int) -> int) = original
        \\    let direct: option.Option(api.Callback) = canonical
        \\    let extracted = option.Option(api.Callback).unwrapOr(direct, callback)
        \\    @assert(extracted(3) == 4)
        \\    let good: api.FunctionResult = .ok[callback]
        \\    let same: result.Result((int) -> int, str) = good
        \\    let function = standard.Result(api.Callback, str).unwrapOr(same, callback)
        \\    @assert(function(4) == 5)
        \\    let bounds = 1..4
        \\    let interval: api.RangeOption = .some[bounds]
        \\    let rangeCanonical: standard.Option(@TypeOf(0..1)) = interval
        \\    let range = rangeCanonical.unwrapOr(0..0)
        \\    let mut count = 0
        \\    loop n in range {
        \\        count += 1
        \\    }
        \\    @assert(count == 3)
        \\}
        \\@[test]
        \\let imported_value_aliases_are_canonical: () = verify()
        \\let main: () = verify()
        \\
    ;
    const api_source =
        \\let standard = @import("std")
        \\let option = @import("lib/option.nov")
        \\let result = @import("lib/./result.nov")
        \\@[public]
        \\let Callback = (int) -> int
        \\@[public]
        \\let Range = @TypeOf(0..1)
        \\@[public]
        \\let FunctionOption = option.Option(Callback)
        \\@[public]
        \\let FunctionResult = result.Result(Callback, str)
        \\@[public]
        \\let RangeOption = standard.Option(Range)
        \\
    ;
    _ = fixtures.add("value-payloads.nov", @embedFile("examples/value-payloads.nov"));
    _ = fixtures.add("aliases.nov", aliases_source);
    _ = fixtures.add("api.nov", api_source);
    const package = b.addWriteFiles();
    const installed_exe = package.addCopyFile(exe.getEmittedBin(), b.fmt("bin/{s}", .{exe.out_filename}));
    _ = package.addCopyDirectory(b.path("lib"), "lib/nov", .{});
    _ = package.add("value-payloads.nov", @embedFile("examples/value-payloads.nov"));
    _ = package.add("aliases.nov", std.mem.replaceOwned(u8, b.allocator, aliases_source, "lib/", "lib/nov/") catch @panic("OOM"));
    _ = package.add("api.nov", std.mem.replaceOwned(u8, b.allocator, api_source, "lib/", "lib/nov/") catch @panic("OOM"));
    const expected_tests = "PASS callable_constructors_match_and_propagation\nPASS escaping_shared_cells_and_overwritten_owner\nPASS creation_snapshots_and_later_copies\nPASS range_copies_exact_bounds_and_inclusive_max\nPASS find_and_map_function_and_range_results\nPASS custom_hooks_nested_helpers_and_inactive_values\n6 passed; 0 failed; 6 total\n";
    for ([_]bool{ false, true }) |installed| {
        for ([_][]const u8{ "value-payloads.nov", "aliases.nov", "api.nov" }) |source| {
            for ([_][]const u8{ "fmt", "semantic", "run", "test" }) |command| {
                const execute = std.mem.eql(u8, command, "run") or std.mem.eql(u8, command, "test");
                if (execute and std.mem.eql(u8, source, "api.nov")) continue;
                const modes: []const []const u8 = if (execute) &.{ "", "--debug", "--release" } else &.{""};
                for (modes) |mode| {
                    const run = if (installed) std.Build.Step.Run.create(b, "CLI value payloads: relocated install") else b.addRunArtifact(exe);
                    run.step.name = b.fmt("CLI value payloads: {s} {s} {s} (installed={})", .{ command, source, mode, installed });
                    if (installed) run.addFileArg(installed_exe);
                    run.setCwd(if (installed) package.getDirectory() else fixtures.getDirectory());
                    run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                    run.setEnvironmentVariable("NOV_CC", "");
                    run.setEnvironmentVariable("CC", "");
                    if (std.mem.eql(u8, command, "fmt")) {
                        run.addArgs(&.{ "fmt", "--check", source });
                    } else if (std.mem.eql(u8, command, "semantic")) {
                        run.addArgs(&.{ "check", "--semantic", source });
                    } else {
                        run.addArgs(&.{ command, source });
                        if (mode.len != 0) run.addArg(mode);
                    }
                    if (!installed and !std.mem.eql(u8, command, "fmt")) {
                        run.addArg("--lib-dir");
                        run.addDirectoryArg(library);
                    }
                    const example = std.mem.eql(u8, source, "value-payloads.nov");
                    const is_test = std.mem.eql(u8, command, "test");
                    run.expectExitCode(0);
                    run.expectStdOutEqual(if (execute) (if (example) (if (is_test) expected_tests else "Custom: callback:5 range:3\nCalled: 4\nSum: 6\n") else if (is_test) "PASS imported_value_aliases_are_canonical\n1 passed; 0 failed; 1 total\n" else "") else "");
                    run.expectStdErrEqual("");
                    test_step.dependOn(&run.step);
                }
            }
        }
    }
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8, command: []const u8 = "check" }{
        .{ .name = "reference", .source = "let U=union{value:&int,none,}\nlet f:(x:U)={}", .diagnostic = "not supported" },
        .{ .name = "reference-array", .source = "let U=union{value:[]&int,none,}\nlet f:(x:U)={}", .diagnostic = "references cannot escape" },
        .{ .name = "reference-capture", .source = "let main:()={let mut n=1\nlet view=&n\nlet f:std.Option(()->int)=.some[()->int{view.*}]\n}", .diagnostic = "local references require" },
        .{ .name = "borrow-escape", .source = "let bad:()->std.Option(&int)={let mut n=1\n.some[&n]}", .diagnostic = "not supported" },
        .{ .name = "print-function", .source = "let main:()={let x:std.Option(()->int)=.none[]\n@println(x)}", .diagnostic = "not implemented yet" },

        .{ .name = "print-nested", .source = "let U=union{value:()->int,none,}\nlet main:()={let x:std.Option(U)=.some[.none[]]\n@println(x)}", .diagnostic = "not implemented yet" },
        .{ .name = "function-reference-value", .source = "let U=union{f:(int)->int,}\nlet bad:(f:(int)->int)->U={let view=&f\n.f[view]}", .diagnostic = "local references require" },
        .{ .name = "range-reference-value", .source = "let U=union{r:@TypeOf(0..1),}\nlet bad:()->U={let range=0..1\n.r[&range]}", .diagnostic = "local references require" },
        .{ .name = "function-exact", .source = "let U=union{f:(int)->int,}\nlet bad:(f:(uint)->uint)->U=.f[f]", .diagnostic = "expected type" },
        .{ .name = "function-equality", .source = "let f:(a:()->int)->bool=a==a", .diagnostic = "operator is not supported" },
        .{ .name = "function-nominal", .source = "let A=union{f:()->int,}\nlet B=union{f:()->int,}\nlet f:(x:A)->B=x", .diagnostic = "expected type" },
        .{ .name = "range-exact", .source = "let U=union{r:@TypeOf(0..1),}\nlet f:(n:int)->U={let bounds=n..n\n.r[bounds]}", .diagnostic = "expected type" },
        .{ .name = "struct-function-exact", .source = "let S=struct{f:(int)->int,}\nlet bad:(f:(uint)->uint)->S=S.[f=f]", .diagnostic = "expected type" },
        .{ .name = "struct-range-exact", .source = "let S=struct{r:@TypeOf(0..1),}\nlet bad:(n:int)->S=S.[r=n..n]", .diagnostic = "expected type" },
        .{ .name = "recursive-array-union", .source = "let U=union{items:[][]U,function:()->int,none,}\nlet main:()={}", .diagnostic = "cyclic" },
        .{ .name = "wide-function", .source = "let U=union{f:(i128)->i128,none,}\nlet f:(x:U)={}\nlet main:()={}", .diagnostic = "C backend only supports", .command = "emit-c" },
        .{ .name = "wide-range", .source = "let U=union{r:@TypeOf(@as(i128,0)..@as(i128,1)),none,}\nlet f:(x:U)={}\nlet main:()={}", .diagnostic = "C backend only supports", .command = "emit-c" },
    }) |case| {
        const source = b.fmt("invalid-value-payload-{s}.nov", .{case.name});
        _ = fixtures.add(source, b.fmt("let std=@import(\"std\")\n{s}\n", .{case.source}));
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI value payloads: reject {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArg(case.command);
        if (std.mem.eql(u8, case.command, "check")) check.addArg("--semantic");
        check.addArgs(&.{ source, "--lib-dir" });
        check.addDirectoryArg(library);
        check.expectExitCode(1);
        check.expectStdOutEqual("");
        check.expectStdErrMatch(".nov:");
        check.expectStdErrMatch("error:");
        check.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&check.step);
    }
}

fn addStorableFieldCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    _ = fixtures.add("storable-fields.nov", @embedFile("examples/storable-fields.nov"));
    const package = b.addWriteFiles();
    const installed_exe = package.addCopyFile(exe.getEmittedBin(), b.fmt("bin/{s}", .{exe.out_filename}));
    _ = package.addCopyDirectory(b.path("lib"), "lib/nov", .{});
    _ = package.add("storable-fields.nov", @embedFile("examples/storable-fields.nov"));
    const expected_tests = "PASS constructors_updates_and_nested_paths\nPASS union_field_arrays_copy_across_calls\nPASS escaping_closures_aliases_and_snapshots\nPASS direct_computed_pipe_and_callee_order\nPASS range_fields_reloop_and_inclusive_max\nPASS generic_fields_methods_and_printing\n6 passed; 0 failed; 6 total\n";
    for ([_]bool{ false, true }) |installed| {
        for ([_][]const u8{ "fmt", "semantic", "run", "test" }) |command| {
            const execute = std.mem.eql(u8, command, "run") or std.mem.eql(u8, command, "test");
            const modes: []const []const u8 = if (execute) &.{ "", "--debug", "--release" } else &.{""};
            for (modes) |mode| {
                const run = if (installed) std.Build.Step.Run.create(b, "CLI storable fields: relocated install") else b.addRunArtifact(exe);
                run.step.name = b.fmt("CLI storable fields: {s} {s} (installed={})", .{ command, mode, installed });
                if (installed) run.addFileArg(installed_exe);
                run.setCwd(if (installed) package.getDirectory() else fixtures.getDirectory());
                run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
                run.setEnvironmentVariable("NOV_CC", "");
                run.setEnvironmentVariable("CC", "");
                if (std.mem.eql(u8, command, "fmt")) {
                    run.addArgs(&.{ "fmt", "--check", "storable-fields.nov" });
                } else if (std.mem.eql(u8, command, "semantic")) {
                    run.addArgs(&.{ "check", "--semantic", "storable-fields.nov" });
                } else {
                    run.addArgs(&.{ command, "storable-fields.nov" });
                    if (mode.len != 0) run.addArg(mode);
                }
                if (!installed and !std.mem.eql(u8, command, "fmt")) {
                    run.addArg("--lib-dir");
                    run.addDirectoryArg(library);
                }
                run.expectExitCode(0);
                run.expectStdOutEqual(if (execute) (if (std.mem.eql(u8, command, "test")) expected_tests else "Called: 5 5\nCustom: Processor(4)\nReport: Report[ outcome: Result([]i64, str).ok[[1, 2]] ]\n") else "");
                run.expectStdErrEqual("");
                test_step.dependOn(&run.step);
            }
        }
    }
    for ([_]struct { name: []const u8, source: []const u8, diagnostic: []const u8 }{
        .{ .name = "void", .source = "let S=struct{value:void,}\nlet f:(s:S)={}", .diagnostic = "not supported" },
        .{ .name = "type", .source = "let S=struct{value:type,}\nlet f:(s:S)={}", .diagnostic = "not supported" },
        .{ .name = "reference", .source = "let S=struct{value:&int,}\nlet f:(s:S)={}", .diagnostic = "not supported" },
        .{ .name = "reference-array", .source = "let S=struct{value:[]&int,}\nlet f:(s:S)={}", .diagnostic = "references cannot escape" },
        .{ .name = "reference-capture", .source = "let S=struct{run:()->int,}\nlet main:()={let mut n:int=1\nlet view=&n\nlet s=S.[run=()->int{view.*}]\n}", .diagnostic = "local references require" },
        .{ .name = "direct-cycle", .source = "let S=struct{value:S,}\nlet f:(s:S)={}", .diagnostic = "cyclic" },
        .{ .name = "array-cycle", .source = "let S=struct{value:[][]S,}\nlet f:(s:S)={}", .diagnostic = "cyclic" },
        .{ .name = "union-cycle", .source = "let S=struct{value:U,}\nlet U=union{record:S,none,}\nlet f:(s:S)={}", .diagnostic = "cyclic" },
        .{ .name = "generic-cycle", .source = "let S:(T:type)->type=struct{value:S(T),}\nlet f:(s:S(int))={}", .diagnostic = "cyclic" },
        .{ .name = "callback-constructor", .source = "let S=struct{run:(int)->int,}\nlet f:(run:(uint)->uint)->S=S.[run]", .diagnostic = "expected type" },
        .{ .name = "callback-update", .source = "let S=struct{run:(int)->int,}\nlet f:(s:S,run:(uint)->uint)={let mut copy=s\ncopy.run=run}", .diagnostic = "expected type" },
        .{ .name = "callback-reference-exact", .source = "let S=struct{run:(&int),}\nlet f:(run:(&mut int))->S=S.[run]", .diagnostic = "expected type" },
        .{ .name = "range-update", .source = "let S=struct{interval:@TypeOf(0..1),}\nlet f:(s:S,n:int)={let mut copy=s\ncopy.interval=n..n}", .diagnostic = "expected type" },
        .{ .name = "method-field-conflict", .source = "let S=struct{run:(int)->int,let run:(self:S,n:int)->int=n\n}\nlet f:(s:S)->int=s.run(1)", .diagnostic = "duplicate" },
        .{ .name = "print-function", .source = "let S=struct{run:()->int,}\nlet f:(s:S)=@println(s)", .diagnostic = "not implemented yet" },

        .{ .name = "extern-record", .source = "let S=struct{run:()->int,}\n@[extern]\nlet foreign:(s:S)\nlet main:()={}", .diagnostic = "extern ABI supports only bool" },
        .{ .name = "export-record", .source = "let S=struct{value:std.Result([]int,str),}\n@[export]\nlet foreign:(s:S)={}\nlet main:()={}", .diagnostic = "export ABI supports only bool" },
    }) |case| {
        const source = b.fmt("invalid-storable-field-{s}.nov", .{case.name});
        _ = fixtures.add(source, b.fmt("let std=@import(\"std\")\n{s}\n", .{case.source}));
        const check = b.addRunArtifact(exe);
        check.step.name = b.fmt("CLI storable fields: reject {s}", .{case.name});
        check.setCwd(fixtures.getDirectory());
        check.addArgs(&.{ "check", "--semantic", source, "--lib-dir" });
        check.addDirectoryArg(library);
        check.expectExitCode(1);
        check.expectStdOutEqual("");
        check.expectStdErrMatch(".nov:");
        check.expectStdErrMatch("error:");
        check.expectStdErrMatch(case.diagnostic);
        test_step.dependOn(&check.step);
    }
}

fn addRangeValueCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step, fixtures: *std.Build.Step.WriteFile) void {
    _ = fixtures.add("range-values.nov",
        \\let main: () = {
        \\    let mut range = 0..10
        \\    let copy = range
        \\    loop i in range {
        \\        @print("{i} ")
        \\        range = 8..=9
        \\    }
        \\    @println()
        \\    loop i in copy {
        \\        @print("{i} ")
        \\    }
        \\    @println()
        \\    loop i in range {
        \\        @print("{i} ")
        \\    }
        \\    @println()
        \\}
        \\
    );
    for ([_][]const u8{ "--debug", "--release" }) |mode| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI: reusable range values ({s})", .{mode});
        run.setCwd(fixtures.getDirectory());
        run.setEnvironmentVariable("PATH", std.fs.path.dirname(b.graph.zig_exe).?);
        run.setEnvironmentVariable("NOV_CC", "");
        run.setEnvironmentVariable("CC", "");
        run.addArgs(&.{ "run", "range-values.nov", mode });
        run.expectExitCode(0);
        run.expectStdOutEqual("0 1 2 3 4 5 6 7 8 9 \n0 1 2 3 4 5 6 7 8 9 \n8 9 \n");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    for ([_][]const u8{ "check", "fmt" }) |command| {
        const run = b.addRunArtifact(exe);
        run.setCwd(fixtures.getDirectory());
        run.addArgs(&.{ command, if (std.mem.eql(u8, command, "check")) "--semantic" else "--check", "range-values.nov" });
        run.expectExitCode(0);
        run.expectStdOutEqual("");
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
}
