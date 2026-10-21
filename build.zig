const std = @import("std");
const builtin = @import("builtin");

const nov_version: std.SemanticVersion = .{ .major = 0, .minor = 0, .patch = 0 };

pub fn build(b: *std.Build) void {
    comptime {
        const current_zig = builtin.zig_version;
        const min_zig = std.SemanticVersion.parse("0.16.0") catch unreachable;
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

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const release = b.step("release", "Make an upstream binary release");
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

    const tests = b.addTest(.{ .root_module = exe.root_module });
    const run_tests = b.addRunArtifact(tests);
    // Generated-C tests compile with the pinned toolchain's `zig cc`.
    run_tests.setEnvironmentVariable("NOV_TEST_ZIG", b.graph.zig_exe);
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_tests.step);
    addCliTests(b, exe, test_step);
    addGcTests(b, test_step);

    const fmt_step = b.step("fmt", "Format all source files");
    fmt_step.dependOn(&b.addFmt(.{ .paths = &.{ "build.zig", "src" } }).step);

    const clean_step = b.step("clean", "Remove build artifacts");
    clean_step.makeFn = clean;
}

fn addGcTests(b: *std.Build, test_step: *std.Build.Step) void {
    const compile = b.addSystemCommand(&.{ b.graph.zig_exe, "cc", "-std=c11", "-Wall", "-Wextra", "-Wpedantic", "-Werror", "-O2", "-fsanitize=undefined", "-fno-sanitize-recover=all" });
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
    _ = fixtures.add("strings.nov", @embedFile("examples/strings.nov"));
    _ = fixtures.add("interpolation.nov", @embedFile("examples/interpolation.nov"));
    _ = fixtures.add("interpolation-format.nov", "let main: () -> void = {let n: int = 1\n@println(\"{n:02}\")}\n");
    _ = fixtures.add("interpolation-name.nov", "let main: () -> void = @println(\"{missing}\")\n");
    _ = fixtures.add("interpolation-global.nov", "let n: int = 1\nlet text: string = \"{n}\"\n");
    _ = fixtures.add("inferred-locals.nov", @embedFile("examples/inferred-locals.nov"));
    _ = fixtures.add("inferred-integer.nov", "let main: () -> void = {let n = 1\n@println(n)}\n");
    _ = fixtures.add("inferred-empty-array.nov", "let main: () -> void = {let a = []\n}\n");
    _ = fixtures.add("scalar-arrays.nov", @embedFile("examples/arrays.nov"));
    _ = fixtures.add("membership-default.nov", "let main: () -> void = @println(1 in [1, 2])\n");
    _ = fixtures.add("membership-width.nov", "let f: (n: i16, a: []i8) -> bool = n in a\n");
    _ = fixtures.add("membership-string.nov", "let main: () -> void = @println(\"a\" in \"abc\")\n");
    _ = fixtures.add("array-bounds.nov", "let main: () -> void = {let a: []bool = [true]\n@println(a[-2])}\n");
    _ = fixtures.add("array-compound.nov", "let main: () -> void = {\n    let mut a: []i8 = [127, 10]\n    a[0] += 1\n    a[0] -= 1\n    a[0] *= 2\n    a[0] /= -1\n    a[0] %= 3\n    a[-1] += {\n        a = []\n        5\n    }\n    @println(a[0], a[-1], a.len)\n}\n");
    _ = fixtures.add("array-bool-compound.nov", "let main: () -> void = {let mut a: []bool = [true]\na[0] += false}\n");
    _ = fixtures.add("array-compound-bounds.nov", "let main: () -> void = {let mut a: []int = []\na[-1] += {@print(true)\nreturn}}\n");
    _ = fixtures.add("array-length-write.nov", "let main: () -> void = {let mut a: []int = []\na.len = 1}\n");
    _ = fixtures.add("if-program.nov", "let divide: (n: int) -> int = 1 / n\nlet main: () -> void = {\nlet n: int = if false {divide(0)} else if true {42} else {divide(0)}\nif n == 42 {@println(n)}\n}\n");
    _ = fixtures.add("return-program.nov", "let f: (c: bool) -> int = {if c {{return 42}}\n1 / 0}\nlet main: () -> void = {let b: bool = false and {return}\n@println(f(true), b)\nreturn}\n");
    _ = fixtures.add("mutable-program.nov", "let f: (c: bool) -> int = {let mut n: int = 1\nn += if c {return 42} else {2}\nn}\nlet main: () -> void = {let mut n: int = 1\nif true {n += 2}\nlet b: bool = false and {n = 99\ntrue}\n@println(n, b, f(true), f(false))}\n");
    _ = fixtures.add("loop-values.nov", @embedFile("examples/loop-values.nov"));
    _ = fixtures.add("loop-program.nov", "let f: () -> int = {let mut n: int = 0\nloop {if n == 5 {break}\nn += 1\nloop false {continue}\nif n == 2 {continue}\nif n == 4 {break}\n@println(n)}\nn}\nlet main: () -> void = @println(f())\n");
    _ = fixtures.add("else-break-outside.nov", "let main: () -> void = loop false {} else {break}\n");
    _ = fixtures.add("else-mismatch.nov", "let main: () -> void = {let x: bool = loop true {break true} else {1}\n}\n");
    _ = fixtures.add("break-outside.nov", "let main: () -> void = {break}\n");
    _ = fixtures.add("value-break.nov", "let main: () -> void = loop true {break 1}\n");
    _ = fixtures.add("immutable-assignment.nov", "let main: () -> void = {let n: int = 1\nn = 2}\n");
    _ = fixtures.add("return-invalid.nov", "let main: () -> void = return true\n");
    _ = fixtures.add("return-unreachable.nov", "let main: () -> void = {return @println(true)\n@println(false)}\n");
    _ = fixtures.add("if-invalid.nov", "let main: () -> void = if true {} else {@println(missing)}\n");
    _ = fixtures.add("panic-program.nov", "let zero: i32 = 0\nlet divide: () -> i32 = 1 / zero\nlet main: () -> void = @println(divide())\n");

    const Case = struct {
        name: []const u8,
        args: []const []const u8,
        exit_code: u8,
        stdout: []const u8 = "",
        stderr_match: ?[]const u8 = null,
    };
    const cases = [_]Case{
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
        .{ .name = "check interpolation", .args = &.{ "check", "--semantic", "interpolation.nov" }, .exit_code = 0 },
        .{ .name = "fmt interpolation", .args = &.{ "fmt", "--check", "interpolation.nov" }, .exit_code = 0 },
        .{ .name = "reject interpolation formats", .args = &.{ "emit-c", "interpolation-format.nov" }, .exit_code = 1, .stderr_match = "expected {identifier}; expressions, calls and format options are unsupported" },
        .{ .name = "reject interpolation unknown names", .args = &.{ "emit-c", "interpolation-name.nov" }, .exit_code = 1, .stderr_match = "unknown name 'missing'" },
        .{ .name = "reject global interpolation", .args = &.{ "check", "--semantic", "interpolation-global.nov" }, .exit_code = 1, .stderr_match = "global strings require literal initializers; move interpolation into a function" },
        .{ .name = "check unchanged fibo", .args = &.{ "check", "--semantic", "fibo.nov" }, .exit_code = 0 },
        .{ .name = "check fizzbuzz", .args = &.{ "check", "--semantic", "fizzbuzz.nov" }, .exit_code = 0 },
        .{ .name = "fmt fizzbuzz", .args = &.{ "fmt", "--check", "fizzbuzz.nov" }, .exit_code = 0 },
        .{ .name = "fmt unchanged fibo", .args = &.{ "fmt", "--check", "fibo.nov" }, .exit_code = 0 },
        .{ .name = "check inferred locals", .args = &.{ "check", "--semantic", "inferred-locals.nov" }, .exit_code = 0 },
        .{ .name = "fmt inferred locals", .args = &.{ "fmt", "--check", "inferred-locals.nov" }, .exit_code = 0 },
        .{ .name = "reject inferred integer default", .args = &.{ "emit-c", "inferred-integer.nov" }, .exit_code = 1, .stderr_match = "integer literal needs an expected type (no default chosen)" },
        .{ .name = "reject inferred empty array", .args = &.{ "check", "--semantic", "inferred-empty-array.nov" }, .exit_code = 1, .stderr_match = "array literal needs an expected []T type" },
        .{ .name = "check array compounds", .args = &.{ "check", "--semantic", "array-compound.nov" }, .exit_code = 0 },
        .{ .name = "fmt array compounds", .args = &.{ "fmt", "--check", "array-compound.nov" }, .exit_code = 0 },
        .{ .name = "reject bool array compounds", .args = &.{ "emit-c", "array-bool-compound.nov" }, .exit_code = 1, .stderr_match = "operator is not supported" },
        .{ .name = "check scalar arrays", .args = &.{ "check", "--semantic", "scalar-arrays.nov" }, .exit_code = 0 },
        .{ .name = "reject membership integer defaults", .args = &.{ "check", "--semantic", "membership-default.nov" }, .exit_code = 1, .stderr_match = "integer literal needs an expected type (no default chosen)" },
        .{ .name = "reject membership widening", .args = &.{ "emit-c", "membership-width.nov" }, .exit_code = 1, .stderr_match = "expression does not match the expected type" },
        .{ .name = "reject string membership", .args = &.{ "emit-c", "membership-string.nov" }, .exit_code = 1, .stderr_match = "operator is not supported for this operand type" },
        .{ .name = "reject array length writes", .args = &.{ "emit-c", "array-length-write.nov" }, .exit_code = 1, .stderr_match = "array .len is read-only" },
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
        .{ .name = "fmt rejects array length writes", .args = &.{ "fmt", "--check", "array-length-write.nov" }, .exit_code = 1, .stderr_match = "formatting this syntax is not supported yet; file unchanged" },
        .{ .name = "fmt executable example", .args = &.{ "fmt", "--check", "checkout.nov" }, .exit_code = 0 },
        .{ .name = "check semantic mutable locals", .args = &.{ "check", "--semantic", "mutable-program.nov" }, .exit_code = 0 },
        .{ .name = "emit rejects immutable assignment", .args = &.{ "emit-c", "immutable-assignment.nov" }, .exit_code = 1, .stderr_match = "immutable-assignment.nov:2:1: error: assignment requires a mutable local" },
        .{ .name = "check semantic returns", .args = &.{ "check", "--semantic", "return-program.nov" }, .exit_code = 0 },
        .{ .name = "check return signature diagnostic", .args = &.{ "check", "--semantic", "return-invalid.nov" }, .exit_code = 1, .stderr_match = "return-invalid.nov:1:31: error: literal does not match the annotated type" },
        .{ .name = "emit rejects unreachable statement", .args = &.{ "emit-c", "return-unreachable.nov" }, .exit_code = 1, .stderr_match = "return-unreachable.nov:2:1: error: unreachable statement after return" },
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
            run.expectStdErrMatch("require the build or run command");
            test_step.dependOn(&run.step);
        }
    }

    const stdin_cases = [_]struct { source: []const u8, check: bool = false, output: []const u8 = "", diagnostic: ?[]const u8 = null, code: u8 = 0 }{
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
        .{ .source = "let x:[][]int=[[1]]", .diagnostic = "<stdin>: error: formatting this syntax is not supported yet", .code = 1 },
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
        fizzbuzz.expectStdOutEqual("FizzBuzz\n1\n2\nFizz\n4\nBuzz\nFizz\n7\n8\nFizz\n");
        fizzbuzz.expectStdErrEqual("");
        test_step.dependOn(&fizzbuzz.step);

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

        const interpolation_output = "1: 1\n2: 2\n3: Fizz\n4: 4\n5: Buzz\n6: Fizz\n7: 7\n8: 8\n9: Fizz\n10: Buzz\n11: 11\n12: Fizz\n13: 13\n14: 14\n15: FizzBuzz\n{literal}: true: é\x00😀 7\n";
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
        strings.expectStdOutEqual("Hello, 世界! 14\n[new binding]!\n[Hello, 世界!]\n{UTF-8} 4\na\x00b 3\nSame greeting: true true\nNUL bytes: true true\nUTF-8 equality: true false\n");
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

fn clean(step: *std.Build.Step, _: std.Build.Step.MakeOptions) !void {
    const b = step.owner;
    for ([_][]const u8{ "zig-out", ".zig-cache" }) |path| {
        std.Io.Dir.cwd().deleteTree(b.graph.io, b.pathFromRoot(path)) catch |err| {
            return step.fail("unable to remove {s}: {s}", .{ path, @errorName(err) });
        };
    }
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
