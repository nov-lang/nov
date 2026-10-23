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
    const install_lib = b.addInstallDirectory(.{
        .source_dir = b.path("lib"),
        .install_dir = .lib,
        .install_subdir = "nov",
    });
    b.getInstallStep().dependOn(&install_lib.step);

    const run_cmd = b.addSystemCommand(&.{b.getInstallPath(.bin, exe.out_filename)});
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
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

    const tests = b.addTest(.{ .root_module = exe.root_module });
    const run_tests = b.addRunArtifact(tests);
    // Generated-C tests compile with the pinned toolchain's `zig cc`.
    run_tests.setEnvironmentVariable("NOV_TEST_ZIG", b.graph.zig_exe);
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_tests.step);
    const cli_step = b.step("test-cli", "Run CLI integration tests");
    test_step.dependOn(cli_step);
    addCliTests(b, exe, cli_step);
    addImportCliTests(b, exe, cli_step);
    addGcTests(b, test_step);

    const fmt_step = b.step("fmt", "Format all source files");
    fmt_step.dependOn(&b.addFmt(.{ .paths = &.{ "build.zig", "src" } }).step);

    const clean_step = b.step("clean", "Remove build artifacts");
    clean_step.makeFn = clean;
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
    _ = fixtures.add("library-result-source.nov", @embedFile("lib/result.nov"));
    const library = fixtures.addCopyDirectory(b.path("lib"), "lib", .{});
    _ = fixtures.add("library-result.nov",
        \\let standard = @import("std")
        \\let Result = standard.Result
        \\
        \\let R = Result(uint, string)
        \\let eager: () -> uint = { @print("E")
        \\    9 }
        \\let recover: (error: string) -> uint = { @print("F")
        \\    7 }
        \\let text: (error: uint) -> string = "fallback:{error}"
        \\let unit: () -> Result(void, string) = .ok[]
        \\let fromUnit: () -> R = { unit().!
        \\    .ok[3] }
        \\let main: () = {
        \\    let good: R = .ok[1]
        \\    let bad: R = .err["missing"]
        \\    @println(good.unwrapOr(eager()), bad.unwrapOr(9), good.unwrapOrElse(recover), bad.unwrapOrElse(recover))
        \\    @println(Result(string, uint).err[2].unwrapOrElse(text))
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
    _ = fixtures.add("enums.nov", "let Kind=enum{not_found,permission_denied,io,\nlet toString:(k:Kind)->string=match k{.not_found=>\"not_found\"\n.permission_denied | .io=>\"other\"}\n}\nlet Result:(T:type,E:type)->type=union{ok:T,err:E,}\nlet R=Result(uint,Kind)\nlet Error=struct{kind:Kind=.not_found,message:string,}\nlet fail:()->R=.err[.not_found]\nlet pass:()->R={let n=fail().!\n.ok[n]}\nlet main:()={let e=Error[.message=\"not \"+\"found\"]\n@println(e.kind,e.message,Kind.io!=.not_found,match pass(){.ok=>|n|\"{n}\"\n.err=>|k|\"{k}\"})}");
    _ = fixtures.add("instance-methods.nov", "let K=enum{x,y,\nlet eql:(a:K,b:K)->bool=a==b\n}\nlet Alias=R\nlet R=struct{s:string,\nlet text:(r:Alias)->string=r.s\n}\nlet main:()={let k=K.x\n@println(Alias[.s=\"hel\"+\"lo\"].text(),k.eql(.x),K.y.eql(k))}");
    _ = fixtures.add("instance-signature.nov", "let R=struct{let bad:()->bool=true\n}\nlet main:()=R[].bad()");
    _ = fixtures.add("enum-missing.nov", "let K=enum{x,y,}\nlet f:(k:K)->bool=match k{.x=>true}");
    _ = fixtures.add("enum-numbered.nov", "let K=enum{x=7,y,}");
    _ = fixtures.add("enum-backing.nov", "let K=enum(u8){x,}");
    _ = fixtures.add("unions.nov", "let Alias=U\nlet U=union{ok:int,err:string,none,}\nlet describe:(u:Alias)->string=match u{.ok=>|v|\"ok:{v}\"\n.err=>|s|s\n.none=>\"none\"}\nlet main:()={@println(describe(U.ok[7]),describe(.err[\"a\"+\"b\"]),describe(.none[]))}");
    _ = fixtures.add("union-missing.nov", "let U=union{ok:int,none,}\nlet f:(u:U)->int=match u{.ok=>|n|n}");
    _ = fixtures.add("union-duplicate.nov", "let U=union{ok:int,none,}\nlet f:(u:U)->int=match u{.ok=>0\n.ok=>1\n_=>2}");
    _ = fixtures.add("union-capture.nov", "let U=union{ok:int,none,}\nlet f:(u:U)->int=match u{.ok=>|n|n\n_=>n}");
    _ = fixtures.add("generic-unions.nov", "let G:(T:type,E:type)->type=union{err:E,ok:T,}\nlet R=G(int,string)\nlet Text=G(string,string)\nlet make:(b:bool)->R={@print(\"M\")\nif b{.ok[7]}else{.err[\"b\"+\"ad\"]}}\nlet wrap:(b:bool)->Text={let n=make(b).!\n.ok[\"{n}\"]}\nlet show:(r:Text)->string=match r{.ok=>|s|s\n.err=>|s|s}\nlet main:()=@println(show(wrap(true)),show(wrap(false)))");
    _ = fixtures.add("record-payloads.nov", "let G:(T:type,Failure:type)->type=union{ok:T,err:Failure,}\nlet U=union{ok:R,err:E,}\nlet R=struct{s:string,}\nlet E=struct{message:string,}\nlet make:(b:bool)->G(R,E)=if b{.ok[R[.s=\"o\"+\"k\"]]}else{.err[E[.message=\"e\"+\"rr\"]]}\nlet wrap:(b:bool)->G(bool,E)={let r=make(b).!\n.ok[r.s == \"ok\"]}\nlet show:(r:G(bool,E))->string=match r{.ok=>|b|\"{b}\"\n.err=>|e|e.message}\nlet main:()=@println(show(wrap(true)),show(wrap(false)),match U.ok[R[.s=\"record\"]]{.ok=>|r|r.s\n.err=>|e|e.message})");
    _ = fixtures.add("record-interpolation.nov", "let G:(T:type,E:type)->type=union{ok:T,err:E,}\nlet Error=struct{s:string,\nlet toString:(err:Error)->string={@print(\"C\")\nerr.s+\"!\"}\n}\nlet fail:()->G(int,Error)=.err[Error[.s=\"not \"+\"found\"]]\nlet main:()=match fail(){.ok=>@panic(\"bad\")\n.err=>|err|@panic(\"Error: {err}\")}");
    _ = fixtures.add("record-interpolation-missing.nov", "let R=struct{s:string,}\nlet f:(r:R)->string=\"{r}\"");
    _ = fixtures.add("wide-record-payload.nov", "let U=union{ok:R,}\nlet R=struct{wide:i128,}\nlet f:(u:U)={}\nlet main:()={}");
    _ = fixtures.add("generic-cycle.nov", "let G:(T:type)->type=union{ok:G(T),}\nlet R=G(int)");
    _ = fixtures.add("generic-methods.nov", "let Result:(T:type,E:type)->type=union{ok:T,err:E,\nlet Self=@This()\nlet unwrapOr:(self:Self,fallback:T)->T=match self{.ok=>|v|v\n.err=>fallback}\nlet unwrapOrElse:(self:Self,fallback:(E)->T)->T=match self{.ok=>|v|v\n.err=>|e|fallback(e)}\n}\nlet R=Result(int,string)\nlet cb:(e:string)->int={@print(e)\n7}\nlet main:()={let r:R=.err[\"E\"]\n@println(R.unwrapOr(r,3),r.unwrapOrElse(cb))}");
    _ = fixtures.add("generic-method.nov", "let G:(T:type)->type=union{ok:T,\nlet m=1\n}");
    _ = fixtures.add("propagation-error.nov", "let G:(T:type,E:type)->type=union{ok:T,err:E,}\nlet f:(r:G(int,string))->G(int,bool)=.ok[r.!]");
    _ = fixtures.add("wide-generic.nov", "let G:(T:type)->type=union{ok:T,err:string,}\nlet f:()->G(i128)=.ok[170141183460469231731687303715884105727]\nlet main:()={}");
    _ = fixtures.add("wide-union.nov", "let U=union{ok:i128,none,}\nlet f:(u:U)={}\nlet main:()={}");
    _ = fixtures.add("wide-record.nov", "let R=struct{x:i128,}\nlet f:(r:R)= {}\nlet main:()={}\n");
    _ = fixtures.add("wide-record-alias.nov", "let Wide=u128\nlet Alias=R\nlet R=struct{x:Wide,}\nlet f:(r:Alias)= {}\nlet main:()={}\n");
    _ = fixtures.add("unused-wide-record.nov", "let R=struct{x:i128,}\nlet main:()={}\n");
    _ = fixtures.add("wide-record-bounds.nov", "let R=struct{x:i128=170141183460469231731687303715884105728,}\n");
    _ = fixtures.add("wide-record-array.nov", "let R=struct{x:[]u128,}\n");
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
        \\let annotated_string: Text = @as(string, @as(Text, "hé\0"))
        \\let Flag = bool
        \\let Count = int
        \\let Text = string
        \\let main: () -> void = {
        \\    @println(inferred_bool, annotated_bool, inferred_int, annotated_int)
        \\    @println("{inferred_string}:{annotated_string}")
        \\}
        \\
    );
    _ = fixtures.add("annotated-global-mismatch.nov", "let n: uint = @as(int, 1)\n");
    _ = fixtures.add("annotated-global-nesting.nov", "let n: uint = @as(uint, @as(int, 1))\n");
    _ = fixtures.add("annotated-global-bounds.nov", "let n: u8 = @as(u8, 256)\n");
    _ = fixtures.add("annotated-global-interpolation.nov", "let s: string = @as(string, \"{true}\")\n");
    _ = fixtures.add("global-untyped.nov", "let n = 1\n");
    _ = fixtures.add("global-bounds.nov", "let n = @as(u8, 256)\n");
    _ = fixtures.add("global-call.nov", "let n = @as(int, missing())\n");
    _ = fixtures.add("global-unknown.nov", "let n = @as(int, missing)\n");
    _ = fixtures.add("global-type.nov", "let n = @as(Missing, 1)\n");
    _ = fixtures.add("global-nesting.nov", "let n = @as(int, @as(uint, 1))\n");
    _ = fixtures.add("global-block.nov", "let n = @as(int, {1})\n");
    _ = fixtures.add("global-interpolation.nov", "let n = @as(string, \"{true}\")\n");
    _ = fixtures.add("alias-value.nov", "let Count = uint\nlet main: () = @println(Count)\n");
    _ = fixtures.add("alias-cycle.nov", "let A = []B\nlet B = A\n");
    _ = fixtures.add("ascription.nov", "let main: () -> void = {\n    @println(\"{@as(int, 1)}\", @as(uint, 1))\n    let a = @as([]int, [2])\n    @println(a[0])\n}\n");
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
    _ = fixtures.add("source-panic.nov", "let message: () -> string = {@print(\"before\")\n\"hé\\0\" + \"!\"}\n@[noreturn]\nlet stop: (s: string) -> void = @panic(s)\nlet main: () = stop(\"{message()}\")\n");
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
        .{ .name = "record interpolation missing method", .args = &.{ "check", "record-interpolation-missing.nov", "--semantic" }, .exit_code = 1, .stderr_match = "printing a struct, enum or union requires its method toString: (MyType) -> string" },
        .{ .name = "record payload semantic", .args = &.{ "check", "record-payloads.nov", "--semantic" }, .exit_code = 0 },
        .{ .name = "record payload 128 bit semantic", .args = &.{ "check", "wide-record-payload.nov", "--semantic" }, .exit_code = 0 },
        .{ .name = "record payload backend width", .args = &.{ "emit-c", "wide-record-payload.nov" }, .exit_code = 1, .stderr_match = "C backend only supports 8, 16, 32 and 64-bit integers" },
        .{ .name = "generic union semantic", .args = &.{ "check", "generic-unions.nov", "--semantic" }, .exit_code = 0 },
        .{ .name = "library Result semantic", .args = &.{ "check", "library-result.nov", "--semantic" }, .exit_code = 0, .lib_dir = true },
        .{ .name = "library Result format", .args = &.{ "fmt", "--check", "library-result-source.nov" }, .exit_code = 0 },
        .{ .name = "generic methods semantic", .args = &.{ "check", "generic-methods.nov", "--semantic" }, .exit_code = 0 },
        .{ .name = "generic cycle", .args = &.{ "check", "generic-cycle.nov", "--semantic" }, .exit_code = 1, .stderr_match = "cyclic generic union instantiation" },
        .{ .name = "generic method diagnostic", .args = &.{ "check", "generic-method.nov", "--semantic" }, .exit_code = 1, .stderr_match = "generic union members require an ordinary function signature" },
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
        .{ .name = "global integer needs context", .args = &.{ "check", "--semantic", "global-untyped.nov" }, .exit_code = 1, .stderr_match = "integer literal needs an expected type" },
        .{ .name = "global ascription bounds", .args = &.{ "check", "--semantic", "global-bounds.nov" }, .exit_code = 1, .stderr_match = "integer literal does not fit" },
        .{ .name = "global ascription rejects eager calls", .args = &.{ "check", "--semantic", "global-call.nov" }, .exit_code = 1, .stderr_match = "dependency analysis for this expression" },
        .{ .name = "global ascription unknown value", .args = &.{ "check", "--semantic", "global-unknown.nov" }, .exit_code = 1, .stderr_match = "unknown name 'missing'" },
        .{ .name = "global ascription unknown type", .args = &.{ "check", "--semantic", "global-type.nov" }, .exit_code = 1, .stderr_match = "unknown type 'Missing'" },
        .{ .name = "global ascription exact nesting", .args = &.{ "emit-c", "global-nesting.nov" }, .exit_code = 1, .stderr_match = "expression does not match the expected type" },
        .{ .name = "global ascription rejects blocks", .args = &.{ "emit-c", "global-block.nov" }, .exit_code = 1, .stderr_match = "inferred globals require immutable" },
        .{ .name = "global ascription rejects interpolation", .args = &.{ "check", "--semantic", "global-interpolation.nov" }, .exit_code = 1, .stderr_match = "move interpolation into a function" },
        .{ .name = "check wide record", .args = &.{ "check", "--semantic", "wide-record.nov" }, .exit_code = 0 },
        .{ .name = "reject reachable wide record backend", .args = &.{ "emit-c", "wide-record.nov" }, .exit_code = 1, .stderr_match = "C backend only supports 8, 16, 32 and 64-bit integers" },
        .{ .name = "check wide record aliases", .args = &.{ "check", "--semantic", "wide-record-alias.nov" }, .exit_code = 0 },
        .{ .name = "reject reachable wide record alias backend", .args = &.{ "emit-c", "wide-record-alias.nov" }, .exit_code = 1, .stderr_match = "C backend only supports 8, 16, 32 and 64-bit integers" },
        .{ .name = "run unused wide record", .args = &.{ "run", "unused-wide-record.nov" }, .exit_code = 0 },
        .{ .name = "wide record literal bounds", .args = &.{ "check", "--semantic", "wide-record-bounds.nov" }, .exit_code = 1, .stderr_match = "integer literal does not fit the annotated type" },
        .{ .name = "wide record arrays remain unsupported", .args = &.{ "check", "--semantic", "wide-record-array.nov" }, .exit_code = 1, .stderr_match = "this type is not supported" },
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
        .{ .name = "reject interpolation formats", .args = &.{ "emit-c", "interpolation-format.nov" }, .exit_code = 1, .stderr_match = "expected {expression}; blocks, control flow, quotes and format options are unsupported" },
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
        .{ .name = "fmt rejects array length writes", .args = &.{ "fmt", "--check", "array-length-write.nov" }, .exit_code = 1, .stderr_match = "formatting this syntax is not supported yet; file unchanged" },
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
            run.expectStdErrMatch("require the build or run command");
            test_step.dependOn(&run.step);
        }
    }

    const stdin_cases = [_]struct { source: []const u8, check: bool = false, output: []const u8 = "", diagnostic: ?[]const u8 = null, code: u8 = 0 }{
        .{ .source = "let f:(r:R)->string=r.next(1).text()", .output = "let f: (r: R) -> string = r.next(1).text()\n" },
        .{ .source = "let K=enum{not_found,io,}\nlet E=struct{k:K=.not_found,}\nlet f:()->K=K.io", .output = "let K = enum {\n    not_found,\n    io,\n}\nlet E = struct {\n    k: K = .not_found,\n}\nlet f: () -> K = K.io\n" },
        .{ .source = "let U=union{ok:R,}\nlet R=struct{s:string,}\nlet f:()->U=.ok[R[.s=\"ok\"]]", .output = "let U = union {\n    ok: R,\n}\nlet R = struct {\n    s: string,\n}\nlet f: () -> U = .ok[R[ .s = \"ok\" ]]\n" },
        .{ .source = "let G:(T:type)->type=union{ok:T,err:string,}\nlet R=G(int)\nlet f:(r:R)->R=.ok[r.!]", .output = "let G: (T: type) -> type = union {\n    ok: T,\n    err: string,\n}\nlet R = G(int)\nlet f: (r: R) -> R = .ok[r.!]\n" },
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

        const interpolation_output = "1: 1\n2: 2\n3: Fizz\n4: 4\n5: Buzz\n6: Fizz\n7: 7\n8: 8\n9: Fizz\n10: Buzz\n11: 11\n12: Fizz\n13: 13\n14: 14\n15: FizzBuzz\n{literal}: true: é\x00😀 7\n";
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

fn addImportCliTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) void {
    const fixtures = b.addWriteFiles();
    const library = fixtures.addCopyDirectory(b.path("lib"), "library", .{});
    const program = fixtures.add("app/main.nov",
        \\let standard = @import("std")
        \\let left = @import("parts/left.nov")
        \\let right = @import("parts/right.nov")
        \\let R = standard.Result(uint, string)
        \\let recover: (error: string) -> uint = { @print(error)
        \\    left.value() }
        \\let main: () = {
        \\    let bad: R = .err["fallback:"]
        \\    @println(bad.unwrapOrElse(recover), right.value())
        \\}
        \\
    );
    _ = fixtures.add("app/parts/left.nov", "let shared = @import(\"../shared.nov\")\nlet value: () -> uint = shared.value()\n");
    _ = fixtures.add("app/parts/right.nov", "let shared = @import(\".././shared.nov\")\nlet value: () -> uint = shared.value()\n");
    _ = fixtures.add("app/shared.nov", "let value: () -> uint = 7\nlet main: () = @println(\"wrong entry\")\n");
    _ = fixtures.add("relative.nov", "let left = @import(\"app/parts/left.nov\")\nlet right = @import(\"app/parts/right.nov\")\nlet main: () = @println(left.value(), right.value())\n");
    _ = fixtures.add("missing.nov", "let absent = @import(\"not-found.nov\")\nlet main: () = {}\n");
    _ = fixtures.add("cycle/a.nov", "let b = @import(\"b.nov\")\n");
    _ = fixtures.add("cycle/b.nov", "let a = @import(\"a.nov\")\n");
    _ = fixtures.add("no-entry.nov", "let imported = @import(\"app/shared.nov\")\n");
    _ = fixtures.add("bad-entry.nov", "let imported = @import(\"app/shared.nov\")\nlet main = true\n");
    _ = fixtures.add("bad-import.nov", "let imported = @import(\"errors/body.nov\")\nlet main: () = {}\n");
    _ = fixtures.add("errors/body.nov", "let bad: () -> bool = absent\n");
    _ = fixtures.add("wide-import.nov", "let imported = @import(\"errors/wide.nov\")\nlet main: () = {}\n");
    _ = fixtures.add("errors/wide.nov", "let wide: () -> i128 = 1\n");
    _ = fixtures.add("hidden.nov", "let standard = @import(\"std\")\nlet value: bool = standard.hidden\n");
    const visibility = fixtures.add("visibility/std.nov", "let hidden = true\n");
    const Case = struct { name: []const u8, args: []const []const u8, code: u8, diagnostic: ?[]const u8 = null };
    const cases = [_]Case{
        .{ .name = "relative diamond", .args = &.{ "check", "--semantic", "relative.nov" }, .code = 0 },
        .{ .name = "relative diamond and library callbacks", .args = &.{ "check", "--semantic", "app/main.nov" }, .code = 0 },
        .{ .name = "missing imports stay syntax only", .args = &.{ "check", "missing.nov" }, .code = 0 },
        .{ .name = "missing import", .args = &.{ "check", "--semantic", "missing.nov" }, .code = 1, .diagnostic = "cannot load imported source: FileNotFound" },
        .{ .name = "missing import buffers C", .args = &.{ "emit-c", "missing.nov" }, .code = 1, .diagnostic = "cannot load imported source" },
        .{ .name = "cycle with source note", .args = &.{ "emit-c", "cycle/a.nov" }, .code = 1, .diagnostic = "a.nov:1:9: note: earlier import in this cycle" },
        .{ .name = "imported main is not entry", .args = &.{ "emit-c", "no-entry.nov" }, .code = 1, .diagnostic = "no-entry.nov:1:1: error:" },
        .{ .name = "root entry signature", .args = &.{ "emit-c", "bad-entry.nov" }, .code = 1, .diagnostic = "bad-entry.nov:2:5: error:" },
        .{ .name = "imported body diagnostic", .args = &.{ "emit-c", "bad-import.nov" }, .code = 1, .diagnostic = "body.nov:1:23: error: unknown name 'absent'" },
        .{ .name = "imported backend diagnostic", .args = &.{ "emit-c", "wide-import.nov" }, .code = 1, .diagnostic = "wide.nov:1:" },
    };
    for (cases) |case| {
        const run = b.addRunArtifact(exe);
        run.step.name = b.fmt("CLI imports: {s}", .{case.name});
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
        const root_source = "let imported = @import(\"imported.nov\")\nlet standard = @import(\"std\")\nlet R = standard.Result(uint, string)\nlet main: () = @println(imported.value())\n";
        const root_file = protected.add(root_name, root_source);
        const imported_source = "let value: () -> uint = 7\n";
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
