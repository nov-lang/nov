const std = @import("std");
const builtin = @import("builtin");
const Ast = @import("Ast.zig");
const Ir = @import("Ir.zig");
const TypeResolver = @import("TypeResolver.zig");
const TypeAliases = @import("TypeAliases.zig");
const Enums = @import("Enums.zig");
const Visibility = @import("Visibility.zig");
const Exports = @import("Exports.zig");
const Externs = @import("Externs.zig");
const Sema = @import("Sema.zig");
const Literal = @import("Literal.zig");
const BodyChecker = @import("BodyChecker.zig");
const GcRuntime = @import("GcRuntime.zig");
const Tokenizer = @import("Tokenizer.zig");
const string_literal = @import("string_literal.zig");
const build_options = @import("build_options");
const clap = @import("clap");
const Parser = @import("Parser.zig");
const Diagnostics = @import("Diagnostics.zig");
const Formatter = @import("Formatter.zig");
const Module = @import("Module.zig");
const Compilation = @import("Compilation.zig");
const Dependencies = @import("Dependencies.zig");
const Lowering = @import("Lowering.zig");
const CEmitter = @import("CEmitter.zig");
const Driver = @import("Driver.zig");
const TestRunner = @import("TestRunner.zig");
const FmtPaths = @import("FmtPaths.zig");

const params = clap.parseParamsComptime(
    \\-h, --help             Display this help and exit.
    \\-v, --version          Display the version and exit.
    \\--check                Check formatting without changing files (fmt only).
    \\--exclude <str>...      Exclude a literal file/directory path (fmt only).
    \\--semantic             Check the bounded semantic/lowering subset (check only).
    \\--lib-dir <str>         Override the library root (semantic check/emit-c/build/run/test).
    \\--library               Emit library C without a process entrypoint (emit-c only).
    \\--debug                Compile with -O0 -g (build/run/test only).
    \\--release              Compile with -O3 (default; build/run/test only).
    \\--cc <str>             C compiler executable (build/run/test only).
    \\-o, --output <str>      Output executable path (build only).
    \\<arg>...
    \\
);

fn usage(writer: *std.Io.Writer) !void {
    try writer.writeAll(
        \\Usage: nov check [--semantic] <file>...
        \\       nov fmt [--check] [--exclude <path>] <file|directory>...
        \\       nov fmt [--check] -
        \\       nov emit-c [--library] <file>
        \\       nov [build] <file> [-o|--output <path>] [--cc <executable>] [--debug|--release]
        \\       nov run <file> [--cc <executable>] [--debug|--release]
        \\       nov test <file> [--cc <executable>] [--debug|--release] [--lib-dir <path>]
        \\
        \\Check syntax, or the bounded semantic subset with --semantic.
        \\Semantic commands accept --lib-dir; the default is executable-relative ../lib/nov.
        \\Emit program C11 with emit-c, compile with build, or compile and run with run.
        \\Run root-source tests in separate processes with test; no main is required.
        \\
        \\Options:
        \\
    );
    try clap.help(writer, clap.Help, &params, .{
        .description_on_new_line = false,
        .description_indent = 2,
        .spacing_between_parameters = 0,
        .indent = 2,
    });
}

pub fn main(init: std.process.Init) !u8 {
    var stderr_buffer: [4096]u8 = undefined;
    var stderr_file = std.Io.File.stderr().writer(init.io, &stderr_buffer);
    const stderr = &stderr_file.interface;
    defer stderr.flush() catch {};

    var diag: clap.Diagnostic = .{};
    var res = clap.parse(clap.Help, &params, .{ .arg = clap.parsers.string, .str = clap.parsers.string }, init.minimal.args, .{
        .allocator = init.gpa,
        .diagnostic = &diag,
    }) catch |err| {
        try diag.report(stderr, err);
        try usage(stderr);
        return 1;
    };
    defer res.deinit();

    if (res.args.version != 0) {
        var buffer: [128]u8 = undefined;
        var stdout = std.Io.File.stdout().writer(init.io, &buffer);
        try stdout.interface.print("nov {f}\n", .{build_options.version});
        try stdout.interface.flush();
        return 0;
    }
    if (res.args.help != 0) {
        try usage(stderr);
        return 0;
    }

    const args = res.positionals[0];
    const is_emit = args.len != 0 and std.mem.eql(u8, args[0], "emit-c");
    const is_fmt = args.len != 0 and std.mem.eql(u8, args[0], "fmt");
    const implicit_build = args.len != 0 and (std.mem.endsWith(u8, args[0], ".nov") or
        std.mem.indexOfAny(u8, args[0], "/\\") != null);
    const is_build = implicit_build or (args.len != 0 and std.mem.eql(u8, args[0], "build"));
    const is_run = args.len != 0 and std.mem.eql(u8, args[0], "run");
    const is_test = args.len != 0 and std.mem.eql(u8, args[0], "test");
    if (res.args.library != 0 and !is_emit) {
        try stderr.writeAll("error: --library requires the emit-c command\n");
        return 1;
    }
    if (res.args.debug != 0 and res.args.release != 0) {
        try stderr.writeAll("error: --debug and --release are mutually exclusive\n");
        return 1;
    }
    if ((res.args.debug != 0 or res.args.release != 0) and !is_build and !is_run and !is_test) {
        try stderr.writeAll("error: --debug and --release require the build, run, or test command\n");
        return 1;
    }
    const mode: Driver.Mode = if (res.args.debug != 0) .debug else .release;
    if (res.args.cc != null and !is_build and !is_run and !is_test) {
        try stderr.writeAll("error: --cc requires the build, run, or test command\n");
        return 1;
    }
    if (res.args.output != null and !is_build) {
        try stderr.writeAll("error: --output requires the build command\n");
        return 1;
    }
    if (res.args.cc) |cc| {
        if (cc.len == 0) {
            try stderr.writeAll("error: --cc requires a nonempty executable name or path\n");
            return 1;
        }
    }
    if (res.args.output) |output| {
        if (output.len == 0) {
            try stderr.writeAll("error: --output requires a nonempty path\n");
            return 1;
        }
    }
    if (res.args.semantic != 0 and (args.len == 0 or !std.mem.eql(u8, args[0], "check"))) {
        try stderr.writeAll("error: --semantic requires the check command\n");
        return 1;
    }
    if (res.args.@"lib-dir") |lib_dir| {
        if (!is_build and !is_run and !is_test and !is_emit and !(args.len != 0 and std.mem.eql(u8, args[0], "check") and res.args.semantic != 0)) {
            try stderr.writeAll("error: --lib-dir requires semantic check, emit-c, build, run, or test\n");
            return 1;
        }
        if (lib_dir.len == 0) {
            try stderr.writeAll("error: --lib-dir requires a nonempty path\n");
            return 1;
        }
    }
    if (res.args.check != 0 and !is_fmt) {
        try stderr.writeAll("error: --check requires the fmt command\n");
        return 1;
    }
    if (res.args.exclude.len != 0 and !is_fmt) {
        try stderr.writeAll("error: --exclude requires the fmt command\n");
        return 1;
    }
    for (res.args.exclude) |path| {
        if (path.len == 0) {
            try stderr.writeAll("error: --exclude requires a nonempty path\n");
            return 1;
        }
    }

    if (is_test) {
        if (args.len != 2) {
            try stderr.writeAll("error: test requires exactly one file\n");
            return 1;
        }
        var buffer: [4096]u8 = undefined;
        var stdout = std.Io.File.stdout().writer(init.io, &buffer);
        defer stdout.interface.flush() catch {};
        return testFile(init.io, init.gpa, init.environ_map, args[1], res.args.cc, mode, res.args.@"lib-dir", &stdout.interface, stderr) catch |err| {
            try stderr.print("{s}: error: {s}\n", .{ args[1], @errorName(err) });
            return 1;
        };
    }
    if (is_build or is_run) {
        const files = if (implicit_build) args else args[1..];
        if (files.len != 1) {
            try stderr.print("error: {s} requires exactly one file; program arguments are not supported yet\n", .{if (is_run) "run" else "build"});
            return 1;
        }
        return buildFileWithLibrary(init.io, init.gpa, init.environ_map, files[0], res.args.output, res.args.cc, mode, is_run, res.args.@"lib-dir", stderr) catch |err| {
            try stderr.print("{s}: error: {s}\n", .{ files[0], @errorName(err) });
            return 1;
        };
    }
    if (is_emit) {
        if (args.len != 2) {
            try stderr.writeAll("error: emit-c requires exactly one file\n");
            return 1;
        }
        var buffer: [4096]u8 = undefined;
        var stdout = std.Io.File.stdout().writer(init.io, &buffer);
        const valid = emitFileMode(init.io, init.gpa, args[1], res.args.@"lib-dir", if (res.args.library != 0) .library else .program, stderr, &stdout.interface) catch |err| {
            try stderr.print("{s}: error: {s}\n", .{ args[1], @errorName(err) });
            return 1;
        };
        try stdout.interface.flush();
        return if (valid) 0 else 1;
    }
    if (args.len < 2 or (!is_fmt and !std.mem.eql(u8, args[0], "check"))) {
        if (args.len != 0) try stderr.print("error: unknown command or missing file for '{s}'; expected 'check', 'fmt', 'emit-c', 'build', 'run', 'test', or a .nov file/path\n", .{args[0]});
        try usage(stderr);
        return 1;
    }

    if (is_fmt) {
        for (args[1..]) |path| {
            if (!std.mem.eql(u8, path, "-")) continue;
            if (args.len != 2 or res.args.exclude.len != 0) {
                try stderr.writeAll("error: fmt stdin '-' requires sole input and cannot use --exclude\n");
                return 1;
            }
            var input_buffer: [4096]u8 = undefined;
            var input = std.Io.File.stdin().readerStreaming(init.io, &input_buffer);
            var output_buffer: [4096]u8 = undefined;
            var output = std.Io.File.stdout().writer(init.io, &output_buffer);
            const valid = formatStdin(init.gpa, &input.interface, &output.interface, stderr, res.args.check != 0) catch |err| {
                try stderr.print("<stdin>: error: {s}\n", .{@errorName(err)});
                return 1;
            };
            try output.interface.flush();
            return if (valid) 0 else 1;
        }
        return if (try formatPaths(init.io, init.gpa, args[1..], res.args.exclude, stderr, res.args.check != 0)) 0 else 1;
    }
    var failed = false;
    for (args[1..]) |path| {
        const valid = (if (res.args.semantic != 0)
            checkFile(init.io, init.gpa, path, res.args.@"lib-dir", stderr)
        else
            processFile(init.io, init.gpa, path, stderr, false, false, false)) catch |err| {
            try stderr.print("{s}: error: {s}\n", .{ path, @errorName(err) });
            failed = true;
            continue;
        };
        failed = failed or !valid;
    }
    return if (failed) 1 else 0;
}

fn formatPaths(io: std.Io, allocator: std.mem.Allocator, inputs: []const []const u8, exclusions: []const []const u8, stderr: *std.Io.Writer, check_only: bool) !bool {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const scratch = arena.allocator();
    const cwd = std.Io.Dir.cwd();
    const base = try cwd.realPathFileAlloc(io, ".", scratch);
    var excluded: std.ArrayList([]const u8) = .empty;
    for (exclusions) |path| {
        try excluded.append(scratch, try std.fs.path.resolve(scratch, &.{ base, path }));
        // Also exclude aliases to an existing target. Nonexistent exclusions are valid.
        if (cwd.realPathFileAlloc(io, path, scratch)) |resolved| {
            try excluded.append(scratch, resolved);
        } else |_| {}
    }
    var paths: FmtPaths = .{ .io = io, .allocator = scratch, .stderr = stderr, .exclusions = excluded.items, .base = base };
    try paths.collect(inputs);
    var seen: std.StringHashMapUnmanaged(void) = .empty;
    for (paths.files.items) |path| {
        const resolved = cwd.realPathFileAlloc(io, path, scratch) catch |err| {
            try stderr.print("{s}: error: {s}\n", .{ path, @errorName(err) });
            paths.failed = true;
            continue;
        };
        const entry = try seen.getOrPut(scratch, resolved);
        if (entry.found_existing) continue;
        const valid = processFile(io, allocator, path, stderr, true, check_only, false) catch |err| {
            try stderr.print("{s}: error: {s}\n", .{ path, @errorName(err) });
            paths.failed = true;
            continue;
        };
        paths.failed = paths.failed or !valid;
    }
    return !paths.failed;
}

fn formatStdin(allocator: std.mem.Allocator, input: *std.Io.Reader, output: *std.Io.Writer, stderr: *std.Io.Writer, check_only: bool) !bool {
    const source = try input.allocRemainingAlignedSentinel(allocator, .limited(1024 * 1024), .of(u8), 0);
    defer allocator.free(source);
    if (std.mem.indexOfScalar(u8, source, 0) != null) return error.InvalidSource;
    var ast = try Parser.parse(allocator, source);
    defer ast.deinit(allocator);
    try Diagnostics.render(ast, "<stdin>", stderr);
    if (ast.errors.len != 0) return false;
    const formatted = Formatter.format(allocator, source) catch |err| switch (err) {
        error.UnsupportedSyntax => {
            try stderr.writeAll("<stdin>: error: formatting this syntax is not supported yet\n");
            return false;
        },
        else => return err,
    };
    defer allocator.free(formatted);
    if (check_only) {
        if (std.mem.eql(u8, source, formatted)) return true;
        try stderr.writeAll("<stdin>: error: input is not formatted\n");
        return false;
    }
    try output.writeAll(formatted);
    return true;
}

fn processFile(io: std.Io, allocator: std.mem.Allocator, path: []const u8, stderr: *std.Io.Writer, is_fmt: bool, check_only: bool, semantic: bool) !bool {
    if (semantic) return checkFile(io, allocator, path, null, stderr);
    const cwd = std.Io.Dir.cwd();
    // Resolve symlinks so atomic replacement updates the target, not the link.
    const resolved = try cwd.realPathFileAlloc(io, path, allocator);
    defer allocator.free(resolved);
    const source = try cwd.readFileAllocOptions(io, resolved, allocator, .limited(1024 * 1024), .of(u8), 0);
    defer allocator.free(source);
    if (std.mem.indexOfScalar(u8, source, 0) != null) return error.InvalidSource;

    var ast = try Parser.parse(allocator, source);
    defer ast.deinit(allocator);
    try Diagnostics.render(ast, path, stderr);
    if (ast.errors.len != 0) return false;
    if (!is_fmt) return true;

    const formatted = Formatter.format(allocator, source) catch |err| switch (err) {
        error.UnsupportedSyntax => {
            try stderr.print("{s}: error: formatting this syntax is not supported yet; file unchanged\n", .{path});
            return false;
        },
        else => return err,
    };
    defer allocator.free(formatted);
    if (std.mem.eql(u8, source, formatted)) return true;
    if (check_only) {
        try stderr.print("{s}: error: file is not formatted\n", .{path});
        return false;
    }

    try replaceFormattedFile(io, allocator, resolved, source, formatted);
    try stderr.print("{s}\n", .{path});
    return true;
}

fn testFile(io: std.Io, allocator: std.mem.Allocator, env: *const std.process.Environ.Map, path: []const u8, cc: ?[]const u8, mode: Driver.Mode, lib_dir: ?[]const u8, stdout: *std.Io.Writer, stderr: *std.Io.Writer) !u8 {
    var compilation = try loadCompilation(io, allocator, path, lib_dir);
    defer compilation.deinit();
    return TestRunner.run(io, allocator, &compilation, .{
        .discovery = .{ .cc = cc, .nov_cc = env.get("NOV_CC"), .env_cc = env.get("CC"), .mode = mode },
        .temp_root = env.get("TMPDIR") orelse env.get("TEMP") orelse (if (builtin.os.tag == .windows) "." else "/tmp"),
    }, stdout, stderr);
}

fn buildFile(io: std.Io, allocator: std.mem.Allocator, env: *const std.process.Environ.Map, path: []const u8, output_option: ?[]const u8, cc: ?[]const u8, mode: Driver.Mode, execute: bool, stderr: *std.Io.Writer) !u8 {
    return buildFileWithLibrary(io, allocator, env, path, output_option, cc, mode, execute, null, stderr);
}

fn buildFileWithLibrary(io: std.Io, allocator: std.mem.Allocator, env: *const std.process.Environ.Map, path: []const u8, output_option: ?[]const u8, cc: ?[]const u8, mode: Driver.Mode, execute: bool, lib_dir: ?[]const u8, stderr: *std.Io.Writer) !u8 {
    var compilation = try loadCompilation(io, allocator, path, lib_dir);
    defer compilation.deinit();
    var source = std.Io.Writer.Allocating.init(allocator);
    defer source.deinit();
    if (!try emitCompilation(allocator, &compilation, stderr, &source.writer)) return 1;

    const temp_root = env.get("TMPDIR") orelse env.get("TEMP") orelse
        (if (builtin.os.tag == .windows) "." else "/tmp");
    const workspace = try Driver.Workspace.init(io, allocator, temp_root);
    defer workspace.deinit(io);
    const c_path = try workspace.file("program.c");
    defer allocator.free(c_path);
    const exe_path = try workspace.file(if (builtin.os.tag == .windows) "program.exe" else "program");
    defer allocator.free(exe_path);
    const cwd = std.Io.Dir.cwd();
    try cwd.writeFile(io, .{ .sub_path = c_path, .data = source.written() });
    if (!try Driver.compile(io, .{ .cc = cc, .nov_cc = env.get("NOV_CC"), .env_cc = env.get("CC"), .mode = mode }, c_path, exe_path, stderr)) return 1;
    if (execute) return Driver.run(io, exe_path, stderr);

    const default_output = if (output_option == null) try Driver.defaultOutput(allocator, path) else null;
    defer if (default_output) |output| allocator.free(output);
    const output = output_option orelse default_output.?;

    const existing_output = cwd.realPathFileAlloc(io, output, allocator) catch |err| switch (err) {
        error.FileNotFound => null,
        else => return err,
    };
    defer if (existing_output) |resolved| allocator.free(resolved);
    if (existing_output) |resolved| {
        for (compilation.modules.items) |module| {
            if (std.mem.eql(u8, module.source.path, resolved)) {
                try stderr.writeAll("error: output path refers to the input source file\n");
                return 1;
            }
        }
    }
    // Publish only a successful build, retaining executable permissions.
    try cwd.copyFile(exe_path, cwd, output, io, .{});
    return 0;
}

fn emitFile(io: std.Io, allocator: std.mem.Allocator, path: []const u8, stderr: *std.Io.Writer, stdout: *std.Io.Writer) !bool {
    return emitFileWithLibrary(io, allocator, path, null, stderr, stdout);
}

fn loadCompilation(io: std.Io, allocator: std.mem.Allocator, path: []const u8, lib_dir: ?[]const u8) !Compilation {
    if (lib_dir) |root| return Compilation.load(allocator, io, path, .{ .lib_dir = root });
    const exe_dir = try std.process.executableDirPathAlloc(io, allocator);
    defer allocator.free(exe_dir);
    const root = try std.fs.path.resolve(allocator, &.{ exe_dir, "..", "lib", "nov" });
    defer allocator.free(root);
    return Compilation.load(allocator, io, path, .{ .lib_dir = root });
}

fn checkFile(io: std.Io, allocator: std.mem.Allocator, path: []const u8, lib_dir: ?[]const u8, stderr: *std.Io.Writer) !bool {
    var compilation = try loadCompilation(io, allocator, path, lib_dir);
    defer compilation.deinit();
    if (compilation.hasErrors()) {
        try Diagnostics.renderCompilation(&compilation, stderr);
        return false;
    }
    var result = try Lowering.lowerCompilation(&compilation);
    defer result.deinit();
    try Diagnostics.renderCompilation(&compilation, stderr);
    return !compilation.hasErrors();
}

const EmitMode = enum { program, library };

fn emitFileWithLibrary(io: std.Io, allocator: std.mem.Allocator, path: []const u8, lib_dir: ?[]const u8, stderr: *std.Io.Writer, stdout: *std.Io.Writer) !bool {
    return emitFileMode(io, allocator, path, lib_dir, .program, stderr, stdout);
}

fn emitFileMode(io: std.Io, allocator: std.mem.Allocator, path: []const u8, lib_dir: ?[]const u8, mode: EmitMode, stderr: *std.Io.Writer, stdout: *std.Io.Writer) !bool {
    var compilation = try loadCompilation(io, allocator, path, lib_dir);
    defer compilation.deinit();
    return emitCompilationMode(allocator, &compilation, mode, stderr, stdout);
}

fn emitCompilation(allocator: std.mem.Allocator, compilation: *Compilation, stderr: *std.Io.Writer, stdout: *std.Io.Writer) !bool {
    return emitCompilationMode(allocator, compilation, .program, stderr, stdout);
}

fn emitCompilationMode(allocator: std.mem.Allocator, compilation: *Compilation, mode: EmitMode, stderr: *std.Io.Writer, stdout: *std.Io.Writer) !bool {
    if (compilation.hasErrors()) {
        try Diagnostics.renderCompilation(compilation, stderr);
        return false;
    }
    var result = try Lowering.lowerCompilation(compilation);
    defer result.deinit();
    if (compilation.hasErrors()) {
        try Diagnostics.renderCompilation(compilation, stderr);
        return false;
    }
    const entry = if (compilation.root_main) |id| result.getCompilationEntry(id).?.function else null;
    const missing_span: Ir.Span = if (compilation.root_main) |id| blk: {
        const ref = compilation.getDeclaration(id).?;
        break :blk compilation.getModule(ref.source).?.getDeclaration(ref.local).?.name_span;
    } else .{ .source = compilation.root.?, .start = 0, .end = 0 };
    var diagnostic: CEmitter.Diagnostic = undefined;
    const c_source = (switch (mode) {
        .program => CEmitter.emitProgramRoot(allocator, &result.ir, entry, missing_span, &diagnostic),
        .library => CEmitter.emit(allocator, &result.ir, &diagnostic),
    }) catch |err| switch (err) {
        error.UnsupportedIntegerWidth, error.MissingEntrypoint, error.InvalidEntrypoint, error.InvalidTestEntrypoint, error.ExportSymbolCollision, error.ExternSymbolCollision => {
            if (err == error.MissingEntrypoint and compilation.root_main != null) diagnostic.reason = .entrypoint_signature;
            try Diagnostics.renderCompilationBackend(compilation, diagnostic, stderr);
            return false;
        },
        else => return err,
    };
    defer allocator.free(c_source);
    try stdout.writeAll(c_source);
    return true;
}

fn emitSource(allocator: std.mem.Allocator, path: []const u8, source: [:0]const u8, stderr: *std.Io.Writer, stdout: *std.Io.Writer) !bool {
    var module = try Module.init(allocator, path, source);
    defer module.deinit();
    if (!module.hasErrors()) {
        var dependencies = try Dependencies.analyze(&module);
        defer dependencies.deinit();
        if (!module.hasErrors()) {
            var result = try Lowering.lower(&module);
            defer result.deinit();
            if (!module.hasErrors()) {
                var diagnostic: CEmitter.Diagnostic = undefined;
                const c_source = CEmitter.emitProgram(allocator, &result.ir, &diagnostic) catch |err| switch (err) {
                    error.UnsupportedIntegerWidth, error.MissingEntrypoint, error.InvalidEntrypoint, error.InvalidTestEntrypoint, error.ExportSymbolCollision, error.ExternSymbolCollision => {
                        if (err == error.MissingEntrypoint) {
                            if (module.lookup("main")) |id| {
                                const declaration = module.getDeclaration(id).?;
                                diagnostic.reason = .entrypoint_signature;
                                diagnostic.span = .{ .start = declaration.name_span.start, .end = declaration.name_span.end };
                            }
                        }
                        try Diagnostics.renderBackend(&module, diagnostic, stderr);
                        return false;
                    },
                    else => return err,
                };
                defer allocator.free(c_source);
                try stdout.writeAll(c_source);
                return true;
            }
        }
    }
    try Diagnostics.renderModule(&module, stderr);
    return false;
}

fn checkSemantic(allocator: std.mem.Allocator, path: []const u8, source: []const u8, stderr: *std.Io.Writer) !bool {
    var module = try Module.init(allocator, path, source);
    defer module.deinit();
    if (!module.hasErrors()) {
        var dependencies = try Dependencies.analyze(&module);
        defer dependencies.deinit();
        if (!module.hasErrors()) {
            // Partial semantic passes leave deferred entries; lowering rejects
            // those rather than reporting unchecked source as valid.
            var result = try Lowering.lower(&module);
            defer result.deinit();
        }
    }
    try Diagnostics.renderModule(&module, stderr);
    return !module.hasErrors();
}

fn replaceFormattedFile(io: std.Io, allocator: std.mem.Allocator, path: []const u8, original: []const u8, formatted: []const u8) !void {
    const cwd = std.Io.Dir.cwd();
    const file = try cwd.openFile(io, path, .{});
    const stat = file.stat(io) catch |err| {
        file.close(io);
        return err;
    };
    file.close(io);
    var atomic = try cwd.createFileAtomic(io, path, .{ .replace = true, .permissions = stat.permissions });
    defer atomic.deinit(io);
    try atomic.file.setPermissions(io, stat.permissions);
    var buffer: [4096]u8 = undefined;
    var writer = atomic.file.writer(io, &buffer);
    try writer.interface.writeAll(formatted);
    try writer.interface.flush();
    // Detect edits made while parsing/rendering or preparing the replacement.
    // There is no portable compare-and-swap rename; a final rename race remains.
    const current = try cwd.readFileAlloc(io, path, allocator, .limited(1024 * 1024));
    defer allocator.free(current);
    if (!std.mem.eql(u8, original, current)) return error.SourceChanged;
    try atomic.replace(io);
}

test "library emission omits process main and buffers export diagnostics" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const cases = [_]struct { source: []const u8, message: ?[]const u8 = null }{
        .{ .source = "@[export]\nlet exact: (x: i32) -> i32 = x\n@[export(\"custom\")]\nlet renamed: (x: bool) -> bool = x\n" },
        .{ .source = "let main: () = {}\n@[export]\nlet exact: () -> u8 = 7\n" },
        .{ .source = "@[export(\"bad-name\")]\nlet f: () -> i32 = 1", .message = "export name must be" },
        .{ .source = "@[export]\nlet f: (x: string) -> string = x", .message = "export ABI supports only" },
        .{ .source = "@[export(\"same\")]\nlet f: () = {}\n@[export(\"same\")]\nlet g: () = {}", .message = "duplicate exported symbol" },
        .{ .source = "let helper: () = {}\n@[export]\nlet nov_helper: () = {}", .message = "export symbol conflicts with a generated function name" },
    };
    for (cases) |case| {
        try tmp.dir.writeFile(io, .{ .sub_path = "library.nov", .data = case.source });
        const path = try tmp.dir.realPathFileAlloc(io, "library.nov", allocator);
        defer allocator.free(path);
        var diagnostics = std.Io.Writer.Allocating.init(allocator);
        defer diagnostics.deinit();
        var output = std.Io.Writer.Allocating.init(allocator);
        defer output.deinit();
        try std.testing.expectEqual(case.message == null, try emitFileMode(io, allocator, path, null, .library, &diagnostics.writer, &output.writer));
        if (case.message) |message| {
            try std.testing.expectEqualStrings("", output.written());
            try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), message) != null);
            try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "library.nov:") != null);
            try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "^") != null);
        } else {
            try std.testing.expectEqualStrings("", diagnostics.written());
            try std.testing.expect(std.mem.indexOf(u8, output.written(), "int main(") == null);
            try std.testing.expect(std.mem.indexOf(u8, output.written(), " exact(") != null);
            if (std.mem.indexOf(u8, case.source, "renamed") != null) {
                try std.testing.expect(std.mem.indexOf(u8, output.written(), "bool custom(bool p0)") != null);
                try std.testing.expect(std.mem.indexOf(u8, output.written(), "nov_renamed") == null);
            }
        }
    }
}

test "extern library prototypes and compilation entrypoint backend diagnostics" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const cases = [_]struct { source: []const u8, mode: EmitMode, message: ?[]const u8 = null }{
        .{ .source = "@[extern(\"foreign_main\")]\nlet main: ()", .mode = .library },
        .{ .source = "@[extern(\"foreign_main\")]\nlet main: ()", .mode = .program, .message = "must have a Nov body, not an extern declaration" },
        .{ .source = "let helper: () = {}\n@[extern(\"nov_helper\")]\nlet foreign: ()\nlet main: () = {}", .mode = .library, .message = "extern symbol conflicts with a generated function name" },
    };
    for (cases) |case| {
        try tmp.dir.writeFile(io, .{ .sub_path = "extern.nov", .data = case.source });
        const path = try tmp.dir.realPathFileAlloc(io, "extern.nov", allocator);
        defer allocator.free(path);
        var diagnostics = std.Io.Writer.Allocating.init(allocator);
        defer diagnostics.deinit();
        var output = std.Io.Writer.Allocating.init(allocator);
        defer output.deinit();
        try std.testing.expectEqual(case.message == null, try emitFileMode(io, allocator, path, null, case.mode, &diagnostics.writer, &output.writer));
        if (case.message) |message| {
            try std.testing.expectEqualStrings("", output.written());
            try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), message) != null);
            try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "extern.nov:") != null);
        } else {
            try std.testing.expectEqualStrings("", diagnostics.written());
            try std.testing.expect(std.mem.indexOf(u8, output.written(), "void foreign_main(void);") != null);
            try std.testing.expect(std.mem.indexOf(u8, output.written(), "int main(") == null);
        }
    }
}

test "emit-c buffers source and renders frontend entrypoint and backend failures" {
    const allocator = std.testing.allocator;
    const cases = [_]struct { source: [:0]const u8, message: ?[]const u8 = null }{
        .{ .source = "let main: () -> void = {}" },
        .{ .source = "", .message = "requires a root function 'main'" },
        .{ .source = "let main: i32 = 1", .message = "must have type () -> void" },
        .{ .source = "let main: () -> bool = true", .message = "must have type () -> void" },
        .{ .source = "let main: (x: bool) -> void = {}", .message = "must have type () -> void" },
        .{ .source = "let helper: () = {}\n@[export]\nlet nov_helper: () = {}\nlet main: () = {}", .message = "export symbol conflicts with a generated function name" },
        .{ .source = "@[extern(\"foreign_main\")]\nlet main: ()", .message = "must have a Nov body, not an extern declaration" },
        .{ .source = "let helper: () = {}\n@[extern(\"nov_helper\")]\nlet foreign: ()\nlet main: () = {}", .message = "extern symbol conflicts with a generated function name" },
        .{ .source = "let broken =", .message = "expected" },
        .{ .source = "let main: () -> void = absent()", .message = "unknown name" },
        .{ .source = "let main: () -> void = {}\r\n\tlet wide: (x: i128) -> i128 = x", .message = "C backend only supports" },
        .{ .source = "let wide: () -> u128 = 1\nlet main: () -> void = {}", .message = "C backend only supports" },
    };
    for (cases) |case| {
        var diagnostics = std.Io.Writer.Allocating.init(allocator);
        defer diagnostics.deinit();
        var output = std.Io.Writer.Allocating.init(allocator);
        defer output.deinit();
        try std.testing.expectEqual(case.message == null, try emitSource(allocator, "input.nov", case.source, &diagnostics.writer, &output.writer));
        if (case.message) |message| {
            try std.testing.expectEqualStrings("", output.written());
            try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), message) != null);
            try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "input.nov:") != null);
            try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "^") != null);
            if (std.mem.indexOf(u8, case.source, "\r\n") != null) {
                try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "input.nov:2:32: error:") != null);
                try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "\n\t" ++ @as([30]u8, @splat(' ')) ++ "^\n") != null);
            }
        } else {
            try std.testing.expectEqualStrings("", diagnostics.written());
            try std.testing.expect(std.mem.indexOf(u8, output.written(), "int main(void)") != null);
        }
    }
}

test "format executable programs without changing generated C or runtime behavior" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    const programs = [_]struct { source: [:0]const u8, output: []const u8, code: u8 = 0, stderr: []const u8 = "" }{
        .{
            .source =
            \\let effect:(n:int)->int={@println(n)
            \\return n}
            \\let choose:(c:bool)->int={let mut n:int=1
            \\n += if c {return 42} else {2}
            \\n}
            \\let main:()->void={let mut n:int=0
            \\loop {n += 1
            \\n < 6} {if n == 2 {continue}
            \\if n == 4 {break}
            \\@println(n)}
            \\let skipped:bool=false and {n = 99
            \\return}
            \\let taken:bool=true or {let bad:int=1 / 0
            \\bad == 0}
            \\n += {n = 10
            \\2}
            \\n *= 2
            \\n /= 3
            \\n %= 3
            \\n -= 1
            \\@println(n,skipped,taken,choose(true),choose(false),effect(7),effect(8))
            \\return @println({let result:int={1 + 2} * 3
            \\result})}
            ,
            .output = "1\n3\n7\n8\n0 false true 42 3 7 8\n9\n",
        },
        .{ .source = "let search:(end:int,needle:int)->bool={let mut n:int=0\nloop n<end:n+=1{if n==needle{break true}}else{false}}\nlet main:()->void=@println(search(0,0),search(3,2),search(2,2))", .output = "false true false\n" },
        .{ .source = "let main:()->void={let n:int=1 / 0\n@println(n)}", .output = "", .code = 1, .stderr = "panic: division by zero\n" },
    };
    const zig_exe = std.testing.environ.getAlloc(allocator, "NOV_TEST_ZIG") catch null;
    defer if (zig_exe) |path| allocator.free(path);
    for (programs) |program| {
        const formatted = try Formatter.format(allocator, program.source);
        defer allocator.free(formatted);
        const terminated = try allocator.dupeSentinel(u8, formatted, 0);
        defer allocator.free(terminated);
        const again = try Formatter.format(allocator, terminated);
        defer allocator.free(again);
        try std.testing.expectEqualStrings(formatted, again);
        var original_c = std.Io.Writer.Allocating.init(allocator);
        defer original_c.deinit();
        var formatted_c = std.Io.Writer.Allocating.init(allocator);
        defer formatted_c.deinit();
        var diagnostics = std.Io.Writer.Allocating.init(allocator);
        defer diagnostics.deinit();
        const original_ok = try emitSource(allocator, "format.nov", program.source, &diagnostics.writer, &original_c.writer);
        if (!original_ok) std.debug.print("{s}", .{diagnostics.written()});
        try std.testing.expect(original_ok);
        try std.testing.expect(try emitSource(allocator, "format.nov", terminated, &diagnostics.writer, &formatted_c.writer));
        try std.testing.expectEqualStrings("", diagnostics.written());
        try std.testing.expectEqualStrings(original_c.written(), formatted_c.written());
        var tmp = std.testing.tmpDir(.{});
        defer tmp.cleanup();
        try tmp.dir.writeFile(io, .{ .sub_path = "formatted.c", .data = formatted_c.written() });
        const executable = "formatted" ++ (if (builtin.os.tag == .windows) ".exe" else "");
        const directory = try tmp.dir.realPathFileAlloc(io, ".", allocator);
        defer allocator.free(directory);
        const path = try std.fs.path.join(allocator, &.{ directory, executable });
        defer allocator.free(path);
        for ([_][]const u8{ "-O0", "-O3" }) |mode| {
            var argv: std.ArrayList([]const u8) = .empty;
            defer argv.deinit(allocator);
            if (zig_exe) |zig| try argv.appendSlice(allocator, &.{ zig, "cc" }) else try argv.append(allocator, "cc");
            try argv.appendSlice(allocator, &.{ "-std=c11", mode, "-Wall", "-Wextra", "-Wpedantic", "-Werror", "-fsanitize=undefined", "-fno-sanitize-recover=all", "formatted.c", "-o", executable });
            const compiled = try std.process.run(allocator, io, .{ .argv = argv.items, .cwd = .{ .dir = tmp.dir } });
            defer allocator.free(compiled.stdout);
            defer allocator.free(compiled.stderr);
            if (compiled.term != .exited or compiled.term.exited != 0) std.debug.print("{s}\n", .{compiled.stderr});
            try std.testing.expectEqual(std.process.Child.Term{ .exited = 0 }, compiled.term);
            const ran = try std.process.run(allocator, io, .{ .argv = &.{path}, .cwd = .{ .dir = tmp.dir } });
            defer allocator.free(ran.stdout);
            defer allocator.free(ran.stderr);
            try std.testing.expectEqual(std.process.Child.Term{ .exited = program.code }, ran.term);
            try std.testing.expectEqualStrings(program.output, ran.stdout);
            try std.testing.expectEqualStrings(program.stderr, ran.stderr);
        }
    }
}

test "recursive formatting sorts reports deduplicates inputs and prunes literal exclusions" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.createDirPath(io, "nested/skip");
    try tmp.dir.createDirPath(io, "nested/skip-more");
    for ([_][]const u8{ "z.nov", "a.nov", "nested/b.nov", "nested/skip/no.nov", "nested/skip-more/yes.nov", "ignored.txt", "UPPER.NOV" }) |name| {
        try tmp.dir.writeFile(io, .{ .sub_path = name, .data = "let x=1\n" });
    }
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    const a = try std.fs.path.join(allocator, &.{ root, "a.nov" });
    defer allocator.free(a);
    const skip = try std.fs.path.join(allocator, &.{ root, "nested/skip/../skip" });
    defer allocator.free(skip);
    const skip_z = try std.fs.path.join(allocator, &.{ root, "z.nov" });
    defer allocator.free(skip_z);
    var diagnostics = std.Io.Writer.Allocating.init(allocator);
    defer diagnostics.deinit();
    try std.testing.expect(try formatPaths(io, allocator, &.{ root, a, root }, &.{ skip, skip_z }, &diagnostics.writer, false));
    const expected = try std.fmt.allocPrint(allocator, "{s}/a.nov\n{s}/nested/b.nov\n{s}/nested/skip-more/yes.nov\n", .{ root, root, root });
    defer allocator.free(expected);
    try std.testing.expectEqualStrings(expected, diagnostics.written());
    for ([_][]const u8{ "z.nov", "nested/skip/no.nov", "ignored.txt", "UPPER.NOV" }) |name| {
        const actual = try tmp.dir.readFileAlloc(io, name, allocator, .limited(4096));
        defer allocator.free(actual);
        try std.testing.expectEqualStrings("let x=1\n", actual);
    }
    diagnostics.clearRetainingCapacity();
    try std.testing.expect(try formatPaths(io, allocator, &.{root}, &.{ skip, skip_z }, &diagnostics.writer, true));
    try std.testing.expectEqualStrings("", diagnostics.written());
}

test "formatter empty selections succeed and explicit extensionless files remain supported" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    var diagnostics = std.Io.Writer.Allocating.init(allocator);
    defer diagnostics.deinit();
    try std.testing.expect(try formatPaths(io, allocator, &.{root}, &.{}, &diagnostics.writer, false));
    try tmp.dir.writeFile(io, .{ .sub_path = "source", .data = "let x=1\n" });
    const file = try std.fs.path.join(allocator, &.{ root, "source" });
    defer allocator.free(file);
    try std.testing.expect(try formatPaths(io, allocator, &.{root}, &.{root}, &diagnostics.writer, false));
    try std.testing.expectEqualStrings("", diagnostics.written());
    try std.testing.expect(try formatPaths(io, allocator, &.{ root, file, file }, &.{}, &diagnostics.writer, false));
    const expected = try std.fmt.allocPrint(allocator, "{s}\n", .{file});
    defer allocator.free(expected);
    try std.testing.expectEqualStrings(expected, diagnostics.written());
}

test "recursive check and errors never stop other supported files" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "a.nov", .data = "let x=1\n" });
    try tmp.dir.writeFile(io, .{ .sub_path = "b.nov", .data = "let x = [[1]]\n" });
    try tmp.dir.writeFile(io, .{ .sub_path = "c.nov", .data = "let x=\n" });
    try tmp.dir.writeFile(io, .{ .sub_path = "z.nov", .data = "let z=2\n" });
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    const missing = try std.fs.path.join(allocator, &.{ root, "missing" });
    defer allocator.free(missing);
    var diagnostics = std.Io.Writer.Allocating.init(allocator);
    defer diagnostics.deinit();
    try std.testing.expect(!try formatPaths(io, allocator, &.{ missing, root }, &.{}, &diagnostics.writer, true));
    const before = try tmp.dir.readFileAlloc(io, "z.nov", allocator, .limited(4096));
    defer allocator.free(before);
    try std.testing.expectEqualStrings("let z=2\n", before);
    try std.testing.expect(!try formatPaths(io, allocator, &.{ root, missing }, &.{}, &diagnostics.writer, false));
    const after = try tmp.dir.readFileAlloc(io, "z.nov", allocator, .limited(4096));
    defer allocator.free(after);
    try std.testing.expectEqualStrings("let z = 2\n", after);
    try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "FileNotFound") != null);
    try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "not supported yet") != null);
}

test "formatter preserves file symlinks permissions and avoids directory cycles" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.createDirPath(io, "dir");
    try tmp.dir.writeFile(io, .{ .sub_path = "target.nov", .data = "let x=1\n" });
    try tmp.dir.setFilePermissions(io, "target.nov", .fromMode(0o640), .{});
    try tmp.dir.symLink(io, "target.nov", "a.nov", .{});
    try tmp.dir.symLink(io, "..", "dir/cycle.nov", .{ .is_directory = true });
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    var diagnostics = std.Io.Writer.Allocating.init(allocator);
    defer diagnostics.deinit();
    try std.testing.expect(try formatPaths(io, allocator, &.{root}, &.{}, &diagnostics.writer, false));
    const expected = try std.fmt.allocPrint(allocator, "{s}/a.nov\n", .{root});
    defer allocator.free(expected);
    try std.testing.expectEqualStrings(expected, diagnostics.written());
    const link = try tmp.dir.statFile(io, "a.nov", .{ .follow_symlinks = false });
    try std.testing.expectEqual(.sym_link, link.kind);
    const target = try tmp.dir.statFile(io, "target.nov", .{});
    try std.testing.expectEqual(@as(std.posix.mode_t, 0o640), target.permissions.toMode() & 0o7777);
    const actual = try tmp.dir.readFileAlloc(io, "target.nov", allocator, .limited(4096));
    defer allocator.free(actual);
    try std.testing.expectEqualStrings("let x = 1\n", actual);
}

test "formatter excludes symlink targets and diagnoses broken links while continuing" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "target.nov", .data = "let x=1\n" });
    try tmp.dir.writeFile(io, .{ .sub_path = "z.nov", .data = "let z=2\n" });
    try tmp.dir.symLink(io, "target.nov", "alias.nov", .{});
    try tmp.dir.symLink(io, "absent", "broken.nov", .{});
    try tmp.dir.symLink(io, "loop.nov", "loop.nov", .{});
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    const target = try std.fs.path.join(allocator, &.{ root, "target.nov" });
    defer allocator.free(target);
    var diagnostics = std.Io.Writer.Allocating.init(allocator);
    defer diagnostics.deinit();
    try std.testing.expect(!try formatPaths(io, allocator, &.{root}, &.{target}, &diagnostics.writer, false));
    const actual = try tmp.dir.readFileAlloc(io, "target.nov", allocator, .limited(4096));
    defer allocator.free(actual);
    try std.testing.expectEqualStrings("let x=1\n", actual);
    try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "broken.nov: error:") != null);
    try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "loop.nov: error:") != null);
    try std.testing.expect(std.mem.endsWith(u8, diagnostics.written(), "/z.nov\n"));
}

test "unreadable formatter input reports failure and continues" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "a.nov", .data = "let x=1\n" });
    try tmp.dir.setFilePermissions(io, "a.nov", .fromMode(0), .{});
    defer tmp.dir.setFilePermissions(io, "a.nov", .fromMode(0o600), .{}) catch {};
    if (tmp.dir.openFile(io, "a.nov", .{})) |file| {
        file.close(io);
        return error.SkipZigTest; // Privileged test processes bypass permission denial.
    } else |_| {}
    try tmp.dir.createDirPath(io, "locked");
    try tmp.dir.setFilePermissions(io, "locked", .fromMode(0), .{});
    defer tmp.dir.setFilePermissions(io, "locked", .fromMode(0o700), .{}) catch {};
    try tmp.dir.writeFile(io, .{ .sub_path = "z.nov", .data = "let z=2\n" });
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    var diagnostics = std.Io.Writer.Allocating.init(allocator);
    defer diagnostics.deinit();
    try std.testing.expect(!try formatPaths(io, allocator, &.{root}, &.{}, &diagnostics.writer, false));
    try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "a.nov: error:") != null);
    try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "locked: error:") != null);
    const actual = try tmp.dir.readFileAlloc(io, "z.nov", allocator, .limited(4096));
    defer allocator.free(actual);
    try std.testing.expectEqualStrings("let z = 2\n", actual);
}

test "stdin formatting buffers validation and check never emits source" {
    const allocator = std.testing.allocator;
    const cases = [_]struct { source: []const u8, check: bool = false, valid: bool, output: []const u8 = "" }{
        .{ .source = "let x=1", .valid = true, .output = "let x = 1\n" },
        .{ .source = "", .valid = true },
        .{ .source = "let x = 1\n", .valid = true, .output = "let x = 1\n" },
        .{ .source = "let x=1", .check = true, .valid = false },
        .{ .source = "let x = 1\n", .check = true, .valid = true },
        .{ .source = "let x=", .valid = false },
        .{ .source = "let x=[1]", .valid = true, .output = "let x = [1]\n" },
        .{ .source = "let x=[[1]]", .valid = false },
    };
    for (cases) |case| {
        var input = std.Io.Reader.fixed(case.source);
        var output = std.Io.Writer.Allocating.init(allocator);
        defer output.deinit();
        var diagnostics = std.Io.Writer.Allocating.init(allocator);
        defer diagnostics.deinit();
        try std.testing.expectEqual(case.valid, try formatStdin(allocator, &input, &output.writer, &diagnostics.writer, case.check));
        try std.testing.expectEqualStrings(case.output, output.written());
        if (!case.valid) try std.testing.expect(std.mem.startsWith(u8, diagnostics.written(), "<stdin>:"));
    }
    const oversized = try allocator.alloc(u8, 1024 * 1024 + 1);
    defer allocator.free(oversized);
    @memset(oversized, ' ');
    var too_long = std.Io.Reader.fixed(oversized);
    var discard: std.Io.Writer.Discarding = .init(&.{});
    try std.testing.expectError(error.StreamTooLong, formatStdin(allocator, &too_long, &discard.writer, &discard.writer, false));
    var nul = std.Io.Reader.fixed("let x=1\x00");
    var output = std.Io.Writer.Allocating.init(allocator);
    defer output.deinit();
    var diagnostics = std.Io.Writer.Allocating.init(allocator);
    defer diagnostics.deinit();
    try std.testing.expectError(error.InvalidSource, formatStdin(allocator, &nul, &output.writer, &diagnostics.writer, false));
    try std.testing.expectEqualStrings("", output.written());
}

test "formatting refuses to overwrite a changed source" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const updated = "; edited concurrently\nlet x = 2\n";
    try tmp.dir.writeFile(io, .{ .sub_path = "input.nov", .data = updated });
    const path = try tmp.dir.realPathFileAlloc(io, "input.nov", allocator);
    defer allocator.free(path);
    try std.testing.expectError(error.SourceChanged, replaceFormattedFile(io, allocator, path, "let x=1\n", "let x = 1\n"));
    const actual = try tmp.dir.readFileAlloc(io, "input.nov", allocator, .limited(4096));
    defer allocator.free(actual);
    try std.testing.expectEqualStrings(updated, actual);
}

test "format files atomically and leave rejected input unchanged" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const cases = [_]struct { source: []const u8, check_only: bool = false, valid: bool, expected: []const u8 }{
        .{ .source = "let x=1+2\n", .valid = true, .expected = "let x = 1 + 2\n" },
        .{ .source = "let x=1+2\n", .check_only = true, .valid = false, .expected = "let x=1+2\n" },
        .{ .source = "let x =\n", .valid = false, .expected = "let x =\n" },
        .{ .source = "let x=foo(1)\n", .valid = true, .expected = "let x = foo(1)\n" },
        .{ .source = "let f:()->void={return}\n", .valid = true, .expected = "let f: () -> void = {\n    return\n}\n" },
        .{ .source = "let x=foo([1])\n", .valid = true, .expected = "let x = foo([1])\n" },
        .{ .source = "let x = foo([[1]])\n", .valid = false, .expected = "let x = foo([[1]])\n" },
        .{ .source = "let f:()->void={let mut a:[]int=[1]\na[-1]=2\na += []}\n", .valid = true, .expected = "let f: () -> void = {\n    let mut a: []int = [1]\n    a[-1] = 2\n    a += []\n}\n" },
        .{ .source = "let f = loop true {break 1}\n", .valid = true, .expected = "let f = loop true {\n    break 1\n}\n" },
        .{ .source = "let f = loop true {break :outer}\n", .valid = false, .expected = "let f = loop true {break :outer}\n" },
    };
    for (cases) |case| {
        try tmp.dir.writeFile(io, .{ .sub_path = "input.nov", .data = case.source });
        const path = try tmp.dir.realPathFileAlloc(io, "input.nov", allocator);
        defer allocator.free(path);
        var diagnostics = std.Io.Writer.Allocating.init(allocator);
        defer diagnostics.deinit();
        try std.testing.expectEqual(case.valid, try processFile(io, allocator, path, &diagnostics.writer, true, case.check_only, false));
        const actual = try tmp.dir.readFileAlloc(io, "input.nov", allocator, .limited(4096));
        defer allocator.free(actual);
        try std.testing.expectEqualStrings(case.expected, actual);
    }
}

test "semantic file checks reject errors and deferred forms without writing" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const cases = [_]struct { source: []const u8, message: ?[]const u8 = null }{
        .{ .source = "let base: i32 = 42\nlet first: (x: i32) -> i32 = second(x)\nlet second: (x: i32) -> i32 = first(x)\nlet empty: () -> void = {}" },
        .{ .source = "let x: i128 = 1" }, // No C backend width restriction.
        .{ .source = "let broken =", .message = "expected" },
        .{ .source = "let x = 1\nlet x = 2", .message = "first declaration here" },
        .{ .source = "let x = absent", .message = "unknown name 'absent'" },
        .{ .source = "let x = y\nlet y = x", .message = "cyclic value dependency" },
        .{ .source = "let x: i8 = 128", .message = "type annotation here" },
        .{ .source = "let f: () -> i32 = true", .message = "does not match" },
        .{ .source = "let f: (x: i32) -> i32 = x\nlet g: () -> i32 = f()", .message = "argument count" },
        .{ .source = "let x = 1", .message = "integer literal needs an expected type" },
        .{ .source = "let yes = true\nlet text = \"literal\"\nlet n = @as(int, 42)" },
        .{ .source = "let x = @as(i128, -170141183460469231731687303715884105728)" },
        .{ .source = "let x = @as(u128, 340282366920938463463374607431768211455)" },
        .{ .source = "let mut x: i32 = 1", .message = "lowering this source form" },
        .{ .source = "let f: (x: bool) -> bool = x and {false or x}" },
        .{ .source = "let f: () -> bool = false and absent", .message = "unknown name" },
        .{ .source = "let f: (x: i32) -> bool = true or x", .message = "does not match" },
    };
    for (cases) |case| {
        try tmp.dir.writeFile(io, .{ .sub_path = "input.nov", .data = case.source });
        const path = try tmp.dir.realPathFileAlloc(io, "input.nov", allocator);
        defer allocator.free(path);
        var diagnostics = std.Io.Writer.Allocating.init(allocator);
        defer diagnostics.deinit();
        try std.testing.expectEqual(case.message == null, try processFile(io, allocator, path, &diagnostics.writer, false, false, true));
        if (case.message) |message| {
            try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), message) != null);
            try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "input.nov:") != null);
            try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "^") != null);
        } else try std.testing.expectEqualStrings("", diagnostics.written());
        const actual = try tmp.dir.readFileAlloc(io, "input.nov", allocator, .limited(4096));
        defer allocator.free(actual);
        try std.testing.expectEqualStrings(case.source, actual);
    }
}

test "build and run integrate discovery, preserve outputs, and clean temporary files" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{ .iterate = true });
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    try tmp.dir.writeFile(io, .{ .sub_path = "input.nov", .data = "let main: () -> void = {}\n" });
    const input = try tmp.dir.realPathFileAlloc(io, "input.nov", allocator);
    defer allocator.free(input);
    var env = std.process.Environ.Map.init(allocator);
    defer env.deinit();
    try env.put("TMPDIR", root);
    var compilers: [3][:0]const u8 = undefined;
    var count: usize = 0;
    defer for (compilers[0..count]) |path| allocator.free(path);
    for (0..3) |index| {
        const name = try std.fmt.allocPrint(allocator, "compiler {d};literal", .{index});
        defer allocator.free(name);
        const script = try std.fmt.allocPrint(
            allocator,
            "#!/bin/sh\n[ \"$1\" = -std=c11 ] && [ \"$2\" = -O3 ] && [ -f \"$3\" ] && [ \"$4\" = -o ] || exit 90\nprintf '#!/bin/sh\\nexit {d}\\n' > \"$5\"\n/bin/chmod 700 \"$5\"\n",
            .{37 + index},
        );
        defer allocator.free(script);
        try tmp.dir.writeFile(io, .{ .sub_path = name, .data = script });
        try tmp.dir.setFilePermissions(io, name, .fromMode(0o700), .{});
        compilers[index] = try tmp.dir.realPathFileAlloc(io, name, allocator);
        count += 1;
    }
    try env.put("NOV_CC", compilers[1]);
    try env.put("CC", compilers[2]);
    var diagnostics = std.Io.Writer.Allocating.init(allocator);
    defer diagnostics.deinit();
    try std.testing.expectEqual(@as(u8, 37), try buildFile(io, allocator, &env, input, null, compilers[0], .release, true, &diagnostics.writer));
    try std.testing.expectEqual(@as(u8, 38), try buildFile(io, allocator, &env, input, null, "/nov-missing/explicit", .release, true, &diagnostics.writer));
    try env.put("NOV_CC", "");
    try std.testing.expectEqual(@as(u8, 39), try buildFile(io, allocator, &env, input, null, null, .release, true, &diagnostics.writer));
    const output = try std.fs.path.join(allocator, &.{ root, "output with spaces;literal" });
    defer allocator.free(output);
    try std.testing.expectEqual(@as(u8, 0), try buildFile(io, allocator, &env, input, output, compilers[0], .release, false, &diagnostics.writer));
    try std.testing.expectEqual(@as(u8, 37), try Driver.run(io, output, &diagnostics.writer));
    try tmp.dir.writeFile(io, .{ .sub_path = "failure", .data = "#!/bin/sh\nexit 51\n" });
    try tmp.dir.setFilePermissions(io, "failure", .fromMode(0o700), .{});
    const failure = try tmp.dir.realPathFileAlloc(io, "failure", allocator);
    defer allocator.free(failure);
    try std.testing.expectEqual(@as(u8, 1), try buildFile(io, allocator, &env, input, output, failure, .release, false, &diagnostics.writer));
    try std.testing.expectEqual(@as(u8, 37), try Driver.run(io, output, &diagnostics.writer));
    try std.testing.expectEqual(@as(u8, 1), try buildFile(io, allocator, &env, input, input, compilers[0], .release, false, &diagnostics.writer));
    const source = try tmp.dir.readFileAlloc(io, "input.nov", allocator, .limited(1024));
    defer allocator.free(source);
    try std.testing.expectEqualStrings("let main: () -> void = {}\n", source);

    const imported_source = "@[public]\nlet value: () -> bool = true\n";
    try tmp.dir.writeFile(io, .{ .sub_path = "imported.nov", .data = imported_source });
    try tmp.dir.writeFile(io, .{ .sub_path = "input.nov", .data = "let imported = @import(\"imported.nov\")\nlet main: () = @println(imported.value())\n" });
    try tmp.dir.symLink(io, "imported.nov", "imported-link.nov", .{});
    for ([_][]const u8{ "imported.nov", "imported-link.nov" }) |name| {
        const imported_output = try std.fs.path.join(allocator, &.{ root, name });
        defer allocator.free(imported_output);
        var guard_diagnostics = std.Io.Writer.Allocating.init(allocator);
        defer guard_diagnostics.deinit();
        try std.testing.expectEqual(@as(u8, 1), try buildFile(io, allocator, &env, input, imported_output, compilers[0], .release, false, &guard_diagnostics.writer));
        try std.testing.expectEqualStrings("error: output path refers to the input source file\n", guard_diagnostics.written());
        const preserved = try tmp.dir.readFileAlloc(io, name, allocator, .limited(1024));
        defer allocator.free(preserved);
        try std.testing.expectEqualStrings(imported_source, preserved);
    }
    const link = try tmp.dir.statFile(io, "imported-link.nov", .{ .follow_symlinks = false });
    try std.testing.expectEqual(.sym_link, link.kind);
    var iterator = tmp.dir.iterate();
    while (try iterator.next(io)) |entry| {
        try std.testing.expect(!std.mem.startsWith(u8, entry.name, "nov-"));
    }
}

test {
    _ = Compilation;
    _ = Ast;
    _ = Ir;
    _ = Module;
    _ = Dependencies;
    _ = TypeResolver;
    _ = TypeAliases;
    _ = Enums;
    _ = Visibility;
    _ = Exports;
    _ = Externs;
    _ = Sema;
    _ = Literal;
    _ = BodyChecker;
    _ = Lowering;
    _ = CEmitter;
    _ = GcRuntime;
    _ = Tokenizer;
    _ = string_literal;
    _ = Driver;
    _ = TestRunner;
    _ = Diagnostics;
    _ = Formatter;
}
