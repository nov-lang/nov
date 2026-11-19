const std = @import("std");
const builtin = @import("builtin");
const Compilation = @import("Compilation.zig");
const Lowering = @import("Lowering.zig");
const CEmitter = @import("CEmitter.zig");
const Diagnostics = @import("Diagnostics.zig");
const Driver = @import("Driver.zig");
const Ir = @import("Ir.zig");

pub const Options = struct {
    discovery: Driver.Discovery = .{},
    filter: ?[]const u8 = null,
    timeout_ms: ?u32 = null,
    temp_root: []const u8 = if (builtin.os.tag == .windows) "." else "/tmp",
};

pub fn parseTimeout(raw: []const u8) error{InvalidTimeout}!u32 {
    if (raw.len == 0) return error.InvalidTimeout;
    for (raw) |byte| if (byte < '0' or byte > '9') return error.InvalidTimeout;
    const milliseconds = std.fmt.parseInt(u32, raw, 10) catch return error.InvalidTimeout;
    return if (milliseconds == 0) error.InvalidTimeout else milliseconds;
}

test "TestRunner timeout parsing accepts only positive bounded decimal milliseconds" {
    try std.testing.expectEqual(@as(u32, 1), try parseTimeout("001"));
    try std.testing.expectEqual(@as(u32, 4294967295), try parseTimeout("4294967295"));
    for ([_][]const u8{ "", "0", "00", "-1", "+1", "1.5", " 1", "1 ", "1_000", "4294967296", "999999999999999999999" }) |raw|
        try std.testing.expectError(error.InvalidTimeout, parseTimeout(raw));
}

/// Compile once, then run each root-source test in its own process. Child output
/// is inherited; process isolation does not isolate filesystem side effects.
pub fn run(io: std.Io, allocator: std.mem.Allocator, comp: *Compilation, options: Options, report: *std.Io.Writer, diagnostics: *std.Io.Writer) !u8 {
    if (comp.hasErrors()) {
        try Diagnostics.renderCompilation(comp, diagnostics);
        return 1;
    }
    const root = comp.getModule(comp.root orelse return error.MissingRoot).?;
    if (options.timeout_ms == 0) return error.InvalidTimeout;
    const previous_mode = root.tests_enabled;
    const previous_filter = root.test_filter;
    root.tests_enabled = true;
    root.test_filter = options.filter;
    defer {
        root.tests_enabled = previous_mode;
        root.test_filter = previous_filter;
    }
    var result = try Lowering.lowerCompilation(comp);
    defer result.deinit();
    if (comp.hasErrors()) {
        try Diagnostics.renderCompilation(comp, diagnostics);
        return 1;
    }

    var tests: std.ArrayList(Ir.FunctionId) = .empty;
    defer tests.deinit(allocator);
    // Module declarations retain source order; global IDs may include imports
    // and demand-created declarations and must not determine report order.
    for (root.declarations.items, 0..) |declaration, local| {
        if (!declaration.is_test or !root.testSelected(declaration.name)) continue;
        const id = comp.declaration_ids.get(.{ .source = root.source.id, .local = @fromBackingInt(@intCast(local)) }).?;
        const entry = result.getCompilationEntry(id).?;
        try tests.append(allocator, entry.function orelse return error.MissingTestFunction);
    }
    var diagnostic: CEmitter.Diagnostic = undefined;
    const source = CEmitter.emitTestProgram(allocator, &result.ir, tests.items, &diagnostic) catch |err| switch (err) {
        error.UnsupportedIntegerWidth, error.MissingEntrypoint, error.InvalidEntrypoint, error.InvalidTestEntrypoint, error.ExportSymbolCollision, error.ExternSymbolCollision => {
            try Diagnostics.renderCompilationBackend(comp, diagnostic, diagnostics);
            return 1;
        },
        else => return err,
    };
    defer allocator.free(source);

    const workspace = try Driver.Workspace.init(io, allocator, options.temp_root);
    defer workspace.deinit(io);
    const c_path = try workspace.file("tests.c");
    defer allocator.free(c_path);
    const exe_path = try workspace.file(if (builtin.os.tag == .windows) "tests.exe" else "tests");
    defer allocator.free(exe_path);
    try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = c_path, .data = source });
    if (!try Driver.compile(io, options.discovery, c_path, exe_path, diagnostics)) return 1;

    var passed: usize = 0;
    for (tests.items, 0..) |id, index| {
        const name = result.ir.getFunction(id).?.name;
        var index_buffer: [32]u8 = undefined;
        const argument = try std.fmt.bufPrint(&index_buffer, "{d}", .{index});
        try report.flush();
        try diagnostics.flush();
        var child = std.process.spawn(io, .{ .argv = &.{ exe_path, argument }, .stdin = .ignore }) catch |err| {
            try report.print("FAIL {s} (cannot start: {s})\n", .{ name, @errorName(err) });
            try report.flush();
            continue;
        };
        defer child.kill(io);
        const term = waitForTest(io, &child, options.timeout_ms) catch |err| {
            child.kill(io);
            if (err == error.TestTimedOut) {
                try report.print("FAIL {s} (timeout after {d} ms)\n", .{ name, options.timeout_ms.? });
            } else try report.print("FAIL {s} (cannot wait: {s})\n", .{ name, @errorName(err) });
            try report.flush();
            continue;
        };
        switch (term) {
            .exited => |code| if (code == 0) {
                passed += 1;
                try report.print("PASS {s}\n", .{name});
            } else {
                try report.print("FAIL {s} (exit {d})\n", .{ name, code });
            },
            .signal => |signal| try report.print("FAIL {s} (signal {d})\n", .{ name, @backingInt(signal) }),
            else => try report.print("FAIL {s} ({s})\n", .{ name, @tagName(term) }),
        }
        try report.flush();
    }
    const failed = tests.items.len - passed;
    try report.print("{d} passed; {d} failed; {d} total\n", .{ passed, failed, tests.items.len });
    try report.flush();
    return if (failed == 0) 0 else 1;
}

fn waitForTest(io: std.Io, child: *std.process.Child, timeout_ms: ?u32) !std.process.Child.Term {
    const milliseconds = timeout_ms orelse return child.wait(io);
    const start = std.Io.Clock.awake.now(io);
    const limit = std.Io.Duration.fromMilliseconds(milliseconds);
    while (true) {
        if (try pollTest(io, child)) |term| return term;
        const elapsed = start.durationTo(std.Io.Clock.awake.now(io));
        if (elapsed.nanoseconds >= limit.nanoseconds) return error.TestTimedOut;
        try io.sleep(.{ .nanoseconds = @min(limit.nanoseconds - elapsed.nanoseconds, 10 * std.time.ns_per_ms) }, .awake);
    }
}

fn pollTest(io: std.Io, child: *std.process.Child) !?std.process.Child.Term {
    if (builtin.os.tag == .windows) {
        const windows = std.os.windows;
        const immediate: windows.LARGE_INTEGER = 0;
        return switch (windows.ntdll.NtWaitForSingleObject(child.id.?, .FALSE, &immediate)) {
            .WAIT_0 => try child.wait(io),
            .TIMEOUT => null,
            else => error.ProcessPollFailed,
        };
    }
    // This runner inherits stdout/stderr, ignores stdin and requests no rusage.
    // Reaping directly therefore leaves no pipes or resource data to clean up.
    std.debug.assert(child.stdin == null and child.stdout == null and child.stderr == null);
    std.debug.assert(!child.request_resource_usage_statistics);
    var status: c_int = 0;
    const waited = std.posix.system.waitpid(child.id.?, &status, std.posix.W.NOHANG);
    switch (std.posix.errno(waited)) {
        .SUCCESS => {},
        .INTR => return null,
        else => return error.ProcessPollFailed,
    }
    if (waited == 0) return null;
    const bits: u32 = @bitCast(status);
    if (std.posix.W.IFEXITED(bits)) {
        child.id = null;
        return .{ .exited = std.posix.W.EXITSTATUS(bits) };
    }
    if (std.posix.W.IFSIGNALED(bits)) {
        child.id = null;
        return .{ .signal = std.posix.W.TERMSIG(bits) };
    }
    return .{ .unknown = bits };
}

fn fakeCompiler(dir: std.Io.Dir, child: []const u8, fail: bool) !void {
    const allocator = std.testing.allocator;
    const script = try std.fmt.allocPrint(
        allocator,
        "#!/bin/sh\ncd -- \"$(dirname -- \"$0\")\" || exit 90\nprintf 'compile\\n' >> compiles\nprintf '%s\\n' \"$@\" > flags\nwhile [ \"$1\" != -o ]; do shift; done\nshift\nprintf '%s' \"$1\" > executable-path\n{s}\ncat > \"$1\" <<'NOV_CHILD'\n#!/bin/sh\ncd -- \"$(dirname -- \"$0\")/..\" || exit 91\nprintf '%s\\n' \"$1\" >> runs\n{s}\nNOV_CHILD\nchmod 700 \"$1\"\n",
        .{ if (fail) "exit 31" else "", child },
    );
    defer allocator.free(script);
    try dir.writeFile(std.testing.io, .{ .sub_path = "compiler with spaces;literal", .data = script });
    try dir.setFilePermissions(std.testing.io, "compiler with spaces;literal", .fromMode(0o700), .{});
}

fn expectFile(dir: std.Io.Dir, path: []const u8, expected: []const u8) !void {
    const text = try dir.readFileAlloc(std.testing.io, path, std.testing.allocator, .limited(1024 * 1024));
    defer std.testing.allocator.free(text);
    try std.testing.expectEqualStrings(expected, text);
}

fn expectCleaned(dir: std.Io.Dir) !void {
    const path = try dir.readFileAlloc(std.testing.io, "executable-path", std.testing.allocator, .limited(4096));
    defer std.testing.allocator.free(path);
    try std.testing.expectError(error.FileNotFound, std.Io.Dir.cwd().openDir(std.testing.io, std.fs.path.dirname(path).?, .{}));
}

const ordered_source =
    \\@[test]
    \\let zebra: () = {}
    \\@[test]
    \\let main: () = {}
    \\@[test]
    \\let alpha: () = {}
;

test "TestRunner compiles once per mode, continues after nonzero and signal, and cleans workspaces" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    for ([_]Driver.Mode{ .release, .debug }) |mode| {
        var tmp = std.testing.tmpDir(.{});
        defer tmp.cleanup();
        try fakeCompiler(tmp.dir, "case \"$1\" in 0) exit 7;; 1) kill -KILL $$;; 2) exit 0;; *) exit 99;; esac", false);
        const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
        defer allocator.free(root);
        const cc = try tmp.dir.realPathFileAlloc(io, "compiler with spaces;literal", allocator);
        defer allocator.free(cc);
        var comp = Compilation.init(allocator);
        defer comp.deinit();
        comp.setRoot(try comp.addSource("/tests.nov", ordered_source));
        var report = std.Io.Writer.Allocating.init(allocator);
        defer report.deinit();
        var diagnostics = std.Io.Writer.Allocating.init(allocator);
        defer diagnostics.deinit();
        try std.testing.expectEqual(@as(u8, 1), try run(io, allocator, &comp, .{ .temp_root = root, .discovery = .{ .cc = cc, .mode = mode } }, &report.writer, &diagnostics.writer));
        try std.testing.expectEqualStrings("FAIL zebra (exit 7)\nFAIL main (signal 9)\nPASS alpha\n1 passed; 2 failed; 3 total\n", report.written());
        try std.testing.expectEqualStrings("", diagnostics.written());
        try expectFile(tmp.dir, "compiles", "compile\n");
        try expectFile(tmp.dir, "runs", "0\n1\n2\n");
        const flags = try tmp.dir.readFileAlloc(io, "flags", allocator, .limited(4096));
        defer allocator.free(flags);
        try std.testing.expect(std.mem.startsWith(u8, flags, if (mode == .debug) "-std=c11\n-O0\n-g\n" else "-std=c11\n-O3\n"));
        try expectCleaned(tmp.dir);
        try std.testing.expect(!comp.getModule(comp.root.?).?.tests_enabled);
    }
}

test "TestRunner empty suites compile, compiler failures never run, and both restore modes" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    for ([_]bool{ false, true }) |fail| {
        for ([_]bool{ false, true }) |previous_mode| {
            var tmp = std.testing.tmpDir(.{});
            defer tmp.cleanup();
            try fakeCompiler(tmp.dir, "exit 0", fail);
            const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
            defer allocator.free(root);
            const cc = try tmp.dir.realPathFileAlloc(io, "compiler with spaces;literal", allocator);
            defer allocator.free(cc);
            var comp = Compilation.init(allocator);
            defer comp.deinit();
            comp.setRoot(try comp.addSource("/empty.nov", if (fail) ordered_source else ""));
            comp.getModule(comp.root.?).?.tests_enabled = previous_mode;
            var report = std.Io.Writer.Allocating.init(allocator);
            defer report.deinit();
            var diagnostics = std.Io.Writer.Allocating.init(allocator);
            defer diagnostics.deinit();
            try std.testing.expectEqual(@as(u8, if (fail) 1 else 0), try run(io, allocator, &comp, .{ .temp_root = root, .discovery = .{ .cc = cc } }, &report.writer, &diagnostics.writer));
            try std.testing.expectEqualStrings(if (fail) "" else "0 passed; 0 failed; 0 total\n", report.written());
            if (fail) try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "failed (exited 31)") != null);
            try std.testing.expectEqual(previous_mode, comp.getModule(comp.root.?).?.tests_enabled);
            try expectFile(tmp.dir, "compiles", "compile\n");
            try std.testing.expectError(error.FileNotFound, tmp.dir.openFile(io, "runs", .{}));
            try expectCleaned(tmp.dir);
        }
    }
}

test "TestRunner selects only root tests in source order and supports repeated ordinary lowering" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try fakeCompiler(tmp.dir, "exit 0", false);
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    const cc = try tmp.dir.realPathFileAlloc(io, "compiler with spaces;literal", allocator);
    defer allocator.free(cc);
    var comp = Compilation.init(allocator);
    defer comp.deinit();
    const imported = try comp.addSource("/library.nov", "@[test]\nlet ignored: (x: Missing) -> Missing = unknown\n");
    comp.getModule(imported).?.tests_enabled = true;
    const root_id = try comp.addSource("/root.nov", "let library = @import(\"library.nov\")\n" ++ ordered_source);
    comp.setRoot(root_id);
    const module = comp.getModule(root_id).?;
    const local = module.lookup("library").?;
    try comp.imports.append(allocator, .{
        .declaration = comp.declaration_ids.get(.{ .source = root_id, .local = local }).?,
        .target = imported,
        .named = false,
        .span = module.getDeclaration(local).?.name_span,
    });
    for (0..2) |_| {
        var report = std.Io.Writer.Allocating.init(allocator);
        defer report.deinit();
        var diagnostics = std.Io.Writer.Allocating.init(allocator);
        defer diagnostics.deinit();
        try std.testing.expectEqual(@as(u8, 0), try run(io, allocator, &comp, .{ .temp_root = root, .discovery = .{ .cc = cc } }, &report.writer, &diagnostics.writer));
        try std.testing.expectEqualStrings("PASS zebra\nPASS main\nPASS alpha\n3 passed; 0 failed; 3 total\n", report.written());
        try std.testing.expectEqualStrings("", diagnostics.written());
        try std.testing.expect(!comp.getModule(comp.root.?).?.tests_enabled);
        try std.testing.expect(comp.getModule(imported).?.tests_enabled);
        try std.testing.expect(comp.root_main == null);
        var normal = try Lowering.lowerCompilation(&comp);
        defer normal.deinit();
        try std.testing.expect(!comp.hasErrors());
        try std.testing.expectEqual(@as(usize, 0), normal.ir.functions.items.len);
        try expectCleaned(tmp.dir);
    }
    try expectFile(tmp.dir, "compiles", "compile\ncompile\n");
    try expectFile(tmp.dir, "runs", "0\n1\n2\n0\n1\n2\n");
}

test "TestRunner spawn failures continue and workspace errors restore mode" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{ .iterate = true });
    defer tmp.cleanup();
    // A compiler reporting success without creating an executable exercises
    // parent-side spawn failures without relying on a particular C compiler.
    try fakeCompiler(tmp.dir, "exit 0", true);
    const cc = try tmp.dir.realPathFileAlloc(io, "compiler with spaces;literal", allocator);
    defer allocator.free(cc);
    try tmp.dir.writeFile(io, .{ .sub_path = "compiler with spaces;literal", .data = "#!/bin/sh\nexit 0\n" });
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    const unavailable = try std.fs.path.join(allocator, &.{ root, "absent" });
    defer allocator.free(unavailable);
    var comp = Compilation.init(allocator);
    defer comp.deinit();
    comp.setRoot(try comp.addSource("/root.nov", ordered_source));
    var report = std.Io.Writer.Allocating.init(allocator);
    defer report.deinit();
    var diagnostics = std.Io.Writer.Allocating.init(allocator);
    defer diagnostics.deinit();
    for ([_]bool{ false, true }) |previous_mode| {
        comp.getModule(comp.root.?).?.tests_enabled = previous_mode;
        try std.testing.expectError(error.FileNotFound, run(io, allocator, &comp, .{ .temp_root = unavailable }, &report.writer, &diagnostics.writer));
        try std.testing.expectEqual(previous_mode, comp.getModule(comp.root.?).?.tests_enabled);
    }
    try std.testing.expectEqual(@as(u8, 1), try run(io, allocator, &comp, .{ .temp_root = root, .discovery = .{ .cc = cc } }, &report.writer, &diagnostics.writer));
    try std.testing.expectEqualStrings("FAIL zebra (cannot start: FileNotFound)\nFAIL main (cannot start: FileNotFound)\nFAIL alpha (cannot start: FileNotFound)\n0 passed; 3 failed; 3 total\n", report.written());
    try std.testing.expect(comp.getModule(comp.root.?).?.tests_enabled);
    var iterator = tmp.dir.iterate();
    while (try iterator.next(io)) |entry| try std.testing.expect(!std.mem.startsWith(u8, entry.name, "nov-"));
}

test "TestRunner filters before semantic demands preserves order and restores selection" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try fakeCompiler(tmp.dir, "exit 0", false);
    const root_path = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root_path);
    const cc = try tmp.dir.realPathFileAlloc(io, "compiler with spaces;literal", allocator);
    defer allocator.free(cc);
    var comp = Compilation.init(allocator);
    defer comp.deinit();
    comp.setRoot(try comp.addSource("/filtered.nov", ordered_source ++ "\n@[test]\nlet excluded:(value:Missing)=unknown\n"));
    const root = comp.getModule(comp.root.?).?;
    root.tests_enabled = true;
    root.test_filter = "previous";
    for ([_]struct { filter: []const u8, output: []const u8 }{
        .{ .filter = "alpha", .output = "PASS alpha\n1 passed; 0 failed; 1 total\n" },
        .{ .filter = "a", .output = "PASS zebra\nPASS main\nPASS alpha\n3 passed; 0 failed; 3 total\n" },
        .{ .filter = "ALPHA", .output = "0 passed; 0 failed; 0 total\n" },
    }) |case| {
        var report = std.Io.Writer.Allocating.init(allocator);
        defer report.deinit();
        var diagnostics = std.Io.Writer.Allocating.init(allocator);
        defer diagnostics.deinit();
        try std.testing.expectEqual(@as(u8, 0), try run(io, allocator, &comp, .{ .filter = case.filter, .temp_root = root_path, .discovery = .{ .cc = cc } }, &report.writer, &diagnostics.writer));
        try std.testing.expectEqualStrings(case.output, report.written());
        try std.testing.expectEqualStrings("", diagnostics.written());
        try std.testing.expect(root.tests_enabled);
        try std.testing.expectEqualStrings("previous", root.test_filter.?);
        try expectCleaned(tmp.dir);
    }
    root.tests_enabled = false;
    var ordinary = try Lowering.lowerCompilation(&comp);
    defer ordinary.deinit();
    try std.testing.expect(!comp.hasErrors());
    try std.testing.expectEqual(@as(usize, 0), ordinary.ir.functions.items.len);
}

test "TestRunner timeout kills and reaps the child then continues subsequent tests" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try fakeCompiler(tmp.dir, "case \"$1\" in 0) while :; do :; done;; 1) exit 0;; 2) exit 7;; esac", false);
    const root_path = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root_path);
    const cc = try tmp.dir.realPathFileAlloc(io, "compiler with spaces;literal", allocator);
    defer allocator.free(cc);
    var comp = Compilation.init(allocator);
    defer comp.deinit();
    comp.setRoot(try comp.addSource("/timeout.nov", ordered_source));
    var report = std.Io.Writer.Allocating.init(allocator);
    defer report.deinit();
    var diagnostics = std.Io.Writer.Allocating.init(allocator);
    defer diagnostics.deinit();
    try std.testing.expectEqual(@as(u8, 1), try run(io, allocator, &comp, .{ .timeout_ms = 250, .temp_root = root_path, .discovery = .{ .cc = cc } }, &report.writer, &diagnostics.writer));
    try std.testing.expectEqualStrings("FAIL zebra (timeout after 250 ms)\nPASS main\nFAIL alpha (exit 7)\n1 passed; 2 failed; 3 total\n", report.written());
    try std.testing.expectEqualStrings("", diagnostics.written());
    try expectFile(tmp.dir, "runs", "0\n1\n2\n");
    try expectCleaned(tmp.dir);
}

test "TestRunner rejects frontend semantic and backend errors before compiling and restores mode" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    const cases = [_][]const u8{
        "let =\n",
        "@[test]\nlet bad: () = unknown\n",
        "@[test]\nlet bad: (x: uint) = {}\n",
        "let wide: (x: u7) -> u7 = x\n",
    };
    for (cases) |source| {
        for ([_]bool{ false, true }) |previous_mode| {
            var comp = Compilation.init(allocator);
            defer comp.deinit();
            comp.setRoot(try comp.addSource("/errors.nov", source));
            comp.getModule(comp.root.?).?.tests_enabled = previous_mode;
            var report = std.Io.Writer.Allocating.init(allocator);
            defer report.deinit();
            var diagnostics = std.Io.Writer.Allocating.init(allocator);
            defer diagnostics.deinit();
            // A nonexistent workspace root also proves compilation was not reached.
            try std.testing.expectEqual(@as(u8, 1), try run(io, allocator, &comp, .{ .temp_root = "/nov-missing-test-root" }, &report.writer, &diagnostics.writer));
            try std.testing.expectEqual(previous_mode, comp.getModule(comp.root.?).?.tests_enabled);
            try std.testing.expectEqualStrings("", report.written());
            try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "/errors.nov:") != null);
        }
    }
}
