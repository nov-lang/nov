const std = @import("std");
const builtin = @import("builtin");
const v = @import("value.zig");
const Object = v.Object;
const VirtualMachine = @import("vm.zig").VM;
const log = std.log.scoped(.GC);

const GarbageCollector = @This();

allocator: std.mem.Allocator,
bytes_allocated: usize,
next_gc: usize,
vm: *VirtualMachine,
gray_stack: std.ArrayListUnmanaged(*Object),
head: ?*Object,

const next_gc_factor = 2; // TODO

// TODO: https://craftinginterpreters.com/garbage-collection.html#garbage-collection-bugs
// TODO: 2. and 3. of https://craftinginterpreters.com/garbage-collection.html#challenges

pub fn init(allocator: std.mem.Allocator, vm: *VirtualMachine) GarbageCollector {
    return .{
        .allocator = allocator,
        .bytes_allocated = 0,
        .next_gc = if (builtin.mode == .Debug) 1024 else 1024 * 1024, // TODO
        .vm = vm,
        .gray_stack = .{},
        .head = null,
    };
}

pub fn deinit(self: *GarbageCollector) void {
    _ = self;
    // TODO
}

pub fn create(self: *GarbageCollector, comptime T: type) !*T {
    self.bytes_allocated += @sizeOf(T);
    if (self.bytes_allocated >= self.next_gc) {
        try self.collectGarbage();
    }
    const ptr = try self.allocator.create(T);
    if (builtin.mode == .Debug) {
        log.info("alloc {} at {*} (now {}/{})", .{
            std.fmt.fmtIntSizeDec(@sizeOf(T)),
            ptr,
            std.fmt.fmtIntSizeDec(self.bytes_allocated),
            std.fmt.fmtIntSizeDec(self.next_gc),
        });
    }
    return ptr;
}

pub fn alloc(self: *GarbageCollector, comptime T: type, count: usize) ![]T {
    self.bytes_allocated += @sizeOf(T) * count;
    if (self.bytes_allocated >= self.next_gc) {
        try self.collectGarbage();
    }
    const ptr = try self.allocator.alloc(T, count);
    if (builtin.mode == .Debug) {
        log.info("alloc {} at {*} (now {}/{})", .{
            std.fmt.fmtIntSizeDec(@sizeOf(T) * count),
            ptr,
            std.fmt.fmtIntSizeDec(self.bytes_allocated),
            std.fmt.fmtIntSizeDec(self.next_gc),
        });
    }
    return ptr;
}

// pub fn allocString(self: *GarbageCollector, bytes: []const u8) *v.String {
//     if (strings.get

// }

// TODO: use Object.create instead
pub fn createObject(self: *GarbageCollector, comptime tag: Object.Tag, data: tag.Type()) !*tag.Type() {
    const obj = try self.create(tag.Type());
    obj.* = data;
    obj.obj().setNext(self.head);
    self.head = obj.obj();
    return obj;
}

fn destroy(self: *GarbageCollector, comptime T: type, ptr: *T) void {
    if (builtin.mode == .Debug) {
        log.info("free {} at {*} (now {}/{})", .{
            std.fmt.fmtIntSizeDec(@sizeOf(T)),
            ptr,
            std.fmt.fmtIntSizeDec(self.bytes_allocated),
            std.fmt.fmtIntSizeDec(self.next_gc),
        });
    }
    self.bytes_allocated -= @sizeOf(T);
    self.allocator.destroy(ptr);
}

fn free(self: *GarbageCollector, comptime T: type, ptr: []const T) void {
    if (builtin.mode == .Debug) {
        log.info("free {} at {*} (now {}/{})", .{
            std.fmt.fmtIntSizeDec(@sizeOf(T) * ptr.len),
            ptr,
            std.fmt.fmtIntSizeDec(self.bytes_allocated),
            std.fmt.fmtIntSizeDec(self.next_gc),
        });
    }
    self.bytes_allocated -= @sizeOf(T) * ptr.len;
    self.allocator.free(ptr);
}

fn destroyObject(self: *GarbageCollector, obj: *Object) void {
    _ = self;
    _ = obj;
    // TODO
}

fn markRoots(self: *GarbageCollector) !void {
    for (self.vm.stack.slice()) |*slot| {
        try markValue(slot);
    }

    // TODO: https://craftinginterpreters.com/garbage-collection.html#less-obvious-roots

    var it = self.vm.globals.valueIterator();
    while (it.next()) |global| {
        // TODO: mark key?
        try self.markValue(global);
    }
}

fn markValue(self: *GarbageCollector, value: v.Value) !void {
    // TODO: how to know if it's an object?
    // mark one bit of the value to know if it's an object
    // so we have 63 bits int...
    if (value.is(.obj)) {
        try self.markObject(value.as(.obj));
    }
}

fn markObject(self: *GarbageCollector, obj: *Object) !void {
    if (obj.is_marked) {
        return;
    }

    if (builtin.mode == .Debug) {
        log.info("marking {*}", .{obj});
    }
    obj.is_marked = true;
    try self.gray_stack.append(self.allocator, obj);
}

fn traceReferences(self: *GarbageCollector) !void {
    for (self.gray_stack.items) |obj| {
        try self.blackenObject(obj);
    }
    self.gray_stack.clearRetainingCapacity();
}

fn blackenObject(self: *GarbageCollector, obj: *Object) !void {
    if (builtin.mode == .Debug) {
        log.info("blackening {*}: {}", .{ obj, obj });
    }

    switch (obj.tag) {
        .string => {},
        .function => {
            // TODO
            const function = obj.as(.function);
            try self.markObject(function.name);
            for (function.chunk.constants.items) |constant| {
                try self.markValue(constant);
            }
        },
        else => {}, // TODO: https://craftinginterpreters.com/garbage-collection.html#processing-gray-objects
    }
}

fn sweep(self: *GarbageCollector) !void {
    var prev: ?*Object = null;
    var obj = self.head;
    while (obj) |o| {
        if (o.is_marked) {
            o.is_marked = false;
            prev = obj;
            obj = o.getNext();
        } else {
            obj = o.getNext();
            if (prev) |p| {
                p.setNext(obj);
            } else {
                self.head = obj;
            }
            try self.destroyObject(o);
        }
    }
}

fn collectGarbage(self: *GarbageCollector) !void {
    if (builtin.mode == .Debug) {
        log.info("-- gc start", .{});
    }
    const before = self.bytes_allocated;

    try self.markRoots();
    try self.traceReferences();
    // TODO: tableRemoveWhite: https://craftinginterpreters.com/garbage-collection.html#weak-references-and-the-string-pool
    try self.sweep();

    self.next_gc = self.bytes_allocated * next_gc_factor;

    if (builtin.mode == .Debug) {
        log.info("-- gc end: collected {} (from {} to {}) next at {}", .{
            std.fmt.fmtIntSizeDec(before - self.bytes_allocated),
            std.fmt.fmtIntSizeDec(before),
            std.fmt.fmtIntSizeDec(self.bytes_allocated),
            std.fmt.fmtIntSizeDec(self.next_gc),
        });
    }
}
