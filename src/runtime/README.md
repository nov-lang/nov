# Internal GC foundation

`gc.h` is a standalone libc-only C11, nonmoving, single-threaded mark/sweep
collector for **fixed-layout internal objects**, not a Nov language feature.
`GcRuntime.zig.source` embeds it for future conditional C emission. No current
IR operation requests it and the emitter is deliberately unchanged. Future
emission must include it only when managed operations are present and define
`NOV_GC_PANIC` as `nov_rt_panic` to share the emitted panic helper. The standalone
fallback follows the existing contract: flush stdout, `panic: <message>\n` on
stderr, exit 1. No external collector, finalizers, weak references, moving,
concurrency, or conservative stack scanning is involved.

## Layout and allocation

Initialize one `nov_gc_heap` with `nov_gc_init`; do not copy it or reinitialize a
live heap. Allocate using `nov_gc_alloc` (panic on failure) or
`nov_gc_try_alloc` (NULL on size overflow or exhausted allocation retry).
Descriptors and their offset tables are immutable and must outlive their objects.
Use `sizeof(T)`, `_Alignof(T)` and sorted `offsetof(T, field)` entries. Every strong
managed field must be an actual C `void *` object and appear exactly once. Nested
inline records can be flattened into offsets. Do not describe arbitrary typed
pointer fields, union alternatives, function pointers, integers or foreign
pointers as managed fields. The descriptor is trusted compiler metadata: bounds,
alignment, and overlap are checked, but the runtime cannot verify its C types or
that no pointer fields were omitted.

Allocation includes an aligned private header; the returned pointer is the stable
payload base. Sizes, accounting additions and alignment are checked. Only positive
fixed payload sizes and fundamental alignment up to `_Alignof(max_align_t)` are
supported, not over-aligned objects or variable-length pointer maps. The payload
is zero-filled and every described pointer field is explicitly initialized to
NULL, so collection during partial initialization is safe. This promises byte
zeroes for scalar storage, not default constructors or language-level zero values.
Survivors never move. Unreachable objects, including cycles, are reclaimed.

## Root discipline and C boundaries

All root slots must be real, initialized, address-stable `void *` variables.
Do **not** cast a `T **` to `void **`; use a canonical `void *` slot and cast its
value when accessing a payload. Root tables contain addresses of these slots:

```c
void *value = NULL;
void **slots[] = { &value };
nov_gc_roots frame = {0};
nov_gc_push(&heap, &frame, slots, 1);
value = nov_gc_alloc(&heap, &descriptor);
/* Initialize fields, call allocating functions, etc. */
nov_gc_pop(&heap, &frame);
```

Frame storage, slot-address tables and slots must remain live and unmoved until
pop. Frames pop strictly LIFO, including early returns, break/continue cleanup,
and all C boundary exits. Registering the same record twice is an error. Root
records belong to one heap; cross-heap registration/reuse while active is forbidden.
Global registrations use the same slot/table rules with
`nov_gc_register_globals` / `nov_gc_unregister_globals`, support arbitrary removal
order, and must be removed before their C storage goes away. The same slot may
appear in multiple tables. Mutating a slot updates the root immediately; no write
barrier is required in this stop-the-world single-threaded collector.

Every allocation is a potential collection safepoint, including a malloc failure
retry. Root **all** live managed values before it: arguments, callees' managed
parameters, earlier evaluated arguments, partially initialized parents, locals,
and return temporaries. Evaluate allocating expressions in separate C statements;
never depend on C argument evaluation order. A newly returned object is safe only
until the next allocation/explicit collection: put it in an already registered
slot or a field of a rooted object first. The caller must root allocating-call
arguments before entry; a callee cannot rescue an argument already collected by
another argument's evaluation. A callee must transfer its result without an
intervening safepoint after popping its own frame. Ordinary C locals and registers
are not roots. Callbacks and foreign code retaining objects require explicit root
registrations spanning their entire retention period. `longjmp`, exceptions and
foreign unwinding across active frames are unsupported.

Only NULL and exact payload-base pointers owned by the same heap are valid in
managed slots/fields. Interior references, tagged pointers, foreign pointers and
stale references are unsupported. Retain a rooted base and an integer offset if
future compiler lowering needs a derived address; do not keep the derived address
across a safepoint. Invalid pointers encountered during tracing panic before
being dereferenced. This is not a memory-safety boundary: unregistered dangling
roots, malformed metadata, cross-heap root record reuse and use-after-free outside
tracing remain caller/compiler errors. A stale pointer whose address has been
reused cannot be distinguished from a current allocation.

## Collection, OOM, shutdown

Tracing is iterative with intrusive gray links and allocates no memory, even if
the host allocator is exhausted. Sweep also allocates nothing. Pointer validation
currently uses a linear object-list lookup: worst-case tracing is quadratic in
live graph size. This deliberately small foundation is not a performance claim;
a future address index must preserve tracing's OOM guarantees. Collection runs at
a byte threshold or on allocation failure; malloc is retried once after collection.
`collect_every_allocation` enables deterministic stress. Heap counters are internal
test/diagnostic accounting (header plus payload bytes), not language semantics.

Remove all frames and global registrations, then `nov_gc_shutdown` frees all
remaining allocations without tracing; repeated shutdown is harmless. No atexit
hook or finalizer runs. Allocation after shutdown starts a fresh heap. Panic exits
the process and does not attempt heap shutdown. `NOV_GC_MALLOC` and `NOV_GC_FREE`
may be defined together before inclusion for testing; they must be malloc/free
compatible, fundamentally aligned and nonreentrant. No hook may reenter the heap.

`zig build test` compiles/runs `gc_test.c` with pinned `zig cc`, strict C11 warnings,
`-O2`, and fatal UBSan, including failure-injected allocations and panic subprocess
checks. GCC can run the same harness with the identical flags. The harness counts
outstanding host allocations to check deterministic cleanup.

## Still blocked on language decisions

This API does not decide array storage/growth/copying/aliasing, slice ownership and
invalidation, string encoding/mutability/layout/interpolation, container mutation,
or any source-level allocation/FFI syntax. Those contracts and compiler root
lowering must be approved/implemented before adding memory-bearing IR or syntax.
