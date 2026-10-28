# Internal GC foundation

`gc.h` is a standalone libc-only C11, nonmoving, single-threaded mark/sweep
collector for **fixed-layout internal objects**. `GcRuntime.zig.source` embeds
it conditionally for scalar/managed-array and immutable-string code generation alongside
`array.h` and on-demand `string.h` concatenation helpers. Closure programs also
use this collector for environments and captured cells.
Scalar-only emission without closures is unchanged. Managed emission defines `NOV_GC_PANIC` as
`nov_rt_panic` to share the emitted panic helper. The standalone
fallback follows the existing contract: flush stdout, `panic: <message>\n` on
stderr, exit 1. No external collector, finalizers, weak references, moving,
concurrency, or conservative stack scanning is involved.

## Layout and allocation

Initialize one `nov_gc_heap` with `nov_gc_init`; do not copy it or reinitialize a
live heap. Allocate using `nov_gc_alloc` (panic on failure) or
`nov_gc_try_alloc` (NULL on size overflow or exhausted allocation retry).
The descriptor and pointer-offset map are copied into the allocation itself, so
caller metadata need not outlive the allocation. `nov_gc_alloc_repeated` and its
try variant expand an element layout across a checked element count; arrays use
this to trace string owners and array backing fields inside string or nested-record
elements. Expanded offset storage scales with capacity times managed fields and is included in heap
accounting. Metadata and payload share one allocation and are freed together.
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
supported, not over-aligned objects. Repeated pointer-bearing layouts require
correctly aligned element strides; size/map/padding arithmetic is overflow-checked.
The payload
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
stale references are unsupported. Local array-element borrowing retains a rooted
owner slot and normalized integer index, derives addresses only for immediate
reads/stores, and statically excludes conflicting owner access until last use.
Borrowed references cannot escape; there are no runtime generation checks. Do not
keep a derived interior address across a safepoint or put it in a root table. Invalid pointers encountered during tracing panic before
being dereferenced. This is not a memory-safety boundary: unregistered dangling
roots, malformed metadata, cross-heap root record reuse and use-after-free outside
tracing remain caller/compiler errors. A stale pointer whose address has been
reused cannot be distinguished from a current allocation.

## Closure environments and shared cells

When a program contains closures, internal function values carry a
code pointer and a real `void *env` field. Only `env` is traced; ordinary functions
use adapters with NULL environments. The Nov function type and scalar foreign
ABI are unchanged; function-valued foreign boundaries remain unsupported.

Environments are repeated arrays of real `void *` fields, rooted before any
capture allocation. Immutable captures use typed snapshot boxes: array backing is
cloned recursively through nested records, record elements and union record payloads
at capture creation; immutable strings share. Mutable captures point to shared typed
cells: the defining local and all closures access the same GC allocation, never a
stack address.
Ordinary reads of a mutable capture still recursively copy array-bearing values;
cell sharing is not permission to alias ordinary value results.
Descriptors trace the managed fields of each box/cell, including nested record
owners, array backing, strings and function environments. Cells created in loop
iterations are distinct allocations. Unreachable environment/cell cycles are
collected normally.

Captured parameters, callee snapshots, closure results, environments under
construction and boxed locals all follow the root discipline above. Copying a
function value shares its environment without allocation. Borrowed capabilities
cannot be captured; calls while shared captured storage is borrowed are rejected
conservatively until effect summaries can establish disjoint access.

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
`-O3`, and fatal UBSan, including failure-injected allocations and panic subprocess
checks. GCC can run the same harness with the identical flags. The harness counts
outstanding host allocations to check deterministic cleanup.

## Generated nominal value structs

Bounded plain/generic records use inline C value structs, not GC allocations for
the records themselves. Fields support bool/integer/rune/enum scalars, immutable
`nov_string`, acyclic nested records and arrays of supported elements. Record arrays
and struct payloads of unions may contain these array-bearing records. Nested array
types, layout cycles (including through arrays), union/function fields and managed
foreign retention remain unsupported.

Ordinary value boundaries preserve recursive array independence; immutable string
backing shares. Storage reads clone when producing independent values, while fresh
expression temporaries transfer through initialization, argument, return and
control-flow boundaries without another clone. Readonly field/simple-index
projections avoid copying the enclosing aggregate. Effectful indices retain
pre-effect snapshots, including when shared closure cells can mutate the source. Generated typed copy helpers
clone outer backing and recursively detach array-bearing elements/fields. Tagged
unions copy only the active payload. Helpers root both input and partially copied
output across allocations; nested allocations can trigger collection/retry. The
scalar array helper's one-backing-per-call property does not bound allocations of a
recursive generated copy.

Constructor-field shorthand `Type[.width, .height]` is an ordinary same-name
scoped read at the exact expected field type, not an ownership transfer. Mixing
explicit fields preserves source order; omitted defaults follow supplied fields.
Imported types and generic aliases use the same path. Array backing still copies
recursively on ordinary reads, borrowed values produce normal read copies, and
immutable/mutable closure captures retain their existing snapshot/shared-cell
rules. Union/enum payload grammar is unchanged; no contextual/anonymous structs
or new `use`/`pub` syntax are introduced. See `examples/constructor-shorthand.nov`.

The emitter predeclares instruction/slot/merge storage at function scope,
zero-initializes it, and roots actual `void *owner` and `void *data` fields, flattened
through inline records and parameters. Repeated element maps trace backing fields
inside record elements; each backing allocation traces its own elements. Skipped
branches therefore contain initialized NULL roots. No `T **` cast, string byte
pointer or derived interior address is traced. Record/element loans retain their
existing stable owner-slot/index representation: no field-specific loan form is
added. Whole-record/record-element loans support array-field replacement/append;
indexed array-field updates through these loans remain unsupported.

Nested mutable-local field paths support replacement, append and indexed updates.
Append/index updates capture independent target-array snapshots before index/RHS
effects and store only that field into the current record slot, preserving siblings.
A terminal index/RHS skips the pending store. Borrowing and GC do not introduce
implicit mutable receivers. Allocation retry, panic, shutdown and immutable-string
contracts apply unchanged.

### IMPORTANT: recursive-copy costs tracked — optimization PAUSED

All further optimizations are deferred; continue language features. Copy costs
remain important tracked work, not the immediate next action. Separately, `defer`
implementation remains postponed.

Eager recursive backing copies are a major performance concern, not an optimized
ownership strategy. Reads, snapshots and updates may copy entire reachable array
contents repeatedly and keep temporary backing rooted until function exit/reuse.
Targeted fresh-value and projection elision is implemented and allocation-count
regressions cover it at O0/O3 with forced GC. Membership projects its RHS without
cloning existing backing; computed-base effects retain their internal snapshots,
and managed inputs stay rooted. Contextual literals/construction may still allocate.
The backend additionally transfers
proven-detached single-use elements/bases into insertion/replacement, and append
retains its freshly allocated outer buffer while recursively copying only unproven
input portions. Append's final RHS read is a rooted readonly projection, avoiding
an extra outer-buffer clone. Projected backing is not ownership evidence: unproven
nested portions still copy into the fresh result. The LHS remains a pre-RHS snapshot,
as do computed RHS bases evaluated before effectful indices. Plain assignment and
arbitrary RHS block/control-flow evaluation keep their existing copy rules.
The proof counts all IR references and checks recursive freshness.
A detached indexed-update base may have earlier scalar/length observations before
exactly one consuming array_set, provided all share conditional/loop execution
regions. Later reads, escaping projections, repeated consuming operands and region
crossings reject transfer. Calls, merges and storage reads cannot establish ownership. It does not trust lowering
to have inserted copies in forged IR. Snapshot/conservative-fallback copies remain
potentially expensive. Broader
elision, moves/consuming transfers and copy-on-write remain deferred candidates, **not approved
designs**. Any optimization must preserve observable independence,
static borrow exclusivity and precise GC/root/OOM safety. No aliasing or COW behavior
is delivered by this implementation. See `examples/array-fields.nov`.

Ordinary iteration over a direct unborrowed local array retains a rooted readonly
backing descriptor, avoiding a recursive source clone. Per-element ordinary copies
still detach when passed, stored or captured. Computed and borrowed sources retain
snapshot evaluation. The checker prohibits source mutation/conflicting loans and
centralizes call protection (including methods/conversions/pipes). Pending checks
retain stable owner slot IDs and validate final boxing state after top-level and
child bodies are lowered; captures discovered later cannot invalidate earlier
call checks across loop backedges. Without effect summaries this remains conservative.

## Generated scalar arrays

The approved first array slice uses `nov_array { void *data; uintptr_t len, cap; }`.
The emitter declares this type for any transitively reachable array type, including
arrays nested in callback parameter/result signatures. Signature-only callback
uses need neither the GC helpers nor root frames; unreferenced pooled types emit
nothing. Runtime emission and function rooting instead depend on direct managed
parameters, results, or instruction values.
Only backing storage is managed. Each nonempty allocation has an exact descriptor
of `cap * sizeof(element)` bytes, element alignment, and zero pointer fields.
Empty backing is NULL and requires no allocation. Size addition/multiplication
are checked before allocation; self-append copies from rooted inputs into fresh
backing. Copies and updates are eager, not a copy-on-write or amortized-growth
optimization. Bool elements are ordinary C bool objects, not bit-packed.

Typed IR owns literal operand lists and represents clone, len, index, replacement
and append explicitly, with exact element/index/result types and region privacy.
Array value boundaries clone; returns transfer freshly copied backing without an
intervening safepoint after popping the callee frame. C helpers allocate at most
one backing per call and rely on compiler-rooted input values.

The emitter predeclares every managed instruction result at **function scope**,
initializes all data fields to NULL, and pushes one frame containing their actual
`void *data` addresses plus every managed parameter's data field before executing
any body instruction. No typed-pointer-to-void-pointer slot casts are used.
Locals, prior arguments, branch merges, snapshots and return values thus remain
rooted across all allocations, including retry collection. Slots outlive nested
blocks and loops: break/continue leave the function frame registered (no nested
root record exists to dangle), and every explicit/fallthrough return pops it.
Retention is conservative: an instruction slot retains its previous iteration's
backing until overwritten, and unused temporary slots survive until return.

Program main shuts down the heap on normal completion; panic still exits without
heap cleanup as above. Library emission's host owns lifetime/shutdown; the managed
C ABI is internal, not an approved FFI. Single-threading and no foreign unwinding
across frames remain requirements. Tests run generated programs with collection
on **every** allocation, injected allocation failures/retries, strict C11/UBSan,
-O0 and -O3 bounds checks, argument-order effects, nested loops and early returns.

Writable length (especially growth initialization), slices/invalidation, nested
array or pointer elements, explicit aliases and managed FFI remain deferred.
String/record elements and array-valued record fields use managed element maps
and recursive generated copies as described above.
See the main README for delivered source syntax and copy/evaluation contracts.

## Generated immutable strings

`nov_string { void *owner; const unsigned char *data; uintptr_t len; }` separates
traced ownership from byte access. Literals use read-only static C bytes, explicit
lengths and a NULL owner: foreign/static data is **never** submitted to tracing.
Concatenation checks native length addition and size limits before allocation,
then uses an exact `len`-byte, byte-aligned, zero-pointer descriptor. Empty results
need no allocation. Both operands are rooted by the caller while allocation may
collect or retry. Only the result owner is mutable inside runtime construction;
Nov never exposes byte mutation. No NUL terminator is required or inspected.

String parameters, results, local slots, snapshots and branch merges share the
view without clones. Function root tables contain actual `void *owner` slots for
strings (and `void *data` for arrays), initialized before frame registration.
Returning a view pops the frame without an intervening safepoint, just like arrays.
Immutable literal-only globals require no GC registration or eager initialization.
The internal C ABI trusts lengths and valid UTF-8; it is not a checked foreign API.
UTF-8 validation occurs at literal checking, and concatenation preserves it.

Printing uses checked `fwrite` lengths, including NUL; first printing switches
Windows stdout to binary mode. The host must not change stdout mode while using
the emitted printing helpers. Library hosts remain responsible for the final
stdout flush and heap shutdown.

Validation includes strict generated-C compilation for Windows. Windows binary
stdout behavior has not been runtime-tested on Windows; the byte-output execution
tests currently run on the native host.
