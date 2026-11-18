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

`nov_array_clear` mutates the actual array slot (including a borrowed array or
nested struct field) without allocating. It zeroes all bytes of the old active
elements, then sets length to zero, retaining the backing pointer and capacity.
Zeroing is required because repeated GC descriptors trace the entire capacity,
not just the current length: removed string owners, nested-array backing and
closure environments must not remain strongly reachable through cleared elements.
Empty arrays, including NULL-backed arrays, require no memory access. The element
descriptor must match the backing layout; clearing does not construct defaults,
resize storage or change append's existing allocation behavior.

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
managed root slots/traced fields. Interior pointers, tagged pointers, foreign
pointers and stale pointers are unsupported there. Local array-element borrowing
uses typed C pointer handles and target snapshots, backed by rooted stable owners
and normalized indices. Static checks exclude conflicting owner access until the
last live use. These internal handles may survive a safepoint only with the
managed backing retained by owner roots; never put an interior pointer in a root
table. Borrowed references cannot escape; there are no runtime generation checks.
Invalid pointers encountered during tracing panic before being dereferenced. This is not a memory-safety boundary: unregistered dangling
roots, malformed metadata, cross-heap root record reuse and use-after-free outside
tracing remain caller/compiler errors. A stale pointer whose address has been
reused cannot be distinguished from a current allocation.

## Explicit local and parameter reference capabilities

The source contract uses readonly `&T` and mutable-pointee `&mut T`, not `&const T`.
`&a` infers the mutable-pointee form iff owner `a` is mutable. An explicit
`let p: &T = &mutableOwner` downgrades to readonly. Binding mutability is separate:
`let p = &a` cannot rebind, but `p.* = value` is valid for `&mut T`; `let mut p`
permits `p = &b` with the same exact reference type (including any explicit
readonly annotation). Rebinding works across `if`, `match`, short-circuit
expressions, nested blocks and loops, including conditions, continuation and
exhaustion paths. The static proof merges possible targets per CFG, with slot
kills and NLL snapshot liveness. Conflicting owner reads, writes or storage
invalidation are blocked if any possible live alias targets that owner; readonly
loans remain exclusive. Conservative frontend owner sets may overreject
target-sensitive iteration.

Newly borrowed owner storage must be in the same or an outer lexical scope
relative to the reference binding. An outer reference cannot bind to an inner
owner, even if that reference is unused afterward.

All pointee reads/writes are explicit: `p.*`, `p.*[i]`, `p.*.field`, `p.*()` and
`p.* = value`. No autodereference exists. `loop &item in items` yields a scoped
reference accessed via `item.*`. `@TypeOf(p)` inspects the compile-time reference
descriptor and renders canonical names such as `&u8` / `&mut u8`; it does not
read the target. `@TypeOf(p.*)` remains unsupported by the bounded binding query.

Runtime reference handles lower to typed C pointers. Unit-typed IR
`reference_parameter` and `reference_argument` capabilities carry reference
descriptors without becoming ordinary first-class reference values. Internal Nov
C prototypes use `const T *` for readonly and `T *` for mutable references; these
prototypes are not a source-level foreign ABI. `@[extern]` / `@[export]` reference
parameters and results remain forbidden. A load snapshots the current target
before dereference RHS effects. Thus rebinding `p` inside the RHS of `p.* += ...` does not redirect
the pending store: it updates the old pointee. Explicit reads still yield child
values with normal recursive-copy semantics and may allocate; borrow creation
itself does not clone/allocate its target.

Static NLL/CFG checks keep the captured target loan live through compound RHS
evaluation and the final store. Rebinding to a different owner can work, but a
new borrow of another element of the same array still conflicts with the live
compound's whole-owner loan; no element disjointness is inferred. Ordinary
rebinding after the last use works. Stable owner slots and normalized indices
support the handles, while owner roots retain managed backing for all live target
snapshots under the existing root discipline. Bounds checks are unchanged.
Interior pointers are never traced as GC roots. There are no runtime
generation/reference-validity checks.

Functions, callbacks, closures, inferred generic functions, methods and pipes may
accept reference parameters. Calls use `f(p)`, `f(&local)` or `f(&array[index])`;
`f(p.*)` passes an ordinary child value instead. Mutable arguments may weaken to
readonly for the same child type, never the reverse. Function/callback types
remain invariant. Formal reference bindings cannot rebind, but mutable-pointee
`.*` writes and forwarding to compatible reference parameters work. Method
receivers retain existing by-value semantics.

A `reference_argument` snapshots a typed pointer before later arguments execute;
`call(p, later)` still receives the old target if `later` rebinds `p` to `&b`.
The caller retains that
snapshot's loan and stable owner roots throughout remaining argument evaluation
and the entire call, including nested forwarding. Managed backing is retained by
caller owner roots, not by tracing the interior argument pointer; reference
parameters are not themselves managed roots. Callee mutation updates rooted owner
storage under the existing root discipline. Multiple reference arguments must not
possibly alias, even when readonly: `f(p, p)` / `f(&a, &a)` reject. Whole-array
loans do not infer element disjointness. Unsafe owner changes in later arguments
reject. Conservative boxed/captured-owner call protection may also reject safe
calls; no interprocedural effect analysis is promised.

Copying (`let q = p`), returning, storing, capturing and reborrowing references
remain rejected. Reference types are not storable array elements, aggregate
fields/payloads, constants or results. Nonlocal/temporary/void
owners and references to references remain unsupported. Passing or returning `p.*`
is an ordinary child-value operation, not reference escape. No manual memory
management, pointer arithmetic, null references or unsafe dereference is added.

**Accepted roadmap:** passing references is delivered with call-scoped loans and
caller-held owner roots. Returning and storing references, ordinary copying,
capture and reborrowing remain **not implemented**. Escape requires extended
lifetime/referent tracking across frames and managed aggregates.
Managed referents must stay GC-retained for every live reference; owner replacement,
resizing or lifetime end must be statically prevented or safely invalidate affected
references, never expose dangling storage. The current local owner-slot scheme
alone does not implement escaping-reference lifetimes. Future representations,
tracing and invalidation checks must satisfy that contract before escape is enabled.
Broader raw-pointer and managed-FFI proposals remain separate. See
`examples/references.nov`, `examples/reference-rebinding.nov`,
`examples/reference-parameters.nov` and the root README
for the source-facing contract.

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

## Generated floats

`f32`/`f64` are inline scalar C `float`/`double` values, including in arrays,
record fields, active union payloads, function signatures and closure captures.
Native `float` aliases `f32` on 32-bit targets and `f64` on 64-bit targets.
Both widths are supported by the scalar extern/export ABI. They introduce no GC
roots or managed pointer fields; existing recursive copy boundaries, closure
sharing and stringify-hook behavior are unchanged.

Generated C guards IEEE storage widths, binary radix, subnormal support and
`FLT_EVAL_METHOD == 0`, rejects fast-math and disables FP contraction. Embedding
assumes the C numeric locale and normal round-to-nearest rounding environment.
Float arithmetic/comparisons and array membership use IEEE semantics: division by
zero yields infinity/NaN rather than an integer panic; NaN is unequal and
unordered, while positive/negative zero compare equal. Literals round directly to
target width, diagnose finite overflow and support subnormals/underflow.

Direct printing, interpolation and nested default formatting share round-trip-safe
`%.9g` (`f32`) / `%.17g` (`f64`) output, normalizing `nan`, `inf`, `-inf`, `-0`.
This is not shortest-decimal formatting. Float formatting may allocate strings
through the existing managed string path, but float leaves themselves are untraced.
`f16`/`f80`/`f128` and float match subjects remain deferred; remainder, bitwise
operations, ranges and indices remain integer-only. See the root README's
[float contract](../../README.md#floats) and `examples/floats.nov`.

## Generated nominal value structs

Bounded plain/generic records use inline C value structs, not GC allocations for
the records themselves. Fields support bool/integer/float/rune/enum scalars, immutable
`nov_string`, acyclic nested records and arrays of supported elements. Record arrays
and struct payloads of unions may contain these array-bearing records. Nested
arrays are supported. Layout cycles (including through arrays), direct union/function
record fields and managed foreign retention remain unsupported. Aggregate depth
is bounded at 128, counting nested array edges.

Ordinary value boundaries preserve recursive array independence; immutable string
backing shares. Storage reads clone when producing independent values, while fresh
expression temporaries transfer through initialization, argument, return and
control-flow boundaries without another clone. Readonly field/simple-index
projections avoid copying the enclosing aggregate. Effectful indices retain
pre-effect snapshots, including when shared closure cells can mutate the source. Generated typed copy helpers
clone outer backing and recursively detach array-bearing elements/fields, including
nested arrays. Tagged unions recursively detach only the active payload's array
backing, including record payloads; direct array/union/function union payloads
remain unsupported under the unchanged payload rules. Function values share their
existing closure environment and captured cells, rather than deep-cloning a
closure; immutable snapshot/mutable shared-cell capture rules remain unchanged.
Range values copy bounds and inclusivity by value. Helpers root both input and partially copied
output across allocations; nested allocations can trigger collection/retry. The
scalar array helper's one-backing-per-call property does not bound allocations of a
recursive generated copy.

Constructor-field shorthand `Type.[width, height]` is an ordinary same-name
scoped read at the exact expected field type, not an ownership transfer.
Contextual `let p: Type = .[width, height]` uses the same bare-name rule; old dotted
shorthand `Type.[.width]` is rejected. Inferred all-bare `.[x, y]` is positional,
while an explicit assignment plus bare names is named. Contextual named entries
map to the expected layout regardless of source order. Mixing
explicit fields preserves source order; omitted defaults follow supplied fields.
Imported types and generic aliases use the same path. Array backing still copies
recursively on ordinary reads, borrowed values produce normal read copies, and
immutable/mutable closure captures retain their existing snapshot/shared-cell
rules. Explicit constructors use `Type.[field = value]`; contextual named
`.[field = value]` keeps the expected type and scalar/array literal defaults.
Anonymous named `.[a = 1]` and positional `.[1, "a"]` values use the same recursive
copy/rooting and closure-field sharing rules. Structural identity is ordered names
and canonical exact types, distinct from nominal types. Even homogeneous `.[1, 2]`
is a struct, not an array. No tuple access/destructuring, mixed entries, anonymous
unions/enums or new `use`/`pub` syntax are introduced. Union/enum payload grammar,
array indexing and slicing are unchanged. Nominal debug output stays `Type[ ... ]`
/ `Empty[]`; anonymous output is `.[ a: 1 ]` / `.[1, a]`, independent of source
constructor spelling. See `examples/constructor-shorthand.nov` and
`examples/anonymous-structs.nov`.

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

### Last-use transfers and remaining recursive-copy costs

Last-use copy elimination is approved; broader optimizations remain deferred.
Copy-on-write is an unaccepted proposal, and explicit `@move` remains deferred.
Separately, `defer` implementation and array API redesign remain postponed.

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
as do computed RHS bases evaluated before effectful indices. Unproven assignments
and arbitrary RHS block/control-flow evaluation retain recursive copies.
The proof counts all IR references and checks recursive freshness.
A detached indexed-update base may have earlier scalar/length observations before
exactly one consuming array_set, provided all share conditional/loop execution
regions. Later reads, escaping projections, repeated consuming operands and region
crossings reject transfer. Calls, merges and arbitrary storage reads cannot establish
ownership. It does not trust lowering to have inserted copies in forged IR.

A clone may also transfer at a value's last use. Fresh values and final loads of
private, unboxed local slots are eligible within matching conditional/loop regions.
Slot initialization/latest assignment must itself transfer owned backing. Earlier
raw loads may have only completed independent copies or nonescaping scalar reads;
other aliases, loans, field writes, mutable captures, future loads and globals
reject the proof. Unknown C caller parameters stay independent. Recursive record
and active union backing follow the same rules; strings and closure environments
retain their existing shared semantics.

Elided copies still run allocation-free payload validity checks, including for
unused results. The normal rooted destination receives the transferred descriptor
without an intervening safepoint. Dead source root slots can still retain backing
until function exit; early root clearing is not implemented. O0/O3 allocation-count,
forged-alias, forced-GC and OOM tests cover these transfers. Snapshot/conservative
fallback copies remain potentially expensive. No COW, reference counting, source
move operator or new array API is delivered. See `examples/last-use.nov`.

Ordinary iteration over a direct unborrowed local array retains a rooted readonly
backing descriptor, avoiding a recursive source clone. Per-element ordinary copies
still detach when passed, stored or captured. Computed and borrowed sources retain
snapshot evaluation. The checker prohibits source mutation/conflicting loans and
centralizes call protection (including methods/conversions/pipes). Pending checks
retain stable owner slot IDs and validate final boxing state after top-level and
child bodies are lowered; captures discovered later cannot invalidate earlier
call checks across loop backedges. Without effect summaries this remains conservative.

## Generated storable-value arrays

Arrays support every existing storable runtime type: bool, integer, rune, string,
enum, finite acyclic record, existing tagged union, function/closure, nested array
and inferred integer range, plus `f32`/`f64`. `void` and compile-time `type` are
not storable. Semantic integer widths extend through 128 bits, but reachable
backend leaves and function signatures still require 8/16/32/64-bit widths; native
`int/uint` retain their pointer-width contract. There is no new range annotation
syntax. Storage support does not grant equality, membership or printing: enums
retain exact nominal equality/membership alongside existing scalar/string support;
functions, unions, records, arrays and ranges reject comparisons/membership. Arrays
print recursively when their elements are printable, using scalar/default aggregate
formatting or exact `(SelfType) -> str` container methods marked `@[stringify]`.
Marked converters override a value at any depth and stop recursion; malformed or
inaccessible marked methods reject rather than falling back. Cross-file converters
require `@[public]`. An unmarked `toString` is an ordinary callable method, not a
printing hook.

Source-level `@len(values)` returns an array's element count as native `uint`,
not its byte size or capacity. It accepts an array, string or finite bounded
integer range expression, not a record with a field named `len`; real record
fields retain ordinary field access. Synthetic `.len` properties stay removed.

For ranges, length is an O(1), allocation-free cardinality calculation returning
native `uint`, independent of endpoint width. Reversed ranges count 0; otherwise
exclusive counts are end-start and inclusive counts add 1. Arithmetic must
handle signed cross-zero spans without signed overflow and widen narrow domains
before counting (full inclusive u8 is 256). A count exceeding `UINTPTR_MAX`
panics `range length overflow` in every execution mode, rather than wrapping or
truncating. Compile-time-known global initializer overflow diagnoses at compile
time. This count limit does not restrict otherwise valid finite iteration.

The operand evaluates once; inline endpoints evaluate once left-to-right.
Stored, passed and returned ranges retain their copied bounds/inclusivity, and
generic length helpers specialize for the concrete operand type. Type-query
operands are checked but not executed and `@TypeOf(@len(...))` yields native
`uint`. Unbounded range length is rejected. Semantic 128-bit integers do not
extend the existing reachable backend width support beyond 8/16/32/64 bits.

The array representation uses `nov_array { void *data; uintptr_t len, cap; }`.
The emitter declares this type for any transitively reachable array type, including
arrays nested in callback parameter/result signatures. Signature-only callback
uses need neither the GC helpers nor root frames; unreferenced pooled types emit
nothing. Runtime emission and function rooting instead depend on direct managed
parameters, results, or instruction values.
Only backing storage is managed. Each nonempty allocation has an exact descriptor
of `cap * sizeof(element)` bytes and element alignment. Scalar elements have zero
pointer fields; managed elements use owned expanded pointer maps over the entire
zero-initialized capacity. Maps trace actual string owners, nested-array backing,
closure environments and managed record/union payload fields, never code pointers,
static byte pointers or interior addresses.
Empty backing is NULL and requires no allocation. Size addition/multiplication
are checked before allocation; self-append copies from rooted inputs into fresh
backing. Copies and updates are eager, not a copy-on-write or amortized-growth
optimization. Bool elements are ordinary C bool objects, not bit-packed.

Typed IR owns literal operand lists and represents clone, len, index, replacement
and append explicitly, with exact element/index/result types and region privacy.
Array value boundaries clone unless a last-use transfer is proven; returns transfer backing without an
intervening safepoint after popping the callee frame. The low-level C array helper
allocates at most one outer backing per call and relies on compiler-rooted inputs;
recursive generated copies may allocate additional nested backing.

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

Array clear/truncation APIs, length growth initialization, array slices/invalidation,
general pointer elements and managed FFI remain deferred. Local references and
call-scoped reference parameters are delivered; returning and storing references
remain accepted but unimplemented, as described above. These are not general
storable aliases.
Direct chained indexed writes, record-field writes through computed elements and
arbitrary computed-base writes remain unsupported. Existing scoped stable-owner
loans allow supported nested-array element updates and replacement; no new borrow
syntax, reborrows or escaping capabilities are added.
Managed elements and array-valued record fields use precise element maps and
recursive generated copies as described above. `examples/storable-arrays.nov` is
being added; parent validation gates for this expansion remain pending. Existing
validation descriptions above do not claim that those gates have passed.
Optimizations beyond approved last-use transfers remain paused; `defer` remains postponed. Namespace `use`
syntax remains undecided and its unfinished work stashed, not implemented.
See the main README for delivered source syntax and copy/evaluation contracts.

## Generated immutable strings

Source-level `@len(text)` for a `str` scans UTF-8 without allocation and returns
its Unicode scalar count as native `uint`, not bytes or grapheme clusters. The
internal byte length and real C fields below are unchanged by this source builtin.

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
UTF-8 validation occurs at literal checking and byte-to-string conversion;
concatenation preserves it.

### Copying between strings and bytes

IR `string_to_bytes` (`@stringToBytes`) copies a string's exact byte view into
independent `[]u8` backing. IR `bytes_to_string` (`@stringFromBytes`) strictly
validates the entire byte array, then copies it into an independent string owner.
The helpers in `string_bytes.h` preserve embedded NUL and do not add a terminator,
normalize Unicode, or reuse input storage. A string subview copies only its view,
not the owner's prefix or suffix. Empty inputs return empty values without
allocation or dereferencing their data pointers.

The strict primitive rejects stray continuations, illegal leads, truncated
sequences, overlong encodings, surrogates, and code points above U+10FFFF with
`panic: invalid UTF-8`. Validation precedes its output allocation, including for
direct builtin calls. The public `std.string.fromBytes` first validates in Nov
and returns `.err[Utf8Error.invalidUtf8]` for malformed input without entering the
primitive. That recoverable error is not an OOM guarantee: ordinary array-value
argument/snapshot copies can still allocate and fail before validation.

Each runtime helper allocates at most one exact-length, byte-aligned backing with
no traced fields; the array result has capacity equal to its length. Array size
checks and GC allocation overflow/OOM checks apply normally. Generated callers
root string `owner` or array `data` operands and zero-initialized output slots
before any allocating operation. There is no safepoint between the helper's
allocation and installing its returned owner in the rooted output slot. Internal
C callers must follow the same discipline and must supply valid buffers/lengths;
UTF-8 checking does not make this a checked foreign-memory API.

Neither conversion can be elided into input storage, even for single-use input.
A fresh `string_to_bytes` result is ownership evidence for subsequent array
transfers only under the emitter's existing use-count and control-region proof;
reused snapshots still require independent backing.

Source-free generated-C tests exercise the actual stdlib at O0/O3 with collection
on every allocation, retained results, managed subviews, mutation isolation,
recoverable errors, empty inputs, and OOM. Differential tests compare Nov's
validator and a test-only panic-catching copy of the strict runtime helper against
Zig's UTF-8 validator across exhaustive byte-pair and continuation-boundary
families, including prefixes containing NUL and completed multibyte sequences.
The ordinary stdlib path keeps its real panic behavior; separate direct-IR tests
assert the primitive's actual panic text on malformed inputs.

Printing uses checked `fwrite` lengths, including NUL; first printing switches
Windows stdout to binary mode. The host must not change stdout mode while using
the emitted printing helpers. Library hosts remain responsible for the final
stdout flush and heap shutdown.

Validation includes strict generated-C compilation for Windows. Windows binary
stdout behavior has not been runtime-tested on Windows; the byte-output execution
tests currently run on the native host.
