# Nov standard library foundation

Import the standard namespace and alias its ordinary union templates and array
helper namespace:

```nov
let std = @import("std")
let Result = std.Result
let Option = std.Option
let array = std.array
```

`std.nov` is the entry module. It publicly re-exports `Result` from `result.nov`,
`Option` from `option.nov`, and the `array` namespace from `array.nov`. These are
ordinary library declarations, not compiler builtins or automatically injected
bare names.

## Result

`Result(T, E)` has variants `ok: T` and `err: E`.

- `value.isOk()` / `value.isErr()` report the active variant as a bool, without
  inspecting or unwrapping its payload. They also work with void payloads.

- `value.unwrapOr(fallback)` returns the success payload or the supplied fallback.
  The fallback expression is an ordinary argument and always evaluates.
- `value.unwrapOrElse(callback)` returns the success payload or calls the callback
  with the error payload. The callback expression evaluates normally, but its body
  runs only for an error. Both ordinary functions and capturing closures are
  supported.
- `value.!` yields success or propagates an exactly typed error to the enclosing
  Result. This operator recognizes the `ok`/`err` shape, not the library name.

Methods specialize on demand. Void success/error payloads remain valid; a method
whose concrete signature would contain a void parameter cannot be used, following
Nov's existing parameter rules. For example, `Result(void, string)` can be
constructed and propagated without requesting `unwrapOr`.

## Option

`Option(T)` has variants `some: T` and payloadless `none`. Construct it explicitly
with `Option(uint).some[21]`, or use `.some[21]` / `.none[]` under an expected
Option type. Exhaustive matches can capture the `some` payload or handle `none`.

- `value.isSome()` / `value.isNone()` report the active variant as a bool, without
  inspecting or unwrapping its payload. `some[false]` is still present; these methods
  work with void and record payloads too.

- `value.unwrapOr(fallback)` returns the payload or the supplied fallback. As an
  ordinary argument, the fallback expression always evaluates.
- `value.unwrapOrElse(callback)` returns the payload or invokes a zero-argument
  `() -> T` callback for `none` only. Evaluating the callback value is not deferred.
- `Option(void)` can be constructed and matched. `unwrapOr` cannot be requested
  for it because void parameters are disallowed; `unwrapOrElse: (() -> void)` works.
- `value.!` evaluates once, yields the `some` payload, or immediately returns
  `.none[]` from the enclosing Option-returning function. The enclosing success
  payload type may differ. The operator structurally requires exactly `some` and
  `none` variants with a void `none` payload; it does not depend on the Option name.
  Result-to-Option and Option-to-Result propagation are rejected.

See `examples/options.nov`: division by zero yields `none` rather than panicking,
`.!` propagates it from `Option(uint)` into `Option(string)`, and the fallback runs
only for the absent quotient.

## Array helpers

`array.nov` implements three public generic functions.
Signatures here use `T`/`U` for inferred backtick type variables:

- `array.map(items: []T, convert: (T) -> U) -> []U` converts every element.
- `array.filter(items: []T, predicate: (T) -> bool) -> []T` keeps matching elements
  in stable source order.
- `array.fold(items: []T, initial: U, combine: (U, T) -> U) -> U` is a left fold
  starting with `initial`, passing the accumulator before the element.

Each callback runs exactly once per input element, in source order. Empty input
invokes no callback: map/filter return empty arrays and fold returns `initial`.
This does not defer argument expressions: callee then arguments evaluate eagerly
in ordinary left-to-right order, including callback construction and the seed.
Ordinary functions and capturing closures are supported, as are inferred generic
closures/local generic functions within the bounded solver. Exact typed inputs
and seeds are required to resolve inference; ambiguity is an error, not a numeric
default or implicit conversion.

```nov
let std = @import("std")
let array = std.array

let main: () = {
    let items: []int = [1, 2, 3, 4]
    let mapped = array.map(items, (value: int) -> int { value * 2 })
    let filtered = array.filter(mapped, (value: int) -> bool { value > 4 })
    let folded = array.fold(
        filtered,
        @as(int, 0),
        (sum: int, value: int) -> int { sum + value }
    )
    @println("Mapped:", mapped[0], mapped[1], mapped[2], mapped[3])
    @println("Filtered:", filtered[0], filtered[1])
    @println("Folded:", folded)
}
```

```text
Mapped: 2 4 6 8
Filtered: 6 8
Folded: 14
```

Normal by-value semantics apply: nested arrays, structs and active union payloads
have recursively independent array backing; immutable strings share. Copies of
function/closure values share their existing environments and mutable capture
cells. No aliasing pointer mutation is introduced. Callback failures and panics
propagate normally; these helpers do not catch errors or panics.

Map/filter use repeated append and can be quadratic; by-value copies can add
substantial overhead. Further optimization remains **PAUSED**. See
`examples/array-helpers.nov` for the runnable example and six tests covering typed
and generic callbacks, stable order, empty inputs, left folds, recursive copies,
shared closure environments and eager argument order.

## Loading and installation

`zig build` and `zig build release` install this directory at `<prefix>/lib/nov`;
Zig package sources include it as well. The installed compiler resolves `"std"`
from executable-relative `../lib/nov/std.nov`. Semantic check, emit-c, build, and
run accept `--lib-dir <path>` to override that library root. There is no implicit
working-directory library search or embedded developer-checkout path.

Explicit relative `.nov` imports resolve from the importing file, not the process
working directory. For example, `@import("result.nov")` in `std.nov` refers to this
directory's file. A re-export may select a member directly:
`@[public] let Result = @import("result.nov").Result` (place the attribute on its
own line). This has the same identity and visibility as a separate namespace binding.
Imports yield compile-time namespaces, never runtime values.
Canonical paths share one source and nominal identity across repeated/diamond
imports; cycles diagnose. Declarations and methods are file-private by default;
both named and file imports expose only `@[public]` declarations. Public types do
not automatically expose their methods. Aliases/re-exports and implicit
`@[stringify]` conversion preserve access checks: a marked converter must itself
be `@[public]` for cross-file printing/interpolation, including nested values.
Private or malformed marked converters reject rather than falling back to
structural formatting. The converter may have any name and must be an ordinary
immutable container method with a body and exact `(SelfType) -> string` signature.
At most one argument-free `@[stringify]` is allowed per struct/union/enum; ordinary
methods in generic containers are allowed, but the converter's own generic
function parameters and `@[noreturn]`/`@[test]`/`@[export]`/`@[extern]` are not.
Unmarked `toString` methods are ordinary callable methods with no print effect.
See the root README's [converter contract](../README.md#explicit-stringify-converters).
There is no `@[private]` attribute.

The actual library is tested through real imports in semantic, emit-C, build/run,
formatter, and relocated-install checks. `examples/results.nov` and
`examples/structured-errors.nov` use `std.Result` rather than copied definitions.

Filesystem and other OS-resource APIs remain outside this first library slice.
