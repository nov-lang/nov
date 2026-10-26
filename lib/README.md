# Nov standard library foundation

Import the standard namespace and alias its ordinary union templates:

```nov
let std = @import("std")
let Result = std.Result
let Option = std.Option
```

`std.nov` is the entry module. It publicly re-exports `Result` from `result.nov`
and `Option` from `option.nov`; neither is a compiler builtin or automatically
injected bare name.

## Result

`Result(T, E)` has variants `ok: T` and `err: E`.

- `value.isOk()` / `value.isErr()` report the active variant as a bool, without
  inspecting or unwrapping its payload. They also work with void payloads.

- `value.unwrapOr(fallback)` returns the success payload or the supplied fallback.
  The fallback expression is an ordinary argument and always evaluates.
- `value.unwrapOrElse(callback)` returns the success payload or calls the callback
  with the error payload. The callback expression evaluates normally, but its body
  runs only for an error. Callbacks are ordinary function values, not closures.
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

## Loading and installation

`zig build` and `zig build release` install this directory at `<prefix>/lib/nov`;
Zig package sources include it as well. The installed compiler resolves `"std"`
from executable-relative `../lib/nov/std.nov`. Semantic check, emit-c, build, and
run accept `--lib-dir <path>` to override that library root. There is no implicit
working-directory library search or embedded developer-checkout path.

Explicit relative `.nov` imports resolve from the importing file, not the process
working directory. For example, `@import("result.nov")` in `std.nov` refers to this
directory's file. Imports yield compile-time namespaces, never runtime values.
Canonical paths share one source and nominal identity across repeated/diamond
imports; cycles diagnose. Declarations and methods are file-private by default;
both named and file imports expose only `@[public]` declarations. Public types do
not automatically expose their methods. Aliases/re-exports and implicit `toString`
calls preserve access checks. There is no `@[private]` attribute.

The actual library is tested through real imports in semantic, emit-C, build/run,
formatter, and relocated-install checks. `examples/results.nov` and
`examples/structured-errors.nov` use `std.Result` rather than copied definitions.

Filesystem and other OS-resource APIs remain outside this first library slice.
