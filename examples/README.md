# Examples

These programs combine related features into small, runnable demonstrations.
Regression cases live in [`tests/`](../tests/), not alongside the examples.

| Program | Demonstrates |
| --- | --- |
| [`basics.nov`](basics.nov) | Fibonacci, FizzBuzz, functions, ranges and control flow |
| [`collections.nov`](collections.nov) | A score-processing pipeline using arrays and collection helpers |
| [`text.nov`](text.nov) | Unicode strings, runes, text processing and UTF-8 byte conversion |
| [`types.nov`](types.nov) | A parcel model with structs, enums, unions, methods and converters |
| [`functions.nov`](functions.nov) | Generic functions, closures, composition and captured state |
| [`errors.nov`](errors.nov) | Input validation with `Option`, `Result`, matching and propagation |
| [`references.nov`](references.nov) | Mutable/readonly references, rebinding and automatic field/method dereferencing |
| [`numbers.nov`](numbers.nov) | Integer and float types, math helpers and validated integer parsing |
| [`ffi.nov`](ffi.nov) + [`ffi.c`](ffi.c) | Calling C from Nov and Nov from C; build commands are in the Nov file |
| [`test.nov`](test.nov) | `@[test]`, assertions, filtering and test timeouts |

From the repository root:

```sh
zig build run -- run examples/collections.nov
zig build run -- test examples/test.nov
zig build run -- test examples/test.nov --filter addition --timeout-ms 1000
zig build test-examples
```

`test-examples` checks formatting, semantics and output in default, debug and
release modes, and compiles the FFI demonstration as strict C11. Only `test.nov`
contains test declarations; the other examples are ordinary programs.
