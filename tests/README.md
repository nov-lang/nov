# Regression tests and CLI fixtures

The former fine-grained examples live here so their coverage remains intact.
Files with `@[test]` declarations are language regression suites. Others supply
programs, imported modules or C harnesses to the CLI integration tests in
[`build.zig`](../build.zig); they are not all standalone test programs.

```sh
zig build test-cli
zig build test-cli-array-count test-cli-array-find-last-index
zig build run -- test tests/array-count.nov
zig build run -- test tests/array-count.nov --filter source_order --release
```

`zig build test` also runs the owning-module Zig tests and GC runtime tests.
Individual CLI targets retain their existing names and exercise formatting,
semantic checks, generated C, runtime output, imports and rejection cases as
appropriate. Test fixtures are included in source packages.

For a tutorial rather than regression coverage, see
[`examples/test.nov`](../examples/test.nov). The other consolidated demonstrations
are indexed in [`examples/README.md`](../examples/README.md).

`error.nov` remains a filesystem design sketch, not an executable conformance
suite. Do not use a blanket run of every file here as a substitute for the CLI
build targets, which select the appropriate modes and expected outcomes.
