# TODO
Next step: add new IR that
- forward declaration
- resolve literals
- desugar operator overloading?
- have more usage safety than Ast
- start storing values in a constant pool

AstGen/Sema from zig looks to complex if we just want to generate some C code.
Move to a custom IR?

Figure out what (first) backend to chose
- **A language, probably C** ~but can be Zig or something else (JS)~
- A VM, a custom one (support JIT) or the JVM (or WASM)
- Native, either unoptimized for fun or dig into LLVM

Keep to the main features aka:
- functional: functions are first class citizen (?)
- async: good and lightweight async support is mandatory (no semantic yet)
- GC: nov should handle everything memory related
- interoperability with C unless it harms the features above
Keep Nov away from:
- Really low level stuff aka assembly, kernel or drivers. Use zig instead.

# Proposals and stuff to add
- transform this file into GitHub issues
- commands:
  - no cmd: same as build
  - build: output a compiled file, 2 modes debug & release (ReleaseSafe)
  - run: execute a nov file
  - ~repl: run the repl~
  - test: run unit tests
  - fmt: format nov code
    - works like zig fmt but for nov
- add tests, mainly for Parser, IRs, Codegen and Runtime
- add render.zig which render an Ast, core of fmt and used for testing parser
  - based on std.zig.render from https://github.com/ziglang/zig/pull/21727
- Error type for each step (~Tokenizer, Parser~, IRs, Codegen, Runtime)
- handle SIG.INT correctly, need to write an alternative to isocline in zig
  - [zigline](https://github.com/alimpfard/zigline)?
- render (parser) error with caret under the error + full line info
- add Timer for parsing_time, cod, 2 modes debug & release (ReleaseSafe)egen_time, runnning_time (or use tracy)
- implement correct leaking allocation to have fast exit time?
- add `let _ =` or `_ =` to discard the return value of an expression, it's mandatory to not ignore the returned value if it's != void
- add operator overloading:
  - compilation error when trying to use an operator for values of different types
  - compilation error when trying to use an operator that is not defined for the said value
  - `+` binary op is syntaxic sugar for Object.add, or iadd/fadd for int/float
  - How to handle string * int?
    - use String.repeat() (how to make it comptime?)
  - How to handle int + float? or int + uint? or BigInt + int?
    - have to cast variable e.g. `1.0 + @as(float, 3)`
    - have seemless transition between numbers types, at least for int and uint
    - for BigInt use addScalar
- optimize tail call recursion
- tree shaking
- constant folding
- write nov website with nov as backend? idk
- add cache either in cwd or in $XDG_CACHE_HOME/nov/... (ofc finding the dir is
  handled by known-folders) like \_\_pycache__
- add a way to have better OOP, interface, traits etc idk I hate OOP but it
  sometimes stuff sucks without correct OOP
- add interfaces? (no)
- autodoc with doc comments `;;;`
- package system
- [compile time pseudo variables](https://docs.vlang.io/conditional-compilation.html#compile-time-pseudo-variables)?
  - like builtin module in zig
- formal grammar definition
  - https://craftinginterpreters.com/appendix-i.html
  - https://github.com/ziglang/zig-spec/blob/master/grammar/grammar.peg
  - https://doc.rust-lang.org/stable/reference/introduction.html
- IO API:
  - like zig: thin wrapper around read()/write() with optional buffering
  - same as C
  - buffering and async by default? (maybe customizable with compiler option)
- How to handle arithmetic overflow? current behaviour is to wrap,
  we don't want crash on arithmetic overflow
- add regex pattern as builtin type?
- [Switch Prongs Defined as Comptime-Known Arrays](https://github.com/ziglang/zig/issues/21507)
- dependant types?
- In std provide a simple way to create Struct of Arrays
  - `soa[.field][i]` and `soa[i]` with combining results
  - `&soa[i]` not possible
- Add in scope closure so `xxx.orelse(return 0)` could smh work?
- Look at [gleam](https://gleam.run) parser because they have no ; and cool expr
- Look at https://tour.gleam.run/data-types/bit-arrays/ idk if it's useful in Nov
- change attributes as a special anonymous struct?
  - from
```nov
@[public]
@[packed]
let myStruct = struct {}
```
  - to
```nov
@[ .visibility = .public, .layout = .packed ]
let myStruct = struct {}
```
- Support [APE](https://justine.lol/ape.html) & [Cosmopolitan](https://github.com/jart/cosmopolitan)? / Use it by default?
- Support casting? (check [this](https://c3-lang.org/language-rules/conversion/))
- type as values
- add `any` similar to zig's `anytype`
- assign destructure like zig
- make slice immutable or find a way to differentiate ref from copy...
- embed tcc into the compilator? (revive the repl proposal)
- arbitrary sized integers?
- support relative import e.g. `@import("github.com/nov-lang/idk_lib")`
- support circular imports
- variable number of parameters in a function?
  - implem `any` instead which works like zig's `anytype`
- support C variadic functions?
- add a way to convert a int to enum and enum to int.
- add a way to create an enum from a string.
- `opaque` container...
- add a [cons](https://en.wikipedia.org/wiki/Cons) operator instead of doing
  `+ [x]` to append a single element to an array?
- I think that we can actually allow parenthesis for grouping since function is
  now a type so it shouldn't make any issue in the parser
  - test this after implementing basic IR/CodeGen
  - note that this conflicts with one of the closure proposal
- Move `@print` etc... to stdlib? (either need to wrap args in an anonymous
  struct or allow to pass a variadic amount of parameters to a function).
- Make `*` always `*mut`? What are the positives and negatives of this change?
- Change string interpolation to
  `"{varname:[fill][alignment][width][.precision]:[custom options]}"`
  and use format like zig instead of toString?
- **Rework Parser to remove newline if they are unnecessary**
  - idk about render but this will simplify other steps a lot
- provide a way to duplicate a function based on parameters?
  - useful when an argument can be comptime or runtime
- Add `unroll` keyword to `match` and `for`, works like `inline` in zig
- Eliminate hidden pass-by-reference from the codebase, i.e. use \*const more ofth for params
- Make type optional (default to `any`) for function param?
- Remove c_int, c_long, ...  from language and add them to std.c smh instead.

# proposal: add Traits or something similar
Currently many proposals are about using arbitrary declaration for overloading
behavior like `.toString()` overwrite the default format, `.len` override idk
what, `.next()` implem an iterator, etc...

I think it's a better idea to uniformize all of that with specific description
like traits which should make the language easier to understand and read.

Add a way to tell that a container implement a certain behavior, maybe
`@[impl(Iterator)]`, need a way to describe behavior.

https://github.com/ziglang/zig/issues/1268

# proposal: Annotations for container fields aka Tags
add [tags](https://github.com/Hejsil/zig-clap/issues/8#issuecomment-381637825)
to container fields, this is a must have (e.g. for arg parsing or json parsing)

this can allow to specify if a field is public/private or if it's constant/mutable
```nov
let User = struct {
    @[key]
    @[auto]
    id: Id,

    name: string,

    @[unique]
    email: string,

    todos: []Todo,

    moto: Option(string),
}

let Todo = struct {
    @[key]
    @[auto]
    id: Id,

    @[index]
    user_id: Id(User),

    @[relation(key = user_id, references = id)]
    user: User,

    title: string,
}
```

# Closures, anonymous functions & bind
Missing README examples:
- return a function
- return a closure
- create a closure
- pass a closure as argument
- ...?

```nov
let forEach: (a: *mut []int, f: (int) -> int) = {
    for i in 0..a.len {
        a[i] = f(a[i])
    }
}
let a = [1, 2, 3]
; we need to specify the name of the argument but that's it (?)
; no need to specify the return type since it's inferred, no need for ->
; no need to specify the type of params sicne it's inferred
; body can be an expr since || delimit it
;
; so we have 2 types of functions, classic functions and instant function that
; needs to name its parameter but everything else is inferred from type,
forEach(a, |x| x + 1)

; similar to rust
let add = |a: int, b: int| -> int { a + b }
let add = |a, b| a + b

let x = [1, 2, 3].map(|x| x + 1).filter(|x| x % 2).sum()

; similar to c++
; []: capture list
; (): arguments
; {}: body
let add = [](a: int, b: int) { a + b }

; proposal: allow for below syntax so there is no difference between a function and a closure
; body block is necessary if we specify type
; pretty hard to parse if we accept grouped expr!
let add = (a: int, b: int) -> int { a + b }

; closure / anon/instant function
; proposal
let plusF: (x: *mut int, f: () -> int) = {
    x.* += f()
}
let n = 3
plusF(&n, () 3)
plusF(&n, () {
    let x = 120
    let y = 300
    return {x * x} / y - 3
})
; if f actually took an argument, f: (int) -> int
plusF(&n, (n) n)
plusF(&n, (n) {
    let x = 120
    let y = 300
    return {x * x} / y - n
})
; we can specify all infos like a function type
; body block is necessary if we specify type
plusF(&n, (n: int) -> void {n}

; or closure like
(arg) -> body

; bind
let x = Option.none[]
    >>= (arg) -> {body}
```

# Union
Problem: invalidating an interior pointer
```nov
@[public]
let main: () -> !void = {
    let Value = union {
        s: string,
        n: uint,
    }
    let mut value = Value[ .n = 42 ]
    let number = &value.n
    value = Value[ .s = "hello world" ]
    number.* -= 42
    ; outputs memory garbage
    @println(value.s)
    @println(number.*)
}
```
Solutions?:
- Disallow reassignment on union
  - In what situation is this an issue?
  - This means that we only need to alloc for the tag + size of the field we
    use instead of tag + max size of fields thus saving memory

# If
Add catch syntax for unions?
```nov
if expr |val| {
    ...
} else |err| {
    ...
}
```

# Arrays
Side note about mutability. A constant array cannot be modified in any way, its
values are constant too. A mutable array can be reassigned/extanded and its
values can be modified. TODO: talk about * and *mut array.

Arrays don't have methods? (only a .len field)

TODO:
- keep arrays like that aka growable size list etc... or move to static array like zig?
  - probably continue like that because it's garbage collected and easier for programmer ig
- add a way to initialize the capacity of an array without adding any element
  - `let x = []int.initCapacity(50)` x is an array with len = 0 but with capacity = 50
  - use a builtin?
  - `let x: [50]int`
  - `let x = []int{0}.repeat(50)`
- add a way to repeat an array like a string, allow to use that as an initializer
  - `let x = [1].repeat(50)` x is an array of 50 int with value 1
  - `let x = [@as(uint, 1)].repeat(50)` x is an array of 50 uint with value 1
  - `let x: []uint = [1].repeat(50)` x is an array of 50 uint with value 1

Store arrays/strings as:
```
buffer: [*]T
len: u32
capacity: u32 // 0 when literal
```
Use uint instead of u32?
-> is it better to save a bunch of memory or handle arrays of more than 4GB?

See [1](https://docs.vlang.io/v-types.html#array-methods)
[2](https://docs.vlang.io/v-types.html#array-method-chaining)
[3](https://tour.gleam.run/standard-library/list-module/)

```
; experimentation
; [1,2,3].map(a => a + 1).filter(b => b != 3)
[1,2,3].map(|a| -> a + 1).filter(|b| -> b != 3)

; [1,2,3] >>= \a -> [a+1] >>= \b -> if b == 3 then [] else [b]
;
; do
;   a <- [1,2,3]
;   b <- [a+1]
;   if b == 3 then [] else [b]
[1,2,3] >>= |a| -> [a + 1] >>= |b| -> if b == 3 { [] } else { [b] }
```

# Higher-order functions
Highly experimental syntax
```nov
; Traditional imperative loop
let num_list = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
let mut result = 0
for i in 0..num_list.len {
    if num_list[i] % 2 == 0 {
        result += num_list[i] * 10
    }
}

; With Higher-order functions
let result = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    .filter(|n| n % 2 == 0)
    .map(|a| a * 10)
    .reduce(|a, b| a + b)
```

# Slice
TODO: https://docs.vlang.io/v-types.html#array-slices

slicing creates a ref by default unless type attribute state otherwise?

# Map
TODO: https://docs.vlang.io/v-types.html#maps

This will probably be implemented in stdlib instead.

# Match
TODO: check https://docs.vlang.io/statements-&-expressions.html#match

TODO: check https://tour.gleam.run/everything/#flow-control-case-expressions
- rework how match work and remove catch syntax
  - add [Flow-sensitive typing](https://en.wikipedia.org/wiki/Flow-sensitive_typing)?
    that could solve the issue with `if`

proposal: match over multiple values
```nov
_ = match x % 3, x % 5 {
    0, 0 => "FizzBuzz"
    0, _ => "Fizz"
    _, 0 => "Buzz"
    _, _ => "{x}" ; use `_` instead of `_, _`?
}
```

proposal: match over array
proposal2: revert to using `,` to separate values and allow match over
arrays/tuples instead of match over multiple values (no, allow both)
```nov
_ = match [1, 2, 3] {
    [] => "Empty array"
    [1] => "Array of just 1"
    [4, ..] => "Array starting with 4"
    [_, _] => "Array of 2 elements"
    _ => "Some other array"
}
```

proposal: match over start/end of string
```nov
let getName: (s: string) -> string = {
    match s {
        ; "Foo".. => TODO idk
        ; .."Bar" => TODO idk
        "Hello, ".. => |name| name ; no this is the full string? how to catch just the end?
        _ => "Unknown"
    }
}
```

# Loop
- loop over multiple values with iterators, how? they need to have the same length
- allow backward iteration, this means that indexes are int instead of uint?
  - what about backward iteration on arrays, etc...?
```nov
let arr = [0, 4, 5, 1]
for i in 3..=0 {
    @println(arr[i])
}
```
- allow type annotation? (use that instead of taking *mut/& of item?)
```nov
let arr = [1, 2, 3, 4]
for x: int in arr {
    @println(x)
}
```

## Iterators
- an iterator is any object with a public .next() method that returns an
  Option(T) e.g. `next: (*T) -> ?U` (yes)
  - need a `(*T) -> ?*mut U` (no, \*mut is included in U if needed)
  - It's fine to iterate on an array and an iterator at the same time. It will
    panic if the iterator is null when array still has elelemnts and when
    iterator is not null when array as no more elements.
- use &[], [] and .len operator overloading instead? (no, too complicated and doesn't work for e.g. tokenizer)
  - []: `(*T, int) -> U`
  - &[]: `(*T, int) -> *mut U`
  - this is better than .next() because we know the len and [] op overloading
    is a cool side effect but it's a pain to have separate [] and .len
    overloading doesn't make much sense
- use async with yield for iterators

## Use after realloc / iterator invalidation
Make all of these work fine in Nov.
Make sure that all kind of Nov loop work fine.

Proposal:
- make it work by definition for all primitives (i.e. everything but iterators)
- for iterator do it like rust aka force immutability and stuff to prevent this from happening
```zig
const std = @import("std");

pub fn main() !void {
    const init_queue = [5]usize{ 0, 1, 2, 3, 4 };
    var queue = try std.ArrayList(usize).initCapacity(std.heap.page_allocator, init_queue.len);
    defer queue.deinit();
    try queue.appendSlice(&init_queue);
    // var i: usize = 0;

    // works fine...
    // const len = queue.items.len;
    // while (i < len) : (i += 1) {
    //     const item = queue.items[i];
    //     try queue.append(item);
    // }

    // infinite loop (expected behavior)
    // while (i < queue.items.len) : (i += 1) {
    //     const item = queue.items[i];
    //     try queue.append(item);
    // }

    // segmentation fault
    // for (queue.items) |*item| {
    //     item.* += 1;
    //     try queue.append(item.*);
    // }
    std.debug.print("{any}\n", .{queue.items});
}
```

# Operator Overloading
TODO:
- `>>=`: `(*T, (*T) -> U) -> U`
- `[]`: `(*T, int) -> U`
  - probably too complex since we should be able to overload it twice with `U` and `*mut U`
- `.?`: unwrap...
- for unary negation `-`, autogen it from `-` operator and `zero` decl?

# C FFI
TODO
```nov
@[extern("malloc")]
let c_malloc: (uint) -> voidptr

@[extern("malloc")]
let c_malloc: (uint) -> voidptr = {} ; error extern fn can't have a body

@[export("nov_add")]
let add: (a: c_int, b: c_int) -> c_int = a + b
```

# Concurrency
TODO: add async/await/yield

What we want:
- Green threads i.e. lightweight user space threads that can run on multiple Kernel threads
  - this is what go does

Notes from the ignorant:
- Fibers and Async/Await tend to always have function coloring but are lightweight.
- "Goroutines" do not have color but are harder to implement? and heavier because relying "mostly" on OS threads.
- Thread Pool are easy to implement but only use kernel thread and are not incorporated into the language.

## [Fibers](https://en.m.wikipedia.org/wiki/Fiber_(computer_science))
- wren [fibers](https://wren.io/concurrency.html)
- buzz [fibers](https://buzz-lang.dev/guide/fibers.html)
- https://medium.com/the-tech-collective/the-unbelievable-simplicity-of-fibers-3339097948c4
  - https://github.com/roscopeco/fiber-blog-example/tree/main
- zig async...
- https://ayazhafiz.com/articles/23/a-lambda-calculus-with-coroutines-and-heapless-closures

## "Goroutines"
- https://docs.vlang.io/concurrency.html
- go goroutines

## Thread Pool
- zig std.Thread.Pool

## Previous Syntax Proposal
```nov
; async func, can be run normally or asynchronous
; signature: `(int) -^ int -> void
let range: `(n: int) -^ int = {
    loop i in 0..n {
        yield i
    }
}
```

# Attributes
TODO add a good way to make code usable on multiple systems through attributes.

TODO: add an attribute for signal handler and add a way to pass parameters to it
via SA\_SIGINFO (see std.debug.attachSegfaultHandler) `@[signal_handler(signal, args)]`

# Comptime
```nov
; comptime variable?
let mut x = #0 ; no
let mut x: #int = 0 ; probably the best, #int and #uint can be bigger than their runtime equivalent
let mut #x = 0
let #mut x = 0

; change the last two change param to this to match with comptime var
let ArrayList: (#T: type) -> type = { ... }
; instead of
let ArrayList: (T: #type) -> type = { ... }
```

# Notes
- shifting warning: `x >> y` (same for `<<`)
  - error if y is signed
  - warn if y >= @bitSizeOf(x): `x >> ${y} is the same as x >> ${y % @bitSizeOf(x)}`
- Remember to check [value.zig](https://github.com/nov-lang/nov/blob/d8cc0edc95c43461e37b48a72d1e02b2307d278c/src/vm/value.zig) for cool stuff
- Declarations in top level can't be `mut`
- All arrays and strings should be 0 terminated (what about array of structs?)
- strings:
  - string is an array of u8. (u16 on windows)
    - array of rune for unicode aware strings
```nov
let s: []rune = "hey! 😀"
s.len == 6 ; true, actual size is 9
s[-1] == 😀
```
  - all strings are 0 terminated by default? or add a `toCString()` method
  - strings can't be directly modified like an array, `str[i] = ...` is a compile error
  - const strings are fixed length, stored in .rodata like a C string
    - note that the "a" + "b" is done at compile time
  - mut string are variable length, allocated "slices" (storing ptr and len), just like a []u8
  - it's a compile error to modify a string with []?
  - PROPOSAL: make all string immutable they are all pointer to rodata, that pointer can be mut
    - allow for more optimization
    - probably better semantic
    - string manipulation is done with []u8 instead
- `void` is Nov's unit type (`()` in Rust and OCaml, `Nil` in Gleam)
  - `void` only value is `[]` (`{}` in zig)
- about variable initialization:
  - initialize to zero by default if there is no initializer
  - no initialization if there is no initializer
  - require an initializer (need `undefined` keyword?)
- since Nov has first-class functions, how can we return a function?
  - do just like zig aka comptime function generation is possible e.g. with a
    generic or by making a function with comptime parameters and returning it
  - put it on the stack/heap? need executable memory which is bad, also idk how gc will handle that
  - look at how ocaml or rust does it
- comptime assertion like `let _ = #{}`?
- move generic to attributes? `@[generic(T)]`

# Stuff to check & links
- check https://docs.vlang.io/functions-2.html#anonymous-&-higher-order-functions + closures
- Check std.zig.AstGen, std.zig.Zir and zig/src/Sema.zig for IR
- Check zig/src/InternPool.zig for storing variables
- Check std.zig.start.callMainWithArgs
- Defunctionalization
  - 3591260.pdf
  - https://blog.sigplan.org/2019/12/30/defunctionalization-everybody-does-it-nobody-talks-about-it/
- about compiler codegen optimization https://github.com/vnmakarov/mir/blob/master/mir-gen.c#L26-L47
- following to @dupe/@clone https://jvns.ca/blog/2024/08/06/go-structs-copied-on-assignment/
- https://go.dev/blog/slices-intro
- Go design
  - [What are the NO's in GO design?](https://kuree.gitbooks.io/the-go-programming-language-report/content/32/text.html)
  - https://www.bluxte.net/musings/2018/04/10/go-good-bad-ugly/
- Check [SSA](https://en.wikipedia.org/wiki/Static_single-assignment_form) and
        [CPS](https://en.wikipedia.org/wiki/Continuation-passing_style)
- Build system / project configuration: https://c3-lang.org/build-your-project/project-config/
- [roc-lang](https://www.roc-lang.org/examples/FizzBuzz/README.html) function pipes usage (instead of monads)
- [ML functions](https://en.wikipedia.org/wiki/Standard_ML#Language)
- 14-Semantic-Analysis.pdf
- https://stackoverflow.com/questions/44965/what-is-a-monad
- https://en.wikipedia.org/wiki/Algebraic_data_type
- https://en.wikipedia.org/wiki/Coroutine
- https://doc.rust-lang.org/book/ch13-01-closures.html
- Result
  - https://docs.vlang.io/type-declarations.html#custom-error-types
  - https://docs.vlang.io/type-declarations.html#optionresult-types-and-error-handling
  - https://doc.rust-lang.org/std/result/index.html
  - https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html
- Change Parser.parseExprPrecedence? https://www.scattered-thoughts.net/writing/better-operator-precedence/
- https://www.scattered-thoughts.net/writing/notes-on-compiler-irs/
- Builtins
  - https://docs.vlang.io/conditional-compilation.html
  - https://docs.python.org/3.12/library/functions.html
- https://en.wikipedia.org/wiki/Clean_(programming_language)#Examples
