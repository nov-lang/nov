# Nov

Nov is a functional programming language.

Nov type system is static, strong and sound. Also most types should be inferrable.

Nov has [uncolored](https://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/) async! (well not yet tho)

Nov has automatic memory management via a minimal tracing GC.
<!-- TODO: https://github.com/ivmai/bdwgc -->
<!-- TODO: autofree https://docs.vlang.io/memory-management.html -->

# TODO
Add render.zig! see line 60 of this readme

Next step: add new IR that
- forward declaration
- resolve literals
- desugar operator overloading?
- have more usage safety than Ast
- start storing values in a constant pool

AstGen/Sema from zig looks to complex if we just want to generate some C code.
Move to a custom IR?

Need to work on Parser/Ast for more complex stuff (e.g. functions, for loops,
...) after basic IR and codegen is implemented.

Separate frontend (Tokenizer, Parser, IRs) from backend (Codegen, ...).
Also separate CLI, REPL and VM from all the that.
And cleanup the repo

Figure out what (first) backend to chose
- **A language, probably C** but can be Zig or something else (JS)
- A VM, a custom one or the JVM (or WASM)
  - a custom one allow support for JIT
- Native, either unoptimized for fun or dig into LLVM

Fix this README:
- add links where there should be e.g. for the Inspirations section.
- transform proposal and TODOs into GitHub issues
- sort this README and move stuff e.g. Concepts into a specific doc

Keep to the main features aka:
- functional: functions are first class citizen
- async: good and lightweight async support is mandatory
- GC: nov should handle everything memory related
- interoperability with C unless it harms the features above
Keep Nov away from:
- Really low level stuff aka assembly, kernel or drivers. Use zig instead.

## Proposals and stuff to add
- commands:
  - no cmd: same as build
  - build: output a compiled file, 2 modes debug & release (ReleaseSafe)
  - run: execute a nov file
  - ~repl: run the repl~
  - test: run unit tests
  - fmt: format nov code
    - works like zig fmt but for nov
    - rename variable, functions and types to snake_case, camelCase and PascalCase?
- add tests, mainly for Parser, IRs, Codegen and Runtime
- add render.zig which render an Ast, core of fmt and used for testing parser
  - based on std.zig.render from https://github.com/ziglang/zig/pull/21727
- Error type for each step (~Tokenizer, Parser~, IRs, Codegen, Runtime)
- handle SIG.INT correctly, need to write an alternative to isocline in zig
- render (parser) error with caret under the error + full line info
- add Timer for parsing_time, cod, 2 modes debug & release (ReleaseSafe)egen_time, runnning_time (or use tracy)
- implement correct leaking allocation to have fast exit time?
- add `_ =` to discard the return value of an expression, it's mandatory to not
  ignore the returned value if it's != void
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
- rename `for` to `loop`?
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
- Use [chameleon](https://github.com/tr1ckydev/chameleon/) for colors
- Support [APE](https://justine.lol/ape.html)? / Use it by default?
- Support casting? (check [this](https://c3-lang.org/language-rules/conversion/))
- type as values
- add `any` similar to zig's `anytype`
- assign destructure like zig
- make slice immutable or find a way to differentiate ref from copy...
- embed tcc into the compilator?
- add [tags](https://github.com/Hejsil/zig-clap/issues/8#issuecomment-381637825)
  to container fields, this is a must have (e.g. for arg parsing or json parsing)
  - example:
```nov
let User = struct [
  @[key]
  @[auto]
  id: Id,

  name: string,

  @[unique]
  email: string,

  todos: []Todo,

  moto: Option(string),
]

let Todo = struct [
  @[key]
  @[auto]
  id: Id,

  @[index]
  user_id: Id(User),

  @[relation(key = user_id, references = id)]
  user: User,

  title: string,
]
```
- arbitrary sized integers?

## Notes
- it's a compile error to modify a string with [] or to take a mut of a string in a for loop
- shifting warning: `x >> y` (same for `<<`)
  - error if y is signed
  - warn if y >= @bitSizeOf(x): `x >> ${y} is the same as x >> ${y % @bitSizeOf(x)}`
- Remember to check src/vm/value.zig for cool stuff
- Declarations in top level can't be `mut`
- All arrays and strings should be 0 terminated (what about array of structs?)
- strings:
  - string is an array of u8. (u16 on windows)
  - all strings are 0 terminated by default? or add a `toCString()` method
  - strings can't be directly modified like an array, `str[i] = ...` is a compile error
  - const strings are fixed length, stored in .rodata like a C string
    - note that the "a" + "b" is done at compile time
  - mut string are variable length, allocated "slices" (storing ptr and len), just like a []u8
- `void` is Nov's unit type (`()` in Rust and OCaml, `Nil` in Gleam)
- about variable initialization:
  - initialize to zero by default if there is no initializer
  - no initialization if there is no initializer
  - require an initializer (need `undefined` keyword?)
- since Nov has first-class functions, how can we return a function?
  - do just like zig aka comptime function generation is possible e.g. with a
    generic or by making a function with comptime parameters and returning it
  - put it on the stack/heap? need executable memory which is bad, also idk how gc will handle that
  - look at how ocaml or rust does it

# Concepts

## Builtins
- `@import(path: string)`: Import a nov file.
  - When importing other files only declarations gets imported which means that
    if there is a print in global scope in a library it will be ignored (or
    error?)
  - support relative import e.g. `@import("github.com/nov-lang/idk_lib")`
  - support circular imports
- `@TypeOf(...)`: Returns the type of a value.
- `@typeInfo(...)`: Returns type information about a value.
- `@print(...)`: Output all args separated with a space to stdout. (supports string interpolation)
                 TODO: what about printf? what about print to another file?
- `@println(...)`: Same as print with a newline at the end.
- `@fprint(file: File, ...)`: Same as print but output to a specific file. Returns a Result<>.
<!-- - `@eprint(...)`: Same as print but output to stderr. -->
<!-- - `@printf(fmt: #string, args: ...)` works like std.fmt.format and print to stdout -->
- `@panic(s: string)`: Output `s` and backtrace to stderr, then terminate the program with error code 1.
- `@max(a: T, b: T, ...)`: Returns the maximum value between all the supplied arguments
- `@min(a: T, b: T, ...)`: Returns the minimum value between all the supplied arguments
- `@dump(expr)`: TODO: https://docs.vlang.io/builtin-functions.html#dumping-expressions-at-runtime
- `@embedFile()`: TODO, also allow for compressing the file
- `@sizeOf()`: TODO
- `@bitSizeOf()`: TODO
<!-- - `@as(T: type, arg: any)`: Cast arg to T if possible. -->
<!-- - `@bitCast()`: Same as zig -->
- see https://docs.vlang.io/conditional-compilation.html
- see https://docs.python.org/3.12/library/functions.html
- `@swap(a: any, b: @Typeof(a))`
- `@unreachable()`: TODO
- atomic stuff
- `@memcpy()` or `@dupe()`/`@clone()`?
- math func? prob no, put them in std.math
<!-- - `@fieldParentRef()` similar to `@fieldParentPtr()` from zig, useful for C-style OOP -->

TODO: builtin variables
- os type
- architecture
- support for i128...
- optimization level
- safety check enabled
- current file name
- current func name
- current line

## Functions
Parameters are immutable by default.

TODO: variable number of parameters? (just use an array)

TODO: check https://docs.vlang.io/functions-2.html#anonymous-&-higher-order-functions + closures
```nov
; signature: `() -> void
let doNothing: `() = {}

; here x is passed as reference and its value is mutable
; signature: `(*mut int) -> void
let retNothing: `(x: *mut int) = {
    x += 1
}

; functions body doesn't need to be a block
; signature: `() -> int
let ret2: `() -> int = 2

; signature: `(int, int) -> int
let add: `(a: int, b: int) -> int = a + b

; return a tuple
; signature: (int, int) -> div__struct_xxxx
let div: `(a: int, b: int) -> struct[int, int] = {
    [a / b, a % b]
}

; generic functions
; signature: (#type, any, any) -> bool
let eql: (T: #type, x: T, y: T) -> bool = x == y
eql(int, 1, 2) ; returns false
eql(int, true, 0) ; compile error

let add: (T: #type, x: T, y: T) -> T = x + y
; it's easy to create a custom function from a generic one
let addInt: (x: int, y: int) -> int = add(int, x, y)
addInt(1, 2) ; returns 3
add(string, "he", "llo") ; returns "hello"
add(bool, true, false) ; compile error: bool doesn't support `+` operator
```

## Enum
Nov enums are just like C enums except that they can have methods and are not
global.

TODO:
- is it possible to do `let x: MyEnum = .xxx` also for match do we specify the `.`?
- add a way to convert a int to enum and enum to int.
- add a way to create an enum from a string.
```nov
let MyEnum = enum {
    x ; default to 0
    y = 5
    z ; default to y + 1 = 6

    let eql: `(self: MyEnum, other: MyEnum) -> bool = {
        self == other
    }
}

let x: MyEnum = MyEnum.x
let y = MyEnum.y ; type is inferred
x.eql(y) |> @println ; print false

match x {
    x => ... ; do something
    y, z => {} ; do nothing
}
```

## Struct
TODO:
- add a way to init a struct without its type like in zig or c99?
- support anonymous structs, useful for json, I think it's easy since struct is
  like a function that returns a type but syntax is ugly outside of a struct
  which is fine and intended
- make fields immutable by default? -> add mut keyword to fields
- pub/priv keyword on struct fields?
- do like C++/V for telling what is pub/mut?
```nov
let MyStruct = struct {
    name: string
    x: float = 1.
    y: float = 1.
    z: float = 1.

    let max = 100.

    let init: `(name: string) -> MyStruct = MyStruct{ .name = name }

    ; TODO: should we omit the type on self?
    let eql: `(self: MyStruct, other: MyStruct) -> bool = {
        ; side note, parenthesis allows to bypass newline checks
        return (
            self.name == other.name and
            self.x == other.x and
            self.y == other.y and
            self.z == other.z
        )
    }
}

let a = MyStruct{} ; error: missing struct field: name

let x = MyStruct.init("aaa")
let y = MyStruct{ .name = "aaa" }
x.eql(y) ; true
x |> @println ; idk what this prints
```

## Union
See [Result and Option unions](#result-and-option-unions) for another example.
```nov
let myEnum = enum {
    a
    b
    c
    d
}

; with an enum
let MyUnion = union(MyEnum) {
    a: int
    b: float
    c: int
    d: string
}

let x = MyUnion{ .b = 3 };
x.a ; error

; or without, both are tagged
let Tree = union {
    empty
    node: struct {
        value: int
        left: Tree
        right: Tree
    }

    let sum: `(self: Tree) -> int = {
        ; TODO: match with .empty?
        match self {
            empty => 0
            node => |n| n.value + n.left.sum() + n.right.sum()
        }

        ; we could also use an if here
        ; TODO: will this cause problem with operator overloading?
        ;       maybe replace with `is` keyword and add a | | for union?
        if self == empty {
            0
        } else {
            n = n.node
            n.value + n.left.sum() + n.right.sum()
        }
    }
}
```

Problem:
```nov
@[public]
let main: () -> !void = {
    ; invalidating an interior pointer
    let Value = union [
        s: string,
        n: uint,
    ]
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

## Generics
similar to zig e.g. generate a type with a func

```nov
let Stack: (T: type) -> type = struct [
    list: []T = [],

    let Self = @This()

    @[public]
    let push: (self: *mut Self, value: T) = {
        self.list += [value]
    }

    @[public]
    let pop: (self: *mut Self) -> ?T = {
        let value = self.list[self.list.len - 1]
        self.list.len -= 1
        return value
    }

    @[public]
    let isEmpty: (self: Self) -> bool = self.list.len == 0
]
```

See [Result and Option unions](#result-and-option-unions) for example.

## Result and Option unions
The Result and Option unions are created with functions since types are values.
In nov there is no untagged union thus  we can match on any union to find the
active field. Note that it isn't represented here but an union field can have a
default value just like a struct field.

Sugar for Result and Option:
- !T is Result(T, string) or Result(T, any)?
- E!T is Result(T, E)
- ?T is Option(T)
- expr.! is unwrap or propagate for result
- expr.? is unwrap or propagate for option

TODO:
- allow for this syntax `let a: MyUnion = .field{value}`
  - current syntax is `MyUnion{ .field = value }` or `.{ .field = value }` with inference
- add a way to merge multiple error type into one?
- find a way to reduce nested Result
```
`(Result<T>, `(T) -> U) -> Result<U> ; replace the value type but keep the same error
`(Result<T>, `(T) -> Result<U>) -> Result<U> ; bind, see https://doc.rust-lang.org/std/result/enum.Result.html#method.and_then
```
- remove type sugar and only keep `.!` and `.?`?
  - keep `?T` because it's simple and allow for good C interop with ptr/ref because it can be NULL
- Check https://docs.vlang.io/type-declarations.html#custom-error-types
- Check https://docs.vlang.io/type-declarations.html#optionresult-types-and-error-handling
- Check https://doc.rust-lang.org/std/result/index.html
- add `else {}` sugar which works like `orelse` or `catch` in zig (`else |val| {}`)?

```nov
let Result = union<T, E> {
    ok: T
    err: E

    ; TODO:
    ;  - keep .{} syntax? (no)
    ;  - Generic works like a function...
    let newError: `(err: E) -> Result = .{ .err = err }
    ; same as
    let newError: `(err: E) -> Result<T, E> = Result<T, E>{ .err = err }

    ; same comment as orelse below
    @[public]
    @[inline]
    let catch: `(self: Result, op: `(E) -> T) -> T = {
        match self {
            .ok => |val| val
            .err => |err| op(err)
        }
    }
}

let MyResult = Result<int, string>
let my_err = MyResult.newError("my error string")

; error handling
let file = match File.open("file.txt") {
    .ok => |f| f
    .err => |err| match err.kind {
        not_found => {
            ; do something
        }
        _ => return err ; return early with the error
    }
}
; .! unwrap and returns the err if there is any
let file = File.open("file.txt").!

let Option = union<T> {
    some: T
    none

    ; unions can also have methods
    ; side note here orelse takes a function and not a value so the fallback is
    ; only evaluated when needed
    ; TODO: have both normal and lazy version (this one is lazy obviously)
    @[public]
    @[inline]
    let orelse: `(self: Option, fallback: `() -> T) -> T = {
        match self {
            .some => |val| val ; catch the value and return it
            .none => fallback()
        }
    }
}

let x = 5
let y = Option<int>{ .some = 5 }
let sum = x + y.orelse(0)

let MyOption = Option<float>
let a = MyOption{ .some = 1.0 }
let b = MyOption{ .none }
let prod = x.? * y.?
```

## Arrays
Side note about mutability. A constant array cannot be modified in any way, its
values are constant too. A mutable array can be reassigned/extanded and its
values can be modified. TODO: talk about * and *mut array.

Arrays don't have methods?, only a .len field.

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
- use zig/go syntax?

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
```nov
let mut my_array = [1, 2, 3]
@TypeOf(my_array) ; returns []int
; arrays have a special field for its len, it is mutable if the string is mutable
my_array.len == 3 ; true
my_array[0] == 1 ; true
my_array[-1] == 3 ; true
; append an element
my_array += [5]
my_array |> @println ; prints [1, 2, 3, 5]
; append multiple elements
my_array += [1, 1, 7]
my_array |> @println ; prints [1, 2, 3, 5, 1, 1, 7]
; remove the last element
my_array.len -= 1
my_array |> @println ; prints [1, 2, 3, 5, 1, 1]
my_array = []
@TypeOf(my_array) ; still return []int
; check if an element is in the array
6 in my_array ; false

let my_array_of_array = [["Hello", "World!"], ["Bonjour", "Monde!"]]
@TypeOf(my_array_of_array) ; returns [][]string

; proposals about copy/ref of variables
; side note, it kinda sucks to hide through type is something is copied or not
; we're trying to solve that btw https://jvns.ca/blog/2024/08/06/go-structs-copied-on-assignment/
; alias because it's long to type. we specify the type so it's passed by
; reference and create an actual alias instead of copying the data
let arr_arr: *[][]string = my_array_of_array
arr_arr.len == 2 ; true

; this works like a pointer in C because it's mutable, note that the compiler
; will emit an error if it is never mutated like here
let mut ref_arr: *[][]string = my_array_of_array

; this is a copy
let arr_copy = my_array_of_array

; error type mismatch my_array_of_array is immutable
let arr_error: *mut [][]string = my_array_of_array

; python like way of printing the array
for arr in arr_arr {
    for w in arr {
        @print(w + " ")
    }
    @println()
}

; functional way, I think
arr_arr.map(|arr| {
    arr.map(|word| word + " " |> @print)
    @println
}

; with monad bind operator
; type annotation is optional
arr_arr >>= |arr: []string| {
    arr >>= |word| word + " " |> @print
    @println()
}

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

## Slice
TODO: https://docs.vlang.io/v-types.html#array-slices

slicing creates a ref by default unless type annotation state otherwise?

## Map
TODO: https://docs.vlang.io/v-types.html#maps

Our implem is different, check src/value.zig.

implem this in stdlib instead?

## Match
TODO: check https://docs.vlang.io/statements-&-expressions.html#match

TODO: add inline like zig (need a different keyword) or make it implicit?

```nov
_ = match 5 {
    0 | 1 | 2 => ... ; use `|` to specify multiple cases
    {0b100 | 0b001} => ... ; wrap your expr in a {} to use `|` bitwise operator
    10...20 => ... ; use `x...y` to match over x to y included, [x;y]
    21 => {} ; {} does nothing, it's a empty block
    _ => {} ; _ correspond to every other possible values
}
```

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
let getName: `(s: string) -> string = {
    match s {
        ; "Foo".. => TODO idk
        ; .."Bar" => TODO idk
        "Hello, ".. => |name| name ; no this is the full string? how to catch just the end?
        _ => "Unknown"
    }
}
```

## If/Else
TODO: add if unwrapping sugar for Result and Option?
```nov
let a = 10
let b = 20
; braces are mandatory, else is optional
if a < b {
    @println("${a} < ${b}")
} else if a > b {
    @println("${a} > ${b}")
} else {
    @println("${a} == ${b}")
}

; all If are expression which means that they all return a value
; the previous if returns `void`
; this one returns a bool
let is_even = if 69 % 2 == 0 { true } else { false }
; another example which returns an Option(int)
let x: ?int = if is_even {
    @println("even")
    42
} else {
    @println("not even")
    .none
}
```

## For loop
```nov
; names can be an array, a slice, a string or an iterator
; an iterator is any object with a public .next() method that returns an Option(T)
; next: `(T) -> ?U ; U may not be directly related to T
; proposal: or use [] and .len operator overloading
;           what about getting reference or making assignement to object[i]
; proposal: use async with yield for iterators
; type is always inferred
for name in names {
}

; n takes values [0;10[
; note than the boundary doesn't need to be literal
; for n in x..y {} is fine as long as x and y are unsigned integers
for n in 0..10 {
}

; it's possible to loop on multiple values at the same time as long as they
; have the same length
; TODO: what about iterators?
for name, i in names, 0.. {
}

; loop over a map (need parenthesis or give a kv "object" (in compiler it will
; be replaces by 2 values but in code it will look like an object) instead?)
for k, v in m {
}

; values are const by default, add *mut to take a ref to a mutable value
; a range cannot be mutable
for *mut name in names {
}

; use underscore to ignore a value
for _ in 0..10 {
}

; we can also loop on a condition
for x > 10 {
}

; infinite loop
for {
}

; "zig while" for loop
let mut i = 0
for i < 100 : i += 2 {
}

; else branch
; the else branch is evaluated when the loop is not exited with a break
let rangeHasNumber: `(begin: uint, end: uint, number: uint) -> bool = {
    var i = begin;
    return for i < end : i += 1 {
        if i == number {
            break true;
        }
    } else {
        false;
    }
}
```

## Break & Continue
TODO: same as zig

## Defer
TODO: same as zig

## In
Check if an element is in an array or if it's a key in a map.
This should be faster than using a for loop because it should be vectorized.
(with std.mem.indexOfScalar)
```
let nums = [1, 2, 3]
@println(1 in nums) ; true
@println(5 in nums) ; false
```

## Assignment
```nov
let mut a = 3 ; @TypeOf(a) == int
let b = 4 ; @TypeOf(b) == int

let x = y = 3 ; parse error
let x = a += b ; parse error
let x = {y = 3} ; should be fine, @TypeOf(x) == void
let z += 3 ; compile error, z is not defined
```

## Higher-order functions
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

## Primitive Types
TODO
- bool
- string
- rune

See also:
- [Integers](#Integers)
- [Floats](#Floats)
- [Arrays](#Arrays)
- Maps

## Integers
TODO
- c_int, ...
- u8, u16, u32, u64, u128, i8, i16, i32, i64, i128, f16, f32, f64, f80, f128
  - int = i32 or i64 based on architecture
  - uint = u32 or u64 based on architecture
  - float = f32 or f64 based on architecture

## Floats
- `float` - f32 or f64 depending on architecture
- `f16` - IEEE-754-2008 binary16
- `f32` - IEEE-754-2008 binary32
- `f64` - IEEE-754-2008 binary64
- `f80` - IEEE-754-2008 80-bit extended precision
- `f128` - IEEE-754-2008 binary128
- `c_longdouble` - matches long double for the target C ABI

## Operators
<!-- Yes this is not really lisible as raw text -->
| Name                  | Syntax            | Types                                        | Remarks                                                             |
|-----------------------|-------------------|----------------------------------------------|---------------------------------------------------------------------|
| Assignment            | a = b             | All types                                    | `a` is an identifier and `b` is any type.                           |
| Addition              | a + b <br> a += b | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Concatenation         | a + b <br> a += b | string <br> [Arrays](#Arrays)                | TODO                                                                |
| Substraction          | a - b <br> a -= b | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Negation              | -a                | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Multiplication        | a * b <br> a *= b | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Division              | a / b <br> a /= b | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Remainder Division    | a % b <br> a %= b | [Integers](#Integers)                        | TODO                                                                |
| Bit Shift Left        | a << b            | [Integers](#Integers)                        | TODO                                                                |
| Bit Shift Right       | a >> b            | [Integers](#Integers)                        | TODO                                                                |
| Bitwise And           | a & b             | [Integers](#Integers)                        | TODO                                                                |
| Bitwise Or            | a \| b            | [Integers](#Integers)                        | TODO                                                                |
| Bitwise Xor           | a ^ b             | [Integers](#Integers)                        | TODO                                                                |
| Bitwise Not           | ~a                | [Integers](#Integers)                        | TODO                                                                |
| Optionify             | ?T                | All types                                    | Equivalent to Option(T)                                             |
| Optional Unwrap       | a.?               | Option                                       | Unwrap an Option type or return it's error.                         |
| Resultify             | E!T <br> !T       | All types                                    | Equivalent to Result(T, E) <br> Equivalent to Result(T, string).    |
| Result Unwrap         | a.!               | Result                                       | Unwrap a Result type or return it's error. (same as `try` in zig)   |
| Logical And           | a **and** b       | bool                                         | TODO                                                                |
| Logical Or            | a **or** b        | bool                                         | TODO                                                                |
| Boolean Not           | !a                | bool                                         | TODO                                                                |
| Equality              | a == b            | All types                                    | TODO                                                                |
| Inequality            | a != b            | All types                                    | TODO                                                                |
| Greater Than          | a > b             | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Greater or Equal      | a >= b            | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Less Than             | a < b             | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Less or Equal         | a <= b            | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Bind                  | a >>= \|b\| ...   | Monads                                       | TODO                                                                |
| Function Pipe         | a \|> f           | [Functions](#Functions)                      | TODO                                                                |
| Member Search         | a **in** b        | [Arrays](#Arrays) <br> Maps                  | TODO                                                                |
| Access                | a\[b]             | [Arrays](#Arrays) <br> Maps <br> string      | TODO: for Arrays and string b is an Integer, for Maps it's a key    |
| Field / Method Access | a.b               | All types                                    | TODO                                                                |
| Reference Type        | *T <br> *mut T    | All types                                    | Create a reference type from `T`. Unless `mut` is specified the wrapped value is constant |
| Reference Of          | &a                | All types                                    | Returns a reference to `a`.                                         |
| Dereference           | a.*               | Reference                                    | Unwrap a reference type, this is done automatically when using `.`. |

## Precedence
```
x() x[] x.y x.? x.! x.*
E!T
x{}
!x -x ~x &x *T ?T
* / %
+ -
<< >>
& ^ | in
== != < > <= >=
and
or
|> >>=
= *= /= %= += -=
```

## Operator Overloading
Operator overloading is possible on the following operators:
- `+`: (T, T) -> T
- `-`: (T, T) -> T
- `*`: (T, T) -> T
- `/`: (T, T) -> T
- `%`: (T, T) -> T
- `<`: (T, T) -> bool
- `==`: (T, T) -> bool
- TODO: `>>=`: (T, (T) -> U) -> U
- TODO: `[]`: (T, int) -> U

Note:
- `==` is automatically generated for all types by the compiler but can be overridden.
- `!=`, `>`, `<=`, `>=` are automatically generated when `==` and `<` are defined.
- `+=`, `-=`, `*=`, `/=`, `%=` are automatically generated when the corresponding operator is defined.
- for unary negation `-`, autogen it from `-` operator and `zero` decl?
```nov
let Complex = struct {
    re: float
    im: float

    ; TODO: make the arg an enum so parsing is handled like an expr and it
    ; solve the binary/unary issue with `-`
    @[operator(+)]
    let add: `(self: Complex, other: Complex) -> Complex = {
        Complex{
            .re = self.re + other.re,
            .im = self.im + other.im,
        }
    }

    ; used by print
    ; signature must be `toString: (T) -> string` where T is the container type
    let toString: `(self: Complex) -> string = {
        "${self.re} + i${self.im}"
    }
}

let x = Complex{ .re = 5, .im = 3 }
let y = Complex{ .re = 2, .im = 7 }
x + y ; returns Complex{ .re = 7, .im = 10 }
```

## C FFI
TODO...

also add binding generator for zig?
```nov
@[extern("malloc")]
let c_malloc: `(uint) -> voidptr

@[extern("malloc")]
let c_malloc: `(uint) -> voidptr = {} ; error extern fn can't have body

@[export("nov_add")]
let add: `(a: c_int, b: c_int) -> c_int = a + b
```

## String Interpolation
`"{varname:[fill][alignment][width][.precision][type]}"`

Escape `{` and `}` with `\`.

## Concurrency
TODO: add async/await/yield

Notes from the ignorant:
- Fibers and Async/Await tend to always have function coloring but are lightweight.
- "Goroutines" do not have color but are harder to implement? and heavier because relying "mostly" on OS threads.
- Thread Pool are easy to implement but only use kernel thread and are not incorporated into the language.

### [Fibers](https://en.m.wikipedia.org/wiki/Fiber_(computer_science))
- wren [fibers](https://wren.io/concurrency.html)
- buzz [fibers](https://buzz-lang.dev/guide/fibers.html)
- https://medium.com/the-tech-collective/the-unbelievable-simplicity-of-fibers-3339097948c4
  - https://github.com/roscopeco/fiber-blog-example/tree/main
- zig async...
- https://ayazhafiz.com/articles/23/a-lambda-calculus-with-coroutines-and-heapless-closures

### "Goroutines"
- https://docs.vlang.io/concurrency.html
- go goroutines

### Thread Pool
- zig std.Thread.Pool

### Previous Syntax Proposal
```nov
; async func, can be run normally or asynchronous
; signature: `(int) -^ int -> void
let range: `(n: int) -^ int = {
    for i in 0..n {
        yield i
    }
}
```

## Attributes
- `@[deprecated]` - `@[deprecated("message...")]`
- `@[warnif(cond, message)]` idk
- `@[operator(op)]`
- `@[pure]` (mainly for extern functions) ([Pure functions](https://en.wikipedia.org/wiki/Functional_programming#Pure_functions))
- `@[extern]`
- `@[packed]`
- `@[export]`
- `@[inline]`
- `@[noinline]`
- `@[cold]`
- `@[noreturn]` or have it as a return type?
- `@[test]` decl with it are ignored unless run with `nov test` where it behave like `zig test`
- `@[comptime]` run the decl function at comptime, TODO: or `@[assert]` idk it's for comptime assertion
- `@[public]`
- `@[private]`
- ~`@[entry]` mostly only on main~ this is more of a low level stuff so we just
              default to public main method of the given file (see std.start)
- `@[memoize]` (?)

Attributes can be set only on top level / container declarations

Side note on visibility:
- public: visible everywhere
- private: visible only in current file
- no attribute: visible when importing as a file (`@import("std.nov")`) but not
  when importing as a module (`@import("std")`)

TODO add a good way to make code usable on multiple systems through attributes.

TODO: add an attribute for signal handler and add a way to pass parameters to a
signal (have a global array with a *void for each signal that points to the
value specified?)

## Comptime
work like zig except that we use `#` instead of `comptime`
```nov
; comptime expr
let x = #fibonacci(10)

; comptime block
let x = #{}

; comptime variable, weird
let mut #x = 0

; comptime param, idk between the 2
let ArrayList: (#T: type) -> type = { ... }
let ArrayList: (T: #type) -> type = { ... }
```

# Proposal for Syntax rework
TLDR:
- () for functions
- {} for grouping and block
- <> for generic smh?
- [] for containers

```nov
; function
; signature: (int, int) -> int
let add: (a: int, b: int) -> int = a + b
; proposal: allow for below syntax so there is no difference between a function and a closure
; body block is necessary if we specify type
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

; generic function (?)
let add: <T>(a: T, b: T) -> T = a + b

; grouping
let x = {1 + 2} * 3

; block
let y = {
    let tmp = 1 + 2
    tmp * 3
}

; Side note about containers, we keep `,` because it's also here for the
; initialization of a new instance so it make sense to have it everywhere
; and it makes easier to have one line initialization

; enum
let Season = enum [
    spring, ; 0
    summer = 5,
    autumn, ; 6
    winter, ; 7

    let eql: (self: Season, other: Season) -> bool = {
        self == other
    }
]
let s1 = Season.spring
let s2: Season = .spring
match s1 {
    .spring => ...
    _ => ...
}

; struct
let MyStruct = struct [
    name: string,
    x: float = 1.0,
    y: float = 1.0,

    let max = 100.0

    let init: (name: string, x: float, y: float) -> MyStruct {
        if x > max or y > max {
            ; error
        }
        return [ .name = name, .x = x, .y = y ]
    }
]
let a = MyStruct.init("", 0, 0)
let b = MyStruct[ .name = "", .x = 0, .y = 0 ]
let c: MyStruct = [ .name = "", .x = 0, .y = 0 ]

; union
let Tree = union [
    empty
    node: struct [
        value: int,
        left: *mut Tree,
        right: *mut Tree,
    ]

    let sum: (self: *mut Tree) -> int = {
        match self {
            .empty => 0
            .node => |n| n.value + n.left.sum() + n.right.sum()
        }

        ; with an if
        ; TODO: replace `==` with `is` or something else to compare union
        ; correctly to not shadow with operator overloading?
        ; also do something like that? `if self is .node |n| {}`
        if self == .empty {
            return 0
        } else {
            let n = self.node
            return n.value + n.left.sum() + n.right.sum()
        }
    }
]
let mut leaf = Tree.empty[]
let a = Tree.node[ .value = 0, .left = &leaf, .right = &leaf ]
let b: Tree = .node[ .value = 0, .left = &leaf, .right = &leaf ]

; generic container
let Result = union<T, E> [
    ok: T,
    err: E,

    let Self = @This()

    let newErr: (err: E) -> Self = [ .err = err ]

    @[public]
    @[inline]
    let orLazy: (self: Self, op: (E) -> T) -> T = {
        match self {
            .ok => |val| val
            .err => |err| op(err)
        }
    }
]

; array
; no change

; tuples
; same syntax as array but no method, it's actually an anonymous struct

; map
; put it in stdlib and do like zig tbh
let myMap = [
    [ 0, "aaa" ]
    [ 1, "bbb" ]
]
```

## Type (proposal to replace struct/enum/union)
```nov
; enum
let Season: type = {
    Spring
    Summer
    Autumn
    Winter
}

; union
let SchoolPerson: type = {
    ; anon struct (replace paren with bracket for parsing?)
    Teacher(name: string, subject: string)
    Student(string)
}

let Fish: type = {
    Starfish(name: string, favourite_color: string)
    Jellyfish(name: string, jiggly: bool)
}

; this is very subject to change because I prefer catch syntax
; and idk about that init syntax
; also it is weird to directly use a child type like that, it should be Fish.Starfish
let lucy = Starfish("Lucy", "Pink")
match lucy {
    Starfish(_, color) => @println(color)
    Jellyfish(name, _) => @println(color)
}

; or <T>?
let Option: type(T) = {
    Some(T)
    None
}

let name: Option(string) = Some("Annah")
let level: Option(int) = Some(10)

let Result(T, E) = {
    Ok(T)
    Err(E)
}

let PurchaseError: type = {
    NotEnoughMoney(required: int)
    NotLuckyEnough
}
let buyPastry: `(money: int) -> Result(int, PurchaseError) = {
    match money >= 5 {
        ; not a real function, just for example
        true => match std.rand.int(4) == 0 {
            true => Err(NotLuckyEnough)
            false => Ok(money - 5)
        }
        false => Err(NotEnoughMoney(required: 5))
    }
}
```


```nov
let x = [ .a = 3, .b = 2 ] ; this coerce to tuple aka anonymous struct because we don't know the return type
let x = [ 10, "salut" ] ; this coerce to tuple because values are of different type
let x = [ 10, 20, 30 ] ; this coerce to int list because all values are of the same type
```

# Inspirations
Zig, OCaml, Rust, C, V

<!--Random notes
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
- https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html
- Change Parser.parseExprPrecedence? https://www.scattered-thoughts.net/writing/better-operator-precedence/
- https://www.scattered-thoughts.net/writing/notes-on-compiler-irs/
-->
