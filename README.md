# Nov

Nov is a multi-paradigm ~functional~ programming language.

Nov [typing discipline](https://en.wikipedia.org/wiki/Type_system) is
[static](https://en.wikipedia.org/wiki/Static_typing),
[strong](https://en.wikipedia.org/wiki/Strong_and_weak_typing),
sound and
[manifest](https://en.wikipedia.org/wiki/Manifest_typing) with bidirectional inference.

Nov has automatic memory management via a minimal tracing GC.
<!-- https://github.com/ivmai/bdwgc -->
<!-- autofree? https://docs.vlang.io/memory-management.html -->

# Concepts

## Comments
Regular comments start with `;` and end on a newline.

Doc comments start with exactly three semicolon (i.e. `;;;` but not `;;;;`).
Doc comments are used to automatically generate documentation.

## Functions
Nov functions can be understood as lazy or postponed expressions that takes parameters.

Parameters are immutable by default.

Parenthesis are specific to functions, whenever there is a parenthesis there is a function involved.
```nov
; signature: () -> void
let doNothing: () = {}

; here x is passed as a reference and its value is mutable
; signature: (*mut int) -> void
let retNothing: (x: *mut int) = {
    x += 1
}

; functions body doesn't need to be a block
; signature: () -> int
let ret2: () -> int = 2

; signature: (int, int) -> int
let add: (a: int, b: int) -> int = a + b

; return a tuple
; see #Struct for more information about tuples and structs
; signature: (int, int) -> struct{ int, int }
let div: (a: int, b: int) -> struct{ int, int } = {
    [a / b, a % b]
}

; generic functions
; see #Comptime for more information about that #
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

## Block & Grouping
Blocks are expression, they are also used for grouping.
```nov
let x = {1} ; this is the same as `let x = 1`

let x = {1 + 1} * 3 ; x = 6

; we can do more complex things with blocks
; here x will be equal to "hahaha"
let x = {
    let max = 1 + 2
    let mut str = ""
    loop _ in 0..max {
        str += "ha"
    }
    str
}
```

## Containers
Containers are types that can have fields and declarations. Available
containers are [Enum](#Enum), [Struct](#Struct) and [Union](#Union).

[Arrays](#Arrays) are builtin containers.

Similarly to functions, brackets are specific to containers.

### Enum
Nov enums are just like C enums except that they can have methods and their fields are not global.

Overall enums are a nice way to represent tags or constant numbers.
```nov
let Season = enum {
    spring,     ; 0
    summer = 5, ; 5
    autumn,     ; 6
    winter,     ; 7

    let eql: (self: Season, other: Season) -> bool = {
        self == other
    }
}

let s1 = Season.spring ; type of s1 is inferred from the initializer
let s2: Season = .spring ; type of the initializer is inferred from the type of s2
s1.eql(s2) |> @println ; prints true

match s1 {
    .spring => ... ; do something
    _ => {} ; do nothing
}
```

### Struct
Struct is Nov's equivalent of [Product type](https://en.wikipedia.org/wiki/Product_type).
```nov
let MyStruct = struct {
    name: string,
    x: float = 1.0,
    y: float = 1.0,

    let max = 100.0

    let init: (name: string, x: float, y: float) -> MyStruct = {
        if x > max or y > max {
            @panic()
        }
        return [ .name = name, .x = x, .y = y ]
    }
}

; create a new MyStruct using its init method
let a = MyStruct.init("", 0, 0)

; create a new MyStruct with the container syntax
; here x and y use the default value specified i.e. 1.0
let b = MyStruct[ .name = "" ]

; same as above except that the type of the initializer is inferred from the type of c
let c: MyStruct = [ .name = "", .x = 0, .y = 0 ]

;;; Tuples
; tuples are simply anonymous structs

; is a tuple of type struct{ int, string }
let x = [ 0, "test" ]

; this is also a tuple
let y = [ .name = "", .x = 0, .y = 0 ]

; this is not a tuple because all values are of the same type, see #Arrays for more information
let z = [ 10, 20, 30 ]
```

### Union
Union is Nov's equivalent of [Sum type](https://en.wikipedia.org/wiki/Tagged_union)
since Nov's unions are always tagged unless annotated with `@[extern]`.
Thus we can match on an union to find its active field.

See [Result and Option unions](#result-and-option-unions) for an example of generic unions.

```nov
let NodeKind = enum { empty, node }
let Tree = union(NodeKind) {
    empty,
    node: struct {
        value: int,
        left: *mut Tree,
        right: *mut Tree,
    },

    let sum: (self: *mut Tree) -> int = match self {
        .empty => 0
        .node => |n| n.value + n.left.sum() + n.right.sum()
    }
}
let mut leaf = Tree.empty[]
let a = Tree.node[ .value = 0, .left = &leaf, .right = &leaf ]
let b: Tree = .node[ .value = 0, .left = &leaf, .right = &leaf ]

; we can also create an union without explicit enum
let Number = union {
    raw: int = 0, ; provide a default value for Number.raw
    text: string,

    ;;; Converts a Number to an int
    let toRaw: (self: Number) -> int = {
        match self {
            .raw => |raw| return raw
            .text => ... ; TODO: parseInt
        }
    }

    @[public]
    @[operator(.@"==")]
    let eql: (a: Number, b: Number) -> bool = {
        a.toRaw() == b.toRaw()
    }
}

let x = Number.raw[3]
x.text ; runtime error

; `==` can be used to check the tag of an union and to compare two unions
; overloading `==` in an union only overload the comparison between two unions,
; not between the tag
@println(x == .raw) ; prints true
@println(x == .text) ; prints false

@println(x == .raw[4]) ; prints false
@println(x == .text["3"]) ; prints true, would have been false if we hadn't overloaded the `==` operator
```

### Generics
We can generate a type with a function thus creating a generic type.
```nov
let Stack: (T: #type) -> type = struct {
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
}
```

### Result and Option unions
Sugar for Result and Option:
- !T is Result(T, string) or Result(T, any)?
- E!T is Result(T, E)
- ?T is Option(T)
- expr.! is unwrap or propagate for result
- expr.? is unwrap or propagate for option

```nov
@[public]
let Result: (T: type, E: type) -> type = union {
    ok: T,
    err: E,

    let Self = @This() ; see #Builtins

    @[public]
    let unwrapOr: (self: Self, fallback: T) -> T = match self {
        .ok => |value| value ; catch the value and return it
        .err => fallback
    }

    ; here unwrapOrElse takes a function and not a value so the fallback is
    ; only evaluated when needed
    @[public]
    let unwrapOrElse: (self: Self, fallback: (E) -> T) -> T = match self {
        .ok => |value| value
        .err => |err| fallback(err)
    }
}

let MyResult = Result(int, string)
let my_value = MyResult.ok[0]
let my_err: MyResult = .err["my error string"]

; error handling
let file = match File.open("file.txt") {
    .ok => |f| f
    .err => |err| match err.kind {
        .not_found => {
            ; do something
        }
        _ => return err ; return early with the error
    }
}
; .! unwrap and returns the err if there is any
let file = File.open("file.txt").!
```

```nov
let Option: (T: type) -> type = union {
    some: T,
    none,

    @[public]
    let unwrapOrElse: (self: @This(), fallback: () -> T) -> T = {
        match self {
            .some => |value| value
            .none => fallback()
        }
    }
}

let x = 5
let y = Option(int).some[5]
let sum = x + y.unwrapOrElse(0)

let MyOption = Option(float)
let a = MyOption.some[1.0]
let b = MyOption.none[]
let prod = a.? * b.? ; will return none to the calling function since b is none
```

## Arrays
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

; TODO:
; proposal about copy/ref of variables
; side note, it kinda sucks to hide through type if something is copied or not
; we're trying to solve that btw https://jvns.ca/blog/2024/08/06/go-structs-copied-on-assignment/
; ---
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

; imperative way of printing an array
loop arr in arr_arr {
    loop w in arr {
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
```

## Slice
TODO

## Match
```nov
match 5 {
    0 | 1 | 2 => ... ; use `|` to specify multiple cases
    {0b100 | 0b001} => ... ; wrap your expr in a {} to use `|` bitwise operator
    10..20 => ... ; use `x..y` to match over x to y excluded, [x;y[
    20..=30 => ... ; use `x..=` to match over x to y included, [x;y]
    31 => {} ; {} does nothing, it's a empty block
    _ => {} ; _ corresponds to every other possible values
}
```

## If/Else
```nov
let a = 10
let b = 20
; braces are mandatory, else is optional
if a < b {
    @println("{a} < {b}")
} else if a > b {
    @println("{a} > {b}")
} else {
    @println("{a} == {b}")
}

; all ifs are expressions which means that they all return a value
; the previous if returns `void`
; this one returns a bool
let is_even = if 69 % 2 == 0 {true} else {false}
; another example which returns an Option(int)
let x: ?int = if is_even {
    @println("even")
    .some[42]
} else {
    @println("not even")
    .none[]
}
```

## Loop
```nov
; items can be an array, a slice, a string or an iterator
; iterator semantic are not defined yet...
; type is always inferred
loop item in items {}

; n takes values [0;10[
; note than the boundary doesn't need to be literal
; loop n in x..y {} is fine as long as x and y are unsigned? integers
loop n in 0..10 {}

; n takes values [0;10]
loop n in 0..=10 {}

; it's possible to loop on multiple values at the same time as long as they
; have the same length
loop item, i in items, 0.. {}

; values are const by default, add & to take a ref to a mutable value
; a range cannot be mutable
loop &item in items {}

; use underscore to ignore a value
loop _ in 0..10 {}

; we can also loop on a condition
loop x > 10 {}

; infinite loop
loop {}

; "zig while" loop
let mut i = 0
loop i < 100 : i += 2 {}

; else branch
; the else branch is evaluated when the loop is not exited with a break
let rangeHasNumber: (begin: uint, end: uint, number: uint) -> bool = {
    var i = begin;
    return loop i < end : i += 1 {
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
Check if an element is in an array
```nov
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
let z += 3 ; compile error, expected `=`
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
| Assignment            | a = b             | All types                                    | `a` is an identifier and `b` is an expression.                           |
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
| Member Search         | a **in** b        | [Arrays](#Arrays)                            | TODO                                                                |
| Access                | a\[b]             | [Arrays](#Arrays) <br> string                | TODO: b is an Integer                                               |
| Field / Method Access | a.b               | All types                                    | TODO                                                                |
| Reference Type        | *T <br> *mut T    | All types                                    | Create a reference type from `T`. Unless `mut` is specified the wrapped value is constant |
| Reference Of          | &a                | All types                                    | Returns a reference to `a`.                                         |
| Dereference           | a.*               | Reference                                    | Unwrap a reference type, this is done automatically when using `.` or `[]`. |

## Precedence
```
x() x[] x.y x.? x.! x.*
E!T
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

Note:
- `==` is automatically generated for all types by the compiler but can be overridden.
- `!=`, `>`, `<=`, `>=` are automatically generated when `==` and `<` are defined.
- `+=`, `-=`, `*=`, `/=`, `%=` are automatically generated when the corresponding operator is defined.

```nov
let Complex = struct {
    re: float
    im: float

    @[operator(.@"+")]
    let add: (self: Complex, other: Complex) -> Complex = [
        .re = self.re + other.re,
        .im = self.im + other.im,
    ]

    ; used by print
    ; signature must be `toString: (T) -> string` where T is the container type
    let toString: (self: Complex) -> string = "{self.re} + i{self.im}"
}

let x: Complex = [ .re = 5, .im = 3 ]
let y: Complex = [ .re = 2, .im = 7 ]
x + y ; returns Complex[ .re = 7, .im = 10 ]
```

## String Interpolation
`"{varname:[fill][alignment][width][.precision]}"`

Escape `{` and `}` with `\`.

## Concurrency
TODO

## Builtins
### Functions
- `@import(path: string)`: Import a nov file. See [Visibility](#Visibility) for which declarations gets imported.
- `@TypeOf(...)`: Returns the type of a value.
- `@typeInfo(...)`: Returns type information about a value.
- `@This()`: Returns the type of the current container
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
- `@swap(a: any, b: @Typeof(a))`
- `@unreachable()`: TODO
- atomic stuff
- `@memcpy()` or `@dupe()`/`@clone()`?
- math func? prob no, put them in std.math
<!-- - `@fieldParentRef()` similar to `@fieldParentPtr()` from zig, useful for C-style OOP -->
- `@call()`: TODO

### Variables
- os type
- architecture
- support i128...
- optimization level
- safety check enabled
- current file name
- current func name
- `@line`: current line

## Attributes
- `@[deprecated]` - `@[deprecated("message...")]`
- `@[operator(op)]`: Note that op is an enum for easier parsing.
- `@[pure]`: Mainly for extern functions, a [pure function](https://en.wikipedia.org/wiki/Functional_programming#Pure_functions)
             can only call pure functions but impure functions can call a pure function without issue.
- `@[extern]`
- `@[packed]`
- `@[export]`
- `@[inline]`
- `@[noinline]`
- `@[cold]`
- `@[noreturn]`: Specify that a function can't return.
- `@[test]`: Decl with it are ignored unless run with `nov test` where it behave like `zig test`.
<!-- - `@[callconv]` -->
- `@[public]`
- `@[private]`
- `@[memoize]`: (?, related to pure...)
- `@[require(condition..., "optional error message")`: Ensure that the specified conditions are verified for the arguments given to a function or to a container initializer.
<!-- - `@[ensure(condition..., "optional error message")`: Ensure that the specified conditions are verified for the values returned by a function. -->

Attributes can be set only on top level / container declarations

## Visibility
Visibility is modified via [attributes](#Attributes).

- public: visible everywhere
- private: visible only in current file
- no modifier: visible when importing as a file (`@import("std.nov")`) but not
               when importing as a module (`@import("std")`)

## Comptime
Works almost like zig except that we use `#` instead of `comptime`
```nov
; comptime expr
let x = #fibonacci(10)

; comptime block
let x = #{}

; comptime variable
let mut x: #int = 0

; comptime parameter
let intList: (len: #uint) -> []int = ...
```

# Inspirations
[Zig](https://ziglang.org/),
[OCaml](https://ocaml.org/),
[Rust](https://www.rust-lang.org/),
C,
[V](https://vlang.io/)
