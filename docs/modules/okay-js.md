# okay-js

> JavaScript as a value: a typed tree, a printer, a macro that emits
> the text as a compile-time constant, and a `js { }` block that
> builds the tree from a deliberately small subset of Scala. Not a
> compiler — that is Scala.js, and this module says so when you leave
> the subset.

Depends on: nothing. Pure Scala — cross-built for JVM, JS and Native;
the suite runs on all three.

## Guide

**JavaScript is a tree, and printing is its interpreter.** That is
the move the rest of this codebase makes: `Ui` is a tree that `Html`,
`React` and `Frame` interpret, `Schema` is a tree the codecs
interpret, `Json` is a tree. So:

```scala
import okay.js.*

val hello = Js.Call(Js.Field(Js.Name("console"), "log"), Vector(Js.Str("hi")))
Js.print(hello)                  // console.log("hi")
```

Expressions (`Js`) and statements (`Stmt`) are different types,
because JavaScript separates them and a printer that pretends
otherwise emits `var x = if (…)`.

### Two roads out of one tree

```scala
Js.print(tree)                       // runtime: built when called
inline val src = Emit.emit(tree)     // compile time: a constant in the jar
```

`Emit` is `transparent inline`, so the result carries the singleton
type of the text: an `inline val` that compiles is proof the work
happened while the compiler ran. A tree the compiler cannot read is a
compile error naming what it could not read, never a silent fall back
to runtime.

### `js { }` — the tree from plain Scala

```scala
val program = Direct.js {
  val n = 0
  while (n < 10) {
    global.console.log(n)
  }
}
```

**The subset is closed and small, and everything outside it is a
compile error that points at Scala.js.** That is the whole design: an
approximation of Scala's semantics in a browser fails by being
*wrong*, not by failing to build, so nothing is approximated. What is
in it:

| Scala | JavaScript |
| --- | --- |
| `val x = e`, `var x = e` | `var x = e;` |
| `x = e` | `x = e;` |
| `if (c) a else b` | statement, or `c ? a : b` in expression position |
| `while (c) { … }` | `while (c) { … }` |
| `+ - * / %`, `< > <= >=`, `&& \|\| !` | the same operators |
| `==` and `!=` on a primitive | `===` and `!==` |
| `d.f`, `d.f(a)` on a `Dyn` | `d.f`, `d.f(a)` |
| `d.f = v` on a `Dyn` | `d.f = v;` |
| `(a, b) => …` | `function (a, b) { … }` |
| `f(a)` on a function `val` | `f(a)` |
| a value of type `Js` | spliced in place |

`global` is a `Dyn`: a `scala.Dynamic` whose only job is to let
`global.console.log(x)` typecheck so the macro can read it.

**A dynamic call is a STATEMENT**, which is why `applyDynamic`
answers `Unit`. In a block like this a call is almost always made for
its effect, and typing it as a value made every one of them a
discarded value — a warning at each call site. A call whose value you
want is outside the subset on purpose: build it with `Js.Call` and
splice the value in, where the shape is explicit.

**An identifier the block declared is a variable; a `Js` value from
outside is a splice.** The two have to be told apart, because `val c =
null` has type `Null`, which is a subtype of everything including
`Js`.

**Scala still typechecks the block**, so a `val` you declare only for
the JavaScript's sake reads as an unused local. Real blocks use what
they declare; one that genuinely does not needs `@nowarn` or a line
that reads it.

**`==` becomes `===`, and only where that is honest.** Scala's `==`
means equality; JavaScript's `==` coerces and `===` does not. The
macro maps to `===` when the operand type is `Int`, `Long`, `Double`,
`String`, `Boolean` or `Dyn`, and refuses otherwise rather than
guessing at what equality means for some other type.

### TypeScript: `Direct.ts { }`

`Direct.ts` reads the same subset and keeps the types the Scala compiler
inferred. `Js.printTs` prints the result as TypeScript, and `Js.print`
prints the same tree as the same JavaScript as before.
`Direct.tsSource { }` is the TypeScript text as a compile-time constant.

| Scala type | TypeScript |
| --- | --- |
| `Int`, `Long`, `Double`, `Float`, `Short`, `Byte` | `number` |
| `String` | `string` |
| `Boolean` | `boolean` |
| `Unit` | `void` |
| `Dyn`, a spliced `Js` | `any` (untyped JavaScript, and it says so) |
| `(A, B) => C` | `(a0: A, a1: B) => C` |

Any other type is refused by name. A function's return is not
annotated: a `js { }` lambda's body is a statement, so the JavaScript
function returns nothing, and annotating the Scala result would be
false. A test checks the printed text with `tsc --strict`. A second test
shows that the annotations bind: a string under `number` is refused.

Two defects of `js { }` itself were found by the first typed program,
and both printed JavaScript that runs and does the wrong thing:
- `f(1)` on a function-typed `val` printed `f.apply(1)`, which is
  `Function.prototype.apply` with `this` set to 1 and no arguments;
- `global.document.title = t` printed `document.updateDynamic("title", t)`.

Both are now the call and the assignment they read as, with tests.

### The escape hatch is named

`Js.Raw` and `Stmt.Raw` carry source the tree cannot express — a
regex literal, a `try/catch`. They exist so that one awkward line does
not push the whole file back to string concatenation, and
`Js.raws(program)` counts them, so a test can hold a program to zero.

### Escaping is a security property

`Js.quote` escapes `<` so that `</script` cannot end the document the
script is written into, and U+2028/U+2029 because they are line
terminators in JavaScript and are not in JSON. A generator whose
output can be ended by its own data is a cross-site scripting hole,
and putting data inside scripts is what this module is for.

## Spec

`specs/js.md`.
