# JavaScript as a value: printed at runtime, inlined at compile time

## Overview

The operator, 2026-09-18, looking at `okay.ui.LiveJs` — 290 lines of
hand-written JavaScript living inside a Scala string literal:

> я хочу чтобы был способ генерации кода на джаваскрипт и в компайл и
> в рантайм — раз уж мы это начали делать … то вопрос в том как мы это
> делаем?

The question is the right one and the answer is NOT a compiler.

**What this module does not do.** It does not translate Scala to
JavaScript. Scala.js does that, it is in this build already
(`crossProject(JSPlatform)`), and the reason not to reimplement it is
specific rather than a matter of taste: emitting JavaScript from Scala
expressions means deciding the semantics of `Long`, of equality, of
exceptions, of closures, of `match` desugaring and of `Int` overflow.
Those decisions are the ten years of work that Scala.js IS. A macro
covering "our little subset" holds until someone writes `==` on a case
class or a `foldLeft`, and then **the failure mode is wrong output in
a browser, not a compile error.** That is the worst kind of cost.

**What it does instead** is the move this codebase already makes
everywhere else: JavaScript becomes a VALUE, and the value has
interpreters. `Ui` is a tree and `Html`, `React` and `Frame` interpret
it. `Schema` is a tree and the codecs interpret it. `Json` is a tree.
So `Js` is a tree, and printing is its interpreter.

Nothing about Scala's semantics is involved, because nothing is
translated: the author writes the JavaScript's STRUCTURE, typed, and
the printer writes the text. What that buys is what a string literal
cannot give — a program you can build up, share, test, diff and
generate from other data.

## The two roads, which are one tree

```scala
val hello = Js.Call(Js.Field(Js.Name("console"), "log"), Vector(Js.Str("hi")))

Js.print(hello)                 // runtime: a String, built now
inline val s = Js.emit(hello)   // compile time: a constant in the jar
```

- **Runtime** is an ordinary function. It is what a page that composes
  a snippet per request needs, and what a generator driven by data —
  a vocabulary, a schema, a route table — needs.
- **Compile time** is an `inline def` backed by a macro that UNLIFTS
  the tree (a `FromExpr[Js]`) and emits the printed text as a string
  constant. The jar then carries the script exactly as it carries
  `LiveJs.source` today, with no build step and no artifact — which is
  the property okay-ui deliberately bought by hand-writing that file
  (`the page IS the deployment`).

A tree the macro cannot unlift is a COMPILE ERROR naming the part it
could not read, and telling the author to use `Js.print`. It never
falls back silently, because a silent fallback would mean the constant
somebody expected in the jar is quietly computed at startup instead.

## The tree

Expressions and statements are separate types, because JavaScript
separates them and a printer that pretends otherwise emits `var x =
if (…)`.

```scala
enum Js:
  case Num(v: Double)
  case Str(s: String)
  case Bool(b: Boolean)
  case Null
  case Undefined
  /** an identifier, or a dotted path already known good */
  case Name(id: String)
  case Arr(items: Vector[Js])
  case Obj(fields: Vector[(String, Js)])
  case Field(of: Js, name: String)        // a.b
  case Index(of: Js, at: Js)              // a[b]
  case Call(fn: Js, args: Vector[Js])
  case New(fn: Js, args: Vector[Js])
  case Unary(op: String, of: Js)          // !x, -x, typeof x
  case Bin(op: String, l: Js, r: Js)      // a + b, a === b
  case Ternary(cond: Js, yes: Js, no: Js)
  case Fun(params: Vector[String], body: Vector[Stmt])
  /** the escape hatch, NAMED so it is visible in a review and
    * countable in a test */
  case Raw(source: String)

enum Stmt:
  case Var(name: String, value: Js)
  case Set(target: Js, value: Js)
  case Do(of: Js)                          // an expression statement
  case Return(of: Option[Js])
  case If(cond: Js, yes: Vector[Stmt], no: Vector[Stmt])
  case While(cond: Js, body: Vector[Stmt])
  case For(init: Option[Stmt], cond: Option[Js], step: Option[Js],
           body: Vector[Stmt])
  case Switch(on: Js, cases: Vector[(Js, Vector[Stmt])], fallback: Vector[Stmt])
  case Break
  case Block(body: Vector[Stmt])
  case Raw(source: String)
```

**`Raw` exists and is not a defeat.** A tree that cannot express a
regex literal or a `try/catch` would push its author back to string
concatenation for the whole file. `Raw` keeps the rest of the program
typed and marks the one place that is not, which is the difference
between a hole you can see and a hole you cannot.

## What the printer must get right

Three things, and each is a test rather than a hope:

- **Escaping.** A `Str` becomes a JavaScript string literal with `"`,
  `\`, the control range, `</script` (which ends a script element
  wherever it appears, including inside a string) and U+2028/U+2029
  (which are line terminators in JavaScript and not in JSON) all
  escaped. A generator whose output can be ended by its own DATA is a
  cross-site scripting hole, and this module's whole purpose is to
  emit data into scripts.
- **Precedence.** `Bin("*", Bin("+", a, b), c)` prints `(a + b) * c`.
  Parenthesise by precedence rather than always, because a script
  measured in kilobytes is served on every page load, and because a
  human reads the output when something is wrong.
- **Statement separation.** Semicolons are written, never inferred:
  automatic semicolon insertion is a language feature nobody should
  depend on twice.

## What it is worth

The concrete first consumer is the one that prompted it. `LiveJs`
already GENERATES one part of itself — the vocabulary the browser
claims is interpolated from `React.Vocabulary`, so the set cannot
drift between the three renderers that serve a browser. That worked,
and it was the one line nobody had to keep in step by hand.

The rest of that file is still three implementations of one mapping
(`React.elem`, `Html.render`, `LiveJs.build`), and the part of it that
is purely a NAME TABLE — a style token to a class, a container to a
tag — is exactly what a tree can emit from the same Scala value the
other two read. The part that is not a table (a `Table`'s column
percentages, a `Box`'s weights, an `Input`'s value semantics) stays
hand-written, and stays honest, because pretending a table can hold
arithmetic is how a table becomes a program.

So the claim here is bounded on purpose: this module makes generated
JavaScript a first-class value. It does not make hand-written
JavaScript disappear, and the lane that follows it (`livejs-verified`)
closes the remaining gap the other way — by EXECUTING the script in a
test and comparing its DOM with `Html.render`, which catches the
arithmetic a shared table never would.

## Decisions

- **A tree, not a compiler.** Scala semantics are never translated;
  JavaScript structure is written directly. That is why there is no
  `Long` question, no equality question and no desugaring question.
- **Two roads, one tree.** `print` at runtime, `emit` at compile time,
  the same value. A macro that cannot unlift says so and fails.
- **Expressions and statements are different types.** A printer that
  conflates them emits JavaScript that does not parse.
- **`Raw` is named.** An escape hatch that is visible and countable
  beats one that is a string concatenation nobody can find.
- **Escaping is a security property, not formatting.** `</script`,
  U+2028 and U+2029 are escaped because this module exists to put data
  inside scripts.

## Stages

- **Stage 1** — the tree, the printer, `print`, and the escaping,
  precedence and separation tests. Cross-built: it is pure string
  building, so JVM, JS and Native alike.
- **Stage 2** — `emit`: the `FromExpr[Js]` unlifting and the inline
  def, with a compile error for a tree it cannot read.
- **Stage 3** — the first real consumer: the name table `LiveJs`
  shares with `Html` and `React`, emitted rather than typed twice.
- **Stage 4** — `js { }`: the tree read from a CLOSED subset of
  Scala, with everything outside it a compile error that names the
  construct and points at Scala.js. DONE 2026-09-18.

## `js { }` — the subset, and why it is closed

```scala
Direct.js {
  val n = 0
  while (n < 10) { global.console.log(n) }
}
```

The temptation with a block like this is to keep adding: a `match`
here, a `foldLeft` there, a `Long` because somebody needed one. Every
one of those is a decision about what Scala MEANS in a browser, and
the failure mode is not a build error — it is **wrong output that
nobody notices**. So the subset is closed, and leaving it is a compile
error naming the construct.

| Scala | JavaScript |
| --- | --- |
| `val x = e`, `var x = e` | `var x = e;` |
| `x = e` | `x = e;` |
| `if (c) a else b` | a statement, or `c ? a : b` where a value is wanted |
| `while (c) { … }` | the same |
| `+ - * / %`, `< > <= >=`, `&& \|\| !` | the same operators |
| `==` / `!=` on a primitive | `===` / `!==` |
| `d.f`, `d.f(a)` on a `Dyn` | `d.f`, `d.f(a)` |
| `(a, b) => …` | `function (a, b) { … }` |
| a value of type `Js` | spliced where it stands |

**`==` becomes `===` and only where that is honest.** Scala's `==` is
equality; JavaScript's `==` coerces and `===` does not. So `===` is
the right mapping for a number, a string, a boolean or a `Dyn`, and
for anything else the macro REFUSES rather than deciding what equality
means for somebody's type.

**`global` is scaffolding and does not appear in the output.** It
exists so `global.console.log(x)` typechecks; the JavaScript is
`console.log(x)`.

**A `Js` value splices into a block**, which is what lets a
hand-written fragment and a generated one be one program.

Four things the building of it found, each now a test:

- the reflection API hands operator names over UNENCODED, so a
  `$amp$amp` key matches nothing and a first draft printed
  `a.&&(b.unary_!)`;
- varargs arrive wrapped in one `Typed(Repeated(…))`, so a call with
  two arguments looks like a call with one;
- `null`'s type is `Null`, which is a subtype of everything including
  `Js`, so a splice case placed above the literals swallows it;
- an `if` at the END of a block is still a statement, and its braces
  are not an expression;
- an identifier the block DECLARED is a variable and a `Js` value from
  outside is a splice, and they are indistinguishable by type because
  `null` has type `Null`, a subtype of everything;
- a dynamic call answers `Unit`, because typing a call as a value made
  every call site a discarded value.
