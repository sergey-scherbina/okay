# Direct style: monads as plain code

How Okay lets you write monadic and effectful programs as ordinary
Scala — `val x = m.!?`, or no marks at all — and why every layer of it
is two one-liners of semantics plus a macro that only ever adds
syntax. Four features, landed in dependency order on 2026-09-01:
`Monadic` (the foundation, no macros), the `direct` block, the
auto-coloring gates, and do-notation statements. Everything here is
covered by `TestMonadic` (10 tests) and `TestDirect`/`TestDirectAuto`
(27 tests); the specs with the full decision history are
[specs/monadic-reflection.md](../specs/monadic-reflection.md),
[specs/direct-macro.md](../specs/direct-macro.md) and
[specs/direct-auto-coloring.md](../specs/direct-auto-coloring.md).

## The problem, and the belief that had to be corrected first

Monadic code in Scala is written in for-comprehensions:

```scala
def add(mx: Option[Int], my: Option[Int]): Option[Int] =
  for
    x <- mx
    y <- my
  yield x + y
```

The ceremony is mild here and heavy at scale: every intermediate
value needs a `<-` line, plain control flow (`if`/`match`/local
`val`s) must be threaded through the comprehension's shape, and the
code reads inside-out relative to what it does. "Direct style" is the
name for the alternative: the monadic value used as a plain value,
the plumbing invisible.

This repository used to hold a recorded rejection: *"direct-style
rewriting via macros — rejected as impossible in the general case
(typed trees arrive after implicit resolution)"*
(specs/context-functions.md). The concern is real — a Scala 3 macro
receives an already-typed tree, and re-typing a restructured tree is
what cost dotty-cps-async years of machinery — but the conclusion
was overstated, and the correction is now recorded in that same
spec: the general transform is *expensive*, not impossible, and two
cheaper roads exist. Okay took both.

1. **No macros at all**: delimited control makes any monad run in
   direct style *relative to Cont* — Filinski proved it in 1994, and
   Okay's core is already a delimited-control library.
2. **A scoped macro**: refuse the one corner that is actually
   expensive (marks under lambdas), and the transform is a few
   hundred lines instead of a compiler project.

A second overstatement fell with the first: "direct style forfeits
multi-shot". That is true of Loom/fiber-based direct style (runtime
continuations are one-shot, JVM-only). It is false here: in both
roads below the continuation is a pure closure, and `List`, `Logic`
and every other multi-shot effect re-runs it as many times as it
likes, on all three platforms.

## Layer 1 — monadic reflection: the semantics, with no macros

**The idea** (Filinski, *Representing Monads*, POPL 1994): with
delimited control, ANY monad runs in direct style. `reflect` delivers
the `A` of an `F[A]` as a plain value; `reify` delimits a block back
into `F`. Okay's `Cont[A, S, R]` — the parameterised continuation
monad with answer-type modification — types the construction
*precisely*, which most languages cannot:

```scala
object Monadic:
  extension [F[_] : Monad, A](m: F[A])
    /** μ: the monadic value as a direct value — one definition, both
     * spellings: m.reflect and reflect(m) */
    inline def reflect[B]: Cont[A, F[B], F[B]] =
      shift(k => m.flatMap(k))
    /** the symbolic μ: m.!? — Rust's postfix question, generalized */
    inline def ?[B]: Cont[A, F[B], F[B]] =
      shift(k => m.flatMap(k))

  /** the delimiter: a direct-style block back into its monad */
  inline def reify[F[_], A, B](p: Cont[A, F[A], F[B]])(using M: Monad[F]): F[B] =
    p / (a => M.pure(a))
```

That is the entire implementation. Read the type of a reflected
value out loud: `Cont[A, F[B], F[B]]` is "*A now, F[B] eventually*"
— the answer-type parameters carry exactly the debt the block owes
its monad. `reflect(m)` captures the whole rest of the block as the
continuation `k` and hands it to the monad's own `flatMap`; the monad
decides everything else. `None` drops `k` (short-circuit). `List`
calls `k` once per element (multi-shot). `reify` settles the debt
with `pure`.

```scala
import Monadic.*

def add(mx: Option[Int], my: Option[Int]): Option[Int] =
  reify:
    for
      x <- mx.reflect   // x: Int — a plain value
      y <- my.!?         // the same μ, spelled short
    yield x + y

// multi-shot comes free, because k is a pure closure:
val r: List[Int] = reify:
  for
    x <- List(1, 2, 3).!?
    y <- List(10, 20).!?
  yield x * y           // List(10,20,20,40,30,60) — k ran 6 times
```

**Why an object, and why these names.** The names are Filinski's and
they are right; they live in `object Monadic` because package-level
`reflect`/`reify` already name the *encoding round-trip* in
Effects.scala — a different construction that deserves the same
words. One definition serves both call shapes because an extension
method *is* a method — `reflect(m)` is its desugared call. (A
separate prefix `def` alongside the extension was refuted by the
compiler: ambiguous overload at every prefix call site.)

**The one honest limit: the stack is the reflected monad's, not
Cont's.** A strict `flatMap` (Option, Either, List) invokes `k` in
place, so each reflect costs a stack frame — a thousand binds is
comfortable, a hundred thousand is not. A trampolined monad returns
a data node instead: reflecting Okay's own program monad `A ! F`
runs 100 000 binds flat (0.14s in the test). This was predicted from
the runner before it was measured, then confirmed; the practical
rule is simply: deep chains go through `A ! F`, and Option/Either
stay in the short code where they belong.

**What one block cannot do**: mix two *different* monads. The answer
type fixes one `F` per `reify` — that is not a weakness of the
construction but the honest statement that monads do not compose;
composing effects is what the effect rows (`F + G`) are for, and
reflection does not replace them. Blocks nest, though — layering in
Filinski's sense — and one block reflects any *single* monad,
including `A ! Row` for an arbitrary row.

## Layer 2 — the `direct` block: syntax, by a macro that adds nothing else

The for-comprehension above still shows. The `direct` macro removes
it:

```scala
import Direct.*

def add(mx: Option[Int], my: Option[Int]): Option[Int] =
  direct[Option] {
    val x = mx.!?
    val y = my.!?
    x + y
  }
```

**What the macro actually does — and does not.** `direct[F] { block }`
rewrites the block, at compile time, into the monad's own plain
`flatMap` binds — exactly the chain a careful hand would write.
(The first cut emitted Layer 1's `Monadic.reflect`/`reify` instead;
the benchmark priced that runtime layer at 3.3x over the hand-written
chain and the target retired — direct-flatmap-emission in
specs/direct-macro.md. Layer 1 remains as the semantic floor and the
no-macro API: `reflect(m)` *is* `shift(k => m.flatMap(k))`, so the
two emissions mean the same program.) Every program the macro emits
is one you could have written by hand; multi-shot, short-circuit and
the stack discipline are *inherited*, not re-implemented. The mark `.?`
inside a block is a different symbol from `Monadic.!?` — it typechecks
as `A` (the block must typecheck *before* the macro expands; that is
how inline macros work), never executes, and throws loudly if it
somehow escapes a block.

Inside the block, plain Scala works:

```scala
// subexpression marks hoist left-to-right (evaluation order kept):
direct[Option] { eff("a", 1).!? + eff("b", 2).!? }  // "a" before "b", always

// if/match with effects in the scrutinee and the branches —
// only the taken branch's effects run:
direct[Option] { if c.!? then branch(1).!? else branch(2).!? }

// && and || keep their short-circuit — the macro desugars them to
// the if they mean (they are compiler intrinsics whose method type
// lies about by-name-ness; hoisting their operands would have
// broken short-circuit silently):
direct[Option] { eff(false).!? && eff(true).!? }   // right side never runs
```

**Effectful iteration** (the shapes the codebase survey named as
the top real pattern) is rewritten, not refused: `for x <- xs do
eff(x).!?` runs per element in order and short-circuits mid-loop;
`for x <- xs yield eff(x).!?` is the traverse shape; `while cond.!?
do body` re-evaluates its condition each turn; loops recurse over an
immutable materialized List, so multi-shot re-entry into a loop body
is sound. Other higher-order arguments keep the refusal below.

**Why scoped, precisely.** Four things are compile errors, each with
its position and its workaround in the message:

- a mark **under a lambda** — the continuation cannot cross a
  function boundary the macro does not rewrite, and rewriting
  higher-order arguments generically is the expensive half of the
  general problem (the half dotty-cps-async solves and pays for).
  Bind the value to a `val` before the lambda.
- a mark **under a by-name argument** — hoisting it would change
  when (whether) it evaluates.
- a mark **in a `lazy val`** of a block whose row does not name
  `Once` — the same "when" question, answered: a lazy val with a
  mark is call-by-need, which is an effect here (the section
  "Call-by-need: `lazy val` is the `Once` effect" below). With `Once`
  in the row it is not a refusal but the by-need word.
- **`try` around marks** — a v2 road (reification into the Throws
  error channel), named, not promised. (`while` and the
  foreach/map loops below graduated out of this list.)
- a mark on a value that is **neither the block's `F[T]` nor an
  operation of its row** — see the next section.

A clear refusal beats a wrong capture: that sentence is the entire
design philosophy of the macro, and it is why it stays ~300 lines.

**One mark, not two.** An operation of an effect row —
`Writer("a")`, a raw `Reader` ask — is not an `F[T]`; it needs
lifting into the program (`Free.Inject`) before it can reflect. An
early version had a second mark (`.!?`) for that. It was refuted as
redundant the day a user asked why there were two: the *type*
already says which case applies, so the macro dispatches — `F[T]`
reflects; an operation of the block's row (the macro extracts `Row`
from `F = A ! Row` and checks membership) injects, then reflects.
One `.?` everywhere:

```scala
type F = Reader % Int + Writer % String
val prog: Int ! F = direct {          // F inferred from the expected type
  val env = Reader.Ask[Int, Int]().!?  // an operation
  Writer(s"env=$env").!?               // an operation
  env + 1                             // plain code
}
// then the ordinary handlers:
!.run(Writer.run(Reader.run(41)(prog)))  // (Seq("env=41"), 42)
```

**One mark, three spellings** — all the same dispatch-by-type, so
the choice is pure style, and each has a niche:

| spelling | shape | use it for |
|---|---|---|
| `.reflect` | name | any scope, any doubt — it never collides |
| `.!?` | postfix symbol | chains: `lookup(u).!?.name` needs no parens |
| `!prog` | prefix glyph | the gesture: statements, wizard lines — `val name = !Form.ask[Name]("who?")` |

The family is the survivor set of a recorded three-strikes history
(specs/direct-macro.md Decisions): `.!` shadows `object !` (every
`!.run` in the importing file breaks — refuted twice, once per
lane); `.?` collided with okay's own Throws row-`?` (Ambiguous
extension methods, found twice independently) and is retired; `.!?`
— once retired as redundant beside `.?` — returned as the one
postfix that collides with nothing; and the prefix rides the method
name `unary_!`, which shadows nothing by construction. Prefix `!`
on an effectful program is Idris's bang-notation and Frank's `!`
arriving at the same point of the design space (see
[theory ch. 8](theory/08-direct-style.md)). One caution inherited
with it: in boolean-heavy code `!x` is negation on a `Boolean` and
a mark on an `F[Boolean]` — mechanically unambiguous (members beat
extensions), but readers parse by type; prefer `.reflect` there.

## Layer 3 — auto-coloring: no marks, behind two gates

The marks can disappear entirely — but only where two explicit gates
both open. This is the part of the design where the danger lives
(implicit conversions that fire where you did not mean them), so the
gates are the whole story:

```scala
import Direct.{*, given}                    // givens need naming in Scala 3 — for ops;
                                            // the block's own PROGRAMS colour without it
import scala.language.implicitConversions   // the language demands consent

given Effect[[X] =>> Reader[Int, X]] with {}    // gate 2: the marker
def ask: Reader[Int, Int] = Reader.Ask()

val prog: Int ! F = direct {
  val env: Int = ask        // no mark: conversion inserted, macro rewrites it
  Writer(s"env=$env")       // no mark either — see Layer 4
  env + 1
}
```

This import cannot be removed by Scala 3.9's `into`, and it is worth
saying why, because `throws` lost its own import that way
(throws-into): `into` marks the conversion's TARGET type, and
auto-coloring's conversions are `Conversion[F[A], A]` — the target is
the bare type variable `A`, and there is no declaration to write
`into` on. Nor by a build-wide `-language:implicitConversions`: that
flag was in `build.sbt` for one day (2026-09-15) and came out again,
because `TestThrows` proves `throws`'s `into` by the ABSENCE of this
import, and a global flag makes the absence prove nothing. The consent
stays per file here, which for the feature where "the danger lives" is
the right answer anyway.

What a file does NOT need any more is `Direct.given` for the block's
own programs: `Free.directColor` lives in `Free`'s companion — the
implicit scope of a `Conversion[Free[R, A], A]`'s source type — so an
`A ! Row` value colours inside any `direct` block with
`import okay.Direct.*` alone (the ops' `opColor` still comes from
`Direct.given`, opt-in per signature as before).

**Without implicit conversions at all: the prefix mark.** If a file
would rather not enable the feature, the marks are the road and
the shortest of them is one glyph: `!prog`. It is an ordinary method
(`unary_!`), no `Conversion` is involved, no language import is
needed, and it composes with everything below — including the
recursion rule in the next section, where `!fib(n - 1) + !fib(n - 2)`
is the annotation-light spelling of the annotation-free one.

**Gate 1 — the capability.** The block is a context function
`DirectCtx[F] ?=> A`, and both conversions require
`using DirectCtx[F]`. Outside a `direct` block the capability does
not exist, the conversion cannot resolve, and `F[A]`-as-`A` stays
the compile error it always was. This is dotty-cps-async's
CpsMonadContext pattern, and it is *stronger* than the marks'
protection (which is a runtime throw): auto-coloring outside a block
fails at compile time. Plain unmarked blocks adapt to the
context-function signature automatically — no v1 call site changed.

**Gate 2 — the marker.** Operation types color only where a
`Direct.Effect[G]` instance exists. Registering a signature for
auto-coloring is a one-line, per-project, explicit decision;
arbitrary `G[A]`s never silently color. The core ships no instances
(the additive doctrine: capabilities are extra doors, never
defaults).

The conversions themselves are phantoms — they never run; the macro
finds their calls in the typed tree (they root at known symbols) and
rewrites them through the same by-type dispatch as `.?`.

**What colors, and what does not — typer physics, documented rather
than fought:**

- *Ascribed, argument and selection positions color*:
  `val x: Int = m`, `f(m)`, `m + 1`. These are where the typer
  actually searches for conversions.
- *`val x = m` does not color* — inference sees `F[A]` and is happy;
  no mismatch, no conversion. Ascribe to color. This is also a
  feature: it is how you *hold* a program as a value on purpose.
- *Unit-typed operations cannot color anywhere*: statement position
  has no expected type, and a `Unit` ascription triggers value
  discard, which preempts conversion search. Found by a failing
  test, kept as one — and answered properly by Layer 4.
- *Coloring resolves at the DECLARED type*: a smart constructor
  typed at the trait (`def ask: Reader[Int, Int]`) colors; a raw
  case constructor's precise type (`Reader.Ask[Int, Int]`) defeats
  the conversion's `G` inference and does not.

Explicit `.?` marks keep working in the same block and remain the
recommended default; auto-coloring is the opt-in ergonomic layer,
and its cost is honest: the language import, and error messages
inside a block that can point one conversion away from the real
mistake.

## Recursion in a block: deep, and with no annotation

A block that calls its own def at the program type has a hazard the
marks do not remove on their own: the self-call is a *program*, and
building it eagerly is the native recursion the block exists to
avoid. So inside a `direct` block a call to the ENCLOSING def, at the
block's own program type, is deferred wherever it is marked or
coloured — `fib(n - 1)` becomes `Free.delay(() => fib(n - 1))` under
the same mark — and the recursion trampolines through the tree's
`Delay` node ([theory ch. 11](theory/11-one-tree.md)) instead of the
JVM stack:

```scala
import okay.Direct.*
import scala.language.implicitConversions

def fib(n: Int): Long ! Pure = direct:
  if n < 2 then n.toLong else fib(n - 1) + fib(n - 2)     // coloured, deferred

def sum(n: Int): Long ! Pure = direct:                      // 1 + sum(n - 1): not tail
  if n == 0 then 0L else 1L + sum(n - 1)

def count(xs: List[Int], acc: Long): Long ! Pure = direct:
  xs match
    case Nil => acc
    case h :: t => count(t, acc + h)

!.run(sum(1_000_000))   // 1000000, on the default stack
```

**Mutual recursion needs nothing either.** By default a call at the
block's program type is deferred wherever it stands, so two functions
calling each other are safe in any position:

```scala
def isEven(n: Int): Boolean ! Pure = direct:
  if n == 0 then true else isOdd(n - 1)      // deferred: tail position
def isOdd(n: Int): Boolean ! Pure = direct:
  if n == 0 then false else isEven(n - 1)

!.run(isEven(1_000_001))   // false, on the default stack
```

The macro expanding `isEven` cannot know that `isOdd` calls back — a
cycle spans files, and the other def may not be typed yet — so the
enclosing-def rule cannot see it. The tail position can, and is exactly
where the node costs one allocation and saves a frame. Before this rule
(direct-tail-defer, 2026-09-16) the same code COMPILED, answered at
small `n` and overflowed the stack at depth, which is the worst failure
mode a library can have.

```scala
def sumEven(n: Int): Long ! Pure = direct:
  if n == 0 then 0L else 1L + sumOdd(n - 1)     // NOT tail — deferred anyway
def sumOdd(n: Int): Long ! Pure = direct:
  if n == 0 then 0L else 1L + sumEven(n - 1)
```

**The default is safety, and it is not free — so it has a switch.**
Deferring every call means one `Delay` and its thunk, 64 bytes, per
call a block marks. Where a block is hot and provably not recursive,
one import buys that back:

```scala
import okay.Direct.eagerCalls.given    // this scope builds calls where they stand
```

measured on `compare/DirectBenchmark`, one run, 10 000 marked calls per
invocation, `okayFlatMap` as the control:

| lane | time | allocation |
|---|---|---|
| `okayDirect` (default) | 179.3 µs | 2 238 113 B |
| `okayDirectEager` (opted out) | 113.6 µs | 1 598 113 B |

With the import in scope two rules stay on, both measured free: a call
to the ENCLOSING def is still deferred anywhere in the block, and a
call to another def is still deferred in TAIL position. What you take
on is the rest: a mutual call outside tail position is then built where
it stands, and needs the word `!.tailcall(other(n))`. `!`, `.reflect`
and `.!?` are NOT substitutes — they are marks ("bind this program"),
not deferrals ("do not build it yet"), so a marked call is still built
when the block is. A call already wrapped in `!.tailcall` is left
alone, so the explicit spelling never pays for two nodes, and a call
that carries definitions of its own — a lambda, a nested `direct`
block — is left where it stands whatever the mode, because moving it
under a thunk would move its symbols with it.

This is what Kozak's `deepRecursive` macro does for Scala 3 over
`TailRec`, at the one place it matters (the deferral) — her `TailRec`
is this library's `Free` node for node — and with the lowering the
rest of the block already gets: two self-calls in one expression,
branches, `match`, a real row whose tells interleave with the
recursion in order. Without the language import the same thing is
`!fib(n - 1) + !fib(n - 2)`. Mutual recursion needs no word either
(direct-defer-default: every call at the block's program type is
deferred, and `import Direct.eagerCalls.given` is the opt-out that
hands `!.tailcall(other(n))` back to you). A self-call under a
lambda is a value and is left alone (v1 does not look under lambdas).
`TestDirectDeep` holds every one of these shapes.

## Call-by-need: `lazy val` is the `Once` effect

`Delay` is by-name: the loop forces its thunk every time it reaches
the node, and a node shared between two places runs twice — Scala's
by-name parameter, not its `lazy val`. Haskell's laziness is by-NEED:
by-name plus a cell that remembers the answer. For a pure thunk the
cell is an optimisation, unobservable. For a program it is a
semantics — "run these effects at most once" can be seen in the log —
and the library's rule for a semantics is that it is an effect in
the row, not a mutable field in the tree. So it is one (direct-once,
2026-09-16):

```scala
enum Once[+A] derives Effect:
  case Force[A](h: Once.Handle[A]) extends Once[Option[A]]   // what the cell holds
  case Store[A](h: Once.Handle[A], a: A) extends Once[A]     // fill it; answers what it holds after

def once[A, F[+_]](p: => A ! (Once + F)): A ! (Once + F)     // !.once
def run[A, F[+_]](a: A ! (Once + F)): A ! F                  // Once.run
```

`!.once(p)` is a program value: its first demand runs `p` and stores
the answer under a fresh handle; every later demand of *that value*
answers from the store. The handle carries no program, which is what
keeps `Once`'s type free of the row it lives in and lets it be
written like any other effect. The cells are the handler's STATE,
threaded through `Once.run`'s loop as `State.handle` threads `S`, so
the tree holds no cell: the same program run twice replays the same
trace.

In a block the word is Scala's own:

```scala
val prog: Int ! (Once + Writer % String) = direct:
  lazy val x = !told("abc")      // runs at the FIRST use, in that position, once
  val y = !told("de")            // runs here
  x + x + y + !told("f")         // log: de, abc, f
```

Three words, three semantics, all visible in the source: `val` runs
now, `lazy val` runs at first demand, a bare mark runs at every use.
The macro emits `val x$once = Once.at[T, Row](handle)(force)(store)(rhs')`
and turns every use of `x` into a mark on it, so a use is a bind in
the position of the use. A `lazy val` never demanded never runs; one
demanded in one `if` branch runs only there; one declared in a loop
body is a fresh cell per iteration, exactly as a `lazy val` would be.
A use under a lambda is the usual refusal; a use inside a for-loop
the macro owns works, and runs the cell at the first element. The
rule keys on the compiled right-hand side, not on the mark: `lazy val
x = { Writer("x"): Unit; 3 }`, whose statement runs by do-notation,
is by-need too, and a pure right-hand side stays a plain Scala `lazy
val`.

**What it is for.** A handler whose branches need different data,
written top to bottom as if everything were already loaded:

```scala
def page(path: String, token: String): String ! (Once + Fetch) = direct:
  lazy val user = effect(GetUser(token))
  lazy val plan = effect(GetPlan(user.planId))
  lazy val feed = effect(GetFeed(user.id))
  if path == "/health" then "200 healthy"
  else if user.banned then "403 banned"
  else if plan.expired then "302 /renew"
  else s"200 ${feed.size} picks for ${user.name}, top ${feed.head}"
```

The calls each request actually makes, recorded by the handler
(`TestDirectOnce`, which runs exactly this):

| request | `val` | `def` | `lazy val` |
|---|---|---|---|
| `/health` | 3 | 0 | 0 |
| a banned user | 3 | 1 | 1 |
| an expired plan | 3 | 3 | 2 |
| the full page | 3 | 8 | 3 |

One word changed, nothing else. `val` is by value: all three lookups
on every request, `/health` included. `def` is by name: nothing until
a branch asks, then a call per mention — and since `plan` and `feed`
both read `user`, the full page costs eight. `lazy val` is by need:
what the branch reaches, once. `user` is read in three branches and
`feed` twice in one line; each is fetched once, and the chain stays
lazy through `plan`'s dependence on `user.planId` without a single
`flatMap` or `Option` in the source.

A nested `def` at the block's program type whose body has marks —
`def plan = effect(GetPlan(user.planId))` — used to be refused ("a
mark inside a nested definition"); it compiles now
(direct-nested-def). Its body is its own program, so binding the marks
inside changes nothing about what the def means. A def with
PARAMETERS still keeps the refusal: v1 does not rewrite a signature.

**No marks, no ascriptions.** The three words hold with nothing
written on them (direct-colourless-val, 2026-09-16):

```scala
def demo(use: Boolean): Int ! (Once + Fetch) = direct:
  val      x = fetch("val")        // by value
  lazy val y = fetch("lazy val")   // by need
  def      z = fetch("def")        // by name
  if use then x + x + y + y + z + z else 0

// nothing used:      val
// each used twice:   val, lazy val, def, def
```

This needed a rule, and the reason is worth knowing. Inference gives
`val x = fetch("val")` the PROGRAM type, so the colouring conversion
does not fire at the declaration — it fires at every USE, where an
`Int` is finally demanded. Before the rule, `val` and `lazy val` both
silently meant `def`: measured as `val, val, lazy val, lazy val, def,
def`. Now the declaration decides, as the words do everywhere else in
Scala. A val whose uses are coloured is a binding; a lazy val is the
`Once` cell; a val held as a PROGRAM — marked at its uses, passed to
`!.once`, stored — is a value and is untouched. A val read both ways
in one block is a compile error naming both readings.

**What "once" counts.** Once per handle, and a handle is made per
`!.once(p)` evaluated — as a `lazy val` is per declaration, not per
right-hand side. `!.once(p) + !.once(p)` runs `p` twice; a bare `!p`
beside a `!.once(p)` runs every time and knows nothing of the cell;
`!.once` inside a `def` makes a new handle per call. Share the VALUE
to share the cell. The type does not say which values are once'd,
any more than `State` in a row says what the state is.

**Multi-shot is handler order, not a flag.** The cells are state, so
under a search they behave exactly as `State` does:

| order | cells | reading |
|---|---|---|
| `runChoice(Once.run(p))` | backtrack with the search | each branch its own once; nothing leaks — the default |
| `Once.run(runChoice(p))` | one store for the whole search | the second branch sees what the first stored — deliberate, and the types show it |

The macro could not check a flag for this — it does not know the
handlers — and does not try. A handle demanded while its own program
is still running (a knot, or an interleaved search with `Once.run`
OUTSIDE it) is a loud `IllegalStateException`, not a second run and
not a hang. The Prolog cut, once `Logic.once`, is `Logic.cut`
(logic-cut), so `once` means one thing here. `TestDirectOnce` holds every shape above,
including both handler orders.

## Layer 4 — do-notation statements: the statement is the mark

The `tell` problem: a `Unit`-typed operation on its own line cannot
auto-color (above), and demanding `.?` on every log line is
ceremony. The answer needs no conversion at all — the macro can see
a bare statement's type directly, and there is exactly one thing a
monadic statement in a direct block can mean:

**`w.tell`** (direct-tell, 2026-09-16) is the statement form with the
warning designed out: inside a block it is the mark on the Writer
operation `Writer(w)`, typed `Unit`, so `"start".tell` on its own line
runs and `-Wall` has nothing to flag (a bare `Writer("start")` runs
too, by the do-notation rule below, but the typer sees an unused
non-`Unit` value first — E176 — which the `: Unit` ascriptions in the
tests answer). Outside a block the same name is the program
`Writer.tell(w)`, `Unit ! Writer % W`: one `transparent inline`,
decided per call site by whether the block's `DirectCtx` capability
is in scope, the gate the colouring conversions stand behind.

```scala
val prog: Int ! F = direct {
  val env: Int = ask
  Writer(s"env=$env")     // a bare statement of a row type: RUNS
  env + 1
}

direct[Option] { None; 2 }          // None — the rest never runs
direct[List]   { List(1,2,3); 7 }   // List(7,7,7) — do-notation multi-shot
```

This is Haskell's do-notation reading (`_ <- op`, or `op >> rest`):
a bare statement whose type is the block's `F[T]` or an operation of
its row is bound as an implicit `.?` and its value dropped. Building
a program and *not* running it, as a statement, was dead code in
every reading — so running it is not a surprise, it is the meaning.

The boundaries that keep it honest:

- **`val` holds, statements run.** `val held = make` keeps the
  program as a value un-run — binding is explicit consent to hold;
  only bare statement position carries the do reading. Constructing
  sub-programs inside a block stays a normal thing to do.
- **Foreign marked types still refuse.** A statement of an
  `Effect[G]`-registered type that is neither this block's monad nor
  in its row can be neither run nor meaningfully dropped — compile
  error. Unmarked foreign types stay under the compiler's own
  unused-value warning, as everywhere else.
- **Lambda bodies are untouched** — they are not this block's code.

One wrinkle, paid for once and recorded: `None` on its own line has
type `None.type`, which carries no type arguments to guess the
element type from — the macro also consults the base type at the
block's monad (`Option[Nothing]`), and every guess is verified with
`<:<` before it is believed.

## Resumable exceptions read as they were meant to

Okay's condition system (`Condition.scala`,
[specs/condition.md](../specs/condition.md)) is the road between
throwing and tolerating: `signal` raises WITHOUT unwinding, a policy
decides while the signal point is still live, and named restarts are
frames the policy can unwind to. In direct style a condition reads
exactly as Common Lisp meant it — **a call that may return**:

```scala
val prog: Int ! Op = direct {
  steps :+= "before"
  val v = signal[Int]("how many?").!?   // raise; Resume(41) lands HERE
  steps :+= s"after($v)"               // ... and this line runs
  v + 1
}
Condition.run((_, _) => Resume(41))(prog)   // 42; before, after(41)
```

Restart frames take a direct body through the `frame` door (two
lines, forwarding to `within` over `direct`):

```scala
val a = frame[String, Pure]("skip") {
  val v = signal[String]("bad").!?      // policy says Invoke("skip", x)
  v                                    // ...so this never runs
}(v => s"skipped:$v").!?                // ...and the frame answers
```

And the operator's story — repair a malformed element mid-stream and
continue — is a for-do loop with a signal in the body: the policy
resumes each signal point, the loop keeps going (`TestConditionDirect`,
"repair per element"). The two recorded roads onward — lexical
restarts as capabilities (a nonexistent restart uncompilable in
scope) and typed condition/answer pairs — are in BACKLOG.md.

## Choosing a layer

| you are writing | use |
|---|---|
| library code, generic over `Monad[F]` | `Monadic.reflect`/`reify` — no macro in the way, works in for-comprehensions |
| application blocks, explicit is fine | `direct { ... .? ... }` — one mark, all monads and operations |
| effect-heavy blocks, ceremony hurts | auto-coloring + do-statements: ascribe values, write ops as statements |
| deep bind chains (10⁴+) | any layer — but over `A ! F`, not a strict monad (the stack rule) |
| two different monads in one computation | not a direct-style problem: effect rows (`F + G`), then one block over the row |

## Composing with capabilities

The block composes with the [capability vocabulary](capabilities.md)
— the door outside, the block inside (E20 in
specs/context-functions.md, executable as `TestDirectDoors`):

```scala
def told: Env ?=> Int ! (Writer % String) = direct {
  Writer(s"hello ${wire[Env].user}")
  wire[Env].uid
}
provide(Env("ada", 7)) { !.run(Writer.run(told)) }
```

A `direct` block is itself a context function (`DirectCtx[F] ?=> A`
— Layer 3's own gate), so it nests under any environment layer by
nearest-wins, and `wire` resolves inside it; the DI guarantee — a
missing capability does not compile — survives the block.

## The graveyard, kept on purpose

Every alternative below was implemented or attempted, and the
compiler or a test refuted it. They are recorded here and in the
specs' Decisions so the next person does not pay twice.

- **A general (unscoped) macro** — exists (dotty-cps-async), costs
  years of re-typing machinery for the lambda-coloring corner;
  refusing that corner costs one error message.
- **A separate op mark (`.!?`)** — redundant: the type dispatches.
- **`.!` as the mark** — an imported extension named `!` shadows
  `object !`; `!.run` breaks file-wide.
- **A prefix `def reflect` beside the extension** — ambiguous
  overload at every prefix call site; the extension alone serves
  both spellings.
- **`isInstanceOf[ByNameType]` in the macro** — quotes-reflect types
  are abstract and erase to `TypeRepr`: the test is always true.
  Pattern-match through the API's `TypeTest`s, always.
- **Hoisting `&&`/`||` operands** — their method types are by-value
  but the short-circuit is compiler magic; the only correct rewrite
  is the `if` they mean.
- **`Lambda` after `Block` in the dispatch** — a lambda IS
  `Block(DefDef :: Nil, Closure)`; order the cases or degrade every
  lambda error message.
- **Unit-op auto-coloring via ascription** — value discard preempts
  conversion search; statements-run (Layer 4) is the answer, not a
  cleverer conversion.
- **Loom as the direct-style engine** — one-shot continuations,
  JVM-only; would genuinely forfeit multi-shot (Logic, sim,
  Stepper). The closure-based roads forfeit nothing.

## References

- Andrzej Filinski, *Representing Monads*, POPL 1994 — reflection
  and reification; layered monads in the follow-up work.
- Robert Atkey, *Parameterised notions of computation* — the
  answer-type-modified `Cont[A, S, R]` that types `reflect`
  precisely (see [theory](theory/index.md)).
- dotty-cps-async — the existence proof for the general Scala 3
  transform, and the CpsMonadContext capability pattern Layer 3
  borrows.
- Kobori, Kameyama, Kiselyov, *Answer-type modification without
  tears* — direct style with ATM inside one block; the road not
  (yet) taken, recorded in specs/monadic-reflection.md's Out of
  scope.


