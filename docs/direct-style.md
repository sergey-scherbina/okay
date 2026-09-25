# Direct style: monads as plain code

How Okay lets you write monadic and effectful programs as ordinary
Scala — `val x = m.?`, or no marks at all — and why every layer of it
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
     * spellings: `m.reflect` and `reflect(m)` (an extension is a
     * method; the prefix form is its desugared call) */
    inline def reflect[B]: Cont[A, F[B], F[B]] =
      shift(k => m.flatMap(k))
    /** the symbolic μ — the same glyph as Direct's mark and as
     * `Throws.?` (specs/unwrap-glyph.md): the value, the context deals
     * with what was around it */
    inline def ?[B]: Cont[A, F[B], F[B]] = reflect[B]

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
import okay.Cont.Monadic.*

def add(mx: Option[Int], my: Option[Int]): Option[Int] =
  reify:
    for
      x <- mx.reflect   // x: Int — a plain value
      y <- my.?         // the same μ, spelled short
    yield x + y

// multi-shot comes free, because k is a pure closure:
val r: List[Int] = reify:
  for
    x <- List(1, 2, 3).?
    y <- List(10, 20).?
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

**What one `Monadic` block cannot do**: mix two *different* monads.
`Cont` has one prompt, so the answer type fixes one `F` per `reify`.
Composing effects is what the effect rows (`F + G`) are for, and one
block reflects any *single* monad, including `A ! Row` for an
arbitrary row. For several monads in one block, see the next section;
for WHY two monads need anything special at all — the missing "swap",
what monad transformers do about it (on a real cats stack) and what
they cost — see the book's chapter
[16b · Two monads at once](continuations/16b-two-monads-at-once.md).

## Layer 1½ — several monads in one block: layered reflection

Filinski's follow-up paper, *Representing Layered Monads* (POPL 1999),
gives each of several monads its own `reflect`/`reify`. Brachthäuser,
Boruch-Gruszecki and Odersky (*Representing Monads with
Capabilities*, 2020) point out that multi-prompt delimited control is
all this needs: each `reify` installs its own delimiter and hands the
body a capability, and `reflect` through that capability captures up
to the right delimiter, past any inner ones. `Delim` is multi-prompt,
so `Layered` is short (specs/layered-reflection.md):

```scala
val prog = reify[List, Option[Int], Pure]:
  reify[Option, Int, Pure]:
    for
      x <- List(1, 2, 3).reflect[Option[Int], Pure]
      y <- (if x == 2 then None else Some(x * 10)).reflect[Int, Pure]
    yield x + y
assertEquals(run(prog), List(Some(11), None, Some(33)))
```

`List(…).reflect` reaches the OUTER `reify[List]`, past the inner
`Option` delimiter, and the list continues the rest of the block once
per element. Each continuation re-installs the inner layer.
`Option.reflect` reaches the inner one.

**The order of the blocks is the order of the layers**, the way the
order of handlers is. With `Option` outside `List`, the same body
answers `Option[List[Int]]`, and one `None` empties everything:

```scala
val prog = reify[Option, List[Int], Pure]:
  reify[List, Int, Pure]:
```

That returns `None`, where the version above returns a `None` per
failing branch. These are the two answers `OptionT[List]` and
`ListT[Option]` would give, obtained here without transformers.

**Why a `Layer[M]` and not a `Monad[M]`.** The continuation a layer
captures is a program: it still contains the inner layers, and it may
reflect into outer ones. So a layer's bind has to sequence programs:

```scala
trait Layer[M[_]]:
  def pure[A](a: A): M[A]
  def bind[A, B, G[+_]](m: M[A])(k: A => M[B] ! G): M[B] ! G
```

That is a monad transformer over whatever runs outside the layer.
`Option`, `Either[E, _]` and `List` have one (a traversal in order).
A monad that cannot run a program inside its `flatMap`, such as a
`Future`, does not. The fibre road of the capabilities paper avoids
this because its continuation is an impure function, but that road is
one-shot and JVM-only.

**In a `direct` block the mark is enough.** Inside a layer, `.?` on the
monad's own value IS its reflect: the macro finds the layer in scope by
the value's type, so an `if` whose branches are `None` and `Some(…)`
reaches `Option`'s layer too (direct-layers-instances):

```scala
val x = List(1, 2, 3).?
val y = (if x == 2 then None else Some(x * 10)).?
```

A layer's reflect is a `Delim` capture, so the block's row must have
`Delim`. Outside every layer the mark is refused as before. `Lexical`
instances need nothing new in a block: their operations are programs, so
`s.get.?` works, and `s.put(v).?` is the statement form of `set`.

**Two practical notes.** `M` is read off the receiver, so write
`Option(2).reflect`, not `Some(2).reflect` (the same trap as `.some`
in cats). A capability kept past its `reify` fails with `NoPrompt`
when used. `Layered.Stacked` refuses it at compile time. There, a
layer is a stacked `dollar` whose return function is the monad's unit,
Filinski's `reify` read in λ$ as `η $ e`. Its capability is the
dollar's own `In`, and `m.reflect(layer)` needs evidence that the
layer's prompt is still on the stack.

## Layer 2 — the `direct` block: syntax, by a macro that adds nothing else

The for-comprehension above still shows. The `direct` macro removes
it:

```scala
import Direct.*

def add(mx: Option[Int], my: Option[Int]): Option[Int] =
  direct[Option] {
    val x = mx.?
    val y = my.?
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
inside a block is a different symbol from `Monadic.?` — it typechecks
as `A` (the block must typecheck *before* the macro expands; that is
how inline macros work), never executes, and throws loudly if it
somehow escapes a block.

Inside the block, plain Scala works:

```scala
// subexpression marks hoist left-to-right (evaluation order kept):
direct[Option] { eff("a", 1).? + eff("b", 2).? }  // "a" before "b", always

// if/match with effects in the scrutinee and the branches —
// only the taken branch's effects run:
direct[Option] { if c.? then branch(1).? else branch(2).? }

// && and || keep their short-circuit — the macro desugars them to
// the if they mean (they are compiler intrinsics whose method type
// lies about by-name-ness; hoisting their operands would have
// broken short-circuit silently):
direct[Option] { eff(false).? && eff(true).? }   // right side never runs
```

**Effectful iteration** (the shapes the codebase survey named as
the top real pattern) is rewritten, not refused: `for x <- xs do
eff(x).?` runs per element in order and short-circuits mid-loop;
`for x <- xs yield eff(x).?` is the traverse shape; `while cond.?
do body` re-evaluates its condition each turn; loops recurse over an
immutable materialized List, so multi-shot re-entry into a loop body
is sound. Since direct-loops v2 (2026-09-22) the whole
for-comprehension is in: guards (`for x <- xs if p(x).? …`, the
guard may itself be marked), several generators (`for x <- xs; y <-
ys(x).? yield …`, results in the comprehension's order, a
short-circuit in the inner generator ending the whole thing), and a
`yield` that answers the node's own collection — `List`/`Seq`,
`Vector`, `Set`, `Map` of pairs. So are the HOFs a marked lambda most
often lands in: `exists`/`forall`/`find` stop at the element that
decides, `filter` keeps the matches, `foldLeft(z)(f)` threads the
accumulator (a marked `z` binds first). Everything else higher-order
(`collect` with a partial function, `sortBy`, `count`, `zip`…) keeps
the refusal below until a consumer names it.

**Why scoped, precisely.** Four things are compile errors, each with
its position and its workaround in the message:

- a mark **under a lambda** — the continuation cannot cross a
  function boundary the macro does not rewrite, and rewriting
  higher-order arguments generically is the expensive half of the
  general problem (the half dotty-cps-async solves and pays for).
  Bind the value to a `val` before the lambda. ONE lambda is
  rewritten: one whose body already ENDS at the block's program type
  (direct-program-lambda), which is the `try` body's treatment and the
  nested def's, and sound for the same reason — binding the marks
  inside changes neither the lambda's type nor where it is evaluated.
  That is what lets a continuation handler read as ordinary code:
  `Delim.shift(p) { k => "deciding".tell; if ok then k(n) else pure(-1) }`
  with no inner block.
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
early version had a second mark for that. It was refuted as
redundant the day a user asked why there were two: the *type*
already says which case applies, so the macro dispatches — `F[T]`
reflects; an operation of the block's row (the macro extracts `Row`
from `F = A ! Row` and checks membership) injects, then reflects.
One `.?` everywhere:

```scala
type F = Reader % Int + Writer % String
val prog: Int ! F = direct {          // F inferred from the expected type
  val env = Reader.Ask[Int, Int]().?  // an operation
  Writer(s"env=$env").?               // an operation
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
| `.?` | postfix glyph | chains: `lookup(u).?.name` needs no parens |
| `!prog` | prefix glyph | the gesture: statements, wizard lines — `val name = !Form.ask[Name]("who?")` |

The family is the survivor set of a recorded three-strikes history
(specs/direct-macro.md Decisions): `.!` shadows `object !` (every
`!.run` in the importing file breaks — refuted twice, once per
lane); `.?` collided with okay's own Throws row-`?` (Ambiguous
extension methods, found twice independently) and was retired while
`.?` stood in for it as the one postfix that collided with nothing;
once the Throws glyphs moved into their type's companion and the row
peek became `peek`, `.?` came back (unwrap-glyph) and `.?` retired
for good (mark-glyph-only) — `.?` means the same on `A throws E`:
the value, the context deals with the rest. The prefix rides the
method name `unary_!`, which shadows nothing by construction. Prefix `!`
on an effectful program is Idris's bang-notation and Frank's `!`
arriving at the same point of the design space (see
[theory ch. 8](theory/08-direct-style.md)). One caution inherited
with it: in boolean-heavy code `!x` is negation on a `Boolean` and
a mark on an `F[Boolean]` — mechanically unambiguous (members beat
extensions), but readers parse by type; prefer `.reflect` there.

## Layer 2½ — the staged block: the handler known at the call site

A `direct` block lowers to a `Free` tree, and handlers walk it
afterwards: every operation is dispatched at run time, and the tree
is re-materialised by every continuation. When the handlers are
known where the block is written, there is a faster road — the same
block text, run as a function of its continuation with each
operation compiled to its handler's arm:

```scala
val sw = Stager.StateWriter[Int, String, Int]()   // the row's staged interpreter

def step(i: Int, acc: Int): Handled[sw.Row, sw.R, Int] =
  if i >= 100 then Handled.pure(acc)
  else Direct.staged(sw) {
    val a = State.get[Int].?
    val _ = State.set[Int](i).?
    Writer.tell("w").?
    step(i + 1, acc + a).?
  }

val ((state, log), answer) = sw.run(0)(step(0, 0))
```

Measured on a thousand operations (specs/direct-staged.md): 7.5 µs and
85 KB against 16.8 µs and 165 KB for the identical block as a Free
block run by `State.run(Writer.run(_))` — 2.24x, and to within 1% of
the same program written by hand against the stage. The reason is
one line: `sw.stage` is an `inline match` over the row's
constructors, applied by the macro to the operation as you wrote it,
so the compiler picks the arm — no `split`, no test, no tree.

What the road costs, stated: a `Stager` object per row and answer
layout (the ones that ship are next); a marked program must be one
the macro can walk at compile time — an operation, a leaf, a
combinator like `State.modify(f)` or a for-comprehension over the row
— and a program built at run time is refused with the shape in the
message; and a staged block is `Func`, fast and NOT stack-safe on a
left-nested chain — a loop of thousands of operations is fine, a loop
of millions is a Free block under `Cont`.

**Which stagers ship** (specs/direct-stagers.md). `Stager.All[E, S, W,
Err, A]` covers every effect a block is written over when it is not
`Async` — `Reader % E + State % S + Writer % W + Throws % Err` — in one
layout: the environment an argument, the state and the log threaded,
the error the answer's `Left`. A block over a SUBROW passes `Unit` for
a slot it never reads and `Nothing` for one it never writes; the arms
for those members are in the match and never chosen:

```scala
case class Cfg(k: Int, limit: Int)
val rt = Stager.All[Cfg, Unit, Nothing, String, Int]()   // Reader + Throws, nothing else

def total(xs: List[Int]): Handled[rt.Row, rt.R, Int] = Direct.staged(rt) {
  val cfg = Reader.ask[Cfg].?
  var acc = 0
  for x <- xs do
    acc += x * cfg.k
    if acc > cfg.limit then raise[String, Unit](s"over $acc").?   // ends the block: Left
  acc
}

rt.run(Cfg(2, 100), ())(total(List(1, 2, 3)))._2     // Right(12)
rt.run(Cfg(2, 5), ())(total(List(1, 2, 3)))._2       // Left("over 6")
```

`raise(e).?` inside a staged block drops the continuation and
answers `Left(e)` — `run` is the block's catch; `catching`/`local`
are handlers, i.e. programs, and stay outside (the rule above). The
four singles — `Stager.Reading[E, A]`, `Stateful[S, A]`,
`Logging[W, A]`, `Failing[Err, A]` — are the same arms with the tuple
removed, for a block over one effect: `Stager.Stateful[Int, Int]()`
runs as `st.run(s0)(block)` to `(state, answer)`. Why one class over
the full row and not one per combination: a stager composed from
per-effect arms cannot be written in plain Scala (the product would
call the arm through a trait's abstract member, which is never
inlined), and fifteen hand-written combinations is boilerplate; the
unused slots were priced instead: the State+Writer block through
`All` with two empty slots is 7.81 µs / 90 KB against `StateWriter`'s
7.64 / 85 KB (+2%, +5.6% bytes — the extra `env =>` level per arm),
and the Reader+Throws block above, at a thousand operations, is
4.40 µs / 51 KB staged against 11.25 / 113 KB as a Free block under
`Reader.run` + `runEither` — 2.56x, parity to 1% with the hand-written
program (specs/direct-stagers.md).

## Layer 3 — auto-coloring: no marks, behind two gates

The marks can disappear entirely — but only where two explicit gates
both open. This is the part of the design where the danger lives
(implicit conversions that fire where you did not mean them), so the
gates are the whole story:

```scala
import okay.Direct.{*, given}
import scala.language.implicitConversions   // the language demands consent

given Effect[[X] =>> Reader[Int, X]] with {}    // gate 2: the marker
def ask: Reader[Int, Int] = Reader.Ask()

val prog: Int ! F = direct {
  val env: Int = ask        // no mark: conversion inserted, macro rewrites it
  Writer(s"env=$env"): Unit  // no mark either — see Layer 4
  env + 1
}
```

The `given`s need naming in Scala 3 — for the ops; the block's own
PROGRAMS colour without it. This import cannot be removed by Scala
3.9's `into`, and it is worth saying why, because `throws` lost its
own import that way
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
and `.?` are NOT substitutes — they are marks ("bind this program"),
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
  /** what the cell holds, or None (which marks it running) */
  case Force[A](h: Once.Handle[A]) extends Once[Option[A]]
  /** fill the cell; answers what it holds after — the first store wins */
  case Store[A](h: Once.Handle[A], a: A) extends Once[A]

def once[A, F[+_]](p: => A ! Once + F): A ! Once + F =
def run[A, F[+_]](a: A ! Once + F): A ! F =
```

`once` is the word behind `!.once`, `run` behind `Once.run`. `!.once(p)`
is a program value: its first demand runs `p` and stores
the answer under a fresh handle; every later demand of *that value*
answers from the store. The handle carries no program, which is what
keeps `Once`'s type free of the row it lives in and lets it be
written like any other effect. The cells are the handler's STATE,
threaded through `Once.run`'s loop as `State.handle` threads `S`, so
the tree holds no cell: the same program run twice replays the same
trace.

That threading is also why a handle shared BETWEEN FIBRES runs twice:
each fibre's `Once.run` has its own cells. `SharedOnce` (okay-async)
is the other reading for that case — the `memoize`/`Deferred` of the
async libraries: one store for every fibre that runs through it, and a
demand met while the program is in flight WAITS for its answer (an
`Async` await, resumed by the store) instead of running it again:

```scala
val store = SharedOnce()
val (a, b) = Async.par(store.run(p), store.run(p)).runWith   // p ran once; a == b
```

`store.runIn` forwards the rest of a wider row. The price is what the
threading bought: a program under a shared store is not replayable,
and a knot — a program demanding its own handle — is a hang rather
than `Once.run`'s exception, because the waiter is the fibre that
would have stored. `Once.run` stays the default and what a `lazy val`
compiles to; reach for the store only where the handle crosses fibres
([cats-effect's `Deferred`](https://typelevel.org/cats-effect/docs/std/deferred)
and ZIO's `Promise` are the same cell with a different name).

In a block the word is Scala's own:

```scala
def told(s: String): Int ! (Once + Writer % String) = direct { Writer(s).reflect; s.length }
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

**What it is for.** One page handler that stamps its own duration,
with a different word on each thing it needs — and every word chosen
for CORRECTNESS, not for speed:

```scala
def page(token: String): Response ! Fetch + Once = direct:
  val      started = Fetch.time          // by value: pin the start, once
  val      user    = Fetch.user(token)   // by value: every branch needs it
  lazy val feed    = Fetch.feed(user.id) // by need:  costly, and ONE list for both reads
  def      now     = Fetch.time          // by name:  time moves, read it again

  if user.banned then Response.Banned(user)
  else Response.Page(s"${feed.size} picks for ${user.name}, top ${feed.head}", now - started)
```

`started` and `now` are the SAME operation under two words, and both
are right: one pinned, one fresh. Swap any of the four and you have a
bug, not a slowdown — `def started` would move with the end and the
duration would always be 0, `lazy val now` the same; `def feed` would
fetch twice and could report one list's size beside another's head.

The test is another handler for the same effect: your data goes in
through `Reader`, the calls come out through `Writer`, and the clock
moves because each call costs time (`State`). No mocks, no doubles,
and the harness is itself a `direct` block:

```scala
type Test = Writer % String + Reader % (Users, Feeds) + State % Long

def test[X](e: Fetch[X]): X ! Test = direct:
  e.show.tell
  (e match
    case Fetch.Time() => !State.modify[Long](_ + 10)
    case Fetch.User(t) => read[Users].get(t)
    case Fetch.Feed(i) => read[Feeds].get(i)): X
```

`State.modify` and `Reader.read` answer at their OWN rows, narrower
than this block's. A mark coerces them into it, and so does plain
colouring (direct-narrow-row, direct-narrow-colour), which is what
keeps the body free of a hand-written `.plus[...]` per operation. The
macro decides membership by SUBTYPING — `R2 <:< R`, which is what
membership means for a union — because by the time it holds a row the
row has been beta-reduced and no longer matches the `F + G` shape the
`In` givens are written against; `Row.into` is that door, with the
side condition named there.

The one mark left in the harness is on a GADT branch whose value IS
the match's answer: there the branch types at the abstract `X`, and a
conversion cannot target it. Everywhere the target is a concrete type,
colouring reaches.

What that test prints (`TestDirectOnce` asserts exactly this):

| request | answer | calls |
|---|---|---|
| a banned user | `Banned(Ada)` | `CLOCK`, `GET /user?token=b` |
| the full page | `Page("3 picks for Cleo, top scala", 10)` | `CLOCK`, `GET /user?token=o`, `GET /feed/3`, `CLOCK` |

`feed` is read twice in that one line and fetched once. The clock is
read twice and answers twice, 10ms apart. The user is fetched on both
requests, and never twice.

**No marks, no ascriptions.** The three words hold with nothing
written on them (direct-colourless-val, 2026-09-16):

```scala
type W = Writer % String
type R = Once + W

def fetch(key: String): Int ! R = direct { key.tell; key.length }
def demo(use: Boolean): Int ! R = direct:
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
  Writer(s"env=$env"): Unit  // a bare statement of a row type: RUNS
  env + 1
}

direct[Option] { None: Unit; 2 }          // None — the rest never runs
direct[List]   { List(1,2,3): Unit; 7 }   // List(7,7,7) — do-notation multi-shot
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
  val v = signal[Int]("how many?").?   // raise; Resume(41) lands HERE
  steps :+= s"after($v)"               // ... and this line runs
  v + 1
}
!.run(Condition.run[Int, Pure]((_, _) => Resume(41))(prog))   // 42; before, after(41)
```

Restart frames take a direct body through the `frame` door (two
lines, forwarding to `within` over `direct`):

```scala
val a = frame[String, Pure]("skip") {
  val v = signal[String]("bad").?      // policy says Invoke("skip", x)
  v                                    // ...so this never runs
}(v => s"skipped:$v").?                // ...and the frame answers
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
def told: Env ?=> Int ! Writer % String = direct {
  Writer(s"hello ${wire[Env].user}"): Unit
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
- **A separate op mark** — redundant: the type dispatches.
- **A second postfix symbol for the one mark (`.?` beside `.?`)** —
  kept for a week after `.?` returned, then retired: two symbols for
  one mark was a question every reader asked and none needed answered.
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

## Generators: `yield`, pulled by the reader

A Python generator is a body that runs until its next `yield`, hands
the value over, and does not run again until asked again. In this
library that is not a new thing: **a generator is a program that
tells**. `Writer.tell(w)` suspends the body at a `Bind(Inject(Say(w)),
k)`; nothing past it exists until a reader calls `k`. `Gen[W]` is the
name for that program with one more member in its row, `Stop`, so the
body can end itself from inside a loop:

```scala
final class Gen[W](val chain: Gen.Chain[W]) extends AnyVal:
  def program: Unit ! Gen.Row[W] = chain.program
// a value class over the program (no allocation): `.program` is the
// program back, `Gen.fromProgram` the name onto one, `Gen.of` a plain
// Writer program widened
```

Three ways to write one, and they compose:

```scala
// 1. with no macro at all — a plain for-comprehension over Gen is a
//    generator, lazy, nested, guarded
val evens: Gen[Int] =
  for x <- Gen.unfold(1)(i => Some((i, i + 1))) if x % 2 == 0 yield x * x
evens.take(3).toList                          // List(4, 16, 36) — the body ran to its sixth yield

// 2. as a generator block — while/if/recursion, `yield` inside `for` emits
val fib: Gen[Long] = generator[Long] {
  var (a, b) = (0L, 1L)
  while true do
    Gen.emit(a).?
    val t = a; a = b; b = t + b
}
fib.drop(10).first                            // Some(55)

def countdown(n: Int): Gen[Int] = generator[Int] {
  if n > 0 then
    Gen.emit(n).?
    countdown(n - 1).?                       // a recursive generator, flat on the stack
}

// 3. any Writer program you already have, or two generators in sequence
Gen.of(Writer.tell(1).flatMap(_ => Writer.tell(2))).toList   // List(1, 2)
(Gen(1) ++ Gen(2, 3)).toList                                   // List(1, 2, 3); flatMap is `yield from`
```

**When the body runs.** `g.iterator` is the Python semantics made
literal: `next()` runs the body to its next tell and holds the
continuation; the code between two yields runs when the *second*
value is asked for. Every reader that stops — `first`, `find`,
`exists`, `take(n).toList`, a `foreach` that throws — stops the body
where it has read enough: this is `FoldUntil` (specs/fold-until.md),
whose law is that the continuation is not called once the fold is
done. A `Gen` is a value: reading it twice runs the body twice (a
Python generator is one-shot; `toLazyList` gives you that, memoised).

**How a generation ends** — three ways, each tested:

```scala
Gen(1, 2, 3).iterator.toList                   // the body ended: exhausted
generator[Int] {                               // Gen.stop from inside a loop:
  var i = 0                                    //   nothing after it runs
  while true do { i += 1; if i > 3 then Gen.stop[Int].?; Gen.emit(i).? }
}.toList                                       // List(1, 2, 3)
infinite.take(5)                               // the reader stopped: the rest never runs
```

**Consuming one in an ordinary block.** `for x <- gen do body` reads
the generator through its `iterator`, as far as the loop drives — a
`take(3)` upstream means the body ran to its third yield and no
further — and the loop's memoised `LazyList` keeps multi-shot re-entry
sound. Inside a `generator` block, `for x <- xs yield e` as the
block's value emits each `e`; that is the one place `yield` means
emit, because the block has said what it is. Mid-block, spell the
emitting loop `for x <- xs do Gen.emit(e)` — a for-yield whose value
is dropped is something Scala itself warns about before any macro
runs. Everywhere else `yield` collects, as Scala means it (the
section below).

What is not here, on purpose: sending values INTO a generator
(Python's `send`) is a coroutine pairing, and it exists as `Take` +
`Writer` + `pipe` (Pipe.scala — Kiselyov's iteratee); an async
generator is a `Source`; a body that must release a resource on an
early stop is a lane of its own (specs/generators.md, Out of scope).

**What it costs**, measured (compare `GenBenchmark`, 10 000 Longs,
quiet alternated pairs, `-prof gc`; docs/benchmarks.md §21 has the
tables and the method): the wrapper is free — `gen.iterator`
allocates what the underlying `Writer` program read by `Writer.run`
allocates, 191 against 192 bytes per element; `toList` adds the list
(48 B/elem). A chain is not walks: the stages are data and every
stopping reader walks the source once, applying them per element, so
`map(_ * 2).filter(_ % 3 == 0).toList` reads 202 µs / 231 B/elem
against 215 / 272 for the same work hand-written over `Writer` —
under the hand road, which still walks `Writer.map`. `take` fused is
0.77 of the walk it replaced; `flatMap`, `++` and `zipWithIndex` are
fused the same way, an inner generator read where the reader stands
(0.82, 0.83 and 0.69 of the walks in bytes). `program` materialises a
chain as the walks when a road needs a program: `iterator` and a
`generator` block. Prefer `iterator` or `first`/`find`/`exists` when
the answer is not a list; those stop where the answer is.

## Loops and comprehensions, in full (direct-loops v2)

The one thing a for-comprehension is — Wadler's *Comprehending
Monads* (1992) — is a `flatMap` chain ending in a `map`, with guards as
`withFilter`; Scala's compiler desugars it exactly so before the
`direct` macro sees it. So the macro does not "support `for`": it
recognises the four combinators the desugaring produces (`foreach`,
`map`, `flatMap`, `withFilter`) and three more a marked lambda most
often lands in, and rewrites each into a loop of one shape — an
immutable `LazyList` of the elements, a recursive `def`, the body
compiled per element against the loop's own tail. Everything below is
that one loop, worn seven ways.

```scala
type W = Writer % String
def look(i: Int): Int ! W = Writer.tell(s"look $i").flatMap(_ => pure(i * 10))

// a guard — `xs.withFilter(x => p)` — runs per element, in source
// order; a MARKED guard binds before the body runs
for x <- xs if isEven(x).? do say(s"body $x").?

// two generators — `xs.flatMap(x => ys.map(y => …))` — results in
// the comprehension's order; a guard between them is honoured
val r: List[Int] ! W = direct {
  for
    x <- List(1, 2)
    y <- List(10, 20) if y > 10
  yield look(x + y).?
}                                    // List(210, 220); log: look 21, look 22

// the yield answers the node's own collection: Vector, Set, Map of pairs
val m: Option[Map[String, Int]] = direct[Option] {
  for (k, n) <- Map("a" -> 1, "b" -> 2) yield (k * 2, Some(n * 10).?)
}                                    // Some(Map("aa" -> 10, "bb" -> 20))

// the HOFs: exists/forall/find STOP at the element that decides
val (log, e) = run(direct { List(1, 2, 3, 4).exists(x => look(x).? > 15) })
// e == true, log == Seq("look 1", "look 2") — 3 and 4 never looked at

// filter keeps the matches; foldLeft threads the accumulator
direct { List(1, 2, 3).foldLeft(0)((acc, x) => acc + look(x).?) }   // 60
```

**What runs when.** A short-circuiting monad ends the whole
comprehension at the first short-circuit, wherever it sits — in the
inner generator of a two-generator `for`, in a guard, in a `foldLeft`
step: the loop is a `flatMap` chain, and a `None` has no continuation
to call. The Writer log is the honest witness: `exists` over four
elements that decides at the second writes two lines. Multi-shot is
sound for the same reason as in v1 — a `List` reflect inside a body
re-runs the REST of the loop per element over an immutable
materialised `LazyList`, never a live iterator.

**What is refused, and why.** `collect` with a partial function,
`sortBy`, `count`, `zip` and every other higher-order argument keep
the "under a lambda" refusal: each shape is a loop of its own to
write, and the rule since v1 is that a consumer names it first. A
`yield` into `LazyList`, `Iterator` or a stream is refused too — a
strict traverse would force a lazy target; the generator road
(specs/generators.md) is the lazy one. `Array` receivers work for
`for … do` (as in v1) but not for `yield`: `ArrayOps.map` takes a
`ClassTag` in a second argument list and is not the shape the macro
reads.

## A loop over a source (direct-loops v3)

Everything above iterates something with an `iterator`. A source
whose next element is a PROGRAM — a `Stream[S, G]` carrier, a writer
program's told values under another effect, the `Take` side of a
`Stage` — has none, and until v3 its consumer loop was `!.loop` over
`uncons`/`await`, written by hand. `Pull[A, G]` (core) is that source
as a value: one `step` as a program in `G`, built by `Pull.of(s)` from
any `Stream` carrier, `Pull.told(p)`/`toldIn(p)` from a writer
program, `Take.each[I]` from the input of a stage. Inside a block:

```scala
direct[[A] =>> A ! State % Int + Writer % String] {
  for x <- Pull.toldIn(producer) do            // the producer performs State between tells
    say(s"got $x after ${State.get[Int].?} steps").?
}
```

The loop is emitted as a program — one `step` bound per element,
through the same row lift a mark takes, the body compiled against
the recursive call as its tail like every loop above, guards
honoured — and it fires on the RECEIVER'S TYPE, marks in the body or
not: an unmarked source loop would otherwise be a `Unit ! G` in
statement position, which build.sbt's discarded-program lint rightly
refuses. That lint is also why `for x <- src do body` exists ONLY
inside a block: `Pull` has no `foreach` of its own — `Direct`'s
extension provides it where the block's ambient `DirectCtx` is in
scope, typed `Unit`, never called — and outside a block the loop is
`src.loop(f)`, a program by name. `yield` over a source is not a
loop (a `Pull` has no `map`); the stream combinators are for that.
Theory: ch. 7, the iteratee's consumer side.

## References

- Oleg Kiselyov, Simon Peyton Jones, Amr Sabry, *Lazy v. Yield:
  Incremental, Linear Pretty-printing*, APLAS 2012 — `yield` as a
  delimited-control effect and the argument that it is the honest
  form of laziness; `Gen` is that effect (it is `Writer.tell`), read
  by a fold that can stop.
- Roshan P. James, Amr Sabry, *Yield: Mainstream Delimited
  Continuations*, TPDC 2011 — generators are the delimited
  continuation programmers already use; the reason a `Gen` needs no
  machinery of its own here, where every program is one.
- PEP 255, *Simple Generators* (Python, 2001) — the semantics
  `TestGen` asserts: the body runs to its next `yield` when asked,
  `return` ends it, the consumer may stop.
- Philip Wadler, *Comprehending Monads*, Mathematical Structures in
  Computer Science 2(4), 1992 — a comprehension IS a `flatMap` chain
  with guards as filters; the desugaring the `direct` macro reads is
  this paper's translation, which is why "supporting `for`" is
  recognising four combinators.
- Andrzej Filinski, *Representing Monads*, POPL 1994 — reflection
  and reification; layered monads in the follow-up work.
- Andrzej Filinski, *Representing Layered Monads*, POPL 1999 —
  several monads, each with its own reflect/reify (Layer 1½).
- Jonathan Immanuel Brachthäuser, Aleksander Boruch-Gruszecki, Martin
  Odersky, *Representing Monads with Capabilities*, 2020 — layered
  reflection from multi-prompt control and capabilities.
- Ningning Xie, Jonathan Brachthäuser, Daniel Hillerström, Philipp
  Schuster, Daan Leijen, *Effect Handlers, Evidently*, ICFP 2020, and
  Xie & Leijen, *Generalized Evidence Passing*, ICFP 2021 — the
  handler travels with the program and a tail-resumptive operation is
  a direct call; Layer 2½ is this idea taken to its limit, the
  handler's arm chosen by the COMPILER per operation (specs/direct-
  staged.md measured the difference: 1.0x with the handler applied at
  run time, 1.55x with the arm selected at compile time).
- Philipp Schuster, Jonathan Brachthäuser, Klaus Ostermann, *Compiling
  Effect Handlers in Capability-Passing Style*, ICFP 2020 — effect
  handlers compiled to plain code when the capability is known
  statically; the staged block is the same bargain, stated as "a
  `Stager` object per row".
- Nicolas Wu, Tom Schrijvers, *Fusion for Free*, MPC 2015 — handlers
  are folds and folds fuse; specs/handler-fusion.md is the record of
  what that buys on THIS tree (10–30% between passes, the arm
  selection being where the rest was).
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


