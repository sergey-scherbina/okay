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
     * spellings: m.reflect and reflect(m) */
    inline def reflect[B]: Cont[A, F[B], F[B]] =
      shift(k => m.flatMap(k))
    /** the symbolic μ: m.? — Rust's postfix question, generalized */
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
    val x = mx.?
    val y = my.?
    x + y
  }
```

**What the macro actually does — and does not.** `direct[F] { block }`
rewrites the block, at compile time, into exactly the
`Monadic.reflect`/`reify` chain of Layer 1. Every program it emits is
one you could have written by hand; multi-shot, short-circuit and the
stack discipline are *inherited*, not re-implemented. The mark `.?`
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
  val env = Reader.Ask[Int, Int]().?  // an operation
  Writer(s"env=$env").?               // an operation
  env + 1                             // plain code
}
// then the ordinary handlers:
!.run(Writer.run(Reader.run(41)(prog)))  // (Seq("env=41"), 42)
```

(A symbolic rename of the op mark to `.!` was also tried and refuted
by the compiler: an imported extension named `!` shadows `object !`
as an identifier, and every `!.run(...)` in the importing file stops
compiling. The two-character `.!?` died for redundancy, the
one-character `.!` for namespace collision; `.?` survived both.)

## Layer 3 — auto-coloring: no marks, behind two gates

The marks can disappear entirely — but only where two explicit gates
both open. This is the part of the design where the danger lives
(implicit conversions that fire where you did not mean them), so the
gates are the whole story:

```scala
import Direct.{*, given}                    // givens need naming in Scala 3
import scala.language.implicitConversions   // the language demands consent

given Effect[[X] =>> Reader[Int, X]] with {}    // gate 2: the marker
def ask: Reader[Int, Int] = Reader.Ask()

val prog: Int ! F = direct {
  val env: Int = ask        // no mark: conversion inserted, macro rewrites it
  Writer(s"env=$env")       // no mark either — see Layer 4
  env + 1
}
```

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

## Layer 4 — do-notation statements: the statement is the mark

The `tell` problem: a `Unit`-typed operation on its own line cannot
auto-color (above), and demanding `.?` on every log line is
ceremony. The answer needs no conversion at all — the macro can see
a bare statement's type directly, and there is exactly one thing a
monadic statement in a direct block can mean:

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
