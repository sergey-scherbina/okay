# generators — Python-style yield, pulled by the reader

## Overview

A Python generator is a body that runs until its next `yield`, hands
the value to whoever asked, and does not run again until asked again;
it ends when the body returns, and the reader may stop asking at any
point. Every piece of that already exists here and is measured: a
program that tells — `Unit ! Writer % W` — suspends at each
`Bind(Inject(Say(w)), k)` until a consumer calls `k` (nothing past the
tell exists until then; the continuation is a function), okay-stream's
`Source[W]` IS `Unit ! (Writer % W + Async)`, and fold-until
(2026-09-22, the sibling lane) gave the stopping consumer,
`Writer.foldUntil`, whose law is that `k` is not called once the fold
is done. What was missing was the NAME and the SYNTAX. The operator's
ask (2026-09-22): "как в питоне через yield останавливаться и ждать
пока нас дальше прочитают", "не только в директ блоках, но и без них",
and "предусмотреть выход (окончание) генерации". This spec adds a type
with the words a for-comprehension uses, a direct-block form in which
`for … yield` emits, the three ways a generation ends — and no new
carrier: a generator is a program, and stays one.

## Interface

```scala
// core, Gen.scala
enum Stop[+A] derives Effect { case Now extends Stop[Nothing] }   // the early end
/** a generator IS a program that tells — a VALUE CLASS over it, so the
 *  operators are members (see Decisions) and `program` is the same value back */
final class Gen[W](val program: Unit ! Gen.Row[W]) extends AnyVal:
    // element-wise: the for-comprehension words, no macro
    def map[V](f: W => V): Gen[V]; def flatMap[V](f: W => Gen[V]): Gen[V]   // flatMap = yield from
    def filter(p: W => Boolean): Gen[W]; def withFilter(p: W => Boolean): Gen[W]
    def take(n: Int): Gen[W]; def takeWhile(p: W => Boolean): Gen[W]; def drop(n: Int): Gen[W]
    def ++(h: Gen[W]): Gen[W]; def zipWithIndex: Gen[(W, Int)]
    // readers — each stops the body where it has read enough (FoldUntil)
    def foldUntil[S, R](using FoldUntil[W, S, R]): R
    def toList: List[W]; def toVector: Vector[W]
    def first: Option[W]; def find(p: W => Boolean): Option[W]
    def exists(p: W => Boolean): Boolean; def forall(p: W => Boolean): Boolean
    def foreach(f: W => Unit): Unit
    def iterator: Iterator[W]        // THE PYTHON ONE: next() runs the body to its next tell, holds k
    def toLazyList: LazyList[W]      // memoising

object Gen:
  type Row[W] = Writer % W + Stop
  def fromProgram[W](p: Unit ! Row[W]): Gen[W]   // the name on a program that tells
  def emit[W](w: W): Gen[W]                      // yield one value; sequence with ++
  def stop[W]: Gen[W]                            // end here, whatever follows
  def empty[W]: Gen[W]; def apply[W](ws: W*): Gen[W]
  def from[W](it: IterableOnce[W]): Gen[W]       // lazy (a Delay); an Iterator is read once, memoised
  def unfold[S, W](s: S)(f: S => Option[(W, S)]): Gen[W]
  def of[W](p: Unit ! Writer % W): Gen[W]         // a plain Writer program, row widened by Stop

// okay-direct, Direct.scala
inline def generator[W](inline block: DirectCtx[[A] =>> A ! Gen.Row[W]] ?=> Any): Gen[W]
```

Inside `generator[W] { … }`: `Gen.emit(w).!?` (or bare), a bare
`Writer(w)` statement, `Gen.stop[W].!?`, `while`/`if`/recursion as in
any block — and `for x <- xs yield e` as the block's VALUE emits each
`e`. That is the only place `yield` means emit: the block has said it
is a generator; everywhere else `yield` collects, as Scala means it.
Mid-block, the emitting loop is spelled `for x <- xs do Gen.emit(e)`:
a for-yield in statement position is a value Scala's own checker
reports discarded (E176) before the macro runs, and this repository
compiles with warnings as errors — the macro's statement hook is there
and works, but a user under `-Wall` cannot reach it cleanly.
In an ordinary block, `for x <- gen do body` over a `Gen` reads it
through `iterator` — as far as the loop drives, and memoised so
multi-shot re-entry is sound (direct-loops' rule).

## Behavior

- [x] LAZINESS (the Python law): a body that counts its steps, read
      through `iterator`, has run to its k-th yield after k `next()`
      calls and no further — the code between two yields runs when
      the SECOND is asked for; the same through `take(k).toList`,
      `first`, `find`, `exists`, and through the generator block.
- [x] TERMINATION, three ways: the body ends → the iterator is
      exhausted, `toList` has everything; `Gen.stop` in the middle of
      a loop ends it there and nothing after it runs — through
      `map`/`take` too; a reader that stops (`take`, `first`, a
      `foreach` that throws) never runs the rest of the body.
- [x] An INFINITE generator (`unfold`, `while true` in a block)
      composes with `map`/`filter`/`take`/`takeWhile`/`drop` and
      terminates through every stopping reader.
- [x] `for x <- g; y <- h(x) if p(x, y) yield f(x, y)` over `Gen` —
      no direct block — is the nested generator, lazy: the inner
      generator's counter shows it ran only as far as read.
- [x] `generator[W] { for x <- xs yield f(x) }` emits each `f(x)`;
      mid-block `for x <- xs do Gen.emit(e)`, a guard and two
      generators inside, a yielded value that is itself an effect of
      the row (its mark runs first), `stop` inside `while true`, and a
      recursive generator (the block calls its own def; 100 000 deep
      through `take(2)`).
- [x] NON-MEMOISING: reading a `Gen` twice runs the body twice;
      `toLazyList` reads once.
- [x] Deep: 100 000 elements through `toList`, `drop`, `iterator`
      on the default stack — a `Gen` is a Free program, flat by
      `Free.flatMap`'s laziness (unlike a `Handled` staged block).
- [x] `for x <- gen do body` in an ordinary block reads the generator
      as far as the loop drives (the counter says so).
- [x] Every existing direct suite and the core suite unchanged.

## Out of scope

- Resources: a body under `Resource`/`bracket` releasing on an early
  stop — the finalizer rides a continuation the reader dropped; a lane
  of its own if the runner's drop does not fire it (a Python
  generator's `finally` on `close()`).
- Sending values INTO a generator (Python's `send`): that is a
  coroutine pairing, and it exists — `Take.Await` + `Writer` + `pipe`
  (Pipe.scala, Kiselyov's iteratee by delimited continuation).
- A stateful `takeWhile` routed through a stopping transducer
  (fold-until stage 3) when it lands.
- Async generators: `Source` is that already.

## Design

- **A generator is a program that tells; `Stop` is one more member of
  its row.** `Gen.of` is a row widening, a `generator` block is a
  `direct` block over the row, and a `Source` is the same thing plus
  `Async`. The class around it is a VALUE CLASS — no allocation — and
  exists for name resolution alone (Decisions).
- **Element-wise operators are STAGES, read as one walk**
  (specs/gen-chain-fusion.md, after this lane): a stopping reader
  walks the source once and applies the chain per element, so
  `take(n)` is done at its n-th kept element and the body runs no
  further. The walks this lane wrote (`splice`: `relay`'s shape with a
  program-valued answer, the continuation called only when `f(w)` has
  been read through, a `Stop` dropping it; `taking`, which drops the
  continuation after the n-th tell; `filtering`; `dropping`, deferring
  each skipped step so a long skip is flat) remain as `program` — the
  chain materialised for `iterator` and a `generator` block.
- **Readers are `FoldUntil`** (the sibling's): `done` asked before the
  first element and after each, a `Stop` ending the read as the
  body's end does. `toList`/`foreach` are never-done instances.
- **`iterator` is the Python semantics made literal:** a stepper over
  `resume` that hands out `w` from `Bind(Inject(Say(w)), k)` and
  holds `k`, applying it only on the NEXT `advance()` — so the code
  between two yields runs when the second is asked for, not when the
  first is delivered (the first cut applied `k` eagerly and was one
  step ahead of Python).
- **`yield`-as-emit is three hooks in the macro**, all gated on the
  block's row naming `Stop` (`genRow`): a `map` HofCall in statement
  position (compileBlockSeq, stmtsTail) and as the block's final
  expression is rewritten to a `foreach` whose body is
  `Writer[W](e)` — the `Say` op itself, a bare runnable statement,
  which RUNS by do-notation (`Writer.tell` is a program, and a program
  in statement position is a value, not a step). A yielded `e` with a
  mark in it is bound to a val FIRST and the op built from the val: a
  mark hoisted out of the op's ARGUMENT would leave the op a bound
  value, and a bound value does not run (the last failing law).
  `for x <- gen do` needs no hook: `iteratorOf` recognises a `Gen`
  receiver and reads it through `Gen.iterator`.

## Decisions

- **`Stop` as an effect, not a `Pure` from the middle** — chosen
  because a loop has no `return`; an effect the readers all understand
  ends the generation from anywhere, and a `Gen` without one is a
  plain Writer program widened by `of`. Rejected: `Throws`-shaped
  abort with `runEither` at the block edge (a second row member the
  user would see in every type).
- **`generator` block typed `?=> Any`, value dropped** — chosen so a
  for-yield may be the last line without a `()` after it; the block
  answers `()` by construction.
- **Non-memoising by default** — chosen because a program is a value
  and re-running it is what the library does everywhere; Python's
  one-shot generator is the special case, `toLazyList` gives it.
- **A value class, after two refutations in one lane** — the first
  cut was a transparent alias with extensions: an extension on
  `Unit ! (Writer % W + Stop)` cannot infer `W` from a type-lambda row
  (every `Writer.run` in this repository passes its types by hand for
  that reason). The second was an opaque type with extensions: the
  package's generic `map` over `Id` sits in LEXICAL scope and beat the
  extension in the type's prefix scope — `y` in a for-comprehension
  typed as the whole generator. A member beats both; `AnyVal` makes it
  free; `program`/`fromProgram` cross the line. What was feared of a
  wrapper — re-exporting every Writer combinator — did not happen:
  `program` is one field, and the Writer road is one call away.

## Results

2026-09-22. `TestGen` 10/10 (core), `TestGenerator` 9/9 (okay-direct, the docs snippets verbatim as the ninth);
okay-direct suite 348/348. Every box above closed by a law that names
its counter: the laziness laws by a `Counted` body (k `next()` → k
steps, `take(3)` → 3, `first` → 1, `find(_ > 4)` → 5, `exists(_ == 2)`
→ 2), termination by `after == 0` past a `stop` and `steps == 3` past a
throwing reader, the nested comprehension by the inner counter (≤ 4 for
two pairs read), the deep laws at 100 000, the generator block by the
same counter through `iterator` and `take`.

Measured (generators-jmh, 2026-09-23; `compare/GenBenchmark`, 10 000
Longs unfolded, JDK 26 forks; time from per-lane gated runs — each
lane `-f 2 -wi 3 -i 5` alone, started only after 20 s with no sibling
sbt or JMH fork and CPU under 200%, re-run when the box was busy at
its end, because three whole-matrix rounds on a box running sibling
gates back to back came out at ±50–110% — and B/op from `-prof gc`
the same way; src/jmh/history.tsv `gj-*`):

| lane | µs / 10k | B / elem |
|---|---|---|
| `Source.range.runCollect` (Async, Vector) | 135.4 ± 1.4 | 164 |
| the same program at `Writer % Long`, `Writer.run` | 134.2 ± 1.5 | 192 |
| `Gen.unfold.iterator`, summed (the Stepper) | 150.5 ± 7.3 | **191** |
| the same program, `Writer.foldUntil(collecting)` | 157.3 ± 0.9 | 215 |
| `Gen.unfold.toList` (`Gen.read`, collecting) | 219.4 ± 4.7 | 239 |
| `Writer.map` + a filtering `Writer.fold` step | 214.9 ± 1.4 | 272 |
| infinite `Gen.unfold.take(10k).toList` | 252.3 ± 3.5 | 295 |
| `Gen.unfold.map(_ * 2).filter(_ % 3 == 0).toList` | 338.7 ± 2.5 | 381 |
| the same, `filter` as a walk (gen-filter-as-walk, 2026-09-23) | **283.9** | **354** |

The entry's question answered in two halves. THE VALUE CLASS ADDS NO
FRAME: `Gen.iterator` allocates what `Writer.run` allocates, to the
byte (191 vs 192 B/elem), and the 48 B/elem `toList` adds over it is
the `List` cons and the reverse, not the wrapper. SPLICE DOES:
`filter` is `splice`, a program (`emit`/`empty` plus its `flatMap`)
built per element, +109 B and +124 µs on 10k over the hand road —
the one combinator where the pipeline is not parity, filed as
`gen-filter-as-walk` and CLOSED the same day: written as a walk (the
kept tell is the input's own `Inject` node re-bound to the rest of the
walk, the rejected one a `Free.delay` skip), the pipeline reads 283.9 µs
/ 354 B/elem — 0.83 of the splice in time, 0.93 in bytes, on two quiet
alternated pairs (history `gf-*`). What remains over the hand road's
272 is the walk's own `Bind` + closure per kept element and `Delay` +
thunk per rejected one; a budget that recursed straight through up to
64 rejections before deferring allocated LESS (330 B/elem) and read 7%
SLOWER — the runner's trampoline beats a call chain through `split`'s
closure — and was not kept. The rest is `gen-chain-fusion`'s question
(the filter fused into the reader, no program between). Between
them, `Gen.unfold.toList` over `Writer.foldUntil` on the same program
is +40% and +24 B/elem — REFUTED as a Stop-row cost
(gen-read-stop-residual, 2026-09-23): `Gen.of(prog)`, the identical
`prog` widened by `Stop` and nothing else, allocates LESS than the
Stop-free `Writer.foldUntil` floor. The whole gap is `unfold`'s own
`S => Option[(W, S)]` step — an `Option`+`Tuple2`+two boxed `Long`s
per step, Scala's own `unfold` convention, not worth avoiding for a
scalar-state benchmark shape production code rarely hits. `take`
re-emits and costs +15% / +56 B. `Writer.run` and `Source.runCollect` sit at parity in
time; the Vector road allocates 28 B/elem less than a `delay`-per-step
program collected into a `List`.
