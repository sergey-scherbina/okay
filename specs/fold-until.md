# fold-until — a fold that can stop

## Overview

`Fold[A, S]` (Fold.scala) is a start and a step, and every consumer
that runs one walks to the END of its input by construction: a
`Foldable` runs it over all its elements, `Writer.fold` over every
tell, `Chunks.fold` over every chunk. Its own `exists` says so — "it
keeps scanning: a `Fold` has no way to stop, which is what
`Chunks.exists` is for" — and `Chunks.exists` does not exist (grep
`def exists` over okay-stream: nothing). The comment cites a phantom,
and the phantom is the gap: nothing in the library expresses "read
until the state says it has seen enough", so `find`, `headOption`,
`take(n)`, "pull until the header has parsed" are each either a
full scan or a hand-written walk.

The shape the operator brought (2026-09-22) is the general form:

```scala
@tailrec def loop[S, A](s: S)(f: S => Either[S, A]): A =
  f(s) match
    case Left(s) => loop(s)(f)
    case Right(a) => a
```

— a state that DECIDES when the iteration ends: `tailRecM` at `Id`,
and over an input, a catamorphism with a halt. The library already
has it at two seams and lacks it at the third: `Proc.Iter(body:
Proc[F, X, Either[X, Y]])` is exactly this as a free arrow
(okay-workflow), `Source.unfold(s)(S => Option[(A, S)])` is its dual
on the producing side, and the CONSUMING side — the fold — cannot
stop. This spec adds the stopping fold, then names the three other
places the same form belongs, each with the trigger that opens it.

## Interface

Stage 1 (this lane):

```scala
/** a left fold that may stop: `done` is asked after every step and
 *  before the first; `end` turns the final state into the result
 *  whether the input ended or the fold did */
trait FoldUntil[-A, S, R]:
  def init: S
  def add(s: S, a: A): S
  def done(s: S): Boolean
  def end(s: S): R

object FoldUntil:
  /** the operator's shape: step answers Left(next) or Right(result) */
  def until[A, S, R](z: S)(f: (S, A) => Either[S, R])(end: S => R): FoldUntil[A, Either[S, R], R]
  def find[A](p: A => Boolean): FoldUntil[A, Option[A], Option[A]]
  def headOption[A]: FoldUntil[A, Option[A], Option[A]]
  def exists[A](p: A => Boolean): FoldUntil[A, Boolean, Boolean]
  def forall[A](p: A => Boolean): FoldUntil[A, Boolean, Boolean]
  def take[A](n: Int): FoldUntil[A, Vector[A], Vector[A]]
```

One consumer per carrier, each beside the `fold` it mirrors:

```scala
Stream.foldUntil[S[_], F[+_], A, B, R](s: S[A])(using FoldUntil[A, B, R])(using Stream[S, F], Handler[F]): R
Chunks.foldUntil[A, S, R](p: Chunks[A])(using FoldUntil[A, S, R]): R
Writer.foldUntil[W, S, A, R, F[+_]](a: A ! Writer % W + F)(using TypeableK[Writer % W], FoldUntil[W, S, R]): R ! F
extension [A](s: Source[A]) def runFoldUntil[S, R](using FoldUntil[A, S, R]): R ! Async
```

`Writer.foldUntil` answers `R` alone, not `(R, A)`: a fold that stops
early never sees the program's answer, and a signature that promised
it would have to invent one.

Stages 2–4, specified here so the form is written down once, opened
by their triggers (Decisions):

- Stage 2 — `loop` on `!`: `def loop[S, A, F[+_]](s: S)(f: S =>
  Either[S, A] ! F): A ! F`, `tailRecM` for programs, stack-safe by
  `Free.flatMap`'s laziness. Trigger: rewriting two of the twelve
  hand-written `def loop(state)` receive/dialog loops (Actor.scala:287,
  Toolkit.scala:14–47, Dialog.scala:60, Ui.scala:693, Form.scala,
  Conversation.scala:278, Nio.scala:97, chatweb Main.scala:30) and
  finding them shorter.
- Stage 3 — a stopping `Stage.transduce`: `step: (S, I) => Stage[I,
  O, Either[S, R]]`, a stage that stops PULLING when its state says
  so (a prefix parser, a stateful `takeWhile`, "the first n matches").
  Trigger: a consumer that needs it — `Chunks.takeWhile` is
  predicate-only today and nobody has asked for state.
- Stage 4 — the `Foldable` side: `foldUntilTo[S, R](using FoldUntil)`
  beside `foldTo`, so a `List`, a `Producer` and a `Source` share one
  instance. Trigger: a second collection caller beyond `Stream`.

## Behavior

Stage 1:

- [x] `FoldUntil.find/headOption/exists/forall/take` answer what the
      `List` methods of those names answer, on every input including
      the empty one and one where the stop never fires.
- [x] `FoldUntil.until(z)(f)(end)` runs the operator's `loop`: the
      `Right` ends it, a `Left` continues, and `end` is applied to the
      last `Left` state when the input runs out first.
- [x] `Stream.foldUntil`, `Chunks.foldUntil`, `Writer.foldUntil` and
      `Source.runFoldUntil` agree with `foldLeft` over the
      `takeWhile`-prefix on the pure road: the same instance over the
      same elements gives the same `R` on every carrier.
- [x] THE WALK STOPS: a source that counts its productions, asked for
      3 of 1000 through `take(3)`, produces 3 elements — and a chunked
      one produces one chunk (`Chunks.foldUntil` pulls no chunk after
      the one that satisfied it).
- [x] `done(init)` is honoured: `take(0)` consumes nothing, on every
      carrier.
- [x] `Writer.foldUntil` performs a forwarded `F` operation that
      precedes the stop and does NOT perform one that follows it
      (counted through a `Handler`).
- [x] `Writer.foldUntil` is tail-recursive across tells: a source of
      100 000 elements with the stop never firing folds on the
      default stack.

## Out of scope

- An unboxed `FoldUntil` (`OfLong` and friends). `Fold`'s
  specialisations exist because a fold over 10k longs measured the
  boxed accumulator as the whole cost; nothing has measured a
  stopping fold on that shape, and a `Boolean`-returning `done` on a
  boxed `S` is one branch per element, not an allocation. Measure
  before adding — the rule that built `Fold.OfLong`.
- Replacing `Fold.exists`/`forall`: they stay, they are `Fold`s and a
  `Fold` consumer cannot stop; their doc line is corrected to name
  `FoldUntil.exists` instead of the phantom.
- Stages 2–4, each behind its trigger above.

## Design

The trait is the MACHINE form — `add` then `done`, a halting Moore
machine — and the operator's `Either` form is an adapter over it,
rather than the other way round, because of what this library has
measured about per-element allocation: `split` not `<|>` ("no Either
per tell", Writer.scala), no `Option`/tuple per element in every
specialised `iterator` (Generate.scala), the boxed accumulator as the
whole cost of a fold (Fold.scala). `step(s, a): Either[S, R]` puts
one `Left` on the heap per element in every consumer; `add` + `done`
puts nothing, and `until(z)(f)(end)` pays the `Either` only where the
caller wrote one. The two are equivalent: `until`'s state is
`Either[S, R]`, `done` is `isRight`, `end` reads either side.

`done` is asked BEFORE the first element as well as after each step,
so `take(0)` consumes nothing and a consumer never pulls an element
it will not use — for a chunked or async source that pull is the
cost being avoided.

The walks:

- `Stream.foldUntil`: the iterator, `while !done(s) && it.hasNext`.
- `Chunks.foldUntil`: the per-chunk `while` of `Chunks.fold`'s generic
  arm with `done` checked per element, and the OUTER loop checking it
  before pulling the next chunk — the second check is the one that
  saves a chunk's production.
- `Writer.foldUntil`: `Writer.loopWith`'s tail-recursive walk with an
  early `Pure(end(s))` on `done` after a tell — the `Bind(Inject(Say),
  k)` arm does not call `k` when the state is done, which is what
  stops the producer. A forwarded `F` operation is still re-entered
  through `flatMap` as in `loopWith`. NOT `inline`, unlike `loopWith`:
  the fold arrives as data, so there is no step to beta-reduce — the
  same reason `Writer.fold` is a plain def over the inline `foldWith`.

## Decisions

- **`add` + `done`, not `Either`** — chosen for the allocation
  argument above; the `Either` form is `FoldUntil.until`. Rejected:
  `step: (S, A) => Either[S, R]` as the primitive (an allocation per
  element in every consumer, to save an adapter).
- **`FoldUntil` does not extend `Fold`** — a `Fold` consumer walks to
  the end, so passing a stopping fold where a `Fold` is expected would
  silently lose the stop (`take(3)` would take everything). The two
  are different contracts; a `Fold` that never stops is
  `FoldUntil` with `done = false` if anyone needs the lift, and nobody
  has.
- **`Writer.foldUntil` answers `R`, not `(R, A)`** — the answer does
  not exist when the fold stopped early; see Interface.
- **Stages 2–4 stay `- [ ]` behind triggers** — the repository's rule
  is a consumer first; the form is written down here so the next lane
  does not re-derive it.

## Results

Stage 1 (2026-09-22): `TestFoldUntil` (core, 5) and
`TestFoldUntilStreams` (okay-stream, 4), gate green with no warnings.
The stop is measured by counting, not asserted by reading the code:
`take(3)` over an on-demand `LazyList` evaluates 3 elements,
`find(_ == 4)` evaluates 5; over an infinite `Chunks.generateWith`
`take(3)` pulls 1 chunk and `find(_ == 5)` pulls 2; over a source
with an Async operation after every tell, `take(3)` performs 2 of
them and `take(0)` none. 100 000 tells with the stop never firing
fold on the default stack on both the writer and the chunk road.
Not measured: the per-element cost of `done` against `foldLeft` on
the unboxed shape — Out of scope until a caller has that fold.
