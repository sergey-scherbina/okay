# stream-fold-via-iterator — the linear consumers walk the linear view

## Overview

`Stream[S, F]` (Stream.scala) has two observations: `uncons`, the
codata one — an `Option[(A, S[A])]` inside a program in F, repeatable,
what `zip`, `interleave` and the `LazyList` bridge are built on — and
`iterator`, the LINEAR one, added instance by instance as each was
measured: `LazyList` and `List` walk their own iterators, `Producer`
and the G-effectful producer walk the freer tree directly, `feedStream`
and `writerStreamIn` the same for a writer program. Every instance in
the tree overrides it, and the reason is the same each time: the
default `Iterator.unfold(s)(uncons(_).runWith)` builds an `Option`, a
tuple and a program per element and runs the program — measured on
the G-effectful producer at 103.6–114.5 us per 10k against 57.3
through the specialised walk (producer-effectful-stream-iterator,
2026-09-19).

`Stream.fold` and the postfix consumers `foldLeft`, `foreach`, `find`,
`exists`, `forall`, `toList` did not use it. `fold` stepped by
`uncons(_).runWith` per element, and the six went through
`toLazyList`, which is `uncons` per element plus a memoised cell and
a synchronised lazy state per element for a value that is read once.
`Stream.fold`'s own doc declined the accumulator dispatch `Chunks.fold`
makes on the ground that "the freer tree steps once per ELEMENT and
that step is ~150us per 10k" (history.tsv:140) — which is the cost
this spec removes, so that reasoning is re-measured here rather than
carried forward.

## Interface

Unchanged. `Stream.fold[S, F, A, B](s)(using Fold[A, B])(using
Stream[S, F], Handler[F]): B`, and the six postfix consumers keep
their signatures and answers. `Foldable[Producer]` (Generate.scala),
which is `Stream.fold`, follows.

`filter`, `collect`, `drop`, `takeWhile`, `dropWhile`, `zip`,
`zipWithIndex`, `++`, `Stream.map`, `Stream.flatMap` and `take(n)`
answer a `LazyList` and are NOT touched: their carrier is an API
decision (a memoised stream a caller may re-observe), not a
consumption detail.

## Behavior

- [x] `Stream.fold` answers what it answered on every instance
      (TestStream: `Seq`, `Monoid`, `sum`, `count`, `first`, `last`
      over a `Producer`).
- [x] `find`, `exists`, `forall` still stop at the first deciding
      element on an ENDLESS stream (TestStream over `nats`), which
      `Iterator`'s own `find`/`exists`/`forall` guarantee.
- [x] `foldLeft`, `foreach`, `toList` agree with the `toLazyList` road
      on a `Producer` and on a `Feed` (new test), and `Stream.fold`
      over a G-effectful writer stream performs its G exactly once per
      operation (TestGenerate's oracle already counts this for the
      iterator; the fold is the iterator now).

## Out of scope

- (The accumulator dispatch in `Stream.fold` was out of scope until
  round 2 measured it as the remaining cost — see Results; it is in.)
- `take(n)` and the LazyList-answering combinators (above).

## Design

`Stream.fold(s)` is `val it = St.iterator(s); var b = f.init; while
it.hasNext do b = f.add(b, it.next()); b`, in five copies dispatched
on the accumulator's shape exactly as `Chunks.fold` does
(`Fold.OfLong`/`OfInt`/`OfDouble`/`OfBoolean`, then the generic arm).
Each postfix consumer is the same call on `Iterator`'s method of the
same name.

## Decisions

- **Iterator, not a hand-rolled walk per consumer** — chosen because
  the walk is already written once per instance, where the carrier's
  shape is known; six consumers times six instances is the
  alternative. Rejected: dispatching on `S` inside `fold` (the instance
  already did).
- **No `Handler[F]` change** — `iterator` needs the same handler
  `uncons(_).runWith` needed; on JS a `Source` still cannot be
  iterated (no `CanBlock`), exactly as it could not be folded before.

## Results

Measured 2026-09-20, `FoldConsumersBenchmark` with the old walk
written out verbatim as control rows (`streamGenericControl`,
`streamSpecializedControl`, `foldLeftLazyListControl`) so each pair
alternates in one run; `-f 2 -wi 3 -i 5 -prof gc`; 10k elements. Rows
`sfi-*` in history.tsv. Round 1 ran on a BUSY box (load 14, a
sibling's gate), rounds 2–3 at load ~4.

| lane | new | control | B/op new | B/op control |
|---|---|---|---|---|
| `Stream.fold`, sumLong, round 1 (no dispatch) | 92.3 ±1.4 us | 135.0 ±3.2 | 1 116 585 | 1 516 561 |
| `Stream.fold`, sumLong, round 2 (no dispatch) | 92.7 ±2.4 | 133.4 ±2.5 | 1 116 585 | 1 516 561 |
| `Stream.fold`, generic Fold, round 2 | 93.1 ±1.9 | 142.1 ±3.4 | 1 116 585 | 1 516 561 |
| `foldLeft` via iterator vs `toLazyList`, round 1 | 92.0 ±1.3 | 224.5 ±5.9 | 1 116 585 | 2 076 634 |
| `foldLeft` via iterator vs `toLazyList`, round 2 | 88.1 ±2.1 | 209.1 ±12.7 | 1 116 585 | 2 076 633 |
| **`Stream.fold`, sumLong, round 3 (WITH dispatch)** | **58.3 ±0.9** | 133.7 ±0.8 | **876 992** | 1 516 561 |
| `Stream.fold`, generic Fold, round 3 | 95.6 ±7.7 | 137.3 ±1.4 | 1 116 585 | 1 516 561 |
| `bridgeProducerDirect` (Async producer, sumLong), round 3 | 64.0 ±4.4 | 114.5 (recorded) | 877 032 | — |

What the numbers say:

- **The iterator alone: 0.68x time, −26% B/op on `Stream.fold`,
  same sign in two rounds.** 40 bytes per element gone — the `Some`
  and the tuple `uncons` built. The postfix road was worse than the
  fold's: `toLazyList` cost 0.41–0.42x and −46% B/op, 96 bytes per
  element for the memoised cell and its lazy state.
- **The dispatch question, answered the other way round.** With the
  iterator in and no dispatch, `Stream.fold` over the Async producer
  read 98.4 us / 1 116 625 B where a hand-written `while` over the
  SAME iterator (`producerIteratorSpecialized`) read 58.1 / 876 944:
  24 bytes per element, a boxed `Long` accumulator through the generic
  `add(Object, Object)Object`. On the old walk that was invisible
  under the per-element program (139.9 vs 157.2, bars overlapping —
  the doc was right for the walk it described); on the iterator it is
  40% of what is left. With the dispatch (round 3) `Stream.fold`
  reads 58.3 and 876 992 B — the hand loop to within 88 bytes — and
  the generic arm is unchanged, so the dispatch is the specialised
  accumulator's win and nothing else moved.
- **The G-effectful producer: 114.5 recorded → 64.0.** The 0.56x the
  backlog entry predicted from producer-effectful-stream-iterator's
  own pair (103.6–114.5 vs 57.3), reached through the library call
  rather than the benchmark's hand loop.

Behaviour: TestStream (25) and TestGenerate green on the JVM before
the gate, including the early-stop laws over `nats` and the new
agreement test against the `toLazyList` oracle; the full family gate
is the landing's.
