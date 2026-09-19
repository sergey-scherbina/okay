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

- [ ] `Stream.fold` answers what it answered on every instance
      (TestStream: `Seq`, `Monoid`, `sum`, `count`, `first`, `last`
      over a `Producer`).
- [ ] `find`, `exists`, `forall` still stop at the first deciding
      element on an ENDLESS stream (TestStream over `nats`), which
      `Iterator`'s own `find`/`exists`/`forall` guarantee.
- [ ] `foldLeft`, `foreach`, `toList` agree with the `toLazyList` road
      on a `Producer` and on a `Feed` (new test), and `Stream.fold`
      over a G-effectful writer stream performs its G exactly once per
      operation (TestGenerate's oracle already counts this for the
      iterator; the fold is the iterator now).

## Out of scope

- The accumulator dispatch in `Stream.fold`: the lane MEASURES whether
  it is worth it now (the `streamGeneric`/`streamSpecialized` pair of
  `FoldConsumersBenchmark` is exactly that question) and records the
  answer; it adds the dispatch only if the bars separate.
- `take(n)` and the LazyList-answering combinators (above).

## Design

`Stream.fold(s)` is `val it = St.iterator(s); var b = f.init; while
it.hasNext do b = f.add(b, it.next()); b`. Each postfix consumer is
the same call on `Iterator`'s method of the same name.

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

(filled by the lane: `FoldConsumersBenchmark.streamSpecialized` and
`.streamGeneric` against their old shape written out verbatim as
control rows, two rounds; `ProducerWriterCarrierBenchmark.
bridgeProducerDirect` against its recorded 114.5.)
