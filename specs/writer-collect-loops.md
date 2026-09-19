# writer-collect-loops — one loop under `collect`, `runCollect`, `concat`

## Overview

`Writer.loopWith` (Writer.scala) is the tail-recursive walk every
Writer drain is made of: tail-recursive across tells, re-entered
through `flatMap` only when an operation of the other row member has
to be forwarded, `finish` applied where the PROGRAM ends rather than
as a `.map` over the residual. `Writer.fold` and `Writer.run` are
calls to it. Three more drains were written as their own copies of
that loop — `Writer.collect`, `Source.runCollect`, `Source.concat` —
and each copy accumulates with `Vector :+` per element, the one cost
either-scalarised (2026-09-09) measured as the WHOLE price of
`Writer.run` (~150 B per append against 24 B for a cons) and removed
there. `Source.concat` additionally finishes with `.map(_._1.flatten)`
over a program that still forwards Async, which is the trap
`loopWith`'s own doc names: a rotation per forwarded operation.

The reason they were copies rather than calls was real and is gone.
`loopWith` splits on `TypeableK[Writer % W]`; until 2026-09-19 that
instance was derived through a `Typeable[W]` and at a parameterised W
(`Chunk[Byte]`, an abstract `A`) it was an unchecked test with an E092
warning the build treats as red, so the three split on the concrete
G instead. writer-typeablek-by-class (87be6174) made `writerK` a test
on the class of `Say` — total, no `Typeable[W]`, no warning — and the
comments on all three still cite the vanished reason.

## Interface

- `Writer.loopWith[W, S, A, R, F](a)(z)(step)(finish)`, `finish:
  (S, A) => R`, answering `R ! F`. The answer reaches the finisher, so
  a drain that wants only the accumulator (`runCollect`), or the
  accumulator flattened (`concat`), finishes inside the loop.
- `Writer.foldWith`, `Writer.fold`, `Writer.run`, `Writer.collect`:
  signatures unchanged; `collect` still answers `(Vector[W], A) ! G`.
- `Source.runCollect: Vector[A] ! Async` and `Source.concat:
  Vector[X] ! Async`: unchanged signatures.

## Behavior

- [x] `collect`, `runCollect`, `concat` answer what they answered:
      every told value in order, the program's answer kept where the
      signature keeps it, every forwarded operation performed once and
      in place (TestGenerate's oracle law over the pure iterator,
      TestChunkEdges' runCollect tests, TestSourceProducer's
      `Writer.collect` over a `fromProducer` source, the okay-jdbc
      suites over `Source.concat`).
- [x] no `.map` is applied over a residual by any of the three: the
      finisher runs in the loop's `Pure` arm.
- [x] the accumulator is immutable at every re-entry (a cons list),
      so a forwarded continuation resumed twice sees the accumulator
      it captured — the multi-shot condition `State.handle` and
      `Fused` state.

## Out of scope

- `Producer.concat` / `Producer.fold`: zero callers of `concat`, and
  pwc-arc-close kept `Producer` whole; the same `finish` could be
  threaded through `Producer.fold` the day a caller wants it.
- `Stream.fold` and the `toLazyList` consumers — a separate entry
  (`stream-fold-via-iterator`).
- Changing what any of the three answers (a `List` instead of a
  `Vector`, say): the callers in okay-jdbc/okay-rag read `Vector`.

## Design

`finish` gains the answer as a second argument. `loopWith` is
`inline` with inline `step` and `finish`, so the tuple `foldWith`
and `run` build in their finisher is built exactly where the old
`Pure((finish(s), a))` built it — no closure, no extra allocation.

The three drains:

    collect    = loopWith(a)(Nil)(w :: _)((l, a) => (l.reverse.toVector, a))
    runCollect = loopWith(s)(Nil)(a :: _)((l, _) => l.reverse.toVector)
    concat     = loopWith(s)(Nil)(c :: _)((l, _) => <flatten reversed l into one Vector>)

`concat`'s finisher walks the reversed list once through a
`Vector.newBuilder` with `sizeHint` on the summed chunk lengths, so a
page-fetching source (okay-jdbc's drains) pays one array copy per
chunk and no intermediate `Vector` of chunks.

## Decisions

- **`finish: (S, A) => R`, not a second `loopWith` overload** —
  chosen because one loop is the point; the old `S => S2` is
  `(s, a) => (finish(s), a)` at its two callers and nothing else
  called it. Rejected: keeping `S => S2` and mapping over the result
  (the residual trap this spec exists to remove).
- **Cons + one reverse, not a `Vector.newBuilder` threaded through
  the loop** — a builder is mutable and a forwarded continuation may
  be resumed more than once (multi-shot), so the accumulator must be
  the value at capture time; `Writer.run` made the same choice for
  the same reason.

## Results

(filled by the lane: `IdiomaticApiBenchmark.okayCollection_elem_runCollect`
before/after, and a `Source.concat` row.)
