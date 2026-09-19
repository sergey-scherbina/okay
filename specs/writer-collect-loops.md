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

Measured 2026-09-20, `ProducerWriterCarrierBenchmark` section 5: each
new drain beside the shape it replaced, written out verbatim as its
control row so the pair alternates in one run. `-f 2 -wi 3 -i 5 -prof
gc`, two whole rounds, quiet box (load 2.7–2.8 at start). Rows
`wcl-*` in history.tsv.

| lane | new | control | B/op new | B/op control |
|---|---|---|---|---|
| `runCollect`, 10k Longs, round 1 | 141.4 ±15.7 us | 154.4 ±4.5 | 1 564 129 | 2 365 497 |
| `runCollect`, 10k Longs, round 2 | 135.9 ±13.7 | 158.9 ±6.2 | 1 564 121 | 2 365 497 |
| `concat`, 64×1024 bytes, round 1 | 89.6 ±1.2 | 90.4 ±1.5 | 313 512 | 317 496 |
| `concat`, 64×1024 bytes, round 2 | 89.6 ±1.7 | 91.3 ±3.6 | 313 512 | 317 496 |
| `concat` + one `async` per chunk, round 1 | 90.4 ±0.9 | 91.1 ±0.4 | 323 240 | 331 329 |
| `concat` + one `async` per chunk, round 2 | 91.9 ±3.2 | 95.1 ±5.9 | 323 240 | 331 329 |

What the numbers say, and no more:

- **`runCollect`: −34% B/op, exact both rounds, and 8–14% time with
  the same sign both rounds.** 80 bytes per element gone, which is
  the `Vector :+` price either-scalarised measured on `Writer.run`
  (−35% B/op there). The time bars are wide on the new arm
  (±14–16 us); the sign is what two rounds establish, not the digit.
- **`concat`: time PARITY.** The lane is a 64 KB byte copy; the
  drain's own cost is a few percent of it, and the bars overlap. Not
  a win in time and not claimed as one — the change is the one loop
  and the allocation.
- **The mapped residual, priced: 126 B per forwarded operation.**
  The plain and the async source differ only by one `async` per
  chunk, and the async control row allocates 8 088 B more than the
  new async row over 64 chunks where the plain pair differ by
  3 984 B — the extra 4 104 B are 64 rotations of ~64 B plus the
  per-op re-bind, the trap `loopWith`'s doc describes, seen
  directly. On a page-fetching driver that is per page and does not
  matter in time; it is recorded because it is the first time the
  trap has been priced at the seam rather than argued.

Behaviour: core 748, okay-stream 347, okay-blob 21 tests green on the
JVM before the gate; the full affected gate is the landing's.
