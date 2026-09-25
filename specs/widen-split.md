# widen-split — `!.widen` is a coercion; the walk is `!.normalize`

## Overview

`!.widen[A, F, G](p: A ! F): A ! (F + G)` carries two jobs under one
name. Its TYPE says "the same program in a wider row" — which
`Row.plus`/`.at`/`.up` already do for free: one commented
coercion (`Row.coerce`), nothing forced, nothing walked. Its BODY
is a walk: it resumes the head and rebuilds the tree node by node —
a normalisation whose measured beneficiary is `Source.merge`, and
there through **`Writer.widen`** (the element-type re-tell `A → A|B`,
history `free-row-variance-widen-in-merge`: without those two passes
the merge is 5.3% slower), not through `!.widen` itself. `!.widen`'s
own comment claimed the same benefit by analogy.

The walk has a cost every caller pays and one bit them: the eager
head. `resume` forces a `Delay`, so a program whose state is made
under `Free.delay` started at widen time and the widened VALUE held
that start — run twice, the second run met the first run's state
(windows-stage-rerun-loses-pane, 2026-09-23; fixed there by rebuilding
the two deferred shapes as deferred, which treats a symptom of the
doubled name). 71 call sites over 30 files say `!.widen` and, read one
by one, want the upcast — a stage joining an effectful row
(`Http.framing`, `Ws.session`, `Sessions`), a pure program entering an
`Async` row, an operation typed at its own signature entering the
row of the block. Operator, 2026-09-23: "widen несёт двойственную
нагрузку — может стоит разделить?"

## Interface

- `!.widen[A, F, G](p: A ! F): A ! (F + G)` — the coercion, by
  `Row.into` (the one cast, sound by the erasure argument
  Row.scala states). Signature unchanged: every call site
  compiles as it is.
- `!.normalize[A, F, G](p: A ! F): A ! (F + G)` — the walk `widen`
  used to be, under the name of what it does: resume the head,
  rebuild the tree node by node, deferred shapes rebuilt as deferred.
  For a caller who wants the rotation done up front — none is known;
  `Source.merge` keeps `Writer.widen` for the element type, which is a
  different function and stays.
- `Writer.widen` — unchanged.

## Behavior

- [x] every `!.widen` call site compiles unchanged, and the full
      family is green: the coercion produces the same told elements
      and the same answer as the walk on every shape (`Pure`, `Inject`,
      `Bind`, `Delay`, `Bind(Delay, f)`), asserted by a law over the
      two names on one program.
- [x] `!.widen` forces nothing: a program under `Free.delay` whose
      thunk counts is widened with the count at 0, and the count is 1
      after ONE run and 2 after two — the rerun law the eager head
      broke, now true by construction and pinned in core rather than
      only in okay-clojure/okay-java's stages.
- [x] `!.normalize` agrees with `!.widen` on every shape (the law
      above, both directions) and is what the old `widen` was, so the
      stage suites that pinned the deferred-head fix stay green
      through it too.
- [x] NUMBER OWED, a `through` over a widened pure stage (TestPipe's
      shape, 10 000 elements): `!.widen` (coercion) against
      `!.normalize` (the walk) against the stage at the row directly
      (the floor) — the coercion at the floor, the walk above it, with
      the count of nodes rebuilt per pull stated.
- [x] NUMBER OWED, the merge lanes: `MergeBenchmark.okaySourceMerge`
      unchanged (it never used `!.widen`; `Writer.widen` stays), and the
      chunked merge (`ChunkFlushBenchmark.okayChunked`, whose
      `Stage.unchunk` is widened into the Async row by `!.widen`) equal
      or faster — measured per lane on a quiet box, with its known
      ±4x variance (merge-lane-variance) stated beside the number.

### Coherence (row-coercion-coherence-law, 2026-09-25)

The meaning of a program must not depend on HOW it reached a wider row:
coherence of effect subtyping (Biernacki & Polesiuk, "Logical relations
for coherence of effect subtyping", TLCA 2015 / LMCS 2018). Here a row
member is found by a runtime test on the operation VALUE (its class,
a `Tag`'s key, a `byValue` element class), so coherence holds exactly
when neither road changes an operation. The law says so as a tree
equivalence, with `Bisim.check` over sampled answers rather than one
run's result:

- [x] `Bisim.check(!.widen(p), !.normalize(p))` is `Same`, with ended
      paths, for a program of every core signature the oracle answers
      (State, Reader, Writer, Stop) and for mixed rows: State + Writer,
      two `Tag` keys over ONE signature, and a `Writer.byValue` pair.
- [x] and running agrees with the program before widening: the
      widened program under the wider row's handlers gives what the
      program gives under its own.
- [x] MUTANT: a `Row.into` that walks the tree and swaps the keys of two
      `Tag` instances of one signature (the row "reordered") is caught
      with a path. So is okay2's real incoherence, the intersection
      row's `#Op` taking the LAST parent's, if okay2 can state the law
      (see the twin below).
- [x] okay2 twin, if its intersection encoding can express it.
- RESULT (TestRowCoherence, both cores):
  - The law holds over State, Reader, Writer, Writer + Stop, the
    deferred shapes and a 200-deep fold (depth 1 000, about 800
    operations on one path), State + Writer, two `Tag` keys over one
    State, and a `Writer.byValue` pair. The widened programs also run as
    the originals did.
  - The mutant `into` (a walk swapping the keys `small` and `big`) was
    caught twice: `Differ at the start: left performed Tag(big,Get()),
    right performed Tag(small,Get())`, and the two-key run disagreed.
  - okay2 has ONE road: `Free` is contravariant in its row, so
    `!.widen` returns the program itself (`eq`), and there is no walk to
    compare it with. Its twin pins that, and that the program runs as
    it did. okay2's real incoherence (`#Op` of an intersection) is in
    typing an operation, not in widening.
  - A note for the next law: `Answers.+` takes its runtime test from
    its LEFT member, so a row of answers nests to the right,
    `A + (B + C)`; `(A + B) + C` asks for a `TypeableK` of a row.

## Out of scope

- `Writer.widen` (element-type widening) — a walk by necessity while
  `Free` is row-invariant, and the measured beneficiary; stays.
- Making `Free` row-covariant (free-row-variance, refuted 2026-09-03).
- Renaming the 71 call sites to `.plus[G]`: they are correct as
  written and `widen` now means what they meant.

## Design

The split is one rename and one redirection in `object !`:
`normalize` is the old body verbatim; `widen` is `Row.into(p)`.
`Row.coerce`'s comment ("NOT a replacement for `!.widen`: on a
streaming path the walk is also a normalisation") is corrected to
name `Writer.widen` as the walk that pays and `!.normalize` as the
one that remains.

## Decisions

- **The coercion is `widen`, not a new name** — 71 sites want it and
  are already spelled for it; a rename would touch 30 files to say
  what they said. The walk gets the new name because it is the
  minority and the one that needs its reason beside it.
- **The operator's question answered** ("is a row upcast ever
  legitimately a walk?"): no — an upcast is a coercion; a walk is a
  NORMALISATION, worth doing only where a rotation would otherwise be
  paid inside a contended region (`Source.merge`'s `Writer.widen`),
  and there it is a different operation with a different name.

## Results

2026-09-23. `TestWidenSplit` (core, 2): the two names agree on six
tree shapes including a 200-deep chain, and `widen` enters a deferred
head 0 times at widen, 1 after one run, 2 after two — the rerun law
by construction. `TestWidenDelay`, `TestPipe`, `TestStreamSource`,
`TestGather` (okay-java), `TestTransducers` (okay-clojure) — the
suites that pinned the deferred-head fix — unchanged, green.

`compare/WidenBenchmark`, `through(Source.range 10k)(stage)` with a
pure doubling stage joined to the Async row, measured on a box
running sibling gates all day: three ALTERNATED rounds, `-f 1`, the
instantaneous CPU recorded before and after every run, the rounds a
sibling matrix hit (cpu% > 1300) discarded on the record
(history `ws-*`):

| lane | round 2 (quiet) | round 3 (quiet) | B/op |
|---|---|---|---|
| the stage at the row directly (floor) | 289.2 (min) | 307.8 ± 87, min 289.5 | 4 075 475 |
| `!.widen` — the coercion | 290.6 ± 2.0 | 288.3 ± 10.9 | 4 075 481 |
| `!.normalize` — the walk | 413.3 ± 6.7 | 415.7 ± 7.6 | 5 195 556 |

The walk cost a widened stage **+43%** in time and **+112 B per
element** — an `Inject` and a `flatMap` closure per operation,
rebuilt as the stage ran; the coercion is the floor to the microsecond
and to ±200 bytes in four megabytes. The count owed: one node
rebuilt per operation per pull, zero now.

The merge lanes: `okaySourceMerge` never used `!.widen` (its walk is
`Writer.widen`, kept). The chunked merge, whose `Stage.unchunk` IS
joined to the row by `!.widen`, read 203.7 ± 4.0 and 209.3 ± 29.3
(minima 202.6 / 202.2) at k=16 against the 2026-09-20 record 224.6 —
about 9% faster; merge-lane-variance says this lane swings, so the
size carries that caveat and the sign rests on two agreeing minima
that both beat the record.

Operator's question answered: a row upcast is never legitimately a
walk. The walk is a normalisation, worth its cost only where a
rotation would otherwise be paid inside a contended region — and
there it is `Writer.widen`, a different operation over the element
type, under its own name.
