# widen-split — `!.widen` is a coercion; the walk is `!.normalize`

## Overview

`!.widen[A, F, G](p: A ! F): A ! (F + G)` carries two jobs under one
name. Its TYPE says "the same program in a wider row" — which
`RowLift.plus`/`.at`/`.up` already do for free: one commented
coercion (`RowLift.coerce`), nothing forced, nothing walked. Its BODY
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
  `RowLift.into` (the one cast, sound by the erasure argument
  RowLift.scala states). Signature unchanged: every call site
  compiles as it is.
- `!.normalize[A, F, G](p: A ! F): A ! (F + G)` — the walk `widen`
  used to be, under the name of what it does: resume the head,
  rebuild the tree node by node, deferred shapes rebuilt as deferred.
  For a caller who wants the rotation done up front — none is known;
  `Source.merge` keeps `Writer.widen` for the element type, which is a
  different function and stays.
- `Writer.widen` — unchanged.

## Behavior

- [ ] every `!.widen` call site compiles unchanged, and the full
      family is green: the coercion produces the same told elements
      and the same answer as the walk on every shape (`Pure`, `Inject`,
      `Bind`, `Delay`, `Bind(Delay, f)`), asserted by a law over the
      two names on one program.
- [ ] `!.widen` forces nothing: a program under `Free.delay` whose
      thunk counts is widened with the count at 0, and the count is 1
      after ONE run and 2 after two — the rerun law the eager head
      broke, now true by construction and pinned in core rather than
      only in okay-clojure/okay-java's stages.
- [ ] `!.normalize` agrees with `!.widen` on every shape (the law
      above, both directions) and is what the old `widen` was, so the
      stage suites that pinned the deferred-head fix stay green
      through it too.
- [ ] NUMBER OWED, a `through` over a widened pure stage (TestPipe's
      shape, 10 000 elements): `!.widen` (coercion) against
      `!.normalize` (the walk) against the stage at the row directly
      (the floor) — the coercion at the floor, the walk above it, with
      the count of nodes rebuilt per pull stated.
- [ ] NUMBER OWED, the merge lanes: `MergeBenchmark.okaySourceMerge`
      unchanged (it never used `!.widen`; `Writer.widen` stays), and the
      chunked merge (`ChunkFlushBenchmark.okayChunked`, whose
      `Stage.unchunk` is widened into the Async row by `!.widen`) equal
      or faster — measured per lane on a quiet box, with its known
      ±4x variance (merge-lane-variance) stated beside the number.

## Out of scope

- `Writer.widen` (element-type widening) — a walk by necessity while
  `Free` is row-invariant, and the measured beneficiary; stays.
- Making `Free` row-covariant (free-row-variance, refuted 2026-09-03).
- Renaming the 71 call sites to `.plus[G]`: they are correct as
  written and `widen` now means what they meant.

## Design

The split is one rename and one redirection in `object !`:
`normalize` is the old body verbatim; `widen` is `RowLift.into(p)`.
`RowLift.coerce`'s comment ("NOT a replacement for `!.widen`: on a
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

(after the measurement)
