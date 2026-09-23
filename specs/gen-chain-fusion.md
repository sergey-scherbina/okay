# gen-chain-fusion — a `Gen` chain read as one walk

## Overview

A `Gen` pipeline `g.map(f).filter(p).take(n).toList` (specs/generators.md)
is three element-wise WALKS, each a program that re-tells into the
next, and then a reader. generators-jmh priced it: the wrapper is free
(`iterator` = `Writer.run` to the byte) but `filter` as a splice cost
+109 B per element; gen-filter-as-walk took that to +82 and named what
is left — the walk's own `Bind` per kept element and `Delay` per
rejected one, against a hand road (`Writer.map` + a filtering fold,
272 B/elem) that has no program between the map and the read at all.
The staging survey (2026-09-22) ranked this third with the trigger
"a per-stage cost above noise"; both lanes met it. This spec fuses the
chain INTO THE READER: the stages become data — a typed
fold-transformer chain, Clojure's transducer \[Hickey 2014\] with the
state type carried — and a stopping reader walks the source once,
applying every stage per element inside `add`. Nothing is built
between stages; a `take` that is done stops the source where the
reader would have. `program` still materialises the chain as the
walks it was, for the roads that need a program (`iterator`,
`flatMap`, `++`, a `Gen` marked inside a `generator` block).

## Interface

```scala
// core, Gen.scala
final class Gen[W](val chain: Gen.Chain[W]) extends AnyVal:     // still a value class: one field
  def program: Unit ! Gen.Row[W]                                // the chain MATERIALISED (lazy)
  // element-wise: stages appended, nothing walked
  def map[V](f: W => V): Gen[V]; def filter(p: W => Boolean): Gen[W]; def withFilter(p): Gen[W]
  def take(n: Int): Gen[W]; def takeWhile(p: W => Boolean): Gen[W]; def drop(n: Int): Gen[W]
  // fused too (gen-flatmap-fusion): flatMap and zipWithIndex are stages, ++ is a Cat node
  def flatMap[V](f: W => Gen[V]): Gen[V]; def ++(h: Gen[W]): Gen[W]; def zipWithIndex: Gen[(W, Int)]
  // readers: ONE walk of the source, the chain applied per element
  def foldUntil[S, R](using FoldUntil[W, S, R]): R; toList; toVector; first; find; exists; forall; foreach
  def iterator: Iterator[W]; def toLazyList: LazyList[W]       // over `program` (the Python stepper, unchanged)

object Gen:
  final class Halt[S](val s: S, val stopped: Boolean)          // where a read ended, and whether a Stop did
  /** a source and the stages to read it through; read FROM a downstream state */
  abstract class Chain[W]:
    type A; val xf: Xf[A, W]
    def readSource[S, R](K: FoldUntil[A, S, R])(s0: S): Halt[S]        // the raw source(s)
    final def readState[S, R](K: FoldUntil[W, S, R])(s0: S): Halt[S]   // the whole chain, stages' state wrapped/unwrapped
    def program: Unit ! Row[W]
  object Chain: Plain(source), Staged(source, xf), Cat(l, r, xf)      // Cat: one after the other, xf AFTER both
  /** a stage, with its two readings */
  sealed trait Xf[A, B]:
    type St[S]                                                  // the state the stage adds around the reader's
    def fold[S, R](k: FoldUntil[B, S, R]): FoldUntil[A, St[S], R]   // fused: apply per element
    def inject[S](s: S): St[S]; def project[S](st: St[S]): S     // wrap/unwrap, so a chain reads from a state
    def walk(p: Unit ! Row[A]): Unit ! Row[B]                   // materialised: the walk it was
    def andThen[C](that: Xf[B, C]): Xf[A, C]
  object Xf: Id, Map(f), Filter(p), Take(n), TakeWhile(p), Drop(n), FlatMap(f), Indexed, Compose(a, b)
```

## Behavior

- [x] Every law of `TestGen` and `TestGenerator` unchanged — the
      laziness counter through `map`/`filter`/`take`/`first`/`find`/
      `exists`/`iterator`, the three endings, the nested
      for-comprehension, non-memoising, 100 000 deep.
- [x] FUSED = MATERIALISED: for generated chains of `map`/`filter`/
      `take`/`takeWhile`/`drop` over `unfold`, `g.toList` (fused) equals
      `Gen.fromProgram(g.program).toList` (the walks) and
      `g.iterator.toList` (the stepper over the walks).
- [x] A fused `take(n)` runs the body EXACTLY to its n-th kept
      element (the counter), and a fused `find` stops at the element
      found — through a `map` and a `filter` before it.
- [x] A `Stop` in the source ends a fused read as it ends a walk;
      `drop` past the end answers empty; `take(0)` runs nothing.
- [x] MEASURED (compare `GenBenchmark`, quiet alternated pairs,
      `-prof gc`): `genPipelineToList` against the hand road
      `writerPipelineCollect` (272 B/elem) — the fused chain at or
      under it; `genTakeToList` before/after; `genUnfoldToList`
      unchanged (an identity chain adds nothing). Rows `gcf-*`.
- [x] Docs: direct-style "What it costs", the spec's Results, theory
      ch. 7 one sentence (a transducer with its state type carried).
- [x] FUSED BARRIERS (gen-flatmap-fusion, operator 2026-09-23): with
      `flatMap`, `++` and `zipWithIndex` in generated chains, fused =
      materialised = stepper; a `take` over `++` counts through and each
      side's own `take` counts its own; indices continue across `++`;
      the right side never runs when the left is enough (counter); a
      fused `flatMap` is lazy to the inner counter and an inner `Stop`
      ends the whole generation (the spliced program's law).
- [x] MEASURED: `genFlatMapToList`, `genConcatToList`,
      `genZipWithIndexSum` before/after on quiet alternated pairs; the
      pipeline and identity lanes unmoved. Rows `gfm-*`.

## Out of scope

- Fusing into `iterator`: the Python stepper pulls one element and
  holds a continuation; a stateful stage would need its state kept
  across pulls beside the reader's. `iterator` reads `program`.
- The direct macro: a `Gen` marked in a block reads `program`, as
  before.

## Design

- **A transducer with its state type as a type member.** A stateless
  stage (`Map`, `Filter`) has `St[S] = S`; `Take`/`Drop`/`TakeWhile`
  carry `Counted[S]` — a count beside the reader's state as a CLASS,
  not a `(Int, S)` tuple, which boxed the count (Results); `Compose`
  nests `a.St[b.St[S]]`. So the fused reader is a `FoldUntil` at a KNOWN
  state type and `Gen.read` needs no change — `done` before `k`, the
  law fold-until gave it, now decides for the whole chain.
- **`Chain[W]` with `type A`** — the source's element type is an
  existential the value class cannot name; a type member names it
  once, and every reader is `Gen.read(chain.source)(chain.xf.fold(K))`.
- **`program` is a method of `Chain`**: a plain chain answers its
  source with no node (the walks say `say(w)`, never `emit(w).program`
  — a wrapper per element on a hot path, Results); a staged chain
  materialises its walks under a `Free.delay`, so construction still
  runs nothing (Python's law).
- **Reads are FROM a state (gen-flatmap-fusion).** `readState(K)(s0)`
  answers a `Halt`: the reader's state where the walk ended and
  whether a `Stop` ended it. A `Cat` reads its left side, then — unless
  it stopped or the reader is done — its right side from that state;
  a `FlatMap` stage reads the inner generator's chain from the
  reader's state inside `add` and carries the inner's `Stop` as its
  own `done`, so the whole generation ends as the spliced program's
  would. Each `Xf` wraps (`inject`) and unwraps (`project`) the state
  it adds, so a side's or an inner chain's own stages start fresh —
  `(g.take(2) ++ h.take(3))` counts each side, `(g ++ h).take(3)`
  counts through, because the `take` sits on the `Cat` and applies
  after both.

## Decisions

- **Fuse into the reader, not into a smarter walk** — a walk is a
  program between stages and pays a node per element by construction;
  the hand road's number says the reader is where the filter lives.
- **Keep `AnyVal`** — one field, `Chain[W]`; a stage appended is one
  small object at construction, never per element.
- **`iterator` over `program`** — chosen because the stepper's
  contract (`next()` runs to the next tell and HOLDS the continuation)
  is the Python law itself and is measured at parity; fusing it would
  re-derive the stepper for every stateful stage.

## Results

**2026-09-23** (compare `GenBenchmark`, 10 000 Longs, per-arm minima of
two QUIET alternated pairs, load 1.7–3.6, `-prof gc`; history `gcf-*`):

| lane | before (walks) | fused | hand road |
|---|---|---|---|
| `genPipelineToList` — `map(_ * 2).filter(_ % 3 == 0).toList` | 281.8 µs / 354 B/elem | **201.8 / 231** | `writerPipelineCollect` 215.2 / 272 |
| `genTakeToList` — infinite `unfold`, `take(10k).toList` | 246.0 / 295 | **188.9 / 239** | — |
| `genUnfoldToList` — the identity chain | 218.4 / 239 | 217.2 / 239 | `writerUnfoldFoldUntil` 158.4 / 215 |

The pipeline: 0.72 of the walks in time, 0.65 in bytes — and UNDER
the hand road (0.94 / 0.85), because the hand road still walks
`Writer.map` and the fused chain walks nothing: the source is read
once and `f`, `p` run inside `add`. `take` fused: 0.77 / 0.81, and
its bytes equal the plain read's to the byte — the count lives in a
`Counted` beside the reader's state, one small object the JIT
scalar-replaces. The identity chain: bytes identical, time within
noise. What the first cut got wrong, twice, in the order the numbers
said: (1) `program` as a def that always delays put a `Gen`, a
`Chain`, an `Id` and a `Delay` per element under every walk's
`emit(w).program` — the identity lane read +96 B/elem — so `program`
is a method of `Chain` (a plain chain answers its source, no node) and
the walks say `say(w)`; (2) `(Int, S)` as `take`'s state boxed the
count — +40 B/elem, `genTakeToList` 1.19x WORSE than the walk — so
the stateful stages carry a `Counted[S]` class. Laws: `TestGen` 13/13
(fused = materialised = stepper on 200 random chains; the counter
through `map.filter.take(2)` at four steps; `find` through `map`;
`take(0)` runs nothing; a `Stop` ends a fused read), `TestGenerator`
9/9, `TestDocExamplesGen` 2/2.

**gen-flatmap-fusion, 2026-09-23** (the same benchmark, three lanes
added; `gfm-*`). The box carried sibling gates at load 85–200 for
hours; the first alternated pair ran with the load rising through it,
so its TIMES are not reported — its BYTES are load-proof (they held to
the byte across every contaminated round of every lane this day):

| lane | before (walks) B/elem | fused B/elem | ratio |
|---|---|---|---|
| `genConcatToList` — `(gen ++ gen).toList`, 20k | 287 | **239** | 0.83 — the plain read's bytes exactly |
| `genFlatMapToList` — `flatMap(i => Gen.emit(i).map(_ + 1))` | 495 | **407** | 0.82 — what is left is building the inner `Gen` per element, the API's own |
| `genZipWithIndexSum` — `zipWithIndex.foreach` | 311 | **215** | 0.69 — the `Counted` scalar-replaced |
| `genPipelineToList`, `genUnfoldToList` | 231 / 239 | 231 / 239 | unmoved |

Times, from the quiet rounds that came four hours later (after ×2,
before ×1 at load 3.4–3.9; a fourth round hit a load of 120 and is
discarded), per-arm minima: `genConcatToList` 550.2 → **403.4 µs**
(0.73), `genFlatMapToList` 484.3 → **363.3** (0.75; the hand road
`writerFlatMapCollect` 271.5), `genZipWithIndexSum` 248.7 → **173.0**
(0.70). The pipeline (213.5 vs 201.9) and the identity chain (232.0
vs 196.5) moved within the spread quiet rounds show on those lanes
(196–232 for the identity lane across the day), with bytes identical —
unmoved.
