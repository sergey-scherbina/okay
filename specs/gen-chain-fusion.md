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
  // barriers: a new source from the materialised program, identity chain
  def flatMap[V](f: W => Gen[V]): Gen[V]; def ++(h: Gen[W]): Gen[W]; def zipWithIndex: Gen[(W, Int)]
  // readers: ONE walk of the source, the chain applied per element
  def foldUntil[S, R](using FoldUntil[W, S, R]): R; toList; toVector; first; find; exists; forall; foreach
  def iterator: Iterator[W]; def toLazyList: LazyList[W]       // over `program` (the Python stepper, unchanged)

object Gen:
  /** a source program and the stages to read it through */
  abstract class Chain[W]:
    type A; def source: Unit ! Row[A]; def xf: Xf[A, W]
  /** a stage, with its two readings */
  sealed trait Xf[A, B]:
    type St[S]                                                  // the state the stage adds around the reader's
    def fold[S, R](k: FoldUntil[B, S, R]): FoldUntil[A, St[S], R]   // fused: apply per element
    def walk(p: Unit ! Row[A]): Unit ! Row[B]                   // materialised: the walk it was
    def andThen[C](that: Xf[B, C]): Xf[A, C]
  object Xf: Id, Map(f), Filter(p), Take(n), TakeWhile(p), Drop(n), Compose(a, b)
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

## Out of scope

- Fusing `flatMap`/`zipWithIndex`/`++` — barriers; a `flatMap` fused
  into the reader needs the reader to read an inner generator from a
  running state (a `FoldUntil` with a start state), its own lane.
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
  runs nothing (Python's law), and a barrier (`flatMap`) takes that
  program as its source.

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
