# strymonas-zip-fusion — `Gen.zip`, without materializing either side

## Overview

Kiselyov, Biboudis, Palladinos & Smaragdakis, "Stream fusion, to
completeness" (POPL 2017; the strymonas library the paper and its
JFP 2022 successor build) name `zip` as the construction where naive
fold-based fusion breaks: a fold consumes ONE source to completion
before answering, but `zip` needs BOTH sources advanced ONE STEP AT A
TIME, in lockstep — and the hardest instance of that is zipping a
STREAM BUILT BY `flatMap` (several elements per outer step) without
first materializing it into a list.

`Gen`'s existing fusion (specs/gen-chain-fusion.md) is fold-based:
every stage (`map`, `filter`, `flatMap` itself) is an `Xf`, composed
via `FoldUntil`, and a `flatMap`'s OWN fused reading works by reading
its inner generator TO COMPLETION inside the outer fold's `add` step
— exactly the shape that cannot express `zip`, which needs to STOP
mid-inner-generator to hand control to the OTHER side. `zip` is
therefore not an `Xf` stage; it is a new, PROGRAM-level walk, built
directly on `.resume` — and the finding this spec's Results section
answers is whether that walk reaches through a `flatMap`-fused
`.program` with no special case, which the paper's own hard case asks
for and this library's `.resume` already answers by construction
(Decisions).

## Interface

```scala
final class Gen[W]:
  def zip[V](other: Gen[V]): Gen[(W, V)]
  def zipWith[V, U](other: Gen[V])(f: (W, V) => U): Gen[U]

object Gen:
  private def pull[A](x: Unit ! Row[A]): Option[(A, Unit ! Row[A])]
  private[okay] def zipping[A, B](pa: Unit ! Row[A], pb: Unit ! Row[B]): Unit ! Row[(A, B)]
```

`pull` resumes a program ONE step: the next told value and the rest,
or `None` at the end (`Return` or `Stop`) — the same shape `Stepper.
advance` already holds in a mutable field, as a pure function instead.
`zipping` pulls one element from each side and pairs them, ending the
moment either side does; `zip`/`zipWith` build it from both sides'
`.program` and wrap the result with `Gen.fromProgram`, so anything
chained AFTER `zip` (`.map`, `.filter`, another `.zip`) gets the
ordinary `Xf` fusion any other `Gen` source has.

## Behavior

- [x] equal-length sources pair up in order; either side ending first
      ends the zip, including an empty side
- [x] `zipWith` combines each pair in the same pass
- [x] `zip` composes with further `Xf` stages afterward (`.map`,
      `.filter`) — the ordinary fusion, unaffected by how the source
      was built
- [x] `zip` after `++` (`Chain.Cat`) on either side
- [x] THE HARD CASE: zipping a `flatMap`-fused generator (several
      elements per outer step) against a plain one, both orders
      (flatMap on the left, then on the right) — correct, element for
      element
- [x] THE HARD CASE IS LAZY, finite witness: a `Counted` source
      wrapped in `flatMap` (2 inner elements per outer step), zipped
      down to 3 elements, ran the OUTER source exactly 2 steps — not
      the whole (in this case, 4-step) source, MEASURED not assumed
      (the first draft guessed the number before running the test,
      and it happened to be right, but the test asserts what the run
      produced, not the guess)
- [x] THE HARD CASE IS LAZY, infinite witness: an INFINITE `flatMap`-
      fused generator (`Gen.unfold` with no end, `flatMap`ped 2-per-
      step), zipped against a 5-element finite source, TERMINATES
      with the correct 5 pairs — the strongest possible proof, since
      full materialization of the left side is not merely undesired
      but impossible

## Out of scope

- **A stage-level (`Xf`) fusion of `zip` itself.** `Xf`/`FoldUntil`
  fundamentally folds ONE source; `zip` needs two sources advanced
  independently, which is why it is a program-level `pull`/`zipping`
  walk rather than another `Xf` case. This is not a gap to close —
  it is the same shape strymonas's own paper describes zip as needing
  (a genuinely different construction from ordinary fusion, not a
  variant of it).
- **`zip3` and beyond.** `zipWith`/`zip` generalize to N sources the
  same way (`pull` on each, stop at the first `None`); not built until
  a caller asks for three.
- **Interleaving strategies other than lockstep** (round-robin merge,
  the union of two independently-progressing generators) — a
  different operator (`merge`, not `zip`), not attempted here.

## Decisions

- **`zip` is built on `pull`, a NEW single-step primitive, not on
  `readState`'s `FoldUntil` machinery.** `readState` folds a WHOLE
  source to `done`; zip needs exactly one element and the REST as a
  program value, so that the other side can be pulled in between —
  a shape `FoldUntil` has no way to express (its `add` never returns
  early with "and here is what's left").
- **`pull`'s uniformity over `.resume` is what makes the hard case
  work, and it needed no special-casing for `flatMap`.** `Free.
  resume` already normalizes through however many `Delay`/`Bind`
  nodes a `splice` (flatMap's own walk) or a `Cat` needed to reach
  the next real tell; `pull` calls `.resume` once and trusts that
  normalization completely, the same way `readState`'s own tailrec
  loop and `Stepper.advance` already do. The "fusion" in this lane is
  therefore inherited from `Free`'s existing resume contract, not a
  new mechanism built for `zip` specifically.
- **The recursive step sits inside a `flatMap`, not a direct call.**
  `zipping`'s `loop` recurses via `say((a, b)).flatMap(_ =>
  loop(xRest, yRest))` — a `Bind` node returned as data, not a call
  made at construction time — so the recursion's stack cost lands on
  whichever driver eventually resumes the zip's own program (an
  ordinary `Gen` reader), never on `zip`'s own construction.

## Results

Landed and tested (TestGenZip, 9 tests, all green on the first real
run after two small syntax/typo fixes). The laziness numbers were
verified by running rather than assumed: the finite witness's outer
step count (2, to produce 3 zipped pairs from a flatMap yielding 2
per step) matched a hand prediction, and the infinite-source test is
the stronger, prediction-free proof — it could not have passed at all
under eager materialization.
