# handler-equivalence-oracle — are these two programs the same program?

## Overview

The fusion and staging lanes (handler-fusion, gen-chain-fusion,
optics-fuse, `Direct.staged`, the stagers) rewrite programs and show
the rewrite is correct with EXAMPLES: this input, this answer. The
semantics literature has a stronger check that is also cheap to run
here. Normal-form bisimilarity (Biernacki, Lenglet & Polesiuk, "A
complete normal-form bisimilarity for algebraic effects and handlers",
FSCD 2020) calls two programs equivalent when they reach the same
operation with equivalent continuations. A free tree already stops at
every operation, so that definition can be executed directly: walk
both programs in lockstep, demand the same operation, feed both
continuations the same answer, and go on. It is equivalence of
interaction trees (Xia et al., POPL 2020) with the silent steps
already removed by `Free.resume`.

The one change needed to make it executable: answers are a finite
SAMPLE per operation (`Answers[F]`), not a fresh symbol. So `Differ`
is a proof (a concrete path), and `Same` is evidence that reports its
own counts.

## Interface

```scala
// core, Bisim.scala
object Bisim:
  trait Answers[F[+_]]:
    def apply[X](op: F[X]): List[X]                       // a FUNCTION of the operation
    infix def +[G[+_]](g: Answers[G])(using TypeableK[F]): Answers[F + G]
  object Answers:
    def state[S](samples: S*): Answers[State % S]
    def reader[R](samples: R*): Answers[Reader % R]
    def writer[W]: Answers[Writer % W]
    val stop: Answers[Stop]                                // a leaf: Nil
  enum Verdict:
    case Same(paths: Int, cut: Int)
    case Differ(path: List[String], left: String, right: String)
  def check[F[+_], A](p: A ! F, q: A ! F, depth: Int = 32, budget: Int = 100_000)
                     (using Answers[F]): Verdict
```

## Behavior

- [x] A program is the same as itself, and `Same` counts the paths
      walked (3 for one `get` over three samples).
- [x] A difference on one answer is found, and the path to it is
      printed (`Get() -> 2`).
- [x] The sample decides: the same pair passes when the separating
      answer is not in it (the reason `Same` is evidence).
- [x] Returning and performing are told apart, and so are two values.
- [x] A program that never ends is CUT, counted, and never reported as
      paths that ended.
- [x] Scope: `get; get` and `get` differ freely and agree after
      `State.handle`. A rewrite that holds only by an effect's
      equations is checked after that effect's handler.
- [x] Gen stage laws hold on the materialised programs: map.map,
      filter.filter, drop.drop, map-then-filter, flatMap emit, and
      take.take. And MODELS: take, take 0, drop, takeWhile, filter, map,
      zipWithIndex against the same operation on a List.
- [x] A misstated law (filter and map swapped) is refused, with the
      first differing operation named.
- [x] A mutant in the library's own stage code is caught (see Results).

## Out of scope

- Operations that hold functions (`Delim.Shift`, `Push`) do not compare
  with `==`. A custom operation equality waits for a consumer.
- Symbolic answers. A sample is what makes the check executable.
- The FUSED readers (`Gen.toList` over `readSource`) are not trees and
  are not checked here. The materialised program is, and it is what
  `iterator`, `flatMap` and `++` run.

## Decisions

- NO CAST. The two programs' operations are compared with `==`, and each
  continuation is then answered from ITS OWN `Answers` call, with the two
  lists zipped. `Answers` must be a function of the operation. When it is
  not (the lists differ), that is reported as a `Differ`, not trusted.
- A core module, not a test fixture: users write handlers and rewrites
  too, and okay's `src/test/scala` is JVM-only.

## Results

MEASURED 2026-09-24 (TestBisim, 20 tests, okayJVM):

- The first run of the oracle's own controls caught an error in the
  TEST, not the oracle: "never ends" expected `Same(0, 1000)` from the
  budget, and the walk answered `Same(0, 243)`. That is 3^5: three
  answers per `get`, five deep, every path cut by the depth bound
  before the budget was reached. The test now pins 243 and checks the
  budget separately (depth 1000, budget 50).
- MUTANT 1, `dropping` keeps `n <= 0` as `n < 0` (drops one more):
  caught by the LAW drop.drop = drop(n + m), "Differ at the start:
  left performed Say(9), right performed Say(8)".
- MUTANT 2, `taking` with `n <= 0` as `n < 0`: GREEN at first, and
  the reason was not the one expected. The `n == 1` branch ends the
  walk before the `n <= 0` test is reached, so the mutant is
  EQUIVALENT for every n >= 1. Only `take(0)` tells it apart, and the
  model "take 0" now catches it. The first reading of this result
  ("a law misses a uniform mutant") was wrong and was corrected
  before it was written anywhere.
- MUTANT 3, `taking` counting down by two (`loop(n - 2)`): the LAW
  take(5).take(3) = take(3) stays GREEN (both sides keep 2 elements),
  and the MODEL "take" fails. This is the case that makes models
  necessary: a law relates a stage to itself, so an error shared by
  both sides cancels.
- Each mutant was applied to a committed tree and reverted with
  `git checkout`, and each red was read in the gate log.
