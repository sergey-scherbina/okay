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

- [ ] A program is the same as itself, and `Same` counts the paths
      walked (3 for one `get` over three samples).
- [ ] A difference on one answer is found, and the path to it is
      printed (`Get() -> 2`).
- [ ] The sample decides: the same pair passes when the separating
      answer is not in it (the reason `Same` is evidence).
- [ ] Returning and performing are told apart, and so are two values.
- [ ] A program that never ends is CUT, counted, and never reported as
      paths that ended.
- [ ] Scope: `get; get` and `get` differ freely and agree after
      `State.handle`. A rewrite that holds only by an effect's
      equations is checked after that effect's handler.
- [ ] Gen stage laws hold on the materialised programs: map.map,
      filter.filter, drop.drop, map-then-filter, flatMap emit, and
      take.take.
- [ ] A misstated law (filter and map swapped) is refused, with the
      first differing operation named.
- [ ] A mutant in the library's own stage code is caught (see Results).

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
