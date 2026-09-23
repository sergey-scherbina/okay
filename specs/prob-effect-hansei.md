# prob-effect-hansei — probabilistic programming as an effect

## Overview

Kiselyov & Shan, "Embedded probabilistic programming" (DSL 2009, the
Hansei system) treat a probabilistic model as an ordinary program with
one extra operation — a weighted choice — and treat INFERENCE as a
HANDLER over it, the same "one program, several readings" shape this
library already gives `Choose`/nondeterminism (`Choice.scala`'s own
comment states it for that effect; this is its weighted twin). The
showcase reason it belongs here specifically: exact inference by
exhaustive enumeration is MULTI-SHOT — it must invoke the delimited
continuation once per alternative to see every branch — which is
exactly the shape `docs/benchmarks.md` already measured no one-shot
effect runtime (kyo, cats-effect's `IO`, ZIO) can host at all, not
merely run slower. Nothing of this existed here before this lane: no
module, no spec, no `dist`/`observe`.

## Interface

```scala
case class Dist[+A](choices: Seq[(A, Double)]) derives Effect

object Prob:
  inline def dist[A](choices: (A, Double)*): A ! Dist
  inline def uniform[A](as: A*): A ! Dist
  inline def observe(cond: Boolean): Unit ! Dist
  def runExact[A, F[+_]](p: A ! (Dist + F)): Map[A, Double] ! F
  extension [A](m: Map[A, Double]) def posterior: Map[A, Double]
  def sampleOnce[A](p: A ! Dist)(using scala.util.Random): Option[A]
  def runRejection[A](n: Int)(p: => A ! Dist)(using scala.util.Random): Map[A, Double]
```

`Dist` is its own signature, the same shape `Choose` uses (Choice.scala)
— one case class, its answer type IS the choice's element type, no
GADT refinement needed anywhere. `observe(false)` is `Dist(Seq.empty)`
— a weighted choice with NO alternatives, pruning the branch exactly
as `Choose(Seq.empty)` prunes a search. Everything but `Dist` itself
lives under `object Prob`, matching `Logic`'s own placement — see
Decisions for why `observe` in particular is never bare.

## Behavior

- [x] `runExact` on the textbook wet-grass Bayes net answers the exact
      rational posterior (15/29, 14/29), checked against hand
      arithmetic, not a remembered "well-known" approximate figure
- [x] the UNNORMALIZED joint sums to less than 1 when a branch was
      pruned by `observe` — the pruned mass (0.42 of 1.0 here) is gone,
      not redistributed silently
- [x] a small two-state HMM, two days both observed "Happy": `runExact`
      agrees with an independent, effect-free hand enumerator bit for
      bit (both walk the same four (s1, s2) branches; one via
      `Effects.handle`'s multi-shot capture, the other via four nested
      `for` loops)
- [x] `runRejection` (single-shot, comonadic — no capture at all) agrees
      with `runExact` within statistical tolerance on both models
- [x] `observe(false)` on every branch answers the empty map, not an
      exception and not a default
- [x] `uniform` weights every alternative equally
- [x] `runExact` forwards other effects (`Writer` alongside `Dist`):
      both branches' tells happen, because both branches ran
- [x] THE NUMBER: exact inference's per-branch price against a
      hand-written enumerator with no effect machinery at all — 9310
      ns/run against 1875 ns/run on the 4-branch HMM, ~5x (session
      measurement, JVM, warmed 2000 iterations first; not a JMH lane —
      see Out of scope)

## Out of scope

- **A formal JMH lane.** The number above is a same-process,
  order-of-magnitude check (`System.nanoTime` around 20 000 runs each,
  warmed first) — enough to show the price is bounded and real, not a
  published, gated benchmark under `docs/benchmarks.md`'s lane rules
  (a competitor comparison, three platforms, `src/jmh/history.tsv`).
  If a consumer wants the number load-bearing for a design decision,
  that is its own lane.
- **Importance sampling as a THIRD, distinct handler.** With `observe`
  boolean-only (hard evidence), importance sampling and rejection
  sampling coincide — both discard a run the instant a hard observation
  fails; the two diverge only with SOFT (weighted) evidence, which
  `observe` does not carry here. A `Dist`-shaped soft-evidence
  operation (`factor(weight: Double)`, Hansei's own term) is the
  natural next step if a consumer needs it — filed nowhere yet because
  none has asked.
- **`Once` for memoised sub-models** (Fischer, Kiselyov & Shan,
  "Purely functional lazy non-deterministic programming", ICFP 2009 —
  call-time choice): sharing a `Dist`-performing program VALUE across
  several uses already memoises it the way `Once` memoises anything
  else (once-across-fibres's own memory: share the value, not the
  effect) — no new mechanism, so no test was written FOR it
  specifically; a consumer who wants it writes `!.once(model)`.

## Decisions

- **Everything but `Dist` itself lives under `object Prob`** — unlike
  `Choice.scala`'s own split (`Choose`, `choose` and `runChoice` are
  ALL package-level there), because `Logic.observe(n: Int)` already
  names an unrelated operation, and a package-level `Prob.observe`'s
  namesake (`observe(cond: Boolean)`) would be silently SHADOWED the
  instant a file also writes `import Logic.*` — an explicit import
  outranks a same-package member, so the shadowing is silent rather
  than a name clash the compiler flags. Namespacing under `Prob`
  (`import Prob.*`, matching `Logic`'s own placement) sidesteps it
  outright, and keeps `posterior` — an extension on a bare `Map` —
  from contesting every `Map`'s own methods at package level the way
  `Comonad[Id]` once did (comonad-id-map-capture): one fewer thing to
  discover the same way twice.
- **`sampleOnce`/`runRejection` are a bespoke resume-loop, not
  `Effects.handle`** — the same reason `State.handle`/`Once.run` are:
  a program that may FAIL (an empty `Dist`) has nothing to answer the
  ordinary `Return` case with, and `Effects.handle`'s `ret: A =>
  Free[G, B]` argument has no slot for "there is no A". A resume loop
  answering `Option[A]` says exactly what happened.
- **In core (`src/main/scala/Prob.scala`), not a new module.** The
  backlog entry allowed either; the whole effect is ~90 lines with no
  platform-specific code (`scala.util.Random` is cross-platform), so a
  new crossProject with its own build.sbt entry, three platform
  source roots and a README would be ceremony without a reason.

## Results

Landed as written. TestProb: 10 tests, all green, including the
arithmetic checked against two independently-written oracles (the
wet-grass model's exact fractions, and the HMM's hand enumerator).
