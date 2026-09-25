# java-gatherers — JDK Stream Gatherers in okay-java

## Overview

JDK 24 made `java.util.stream.Gatherer` final (JEP 485, preview in
22/23 as JEP 461/473): a user-defined INTERMEDIATE operation,
`stream.gather(g)`. Four parts — an initializer (state), an integrator
`(state, element, downstream) -> boolean`, an optional combiner, a
finisher `(state, downstream)` — and the integrator may push zero, one
or many elements downstream and answer `false` to end the stream
early. It is what `Collector` is for the terminal operation, one step
earlier in the pipeline.

okay already has that shape, and names it: `Stage[I, O, A]`
(okay-stream, `A ! (Take % I + Writer % O)`) is a transducer as a
program — it awaits `I`, tells `O`, and ends when it has answered.
`Collect` says an `Aggregator` IS a `Collector`; this says a `Stage` IS
a `Gatherer`, both ways, and gives `Windowed` the operator it was
missing.

## The build: sbt compiles on JDK 25 (operator, 2026-09-23)

`Gatherer` exists in no JDK 21 class library, and dotc sees only the
library of the JVM it runs in: on 21, `import java.util.stream.Gatherer`
is E008; `-release 25` is "not a valid choice"; `-system` is not a
Scala 3 option (probe 2026-09-23, all three). So the ambient JDK in
`.sdkmanrc` moves 21 -> 25, and what compiling on 21 used to guarantee
BY ACCIDENT is now asked for by flag:

- dotc on 25 emits JDK 17 bytecode (major 61) by default — the same as
  on 21, measured — so no module's bytecode moves.
- the API guard moves: on 21 nothing could call a 22+ API; on 25
  anything could, and tests run on 26, so nothing would notice.
  `-java-output-version N` restores it — but it sets the API check AND
  the bytecode version together (the `-Xunchecked-` variant is
  overridden by it; measured), so N is per module: **17** by default
  (the bytecode every module already emits), **21** only where a module
  really calls a 21 API (the compiler names them), and **none** for
  okay-java, which needs 24+ API at bytecode 61.
- okay-java still LOADS on 17/21: the JVM links `Gatherer` lazily, so a
  class that names it fails with `NoClassDefFoundError` only when
  called (probe: `G.windowed` on 21). Everything else in okay-java is
  untouched by it.

## Behavior

- [x] `.sdkmanrc` pins 25; `gate.sh` picks it up (it exports
      `JAVA_HOME` from `.sdkmanrc`); the full matrix is green on it
- [x] every Scala 3 module carries `-java-output-version` 17 or 21;
      okay-java carries none; a module on 17 that reaches a 21 API
      fails to COMPILE (that is how the 21 list is made, not by hand)
- [x] `Gather.gatherer(stage)`: a `Stage[I, O, ?]` as a
      `Gatherer[I, ?, O]` — each element resumes the stage with
      `Some(i)`, each tell is a `downstream.push`, the finisher resumes
      with `None` until the stage answers
- [x] a stage that ANSWERS (transduceUntil's `Right`, a take-n)
      short-circuits: the integrator returns `false` and the upstream
      is not pulled again — 3 elements pulled of a 1000-element stream
      (bounded on purpose: see Results, "Mutants")
- [x] a downstream that rejects (`limit`, `findFirst`) stops the stage:
      the integrator returns `false` after a refused push — and the
      stage's continuation past the refused tell is never run
      (1000 tells per element under `limit(3)`: at most 4 made)
- [x] a pipeline BUILT by `through` over `Gather.stage` runs twice,
      the same windows; a continuation from inside a run resumed after
      it finished is refused by name (was: "runs once and refuses a
      second run" — see Results, the door was `through`'s)
- [x] sequential by construction: no combiner, so a `.parallel()`
      stream evaluates the gatherer in encounter order (JEP 485's
      rule for a combiner-less gatherer), never on a split
- [x] a stage's output before its first await (a header) is emitted
      at the first element, or by the finisher on an empty stream
- [x] `Gather.stage(gatherer)`: a JDK `Gatherer` as a `Stage` — the
      JDK's own `Gatherers.windowFixed/windowSliding/scan/fold` run
      inside okay pipelines; an integrator's `false` stops the stage
      awaiting; the finisher's pushes are told
- [x] law, both directions: over the same input,
      `stream.gather(gatherer(s))` and okay's own run of `s` agree —
      `id`, `chunked`, `mapAccumulate`, a `transduceUntil` take-n, a
      stage that emits before awaiting
- [x] `Windowed.gatherer`: an event-time window as a Gatherer that
      EMITS each pane the moment the watermark closes it (the
      `Collector` can only hand them over at the end); the remainder
      is flushed by the finisher; its state is the live panes only
- [x] docs: docs/modules/okay-java.md, the guide paragraph with
      examples and the literature; every snippet in a gated test

## Decisions

- **A Stage, not an Aggregator, is the Gatherer's counterpart.** A
  Gatherer's defining powers are 1:N emission and early termination;
  an `Aggregator` has neither, a `Stage` has both natively (tell,
  answer). `Gatherers.fold` is the degenerate case and needs no bridge.
- **No combiner, ever, from a Stage.** A stage's state is a suspended
  program at a position in the stream; two such states cannot be
  merged, for the reason `Windowed`'s collector records (a split holds
  a range, not a prefix). Where a Collector's combiner had to THROW to
  be honest, a Gatherer simply omits it and the JDK serialises — the
  better-fitting contract.
- **Compile on 25, not a side script.** `scripts/build-mrjar-jdk25.sh`
  compiled ScopedValue outside sbt, and its own spec recorded the
  price: the gate never exercised that path (the script is gone since
  mrjar-jdk25-ci-gap, 2026-09-25: the variant is an sbt project
  now). Gatherers are new API,
  not an alternative implementation, so the precedent's shape does
  not even fit; and the operator chose the build change over it.
  Rejected also: stub `java.util.stream.Gatherer` classes on the
  compile classpath (a split package with java.base, signatures kept
  in sync by hand).

## Results

Landed 2026-09-23 (java-gatherers).

**The build.** sbt on 25; `-java-output-version 17` on every Scala 3
module; the compiler, not a survey, named the exceptions — one full
`Test/compile` with 17 everywhere failed in exactly three main-code
modules: `okay-platform` and `okay-http` (Loom behind
`Schedulers.hasVirtualThreads`; bytecode 61 is what lets the guard
work, so `jdkFloor(0)`), `compare` (21 API unconditionally,
`jdkFloor(21)`). Test code failed in forty-odd files (virtual
threads, `Thread.ofPlatform`); it is unflagged by an AutoPlugin
(project/JdkFloor.scala) rather than floored at 21, because floor 21
would make test classes major 65 and blind `verifyJdk17`. One test
(`TestUidConcurrent`) used `ofPlatform` for no reason of its own and
was made 17-clean instead. `javap` on the built classes: 61 everywhere
(`Free`, `Schedulers`, `Docs`, `Gather`, a test class), 65 in
`compare` only — the bytecode did not move.

**What the move broke, at once:** `okay-delta` pinned its tests to the
AMBIENT JDK (`Test / javaHome := None`) because delta-kernel's Hadoop
fails on 24+; the ambient became 25 and `TestDelta` failed 4/4 with
`getSubject is not supported`. Pinned to a named JDK 21 now
(`jdk21Home`), green.

**Mutants.** Four, each failing exactly its own test: the integrator
never answering `false` (short-circuit), a refused push ignored,
`Gather.stage` ignoring the integrator's `false`, and the re-run
check removed. The first cut of the short-circuit test used an
INFINITE stream, and its mutant HUNG the suite instead of failing it
— bounded to 1000 elements, it fails with `pulled = 1000`.

**Found by the doc test: a built pipeline WAS one-shot — and the
door was `through`'s, not the gatherer's.** `through(p)(stage)` drove
the stage eagerly to its first output, so the program it answered
already held that run's state. Running the same built value twice fed
the JDK's `windowFixed` its spent state — an NPE inside the JDK (its
finisher nulls the array) — and this lane refused the second run by
name. Counting the doors: `Stage.chunked` survived the same re-run
(its state resets after each emission); okay-stream's `Windows.stage`
SILENTLY DROPPED A PANE on it — filed as
`okay-core/windows-stage-rerun-loses-pane` and fixed the same day
where all the doors are: `through` and the effectful `pipe` now answer
`Free.delay(() => loop(...))`, so the drive starts when the program
runs and each run makes its own state (specs/stage-pipeline.md). The
refusal stays for the case that remains — a continuation from INSIDE
a run resumed after that run finished, which a multi-shot Free
continuation allows and an opaque JDK state cannot survive.
`Windowed.gatherer` was never affected: the JDK path starts the stage
program per evaluation.

**Refuted alternative, measured:** `-Xunchecked-java-output-version`
to keep bytecode 61 under an API floor of 21 — dotc overrides it with
`-java-output-version` when both are given ("The value of
-Xunchecked-java-output-version was overridden").
