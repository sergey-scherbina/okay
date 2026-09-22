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

- [ ] `.sdkmanrc` pins 25; `gate.sh` picks it up (it exports
      `JAVA_HOME` from `.sdkmanrc`); the full matrix is green on it
- [ ] every Scala 3 module carries `-java-output-version` 17 or 21;
      okay-java carries none; a module on 17 that reaches a 21 API
      fails to COMPILE (that is how the 21 list is made, not by hand)
- [ ] `Gather.gatherer(stage)`: a `Stage[I, O, ?]` as a
      `Gatherer[I, ?, O]` — each element resumes the stage with
      `Some(i)`, each tell is a `downstream.push`, the finisher resumes
      with `None` until the stage answers
- [ ] a stage that ANSWERS (transduceUntil's `Right`, a take-n)
      short-circuits: the integrator returns `false` and the upstream
      is not pulled again — observable on an infinite `Stream.iterate`
- [ ] a downstream that rejects (`limit`, `findFirst`) stops the stage:
      the integrator returns `false` after a refused push
- [ ] sequential by construction: no combiner, so a `.parallel()`
      stream evaluates the gatherer in encounter order (JEP 485's
      rule for a combiner-less gatherer), never on a split
- [ ] a stage's output before its first await (a header) is emitted
      at the first element, or by the finisher on an empty stream
- [ ] `Gather.stage(gatherer)`: a JDK `Gatherer` as a `Stage` — the
      JDK's own `Gatherers.windowFixed/windowSliding/scan/fold` run
      inside okay pipelines; an integrator's `false` stops the stage
      awaiting; the finisher's pushes are told
- [ ] law, both directions: over the same input,
      `stream.gather(gatherer(s))` and okay's own run of `s` agree —
      `id`, `chunked`, `mapAccumulate`, a `transduceUntil` take-n, a
      stage that emits before awaiting
- [ ] `Windowed.gatherer`: an event-time window as a Gatherer that
      EMITS each pane the moment the watermark closes it (the
      `Collector` can only hand them over at the end); the remainder
      is flushed by the finisher; its state is the live panes only
- [ ] docs: docs/modules/okay-java.md, the guide paragraph with
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
  compiles ScopedValue outside sbt, and its own spec records the
  price: the gate never exercises that path. Gatherers are new API,
  not an alternative implementation, so the precedent's shape does
  not even fit; and the operator chose the build change over it.
  Rejected also: stub `java.util.stream.Gatherer` classes on the
  compile classpath (a split package with java.base, signatures kept
  in sync by hand).

## Results
