# okay from Scala 2.13 — a facade, not a cross-build

## Overview
The operator asked (2026-09-22) for okay to "work and compile under
both Scala 3.9 and Scala 2.13". This spec records why that is done
as a FACADE MODULE consumed through Scala 2.13's TASTy reader, and
not as a cross-build of the library's own sources, and what the
facade is.

A 2.13 user adds `okay-scala2`, compiles with `-Ytasty-reader`, and
writes ordinary 2.13 code — for-comprehensions, lambdas, pattern
matches — over types whose SIGNATURES a Scala 2 compiler can read.
The facade is written in Scala 3 and calls the real library; the
2.13 side never sees an effect row, an inline method or an opaque
type. It is `okay-java`'s idea one level up: that module makes the
library usable from Java, this one from Scala 2.

## Why not a cross-build (measured 2026-09-22)
- THE ROW IS A UNION. `F + G` is `[A] =>> F[A] | G[A]` (Effects.scala)
  and every program's type is `A ! Row`. Scala 2 has no union types,
  so the core's central type has no 2.13 spelling; a cross-build
  means a new row encoding (a coproduct with membership type
  classes), which is a rewrite of the core and would undo measured
  decisions (RowLift's zero-cost widening, row-membership-crash).
- `inline` IS THE HOT PATH. 182 `inline def`s in `src/main` alone,
  including `Free.flatMap`, `State.get`, `raise`, `async`. The
  performance arcs of the last month (direct-staged, fold-until,
  handler-fusion) are paid for by them.
- The rest compounds: 19 opaque types, ~23 files of `scala.quoted`
  macros, `given`/`using` in ~200 files, braceless syntax in ~277.
  Across `*/src/main`: 465 files, 93 461 lines.
- Consuming the Scala 3 artifacts DIRECTLY from 2.13 is refused at
  the first call: scalac 2.13.18 `-Ytasty-reader` reads 3.9 TASTy
  fine but stops at `Unsupported Scala 3 inline method flatMap;
  found in class okay.Free` (and the same for `State.get`).

## Stage 0 — the round trip, by hand (DONE 2026-09-22)
A 9-line Scala 3 class wrapping `A ! State % Int` behind non-inline
`map`/`flatMap`/`run`, compiled by 3.9.0 against the core; a 2.13
`for`-comprehension over it, compiled by scalac 2.13.18
`-Ytasty-reader`; run on the JVM. Printed `(42,84)`, the right answer.
- A `private val` of a union-row type inside the facade class does
  NOT stop the TASTy reader; only what 2.13 code touches must be
  readable.
- COMPILE classpath: the 2.13 stdlib must stand AHEAD of the 3.9 one.
  3.9's first → `Unsupported Scala 3 union in bounds of type T;
  found in method wrapRefArray in class scala.LowPriorityImplicits`
  (3.9's stdlib carries TASTy, not Scala 2 pickles — the okay-spark
  finding, from the other side). 3.9's absent → `could not find
  package scala.annotation.internal`.
- RUN classpath: 3.9's stdlib is REQUIRED (2.13's alone →
  `NoClassDefFoundError: scala/reflect/Enum`); with it present the
  order does not matter.
- So a 2.13 build that simply depends on okay gets a classpath
  NEITHER compiler nor runtime accepts until it arranges the two
  stdlibs, and that arrangement is part of this module's contract —
  stated in its docs and PROVED by a 2.13 project in this build.

## Stage 1 — `okay.scala2.Prog` and the 2.13 probe project
The one carrier every 2.13 caller needs first: a program over
`Async + Throws % Throwable` — suspended, failing, recoverable,
runnable — the IO-shaped subset of the library.

- `okay-scala2` (JVM, Scala 3.9): `Prog[A]` with `map`, `flatMap`,
  `recover`, `attempt`, `run()`, `runEither()`; `Prog.pure`,
  `Prog.delay`, `Prog.fail`, `Prog.fromEither`, `Prog.sequence`;
  and for Scala 3 callers the bridges `Prog.lift(A ! Async)` and
  `.program`. Nothing in its public API is inline, a union, opaque,
  a match type or an extension method.
- `okay-scala2-probe` (JVM, Scala 2.13.18, `-Ytasty-reader`,
  `-Werror`): munit suites written in Scala 2 against `okay-scala2`,
  compiled by the Scala 2 compiler in the ordinary gate. This is the
  test that the facade stays readable from 2.13 — a new method with
  a union in its signature breaks THIS project's compile, not a
  user's.
- The stdlib arrangement the probe needs is the one a user needs,
  and the docs give it verbatim.

## Behavior
- [ ] a 2.13 for-comprehension over `Prog` compiles and runs
- [ ] `Prog.delay` suspends: nothing runs until `run()`
- [ ] a thrown exception inside `delay` and a `Prog.fail` both reach
      `recover`/`attempt`/`runEither` as the same Throwable
- [ ] `sequence` over 10 000 progs is stack-safe from 2.13
- [ ] Scala 3 code can `lift` an `A ! Async` and read `.program` back
- [ ] the probe project compiles with `-Werror` under 2.13.18

## Later stages (not in stage 1)
- State / Reader / Writer carriers with a FIXED state type per
  carrier (`StateProg[S, A]`), since a 2.13 caller cannot name a row.
- Streams: a 2.13 `Source` facade over okay-stream.
- Fibers and channels over `Prog`.

## Decisions
- FACADE over cross-build (operator choice, 2026-09-22, after the
  measurements above).
- The facade is written in Scala 3, not 2.13: it must call inline
  methods and name rows, which only a Scala 3 compiler can.

## Results
