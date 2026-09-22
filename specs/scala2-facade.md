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
- A `private val` of the row `State % Int` inside the facade class
  did NOT stop the TASTy reader. That row has no `+`; stage 1 found
  that a constructor naming `+` does (Results).
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
  and for Scala 3 callers `Bridge.lift(A ! Async)` and
  `Bridge.program(prog)`, in their own object so 2.13 code never
  loads them. Nothing in its public API is inline, a union, opaque,
  a match type or an extension method.
- `okay-scala2-probe` (JVM, Scala 2.13.18, `-Ytasty-reader`,
  `-Werror`): munit suites written in Scala 2 against `okay-scala2`,
  compiled by the Scala 2 compiler in the ordinary gate. This is the
  test that the facade stays readable from 2.13. A class the reader
  refuses breaks THIS project's compile, not a user's. The reader
  reads a method only when something calls it, so the probe calls
  EVERY public method of the facade.
- The stdlib arrangement the probe needs is the one a user needs,
  and the docs give it verbatim.

## Behavior
- [x] a 2.13 for-comprehension over `Prog` compiles and runs
- [x] `Prog.delay` suspends: nothing runs until `run()`
- [x] a thrown exception inside `delay` and a `Prog.fail` both reach
      `recover`/`attempt`/`runEither` as the same Throwable
- [x] `sequence` over 10 000 progs is stack-safe from 2.13
- [x] Scala 3 code can `lift` an `A ! Async` and read `.program` back
- [x] the probe project compiles with `-Werror` under 2.13.18

## Stage 2 — everything a 2.13 program needs, in `okay.scala2`
Operator (2026-09-23): the Scala 2 surface lives in ONE package,
`okay.scala2`, and its types carry the library's own names ("вместо
Cont2 называй тип просто Cont"). A 2.13 caller writes
`import okay.scala2._` and gets `Cont`, `Eff`, `State`, `Reader`,
`Writer`, `Throws`, `Async` — Scala 2 types, with the same names as
the Scala 3 ones they stand for.

- `Cont[A, S, R]` — the continuation paramonad (answer-type
  modification) as a class: `map`, `flatMap`, `run(k)`;
  `Cont.pure`, `Cont.shift`, `Cont.reset`. Stack-safe, because it is
  okay's own `Cont` underneath.
- `Eff[-R, A]` — a program over an OPEN row. The row is an
  intersection of phantom capability types, `State[Int] with
  Writer[String]`, which Scala 2 can spell; contravariance makes a
  single-effect program widen into a wider row for free, and
  `flatMap[R1 <: R, B]` accumulates rows the way ZIO 1's `R` did.
  Each capability's companion holds its operations and its handler:
  `State.get/put/modify/run`, `Reader.ask/run`,
  `Writer.tell/run`, `Throws.raise/run` (into Either),
  `Async.delay`; `Eff.run` for an `Eff[Any, A]`, `Eff.runAsync` for
  an `Eff[Async, A]` (JVM, blocking).
  Measured before writing (2026-09-22, by hand): scalac 2.13 infers
  the residual row through `State.run(1)(prog)` with no annotation,
  either handler order works, and running a program with an
  unhandled effect is a compile error (its message names the row,
  not the missing handler).
- THE ONE CAST. On the Scala 2 side the row is a phantom, so the
  program is stored at a single top row and each handler re-types it
  at the concrete row it handles. There is no typed route: `Free` is
  invariant in its row, and the phantom has no Scala 3 counterpart to
  carry. The cast lives in one function with this reason beside it
  (operator rule, no cast without necessity).
- `Prog` stays — `Eff[Async with Throws[Throwable], A]` is the same
  program, and `Eff.fromProg` / `Eff.toProg` cross between them.

## Behavior (stage 2), all from Scala 2.13
- [x] `Cont`: shift/reset, a continuation called twice, answer-type
      change (Int → String), 100 000 binds without a stack overflow
- [x] `Eff`: State + Writer in one for-comprehension, handled in
      both orders with the documented answers
- [x] `Eff`: Reader + State + Throws; a raise stops the program and
      `Throws.run` answers Left, state handled outside it still
      answers
- [x] `Eff`: Async + Throws run with `runAsync`
- [x] `Prog` ↔ `Eff` round trip

## Stage 3 — a Scala 2 user's own effect
Operator (2026-09-23): "свои эффекты в скала 2 нужно будет подумать
как сделать". okay declares an effect with `derives Effect`, a Scala 3
derivation, but all a row split needs from it is
`TypeableK.test(x: Any): Boolean` (Handler.scala). The facade can
build that test itself, from a `ClassTag` that scalac 2 supplies.

```scala
sealed trait Console[A] extends Op[A]
final case class PrintLn(s: String) extends Console[Unit]
case object ReadLn extends Console[String]
object Console extends Effect[Console]
```

- `Op[+A]` — ONE Scala 3 signature that every Scala 2 effect's
  operations extend, so the stored program's row stays `Top`.
- `Effect[F[_]](implicit ClassTag[F[Any]])` — `send(op)` performs an
  operation as `Eff[this.type, A]`: the capability is the effect
  object's SINGLETON type, so two effects are two capabilities with no
  declaration beyond the object. `handle(e)(ret)(h)` removes it.
- `Handler[F, R, B]` — `apply[X](op: F[X], k: X => Eff[R, B])`: the
  operation AND its continuation. Resuming once is an ordinary effect;
  resuming never is abort; resuming twice is nondeterminism.
- A SECOND CAST, `narrow`, from `Op[X]` to `F[X]`. It is right because
  the class test has just passed; it lives in one function next to
  the test that proves it.

Prototype measured from 2.13 by hand (2026-09-23): Console
(resumptive, beside `State`) answered `(3,ada)` and logged `hi ada`;
Choose (multi-shot) answered all four `(Boolean, Boolean)` pairs.
scalac 2.13 types the GADT match (`case PrintLn(s) => k(())`).
OPEN, to be settled by the probe under `-Werror`: at the LAST handler
scalac inferred `R = Any` and `-Xlint` warned "a type was inferred to
be `Any`". Plain `State.run` at the last position did not warn, so the
likely cause is `R` also being inferred through the handler's type.

## Behavior (stage 3), all from Scala 2.13
- [ ] a resumptive effect beside `State`, handled in the same program
- [ ] a multi-shot handler: every answer of two flips
- [ ] an aborting handler: the continuation dropped, the rest never runs
- [ ] two user effects in one row, each handled by its own object
- [ ] an effect left unhandled does not compile
- [ ] the probe stays at `-Xlint -Werror`, whatever that costs the API

## Later stages (not in stage 2)
- Streams: a 2.13 `Source` facade over okay-stream.
- Fibers and channels.
- A 2.13 user's OWN effect: okay declares one with `derives Effect`,
  which is Scala 3; the facade needs a Scala 2 door for it.

## Decisions
- FACADE over cross-build (operator choice, 2026-09-22, after the
  measurements above).
- The facade is written in Scala 3, not 2.13: it must call inline
  methods and name rows, which only a Scala 3 compiler can.

## Results
- STAGE 1 LANDED (2026-09-22). `okay-scala2` (`Prog`, `Bridge`) and
  `okay-scala2-probe`: 6 suites written in Scala 2 and compiled by
  scalac 2.13.18 with `-Ytasty-reader -Xlint -Werror`, plus 1 Scala 3
  bridge suite, all green, from a cold probe target.
- A CONSTRUCTOR MAY NOT NAME THE ROW. scalac 2.13 reads a class's
  primary-constructor parameter types when it first loads the class.
  If one of them names `okay.+`, the class is refused: "Unsupported
  Scala 3 union in bounds of type +; found in object
  okay.Effects$package", reported at the user's `package` line.
  Bisected by hand: a public val, a private val, and a plain parameter
  kept in a def or in a val all failed. METHODS are read lazily, so a
  public method returning the row compiled against the cold probe (tried
  and reverted). Stage 0 never hit this because its row, `State % Int`,
  has no `+`. The fix is `Body`, a value class holding the program:
  `Prog`'s constructor names a class, and naming a class does not read
  its constructor. It costs no allocation.
- REFUTED along the way, and recorded because each one looked
  plausible: (1) "the facade must not live under package `okay`".
  Moving it to `dev.okay.scala2` changed nothing, and 2.13 code in a
  package `okay.userland` compiles and runs. (2) "`private` hides a
  member from the reader". It does not, for constructor parameters.
- sbt ADDS A FOURTH CONSTRAINT to stage 0's three. For a 2.13 project
  whose dependencies bring `scala-library:3.9.0`, sbt stops: "Expected
  scalaVersion to be 3.9.0 or later" (SIP-51). `allowUnsafeScalaLibUpgrade`
  makes 3.9's jar the COMPILE stdlib, which is exactly stage 0's
  failure. What works is to exclude the transitive jar, resolve it in
  a hidden configuration, and APPEND it to Compile/Runtime/Test
  `dependencyClasspath`. The probe uses exactly the settings
  docs/modules/okay-scala2.md gives users, so the page's snippet is
  gated. The one difference: the probe excludes through
  `projectDependencies`, the page through the library dependency.
- The TASTy reader in 2.13.18 accepts 3.9.0 TASTy. That is not
  guaranteed across releases: the reader supports Scala 3 only up to
  a version tied to each 2.13 release. So when this build's Scala is
  bumped, the probe is the first thing to break, and that is the probe
  doing its job.
- STAGE 2 (2026-09-23). `okay.scala2` now holds `Cont`, `Eff` and the
  capabilities `State`/`Reader`/`Writer`/`Throws`/`Async` beside
  `Prog`. The 2.13 probe has 17 suites, all green on their first run
  under `-Xlint -Werror` against a cold target. Every public method is
  called, and a `compileErrors` check pins that an unhandled effect is
  a type error. Two traps, both avoided by construction: (1) the phantom
  capabilities are TRAITS with companions, so the same name is the
  type in a row and the object holding the operations, as in Scala 3
  okay; (2) the Scala 3 source spells the rows with `&`, because
  `with` as a type operator warns in 3.9, and scalac 2.13 reads `&`
  as its own `with`.
