# okay-scala2

okay from **Scala 2.13**. It is a facade module, written in Scala 3,
whose public types a Scala 2 compiler can read through its TASTy
reader (`-Ytasty-reader`). You write ordinary 2.13 code (for-comprehensions,
lambdas, pattern matches) and the real library runs underneath.

| | |
|---|---|
| `Eff[-R, A]` | a program over an OPEN row of effects, spelled as an intersection: `Eff[State[Int] with Writer[String], A]`. The operations and handlers live on the capabilities' companions: `State`, `Reader`, `Writer`, `Throws`, `Async` |
| `Cont[A, S, R]` | delimited continuations: `Cont.shift`, `Cont.reset`, `Cont.pure`, `map`, `flatMap`, `run(k)`, with answer-type modification |
| `Prog[A]` | a program over `Async + Throws % Throwable`: suspended, failing, recoverable, runnable. `map`, `flatMap`, `attempt`, `recover`, `run()`, `runEither()`; `Prog.pure`, `delay`, `fail`, `fromEither`, `sequence` |
| `Bridge` | the Scala 3 side of `Prog`: `Bridge.lift(p: A ! Async)` and `Bridge.program(prog)`. 2.13 code never names it |

## Setting up a 2.13 build

These are the settings `okay-scala2-probe` uses in this repository's
build.sbt, where the gate compiles a Scala 2.13 test suite with them.
The only difference is that the probe reaches the facade through
`dependsOn` + `projectDependencies`, while you reach it through a
library dependency:

```scala
scalaVersion := "2.13.18"
scalacOptions += "-Ytasty-reader"

libraryDependencies +=
  ("dev.okay" %% "okay-scala2" % "0.1.1")
    .cross(CrossVersion.for2_13Use3)
    .exclude("org.scala-lang", "scala-library")

// Scala 3's standard library, BEHIND 2.13's
lazy val Scala3Stdlib = config("scala3Stdlib").hide
ivyConfigurations += Scala3Stdlib
libraryDependencies += "org.scala-lang" % "scala-library" % "3.9.0" % Scala3Stdlib
Seq(Compile, Runtime, Test).map(c =>
  c / dependencyClasspath ++= Classpaths.managedJars(Scala3Stdlib, Set("jar"), update.value))
```

Why two standard libraries, and why in that order (all three measured
2026-09-22):

- Since 3.8, Scala 3's stdlib is published as `scala-library:3.x`. Its
  classes carry TASTy instead of Scala 2 pickles. If it comes FIRST,
  scalac 2.13 cannot read its own `Predef`: *Unsupported Scala 3 union
  in bounds of type T; found in method wrapRefArray in class
  scala.LowPriorityImplicits*. sbt also refuses outright to put a
  2.13 compiler under a newer stdlib (SIP-51). So the transitive one
  is excluded.
- If it is MISSING, compilation still fails (*could not find package
  scala.annotation.internal*), and at run time `scala.reflect.Enum`
  and the other Scala-3-only classes cannot be found. So it goes
  back in, at the end of the classpath.
- This is the same pair of jars as okay-spark's test classpath, only
  in the opposite order. Both are the same library compiled twice.

## Writing it

This code is copied from `okay-scala2/probe/src/test/scala/TestFromScala2.scala`,
which the gate compiles with scalac 2.13.18 under `-Xlint -Werror`:

```scala
import okay.scala2.Prog

val prog = for {
  a <- Prog.pure(20)
  b <- Prog.delay(a + 1)
} yield a + b + 1
assertEquals(prog.run(), 42)
```

`delay` is the door for code that may throw. A throw inside it
becomes the program's failure, the same failure as `Prog.fail`, so
`attempt`, `recover` and `runEither()` all see it. A throw from a
function passed to `map` or `flatMap` is not caught by anything, and
`run()` rethrows it.

## Several effects in one program: `Eff`

Everything a 2.13 program needs is in one package: `import okay.scala2._`.
The types there have the same names as the Scala 3 types they wrap.
Scala 2 cannot spell a union type, so the effect row is written as an
intersection of capabilities. This is the same shape as the
environment `R` in ZIO 1. The code below is copied from
`okay-scala2/probe/src/test/scala/TestEffFromScala2.scala`:

```scala
val prog: Eff[State[Int] with Writer[String], Int] = for {
  n <- State.get[Int]
  _ <- Writer.tell("saw " + n)
  _ <- State.put(n + 1)
  m <- State.get[Int]
  _ <- Writer.tell("now " + m)
} yield m * 10

assertEquals(Eff.run(Writer.run(State.run(1)(prog))), (Vector("saw 1", "now 2"), (2, 20)))
assertEquals(Eff.run(State.run(1)(Writer.run(prog))), (2, (Vector("saw 1", "now 2"), 20)))
```

- Each handler removes one capability from the row: `State.run(1)`
  turns `Eff[State[Int] with R, A]` into `Eff[R, (Int, A)]`. scalac
  2.13 infers `R` by itself.
- The handler order decides the shape of the answer, exactly as in
  okay's Scala 3 API.
- `Eff.run` accepts only `Eff[Any, A]`, so a program with an unhandled
  effect does not compile. The probe checks this with `compileErrors`.
  The message says `type mismatch` and does not name the missing
  handler.
- `Eff.runAsync` runs a program whose only remaining effect is `Async`.
  `Async.attempt` turns a throw into a `Throws[Throwable]` failure.
- `Eff.fromProg` and `Eff.toProg` convert between `Prog` and
  `Eff[Async with Throws[Throwable], A]`; they are the same program.

Underneath are okay's own `Free` and okay's own handlers. On the Scala 2
side the row is only a phantom type, so the facade needs ONE cast: it
stores the program at a single top row, and each handler re-types it
at the concrete row it handles. The reason is written beside that one
function (`Rows.coerce`).

## Continuations: `Cont`

`Cont[A, S, R]` is okay's continuation paramonad, and it is
stack-safe. `shift` captures the continuation up to the nearest
`reset` and may change the answer type (Danvy & Filinski, *Abstracting
Control*, LFP 1990, doi:10.1145/91556.91622; answer-type modification
and its typing: Asai, *On typing delimited continuations: three new
solutions to the printf problem*, HOSC 2009,
doi:10.1007/s10990-009-9049-5). The code below is copied from
`okay-scala2/probe/src/test/scala/TestContFromScala2.scala`:

```scala
val c: Cont[Int, Int, Int] = for {
  a <- Cont.shift[Int, Int, Int](k => k(k(10)))
  b <- Cont.pure[Int, Int](1)
} yield a + b
assertEquals(Cont.reset(c), 12)

val s: String = Cont.reset(Cont.shift[Int, Int, String](k => "k(5)=" + k(5)).map(_ * 2))
assertEquals(s, "k(5)=10")
```

Scala 2 cannot infer the type arguments of `shift` and `pure` from
where they are used, so write them out.

## What is not here yet

- **Your own effects.** okay declares an effect with `derives Effect`,
  which is Scala 3, and there is no Scala 2 way to do it yet.
- **Streams, fibers and channels.**
- **Direct style.** It is built from Scala 3 macros, so from Scala 2
  it will never be available; write for-comprehensions instead.

## Why a facade and not a cross-build

okay's central type is `A ! Row`, and a row is a union:
`F + G = [A] =>> F[A] | G[A]`. This is okay's encoding of an
extensible effect row (Leijen, *Koka: Programming with Row Polymorphic
Effect Types*, MSFP 2014, doi:10.4204/EPTCS.153.8), over the freer
monad of Kiselyov & Ishii (*Freer Monads, More Extensible Effects*,
Haskell 2015, doi:10.1145/2804302.2804319). Scala 2 cannot spell a
union type. The library's combinators are also `inline`, and the
Scala 2 TASTy reader refuses to call those: *Unsupported Scala 3
inline method flatMap; found in class okay.Free*. So a 2.13 build
cannot use okay's own API directly. It gets a small, fixed-row
surface instead, the way `okay-java` gives Java one. The measurements
and the stages still to come (State/Reader/Writer carriers, streams)
are in [`specs/scala2-facade.md`](../../specs/scala2-facade.md).

References on the mechanism:
[the Scala 2 TASTy reader](https://docs.scala-lang.org/scala3/guides/migration/compatibility-classpath.html),
[SIP-51, dropping forward binary compatibility](https://docs.scala-lang.org/sips/drop-stdlib-forwards-bin-compat.html).
