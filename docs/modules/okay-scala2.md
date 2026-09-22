# okay-scala2

okay from **Scala 2.13**. It is a facade module, written in Scala 3,
whose public types a Scala 2 compiler can read through its TASTy
reader (`-Ytasty-reader`). You write ordinary 2.13 code (for-comprehensions,
lambdas, pattern matches) and the real library runs underneath.

| | |
|---|---|
| `Prog[A]` | a program over `Async + Throws % Throwable`: suspended, failing, recoverable, runnable. `map`, `flatMap`, `attempt`, `recover`, `run()`, `runEither()`; `Prog.pure`, `delay`, `fail`, `fromEither`, `sequence` |
| `Bridge` | the Scala 3 side: `Bridge.lift(p: A ! Async)` and `Bridge.program(prog)`. 2.13 code never names it |

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

This code is copied from `okay-scala2-probe/src/test/scala/TestFromScala2.scala`,
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
