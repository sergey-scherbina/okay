/**
 * okay2 (../specs/okay2.md): the okay CORE written a second time, in
 * Scala 2.13 — not the facade (`okay-scala2`), which wraps the Scala 3
 * library for a 2.13 caller, but a full implementation of the same
 * tree, rotation, handlers and Cont with nothing of Scala 3 on its
 * classpath.
 *
 * A SEPARATE sbt BUILD, not a project of the root build (operator,
 * 2026-09-24): it depends on nothing there, and the root's Scala 3
 * settings (`ThisBuild / scalaVersion`, the JDK floor flags, the
 * `-Wconf` lint set) mean nothing to scalac 2. Gate it from THIS
 * directory with the same script as everything else:
 *
 *     cd okay2 && ../scripts/gate.sh test
 *
 * `-Werror` because the gate's warning check reads Scala 3's
 * `[warn] -- [Exxx]` format and would not see a Scala 2 warning.
 *
 * The interop modules (okay2-interop, 2026-09-24) live beside the core
 * in this build: cats-effect, fs2 and zio all publish for 2.13. kyo
 * does not (Scala 3 only), so there is no okay2-kyo.
 */
ThisBuild / organization := "dev.okay"
ThisBuild / scalaVersion := "2.13.18"

lazy val common = Seq(
  scalacOptions := Seq("-deprecation", "-feature", "-Xlint", "-Werror", "-language:higherKinds"),
  libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
)

lazy val okay2: Project = (project in file("."))
  .aggregate(okay2Async, okay2Platform, okay2Stm, okay2Stream, okay2Data, okay2Optics, okay2Cats, okay2Fs2, okay2Zio)
  .settings(
    name := "okay2",
    common,
    // one blackbox macro: `Replayable` over an intersection row, which
    // implicit search cannot take apart (Replayable.scala, stage 7)
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  )

/** the Async effect: Run/Await, the Drive, Fiber/Scheduler/Timer/CanBlock
 * as traits, par/race/timeout/supervised/attempt/sleep, Retry, Par */
lazy val okay2Async: Project = (project in file("okay2-async"))
  .dependsOn(LocalProject("okay2") % "compile->compile;test->test")
  .settings(name := "okay2-async", common)

/** the JVM under okay2-async: CanBlock, the timer, the schedulers
 * (Loom, fork-join, drive, own/adaptive, threads), Threads, Interruptible,
 * Scoped, Net, parAll/parTraverse/retry/supervised. Compiles on JDK 21+
 * (virtual threads are named); runs on 17+ (the Loom road is taken only
 * where `Schedulers.hasVirtualThreads`). */
lazy val okay2Platform: Project = (project in file("okay2-platform"))
  .dependsOn(okay2Async % "compile->compile;test->test")
  .settings(name := "okay2-platform", common)

/** okay-stm for the Scala 2 core: the transaction language `Tx` over
 * `TRef` and the `Stm` runtimes (TL2 over Async, direct, and the one on
 * the simulator) — specs/okay2.md stage 17 */
lazy val okay2Stm: Project = (project in file("okay2-stm"))
  .dependsOn(okay2Async, okay2Platform % "test->test")
  .settings(name := "okay2-stm", common)

/** okay-stream's pure layer: chunks, Take/pipe, stages and through,
 * the pipeline as a value, lines, event-time windows */
lazy val okay2Stream: Project = (project in file("okay2-stream"))
  .dependsOn(LocalProject("okay2") % "compile->compile;test->test", okay2Async, okay2Platform % "test->test")
  .settings(
    name := "okay2-stream",
    common,
    libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "1.1.0" % Test,
  )

/** okay-data for the Scala 2 core: the approximate aggregators
 * (`Sketch`: HyperLogLog, Count-Min, t-digest), the hybrid logical clock
 * `Hlc` and the sortable id `Uid` over it — specs/okay2.md stage 22 */
lazy val okay2Data: Project = (project in file("okay2-data"))
  .dependsOn(LocalProject("okay2"))
  .settings(name := "okay2-data", common)

/** okay-optics for the Scala 2 core: the profunctor lattice, its
 * interpretations and constructors, `Lens[S](_.f)` and `Lens.field` as
 * Scala 2 macros, zooming State and PState by a lens — specs/okay2.md
 * stage 24 */
lazy val okay2Optics: Project = (project in file("okay2-optics"))
  .dependsOn(LocalProject("okay2"))
  .settings(name := "okay2-optics", common)

/** cats: `Monad`/`MonadError` for programs, a fold into any monad, the
 * `Io` row (an operation IS an `IO`), `cats.free.Free` both ways */
lazy val okay2Cats: Project = (project in file("okay2-cats"))
  // by NAME: the root aggregates this project and this project depends
  // on the root, and two lazy vals naming each other overflow at load
  .dependsOn(LocalProject("okay2") % "compile->compile;test->test")
  .settings(
    name := "okay2-cats",
    common,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.5.7",
      "org.typelevel" %% "cats-free" % "2.12.0",
    ),
  )

/** fs2: a Writer program IS a stream, and a stream is a Writer program */
lazy val okay2Fs2: Project = (project in file("okay2-fs2"))
  .dependsOn(okay2Cats % "compile->compile;test->test")
  .settings(
    name := "okay2-fs2",
    common,
    libraryDependencies += "co.fs2" %% "fs2-core" % "3.10.2",
  )

/** zio: the `Zio` row, a fold into any ZIO, a Writer program as a ZStream */
lazy val okay2Zio: Project = (project in file("okay2-zio"))
  // by NAME: the root aggregates this project and this project depends
  // on the root, and two lazy vals naming each other overflow at load
  .dependsOn(LocalProject("okay2") % "compile->compile;test->test")
  .settings(
    name := "okay2-zio",
    common,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.1.14",
      "dev.zio" %% "zio-streams" % "2.1.14",
    ),
  )
