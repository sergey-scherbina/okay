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

/**
 * ONE TEST PROCESS PER MODULE AT A TIME — the root build's
 * `gate-bound-test-fanout`, met again here the day okay2 crossed
 * (okay2-cross): on JS and Native a test CLASS is an OS process, and a
 * module's task started all of its classes at once. The first full
 * cross run stalled after 1437 results with a dozen `node` and three
 * Native test binaries all at 0.0% CPU, sbt waiting on them. The root
 * build measured the JVM side of the same line and found serial faster
 * (8%), so it is one line for all three platforms.
 */
ThisBuild / Test / parallelExecution := false

/**
 * AT MOST SIX TEST TASKS AT ONCE (okay2-cross stage B). With async and
 * platform crossed, 22 test projects started together: `node`s, Native
 * binaries and forked JVMs on a 14-core box. Scala Native's test adapter
 * gives a binary a hard-coded 40 s to connect (ComRunner, 0.5.12) and
 * then SIGKILLs it; two full runs in a row lost a different module's
 * runner that way at `loadedTestFrameworks`, before any test ran — the
 * same module green alone. The bound keeps startup inside the window.
 */
Global / concurrentRestrictions += Tags.limit(Tags.Test, 6)

lazy val common = Seq(
  scalacOptions := Seq("-deprecation", "-feature", "-Xlint", "-Werror", "-language:higherKinds"),
  libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
)

/**
 * SCALA.JS AND SCALA NATIVE (okay2-cross, specs/okay2.md stage 31). The
 * pure modules are crossProjects with `CrossType.Pure`: one `src/`,
 * three targets. Each JVM project KEEPS ITS ID (`okay2`, `okay2Data`,
 * ...), so `okay2/testOnly X`, CI and the JVM-only modules' dependencies
 * read as before; the others are `okay2JS`, `okay2Native` and so on.
 * A suite that starts real threads lives in `src/test/scala-jvm` and
 * runs on the JVM only (`jvmOnlyTests`).
 */
/** Scala.js's development linker makes a failed cast UNDEFINED
 * BEHAVIOUR (a fatal `UndefinedBehaviorError`); the suites that measure
 * a `ClassCastException` — the defects `Distinct` and the row rules
 * exist to stop — need the JVM's answer, so test linking is compliant */
lazy val jsTests = Test / scalaJSLinkerConfig ~= (_.withSemantics(_.withAsInstanceOfs(org.scalajs.linker.interface.CheckedBehavior.Compliant)))

lazy val jvmOnlyTests = Seq(
  Test / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
  // the JVM suites in a JVM of their own, as the root build forks its
  // core suite: a deep test's heap is then not sbt's (okay2-cross)
  Test / fork := true,
  Test / javaOptions ++= Seq("-Xmx2g", "-Xss8m"),
)

/** a macro module's scala-reflect: on the JVM classpath as before,
 * compile-only on JS and Native, where no macro implementation is
 * reachable at run time */
def reflect(scope: Option[Configuration]) =
  libraryDependencies += scope.fold("org.scala-lang" % "scala-reflect" % scalaVersion.value)(c => "org.scala-lang" % "scala-reflect" % scalaVersion.value % c)

/** a module's per-platform source directories beside its shared `src/main/scala` */
def platformSources(dirs: String*) =
  Compile / unmanagedSourceDirectories ++= dirs.map(d => baseDirectory.value.getParentFile / "src" / "main" / d)

/** the same for tests: `scala-jvm-native` holds the suites that park a thread */
def platformTests(dirs: String*) =
  Test / unmanagedSourceDirectories ++= dirs.map(d => baseDirectory.value.getParentFile / "src" / "test" / d)

/** the aggregate, and nothing else: its own `src` is the core's shared
 * sources, which the crossProject compiles */
lazy val root: Project = (project in file("."))
  .aggregate(
    okay2.jvm, okay2.js, okay2.native,
    okay2Data.jvm, okay2Data.js, okay2Data.native,
    okay2Optics.jvm, okay2Optics.js, okay2Optics.native,
    okay2Workflow.jvm, okay2Workflow.js, okay2Workflow.native,
    okay2Async.jvm, okay2Async.js, okay2Async.native,
    okay2Platform.jvm, okay2Platform.js, okay2Platform.native,
    okay2Stm.jvm, okay2Stm.js, okay2Stm.native,
    okay2Stream, okay2Cats, okay2Fs2, okay2Zio)
  .settings(
    name := "okay2-root",
    publish / skip := true,
    Compile / unmanagedSourceDirectories := Nil,
    Test / unmanagedSourceDirectories := Nil,
    Compile / unmanagedResourceDirectories := Nil,
    Test / unmanagedResourceDirectories := Nil,
  )

lazy val okay2 = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("."))
  .settings(name := "okay2", common)
  // one blackbox macro family (`Replayable`, `Distinct`) over an
  // intersection row, which implicit search cannot take apart (stage 7)
  .jvmSettings(reflect(None), jvmOnlyTests)
  .jsSettings(reflect(Some(Provided)), jsTests)
  .nativeSettings(reflect(Some(Provided)))
  // okay2-bench: the core's benchmarks, in src/jmh as in the root build,
  // on the JVM project only. `test` does not compile them — `Jmh/compile`
  // does (`../scripts/gate.sh "okay2/Jmh/compile"`)
  .jvmConfigure(_.withId("okay2").enablePlugins(JmhPlugin))
  .jvmSettings(Jmh / sourceDirectory := baseDirectory.value.getParentFile / "src" / "jmh")

/** the Async effect: Run/Await, the Drive, Fiber/Scheduler/Timer/CanBlock
 * as traits, par/race/timeout/supervised/attempt/sleep, Retry, Par */
lazy val okay2Async = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay2-async"))
  .dependsOn(okay2 % "compile->compile;test->test")
  .settings(name := "okay2-async", common)
  .jvmSettings(jvmOnlyTests)
  .jsSettings(jsTests)
  .jvmConfigure(_.withId("okay2Async"))

/** the JVM under okay2-async: CanBlock, the timer, the schedulers
 * (Loom, fork-join, drive, own/adaptive, threads), Threads, Interruptible,
 * Scoped, Net, parAll/parTraverse/retry/supervised. Compiles on JDK 21+
 * (virtual threads are named); runs on 17+ (the Loom road is taken only
 * where `Schedulers.hasVirtualThreads`). */
lazy val okay2Platform = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay2-platform"))
  .dependsOn(okay2Async % "compile->compile;test->test")
  .settings(name := "okay2-platform", common)
  // THE PLATFORM FILES (okay2-cross stage B), in the Scala 3 core's
  // layout: `scala` shared, `scala-jvm-native` where a thread can park
  .jvmSettings(jvmOnlyTests, platformSources("scala-jvm", "scala-jvm-native"), platformTests("scala-jvm-native"))
  .nativeSettings(platformSources("scala-native", "scala-jvm-native"), platformTests("scala-jvm-native"))
  .jsSettings(jsTests, platformSources("scala-js"), platformTests("scala-js"))
  .jvmConfigure(_.withId("okay2Platform"))

/** okay-stm for the Scala 2 core: the transaction language `Tx` over
 * `TRef` and the `Stm` runtimes (TL2 over Async, direct, and the one on
 * the simulator) — specs/okay2.md stage 17 */
lazy val okay2Stm = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay2-stm"))
  .dependsOn(okay2Async, okay2Platform % "test->test")
  .settings(name := "okay2-stm", common)
  .jvmSettings(jvmOnlyTests)
  .jsSettings(jsTests)
  .jvmConfigure(_.withId("okay2Stm"))

/** okay-stream's pure layer: chunks, Take/pipe, stages and through,
 * the pipeline as a value, lines, event-time windows */
lazy val okay2Stream: Project = (project in file("okay2-stream"))
  .dependsOn(LocalProject("okay2") % "compile->compile;test->test", okay2Async.jvm, okay2Platform.jvm % "test->test")
  .settings(
    name := "okay2-stream",
    common,
    // its own JVM, as every JVM suite here: a deep test's heap is not
    // sbt's. (Law 1b's hang, first met unforked, was a real defect —
    // `Growing` grew after its seal — fixed by okay2-channel-close-wakeup.)
    Test / fork := true,
    Test / javaOptions ++= Seq("-Xmx2g", "-Xss8m"),
    libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "1.1.0" % Test,
  )

/** okay-data for the Scala 2 core: the approximate aggregators
 * (`Sketch`: HyperLogLog, Count-Min, t-digest), the hybrid logical clock
 * `Hlc` and the sortable id `Uid` over it — specs/okay2.md stage 22 */
lazy val okay2Data = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay2-data"))
  .dependsOn(okay2)
  .settings(name := "okay2-data", common)
  .jvmSettings(jvmOnlyTests)
  .jsSettings(jsTests)
  .jvmConfigure(_.withId("okay2Data"))

/** okay-optics for the Scala 2 core: the profunctor lattice, its
 * interpretations and constructors, `Lens[S](_.f)` and `Lens.field` as
 * Scala 2 macros, zooming State and PState by a lens — specs/okay2.md
 * stage 24 */
lazy val okay2Optics = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay2-optics"))
  .dependsOn(okay2)
  .settings(name := "okay2-optics", common)
  .jvmSettings(jvmOnlyTests)
  // its own macros (`Lens[S](_.f)`, `field`): `Provided` is not passed on
  .jsSettings(jsTests, reflect(Some(Provided)))
  .nativeSettings(reflect(Some(Provided)))
  .jvmConfigure(_.withId("okay2Optics"))

/** okay-workflow for the Scala 2 core: `Wf`, the durable program's
 * own questions over `Delim`'s dialogue, and `Proc`, the free arrow
 * over a row, with the static workflow over it (`Wf.Proc`) —
 * specs/okay2.md stage 27 */
lazy val okay2Workflow = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay2-workflow"))
  .dependsOn(okay2, okay2Optics)
  .settings(name := "okay2-workflow", common)
  .jvmSettings(jvmOnlyTests)
  .jsSettings(jsTests)
  .jvmConfigure(_.withId("okay2Workflow"))

/** cats: `Monad`/`MonadError` for programs, a fold into any monad, the
 * `Io` row (an operation IS an `IO`), `cats.free.Free` both ways */
lazy val okay2Cats: Project = (project in file("okay2-cats"))
  // by NAME: the root aggregates this project and this project depends
  // on the root, and two lazy vals naming each other overflow at load.
  // okay2-async for `toIO`/`fromIO`/`scheduler` (okay2-interop-async);
  // the platform only for the tests' CanBlock
  .dependsOn(LocalProject("okay2") % "compile->compile;test->test", okay2Async.jvm, okay2Platform.jvm % "test->compile")
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
  // on the root, and two lazy vals naming each other overflow at load.
  // okay2-async for `toZIO`/`fromZIO`/`scheduler` (okay2-interop-async);
  // the platform only for the tests' CanBlock
  .dependsOn(LocalProject("okay2") % "compile->compile;test->test", okay2Async.jvm, okay2Platform.jvm % "test->compile")
  .settings(
    name := "okay2-zio",
    common,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.1.14",
      "dev.zio" %% "zio-streams" % "2.1.14",
    ),
  )
