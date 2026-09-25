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

/** integration-test-gate, as the root build has it: a suite reaching
 * outside the process (a bound port, a live service) is tagged `Live`
 * and left out of `test`; `integrationTest` runs exactly those */
ThisBuild / Test / testOptions += Tests.Argument(TestFrameworks.MUnit, "--exclude-tags=Live")
addCommandAlias("integrationTest",
  "; set every Test / testOptions := Seq(Tests.Argument(TestFrameworks.MUnit, \"--include-tags=Live\")); test")
// the same switch alone, for one project's Live suites through
// scripts/gate.sh, which cannot pass a quoted `set`: `liveOnly; okay2Http/test`
addCommandAlias("liveOnly",
  "set every Test / testOptions := Seq(Tests.Argument(TestFrameworks.MUnit, \"--include-tags=Live\"))")

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
  // a small first room for Cont's stack switch, so the suite reaches
  // exhaustion on small threads (cont-stack-okay2, as the root build's
  // core suite runs); the derived default is what users get
  Test / javaOptions += "-Dokay.cont.room=64",
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

/** okay2-stream's suites that need what JS and Native lack — REAL
 * threads (the channel laws, the rings under producers, the growth
 * race; `Thread.ofVirtual` is JVM 21) or files (`java.nio.file`, the
 * table layer's CSV) — kept on the JVM by NAME (okay2-cross stage C),
 * so their files stay where the lanes editing them expect */
lazy val jvmSuitesOnly = Test / unmanagedSources / excludeFilter := HiddenFileFilter ||
  "TestChannelLaws.scala" || "TestChannel.scala" || "TestGrowing.scala" || "TestGrowingSeal.scala" ||
  "TestRing.scala" || "TestBulk.scala" || "TestPlan.scala" || "TablesFixtures.scala" ||
  "TestFlush.scala" || "TestFlushDepth.scala" || "TestParallelChunks.scala" || "TestSchedulerLawsChannel.scala"

/** the aggregate, and nothing else: its own `src` is the core's shared
 * sources, which the crossProject compiles */
lazy val root: Project = (project in file("."))
  .aggregate(
    okay2.jvm, okay2.js, okay2.native,
    okay2Data.jvm, okay2Data.js, okay2Data.native,
    okay2Optics.jvm, okay2Optics.js, okay2Optics.native,
    okay2Lex.jvm, okay2Lex.js, okay2Lex.native,
    okay2Parse.jvm, okay2Parse.js, okay2Parse.native,
    okay2Codec.jvm, okay2Codec.js, okay2Codec.native,
    okay2Sql.jvm, okay2Sql.js, okay2Sql.native,
    okay2Http.jvm, okay2Http.js, okay2Http.native,
    okay2Workflow.jvm, okay2Workflow.js, okay2Workflow.native,
    okay2Async.jvm, okay2Async.js, okay2Async.native,
    okay2Platform.jvm, okay2Platform.js, okay2Platform.native,
    okay2Stm.jvm, okay2Stm.js, okay2Stm.native,
    okay2Stream.jvm, okay2Stream.js, okay2Stream.native,
    okay2Cats, okay2Fs2, okay2Zio, okay2Spark, okay2Jdbc)
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
  // Cont past the stack (cont-stack-okay2): the switch and the room per
  // platform — the JVM's count, the pool it shares with Native, Native's
  // exact reading, JS's bound — beside the shared core
  .jvmSettings(reflect(None), jvmOnlyTests, platformSources("scala-jvm", "scala-jvm-native"))
  .jsSettings(reflect(Some(Provided)), jsTests, platformSources("scala-js"))
  .nativeSettings(reflect(Some(Provided)), platformSources("scala-native", "scala-jvm-native"), platformTests("scala-native"))
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
lazy val okay2Stream = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay2-stream"))
  .dependsOn(okay2 % "compile->compile;test->test", okay2Async, okay2Platform % "test->test")
  .settings(
    name := "okay2-stream",
    common,
    libraryDependencies += "org.scalameta" %%% "munit-scalacheck" % "1.1.0" % Test,
  )
  .jvmSettings(
    // its own JVM, as every JVM suite here: a deep test's heap is not
    // sbt's. (Law 1b's hang, first met unforked, was a real defect —
    // `Growing` grew after its seal — fixed by okay2-channel-close-wakeup.)
    Test / fork := true,
    Test / javaOptions ++= Seq("-Xmx2g", "-Xss8m"),
  // a small first room for Cont's stack switch, so the suite reaches
  // exhaustion on small threads (cont-stack-okay2, as the root build's
  // core suite runs); the derived default is what users get
  Test / javaOptions += "-Dokay.cont.room=64",
  )
  // `ParallelChunks` joins a fiber by parking: where a thread can park
  .jvmSettings(platformSources("scala-jvm-native"))
  .jsSettings(jsTests, jvmSuitesOnly)
  .nativeSettings(jvmSuitesOnly, platformSources("scala-jvm-native"))
  .jvmConfigure(_.withId("okay2Stream"))

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

/** okay-lex for the Scala 2 core (okay2-lex-parse): the total, lossless
 * scanner as a pure step function (`Scan`, `ScanInto`), its drivers
 * (Stage, Chunks, fold, relex), `Mealy` as an arrow, and the JSON dialect */
lazy val okay2Lex = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay2-lex"))
  .dependsOn(okay2Stream, okay2Optics)
  .settings(name := "okay2-lex", common)
  .jvmSettings(jvmOnlyTests)
  .jsSettings(jsTests)
  .jvmConfigure(_.withId("okay2Lex"))

/** okay-parse for the Scala 2 core: the instruction language, the total
 * builder into a lossless CST, incremental reparse, and the JSON driver */
lazy val okay2Parse = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay2-parse"))
  .dependsOn(okay2Lex)
  .settings(
    name := "okay2-parse",
    common,
    libraryDependencies += "org.scalameta" %%% "munit-scalacheck" % "1.1.0" % Test,
  )
  .jvmSettings(jvmOnlyTests)
  .jsSettings(jsTests)
  .jvmConfigure(_.withId("okay2Parse"))

/** okay-codec for the Scala 2 core: `Schema` and its derivation (a
 * blackbox macro where Scala 3 reads a Mirror), and JSON — the value,
 * the fast and lossless parsers, merge patch, encode/decode, the strict
 * reader */
lazy val okay2Codec = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay2-codec"))
  .dependsOn(okay2Parse)
  .settings(
    name := "okay2-codec",
    common,
    libraryDependencies += "org.scalameta" %%% "munit-scalacheck" % "1.1.0" % Test,
  )
  .jvmSettings(reflect(None), jvmOnlyTests)
  .jsSettings(jsTests, reflect(Some(Provided)))
  .nativeSettings(reflect(Some(Provided)))
  .jvmConfigure(_.withId("okay2Codec"))

/** okay-sql for the Scala 2 core: the relational seam over okay2-codec's
 * Schema — values, the typed layer, transactions, Query, Pool. No
 * java.sql, so it cross-builds; java.time instances on the JVM only */
lazy val okay2Sql = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay2-sql"))
  .dependsOn(okay2Codec, okay2Stream)
  .settings(name := "okay2-sql", common)
  .jvmSettings(jvmOnlyTests, platformSources("scala-jvm"))
  .jsSettings(jsTests, platformSources("scala-js-native"))
  .nativeSettings(platformSources("scala-js-native"))
  .jvmConfigure(_.withId("okay2Sql"))

/** okay-http's transport half for the Scala 2 core: Request/Response/
 * Http, WebSocket sessions as Stages (shared), and on the JVM the JDK
 * server, raw NIO and the JDK client/WebSocket transports */
lazy val okay2Http = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay2-http"))
  .dependsOn(okay2Codec, okay2Stream)
  .settings(name := "okay2-http", common)
  // its own macro (`route.of[C]`)
  .jvmSettings(reflect(None), jvmOnlyTests, platformSources("scala-jvm"))
  .jsSettings(jsTests, reflect(Some(Provided)))
  .nativeSettings(reflect(Some(Provided)))
  .jvmConfigure(_.withId("okay2Http").dependsOn(okay2Platform.jvm))

/** okay-jdbc's driver for the Scala 2 core: `JdbcSql`, tested against
 * embedded SQLite and H2 */
lazy val okay2Jdbc: Project = (project in file("okay2-jdbc"))
  .dependsOn(okay2Sql.jvm, okay2Platform.jvm % "test->compile")
  .settings(
    name := "okay2-jdbc",
    common,
    libraryDependencies ++= Seq(
      "com.h2database" % "h2" % "2.3.232" % Test,
      "org.xerial" % "sqlite-jdbc" % "3.47.1.0" % Test,
    ),
    // DriverManager registers drivers per classloader: a JVM of its own
    // (okay-jdbc forks for the same reason)
    Test / fork := true,
  )

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

/**
 * SPARK: the Scala 3 core's `okay-spark`, native to this build's own
 * Scala 2.13 (operator request, specs/cluster-pool.md's own numbers
 * work led here) — no `CrossVersion.for3Use2_13` shim, no explicit
 * `scala-reflect` pin, no cross-stdlib `ArraySeq` serialization trap:
 * this whole build already IS Spark's own Scala 2.13, so its artifacts
 * are ordinary same-Scala-version dependencies.
 *
 * `SparkInterop.aggregate`/`aggregateByKey`/`toSpark` need only
 * `okay2.Aggregator` (core); `SparkBulk` needs `okay2.stream.Bulk`/
 * `Csv`/`Sort`/`Tables` (already ported); `SparkSchema` needs
 * `okay2-codec`'s `Columns`/`Schema`/`Json` (landed).
 */
lazy val okay2Spark: Project = (project in file("okay2-spark"))
  .dependsOn(LocalProject("okay2") % "compile->compile;test->test", okay2Stream.jvm % "compile->compile;test->test",
    okay2Codec.jvm % "compile->compile;test->test")
  .settings(
    name := "okay2-spark",
    common,
    libraryDependencies += "org.apache.spark" %% "spark-sql" % "4.2.0",
    // THE SAME `--add-opens` SET the Scala 3 `okaySpark` build.sbt's
    // `sparkTestSettings` names (root build.sbt) — measured there, not
    // guessed here: Kryo's shuffle serializer reflects into
    // `java.nio.HeapByteBuffer` and JDK 17+'s module system refuses it
    // without these, which surfaced on the FIRST shuffle-shaped call
    // this port made (`aggregateByKey`; the three non-shuffling tests
    // passed with no flags at all). `LegacyStdlib`/`for3Use2_13`'s own
    // settings do NOT apply here — this build already compiles as
    // Spark's own Scala 2.13, nothing to cross.
    Test / fork := true,
    Test / javaOptions ++= Seq(
      "-Xmx2g",
      "--add-opens=java.base/java.lang=ALL-UNNAMED",
      "--add-opens=java.base/java.lang.invoke=ALL-UNNAMED",
      "--add-opens=java.base/java.lang.reflect=ALL-UNNAMED",
      "--add-opens=java.base/java.io=ALL-UNNAMED",
      "--add-opens=java.base/java.net=ALL-UNNAMED",
      "--add-opens=java.base/java.nio=ALL-UNNAMED",
      "--add-opens=java.base/java.util=ALL-UNNAMED",
      "--add-opens=java.base/java.util.concurrent=ALL-UNNAMED",
      "--add-opens=java.base/java.util.concurrent.atomic=ALL-UNNAMED",
      "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED",
      "--add-opens=java.base/sun.nio.cs=ALL-UNNAMED",
      "--add-opens=java.base/sun.security.action=ALL-UNNAMED",
      "--add-opens=java.base/sun.util.calendar=ALL-UNNAMED",
    ),
  )
