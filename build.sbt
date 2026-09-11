import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

ThisBuild / version := "0.1.0-SNAPSHOT"
// Scala 3.9.0 — the LTS line, opened by 3.9 as 3.3's successor and
// maintained for at least three years. Until 3.9 this build ran the
// latest non-LTS release on purpose (specs/modules-infra.md), because
// LTS meant 3.3 and that was two years behind; 3.9 makes "latest" and
// "LTS" the same choice, so the deliberate decision now goes the other
// way for the same reason it went that way before.
//
// The floor is 3.6 — this code uses the redesigned given syntax
// (`given [A, E] => Conversion[…]`) and named context bounds
// (`[M[_] : Monad as M]`), both 3.6 features, and 3.5 fails with
// hundreds of syntax errors.
//
// THERE IS NO CEILING ANY MORE. It used to be okay-spark, and only
// okay-spark: Spark ships for 2.13, and from 3.8 the Scala 3 stdlib
// is published as `org.scala-lang:scala-library:3.x`, which evicts
// Spark's 2.13 one and carries TASTy where that one carried
// `@ScalaSignature` — so Spark 4's Scala-2-runtime-reflection lookup
// of its own SparkSession could not bootstrap. Its suite now passes
// on 3.9.0; the mechanism, the measurement that found it and the
// four-line fix are written out beside okay-spark's settings below.
// (scala-3-9, 2026-09-07 — full matrix green, 3024 tests, 84 module
// runs, 0 failures, 0 warnings on main, test and Jmh.)
ThisBuild / scalaVersion := "3.9.0"
ThisBuild / scalacOptions ++= Seq(
  "-Xkind-projector",
  "-Wall",
  // `-Wall` includes a lint that fires whenever a non-String is
  // interpolated. Everywhere it fires here, the interpolation is a
  // DIAGNOSTIC — a test's failure message, a decoder's "expected X,
  // got Y" — and the value's own toString is precisely what should
  // appear. Silencing it costs nothing; satisfying it would mean
  // forty-three `.toString` calls that change not one byte of output
  // and hide the warnings that mean something. Said once, here.
  "-Wconf:msg=interpolation uses toString:s",
  // A DISCARDED PROGRAM is a bug, not a style issue: `c.send(x)` in
  // statement position, or as the body of a Unit def, or eta-expanded
  // into a Unit function, builds an `A ! F` value and throws it away —
  // nothing runs. -Wall already sees these three shapes (value
  // discard, non-unit statement); here they are errors when the
  // discarded type is a program. Found by channel-callback (2026-09-02):
  // five silent discards across ui/jetty/netty. The remaining holes,
  // where the compiler cannot help: `xs.foreach(c.send)` and
  // `for x <- xs do c.send(x)` (foreach takes any result). Write
  // `offer(x): Unit` from plain code, or flatMap the program.
  // (the regex takes the TOP-LEVEL type only — a `!` nested inside a
  // Queue[...] element type is not a program being dropped)
  "-Wconf:msg=^(discarded non-Unit|unused) value of type ([^\\[ ]+|[^\\[ ]+\\[[^\\]]*\\]) ! :e",
  // The safe-initialization checker cannot see through munit's
  // `test("…") { … }`, which necessarily captures `this` from a
  // FunSuite body. It is the framework's shape, not ours, and there
  // is nothing at the call site to change.
  "-Wconf:msg=transitively initialized:s",
  // The ctx-capabilities idiom IS juxtaposition — `installer { block
  // }` (providing[Env](x) { ... }, base and providing[..](y) { ... })
  // — a PLAIN block passed where a context function is expected, the
  // deliberate calling shape (docs/capabilities.md). The compiler's
  // own heuristic cannot tell that apart from a stray `=>` that meant
  // `?=>`, and flags every such call site. Rewriting the block to a
  // literal `?=>` lambda would still work, but changes what the test
  // demonstrates (plain-block auto-coloring) for a warning the
  // compiler cannot actually resolve either way.
  "-Wconf:msg=Context function adapts a lambda with the same parameter types:s",
  // Four test files carry a documented `@nowarn("msg=cannot be
  // checked at runtime")` on a JVM erasure warning (Effects.scala's
  // trusted-kernel `@unchecked` pattern match, or okay-sql's `<|>`
  // split). Scala.js and Scala Native never emit that warning — no
  // JVM-style type erasure to warn about — so the very same
  // annotation is legitimately inert there, and would otherwise fail
  // the build under -Wall's "unused nowarn" lint on those platforms
  // for having done its one job correctly on the JVM.
  "-Wconf:msg=@nowarn annotation does not suppress any warnings:s",
  // `-Winfer-union` is new to -Wall in 3.9 and it fires 29 times here
  // with a true-positive rate of ZERO. Every hit was read (scala-3-9,
  // 2026-09-07) and falls in one of four families, all of them shapes
  // this code chose on purpose:
  //   * `Builder | Unit` — `xs.foreach { x => if p then buf += y }`,
  //     the commonest by far (Markdown, Yaml, Typed, Llm, Aws, Keyword,
  //     Split, Provider, ScalaScript, Dom). `foreach[U]` infers U from a
  //     body whose branches are a builder and (). The result is
  //     discarded by construction; there is nothing to fix.
  //   * `Int | Null` — Ring's pop() answers a value or null, the
  //     sentinel the ring is DESIGNED around (TestRing, seven sites).
  //   * `None.type | Some[X]` — Option widening under collect /
  //     collectFirst (Conf).
  //   * a genuine ad-hoc union — `Seq(1.5, 2.5, Some("a"))` typed
  //     `Seq[Any]` on purpose (TestRuntimeStaged), `battery :+ charging`
  //     over two unrelated case classes in a fixture (TestCombine).
  // Suppressed rather than "fixed" for the reason the policy gives: a
  // shape rewritten to please a linter measures the rewrite, not the
  // program, and 29 rewrites would hide the one real defect this lint
  // might one day find. If a future hit looks like a real bug, delete
  // this line and read them all again — that is a half-hour, not a day.
  "-Wconf:msg=A type argument was inferred to be union type:s",
)

ThisBuild / organization := "dev.okay"
ThisBuild / licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / homepage := Some(url("https://github.com/sergey-scherbina/okay"))
ThisBuild / versionScheme := Some("early-semver")

/**
 * integration-test-gate: a test tagged `Live` (a LIVE model gateway,
 * a docker service — kafka/mongo/pg/redis/tls/s3/spark/python/java
 * interop, anything reaching outside the JVM) is real and worth
 * running, but its RESULT depends on something `sbt test` cannot
 * control — and this session alone hit that three times (TestChatDemo's
 * LIVE suite failing identically on untouched master, twice, under
 * load on the live model gateway) before a genuine regression could
 * be told apart from an unrelated flake. `sbt test` now excludes
 * `Live`-tagged tests everywhere by default; `sbt integrationTest`
 * runs the exact same suite with nothing excluded — the services
 * still SKIP where absent (`assume`/`munitIgnore`, unchanged), this
 * only removes them from the gate a landing is measured against.
 *
 * The tag has since been widened, by the operator's call (2026-09-03),
 * past "reaches outside the JVM" to "its result depends on something
 * `sbt test` cannot control": the real-socket suites that flake on
 * port and readiness timing under the full matrix (TestMcpAuth,
 * TestBackends), and TestElectionReplicated, whose single recorded
 * failure was the RUNNER crashing under parallel load rather than
 * anything the suite does. The rule stays what the name says — a
 * green gate must mean the code is good, so a test whose red can be
 * the machine's fault belongs in `integrationTest`, where it still
 * runs and is still read.
 */
ThisBuild / Test / testOptions += Tests.Argument(TestFrameworks.MUnit, "--exclude-tags=Live")
addCommandAlias("integrationTest",
  "; set every Test / testOptions := Seq(Tests.Argument(TestFrameworks.MUnit, \"--include-tags=Live\")); test")

/**
 * The core: plain `okay`, no suffix, dependency-free. One shared
 * source tree (src/main/scala) for JVM, JS and Native — Async included
 * (specs/cross-platform-async.md): each platform contributes its
 * givens (CanBlock/Timer/Scheduler) in src/main/scala-{jvm,js,native};
 * Channel and parMap stay jvm-only for now. The full suite runs on
 * the JVM; the cross suite (src/test/scala-cross) also runs on JS.
 */
lazy val okay = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("."))
  .settings(
    name := "okay",
  )
  .jvmConfigure(_.enablePlugins(JmhPlugin))
  .jvmSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm",
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm-native",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-cross",
    Jmh / sourceDirectory := baseDirectory.value.getParentFile / "src" / "jmh",
    // The core suite runs in its OWN JVM, and that is not a
    // workaround for heavy tests — they are not heavy. Measured: the
    // 1M-operation stack-safety tests pass in 256MB in 0.2s.
    //
    // What they cannot do is share. Unforked, they run inside sbt's
    // own JVM, which its launcher caps at -Xmx4g and which by then
    // also holds zinc's analysis for two dozen modules, the compiler,
    // every module's test classloader, and the dependency classes of
    // Spark, Kafka, ZIO, kyo, fs2 and cats. Then a test that wants a
    // few hundred megabytes at once meets a heap that has no
    // contiguous few hundred megabytes left, and the failure looks
    // like the test's fault: OutOfMemoryError on `1M produced values`,
    // and 30-second timeouts elsewhere from the GC thrashing.
    //
    // With a fork, 1GB — four times what the suite needs — is enough
    // and the whole build passes. Run it alone and it passed all
    // along, which is exactly why this was easy to dismiss.
    Test / fork := true,
    Test / javaOptions += "-Xmx1g",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
    libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "1.1.0" % Test,
  )
  .jsSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-js",
    // the cross suite (Await-based programs) is the ONLY js test source:
    // the full shared suite still leans on jvm-only pieces (Channel, merge)
    Test / unmanagedSourceDirectories :=
      Seq(baseDirectory.value.getParentFile / "src" / "test" / "scala-cross"),
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .nativeSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-native",
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm-native",
    // the cross suite (Await-based programs) plus a native-only dir
    // for what only makes sense here (native-scheduler-pool: the
    // pool Scheduler, CanBlock-based — the cross suite deliberately
    // never uses CanBlock)
    Test / unmanagedSourceDirectories :=
      Seq(baseDirectory.value.getParentFile / "src" / "test" / "scala-cross",
        baseDirectory.value.getParentFile / "src" / "test" / "scala-native"),
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )

/** interop with cats: instances and conversions, nothing more (P3) */
lazy val okayCats = (project in file("okay-cats"))
  .dependsOn(okay.jvm)
  .settings(
    name := "okay-cats",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-free" % "2.12.0",
      "org.typelevel" %% "cats-effect" % "3.5.7",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
      "org.typelevel" %% "cats-laws" % "2.12.0" % Test,
      "org.scalameta" %% "munit-scalacheck" % "1.1.0" % Test,
    ),
  )

/** interop with ZIO: Async <-> ZIO, ZStream <-> Chunks (P3) */
lazy val okayZio = (project in file("okay-zio"))
  .dependsOn(okay.jvm, compare % "test->compile")
  .settings(
    name := "okay-zio",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.1.14",
      "dev.zio" %% "zio-streams" % "2.1.14",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
  )

/** interop with kyo: value and Async bridges (P3) */
lazy val okayKyo = (project in file("okay-kyo"))
  .dependsOn(okay.jvm, compare % "test->compile")
  .settings(
    name := "okay-kyo",
    libraryDependencies ++= Seq(
      "io.getkyo" %% "kyo-core" % "0.16.2",
      "io.getkyo" %% "kyo-direct" % "0.16.2",
      "dev.zio" %% "zio-direct" % "1.0.0-RC7",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
  )

/**
 * interop with the JDK itself: java.util.stream and
 * java.util.function. No dependency to add — it is the platform.
 */
lazy val okayJava = (project in file("okay-java"))
  .dependsOn(okay.jvm, compare % "test->compile")
  .settings(
    name := "okay-java",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )

/**
 * The lead ledger (specs/leads.md): what the chat learned from one
 * request, appended to a CSV nobody needs a database for, and the
 * demand it adds up to. JVM-only because it writes files; it depends
 * on okay-intent for the parsers that read a budget and a date out of
 * a sentence, and on the core for the aggregation algebra.
 */
lazy val okayLeads = (project in file("okay-leads"))
  .dependsOn(okay.jvm, okayIntent.jvm)
  .settings(
    name := "okay-leads",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )

/** interop with fs2: Stream <-> Chunks, chunk for chunk (P3) */
lazy val okayFs2 = (project in file("okay-fs2"))
  .dependsOn(okay.jvm, compare % "test->compile")
  .settings(
    name := "okay-fs2",
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % "3.10.2",
      // the benchmark lane only: fs2 parallelism needs `Concurrent`,
      // so §20's multi-core fs2 rows run on IO and the runtime comes
      // with them. The interop itself stands on fs2-core alone.
      "org.typelevel" %% "cats-effect" % "3.5.7" % Test,
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
  )

/**
 * Actors: a mailbox you already have (a `Channel`), a loop that reads
 * it one message at a time, and the one thing composition does not
 * give — supervision. specs/actor.md.
 *
 * Cross-built: nothing here needs a platform, because everything it
 * stands on is already cross-built.
 */
lazy val okayActor = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-actor"))
  .dependsOn(okay)
  .settings(
    name := "okay-actor",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  // The laws run on ALL THREE platforms (src/test/scala), and that is
  // new (actor-on-js, 2026-09-09). Until then the loop read with
  // `receiveBlocking` and ran behaviours with `runWith`, both of which
  // need `CanBlock` — which JS does not have — so the module
  // cross-built for a platform on which no actor could ever be
  // spawned. The loop is an Async program now, driven by whatever the
  // platform's Scheduler is, and on JS that is the event loop itself.
  //
  // `scala-jvm` keeps the tests that genuinely need a thread: the
  // poison/supervision laws that assert across a blocking join.
  .jvmSettings(
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
  )

/**
 * Reactive Streams interop, on `java.util.concurrent.Flow` — in the
 * JDK since 9, so the zero-dependency rule holds for the main
 * artifact. The TCK is a TEST dependency and is not optional: the
 * spec is thirty-odd rules a publisher must obey, and writing to the
 * prose is a reliable way to produce something that works and is
 * formally wrong.
 *
 * JVM only: `Flow` exists on neither Scala.js nor Native.
 */
lazy val okayReactive = (project in file("okay-reactive"))
  .dependsOn(okay.jvm)
  .settings(
    name := "okay-reactive",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.1.1" % Test,
      "org.reactivestreams" % "reactive-streams-tck-flow" % "1.0.4" % Test,
      "org.reactivestreams" % "reactive-streams-examples" % "1.0.4" % Test,
    ),
  )

/** Kafka as chunked async streams: one poll, one chunk (P4) */
lazy val okayKafka = (project in file("okay-kafka"))
  // okay-persist rides along: KafkaStore is the stage-3 interop
  // engine behind the same Store trait (specs/persist.md);
  // test->test borrows the ElectionSuite for the Kafka control-log
  // leg of the consensus battery (specs/consensus.md)
  .dependsOn(okay.jvm, okayPersist.jvm % "compile->compile;test->test")
  .settings(
    name := "okay-kafka",
    libraryDependencies ++= Seq(
      "org.apache.kafka" % "kafka-clients" % "3.9.0",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
  )

/**
 * The Scala 2 standard library, resolved but NOT put on any compile
 * classpath — okay-spark's test fork is the only consumer, and the
 * comment there says why it needs it (scala-3-9, 2026-09-07).
 */
lazy val LegacyStdlib = config("legacyStdlib").hide

/** Spark via the Aggregator triple (P4); Spark ships for 2.13 only,
 * so the standard for3Use2_13 cross applies */
lazy val okaySpark = (project in file("okay-spark"))
  .dependsOn(okay.jvm, compare % "test->compile")
  .settings(
    name := "okay-spark",
    libraryDependencies ++= Seq(
      ("org.apache.spark" %% "spark-sql" % "4.0.0").cross(CrossVersion.for3Use2_13),
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
    // Spark's 2.13 artifacts bring scala-reflect, a Scala 2 artifact
    // published for NO Scala 3 version. The dependency tree resolves
    // it correctly (2.13.16); what fails is sbt asking for it at the
    // project's own Scala version. Naming the 2.13 artifact
    // explicitly settles it before anything can rewrite the version.
    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.16",
    /**
     * THE ONE THING 3.9 BROKE, and the whole reason okay-spark used to
     * cap this build at 3.7 (scala-3-9, 2026-09-07).
     *
     * Spark 4 does not find its own SparkSession by name. It reads it
     * through SCALA 2 RUNTIME REFLECTION — spark-sql-api's
     * `lookupCompanion` is `scala.reflect.runtime.currentMirror`,
     * `classSymbol(cls).companion.asModule` — and it swallows any
     * failure into a `Try`, so all you ever see is "Cannot find a
     * SparkSession implementation on the Classpath". The real error,
     * caught by running that lookup under a bare `java -cp` on this
     * project's own test classpath, is
     *
     *   scala.reflect.internal.FatalError: class Array does not have
     *   a member apply
     *
     * — the Scala 2 mirror cannot bootstrap at all, because through
     * Scala 3.7 the stdlib on the classpath was
     * `org.scala-lang:scala-library:2.13.x`, and from 3.8 it is
     * `scala-library:3.x`: the same groupId:artifactId, so the 3.x one
     * EVICTS Spark's, and its classes carry TASTy where the 2.13 ones
     * carried `@scala.reflect.ScalaSignature`. No pickle, no members,
     * no mirror. (`scala3-library_3` is now an empty 344-byte shim, so
     * the old two-jar arrangement cannot be rebuilt that way either.)
     *
     * The fix is ORDER, not eviction: the 2.13 stdlib goes on the test
     * fork's classpath AHEAD of the 3.x one, so Scala 2 reflection
     * finds pickled `scala.*` classes, and the Scala-3-only classes
     * (CanEqual, deriving, quoted, runtime.LazyVals) fall through to
     * the 3.x jar behind it. `unmanagedJars` is what buys the order —
     * sbt builds externalDependencyClasspath as unmanaged ++ managed —
     * and the `legacyStdlib` configuration is only how the jar gets
     * RESOLVED; it reaches no compile classpath, so okay-spark is
     * still compiled against 3.9's stdlib like every other module.
     * The version tracks the `scala-reflect` pin three lines up — one
     * Scala 2 library and its own reflect, never a mixed pair.
     *
     * Spark 4.2.0 was tried first and changes NOTHING here: same error,
     * same line. Spark is still 2.13-only at 4.2.0, so a Spark bump is
     * an independent decision and is deliberately not part of this one.
     *
     * This is a deliberate two-stdlib classpath in ONE module's tests.
     * It is legitimate because the two jars are the same library
     * compiled twice, and it is confined because nothing but this test
     * fork sees it. If Spark ever publishes for Scala 3, delete all of
     * it — the config, the jar, and this comment.
     */
    ivyConfigurations += LegacyStdlib,
    libraryDependencies += "org.scala-lang" % "scala-library" % "2.13.16" % LegacyStdlib,
    Test / unmanagedJars ++= Classpaths.managedJars(LegacyStdlib, Set("jar"), update.value),
    Test / fork := true,
    // bench-across-processes: SparkClusterBench starts a REAL
    // standalone cluster — a Master and Workers as their own JVMs —
    // and the driver must come out of THIS build, or it serialises a
    // collection from a different scala-library and the master
    // answers InvalidClassException.
    Test / javaOptions += "-Dokay.spark.cp=" +
      (Test / fullClasspath).value.map(_.data.getAbsolutePath)
        .mkString(java.io.File.pathSeparator),
    Test / javaOptions ++= Seq(
      // Run the fork on the JDK the tests were compiled for. Without
      // this the fork inherits sbt's JVM, and if sbt itself was
      // started on a JDK 24+ (JEP 486 removed the Security Manager,
      // and with it the Subject.getSubject that Hadoop's
      // UserGroupInformation calls) Spark fails with
      // "UnsupportedOperationException: getSubject is not supported".
      // The suite skips itself there rather than failing, but the
      // clearer arrangement is not to be there at all: .sdkmanrc pins
      // Java 21, which is what Spark 4.0.0 supports.
      //
      // A forked JVM with no -Xmx takes the ergonomic default, which
      // on this 36g machine is 9g — for a `local[2]` session over ten
      // thousand doubles. That is not a problem alone, and it is one
      // in a full build: sbt already holds 6g (see .jvmopts) and an
      // IDE with its own compile server can hold another 17g, so the
      // fork asks for memory the machine has already promised away
      // and Spark's driver fails to come up. Two gigabytes is more
      // than this suite has ever needed.
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

/** Flink via the same Aggregator triple (P4); flink-core is pure Java */
lazy val okayFlink = (project in file("okay-flink"))
  // okay-java is TEST only, and only for §20's third lane: the same
  // job over java.util.stream, whose `Collector` an okay Aggregator
  // already is (okay-java's Collect.collector)
  .dependsOn(okay.jvm, okayJava % Test, compare % "test->compile")
  .settings(
    name := "okay-flink",
    // bench-across-processes: FlinkClusterBench starts a REAL
    // standalone cluster — a JobManager and TaskManagers as their own
    // JVMs — and a process needs a classpath the test cannot
    // reconstruct. Written down as a RESOURCE rather than handed over
    // as a `-D`, because this module's tests do not fork and forking
    // them to pass one property would change how every existing lane
    // here runs. (okay-spark forks already, so its own copy of this
    // is a javaOption.) Same arrangement as `compare`.
    Test / resourceGenerators += Def.task {
      val f = (Test / resourceManaged).value / "okay-flink-cp.txt"
      val cp = (Test / classDirectory).value +: (Compile / classDirectory).value +:
        (Test / dependencyClasspath).value.map(_.data)
      IO.write(f, cp.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator))
      Seq(f)
    }.taskValue,
    libraryDependencies ++= Seq(
      "org.apache.flink" % "flink-core" % "1.20.0",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
      // the ENGINE, for the comparison benchmark only (docs/benchmarks.md
      // section 20): flink-streaming-java is the DataStream API,
      // flink-clients brings the MiniCluster a local environment runs on.
      // The Scala DataStream API is not used and could not be: it is
      // published for 2.13 and its TypeInformation macros do not exist for
      // Scala 3 — Flink's own advice since 1.18 is to call the Java API,
      // which is what the lanes do (explicit `.returns(...)` everywhere a
      // Scala lambda erases the type Flink would have extracted).
      "org.apache.flink" % "flink-streaming-java" % "1.20.0" % Test,
      "org.apache.flink" % "flink-clients" % "1.20.0" % Test,
      // §20's three in-process stream libraries. TEST only, and they
      // are here rather than in `compare` because the lane they serve
      // is this job: none of them has an event-time window, so each
      // gets okay.Windows and what is measured is the plumbing.
      "co.fs2" %% "fs2-core" % "3.10.2" % Test,
      "dev.zio" %% "zio-streams" % "2.1.14" % Test,
      "io.getkyo" %% "kyo-core" % "0.16.2" % Test,
    ),
    // Flink 1.20 on JDK 21 reaches into java.base by reflection (Kryo,
    // its own MemorySegment); the same list okay-spark needs, and for
    // the same reason
    Test / fork := true,
    Test / javaOptions ++= Seq(
      // 8 GB because of ONE lane: java.util.stream has no event time, so
      // its "windows" are keys and the whole history stays resident.
      // Measured 2026-09-10: the parallel JDK lane dies with an
      // OutOfMemoryError at 2.4M events on 4 GB, where the okay and
      // Flink lanes — both of which EVICT on a watermark — never came
      // near it. The heap is a lane's requirement, so it is stated here
      // rather than tuned until the red went away.
      "-Xmx8g",
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
      "--add-opens=java.base/java.text=ALL-UNNAMED",
      "--add-opens=java.base/java.time=ALL-UNNAMED",
    ),
  )

/** JDBC as chunked async streams under the Resource region (P4) */
lazy val okayJdbc = (project in file("okay-jdbc"))
  // the first DRIVER of the Sql seam (specs/sql.md); the raw
  // JdbcInterop streaming stays alongside, unchanged. okay-persist
  // backs the write bridge's intent-first journal (specs/jdbc.md);
  // test->test borrows the persist StoreSuite for SqlStore's
  // cross-engine contract run
  .dependsOn(okay.jvm, okaySql.jvm,
    okayPersist.jvm % "compile->compile;test->test")
  .settings(
    name := "okay-jdbc",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.1.1" % Test,
      "com.h2database" % "h2" % "2.3.232" % Test,
      // the lake read road (specs/data.md): DuckDB embedded reads
      // Parquet through the same Sql seam — test-scope, since the
      // road adds ZERO main-code machinery, which is the point
      "org.duckdb" % "duckdb_jdbc" % "1.3.2.0" % Test,
      // the embedded engine everyone actually has: SQLite through
      // the same seam, same zero-machinery argument
      "org.xerial" % "sqlite-jdbc" % "3.47.1.0" % Test,
      // pgjdbc, Test only (jdbc-tails): the JDBC road to Postgres is
      // probed Live — COMMIT on an aborted transaction, temporal binds
      // by ParameterMetaData, ?::jsonb — where the wire driver was
      "org.postgresql" % "postgresql" % "42.7.3" % Test,
    ),
    // JDBC suites fork: DriverManager registers drivers per
    // classloader, and TWO modules carrying H2 in one sbt JVM
    // (okay-match joined okay-jdbc) made "No suitable driver" a
    // matter of which suite ran first — a clean JVM ends that
    Test / fork := true,
  )

/** Delta Lake without Spark (lake-delta, specs/data.md): Delta
 * Kernel — the Delta project's own JVM library — writes and scans
 * tables from the seam's SqlValue rows; the optimistic log commit is
 * theirs to version. Reads at scale stay the JDBC road (DuckDB's
 * delta extension), tested here against the kernel-written table. */
lazy val okayDelta = (project in file("okay-delta"))
  .dependsOn(okay.jvm, okaySql.jvm, okayJdbc % Test)
  .settings(
    name := "okay-delta",
    libraryDependencies ++= Seq(
      "io.delta" % "delta-kernel-api" % "4.4.0",
      "io.delta" % "delta-kernel-defaults" % "4.4.0",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
      "org.duckdb" % "duckdb_jdbc" % "1.3.2.0" % Test,
    ),
    Test / fork := true,
  )

/** the R2DBC hatch of the Sql seam (sql-r2dbc, specs/sql.md): any
 * io.r2dbc.spi.Connection behind the same trait. Honestly framed —
 * on virtual threads it buys DRIVER availability (MSSQL, Oracle,
 * MySQL, ...), not speed: publishers are pulled behind Async.Run as
 * JDBC's blocking calls are parked there. r2dbc-h2 in tests, the
 * Postgres driver live against the dockerized server. */
lazy val okayR2dbc = (project in file("okay-r2dbc"))
  .dependsOn(okay.jvm, okaySql.jvm)
  .settings(
    name := "okay-r2dbc",
    libraryDependencies ++= Seq(
      "io.r2dbc" % "r2dbc-spi" % "1.0.0.RELEASE",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
      "io.r2dbc" % "r2dbc-h2" % "1.1.0.RELEASE" % Test,
      "com.h2database" % "h2" % "2.3.232" % Test,
      "org.postgresql" % "r2dbc-postgresql" % "1.1.2.RELEASE" % Test,
    ),
    // H2 again: the per-classloader driver registration lesson
    Test / fork := true,
  )

/** the Postgres v3 wire as an Sql driver: SCRAM-SHA-256, extended
 * query with portal streaming — the direct road, no JDBC anywhere
 * (specs/sql.md). CROSS-BUILT since sql-pg-node: the pump pulls
 * bytes through the Net seam, SCRAM speaks okay-security's Crypto
 * seam, and a Node process reaches Postgres with no JVM present */
/** the primitive crypto seam (security-crypto-split): hmac/sha256/
 * pbkdf2/random on the platform's own crypto (JCA / node:crypto),
 * resting on NOTHING — so a module that must not cycle through the
 * security stack (okay-pg's SCRAM) stands on a shared seam instead of
 * a private copy. The signing surface stays in okay-security. */
lazy val okayCrypto = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-crypto"))
  .settings(
    name := "okay-crypto",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / (crossProjectPlatform.value match {
        case JVMPlatform => "scala-jvm"
        case _ => "scala-js"
      }),
  )
  .jvmSettings(
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
  )

lazy val okayPg: sbtcrossproject.CrossProject = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-pg"))
  // NOT okaySecurity: its module drags okayHttp (JWKS), which cycles
  // back through mcp/agent/rag to this project's test edge — so SCRAM's
  // primitives ride okay-crypto, the shared crypto-only seam that rests
  // on nothing (security-crypto-split, landed)
  .dependsOn(okay, okaySql, okayCrypto)
  .settings(
    name := "okay-pg",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / (crossProjectPlatform.value match {
        case JVMPlatform => "scala-jvm"
        case _ => "scala-js"
      }),
  )
  .jvmConfigure(
    // okay-jdbc joins the TEST scope for the two-driver acceptance:
    // the same typed program over PgSql and JdbcSql/H2
    _.dependsOn(okayJdbc % Test))
  // okay-tls (JVM only) backs the pg sslmode connect (specs/tls.md, pg
  // lane): the SSLRequest dance is the driver's, the TLS session is the
  // seam's. Compile scope on the JVM leg where SSLSocket lives; the JS
  // leg has no okay-tls, so PgTls is scala-jvm only.
  .jvmConfigure(_.dependsOn(okayTls))
  .jvmSettings(
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
    libraryDependencies += "com.h2database" % "h2" % "2.3.232" % Test,
    // forked like the other database suites: sockets and DriverManager
    // neighbors both behave better in a clean JVM
    Test / fork := true,
  )
  .jsSettings(
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-js",
  )

/** streaming tokenization: pure-state scanners, total, incremental
 * (P5); pure Scala — cross-built, tests run on JS too */
lazy val okayLex = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-lex"))
  .dependsOn(okay)
  .settings(
    name := "okay-lex",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.1.1" % Test,
      "org.scalameta" %%% "munit-scalacheck" % "1.1.0" % Test,
    ),
  )

/**
 * Convergent replicated data types (specs/coordination-free.md stage
 * 2): a `Crdt[A]` whose three laws ship as a runnable check, and the
 * instances that obey them. Depends on `okay` for `Hlc` — an LWW
 * register and a sortable id want the same clock — and for `Uid`,
 * which is what an OR-Set's tags are.
 */
lazy val okayCrdt = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-crdt"))
  // okay for `Hlc` and `Uid`; okay-codec so a replica ships as data
  // (`Wire`). Both are JVM + JS + Native, so nothing narrows.
  .dependsOn(okay, okayCodec)
  .settings(
    name := "okay-crdt",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.1.1" % Test,
      "org.scalameta" %%% "munit-scalacheck" % "1.1.0" % Test,
    ),
  )

/** streaming error-tolerant parsing: total, lossless, two surfaces (P5) */
lazy val okayParse = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-parse"))
  .dependsOn(okayLex)
  .settings(
    name := "okay-parse",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.1.1" % Test,
      "org.scalameta" %%% "munit-scalacheck" % "1.1.0" % Test,
    ),
  )

/** codecs: the Schema algebra and the dialects (P5) */
lazy val okayCodec = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-codec"))
  .dependsOn(okayParse)
  .settings(
    name := "okay-codec",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.1.1" % Test,
      "org.scalameta" %%% "munit-scalacheck" % "1.1.0" % Test,
    ),
  )
  // scala-jvm: `Staging.autoInstall()` reaches okay-staging by name
  // (staging-seam) — reflection, so the JVM only. The JVM-only TEST
  // dir holds what needs a thread with a CHOSEN stack size
  // (TestStackBytes, stack-depth-margin): the only API that measures a
  // decoder's stack cost in bytes, and it exists on no other platform
  .jvmSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm")

/** the document seam: get/put/delete by key with CAS as data,
 * declared-index queries, per-item atomicity — the one new seam of
 * specs/data.md; the own engine is a fold of a compacted topic */
lazy val okayDocs = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-docs"))
  .dependsOn(okay, okayCodec, okayPersist)
  .settings(
    name := "okay-docs",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )

/** the Mongo adapter of the Docs seam — a satellite that pays the
 * driver dependency (the argon2 precedent); live suite against the
 * dockerized Mongo, skips where absent */
lazy val okayDocsMongo = (project in file("okay-docs-mongo"))
  .dependsOn(okay.jvm, okayDocs.jvm % "compile->compile;test->test")
  .settings(
    name := "okay-docs-mongo",
    libraryDependencies ++= Seq(
      "org.mongodb" % "mongodb-driver-sync" % "5.2.1",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
    Test / fork := true,
  )

/** caching with NAMED invalidation: no default TTL — every cache
 * states where its truth lives and how wrong it may be
 * (specs/cache.md); memory engine v1, Redis and the log-fed view
 * behind the same trait later */
lazy val okayCache = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-cache"))
  // okay-persist backs regime 1: the log-fed View is a consumer
  .dependsOn(okay, okayPersist)
  .settings(
    name := "okay-cache",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  // regime 2's write-through test drives H2 through the Sql seam
  .jvmConfigure(_.dependsOn(okayJdbc % Test))
  .jvmSettings(
    // the Redis engine: the RESP client is jvm (a blocking socket)
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
    libraryDependencies += "com.h2database" % "h2" % "2.3.232" % Test,
  )
  .jvmSettings(
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
  )

/** the relational seam: the Sql driver trait and the typed layer
 * (rows/params/verify/transact) written once against it — no
 * java.sql anywhere, asserted by the JS and Native cross-builds
 * (specs/sql.md, specs/jdbc.md) */
lazy val okaySql = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-sql"))
  .dependsOn(okay, okayCodec)
  .settings(
    name := "okay-sql",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
    // sql-temporal-types: java.time givens on the JVM, an empty table
    // elsewhere (JavaTime.scala per platform)
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / (crossProjectPlatform.value match {
        case JVMPlatform => "scala-jvm"
        case JSPlatform => "scala-js"
        case _ => "scala-native"
      }),
  )
  // scala-jvm tests: a suite that DRAINS a `Produce + Async` stream
  // summons a `Handler[Async]`, which needs the `CanBlock` JS and
  // Native do not have (TestRowDecode, sql-plan-cells). The module
  // itself stays cross-built — this is only where such a suite lives.
  .jvmSettings(
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm")

/** the durable log: partitioned append-only persistence, offsets as
 * resume tokens; memory and file engines behind one trait
 * (specs/persist.md) */
lazy val okayPersist = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-persist"))
  // the core for the streaming reads (Chunk ! Produce + Async, the
  // JdbcInterop shape); the codec for the typed Schema view
  .dependsOn(okay, okayCodec)
  // okay-tls joins TEST scope only, for the persist-wire-over-TLS
  // acceptance: the wire's transport is injectable, so the SSLSocket
  // is built in the test and okay-persist keeps its core-only compile
  // graph (rests on okay + codec, nothing that cycles through http)
  .jvmConfigure(_.dependsOn(okayTls % Test))
  .settings(
    name := "okay-persist",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.1.1" % Test,
      "org.scalameta" %%% "munit-scalacheck" % "1.1.0" % Test,
    ),
  )
  .jvmSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
  )
  .jsSettings(
    // the Node leg of the wire client (specs/net.md): the scripted
    // Node server test lives here
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-js",
  )

/**
 * Configuration as data, secrets as references (specs/conf.md):
 * Secret is a reference a config can store and log by construction;
 * Secrets is the resolver seam at the application edge; Conf is the
 * codec plus a file. Cross-built; file:/load are JVM/Native.
 */
lazy val okayConf = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-conf"))
  .dependsOn(okayCodec)
  .settings(
    name := "okay-conf",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .jvmSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm-native",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
  )
  .nativeSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm-native",
  )
  .jsSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-js",
    // node reads process.env by require-time global, no module kind needed
  )

/**
 * The missing third of the observability doctrine (specs/obs.md):
 * spans as VALUES on a trace topic, W3C traceparent as the one
 * propagation vocabulary, and the tracing handler that wraps any
 * other — programs stay observability-blind; export is a consumer.
 */
lazy val okayObs = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-obs"))
  .dependsOn(okay, okayCodec, okayPersist)
  // the OTLP push glue speaks to a collector through the one client;
  // the composition crown test stacks Principal over Tracer
  .jvmConfigure(_.dependsOn(okayHttp.jvm, okaySecurity.jvm % Test))
  .settings(
    name := "okay-obs",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .jvmConfigure(_.dependsOn(okayHttp.jvm % Test, okayJdbc % Test))
  // the overlay test (obs-durable-overlay) drives a real Tracer around
  // a Durable handler; okay-agent joins TEST scope only. okay-obs is a
  // leaf (nothing depends on it), so this arrow makes no cycle.
  .jvmConfigure(_.dependsOn(okayAgent.jvm % Test))
  .jvmSettings(
    // the crossing test (http -> sql, one traceId) needs a database
    libraryDependencies += "com.h2database" % "h2" % "2.3.232" % Test,
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
  )

/**
 * The object-store seam (specs/blob.md): bytes and streams in the
 * engine, meaning at the edge — the trait three landed specs already
 * assumed. Stage 0 is the filesystem engine (jvm); the S3 subset
 * with own SigV4 is blob-s3.
 */
/**
 * Health, stats and Prometheus over the values that already exist
 * (specs/ops.md): a mapping, like OTLP is for tracing, never an
 * SDK. `Ops.routes` is a thin okay-http admin surface any server can
 * wire in; `Prom.render` is a pure Store.Stats -> Prometheus text
 * function, pinned by a golden-string test.
 */
lazy val okayOps = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-ops"))
  // okayResilience: the breaker/bulkhead/limiter Stats become /metrics rows;
  // okaySql: Pool.Stats joins them (persistence-e2e)
  // okayDocs/okayBlob: Docs.Stats and Blob.Stats join too (adapter-stats)
  .dependsOn(okay, okayCodec, okayPersist, okayHttp, okayResilience, okaySql, okayDocs, okayBlob)
  // a real socket for the route-level acceptance test, JVM only
  .jvmConfigure(_.dependsOn(okayJetty % Test))
  .settings(
    name := "okay-ops",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .jvmSettings(
    // the shutdown hook (Signals) is the JVM's own; the routes and
    // the values stay shared
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
  )

lazy val okayBlob = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-blob"))
  // okayCodec: Blob.Stats derives Schema on every platform (adapter-stats)
  .dependsOn(okay, okayCodec)
  // the S3 engine (jvm) speaks the wire through the one http client;
  // persist joined COMPILE scope with the offload tier (this
  // direction is safe — persist depends on core+codec only; the
  // reverse arrow would cycle through http)
  .jvmConfigure(_.dependsOn(okayHttp.jvm, okayPersist.jvm))
  .settings(
    name := "okay-blob",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .jvmSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
  )

/** language models as streams: the thin client (P4/llm.md).
 * Cross-built — only the Transport is platform-bound (java.net.http
 * on the JVM, fetch on JS), everything else is pure Scala */
lazy val okayLlm = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-llm"))
  .dependsOn(okayCodec)
  .settings(
    name := "okay-llm",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .jvmSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm",
  )
  .jsSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-js",
    // the suites use the JVM transport; the JS side is proven by the
    // agent's own cross suite, which mocks the seam
    // no JS-side suite here: the acceptance run drives the linked
    // client from the JVM side (`Test / sources := Seq()` is what
    // makes this project's own test task a no-op)
    Test / sources := Seq(),
  )

/** retrieval from our own primitives: documents split over the
 * lossless CST (exact provenance), embeddings as an effect, the
 * store as an interface (P10) */
lazy val okayRag = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-rag"))
  .dependsOn(okayCodec)
  .settings(
    name := "okay-rag",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.1.1" % Test,
      "org.scalameta" %%% "munit-scalacheck" % "1.1.0" % Test,
    ),
  )
  .jvmSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
  )
  // the pgvector adapter rides the Sql seam (either driver serves);
  // JVM leg only — the JS leg keeps its pure reference store
  .jvmConfigure(_.dependsOn(okaySql.jvm, okayPg.jvm % Test))

/** two-sided matching over LLM-structured chats (specs/match.md):
 * log-first, an attribute registry against vocabulary drift, facts
 * with provenance, hybrid search */
lazy val okayFrame = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-frame"))
  .settings(
    name := "okay-frame",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.1.1" % Test,
      "org.scalameta" %%% "munit-scalacheck" % "1.1.0" % Test,
    ),
  )

lazy val okayIntent = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-intent"))
  .dependsOn(okayCodec, okayRag, okayFrame)
  // TEST only, and worth naming: the live suites reach for okay-agent's
  // journal (Rerun, FileVersions) to replay recorded model answers, and
  // for okay-llm to talk to a gateway. Neither is a dependency of the
  // tiers themselves — main compiles against codec and rag alone, which
  // is the boundary this split exists to draw.
  .dependsOn(okayAgent % Test, okayLlm % Test)
  .settings(
    name := "okay-intent",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.1.1" % Test,
      "org.scalameta" %%% "munit-scalacheck" % "1.1.0" % Test,
    ),
  )
  // JMH on the JVM side: this module quoted microseconds for every
  // tier and each was a System.nanoTime around a loop in a test — not
  // a measurement by this repository's own standard
  // (intent-jmh-row).
  .jvmConfigure(_.enablePlugins(JmhPlugin))
  .jvmSettings(
    // the live suites are JVM-only: they hold an HTTP connection to a
    // gateway, and the tiers themselves are portable
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-cross",
    Jmh / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "jmh" / "scala",
  )
  .jsSettings(
    // The MAIN sources cross; most of the tests do not, and this
    // mirrors what okay-agent already does. Several suites summon a
    // Handler[Async], which needs a CanBlock that only the JVM has —
    // on JS they do not merely fail at runtime, they fail to COMPILE,
    // which is the platform saying the test is asking for something it
    // does not have.
    //
    // `scala-cross` holds the portable ones and is now non-empty: for
    // months this said "there is none yet", which meant okayIntentJS
    // ran ZERO tests while gates reported it as passing — a compile,
    // honestly, and nothing more. `TestModelsCross` is the first, and
    // it is there because a model that loads with no network should
    // demonstrably load on the platform with no filesystem.
    Test / unmanagedSourceDirectories :=
      Seq(baseDirectory.value.getParentFile / "src" / "test" / "scala-cross"),
  )

lazy val okayAgent = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-agent"))
  // okay-persist backs the durable journal: intent and completion
  // are records of a keyed topic (specs/persist.md, stage 1).
  // okay-frame is the FORM a conversation fills: this module owns the
  // suspension and okay-frame owns the slots, which is the split that
  // ended two slot models living in one repository.
  .dependsOn(okayLlm, okayRag, okayPersist, okayFrame)
  .settings(
    name := "okay-agent",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.1.1" % Test,
      "org.scalameta" %%% "munit-scalacheck" % "1.1.0" % Test,
    ),
  )
  .jvmSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-cross",
  )
  .jsSettings(
    // on JS the model is reached by the RELAY (a comonadic handler
    // cannot do I/O where nothing may park), so the cross suite is
    // the portable half
    Test / unmanagedSourceDirectories :=
      Seq(baseDirectory.value.getParentFile / "src" / "test" / "scala-cross"),
  )

/** the own distributed runtime, assembled from existing parts (P7);
 * cross-built: the JVM side holds Remote/Cluster, the JS side the
 * Node client of the acceptance run, the shared tree the ONE program
 * both ends compile (specs/cluster.md) */
lazy val okayCluster = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-cluster"))
  .dependsOn(okayCodec)
  // okay-persist joins in TEST scope only, for the coordinator's
  // journal (specs/dataflow.md, stage 8): `Checkpoint` is two methods
  // over bytes and the STORE is the caller's, so okay-cluster's
  // compile graph stays at okay-codec and `TestPersisted` shows the
  // assembly against the real compacted log. Same arrangement
  // okay-persist itself uses for okay-tls.
  .jvmConfigure(_.dependsOn(okayPersist.jvm % Test))
  .settings(
    name := "okay-cluster",
  )
  .jvmSettings(
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm",
    // the acceptance test runs `node <linked client>` against a local server
    Test / fork := true,
    // bounded for the same reason as okay-spark: a fork with no -Xmx
    // asks for a quarter of the machine
    Test / javaOptions += "-Xmx1g",
    Test / javaOptions += {
      val client = baseDirectory.value.getParentFile / ".js" / "target" /
        ("scala-" + scalaVersion.value) / "okay-cluster-fastopt" / "main.js"
      s"-Dokay.client.js=${client.getAbsolutePath}"
    },
    // stage 4b (specs/dataflow.md): TestDistributed spawns REAL
    // worker processes — `java -cp … okay.cluster.WorkerMain` — and a
    // process needs a classpath. Handed over exactly the way the
    // linked JS client's path above is, for the same reason: the test
    // cannot reconstruct what sbt already knows.
    Test / javaOptions += "-Dokay.cluster.cp=" +
      (Test / fullClasspath).value.map(_.data.getAbsolutePath)
        .mkString(java.io.File.pathSeparator),
    // hang the JS client's linking off Test/compile, not Test/test:
    // `test` is an InputTask in sbt 2 and a Task in sbt 1, while
    // `compile` is a plain TaskKey in both — and compiling before the
    // tests run is exactly when the linked client has to exist
    Test / compile := (Test / compile)
      .dependsOn(LocalProject("okayClusterJS") / Compile / fastLinkJS).value,
  )
  .jsSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-js",
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
    // no JS-side suite here: the acceptance run drives the linked
    // client from the JVM side (`Test / sources := Seq()` is what
    // makes this project's own test task a no-op)
    Test / sources := Seq(),
  )

/**
 * Authorization for services, once (specs/security.md): identities,
 * claims and decisions as values, crypto as a platform seam,
 * protection as a route wrapper. Zero dependencies — the JDK carries
 * the primitives. JVM-first; the JS crypto seam is a stage.
 */
lazy val okaySecurity = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-security"))
  .dependsOn(okayHttp)
  .settings(
    name := "okay-security",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .jvmSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
  )
  .jsSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-js",
    // += (not :=) so the SHARED test dir survives: the pure Es256
    // battery runs on JS precisely because it needs no crypto
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-js",
    // node:crypto arrives by require, which needs a module kind
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
  )

/**
 * The toolkit that is not a toolkit (specs/ui.md): the view is a
 * VALUE, the loop is transduce, the renderer is a seam — a terminal,
 * the DOM, React or a test host, one application on all of them.
 * Pure core cross-built everywhere; the terminal host is jvm+native,
 * the DOM/React glue is js.
 */
lazy val okayUi = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-ui"))
  // okay-persist backs the durable sessions (specs/ui.md low level):
  // the journal is a topic, one session = one key, and recovery is a
  // refold — transitively still zero external dependencies
  .dependsOn(okay, okayPersist)
  // Form is the fifth algebra over Schema and rides where the codec
  // does — which since codec-native is every platform.
  .jvmConfigure(_.dependsOn(okayCodec.jvm))
  .jsConfigure(_.dependsOn(okayCodec.js))
  .nativeConfigure(_.dependsOn(okayCodec.native))
  .settings(
    name := "okay-ui",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .jvmSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm-native",
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-form",
    // the Swing host (ui-native-toolkits): the JVM's own toolkit as a
    // Backend over the same seam, zero dependencies, headless-testable
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm",
  )
  .nativeSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm-native",
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-form",
    Test / sources := Seq(),
  )
  .jsSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-js",
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-form",
    // the shared suite stays JVM; the js dir carries what MUST run
    // under Node — the raw-DOM backend against its fake document
    Test / unmanagedSourceDirectories :=
      Seq(baseDirectory.value.getParentFile / "src" / "test" / "scala-js"),
  )

/**
 * The Model Context Protocol (specs/mcp.md): an MCP server is another
 * `Tool` handler and our tools are another MCP server. The protocol
 * layer is pure — cross-built; only the stdio transport is platform.
 */
lazy val jettyVersion = "12.0.13"

/**
 * The one satellite that buys a dependency (specs/security.md stage
 * 5): Argon2id via Bouncy Castle, because a memory-hard KDF cannot
 * be had from the JDK. Separate module so okay-security keeps its
 * zero; services opt in by classpath.
 */
lazy val okaySecurityArgon2 = project
  .in(file("okay-security-argon2"))
  .dependsOn(okaySecurity.jvm)
  .settings(
    name := "okay-security-argon2",
    libraryDependencies += "org.bouncycastle" % "bcprov-jdk18on" % "1.78.1",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )

/**
 * TLS for the own wires (specs/tls.md): one seam at the transport,
 * postgres's sslmode vocabulary stack-wide, verify-full the only
 * default. Platform crypto only; private keys are Secret references.
 */
/**
 * Python as a handler (specs/py.md; the model is specs/r.md's):
 * call-shaped foreign compute — operations, named functions only,
 * conditions as data, a stdlib-only shim per session, verify makes
 * "wrong venv" a loud startup refusal.
 */
lazy val okayPy = (project in file("okay-py"))
  .dependsOn(okay.jvm, okayCodec.jvm)
  .settings(
    name := "okay-py",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )

// okay-r: R as a handler (specs/r.md) — the same shape okay-py built
// first, with R's own two absences. okay-agent is deliberately NOT a
// dependency: Durable journals R steps because they are operations,
// not because the modules know each other.
lazy val okayR = (project in file("okay-r"))
  .dependsOn(okay.jvm, okayCodec.jvm)
  .settings(
    name := "okay-r",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )

lazy val okayTls = (project in file("okay-tls"))
  .dependsOn(okayConf.jvm)
  .settings(
    name := "okay-tls",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )

/**
 * Sending mail: SMTP as a wire, not a driver dependency
 * (specs/mail.md).
 *
 * The same shape as okay-pg -- a line-oriented protocol over a socket,
 * STARTTLS over okay-tls, AUTH PLAIN and LOGIN -- and SEND ONLY:
 * receiving is IMAP or POP and a much larger module. Asked for by a
 * consumer whose service worked and could not have users without it.
 */
lazy val okayMail = (project in file("okay-mail"))
  .dependsOn(okay.jvm, okayTls, okayConf.jvm)
  .settings(
    name := "okay-mail",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )

/**
 * Resilience (specs/resilience.md): a circuit breaker, a bulkhead, a
 * keyed token-bucket limiter, hedged requests and a travelling
 * deadline, as handlers around any `A ! Async` and around `Http`.
 * JVM + JS like okay-http, which it depends on for the Request it
 * reads the deadline header from.
 */
lazy val okayResilience = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-resilience"))
  .dependsOn(okayHttp)
  .settings(
    name := "okay-resilience",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .jvmSettings(
    // the parking tests: a fiber that waits, a hedge that races a timer
    // DiscoveryJvm: the resolver and sys.env are the JVM's own
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
  )

/**
 * Outbox, inbox, dead-letter (specs/outbox.md): the log and a database
 * that is ours — an event as a row in the business transaction, a
 * relay into a Topic, a consumer that records ids inside its own
 * transaction, a dead-letter topic for poison records. Two seams,
 * nothing new minted; cross-built because both seams are. The H2
 * tests are JVM (okay-jdbc).
 */
lazy val okayOutbox = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-outbox"))
  .dependsOn(okay, okayCodec, okaySql, okayPersist)
  .settings(
    name := "okay-outbox",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .jvmConfigure(_.dependsOn(okayJdbc % Test))
  .jvmSettings(
    libraryDependencies += "com.h2database" % "h2" % "2.3.232" % Test,
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
  )

lazy val okayJetty = project
  .in(file("okay-jetty"))
  .dependsOn(okayHttp.jvm)
  .settings(
    name := "okay-jetty",
    // the acceptance run: okay-http's JS transports, linked as a Node
    // program, driven against a Jetty server that serves both halves.
    // Hung off Test/compile rather than Test/test for the reason
    // okay-cluster records: `test` is an InputTask in sbt 2 and a Task
    // in sbt 1, while `compile` is a plain TaskKey in both.
    Test / javaOptions += {
      // okay-jetty is a plain project, so the path is taken from the
      // build root rather than from a crossProject's sibling directory
      val client = (ThisBuild / baseDirectory).value / "okay-http" / ".js" /
        "target" / ("scala-" + scalaVersion.value) / "okay-http-fastopt" / "main.js"
      s"-Dokay.http.client.js=${client.getAbsolutePath}"
    },
    Test / fork := true,
    Test / compile := (Test / compile)
      .dependsOn(LocalProject("okayHttpJS") / Compile / fastLinkJS).value,
    libraryDependencies ++= Seq(
      "org.eclipse.jetty" % "jetty-client" % jettyVersion,
      "org.eclipse.jetty" % "jetty-server" % jettyVersion,
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-server" % jettyVersion,
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-client" % jettyVersion,
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
  )

lazy val okaySubscription = project
  .in(file("okay-subscription"))
  .dependsOn(okayAgent.jvm)
  .settings(
    name := "okay-subscription",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )

lazy val okayAcme = (project in file("okay-acme"))
  // the client half of a wire (okayHttp) signing with the stack's own
  // ES256 (okaySecurity) -- an ACME client is those two and a state
  // machine, which is why it is 600 lines and not a dependency
  // okayJetty is the client an operator's `Revoke` reaches the CA
  // with -- the module's own flow takes any `Http`, this is the one
  // it hands itself
  // okayBlob for its SigV4: Route 53 is signed with the repository's
  // OWN signer at service=route53, rather than a second copy of AWS's
  // signature algorithm living here (acme-dns-providers)
  .dependsOn(okayHttp.jvm, okaySecurity.jvm, okayJetty, okayBlob.jvm, okayConf.jvm)
  .settings(
    name := "okay-acme",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
    Test / fork := true,
  )

lazy val okayScript = project
  .in(file("okay-script"))
  // okayHttp.jvm is the MAIN dependency since okay-script-site: `Site`
  // answers okay-http's `Request` with its `Response`, so any server
  // speaking those serves a directory of pages. okayJetty joined as a
  // MAIN dependency with okay-script-serve: `Site.serve(port)` and the
  // stock `okay.script.Serve` entry point run a Site over Jetty with
  // no code of the caller's own (it was Test-only before, for the
  // lifecycle proof alone).
  // okayPersist.jvm: Sessions.persisted writes sessions through to a
  // keyed, compacted topic so a restart keeps them (okay-script-
  // persistent-sessions).
  // okayUi.jvm: a page's server-driven Live app is okay-ui's Wire.serve
  // over the page's own WebSocket (okay-script-live).
  // okaySecurity.jvm: a page's `secure:` front-matter is enforced with
  // okay-security's own Verified/Policy ladder (okay-script-secure).
  // okayTls: `Site.serve(port, ssl)` terminates HTTPS through the one
  // transport seam (script-tls); okay-conf rides in with it, for the
  // `Secret` a private key travels as.
  // okayDeploy: okay-script/deploy is ScriptDeploy's value rendered --
  // a container that serves a pages directory (okay-script-image).
  // okayAcme: OKAY_ACME asks a certificate authority for the
  // certificate instead of being handed one (okay-acme).
  // okayStaging: the container already carries the compiler, so the
  // staged codec for every generic door costs it only the staging jar;
  // Serve installs it at boot, `-Dokay.staging=off` keeps the
  // interpreter (staging-seam).
  .dependsOn(okayHttp.jvm, okayPersist.jvm, okayUi.jvm, okaySecurity.jvm, okayJetty, okayTls, okayDeploy, okayAcme, okayStaging)
  .settings(
    name := "okay-script",
    // drives dotty.tools.dotc IN-PROCESS -- no scala/scala-cli
    // subprocess, no new language: markdown fenced ```scala blocks
    // are compiled and run through the real Scala 3 compiler API.
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
    // deployable (specs/deploy.md): the fat jar ScriptDeploy's
    // Dockerfile runs, entry point okay.script.Serve
    _root_.okay.deploy.sbt.OkayDeploy.deployable("okay.script.Serve"),
    // MUST fork (okay-script-scalac-classpath, 2026-09-03): un-forked,
    // the test JVM IS sbt's own JVM, whose System.getProperty(
    // "java.class.path") is just sbt-launch.jar -- sbt manages its
    // real classpath through its own classloaders, invisible to that
    // property. Classpath.ambient (ScalaScript.scala) reads that
    // property, so a script compiled with it saw no scala-library
    // and dotc crashed deep in the Typer (NoSymbol -> ClassSymbol on
    // IntClass). Forking gives the test JVM a real `-cp`.
    Test / fork := true,
    // and `run` MUST fork for the same reason (script-runmain-fork,
    // 2026-09-07): `sbt "okayScript/runMain okay.script.Serve ..."`
    // booted and printed, and every page failed to compile against
    // sbt-launch.jar. Forked, the JVM has a real -cp; its working
    // directory stays the REPO ROOT so a pages directory given
    // relative to it (`store`) still resolves.
    run / fork := true,
    run / baseDirectory := (ThisBuild / baseDirectory).value,
  )

lazy val okayAdmin = project
  .in(file("okay-admin"))
  .dependsOn(okaySecurity.jvm)
  .settings(
    name := "okay-admin",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )

lazy val okayChat = project
  .in(file("okay-chat"))
  .dependsOn(okayLlm.jvm, okayHttp.jvm, okayConf.jvm)
  .settings(
    name := "okay-chat",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )

lazy val okayLive = project
  .in(file("okay-live"))
  .dependsOn(okay.jvm)
  .settings(
    name := "okay-live",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )

lazy val okayNetty = project
  .in(file("okay-netty"))
  .dependsOn(okayHttp.jvm, okayJetty % Test)
  .settings(
    name := "okay-netty",
    libraryDependencies ++= Seq(
      "io.netty" % "netty-codec-http" % "4.1.125.Final",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
  )

lazy val okayHttp = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-http"))
  .dependsOn(okayMcp)
  // the resumable GET stream journals pushes into a topic (specs/mcp.md v7)
  .jvmConfigure(_.dependsOn(okayPersist.jvm))
  .settings(
    name := "okay-http",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .jvmSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
  )
  .jsSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-js",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-js",
    // the acceptance client is linked as a Node program and driven from
    // okay-jetty's JVM suite. Unlike okay-cluster, this project KEEPS
    // its own JS tests — the linked main and the test suite link
    // separately, so both can exist.
    scalaJSUseMainModuleInitializer := true,
    Compile / mainClass := Some("okay.http.Client"),
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
  )

lazy val okayMcp = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-mcp"))
  .dependsOn(okayAgent)
  .settings(
    name := "okay-mcp",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .jvmSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
    Test / fork := true,
    Test / javaOptions += "-Xmx1g",
  )
  .jsSettings(
    // the protocol is pure and compiles here; the transports are not
    Test / sources := Seq(),
  )

/**
 * Not a library module: a real user of the library, from outside.
 * A coding agent over this very repository — okay-rag indexes it,
 * okay-agent runs the loop, okay-llm reaches a local model. It exists
 * to find what tests written by the author of the code cannot.
 */
/** the chat demo's React frontend (specs/demo-chat.md), living
 * INSIDE okay-demo (okay-demo/web): a separate sbt module only
 * because the frontend needs the JS cross-build a plain JVM project
 * cannot carry; the logic is CROSS (view/update pure, tested on the
 * JVM), the browser gets the thin glue over okay-ui's ReactJs
 * against a CDN React */
lazy val okayChatWeb = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-demo/web"))
  .dependsOn(okayUi)
  .settings(
    name := "okay-chat-web",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .jsSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-js",
    scalaJSUseMainModuleInitializer := true,
  )

/**
 * Deployment as a value (specs/deploy.md): `Deploy(...)` plus pure
 * renderers to a Dockerfile, a Helm chart's values, a compose file;
 * the generic chart rides as resources. Knows no application — each
 * app declares its own Deploy and owns the rendered files.
 */
lazy val okayDeploy = (project in file("okay-deploy"))
  // okayConf for `Secret`: a deployment carries secret REFERENCES and
  // never values (specs/deployment.md)
  .dependsOn(okayCodec.jvm, okayConf.jvm)
  .settings(
    name := "okay-deploy",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
    // the `okay` CLI is this module's own fat jar (specs/deployment.md,
    // "What it is, and how it gets onto a machine"): okay-deploy/bin/okay
    // runs it, and a JRE is the one prerequisite -- which `doctor`
    // names first, because a tool that cannot report its own missing
    // runtime is the exact failure this spec set out to prevent
    _root_.okay.deploy.sbt.OkayDeploy.deployable("okay.deploy.Cli"),
  )

lazy val okayDemo = (project in file("okay-demo"))
  // okayResilience: the guards around the one live outbound call
  // (demo-guarded-llm) — the arc's worked instance
  .dependsOn(okayAgent.jvm, okayIntent.jvm, okayMcp.jvm, okayUi.jvm, okayJetty, okayJdbc, okayPg.jvm, okaySecurity.jvm, okaySubscription, okayOps.jvm, okayResilience.jvm, okayAdmin, okayChat, okayLive, okayDeploy)
  // deployable (specs/deploy.md): the fat jar DemoDeploy's Dockerfile runs
  .settings(_root_.okay.deploy.sbt.OkayDeploy.deployable("okay.demo.ChatDemo"))
  .settings(
    name := "okay-demo",
    libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.47.1.0",
    publish / skip := true,
    // RepoMcp is an MCP server on stdio, so `run` needs its own
    // process and its own stdin. Note that `sbt -batch` still keeps
    // stdin for itself — a client should launch the class directly
    // (see okay-mcp's module doc for the command).
    run / fork := true,
    // the DriverManager per-classloader rule (the okay-jdbc lesson):
    // a module carrying a JDBC driver forks its tests — and the
    // forked JVM keeps the REPO ROOT as its working directory,
    // because RepoAgent indexes File(".")
    Test / fork := true,
    Test / baseDirectory := (ThisBuild / baseDirectory).value,
    // test isolation (gate-honesty): `chatStore` is a lazy singleton
    // reading OKAY_CHAT_LOG, and its default is a FileStore directory
    // — under the forked baseDirectory above that is `okay-chat.log/`
    // in the REPO ROOT, so a suite run inherited every previous run's
    // facts and left its own behind. Tests get a MemoryStore; a test
    // that wants a real file store still asks for one by name
    // (`ChatDemo.logOf(path)`, or its own env, as TestTwoNode does
    // for the child processes it spawns).
    Test / envVars += "OKAY_CHAT_LOG" -> ":memory:",
    run / connectInput := true,
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )

/**
 * The interop sentence's Model half (specs/llm-agentic.md): their
 * ChatModel becomes a Handler[Model] — we inherit langchain4j's
 * provider breadth in one small module, they get a composable
 * runtime. Depends on their CORE only; the caller constructs any of
 * their provider models and hands it in.
 */
lazy val okayLangchain4j = (project in file("okay-langchain4j"))
  .dependsOn(okayAgent.jvm)
  .settings(
    name := "okay-langchain4j",
    libraryDependencies ++= Seq(
      "dev.langchain4j" % "langchain4j-core" % "1.19.0",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
  )

/**
 * The interop sentence's OTHER half, narrower than the name
 * (specs/llm-agentic.md, rag-langchain4j): their EmbeddingModel as
 * String => Embedding and as okay-rag's Handler[Embed]. A SEPARATE
 * module from okayLangchain4j (chat) — the local ONNX model this
 * pulls in is a real ~90MB download, and DELIBERATELY NOT in the
 * root `.aggregate(...)` list below: nothing about compiling or
 * testing this repo should force that download on a contributor who
 * never touches embeddings. Build/test it explicitly:
 * `sbt okayLangchain4jEmbed/test`.
 */
lazy val okayLangchain4jEmbed = (project in file("okay-langchain4j-embed"))
  .dependsOn(okayRag.jvm)
  .settings(
    name := "okay-langchain4j-embed",
    libraryDependencies ++= Seq(
      "dev.langchain4j" % "langchain4j-embeddings-all-minilm-l6-v2" % "1.19.0-beta29",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
  )

/**
 * The direct ONNX session (specs/intent-spans.md): the same model file
 * `okay-langchain4j-embed` wraps, opened by this repository's own
 * hands so that the TOKEN vectors come back beside the pooled one —
 * one forward pass, both readings. The operator's name for the
 * module. A native runtime and a model on disk, so — as with
 * okayLangchain4jEmbed — DELIBERATELY NOT in the root `.aggregate`:
 * `sbt okayOnnx/test`, with OKAY_ONNX_MODEL naming a model directory
 * (model.onnx + tokenizer.json), and the suite says SKIPPED without it.
 */
lazy val okayOnnx = (project in file("okay-onnx"))
  .dependsOn(okayRag.jvm)
  .settings(
    name := "okay-onnx",
    libraryDependencies ++= Seq(
      "com.microsoft.onnxruntime" % "onnxruntime" % "1.20.0",
      "ai.djl.huggingface" % "tokenizers" % "0.36.0",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
    Test / fork := true,
  )

/**
 * A browser-level proof of the chat demo (specs/demo-chat.md,
 * demo-e2e-browser): one real chat round through a headless
 * Chromium, driven by Playwright — the same reasoning as
 * okayLangchain4jEmbed above. Playwright downloads its OWN browser
 * on first use (no system browser needed anywhere), but that
 * download is real, so this module is DELIBERATELY NOT in the root
 * `.aggregate(...)` list and not a dependency of okayDemo's own test
 * sourceset. Build/test it explicitly (the React bundle must be
 * linked first): `sbt "okayChatWebJS/fastLinkJS"
 * "okayDemoE2eBrowser/test"`.
 */
lazy val okayDemoE2eBrowser = (project in file("okay-demo-e2e-browser"))
  .dependsOn(okayDemo, okayScript)   // okayScript: the mobile-web proof drives a Live page (ui-mobile)
  .settings(
    name := "okay-demo-e2e-browser",
    // forked, as okay-script's own tests are: a Live page is compiled
    // by the embedded compiler from java.class.path, which in-process
    // is sbt's launcher alone (ui-mobile found it as a parser crash)
    Test / fork := true,
    libraryDependencies ++= Seq(
      "com.microsoft.playwright" % "playwright" % "1.62.0",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
  )

/**
 * The embedder demo module went with okay-match: what it measured
 * was the marketplace's attribute registry deduplicating
 * "разработчик" against "программист", which is a claim about a
 * product this library no longer carries.
 */
/**
 * The GTK 4 host on Scala Native (ui-gtk): aggregated ONLY when the
 * machine has GTK — `pkg-config --exists gtk4` — so `sbt test` on a
 * box without it never sees the project. Headers come from
 * `brew install gtk4 pkg-config` (macOS) or the distribution's
 * libgtk-4-dev; the linking flags are pkg-config's, read at load.
 */
lazy val gtkAvailable: Boolean =
  scala.util.Try(scala.sys.process.Process(Seq("pkg-config", "--exists", "gtk4")).! == 0).getOrElse(false)
lazy val gtkLinkFlags: Seq[String] =
  if (gtkAvailable) scala.sys.process.Process(Seq("pkg-config", "--libs", "gtk4")).!!.trim.split("\\s+").toSeq
  else Seq.empty
lazy val okayUiGtk = (project in file("okay-ui-gtk"))
  .enablePlugins(ScalaNativePlugin)
  .dependsOn(okayUi.native)
  .settings(
    name := "okay-ui-gtk",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
    nativeConfig ~= { c => c.withLinkingOptions(c.linkingOptions ++ gtkLinkFlags) },
  )
lazy val gtkProjects: Seq[ProjectReference] = if (gtkAvailable) Seq(okayUiGtk) else Seq.empty

lazy val root = (project in file("."))
  .aggregate(gtkProjects: _*)
  .aggregate(okay.jvm, okay.js, okay.native, okayStaging, okayCats, okayZio, okayKyo, okayFs2, okayReactive, okayActor.jvm, okayActor.js, okayActor.native, okayKafka,
    okayJava, okaySpark, okayFlink, okayJdbc, okayR2dbc, okayDelta,
    okayLex.jvm, okayLex.js, okayLex.native, okayCrdt.jvm, okayCrdt.js, okayCrdt.native,
    okayParse.jvm, okayParse.js, okayParse.native,
    okayCodec.jvm, okayCodec.js, okayCodec.native, okayLlm.jvm, okayLlm.js,
    okayPersist.jvm, okayPersist.js, okayPersist.native,
    okaySql.jvm, okaySql.js, okaySql.native, okayPg.jvm, okayPg.js,
    okayCrypto.jvm, okayCrypto.js, okayMail,
    okayCache.jvm, okayCache.js, okayCache.native,
    okayDocs.jvm, okayDocs.js, okayDocs.native, okayDocsMongo,
    okayConf.jvm, okayConf.js, okayConf.native,
    okayObs.jvm, okayObs.js, okayObs.native,
    okayBlob.jvm, okayBlob.js, okayBlob.native, okayTls, okayPy, okayR,
    okaySecurity.jvm, okaySecurity.js, okaySecurityArgon2,
    okayFrame.jvm, okayFrame.js,
    okayAgent.jvm, okayAgent.js, okayIntent.jvm, okayIntent.js, okayLeads, okayChatWeb.jvm, okayChatWeb.js, okayLangchain4j, okayRag.jvm, okayRag.js, okayDemo, okaySubscription, okayAdmin, okayChat, okayDeploy, okayLive, okayScript,
    okayMcp.jvm, okayMcp.js, okayUi.jvm, okayUi.js, okayUi.native,
    okayHttp.jvm, okayHttp.js, okayJetty, okayNetty,
    okayResilience.jvm, okayResilience.js,
    okayOutbox.jvm, okayOutbox.js, okayOutbox.native,
    okayCluster.jvm, okayCluster.js, compare)
  .settings(
    name := "okay-root",
    publish / skip := true,
    Compile / sources := Seq(),
    Test / sources := Seq(),
  )

/** comparison benchmarks against the ecosystem: the heavy dependencies live here */
/**
 * Run-time staging (staged-runtime): the staged fold over a Schema
 * VALUE, for schemas that exist only at run time. Its own module on
 * purpose — it carries the Scala 3 compiler (`scala3-staging` and the
 * compiler jar) into whichever program depends on it, and it is JVM
 * only; nothing else here depends on it, and a program that does can
 * still switch it off at launch (`-Dokay.staging=off`). Optional by
 * construction, not by convention.
 */
lazy val okayStaging = project
  .in(file("okay-staging"))
  .dependsOn(okayCodec.jvm)
  .settings(
    name := "okay-staging",
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-staging" % scalaVersion.value,
      "org.scalameta" %% "munit" % "1.1.1" % Test))

lazy val compare = (project in file("compare"))
  .dependsOn(okay.jvm, okayLlm.jvm, okayRag.jvm, okayAgent.jvm, okayHttp.jvm, okayCluster.jvm,
    okayActor.jvm, okayReactive,   // actor-reactive-bench: the two modules that had no numbers
    okayStaging)                   // staged-runtime: the run-time staged codec beside the compile-time one
  .enablePlugins(JmhPlugin)
  .settings(
    name := "okay-compare",
    publish / skip := true,
    // §20's SHARED HALF lives in this project's `src/main`: the Wrocław
    // feed, the definition every engine computes, okay's own lanes and
    // the measurement (docs/benchmarks.md §20). Each engine's lane lives
    // in ITS OWN interop module's tests and depends on this — which is
    // also what gives the benchmark a JVM per lane, and the only
    // arrangement in which Spark can be measured at all: its
    // `SparkSession` needs a two-stdlib classpath that breaks the
    // compilation of anything inlining okay's core.
    // The comparison lanes are written in the COMPETITORS' idioms on
    // purpose — a benchmark that rewrites a library's natural shape to
    // please our linter is measuring the rewrite, not the library. Two
    // of their shapes warn, and neither is ours to fix: kyo's `Loop`
    // takes a default argument the recursive call necessarily uses
    // (E221, GeneratorBenchmark/StreamOpsBenchmark), and kyo-direct's
    // `defer` macro expands to a lambda with a parameter its own
    // expansion does not read (E198 at the `defer`, not at our
    // `kloop`, whose parameters are both used). Silenced HERE, scoped
    // to this project alone, so the zero-warnings rule stays sharp
    // everywhere it can catch a real defect (jmh-warnings, 2026-09-03).
    scalacOptions ++= Seq(
      "-Wconf:msg=Recursive call used a default argument:s",
      "-Wconf:msg=unused explicit parameter:s",
    ),
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
    // THE TEST CLASSPATH, AS A RESOURCE. §20's distributed lane
    // (specs/dataflow.md, stage 7) starts real worker PROCESSES with
    // `java -cp`, and a test running inside sbt cannot read its own
    // classpath — `java.class.path` there is sbt's launcher. okay-
    // cluster hands the same string over as a `-D` because its tests
    // fork; this project's do not, and forking every comparison lane
    // to pass one property would be a heavier change than writing the
    // string down.
    // (`dependencyClasspath`, not `fullClasspath`: the full one
    // contains this project's own resources, so asking for it here
    // would be a task cycle. The two class directories are settings.)
    Test / resourceGenerators += Def.task {
      val f = (Test / resourceManaged).value / "okay-cluster-cp.txt"
      val cp = (Test / classDirectory).value +: (Compile / classDirectory).value +:
        (Test / dependencyClasspath).value.map(_.data)
      IO.write(f, cp.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator))
      Seq(f)
    }.taskValue,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-free" % "2.12.0",
      "org.typelevel" %% "cats-effect" % "3.5.7",
      "dev.zio" %% "zio" % "2.1.14",
      "io.getkyo" %% "kyo-core" % "0.16.2",
      "io.getkyo" %% "kyo-direct" % "0.16.2",
      "dev.zio" %% "zio-direct" % "1.0.0-RC7",
      "org.atnos" %% "eff" % "7.0.4",
      "co.fs2" %% "fs2-core" % "3.10.2",
      "dev.zio" %% "zio-streams" % "2.1.14",
      "io.circe" %% "circe-parser" % "0.14.10",
      "io.circe" %% "circe-generic" % "0.14.10",
    ),
  )

/** The DynamoDB adapter of the Docs seam (docs-dynamo, specs/data.md):
 * the JSON protocol over the one http client, signed by okay-blob's
 * SigV4 with service "dynamodb" — no AWS SDK. JVM, like the Mongo
 * adapter; the DocsSuite contract runs Live against dynamodb-local. */
lazy val okayDocsDynamo = (project in file("okay-docs-dynamo"))
  // okayPg/okaySql in Test: the end-to-end suite (persistence-e2e) runs
  // a Pool over the pg wire and a Saga over this adapter side by side
  .dependsOn(okay.jvm, okayDocs.jvm % "compile->compile;test->test", okayBlob.jvm, okayHttp.jvm,
    okaySql.jvm % Test, okayPg.jvm % Test)
  .settings(
    name := "okay-docs-dynamo",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
    Test / fork := true,
  )

/** The Spring Boot bridge of specs/di.md (stage 2): a Module's
 * installed values as singletons in a Spring context, closed with it;
 * a Spring bean as a module; a controller returning `A ! Async` served
 * through Spring's ReactiveAdapterRegistry, wired by a Boot
 * auto-configuration. JVM; the Boot test scope runs the
 * auto-configuration under ApplicationContextRunner. */
lazy val okaySpring = (project in file("okay-spring"))
  .dependsOn(okay.jvm)
  .settings(
    name := "okay-spring",
    libraryDependencies ++= Seq(
      "org.springframework" % "spring-context" % "6.2.10",
      "org.springframework" % "spring-webflux" % "6.2.10",   // the result handler (OkayResultHandler)
      "io.projectreactor" % "reactor-core" % "3.7.9",
      "org.springframework.boot" % "spring-boot-autoconfigure" % "3.5.5",
      "org.springframework.boot" % "spring-boot-test" % "3.5.5" % Test,
      "org.assertj" % "assertj-core" % "3.27.3" % Test,   // ApplicationContextRunner's assertable context
      // the WebFlux end-to-end: the handler stack under WebTestClient in
      // the default gate, a Netty server on a random port under Live
      "org.springframework.boot" % "spring-boot-starter-webflux" % "3.5.5" % Test,
      "org.springframework" % "spring-test" % "6.2.10" % Test,
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
  )

/** The Guice bridge of specs/di.md (stage 2): a Module as a Guice
 * module (bound by name from the plan, by type when unique), the
 * scope's closer as a bound instance, and an injector's instance as
 * a module. JVM; Guice 7 (jakarta.inject). */
lazy val okayGuice = (project in file("okay-guice"))
  .dependsOn(okay.jvm)
  .settings(
    name := "okay-guice",
    libraryDependencies ++= Seq(
      "com.google.inject" % "guice" % "7.0.0",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
  )

/** The Cassandra adapter of the Docs seam (docs-cassandra, specs/data.md):
 * CQL through the Apache java driver (the Mongo precedent: the vendor
 * driver lives in its satellite), lightweight transactions as the CAS,
 * the engine where a Quorum request means a quorum. JVM; the DocsSuite
 * contract runs Live against a dockerized cassandra:5. */
lazy val okayDocsCassandra = (project in file("okay-docs-cassandra"))
  .dependsOn(okay.jvm, okayDocs.jvm % "compile->compile;test->test")
  .settings(
    name := "okay-docs-cassandra",
    libraryDependencies ++= Seq(
      "org.apache.cassandra" % "java-driver-core" % "4.18.1",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
    Test / fork := true,
  )

/** The CDI bridge of specs/di.md (stage 2, the documented shape built
 * on the operator's go): a Module's installed values as synthetic
 * application-scoped beans through a portable Extension, the closer
 * destroyed with the container; a container's instance as a module.
 * The API only at compile time; Weld SE is the test container. */
lazy val okayCdi = (project in file("okay-cdi"))
  .dependsOn(okay.jvm)
  .settings(
    name := "okay-cdi",
    libraryDependencies ++= Seq(
      "jakarta.enterprise" % "jakarta.enterprise.cdi-api" % "4.1.0",
      "org.jboss.weld.se" % "weld-se-core" % "5.1.6.Final" % Test,
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
  )

/** The OpenAPI document as a rendering of the router that serves it
 * (specs/openapi.md, stage 0): paths, methods, path parameters and
 * request bodies from `Router.entries`, as OpenAPI 3.1 — whose schema
 * dialect IS JSON Schema, so okay-codec's renderer is the whole schema
 * story. JVM: a document is written out or served, and both are. */
lazy val okayOpenapi = (project in file("okay-openapi"))
  .dependsOn(okay.jvm, okayHttp.jvm, okayCodec.jvm)
  .settings(
    name := "okay-openapi",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )
