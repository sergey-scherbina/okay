import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

// The NEXT release, as a snapshot: v0.1.1 was tagged on 2026-09-14,
// and a build that goes on calling itself 0.1.1 publishes artifacts
// named like a release they are not. After tagging vX, this line moves
// to the next -SNAPSHOT in the same push; TestVersionIsNotAReleasedTag
// (okay-deploy) fails the gate when it has not (version-snapshot).
ThisBuild / version := "0.2.0-SNAPSHOT"
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
  // NOT `-language:implicitConversions` build-wide: it was here for a
  // day (direct-no-ceremony, 2026-09-15) and the operator took it out
  // (2026-09-16) because TestThrows proves `throws`'s `into` by the
  // ABSENCE of that import, and a build-wide flag makes the absence
  // prove nothing. A file that colours inside `direct` imports
  // `scala.language.implicitConversions` itself.
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
  // SplitBenchmark's mixedListNoRev/mixedVecInline instantiate
  // Writer.foldWith/loopWith at A=Int, whose `Say(v) =>
  // Pure(finish(step(s, v), ()))` arm (Writer.scala:118) passes `()`
  // for a program that ends bare in a tell — correct there (a tell's
  // own answer IS Unit) and unreachable for `mixed`, which always
  // continues past a tell with `.flatMap`. Inlining at a concrete A
  // surfaces the arm's generic Unit as a "conversion... will always
  // fail at runtime" diagnostic at the CALL site, past the inliner.
  // Found by single-path-verification's bench.sh smoke test,
  // 2026-09-22, and two narrower suppressions tried and refused
  // first: a plain `@nowarn` on the method and `@nowarn("msg=...")`,
  // neither reaching a diagnostic reported past the inliner; then a
  // `src=`-scoped `-Wconf` entry aimed at this one file, which also
  // did nothing — inlined code seems to report its ORIGIN's source
  // (Writer.scala) to `-Wconf`'s `src` filter, not the expansion site
  // the warning prints at. Only a plain `msg=` works, so this is
  // repo-wide rather than file-scoped — an acceptable trade for a
  // message this specific: another file hitting these exact words is
  // this same shape, not a different bug borrowing them.
  "-Wconf:msg=conversion from Unit to Int:s",
  // THE JDK FLOOR, CHECKED BY THE COMPILER (java-gatherers,
  // 2026-09-23). sbt runs on JDK 25 now (.sdkmanrc), and dotc sees the
  // class library of the JVM it runs in — so without this flag any
  // module could call a 22..25 API and no gate would notice (tests run
  // on 26). `-java-output-version 17` refuses an API past 17 AND emits
  // bytecode major 61, which is what every module already emitted
  // when sbt ran on 21 (dotc's default target; measured). The two
  // cannot be separated — the `-Xunchecked-` variant is overridden by
  // this one — so a module that really calls a 21 API says so with
  // `jdkFloor(21)`, and okay-java, which bridges JDK 24's Gatherer,
  // with `jdkFloor(0)` (no flag). TEST code carries no floor at all
  // (project/JdkFloor.scala says why). specs/java-gatherers.md.
  "-java-output-version", "17",
)

/** a module's JDK floor, replacing the build's 17 (see above):
 * 21 where the module calls a JDK 21 API outside any adaptive guard,
 * 0 for no check at all (okay-java: JDK 24+ API, loaded lazily) */
def jdkFloor(n: Int) = scalacOptions ~= { opts =>
  val rest = JdkFloor.unflagged(opts)
  if (n == 0) rest else rest ++ Seq("-java-output-version", n.toString)
}

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

/**
 * ONE TEST PROCESS PER MODULE AT A TIME (gate-bound-test-fanout,
 * 2026-09-18). On JS and Native a test CLASS is an OS PROCESS — a
 * `node` for Scala.js, a linked binary for Native — and with
 * `parallelExecution` on, one module's task starts all of its classes
 * at once. sbt caps concurrent TASKS at the core count (measured
 * here: `Limit all to 14`), so the ceiling was 14 modules times their
 * classes, and two dumps caught it: 100 node + 65 Native on a 14-core
 * box, and 88 + 57 an hour later, every one of them at 0.0% CPU while
 * sbt waited for tasks that never finished. The gate hung for 57
 * minutes and then again for 18.
 *
 * The bound is per MODULE, not global, so the 14 modules still run at
 * once — what stops is a single module fanning out inside its own
 * task. It applies to the JVM too, where a "class" is a thread rather
 * than a process and the cost is different; that it is one line
 * instead of forty is the reason, and the measurement below is what
 * pays for it.
 *
 * MEASURED, same worktree, same `affected master` scope: the run that
 * STALLED TWICE at module 77 and 78 finishes, and the numbers are in
 * CHANGELOG `gate-bound-test-fanout`.
 *
 * AND WHAT IT COSTS THE JVM — MEASURED 2026-09-18, AND IT REFUTED THE
 * WORRY THIS COMMENT WAS WRITTEN WITH (`jvm-parallel`). The line above
 * said the bound "applies to the JVM too, where a class is a thread
 * rather than a process and the cost is different", and left that
 * unpriced. Priced now: `family jvm` (59 projects), three alternating
 * rounds, `set` in BOTH arms so neither pays for the other's reload —
 *
 *   parallelExecution := false   145, 118, 126   min 118 s
 *   parallelExecution := true    204, 128, 141   min 128 s
 *
 * Serial wins all three PAIRED rounds and the minima by 8%. Turning
 * test classes loose inside a module does not help when fourteen
 * modules are already running: it oversubscribes a 14-core box that
 * the coarse parallelism already fills. So one line for all three
 * platforms is not a compromise the JVM pays for — re-measure before
 * changing it, and note the box is never quiet here, which is why the
 * arms alternate and the minima are what is compared.
 */
ThisBuild / Test / parallelExecution := false
addCommandAlias("integrationTest",
  "; set every Test / testOptions := Seq(Tests.Argument(TestFrameworks.MUnit, \"--include-tags=Live\")); test" +
    // okay-ui-gtk's `test` only compiles (ui-gtk-integration); its suite
    // runs here, by name, where the box has GTK at all
    (if (gtkAvailable) "; okayUiGtk/testOnly okay.ui.gtk.TestGtk" else ""))

/**
 * jdk17-compat-check (2026-09-19): "does this module actually RUN on
 * JDK 17 today", push-button, rather than re-derived by hand each
 * time (Scoped, Schedulers and this itself all started as a manual
 * probe this project wrote from scratch). `sbt verifyJdk17` forks
 * every already-forking module's tests onto the installed JDK 17
 * candidate and reports pass/fail per module -- real, current data,
 * not a table kept by inspection (which already went stale once: see
 * specs/jdk-compatibility.md's own correction history). A machine
 * without the candidate gets a clear one-line failure from `set`
 * itself (`jdk17Home` below does not exist), not a silent no-op --
 * unlike the Test/run DEFAULT (jdk26-default-runtime), this command
 * is explicitly opt-in, so "I asked for it and it's missing" should
 * be loud, not swallowed.
 *
 * Deliberately does NOT fix anything: okay-http, okay-jetty's own
 * direct virtual-thread call (the connector's VirtualThreadPool
 * itself needs nothing from OUR source, only its jetty-virtual-threads
 * import), okay-netty, okay-cluster, okay-persist and okay-script all
 * call a JDK21+ API unconditionally outside any Scoped/Schedulers-
 * style guard — expect this command to name them FAILING until each
 * gets the same per-callsite adaptive treatment those two got
 * (jdk-adaptive-scheduler). Expect okaySpark and okayDelta to PASS —
 * their ceiling is JDK 24+ specifically (JEP 486 removing the
 * Security Manager), and 17 is well under that.
 */
val jdk17Home = file(System.getProperty("user.home")) / ".sdkman" / "candidates" / "java" / "17.0.19-tem"
addCommandAlias("verifyJdk17",
  "; set every Test / javaHome := Some(file(\"" + jdk17Home.getAbsolutePath + "\")); test")

/**
 * jdk26-default-runtime (2026-09-19): the ambient JVM that launches
 * sbt itself — and so compiles everything, `.sdkmanrc` pins it — is
 * JDK 25 since java-gatherers (2026-09-23; the API floor it used to
 * imply is `-java-output-version` now, see `jdkFloor`). This is a SEPARATE knob: the JVM a forked `Test` or
 * `run` actually EXECUTES on, which only a module that already sets
 * `Test / fork := true` / `run / fork := true` (most test-bearing
 * ones already do, for reasons of their own — a real classpath, a
 * real -Xmx, isolation) ever reads. Compiling is the ambient JDK;
 * running now defaults to the newest GA JDK this project has
 * actually checked works, not the oldest one it still supports.
 *
 * Why 26 and not the true latest: 27 is not GA at this date (Adoptium
 * `available_releases` tops out at 26; 27/28 are tip/EA only) —
 * finding was this project already made once with a preview API
 * (script-scoped-state), not repeated here for a whole JDK.
 *
 * okaySpark overrides this back down, below its own settings: Spark
 * 4.2.0's own confirmed range is 17/21/25 (spark-jdk25-guard-fix) —
 * no JDK 26 support is documented upstream, and `.sdkmanrc`'s own
 * comment already recorded 26 refusing the security-manager flag
 * outright. Every OTHER module inherits this default untested against
 * 26 before today — see specs/jdk-compatibility.md for what the first
 * full run under it found.
 */
val jdk26Home = file(System.getProperty("user.home")) / ".sdkman" / "candidates" / "java" / "26.0.2.1-tem"
// the JDK 21 a Hadoop-bound suite (okay-delta) is pinned to, now that
// the ambient compile JDK is 25 (java-gatherers, 2026-09-23)
val jdk21Home = file(System.getProperty("user.home")) / ".sdkman" / "candidates" / "java" / "21.0.7-tem"
// a machine that never installed it keeps the ambient JDK for
// Test/run too — ADDITIVE, exactly like the JDK25 MRJar script, never
// a hard new dependency to build or test this project at all
Seq(
  ThisBuild / Test / javaHome := (if (jdk26Home.exists) Some(jdk26Home) else None),
  ThisBuild / run / javaHome := (if (jdk26Home.exists) Some(jdk26Home) else None),
)

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
    // Multi-Release JAR (script-scoped-state-mrjar, scoped-to-core,
    // 2026-09-19): okay.Scoped ships a JDK21-and-up ThreadLocal
    // backend in the jar root and, WHEN scripts/build-mrjar-jdk25.sh
    // has been run, a java.lang.ScopedValue backend under
    // META-INF/versions/25/ -- the JVM picks per JEP 238, nothing here
    // branches at runtime. The script needs an actual JDK 25+ JVM to
    // compile against (no -release flag can grant an older compiler
    // that API), so this is NOT a normal sbt sub-project on this
    // session's own JDK -- it is a standalone compile whose output
    // this task picks up IF PRESENT. A checkout that never ran the
    // script packages the exact jar it always has: this is additive,
    // never a new hard dependency. See specs/script-scoped-state-mrjar.md.
    Compile / packageBin / mappings := {
      val base = (Compile / packageBin / mappings).value
      val classesDir = baseDirectory.value.getParentFile / "jdk25" / "target" / "classes"
      def classFiles(dir: File): Seq[File] =
        Option(dir.listFiles).toSeq.flatten.flatMap { f =>
          if (f.isDirectory) classFiles(f)
          else if (f.getName.endsWith(".class")) Seq(f)
          else Seq.empty
        }
      if (classesDir.exists) {
        val extra = classFiles(classesDir).map { f =>
          f -> ("META-INF/versions/25/" + IO.relativize(classesDir, f).get)
        }
        base ++ extra
      } else base
    },
    packageOptions ++= {
      val classesDir = baseDirectory.value.getParentFile / "jdk25" / "target" / "classes"
      if (classesDir.exists) Seq(Package.ManifestAttributes("Multi-Release" -> "true"))
      else Seq.empty
    },
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

/**
 * The portable asynchronous effect and its callback-based runtime
 * semantics.  It deliberately supplies no platform default instances.
 */
lazy val okayAsync = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-async"))
  .dependsOn(okay)
  .settings(
    name := "okay-async",
  )
  .jvmSettings(
    Compile / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm-native",
    Test / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "src" / "test" / "scala-cross",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )
  .nativeSettings(
    Compile / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm-native",
    // the shared src/test/scala leans on platform defaults (Comonad/
    // Handler[Async]) this module deliberately does not supply — see
    // okay-platform, which is what has them. Only the cross suite runs here.
    Test / unmanagedSourceDirectories :=
      Seq(baseDirectory.value.getParentFile / "src" / "test" / "scala-cross"),
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .jsSettings(
    Test / unmanagedSourceDirectories :=
      Seq(baseDirectory.value.getParentFile / "src" / "test" / "scala-cross"),
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )

/** The optional direct syntax and its compile-time implementation. */
lazy val okayDirect = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-direct"))
  .dependsOn(okayAsync, okayPlatform % "test->compile")
  .settings(
    name := "okay-direct",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  // jmh->compile is JVM-only: js/native never enable JmhPlugin, so a
  // `jmh` configuration doesn't exist there to depend into — putting
  // this on the base .dependsOn (as one string alongside test->compile)
  // broke ivy resolution for okay-direct_sjs1_3 with "Cannot add
  // dependency ... to configuration 'jmh' ... because this
  // configuration doesn't exist!" (single-path-verification's own
  // ./build.sh test smoke test caught it, 2026-09-22). Scoped here
  // for the same reason as the test dependency above: a benchmark
  // that actually RUNS a direct block needs okay-platform's CanBlock.
  .jvmConfigure(_.enablePlugins(JmhPlugin).dependsOn(okayPlatform.jvm % "jmh->compile"))
  .jvmSettings(
    Test / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
    Jmh / sourceDirectory := baseDirectory.value.getParentFile / "src" / "jmh",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )
  .nativeSettings(
    // the shared src/test/scala needs a CanBlock this module does not
    // provide (okay-platform does); there is no direct-specific cross
    // suite yet, so js/native simply carry no tests of their own.
    Test / unmanagedSourceDirectories := Seq(),
  )
  .jsSettings(
    Test / unmanagedSourceDirectories := Seq(),
  )

/** Concrete JVM, JavaScript and Native runtimes plus system facades. */
lazy val okayPlatform = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-platform"))
  .dependsOn(okayAsync)
  .settings(
    name := "okay-platform",
  )
  .jvmConfigure(_.enablePlugins(JmhPlugin))
  .jvmSettings(
    Compile / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm",
    Compile / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm-native",
    Test / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
    Test / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "src" / "test" / "scala-cross",
    Jmh / sourceDirectory := baseDirectory.value.getParentFile / "src" / "jmh",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
    // Loom is used ON PURPOSE past the 17 floor, behind
    // `Schedulers.hasVirtualThreads` (jdk-adaptive-scheduler): the
    // guard works because call sites link lazily AND the bytecode is
    // 61 so the class loads on 17 at all. `jdkFloor(21)` would make it
    // 65 and break every module above this one on 17; so no API check
    // here — the guard is this module's own (java-gatherers)
    jdkFloor(0),
  )
  .jsSettings(
    Compile / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "src" / "main" / "scala-js",
    // the shared src/test/scala leans on jvm-only pieces (CanBlock is
    // not defined on JS); only the cross suite runs here.
    Test / unmanagedSourceDirectories :=
      Seq(baseDirectory.value.getParentFile / "src" / "test" / "scala-cross"),
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .nativeSettings(
    Compile / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "src" / "main" / "scala-native",
    Compile / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm-native",
    Test / unmanagedSourceDirectories :=
      Seq(baseDirectory.value.getParentFile / "src" / "test" / "scala-cross",
        baseDirectory.value.getParentFile / "src" / "test" / "scala-native"),
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )

/**
 * Streams, channels and the buffers under them (core-modules stage 1,
 * 2026-09-18). 6 136 lines that left `okay` because nothing in the
 * control layer referred to them in code — every apparent dependency
 * from Cont/Free/Effects/Monad/Delim was a comment. What the core
 * kept is the two INTERFACES it is genuinely typed against:
 * `Stream.scala` (the `uncons` typeclass, which `Writer` implements)
 * and `Handoff.scala` (the rendezvous `Async` returns). See
 * specs/core-modules.md.
 *
 * The package is still `okay`, deliberately: measured on 3.9.0
 * before the move, one package across two artifacts resolves
 * `import okay.*` and `import okay.given` in both directions, so no
 * consumer's imports change and only `dependsOn` lines were added.
 *
 * The test layout mirrors the core's, INCLUDING the part that is
 * easy to get wrong: js and native REPLACE `Test /
 * unmanagedSourceDirectories` rather than adding to it, because the
 * shared suite leans on JVM-only pieces.
 */
lazy val okayStream = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-stream"))
  .dependsOn(okayAsync % "compile->compile;test->test", okayPlatform % "compile->compile;test->test", okayStm % "test->compile")
  .settings(
    name := "okay-stream",
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
    // the same reason the core suite forks: see okay's own comment
    Test / fork := true,
    Test / javaOptions += "-Xmx1g",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
    libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "1.1.0" % Test,
  )
  .jsSettings(
    Test / unmanagedSourceDirectories :=
      Seq(baseDirectory.value.getParentFile / "src" / "test" / "scala-cross"),
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .nativeSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm-native",
    Test / unmanagedSourceDirectories :=
      Seq(baseDirectory.value.getParentFile / "src" / "test" / "scala-cross"),
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )

/**
 * The static workflow: `Wf`'s questions, `Proc`'s free arrow over
 * them, and the macro that builds one (core-modules stage 2,
 * 2026-09-18). 2 229 lines that left `okay` because the core is a
 * LEAF here — no file in the core names Wf, Proc or ProcMacro in
 * code, on any platform source directory. What the core keeps is
 * `Replayable`, the 78-line marker `Delim` is typed on, which is the
 * whole of its side of the seam.
 *
 * It is a crossProject because these three files sat in the shared
 * source directory and so compile on JS and Native today; making the
 * module JVM-only would have been a silent loss of that. The suites
 * are JVM, exactly as they were.
 *
 * The package is still `okay` — see okay-stream's comment and
 * specs/core-modules.md for the probe that settled it.
 */
lazy val okayWorkflow = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-workflow"))
  .dependsOn(okayDirect % "compile->compile;test->test", okayOptics % "compile->compile;test->test")
  .settings(
    name := "okay-workflow",
  )
  .jvmSettings(
    Test / fork := true,
    Test / javaOptions += "-Xmx1g",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
    libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "1.1.0" % Test,
  )
  .jsSettings(
    // no suite here: the workflow tests are the core's JVM-only shape
    Test / unmanagedSourceDirectories := Seq.empty,
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .nativeSettings(
    Test / unmanagedSourceDirectories := Seq.empty,
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )

/**
 * Data structures that are not the effect system (core-modules stage
 * 3, 2026-09-18): the approximate aggregators (`Sketch`) and the
 * coordination-free pair, a sortable 128-bit identity (`Uid`) and the
 * hybrid logical clock (`Hlc`) underneath it.
 *
 * Two themes in one module, deliberately, and the trigger to split
 * them is either one growing a second file. What made them one lane
 * is a measurement rather than a theme: the core named none of the
 * three, `Sketch` had ZERO consumers among the 73 modules, and `Uid`
 * and `Hlc` had exactly two each. `Aggregator` STAYED in the core —
 * ten modules are typed on it, which is what an interface looks like.
 *
 * A crossProject because these files sat in the shared source
 * directory and compile on JS and Native today.
 */
lazy val okayData = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-data"))
  .dependsOn(okayAsync % "compile->compile;test->test")
  .settings(
    name := "okay-data",
  )
  .jvmSettings(
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-cross",
    Test / fork := true,
    Test / javaOptions += "-Xmx1g",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
    libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "1.1.0" % Test,
  )
  .jsSettings(
    Test / unmanagedSourceDirectories :=
      Seq(baseDirectory.value.getParentFile / "src" / "test" / "scala-cross"),
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .nativeSettings(
    Test / unmanagedSourceDirectories :=
      Seq(baseDirectory.value.getParentFile / "src" / "test" / "scala-cross"),
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )

/**
 * Profunctor optics and the `Fuse` planner (core-modules stage 4,
 * 2026-09-18): `Optic` with its constraint classes and
 * interpretations, `Fuse`, `Focus`, and the optic SPELLING of
 * zooming.
 *
 * WHAT MADE THIS POSSIBLE, after the spec had filed it as blocked on
 * two seams: stage 2 took `Proc` away, which removed one of them, and
 * reading the other settled it. `State.zoom` used a lens for exactly
 * two things, `get` and `set`, so the core now has
 * `State.zoomWith(look, put)` — no optic in it — and `Zoom.scala`
 * here gives the lens spelling back as an extension on `State.type`.
 * `State.zoom(lens)(prog)` still compiles character for character.
 *
 * `ArrowLaws` moved with it, because it is typed on `Optic.Arrow`;
 * okay-workflow and okay-lex reach it through `test->test`.
 */
lazy val okayOptics = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-optics"))
  .dependsOn(okay % "compile->compile;test->test", okayDirect % "test->compile", okayPlatform % "test->compile")
  .settings(
    name := "okay-optics",
  )
  .jvmSettings(
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-cross",
    Test / fork := true,
    Test / javaOptions += "-Xmx1g",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
    libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "1.1.0" % Test,
  )
  .jsSettings(
    Test / unmanagedSourceDirectories :=
      Seq(baseDirectory.value.getParentFile / "src" / "test" / "scala-cross"),
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .nativeSettings(
    Test / unmanagedSourceDirectories :=
      Seq(baseDirectory.value.getParentFile / "src" / "test" / "scala-cross"),
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )

/**
 * Software transactional memory (core-modules stage 5, 2026-09-18):
 * the `Tx` language, the `Stm` runtimes — TL2 with versions and
 * CAS-owned commit, the direct one, the simulated one — and the
 * platform givens that install `Stm[Async]`.
 *
 * `TRef` STAYED IN THE CORE, and that is the whole shape of this
 * lane. Its `modify` is a self-contained CAS loop, "the one-cell
 * transaction", which needs no `Tx` and no runtime; measured across
 * this repository, a single-cell `TRef.modify` is what almost every
 * consumer actually uses, and Scala Native's own scheduler holds its
 * state in one. So the CELL is the interface and stays; the
 * MULTI-CELL machinery that commits several of them together is what
 * left.
 *
 * The spec had this lane blocked on `Providing.Facts` being "backed
 * by `TMap`". That was a naming coincidence: `TMap` is a
 * heterogeneous map with TYPED keys, nothing transactional, and
 * `Refs` is run-time state cells rather than STM. Neither moved.
 */
lazy val okayStm = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-stm"))
  .dependsOn(okayAsync % "compile->compile;test->test", okayPlatform % "test->compile")
  .settings(
    name := "okay-stm",
  )
  .jvmSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm-native",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-cross",
    Test / fork := true,
    Test / javaOptions += "-Xmx1g",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
    libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "1.1.0" % Test,
  )
  .jsSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-js",
    Test / unmanagedSourceDirectories :=
      Seq(baseDirectory.value.getParentFile / "src" / "test" / "scala-cross"),
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .nativeSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm-native",
    Test / unmanagedSourceDirectories :=
      Seq(baseDirectory.value.getParentFile / "src" / "test" / "scala-cross"),
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )

/** interop with cats: instances and conversions, nothing more (P3) */
lazy val okayCats = (project in file("okay-cats"))
  .dependsOn(okayAsync.jvm, okayPlatform.jvm)
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
  .dependsOn(okay.jvm, okayStream.jvm, compare % "test->compile")
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
  .dependsOn(okayAsync.jvm, okayPlatform.jvm, compare % "test->compile")
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
  .dependsOn(okay.jvm, okayStream.jvm, compare % "test->compile")
  .settings(
    name := "okay-java",
    // `Gather` names java.util.stream.Gatherer (JDK 24, JEP 485): no
    // API floor here, bytecode stays dotc's default 61, and the class
    // links lazily — okay-java still loads on 17/21 and only a call
    // into Gather needs 24+ (specs/java-gatherers.md)
    jdkFloor(0),
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )

/**
 * interop with CLOJURE (specs/clojure.md): `Clj` calls into Clojure
 * through its own Java API, and a `Stage` IS a transducer both ways —
 * `Transducers.of(stage)` runs in `into`/`transduce`/`sequence`,
 * `Transducers.stage(xf)` runs Clojure's transducers in okay pipelines.
 * JVM; Clojure 1.12 runs on JDK 8+, so the build's default floor.
 */
lazy val okayClojure = (project in file("okay-clojure"))
  // okayAsync + okayPlatform: `Ops.sleep` builds an Async operation on the platform Timer
  // okayStream test->test: CoreAsyncChannel answers the SAME
  // ChannelLawsSuite every okay channel does (clojure-core-async)
  // okayCodec in Test: EDN written by okay is read by clojure.edn, and
  // Clojure's pr-str is read by okay's Edn (edn-codec)
  .dependsOn(okay.jvm, okayStream.jvm % "compile->compile;test->test", okayAsync.jvm, okayPlatform.jvm,
    okayCodec.jvm % Test)
  .settings(
    name := "okay-clojure",
    libraryDependencies ++= Seq(
      "org.clojure" % "clojure" % "1.12.6",
      "org.clojure" % "core.async" % "1.9.865",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
  )

/**
 * interop with FREGE, a Haskell for the JVM (specs/frege.md): a Frege
 * program written in `Prog` — a thin Frege monad whose operations are
 * okay's (await, tell, perform) and whose `liftIO` takes existing Frege
 * IO — runs as an okay Stage or program; its continuations are Frege
 * functions, so multi-shot handlers work and no thread is involved.
 * `.fr` sources are compiled by okay-frege/sbt-plugin (forked, -target 17):
 * src/main/frege BEFORE the Scala driver that reads its classes.
 */
lazy val okayFrege = (project in file("okay-frege"))
  // okayAsync + okayPlatform: `Ops.sleep` builds an Async operation on the platform Timer
  .dependsOn(okay.jvm, okayStream.jvm, okayAsync.jvm, okayPlatform.jvm)
  .settings(
    name := "okay-frege",
    libraryDependencies ++= Seq(
      "org.frege-lang" % "frege" % "3.25.153",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
    Test / fork := true,
  )
  // the build half is okay-frege/sbt-plugin, what a user of okay-frege enables too
  .enablePlugins(_root_.okay.frege.sbt.OkayFrege)
  .settings(_root_.okay.frege.sbt.OkayFrege.before(Compile))
  .settings(_root_.okay.frege.sbt.OkayFrege.in(Test))

/**
 * okay from SCALA 2.13 (specs/scala2-facade.md): a facade written in
 * Scala 3 whose public signatures a Scala 2 compiler can read through
 * `-Ytasty-reader` — no inline, no union row, no opaque type.
 */
lazy val okayScala2 = (project in file("scala2/okay-scala2"))
  .dependsOn(okay.jvm, okayAsync.jvm, okayPlatform.jvm, okayStream.jvm)
  .settings(
    name := "okay-scala2",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )

/**
 * okay-codec for Scala 2.13 (specs/scala2-facade.md, stage 6): what a
 * Scala 2 compiler cannot use of it — the derivation and the `Json`
 * value type — rebuilt in the same package as okay-scala2. A separate
 * module, so a 2.13 build pulls the codec only if it uses it.
 */
lazy val okayScala2Codec = (project in file("scala2/okay-scala2-codec"))
  .dependsOn(okayCodec.jvm)
  .settings(
    name := "okay-scala2-codec",
  )

/**
 * okay-http for Scala 2.13 (specs/scala2-facade.md, stage 7): a
 * `Response`, routing as Scala 2 extractors, a server and a client.
 * `Request`, `Method` and `Body` are okay-http's own, used directly.
 */
lazy val okayScala2Http = (project in file("scala2/okay-scala2-http"))
  .dependsOn(okayScala2, okayScala2Codec, okayHttp.jvm)
  .settings(
    name := "okay-scala2-http",
  )

/**
 * okay-sql for Scala 2.13 (specs/scala2-facade.md, stage 8): `Db`, the
 * operations as `Eff` and `Source`; okay-sql's data types are used
 * directly. JVM, with okay-jdbc for `Db.jdbc`.
 */
lazy val okayScala2Sql = (project in file("scala2/okay-scala2-sql"))
  .dependsOn(okayScala2, okayScala2Codec, okaySql.jvm, okayJdbc)
  .settings(
    name := "okay-scala2-sql",
  )

/**
 * the agent layer for Scala 2.13 (specs/scala2-facade.md, stage 9):
 * `Chat` over okay-agent's loop, `Model` (scripted or a provider),
 * `Tools`, `Policy`. okay-agent's `Turn` and `Reply` are used directly.
 */
lazy val okayScala2Agent = (project in file("scala2/okay-scala2-agent"))
  .dependsOn(okayScala2, okayScala2Codec, okayAgent.jvm, okayLlm.jvm)
  .settings(
    name := "okay-scala2-agent",
  )

/**
 * okay-ui for Scala 2.13 (specs/scala2-facade.md, stage 10): the loop as
 * an `Eff` (`UiApp`), hosts (`UiHost`, `ScriptedHost`). okay-ui's `Ui`,
 * `Event` and `Frame` are used directly.
 */
lazy val okayScala2Ui = (project in file("scala2/okay-scala2-ui"))
  .dependsOn(okayScala2, okayUi.jvm)
  .settings(
    name := "okay-scala2-ui",
  )

/**
 * WebSockets for Scala 2.13 (specs/scala2-facade.md, stage 12): a
 * client over okay-http's JDK transport, a server over okay-jetty,
 * sessions as folds. Its own module, so okay-scala2-http does not pull
 * Jetty.
 */
lazy val okayScala2Ws = (project in file("scala2/okay-scala2-ws"))
  .dependsOn(okayScala2Http, okayJetty)
  .settings(
    name := "okay-scala2-ws",
  )

/**
 * okay-resilience for Scala 2.13 (specs/scala2-facade.md, stage 15.1):
 * `Guards`, the pieces' program transformations over `Eff`. The pieces
 * themselves are okay-resilience's own, used directly.
 */
lazy val okayScala2Resilience = (project in file("scala2/okay-scala2-resilience"))
  .dependsOn(okayScala2, okayResilience.jvm)
  .settings(
    name := "okay-scala2-resilience",
  )

/**
 * okay-persist for Scala 2.13 (specs/scala2-facade.md, stage 15.2):
 * `Persist`, the defaults a Scala 2 caller cannot see and the streaming
 * reads. The engine API is okay-persist's own, used directly.
 */
lazy val okayScala2Persist = (project in file("scala2/okay-scala2-persist"))
  .dependsOn(okayScala2, okayPersist.jvm)
  .settings(
    name := "okay-scala2-persist",
  )

/**
 * okay-stm for Scala 2.13 (specs/scala2-facade.md, stage 15.3): `Tx`,
 * the transaction language as a capability, and `Stm.atomically`.
 * `TRef` is okay's own, used directly.
 */
lazy val okayScala2Stm = (project in file("scala2/okay-scala2-stm"))
  .dependsOn(okayScala2, okayStm.jvm)
  .settings(
    name := "okay-scala2-stm",
  )

/**
 * okay-cache, okay-blob and okay-docs for Scala 2.13
 * (specs/scala2-facade.md, stage 15.4): `Caches`, `Blobs`, `Documents`,
 * the stores' operations over Eff and Source. The stores themselves are
 * built with their own constructors.
 */
lazy val okayScala2Stores = (project in file("scala2/okay-scala2-stores"))
  .dependsOn(okayScala2, okayCache.jvm, okayBlob.jvm, okayDocs.jvm)
  .settings(
    name := "okay-scala2-stores",
  )

/**
 * okay-llm, okay-rag and okay-mcp for Scala 2.13
 * (specs/scala2-facade.md, stage 15.5): `Llm` (a completion as a
 * token stream, a typed value cut from it), `Rag` (vector and hybrid
 * retrieval with the embedder as a plain function), `McpClient` and
 * `McpServer` (JSON as text, tools from okay-scala2-agent's `Tools`).
 */
lazy val okayScala2Llm = (project in file("scala2/okay-scala2-llm"))
  .dependsOn(okayScala2, okayLlm.jvm)
  .settings(
    name := "okay-scala2-llm",
  )

lazy val okayScala2Rag = (project in file("scala2/okay-scala2-rag"))
  .dependsOn(okayScala2, okayScala2Sql, okayRag.jvm)
  .settings(
    name := "okay-scala2-rag",
  )

lazy val okayScala2Mcp = (project in file("scala2/okay-scala2-mcp"))
  .dependsOn(okayScala2, okayScala2Agent, okayMcp.jvm)
  .settings(
    name := "okay-scala2-mcp",
  )

/**
 * okay-optics for Scala 2.13 (specs/scala2-facade.md, stage 15.6):
 * `Iso`, `Lens`, `Prism`, `Affine`, `Traversal` as Scala 2 classes,
 * each a shell over okay's own optic and its operations.
 */
lazy val okayScala2Optics = (project in file("scala2/okay-scala2-optics"))
  .dependsOn(okayScala2, okayOptics.jvm)
  .settings(
    name := "okay-scala2-optics",
  )

/**
 * okay-workflow for Scala 2.13 (specs/scala2-facade.md, stage 15.7):
 * `Workflow[Q, A]`, a durable program's doors as an Eff capability,
 * and `Workflows`, okay's own drivers over a journal.
 */
lazy val okayScala2Workflow = (project in file("scala2/okay-scala2-workflow"))
  .dependsOn(okayScala2, okayWorkflow.jvm)
  .settings(
    name := "okay-scala2-workflow",
  )

/**
 * okay-actor, okay-outbox, okay-obs, okay-ops, okay-kafka and okay-pg for
 * Scala 2.13 (specs/scala2-facade.md, stage 15.8): `Actors`, `Outboxes`,
 * `Logs`, `Tracing`, `Operations`, `Kafkas`, `Postgres` — the operations
 * that answer programs; the builders are the libraries' own.
 */
lazy val okayScala2Services = (project in file("scala2/okay-scala2-services"))
  .dependsOn(okayScala2, okayScala2Sql, okayScala2Http, okayActor.jvm, okayOutbox.jvm, okayObs.jvm,
    okayOps.jvm, okayKafka, okayPg.jvm)
  .settings(
    name := "okay-scala2-services",
  )

/** Scala 3's stdlib for a Scala 2.13 project: resolved here and
 * placed BEHIND 2.13's by hand — see okay-scala2-probe */
lazy val Scala3Stdlib = config("scala3Stdlib").hide

/**
 * THE PROOF that okay-scala2 stays readable from Scala 2: suites
 * written in Scala 2.13 and compiled by scalac 2.13 in the ordinary
 * gate. Not published — it is a test, and the arrangement below is
 * the one the module's docs give a 2.13 user.
 *
 * Two standard libraries, and the ORDER is the finding (stage 0,
 * measured by hand 2026-09-22). Scala 3.9's stdlib is
 * `scala-library:3.9.0` and carries TASTy, not Scala 2 pickles, so
 * scalac 2.13 must read ITS OWN stdlib first — with 3.9's first it
 * stops at "Unsupported Scala 3 union in bounds of type T; found in
 * method wrapRefArray in class scala.LowPriorityImplicits". At RUN
 * time 3.9's is required (`scala.reflect.Enum` and the rest of the
 * Scala-3-only classes live nowhere else) and the order no longer
 * matters. The dependency on okay-scala2 would
 * bring `scala-library:3.9.0` along AHEAD of nothing in particular,
 * so it is EXCLUDED there and appended at the END of the classpath
 * instead — compile needs it too, behind 2.13.18 (without it: "could
 * not find package scala.annotation.internal whilst reading
 * annotation of package scala2"). The settings are EXACTLY what
 * docs/scala2.md tells a 2.13 user to write, so the page's snippet is
 * gated here. The `AsJars` line is not decoration: `sbt run` builds its
 * classpath from `dependencyClasspathAsJars`, and without it a consumer
 * build compiled and then died on `run`, forked or not, with
 * `NoClassDefFoundError: scala/reflect/Enum` (measured 2026-09-23 in a
 * consumer project against a `publishLocal`). The tests here are NOT
 * forked, so they run the way a user's `sbt test` does.
 *
 * `-Werror` because the gate's warning check reads Scala 3's
 * `[warn] -- [Exxx]` format and would not see a Scala 2 warning.
 */
lazy val okayScala2Probe = (project in file("scala2/okay-scala2/probe"))
  .dependsOn(okayScala2, okayScala2Codec, okayScala2Http, okayScala2Sql, okayScala2Agent, okayScala2Ui, okayScala2Ws, okayScala2Resilience, okayScala2Persist, okayScala2Stm, okayScala2Stores, okayScala2Llm, okayScala2Rag, okayScala2Mcp, okayScala2Optics, okayScala2Workflow, okayScala2Services)
  .settings(
    name := "okay-scala2-probe",
    publish / skip := true,
    scalaVersion := "2.13.18",
    // okay-scala2 brings scala-library 3.9.0 transitively; sbt refuses
    // a 2.13 compiler below the stdlib on its classpath (SIP-51) and
    // `allowUnsafeScalaLibUpgrade` only makes the 3.9 jar the COMPILE
    // stdlib, which is the failure above. Excluding it keeps 2.13.18.
    projectDependencies ~= (_.map(_.exclude("org.scala-lang", "scala-library"))),
    scalacOptions := Seq("-Ytasty-reader", "-deprecation", "-feature", "-Xlint", "-Werror"),
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
    // an in-memory database for the SQL suite, the one okay-jdbc tests on
    libraryDependencies += "com.h2database" % "h2" % "2.3.232" % Test,
    ivyConfigurations += Scala3Stdlib,
    libraryDependencies += "org.scala-lang" % "scala-library" % "3.9.0" % Scala3Stdlib,
    Seq(Compile, Runtime, Test).flatMap(c => Seq(
      c / dependencyClasspath ++= Classpaths.managedJars(Scala3Stdlib, Set("jar"), update.value),
      c / dependencyClasspathAsJars ++= Classpaths.managedJars(Scala3Stdlib, Set("jar"), update.value))),
  )

/** interop with fs2: Stream <-> Chunks, chunk for chunk (P3) */
lazy val okayFs2 = (project in file("okay-fs2"))
  .dependsOn(okay.jvm, okayStream.jvm, compare % "test->compile")
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
  .dependsOn(okay, okayStream)
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
  .dependsOn(okay.jvm, okayStream.jvm)
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
  // leg of the consensus battery (specs/consensus.md).
  //
  // okay-cluster is TEST->TEST ONLY, and the direction is deliberate:
  // the dataflow engine must not know what a Kafka is (its compile
  // graph stops at okay-codec, and `Checkpoint` is two methods over
  // bytes on purpose). What borrows is the staging BATTERY —
  // `StagingTopicSuite` — so stage 11's last box, the same
  // exactly-once run against a real broker, asserts the same things
  // as the memory run rather than a second thing that looks alike.
  .dependsOn(okay.jvm, okayPersist.jvm % "compile->compile;test->test",
             okayCluster.jvm % "test->test")
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
/**
 * How a Spark test JVM has to be started here — shared by okay-spark and
 * okay-scalus-spark so the two cannot drift. Each line's reason is the
 * comment it carried in okay-spark's settings, where they were first
 * found (legacyStdlib order for Spark's Scala 2 reflection, the forked
 * JDK 25 test JVM, the add-opens Spark needs on 17+).
 */
lazy val sparkTestSettings: Seq[Setting[_]] = Seq(
    ivyConfigurations += LegacyStdlib,
    libraryDependencies += "org.scala-lang" % "scala-library" % "2.13.18" % LegacyStdlib,
    Test / unmanagedJars ++= Classpaths.managedJars(LegacyStdlib, Set("jar"), update.value),
    Test / fork := true,
    // jdk26-default-runtime: shadow the build-wide JDK26 Test/run
    // default back down to Spark 4.2.0's own confirmed range
    // (spark-jdk25-guard-fix) -- 25 rather than .sdkmanrc's ambient
    // 21, since 25 is verified end to end and strictly newer.
    Test / javaHome := {
      val jdk25 = file(System.getProperty("user.home")) / ".sdkman" / "candidates" / "java" / "25.0.4.1-tem"
      if (jdk25.exists) Some(jdk25) else None
    },
    run / javaHome := (Test / javaHome).value,
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
    )
)

lazy val okaySpark = (project in file("okay-spark"))
  // okay-codec for `Schema` (SparkSchema: the DataFrame encoder is a fold of it)
  .dependsOn(okay.jvm, okayStream.jvm, okayCodec.jvm, compare % "test->compile")
  .settings(
    name := "okay-spark",
    libraryDependencies ++= Seq(
      ("org.apache.spark" %% "spark-sql" % "4.2.0").cross(CrossVersion.for3Use2_13),
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
    // Spark's 2.13 artifacts bring scala-reflect, a Scala 2 artifact
    // published for NO Scala 3 version. The dependency tree resolves
    // it correctly (2.13.16); what fails is sbt asking for it at the
    // project's own Scala version. Naming the 2.13 artifact
    // explicitly settles it before anything can rewrite the version.
    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.18",
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
     * Spark 4.2.0 was tried against this fix WITHOUT bumping the pair
     * (spark-4-2-0-jdk25, 2026-09-19) and DID reproduce "same error,
     * same line" — because only the `scala-reflect` pin three lines up
     * had moved to 2.13.18, and this jar's own pin was still 2.13.16:
     * exactly the "mixed pair" this comment already warned against.
     * Bumping BOTH to 2.13.18 together passes all of TestSparkInterop
     * on this box's JDK 21. Spark is still 2.13-only at 4.2.0 (that
     * part holds), but the earlier "changes NOTHING" verdict was
     * itself the mixed-pair mistake, not a fact about Spark 4.2.0.
     *
     * This is a deliberate two-stdlib classpath in ONE module's tests.
     * It is legitimate because the two jars are the same library
     * compiled twice, and it is confined because nothing but this test
     * fork sees it. If Spark ever publishes for Scala 3, delete all of
     * it — the config, the jar, and this comment.
     */
    sparkTestSettings,
  )

/** Flink via the same Aggregator triple (P4); flink-core is pure Java */
/**
 * okay-scalus (specs/scalus.md, specs/chain.md): the Cardano chain as
 * okay-chain events. scalus-cardano-ledger is taken for its ledger
 * MODEL and CBOR codecs only (operator, 2026-09-22); the transport —
 * Ouroboros node-to-node: mux, handshake, chain-sync, block-fetch,
 * keep-alive — is written here. JVM: scalus's crypto (blake2b) is
 * bouncycastle on this platform, and the transport is a socket.
 */
lazy val okayScalus = (project in file("okay-scalus"))
  .dependsOn(okay.jvm, okayCodec.jvm, okayChain.jvm)
  .settings(
    name := "okay-scalus",
    libraryDependencies ++= Seq(
      "org.scalus" %% "scalus-cardano-ledger" % "1.2.0",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
  )

/**
 * okay-scalus-spark (specs/scalus.md §6): `spark.read(Stream).format("cardano")`
 * — okay-scalus's follower and CardanoTables as a Spark DataSource V2.
 * Adds no table logic: the rows are CardanoTables', their shape
 * Columns', their Spark types SparkSchema's.
 */
lazy val okayScalusSpark = (project in file("okay-scalus-spark"))
  .dependsOn(okayScalus % "compile->compile;test->test", okaySpark, okayPersist.jvm)
  .settings(
    name := "okay-scalus-spark",
    libraryDependencies ++= Seq(
      ("org.apache.spark" %% "spark-sql" % "4.2.0").cross(CrossVersion.for3Use2_13),
      "org.scala-lang" % "scala-reflect" % "2.13.18",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
    sparkTestSettings,
  )

/** the Flink test JVM, shared by okay-flink and okay-scalus-flink (a forked
 * JVM with the heap and the add-opens Flink's MiniCluster needs) */
lazy val flinkTestSettings: Seq[Setting[_]] = Seq(
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
    )
)

/**
 * okay-scalus-flink (specs/scalus.md §7): the Cardano chain as a Flink
 * FLIP-27 source over okay-scalus's follower and CardanoTables; its row
 * type is okay-flink's FlinkSchema over okay-codec's Columns.
 */
lazy val okayScalusFlink = (project in file("okay-scalus-flink"))
  .dependsOn(okayScalus % "compile->compile;test->test", okayFlink)
  .settings(
    name := "okay-scalus-flink",
    libraryDependencies ++= Seq(
      "org.apache.flink" % "flink-core" % "1.20.0",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
      "org.apache.flink" % "flink-streaming-java" % "1.20.0" % Test,
      "org.apache.flink" % "flink-clients" % "1.20.0" % Test,
    ),
    flinkTestSettings,
  )

lazy val okayFlink = (project in file("okay-flink"))
  // okay-java is TEST only, and only for §20's third lane: the same
  // job over java.util.stream, whose `Collector` an okay Aggregator
  // already is (okay-java's Collect.collector). okay-codec for `Schema`/
  // `Columns`: FlinkSchema translates the engine-free columns.
  .dependsOn(okay.jvm, okayCodec.jvm, okayJava % Test, compare % "test->compile")
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
    flinkTestSettings,
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
    // jdk26-default-runtime, 2026-09-19: found by the first full-matrix
    // run on JDK26 -- delta-kernel resolves a path through Hadoop's
    // Configuration/UserGroupInformation the same as Spark does, so it
    // hits the identical JEP 486 wall (Security Manager gone, JDK
    // 24+): "KernelEngineException: ... getSubject is not supported".
    // A different library than okaySpark's, the same root cause and
    // the same shadow-back-down fix; unlike Spark, delta-kernel 4.4.0
    // has no similar upstream JDK25 fix found, so this pins to 21
    // rather than assuming 25 also works. It pinned to the AMBIENT JDK
    // (None) while that was 21; java-gatherers (2026-09-23) moved the
    // ambient to 25 and TestDelta failed on it at once (getSubject), so
    // the 21 is named now instead of inherited.
    Test / javaHome := (if (jdk21Home.exists) Some(jdk21Home) else None),
    run / javaHome := (Test / javaHome).value,
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

/**
 * JavaScript as a VALUE (specs/js.md): a typed tree, a printer, and
 * a macro that emits the printed text as a compile-time constant.
 *
 * NOT a Scala-to-JavaScript compiler — that is Scala.js, which this
 * build already cross-compiles with. Nothing here translates Scala
 * semantics; the author writes the JavaScript's structure and the
 * printer writes the text, which is why this is a few hundred lines
 * and not a backend. Pure string building, so it cross-builds
 * everywhere and its tests run on JS and Native too.
 */
/**
 * TypeScript programs INSIDE okay, on Scala.js (specs/typescript.md,
 * stage 2): a TS program built from done/perform/then objects is walked
 * in the same JavaScript runtime, its named operations okay callbacks; and
 * an okay program handed to TypeScript as a Promise. JS only: there is no
 * TypeScript runtime on the JVM to share (the JVM road is okay-py's
 * TsWorker, stage 1).
 */
lazy val okayTs = crossProject(JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-ts"))
  .dependsOn(okay, okayCodec, okayAsync)
  .settings(
    name := "okay-ts",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )

/** the npm package of okay-ts-npm: its directory, built by `npmPackage` */
lazy val npmPackage = taskKey[File]("okay-ts-npm as an npm package directory: the ES module, package.json, and the index.d.ts the module writes of itself (typescript-types T9)")

/**
 * okay for TypeScript projects, as an npm package (typescript-types T9):
 * okay-ts, okay-crdt and okay-stream's channels behind an ES module whose
 * `index.d.ts` the module itself writes, from the same Schemas that
 * encode its values. `npmPackage` links it (fullLinkJS), copies the module
 * and npm/package.json into target/npm, and asks Node for the module's
 * `declarations` export to write index.d.ts. ESModule, unlike okay-ts:
 * a package a bundler or Node `import`s is an ES module.
 */
lazy val okayTsNpm = crossProject(JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-ts-npm"))
  .dependsOn(okayTs, okayCrdt, okayStream)
  .settings(
    name := "okay-ts-npm",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .jsSettings(
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule)),
    npmPackage := {
      val report = (Compile / fullLinkJS).value.data
      val linked = (Compile / fullLinkJS / scalaJSLinkerOutputDirectory).value
      val pkg = target.value / "npm"
      IO.delete(pkg)
      IO.createDirectory(pkg)
      report.publicModules.foreach(m => IO.copyFile(linked / m.jsFileName, pkg / m.jsFileName))
      val source = baseDirectory.value.getParentFile / "npm"
      IO.copyFile(source / "package.json", pkg / "package.json")
      IO.copyFile(source / "README.md", pkg / "README.md")
      // a FILE with a static import: a dynamic `import()` from `node -e`
      // never settled here (Node 26, exit 13), while the same module
      // imported statically loads at once
      val writer = target.value / "declarations.mjs"
      IO.write(writer, "import { declarations } from \"./npm/main.js\";\nprocess.stdout.write(declarations);\n")
      val declarations = scala.sys.process.Process(Seq("node", writer.getAbsolutePath)).!!
      IO.write(pkg / "index.d.ts", declarations)
      pkg
    },
  )

lazy val okayJs = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-js"))
  .settings(
    name := "okay-js",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.1.1" % Test,
    ),
  )

/** streaming tokenization: pure-state scanners, total, incremental
 * (P5); pure Scala — cross-built, tests run on JS too */
lazy val okayLex = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-lex"))
  // test->test borrows okay's ArrowLaws for Mealy's instance, which is
  // the only Arrow in the tree until optics-arrow-instances and
  // static-workflow-proc add theirs (specs/arrows-plan.md, Decision 2:
  // one law suite, no carrier writes its own). The suite lives in
  // src/test/scala-cross BECAUSE of this line: src/test/scala is the
  // JVM's alone, so a shared suite put there compiles for okay-lex's
  // JVM and leaves its JS and Native tests with no `okay.laws` at all
  // — measured, as a cyclic-import error, before it was moved.
  .dependsOn(okay % "compile->compile;test->test", okayStream % "compile->compile;test->test", okayOptics % "compile->compile;test->test")
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
/**
 * okay-chain (specs/chain.md): blockchains and ledgers read uniformly
 * — CAIP identifiers, a sans-I/O follower (`Tracker` for push sources,
 * `Poller` for poll sources) and the `Ledger` projection. Abstractions
 * only: no chain's source lives here. Pure, so JVM + JS + Native.
 */
lazy val okayChain = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-chain"))
  .dependsOn(okay, okayCodec)
  .settings(
    name := "okay-chain",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.1.1" % Test,
    ),
  )

lazy val okayCrdt = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-crdt"))
  // okay for `Hlc` and `Uid`; okay-codec so a replica ships as data
  // (`Wire`). Both are JVM + JS + Native, so nothing narrows.
  .dependsOn(okay, okayData, okayCodec)
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
  .dependsOn(okayParse, okay, okayStream, okayOptics)
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
  // the foreign engines live here too, JVM-only (src/*/scala-jvm):
  // Mongo (mongodb-driver-sync), DynamoDB (the JSON protocol over the
  // one http client, signed by okay-blob's SigV4 — no AWS SDK) and
  // Cassandra (the Apache java driver, LWT as the CAS). They were
  // three satellites until docs-adapters-merge (2026-09-23); the
  // price of one module is that okay-docs on the JVM carries both
  // vendor drivers. okayPg/okaySql in Test: the end-to-end suite
  // (persistence-e2e) runs a Pool over the pg wire and a Saga over
  // DynamoDocs side by side. Their DocsSuite contracts are Live.
  .jvmConfigure(_.dependsOn(okayBlob.jvm, okayHttp.jvm, okaySql.jvm % Test, okayPg.jvm % Test))
  .settings(
    name := "okay-docs",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .jvmSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
    libraryDependencies ++= Seq(
      "org.mongodb" % "mongodb-driver-sync" % "5.2.1",
      "org.apache.cassandra" % "java-driver-core" % "4.18.1",
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
  .dependsOn(okay, okayWorkflow, okayCodec)
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
  .dependsOn(okayCodec, okayDirect)
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
  // okay-docs joins in TEST scope too, and for one reason: stage 10's
  // last box wanted a COMPARE-AND-SET commit and said "no store here
  // offers one". One does — `Cond.IfVersion` — and the seam
  // (`Fencing`) is only worth having if something real can implement
  // it. `DocsJournal` in the test tree is that something; the engine
  // still knows nothing about documents.
  .jvmConfigure(_.dependsOn(okayPersist.jvm % Test, okayDocs.jvm % Test))
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
  .dependsOn(okayHttp, okayData, okayCrypto)   // the four primitives are okay-crypto's (security-crypto-dedup)
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
  // okay-js prints the browser client's style-token table (Classes)
  // instead of it being typed a second time in JavaScript — the
  // module is pure string building and carries no dependency itself
  .dependsOn(okayJs)
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
/**
 * Rust kernels for okay (specs/polyglot-rust.md): a Rust crate over the C
 * ABI (okay-rust/kernels/<name>, Cargo.lock checked in, builds offline), bound
 * through FFM and offered as an okay EFFECT whose operations are the
 * kernel's calls. JDK 22 floor: FFM is final there (JEP 454). The tests
 * fork with native access enabled, which JDK 24+ otherwise warns about
 * at the first restricted call.
 */
lazy val okayRust = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-rust"))
  .dependsOn(okay)
  .settings(
    name := "okay-rust",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .jvmConfigure(_.dependsOn(okaySecurityArgon2 % Test))
  .jvmSettings(
    jdkFloor(22),
    // Chicory: a WebAssembly runtime in pure Java, and its WASI — the road
    // with no native code in the process (specs/polyglot-rust.md stage 3)
    libraryDependencies ++= Seq(
      "com.dylibso.chicory" % "runtime" % "1.7.5",
      "com.dylibso.chicory" % "wasi" % "1.7.5",
    ),
    Test / fork := true,
    Test / javaOptions += "--enable-native-access=ALL-UNNAMED",
  )
  // stage 2: the crate's STATICLIB linked into a Native binary. The path is
  // an environment variable because the library is built by cargo, not by
  // sbt — scripts/rust-native-check.sh builds it and names it here. A full
  // path, not `-l`: on macOS `-l` would pick the .dylib beside it. Not in
  // the root aggregate for the same reason: the default gate has no cargo.
  .nativeSettings(
    nativeConfig ~= { c =>
      c.withLinkingOptions(c.linkingOptions ++ sys.env.get("OKAY_RUST_ARGON2_LIB").toSeq)
    },
  )

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
  // okay-agent for TESTS only: its Durable journals these operations
  // through their own `Journalled` instances (foreign-journalled)
  .dependsOn(okay.jvm, okayCodec.jvm, okayStream.jvm, okayAgent.jvm % Test)
  .settings(
    name := "okay-py",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )

// okay-r: R as a handler (specs/r.md) — the same shape okay-py built
// first, with R's own two absences. okay-agent is deliberately NOT a
// dependency: Durable journals R steps because they are operations,
// not because the modules know each other.
lazy val okayR = (project in file("okay-r"))
  // okay-agent for TESTS only: its Durable journals these operations
  // through their own `Journalled` instances (foreign-journalled)
  .dependsOn(okay.jvm, okayCodec.jvm, okayStream.jvm, okayAgent.jvm % Test)
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
    // dotc's classfile target is JDK17 (major 61) regardless of the
    // host JDK compiling it; javac's is NOT -- it defaults to the
    // launching JVM's own version, so this module's one Java source
    // (Listen.java) silently outran the rest of the project's floor
    // whenever sbt itself ran on 21+ (jdk17-adaptive-runtime,
    // UnsupportedClassVersionError on Listen$Sink, class file version
    // 65 vs the 61 every Scala-compiled class here already emits).
    javacOptions ++= Seq("--release", "17"),
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
  // okay-codec: a Watched document is Json addressed by the dotted
  // keys JsonOptic.path resolves against a Schema (specs/optics-outside.md stage 10)
  // okay-http: LiveHttp serves a Watched as server-sent events and a
  // POST (typescript-types T11)
  .dependsOn(okay.jvm, okayStream.jvm, okayCodec.jvm, okayHttp.jvm)
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

/**
 * okay-x402 (specs/x402.md): paying for an HTTP resource with a chain
 * payment — x402 v2's objects and headers, a 402 gate for okay-http
 * routes over a facilitator, and a paying client. Networks, assets and
 * amounts are okay-chain's.
 */
lazy val okayX402 = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-x402"))
  // okay-persist for the payment journal a budget and a replay record are
  // folded from (stage 4); okay-conf for the settings file with secrets
  // as references
  .dependsOn(okayHttp, okayChain, okayPersist, okayConf)
  .settings(
    name := "okay-x402",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )

/**
 * okay-x402-mcp (specs/x402.md stage 3): x402 over MCP — the 402 gate
 * as an `Around` for okay-mcp's server, and a paying `Session`. A
 * satellite so okay-x402 does not pull the agent runtime okay-mcp
 * brings, and okay-mcp knows nothing of payments.
 */
lazy val okayX402Mcp = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-x402-mcp"))
  .dependsOn(okayX402, okayMcp)
  .settings(
    name := "okay-x402-mcp",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )
  .jvmSettings(
    // client and server over a real (in-memory) wire, as okay-mcp's own
    // session tests: a blocking `runWith` the JS runtime has not got
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
  )

/**
 * okay-x402-signers (specs/x402.md stage 4c): Circle developer-controlled
 * wallets, Turnkey and Web3Signer (or Clef) as x402 signers — okay-http
 * and the JDK's RSA and P-256, no provider SDKs. JVM.
 */
lazy val okayX402Signers = (project in file("okay-x402-signers"))
  .dependsOn(okayX402Evm, okayConf.jvm)
  .settings(
    name := "okay-x402-signers",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )

/**
 * okay-x402-cdp (specs/x402.md stage 4b): x402 payments signed by a
 * Coinbase CDP Server Wallet — the EIP-712 typed data sent to CDP, the key
 * in CDP's enclave. JVM, on okay-x402-evm (whose BouncyCastle gives the
 * ASN.1 the JWTs need; the signatures are the JDK's).
 */
lazy val okayX402Cdp = (project in file("okay-x402-cdp"))
  .dependsOn(okayX402Evm, okayConf.jvm)
  .settings(
    name := "okay-x402-cdp",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )

/**
 * okay-x402-evm (specs/x402.md stage 2a): verifying x402's `exact` scheme
 * on EVM locally — keccak-256, secp256k1 recovery, EIP-712 — so a server
 * need not take a facilitator's word for a signature. A JVM satellite
 * because BouncyCastle is its crypto, as okay-security-argon2 carries its
 * own; okay-x402 stays dependency-free and cross-built.
 */
lazy val okayX402Evm = (project in file("okay-x402-evm"))
  .dependsOn(okayX402.jvm)
  .settings(
    name := "okay-x402-evm",
    libraryDependencies += "org.bouncycastle" % "bcprov-jdk18on" % "1.78.1",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
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
    // Server's executor is Loom behind `Schedulers.hasVirtualThreads`,
    // the okay-platform pattern: bytecode 61 is what lets it load on 17
    jdkFloor(0),
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
  // okayCluster + okayBlob: the one-binary story (`Ledger`) — record,
  // report, page, backup, in one process
  .dependsOn(okayAgent.jvm, okayIntent.jvm, okayMcp.jvm, okayUi.jvm, okayJetty, okayJdbc, okayPg.jvm, okaySecurity.jvm, okaySubscription, okayOps.jvm, okayResilience.jvm, okayAdmin, okayChat, okayLive, okayDeploy, okayOpenapi, okayCluster.jvm, okayBlob.jvm)
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
  // okayScript: the mobile-web proof drives a Live page (ui-mobile).
  // test->test as well since script-storefront-look: the storefront
  // fixture those pages ARE lives in okay-script's test resources, and
  // a browser proof of it must read the same files the unit tests do
  // rather than a copy that can drift.
  .dependsOn(okayDemo, okayScript % "compile->compile;test->test")
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
    // INTEGRATION-ONLY (operator, 2026-09-23): TestGtk drives a real GTK
    // widget tree through a linked Native binary, and twice in one day the
    // gate hung at the Native runner's handshake with it — the binaries at
    // 0.0% CPU, sbt's ComRunner blocked in a socket read, sixteen minutes
    // until a human looked (backlog gate-watchdog-idle-sbt-cpu). Tagging the
    // suite `Live` alone is not enough: munit builds the suite to list its
    // tests, and the suite calls `Gtk.init()` as it is built, inside the
    // binary. So the gate's `test` here COMPILES the tests — the warning
    // check still sees them — and runs nothing; `integrationTest` runs the
    // suite (Live-tagged) by name. specs/integration-test-gate.md.
    Test / test := {
      val _ = (Test / compile).value
      streams.value.log.info("okay-ui-gtk: tests compiled; they RUN under `sbt integrationTest` (a real GTK display)")
    },
  )
lazy val gtkProjects: Seq[ProjectReference] = if (gtkAvailable) Seq(okayUiGtk) else Seq.empty

lazy val root = (project in file("."))
  .aggregate(gtkProjects: _*)
  .aggregate(okay.jvm, okay.js, okay.native, okayAsync.jvm, okayAsync.js, okayAsync.native, okayDirect.jvm, okayDirect.js, okayDirect.native, okayPlatform.jvm, okayPlatform.js, okayPlatform.native, okayStream.jvm, okayStream.js, okayStream.native, okayWorkflow.jvm, okayWorkflow.js, okayWorkflow.native, okayData.jvm, okayData.js, okayData.native, okayOptics.jvm, okayOptics.js, okayOptics.native, okayStm.jvm, okayStm.js, okayStm.native, okayStaging, okayCats, okayZio, okayKyo, okayFs2, okayReactive, okayActor.jvm, okayActor.js, okayActor.native, okayKafka,
    okayJava, okayClojure, okayFrege, okayScala2, okayScala2Codec, okayScala2Http, okayScala2Sql, okayScala2Agent, okayScala2Ui, okayScala2Ws, okayScala2Resilience, okayScala2Persist, okayScala2Stm, okayScala2Stores, okayScala2Llm, okayScala2Rag, okayScala2Mcp, okayScala2Optics, okayScala2Workflow, okayScala2Services, okayScala2Probe, okaySpark, okayFlink, okayJdbc, okayR2dbc, okayDelta,
    okayLex.jvm, okayLex.js, okayLex.native, okayCrdt.jvm, okayCrdt.js, okayCrdt.native, okayChain.jvm, okayChain.js, okayChain.native, okayScalus, okayScalusSpark, okayScalusFlink, okayX402.jvm, okayX402.js, okayX402Evm, okayX402Cdp, okayX402Signers, okayX402Mcp.jvm, okayX402Mcp.js,
    okayParse.jvm, okayParse.js, okayParse.native,
    okayCodec.jvm, okayCodec.js, okayCodec.native, okayLlm.jvm, okayLlm.js,
    okayPersist.jvm, okayPersist.js, okayPersist.native,
    okaySql.jvm, okaySql.js, okaySql.native, okayPg.jvm, okayPg.js,
    okayCrypto.jvm, okayCrypto.js, okayMail,
    okayCache.jvm, okayCache.js, okayCache.native,
    okayDocs.jvm, okayDocs.js, okayDocs.native,
    okayConf.jvm, okayConf.js, okayConf.native,
    okayObs.jvm, okayObs.js, okayObs.native,
    okayBlob.jvm, okayBlob.js, okayBlob.native, okayTls, okayPy, okayR,
    okaySecurity.jvm, okaySecurity.js, okaySecurityArgon2, okayRust.jvm,
    okayFrame.jvm, okayFrame.js,
    okayAgent.jvm, okayAgent.js, okayIntent.jvm, okayIntent.js, okayChatWeb.jvm, okayChatWeb.js, okayLangchain4j, okayRag.jvm, okayRag.js, okayDemo, okaySubscription, okayAdmin, okayChat, okayDeploy, okayLive, okayScript,
    okayMcp.jvm, okayMcp.js, okayUi.jvm, okayUi.js, okayUi.native,
    okayHttp.jvm, okayHttp.js, okayJetty, okayNetty,
    okayResilience.jvm, okayResilience.js,
    okayOutbox.jvm, okayOutbox.js, okayOutbox.native,
    okayCluster.jvm, okayCluster.js,
    // okay-js and okay-acme are COMPILE dependencies of published
    // modules (okay-ui on every platform, okay-script), and they were
    // missing from this list, so `publishLocal` never published them
    // and a consumer of okay-ui or okay-script failed to resolve
    // (pom-jmh-and-chat-version, 2026-09-23: the chat guide's own
    // build stopped at "Error downloading dev.okay:okay-js_3:0.1.1").
    // okay-acme's network suites are Live-tagged, so the default gate
    // runs none of them.
    okayJs.jvm, okayJs.js, okayJs.native, okayTs.js, okayTsNpm.js, okayAcme,
    // five more that were simply never listed (root-aggregate-unlisted,
    // 2026-09-23), and it had cost two of them already: okay-spring no
    // longer COMPILED (Async left the core on 2026-09-18 and nothing
    // rebuilt it), and okay-ops' JS tests did not LINK (a shared test
    // read a file through java.nio). Their network suites are
    // Live-tagged. The other two unlisted modules, okay-docs-dynamo and
    // okay-docs-cassandra, no longer exist: docs-adapters-merge moved
    // them into okay-docs.
    okayOps.jvm, okayOps.js, okaySpring, okayGuice, okayCdi, okayOpenapi,
    compare)
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
    okayStaging,                   // staged-runtime: the run-time staged codec beside the compile-time one
    okayData.jvm, okayStm.jvm)     // SketchBenchmark, StmBenchmark: core-modularise moved Sketch and
                                    // Stm/Tx/TRef out of `okay` without adding the two modules here
                                    // (compare-jmh-missing-deps, 2026-09-19) — compare/Jmh/compile has
                                    // been broken on master since that migration, unrelated to any one
                                    // benchmark added after it
  .enablePlugins(JmhPlugin)
  .settings(
    // the benchmark harness calls JDK 21 API unconditionally (Loom
    // samplers, Thread.threadId) and never runs below 21 (java-gatherers)
    jdkFloor(21),
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
      "com.softwaremill.ox" %% "core" % "1.0.7",
    ),
  )

/** The Spring Boot bridge of specs/di.md (stage 2): a Module's
 * installed values as singletons in a Spring context, closed with it;
 * a Spring bean as a module; a controller returning `A ! Async` served
 * through Spring's ReactiveAdapterRegistry, wired by a Boot
 * auto-configuration. JVM; the Boot test scope runs the
 * auto-configuration under ApplicationContextRunner. */
lazy val okaySpring = (project in file("okay-spring"))
  // okayAsync/okayPlatform: `A ! Async` and its JVM runner. Async left
  // the core in core-modules (2026-09-18) and this module, outside the
  // root aggregate, was never recompiled to notice (root-aggregate-unlisted)
  .dependsOn(okay.jvm, okayAsync.jvm, okayPlatform.jvm)
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
