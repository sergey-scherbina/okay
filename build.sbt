import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.7.1"
ThisBuild / scalacOptions ++= Seq("-Xkind-projector", "-Wall")

ThisBuild / organization := "io.sergiy-shcherbyna"
ThisBuild / licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / homepage := Some(url("https://github.com/sergey-scherbina/okay"))
ThisBuild / versionScheme := Some("early-semver")

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
    // like JS: the cross suite is the native test source
    Test / unmanagedSourceDirectories :=
      Seq(baseDirectory.value.getParentFile / "src" / "test" / "scala-cross"),
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
  .dependsOn(okay.jvm)
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
  .dependsOn(okay.jvm)
  .settings(
    name := "okay-kyo",
    libraryDependencies ++= Seq(
      "io.getkyo" %% "kyo-core" % "0.16.2",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
  )

/** interop with fs2: Stream <-> Chunks, chunk for chunk (P3) */
lazy val okayFs2 = (project in file("okay-fs2"))
  .dependsOn(okay.jvm)
  .settings(
    name := "okay-fs2",
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % "3.10.2",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
  )

/** Kafka as chunked async streams: one poll, one chunk (P4) */
lazy val okayKafka = (project in file("okay-kafka"))
  .dependsOn(okay.jvm)
  .settings(
    name := "okay-kafka",
    libraryDependencies ++= Seq(
      "org.apache.kafka" % "kafka-clients" % "3.9.0",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
  )

/** Spark via the Aggregator triple (P4); Spark ships for 2.13 only,
 * so the standard for3Use2_13 cross applies */
lazy val okaySpark = (project in file("okay-spark"))
  .dependsOn(okay.jvm)
  .settings(
    name := "okay-spark",
    libraryDependencies ++= Seq(
      ("org.apache.spark" %% "spark-sql" % "4.0.0").cross(CrossVersion.for3Use2_13),
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
    Test / fork := true,
    Test / javaOptions ++= Seq(
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
  .dependsOn(okay.jvm)
  .settings(
    name := "okay-flink",
    libraryDependencies ++= Seq(
      "org.apache.flink" % "flink-core" % "1.20.0",
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
  )

/** JDBC as chunked async streams under the Resource region (P4) */
lazy val okayJdbc = (project in file("okay-jdbc"))
  .dependsOn(okay.jvm)
  .settings(
    name := "okay-jdbc",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.1.1" % Test,
      "com.h2database" % "h2" % "2.3.232" % Test,
    ),
  )

/** streaming tokenization: pure-state scanners, total, incremental
 * (P5); pure Scala — cross-built, tests run on JS too */
lazy val okayLex = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-lex"))
  .dependsOn(okay)
  .settings(
    name := "okay-lex",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )

/** streaming error-tolerant parsing: total, lossless, two surfaces (P5) */
lazy val okayParse = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-parse"))
  .dependsOn(okayLex)
  .settings(
    name := "okay-parse",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )

/** codecs: the Schema algebra and the dialects (P5) */
lazy val okayCodec = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-codec"))
  .dependsOn(okayParse)
  .settings(
    name := "okay-codec",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )

/** language models as streams: the thin client (P4/llm.md) */
lazy val okayLlm = (project in file("okay-llm"))
  .dependsOn(okayCodec.jvm)
  .settings(
    name := "okay-llm",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
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
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
  )

/** agents as programs: tool calls are operations, the conversation
 * is a fold, policy lives in handlers (P9) */
lazy val okayAgent = (project in file("okay-agent"))
  .dependsOn(okayLlm)
  .settings(
    name := "okay-agent",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )

/** the own distributed runtime, assembled from existing parts (P7);
 * cross-built: the JVM side holds Remote/Cluster, the JS side the
 * Node client of the acceptance run, the shared tree the ONE program
 * both ends compile (specs/cluster.md) */
lazy val okayCluster = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("okay-cluster"))
  .dependsOn(okayCodec)
  .settings(
    name := "okay-cluster",
  )
  .jvmSettings(
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-jvm",
    // the acceptance test runs `node <linked client>` against a local server
    Test / fork := true,
    Test / javaOptions += {
      val client = baseDirectory.value.getParentFile / ".js" / "target" /
        ("scala-" + scalaVersion.value) / "okay-cluster-fastopt" / "main.js"
      s"-Dokay.client.js=${client.getAbsolutePath}"
    },
    Test / test := (Test / test)
      .dependsOn(LocalProject("okayClusterJS") / Compile / fastLinkJS).value,
  )
  .jsSettings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-js",
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
    Test / sources := Seq(),
    Test / test := {},
  )

lazy val root = (project in file("."))
  .aggregate(okay.jvm, okay.js, okay.native, okayCats, okayZio, okayKyo, okayFs2, okayKafka,
    okaySpark, okayFlink, okayJdbc, okayLex.jvm, okayLex.js, okayParse.jvm, okayParse.js,
    okayCodec.jvm, okayCodec.js, okayLlm, okayAgent, okayRag.jvm, okayRag.js,
    okayCluster.jvm, okayCluster.js, compare)
  .settings(
    name := "okay-root",
    publish / skip := true,
    Compile / sources := Seq(),
    Test / sources := Seq(),
  )

/** comparison benchmarks against the ecosystem: the heavy dependencies live here */
lazy val compare = (project in file("compare"))
  .dependsOn(okay.jvm, okayLlm)
  .enablePlugins(JmhPlugin)
  .settings(
    name := "okay-compare",
    publish / skip := true,
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-free" % "2.12.0",
      "org.typelevel" %% "cats-effect" % "3.5.7",
      "dev.zio" %% "zio" % "2.1.14",
      "io.getkyo" %% "kyo-core" % "0.16.2",
      "org.atnos" %% "eff" % "7.0.4",
      "co.fs2" %% "fs2-core" % "3.10.2",
      "dev.zio" %% "zio-streams" % "2.1.14",
      "io.circe" %% "circe-parser" % "0.14.10",
      "io.circe" %% "circe-generic" % "0.14.10",
    ),
  )
