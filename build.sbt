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
 * source tree (src/main/scala) for JVM, JS and Native; the blocking
 * side (Async runtime, Fiber, Scheduler, Channel) lives in
 * src/main/scala-jvm until the Await-based runners land per
 * specs/cross-platform-async.md. Tests run on the JVM.
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
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "test" / "scala-jvm",
    Jmh / sourceDirectory := baseDirectory.value.getParentFile / "src" / "jmh",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
    libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "1.1.0" % Test,
  )
  .jsSettings(
    Test / unmanagedSourceDirectories := Seq(),
  )
  .nativeSettings(
    Test / unmanagedSourceDirectories := Seq(),
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

lazy val root = (project in file("aggregate"))
  .aggregate(okay.jvm, okay.js, okay.native, okayCats, okayZio, okayKyo, okayFs2, okayKafka,
    okaySpark, okayFlink, okayJdbc, compare)
  .settings(
    name := "okay-root",
    publish / skip := true,
  )

/** comparison benchmarks against the ecosystem: the heavy dependencies live here */
lazy val compare = (project in file("compare"))
  .dependsOn(okay.jvm)
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
    ),
  )
