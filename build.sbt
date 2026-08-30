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
  )
  .jsSettings(
    Test / unmanagedSourceDirectories := Seq(),
  )
  .nativeSettings(
    Test / unmanagedSourceDirectories := Seq(),
  )

lazy val root = (project in file("aggregate"))
  .aggregate(okay.jvm, okay.js, okay.native, compare)
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
