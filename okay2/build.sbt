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
 */
ThisBuild / organization := "dev.okay"
ThisBuild / scalaVersion := "2.13.18"

lazy val okay2 = (project in file("."))
  .settings(
    name := "okay2",
    scalacOptions := Seq("-deprecation", "-feature", "-Xlint", "-Werror", "-language:higherKinds"),
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )
