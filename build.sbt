ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.1"

ThisBuild / scalacOptions += "-Xkind-projector"

lazy val root = (project in file("."))
  .settings(
    name := "okay",
    idePackagePrefix := Some("okay"),
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )
