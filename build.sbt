ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.2"

ThisBuild / scalacOptions ++= Seq("-Xkind-projector", "-Wall")

lazy val root = (project in file("."))
  .enablePlugins(JmhPlugin)
  .settings(
    name := "okay",
    idePackagePrefix := Some("okay"),
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )
