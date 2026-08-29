ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.2"

ThisBuild / scalacOptions ++= Seq("-Xkind-projector", "-Wall")

/** the library and its own benchmarks: no dependencies beyond munit for tests */
lazy val root = (project in file("."))
  .enablePlugins(JmhPlugin)
  .settings(
    name := "okay",
    idePackagePrefix := Some("okay"),
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
  )

/** comparison benchmarks against the ecosystem: the heavy dependencies live here */
lazy val compare = (project in file("compare"))
  .dependsOn(root)
  .enablePlugins(JmhPlugin)
  .settings(
    name := "okay-compare",
    idePackagePrefix := Some("okay"),
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
