addSbtPlugin("org.jetbrains.scala" % "sbt-ide-settings" % "1.1.4")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.22.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.12")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.4.0")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.4.0")
// okay-deploy's build half lives in okay-deploy/sbt-plugin (a source
// plugin; it brings sbt-assembly) — the one pointer the root keeps
lazy val root = (project in file(".")).dependsOn(RootProject(file("../okay-deploy/sbt-plugin")))
