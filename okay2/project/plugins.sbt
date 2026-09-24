// okay2 on Scala.js and Scala Native (okay2-cross): the versions the
// root build uses, whose compiler plugins are published for 2.13.18
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.22.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.12")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.4.0")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.4.0")

// JMH for okay2-bench (2026-09-24): the same plugin and version as the
// root build, so a lane here and its Scala 3 twin run under one harness
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")
