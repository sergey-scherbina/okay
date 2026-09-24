// a SEPARATE build (like okay2): the ScalablyTyped converter is loaded
// here only, so the main build and every agent's sbt start stay as they were
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.22.0")
addSbtPlugin("org.scalablytyped.converter" % "sbt-converter" % "1.0.0-beta45")
