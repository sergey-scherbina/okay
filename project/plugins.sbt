addSbtPlugin("org.jetbrains.scala" % "sbt-ide-settings" % "1.1.4")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")
// signing for Maven Central (docs/releasing.md); the upload itself is sbt's own sonaRelease
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.3.1")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.22.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.12")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.4.0")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.4.0")
// okay-deploy's build half lives in okay-deploy/sbt-plugin (a source
// plugin; it brings sbt-assembly), okay-frege's in okay-frege/sbt-plugin
// (compiling Frege sources; frege-sbt-plugin) — the pointers the root keeps
lazy val root = (project in file("."))
  .dependsOn(RootProject(file("../okay-deploy/sbt-plugin")))
  .dependsOn(RootProject(file("../okay-frege/sbt-plugin")))
// symdex (scripts/symdex-mcp.sh, AGENTS.md "Skills"): SemanticDB on for the build, so symdex can
// answer structural questions from the compiler's own output; JVM only (project/SemanticdbJvmOnly.scala).
// Served from GitHub Pages, no Maven Central.
resolvers += Resolver.url("symdex", url("https://sergey-scherbina.github.io/symdex"))(Resolver.ivyStylePatterns)
addSbtPlugin("io.github.sergey-scherbina" % "sbt-symdex" % "0.5.2")
