// okay-frege's build half, as a SOURCE sbt plugin (specs/frege.md): the
// task that compiles Frege sources, published so a user of okay-frege
// compiles their own `.fr` the way this repository does — the repository's
// project/plugins.sbt depends on this directory, as it does on
// okay-deploy/sbt-plugin. No sbt dependency beyond sbt itself: the Frege
// compiler comes from the build's own classpath (okay-frege brings it).
sbtPlugin := true
name := "okay-frege-sbt"
organization := "dev.okay"
