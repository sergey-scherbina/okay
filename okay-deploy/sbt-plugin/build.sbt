// okay-deploy's build half, as a SOURCE sbt plugin (specs/deploy.md):
// the repository's project/plugins.sbt depends on this directory, so
// the assembly settings a deployable module needs live HERE, beside
// the renderers, and nothing deploy-shaped sits at the repository root.
sbtPlugin := true
name := "okay-deploy-sbt"
organization := "dev.okay"
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.3.0")
