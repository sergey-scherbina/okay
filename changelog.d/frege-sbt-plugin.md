## frege-sbt-plugin - compiling Frege is a published sbt plugin

The task that compiles `.fr` sources moved from project/Frege.scala to
okay-frege/sbt-plugin. It is a source sbt plugin published as
`okay-frege-sbt` (`OkayFrege`, enabled explicitly), and the repository's
own project/plugins.sbt depends on it, as it does on okay-deploy-sbt. A
user of okay-frege can now compile their own Frege:
`addSbtPlugin("dev.okay" % "okay-frege-sbt" % v)`,
`.enablePlugins(OkayFrege)`, `OkayFrege.before(Compile)`.

New settings: `fregeTarget` (17), `fregeJavaOptions`, and
`fregeFailOnWarnings` (true). The compile is now incremental on the
sources, the compiler's classpath and the options, where before it
tracked only sources. Checked: a rerun with nothing changed leaves the
classes untouched, and a change to the Scala a native binds to
recompiles them. Docs: docs/modules/okay-frege.md "Building Frege
sources"; specs/frege.md stage 3.
