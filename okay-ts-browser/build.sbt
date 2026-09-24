// okay-ts-browser (polyglot-typescript stage 5): a TypeScript library used
// FROM okay on Scala.js, through a facade ScalablyTyped generates from the
// library's own .d.ts. A SEPARATE build, as okay2 is: the converter plugin
// loads here only. okay comes from `publishLocal` of the main build
// (see README.md), which is what a user's own build depends on as well.

ThisBuild / scalaVersion := "3.9.0"
ThisBuild / organization := "dev.okay"

val okayVersion = "0.2.0-SNAPSHOT"

lazy val root = (project in file("."))
  .enablePlugins(ScalaJSPlugin, ScalablyTypedConverterGenSourcePlugin)
  .settings(
    name := "okay-ts-browser",
    // the library, as npm names it (a local package here, so the test needs
    // no registry; a published one is written `"lodash" -> "4.17.21"`)
    Compile / npmDependencies += "okay-pricing" -> s"file:${baseDirectory.value}/ts-lib/okay-pricing",
    Compile / npmDevDependencies += "typescript" -> "5.9.3",
    // where the generated facade lives
    stOutputPackage := "okay.tsbrowser.facades",
    // the facade's SOURCES go to this project's own compiler: ScalablyTyped's
    // internal compile of them runs a compiler that predates Scala 3.9's
    // standard library (NoSuchMethodError: scala.Option.orNull, measured)
    stSourceGenMode := SourceGenMode.ResourceGenerator,
    // the library needs ES2015 (Promise), not the DOM: the standard library
    // facade is 2229 files with the DOM and a handful without
    stStdlib := List("es2015"),
    stMinimize := Selection.AllExcept("okay-pricing"),
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
    libraryDependencies ++= Seq(
      "dev.okay" %%% "okay" % okayVersion,
      "dev.okay" %%% "okay-async" % okayVersion,
      "org.scalameta" %%% "munit" % "1.1.1" % Test,
    ),
  )
