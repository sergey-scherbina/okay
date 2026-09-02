import sbt._
import sbt.Keys._
import sbtassembly.AssemblyPlugin.autoImport._
import sbtassembly.MergeStrategy

/**
 * The build half of okay-deploy (specs/deploy.md): what a module says
 * to become deployable. ONE line in its build entry —
 * `.settings(OkayDeploy.deployable("your.Main"))` — and the fat jar
 * lands at a stable name the rendered Dockerfile relies on. Nothing
 * here knows any particular application.
 */
object OkayDeploy {
  val jarName = "app.jar"

  def deployable(mainClass: String): Seq[Setting[_]] = Seq(
    assembly / Keys.mainClass := Some(mainClass),
    assembly / assemblyJarName := jarName,
    assembly / assemblyMergeStrategy := { (path: String) =>
      if (path.startsWith("META-INF/services/")) MergeStrategy.concat
      else if (path.startsWith("META-INF/")) MergeStrategy.discard
      else if (path == "module-info.class") MergeStrategy.discard
      else if (path == "reference.conf") MergeStrategy.concat
      else MergeStrategy.first
    },
  )
}
