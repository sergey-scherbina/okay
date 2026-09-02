package okay.deploy.sbt

import _root_.sbt._
import _root_.sbt.Keys._
import sbtassembly.AssemblyPlugin
import sbtassembly.AssemblyPlugin.autoImport._
import sbtassembly.MergeStrategy

/**
 * What a module says to become deployable (specs/deploy.md): ONE
 * line in its build entry — `.settings(OkayDeploy.deployable("your.Main"))`
 * — and the fat jar lands at the stable name the rendered Dockerfile
 * relies on. Knows no particular application.
 */
object OkayDeploy extends AutoPlugin {
  override def requires = AssemblyPlugin
  override def trigger = noTrigger

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
