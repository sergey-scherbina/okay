import sbt._
import sbt.Keys._

/**
 * SemanticDB on the JVM only (symdex-okay, 2026-10-03). sbt-symdex turns it on for the whole
 * build; symdex indexes a cross-built source ONCE, from its JVM copy, so the JS and Native
 * compiles would pay for SemanticDB nobody reads. Measured on a clean compile of okay's core and
 * okay-stream: JVM 30 → 34 s, JS 12 → 16 s with it on. A project setting beats the build's.
 */
object SemanticdbOffJs extends AutoPlugin {
  override def requires = org.scalajs.sbtplugin.ScalaJSPlugin
  override def trigger = allRequirements
  override def projectSettings: Seq[Setting[_]] = Seq(semanticdbEnabled := false)
}

/** the same for Scala Native */
object SemanticdbOffNative extends AutoPlugin {
  override def requires = scala.scalanative.sbtplugin.ScalaNativePlugin
  override def trigger = allRequirements
  override def projectSettings: Seq[Setting[_]] = Seq(semanticdbEnabled := false)
}
