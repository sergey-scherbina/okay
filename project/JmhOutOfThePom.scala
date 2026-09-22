import sbt._
import Keys._
import pl.project13.scala.sbt.JmhPlugin

/**
 * JMH is a BENCHMARK dependency, not a library dependency
 * (published-pom-carries-jmh, 2026-09-23).
 *
 * sbt-jmh adds `jmh-core`, `jmh-generator-bytecode` and
 * `jmh-generator-reflection` with no configuration, which is Compile,
 * so every published module that enables `JmhPlugin` (the core
 * `okay_3` among them) listed JMH as a compile dependency in its pom,
 * and every consumer of the "zero-dependency" core pulled JMH, jopt-
 * simple, commons-math3 and asm. Found by publishing locally and
 * reading the pom, not by any gate: a pom is not compiled.
 *
 * `Jmh` extends `Test`, so moving the three to `test` keeps them on the
 * benchmark classpath (the same way munit already reaches it) and takes
 * them out of what a consumer resolves. As an AutoPlugin triggered by
 * `JmhPlugin`, it applies to every module that enables JMH, including
 * ones added later, with nothing to remember at the call site.
 */
object JmhOutOfThePom extends AutoPlugin {
  override def requires = JmhPlugin
  override def trigger = allRequirements

  override def projectSettings: Seq[Setting[_]] = Seq(
    libraryDependencies := libraryDependencies.value.map { m =>
      if (m.organization == "org.openjdk.jmh" && m.configurations.isEmpty) m.withConfigurations(Some("test"))
      else m
    }
  )
}
