package okay.frege.sbt

import _root_.sbt._
import _root_.sbt.Keys._

/**
 * Compiling Frege sources in sbt (specs/frege.md) — what a user of
 * okay-frege enables to write their own Frege, and what this repository
 * uses for okay-frege itself:
 *
 * {{{
 * // project/plugins.sbt
 * addSbtPlugin("dev.okay" % "okay-frege-sbt" % okayVersion)
 * // build.sbt
 * lazy val app = project
 *   .enablePlugins(OkayFrege)
 *   .settings(libraryDependencies += "dev.okay" %% "okay-frege" % okayVersion)
 *   .settings(OkayFrege.before(Compile))     // src/main/frege, read by src/main/scala
 * }}}
 *
 * The `.fr` files under `src/<config>/frege` go through the Frege
 * compiler FORKED (a whole compiler with its own statics, which may exit
 * its JVM) with `-target` (default 17: Frege generates Java and calls
 * javac, which would otherwise emit the running JDK's major — 69 on a 25
 * — and refuse to load on 17 or 21), into a class directory on the
 * configuration's classpath. Two placements:
 *
 *  - `before(c)`: the Frege sources the configuration's OWN Scala reads.
 *    They see the managed jars and the other projects (the configuration's
 *    own classes do not exist yet); their classes are a product AND are
 *    mapped into the jar (`products` alone does not feed `packageBin`:
 *    measured, the first okay-frege jar shipped without `Prog`).
 *  - `in(c)`: Frege sources that CALL the configuration's Scala (natives
 *    bound to it) — Test, where the main classes already exist.
 *
 * Warnings FAIL the build by default (`fregeFailOnWarnings`): the
 * compiler prints them as "W <file>:<line>:" and a build that logs them
 * is a build nobody reads. One that is right to keep is acknowledged in
 * the source, Frege's own way: a doc comment `--- nowarn: <the message>`.
 *
 * Incremental: recompiled when a source, the compiler's classpath (the
 * jars and class directories the natives bind to) or the options change;
 * otherwise the last output stands.
 */
object OkayFrege extends AutoPlugin {
  override def trigger = noTrigger

  object autoImport {
    val fregeSources = settingKey[File]("the directory of .fr sources of a configuration")
    val fregeClasses = settingKey[File]("where the configuration's compiled Frege classes land")
    val fregeTarget = settingKey[String]("the Java release Frege's javac targets (the JDK floor)")
    val fregeJavaOptions = settingKey[Seq[String]]("options for the forked Frege compiler's JVM")
    val fregeFailOnWarnings = settingKey[Boolean]("fail the build on a Frege compiler warning")
    val fregeCompile = taskKey[File]("compile the configuration's Frege sources; answers the class directory")
  }
  import autoImport._

  override def projectSettings: Seq[Setting[_]] = Seq(
    fregeTarget := "17",
    fregeJavaOptions := Seq("-Xss4m"),
    fregeFailOnWarnings := true,
  )

  /** Frege sources the configuration's own Scala reads (see above) */
  def before(c: Configuration): Seq[Setting[_]] = in(c) ++ inConfig(c)(Seq(
    products ++= Seq(fregeCompile.value),
    packageBin / mappings ++= {
      val d = fregeCompile.value
      (d ** "*.class").get.map(f => f -> IO.relativize(d, f).get)
    },
  ))

  /** Frege sources that call the configuration's Scala (see above) */
  def in(c: Configuration): Seq[Setting[_]] = inConfig(c)(Seq(
    fregeSources := sourceDirectory.value / "frege",
    fregeClasses := target.value / s"frege-${c.name}-classes",
    fregeCompile := compile(c).value,
    unmanagedClasspath += Attributed.blank(fregeCompile.value),
  ))

  /**
   * What the compile depends on, as one string: the sources, the
   * classpath (a jar by size and time, a class directory by its newest
   * class) and the options. A changed string recompiles everything —
   * Frege's own `-make` decides nothing across runs, so neither does this.
   */
  private def fingerprint(srcs: Seq[File], cp: Seq[File], options: Seq[String]): String = {
    def stamp(f: File): String =
      if (f.isDirectory) {
        val newest = (f ** "*.class").get.map(_.lastModified).foldLeft(0L)(math.max)
        s"${f.getAbsolutePath}@dir:$newest"
      } else s"${f.getAbsolutePath}@${f.length}:${f.lastModified}"
    (srcs.map(stamp) ++ Seq("--") ++ cp.map(stamp) ++ Seq("--") ++ options).mkString("\n")
  }

  private def compile(c: Configuration): Def.Initialize[Task[File]] = Def.task {
    val log = streams.value.log
    val out = fregeClasses.value
    val srcs = (fregeSources.value ** "*.fr").get.sorted
    // never `dependencyClasspath` or `externalDependencyClasspath`: both
    // include `unmanagedClasspath`, where this task's own output goes
    val cp = (internalDependencyClasspath.value ++ managedClasspath.value).files
    val options = Seq("-target", fregeTarget.value) ++ fregeJavaOptions.value
    val stampFile = streams.value.cacheDirectory / "frege-fingerprint"
    val now = fingerprint(srcs, cp, options)
    val last = if (stampFile.exists) IO.read(stampFile) else ""
    if (now != last || !out.exists) {
      IO.delete(out)
      IO.createDirectory(out)
      if (srcs.nonEmpty) {
        val runtime = cp.find(_.getName.matches("frege-3\\..*\\.jar")).getOrElse(
          sys.error("fregeCompile: no org.frege-lang:frege jar on the classpath (depend on okay-frege, or on frege itself)"))
        val javaBin = file(System.getProperty("java.home")) / "bin" / "java"
        val args = Seq(javaBin.getAbsolutePath) ++ fregeJavaOptions.value ++
          Seq("-cp", runtime.getAbsolutePath, "frege.compiler.Main", "-target", fregeTarget.value, "-make",
            "-d", out.getAbsolutePath, "-fp", (cp :+ out).map(_.getAbsolutePath).mkString(java.io.File.pathSeparator)) ++
          srcs.map(_.getAbsolutePath)
        val lines = new StringBuilder
        val code = scala.sys.process.Process(args).!(scala.sys.process.ProcessLogger(
          l => lines.append(l).append('\n'), l => lines.append(l).append('\n')))
        lines.toString.linesIterator.foreach(l => log.debug(s"frege: $l"))
        if (code != 0) sys.error(s"fregeCompile: the Frege compiler failed (exit $code):\n$lines")
        val warnings = lines.toString.linesIterator.filter(_.startsWith("W ")).toVector
        if (warnings.nonEmpty) {
          val report = s"fregeCompile: ${warnings.size} Frege warning(s):\n" + warnings.mkString("\n")
          if (fregeFailOnWarnings.value) sys.error(report + "\n(acknowledge one in its source with `--- nowarn: <message>`)")
          else log.warn(report)
        }
      }
      IO.write(stampFile, now)
    }
    out
  }
}
