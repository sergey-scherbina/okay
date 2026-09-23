import sbt._
import sbt.Keys._

/**
 * Compiling Frege sources (specs/frege.md): the `.fr` files under `src/<config>/frege`
 * through the Frege compiler, FORKED — it is a whole compiler with its
 * own statics and may exit its JVM — with `-target 17` (the build's
 * floor, jdk-floor-is-a-flag), into a managed class directory that is
 * put on the configuration's classpath, so Scala in the same
 * configuration calls the compiled Frege modules like any class.
 *
 * The classpath the compiler sees is the configuration's INTERNAL and
 * MANAGED dependencies — for Test that is the module's own main classes
 * (the Scala bridge the Frege `native`s bind to) and the Frege runtime
 * from Maven — never `dependencyClasspath` or even
 * `externalDependencyClasspath`: both include `unmanagedClasspath`,
 * which is where this task's own output goes (a cycle, sbt says so).
 *
 * Incremental by source timestamps: an unchanged source set is not
 * recompiled. A change in the Scala bridge the natives bind to is NOT
 * tracked; the gate builds fresh worktrees, and a local edit of the
 * bridge wants `okayFrege/clean`.
 *
 * The 2015 sbt-frege plugin is unmaintained (sbt 0.13) — refused.
 */
object Frege {
  val fregeSources = settingKey[File]("the directory of .fr sources")
  val fregeClasses = settingKey[File]("where the compiled Frege classes land")
  val fregeCompile = taskKey[File]("compile the Frege sources; answers the class directory")

  /**
   * Frege sources of a configuration that its OWN Scala depends on —
   * okay-frege's `Prog` library, whose generated classes the Scala
   * driver reads. They see only the managed jars and the other
   * projects (not this configuration's classes, which do not exist
   * yet), and their output is a PRODUCT: packaged, and on the classpath
   * of Test and of every project that depends on this one.
   */
  def before(c: Configuration): Seq[Setting[_]] = in(c) ++ inConfig(c)(Seq(
    products ++= Seq(fregeCompile.value),
    // `products` does not feed the jar: the classes are mapped in by
    // name, or a published okay-frege ships the driver without the
    // `Prog` it walks (measured: the first packageBin had no Prog*.class)
    packageBin / mappings ++= {
      val d = fregeCompile.value
      (d ** "*.class").get.map(f => f -> IO.relativize(d, f).get)
    },
  ))

  def in(c: Configuration): Seq[Setting[_]] = inConfig(c)(Seq(
    fregeSources := sourceDirectory.value / "frege",
    fregeClasses := target.value / s"frege-${c.name}-classes",
    fregeCompile := {
      val log = streams.value.log
      val out = fregeClasses.value
      val srcs = (fregeSources.value ** "*.fr").get.sorted
      val cp = (internalDependencyClasspath.value ++ managedClasspath.value).files
      val runtime = cp.find(_.getName.matches("frege-3\\..*\\.jar")).getOrElse(
        sys.error("fregeCompile: no org.frege-lang:frege jar on the classpath"))
      val javaBin = file(System.getProperty("java.home")) / "bin" / "java"
      val cached = FileFunction.cached(streams.value.cacheDirectory / "frege", FilesInfo.lastModified, FilesInfo.exists) { changed =>
        IO.delete(out)
        IO.createDirectory(out)
        if (srcs.nonEmpty) {
          val args = Seq(javaBin.getAbsolutePath, "-Xss4m", "-cp", runtime.getAbsolutePath, "frege.compiler.Main",
            "-target", "17", "-make", "-d", out.getAbsolutePath,
            "-fp", (cp :+ out).map(_.getAbsolutePath).mkString(java.io.File.pathSeparator)) ++ srcs.map(_.getAbsolutePath)
          val lines = new StringBuilder
          val code = scala.sys.process.Process(args).!(scala.sys.process.ProcessLogger(
            l => lines.append(l).append('\n'), l => lines.append(l).append('\n')))
          lines.toString.linesIterator.foreach(l => log.debug(s"frege: $l"))
          if (code != 0) sys.error(s"fregeCompile: the Frege compiler failed (exit $code):\n$lines")
        }
        (out ** "*.class").get.toSet
      }
      cached(srcs.toSet)
      out
    },
    unmanagedClasspath += Attributed.blank(fregeCompile.value),
  ))
}
