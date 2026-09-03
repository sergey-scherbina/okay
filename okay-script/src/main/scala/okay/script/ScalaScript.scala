package okay.script

import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.reporting.{Reporter, Diagnostic}

import java.io.{ByteArrayOutputStream, File, PrintStream}
import java.net.URLClassLoader
import java.nio.file.{Files, Path, Paths}

/** A markdown file's ```scala fenced code blocks, taken as a single
 * literate Scala program: extracted, compiled through the REAL Scala 3
 * compiler (dotty.tools.dotc, in-process) and run. No new language, no
 * interpreter -- see specs/okay-script.md.
 */
final case class Block(code: String, startLine: Int)

final case class Result(
  ok: Boolean,
  stdout: String,
  errors: Vector[String],
  thrown: Option[Throwable],
)

/** The set of classpath entries a script compiles and runs against.
 * `ambient` (the calling JVM's own `-cp`) is a HYPOTHESIS about the
 * environment, not a fact: it reads
 * `System.getProperty("java.class.path")`, which is only correct when
 * the JVM was launched with a real `-cp` -- an un-forked sbt test JVM
 * reports just `sbt-launch.jar` there (found 2026-09-03,
 * okay-script-scalac-classpath: sbt manages its actual classpath
 * through its own classloaders, invisible to that property, and a
 * script compiled against it saw no scala-library at all). A caller
 * building a RUNTIME app from a generated `.md` file -- a storefront,
 * say -- should not depend on inheriting the host process's classpath
 * at all: it supplies exactly what the script needs (its own
 * okay-ui/okay-jetty jars, resolved `using dep` artifacts) instead.
 */
final case class Classpath(entries: Vector[Path]):
  def ++(extra: Vector[Path]): Classpath = Classpath(entries ++ extra)
  def asString: String = entries.map(_.toString).mkString(File.pathSeparator)

object Classpath:
  val ambient: Classpath = Classpath(
    System.getProperty("java.class.path").split(File.pathSeparatorChar).toVector.map(Paths.get(_))
  )

/** `//> using dep "org:artifact:version"` directives -- scala-cli's own
 * convention, reused rather than inventing another one -- hoisted out
 * of a markdown file's ```scala blocks and resolved to jars.
 *
 * Fetching a Maven coordinate is inherently a network operation, so
 * this shells out to the standard `cs`/`coursier` CLI rather than
 * embedding a resolver in-process: dotc's own compilation stays
 * in-process (see ScalaScript.run), only dependency FETCHING crosses
 * the process boundary, and only when a script actually declares one.
 */
object Deps:
  private val usingDep = """//>\s*using\s+dep\s+"([^"]+)"""".r

  def declared(markdown: String): Vector[String] =
    ScalaScript.blocks(markdown).flatMap(b => usingDep.findAllMatchIn(b.code).map(_.group(1))).distinct

  enum Resolved:
    case Jars(paths: Vector[Path])
    case ToolMissing
    case Failed(message: String)

  def resolve(coords: Vector[String]): Resolved =
    if coords.isEmpty then Resolved.Jars(Vector.empty)
    else
      findTool() match
        case None => Resolved.ToolMissing
        case Some(bin) =>
          val proc = new ProcessBuilder((Vector(bin, "fetch") ++ coords)*).start()
          val out = new String(proc.getInputStream.readAllBytes(), "UTF-8")
          val err = new String(proc.getErrorStream.readAllBytes(), "UTF-8")
          val code = proc.waitFor()
          if code == 0 then
            Resolved.Jars(out.linesIterator.filter(_.nonEmpty).map(Paths.get(_)).toVector)
          else
            Resolved.Failed(err.trim)

  private def findTool(): Option[String] =
    val path = Option(System.getenv("PATH")).getOrElse("")
    val names = Vector("cs", "coursier")
    path
      .split(File.pathSeparatorChar)
      .toVector
      .flatMap(dir => names.map(n => Paths.get(dir, n)))
      .find(p => Files.isRegularFile(p) && Files.isExecutable(p))
      .map(_.toString)

object ScalaScript:

  private val fenceOpen = """```scala\s*""".r
  private val fenceClose = """```\s*""".r

  def blocks(markdown: String): Vector[Block] =
    val lines = markdown.linesWithSeparators.toVector.map(_.stripLineEnd)
    val out = Vector.newBuilder[Block]
    var i = 0
    while i < lines.length do
      if fenceOpen.matches(lines(i)) then
        val start = i + 1
        val body = Vector.newBuilder[String]
        var j = start
        while j < lines.length && !fenceClose.matches(lines(j)) do
          body += lines(j)
          j += 1
        out += Block(body.result().mkString("\n"), start + 1)
        i = j + 1
      else
        i += 1
    out.result()

  /** Compile and run a markdown file's ```scala blocks as one program
   * (see Block/Result docs). `classpath` defaults to the calling
   * process's own -- see Classpath's doc for why that is a hypothesis,
   * not a given -- and a script's own `//> using dep` directives are
   * resolved and appended to it before compiling.
   */
  def run(markdown: String, classpath: Classpath = Classpath.ambient): Result =
    val coords = Deps.declared(markdown)
    Deps.resolve(coords) match
      case Deps.Resolved.Failed(msg) =>
        Result(ok = false, stdout = "", errors = Vector(s"dependency resolution failed: $msg"), thrown = None)
      case Deps.Resolved.ToolMissing =>
        Result(ok = false, stdout = "", errors = Vector("`using dep` declared but no cs/coursier found on PATH"), thrown = None)
      case Deps.Resolved.Jars(extra) =>
        runWith(markdown, classpath ++ extra)

  private def runWith(markdown: String, classpath: Classpath): Result =
    val src = blocks(markdown).map(_.code).mkString("\n\n")
    val body = if src.isEmpty then "  ()" else src.linesWithSeparators.map("  " + _).mkString
    val wrapped =
      s"""@main def okayScriptMain(): Unit =
         |$body
         |""".stripMargin

    val dir = Files.createTempDirectory("okay-script-")
    try
      val srcFile = dir.resolve("OkayScriptMain.scala")
      Files.writeString(srcFile, wrapped)
      val outDir = Files.createDirectory(dir.resolve("out"))

      val diagnostics = Vector.newBuilder[String]
      val reporter = collectingReporter(diagnostics)

      val args = Array(
        "-classpath", classpath.asString,
        "-d", outDir.toString,
        "-color:never",
        srcFile.toString,
      )

      val driver = new Driver:
        override protected def sourcesRequired: Boolean = false

      val summary = driver.process(args, reporter, null)
      val errs = diagnostics.result()

      if summary.hasErrors then
        Result(ok = false, stdout = "", errors = errs, thrown = None)
      else
        // platform-only parent (okay-script-classloader-isolation,
        // 2026-09-03): getClass.getClassLoader would let a script
        // resolve anything on okay-script's OWN build classpath
        // (URLClassLoader is parent-first) regardless of what the
        // caller actually put in `classpath` -- defeating the
        // isolation Classpath/Deps exist for. A script sees exactly
        // its own compiled classes, its own Classpath, and the JDK.
        val loaderUrls = (outDir +: classpath.entries).map(_.toUri.toURL).toArray
        val loader = new URLClassLoader(loaderUrls, ClassLoader.getPlatformClassLoader())
        val cls = loader.loadClass("okayScriptMain")
        val method = cls.getMethod("main", classOf[Array[String]])

        val capturedOut = new ByteArrayOutputStream()
        var thrown: Option[Throwable] = None
        try
          val prevOut = System.out
          val ps = new PrintStream(capturedOut, true, "UTF-8")
          System.setOut(ps)
          try
            scala.Console.withOut(ps):
              try method.invoke(null, Array.empty[String])
              catch
                case e: java.lang.reflect.InvocationTargetException =>
                  thrown = Some(Option(e.getCause).getOrElse(e))
                case e: Throwable =>
                  thrown = Some(e)
          finally
            System.setOut(prevOut)
        finally
          loader.close()

        Result(
          ok = thrown.isEmpty,
          stdout = capturedOut.toString("UTF-8"),
          errors = Vector.empty,
          thrown = thrown,
        )
    finally
      deleteRecursively(dir)

  private def collectingReporter(sink: scala.collection.mutable.Builder[String, Vector[String]]): Reporter =
    new dotty.tools.dotc.reporting.StoreReporter(null):
      override def doReport(dia: Diagnostic)(using Context): Unit =
        if dia.level >= dotty.tools.dotc.interfaces.Diagnostic.ERROR then
          sink += dia.msg.message

  private def deleteRecursively(p: Path): Unit =
    if Files.exists(p) then
      Files.walk(p).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))
