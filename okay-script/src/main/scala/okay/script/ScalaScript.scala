package okay.script

import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.reporting.{Reporter, Diagnostic}

import java.io.{ByteArrayOutputStream, PrintStream}
import java.net.URLClassLoader
import java.nio.file.{Files, Path}

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

  def run(markdown: String): Result =
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

      val classpath = System.getProperty("java.class.path")
      val args = Array(
        "-classpath", classpath,
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
        val loader = new URLClassLoader(Array(outDir.toUri.toURL), getClass.getClassLoader)
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
