package okay.caps

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import java.nio.file.Files

/** a snippet compiled by the Scala compiler itself, against this module's classes: its error messages, none when
 * it compiles. Capture checking is a phase after the typer, so `compiletime.testing.typeCheckErrors` never sees
 * its errors — a test of what does NOT compile has to run the whole compiler */
object Compile:
  def errors(code: String, captureChecking: Boolean = true, moduleWide: Boolean = false): List[String] =
    val dir = Files.createTempDirectory("okay-caps")
    val src = dir.resolve("Snippet.scala")
    val header = "package snippet\n" + (if captureChecking && !moduleWide then "import language.experimental.captureChecking\n" else "")
    val option = if moduleWide then Array("-language:experimental.captureChecking") else Array.empty[String]
    Files.writeString(src, header + "import okay.caps.*\n" + code)
    val reporter = StoreReporter(null)
    Driver().process(option ++ Array("-classpath", System.getProperty("java.class.path"), "-d", dir.toString,
      "-color:never", src.toString), reporter): Unit
    reporter.removeBufferedMessages(using dotty.tools.dotc.core.Contexts.NoContext).collect {
      case e: dotty.tools.dotc.reporting.Diagnostic.Error => e.message
    }
