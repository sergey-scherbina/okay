package okay.deploy

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path

/**
 * The one door every subprocess in this module goes through
 * (specs/deployment.md, "No silent failure, anywhere").
 *
 * The rule it makes mechanical: a command that fails must produce a
 * sentence an operator can act on. Several of the tools this module
 * drives exit non-zero with nothing on either stream — `helm upgrade`
 * and `docker compose` both can — so the message is built from what
 * we always have (the command line, the exit code, what it was FOR)
 * and only enriched by output when there is some.
 */
object Shell:

  final case class Out(code: Int, text: String):
    def ok: Boolean = code == 0
    /** the last lines, which is the part that ever says anything */
    def tail(n: Int = 6): String =
      val ls = text.linesIterator.filter(_.trim.nonEmpty).toVector
      ls.drop(ls.length - n).mkString("\n")

  /** a command's exit code and merged output; a command that cannot
   * be STARTED is 127 with the reason, the same shape a shell gives
   * so a caller never has to special-case "not found" */
  def run(cmd: Vector[String], dir: Option[Path] = None, env: Map[String, String] = Map.empty): Out =
    try
      val pb = ProcessBuilder(cmd*).redirectErrorStream(true)
      dir.foreach(d => pb.directory(d.toFile): Unit)
      env.foreach((k, v) => pb.environment.put(k, v): Unit)
      val p = pb.start()
      val text = String(p.getInputStream.readAllBytes(), UTF_8)
      Out(p.waitFor(), text)
    catch
      case e: java.io.IOException => Out(127, s"${cmd.headOption.getOrElse("")}: ${e.getMessage}")
      case e: InterruptedException =>
        Thread.currentThread.interrupt()
        Out(130, s"interrupted: ${e.getMessage}")

  /** the same, as an answer: `Left` carries the command line, the
   * code, the purpose and whatever output there was */
  def must(cmd: Vector[String], what: String, dir: Option[Path] = None): Either[String, String] =
    val out = run(cmd, dir)
    if out.ok then Right(out.text)
    else Left(failure(cmd, out, what))

  def failure(cmd: Vector[String], out: Out, what: String): String =
    val head = s"$what failed (exit ${out.code})\n  command: ${line(cmd)}"
    val t = out.tail()
    if t.isEmpty then head + "\n  output:  none — the command printed nothing at all"
    else head + "\n  output:  " + t.linesIterator.mkString("\n           ")

  /** a command line an operator can paste back */
  def line(cmd: Vector[String]): String =
    cmd.map(a => if a.exists(c => c.isWhitespace || c == '"') then "\"" + a.replace("\"", "\\\"") + "\"" else a)
      .mkString(" ")
