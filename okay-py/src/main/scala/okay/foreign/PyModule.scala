package okay.foreign

import okay.codec.Schema

/**
 * Python source written next to the Scala that calls it
 * (foreign-inline-modules, specs/foreign-highlevel.md stage 4):
 *
 * {{{
 * val scoring = Py.module("scoring", """
 *   def score(xs):
 *       return sum(xs) / len(xs)
 * """)
 * scoring.fn[Double]("score")(Vector(1.0, 2.0))
 * }}}
 *
 * The source is a COMPILE-TIME CONSTANT — `Py.module` refuses anything
 * else — and this class has a private constructor, so a module is
 * reviewed, versioned code in the jar and never a string built at run
 * time. The engine ships it when a worker STARTS (a file on the worker's
 * `PYTHONPATH`); there is no wire operation that evaluates source, so
 * specs/py.md's "untrusted input reaches Python only as data" holds.
 *
 * The common indentation is removed, so the literal may be indented with
 * the Scala around it.
 */
final class PyModule private (val name: String, val source: String):
  /** a function of this module, typed (foreign-typed-calls) */
  def fn[Out: Schema](function: String): Py.Fn[Out] = Py.fn[Out](s"$name:$function")
  /** a function of this module whose result is held (foreign-object-handles) */
  def hold(function: String): Py.Hold = Py.hold(s"$name:$function")

object PyModule:
  private val Identifier = "[A-Za-z_][A-Za-z0-9_]*".r

  /** only `Py.module` calls this, with constants it has checked */
  def fromConstant(name: String, source: String): PyModule =
    require(Identifier.matches(name), s"okay.foreign: a module name is a Python identifier, got '$name'")
    new PyModule(name, dedent(source))

  /** the text with the indentation every non-blank line shares removed */
  private def dedent(text: String): String =
    val lines = text.split("\n", -1).toVector
    val indents = lines.filter(_.trim.nonEmpty).map(_.takeWhile(_ == ' ').length)
    val cut = if indents.isEmpty then 0 else indents.min
    lines.map(l => if l.trim.isEmpty then "" else l.drop(cut)).mkString("\n").trim + "\n"

  /**
   * The child environment that puts `modules` on the worker's path: a
   * fresh directory holding one file per module, in front of whatever
   * `PYTHONPATH` the config already names.
   */
  private[foreign] def env(modules: Seq[PyModule], env: Map[String, String]): Map[String, String] =
    if modules.isEmpty then env
    else
      val names = modules.map(_.name)
      require(names.distinct.size == names.size, s"okay.foreign: two modules share a name: ${names.mkString(", ")}")
      val dir = java.nio.file.Files.createTempDirectory("okay-py-modules")
      dir.toFile.deleteOnExit()
      modules.foreach { m =>
        val f = dir.resolve(s"${m.name}.py")
        java.nio.file.Files.writeString(f, m.source): Unit
        f.toFile.deleteOnExit()
      }
      val sep = java.io.File.pathSeparator
      env.updated("PYTHONPATH", (dir.toString +: env.get("PYTHONPATH").toSeq).mkString(sep))
