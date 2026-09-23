package okay.r

import okay.codec.Schema

/**
 * R source written next to the Scala that calls it
 * (foreign-inline-modules, specs/foreign-highlevel.md stage 4) — okay-py's
 * `PyModule` in R. The source is a compile-time constant (`R.module`
 * refuses anything else) and the constructor is private. The engine hands
 * the file to the shim when R starts, which `sys.source`s it into its own
 * environment; `module::fn` then resolves there before any package, so a
 * module's functions never leak into the global environment and two
 * modules may define the same name.
 */
final class RModule private (val name: String, val source: String):
  def fn[Out: Schema](function: String): R.Fn[Out] = R.fn[Out](s"$name::$function")
  def hold(function: String): R.Hold = R.hold(s"$name::$function")

object RModule:
  private val Identifier = "[A-Za-z.][A-Za-z0-9._]*".r

  /** only `R.module` calls this, with constants it has checked */
  def fromConstant(name: String, source: String): RModule =
    require(Identifier.matches(name), s"okay.r: a module name is an R name, got '$name'")
    new RModule(name, source)

  /** the variable the shim reads: `name=path;name=path` */
  val Variable = "OKAY_R_MODULES"

  /**
   * The child environment that ships `modules`: one file each, in a
   * fresh directory under java.io.tmpdir (which a container shim can
   * mount), named in `OKAY_R_MODULES`.
   */
  private[r] def env(modules: Seq[RModule], env: Map[String, String]): Map[String, String] =
    if modules.isEmpty then env
    else
      val names = modules.map(_.name)
      require(names.distinct.size == names.size, s"okay.r: two modules share a name: ${names.mkString(", ")}")
      val dir = java.nio.file.Files.createTempDirectory("okay-r-modules")
      dir.toFile.deleteOnExit()
      val entries = modules.map { m =>
        val f = dir.resolve(s"${m.name}.R")
        java.nio.file.Files.writeString(f, m.source): Unit
        f.toFile.deleteOnExit()
        s"${m.name}=$f"
      }
      env.updated(Variable, entries.mkString(";"))
