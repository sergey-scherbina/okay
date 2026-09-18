package okay.deploy

import okay.Handler
import java.nio.file.Path

/**
 * WHAT A MODULE ASKS THE PLACE FOR, AS AN OPERATION — the thing it
 * needs is the ANSWER (di-needs-from-static, ROADMAP P13 item 4).
 *
 * `Needs.needs(Need.Volume(dir))` declared a need BESIDE the code
 * that opened the thing, and the two could drift: a volume declared
 * at one path and a file opened at another is a container writing to
 * an unmounted directory, which is the incident module-facts found
 * (the board went to a path the deployment believed was memory).
 * Here the path a module opens IS the path it asked the place for,
 * because it has no other way to get one: the volume arrives as the
 * answer to `Volume(dir)`, the database as the answer to
 * `Database(…)` — the `*_URL` setting every target already sets —
 * and the port as the answer to `Port(n)`.
 *
 * A module built over these (`Needs.provisioned`) is a `Static`
 * spine, so `Static.leaves` reads every need off the program before
 * it runs — no declaration, nothing to keep in step. Each case
 * carries its `Need`, which is the deployment's vocabulary
 * unchanged: renderers see what they always saw.
 *
 * The signature is covariant, as `Static` asks; each case fixes its
 * own answer type, so a handler answers a `Path` to a volume and
 * cannot answer a `String`.
 */
enum Provision[+A]:
  /** a directory the place mounts; the answer is where it is mounted */
  case Volume(path: String, size: String = "1Gi", name: String = "data") extends Provision[Path]
  /** a database the place runs; the answer is its connection URL */
  case Database(engine: Engine, version: String, database: String, as: String = "db")
    extends Provision[String]
  /** a port the place exposes; the answer is the number to listen on */
  case Port(number: Int, public: Boolean = true) extends Provision[Int]

  /** the same thing in the deployment's words */
  def need: Need = this match
    case Volume(p, s, n) => Need.Volume(p, s, n)
    case Database(e, v, d, a) => Need.Database(e, v, d, a)
    case Port(n, p) => Need.Port(n, p)

object Provision:
  /** the setting a target hands a database's URL in: `DB_URL` for `as = "db"` */
  def urlSetting(as: String): String = okay.conf.Conf.envName("", s"${as}_url")

  /**
   * THE PLACE THE PROCESS IS RUNNING IN, as a handler: a volume is
   * mounted where it was asked for, a port is its number, a database
   * is the `*_URL` setting the targets set (compose, the chart, the
   * PaaS scripts all write it) — and a missing one is an error that
   * NAMES the variable rather than an empty string handed to a
   * driver. The default given, so a module written with
   * `Needs.provisioned` runs with nothing else said; a test that
   * wants the volume in a temporary directory brings its own.
   */
  given local: Handler[Provision] = new:
    def handle[A](p: Provision[A]): A = p match
      case Volume(path, _, _) => Path.of(path)
      case Port(n, _) => n
      case Database(_, _, _, as) =>
        val key = urlSetting(as)
        sys.env.getOrElse(key, throw IllegalStateException(
          s"the place did not set $key: the database this module asked for has no URL here"))
