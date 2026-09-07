package okay.conf

import okay.codec.{Json, Schema}

/**
 * Configuration as data, secrets as references (specs/conf.md): a
 * config is a case class with a derived Schema; a Secret is a
 * REFERENCE (`env:PG_PASSWORD`, `file:/run/secrets/pg`) that a config
 * stores, prints and round-trips — the value exists only in the
 * narrow gap between `Secrets.get` and a constructor argument, which
 * is why nothing okay-owned can ever persist it.
 */
final case class Secret(ref: String):
  /** safe to log by default — logging is where discipline fails */
  override def toString: String = ref

object Secret:
  /** the reference travels as the BARE string — the iso node it
   * waited for (codec-iso) arrived, and the wrapper does not exist
   * on the wire */
  given Schema[Secret] = Schema.wrap(Secret(_), _.ref)

/**
 * The resolver seam — a trait, not an effect row (programs do not
 * request secrets; the edge that builds their handlers does). Total:
 * a miss is an answer naming the REFERENCE and what was tried,
 * never any value.
 */
trait Secrets:
  def get(s: Secret): Either[String, String]

object Secrets {

  private[conf] def scheme(ref: String): (String, String) =
    ref.indexOf(':') match
      case -1 => ("", ref)
      case i => (ref.take(i), ref.drop(i + 1))

  /** `env:NAME` — the 12-factor answer; process.env under Node */
  val env: Secrets = Platform.env

  /** `file:/path` — secret mounts and 0400 files; exactly one
   * trailing newline trimmed (the universal mount artifact) */
  val file: Secrets = Platform.file

  /** tests — and nothing else: there is deliberately no `plain:` */
  def memory(m: Map[String, String]): Secrets = s =>
    m.get(s.ref).toRight(s"'${s.ref}' is not in the memory resolver")

  /** the first resolver that answers wins; when all miss, ONE error
   * — the most specific one (a matched scheme's own miss beats an
   * unrecognized-scheme shrug) */
  def chain(first: Secrets, rest: Secrets*): Secrets = s =>
    val all = (first +: rest).map(_.get(s))
    all.collectFirst { case Right(v) => Right(v) }
      .orElse(all.collectFirst { case l @ Left(m) if !m.startsWith("unrecognized scheme") => l })
      .getOrElse(all.last)

  /** the refusal every resolver gives a scheme it does not serve —
   * never a guess, never the reference used as the value */
  private[conf] def unrecognized(s: Secret): Either[String, String] =
    Left(s"unrecognized scheme '${scheme(s.ref)._1}' in '${s.ref}'")
}

/** reading a config is the codec plus a file — total, damage is
 * data, the same rules as every decode in this stack */
object Conf:
  def read[A: Schema](json: String): Either[String, A] = okay.codec.Codecs.readJson[A](json)

  /** JVM/Native; the path is a String so the signature exists on
   * every platform — JS answers a named refusal until Node's fs
   * joins */
  def load[A: Schema](path: String): Either[String, A] =
    Platform.slurp(path).flatMap(read[A])

  /**
   * The ONE derivation of an environment name from a field:
   * camelCase to PREFIX_SNAKE_CASE (specs/conf.md).
   *
   * It lives here rather than in either caller because okay-deploy
   * RENDERS these names into a unit file and a process READS them,
   * and two lists that agree only because a person keeps them
   * agreeing is the drift this repository keeps deleting.
   */
  def envName(prefix: String, field: String): String =
    val snake = field.flatMap(c => if c.isUpper then "_" + c else c.toString).toUpperCase
    if prefix.isEmpty then snake else s"${prefix.toUpperCase}_$snake"

  /**
   * What the environment says, as a PATCH: only the fields an actual
   * variable is set for, each typed by the schema.
   *
   * A variable whose text is not a value of that field's type is a
   * refusal naming the variable — `OKAY_PORT=eighty` says so here,
   * not in production. A field of a shape the environment cannot
   * carry is a refusal too, and only when a variable is actually set
   * for it: a config may hold a Vector no one configures from a
   * container.
   */
  def fromEnv[A](prefix: String, env: String => Option[String])(using s: Schema[A]): Either[String, Json] =
    s match
      case p: Schema.SProduct[A] =>
        val parts = p.fields.map { (field, under) =>
          val name = envName(prefix, field)
          env(name).filter(_.nonEmpty) match
            case None => Right(None)
            case Some(text) => scalar(under(), text, name, field).map(j => Some(field -> j))
        }
        parts.collectFirst { case Left(m) => Left(m) }
          .getOrElse(Right(Json.JObj(parts.collect { case Right(Some(kv)) => kv })))
      case other =>
        Left(s"a config read from the environment must be a case class; this one is ${nameOf(other)}")

  /**
   * Defaults, then a file over them, then the environment over that
   * (specs/conf.md, "The layering, and why the order is that one").
   *
   * Each source is closer to the running process than the one before
   * it, and closer wins. The merge is RFC 7396 over the value's own
   * JSON, so a file that names one field changes one field — a config
   * file that had to be complete is a config file nobody edits.
   */
  def layered[A](
    defaults: A,
    file: Option[String],
    env: String => Option[String],
    prefix: String,
  )(using s: Schema[A]): Either[String, A] =
    val base = Json.parse(okay.codec.Codecs.writeJson(defaults))
    for
      fromFile <- file match
        case None => Right(Json.JObj(Vector.empty))
        case Some(text) => Json.parse(text) match
          case o: Json.JObj => Right(o)
          case other => Left(s"a config file must be a JSON object; this one is ${Json.print(other).take(40)}")
      overEnv <- fromEnv[A](prefix, env)
      value <- read[A](Json.print(Json.mergePatch(Json.mergePatch(base, fromFile), overEnv)))
    yield value

  /** the scalars an environment can carry, and the named refusal for
   * everything else — a list smuggled through a comma-separated
   * variable is a parser nobody agreed on */
  private def scalar(under: Schema[?], text: String, name: String, field: String): Either[String, Json] =
    under match
      case Schema.SString => Right(Json.JStr(text))
      case Schema.SInt => text.toIntOption.map(i => Json.JNum(i.toDouble))
        .toRight(s"$name is not a whole number: '$text' (the `$field` setting)")
      case Schema.SLong => text.toLongOption.map(l => Json.JNum(l.toDouble))
        .toRight(s"$name is not a whole number: '$text' (the `$field` setting)")
      case Schema.SDouble => text.toDoubleOption.map(Json.JNum(_))
        .toRight(s"$name is not a number: '$text' (the `$field` setting)")
      case Schema.SBool => text.trim.toLowerCase match
        case "1" | "true" | "yes" | "on" => Right(Json.JBool(true))
        case "0" | "false" | "no" | "off" => Right(Json.JBool(false))
        case _ => Left(s"$name is not a yes or a no: '$text' (the `$field` setting; 1/true/yes/on or 0/false/no/off)")
      // a Secret is a String on the wire, and the string is the
      // REFERENCE -- which is exactly what belongs in a unit file
      case iso: Schema.SIso[?, ?] => scalar(iso.under(), text, name, field)
      case other =>
        Left(s"$name cannot come from the environment: the `$field` setting is ${nameOf(other)}, " +
          "and only text, numbers, yes/no and secret references do")

  private def nameOf(s: Schema[?]): String = s match
    case Schema.SProduct(n, _, _, _, _) => s"the case class $n"
    case Schema.SSum(n, _, _) => s"the enum $n"
    case Schema.SVector(_) | Schema.SList(_) => "a list"
    case Schema.SOption(_) => "an optional value"
    case other => other.toString
