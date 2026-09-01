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
  /** the reference travels as a one-field product ({"ref": ...});
   * a bare-string form waits for an iso node in the Schema algebra */
  given Schema[Secret] = Schema.derived

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
  def read[A: Schema](json: String): Either[String, A] = Json.read[A](json)

  /** JVM/Native; the path is a String so the signature exists on
   * every platform — JS answers a named refusal until Node's fs
   * joins */
  def load[A: Schema](path: String): Either[String, A] =
    Platform.slurp(path).flatMap(read[A])
