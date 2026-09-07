package okay.acme

import okay.*
import okay.given
import okay.security.given

import java.nio.file.{Files, Path, Paths}

/**
 * Taking a certificate back, from a shell — because revocation is an
 * OPERATOR's action at an hour nobody planned for, and a library call
 * they cannot reach at 3am is not an answer.
 *
 * ```
 * java -cp … okay.acme.Revoke ./data/acme keyCompromise
 * ```
 *
 * The directory is the one `okay.script.Serve` writes with
 * `OKAY_DATA` (`<data>/acme`), so an operator names what they already
 * know. `OKAY_ACME` supplies the account's email, `OKAY_ACME_PROD=1`
 * picks production over staging — the same switches the server runs
 * with, so a revoke cannot accidentally talk to the wrong CA while
 * believing it talked to the right one.
 */
object Revoke:

  final case class Args(dir: Path, reason: Acme.Reason, email: String, production: Boolean):
    def certFile: Path = dir.resolve("cert.pem")
    def accountKey: Path = dir.resolve("account.pem")

  def parse(argv: Array[String], env: String => Option[String] = k => Option(System.getenv(k)))
  : Either[String, Args] =
    argv.toList match
      case dir :: rest if rest.length <= 1 =>
        val path = Paths.get(dir)
        for
          _ <- Either.cond(Files.isDirectory(path), (), s"not a directory: $dir")
          reason <- rest.headOption.map(reasonOf).getOrElse(Right(Acme.Reason.Unspecified))
          email <- env("OKAY_ACME").toRight("OKAY_ACME (the account's email) is not set")
          _ <- Either.cond(Files.isRegularFile(path.resolve("cert.pem")), (),
            s"no certificate at ${path.resolve("cert.pem")}")
          _ <- Either.cond(Files.isRegularFile(path.resolve("account.pem")), (),
            s"no account key at ${path.resolve("account.pem")} — only the account that ordered a " +
              "certificate can revoke it with its key")
        yield Args(path, reason, email,
          env("OKAY_ACME_PROD").exists(v => v == "1" || v.equalsIgnoreCase("true")))
      case _ => Left("usage: okay.acme.Revoke <acme-dir> [unspecified|keyCompromise|superseded|cessationOfOperation]")

  def reasonOf(name: String): Either[String, Acme.Reason] =
    Acme.Reason.values.find(_.toString.equalsIgnoreCase(name))
      .toRight(s"'$name' is not a reason; one of ${Acme.Reason.values.map(_.toString).mkString(", ")}")

  def main(argv: Array[String]): Unit =
    parse(argv) match
      case Left(msg) =>
        System.err.println(msg)
        System.exit(2)
      case Right(a) =>
        val cfg = Acme.Config(a.email, Vector.empty, a.accountKey, a.certFile, a.dir.resolve("key.pem"),
          directory = if a.production then Acme.Directory.letsEncrypt else Acme.Directory.letsEncryptStaging)
        val out = Resource.run[Either[String, Unit], Pure](
          okay.jetty.Jetty.http().map(http => Acme.revoke(cfg, http, a.reason))).runWith
        out match
          case Right(()) =>
            println(s"okay-acme: revoked ${a.certFile} (${a.reason})")
            println("okay-acme: the certificate is still on disk — delete it, or the next start serves a revoked identity")
          case Left(msg) =>
            System.err.println(s"okay-acme: $msg")
            System.exit(1)
