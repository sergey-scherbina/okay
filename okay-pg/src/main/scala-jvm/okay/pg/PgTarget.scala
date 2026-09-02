package okay.pg

import okay.tls.{SslMode, TlsConfig}

/**
 * A Postgres URL as operators write it: `postgres://user:pass@host
 * :port/db?sslmode=…&sslrootcert=…` — parsed purely, so "does this
 * URL configure TLS the way I meant" is testable with no server.
 * `sslmode` is the TLS seam's ladder by its postgres names; absent
 * means plaintext (the dockerized default); `sslrootcert` is the CA
 * for verify-ca/full. Extracted 2026-09-02 from okay-demo (specs/
 * sql.md) — zero demo dependencies from the start, a pure move.
 */
final case class PgTarget(host: String, port: Int, user: String, password: String,
                          database: String, tls: Option[TlsConfig])

object PgTarget:
  def is(s: String): Boolean = s.startsWith("postgres://") || s.startsWith("postgresql://")

  def parse(url: String): Either[String, PgTarget] =
    try
      val u = java.net.URI(url)
      if u.getHost == null then Left(s"no host in '$url'")
      else
        val userInfo: String = Option(u.getUserInfo).getOrElse("")
        val (user, pass): (String, String) =
          if userInfo.isEmpty then ("okay", "")
          else userInfo.split(":", 2) match
            case Array(un, pw) => (un, pw)
            case Array(un) => (un, "")
            case _ => ("okay", "")
        val path: String = Option(u.getPath).getOrElse("")
        val db: String = if path.stripPrefix("/").isEmpty then user else path.stripPrefix("/")
        val query: String = Option(u.getQuery).getOrElse("")
        val q: Map[String, String] = query.split("&").toVector.filter(_.nonEmpty).map { kv =>
          kv.split("=", 2) match
            case Array(k, v) => k -> v
            case _ => kv -> ""
        }.toMap
        val ca: Option[String] = q.get("sslrootcert")
        val tls: Either[String, Option[TlsConfig]] = q.get("sslmode") match
          case None | Some("disable") => Right(None)
          case Some("require") => Right(Some(TlsConfig(SslMode.Require, None, None, None)))
          case Some("verify-ca") => Right(Some(TlsConfig(SslMode.VerifyCa, ca, None, None)))
          case Some("verify-full") => Right(Some(TlsConfig(SslMode.VerifyFull, ca, None, None)))
          case Some(bad) => Left(s"sslmode '$bad' is not one of disable/require/verify-ca/verify-full")
        tls.map(t => PgTarget(u.getHost, if u.getPort < 0 then 5432 else u.getPort, user, pass, db, t))
    catch case e: Exception => Left(s"not a URL: ${e.getMessage}")
