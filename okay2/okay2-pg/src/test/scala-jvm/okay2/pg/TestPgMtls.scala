package okay2.pg

import okay2.sql.SqlValue

/**
 * mTLS against Postgres (okay-pg's TestPgMtls, specs/tls.md, pg-mtls):
 * the client PRESENTS an identity and the server authenticates it — no
 * password at all. Live against the dockerized Postgres provisioned by
 * `okay-pg/mtls-provision.sh` (role `okay_mtls` under `hostssl ... cert
 * clientcert=verify-full`); skips where the container or its client cert
 * is absent.
 */
class TestPgMtls extends PgLive {

  /** copies a file out of the container; None if docker or the file is absent */
  private def fromContainer(name: String, mode: String): Option[String] =
    try {
      val tmp = java.nio.file.Files.createTempFile("okay2-pg-mtls", s"-$name")
      val ok = new ProcessBuilder("docker", "cp", s"okay-pg:/var/lib/postgresql/data/$name", tmp.toString)
        .redirectErrorStream(true).start().waitFor() == 0
      if (ok && java.nio.file.Files.size(tmp) > 0) {
        new ProcessBuilder("chmod", mode, tmp.toString).start().waitFor(): Unit
        Some(tmp.toString)
      } else None
    } catch { case _: Throwable => None }

  lazy val caFile = fromContainer("server.crt", "0644")
  lazy val clientCert = fromContainer("okay_mtls.crt", "0644")
  lazy val clientKey = fromContainer("okay_mtls.key", "0400")

  lazy val provisioned: Boolean =
    caFile.isDefined && clientCert.isDefined && clientKey.isDefined && {
      try { connectTls("okay", "okay", TlsConfig(mode = SslMode.Require)).close(); true }
      catch { case _: Throwable => false }
    }

  private def connectTls(user: String, password: String, cfg: TlsConfig): PgSql =
    run(PgTls.connect(host, port, user, password, "okay", cfg))

  private def identity = TlsConfig(mode = SslMode.VerifyFull, caFile = caFile, clientCert = clientCert, clientKey = clientKey)

  private def one(db: PgSql, sql: String): Vector[SqlValue] =
    chunks(db.query(sql)).flatten match {
      case List(row) => row
      case other => fail(s"expected one row, got $other")
    }

  private val unprovisioned = s"okay-pg at $host:$port is not provisioned for mTLS (okay-pg/mtls-provision.sh) — skips"

  test("with the client certificate: okay_mtls logs in with NO password and queries as itself") {
    assume(provisioned, unprovisioned)
    val db = connectTls("okay_mtls", "", identity)
    try {
      assertEquals(one(db, "select 42"), Vector[SqlValue](SqlValue.I32(42)))
      assertEquals(one(db, "select current_user"), Vector[SqlValue](SqlValue.Text("okay_mtls")))
    } finally db.close()
  }

  test("without the certificate: the SERVER refuses okay_mtls by name — TLS alone is not an identity") {
    assume(provisioned, unprovisioned)
    val e = intercept[PgError](connectTls("okay_mtls", "", TlsConfig(mode = SslMode.VerifyFull, caFile = caFile)))
    assert(e.getMessage.contains("valid client certificate"), e.getMessage)
  }

  test("the rule is the role's alone: password roles (with or without an identity offered) still SCRAM in") {
    assume(provisioned, unprovisioned)
    val plain = connectTls("okay", "okay", TlsConfig(mode = SslMode.VerifyFull, caFile = caFile))
    try assertEquals(one(plain, "select current_user"), Vector[SqlValue](SqlValue.Text("okay"))) finally plain.close()
    // offering an identity to a rule that does not ask for one changes nothing
    val offered = connectTls("okay", "okay", identity)
    try assertEquals(one(offered, "select current_user"), Vector[SqlValue](SqlValue.Text("okay"))) finally offered.close()
  }
}
