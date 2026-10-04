package okay2.pg

import okay2.sql.SqlValue

/**
 * The pg driver over TLS (okay-pg's TestPgTls, specs/tls.md): the
 * SSLRequest dance in the driver. Live against the dockerized Postgres
 * with ssl=on (skips where TLS is not offered). SCRAM and the query run
 * UNCHANGED over the encrypted transport.
 */
class TestPgTls extends PgLive {

  private def connectTls(cfg: TlsConfig): PgSql = run(PgTls.connect(host, port, "okay", "okay", "okay", cfg))

  /** ssl must be OFFERED by the server (SSLRequest answered 'S') */
  lazy val tls: Boolean =
    try { connectTls(TlsConfig(mode = SslMode.Require)).close(); true }
    catch { case _: Throwable => false }

  /** the server's own cert, copied out of the container so verify-full
   * has a CA to check the chain against; None if docker/cert absent */
  lazy val caFile: Option[String] =
    try {
      val tmp = java.nio.file.Files.createTempFile("okay2-pg-ca", ".crt")
      val ok = new ProcessBuilder("docker", "cp", "okay-pg:/var/lib/postgresql/data/server.crt", tmp.toString)
        .redirectErrorStream(true).start().waitFor() == 0
      if (ok && java.nio.file.Files.size(tmp) > 0) Some(tmp.toString) else None
    } catch { case _: Throwable => None }

  private def selects42(db: PgSql): Unit =
    assertEquals(chunks(db.query("select 42")).flatten, List(Vector[SqlValue](SqlValue.I32(42))))

  private val noTls = s"no TLS Postgres at $host:$port — the pg-TLS suite skips"

  test("sslmode=require: the SSLRequest dance, TLS handshake, SCRAM and a query — all over the wire") {
    assume(tls, noTls)
    val db = connectTls(TlsConfig(mode = SslMode.Require))
    try selects42(db) finally db.close()
  }

  test("sslmode=verify-full with the server CA: the chain AND the hostname check pass") {
    assume(tls, noTls)
    assume(caFile.isDefined, "could not copy the server cert out of the container — skips")
    val db = connectTls(TlsConfig(mode = SslMode.VerifyFull, caFile = caFile))
    try selects42(db) finally db.close()
  }

  test("verify-full with an UNKNOWN CA is refused by name, not silently downgraded") {
    assume(tls, noTls)
    val e = intercept[PgError](connectTls(TlsConfig(mode = SslMode.VerifyFull)))
    assert(e.getMessage.contains("TLS") || e.getMessage.toLowerCase.contains("handshake"), e.getMessage)
  }
}
