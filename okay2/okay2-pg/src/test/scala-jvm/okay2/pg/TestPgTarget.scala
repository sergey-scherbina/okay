package okay2.pg

/** PgTarget: a pure postgres:// connection URL parser (okay-pg's
 * TestPgTarget, specs/sql.md) */
class TestPgTarget extends munit.FunSuite {

  test("parses every field a postgres:// URL names, sslmode as the TLS ladder, refusals named") {
    val t = PgTarget.parse(
      "postgres://okay:s3cret@db.internal:5433/market?sslmode=verify-full&sslrootcert=/etc/ca.crt")
      .fold(e => fail(e), identity)
    assertEquals((t.host, t.port, t.user, t.password, t.database), ("db.internal", 5433, "okay", "s3cret", "market"))
    assertEquals(t.tls, Some(TlsConfig(mode = SslMode.VerifyFull, caFile = Some("/etc/ca.crt"))))
    // the defaults are the dockerized ones: port 5432, plaintext, db = user
    assertEquals(PgTarget.parse("postgresql://okay@localhost").map(x => (x.port, x.database, x.tls)),
      Right((5432, "okay", None)))
    assert(PgTarget.parse("postgres://h/db?sslmode=prefer").left.exists(_.contains("prefer")))
    assert(!PgTarget.is("okay-chat.db") && PgTarget.is("postgres://h/db"))
  }

  test("sslmode disable or absent means plaintext") {
    assertEquals(PgTarget.parse("postgres://u:p@h/db?sslmode=disable").map(_.tls), Right(None))
    assertEquals(PgTarget.parse("postgres://u:p@h/db").map(_.tls), Right(None))
  }

  test("require carries no CA (verify-ca/verify-full do)") {
    assertEquals(PgTarget.parse("postgres://u:p@h/db?sslmode=require").map(_.tls),
      Right(Some(TlsConfig(SslMode.Require, None, None, None))))
  }

  test("a malformed URL is a named refusal, never a throw") {
    assert(PgTarget.parse("not a url at all  ").isLeft)
    assert(PgTarget.parse("postgres:///db").left.exists(_.contains("no host")))
  }

  test("a client key that holds PEM instead of naming a file is refused by name") {
    val cfg = TlsConfig(SslMode.Require, clientCert = Some("/c.pem"), clientKey = Some("-----BEGIN PRIVATE KEY-----"))
    val e = Tls.client(new java.net.Socket(), "h", cfg)
    assert(e.left.exists(_.contains("INLINE")), e.toString)
  }
}
