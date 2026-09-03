package okay.pg

import okay.{!, +, Async, Chunk, Handler, Produce}
import okay.given
import okay.crypto.given
import okay.sql.SqlValue
import okay.tls.{TlsConfig, SslMode}
import okay.conf.{Secret, Secrets}

/**
 * mTLS against Postgres (specs/tls.md, pg-mtls): the client PRESENTS
 * an identity and the server authenticates it — no password at all.
 * Live against the dockerized Postgres provisioned by
 * `okay-pg/mtls-provision.sh`: a role `okay_mtls` under
 * `hostssl all okay_mtls all cert clientcert=verify-full`, whose
 * certificate (CN = the role) is signed by a CA the server trusts
 * (`ssl_ca_file`). Skips where the container or its client cert is
 * absent. The three things proven: with the cert the role logs in
 * and queries; without it the SERVER refuses by name; the password
 * roles and the plaintext suite are untouched by the rule.
 */
class TestPgMtls extends munit.FunSuite:

  // integration-test-gate: out of the default gate, into `sbt integrationTest`
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  val host = sys.env.getOrElse("OKAY_PG_HOST", "127.0.0.1")
  val port = sys.env.get("OKAY_PG_PORT").flatMap(_.toIntOption).getOrElse(5432)

  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  /** copies a file out of the container; None if docker or the file is absent */
  private def fromContainer(name: String, mode: String): Option[String] =
    try
      val tmp = java.nio.file.Files.createTempFile("okay-pg-mtls", s"-$name")
      val ok = ProcessBuilder("docker", "cp",
        s"okay-pg:/var/lib/postgresql/data/$name", tmp.toString)
        .redirectErrorStream(true).start().waitFor() == 0
      if ok && java.nio.file.Files.size(tmp) > 0 then
        // Secrets.file demands the key be private to its owner
        ProcessBuilder("chmod", mode, tmp.toString).start().waitFor()
        Some(tmp.toString)
      else None
    catch case _: Throwable => None

  lazy val caFile = fromContainer("server.crt", "0644")
  lazy val clientCert = fromContainer("okay_mtls.crt", "0644")
  lazy val clientKey = fromContainer("okay_mtls.key", "0400")

  lazy val provisioned: Boolean =
    caFile.isDefined && clientCert.isDefined && clientKey.isDefined && {
      try { connectTls("okay", "okay", TlsConfig(mode = SslMode.Require)).close(); true }
      catch { case _: Throwable => false }
    }

  private def connectTls(user: String, password: String, cfg: TlsConfig): PgSql =
    run(PgTls.connect(host, port, user, password, "okay", cfg, Secrets.file))

  private def identity = TlsConfig(mode = SslMode.VerifyFull, caFile = caFile,
    clientCert = clientCert, clientKey = clientKey.map(k => Secret(s"file:$k")))

  private def collectChunks[A](s: Chunk[A] ! (Produce + Async)): List[Chunk[A]] =
    import okay.!.*
    def go(rest: Chunk[A] ! (Produce + Async), acc: List[Chunk[A]]): List[Chunk[A]] =
      (rest.resume: @unchecked) match
        case Pure(_) => acc.reverse
        case Effect(e) => okay.<|>[Async, Produce](e) match
          case Left(a) => (summon[Handler[Async]].handle(a): Unit); acc.reverse
          case Right(c) => (c.asInstanceOf[Chunk[A]] :: acc).reverse
        case Bind(Effect(e), k) => okay.<|>[Async, Produce](e) match
          case Left(a) => go(k(summon[Handler[Async]].handle(a)), acc)
          case Right(c) => go(k(c), c.asInstanceOf[Chunk[A]] :: acc)
    go(s, Nil)

  private def one(db: PgSql, sql: String): Vector[SqlValue] =
    collectChunks(db.query(sql)).flatten match
      case List(row) => row
      case other => fail(s"expected one row, got $other")

  test("with the client certificate: okay_mtls logs in with NO password and queries as itself") {
    assume(provisioned, s"okay-pg at $host:$port is not provisioned for mTLS (okay-pg/mtls-provision.sh) — skips")
    val db = connectTls("okay_mtls", "", identity)
    try
      assertEquals(one(db, "select 42"), Vector(SqlValue.I32(42)))
      assertEquals(one(db, "select current_user"), Vector(SqlValue.Text("okay_mtls")))
    finally db.close()
  }

  test("without the certificate: the SERVER refuses okay_mtls by name — TLS alone is not an identity") {
    assume(provisioned, s"okay-pg at $host:$port is not provisioned for mTLS — skips")
    val e = intercept[PgError](
      connectTls("okay_mtls", "", TlsConfig(mode = SslMode.VerifyFull, caFile = caFile)))
    assert(e.getMessage.contains("valid client certificate"), e.getMessage)
  }

  test("the rule is the role's alone: password roles (with or without an identity offered) still SCRAM in") {
    assume(provisioned, s"okay-pg at $host:$port is not provisioned for mTLS — skips")
    val plain = connectTls("okay", "okay", TlsConfig(mode = SslMode.VerifyFull, caFile = caFile))
    try assertEquals(one(plain, "select current_user"), Vector(SqlValue.Text("okay"))) finally plain.close()
    // offering an identity to a rule that does not ask for one changes nothing
    val offered = connectTls("okay", "okay", identity)
    try assertEquals(one(offered, "select current_user"), Vector(SqlValue.Text("okay"))) finally offered.close()
  }
