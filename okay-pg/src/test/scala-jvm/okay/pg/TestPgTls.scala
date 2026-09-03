package okay.pg

import okay.{!, +, Async, Chunk, Handler, Produce}
import okay.given
import okay.crypto.given
import okay.sql.SqlValue
import okay.tls.{TlsConfig, SslMode}
import okay.conf.Secrets

/**
 * The pg driver over TLS (specs/tls.md, the pg lane): the SSLRequest
 * dance in the driver, the encrypted session from the one seam. Live
 * against the dockerized Postgres with ssl=on (skips where TLS is not
 * offered or the broker is absent). The point proven: SCRAM and the
 * query run UNCHANGED over the encrypted transport — connectOver never
 * learns it is on TLS.
 */
class TestPgTls extends munit.FunSuite:

  // integration-test-gate: out of the default gate, into `sbt integrationTest`
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  val host = sys.env.getOrElse("OKAY_PG_HOST", "127.0.0.1")
  val port = sys.env.get("OKAY_PG_PORT").flatMap(_.toIntOption).getOrElse(5432)

  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  private def connectTls(cfg: TlsConfig): PgSql =
    run(PgTls.connect(host, port, "okay", "okay", "okay", cfg, Secrets.file))

  /** ssl must be OFFERED by the server (SSLRequest answered 'S'); if
   * not (plaintext-only broker, or none), the whole suite skips */
  lazy val available: Boolean =
    try { connectTls(TlsConfig(mode = SslMode.Require)).close(); true }
    catch { case _: Throwable => false }

  /** the server's own cert, copied out of the container so verify-full
   * has a CA to check the chain against; None if docker/cert absent */
  lazy val caFile: Option[String] =
    try
      val tmp = java.nio.file.Files.createTempFile("okay-pg-ca", ".crt")
      val ok = ProcessBuilder("docker", "cp",
        "okay-pg:/var/lib/postgresql/data/server.crt", tmp.toString)
        .redirectErrorStream(true).start().waitFor() == 0
      if ok && java.nio.file.Files.size(tmp) > 0 then Some(tmp.toString) else None
    catch case _: Throwable => None

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

  private def selects42(db: PgSql): Unit =
    val got = collectChunks(db.query("select 42")).flatten
    assertEquals(got, List(Vector(SqlValue.I32(42))))

  test("sslmode=require: the SSLRequest dance, TLS handshake, SCRAM and a query — all over the wire") {
    assume(available, s"no TLS Postgres at $host:$port — the pg-TLS suite skips")
    val db = connectTls(TlsConfig(mode = SslMode.Require))
    try selects42(db) finally db.close()
  }

  test("sslmode=verify-full with the server CA: the chain AND the hostname check pass") {
    assume(available, s"no TLS Postgres at $host:$port — the pg-TLS suite skips")
    assume(caFile.isDefined, "could not copy the server cert out of the container — skips")
    val db = connectTls(TlsConfig(mode = SslMode.VerifyFull, caFile = caFile))
    try selects42(db) finally db.close()
  }

  test("verify-full with an UNKNOWN CA is refused by name, not silently downgraded") {
    assume(available, s"no TLS Postgres at $host:$port — the pg-TLS suite skips")
    val e = intercept[PgError](connectTls(TlsConfig(mode = SslMode.VerifyFull)))
    assert(e.getMessage.contains("TLS") || e.getMessage.toLowerCase.contains("handshake"),
      e.getMessage)
  }
