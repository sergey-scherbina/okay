package okay.r2dbc

import io.r2dbc.postgresql.{PostgresqlConnectionConfiguration, PostgresqlConnectionFactory}
import io.r2dbc.spi.Connection
import okay.sql.Isolation

/** Postgres through r2dbc-postgresql — the hatch against a real
 * server (the dockerized one; skips where absent). The same suite as
 * H2's: the seam's contract does not know which driver is under it. */
class TestR2dbcPg extends R2dbcSuite:
  // integration-test-gate: out of the default gate, into `sbt integrationTest`
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  def engine = "postgres (r2dbc)"
  override def knowsNullability = false
  val host = sys.env.getOrElse("OKAY_PG_HOST", "127.0.0.1")
  val port = sys.env.get("OKAY_PG_PORT").flatMap(_.toIntOption).getOrElse(5432)
  private lazy val factory = PostgresqlConnectionFactory(PostgresqlConnectionConfiguration.builder()
    .host(host).port(port).username("okay").password("okay").database("okay").build())
  lazy val available: Boolean =
    try { java.net.Socket(host, port).close(); true } catch { case _: Exception => false }
  override def munitIgnore: Boolean = !available
  def open(): Connection = Rx.first(factory.create()).get

  /** sql-commit-tag, the same probe as TestPg's: a statement fails
   * inside the transaction, the program handles it, COMMIT follows.
   * Postgres answers ROLLBACK with no error; the question is whether
   * this driver notices. The assertion is the one that matters —
   * a commit that answers success while the row is absent is the
   * defect, whichever side reports it. */
  test(s"$engine: a handled error inside a transaction — COMMIT must not report success (sql-commit-tag)") {
    val db = fresh()
    try
      run(db.begin(Isolation.ReadCommitted)): Unit
      run(db.update("insert into okay_r2dbc values (1, 'x', 0, true, 0, null)")): Unit
      val _ = intercept[Exception](run(db.update("select syntax error from")))
      val committed = try { run(db.commit()); true } catch { case _: Exception => false }
      val rows = collectChunks(db.query("select id from okay_r2dbc")).flatten
      assert(!committed || rows.nonEmpty,
        s"commit reported success and the row is absent: the rollback that does not roll back")
      assertEquals(rows, Nil)
    finally db.close()
  }

