package okay.matching

import okay.{!, Async}
import okay.given
import okay.crypto.given
import okay.pg.PgSql
import okay.sql.Placeholders

/**
 * Postgres over the wire driver (demo-pg-backend): the engine suite
 * VERBATIM — the `?` program renumbered by `Placeholders.numbered`,
 * nothing else changed. Live against the dockerized Postgres
 * (skips where it is absent); each store lives in its own schema so
 * the tests do not see each other, and "reopen" is a second
 * connection with the same search_path.
 */
class TestPgMatch extends MatchEngineSuite {
  def engine = "postgres"

  val host = sys.env.getOrElse("OKAY_PG_HOST", "127.0.0.1")
  val port = sys.env.get("OKAY_PG_PORT").flatMap(_.toIntOption).getOrElse(5432)

  private def connect(): PgSql =
    !.run(Async.run[PgSql, Nothing](PgSql.connect(host, port, "okay", "okay", "okay")))
  private def exec(db: PgSql, sql: String): Unit =
    !.run(Async.run[Long, Nothing](db.update(sql)))

  lazy val available: Boolean =
    try { connect().close(); true } catch { case _: Throwable => false }
  override def munitIgnore: Boolean = !available

  private val schemas = scala.collection.mutable.ListBuffer.empty[String]

  def fresh(): (SqlMatch, () => SqlMatch) =
    val schema = s"okay_match_${java.util.UUID.randomUUID().toString.replace("-", "").take(12)}"
    schemas += schema
    def open() =
      val db = connect()
      exec(db, s"CREATE SCHEMA IF NOT EXISTS $schema")
      exec(db, s"SET search_path TO $schema")
      SqlMatch(db, placeholders = Placeholders.numbered)
    (open(), () => open())

  override def afterAll(): Unit =
    if available && schemas.nonEmpty then
      val db = connect()
      try schemas.foreach(s => exec(db, s"DROP SCHEMA IF EXISTS $s CASCADE"))
      finally db.close()
}
