package okay.r2dbc

import io.r2dbc.postgresql.{PostgresqlConnectionConfiguration, PostgresqlConnectionFactory}
import io.r2dbc.spi.Connection

/** Postgres through r2dbc-postgresql — the hatch against a real
 * server (the dockerized one; skips where absent). The same suite as
 * H2's: the seam's contract does not know which driver is under it. */
class TestR2dbcPg extends R2dbcSuite:
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
