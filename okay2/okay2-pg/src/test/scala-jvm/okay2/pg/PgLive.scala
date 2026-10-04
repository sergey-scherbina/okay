package okay2.pg

import okay2.{!, Pure}
import okay2.async.Async
import okay2.platform._
import okay2.stream.{Chunk, Source}

/**
 * What every Live pg suite shares (okay-pg's suites each carried their
 * own copy): the endpoint from OKAY_PG_HOST / OKAY_PG_PORT (okay/okay/okay
 * on 127.0.0.1:5432, the okay-pg docker container), the Live tag that
 * keeps them out of the default gate (integration-test-gate; `liveOnly;
 * okay2Pg/test` runs them), and the skip where no server answers.
 */
trait PgLive extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  val host: String = sys.env.getOrElse("OKAY_PG_HOST", "127.0.0.1")
  val port: Int = sys.env.get("OKAY_PG_PORT").flatMap(_.toIntOption).getOrElse(5432)

  def run[A](prog: A ! Async): A = !.run(Async.run[A, Pure](prog))

  def connect(): PgSql = run(PgSql.connect(host, port, "okay", "okay", "okay"))

  lazy val available: Boolean =
    try { connect().close(); true }
    catch { case _: Throwable => false }

  def skipped: String = s"no Postgres at $host:$port — the live suite skips"

  def chunks[A](s: Source[Chunk[A]]): List[Chunk[A]] = run(Source.SourceOps(s).runCollect).toList

  def withDb[A](f: PgSql => A): A = {
    val db = connect()
    try f(db)
    finally db.close()
  }
}
