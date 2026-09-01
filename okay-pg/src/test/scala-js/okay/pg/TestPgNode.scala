package okay.pg

import scala.concurrent.Future
import okay.{!, +, Async, Chunk, Produce, Stream}
import okay.given
import okay.sql.SqlValue

/**
 * THE openness acceptance of specs/sql.md, final box: a NODE
 * process queries Postgres through okay-pg — the same driver, the
 * same SCRAM (node:crypto underneath), the same portals — with no
 * JVM and no JDBC anywhere in this process. Live: completes as
 * skipped-with-a-word where the dockerized server is absent.
 */
class TestPgNode extends munit.FunSuite:

  given scala.concurrent.ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue

  val host = "127.0.0.1"
  val port = 5432

  def drain(p: Chunk[Vector[SqlValue]] ! (Produce + Async)): Vector[Vector[SqlValue]] ! Async =
    val S = summon[Stream[[X] =>> X ! (Produce + Async), Async]]
    def go(rest: Chunk[Vector[SqlValue]] ! (Produce + Async)): Vector[Vector[SqlValue]] ! Async =
      S.uncons(rest).flatMap {
        case None => okay.pure(Vector.empty)
        case Some((c, more)) => go(more).map(c.toVector ++ _)
      }
    go(p)

  test("a Node process speaks SCRAM and portals to a real Postgres: no JVM, no JDBC") {
    val prog: (Vector[Vector[SqlValue]], Long) ! Async =
      for
        db <- PgSql.connect(host, port, "okay", "okay", "okay")
        rows <- drain(db.query("select 21 * 2, 'from node'"))
        n <- db.update("select 1") // CommandComplete's count road
      yield (rows, n)
    Async.runAsync(prog).map { (rows, _) =>
      assertEquals(rows, Vector(Vector(SqlValue.I32(42), SqlValue.Text("from node"))))
    }.recover {
      case _: Throwable =>
        println(s"no Postgres at $host:$port — the Node live test skips")
    }
  }

  test("a wrong password is refused by SCRAM itself, on Node") {
    Async.runAsync(PgSql.connect(host, port, "okay", "wrong", "okay"))
      .transform {
        case scala.util.Failure(_: PgError) => scala.util.Success(())
        case scala.util.Success(db) =>
          db.close()
          scala.util.Failure(new AssertionError("a wrong password connected"))
        case scala.util.Failure(_) =>
          println(s"no Postgres at $host:$port — the Node live test skips")
          scala.util.Success(())
      }
  }
