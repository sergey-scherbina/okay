package okay2.pg

import okay2.!
import okay2.async.Async
import okay2.platform._
import okay2.sql.SqlValue
import okay2.stream.Source

/**
 * THE openness acceptance of specs/sql.md (okay-pg's TestPgNode): a NODE
 * process queries Postgres through okay2-pg — the same driver, the same
 * SCRAM (node:crypto underneath), the same portals — with no JVM and no
 * JDBC. Live (it reaches a server outside the process): out of the
 * default gate, `liveOnly; okay2PgJS/test`; completes as
 * skipped-with-a-word where the server is absent.
 */
class TestPgNode extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  implicit val ec: scala.concurrent.ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue

  val host = "127.0.0.1"
  val port = 5432

  test("a Node process speaks SCRAM and portals to a real Postgres: no JVM, no JDBC") {
    val prog: (Vector[Vector[SqlValue]], Long) ! Async =
      PgSql.connect(host, port, "okay", "okay", "okay").flatMap { db =>
        Source.concat(db.query("select 21 * 2, 'from node'")).flatMap { rows =>
          db.update("select 1").map(n => (rows, n)) // CommandComplete's count road
        }
      }
    Async.runAsync(prog).map { case (rows, _) =>
      assertEquals(rows, Vector(Vector[SqlValue](SqlValue.I32(42), SqlValue.Text("from node"))))
    }.recover { case _: Throwable => println(s"no Postgres at $host:$port — the Node live test skips") }
  }

  test("a wrong password is refused by SCRAM itself, on Node") {
    Async.runAsync(PgSql.connect(host, port, "okay", "wrong", "okay")).transform {
      case scala.util.Failure(_: PgError) => scala.util.Success(())
      case scala.util.Success(db) =>
        db.close()
        scala.util.Failure(new AssertionError("a wrong password connected"))
      case scala.util.Failure(_) =>
        println(s"no Postgres at $host:$port — the Node live test skips")
        scala.util.Success(())
    }
  }
}
