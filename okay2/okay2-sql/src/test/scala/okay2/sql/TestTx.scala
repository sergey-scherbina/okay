package okay2.sql

import okay2.{!, pure}
import okay2.async.Async
import okay2.stream.{Chunk, Source}

/** the transaction protocol as a typestate: the right order runs, and a
 * begin left open, a statement's state changed, or a second begin are
 * compile errors — each paired with the shape that compiles */
class TestTx extends munit.FunSuite {

  /** records every call it is asked to make */
  final class Recording extends Sql {
    val log = Vector.newBuilder[String]
    def describe(sql: String): Vector[Col] ! Async = { log += s"describe $sql"; pure[Async, Vector[Col]](Vector.empty) }
    def query(sql: String, params: Vector[SqlValue]): Source[Chunk[Vector[SqlValue]]] = pure(())
    def update(sql: String, params: Vector[SqlValue]): Long ! Async = Async { log += s"update $sql"; 1L }
    def batch(sql: String, rows: Chunk[Vector[SqlValue]]): Long ! Async = Async { log += s"batch $sql"; rows.length.toLong }
    def begin(isolation: Isolation, readOnly: Boolean): Granted ! Async = Async { log += "begin"; Granted(isolation, isolation) }
    def commit(): Unit ! Async = Async { log += "commit" }
    def rollback(): Unit ! Async = Async { log += "rollback" }
    def cancel(): Unit = ()
  }

  test("begin, statements, commit: an Idle -> Idle program runs as the calls it names") {
    val db = new Recording
    val tx = new Tx(db)
    val p = for {
      _ <- tx.begin()
      a <- tx.update[Tx.Open]("insert 1")
      b <- tx.update[Tx.Open]("insert 2")
      _ <- tx.commit()
    } yield a + b
    assertEquals(Drive.run(Tx.run(p)), 2L)
    assertEquals(db.log.result(), Vector("begin", "update insert 1", "update insert 2", "commit"))
  }

  // munit's compileErrors takes a LITERAL: each snippet builds its own tx
  test("a begin left open does not run: only Idle -> Idle is accepted") {
    assert(compileErrors("val tx = new okay2.sql.Tx(null); okay2.sql.Tx.run(tx.begin())").nonEmpty)
    assertEquals(compileErrors("val tx = new okay2.sql.Tx(null); okay2.sql.Tx.run(tx.begin().flatMap(_ => tx.commit()))"), "")
  }

  test("a second begin, and a commit outside a transaction, do not compile") {
    assert(compileErrors("val tx = new okay2.sql.Tx(null); okay2.sql.Tx.run(tx.begin().flatMap(_ => tx.begin()).flatMap(_ => tx.commit()))").nonEmpty)
    assert(compileErrors("val tx = new okay2.sql.Tx(null); okay2.sql.Tx.run(tx.commit())").nonEmpty)
    assertEquals(compileErrors("val tx = new okay2.sql.Tx(null); okay2.sql.Tx.run(tx.begin().flatMap(_ => tx.rollback()))"), "")
  }
}
