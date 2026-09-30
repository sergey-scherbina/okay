package okay.sql

import okay.{!, Async, Chunk, Free, pure}
import okay.given
import scala.collection.mutable.ArrayBuffer

/**
 * specs/indexed-effects.md, stage 2: the transaction protocol as an
 * indexed DATA signature over the row `TxOp +~ Unary[Async]`, and the
 * handler that threads a connection TYPED by the index. The same
 * recording fake as TestTx's: the protocol is about order, and order
 * is what it logs. JVM: running an `Async` program takes `CanBlock`.
 */
class TestTxData extends munit.FunSuite:

  final class Recording extends Sql:
    val log = ArrayBuffer.empty[String]
    def describe(sql: String): Vector[Col] ! Async = { log += s"describe $sql"; pure(Vector.empty) }
    def query(sql: String, params: Vector[SqlValue] = Vector.empty): okay.Source[Chunk[Vector[SqlValue]]] =
      throw UnsupportedOperationException("not a step")
    def update(sql: String, params: Vector[SqlValue] = Vector.empty): Long ! Async = { log += s"update $sql"; pure(1L) }
    def batch(sql: String, rows: Chunk[Vector[SqlValue]]): Long ! Async = { log += s"batch $sql"; pure(rows.size.toLong) }
    def begin(isolation: Isolation, readOnly: Boolean = false): Granted ! Async =
      { log += s"begin $isolation"; pure(Granted(isolation, isolation, readOnly)) }
    def commit(): Unit ! Async = { log += "commit"; pure(()) }
    def rollback(): Unit ! Async = { log += "rollback"; pure(()) }
    def cancel(): Unit = ()

  import Tx.Data.{begin, commit, update, async, interpret}

  test("a well-bracketed program runs its steps in the order the type promised, the connection moved by the handler") {
    val db = Recording()
    val n = interpret(
      begin().flatMap { g =>
        update[Tx.Open]("insert into t values (1)").flatMap(_ => commit()).map(_ => g.granted)
      })(db).runWith
    assertEquals(n, Isolation.ReadCommitted)
    assertEquals(db.log.toList, List("begin ReadCommitted", "update insert into t values (1)", "commit"))
  }

  test("an Async program runs INSIDE the body, on the diagonal, forwarded by the handler") {
    val db = Recording()
    val seen = ArrayBuffer.empty[String]
    val body: Unit ! Async = Free.delay(() => { seen += "inside"; pure(()) })
    val r = interpret(
      begin().flatMap(_ => async[Tx.Open, Unit](body)).flatMap(_ => update[Tx.Open]("x")).flatMap(_ => commit()))(db).runWith
    assertEquals(r, ())
    assertEquals(seen.toList, List("inside"))
    assertEquals(db.log.toList, List("begin ReadCommitted", "update x", "commit"))
  }

  test("a statement outside any transaction is a closed program too") {
    val db = Recording()
    assertEquals(interpret(update[Tx.Idle]("select 1"))(db).runWith, 1L)
    assertEquals(db.log.toList, List("update select 1"))
  }

  test("the run-time refusals are compile errors on the data road: nested begin, commit with no begin, a program left open") {
    assert(compileErrors("okay.sql.Tx.Data.begin().flatMap(_ => okay.sql.Tx.Data.begin())").nonEmpty, "a nested begin compiled")
    assert(compileErrors("okay.sql.Tx.Data.interpret(okay.sql.Tx.Data.commit())(new okay.sql.TestTxData().Recording())").nonEmpty,
      "a commit with no begin compiled")
    assert(compileErrors("""okay.sql.Tx.Data.interpret(
      okay.sql.Tx.Data.begin().flatMap(_ => okay.sql.Tx.Data.update[okay.sql.Tx.Open]("insert")))(new okay.sql.TestTxData().Recording())""").nonEmpty,
      "a program that ends inside a transaction compiled")
  }

  test("the connection's type is the index: closing an idle connection, or opening an open one, does not type inside the handler") {
    assert(compileErrors("(c: okay.sql.Tx.Conn[okay.sql.Tx.Idle]) => c.closed").nonEmpty, "closed on Conn[Idle] compiled")
    assert(compileErrors("(c: okay.sql.Tx.Conn[okay.sql.Tx.Open]) => c.opened").nonEmpty, "opened on Conn[Open] compiled")
  }

  test("the caveat: a failure inside the body drops the continuation and the commit with it — the type is not a run-time guarantee") {
    val db = Recording()
    val boom: Unit ! Async = Free.delay(() => throw RuntimeException("boom"))
    val p = begin().flatMap(_ => async[Tx.Open, Unit](boom)).flatMap(_ => commit())
    val _ = intercept[RuntimeException](interpret(p)(db).runWith)
    assertEquals(db.log.toList, List("begin ReadCommitted"), "the commit ran after a failure")
  }
