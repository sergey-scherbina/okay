package okay.sql

import okay.{!, Answers, Async, pure}
import okay.core.{+:, Eff}
import okay.one.Cont
import okay.one.handler.Answering
import okay.std.Chunk
import okay.given
import scala.collection.mutable.ArrayBuffer

/**
 * okay-sql's transactions on okay-tx: a transaction is a bracket that holds its body, so the protocol (one begin, its
 * commit or its rollback) is the program's shape. The recording fake logs the driver's calls: the protocol is about
 * order, and order is what it logs. JVM: running an `Async` program takes `CanBlock`.
 */
class TestTxData extends munit.FunSuite:

  final class Recording(failCommitsWith: Option[String] = None) extends Sql:
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
    override def sqlState(t: Throwable): Option[String] = failCommitsWith.filter(_ => t.getMessage == "conflict")

  /** the row outside the transactions: `Async` alone */
  type T = Async +: okay.core.Pure
  type R = Tx.Txn[T] +: T
  type B = Tx.Inside[T]

  /** the `Async` left at the end answered in place */
  def run[A](e: Eff[T, A]): A =
    Cont.handle(e)(new Answering[Async, okay.core.Pure, A, A]:
      def ret(a: A) = Cont.pure(a)
      def value[X](op: Async[X]): X = summon[Answers[Async]].handle(op)).run

  test("a bracket runs begin, its statements, commit — in that order; the granted isolation read inside") {
    val db = Recording()
    val p: Eff[R, Isolation] = Tx.atomic[T, Isolation, R](
      Tx.update[B]("insert into t values (1)").flatMap(_ => Tx.granted[B]).map(_.granted))
    assertEquals(run(Tx.interpret[Isolation, T](db)(p)), Isolation.ReadCommitted)
    assertEquals(db.log.toList, List("begin ReadCommitted", "update insert into t values (1)", "commit"))
  }

  test("brackets inside brackets: one begin, one commit") {
    val db = Recording()
    def inner: Eff[B, Long] = Tx.atomic[T, Long, B](Tx.update[B]("inner"))
    val p: Eff[R, Long] = Tx.atomic[T, Long, R](Tx.update[B]("outer").flatMap(_ => inner))
    assertEquals(run(Tx.interpret[Long, T](db)(p)), 1L)
    assertEquals(db.log.toList, List("begin ReadCommitted", "update outer", "update inner", "commit"))
  }

  test("an Async program runs INSIDE the body") {
    val db = Recording()
    val seen = ArrayBuffer.empty[String]
    val body: Unit ! Async = okay.defer { seen += "inside"; pure(()) }
    val p: Eff[R, Long] = Tx.atomic[T, Long, R](Tx.async[Unit, B](body).flatMap(_ => Tx.update[B]("x")))
    assertEquals(run(Tx.interpret[Long, T](db)(p)), 1L)
    assertEquals(seen.toList, List("inside"))
    assertEquals(db.log.toList, List("begin ReadCommitted", "update x", "commit"))
  }

  test("a statement outside any bracket does not type: the connection is read inside one only") {
    assert(compileErrors("""okay.sql.Tx.update[okay.sql.Tx.Txn[okay.core.+:[okay.Async, okay.core.Pure]] +:
      okay.core.+:[okay.Async, okay.core.Pure]]("select 1")""").nonEmpty, "a statement outside a bracket compiled")
  }

  test("a failure inside the body rolls back, and goes on as the failure (the indexed tree's caveat is gone)") {
    val db = Recording()
    val p: Eff[R, Long] = Tx.atomic[T, Long, R](
      Tx.update[B]("a").flatMap(_ => Tx.async[Long, B](okay.defer(throw RuntimeException("boom")))))
    val e = intercept[RuntimeException](run(Tx.interpret[Long, T](db)(p)))
    assertEquals(e.getMessage, "boom")
    assertEquals(db.log.toList, List("begin ReadCommitted", "update a", "rollback"))
  }

  test("a rollback is a leaf: nothing after it runs; outside, Rolled") {
    val db = Recording()
    val p: Eff[R, Long] = Tx.atomic[T, Long, R](
      Tx.update[B]("a").flatMap(_ => Tx.rollback[T, B](RuntimeException("no funds"))).flatMap(_ => Tx.update[B]("b")))
    val e = intercept[okay.tx.Tx.Rolled](run(Tx.interpret[Long, T](db)(p)))
    assertEquals(e.reason.getMessage, "no funds")
    assertEquals(db.log.toList, List("begin ReadCommitted", "update a", "rollback"))
  }

  test("a serialization failure the driver classifies runs the bracket again, within `retries`") {
    val db = Recording(failCommitsWith = Some("40001"))
    var calls = 0
    val p: Eff[R, Long] = Tx.atomic[T, Long, R](Tx.update[B]("x").flatMap { n =>
      calls += 1
      if calls < 2 then Tx.async[Long, B](okay.defer(throw RuntimeException("conflict"))) else Cont.pure(n)
    })
    assertEquals(run(Tx.interpret[Long, T](db, Isolation.Serializable, retries = 1)(p)), 1L)
    assertEquals(db.log.toList,
      List("begin Serializable", "update x", "rollback", "begin Serializable", "update x", "commit"))
  }
