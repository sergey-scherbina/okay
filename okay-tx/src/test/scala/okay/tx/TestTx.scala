package okay.tx

import okay.one.{!, Cont}
import okay.one.Cont.{defer, pure}
import okay.one.row.*
import okay.one.std.*

/** TRANSACTIONS AS A TREE: one begin and one commit however deep the brackets, a rollback or a failure anywhere
 * inside rolls back, an abort crossing rolls back, the owner's retry runs the subtree again */
class TestTx extends munit.FunSuite:
  type T = Writer % String +: Throws % String +: Pure
  type Conn = Int
  /** the owner: the log tells what it does; the connection a number */
  object owner extends Tx.Transactor[Conn, T]:
    def begin = Writer.tell[String, T]("BEGIN").map(_ => 7)
    def commit(c: Conn) = Writer.tell[String, T](s"COMMIT $c")
    def rollback(c: Conn) = Writer.tell[String, T](s"ROLLBACK $c")
  type R = Txs[Conn, T] +: T
  type B = Txs[Conn, T] +: Reader % Conn +: T
  def sql(s: String): Unit ! B = Tx.resource[Conn, B].flatMap(c => Writer.tell[String, B](s"$s on $c"))
  def run[A](p: A ! R): (List[String], Either[String, A]) =
    p.handle(Tx.closed(owner)).handle(Writer.collect).handle(Throws.either).run match
      case Left(e) => (Nil, Left(e))
      case Right((log, a)) => (log, Right(a))
  def runRec[A](p: A ! R): (List[String], Either[String, A]) =
    run[Either[String, A]](p.map(Right(_): Either[String, A]).recover(e => pure(Left(e.getMessage)))) match
      case (log, Right(r)) => (log, r)
      case (log, Left(e)) => (log, Left(e))

  def charge(ok: Boolean): Unit ! B = Tx.atomic[Conn, T, Unit, B](
    sql("update balance").flatMap(_ => if ok then sql("insert payment") else Tx.rollback[Conn, T, B](RuntimeException("no funds"))))
  def place(ok: Boolean): Unit ! R = Tx.atomic[Conn, T, Unit, R](sql("insert order").flatMap(_ => charge(ok)))

  test("brackets inside brackets: one BEGIN, one COMMIT; the resource read inside at any depth") {
    assertEquals(run(place(true)),
      (List("BEGIN", "insert order on 7", "update balance on 7", "insert payment on 7", "COMMIT 7"), Right(())))
  }

  test("a rollback in the inner bracket rolls the whole transaction back at once; Rolled outside") {
    assertEquals(runRec(place(false)), (List("BEGIN", "insert order on 7", "update balance on 7", "ROLLBACK 7"), Left("no funds")))
  }

  test("a failure anywhere inside rolls back and goes on as the failure") {
    val boom = RuntimeException("boom")
    val p: Unit ! R = Tx.atomic[Conn, T, Unit, R](sql("a").flatMap(_ => defer(throw boom)))
    assertEquals(runRec(p), (List("BEGIN", "a on 7", "ROLLBACK 7"), Left("boom")))
  }

  test("an abort crossing the bracket — a raise to a handler outside — rolls back") {
    val p: Unit ! R = Tx.atomic[Conn, T, Unit, R](sql("a").flatMap(_ => Throws.raise[String, B]("stop")))
    assertEquals(run(p), (Nil, Left("stop")))
    // the log is inside `either`: see it with the handlers the other way round
    val seen = p.handle(Tx.closed(owner)).handle(Throws.either).handle(Writer.collect).run
    assertEquals(seen, (List("BEGIN", "a on 7", "ROLLBACK 7"), Left("stop")))
  }

  test("the owner's retry: a deadlock rolls back and runs the subtree again; a Rollback is never retried") {
    var calls = 0
    object retrying extends Tx.Transactor[Conn, T]:
      def begin = Writer.tell[String, T]("BEGIN").map(_ => 1)
      def commit(c: Conn) = Writer.tell[String, T]("COMMIT")
      def rollback(c: Conn) = Writer.tell[String, T]("ROLLBACK")
      override def retry(e: Throwable, attempt: Int) = e.getMessage == "deadlock" && attempt < 3
    val flaky: Unit ! R = Tx.atomic[Conn, T, Unit, R](sql("x").flatMap { _ => calls += 1; if calls < 2 then defer(throw RuntimeException("deadlock")) else pure(()) })
    val r = flaky.handle(Tx.closed(retrying)).handle(Writer.collect).handle(Throws.either).run
    assertEquals(r, Right((List("BEGIN", "x on 1", "ROLLBACK", "BEGIN", "x on 1", "COMMIT"), ())))
    calls = 0
    val rolled: Unit ! R = Tx.atomic[Conn, T, Unit, R](sql("y").flatMap(_ => Tx.rollback[Conn, T, B](RuntimeException("deadlock"))))
    val r2 = rolled.map(_ => "no").recover(e => pure(e.getMessage)).handle(Tx.closed(retrying)).handle(Writer.collect).handle(Throws.either).run
    assertEquals(r2, Right((List("BEGIN", "y on 1", "ROLLBACK"), "deadlock")))
  }

  test("a recover around the bracket, inside the transaction's scope, sees the rollback") {
    val p: String ! R = place(false).map(_ => "placed").recover(e => pure(s"not placed: ${e.getMessage}"))
      .flatMap(s => Writer.tell[String, R]("after").map(_ => s))
    assertEquals(run(p), (List("BEGIN", "insert order on 7", "update balance on 7", "ROLLBACK 7", "after"), Right("not placed: no funds")))
  }

  test("a rollback with no transaction open is a failure where it is") {
    val p: Unit ! R = Tx.rollback[Conn, T, R](RuntimeException("none"))
    assertEquals(runRec(p), (Nil, Left("none")))
  }
