package okay.tx

import java.sql.{Connection, DriverManager, SQLException}
import okay.one.{!, Cont}
import okay.one.Cont.{defer, pure}
import okay.one.row.*
import okay.one.std.*

/** the JDBC transactor on H2: brackets inside brackets one transaction on one connection, a rollback leaves nothing,
 * a serialization failure runs the bracket again */
class TestJdbc extends munit.FunSuite:
  val url = "jdbc:h2:mem:tx;DB_CLOSE_DELAY=-1"
  var opened = 0
  def connect(): Connection = { opened += 1; DriverManager.getConnection(url) }
  type T = Pure
  type R = Txs[Connection, T] +: T
  type B = Txs[Connection, T] +: Reader % Connection +: T
  def count(): Int =
    val c = DriverManager.getConnection(url)
    try { val rs = c.createStatement().executeQuery("select count(*) from orders"); rs.next(); rs.getInt(1) } finally c.close()

  override def beforeEach(ctx: BeforeEach): Unit =
    val c = DriverManager.getConnection(url)
    try c.createStatement().execute("drop table if exists orders; create table orders(id int primary key, paid boolean)") finally c.close()
    opened = 0

  def charge(id: Int, ok: Boolean): Unit ! B = Tx.atomic[Connection, T, Unit, B](
    Jdbc.execute[B](s"update orders set paid = true where id = $id").flatMap(_ =>
      if ok then pure(()) else Tx.rollback[Connection, T, B](RuntimeException("no funds"))))
  def place(id: Int, ok: Boolean): Unit ! R = Tx.atomic[Connection, T, Unit, R](
    Jdbc.execute[B](s"insert into orders values ($id, false)").flatMap(_ => charge(id, ok)))

  test("nested brackets: one connection, one commit; the rows there after") {
    place(1, true).handle(Tx.closed(Jdbc[T](connect))).run
    assertEquals((count(), opened), (1, 1))
  }

  test("a rollback in the inner bracket: nothing committed, the connection given back") {
    val r = place(2, false).map(_ => "placed").recover(e => pure(e.getMessage)).handle(Tx.closed(Jdbc[T](connect))).run
    assertEquals((r, count(), opened), ("no funds", 0, 1))
  }

  test("a serialization failure runs the bracket again on a fresh connection; the second run commits") {
    var attempts = 0
    val flaky: Unit ! R = Tx.atomic[Connection, T, Unit, R](
      Jdbc.execute[B]("insert into orders values (3, true)").flatMap { _ =>
        attempts += 1
        if attempts == 1 then defer(throw SQLException("could not serialize", "40001")) else pure(()) })
    flaky.handle(Tx.closed(Jdbc[T](connect, retries = 2))).run
    assertEquals((count(), attempts, opened), (1, 2, 2))
  }

  test("the isolation asked is the connection's inside the bracket") {
    val iso: Int ! R = Tx.atomic[Connection, T, Int, R](Tx.resource[Connection, B].map(_.getTransactionIsolation))
    assertEquals(iso.handle(Tx.closed(Jdbc[T](connect, isolation = Connection.TRANSACTION_SERIALIZABLE))).run,
      Connection.TRANSACTION_SERIALIZABLE)
  }
