package okay.tx

import java.sql.{Connection, DriverManager, SQLException}
import okay.one.!
import okay.one.Cont.{defer, pure}
import okay.one.row.*
import okay.one.std.*

/** `Sessions` on H2: the bracket's connection a guarded session — its statements commit, a session kept past its
 * bracket is `Session.Closed`, nested brackets one session, a retry a fresh one */
class TestSessions extends okay.testkit.Munit.Diagnosed:
  val url = "jdbc:h2:mem:sessions;DB_CLOSE_DELAY=-1"
  var opened = 0
  def connect(): Connection = { opened += 1; DriverManager.getConnection(url) }
  type T = Pure
  type R = Txs[Session, T] +: T
  type B = Txs[Session, T] +: Reader % Session +: T
  def count(): Int =
    val c = DriverManager.getConnection(url)
    try { val rs = c.createStatement().executeQuery("select count(*) from orders"); rs.next(); rs.getInt(1) } finally c.close()

  override def beforeEach(ctx: BeforeEach): Unit =
    val c = DriverManager.getConnection(url)
    try c.createStatement().execute("drop table if exists orders; create table orders(id int primary key)") finally c.close()
    opened = 0

  def insert(id: Int): Unit ! B = Sessions.execute[B](s"insert into orders values ($id)")

  test("a session's statements in its bracket commit; nested brackets one session") {
    val inner: Unit ! B = Tx.atomic[Session, T, Unit, B](insert(2))
    val p: Int ! R = Tx.atomic[Session, T, Int, R](insert(1).flatMap(_ => inner).flatMap(_ =>
      Sessions.queryInt[B]("select count(*) from orders")))
    assertEquals(p.handle(Tx.closed(Sessions[T](connect))).run, 2)
    assertEquals((count(), opened), (2, 1))
  }

  test("a session kept past its commit: Session.Closed, naming the leak") {
    val kept: (() => Unit) ! R = Tx.atomic[Session, T, () => Unit, R](
      Tx.resource[Session, B].map(s => () => s.execute("insert into orders values (9)")))
    val late = kept.handle(Tx.closed(Sessions[T](connect))).run
    val e = intercept[Session.Closed](late())
    note(e.getMessage)
    assertEquals(e.getMessage,
      "session closed: 'insert into orders values (9)' after its bracket's commit or rollback — the session leaked out of the bracket")
    assertEquals(count(), 0)
  }

  test("a session kept past a rollback: Session.Closed; nothing committed") {
    var kept: Option[Session] = None
    val p: Unit ! R = Tx.atomic[Session, T, Unit, R](Tx.resource[Session, B].flatMap { s =>
      kept = Some(s)
      insert(5).flatMap(_ => Tx.rollback[Session, T, B](RuntimeException("no")))
    })
    val r = p.map(_ => "done").recover(e => pure(e.getMessage)).handle(Tx.closed(Sessions[T](connect))).run
    assertEquals(r, "no")
    intercept[Session.Closed](kept.get.queryInt("select 1")): Unit
    intercept[Session.Closed](kept.get.withConnection(_.isClosed)): Unit
    assertEquals(count(), 0)
  }

  test("a serialization failure runs the bracket again on a fresh session") {
    var attempts = 0
    val flaky: Unit ! R = Tx.atomic[Session, T, Unit, R](insert(3).flatMap { _ =>
      attempts += 1
      if attempts == 1 then defer(throw SQLException("could not serialize", "40001")) else pure(()) })
    flaky.handle(Tx.closed(Sessions[T](connect, retries = 2))).run
    assertEquals((count(), attempts, opened), (1, 2, 2))
  }
