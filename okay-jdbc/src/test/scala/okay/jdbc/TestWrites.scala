package okay.jdbc

import okay.{!, Async}
import okay.given
import okay.persist.{Ack, MemoryStore, Topic, Typed}
import okay.sql.{Sql, SqlValue}
import java.sql.DriverManager

/**
 * The write bridge against the crash it exists for (specs/jdbc.md):
 * an insert with a natural key, "crashed" between journal and
 * completion, retried under WithKey lands ONCE — their unique
 * constraint dedups; under Reconcile the SELECT by key settles the
 * journal without re-executing anything.
 */
class TestWrites extends munit.FunSuite {

  val url = "jdbc:h2:mem:writes;DB_CLOSE_DELAY=-1"

  override def beforeAll(): Unit =
    val c = DriverManager.getConnection(url, "sa", "")
    try
      val st = c.createStatement()
      st.execute("create table orders(id varchar(32) primary key, amount double precision not null)")
      st.close()
    finally c.close()

  def withDb[A](f: Sql => A): A =
    val conn = DriverManager.getConnection(url, "sa", "")
    try f(JdbcSql(conn))
    finally conn.close()

  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  def clear(db: Sql): Unit = { run(db.update("delete from orders")); () }

  val merge = "merge into orders key(id) values (?, ?)"
  def params(id: String, amount: Double) =
    Vector(SqlValue.Text(id), SqlValue.F64(amount))

  def orderCount(db: Sql, id: String): Long =
    var n = -1L
    val c = DriverManager.getConnection(url, "sa", "")
    try
      val ps = c.prepareStatement("select count(*) from orders where id = ?")
      ps.setString(1, id)
      val rs = ps.executeQuery(); rs.next(); n = rs.getLong(1)
      ps.close()
    finally c.close()
    n

  test("a clean write journals intent then completion, and lands") {
    withDb { db =>
      clear(db)
      val topic = MemoryStore().topic("writes")
      val w = Writes(db, topic, "run-1")
      assertEquals(run(w.write(merge, params("ord-1", 10.0), "ord-1")), 1L)
      assertEquals(orderCount(db, "ord-1"), 1L)
      val es = w.entries
      assertEquals(es.length, 1)
      assertEquals(es.head._1.key, "ord-1")
      assertEquals(es.head._2, Some(1L))
    }
  }

  test("WithKey: the crash-window write, retried with the same key, lands once") {
    withDb { db =>
      clear(db)
      val topic = MemoryStore().topic("writes")
      // the crash: intent journaled, statement EXECUTED, ack lost
      // before the completion record — the worst window
      Typed[Writes.Rec](topic, 1, Map.empty).append(0, "run-1".getBytes("UTF-8"),
        Writes.Rec.Intent(0, merge, params("ord-2", 20.0), "ord-2"), Ack.Durable)
      assertEquals(run(db.update(merge, params("ord-2", 20.0))), 1L)

      // the process comes back: a fresh bridge over the same topic
      val w = Writes(db, topic, "run-1")
      val out = run(w.recover(_ => Writes.Policy.WithKey))
      assertEquals(out, Vector(Writes.Recovered.Reapplied("ord-2", 1L)))
      assertEquals(orderCount(db, "ord-2"), 1L, "the retry duplicated the row")
      assertEquals(w.entries.head._2.isDefined, true, "the journal was not settled")
      // a second recovery finds nothing open
      assertEquals(run(w.recover(_ => Writes.Policy.WithKey)), Vector.empty)
    }
  }

  test("Reconcile: the SELECT by key settles the journal without re-executing") {
    withDb { db =>
      clear(db)
      val topic = MemoryStore().topic("writes")
      // a NON-idempotent plain insert this time: re-execution would
      // throw on the primary key — proving reconcile never re-runs
      val insert = "insert into orders(id, amount) values (?, ?)"
      Typed[Writes.Rec](topic, 1, Map.empty).append(0, "run-1".getBytes("UTF-8"),
        Writes.Rec.Intent(0, insert, params("ord-3", 30.0), "ord-3"), Ack.Durable)
      assertEquals(run(db.update(insert, params("ord-3", 30.0))), 1L)

      val w = Writes(db, topic, "run-1")
      val out = run(w.recover(_ =>
        Writes.Policy.Reconcile("select id from orders where id = ?")))
      assertEquals(out, Vector(Writes.Recovered.Settled("ord-3", 1L)))
      assertEquals(orderCount(db, "ord-3"), 1L)
      assertEquals(run(w.recover(_ => Writes.Policy.Fail)), Vector.empty)
    }
  }

  test("Reconcile that finds nothing, and Fail: Unresolved as data, world untouched") {
    withDb { db =>
      clear(db)
      val topic = MemoryStore().topic("writes")
      // the other crash: intent journaled, statement NEVER ran
      Typed[Writes.Rec](topic, 1, Map.empty).append(0, "run-1".getBytes("UTF-8"),
        Writes.Rec.Intent(0, merge, params("ord-4", 40.0), "ord-4"), Ack.Durable)

      val w = Writes(db, topic, "run-1")
      val rec = run(w.recover(_ =>
        Writes.Policy.Reconcile("select id from orders where id = ?")))
      rec match
        case Vector(Writes.Recovered.Unresolved("ord-4", why)) => assert(why.nonEmpty)
        case other => fail(s"expected Unresolved, got $other")
      val failed = run(w.recover(_ => Writes.Policy.Fail))
      assertEquals(failed.length, 1)
      assertEquals(orderCount(db, "ord-4"), 0L, "recovery touched the world under Fail/empty Reconcile")
      // the entry stays open for a later, better answer
      assertEquals(w.entries.head._2, None)
    }
  }

  test("sequence numbers continue over restart") {
    withDb { db =>
      clear(db)
      val topic = MemoryStore().topic("writes")
      val w1 = Writes(db, topic, "run-1")
      assertEquals(run(w1.write(merge, params("a", 1.0), "a")), 1L)
      val w2 = Writes(db, topic, "run-1")
      assertEquals(run(w2.write(merge, params("b", 2.0), "b")), 1L)
      assertEquals(w2.entries.map(_._1.seq), Vector(0, 1))
    }
  }
}
