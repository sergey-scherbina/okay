package okay2.jdbc

import java.sql.DriverManager
import okay2.persist.{Ack, MemoryStore, Typed}
import okay2.sql.{Sql, SqlValue}

/**
 * The write bridge against the crash it exists for (okay-jdbc's
 * TestWrites, specs/jdbc.md): an insert with a natural key, "crashed"
 * between journal and completion, retried under WithKey lands ONCE —
 * their unique constraint dedups; under Reconcile the SELECT by key
 * settles the journal without re-executing anything.
 */
class TestWrites extends munit.FunSuite {

  val url = "jdbc:h2:mem:writes;DB_CLOSE_DELAY=-1"

  override def beforeAll(): Unit = {
    val c = DriverManager.getConnection(url, "sa", "")
    try c.createStatement().execute("create table orders(id varchar(32) primary key, amount double precision not null)"): Unit
    finally c.close()
  }

  def withDb[A](f: Sql => A): A = {
    val conn = DriverManager.getConnection(url, "sa", "")
    try f(new JdbcSql(conn))
    finally conn.close()
  }

  def clear(db: Sql): Unit = Run(db.update("delete from orders")): Unit

  val merge = "merge into orders key(id) values (?, ?)"
  def params(id: String, amount: Double): Vector[SqlValue] = Vector(SqlValue.Text(id), SqlValue.F64(amount))

  def orderCount(id: String): Long = {
    val c = DriverManager.getConnection(url, "sa", "")
    try {
      val ps = c.prepareStatement("select count(*) from orders where id = ?")
      ps.setString(1, id)
      val rs = ps.executeQuery(); rs.next()
      val n = rs.getLong(1)
      ps.close()
      n
    } finally c.close()
  }

  /** the crash: an intent journaled with no completion after it */
  def intentOnly(topic: okay2.persist.Topic, sql: String, ps: Vector[SqlValue], key: String): Unit =
    Typed[Writes.Rec](topic, 1, Map.empty).append(0, "run-1".getBytes("UTF-8"),
      Writes.Rec.Intent(0, sql, ps, key), Ack.Durable): Unit

  test("a clean write journals intent then completion, and lands") {
    withDb { db =>
      clear(db)
      val topic = new MemoryStore().topic("writes")
      val w = new Writes(db, topic, "run-1")
      assertEquals(Run(w.write(merge, params("ord-1", 10.0), "ord-1")), 1L)
      assertEquals(orderCount("ord-1"), 1L)
      val es = w.entries
      assertEquals(es.length, 1)
      assertEquals(es.head._1.key, "ord-1")
      assertEquals(es.head._2, Some(1L))
    }
  }

  test("WithKey: the crash-window write, retried with the same key, lands once") {
    withDb { db =>
      clear(db)
      val topic = new MemoryStore().topic("writes")
      // the crash: intent journaled, statement EXECUTED, ack lost before
      // the completion record — the worst window
      intentOnly(topic, merge, params("ord-2", 20.0), "ord-2")
      assertEquals(Run(db.update(merge, params("ord-2", 20.0))), 1L)

      // the process comes back: a fresh bridge over the same topic
      val w = new Writes(db, topic, "run-1")
      val out = Run(w.recover(_ => Writes.Policy.WithKey))
      assertEquals(out, Vector[Writes.Recovered](Writes.Recovered.Reapplied("ord-2", 1L)))
      assertEquals(orderCount("ord-2"), 1L, "the retry duplicated the row")
      assertEquals(w.entries.head._2.isDefined, true, "the journal was not settled")
      // a second recovery finds nothing open
      assertEquals(Run(w.recover(_ => Writes.Policy.WithKey)), Vector.empty[Writes.Recovered])
    }
  }

  test("Reconcile: the SELECT by key settles the journal without re-executing") {
    withDb { db =>
      clear(db)
      val topic = new MemoryStore().topic("writes")
      // a NON-idempotent plain insert: re-execution would throw on the
      // primary key — proving reconcile never re-runs
      val insert = "insert into orders(id, amount) values (?, ?)"
      intentOnly(topic, insert, params("ord-3", 30.0), "ord-3")
      assertEquals(Run(db.update(insert, params("ord-3", 30.0))), 1L)

      val w = new Writes(db, topic, "run-1")
      val out = Run(w.recover(_ => Writes.Policy.Reconcile("select id from orders where id = ?")))
      assertEquals(out, Vector[Writes.Recovered](Writes.Recovered.Settled("ord-3", 1L)))
      assertEquals(orderCount("ord-3"), 1L)
      assertEquals(Run(w.recover(_ => Writes.Policy.Fail)), Vector.empty[Writes.Recovered])
    }
  }

  test("Reconcile that finds nothing, and Fail: Unresolved as data, world untouched") {
    withDb { db =>
      clear(db)
      val topic = new MemoryStore().topic("writes")
      // the other crash: intent journaled, statement NEVER ran
      intentOnly(topic, merge, params("ord-4", 40.0), "ord-4")

      val w = new Writes(db, topic, "run-1")
      Run(w.recover(_ => Writes.Policy.Reconcile("select id from orders where id = ?"))) match {
        case Vector(Writes.Recovered.Unresolved("ord-4", why)) => assert(why.nonEmpty)
        case other => fail(s"expected Unresolved, got $other")
      }
      val failed = Run(w.recover(_ => Writes.Policy.Fail))
      assertEquals(failed.length, 1)
      assertEquals(orderCount("ord-4"), 0L, "recovery touched the world under Fail/empty Reconcile")
      // the entry stays open for a later, better answer
      assertEquals(w.entries.head._2, None)
    }
  }

  test("sequence numbers continue over restart") {
    withDb { db =>
      clear(db)
      val topic = new MemoryStore().topic("writes")
      val w1 = new Writes(db, topic, "run-1")
      assertEquals(Run(w1.write(merge, params("a", 1.0), "a")), 1L)
      val w2 = new Writes(db, topic, "run-1")
      assertEquals(Run(w2.write(merge, params("b", 2.0), "b")), 1L)
      assertEquals(w2.entries.map(_._1.seq), Vector(0, 1))
    }
  }

  test("every SqlValue a parameter can be survives the journal") {
    val topic = new MemoryStore().topic("values")
    val all: Vector[SqlValue] = Vector(SqlValue.Null, SqlValue.Bool(true), SqlValue.I32(1), SqlValue.I64(2L),
      SqlValue.F64(1.5), SqlValue.Text("t"), SqlValue.Num(BigDecimal("12.345")),
      SqlValue.Arr(Vector(SqlValue.I32(1), SqlValue.Null)), SqlValue.Row(Vector(SqlValue.Text("r"))),
      SqlValue.Timestamp(5L), SqlValue.Date(6), SqlValue.Time(7L),
      SqlValue.Uuid(java.util.UUID.fromString("123e4567-e89b-12d3-a456-426614174000")), SqlValue.Json("{}"))
    intentOnly(topic, "x", all, "k")
    withDb(db => assertEquals(new Writes(db, topic, "run-1").entries.head._1.params, all))
  }
}
