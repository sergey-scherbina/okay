package okay.outbox

import okay.*
import okay.given
import okay.codec.Schema
import okay.jdbc.JdbcSql
import okay.persist.{Ack, MemoryStore, Store, Topic, Policy}
import okay.sql.{Sql, SqlValue, Typed}
import java.sql.DriverManager

/**
 * The outbox and the inbox over H2 and a MemoryStore (specs/outbox.md):
 * one fate for the rows and the event, the relay's at-least-once said
 * out loud, and the inbox collapsing what the relay repeats.
 */
class TestOutboxInbox extends munit.FunSuite:

  def run[A](p: A ! Async): A = Async.run(p).runWith

  var now = 1_000L
  val clock: () => Long = () => now
  private var n = 0
  private var seq = 0

  final case class Order(id: String, total: Long) derives Schema

  def fixture(): (Sql, Outbox, Inbox) =
    n += 1; seq = 0
    Class.forName("org.h2.Driver")
    val conn = DriverManager.getConnection(s"jdbc:h2:mem:outbox$n;DB_CLOSE_DELAY=-1")
    val outbox = Outbox(clock = clock, ids = () => { seq += 1; f"id-$seq%03d" })
    val inbox = Inbox(clock = clock)
    val st = conn.createStatement()
    st.execute("create table orders(id varchar(16) primary key, total bigint not null)")
    st.execute(outbox.ddl(Dialect.H2))
    st.execute(inbox.ddl(Dialect.H2))
    st.close()
    (JdbcSql(conn), outbox, inbox)

  def orders(db: Sql): Vector[Order] =
    run(Rows.all[Order](db, "select id, total from orders order by id"))

  def placeOrder(db: Sql, outbox: Outbox, o: Order, abort: Boolean = false): Unit ! (Resource + Async) =
    Typed.transact[Unit, Async](db) { _ =>
      for
        _ <- !.widen[Long, Async, Resource](Typed.update[Order](db, "insert into orders (id, total) values (?, ?)")(o))
        _ <- !.widen[String, Async, Resource](outbox.enqueue(db, "orders", s"placed ${o.id}".getBytes("UTF-8"), key = o.id.getBytes("UTF-8")))
        _ <- !.widen[Unit, Async, Resource](okay.async(if abort then throw RuntimeException("payment declined")))
      yield ()
    }

  def records(t: Topic): Vector[(String, String)] =
    t.read(0, t.begin(0), 100) match
      case Topic.Read.Records(rs) => rs.map(r => (new String(r.key, "UTF-8"), new String(r.value, "UTF-8")))
      case Topic.Read.TooEarly(_) => fail("too early")

  test("one fate: a committed transaction's event is relayed, an aborted one's is not") {
    val (db, outbox, _) = fixture()
    val store = MemoryStore()
    run(Resource.run[Unit, Async](placeOrder(db, outbox, Order("o1", 10))))
    assertEquals(intercept[RuntimeException](run(Resource.run[Unit, Async](placeOrder(db, outbox, Order("o2", 20), abort = true)))).getMessage, "payment declined")
    assertEquals(orders(db).map(_.id), Vector("o1"))         // the rows share the fate
    assertEquals(run(outbox.pending(db)), 1L)

    assertEquals(run(outbox.relayOnce(db, store)), 1)
    assertEquals(records(store.topic("orders")), Vector(("id-001", "placed o1")))
    assertEquals(run(outbox.pending(db)), 0L)
    assertEquals(run(outbox.relayOnce(db, store)), 0)        // nothing pending, nothing appended
    assertEquals(records(store.topic("orders")).size, 1)
  }

  test("creation order, batches, and the id as the record key") {
    val (db, outbox, _) = fixture()
    val store = MemoryStore()
    for i <- 1 to 5 do
      now += 1
      run(Resource.run[Unit, Async](placeOrder(db, outbox, Order(s"o$i", i))))
    assertEquals(run(outbox.relayOnce(db, store, batch = 2)), 2)
    assertEquals(run(outbox.relayOnce(db, store, batch = 2)), 2)
    assertEquals(run(outbox.relayOnce(db, store, batch = 2)), 1)
    assertEquals(records(store.topic("orders")).map(_._1), (1 to 5).map(i => f"id-$i%03d").toVector)
  }

  /** a store whose topic appends fine and whose relay then dies
    * before the mark: the crash window, forced */
  final class Dying(inner: Store, var diesAfterAppend: Boolean) extends Store:
    def topic(name: String, partitions: Int, policy: Policy): Topic =
      val t = inner.topic(name, partitions, policy)
      new Topic:
        def name = t.name
        def partitions = t.partitions
        def append(partition: Int, key: Array[Byte], value: Array[Byte], ack: Ack): Long =
          val off = t.append(partition, key, value, ack)
          if diesAfterAppend then throw RuntimeException("relay died after the append")
          off
        def read(partition: Int, from: Long, max: Int) = t.read(partition, from, max)
        def begin(partition: Int) = t.begin(partition)
        def end(partition: Int) = t.end(partition)
        def compact(partition: Int): Unit = t.compact(partition)
    def topics = inner.topics
    def stats = inner.stats

  test("at-least-once: a relay that dies between append and mark re-appends the same key; the inbox collapses it") {
    val (db, outbox, inbox) = fixture()
    val memory = MemoryStore()
    val store = Dying(memory, diesAfterAppend = true)
    run(Resource.run[Unit, Async](placeOrder(db, outbox, Order("o1", 10))))
    assertEquals(intercept[RuntimeException](run(outbox.relayOnce(db, store))).getMessage, "relay died after the append")
    assertEquals(run(outbox.pending(db)), 1L)                // not marked: will be relayed again
    store.diesAfterAppend = false
    assertEquals(run(outbox.relayOnce(db, store)), 1)
    val rs = records(memory.topic("orders"))
    assertEquals(rs, Vector(("id-001", "placed o1"), ("id-001", "placed o1")))   // twice, same key

    // the consumer: an effect per record, through the inbox
    var shipped = Vector.empty[String]
    def ship(key: String): Option[Unit] =
      run(inbox.once(db, key)(okay.async { shipped :+= key }))
    assertEquals(ship(rs(0)._1), Some(()))
    assertEquals(ship(rs(1)._1), None)
    assertEquals(shipped, Vector("id-001"))
  }

  test("inbox: first once per id; a failing body leaves the id unrecorded so the record is tried again") {
    val (db, _, inbox) = fixture()
    val transactional = Typed.transact[Boolean, Async](db)(_ => !.widen[Boolean, Async, Resource](inbox.first(db, "m1")))
    assertEquals(run(Resource.run[Boolean, Async](transactional)), true)
    assertEquals(run(Resource.run[Boolean, Async](transactional)), false)

    var tries = 0
    def attempt(): Option[Int] = run(inbox.once(db, "m2") { okay.async { tries += 1; if tries == 1 then throw RuntimeException("db hiccup") else tries } })
    assertEquals(intercept[RuntimeException](attempt()).getMessage, "db hiccup")
    assertEquals(attempt(), Some(2))                          // the first try was rolled back with its id
    assertEquals(attempt(), None)
  }

  test("ddl renders per dialect") {
    val o = Outbox()
    assert(o.ddl(Dialect.Postgres).contains("msg_value bytea not null"))
    assert(o.ddl(Dialect.Sqlite).contains("msg_key blob"))
    assert(Inbox().ddl(Dialect.Postgres).contains("id varchar(64) primary key"))
    val _ = SqlValue.Null
  }
