package okay2.jdbc

import java.sql.DriverManager
import okay2.codec.Schema
import okay2.persist.{MemoryStore, Offsets, Store}
import okay2.sql.Sql

final case class Ev(seq: Long, payload: String)
object Ev {
  implicit val schema: Schema[Ev] = Schema.derived
}

/**
 * The watermark poll (okay-jdbc's TestPoll, specs/jdbc.md): resumes from
 * the journaled watermark; and the late-commit caveat is DEMONSTRATED,
 * not hidden — one test shows the miss, the next shows the lag-window
 * mitigation holding the watermark back.
 */
class TestPoll extends munit.FunSuite {

  val url = "jdbc:h2:mem:poll;DB_CLOSE_DELAY=-1"

  override def beforeAll(): Unit = {
    val c = DriverManager.getConnection(url, "sa", "")
    try {
      val st = c.createStatement()
      st.execute("create table events(seq bigint primary key, payload varchar(64) not null)")
      // nullable payload: the damage fixture (Ev.payload is not Option)
      st.execute("create table events_d(seq bigint primary key, payload varchar(64))")
      st.close()
    } finally c.close()
  }

  def withDb[A](f: Sql => A): A = {
    val conn = DriverManager.getConnection(url, "sa", "")
    try f(new JdbcSql(conn))
    finally conn.close()
  }

  def insert(db: Sql, seqs: Long*): Unit =
    seqs.foreach(s => Run(db.update(s"insert into events values ($s, 'p-$s')")))

  def clear(db: Sql): Unit = Run(db.update("delete from events")): Unit

  val bySeq = "select seq, payload from events where seq > ? order by seq"

  test("poll resumes from the journaled watermark, across a restart") {
    withDb { db =>
      clear(db)
      val store: Store = new MemoryStore
      val p1 = new Poll(db, Offsets(store), "g", "events")
      insert(db, 1, 2, 3, 4, 5)
      val b1 = Run(p1.poll[Ev](bySeq)(_.seq))
      assertEquals(b1.rows.map(_.seq), Vector(1L, 2L, 3L, 4L, 5L))
      assertEquals(b1.watermark, 5L)

      insert(db, 6, 7, 8)
      assertEquals(Run(p1.poll[Ev](bySeq)(_.seq)).rows.map(_.seq), Vector(6L, 7L, 8L))

      // the restart: a fresh Poll over a fresh Offsets, same store
      val p2 = new Poll(db, Offsets(store), "g", "events")
      assertEquals(p2.watermark, 8L)
      assertEquals(Run(p2.poll[Ev](bySeq)(_.seq)).rows, Vector.empty[Ev])
      insert(db, 9)
      assertEquals(Run(p2.poll[Ev](bySeq)(_.seq)).rows.map(_.seq), Vector(9L))
      // groups are independent: another group replays from start
      assertEquals(Run(new Poll(db, Offsets(store), "g2", "events").poll[Ev](bySeq)(_.seq)).rows.length, 9)
    }
  }

  test("the late-commit caveat, DOCUMENTED: a smaller value behind the watermark is missed") {
    withDb { db =>
      clear(db)
      val p = new Poll(db, Offsets(new MemoryStore), "g", "events")
      // a gap: 11 is a transaction still in flight when 12 commits
      insert(db, 10, 12)
      assertEquals(Run(p.poll[Ev](bySeq)(_.seq)).rows.map(_.seq), Vector(10L, 12L))
      // ...and it commits late, BEHIND the watermark
      insert(db, 11)
      val after = Run(p.poll[Ev](bySeq)(_.seq))
      // the miss the spec refuses to hide: this reader is NOT CDC
      assertEquals(after.rows, Vector.empty[Ev])
      assertEquals(after.watermark, 12L)
    }
  }

  test("the lag window holds the watermark back, so the late commit is not missed") {
    withDb { db =>
      clear(db)
      val p = new Poll(db, Offsets(new MemoryStore), "g", "events")
      // the caller's SQL declares the window: do not trust the newest ε
      val windowed = "select seq, payload from events where seq > ? and seq <= 2 order by seq"
      insert(db, 1, 2, 4) // 3 is the in-flight transaction
      val b1 = Run(p.poll[Ev](windowed)(_.seq))
      assertEquals(b1.rows.map(_.seq), Vector(1L, 2L))
      assertEquals(b1.watermark, 2L, "the window held the watermark back of the gap")

      insert(db, 3) // the late commit — still ahead of the watermark
      val b2 = Run(p.poll[Ev](bySeq)(_.seq))
      assertEquals(b2.rows.map(_.seq), Vector(3L, 4L), "the late row was not missed")
    }
  }

  test("a damaged row stops the watermark: nothing is silently skipped") {
    withDb { db =>
      clear(db)
      // seq 2 carries a NULL payload; Ev.payload is not Option
      val bySeqD = "select seq, payload from events_d where seq > ? order by seq"
      Run(db.update("insert into events_d values (1, 'ok')")): Unit
      Run(db.update("insert into events_d(seq) values (2)")): Unit
      Run(db.update("insert into events_d values (3, 'ok')")): Unit
      val p = new Poll(db, Offsets(new MemoryStore), "g", "events_d")
      val b = Run(p.poll[Ev](bySeqD)(_.seq))
      assertEquals(b.rows.map(_.seq), Vector(1L))
      assert(b.damage.isDefined, "the damage did not surface")
      assertEquals(b.watermark, 1L, "the watermark passed a row that did not decode")
      // the fix arrives; the next poll re-serves from the damage on
      Run(db.update("update events_d set payload = 'fixed' where seq = 2")): Unit
      assertEquals(Run(p.poll[Ev](bySeqD)(_.seq)).rows.map(_.seq), Vector(2L, 3L))
    }
  }
}
