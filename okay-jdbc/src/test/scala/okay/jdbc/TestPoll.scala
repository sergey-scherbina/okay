package okay.jdbc

import okay.{!, Async}
import okay.given
import okay.codec.Schema
import okay.persist.{MemoryStore, Offsets, Store}
import okay.sql.Sql
import java.sql.DriverManager

/**
 * The watermark poll (specs/jdbc.md): resumes from the journaled
 * watermark; and the late-commit caveat is DEMONSTRATED, not
 * hidden — one test shows the miss, the next shows the lag-window
 * mitigation holding the watermark back.
 */
class TestPoll extends munit.FunSuite {

  final case class Ev(seq: Long, payload: String)
  given Schema[Ev] = Schema.derived

  val url = "jdbc:h2:mem:poll;DB_CLOSE_DELAY=-1"

  override def beforeAll(): Unit =
    val c = DriverManager.getConnection(url, "sa", "")
    try
      val st = c.createStatement()
      st.execute("create table events(seq bigint primary key, payload varchar(64) not null)")
      // nullable payload: the damage fixture (Ev.payload is not Option)
      st.execute("create table events_d(seq bigint primary key, payload varchar(64))")
      st.close()
    finally c.close()

  def withDb[A](f: Sql => A): A =
    val conn = DriverManager.getConnection(url, "sa", "")
    try f(JdbcSql(conn))
    finally conn.close()

  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  def insert(db: Sql, seqs: Long*): Unit =
    seqs.foreach(s => run(db.update(s"insert into events values ($s, 'p-$s')")))
    ()

  def clear(db: Sql): Unit = { run(db.update("delete from events")); () }

  val bySeq = "select seq, payload from events where seq > ? order by seq"

  test("poll resumes from the journaled watermark, across a restart") {
    withDb { db =>
      clear(db)
      val store: Store = MemoryStore()
      val p1 = Poll(db, Offsets(store), "g", "events")
      insert(db, 1, 2, 3, 4, 5)
      val b1 = run(p1.poll[Ev](bySeq)(_.seq))
      assertEquals(b1.rows.map(_.seq), Vector(1L, 2L, 3L, 4L, 5L))
      assertEquals(b1.watermark, 5L)

      insert(db, 6, 7, 8)
      assertEquals(run(p1.poll[Ev](bySeq)(_.seq)).rows.map(_.seq), Vector(6L, 7L, 8L))

      // the restart: a fresh Poll over a fresh Offsets, same store
      val p2 = Poll(db, Offsets(store), "g", "events")
      assertEquals(p2.watermark, 8L)
      assertEquals(run(p2.poll[Ev](bySeq)(_.seq)).rows, Vector.empty)
      insert(db, 9)
      assertEquals(run(p2.poll[Ev](bySeq)(_.seq)).rows.map(_.seq), Vector(9L))
      // groups are independent: another group replays from start
      assertEquals(run(Poll(db, Offsets(store), "g2", "events")
        .poll[Ev](bySeq)(_.seq)).rows.length, 9)
    }
  }

  test("the late-commit caveat, DOCUMENTED: a smaller value behind the watermark is missed") {
    withDb { db =>
      clear(db)
      val p = Poll(db, Offsets(MemoryStore()), "g", "events")
      // a gap: 11 is a transaction still in flight when 12 commits
      insert(db, 10, 12)
      assertEquals(run(p.poll[Ev](bySeq)(_.seq)).rows.map(_.seq), Vector(10L, 12L))
      // ...and it commits late, BEHIND the watermark
      insert(db, 11)
      val after = run(p.poll[Ev](bySeq)(_.seq))
      // this is the miss the spec refuses to hide: the watermark
      // (12) already passed 11 — the row is invisible to this poll,
      // which is exactly why this reader is NOT CDC
      assertEquals(after.rows, Vector.empty)
      assertEquals(after.watermark, 12L)
    }
  }

  test("the lag window holds the watermark back, so the late commit is not missed") {
    withDb { db =>
      clear(db)
      val p = Poll(db, Offsets(MemoryStore()), "g", "events")
      // the same story, but the caller's SQL declares the window:
      // do not trust the newest ε of the column (here: seq <= 2)
      val windowed = "select seq, payload from events where seq > ? and seq <= 2 order by seq"
      insert(db, 1, 2, 4)   // 3 is the in-flight transaction
      val b1 = run(p.poll[Ev](windowed)(_.seq))
      assertEquals(b1.rows.map(_.seq), Vector(1L, 2L))
      assertEquals(b1.watermark, 2L, "the window held the watermark back of the gap")

      insert(db, 3)         // the late commit — still ahead of the watermark
      val b2 = run(p.poll[Ev](bySeq)(_.seq))
      assertEquals(b2.rows.map(_.seq), Vector(3L, 4L), "the late row was not missed")
    }
  }

  test("a damaged row stops the watermark: nothing is silently skipped") {
    withDb { db =>
      clear(db)
      // seq 2 carries a NULL payload; Ev.payload is not Option
      val byseqD = "select seq, payload from events_d where seq > ? order by seq"
      run(db.update("insert into events_d values (1, 'ok')"))
      run(db.update("insert into events_d(seq) values (2)"))
      run(db.update("insert into events_d values (3, 'ok')"))
      val p = Poll(db, Offsets(MemoryStore()), "g", "events_d")
      val b = run(p.poll[Ev](byseqD)(_.seq))
      assertEquals(b.rows.map(_.seq), Vector(1L))
      assert(b.damage.isDefined, "the damage did not surface")
      assertEquals(b.watermark, 1L, "the watermark passed a row that did not decode")
      // the fix arrives; the next poll re-serves from the damage on
      run(db.update("update events_d set payload = 'fixed' where seq = 2"))
      assertEquals(run(p.poll[Ev](byseqD)(_.seq)).rows.map(_.seq), Vector(2L, 3L))
    }
  }
}
