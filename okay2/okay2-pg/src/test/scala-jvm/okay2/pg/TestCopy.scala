package okay2.pg

import okay2.sql.{Isolation, SqlValue, Typed}
import Models._

/**
 * COPY through the wire and the load-id posture on the free engine
 * (okay-pg's TestCopy): the retry after a crash between journal and
 * commit lands ONCE, because the registry row and the data commit
 * together. Live; skips where Postgres is absent.
 */
class TestCopy extends PgLive {

  override def beforeAll(): Unit =
    if (available) withDb { db =>
      run(db.update("drop table if exists bulk")): Unit
      run(db.update("create table bulk(id bigint not null, label text, amount double precision)")): Unit
      run(db.update("drop table if exists okay_loads")): Unit
      run(Load.ensure(db))
    }

  def countBulk(db: PgSql): Long =
    chunks(db.query("select count(*) from bulk")).flatten.head.head match {
      case SqlValue.I64(n) => n
      case other => fail(s"expected a count, got $other")
    }

  test("copyIn streams a thousand rows in one command; special characters survive the text format") {
    assume(available, skipped)
    withDb { db =>
      run(db.update("truncate bulk")): Unit
      val rows = (1 to 1000).iterator.map(i =>
        PgSql.copyRow(Vector(SqlValue.I64(i.toLong), SqlValue.Text(s"row-$i"), SqlValue.F64(i / 2.0))))
      assertEquals(run(db.copyIn("copy bulk (id, label, amount) from stdin", rows)), 1000L)
      assertEquals(countBulk(db), 1000L)
      // the escapes: tab, newline, backslash, NULL — round-trip
      val tricky = Vector(
        Vector[SqlValue](SqlValue.I64(2001), SqlValue.Text("tab\there"), SqlValue.Null),
        Vector[SqlValue](SqlValue.I64(2002), SqlValue.Text("line\nbreak"), SqlValue.F64(1.0)),
        Vector[SqlValue](SqlValue.I64(2003), SqlValue.Text("back\\slash"), SqlValue.F64(2.0)))
      assertEquals(run(db.copyIn("copy bulk (id, label, amount) from stdin", tricky.iterator.map(PgSql.copyRow))), 3L)
      val back = chunks(Typed.rows[Label](db, "select label from bulk where id >= 2001 order by id")).flatten
      assertEquals(back.collect { case Right(r) => r.label },
        List(Some("tab\there"), Some("line\nbreak"), Some("back\\slash")))
    }
  }

  test("the load id dedups: the same load twice lands once, and says so") {
    assume(available, skipped)
    withDb { db =>
      run(db.update("truncate bulk")): Unit
      run(db.update("delete from okay_loads")): Unit
      val rows = (1 to 50).toVector.map(i => Vector[SqlValue](SqlValue.I64(i.toLong), SqlValue.Text(s"r$i"), SqlValue.F64(1.0)))
      assertEquals(run(Load.load(db, "batch-2026-09-01", "bulk", Vector("id", "label", "amount"), rows)),
        Load.Result.Loaded(50L): Load.Result)
      // the retry — the ack was lost, the caller loads again
      assertEquals(run(Load.load(db, "batch-2026-09-01", "bulk", Vector("id", "label", "amount"), rows)),
        Load.Result.AlreadyLoaded: Load.Result)
      assertEquals(countBulk(db), 50L)
    }
  }

  test("a crash between COPY and commit rolls back the CLAIM too: the retry lands, once overall") {
    assume(available, skipped)
    withDb { db => run(db.update("truncate bulk")): Unit; run(db.update("delete from okay_loads")): Unit }
    val rows = (1 to 20).toVector.map(i => Vector[SqlValue](SqlValue.I64(i.toLong), SqlValue.Text(s"r$i"), SqlValue.F64(1.0)))
    // the crash: claim + COPY, then the connection DIES uncommitted
    val dying = connect()
    run(dying.begin(Isolation.ReadCommitted)): Unit
    assertEquals(run(dying.update("insert into okay_loads(load_id) values ($1) on conflict do nothing",
      Vector(SqlValue.Text("batch-crash")))), 1L)
    assertEquals(run(dying.copyIn("copy bulk (id, label, amount) from stdin", rows.iterator.map(PgSql.copyRow))), 20L)
    dying.close() // no commit: the server rolls back claim AND data together
    withDb { db =>
      assertEquals(countBulk(db), 0L, "uncommitted COPY data survived the crash")
      // the claim rolled back with the data, so the load runs — once overall
      assertEquals(run(Load.load(db, "batch-crash", "bulk", Vector("id", "label", "amount"), rows)),
        Load.Result.Loaded(20L): Load.Result)
      assertEquals(countBulk(db), 20L)
    }
  }
}
