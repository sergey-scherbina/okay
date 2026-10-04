package okay2.pg

import okay2.{!, Pure, Resource, Throws, pure}
import okay2.async.Async
import okay2.platform._
import okay2.sql.{Granted, Isolation, Sql, SqlType, SqlValue, Typed}
import okay2.stream.{Chunk, ChunkBuf, Source}
import Models._

/**
 * The wire against a REAL Postgres (okay-pg's TestPg): Live, skips where
 * the endpoint is absent. SCRAM is proven by connecting at all: the
 * container authenticates scram-sha-256 and nothing else.
 */
class TestPg extends PgLive {

  override def beforeAll(): Unit =
    if (available) withDb { db =>
      run(db.update("drop table if exists customer")): Unit
      run(db.update("""create table customer(
        id bigint primary key not null,
        user_name varchar(64) not null,
        age int,
        balance double precision not null,
        active boolean not null,
        avatar bytea)""")): Unit
      run(db.update("insert into customer values " +
        "(1, 'ann', 25, 10.5, true, '\\x0102')," +
        "(2, 'bob', null, -3.25, false, null)")): Unit
      run(db.update("drop table if exists big")): Unit
      run(db.update("create table big(n int not null, label varchar(32) not null)")): Unit
      run(db.update("insert into big select g, 'row-' || g from generate_series(1, 500) g")): Unit
    }

  def region[A](prog: A ! (Resource with Async)): A = run(Resource.run[A, Async](prog))

  test("startup + SCRAM-SHA-256 lands a working session (and a bad password refuses)") {
    assume(available, skipped)
    withDb { db =>
      assertEquals(chunks(db.query("select 1")).flatten, List(Vector[SqlValue](SqlValue.I32(1))))
    }
    intercept[PgError](run(PgSql.connect(host, port, "okay", "wrong-password", "okay"))): Unit
  }

  test("the typed layer runs over the wire: rows by label, verify with catalog nullability") {
    assume(available, skipped)
    withDb { db =>
      // pg_attribute answers nullability, so a clean verify needs no
      // Option-everything concession here
      assertEquals(run(Typed.verify[Customer](db, "select * from customer")), Vector.empty)
      val rs = chunks(Typed.rows[Customer](db, "select * from customer order by id")).flatten
      assertEquals(rs.length, 2)
      val ann = rs.head.toOption.get
      assertEquals(ann.userName, "ann")
      assertEquals(ann.age, Some(25))
      assertEquals(ann.avatar.map(_.toList), Some(List[Byte](1, 2)))
      assertEquals(rs(1).toOption.get.age, None)
      val drifts = run(Typed.verify[Customer](db, "select id, age, balance, active, avatar from customer"))
      assertEquals(drifts.map(_.column), Vector("user_name"))
    }
  }

  test("portal streaming: 500 rows arrive at fetch-size chunks — the protocol IS the fetch-size story") {
    assume(available, skipped)
    withDb { db =>
      val cs = chunks(Typed.rows[Big](db, "select * from big order by n"))
      assertEquals(cs.map(_.length), List(64, 64, 64, 64, 64, 64, 64, 52))
      assertEquals(cs.flatten.collect { case Right(r) => r.n }.take(3), List(1, 2, 3))
      assert(cs.flatten.forall(_.isRight))
    }
  }

  test("params bind, updates count, an error names itself and the session survives") {
    assume(available, skipped)
    withDb { db =>
      assertEquals(run(Typed.update(db, "insert into customer(id, user_name, balance, active) values ($1, $2, $3, $4)")(
        NewRow(10, "dee", 5.0, true))), 1L)
      val e = intercept[PgError](run(db.update("select syntax error from")))
      assert(e.getMessage.nonEmpty)
      // the connection reached quiet and keeps working
      assertEquals(run(db.update("delete from customer where id = 10")), 1L)
    }
  }

  test("transact over the wire: granted isolation read back; an abort rolls back") {
    assume(available, skipped)
    withDb { db =>
      val g = region(Typed.transact[Granted, Async](db, Isolation.RepeatableRead)(g => pure[Async, Granted](g)))
      assertEquals(g.granted, Isolation.RepeatableRead: Isolation)
      assert(!g.downgraded)
      val prog = Typed.transact[Long, Throws[String]](db, Isolation.ReadCommitted) { _ =>
        db.update("insert into customer(id, user_name, balance, active) values (30, 'tx', 1, true)")
          .flatMap(_ => Throws.raise[String, Long]("no"))
      }
      assertEquals(region(Throws.runEither[Long, String, Resource with Async](prog)), Left("no"))
      val n = chunks(db.query("select count(*) from customer where id = 30")).flatten
      assertEquals(n.head.head, SqlValue.I64(0): SqlValue, "the insert survived the abort")
    }
  }

  test("a handled error inside a region: pg's COMMIT answers ROLLBACK, and the region must not report success (sql-commit-tag)") {
    assume(available, skipped)
    withDb { db =>
      // the body inserts, then RECOVERS from a failed statement; pg is now
      // in the aborted state, and its COMMIT answers the tag ROLLBACK
      val prog = Typed.transact[Long, Async](db, Isolation.ReadCommitted) { _ =>
        db.update("insert into customer(id, user_name, balance, active) values (31, 'tag', 1, true)")
          .flatMap(_ => Async {
            try run(db.update("select syntax error from"))
            catch { case _: PgError => 0L }
          })
      }
      val e = intercept[PgError](region(prog))
      assert(e.getMessage.contains("ROLLBACK"), e.getMessage)
      val n = chunks(db.query("select count(*) from customer where id = 31")).flatten
      assertEquals(n.head.head, SqlValue.I64(0): SqlValue, "the aborted transaction's insert is not there")
      // and the connection is usable afterwards, outside any transaction
      assertEquals(run(db.update("delete from customer where id = 31")), 0L)
    }
  }

  test("write skew under Serializable: the loser gets 40001 through the wire, and transactRetry re-runs it (sql-serialization-retry)") {
    assume(available, skipped)
    val a = connect(); val b = connect()
    try {
      run(a.update("drop table if exists ssi")): Unit
      run(a.update("create table ssi(k int not null)")): Unit
      def count(db: Sql): Long ! Async =
        Source.concat(db.query("select count(*) from ssi")).map(_.head.head match {
          case SqlValue.I64(n) => n
          case other => throw new AssertionError(s"count: $other")
        })
      // a's whole transaction, run to completion INSIDE b's first run,
      // between b's read and b's write — the classic rw-conflict cycle
      def aWrites(): Unit =
        region(Typed.transact[Long, Async](a, Isolation.Serializable)(_ =>
          count(a).flatMap(_ => a.update("insert into ssi values (1)")))): Unit
      var runs = 0
      val r = run(Typed.transactRetry(b, Isolation.Serializable, Typed.Retry(3)) { _ =>
        count(b).flatMap { _ =>
          runs += 1
          if (runs == 1) aWrites()
          b.update("insert into ssi values (2)")
        }
      })
      assertEquals(r.attempts, 2, "the first run lost to a, the second landed")
      assertEquals(run(count(b)), 2L)
      // and the loser's failure, seen raw, is the SQLSTATE the retry keys on
      run(b.update("delete from ssi")): Unit
      runs = 0
      val e = intercept[PgError](run(Typed.transactRetry(b, Isolation.Serializable) { _ =>
        count(b).flatMap { _ =>
          runs += 1
          if (runs == 1) aWrites()
          b.update("insert into ssi values (2)")
        }
      }))
      assertEquals(b.sqlState(e), Some("40001"), e.getMessage)
    } finally { a.close(); b.close() }
  }

  test("sql-temporal-types over the wire: timestamptz/timestamp/date/time/uuid/jsonb and a timestamptz[] read typed, bind back exact, verify clean") {
    assume(available, skipped)
    withDb { db =>
      run(db.update("drop table if exists stamps")): Unit
      run(db.update("create table stamps(id int not null, at timestamptz not null, plain timestamp not null, " +
        "d date not null, t time not null, ref uuid not null, doc jsonb not null, ats timestamptz[] not null)")): Unit
      // the session zone is whatever the server has; the offset on the wire is applied, not assumed
      run(db.update("set time zone 'Europe/Kyiv'")): Unit
      run(db.update("insert into stamps values (1, '2026-09-02 06:00:00+00', '2026-09-02 06:00:00', '2026-09-02', " +
        "'06:00:00.5', '6ba7b810-9dad-11d1-80b4-00c04fd430c8', '{\"k\": [1, 2]}', " +
        "array['2026-09-02 06:00:00+00', '1969-12-31 23:59:59.999999+00']::timestamptz[])")): Unit
      val six = java.time.Instant.parse("2026-09-02T06:00:00Z")
      val one = Stamp(1, six, six, java.time.LocalDate.of(2026, 9, 2), java.time.LocalTime.of(6, 0, 0, 500000000),
        java.util.UUID.fromString("6ba7b810-9dad-11d1-80b4-00c04fd430c8"), "{\"k\": [1, 2]}",
        Vector(six, java.time.Instant.parse("1969-12-31T23:59:59.999999Z")))
      val sql = "select id, at, plain, d, t, ref, doc, ats from stamps order by id"
      assertEquals(run(db.describe(sql)).map(_.tpe), Vector[SqlType](SqlType.I32, SqlType.Timestamp, SqlType.Timestamp,
        SqlType.Date, SqlType.Time, SqlType.Uuid, SqlType.Json, SqlType.Arr(SqlType.Timestamp)))
      assertEquals(run(Typed.verify[Stamp](db, sql)), Vector.empty)
      assertEquals(chunks(Typed.rows[Stamp](db, sql)).flatten, List(Right(one)))
      val two = one.copy(id = 2, at = six.plusNanos(1000), plain = java.time.Instant.parse("1969-12-31T23:59:59.999999Z"),
        d = java.time.LocalDate.of(1899, 12, 31), t = java.time.LocalTime.of(23, 59, 59, 999999000),
        ref = java.util.UUID.randomUUID(), doc = "{\"z\": true}", ats = Vector.empty)
      assertEquals(run(Typed.update(db, "insert into stamps values ($1, $2, $3, $4, $5, $6, $7, $8)")(two)), 1L)
      assertEquals(chunks(Typed.rows[Stamp](db, sql)).flatten, List(Right(one), Right(two)))
      run(db.update("set time zone 'UTC'")): Unit
    }
  }

  test("a READ ONLY region on the wire: granted and read back, a write inside answers 25006, writes work again after (sql-readonly-region)") {
    assume(available, skipped)
    withDb { db =>
      val e = intercept[PgError](region(
        Typed.transact[Long, Async](db, Isolation.ReadCommitted, readOnly = true) { g =>
          assert(g.readOnly, "the server granted READ ONLY")
          db.update("insert into customer(id, user_name, balance, active) values (40, 'ro', 1, true)")
        }))
      assertEquals(db.sqlState(e), Some("25006"), e.getMessage)
      val g = region(Typed.transact[Granted, Async](db, readOnly = true)(g => pure[Async, Granted](g)))
      assert(g.readOnly)
      val plain = region(Typed.transact[Granted, Async](db)(g => pure[Async, Granted](g)))
      assert(!plain.readOnly)
      assertEquals(run(db.update("insert into customer(id, user_name, balance, active) values (40, 'rw', 1, true)")), 1L)
      assertEquals(run(db.update("delete from customer where id = 40")), 1L)
    }
  }

  test("nested transact refuses loudly on the wire too") {
    assume(available, skipped)
    withDb { db =>
      val prog = Typed.transact[Granted, Async](db)(_ => Typed.transact[Granted, Async](db)(g2 => pure[Async, Granted](g2)))
      val e = intercept[IllegalStateException](!.run(Async.run[Granted, Pure](Resource.run[Granted, Async](prog))))
      assert(e.getMessage.contains("nested"))
    }
  }

  test("batch: one parse, many binds, summed counts") {
    assume(available, skipped)
    withDb { db =>
      run(db.update("drop table if exists batched")): Unit
      run(db.update("create table batched(n int not null)")): Unit
      val rows: Chunk[Vector[SqlValue]] = ChunkBuf.of((1 to 10).map(i => Vector[SqlValue](SqlValue.I32(i))))
      assertEquals(run(db.batch("insert into batched values ($1)", rows)), 10L)
      assertEquals(chunks(db.query("select count(*) from batched")).flatten.head.head, SqlValue.I64(10): SqlValue)
    }
  }
}
