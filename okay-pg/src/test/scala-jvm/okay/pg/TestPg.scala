package okay.pg

import okay.{!, %, +, Async, Chunk, Handler, Produce, Resource, Throws, effect}
import okay.given
import okay.crypto.given
import okay.codec.Schema
import okay.sql.{Granted, Isolation, Sql, SqlValue, Typed}

/**
 * The wire against a REAL Postgres (the live-suite pattern: skips
 * where the endpoint is absent — a local server or the okay-pg
 * docker container, okay/okay/okay on 5432). SCRAM is proven by
 * connecting at all: the container authenticates scram-sha-256
 * and nothing else.
 */
class TestPg extends munit.FunSuite {

  // integration-test-gate: out of the default gate, into `sbt integrationTest`
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  val host = sys.env.getOrElse("OKAY_PG_HOST", "127.0.0.1")
  val port = sys.env.get("OKAY_PG_PORT").flatMap(_.toIntOption).getOrElse(5432)

  def connect(): PgSql = okay.!.run(okay.Async.run[PgSql, Nothing](PgSql.connect(host, port, "okay", "okay", "okay")))

  lazy val available: Boolean =
    try { connect().close(); true }
    catch { case _: Throwable => false }

  override def beforeAll(): Unit =
    if !available then return
    val db = connect()
    try
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
      ()
    finally db.close()

  def withDb[A](f: Sql => A): A =
    val db = connect()
    try f(db)
    finally db.close()

  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  def collectChunks[A](s: Chunk[A] ! (Produce + Async)): List[Chunk[A]] =
    import okay.!.*
    def go(rest: Chunk[A] ! (Produce + Async), acc: List[Chunk[A]]): List[Chunk[A]] =
      (rest.resume: @unchecked) match
        case Pure(_) => acc.reverse
        case Effect(e) => okay.<|>[Async, Produce](e) match
          case Left(a) => (summon[Handler[Async]].handle(a): Unit); acc.reverse
          case Right(c) => (c.asInstanceOf[Chunk[A]] :: acc).reverse
        case Bind(Effect(e), k) => okay.<|>[Async, Produce](e) match
          case Left(a) => go(k(summon[Handler[Async]].handle(a)), acc)
          case Right(c) => go(k(c), c.asInstanceOf[Chunk[A]] :: acc)
    go(s, Nil)

  final case class Customer(id: Long, userName: String, age: Option[Int],
                            balance: Double, active: Boolean,
                            avatar: Option[Array[Byte]])
  given Schema[Customer] = Schema.derived

  test("startup + SCRAM-SHA-256 lands a working session (and a bad password refuses)") {
    assume(available, s"no Postgres at $host:$port — the live suite skips")
    withDb { db =>
      val one = collectChunks(db.query("select 1")).flatten
      assertEquals(one, List(Vector(SqlValue.I32(1))))
    }
    intercept[PgError](okay.!.run(okay.Async.run[PgSql, Nothing](PgSql.connect(host, port, "okay", "wrong-password", "okay"))))
  }

  test("the typed layer runs over the wire: rows by label, verify with catalog nullability") {
    assume(available, s"no Postgres at $host:$port — the live suite skips")
    withDb { db =>
      // pg_attribute answers nullability, so a clean verify needs no
      // Option-everything concession here — unlike the Parquet road
      assertEquals(run(Typed.verify[Customer](db, "select * from customer")), Vector.empty)
      val rs = collectChunks(Typed.rows[Customer](db, "select * from customer order by id")).flatten
      assertEquals(rs.length, 2)
      val ann = rs.head.toOption.get
      assertEquals(ann.userName, "ann")
      assertEquals(ann.age, Some(25))
      assertEquals(ann.avatar.map(_.toList), Some(List[Byte](1, 2)))
      assertEquals(rs(1).toOption.get.age, None)

      val drifts = run(Typed.verify[Customer](db,
        "select id, age, balance, active, avatar from customer"))
      assertEquals(drifts.map(_.column), Vector("user_name"))
    }
  }

  test("portal streaming: 500 rows arrive at fetch-size chunks — the protocol IS the fetch-size story") {
    assume(available, s"no Postgres at $host:$port — the live suite skips")
    withDb { db =>
      final case class Row(n: Int, label: String)
      given Schema[Row] = Schema.derived
      val chunks = collectChunks(Typed.rows[Row](db, "select * from big order by n"))
      assertEquals(chunks.map(_.length), List(64, 64, 64, 64, 64, 64, 64, 52))
      assertEquals(chunks.flatten.collect { case Right(r) => r.n }.take(3), List(1, 2, 3))
      assert(chunks.flatten.forall(_.isRight))
    }
  }

  test("params bind, updates count, an error names itself and the session survives") {
    assume(available, s"no Postgres at $host:$port — the live suite skips")
    withDb { db =>
      final case class NewRow(id: Long, userName: String, balance: Double, active: Boolean)
      given Schema[NewRow] = Schema.derived
      assertEquals(run(Typed.update(db,
        "insert into customer(id, user_name, balance, active) values ($1, $2, $3, $4)")(
        NewRow(10, "dee", 5.0, true))), 1L)
      val e = intercept[PgError](run(db.update("select syntax error from")))
      assert(e.getMessage.nonEmpty)
      // the connection reached quiet and keeps working
      assertEquals(run(db.update("delete from customer where id = 10")), 1L)
    }
  }

  test("transact over the wire: granted isolation read back; an abort rolls back") {
    assume(available, s"no Postgres at $host:$port — the live suite skips")
    withDb { db =>
      val g = !.run(Async.run[Granted, Nothing](Resource.run[Granted, Async](
        Typed.transact[Granted, Async](db, Isolation.RepeatableRead)(g => okay.pure(g)))))
      assertEquals(g.granted, Isolation.RepeatableRead)
      assert(!g.downgraded)

      type G = Throws % String
      val prog = Typed.transact[Long, G](db, Isolation.ReadCommitted) { _ =>
        !.widen[Long, Async, Resource + G](
          db.update("insert into customer(id, user_name, balance, active) values (30, 'tx', 1, true)"))
          .flatMap(_ => effect[Resource + Async + G, Long](Throws("no")))
      }
      val out = !.run(Async.run[Either[String, Long], Nothing](
        Resource.run[Either[String, Long], Async](
          okay.runEither[Long, Resource + Async, String](prog))))
      assertEquals(out, Left("no"))
      val n = collectChunks(db.query("select count(*) from customer where id = 30")).flatten
      assertEquals(n.head.head, SqlValue.I64(0), "the insert survived the abort")
    }
  }

  test("nested transact refuses loudly on the wire too") {
    assume(available, s"no Postgres at $host:$port — the live suite skips")
    withDb { db =>
      val prog = Typed.transact[Granted, Async](db) { _ =>
        Typed.transact[Granted, Async](db)(g2 => okay.pure(g2))
      }
      val e = intercept[IllegalStateException](
        !.run(Async.run[Granted, Nothing](Resource.run[Granted, Async](prog))))
      assert(e.getMessage.contains("nested"))
    }
  }

  test("batch: one parse, many binds, summed counts") {
    assume(available, s"no Postgres at $host:$port — the live suite skips")
    withDb { db =>
      run(db.update("drop table if exists batched")): Unit
      run(db.update("create table batched(n int not null)")): Unit
      val rows = okay.ChunkBuf.of((1 to 10).map(i => Vector[SqlValue](SqlValue.I32(i))))
      assertEquals(run(db.batch("insert into batched values ($1)", rows)), 10L)
      val n = collectChunks(db.query("select count(*) from batched")).flatten
      assertEquals(n.head.head, SqlValue.I64(10))
    }
  }
}
