package okay.jdbc

import okay.{!, %, +, Async, Chunk, Handler, Produce, Resource, Throws, effect}
import okay.given
import okay.codec.Schema
import okay.persist.{MemoryStore, Typed as PTyped}
import okay.sql.{Granted, Isolation, Sql, SqlValue, Typed}
import java.nio.file.Files
import java.sql.DriverManager

/**
 * SQLite through the seam: the embedded engine everyone actually
 * has, proving the same typed layer with zero new machinery — and
 * two things SQLite does BETTER than the H2 battery could show:
 * native `ON CONFLICT DO NOTHING` serves the Writes bridge in its
 * spec-preferred spelling, and the read-only open mode is the
 * embedded world's no-DDL posture (there are no users to GRANT to;
 * "their database" is a file you were handed, possibly read-only).
 */
class TestSqlite extends munit.FunSuite {

  final case class Customer(id: Long, userName: String, age: Option[Int],
                            balance: Double, active: Boolean,
                            avatar: Option[Array[Byte]])
  given Schema[Customer] = Schema.derived

  private var dbFile: String = null

  override def beforeAll(): Unit =
    dbFile = Files.createTempDirectory("okay-sqlite").resolve("their.db").toString
    val c = DriverManager.getConnection(s"jdbc:sqlite:$dbFile")
    try
      val st = c.createStatement()
      st.execute("""create table customer(
        id integer primary key not null,
        user_name text not null,
        age integer,
        balance real not null,
        active boolean not null,
        avatar blob)""")
      st.execute("insert into customer values " +
        "(1, 'ann', 25, 10.5, 1, x'0102')," +
        "(2, 'bob', null, -3.25, 0, null)")
      st.close()
    finally c.close()

  def withDb[A](f: Sql => A): A =
    val conn = DriverManager.getConnection(s"jdbc:sqlite:$dbFile")
    try f(JdbcSql(conn))
    finally conn.close()

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

  test("rows decode by label from SQLite's honest metadata; verify passes") {
    withDb { db =>
      assertEquals(run(Typed.verify[Customer](db, "select * from customer")), Vector.empty)
      val rs = collectChunks(Typed.rows[Customer](db, "select * from customer order by id")).flatten
      assertEquals(rs.length, 2)
      val ann = rs.head.toOption.get
      assertEquals(ann.userName, "ann")
      assertEquals(ann.age, Some(25))
      assertEquals(ann.avatar.map(_.toList), Some(List[Byte](1, 2)))
      val bob = rs(1).toOption.get
      assertEquals(bob.age, None)
      assertEquals(bob.balance, -3.25)
    }
  }

  test("verify names a drift: the dropped column, on SQLite metadata") {
    withDb { db =>
      val drifts = run(Typed.verify[Customer](db,
        "select id, age, balance, active, avatar from customer"))
      assertEquals(drifts.map(_.column), Vector("user_name"))
      assertEquals(drifts.head.found, "absent")
    }
  }

  test("transact: SQLite grants both levels it maps; commit and abort behave") {
    val conn = DriverManager.getConnection(s"jdbc:sqlite:$dbFile")
    try
      val db = JdbcSql(conn)
      // granted isolation is read back, not assumed
      val g = !.run(Async.run[Granted, Nothing](Resource.run[Granted, Async](
        Typed.transact[Granted, Async](db, Isolation.Serializable)(g => okay.pure(g)))))
      assertEquals(g.granted, Isolation.Serializable)

      // a handled abort crossing the scope rolls back
      type G = Throws % String
      val prog = Typed.transact[Long, G](db, Isolation.Serializable) { _ =>
        !.widen[Long, Async, Resource + G](
          db.update("insert into customer(id, user_name, balance, active) values (50, 'x', 1, 1)"))
          .flatMap(_ => effect[Resource + Async + G, Long](Throws("no")))
      }
      val out = !.run(Async.run[Either[String, Long], Nothing](
        Resource.run[Either[String, Long], Async](
          okay.runEither[Long, Resource + Async, String](prog))))
      assertEquals(out, Left("no"))
      assert(conn.getAutoCommit)
      val n = collectChunks(db.query("select count(*) c from customer where id = 50")).flatten
      assertEquals(n.head.head, SqlValue.I32(0))
    finally conn.close()
  }

  test("the Writes bridge over NATIVE on-conflict: the crash-window retry lands once") {
    withDb { db =>
      // the spec-preferred WithKey spelling, verbatim
      val upsert = "insert into customer(id, user_name, balance, active) " +
        "values (?, ?, ?, ?) on conflict(id) do nothing"
      val params = Vector(SqlValue.I64(60), SqlValue.Text("sq"),
        SqlValue.F64(6.0), SqlValue.Bool(true))
      val topic = MemoryStore().topic("writes")
      // the crash: intent journaled, statement executed, ack lost
      PTyped[Writes.Rec](topic, 1, Map.empty).append(0, "run-1".getBytes("UTF-8"),
        Writes.Rec.Intent(0, upsert, params, "60"), okay.persist.Ack.Durable): Unit
      assertEquals(run(db.update(upsert, params)), 1L)

      val w = Writes(db, topic, "run-1")
      assertEquals(run(w.recover(_ => Writes.Policy.WithKey)),
        Vector(Writes.Recovered.Reapplied("60", 0L)))
      val n = collectChunks(db.query("select count(*) c from customer where id = 60")).flatten
      assertEquals(n.head.head, SqlValue.I32(1), "the retry duplicated the row")
      assertEquals(run(db.update("delete from customer where id = 60")), 1L)
    }
  }

  test("read-only open mode is the embedded no-DDL posture: reads full, writes refuse") {
    val props = new java.util.Properties()
    props.setProperty("open_mode", "1") // SQLITE_OPEN_READONLY
    val conn = DriverManager.getConnection(s"jdbc:sqlite:$dbFile", props)
    try
      val db = JdbcSql(conn)
      // the whole typed read path works against the handed file
      assertEquals(run(Typed.verify[Customer](db, "select * from customer")), Vector.empty)
      val rs = collectChunks(Typed.rows[Customer](db, "select * from customer")).flatten
      assertEquals(rs.length, 2)
      // and any write — DML or DDL — refuses
      intercept[java.sql.SQLException](
        run(db.update("insert into customer(id, user_name, balance, active) values (70, 'w', 0, 1)"))): Unit
      intercept[java.sql.SQLException](run(db.update("create table mine(x int)")))
    finally conn.close()
  }
}
