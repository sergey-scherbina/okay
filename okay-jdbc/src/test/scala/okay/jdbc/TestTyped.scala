package okay.jdbc

import okay.{!, %, +, Async, Chunk, Handler, Produce, Resource, Throws, effect}
import okay.given
import okay.codec.Schema
import okay.sql.{Bad, Drift, Granted, Isolation, Sql, SqlValue, Typed}
import java.sql.DriverManager

/**
 * The specs/jdbc.md behavior list, through the specs/sql.md seam:
 * the typed layer written against `Sql`, exercised over the JDBC
 * driver on H2 — and the whole suite runs AS A USER WITH NO DDL
 * RIGHTS against tables created by "someone else" (the admin
 * setup), which is the posture the spec exists for.
 */
class TestTyped extends munit.FunSuite {

  final case class Customer(id: Long, userName: String, age: Option[Int],
                            balance: Double, active: Boolean,
                            avatar: Option[Array[Byte]])
  given Schema[Customer] = Schema.derived

  /** the drifted view of the same table: age as non-Option */
  final case class Strict(id: Long, age: Int)
  given Schema[Strict] = Schema.derived

  // DB_CLOSE_DELAY is a SET statement H2 runs per connection and it
  // needs admin rights — so only the admin URL carries it (the
  // setting itself is per-database, once is enough)
  val adminUrl = "jdbc:h2:mem:typed;DB_CLOSE_DELAY=-1"
  val url = "jdbc:h2:mem:typed"

  override def beforeAll(): Unit =
    // "someone else's database": the admin creates the schema and a
    // user with NO DDL rights and limited DML
    val admin = DriverManager.getConnection(adminUrl, "sa", "")
    try
      val st = admin.createStatement()
      st.execute("""create table customer(
        id bigint not null primary key,
        user_name varchar(64) not null,
        age int,
        balance double precision not null,
        active boolean not null,
        avatar varbinary(16))""")
      st.execute("create table big(n int not null, label varchar(32) not null)")
      st.execute("insert into customer values " +
        "(1, 'ann', 25, 10.5, true, x'0102')," +
        "(2, 'bob', null, -3.25, false, null)," +
        "(3, 'cyd', 41, 0.0, true, null)")
      (1 to 500).foreach(i => st.execute(s"insert into big values ($i, 'row-$i')"))
      st.execute("create user app password 'app'")
      st.execute("grant select, insert, update, delete on customer to app")
      st.execute("grant select on big to app")
      st.close()
    finally admin.close()

  /** every test speaks through the seam, as the restricted user */
  def withDb[A](f: Sql => A): A =
    val conn = DriverManager.getConnection(url, "app", "app")
    try f(JdbcSql(conn))
    finally conn.close()

  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  /** drain the effectful chunked stream into a list of chunks */
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

  def allRows[A: Schema](db: Sql, sql: String, params: Vector[SqlValue] = Vector.empty)
  : Vector[Either[Bad, A]] =
    collectChunks(Typed.rows[A](db, sql, params)).flatten.toVector

  // ── rows ─────────────────────────────────────────────────────────

  test("a wrapped column is its underlying kind, both directions (codec-iso)") {
    // the wrapper exists only in the Scala type; the row sees text
    final case class Name(s: String)
    given Schema[Name] = Schema.wrap(Name(_), _.s)
    final case class Named(id: Long, userName: Name)
    given Schema[Named] = Schema.derived
    withDb { db =>
      val rs = allRows[Named](db, "select id, user_name from customer order by id")
      assertEquals(rs(0).toOption.get.userName, Name("ann"))
      // and the encode direction: a wrapped param binds as its Text
      val hit = allRows[Named](db,
        "select id, user_name from customer where user_name = ?",
        okay.sql.Params.bind(Tuple1(Name("bob")))(using Schema.derived))
      assertEquals(hit.map(_.toOption.get.userName), Vector(Name("bob")))
      // a refining wrapper's refusal is a Bad naming the rule
      final case class Short5(s: String)
      given Schema[Short5] = Schema.refine(
        (x: String) => if x.length <= 2 then Right(Short5(x)) else Left(s"'$x' is longer than 2"),
        _.s)
      final case class R(id: Long, userName: Short5)
      given Schema[R] = Schema.derived
      val bad = allRows[R](db, "select id, user_name from customer order by id")
      assert(bad.forall(_.isLeft), bad.toString)
    }
  }

  test("a row decodes by column label: camel-snake, Option for NULL, bytes for binary") {
    withDb { db =>
      val rs = allRows[Customer](db, "select * from customer order by id")
      assertEquals(rs.length, 3)
      val ann = rs(0).toOption.get
      assertEquals(ann.userName, "ann")
      assertEquals(ann.age, Some(25))
      assertEquals(ann.avatar.map(_.toList), Some(List[Byte](1, 2)))
      val bob = rs(1).toOption.get
      assertEquals(bob.age, None)
      assertEquals(bob.avatar, None)
      assertEquals(bob.balance, -3.25)
    }
  }

  test("NULL in a non-Option field is an error value naming the column and row, not a throw") {
    withDb { db =>
      val rs = allRows[Strict](db, "select id, age from customer order by id")
      assertEquals(rs(0), Right(Strict(1, 25)))
      rs(1) match
        case Left(Bad(col, err, row)) =>
          assertEquals(col.toLowerCase, "age")
          assert(err.contains("NULL"), err)
          assertEquals(row, 1L)
        case other => fail(s"expected Bad naming age at row 1, got $other")
      assertEquals(rs(2), Right(Strict(3, 41)))
    }
  }

  test("reordered columns decode identically: label, not position") {
    withDb { db =>
      val natural = allRows[Customer](db,
        "select id, user_name, age, balance, active, avatar from customer order by id")
      val shuffled = allRows[Customer](db,
        "select avatar, active, balance, age, user_name, id from customer order by id")
      // Array[Byte] carries reference equality; compare a value view
      def view(r: Either[Bad, Customer]) = r.map(c => c.copy(avatar = None) -> c.avatar.map(_.toList))
      assertEquals(shuffled.map(view), natural.map(view))
    }
  }

  // ── verify ───────────────────────────────────────────────────────

  test("verify: dropped, renamed, retyped and nullability drifts, each naming the column") {
    withDb { db =>
      def drifts(sql: String) = run(Typed.verify[Customer](db, sql))

      assertEquals(drifts("select * from customer"), Vector.empty)

      val dropped = drifts("select id, age, balance, active, avatar from customer")
      assertEquals(dropped.map(_.column), Vector("user_name"))
      assertEquals(dropped.head.found, "absent")

      val renamed = drifts(
        "select id, user_name as login, age, balance, active, avatar from customer")
      assertEquals(renamed.map(_.column), Vector("user_name"))

      // a cast drifts twice, honestly: the type changed AND the
      // NOT NULL was lost on the way through the expression
      val retyped = drifts(
        "select cast(id as varchar) id, user_name, age, balance, active, avatar from customer")
      assertEquals(retyped.map(_.column.toLowerCase).distinct, Vector("id"))
      val t = retyped.find(_.expected.contains("I64"))
        .getOrElse(fail(s"no type drift in $retyped"))
      assert(t.found.contains("Text"), t.found)

      val nullab = run(Typed.verify[Strict](db, "select id, age from customer"))
      assertEquals(nullab.map(_.column.toLowerCase), Vector("age"))
      assert(nullab.head.found.contains("nullable"), nullab.head.found)

      // a passing verify then decodes every row
      val rs = allRows[Customer](db, "select * from customer")
      assert(rs.forall(_.isRight))
    }
  }

  // ── params ───────────────────────────────────────────────────────

  test("params bind positionally from a product; the prepared path is the only path") {
    withDb { db =>
      final case class Filter(minBalance: Double, active: Boolean)
      given Schema[Filter] = Schema.derived
      val rs = collectChunks(
        Typed.rowsOf[Customer, Filter](db,
          "select * from customer where balance >= ? and active = ? order by id")(
          Filter(0.0, true))).flatten
      assertEquals(rs.map(_.toOption.get.userName), List("ann", "cyd"))

      final case class NewRow(id: Long, userName: String, balance: Double, active: Boolean)
      given Schema[NewRow] = Schema.derived
      val n = run(Typed.update(db,
        "insert into customer(id, user_name, balance, active) values (?, ?, ?, ?)")(
        NewRow(10, "dee", 5.0, true)))
      assertEquals(n, 1L)
      assertEquals(run(db.update("delete from customer where id = 10")), 1L)
    }
  }

  // ── the transaction region ───────────────────────────────────────

  def countBy(db: Sql, where: String): Long =
    final case class C(n: Long)
    given Schema[C] = Schema.derived
    allRows[C](db, s"select count(*) n from customer where $where").head.toOption.get.n

  test("transact commits on success; autocommit restored") {
    val conn = DriverManager.getConnection(url, "app", "app")
    try
      val db = JdbcSql(conn)
      val prog = Typed.transact[Long, Async](db, Isolation.ReadCommitted) { g =>
        !.widen[Long, Async, Resource](
          db.update("insert into customer(id, user_name, balance, active) values (20, 'tx', 1.0, true)"))
      }
      val n = !.run(Async.run[Long, Nothing](Resource.run[Long, Async](prog)))
      assertEquals(n, 1L)
      assert(conn.getAutoCommit, "autocommit not restored after commit")
      assertEquals(countBy(db, "id = 20"), 1L)
      assertEquals(run(db.update("delete from customer where id = 20")), 1L)
    finally conn.close()
  }

  test("transact rolls back on an exception; autocommit restored") {
    val conn = DriverManager.getConnection(url, "app", "app")
    try
      val db = JdbcSql(conn)
      val prog = Typed.transact[Long, Async](db, Isolation.ReadCommitted) { _ =>
        !.widen[Long, Async, Resource](
          db.update("insert into customer(id, user_name, balance, active) values (21, 'boom', 1.0, true)"))
          .map(_ => throw RuntimeException("boom"))
      }
      intercept[RuntimeException](
        !.run(Async.run[Long, Nothing](Resource.run[Long, Async](prog))))
      assert(conn.getAutoCommit, "autocommit not restored after rollback")
      assertEquals(countBy(db, "id = 21"), 0L, "the insert survived the exception")
    finally conn.close()
  }

  test("transact rolls back on a handled abort crossing the scope") {
    val conn = DriverManager.getConnection(url, "app", "app")
    try
      val db = JdbcSql(conn)
      type G = Throws % String
      val prog = Typed.transact[Long, G](db, Isolation.ReadCommitted) { _ =>
        !.widen[Long, Async, Resource + G](
          db.update("insert into customer(id, user_name, balance, active) values (22, 'abort', 1.0, true)"))
          .flatMap(_ => effect[Resource + Async + G, Long](Throws("changed my mind")))
      }
      val out = !.run(Async.run[Either[String, Long], Nothing](
        Resource.run[Either[String, Long], Async](
          okay.runEither[Long, Resource + Async, String](prog))))
      assertEquals(out, Left("changed my mind"))
      assert(conn.getAutoCommit, "autocommit not restored after the abort")
      assertEquals(countBy(db, "id = 22"), 0L, "the insert survived the abort")
    finally conn.close()
  }

  test("nested transact on one connection refuses loudly") {
    withDb { db =>
      val prog = Typed.transact[Granted, Async](db) { _ =>
        Typed.transact[Granted, Async](db)(g2 => okay.pure(g2))
      }
      val e = intercept[IllegalStateException](
        !.run(Async.run[Granted, Nothing](Resource.run[Granted, Async](prog))))
      assert(e.getMessage.contains("nested"), e.getMessage)
    }
  }

  test("requested isolation is passed through; the granted level is exposed") {
    withDb { db =>
      val prog = Typed.transact[Granted, Async](db, Isolation.Serializable)(g => okay.pure(g))
      val g = !.run(Async.run[Granted, Nothing](Resource.run[Granted, Async](prog)))
      assertEquals(g.requested, Isolation.Serializable)
      assertEquals(g.granted, Isolation.Serializable)
      assert(!g.downgraded)
    }
  }

  test("a streaming read inside a transaction stays chunked at fetch-size") {
    withDb { db =>
      val g = run(db.begin(Isolation.ReadCommitted))
      assert(!g.downgraded)
      try
        final case class Row(n: Int, label: String)
        given Schema[Row] = Schema.derived
        val chunks = collectChunks(Typed.rows[Row](db, "select * from big order by n"))
        assertEquals(chunks.map(_.length), List(64, 64, 64, 64, 64, 64, 64, 52))
        assertEquals(chunks.flatten.collect { case Right(r) => r.n }.take(3), List(1, 2, 3))
      finally run(db.commit())
    }
  }

  // ── the posture ──────────────────────────────────────────────────

  test("the restricted user has no DDL: their schema, our types, full function") {
    val conn = DriverManager.getConnection(url, "app", "app")
    try
      intercept[java.sql.SQLException] {
        val st = conn.createStatement()
        try st.execute("create table mine(x int)") finally st.close()
      }
    finally conn.close()
  }
}
