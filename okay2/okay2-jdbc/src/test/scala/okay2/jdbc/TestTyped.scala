package okay2.jdbc

import java.sql.DriverManager
import okay2.{!, +, Resource, Throws, pure}
import okay2.async.Async
import okay2.codec.Schema
import okay2.sql._
import okay2.sql.javatime._

/**
 * okay-jdbc's TestTyped: the typed layer written against `Sql`, over the
 * JDBC driver on H2, the whole suite AS A USER WITH NO DDL RIGHTS
 * against tables "someone else" created.
 */
class TestTyped extends munit.FunSuite {

  val adminUrl = "jdbc:h2:mem:typed;DB_CLOSE_DELAY=-1"
  val url = "jdbc:h2:mem:typed"

  override def beforeAll(): Unit = {
    val admin = DriverManager.getConnection(adminUrl, "sa", "")
    try {
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
      st.execute("create table tagged(id int not null, tags varchar(16) array not null, nums int array)")
      st.execute("insert into tagged values (1, array['a', 'b'], array[1, null, 3]), (2, array[], null)")
      st.execute("grant select, insert on tagged to app")
      st.execute("create table ledger(id int not null, amount decimal(30, 9) not null, at timestamp with time zone not null)")
      st.execute("insert into ledger values (1, 12345678901234567890.123456789, '2026-09-02 06:00:00+00')")
      st.execute("grant select, insert on ledger to app")
      st.execute("create table stamps(id int not null, at timestamp with time zone not null, plain timestamp not null, " +
        "d date not null, t time(6) not null, ref uuid not null, note varchar(64))")
      st.execute("insert into stamps values (1, '2026-09-02 08:00:00+02', '2026-09-02 06:00:00', '2026-09-02', " +
        "'06:00:00.5', '6ba7b810-9dad-11d1-80b4-00c04fd430c8', null)")
      st.execute("grant select, insert on stamps to app")
      st.close()
    } finally admin.close()
  }

  def withDb[A](f: Sql => A): A = {
    val conn = DriverManager.getConnection(url, "app", "app")
    try f(new JdbcSql(conn))
    finally conn.close()
  }

  def region[A](prog: A ! (Resource + Async)): A = Run(Resource.run[A, Async](prog))

  test("a wrapped column is its underlying kind, both directions") {
    implicit val name: Schema[Name] = Schema.wrap(Name(_), _.s)
    withDb { db =>
      val rs = Run.rows[Named](db, "select id, user_name from customer order by id")
      assertEquals(rs(0).toOption.get.userName, Name("ann"))
      val hit = Run.rows[Named](db, "select id, user_name from customer where user_name = ?", Params.bind(Tuple1(Name("bob"))))
      assertEquals(hit.map(_.toOption.get.userName), Vector(Name("bob")))
      implicit val short: Schema[Short5] = Schema.refine(
        (x: String) => if (x.length <= 2) Right(Short5(x)) else Left(s"'$x' is longer than 2"), (s: Short5) => s.s)
      val bad = Run.rows[R](db, "select id, user_name from customer order by id")
      assert(bad.forall(_.isLeft), bad.toString)
    }
  }

  test("a row decodes by column label: camel-snake, Option for NULL, bytes for binary") {
    withDb { db =>
      val rs = Run.rows[Customer](db, "select * from customer order by id")
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
      val rs = Run.rows[Strict](db, "select id, age from customer order by id")
      assertEquals(rs(0), Right(Strict(1, 25)))
      rs(1) match {
        case Left(Bad(col, err, row)) =>
          assertEquals(col.toLowerCase, "age")
          assert(err.contains("NULL"), err)
          assertEquals(row, 1L)
        case other => fail(s"expected Bad naming age at row 1, got $other")
      }
      assertEquals(rs(2), Right(Strict(3, 41)))
    }
  }

  test("reordered columns decode identically: label, not position") {
    withDb { db =>
      val natural = Run.rows[Customer](db, "select id, user_name, age, balance, active, avatar from customer order by id")
      val shuffled = Run.rows[Customer](db, "select avatar, active, balance, age, user_name, id from customer order by id")
      def view(r: Either[Bad, Customer]) = r.map(c => c.copy(avatar = None) -> c.avatar.map(_.toList))
      assertEquals(shuffled.map(view), natural.map(view))
    }
  }

  test("verify: dropped, renamed, retyped and nullability drifts, each naming the column") {
    withDb { db =>
      def drifts(sql: String) = Run(Typed.verify[Customer](db, sql))
      assertEquals(drifts("select * from customer"), Vector.empty)
      val dropped = drifts("select id, age, balance, active, avatar from customer")
      assertEquals(dropped.map(_.column), Vector("user_name"))
      assertEquals(dropped.head.found, "absent")
      assertEquals(drifts("select id, user_name as login, age, balance, active, avatar from customer").map(_.column), Vector("user_name"))
      val retyped = drifts("select cast(id as varchar) id, user_name, age, balance, active, avatar from customer")
      assertEquals(retyped.map(_.column.toLowerCase).distinct, Vector("id"))
      val t = retyped.find(_.expected.contains("I64")).getOrElse(fail(s"no type drift in $retyped"))
      assert(t.found.contains("Text"), t.found)
      val nullab = Run(Typed.verify[Strict](db, "select id, age from customer"))
      assertEquals(nullab.map(_.column.toLowerCase), Vector("age"))
      assert(nullab.head.found.contains("nullable"), nullab.head.found)
      assert(Run.rows[Customer](db, "select * from customer").forall(_.isRight))
    }
  }

  test("params bind positionally from a product; the prepared path is the only path") {
    withDb { db =>
      val rs = Run.chunks(Typed.rowsOf[Customer, Filter](db,
        "select * from customer where balance >= ? and active = ? order by id")(Filter(0.0, true))).flatten
      assertEquals(rs.map(_.toOption.get.userName), List("ann", "cyd"))
      val n = Run(Typed.update(db, "insert into customer(id, user_name, balance, active) values (?, ?, ?, ?)")(
        NewRow(10, "dee", 5.0, true)))
      assertEquals(n, 1L)
      assertEquals(Run(db.update("delete from customer where id = 10")), 1L)
    }
  }

  def countBy(db: Sql, where: String): Long =
    Run.rows[Count](db, s"select count(*) n from customer where $where").head.toOption.get.n

  test("transact commits on success; autocommit restored") {
    val conn = DriverManager.getConnection(url, "app", "app")
    try {
      val db = new JdbcSql(conn)
      val n = region(Typed.transact[Long, Async](db, Isolation.ReadCommitted) { _ =>
        db.update("insert into customer(id, user_name, balance, active) values (20, 'tx', 1.0, true)")
      })
      assertEquals(n, 1L)
      assert(conn.getAutoCommit, "autocommit not restored after commit")
      assertEquals(countBy(db, "id = 20"), 1L)
      assertEquals(Run(db.update("delete from customer where id = 20")), 1L)
    } finally conn.close()
  }

  test("the typed region: same runtime as transact, and the nested begin cannot COMPILE") {
    val conn = DriverManager.getConnection(url, "app", "app")
    try {
      val db = new JdbcSql(conn)
      val n = region(Typed.region[Long, Async](Typed.Db(db)) { tx =>
        tx.update("insert into customer(id, user_name, balance, active) values (22, 'typed', 1.0, true)")
      })
      assertEquals(n, 1L)
      assertEquals(countBy(db, "id = 22"), 1L)
      assertEquals(Run(db.update("delete from customer where id = 22")), 1L)
      val errors = compileErrors(
        "val h2: okay2.sql.Typed.Db[okay2.sql.Typed.Tx.No] = null\n" +
        "okay2.sql.Typed.region[Long, okay2.async.Async](h2) { tx =>\n" +
        "  okay2.sql.Typed.region[Long, okay2.async.Async](tx)(inner => inner.update(\"select 1\"))\n" +
        "}")
      assert(errors.contains("Tx.Yes"), errors)
      // paired: the same shape with the outer handle compiles
      assertEquals(compileErrors(
        "val h2: okay2.sql.Typed.Db[okay2.sql.Typed.Tx.No] = null\n" +
        "okay2.sql.Typed.region[Long, okay2.async.Async](h2)(tx => tx.update(\"select 1\"))"), "")
    } finally conn.close()
  }

  test("transact rolls back on an exception; autocommit restored") {
    val conn = DriverManager.getConnection(url, "app", "app")
    try {
      val db = new JdbcSql(conn)
      val prog = Typed.transact[Long, Async](db, Isolation.ReadCommitted) { _ =>
        db.update("insert into customer(id, user_name, balance, active) values (21, 'boom', 1.0, true)")
          .map(_ => throw new RuntimeException("boom"))
      }
      intercept[RuntimeException](region(prog)): Unit
      assert(conn.getAutoCommit, "autocommit not restored after rollback")
      assertEquals(countBy(db, "id = 21"), 0L, "the insert survived the exception")
    } finally conn.close()
  }

  test("a FAILING STATEMENT inside a region: the brake runs, autocommit is restored, the next region begins") {
    val conn = DriverManager.getConnection(url, "app", "app")
    try {
      val db = new JdbcSql(conn)
      val prog = Typed.transact[Long, Async](db, Isolation.ReadCommitted) { _ =>
        db.update("insert into customer(id, user_name, balance, active) values (22, 'boom', 1.0, true)")
          .flatMap(_ => db.update("select syntax error from"))
      }
      intercept[java.sql.SQLException](region(prog)): Unit
      assert(conn.getAutoCommit, "autocommit not restored: the brake never ran")
      assertEquals(countBy(db, "id = 22"), 0L, "the insert survived the failed statement")
      val g = region(Typed.transact[Granted, Async](db)(g => pure[Async, Granted](g)))
      assertEquals(g.granted, Isolation.ReadCommitted)
    } finally conn.close()
  }

  test("transact rolls back on a handled abort crossing the scope") {
    val conn = DriverManager.getConnection(url, "app", "app")
    try {
      val db = new JdbcSql(conn)
      val prog = Typed.transact[Long, Throws[String]](db, Isolation.ReadCommitted) { _ =>
        db.update("insert into customer(id, user_name, balance, active) values (22, 'abort', 1.0, true)")
          .flatMap(_ => Throws.raise[String, Long]("changed my mind"))
      }
      val out = region(Throws.runEither[Long, String, Resource with Async](prog))
      assertEquals(out, Left("changed my mind"))
      assert(conn.getAutoCommit, "autocommit not restored after the abort")
      assertEquals(countBy(db, "id = 22"), 0L, "the insert survived the abort")
    } finally conn.close()
  }

  test("nested transact on one connection refuses loudly") {
    withDb { db =>
      val prog = Typed.transact[Granted, Async](db)(_ => Typed.transact[Granted, Async](db)(g2 => pure[Async, Granted](g2)))
      val e = intercept[IllegalStateException](region(prog))
      assert(e.getMessage.contains("nested"), e.getMessage)
    }
  }

  test("requested isolation is passed through; the granted level is exposed") {
    withDb { db =>
      val g = region(Typed.transact[Granted, Async](db, Isolation.Serializable)(g => pure[Async, Granted](g)))
      assertEquals(g.requested, Isolation.Serializable)
      assertEquals(g.granted, Isolation.Serializable)
      assert(!g.downgraded)
    }
  }

  test("a streaming read inside a transaction stays chunked at fetch-size") {
    withDb { db =>
      val g = Run(db.begin(Isolation.ReadCommitted))
      assert(!g.downgraded)
      try {
        val chunks = Run.chunks(Typed.rows[Big](db, "select * from big order by n"))
        assertEquals(chunks.map(_.length), List(64, 64, 64, 64, 64, 64, 64, 52))
        assertEquals(chunks.flatten.collect { case Right(r) => r.n }.take(3), List(1, 2, 3))
      } finally Run(db.commit())
    }
  }

  test("a Vector/List field reads an ARRAY column and binds one; verify accepts the unnamed element type") {
    withDb { db =>
      assertEquals(Run.rows[Tagged](db, "select id, tags, nums from tagged order by id"), Vector(
        Right(Tagged(1, Vector("a", "b"), Some(List(Some(1), None, Some(3))))),
        Right(Tagged(2, Vector.empty, None))))
      assertEquals(Run(Typed.verify[Tagged](db, "select id, tags, nums from tagged")), Vector.empty)
      assertEquals(Run(Typed.update(db, "insert into tagged values (?, ?, ?)")(Tagged(3, Vector("x"), Some(List(None, Some(9)))))), 1L)
      assertEquals(Run.rows[Tagged](db, "select id, tags, nums from tagged where id = 3"),
        Vector(Right(Tagged(3, Vector("x"), Some(List(None, Some(9)))))))
    }
  }

  test("a DECIMAL column is exact in a BigDecimal field and lossy in a Double one; a timestamp reads into an Instant, or as ISO text") {
    val money = BigDecimal("12345678901234567890.123456789")
    val six = java.time.Instant.parse("2026-09-02T06:00:00Z")
    withDb { db =>
      assertEquals(Run.rows[Ledger](db, "select id, amount, at from ledger"), Vector(Right(Ledger(1, money, six))))
      assertEquals(Run(Typed.verify[Ledger](db, "select id, amount, at from ledger")), Vector.empty)
      assertEquals(Run.rows[LedgerText](db, "select id, at from ledger"), Vector(Right(LedgerText(1, "2026-09-02T06:00:00Z"))))
      assertEquals(Run(Typed.verify[LedgerText](db, "select id, at from ledger")), Vector.empty)
      val r = Run.rows[Rounded](db, "select id, amount from ledger").head.toOption.get
      assert(BigDecimal(r.amount) != money, "the Double field rounds — by its choice")
      assertEquals(Run(Typed.update(db, "insert into ledger values (?, ?, ?)")(Ledger(2, money + 1, six.plusSeconds(3600)))), 1L)
      assertEquals(Run.rows[Ledger](db, "select id, amount, at from ledger where id = 2").map(_.map(_.amount)), Vector(Right(money + 1)))
    }
  }

  test("timestamptz/timestamp/date/time/uuid read into java.time and UUID fields, verify clean, and bind back exact") {
    val six = java.time.Instant.parse("2026-09-02T06:00:00Z")
    val one = Stamp(1, six, six, java.time.LocalDate.of(2026, 9, 2), java.time.LocalTime.of(6, 0, 0, 500000000),
      java.util.UUID.fromString("6ba7b810-9dad-11d1-80b4-00c04fd430c8"), None)
    withDb { db =>
      val sql = "select id, at, plain, d, t, ref, note from stamps order by id"
      assertEquals(Run(db.describe(sql)).map(_.tpe), Vector[SqlType](SqlType.I32, SqlType.Timestamp, SqlType.Timestamp,
        SqlType.Date, SqlType.Time, SqlType.Uuid, SqlType.Text))
      assertEquals(Run(Typed.verify[Stamp](db, sql)), Vector.empty)
      assertEquals(Run.rows[Stamp](db, sql), Vector(Right(one)))
      val two = Stamp(2, six.plusNanos(1000), java.time.Instant.parse("1969-12-31T23:59:59.999999Z"),
        java.time.LocalDate.of(1899, 12, 31), java.time.LocalTime.of(23, 59, 59, 999999000),
        java.util.UUID.randomUUID(), Some("two"))
      assertEquals(Run(Typed.update(db, "insert into stamps values (?, ?, ?, ?, ?, ?, ?)")(two)), 1L)
      assertEquals(Run.rows[Stamp](db, sql), Vector(Right(one), Right(two)))
    }
  }

  test("the isolation level and the read-only hint are restored after a region; H2 grants no read-only and says so") {
    val conn = DriverManager.getConnection(url, "app", "app")
    try {
      val db = new JdbcSql(conn)
      val before = conn.getTransactionIsolation
      assertNotEquals(before, java.sql.Connection.TRANSACTION_SERIALIZABLE)
      val g = region(Typed.transact[Granted, Async](db, Isolation.Serializable, readOnly = true)(g => pure[Async, Granted](g)))
      assertEquals(g.granted, Isolation.Serializable)
      assertEquals(g.readOnly, conn.isReadOnly)
      assertEquals(conn.getTransactionIsolation, before, "the level before the region is the level after it")
      assert(conn.getAutoCommit)
      assert(!conn.isReadOnly)
    } finally conn.close()
  }

  test("the restricted user has no DDL: their schema, our types, full function") {
    val conn = DriverManager.getConnection(url, "app", "app")
    try intercept[java.sql.SQLException] {
      val st = conn.createStatement()
      try st.execute("create table mine(x int)") finally st.close()
    }: Unit
    finally conn.close()
  }
}
