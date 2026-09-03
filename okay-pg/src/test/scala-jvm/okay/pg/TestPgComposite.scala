package okay.pg

import okay.{!, +, Async, Chunk, Handler, Produce}
import okay.given
import okay.crypto.given
import okay.sql.SqlValue
import okay.sql.SqlValue.*
import okay.sql.given

/**
 * The pg driver decodes COMPOSITE / ROW() and ARRAY types into
 * structure (pg-composite-decode) instead of handing back the raw
 * pg text. Live against the dockerized Postgres; skips when absent.
 */
class TestPgComposite extends munit.FunSuite:

  // integration-test-gate: out of the default gate, into `sbt integrationTest`
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  val host = sys.env.getOrElse("OKAY_PG_HOST", "127.0.0.1")
  val port = sys.env.get("OKAY_PG_PORT").flatMap(_.toIntOption).getOrElse(5432)

  def connect(): PgSql = okay.!.run(okay.Async.run[PgSql, Nothing](
    PgSql.connect(host, port, "okay", "okay", "okay")))
  lazy val available: Boolean =
    try { connect().close(); true } catch { case _: Throwable => false }

  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  private def collectChunks[A](s: Chunk[A] ! (Produce + Async)): List[Chunk[A]] =
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

  /** the single cell of a single-row, single-column query */
  private def cell(sql: String): SqlValue =
    val db = connect()
    try collectChunks(db.query(sql)).flatten.head.head
    finally db.close()

  test("an int array decodes to a typed Arr, in order") {
    assume(available, s"no Postgres at $host:$port — skips")
    assertEquals(cell("select array[1,2,3]::int[]"),
      Arr(Vector(I32(1), I32(2), I32(3))))
  }

  test("a text array: quoting and embedded commas survive; NULL is Null") {
    assume(available, s"no Postgres at $host:$port — skips")
    assertEquals(cell("select array['a','b,c',null,'has \"q\"']::text[]"),
      Arr(Vector(Text("a"), Text("b,c"), Null, Text("has \"q\""))))
  }

  test("a bool and a float8 array type their elements") {
    assume(available, s"no Postgres at $host:$port — skips")
    assertEquals(cell("select array[true,false]::bool[]"), Arr(Vector(Bool(true), Bool(false))))
    assertEquals(cell("select array[1.5,2.25]::float8[]"), Arr(Vector(F64(1.5), F64(2.25))))
  }

  test("an empty array is the empty Arr") {
    assume(available, s"no Postgres at $host:$port — skips")
    assertEquals(cell("select array[]::int[]"), Arr(Vector.empty))
  }

  test("a nested int[][] decodes to Arr of Arr") {
    assume(available, s"no Postgres at $host:$port — skips")
    assertEquals(cell("select array[array[1,2],array[3,4]]::int[][]"),
      Arr(Vector(Arr(Vector(I32(1), I32(2))), Arr(Vector(I32(3), I32(4))))))
  }

  test("ROW()/record decodes to a Row; fields arrive as Text, a NULL field is Null") {
    assume(available, s"no Postgres at $host:$port — skips")
    assertEquals(cell("select row(1, 'ann', 25)"),
      Row(Vector(Text("1"), Text("ann"), Text("25"))))
    assertEquals(cell("select row(1, null, 'x')"),
      Row(Vector(Text("1"), Null, Text("x"))))
  }

  test("a composite field with commas and quotes is unescaped, not split") {
    assume(available, s"no Postgres at $host:$port — skips")
    assertEquals(cell("""select row('a,b', 'has "quote"')"""),
      Row(Vector(Text("a,b"), Text("has \"quote\""))))
  }

  test("a decoded Arr round-trips: re-encoded to the pg literal and read back equal") {
    assume(available, s"no Postgres at $host:$port — skips")
    val lit = PgSql.textOf(Arr(Vector(I32(1), I32(2), Null))).get
    val db = connect()
    try
      val back = collectChunks(db.query(s"select '$lit'::int[]")).flatten.head.head
      assertEquals(back, Arr(Vector(I32(1), I32(2), Null)))
    finally db.close()
  }

  /** define a named composite type on its own connection, so a FRESH
   * connection preloads it (pg-composite-fields-typed's contract) */
  private def defineType(): Unit =
    val setup = connect()
    try
      run(setup.update("drop type if exists okay_addr cascade")): Unit
      run(setup.update("create type okay_addr as (street text, zip int, active bool)")): Unit
    finally setup.close()

  test("a named composite type's fields are TYPED, not handed back as text") {
    assume(available, s"no Postgres at $host:$port — skips")
    defineType()
    // the fresh connection preloaded okay_addr's field OIDs at connect
    assertEquals(cell("select row('main st', 90210, true)::okay_addr"),
      Row(Vector(Text("main st"), I32(90210), Bool(true))))
  }

  test("a named composite with NULL fields types the present ones and nulls the rest") {
    assume(available, s"no Postgres at $host:$port — skips")
    defineType()
    assertEquals(cell("select row('x', null, null)::okay_addr"),
      Row(Vector(Text("x"), Null, Null)))
  }

  test("an anonymous record stays fields-as-text: no field OIDs on the wire to type by") {
    assume(available, s"no Postgres at $host:$port — skips")
    // record (oid 2249), not a named type — unchanged by this feature
    assertEquals(cell("select row(1, 2, 3)"),
      Row(Vector(Text("1"), Text("2"), Text("3"))))
  }

  test("an ARRAY of a named composite decodes to Arr of typed Row (pg-composite-array)") {
    assume(available, s"no Postgres at $host:$port — skips")
    defineType()
    val v = cell(
      "select array[row('main st', 90210, true)::okay_addr, " +
      "row('elm', null, false)::okay_addr]")
    assertEquals(v, Arr(Vector(
      Row(Vector(Text("main st"), I32(90210), Bool(true))),
      Row(Vector(Text("elm"), Null, Bool(false))))))
  }

  // ── sql-schema-composite: the Schema layer over the pg driver ────

  final case class Addr(street: String, zip: Option[Int], active: Boolean)
  given okay.codec.Schema[Addr] = okay.codec.Schema.derived
  final case class Person(id: Int, nums: Vector[Int], home: Addr, moves: Vector[Addr], prev: Option[Addr])
  given okay.codec.Schema[Person] = okay.codec.Schema.derived

  test("a Vector field and a nested case class decode from int[] and a named composite through Typed.rows; verify is clean") {
    assume(available, "no Postgres at the configured endpoint")
    defineType()
    val db = connect()
    try
      // a table, so describe can answer nullability from the catalog
      run(db.update("drop table if exists okay_people")): Unit
      run(db.update("create table okay_people(id int not null, nums int[] not null, " +
        "home okay_addr not null, moves okay_addr[] not null, prev okay_addr)")): Unit
      run(db.update("insert into okay_people values (1, array[1, 2, 3], " +
        "row('main st', 90210, true), array[row('elm', null, false)::okay_addr], null)")): Unit
      val sql = "select id, nums, home, moves, prev from okay_people"
      val rows = collectChunks(okay.sql.Typed.rows[Person](db, sql)).flatten
      assertEquals(rows, List(Right(Person(1, Vector(1, 2, 3), Addr("main st", Some(90210), true),
        Vector(Addr("elm", None, false)), None))))
      // describe types the array and the composite from the connect preload
      assertEquals(run(okay.sql.Typed.verify[Person](db, sql)), Vector.empty)
      // and a row-shape mismatch is a Drift naming the column
      final case class Wrong(id: Int, home: Vector[Int])
      given okay.codec.Schema[Wrong] = okay.codec.Schema.derived
      assertEquals(run(okay.sql.Typed.verify[Wrong](db, sql)).map(_.column), Vector("home"))
    finally db.close()
  }

  // a whole-row column has no table column behind it, so pg cannot
  // promise it not null (an outer join nulls the whole row): Option
  final case class Wrap(p: Option[Person])
  given okay.codec.Schema[Wrap] = okay.codec.Schema.derived
  final case class WrapStrict(p: Person)
  given okay.codec.Schema[WrapStrict] = okay.codec.Schema.derived

  test("a TABLE's row type selected whole is a typed Row; describe names it; Typed.rows reads it nested (pg-composite-rowtype)") {
    assume(available, "no Postgres at the configured endpoint")
    defineType()
    val setup = connect()
    try
      run(setup.update("drop table if exists okay_people")): Unit
      run(setup.update("create table okay_people(id int not null, nums int[] not null, " +
        "home okay_addr not null, moves okay_addr[] not null, prev okay_addr)")): Unit
      run(setup.update("insert into okay_people values (1, array[1, 2, 3], " +
        "row('main st', 90210, true), array[row('elm', null, false)::okay_addr], null)"))
    finally setup.close()
    // a FRESH connection preloads the table's row type beside the composites
    val t0 = System.nanoTime()
    val db = connect()
    val connectMs = (System.nanoTime() - t0) / 1e6
    try
      val sql = "select p from okay_people p"
      assertEquals(collectChunks(db.query(sql)).flatten, List(Vector(SqlValue.Row(Vector(
        SqlValue.I32(1), SqlValue.Arr(Vector(SqlValue.I32(1), SqlValue.I32(2), SqlValue.I32(3))),
        SqlValue.Row(Vector(SqlValue.Text("main st"), SqlValue.I32(90210), SqlValue.Bool(true))),
        SqlValue.Arr(Vector(SqlValue.Row(Vector(SqlValue.Text("elm"), SqlValue.Null, SqlValue.Bool(false))))),
        SqlValue.Null)))))
      // describe names the row type's fields, nested
      assertEquals(run(db.describe(sql)).map(_.tpe), Vector(okay.sql.SqlType.Row(Vector(
        okay.sql.SqlType.I32, okay.sql.SqlType.Arr(okay.sql.SqlType.I32),
        okay.sql.SqlType.Row(Vector(okay.sql.SqlType.Text, okay.sql.SqlType.I32, okay.sql.SqlType.Bool)),
        okay.sql.SqlType.Arr(okay.sql.SqlType.Row(Vector(okay.sql.SqlType.Text, okay.sql.SqlType.I32, okay.sql.SqlType.Bool))),
        okay.sql.SqlType.Row(Vector(okay.sql.SqlType.Text, okay.sql.SqlType.I32, okay.sql.SqlType.Bool))))))
      // the Schema layer reads the whole row as a nested case class
      assertEquals(collectChunks(okay.sql.Typed.rows[Wrap](db, sql)).flatten,
        List(Right(Wrap(Some(Person(1, Vector(1, 2, 3), Addr("main st", Some(90210), true),
          Vector(Addr("elm", None, false)), None))))))
      assertEquals(run(okay.sql.Typed.verify[Wrap](db, sql)), Vector.empty)
      // and the strict shape is told WHY: the whole-row column is nullable
      assertEquals(run(okay.sql.Typed.verify[WrapStrict](db, sql)).map(d => (d.column, d.found)),
        Vector(("p", "nullable")))
      // an ARRAY of the row type
      assertEquals(collectChunks(db.query("select array(select p from okay_people p)")).flatten.head.head match
        case SqlValue.Arr(Vector(SqlValue.Row(fs))) => fs.length
        case other => fail(s"not an Arr(Row): $other"), 5)
      // a table created AFTER connect is unknown to THIS connection (stated):
      // its row type has an OID the preload never saw, so the cell is the raw text
      run(db.update("drop table if exists okay_later")): Unit
      run(db.update("create table okay_later(a int, b text)")): Unit
      run(db.update("insert into okay_later values (7, 'x')")): Unit
      assertEquals(collectChunks(db.query("select l from okay_later l")).flatten.head.head,
        SqlValue.Text("(7,x)"))
      // and a reconnect knows it
      val db2 = connect()
      try assertEquals(collectChunks(db2.query("select l from okay_later l")).flatten.head.head,
        SqlValue.Row(Vector(SqlValue.I32(7), SqlValue.Text("x"))))
      finally db2.close()
      println(f"pg-composite-rowtype: connect with the row-type preload took $connectMs%.1f ms")
    finally db.close()
  }

  test("a Vector param and a nested case class param bind as Arr/Row and are read back typed") {
    assume(available, "no Postgres at the configured endpoint")
    defineType()
    val db = connect()
    try
      final case class In(nums: Vector[Int], home: Addr)
      given okay.codec.Schema[In] = okay.codec.Schema.derived
      final case class Out(nums: Vector[Int], home: Addr)
      given okay.codec.Schema[Out] = okay.codec.Schema.derived
      val rows = collectChunks(okay.sql.Typed.rowsOf[Out, In](db,
        "select $1::int[] as nums, $2::okay_addr as home")(In(Vector(4, 5), Addr("a\"b,c", None, true)))).flatten
      assertEquals(rows, List(Right(Out(Vector(4, 5), Addr("a\"b,c", None, true)))))
    finally db.close()
  }

  // ── pg-scalar-types: numeric exact, vendor scalars named and read as text ──

  final case class Ledger(id: Int, amount: BigDecimal, ref: String, doc: String, at: String)
  given okay.codec.Schema[Ledger] = okay.codec.Schema.derived

  test("numeric is exact (Num), NaN falls to F64; uuid/jsonb/timestamptz are NAMED and a String field fits them") {
    assume(available, "no Postgres at the configured endpoint")
    val money = BigDecimal("12345678901234567890.123456789")
    assertEquals(cell("select 12345678901234567890.123456789::numeric"), Num(money))
    cell("select 'NaN'::numeric") match
      case F64(x) => assert(x.isNaN)
      case other => fail(s"expected F64(NaN), got $other")
    assertEquals(cell("select array[1.5, 2.25]::numeric[]"), Arr(Vector(Num(BigDecimal("1.5")), Num(BigDecimal("2.25")))))
    val db = connect()
    try
      run(db.update("drop table if exists okay_ledger")): Unit
      run(db.update("create table okay_ledger(id int not null, amount numeric(30, 9) not null, " +
        "ref uuid not null, doc jsonb not null, at timestamptz not null)")): Unit
      run(db.update("insert into okay_ledger values (1, 12345678901234567890.123456789, " +
        "'6ba7b810-9dad-11d1-80b4-00c04fd430c8', '{\"k\": [1, 2]}', '2026-09-02 06:00:00+00')")): Unit
      val sql = "select id, amount, ref, doc, at from okay_ledger"
      assertEquals(run(db.describe(sql)).map(_.tpe), Vector(okay.sql.SqlType.I32, okay.sql.SqlType.Num,
        okay.sql.SqlType.Other("uuid"), okay.sql.SqlType.Other("jsonb"), okay.sql.SqlType.Other("timestamptz")))
      assertEquals(run(okay.sql.Typed.verify[Ledger](db, sql)), Vector.empty)
      val rows = collectChunks(okay.sql.Typed.rows[Ledger](db, sql)).flatten
      assertEquals(rows.map(_.map(r => (r.amount, r.ref, r.doc))),
        List(Right((money, "6ba7b810-9dad-11d1-80b4-00c04fd430c8", "{\"k\": [1, 2]}"))))
      // the BigDecimal param binds as its exact text; pg types $2 from the column
      assertEquals(run(db.update("insert into okay_ledger values (2, $1, $2::uuid, '{}', now())",
        Vector(Num(money + 1), Text("6ba7b810-9dad-11d1-80b4-00c04fd430c9")))), 1L)
      assertEquals(cell("select amount from okay_ledger where id = 2"), Num(money + 1))
    finally db.close()
  }
