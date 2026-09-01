package okay.pg

import okay.{!, +, Async, Chunk, Handler, Produce}
import okay.given
import okay.crypto.given
import okay.sql.SqlValue
import okay.sql.SqlValue.*

/**
 * The pg driver decodes COMPOSITE / ROW() and ARRAY types into
 * structure (pg-composite-decode) instead of handing back the raw
 * pg text. Live against the dockerized Postgres; skips when absent.
 */
class TestPgComposite extends munit.FunSuite:

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
      run(setup.update("drop type if exists okay_addr cascade"))
      run(setup.update("create type okay_addr as (street text, zip int, active bool)"))
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
