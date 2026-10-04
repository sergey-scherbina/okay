package okay2.pg

import okay2.sql.{SqlType, SqlValue, Typed}
import okay2.sql.SqlValue._
import Models._

/**
 * The pg driver decodes COMPOSITE / ROW() and ARRAY types into structure
 * (okay-pg's TestPgComposite, pg-composite-decode) instead of handing
 * back the raw pg text. Live; skips when absent.
 */
class TestPgComposite extends PgLive {

  /** the single cell of a single-row, single-column query */
  private def cell(sql: String): SqlValue = withDb(db => chunks(db.query(sql)).flatten.head.head)

  test("an int array decodes to a typed Arr, in order") {
    assume(available, skipped)
    assertEquals(cell("select array[1,2,3]::int[]"), Arr(Vector(I32(1), I32(2), I32(3))): SqlValue)
  }

  test("a text array: quoting and embedded commas survive; NULL is Null") {
    assume(available, skipped)
    assertEquals(cell("select array['a','b,c',null,'has \"q\"']::text[]"),
      Arr(Vector(Text("a"), Text("b,c"), Null, Text("has \"q\""))): SqlValue)
  }

  test("a bool and a float8 array type their elements") {
    assume(available, skipped)
    assertEquals(cell("select array[true,false]::bool[]"), Arr(Vector(Bool(true), Bool(false))): SqlValue)
    assertEquals(cell("select array[1.5,2.25]::float8[]"), Arr(Vector(F64(1.5), F64(2.25))): SqlValue)
  }

  test("an empty array is the empty Arr") {
    assume(available, skipped)
    assertEquals(cell("select array[]::int[]"), Arr(Vector.empty): SqlValue)
  }

  test("a nested int[][] decodes to Arr of Arr") {
    assume(available, skipped)
    assertEquals(cell("select array[array[1,2],array[3,4]]::int[][]"),
      Arr(Vector(Arr(Vector(I32(1), I32(2))), Arr(Vector(I32(3), I32(4))))): SqlValue)
  }

  test("ROW()/record decodes to a Row; fields arrive as Text, a NULL field is Null") {
    assume(available, skipped)
    assertEquals(cell("select row(1, 'ann', 25)"), Row(Vector(Text("1"), Text("ann"), Text("25"))): SqlValue)
    assertEquals(cell("select row(1, null, 'x')"), Row(Vector(Text("1"), Null, Text("x"))): SqlValue)
  }

  test("a composite field with commas and quotes is unescaped, not split") {
    assume(available, skipped)
    assertEquals(cell("""select row('a,b', 'has "quote"')"""), Row(Vector(Text("a,b"), Text("has \"quote\""))): SqlValue)
  }

  test("a decoded Arr round-trips: re-encoded to the pg literal and read back equal") {
    assume(available, skipped)
    val lit = PgSql.textOf(Arr(Vector(I32(1), I32(2), Null))).get
    assertEquals(cell(s"select '$lit'::int[]"), Arr(Vector(I32(1), I32(2), Null)): SqlValue)
  }

  /** define a named composite type on its own connection, so a FRESH
   * connection preloads it */
  private def defineType(): Unit = withDb { setup =>
    run(setup.update("drop type if exists okay_addr cascade")): Unit
    run(setup.update("create type okay_addr as (street text, zip int, active bool)")): Unit
  }

  test("a named composite type's fields are TYPED, not handed back as text") {
    assume(available, skipped)
    defineType()
    assertEquals(cell("select row('main st', 90210, true)::okay_addr"),
      Row(Vector(Text("main st"), I32(90210), Bool(true))): SqlValue)
  }

  test("a named composite with NULL fields types the present ones and nulls the rest") {
    assume(available, skipped)
    defineType()
    assertEquals(cell("select row('x', null, null)::okay_addr"), Row(Vector(Text("x"), Null, Null)): SqlValue)
  }

  test("an anonymous record stays fields-as-text: no field OIDs on the wire to type by") {
    assume(available, skipped)
    assertEquals(cell("select row(1, 2, 3)"), Row(Vector(Text("1"), Text("2"), Text("3"))): SqlValue)
  }

  test("an ARRAY of a named composite decodes to Arr of typed Row (pg-composite-array)") {
    assume(available, skipped)
    defineType()
    assertEquals(cell("select array[row('main st', 90210, true)::okay_addr, row('elm', null, false)::okay_addr]"),
      Arr(Vector(Row(Vector(Text("main st"), I32(90210), Bool(true))), Row(Vector(Text("elm"), Null, Bool(false))))): SqlValue)
  }

  private def people(db: PgSql): Unit = {
    run(db.update("drop table if exists okay_people")): Unit
    run(db.update("create table okay_people(id int not null, nums int[] not null, " +
      "home okay_addr not null, moves okay_addr[] not null, prev okay_addr)")): Unit
    run(db.update("insert into okay_people values (1, array[1, 2, 3], " +
      "row('main st', 90210, true), array[row('elm', null, false)::okay_addr], null)")): Unit
  }

  test("a Vector field and a nested case class decode from int[] and a named composite through Typed.rows; verify is clean") {
    assume(available, skipped)
    defineType()
    withDb { db =>
      people(db)
      val sql = "select id, nums, home, moves, prev from okay_people"
      assertEquals(chunks(Typed.rows[Person](db, sql)).flatten, List(Right(Person(1, Vector(1, 2, 3),
        Addr("main st", Some(90210), true), Vector(Addr("elm", None, false)), None))))
      assertEquals(run(Typed.verify[Person](db, sql)), Vector.empty)
      // and a row-shape mismatch is a Drift naming the column
      assertEquals(run(Typed.verify[Wrong](db, sql)).map(_.column), Vector("home"))
    }
  }

  test("a TABLE's row type selected whole is a typed Row; describe names it; Typed.rows reads it nested (pg-composite-rowtype)") {
    assume(available, skipped)
    defineType()
    withDb(people)
    // a FRESH connection preloads the table's row type beside the composites
    withDb { db =>
      val sql = "select p from okay_people p"
      val addrRow = SqlType.Row(Vector(SqlType.Text, SqlType.I32, SqlType.Bool))
      assertEquals(chunks(db.query(sql)).flatten, List(Vector[SqlValue](Row(Vector(
        I32(1), Arr(Vector(I32(1), I32(2), I32(3))),
        Row(Vector(Text("main st"), I32(90210), Bool(true))),
        Arr(Vector(Row(Vector(Text("elm"), Null, Bool(false))))),
        Null)))))
      assertEquals(run(db.describe(sql)).map(_.tpe), Vector[SqlType](SqlType.Row(Vector(
        SqlType.I32, SqlType.Arr(SqlType.I32), addrRow, SqlType.Arr(addrRow), addrRow))))
      assertEquals(chunks(Typed.rows[Wrap](db, sql)).flatten, List(Right(Wrap(Some(Person(1, Vector(1, 2, 3),
        Addr("main st", Some(90210), true), Vector(Addr("elm", None, false)), None))))))
      assertEquals(run(Typed.verify[Wrap](db, sql)), Vector.empty)
      // the strict shape is told WHY: the whole-row column is nullable
      assertEquals(run(Typed.verify[WrapStrict](db, sql)).map(d => (d.column, d.found)), Vector(("p", "nullable")))
      chunks(db.query("select array(select p from okay_people p)")).flatten.head.head match {
        case Arr(Vector(Row(fs))) => assertEquals(fs.length, 5)
        case other => fail(s"not an Arr(Row): $other")
      }
      // a table created AFTER connect is unknown to THIS connection (stated)
      run(db.update("drop table if exists okay_later")): Unit
      run(db.update("create table okay_later(a int, b text)")): Unit
      run(db.update("insert into okay_later values (7, 'x')")): Unit
      assertEquals(chunks(db.query("select l from okay_later l")).flatten.head.head, Text("(7,x)"): SqlValue)
      // and a reconnect knows it
      withDb(db2 => assertEquals(chunks(db2.query("select l from okay_later l")).flatten.head.head,
        Row(Vector(I32(7), Text("x"))): SqlValue))
    }
  }

  test("a Vector param and a nested case class param bind as Arr/Row and are read back typed") {
    assume(available, skipped)
    defineType()
    withDb { db =>
      val rows = chunks(Typed.rowsOf[Out, In](db, "select $1::int[] as nums, $2::okay_addr as home")(
        In(Vector(4, 5), Addr("a\"b,c", None, true)))).flatten
      assertEquals(rows, List(Right(Out(Vector(4, 5), Addr("a\"b,c", None, true)))))
    }
  }

  test("numeric is exact (Num), NaN falls to F64; uuid/jsonb/timestamptz are TYPED and a String field still fits them") {
    assume(available, skipped)
    val money = BigDecimal("12345678901234567890.123456789")
    assertEquals(cell("select 12345678901234567890.123456789::numeric"), Num(money): SqlValue)
    cell("select 'NaN'::numeric") match {
      case F64(x) => assert(x.isNaN)
      case other => fail(s"expected F64(NaN), got $other")
    }
    assertEquals(cell("select array[1.5, 2.25]::numeric[]"), Arr(Vector(Num(BigDecimal("1.5")), Num(BigDecimal("2.25")))): SqlValue)
    withDb { db =>
      run(db.update("drop table if exists okay_ledger")): Unit
      run(db.update("create table okay_ledger(id int not null, amount numeric(30, 9) not null, " +
        "ref uuid not null, doc jsonb not null, at timestamptz not null)")): Unit
      run(db.update("insert into okay_ledger values (1, 12345678901234567890.123456789, " +
        "'6ba7b810-9dad-11d1-80b4-00c04fd430c8', '{\"k\": [1, 2]}', '2026-09-02 06:00:00+00')")): Unit
      val sql = "select id, amount, ref, doc, at from okay_ledger"
      assertEquals(run(db.describe(sql)).map(_.tpe),
        Vector[SqlType](SqlType.I32, SqlType.Num, SqlType.Uuid, SqlType.Json, SqlType.Timestamp))
      assertEquals(run(Typed.verify[Ledger](db, sql)), Vector.empty)
      assertEquals(chunks(Typed.rows[Ledger](db, sql)).flatten.map(_.map(r => (r.amount, r.ref, r.doc))),
        List(Right((money, "6ba7b810-9dad-11d1-80b4-00c04fd430c8", "{\"k\": [1, 2]}"))))
      // the BigDecimal param binds as its exact text; pg types $2 from the column
      assertEquals(run(db.update("insert into okay_ledger values (2, $1, $2::uuid, '{}', now())",
        Vector(Num(money + 1), Text("6ba7b810-9dad-11d1-80b4-00c04fd430c9")))), 1L)
    }
    assertEquals(cell("select amount from okay_ledger where id = 2"), Num(money + 1): SqlValue)
  }
}
