package okay.r2dbc

import okay.{!, +, Async, Chunk, Handler, Produce}
import okay.given
import okay.sql.{Col, Isolation, SqlType, SqlValue, Typed}
import okay.sql.given
import io.r2dbc.spi.Connection

/**
 * The seam's contract over an R2DBC connection, engine as a
 * constructor: raw query/update/batch/transact through the trait, and
 * the typed layer over it unchanged — the same program the JDBC and
 * pg drivers run (specs/sql.md, sql-r2dbc).
 */
abstract class R2dbcSuite extends munit.FunSuite:
  def engine: String
  def open(): Connection
  /** does the driver KNOW nullability? r2dbc-postgresql answers UNKNOWN
   * for every column (pg's RowDescription carries none, and the hatch
   * has no catalog road as okay-pg does), so verify names each
   * non-Option column there — stated, not hidden */
  def knowsNullability: Boolean = true

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

  final case class Person(id: Int, name: String, score: Double, active: Boolean,
                          balance: BigDecimal, note: Option[String])
  given okay.codec.Schema[Person] = okay.codec.Schema.derived

  def fresh(): R2dbcSql =
    val db = R2dbcSql(open(), fetchSize = 8)
    run(db.update("drop table if exists okay_r2dbc"))
    run(db.update("create table okay_r2dbc(id int not null, name varchar(50) not null, " +
      "score double precision not null, active boolean not null, balance decimal(12,2) not null, note varchar(200))"))
    db

  test(s"$engine: update, batch, a streamed query in pulled chunks, params bound by index") {
    val db = fresh()
    try
      assertEquals(run(db.update("insert into okay_r2dbc values ($1, $2, $3, $4, $5, $6)",
        Vector(SqlValue.I32(1), SqlValue.Text("ann"), SqlValue.F64(1.5), SqlValue.Bool(true),
          SqlValue.Num(BigDecimal("10.25")), SqlValue.Null))), 1L)
      val rows = okay.ChunkBuf.of((2 to 30).toVector.map(i => Vector[SqlValue](SqlValue.I32(i), SqlValue.Text(s"p$i"),
        SqlValue.F64(i / 2.0), SqlValue.Bool(i % 2 == 0), SqlValue.Num(BigDecimal(i)), SqlValue.Text(s"n$i"))))
      assertEquals(run(db.batch("insert into okay_r2dbc values ($1, $2, $3, $4, $5, $6)", rows)), 29L)
      val chunks = collectChunks(db.query("select id, name, score, active, balance, note from okay_r2dbc order by id"))
      assertEquals(chunks.map(_.length).sum, 30)
      assert(chunks.length >= 4, s"pulled in chunks of the fetch size, got ${chunks.length}")
      assertEquals(chunks.head.head, Vector(SqlValue.I32(1), SqlValue.Text("ann"), SqlValue.F64(1.5),
        SqlValue.Bool(true), SqlValue.Num(BigDecimal("10.25")), SqlValue.Null))
      assertEquals(collectChunks(db.query("select name from okay_r2dbc where id = $1", Vector(SqlValue.I32(7)))).flatten,
        List(Vector(SqlValue.Text("p7"))))
      assertEquals(collectChunks(db.query("select name from okay_r2dbc where id = $1", Vector(SqlValue.I32(99)))).flatten, Nil)
    finally db.close()
  }

  test(s"$engine: the typed layer runs unchanged — rows into a case class, verify clean on a populated table, a drift named") {
    val db = fresh()
    try
      run(db.update("insert into okay_r2dbc values ($1, $2, $3, $4, $5, $6)",
        Vector(SqlValue.I32(1), SqlValue.Text("ann"), SqlValue.F64(1.5), SqlValue.Bool(true),
          SqlValue.Num(BigDecimal("10.25")), SqlValue.Null)))
      val sql = "select id, name, score, active, balance, note from okay_r2dbc"
      assertEquals(collectChunks(Typed.rows[Person](db, sql)).flatten,
        List(Right(Person(1, "ann", 1.5, true, BigDecimal("10.25"), None))))
      val drifts = run(Typed.verify[Person](db, sql))
      if knowsNullability then assertEquals(drifts, Vector.empty)
      else
        // the driver cannot say: every non-Option column is named, none invented
        assertEquals(drifts.map(d => (d.column.toLowerCase, d.found)).toSet,
          Set("id", "name", "score", "active", "balance").map(_ -> "nullable"))
      final case class Wrong(id: String, name: String, score: Double, active: Boolean, balance: BigDecimal, note: Option[String])
      given okay.codec.Schema[Wrong] = okay.codec.Schema.derived
      val wrong = run(Typed.verify[Wrong](db, sql)).filterNot(_.found == "nullable")
      assertEquals(wrong.map(_.column.toLowerCase), Vector("id"))
      // the stated limit: an empty result has no metadata to describe
      assertEquals(run(db.describe("select id from okay_r2dbc where id = 0")), Vector.empty)
    finally db.close()
  }

  test(s"$engine: transact — begin with the granted isolation read back, rollback undoes, nested begin refuses") {
    val db = fresh()
    try
      val g = run(db.begin(Isolation.Serializable))
      assertEquals(g.requested, Isolation.Serializable)
      run(db.update("insert into okay_r2dbc values (1, 'x', 0, true, 0, null)"))
      val _ = intercept[IllegalStateException](run(db.begin(Isolation.ReadCommitted)))
      run(db.rollback())
      assertEquals(collectChunks(db.query("select count(*) from okay_r2dbc")).flatten.head.head match
        case SqlValue.I64(n) => n
        case SqlValue.I32(n) => n.toLong
        case SqlValue.Num(n) => n.toLong
        case other => fail(s"count: $other"), 0L)
      run(db.begin(Isolation.ReadCommitted))
      run(db.update("insert into okay_r2dbc values (2, 'y', 0, true, 0, null)"))
      run(db.commit())
      assertEquals(collectChunks(db.query("select id from okay_r2dbc")).flatten, List(Vector(SqlValue.I32(2))))
    finally db.close()
  }
