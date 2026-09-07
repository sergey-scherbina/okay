package okay.jdbc

import okay.{!, +, Async, Chunk, Chunks, Handler, Produce, effect}
import okay.given
import okay.codec.Schema
import okay.sql.{Bad, Col, Granted, Isolation, Sql, SqlValue, Typed}

import java.sql.DriverManager

/**
 * What the ROW FOLD costs, and what share of a row it is
 * (sql-fold-profile). staged-runtime's spec named okay-sql as the
 * best candidate for a run-time staged codec, on a condition: a
 * profile showing the fold at >= 30% of the per-row cost. This
 * measures it, so the condition is settled by a number instead of a
 * guess.
 *
 * Two measurements, because they answer different questions:
 *   - THE FOLD ALONE, over a `Replay` driver that hands back frames
 *     already in memory: no socket, no JDBC, no parsing — the decode
 *     and nothing else. This is what staging could make faster.
 *   - THE SHARE, end to end against in-memory H2: typed rows against
 *     raw frames from the same query. The difference is the fold; the
 *     rest is the driver, which staging cannot touch.
 *
 * Deliberately not JMH, for MeasureScript's reason: the interesting
 * numbers are per-1000-rows milliseconds, the variance is the box's,
 * and a fork per lane would pay the H2 setup again for a number whose
 * shape is already clear. Medians with the warmup discarded, printed
 * as a table, Live-tagged; the ASSERTIONS are sanity bounds only (the
 * rows decode, the typed read is not cheaper than the raw one), never
 * a millisecond threshold that a loaded box turns into a red build.
 */
class MeasureSqlFold extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  final case class Row(id: Long, userName: String, age: Option[Int],
                       balance: Double, active: Boolean, label: String)
  given Schema[Row] = Schema.derived

  private val n = 2000
  private val url = "jdbc:h2:mem:foldprofile;DB_CLOSE_DELAY=-1"
  private val select = "select id, user_name, age, balance, active, label from wide order by id"

  override def beforeAll(): Unit =
    val c = DriverManager.getConnection(url, "sa", "")
    try
      val st = c.createStatement()
      st.execute("""create table wide(
        id bigint not null primary key,
        user_name varchar(64) not null,
        age int,
        balance double precision not null,
        active boolean not null,
        label varchar(64) not null)""")
      val ps = c.prepareStatement("insert into wide values (?, ?, ?, ?, ?, ?)")
      for i <- 1 to n do
        ps.setLong(1, i.toLong)
        ps.setString(2, s"user-$i")
        if i % 3 == 0 then ps.setNull(3, java.sql.Types.INTEGER) else ps.setInt(3, 20 + i % 50)
        ps.setDouble(4, i * 1.5)
        ps.setBoolean(5, i % 2 == 0)
        ps.setString(6, s"a label for row $i")
        ps.addBatch()
      ps.executeBatch(): Unit
      ps.close()
      st.close()
    finally c.close()

  /** a driver that replays frames already in memory: the fold, alone */
  private final class Replay(cols: Vector[Col], frames: Vector[Vector[SqlValue]], per: Int) extends Sql:
    def describe(sql: String): Vector[Col] ! Async = okay.pure(cols)
    def query(sql: String, params: Vector[SqlValue] = Vector.empty)
    : Chunk[Vector[SqlValue]] ! (Produce + Async) =
      type F = Produce + Async
      def go(rest: Vector[Vector[SqlValue]]): Chunk[Vector[SqlValue]] ! F =
        if rest.isEmpty then okay.pure(Chunks.emptyChunk)
        else
          val (c, more) = rest.splitAt(per)
          val chunk: Chunk[Vector[SqlValue]] = scala.collection.immutable.ArraySeq.from(c)
          effect[F, Chunk[Vector[SqlValue]]](chunk).flatMap(_ => go(more))
      go(frames)
    def update(sql: String, params: Vector[SqlValue] = Vector.empty): Long ! Async = okay.pure(0L)
    def batch(sql: String, rows: Chunk[Vector[SqlValue]]): Long ! Async = okay.pure(0L)
    def begin(isolation: Isolation): Granted ! Async = okay.pure(Granted(isolation, isolation))
    def commit(): Unit ! Async = okay.pure(())
    def rollback(): Unit ! Async = okay.pure(())
    def cancel(): Unit = ()

  private def withDb[A](f: Sql => A): A =
    val conn = DriverManager.getConnection(url, "sa", "")
    try f(JdbcSql(conn))
    finally conn.close()

  private def drain[A](s: Chunk[A] ! (Produce + Async)): Int =
    import okay.!.*
    def go(rest: Chunk[A] ! (Produce + Async), seen: Int): Int =
      (rest.resume: @unchecked) match
        case Pure(_) => seen
        case Effect(e) => okay.<|>[Async, Produce](e) match
          case Left(a) => (summon[Handler[Async]].handle(a): Unit); seen
          case Right(c) => seen + c.asInstanceOf[Chunk[A]].length
        case Bind(Effect(e), k) => okay.<|>[Async, Produce](e) match
          case Left(a) => go(k(summon[Handler[Async]].handle(a)), seen)
          case Right(c) => go(k(c), seen + c.asInstanceOf[Chunk[A]].length)
    go(s, 0)

  private def median(xs: Vector[Double]): Double =
    val s = xs.sorted
    if s.isEmpty then 0.0
    else if s.length % 2 == 1 then s(s.length / 2)
    else (s(s.length / 2 - 1) + s(s.length / 2)) / 2

  /** median milliseconds of `samples`, after `warm` discarded runs */
  private def ms(warm: Int, samples: Int)(body: => Int): Double =
    var checked = 0
    for _ <- 1 to warm do checked += body
    assert(checked >= 0)
    median(Vector.fill(samples) {
      val t = System.nanoTime()
      val got = body
      val d = (System.nanoTime() - t) / 1e6
      assertEquals(got, n, "every row arrived")
      d
    })

  private val rows = scala.collection.mutable.ArrayBuffer.empty[(String, String, String)]
  private def row(what: String, value: String, note: String): Unit =
    rows += ((what, value, note))
    println(f"  $what%-40s $value%14s   $note")

  override def afterAll(): Unit =
    println(s"\n| what (per $n rows) | median | note |")
    println("|---|---:|---|")
    rows.foreach((w, v, nt) => println(s"| $w | $v | $nt |"))
    val load = java.lang.management.ManagementFactory.getOperatingSystemMXBean.getSystemLoadAverage
    println(f"%nhost: ${Runtime.getRuntime.availableProcessors} cpus, load average $load%.2f")

  test("the fold alone, and its share of a row read end to end") {
    // the frames and their description, taken from the real driver once
    val (cols, frames) = withDb { db =>
      val cs = !.run(Async.run[Vector[Col], Nothing](db.describe(select)))
      val fs = scala.collection.mutable.ArrayBuffer.empty[Vector[SqlValue]]
      import okay.!.*
      def go(rest: Chunk[Vector[SqlValue]] ! (Produce + Async)): Unit =
        (rest.resume: @unchecked) match
          case Pure(_) => ()
          case Effect(e) => okay.<|>[Async, Produce](e) match
            case Left(a) => (summon[Handler[Async]].handle(a): Unit)
            case Right(c) => fs ++= c.asInstanceOf[Chunk[Vector[SqlValue]]]
          case Bind(Effect(e), k) => okay.<|>[Async, Produce](e) match
            case Left(a) => go(k(summon[Handler[Async]].handle(a)))
            case Right(c) => fs ++= c.asInstanceOf[Chunk[Vector[SqlValue]]]; go(k(c))
      go(db.query(select))
      (cs, fs.toVector)
    }
    assertEquals(frames.length, n, "the fixture loaded")

    val replay = Replay(cols, frames, per = 256)
    // the fold alone: frames already in memory, typed decode over them
    // 50 warmups and 31 samples, not 3 and 7: at 0.6 ms a run the JIT
    // and one GC dominate, and the medians of a 7-sample run moved by
    // half between two runs of the SAME code (sql-plan-cells)
    val foldOnly = ms(50, 31)(drain(Typed.rows[Row](replay, select)))
    // the same replay WITHOUT the fold: the frames handed through
    val replayOnly = ms(50, 31)(drain(replay.query(select)))
    // end to end against H2: typed, and raw frames from the same query
    val typedEnd = withDb(db => ms(20, 15)(drain(Typed.rows[Row](db, select))))
    val rawEnd = withDb(db => ms(20, 15)(drain(db.query(select))))

    val fold = foldOnly - replayOnly
    val endToEndFold = typedEnd - rawEnd
    val share = if typedEnd > 0 then 100.0 * endToEndFold / typedEnd else 0.0

    row("fold alone (replay frames, typed)", f"$foldOnly%.2f ms", "decode + the stream, no driver")
    row("the same replay, frames only", f"$replayOnly%.2f ms", "the stream alone")
    row("=> the fold itself", f"$fold%.2f ms", f"${1000.0 * fold / n}%.2f us per row, 6 columns")
    row("end to end, typed (H2 in memory)", f"$typedEnd%.2f ms", "describe + query + fold")
    row("end to end, raw frames", f"$rawEnd%.2f ms", "the driver alone")
    row("=> the fold's SHARE of a row", f"$share%.1f %%", "staged-runtime's condition is 30%")

    // sanity only: the rows decode, and adding a decode never makes a
    // read cheaper. No threshold on the milliseconds themselves.
    val one = drain(Typed.rows[Row](replay, select))
    assertEquals(one, n)
    assert(typedEnd >= rawEnd * 0.5, s"typed $typedEnd ms vs raw $rawEnd ms: implausible")
    assert(foldOnly >= replayOnly * 0.5, s"fold $foldOnly ms vs stream $replayOnly ms: implausible")
  }

  test("the decoded values are the fixture's, so the numbers measure real work") {
    withDb { db =>
      val got = scala.collection.mutable.ArrayBuffer.empty[Either[Bad, Row]]
      import okay.!.*
      def go(rest: Chunk[Either[Bad, Row]] ! (Produce + Async)): Unit =
        (rest.resume: @unchecked) match
          case Pure(_) => ()
          case Effect(e) => okay.<|>[Async, Produce](e) match
            case Left(a) => (summon[Handler[Async]].handle(a): Unit)
            case Right(c) => got ++= c.asInstanceOf[Chunk[Either[Bad, Row]]]
          case Bind(Effect(e), k) => okay.<|>[Async, Produce](e) match
            case Left(a) => go(k(summon[Handler[Async]].handle(a)))
            case Right(c) => got ++= c.asInstanceOf[Chunk[Either[Bad, Row]]]; go(k(c))
      go(Typed.rows[Row](db, select))
      assertEquals(got.length, n)
      assertEquals(got.head, Right(Row(1L, "user-1", Some(21), 1.5, false, "a label for row 1")))
      assertEquals(got(2), Right(Row(3L, "user-3", None, 4.5, false, "a label for row 3")))
      assert(got.forall(_.isRight), "no row was damage")
    }
  }
