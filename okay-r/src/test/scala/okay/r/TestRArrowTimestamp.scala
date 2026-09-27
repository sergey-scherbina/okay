package okay.r

import okay.arrow.{Column, Table, TimeUnit}
import okay.codec.FrameFormat

object RArrowEcho:
  val mod = R.module("rarrowecho", """
    echo <- function(df) df
  """)

/** r-arrow-timestamp-exact: a timestamp crosses R and comes back to the
 * microsecond. R holds a POSIXct as DOUBLE seconds, so 536.074 s is
 * 536.07399999999996, and arrow's POSIXct → timestamp[us] truncates it
 * to 536073999 µs — found by okay-watch's round-trip property (seed 103). */
class TestRArrowTimestamp extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = RArrow.rscript.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  private given FrameFormat = FrameFormat("arrow", strict = true)
  private lazy val w = RSubprocess.worker(RArrow.rscript.get, Seq(RArrowEcho.mod))
  override def afterAll(): Unit = if !munitIgnore then w.close()

  test("a POSIXct column comes back to the microsecond: whole milliseconds, random microseconds, before 1970, NA") {
    val g = scala.util.Random(27)
    val us = Vector(536074000L, -536074000L, 1790000000123456L, -2208988800000001L, 0L) ++
      Vector.fill(500)(g.nextInt(1_000_000).toLong * 1000L) ++
      Vector.fill(500)((g.nextLong() % 3_155_760_000_000_000L).abs - 2_208_988_800_000_000L)
    val ok = Array.tabulate(us.size + 1)(_ < us.size)
    val sent = Table(Vector("at" -> Column.Timestamp(TimeUnit.Micro, Some("UTC"), (us :+ 0L).toArray, ok)), Vector.empty)
    val back = w.frameTable("rarrowecho::echo", sent, Vector.empty, exact = true).fold(c => fail(c.toString), identity)
    back.cols.head._2 match
      case Column.Timestamp(TimeUnit.Micro, Some("UTC"), v, valid) =>
        assertEquals(valid.toVector, ok.toVector)
        val wrong = us.indices.filter(i => v(i) != us(i)).map(i => s"${us(i)} came back ${v(i)}")
        assertEquals(wrong.take(5), Vector.empty, s"${wrong.size} of ${us.size} changed")
      case other => fail(s"not a UTC microsecond timestamp: ${Column.describe(other)}")
  }

/** arrow-empty-dictionary: a zero-row answer keeps a factor's levels. R
 * arrow's `write_ipc_stream` sends a zero-row table as its schema alone,
 * and the levels go with the batch it never writes */
class TestRArrowZeroRows extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = RArrow.rscript.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  private given FrameFormat = FrameFormat("arrow", strict = true)
  private lazy val w = RSubprocess.worker(RArrow.rscript.get, Seq(RArrowEcho.mod))
  override def afterAll(): Unit = if !munitIgnore then w.close()

  test("a zero-row frame comes back with its factor's levels, in order, the unused ones too") {
    val none = Array.emptyBooleanArray
    val sent = Table(Vector(
      "f" -> Column.Dictionary(Array.emptyIntArray, Column.Utf8(Array("employed", "unemployed", "Київ"), Array.fill(3)(true)),
        ordered = false, none),
      "n" -> Column.Ints(32, true, Array.emptyLongArray, none)), Vector.empty)
    val back = w.frameTable("rarrowecho::echo", sent, Vector.empty, exact = true).fold(c => fail(c.toString), identity)
    assertEquals(back.rows, 0)
    back.cols.head._2 match
      case Column.Dictionary(_, Column.Utf8(vs, _), _, _) => assertEquals(vs.toVector, Vector("employed", "unemployed", "Київ"))
      case other => fail(s"not a dictionary: ${Column.describe(other)}")
  }
