package okay.py

import PyValue.*
import okay.arrow.OkayArrow
import okay.codec.Json

/**
 * py-arrow stage 4: a frame's round trip through a real Python worker,
 * the JSON road against Arrow, with this side's encode and decode split
 * out. Not JMH, for MeasureRFrame's reason: seconds per frame, a real
 * process, medians of five. Live-tagged; the assertions are sanity only.
 */
class MeasurePyArrow extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = PyArrow.python.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(20, "min")

  private def frameOf(rows: Int) = PyFrame(Vector(
    "a" -> Vector.tabulate(rows)(i => F64(i + 0.5)),
    "b" -> Vector.tabulate(rows)(i => I64(i.toLong)),
    "s" -> Vector.tabulate(rows)(i => Str("row" + i))))

  private def ms(body: => Any): Double =
    val t0 = System.nanoTime(); body: Unit; (System.nanoTime() - t0) / 1e6

  private def median(n: Int)(body: => Any): Double =
    val xs = Vector.fill(n)(ms(body)).sorted
    xs(xs.length / 2)

  test("a frame's round trip: JSON against Arrow, and where the time sits") {
    val arrow = ForeignWorker.start(PyArrow.python.get, modules = Seq(ArrowConf.mod))
    val json =
      import FrameFormat.Json.given
      ForeignWorker.start(PyArrow.python.get, modules = Seq(ArrowConf.mod))
    try
      assertEquals((arrow.wire, json.wire), ("json/none+arrow", "json/none"))
      println("%8s | %9s %9s %9s | %9s %9s %9s | %9s %9s %9s".format(
        "rows", "JSON rt", "Arrow rt", "Table rt", "JSON enc", "JSON dec", "JSON MB", "Arrow enc", "Arrow dec", "Arrow MB"))
      for rows <- Vector(100000, 500000) do
        val f = frameOf(rows)
        def call(w: ForeignWorker, fn: String = "identity") = w.handler.handle(ForeignEval.Frame(s"arrowconf:$fn", f, Vector.empty))
        // warm both: the first call pays the JIT here and pyarrow's import there
        assertEquals(call(json).map(_.cols.head._2.length), Right(rows))
        assertEquals(call(arrow).map(_.cols.head._2.length), Right(rows))
        val jsonRt = median(5)(call(json))
        val arrowRt = median(5)(call(arrow))
        // `@okay.arrow` and back as it came: the road with no dict at all
        val tableRt = median(5)(call(arrow, "same"))
        val text = Json.print(Wire.encFrame(f))
        val jsonEnc = median(5)(Json.print(Wire.encFrame(f)))
        val jsonDec = median(5)(Wire.decFrame(Json.parse(text)))
        val table = ArrowFrames.table(f).toOption.get
        val bytes = OkayArrow.write(table)
        val arrowEnc = median(5)(OkayArrow.write(ArrowFrames.table(f).toOption.get))
        val arrowDec = median(5)(ArrowFrames.frame(OkayArrow.read(bytes)))
        println(f"$rows%8d | $jsonRt%9.0f $arrowRt%9.0f $tableRt%9.0f | $jsonEnc%9.0f $jsonDec%9.0f ${text.length / 1e6}%9.1f | $arrowEnc%9.0f $arrowDec%9.0f ${bytes.length / 1e6}%9.1f")
      assertEquals(arrow.arrowFrames._1, arrow.arrowFrames._2)
    finally
      arrow.close(); json.close()
  }
