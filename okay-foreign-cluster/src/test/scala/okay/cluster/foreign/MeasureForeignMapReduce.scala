package okay.cluster.foreign

import okay.{Aggregator, given}
import okay.cluster.{Cluster, Flow, Flows, Job, Jobs, Wire}
import okay.codec.Schema
import okay.foreign.{Foreign, PyArrow, TestPy}

/** the map and the reduce for the measurement: the JSON road's function,
 * the same under `@okay.arrow` with no Python loop at all, and a reduce */
object Measured:
  val mod = Foreign.module("measured", """
    import okay

    def double(frame):
        return {"key": frame["key"], "v": [x * 2 for x in frame["v"]]}

    @okay.arrow
    def double_arrow(t):
        import pyarrow as pa, pyarrow.compute as pc
        return pa.table({"key": t["key"], "v": pc.multiply(t["v"], 2)})

    def step(frame, acc):
        return {"sum": [sum(frame["v"]) + (acc["sum"] if acc else 0)]}

    def merge(a, b):
        return {"sum": a["sum"] + b["sum"]}
  """)

final case class Sum(sum: Long) derives Schema

/** one job per lane, so `Cluster.run` can name it */
final class Lane(val name: String, py: String, fn: String, batch: Int, reduce: Boolean) extends Job[Scale, Long]:
  type A = Out
  def params: Schema[Scale] = summon[Schema[Scale]]
  def answer: Schema[Long] = summon[Schema[Long]]
  def flow(p: Scale, parts: Int): Flow[Out] =
    val src = Flow.slices(Rows.of(p.n), parts)
    if fn == "scala" then src.map(r => Out(r.key, r.v * 2))
    else src.mapPy[Out](Measured.mod, fn, py, batch = batch)
  def sink(p: Scale): Wire[Out, Long] =
    if reduce then
      // the reduce in Python over the mapped rows, then the JVM reads the sum
      val w = Reduce.py[Out, Sum](Measured.mod, "step", "merge", py, batch = batch)
      new Wire[Out, Long]:
        type P = w.P; type W = w.W; type S = w.S
        def wire = w.wire; def state = w.state
        def times = w.times; def slack = w.slack
        def start(bounds: Vector[okay.cluster.Bounds]) = w.start(bounds)
        def step(p: P, a: Out) = w.step(p, a)
        def finish(p: P) = w.finish(p); def peek(p: P) = w.peek(p)
        def empty = w.empty
        def absorb(s: S, ws: Vector[W], watermark: Long) = w.absorb(s, ws, watermark)
        def emit(s: S): Long = w.emit(s).fold(0L)(_.sum)
        def drops(ws: Vector[W]) = w.drops(ws); def merged(ws: Vector[W]) = w.merged(ws)
    else Wire.fold(Aggregator.sum[Long].contramap[Out](_.v))

/**
 * foreign-map-reduce-measure: the map in Python against the map in
 * Scala, the JSON road against Arrow, and the reduce in Python against
 * `Wire.fold` — 1M rows, 4 partitions, in one process (`Flows.fan`: the
 * stage's own cost) and over three in-process workers (`Cluster.run`).
 * Not JMH: seconds per lane, real interpreters, medians of three. Live.
 */
class MeasureForeignMapReduce extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty || PyArrow.python.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(30, "min")

  private def ms(body: => Any): Double =
    val t0 = System.nanoTime(); body: Unit; (System.nanoTime() - t0) / 1e6
  private def median(n: Int)(body: => Any): Double =
    val xs = Vector.fill(n)(ms(body)).sorted
    xs(xs.length / 2)
  private def load: String =
    scala.util.Try(String(ProcessBuilder("sysctl", "-n", "vm.loadavg").start().getInputStream.readAllBytes()).trim)
      .getOrElse("?")

  test("1M rows: the map in Scala, in Python over JSON, in Python over Arrow, under @okay.arrow; the reduce in Python") {
    val json = TestPy.python.get
    val arrow = PyArrow.python.get
    assume(json != arrow, "the JSON lane needs a python WITHOUT pyarrow beside one with it (OKAY_PYARROW_PYTHON)")
    val n = 1000000
    val p = Scale(n)
    val expected = Rows.doubled(n)
    val lanes = Vector(
      Lane("measure.scala", json, "scala", 4096, reduce = false),
      Lane("measure.py.json", json, "double", 4096, reduce = false),
      Lane("measure.py.arrow", arrow, "double", 4096, reduce = false),
      Lane("measure.py.arrow.64k", arrow, "double", 65536, reduce = false),
      Lane("measure.py.arrow.compute", arrow, "double_arrow", 65536, reduce = false),
      Lane("measure.py.arrow.reduce", arrow, "double_arrow", 65536, reduce = true))
    lanes.foreach(Jobs.register)
    println(s"load before: $load")
    println("%-26s | %10s | %10s".format("lane (1M rows, 4 parts)", "fan ms", "3 workers ms"))
    for lane <- lanes do
      def fan = Flows.fan(lane.flow(p, 4), lane.sink(p)).runWith.value
      def cluster = Cluster.run(lane, p, 4, Vector.fill(3)(Cluster.local)).runWith.value
      assertEquals(fan, expected, lane.name)           // warm, and right
      assertEquals(cluster, expected, lane.name)
      val f = median(3)(fan)
      val c = median(3)(cluster)
      println(f"${lane.name}%-26s | $f%10.0f | $c%10.0f")
    println(s"load after: $load")
  }
