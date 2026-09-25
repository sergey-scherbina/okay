package okay.cluster.foreign

import okay.{Aggregator, given}
import okay.cluster.{Cluster, Flow, Flows, Job, Jobs, Wire}
import okay.codec.Schema
import okay.py.{Foreign, TestPy}

/** the map, in Python */
object Scaling:
  val mod = Foreign.module("scaling", """
    def double(frame):
        return {"key": frame["key"], "v": [x * 2 for x in frame["v"]]}

    def boom(frame):
        raise ValueError("no")
  """)

object PyJobs:
  /** the python the tests found; a worker that has none does not get here */
  def python: String = TestPy.python.getOrElse("python3")

  object Doubling extends Job[Scale, Long]:
    type A = Out
    def name: String = "test.foreign.py.double"
    def params: Schema[Scale] = summon[Schema[Scale]]
    def answer: Schema[Long] = summon[Schema[Long]]
    def flow(p: Scale, parts: Int): Flow[Out] =
      Flow.slices(Rows.of(p.n), parts).mapPy[Out](Scaling.mod, "double", python)
    def sink(p: Scale): Wire[Out, Long] = Wire.fold(Aggregator.sum[Long].contramap[Out](_.v))

  object Booming extends Job[Scale, Long]:
    type A = Out
    def name: String = "test.foreign.py.boom"
    def params: Schema[Scale] = summon[Schema[Scale]]
    def answer: Schema[Long] = summon[Schema[Long]]
    def flow(p: Scale, parts: Int): Flow[Out] =
      Flow.slices(Rows.of(p.n), parts).mapPy[Out](Scaling.mod, "boom", python)
    def sink(p: Scale): Wire[Out, Long] = Wire.fold(Aggregator.sum[Long].contramap[Out](_.v))

  Jobs.register(Doubling)
  Jobs.register(Booming)
  def install(): Unit = ()

/** foreign-map-reduce over a REAL python3: the map runs in Python, the
 * reduce on the JVM, the answer is the fan's (Live) */
class TestPyMapReduce extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")
  PyJobs.install()

  test("Cluster.run over three in-process workers computes what the fan computes, every row doubled by python3") {
    val p = Scale(20000)
    val here = Flows.fan(PyJobs.Doubling.flow(p, 4), PyJobs.Doubling.sink(p)).runWith
    val there = Cluster.run(PyJobs.Doubling, p, 4, Vector.fill(3)(Cluster.local)).runWith
    assertEquals(here.value, Rows.doubled(20000))
    assertEquals(there.value, here.value)
    assertEquals(there.retried, 0L)
  }

  test("a Python exception fails the run by name, with the function's own message") {
    val e = intercept[Throwable](Cluster.run(PyJobs.Booming, Scale(1000), 2, Vector.fill(2)(Cluster.local)).runWith)
    assert(e.getMessage.contains("py:scaling:boom") && e.getMessage.contains("no"), e.getMessage)
  }
