package okay.cluster.foreign

import okay.{Aggregator, given}
import okay.cluster.{Cluster, Flow, Flows, Job, Jobs, Wire}
import okay.codec.Schema
import okay.r.{R, TestR}

final case class RecI(key: Int, v: Int) derives Schema
final case class OutI(key: Int, v: Int) derives Schema

/** the map, in R: a data.frame in, a data.frame out */
object RScaling:
  val mod = R.module("rscaling", """
    double <- function(frame) { frame$v <- frame$v * 2L; frame }
  """)

object RJobs:
  def rscript: String = TestR.rscript.getOrElse("Rscript")
  def rows(n: Int): IndexedSeq[RecI] = (0 until n).map(i => RecI(i % 7, (i * 31) % 1000))

  object Doubling extends Job[Scale, Long]:
    type A = OutI
    def name: String = "test.foreign.r.double"
    def params: Schema[Scale] = summon[Schema[Scale]]
    def answer: Schema[Long] = summon[Schema[Long]]
    def flow(p: Scale, parts: Int): Flow[OutI] =
      Flow.slices(rows(p.n), parts).mapR[OutI](RScaling.mod, "double", rscript)
    def sink(p: Scale): Wire[OutI, Long] = Wire.fold(Aggregator.sum[Long].contramap[OutI](_.v.toLong))

  Jobs.register(Doubling)
  def install(): Unit = ()

/** foreign-map-reduce over a REAL R (Live; skipped where there is none) */
class TestRMapReduce extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestR.rscript.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")
  RJobs.install()

  test("Cluster.run over two in-process workers computes what the fan computes, every row doubled by R") {
    val p = Scale(5000)
    val here = Flows.fan(RJobs.Doubling.flow(p, 4), RJobs.Doubling.sink(p)).runWith
    val there = Cluster.run(RJobs.Doubling, p, 4, Vector.fill(2)(Cluster.local)).runWith
    assertEquals(here.value, RJobs.rows(5000).map(_.v.toLong * 2).sum)
    assertEquals(there.value, here.value)
  }
