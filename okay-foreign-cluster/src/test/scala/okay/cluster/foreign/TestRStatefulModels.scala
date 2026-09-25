package okay.cluster.foreign

import okay.given
import okay.cluster.{Cluster, Flow, Job, Jobs, Wire}
import okay.codec.Schema
import okay.r.{R, TestR}

final case class RunI(key: Int, v: Int, run: Int) derives Schema

/** the model and the stateful stage, in R: a held list and an environment */
object RStateful:
  val mod = R.module("rstateful", """
    fit <- function(params) list(by = params$by)
    scale <- function(frame, model) { frame$v <- frame$v * model$by; frame }
    open <- function() { e <- new.env(); e$sum <- 0L; e }
    step <- function(frame, state) {
      runs <- integer(nrow(frame))
      for (i in seq_len(nrow(frame))) { state$sum <- state$sum + frame$v[i]; runs[i] <- state$sum }
      data.frame(key = frame$key, v = frame$v, run = runs)
    }
    finish <- function(frame, state) data.frame(key = -1L, v = 0L, run = as.integer(state$sum))
  """)

final case class FactorI(by: Int) derives Schema

final class ScaleJobI[M](val name: String, mod: M)(using Models[M]) extends Job[Scale, Long]:
  type A = OutI
  def params: Schema[Scale] = summon[Schema[Scale]]
  def answer: Schema[Long] = summon[Schema[Long]]
  def flow(p: Scale, parts: Int): Flow[OutI] =
    Flow.slices(RJobs.rows(p.n), parts).mapModel[OutI](Model.in(mod, "fit", FactorI(3)), "scale")
  def sink(p: Scale): Wire[OutI, Long] = Wire.fold(okay.Aggregator.sum[Long].contramap[OutI](_.v.toLong))

final class RunningJobI[M](val name: String, mod: M)(using Stateful[M]) extends Job[Scale, (Long, Long)]:
  type A = RunI
  def params: Schema[Scale] = summon[Schema[Scale]]
  def answer: Schema[(Long, Long)] = Schema.derived
  def flow(p: Scale, parts: Int): Flow[RunI] =
    Flow.slices(RJobs.rows(p.n), parts).statefulIn[RunI](mod, "open", "step", "finish")
  def sink(p: Scale): Wire[RunI, (Long, Long)] =
    Wire.fold(okay.Aggregator.sum[Long].contramap[RunI](r => if r.key >= 0 then r.run.toLong else 0L))
      .and(Wire.fold(okay.Aggregator.sum[Long].contramap[RunI](r => if r.key < 0 then r.run.toLong else 0L)))

object RStatefulJobs:
  private val rscript = TestR.rscript.getOrElse("Rscript")
  given Models[okay.r.RModule] = Models.r(rscript)
  given Stateful[okay.r.RModule] = Stateful.r(rscript)
  val scale = ScaleJobI("test.held.r", RStateful.mod)
  val running = RunningJobI("test.stream.r", RStateful.mod)
  Jobs.register(scale)
  Jobs.register(running)
  def install(): Unit = ()

  def expected(n: Int, parts: Int): (Long, Long) =
    val xs = RJobs.rows(n)
    var runs = 0L; var totals = 0L
    for i <- 0 until parts do
      val from = (n.toLong * i / parts).toInt; val until = (n.toLong * (i + 1) / parts).toInt
      var s = 0L
      for r <- xs.slice(from, until) do { s += r.v; runs += s }
      totals += s
    (runs, totals)

/** stage 4 over a REAL R (Live; skipped where there is none) */
class TestRStatefulModels extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestR.rscript.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")
  RStatefulJobs.install()

  test("a model fit once in R, used by every chunk over two workers") {
    assertEquals(Cluster.run(RStatefulJobs.scale, Scale(3000), 4, Vector.fill(2)(Cluster.local)).runWith.value,
      RJobs.rows(3000).map(_.v.toLong * 3).sum)
  }

  test("a stateful stage in R: one environment per partition, the running sums and the finals exact") {
    assertEquals(Cluster.run(RStatefulJobs.running, Scale(3000), 4, Vector.fill(2)(Cluster.local)).runWith.value,
      RStatefulJobs.expected(3000, 4))
  }
