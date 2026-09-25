package okay.cluster.foreign

import okay.given
import okay.cluster.{Cluster, Flow, Job, Jobs, Wire}
import okay.codec.Schema
import okay.r.{R, TestR}

/** the map AND the reduce of one R module, for the one job text (R's
 * integers, so the rows are `RecI`/`OutI` and the partial `StatI`) */
object RStatsMod:
  val mod = R.module("rstatsmod", """
    double <- function(frame) { frame$v <- frame$v * 2L; frame }
    step <- function(frame, acc) {
      n <- nrow(frame) + (if (is.null(acc)) 0L else acc$n)
      s <- sum(frame$v) + (if (is.null(acc)) 0L else acc$sum)
      m <- max(c(frame$v, if (is.null(acc)) integer(0) else acc$max))
      data.frame(n = as.integer(n), sum = as.integer(s), max = as.integer(m))
    }
    merge <- function(a, b) list(n = a$n + b$n, sum = a$sum + b$sum, max = max(a$max, b$max))
  """)

final class StatsJobI[M](val name: String, mod: M)(using Engine[M], Reduces[M]) extends Job[Scale, Option[StatI]]:
  type A = OutI
  def params: Schema[Scale] = summon[Schema[Scale]]
  def answer: Schema[Option[StatI]] = Schema.SOption(() => summon[Schema[StatI]])
  def flow(p: Scale, parts: Int): Flow[OutI] = Flow.slices(RJobs.rows(p.n), parts).mapIn[OutI](mod, "double")
  def sink(p: Scale): Wire[OutI, Option[StatI]] = Reduce.in[OutI, StatI](mod, "step", "merge")

object REngineJobs:
  given Engine[okay.r.RModule] = Engine.r(TestR.rscript.getOrElse("Rscript"))
  given Reduces[okay.r.RModule] = Reduces.r(TestR.rscript.getOrElse("Rscript"))
  val job = StatsJobI("test.engine.r", RStatsMod.mod)
  Jobs.register(job)
  def install(): Unit = ()

/** stage 3 over a REAL R: the same job text, handed an `RModule` (Live) */
class TestREngine extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestR.rscript.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")
  REngineJobs.install()

  test("one job text, the module R's: map and reduce in R over two workers, the JVM's answer") {
    val rows = RJobs.rows(5000).map(r => RecI(r.key, r.v * 2))
    val expected = Some(StatI(rows.length, rows.map(_.v).sum, rows.map(_.v).max))
    assertEquals(Cluster.run(REngineJobs.job, Scale(5000), 4, Vector.fill(2)(Cluster.local)).runWith.value, expected)
  }
