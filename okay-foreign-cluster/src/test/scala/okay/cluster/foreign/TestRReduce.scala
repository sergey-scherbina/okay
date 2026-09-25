package okay.cluster.foreign

import okay.given
import okay.cluster.{Cluster, Flow, Flows, Job, Jobs, Wire}
import okay.codec.Schema
import okay.r.{R, TestR}

/** count, sum and max as R's integers */
final case class StatI(n: Int, sum: Int, max: Int) derives Schema

/** the reduce, in R: a data.frame and a named list in, a one-row
 * data.frame out; two named lists in, one out */
object RStats:
  val mod = R.module("rstats", """
    step <- function(frame, acc) {
      n <- nrow(frame) + (if (is.null(acc)) 0L else acc$n)
      s <- sum(frame$v) + (if (is.null(acc)) 0L else acc$sum)
      m <- max(c(frame$v, if (is.null(acc)) integer(0) else acc$max))
      data.frame(n = as.integer(n), sum = as.integer(s), max = as.integer(m))
    }
    merge <- function(a, b) list(n = a$n + b$n, sum = a$sum + b$sum, max = max(a$max, b$max))
  """)

object RReduceJobs:
  def rscript: String = TestR.rscript.getOrElse("Rscript")

  object Statting extends Job[Scale, Option[StatI]]:
    type A = RecI
    def name: String = "test.foreign.r.reduce"
    def params: Schema[Scale] = summon[Schema[Scale]]
    def answer: Schema[Option[StatI]] = Schema.SOption(() => summon[Schema[StatI]])
    def flow(p: Scale, parts: Int): Flow[RecI] = Flow.slices(RJobs.rows(p.n), parts)
    def sink(p: Scale): Wire[RecI, Option[StatI]] = Reduce.r[RecI, StatI](RStats.mod, "step", "merge", rscript)

  Jobs.register(Statting)
  def install(): Unit = ()

/** foreign-reduce over a REAL R (Live; skipped where there is none) */
class TestRReduce extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestR.rscript.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")
  RReduceJobs.install()

  test("Cluster.run over two in-process workers computes count, sum and max in R, the JVM's answer") {
    val p = Scale(5000)
    val rows = RJobs.rows(5000)
    val expected = Some(StatI(rows.length, rows.map(_.v).sum, rows.map(_.v).max))
    val here = Flows.fan(RReduceJobs.Statting.flow(p, 4), RReduceJobs.Statting.sink(p)).runWith
    val there = Cluster.run(RReduceJobs.Statting, p, 4, Vector.fill(2)(Cluster.local)).runWith
    assertEquals(here.value, expected)
    assertEquals(there.value, expected)
  }
