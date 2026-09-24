package okay.r

import okay.{!, %, +, Writer, given}
import okay.RowLift.plus

object TestRStream:
  val m = R.module("streamr", """
    doubled <- function(xs) xs * 2
    evens <- function(xs) xs[xs %% 2 == 0]
    running <- function() {
      total <- 0
      function(xs) {
        total <<- total + cumsum(xs)[length(xs)]
        total
      }
    }
  """)

/** foreign-streaming against a LIVE R (specs/foreign-highlevel.md stage 6) */
class TestRStream extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestR.rscript.isEmpty

  private lazy val r = RSubprocess.start(rscript = TestR.rscript.get, modules = Seq(TestRStream.m))
  private given okay.Handler[REval] = r.handler
  override def afterAll(): Unit = if TestR.rscript.nonEmpty then r.close()

  private def numbers(n: Int): Unit ! Writer % Double + REval =
    (1 to n).foldLeft(okay.pure[Writer % Double, Unit](()))((p, x) => p.flatMap(_ => Writer.tell(x.toDouble))).plus[REval]

  private def run[O](p: Unit ! Writer % O + REval): List[O] = Writer.run(p).runWith._1.toList

  test("an R function over a vector as a stage, in chunks, a partial chunk at the end") {
    assertEquals(run(okay.through(numbers(7))(R.stage[Double, Double]("streamr::doubled", chunk = 3))),
      (1 to 7).map(_ * 2.0).toList)
  }

  test("a filter: R answers fewer than it was given") {
    assertEquals(run(okay.through(numbers(9))(R.stage[Double, Double]("streamr::evens", chunk = 4))), List(2.0, 4.0, 6.0, 8.0))
  }

  test("a held closure as a stateful stage: one running total per chunk") {
    val acc = R.hold("streamr::running")().runWith.toOption.get
    assertEquals(acc.rClass, "function")
    assertEquals(run(okay.through(numbers(6))(acc.stage[Double, Double](chunk = 2))), List(3.0, 10.0, 21.0))
  }
}
