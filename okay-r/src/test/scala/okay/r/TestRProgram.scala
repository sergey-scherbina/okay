package okay.r

import okay.{Choose, Reader, effect, runChoice, given}

object TestRProgram:
  val progs = R.module("progs", """
    pairs <- function() {
      okay_then(okay_perform("choose", c(1, 2)), function(x)
        okay_then(okay_perform("choose", c(10, 20)), function(y)
          okay_done(x + y)))
    }
    priced <- function(sku, qty) okay_then(okay_perform("price_of", sku), function(p) okay_done(p * qty))
  """)

/** remote-foreign against a LIVE R (specs/remote-foreign.md) */
class TestRProgram extends munit.FunSuite {
  import TestRProgram.*

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestR.rscript.isEmpty

  private lazy val r = RSubprocess.start(rscript = TestR.rscript.get, modules = Seq(progs))
  private given okay.Handler[REval] = r.handler
  override def afterAll(): Unit = if TestR.rscript.nonEmpty then r.close()

  private val choose = R.callback[Vector[Double], Double]("choose")(xs => effect[Choose, Double](Choose(xs)))

  test("MULTI-SHOT across a process, from R: an R closure continued twice") {
    val pairs = R.program[Double]("progs::pairs").calling(R.callbacks(choose))()
    assertEquals(runChoice(pairs.program).runWith.toList, List(Right(11.0), Right(21.0), Right(12.0), Right(22.0)))
    pairs.forget.runWith
  }

  test("an R program's operation is a Scala callback under the caller's Reader") {
    val price = R.callback[String, Double]("price_of")(sku => Reader.ask[Map[String, Double]].map(_(sku)))
    val run = R.program[Double]("progs::priced").calling(R.callbacks(price))("tea", 3.0)
    assertEquals(Reader.run(Map("tea" -> 4.0))(run.program).runWith, Right(12.0))
  }
}

object TestRReplay:
  val progs = R.module("rep", """
    pairs <- function() {
      okay_then(okay_perform("choose", c(1, 2)), function(x)
        okay_then(okay_perform("choose", c(10, 20)), function(y)
          okay_done(x + y)))
    }
    slow <- function() { Sys.sleep(30); 1 }
  """)

/** r-supervised-replay: a timeout's respawn in the middle of a multi-shot R
 * program; the continuations the killed R held are re-derived by replay */
class TestRReplay extends munit.FunSuite {
  import TestRReplay.*

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestR.rscript.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(3, "min")

  test("R is replaced between two choices, and every branch still comes back") {
    val r = RSubprocess.start(rscript = TestR.rscript.get, timeoutMillis = Some(3000L), modules = Seq(progs))
    given okay.Handler[REval] = r.handler
    var replaced = false
    val choose = R.callback[Vector[Double], Double]("choose") { xs =>
      if !replaced && xs == Vector(10.0, 20.0) then
        replaced = true
        // a call past the deadline: R is killed and a fresh one takes its place
        val late = r.handler.handle(REval.Call("rep::slow", Vector.empty))
        assert(late.left.exists(_.kind == "timeout"), late.toString)
      effect[Choose, Double](Choose(xs))
    }
    try
      val pairs = R.program[Double]("rep::pairs").calling(R.callbacks(choose))()
      assertEquals(runChoice(pairs.program).runWith.toList, List(Right(11.0), Right(21.0), Right(12.0), Right(22.0)))
      assert(replaced)
      pairs.forget.runWith
    finally r.close()
  }
}
