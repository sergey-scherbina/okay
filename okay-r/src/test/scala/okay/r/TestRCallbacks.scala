package okay.r

import okay.{!, Reader, State}
import okay.given
import okay.agent.Durable

object TestRCallbacks:
  /** R functions that call back into okay, defined by `source`-ing this
   * file (the shim addresses functions by name; it never evals a string) */
  val script: String =
    """twice <- function(x) okay_call("inc", x) + okay_call("inc", x)
      |minimise <- function(lo, hi) optimize(function(x) okay_call("objective", x), c(lo, hi), tol = 1e-9)$minimum
      |nested <- function(x) okay_call("via_r", x) + 1
      |square <- function(x) x * x
      |catches <- function() tryCatch(okay_call("inc", "not a number"), okay_error = function(e) e$kind)
      |unoffered <- function() okay_call("nope", 1)
      |""".stripMargin

/** foreign-callbacks against a LIVE R (specs/foreign-highlevel.md stage 7) */
class TestRCallbacks extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestR.rscript.isEmpty

  private lazy val r =
    // java.io.tmpdir: the docker shim mounts it, so the container sees the file
    val file = java.nio.file.Files.createTempFile(
      java.nio.file.Paths.get(System.getProperty("java.io.tmpdir")), "okay-r-cb", ".R")
    java.nio.file.Files.writeString(file, TestRCallbacks.script): Unit
    val engine = RSubprocess.start(rscript = TestR.rscript.get)
    val _ = engine.handler.handle(REval.Call("base::source", Vector(RValue.Str(file.toString))))
    engine
  private given okay.Handler[REval] = r.handler
  override def afterAll(): Unit = if TestR.rscript.nonEmpty then r.close()

  private val inc = R.callback[Long, Long]("inc")(x => State.modify[Int](_ + 1).map(_ => x + 1))

  test("R's optimize minimises an objective whose target comes from okay's Reader") {
    val objective = R.callback[Double, Double]("objective")(x => Reader.ask[Double].map(t => (x - t) * (x - t)))
    val fit = R.fn[Double]("minimise").calling(R.callbacks(objective))(-10.0, 10.0)
    val best = Reader.run(3.25)(fit).runWith
    assert(best.exists(b => math.abs(b - 3.25) < 1e-4), s"$best")
  }

  test("a callback's State is the caller's: two calls from R, counted in okay") {
    assertEquals(State.handle(0)(R.fn[Long]("twice").calling(R.callbacks(inc))(5L)).runWith, (2, Right(12L)))
  }

  test("a callback that calls R again, on the same process, is answered") {
    val via = R.callback[Long, Long]("via_r")(x => R.fn[Long]("square")(x).map(_.getOrElse(-1L)))
    val prog: Either[Condition, Long] ! REval = R.fn[Long]("nested").calling(R.callbacks(via))(7L)
    assertEquals(prog.runWith, Right(50L))
  }

  test("a callback that fails in okay is an okay_error condition in R, which R may catch") {
    assertEquals(State.handle(0)(R.fn[String]("catches").calling(R.callbacks(inc))()).runWith,
      (0, Right("Decode")))
  }

  test("a callback this call did not offer is refused by name, in R") {
    val got = State.handle(0)(R.fn[Long]("unoffered").calling(R.callbacks(inc))()).runWith._2
    assert(got.left.exists(_.message.contains("'nope'")), s"$got")
  }

  test("Durable journals the dialogue; a replay answers every step without R") {
    val j = Durable.MemoryJournal()
    val prog = R.fn[Long]("twice").calling(R.callbacks(inc))(5L)
    assertEquals(State.handle(0)(prog).runWith(using Durable.over[REval](r.handler, j)()), (2, Right(12L)))
    assertEquals(j.all.map(_.op), Vector("program:twice", "continue", "continue"))
    assertEquals(State.handle(0)(prog).runWith(using Durable.replayingOver[REval](j)), (2, Right(12L)))
  }
}
