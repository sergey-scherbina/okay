package okay.r

import okay.{State, given}

object TestRModule:
  val scoring = R.module("scoring", """
    trimmed <- function(xs) mean(xs, trim = 0.25)
    twice <- function(x) okay_call("inc", x) + okay_call("inc", x)
    fit <- function(x, y) lm(y ~ x)
  """)
  val broken = R.module("broken", "f <- function( {")

/** foreign-inline-modules against a LIVE R (specs/foreign-highlevel.md stage 4) */
class TestRModule extends munit.FunSuite {
  import TestRModule.*

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestR.rscript.isEmpty

  private lazy val r = RSubprocess.start(rscript = TestR.rscript.get, modules = Seq(scoring))
  private given okay.Handler[REval] = r.handler
  override def afterAll(): Unit = if TestR.rscript.nonEmpty then r.close()

  final case class XY(x: Vector[Double], y: Vector[Double]) derives okay.codec.Schema

  test("a module beside the Scala: a function, a callback from it, an object held from it") {
    assertEquals(scoring.fn[Double]("trimmed")(Vector(1.0, 2.0, 3.0, 100.0)).runWith, Right(2.5))
    val inc = R.callback[Long, Long]("inc")(x => State.modify[Int](_ + 1).map(_ => x + 1))
    assertEquals(State.handle(0)(scoring.fn[Long]("twice").calling(R.callbacks(inc))(5L)).runWith, (2, Right(12L)))
    val model = scoring.hold("fit")(Vector(1.0, 2.0, 3.0), Vector(2.0, 4.0, 6.0)).runWith
    assertEquals(model.map(_.rClass), Right("lm"))
  }

  test("a module's functions stay in the module, not the global environment") {
    val leaked = R.fn[Double]("trimmed")(Vector(1.0)).runWith
    assert(leaked.left.exists(_.message.contains("no function named 'trimmed'")), s"$leaked")
  }

  test("a module that does not parse refuses at start, naming itself") {
    val e = intercept[IllegalStateException](RSubprocess.start(rscript = TestR.rscript.get, modules = Seq(broken)))
    assert(e.getMessage.contains("module 'broken' did not load"), e.getMessage)
  }
}

/** the literal-only rule, checked by the compiler (default gate) */
class TestRModuleRule extends munit.FunSuite {
  test("a module's source must be a compile-time constant") {
    val refused = compileErrors("""
      val body = "f <- function() 1"
      R.module("m", body)
    """)
    assert(refused.contains("expected a constant value"), refused)
  }
}
