package okay.r

import okay.given
import okay.codec.Schema

object TestRHandles:
  final case class Data(x: Vector[Double], y: Vector[Double]) derives Schema
  final case class NewData(x: Vector[Double]) derives Schema

/** foreign-object-handles against a LIVE R (specs/foreign-highlevel.md stage 3) */
class TestRHandles extends munit.FunSuite {
  import TestRHandles.*

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestR.rscript.isEmpty

  private lazy val r = RSubprocess.start(rscript = TestR.rscript.get)
  private given okay.Handler[REval] = r.handler
  override def afterAll(): Unit = if TestR.rscript.nonEmpty then r.close()

  test("an lm fit held in R, predicted on new data through the handle") {
    val formula = R.hold("stats::as.formula")("y ~ x").runWith.toOption.get
    assertEquals(formula.rClass, "formula")
    val fit = R.hold("stats::lm")(formula, Data(Vector(1, 2, 3, 4), Vector(3, 5, 7, 9))).runWith.toOption.get
    assertEquals(fit.rClass, "lm")
    val predicted = R.fn[Vector[Double]]("stats::predict")(fit, NewData(Vector(10.0, 0.0))).runWith
    assert(predicted.exists(p => p.size == 2 && math.abs(p(0) - 21) < 1e-9 && math.abs(p(1) - 1) < 1e-9), s"$predicted")
    val coefs = R.fn[Vector[Double]]("stats::coef")(fit).runWith
    assert(coefs.exists(c => math.abs(c(0) - 1) < 1e-9 && math.abs(c(1) - 2) < 1e-9), s"$coefs")
  }

  test("a released ref is refused by name, and releasing twice is harmless") {
    val f = R.hold("stats::as.formula")("y ~ x").runWith.toOption.get
    f.release.runWith
    val after = R.fn[String]("base::class")(f).runWith
    assert(after.left.exists(_.message.contains("not held")), s"$after")
    f.release.runWith
  }
}
