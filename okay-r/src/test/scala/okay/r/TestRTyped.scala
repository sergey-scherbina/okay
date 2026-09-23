package okay.r

import okay.given
import TestRCodec.*

/** foreign-typed-calls against a LIVE R (specs/foreign-highlevel.md stage 2) */
class TestRTyped extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestR.rscript.isEmpty

  private lazy val r = RSubprocess.start(rscript = TestR.rscript.get)
  private given okay.Handler[REval] = r.handler
  override def afterAll(): Unit = if TestR.rscript.nonEmpty then r.close()

  final case class Patch(qty: Int) derives okay.codec.Schema

  test("a case class to R as a named list, and back through identity") {
    val o = Order("kyiv-7", 2, 1.5, Some("gift"))
    assertEquals(R.fn[Order]("base::identity")(o).runWith, Right(o))
  }

  test("R computes over the record: modifyList patches a field") {
    val patched = R.fn[Order]("utils::modifyList")(Order("kyiv-7", 2, 1.5), Patch(5)).runWith
    assertEquals(patched, Right(Order("kyiv-7", 5, 1.5)))
  }

  test("a scalar from R's length-1 vector, and a sum both ways") {
    assertEquals(R.fn[Double]("stats::median")(Vector(3.0, 1.0, 2.0)).runWith, Right(2.0))
    assertEquals(R.fn[Shape]("base::identity")(Shape.Rect(2, 3): Shape).runWith, Right(Shape.Rect(2, 3)))
  }

  test("a Long past 32 bits survives R, which has no 64-bit integer") {
    assertEquals(R.fn[Long]("base::identity")(3000000000L).runWith, Right(3000000000L))
    assertEquals(R.fn[Long]("base::identity")(Long.MaxValue).runWith, Right(Long.MaxValue))
  }

  test("a wrong-shaped answer is a Decode condition naming the field") {
    assertEquals(R.fn[Order]("base::identity")(Patch(5)).runWith, Left(Condition("Decode", "$sku: missing")))
  }
}
