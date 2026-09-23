package okay.r

import okay.given

object TestRFacade:
  val demo = R.module("rfacadedemo", """
    trimmed <- function(xs, trim = 0.25) mean(xs, trim = trim)
    scale2 <- function(x, by) x * by
    t.like <- function(x) x
    .hidden <- function() 1
  """)

/** foreign-module-trait against a LIVE R (specs/foreign-highlevel.md stage 5) */
class TestRFacade extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestR.rscript.isEmpty

  private lazy val r = RSubprocess.start(rscript = TestR.rscript.get, modules = Seq(TestRFacade.demo))
  private given okay.Handler[REval] = r.handler
  override def afterAll(): Unit = if TestR.rscript.nonEmpty then r.close()

  private val rel = "src/test/scala/okay/r/golden/RFacadeDemo.scala"
  private def goldenPath =
    val here = java.nio.file.Paths.get(rel)
    if java.nio.file.Files.exists(here.getParent) then here else java.nio.file.Paths.get("okay-r", rel)

  test("the checked-in facade is what the generator writes today") {
    val sigs = RFacade.describe("rfacadedemo").runWith.toOption.get
    val now = RFacade.render("RFacadeDemo", "okay.r.golden", "rfacadedemo", sigs)
    val path = goldenPath
    val was = if java.nio.file.Files.exists(path) then java.nio.file.Files.readString(path) else ""
    if was != now then
      // the regeneration, left beside the golden for a human to diff and adopt
      java.nio.file.Files.writeString(path.resolveSibling("RFacadeDemo.scala.new"), now): Unit
    assertEquals(now, was)
  }

  test("the generated facade calls R: names and arity fixed, types the caller's") {
    assertEquals(golden.RFacadeDemo.trimmed[Double, Vector[Double]](Vector(1.0, 2.0, 3.0, 100.0)).runWith, Right(2.5))
    assertEquals(golden.RFacadeDemo.scale2[Double, Double, Double](3.0, 2.0).runWith, Right(6.0))
    assertEquals(golden.RFacadeDemo.t_like[String, String]("same").runWith, Right("same"))
  }
}
