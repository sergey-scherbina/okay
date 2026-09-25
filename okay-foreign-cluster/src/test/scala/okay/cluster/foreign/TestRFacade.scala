package okay.cluster.foreign

import okay.r.{R, TestR}

object RFacadeMod:
  val mod = R.module("rfacade", """
    echo <- function(rec) rec
    boom <- function(rec) stop("nope")
    fecho <- function(frame) frame
    fboom <- function(frame) stop("nope")
    priced <- function(order) okay_then(okay_perform("price_of", order$sku), function(p) okay_done(p * order$qty))
    pairs <- function(x) okay_then(okay_perform("choose", c(1, 2)), function(a)
      okay_then(okay_perform("choose", c(10, 20)), function(b) okay_done(a + b)))
  """)

/** the conformance body over a REAL R (Live) */
class TestRFacade extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestR.rscript.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")

  given Calls[okay.r.RModule] = Calls.r(TestR.rscript.getOrElse("Rscript"))
  given Speaks[okay.r.RModule] = Speaks.r(TestR.rscript.getOrElse("Rscript"))
  given Frames[okay.r.RModule] = Frames.r(TestR.rscript.getOrElse("Rscript"))
  given Programs[okay.r.RModule] = Programs.r(TestR.rscript.getOrElse("Rscript"))

  test("Programs over Rscript: a callback under a Reader, and a continuation resumed twice") {
    FacadeConformance.programs(RFacadeMod.mod, "priced", "pairs")
  }

  test("Streams over Rscript: 20 000 rows through fecho in frames of 4 096, every row back in order") {
    FacadeConformance.streams(RFacadeMod.mod, "fecho", 20000, 4096)
  }

  test("Frames over Rscript: a table there and back, the empty one too, boom refused") {
    FacadeConformance.frames(RFacadeMod.mod, "fecho", "fboom")
  }

  test("Calls over Rscript: echo, boom, a missing function") {
    FacadeConformance.calls(RFacadeMod.mod, "echo", "boom")
  }

  test("Speaks over Rscript: pipes, frames as this R crosses them, multi-shot") {
    val r = FacadeConformance.speaks(RFacadeMod.mod, "r")
    assertEquals((r.link, r.programs), ("pipes", "multi-shot"))
  }
