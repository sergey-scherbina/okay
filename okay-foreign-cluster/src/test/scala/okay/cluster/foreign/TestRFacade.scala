package okay.cluster.foreign

import okay.r.{R, TestR}

object RFacadeMod:
  val mod = R.module("rfacade", """
    echo <- function(rec) rec
    boom <- function(rec) stop("nope")
    fecho <- function(frame) frame
    fboom <- function(frame) stop("nope")
  """)

/** the conformance body over a REAL R (Live) */
class TestRFacade extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestR.rscript.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")

  given Calls[okay.r.RModule] = Calls.r(TestR.rscript.getOrElse("Rscript"))
  given Speaks[okay.r.RModule] = Speaks.r(TestR.rscript.getOrElse("Rscript"))
  given Frames[okay.r.RModule] = Frames.r(TestR.rscript.getOrElse("Rscript"))

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
