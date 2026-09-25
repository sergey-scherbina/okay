package okay.kernel.docs

// docs/modules/okay-kernel.md's example, verbatim (TestDocSnippets pins it)

import okay.kernel.*

trait Clock { def now: Long }
val clock: Port[Clock] = Port.one("clock", Version("1.2"))

class SystemClock extends Plugin:
  def id = "system-clock"
  def version = Version("1.0")
  def needs = Vector.empty
  def provides = Vector(Provision.value(clock, "1.2")(_ => new Clock { def now = System.currentTimeMillis }))

class Stamper extends Plugin:
  def id = "stamper"
  def version = Version("1.0")
  def needs = Vector(Need.of(clock, "^1.2"))
  def provides = Vector.empty

class TestDocExamplesKernel extends munit.FunSuite:
  test("the page's two plugins plan in the order it says, and start") {
    val plan = Kernel.plan(Seq(SystemClock(), Stamper())).toOption.get
    assertEquals(plan.order.map(_.id), Vector("system-clock", "stamper"))
    val (r, close) = okay.Resource.open(Kernel.assemble(Seq(SystemClock(), Stamper())))
    assert(r.one(clock).now > 0)
    close()
  }
