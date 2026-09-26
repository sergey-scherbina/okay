package okay.foreign.workflow

import okay.{!, +, Delim, Pure, Wf}
import okay.Direct.*
import okay.codec.Schema
import okay.persist.{Dialogue, MemoryStore}
import okay.foreign.{Foreign, ForeignEval, ForeignWorker, TestPy}
import scala.language.implicitConversions

object Shop:
  val module = Foreign.module("shop", """
    calls = {}

    def count(name):
        calls[name] = calls.get(name, 0) + 1

    def price(sku):
        count("price")
        return {"tea": 4.0, "cake": 2.5}[sku]

    def total(p, n):
        count("total")
        return p * n

    def label(p):
        count("label")
        return "costs %s" % p

    def counted(name):
        return calls.get(name, 0)
  """)

  // no margin: the docs quote these lines
  def order(sku: String)(using w: Wf.Asks[ForeignCall, String, String, Pure]): String ! Delim + Pure = direct:
    val price = !ForeignActivity.call[Double]("shop:price")(sku)
    price match
      case Left(c) => s"no price: ${c.kind}"
      case Right(p) =>
        val total = !ForeignActivity.call[Double]("shop:total")(p, 3L)
        total.fold(c => s"no total: ${c.kind}", t => s"total $t")

/** foreign-workflow stage 1: foreign calls as the ACTIVITIES of a durable
 * workflow written in do-notation, against a live Python worker */
class TestForeignActivity extends munit.FunSuite:
  import Shop.*
  import okay.given
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty

  given Schema[Wf.SysA] = Schema.derived
  given Schema[Wf.Ans[String]] = Schema.derived
  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_700_000_000_000L, id = "id-1", dice = 0.25)

  private lazy val worker = ForeignWorker.start(TestPy.python.get, modules = Seq(module))
  private given okay.Handler[ForeignEval] = worker.handler
  override def afterAll(): Unit = if TestPy.python.nonEmpty then worker.close()

  private def counted(name: String): Long =
    Foreign.fn[Long]("shop:counted")(name).runWith.toOption.get

  test("a workflow in do-notation whose activities are Python calls, journalled in its topic") {
    val topic = MemoryStore().topic("orders")
    val run = Dialogue.workflow[ForeignCall, String, String, Pure](topic, "order-1", "order/1")(order("tea"))
      .runWorkflowIn(q => ForeignActivity.oracle(q))
    assertEquals(run.runWith, Right("total 12.0"))
  }

  test("CRASH-RESUME: the host dies after the first activity; the resumed run does not call it again") {
    val topic = MemoryStore().topic("orders")
    val before = (counted("price"), counted("total"))
    val crashing = Dialogue.workflow[ForeignCall, String, String, Pure](topic, "order-2", "order/1")(order("cake"))
      .runWorkflowIn(q => if q.address == "shop:total" then throw IllegalStateException("the host died") else ForeignActivity.oracle(q))
    intercept[IllegalStateException](crashing.runWith): Unit
    // a restarted process opens the same dialogue on the same topic
    val resumed = Dialogue.workflow[ForeignCall, String, String, Pure](topic, "order-2", "order/1")(order("cake"))
      .runWorkflowIn(q => ForeignActivity.oracle(q))
    assertEquals(resumed.runWith, Right("total 7.5"))
    assertEquals((counted("price"), counted("total")), (before._1 + 1, before._2 + 1),
      "each activity reached Python ONCE: the first came back from the journal")
  }

  test("a far-side failure is a JOURNALLED answer: the workflow branches on it, and a replay does not call again") {
    val topic = MemoryStore().topic("orders")
    val first = Dialogue.workflow[ForeignCall, String, String, Pure](topic, "order-3", "order/1")(order("coffee"))
      .runWorkflowIn(q => ForeignActivity.oracle(q))
    assertEquals(first.runWith, Right("no price: KeyError"))
    val before = counted("price")
    val again = Dialogue.workflow[ForeignCall, String, String, Pure](topic, "order-3", "order/1")(order("coffee"))
      .runWorkflowIn(q => ForeignActivity.oracle(q))
    assertEquals(again.runWith, Right("no price: KeyError"))
    assertEquals(counted("price"), before, "the failure was read back from the journal")
  }

  test("an answer of the wrong shape is a Left from the Schema, not a crash of the workflow") {
    def labelled(using w: Wf.Asks[ForeignCall, String, String, Pure]): String ! Delim + Pure = direct:
      val n = !ForeignActivity.call[Long]("shop:label")(4.0)
      n.fold(c => s"refused: ${c.kind}", v => s"got $v")
    val topic = MemoryStore().topic("orders")
    val run = Dialogue.workflow[ForeignCall, String, String, Pure](topic, "order-4", "order/1")(labelled)
      .runWorkflowIn(q => ForeignActivity.oracle(q))
    assert(run.runWith.exists(_.startsWith("refused:")), run.runWith.toString)
  }
