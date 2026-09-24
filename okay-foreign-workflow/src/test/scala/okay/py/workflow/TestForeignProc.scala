package okay.py.workflow

import okay.{!, +, Delim, Proc, Pure, Wf}
import okay.Direct.*
import okay.Optic.arrows.*
import okay.codec.Schema
import okay.persist.{Dialogue, MemoryStore}
import okay.py.{Condition, ForeignEval, ForeignWorker, PyValue, TestPy}
import scala.language.implicitConversions

object ShopProc:
  val A = Proc.procArrow[ForeignProc.Sig]

  // no margin: the docs quote these lines
  /** the order as a TERM: its leaves, the far functions, are known before it runs */
  val order: Wf.Proc[ForeignCall, String, String, String] =
    ForeignProc.call[String, Double]("shop:price") >>>
      A.arr((e: Either[Condition, Double]) => e.fold(c => Right(s"no price: ${c.kind}"), p => Left((p, 3L)))) >>>
      A.left(ForeignProc.call2[Double, Long, Double]("shop:total") >>>
        A.arr((t: Either[Condition, Double]) => t.fold(c => s"no total: ${c.kind}", v => s"total $v"))) >>>
      A.arr((e: Either[String, String]) => e.merge)

  /** the order in PROC-NOTATION: each far function a helper, drawn by its name */
  def price(sku: String): Wf.Question[ForeignCall, String, String] = ForeignProc.ask("shop:price")(PyValue.Str(sku))
  def total(p: Double): Wf.Question[ForeignCall, String, String] =
    ForeignProc.ask("shop:total")(PyValue.F64(p), PyValue.I64(3L))

  val block: Wf.Proc[ForeignCall, String, String, String] =
    Proc.direct[ForeignProc.Sig, String, String]: sku =>
      val first = ForeignProc.decode[Double](!price(sku))
      if first.isRight then
        val second = ForeignProc.decode[Double](!total(first.getOrElse(0.0)))
        second.fold(c => s"no total: ${c.kind}", t => s"total $t")
      else first.fold(c => s"no price: ${c.kind}", _ => "")

/** foreign-workflow stage 2: foreign calls as leaves of a static Proc, in
 * both spellings, on the same journal as the do-notation workflow */
class TestForeignProc extends munit.FunSuite:
  import ShopProc.*
  import okay.given
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty

  given Schema[Wf.SysA] = Schema.derived
  given Schema[Wf.Ans[String]] = Schema.derived
  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_700_000_000_000L, id = "id-1", dice = 0.25)

  private lazy val worker = ForeignWorker.start(TestPy.python.get, modules = Seq(Shop.module))
  private given okay.Handler[ForeignEval] = worker.handler
  override def afterAll(): Unit = if TestPy.python.nonEmpty then worker.close()

  private def wf(topic: okay.persist.Topic, id: String)
                (body: Wf.Asks[ForeignCall, String, String, Pure] ?=> String ! Delim + Pure) =
    Dialogue.workflow[ForeignCall, String, String, Pure](topic, id, "order/1")(body)

  test("the far functions a procedure may call are known before it runs: named leaves, and the picture") {
    assertEquals(order.leaves.map(_.name), Vector("shop:price", "shop:total"))
    assertEquals(block.leaves.map(_.name), Vector("price", "total"))
    assert(order.mermaid().contains("shop:total"), order.mermaid())
  }

  test("the term and the proc-notation block run through the same oracle and journal") {
    val t = MemoryStore().topic("orders")
    assertEquals(wf(t, "p-1")(Wf.Proc.program(order)("tea")).runWorkflowIn(q => ForeignActivity.oracle(q)).runWith,
      Right("total 12.0"))
    assertEquals(wf(t, "p-2")(Wf.Proc.program(block)("cake")).runWorkflowIn(q => ForeignActivity.oracle(q)).runWith,
      Right("total 7.5"))
    assertEquals(wf(t, "p-3")(Wf.Proc.program(order)("coffee")).runWorkflowIn(q => ForeignActivity.oracle(q)).runWith,
      Right("no price: KeyError"))
  }

  test("ONE TOPIC, THREE FRONT ENDS: do-notation, term and block write the same journal") {
    def journalOf(body: Wf.Asks[ForeignCall, String, String, Pure] ?=> String ! Delim + Pure) =
      val t = MemoryStore().topic("j")
      val _ = wf(t, "o")(body).runWorkflowIn(q => ForeignActivity.oracle(q)).runWith
      wf(t, "o")(body).recovered.answers
    val monadic = journalOf(Shop.order("tea"))
    assertEquals(journalOf(Wf.Proc.program(order)("tea")), monadic)
    assertEquals(journalOf(Wf.Proc.program(block)("tea")), monadic)
  }

  test("walk reads a do-notation run's journal and stands where that run finished, calling nothing") {
    val t = MemoryStore().topic("w")
    val _ = wf(t, "o")(Shop.order("tea")).runWorkflowIn(q => ForeignActivity.oracle(q)).runWith
    val journal = wf(t, "o")(Shop.order("tea")).recovered.answers
    assertEquals(Wf.Proc.walk(order)("tea", journal), Right(Wf.Proc.Standing.Done("total 12.0")))
  }
