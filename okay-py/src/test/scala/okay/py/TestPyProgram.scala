package okay.py

import okay.{Choose, Reader, effect, runChoice, given}
import okay.agent.Durable

object TestPyProgram:
  val progs = Py.module("progs", """
    import okay

    def pairs():
        return okay.perform("choose", [1, 2]).then(lambda x:
               okay.perform("choose", [10, 20]).then(lambda y:
               okay.done(x + y)))

    def priced(sku, qty):
        return okay.perform("price_of", sku).then(lambda p: okay.done(p * qty))

    def not_a_program():
        return 42
  """)

/** remote-foreign against a LIVE python3 (specs/remote-foreign.md) */
class TestPyProgram extends munit.FunSuite {
  import TestPyProgram.*

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty

  private lazy val w = PySubprocess.start(TestPy.python.get, modules = Seq(progs))
  private given okay.Handler[PyEval] = w.handler
  override def afterAll(): Unit = if TestPy.python.nonEmpty then w.close()

  private val choose = Py.callback[Vector[Long], Long]("choose")(xs => effect[Choose, Long](Choose(xs)))

  test("MULTI-SHOT across a process: Choice continues the same Python continuation twice") {
    val pairs = Py.program[Long]("progs:pairs").calling(Py.callbacks(choose))()
    assertEquals(runChoice(pairs.program).runWith.toList, List(Right(11L), Right(21L), Right(12L), Right(22L)))
    pairs.forget.runWith
  }

  test("a named operation is a callback, run under the caller's Reader") {
    val price = Py.callback[String, Double]("price_of")(sku => Reader.ask[Map[String, Double]].map(_(sku)))
    val run = Py.program[Double]("progs:priced").calling(Py.callbacks(price))("tea", 3L)
    assertEquals(Reader.run(Map("tea" -> 4.0))(run.program).runWith, Right(12.0))
    run.forget.runWith
  }

  test("a forgotten run's continuations are refused by name; a function that is not a program too") {
    val pairs = Py.program[Long]("progs:pairs").calling(Py.callbacks(choose))()
    val first = w.handler.handle(PyEval.Program(pairs.id, "progs:pairs", Vector.empty))
    val k = first match
      case Right(PyNode.Perform(_, _, k, _)) => k
      case other => fail(s"$other")
    pairs.forget.runWith
    val after = w.handler.handle(PyEval.Continue(pairs.id, k, Right(PyValue.I64(1))))
    assert(after.left.exists(_.message.contains("is not held here")), s"$after")
    val bad = Py.program[Long]("progs:not_a_program").calling(Py.callbacks(choose))()
    assertEquals(runChoice(bad.program).runWith.map(_.left.map(_.kind)), Seq(Left("TypeError")))
  }

  test("Durable journals the walk; a replay answers every node without Python") {
    val j = Durable.MemoryJournal()
    val pairs = Py.program[Long]("progs:pairs").calling(Py.callbacks(choose))()
    val live = runChoice(pairs.program).runWith(using Durable.over[PyEval](w.handler, j)())
    assertEquals(j.all.map(_.op).distinct, Vector("program:progs:pairs", "continue"))
    assertEquals(runChoice(pairs.program).runWith(using Durable.replayingOver[PyEval](j)), live)
  }
}
