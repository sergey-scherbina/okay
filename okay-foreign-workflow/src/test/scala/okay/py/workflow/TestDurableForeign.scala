package okay.py.workflow

import okay.{Choose, effect, runChoice, given}
import okay.agent.Durable
import okay.py.{Foreign, ForeignEval, ForeignWorker, Reliable, TestPy}

/**
 * foreign-workflow stage 3: a durable foreign PROGRAM surviving a crash of
 * the HOST, not only of the far side. The first host dies in the middle of
 * a multi-shot program, and its Python worker dies with it. A fresh host,
 * with a fresh worker, is given the same journal: `Durable` answers the
 * steps that happened from it, `SupervisedWorker.witness` rebuilds the
 * continuations from those answers, and the first live step re-derives
 * them on the new far side.
 */
class TestDurableForeign extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty

  final class HostCrash extends RuntimeException("the host died")

  private def fresh(): ForeignWorker = ForeignWorker.start(TestPy.python.get, modules = Seq(Reliable.conf))

  /** the first host: dies at the second choice of the first branch */
  private def firstHost(journal: Durable.Journal) =
    val sup = ForeignWorker.supervised(fresh())
    var crash = true
    val choose = Foreign.callback[Vector[Long], Long]("choose") { xs =>
      if crash && xs == Vector(10L, 20L) then
        crash = false
        throw HostCrash()
      effect[Choose, Long](Choose(xs))
    }
    val run = Foreign.program[Long]("rel:pairs").calling(Foreign.callbacks(choose))()
    intercept[HostCrash](runChoice(run.program).runWith(using
      Durable.over[ForeignEval](sup.handler, journal)(replayed = sup.witness))): Unit
    sup.close()      // the host's children die with it
    run

  test("a HOST crash mid multi-shot program: a fresh host on the same journal finishes every branch") {
    val journal = Durable.MemoryJournal()
    val run = firstHost(journal)
    // the resumed host keeps the run (its id is in the fingerprint Durable
    // checks) with the rest of its state
    val sup = ForeignWorker.supervised(fresh())
    try
      val got = runChoice(run.program).runWith(using
        Durable.over[ForeignEval](sup.handler, journal)(replayed = sup.witness))
      assertEquals(got.toList, List(Right(11L), Right(21L), Right(12L), Right(22L)))
    finally sup.close()
  }

  test("without the witness the resumed host is refused by name, never answered wrongly") {
    val journal = Durable.MemoryJournal()
    val run = firstHost(journal)
    val sup = ForeignWorker.supervised(fresh())
    try
      val got = runChoice(run.program).runWith(using Durable.over[ForeignEval](sup.handler, journal)())
      assert(got.exists(_.left.exists(c => c.kind == "LookupError" && c.message.contains("is not held"))), got.toString)
      assert(!got.contains(Right(11L)), "no branch may be answered from a continuation the host could not know")
    finally sup.close()
  }
