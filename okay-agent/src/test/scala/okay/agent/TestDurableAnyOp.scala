package okay.agent

import okay.Handler
import okay.codec.Json

/**
 * The seam proved by a SECOND operation type — which is the only way
 * a generalisation is proved at all (durable-any-operation).
 *
 * `Tool` cannot show it: its answer is already a `String`, so its
 * instance is the identity everywhere and every path through the
 * journal would look right whether or not the codec were consulted.
 * `Calc` answers an `Int`, so an answer that comes back from the
 * journal has to be decoded, and one that goes in has to be encoded —
 * and the test can tell the difference.
 *
 * It also stands for the case the backlog entry raised: an operation
 * whose arguments are expensive to render. `Calc` fingerprints what
 * it was ASKED, not what it will answer, which is the same freedom an
 * R instance needs for a million-row frame.
 */
class TestDurableAnyOp extends munit.FunSuite {

  enum Calc[+A]:
    case Add(x: Int, y: Int) extends Calc[Int]

  given Journalled[Calc] with
    def name[A](op: Calc[A]): String = op match
      case Calc.Add(_, _) => "add"
    def fingerprint[A](op: Calc[A]): String = op match
      case Calc.Add(x, y) => s"add($x,$y)"
    def withKey[A](op: Calc[A], key: String): Calc[A] = op   // nowhere to put one
    // DELIBERATELY NOT `toString`. If the codec were `toString`, a
    // framework that journalled the answer itself instead of asking
    // the instance would pass every test below. The `=` prefix is
    // there so the journal's contents prove whose codec ran.
    def perform[A](op: Calc[A], inner: Handler[Calc]): (A, String) = op match
      case Calc.Add(x, y) =>
        val answer: Int = inner.handle(Calc.Add(x, y))
        (answer, s"=$answer")
    def decode[A](op: Calc[A], written: String): A = op match
      case Calc.Add(_, _) => written.stripPrefix("=").toInt

  /** counts what actually reached the world */
  private def adder(ran: java.util.concurrent.atomic.AtomicInteger): Handler[Calc] =
    new Handler[Calc]:
      def handle[A](op: Calc[A]): A = op match
        case Calc.Add(x, y) => ran.incrementAndGet(); x + y

  test("an operation whose answer is not a String: executed once, journalled, decoded back") {
    val j = Durable.MemoryJournal()
    val ran = java.util.concurrent.atomic.AtomicInteger(0)

    val first = Durable.over[Calc](adder(ran), j)()
    assertEquals(first.handle(Calc.Add(2, 3)), 5)
    assertEquals(ran.get, 1)

    val e = j.all.head
    assertEquals(e.op, "add")
    assertEquals(e.fingerprint, "add(2,3)")
    assertEquals(e.answer, Some("=5"), "the journal stores what the INSTANCE wrote, not the answer")

    // a second handler over the SAME journal: the answer comes back
    // decoded, as an Int, and the world is not touched again
    val again = Durable.over[Calc](adder(ran), j)()
    assertEquals(again.handle(Calc.Add(2, 3)), 5)
    assertEquals(ran.get, 1, "recovery must not re-execute")
  }

  test("replay answers from the journal and never reaches the inner handler") {
    val j = Durable.MemoryJournal()
    val ran = java.util.concurrent.atomic.AtomicInteger(0)
    assertEquals(Durable.over[Calc](adder(ran), j)().handle(Calc.Add(7, 8)), 15)

    val replayed = Durable.replayingOver[Calc](j).handle(Calc.Add(7, 8))
    assertEquals(replayed, 15)
    assertEquals(ran.get, 1, "replay touches no world")
  }

  test("a changed program is caught by the fingerprint, not answered wrongly") {
    val j = Durable.MemoryJournal()
    val ran = java.util.concurrent.atomic.AtomicInteger(0)
    assertEquals(Durable.over[Calc](adder(ran), j)().handle(Calc.Add(2, 3)), 5)

    // the same position, a different question
    val _ = intercept[Durable.Drift](Durable.over[Calc](adder(ran), j)().handle(Calc.Add(2, 4)))
    assertEquals(ran.get, 1, "a drifting program must not execute either")
  }

  test("the crash window: an answerless entry obeys the policy, per operation") {
    val j = Durable.MemoryJournal()
    val ran = java.util.concurrent.atomic.AtomicInteger(0)
    // intent written, answer never — the process died in between
    j.append(Durable.Entry(0, "add", "add(2,3)", "add-0-1", None))

    // Fail refuses rather than repeat
    val _ = intercept[Durable.Unresolved](Durable.over[Calc](adder(ran), j)().handle(Calc.Add(2, 3)))
    assertEquals(ran.get, 0)

    // Redo says it is safe, and the journal is settled by the re-run
    val redone = Durable.over[Calc](adder(ran), j)(policy = _ => Durable.OnRepeat.Redo)
    assertEquals(redone.handle(Calc.Add(2, 3)), 5)
    assertEquals(ran.get, 1)
    assertEquals(j.all.head.answer, Some("=5"))
  }

  test("Reconcile answers in the journal's written form and is decoded for the program") {
    val j = Durable.MemoryJournal()
    val ran = java.util.concurrent.atomic.AtomicInteger(0)
    j.append(Durable.Entry(0, "add", "add(2,3)", "add-0-1", None))

    val h = Durable.over[Calc](adder(ran), j)(
      policy = _ => Durable.OnRepeat.Reconcile,
      reconcile = [X] => (_: Calc[X], _: String) => Some("=5"))
    assertEquals(h.handle(Calc.Add(2, 3)), 5, "the written form is decoded on the way out")
    assertEquals(ran.get, 0, "Reconcile never re-executes")
    assertEquals(j.all.head.answer, Some("=5"), "and it settles the journal")
  }

  test("a parked question shows the fingerprint when the instance offers nothing better") {
    val j = Durable.MemoryJournal()
    val ran = java.util.concurrent.atomic.AtomicInteger(0)
    val h = Durable.over[Calc](adder(ran), j)(policy = _ => Durable.OnRepeat.Await)
    val awaiting = intercept[Durable.Awaiting](h.handle(Calc.Add(2, 3)))
    assertEquals(awaiting.op, "add")
    assertEquals(awaiting.args, Json.JStr("add(2,3)"), "the default `asked` is the fingerprint")
    assertEquals(ran.get, 0, "asking touches no world")
    assertEquals(j.all.head.answer, None, "the question is recorded, unanswered")
  }
}
