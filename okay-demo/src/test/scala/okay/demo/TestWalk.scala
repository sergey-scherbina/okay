package okay.demo

import okay.Handler
import okay.given
import okay.agent.{Conversation, Durable, Handlers, Tool}
import okay.agent.Conversation.{Intake, Outcome, Reply, Say}
import okay.frame.{Frame, Source}
import okay.intent.{Router, Temporal}

/**
 * One message, one exchange, one thing produced — walked end to end
 * (specs/conversation.md).
 *
 * Written because a consumer said the thing this repository could not
 * see about itself: their two defects that day lived BETWEEN two
 * correct code paths, with 237 unit tests green through both. One
 * showed a person their OWN address back, because a two-argument
 * contact lookup was called with a fixed direction. The other told
 * the person who had asked, waited and been accepted precisely
 * nothing, because "they got your contact" was a claim in a comment
 * rather than a call. Neither is inside any single seam.
 *
 * So this test asserts what a CALLER ends up with, not that each
 * piece works. Everything below the surface is real: the router's own
 * tiers, the frame that comes out of them, the suspension in a
 * journal, a restart between two answers, a default nobody typed, and
 * a read-back. The only fake is the person.
 */
class TestWalk extends munit.FunSuite {

  private val today = Temporal.Date(2026, 9, 5) // a Saturday
  private val slots = IntentRouter.Meeting(today)

  /** what the exchange EXISTS to produce — and the two things the
   * consumer's defects got wrong: a real value, and who is told */
  private case class Booking(iso: String, participant: String, toldTo: String)

  /**
   * The caller's act. It takes the frame, THE DESCRIPTORS THAT BUILT
   * IT, and the two people — and the direction matters: the
   * confirmation goes to whoever asked, and names the other one.
   *
   * The descriptors are a parameter and not a capture, which this
   * test learned the hard way. The first version closed over the
   * `slots` value at the top of the file while the frame came from
   * the one rebuilt after the restart; `valueOf` matches a slot by
   * IDENTITY, so it found nothing and the walk reported an exchange
   * that completed and produced NOTHING — the exact shape it was
   * written to catch, arriving from the test's own mistake first.
   * One descriptor value per exchange, passed with the frame.
   */
  private def act(f: Frame[String], from: IntentRouter.Meeting,
                  asker: String, other: String): Option[Booking] =
    f.valueOf(from.when).map(w => Booking(w.date.iso, other, asker))

  // ── the harness: a journal, a program, and a person who answers ──

  private def drive(in: Intake[String], j: Durable.Journal, opening: String)
  : Either[Durable.Awaiting, Outcome[String]] =
    given Handler[Tool] = Durable.tools(
      Handlers.tools(Map(Conversation.AskOp ->
        (_ => fail("the inner handler answered a question meant for a person")))),
      j)(Conversation.policy())
    try Right(Conversation.intake(in, opening).runWith)
    catch case a: Durable.Awaiting => Left(a)

  private def answer(j: Durable.Journal, r: Reply): Unit =
    Conversation.answer(j, Conversation.pending(j).map(_._1)
      .getOrElse(fail("nothing is pending")), r)

  private def pending(j: Durable.Journal): Say =
    Conversation.pending(j).map(_._2).getOrElse(fail("nothing is pending"))

  test("a message becomes a booking: classify, ask, restart, answer, confirm, act") {
    val message = "Hi — can we get together to go over the roadmap?"

    // 1. the router, with its own tiers and no network
    val intent = IntentRouter.route(message, slots) match
      case Router.Action.Ask(i, slot, question, remaining) =>
        assertEquals(i, "MeetingProposal")
        assertEquals(slot, "when")   // the message proposes, but says no day
        assertEquals(remaining, 1)
        i
      case other => fail(s"expected one open question, got $other")

    // 2. the frame that class needs, with a default NOBODY TYPED for
    //    the optional slot — most people will not say who
    val frame = slots.frameFor(intent).fillFrom(message)
      .assume(slots.who, "the design team")
    assertEquals(frame.assumed, Vector("who"))

    val in = Intake(frame,
      f => s"So: ${f.filled.getOrElse("when", "?")} with ${f.filled.getOrElse("who", "?")}. Right?")

    // 3. the exchange parks on the question, in a journal
    val j = Durable.MemoryJournal()
    assert(drive(in, j, message).isLeft, "it did not park on the question")
    pending(j) match
      case Say.Ask(_, "when", q, remaining) =>
        assert(q.contains("When"), q)
        assertEquals(remaining, Some(1))
      case other => fail(s"expected the when question, got $other")

    // 4. THE PROCESS DIES HERE. Nothing is carried over but the
    //    journal — no closure, no frame in memory, and the objects
    //    below are rebuilt from scratch the way a restarted service
    //    would rebuild them.
    val afterRestart = IntentRouter.Meeting(today)
    val rebuilt = Intake(
      afterRestart.frameFor(intent).fillFrom(message)
        .assume(afterRestart.who, "the design team"),
      f => s"So: ${f.filled.getOrElse("when", "?")} with ${f.filled.getOrElse("who", "?")}. Right?")

    answer(j, Reply.Answer("next Tuesday"))
    assert(drive(rebuilt, j, message).isLeft, "it should now park on the read-back")

    // 5. the read-back shows BOTH what was said and what was assumed,
    //    which is what lets a person correct the assumption
    pending(j) match
      case Say.ReadBack(_, values, text) =>
        assertEquals(values.keys.toVector.sorted, Vector("when", "who"))
        assert(text.contains("next Tuesday"), text)
        assert(text.contains("the design team"), text)
      case other => fail(s"expected the read-back, got $other")

    // 6. yes — and the exchange hands back a FRAME, not a map
    answer(j, Reply.Yes)
    val filled = drive(rebuilt, j, message) match
      case Right(Outcome.Filled(f)) => f
      case other => fail(s"expected a filled frame, got $other")

    // 7. and now the part that a per-seam test cannot reach: does the
    //    caller get the thing the whole exchange existed for?
    val booking = act(filled, afterRestart, asker = "ada", other = "grace")
      .getOrElse(fail("the exchange completed and produced NOTHING — " +
        "which is exactly the shape of the defect this test exists for"))

    assertEquals(booking.iso, "2026-09-08")        // the Tuesday after a Saturday
    assertEquals(booking.toldTo, "ada")            // the one who asked
    assertEquals(booking.participant, "grace")     // the OTHER one, not themselves

    // 8. and the provenance survived the whole walk: one answer given,
    //    one assumed, and the frame can still tell them apart
    assertEquals(filled.sourceOf("when"), Some(Source.Said))
    assertEquals(filled.sourceOf("who"), Some(Source.Assumed))
    assertEquals(filled.words, Map("when" -> "next Tuesday"))
    assertEquals(filled.assumed, Vector("who"))
  }

  test("a message that carries its own answer produces the booking with no questions at all") {
    // the same walk with nothing to ask: the router fills `when` from
    // the message itself, so the exchange never starts
    IntentRouter.route("Can we meet next Tuesday about the roadmap?", slots) match
      case Router.Action.Act(intent, frame) =>
        assertEquals(intent, "MeetingProposal")
        val booking = act(frame, slots, asker = "ada", other = "grace")
          .getOrElse(fail("acted with nothing to act on"))
        assertEquals(booking.iso, "2026-09-08")
        assertEquals(booking.toldTo, "ada")
        // and it was taken out of the message, not typed at a prompt.
        // The evidence is the shortest span that resolves to the same
        // day, so "Tuesday" rather than "next Tuesday" — which is
        // what a caller echoes back.
        assertEquals(frame.sourceOf("when"), Some(Source.Found))
        assertEquals(frame.filled("when"), "Tuesday")
      case other => fail(s"expected an action, got $other")
  }

  test("an exchange the person declines produces nothing, and says so") {
    val j = Durable.MemoryJournal()
    val in = Intake(slots.frameFor("MeetingProposal"), _ => "Right?")
    drive(in, j, "can we meet"): Unit
    answer(j, Reply.Answer("next Tuesday"))
    drive(in, j, "can we meet"): Unit
    answer(j, Reply.No)
    assertEquals(drive(in, j, "can we meet"), Right(Outcome.Declined))
    // nothing was written, and the caller has no booking to undo
    assertEquals(Conversation.pending(j), None)
  }
}
