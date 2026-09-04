package okay.agent

import okay.Handler
import okay.given
import okay.codec.Json
import okay.codec.Json.*
import okay.agent.Conversation.*
import scala.collection.mutable

/**
 * A conversation with a person, driven the way one actually happens:
 * a question, a process that dies, an answer days later, a message
 * that turns out not to be an answer at all.
 *
 * What is NOT tested here is any word in any language — the runtime
 * has none, and the closest it comes is carrying a string the caller
 * rendered. That absence is the design, so the fixtures below say
 * "Q(price)" rather than anything a person would read.
 */
class TestConversation extends munit.FunSuite {

  /** a caller's idea of a language: the runtime never looks inside */
  enum Tongue:
    case One, Two

  def slot(name: String, read: String => Option[Json] = s =>
    Option.when(s.trim.nonEmpty)(JStr(s.trim))): Slot[Tongue] =
    Slot(name, l => s"Q($name/$l)", read)

  val amount: String => Option[Json] = s =>
    s.trim.toDoubleOption.map(JNum(_))

  def frame(confirm: Boolean = true, opening: Option[String] = None,
            slots: Vector[Slot[Tongue]] =
              Vector(slot("where"), slot("when"))): Frame[Tongue] =
    Frame("repair", slots, (l, vs) => s"BACK($l/${vs.keys.toVector.sorted.mkString(",")})",
      opening, confirm)

  /** run an intake over a journal, parking where it parks */
  def start(f: Frame[Tongue], j: Durable.Journal, opening: String = "it broke",
            lang: Tongue = Tongue.One, touched: mutable.Buffer[ToolCall] = mutable.Buffer())
  : Either[Durable.Awaiting, Outcome] =
    given Handler[Tool] = Durable.tools(
      Handlers.recording(Handlers.tools(Map(
        AskOp -> (_ => "THE INNER HANDLER ANSWERED, WHICH IT MUST NOT"))))(touched),
      j)(Conversation.policy())
    try Right(Conversation.intake(f, lang, opening).runWith)
    catch case a: Durable.Awaiting => Left(a)

  def say(j: Durable.Journal): Say =
    Conversation.pending(j).map(_._2).getOrElse(fail("nothing is pending"))

  def reply(j: Durable.Journal, r: Reply): Unit =
    Conversation.answer(j, Conversation.pending(j).map(_._1)
      .getOrElse(fail("nothing to answer")), r)

  test("the first question parks the program, in the caller's own words") {
    val j = Durable.MemoryJournal()
    val touched = mutable.Buffer[ToolCall]()
    assert(start(frame(), j, touched = touched).isLeft, "it did not park")
    assertEquals(say(j), Say.Ask("repair", "where", "Q(where/One)"))
    // asking someone touches no world
    assertEquals(touched.toVector, Vector.empty)
  }

  test("answers move it slot by slot, and the read-back comes last") {
    val j = Durable.MemoryJournal()
    val f = frame()
    start(f, j): Unit
    reply(j, Reply.Answer("Wrocław"))
    start(f, j): Unit
    assertEquals(say(j), Say.Ask("repair", "when", "Q(when/One)"))
    reply(j, Reply.Answer("today"))
    start(f, j): Unit
    say(j) match
      case Say.ReadBack("repair", vs, text) =>
        assertEquals(vs, Map("where" -> JStr("Wrocław"), "when" -> JStr("today")))
        assertEquals(text, "BACK(One/when,where)")
      case other => fail(s"expected a read-back, got $other")

    reply(j, Reply.Yes)
    assertEquals(start(f, j), Right(Outcome.Filled(
      Map("where" -> JStr("Wrocław"), "when" -> JStr("today")))))
  }

  // the language is applied ONCE, when the question is asked, and the
  // journal carries the words. A restart renders from the log rather
  // than re-deciding a language from a three-word answer — the bug
  // this design exists to make impossible
  test("the question is rendered in the language the exchange started in") {
    val j = Durable.MemoryJournal()
    start(frame(), j, lang = Tongue.Two): Unit
    assertEquals(Say.text(say(j)), "Q(where/Two)")
    // and a fresh process, holding no language at all, still has it
    assertEquals(Conversation.pending(j).map(p => Say.text(p._2)), Some("Q(where/Two)"))
  }

  test("a slot that cannot read its answer asks once, then takes what was said") {
    val j = Durable.MemoryJournal()
    val f = frame(slots = Vector(slot("price", amount)))
    start(f, j): Unit
    reply(j, Reply.Answer("we'll agree on it"))
    start(f, j): Unit
    say(j) match
      case Say.AskAgain("repair", "price", _) => ()
      case other => fail(s"expected a second ask, got $other")

    reply(j, Reply.Answer("still not a number"))
    start(f, j): Unit
    // said twice, taken as said: an intake held hostage to a parser is
    // worse than one that stores words as words
    say(j) match
      case Say.ReadBack(_, vs, _) => assertEquals(vs("price"), JStr("still not a number"))
      case other => fail(s"expected the read-back, got $other")
  }

  test("a readable answer is read, and reaches the caller as what it MEANS") {
    val j = Durable.MemoryJournal()
    val f = frame(confirm = false, slots = Vector(slot("price", amount)))
    start(f, j): Unit
    reply(j, Reply.Answer(" 2500 "))
    assertEquals(start(f, j), Right(Outcome.Filled(Map("price" -> JNum(2500)))))
  }

  test("an interrupt abandons the intake and names what interrupted it") {
    val j = Durable.MemoryJournal()
    val f = frame()
    start(f, j): Unit
    reply(j, Reply.Interrupt("profile"))
    assertEquals(start(f, j), Right(Outcome.Interrupted("profile")))
  }

  test("no at the read-back writes nothing") {
    val j = Durable.MemoryJournal()
    val f = frame(slots = Vector(slot("where")))
    start(f, j): Unit
    reply(j, Reply.Answer("Wrocław"))
    start(f, j): Unit
    reply(j, Reply.No)
    assertEquals(start(f, j), Right(Outcome.Declined))
  }

  test("what the opening sentence answered is not asked for") {
    val j = Durable.MemoryJournal()
    val f = Frame[Tongue]("repair",
      Vector(slot("what"), slot("where"),
        // a slot that reads the opening for itself
        Slot("when", l => s"Q(when/$l)",
          extract = s => Option.when(s.contains("urgent"))(JStr("urgent")))),
      (l, vs) => s"BACK($l/${vs.keys.toVector.sorted.mkString(",")})",
      opening = Some("what"))
    start(f, j, opening = "the boiler died, urgent"): Unit
    // "what" came from the opening, "when" from its own extract, so the
    // only question left is the one nobody answered
    assertEquals(say(j), Say.Ask("repair", "where", "Q(where/One)"))
  }

  test("a reply survives the journal as what it WAS, not as bare text") {
    Vector(Reply.Answer("2500"), Reply.Interrupt("profile"), Reply.Yes, Reply.No)
      .foreach(r => assertEquals(Reply.decode(Reply.encode(r)), Some(r)))
    // and a Say does too, which is what makes `pending` work at all
    Vector(Say.Ask("f", "s", "t"), Say.AskAgain("f", "s", "t"),
      Say.ReadBack("f", Map("a" -> JNum(1)), "t"))
      .foreach(s => assertEquals(Say.decode(Say.encode(s)), Some(s)))
  }

  test("nothing is pending once the intake is done") {
    val j = Durable.MemoryJournal()
    val f = frame(confirm = false, slots = Vector(slot("where")))
    start(f, j): Unit
    reply(j, Reply.Answer("Wrocław"))
    start(f, j): Unit
    assertEquals(Conversation.pending(j), None)
  }
}
