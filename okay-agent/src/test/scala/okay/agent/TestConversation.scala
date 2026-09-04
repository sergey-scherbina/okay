package okay.agent

import okay.Handler
import okay.given
import okay.codec.Json.*
import okay.agent.Conversation.*
import okay.frame.{Frame, Slot}
import scala.collection.mutable

/**
 * A conversation with a person, driven the way one actually happens:
 * a question, a process that dies, an answer days later, a message
 * that turns out not to be an answer at all.
 *
 * What is NOT tested here is any word in any language — the runtime
 * has none, and the closest it comes is carrying a string the caller
 * wrote. That absence is the design, so the fixtures below say
 * "Q(price)" rather than anything a person would read.
 *
 * The slots are `okay.frame`'s since the merge: this module owns the
 * suspension and that one owns the form.
 */
class TestConversation extends munit.FunSuite {

  /** two languages, as the codes a journal can carry */
  val One = "one"
  val Two = "two"

  def asks(name: String): Map[String, String] =
    Map(One -> s"Q($name/one)", Two -> s"Q($name/two)")

  def slot(name: String): Slot[String] =
    Slot(name, asks(name), s => Option.when(s.trim.nonEmpty)(s.trim))

  val price: Slot[Double] =
    Slot("price", asks("price"), s => s.trim.toDoubleOption)

  def intake(confirm: Boolean = true, lang: String = One,
             slots: Vector[Slot[?]] = Vector(slot("where"), slot("when")))
  : Intake[String] =
    Intake(Frame("repair", slots).in(lang),
      f => s"BACK(${f.lang}/${f.filled.keys.toVector.sorted.mkString(",")})",
      confirm)

  /** run an intake over a journal, parking where it parks */
  def start(in: Intake[String], j: Durable.Journal, opening: String = "it broke",
            touched: mutable.Buffer[ToolCall] = mutable.Buffer())
  : Either[Durable.Awaiting, Outcome[String]] =
    given Handler[Tool] = Durable.tools(
      Handlers.recording(Handlers.tools(Map(
        AskOp -> (_ => "THE INNER HANDLER ANSWERED, WHICH IT MUST NOT"))))(touched),
      j)(Conversation.policy())
    try Right(Conversation.intake(in, opening).runWith)
    catch case a: Durable.Awaiting => Left(a)

  def say(j: Durable.Journal): Say =
    Conversation.pending(j).map(_._2).getOrElse(fail("nothing is pending"))

  def reply(j: Durable.Journal, r: Reply): Unit =
    Conversation.answer(j, Conversation.pending(j).map(_._1)
      .getOrElse(fail("nothing to answer")), r)

  def filled(o: Either[Durable.Awaiting, Outcome[String]]): Frame[String] = o match
    case Right(Outcome.Filled(f)) => f
    case other => fail(s"expected a filled frame, got $other")

  test("the first question parks the program, in the caller's own words") {
    val j = Durable.MemoryJournal()
    val touched = mutable.Buffer[ToolCall]()
    assert(start(intake(), j, touched = touched).isLeft, "it did not park")
    assertEquals(say(j), Say.Ask("repair", "where", "Q(where/one)", Some(2)))
    // asking someone touches no world
    assertEquals(touched.toVector, Vector.empty)
  }

  test("a question says how many are left, so the caller does not count") {
    // the third of the consumer's three asks: Say could not say this,
    // and after a restart the journal is all there is to count from
    val j = Durable.MemoryJournal()
    val in = intake()
    start(in, j): Unit
    assertEquals(say(j) match { case Say.Ask(_, _, _, r) => r; case _ => None }, Some(2))
    reply(j, Reply.Answer("Wrocław"))
    start(in, j): Unit
    assertEquals(say(j) match { case Say.Ask(_, _, _, r) => r; case _ => None }, Some(1))
  }

  test("answers move it slot by slot, and the read-back comes last") {
    val j = Durable.MemoryJournal()
    val in = intake()
    start(in, j): Unit
    reply(j, Reply.Answer("Wrocław"))
    start(in, j): Unit
    assertEquals(say(j), Say.Ask("repair", "when", "Q(when/one)", Some(1)))
    reply(j, Reply.Answer("today"))
    start(in, j): Unit
    say(j) match
      case Say.ReadBack("repair", vs, text) =>
        assertEquals(vs, Map("where" -> JStr("Wrocław"), "when" -> JStr("today")))
        assertEquals(text, "BACK(one/when,where)")
      case other => fail(s"expected a read-back, got $other")

    reply(j, Reply.Yes)
    val f = filled(start(in, j))
    assert(f.complete)
    assertEquals(f.filled, Map("where" -> "Wrocław", "when" -> "today"))
  }

  test("an answer may answer more than was asked") {
    // the live defect: asked where, told "Wrocław, and remote works",
    // the runtime took the city and then asked about the terms it had
    // just been told
    val j = Durable.MemoryJournal()
    val terms = Slot[String]("terms", asks("terms"),
      s => Option.when(s.trim.nonEmpty)(s.trim),
      extract = s => Option.when(s.toLowerCase.contains("remote"))(
        okay.frame.Found("remote", "remote")))
    val in = intake(confirm = false, slots = Vector(slot("where"), terms))
    start(in, j): Unit
    assertEquals(say(j), Say.Ask("repair", "where", "Q(where/one)", Some(2)))
    reply(j, Reply.Answer("Wrocław, and remote works"))
    val f = filled(start(in, j))
    assertEquals(f.valueOf(terms), Some("remote"))
    assertEquals(f.filled("where"), "Wrocław, and remote works")
    // and nothing was asked after the first question
    assertEquals(Conversation.pending(j), None)
  }

  test("the question is in the language the exchange was built in") {
    val j = Durable.MemoryJournal()
    start(intake(lang = Two), j): Unit
    assertEquals(Say.text(say(j)), "Q(where/two)")
    // and a fresh process, holding no language at all, still has it
    assertEquals(Conversation.pending(j).map(p => Say.text(p._2)), Some("Q(where/two)"))
  }

  test("there is no way to hand an intake a different language mid-exchange") {
    // the consumer's operational warning, as a property rather than a
    // convention: the language is the FRAME's, so answering cannot
    // carry one and cannot change it
    val f = Frame.of("repair", slot("where")).in(Two)
    val after = f.answer("where", "Wrocław").toOption.get
    assertEquals(after.lang, Two)
    assertEquals(after.fillFrom("anything at all").lang, Two)
    assertEquals(after.missing, Vector.empty)
  }

  test("every slot can be asked in the language of the exchange") {
    // a four-language intake that quietly falls back to English on one
    // question is the defect this makes visible before it ships
    assertEquals(intake(lang = Two).frame.untranslated, Vector.empty)
    val half = Frame.of("repair", Slot("where", Map(One -> "only one"), Some(_))).in(Two)
    assertEquals(half.untranslated, Vector("where"))
  }

  test("a slot that cannot read its answer asks once, then keeps the words") {
    val j = Durable.MemoryJournal()
    val in = intake(slots = Vector(price))
    start(in, j): Unit
    reply(j, Reply.Answer("we'll agree on it"))
    start(in, j): Unit
    say(j) match
      case Say.AskAgain("repair", "price", _, _) => ()
      case other => fail(s"expected a second ask, got $other")

    reply(j, Reply.Answer("still not a number"))
    start(in, j): Unit
    say(j) match
      case Say.ReadBack(_, vs, _) => assert(!vs.contains("price"), vs)
      case other => fail(s"expected the read-back, got $other")

    reply(j, Reply.Yes)
    val f = filled(start(in, j))
    // said twice, taken as said — but as WORDS, beside the slot, not
    // as its value. The old runtime stored the unparsed text AS the
    // value, which is how a field typed as a number held a sentence.
    assertEquals(f.valueOf(price), None)
    assertEquals(f.said("price"), Some("still not a number"))
    assert(!f.complete)
  }

  test("a readable answer reaches the caller as what it MEANS, at its type") {
    val j = Durable.MemoryJournal()
    val in = intake(confirm = false, slots = Vector(price))
    start(in, j): Unit
    reply(j, Reply.Answer(" 2500 "))
    val f = filled(start(in, j))
    // the first of the consumer's three asks: a Double, not a Json,
    // and not the text to be parsed a second time
    assertEquals(f.valueOf(price), Some(2500.0))
    // the value is what the parser made of it; the text is what the
    // person actually typed, spaces and all, because a frame shown
    // back to someone should show what they wrote
    assertEquals(f.filled("price"), " 2500 ")
  }

  test("an interrupt abandons the intake and names what interrupted it") {
    val j = Durable.MemoryJournal()
    val in = intake()
    start(in, j): Unit
    reply(j, Reply.Interrupt("profile"))
    assertEquals(start(in, j), Right(Outcome.Interrupted("profile")))
  }

  test("no at the read-back writes nothing") {
    val j = Durable.MemoryJournal()
    val in = intake(slots = Vector(slot("where")))
    start(in, j): Unit
    reply(j, Reply.Answer("Wrocław"))
    start(in, j): Unit
    reply(j, Reply.No)
    assertEquals(start(in, j), Right(Outcome.Declined))
  }

  test("what the opening sentence answered is not asked for") {
    val j = Durable.MemoryJournal()
    val what = Slot[String]("what", asks("what"), s => Option.when(s.trim.nonEmpty)(s.trim),
      // the slot that swallows the opening says so itself — which is
      // what replaced the frame naming one slot as special
      extract = s => Option.when(s.trim.nonEmpty)(okay.frame.Found(s.trim, s.trim)))
    val when = Slot[String]("when", asks("when"), Some(_),
      extract = s => Option.when(s.contains("urgent"))(okay.frame.Found("urgent", "urgent")))
    val in = intake(slots = Vector(what, slot("where"), when))
    start(in, j, opening = "the boiler died, urgent"): Unit
    // "what" and "when" came out of the opening, so the only question
    // left is the one nobody answered
    assertEquals(say(j), Say.Ask("repair", "where", "Q(where/one)", Some(1)))
  }

  test("a reply survives the journal as what it WAS, not as bare text") {
    Vector(Reply.Answer("2500"), Reply.Interrupt("profile"), Reply.Yes, Reply.No)
      .foreach(r => assertEquals(Reply.decode(Reply.encode(r)), Some(r)))
    // and a Say does too, which is what makes `pending` work at all
    Vector(Say.Ask("f", "s", "t", Some(3)), Say.AskAgain("f", "s", "t", Some(1)),
      Say.ReadBack("f", Map("a" -> JNum(1)), "t"))
      .foreach(s => assertEquals(Say.decode(Say.encode(s)), Some(s)))
  }

  test("a question parked before `remaining` existed still decodes") {
    // a journal outlives a deploy, and an intake parked by the old
    // build must still be renderable by the new one
    val old = JObj(Vector("say" -> JStr("ask"), "frame" -> JStr("repair"),
      "slot" -> JStr("where"), "text" -> JStr("Q(where/one)")))
    assertEquals(Say.decode(old), Some(Say.Ask("repair", "where", "Q(where/one)", None)))
  }

  test("nothing is pending once the intake is done") {
    val j = Durable.MemoryJournal()
    val in = intake(confirm = false, slots = Vector(slot("where")))
    start(in, j): Unit
    reply(j, Reply.Answer("Wrocław"))
    start(in, j): Unit
    assertEquals(Conversation.pending(j), None)
  }
}
