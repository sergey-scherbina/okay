package okay.agent

import okay.!
import okay.codec.Json
import okay.codec.Json.*
import okay.frame.Frame

/**
 * A conversation with a PERSON (specs/conversation.md).
 *
 * specs/intent-classify.md answers "what is this message". This
 * answers "what happens next", which has state, outlives the process
 * that started it, and turns on a message that may not be an answer
 * at all.
 *
 * The claim this module makes good on: all of that is MECHANISM.
 * What is here asks the next unanswered question, asks again when the
 * answer cannot be read, reads back what it understood before it is
 * done, and remembers where it was across a restart — and not one of
 * those sentences mentions what the conversation was about. The
 * missing noun is the caller's: which frames exist, which slots they
 * have, what those slots ask, and the words.
 *
 * NO WORDS LIVE HERE, in any language. `Say` names a KIND of message
 * and the caller renders it; a slot's own `ask` is a map the caller
 * writes, which this module reads by key and never composes. The
 * alternative — phrasing in the library — makes it accumulate
 * four-language copy for every product that ever uses it.
 *
 * WHAT IS NO LONGER HERE: the slots. This module had grown its own
 * `Slot` and `Frame` on the same day `okay.intent` grew theirs, and
 * two slot models in one repository is one too many. `okay.frame` is
 * the merged form — typed values instead of `Option[Json]`, an answer
 * addressed by NAME, and a count of what is left to ask — and this
 * module keeps what the frame cannot have: the suspension.
 *
 * The suspension is `Durable`: an intake is a straight-line program
 * that stops at every question, and `OnRepeat.Await` is what parks it
 * there with the question in the log. A closure on the heap cannot be
 * appended to a log and read back after a deploy, which is the whole
 * reason the state is journalled rather than captured.
 */
object Conversation:

  /**
   * The operation whose answer is a person's.
   *
   * Named rather than typed because `Durable`'s policy is keyed by
   * operation name — and one name is enough: what KIND of question it
   * is travels in the arguments, where the caller can read it after a
   * restart without running the program to find out.
   */
  val AskOp = "conversation.ask"

  /** hand this to `Durable.tools` as its policy, or fold it into one
   * of the caller's own; every other operation keeps whatever repeat
   * semantics it had */
  def policy(inner: String => Durable.OnRepeat = _ => Durable.OnRepeat.Fail)
  : String => Durable.OnRepeat =
    name => if name == AskOp then Durable.OnRepeat.Await else inner(name)

  /**
   * The words a frame cannot carry, and the one decision it does not
   * make.
   *
   * A `Frame` describes and holds; it has no opinion about whether an
   * exchange ends with a read-back, and it cannot compose the sentence
   * that says everything back at once — that sentence is the caller's,
   * in the caller's language, and inventing it here is exactly the
   * "no words live in this module" line being crossed.
   */
  final case class Intake[I](frame: Frame[I],
                             /** the whole thing said back, for a yes */
                             readBack: Frame[I] => String,
                             /** ask before this is `Filled` */
                             confirm: Boolean = true)

  /**
   * What arrives at a suspension, and the reason it is not a
   * `String`: at every question the next message may be a correction,
   * an unrelated request or an exact command, and only the caller's
   * own deterministic layer can tell which. Deciding that here would
   * be this module guessing about a domain it does not have.
   */
  enum Reply:
    case Answer(text: String)
    /** an exact command, recognised by the caller — the ONLY thing
     * that aborts an intake. A similarity guess must not: the
     * free-text answer to "what are your skills" is exactly the shape
     * a vector layer misreads as a new request */
    case Interrupt(intent: String)
    case Yes
    case No

  object Reply:
    /** the journal is self-describing: a reply is stored as what it
     * WAS, not as bare text that has to be re-classified later */
    def encode(r: Reply): String = Json.print(r match
      case Answer(t) => JObj(Vector("reply" -> JStr("answer"), "text" -> JStr(t)))
      case Interrupt(i) => JObj(Vector("reply" -> JStr("interrupt"), "intent" -> JStr(i)))
      case Yes => JObj(Vector("reply" -> JStr("yes")))
      case No => JObj(Vector("reply" -> JStr("no"))))

    def decode(s: String): Option[Reply] =
      def field(fs: Vector[(String, Json)], k: String) =
        fs.collectFirst { case (`k`, JStr(v)) => v }
      Json.parse(s) match
        case JObj(fs) => field(fs, "reply") match
          case Some("answer") => field(fs, "text").map(Answer(_))
          case Some("interrupt") => field(fs, "intent").map(Interrupt(_))
          case Some("yes") => Some(Yes)
          case Some("no") => Some(No)
          case _ => None
        case _ => None

  /** what the runtime is asking for, for the CALLER to render */
  /**
   * What the runtime is asking for — and the TEXT it was asked with.
   *
   * The text is the caller's own, applied once at the moment of
   * asking, and that is what pins the LANGUAGE of an exchange: a
   * restart renders the outstanding question by reading the journal,
   * not by re-deriving a language from whatever the person typed
   * last. An intake that re-decides its language per answer switches
   * it on a three-word reply — measured, in the implementation this
   * module was lifted from, on the second-to-last question.
   */
  enum Say:
    /**
     * `remaining` is how many questions are still to ask, INCLUDING
     * this one — which the caller had to count for itself, and could
     * not after a restart, since only the journal is left.
     *
     * `Option` because a journal outlives a deploy: an entry parked
     * before this field existed decodes as `None`, which is "not
     * written down", not "none left". A caller renders no count for
     * those rather than a wrong one.
     */
    case Ask(frame: String, slot: String, text: String,
             remaining: Option[Int] = None)
    /** the slot could not read the last answer. Asked once, and then
     * whatever was said is taken as said — the caller renders its own
     * "I could not read that" around the same question */
    case AskAgain(frame: String, slot: String, text: String,
                  remaining: Option[Int] = None)
    case ReadBack(frame: String, values: Map[String, Json], text: String)

  object Say:
    def text(s: Say): String = s match
      case Ask(_, _, t, _) => t
      case AskAgain(_, _, t, _) => t
      case ReadBack(_, _, t) => t

    private def asked(kind: String, f: String, sl: String, t: String, r: Option[Int]) =
      JObj(Vector("say" -> JStr(kind), "frame" -> JStr(f), "slot" -> JStr(sl),
        "text" -> JStr(t)) ++ r.map(n => "remaining" -> JNum(n.toDouble)))

    def encode(s: Say): Json = s match
      case Ask(f, sl, t, r) => asked("ask", f, sl, t, r)
      case AskAgain(f, sl, t, r) => asked("askAgain", f, sl, t, r)
      case ReadBack(f, vs, t) => JObj(Vector(
        "say" -> JStr("readBack"), "frame" -> JStr(f),
        "values" -> JObj(vs.toVector.sortBy(_._1)), "text" -> JStr(t)))

    def decode(j: Json): Option[Say] =
      def str(fs: Vector[(String, Json)], k: String) =
        fs.collectFirst { case (`k`, JStr(v)) => v }
      j match
        case JObj(fs) =>
          // absent in every entry written before the field existed,
          // and that is what None means here
          val left = fs.collectFirst { case ("remaining", JNum(n)) => n.toInt }
          (str(fs, "say"), str(fs, "text")) match
          case (Some("ask"), Some(t)) =>
            for f <- str(fs, "frame"); s <- str(fs, "slot") yield Ask(f, s, t, left)
          case (Some("askAgain"), Some(t)) =>
            for f <- str(fs, "frame"); s <- str(fs, "slot") yield AskAgain(f, s, t, left)
          case (Some("readBack"), Some(t)) =>
            for
              f <- str(fs, "frame")
              vs <- fs.collectFirst { case ("values", JObj(v)) => v.toMap }
            yield ReadBack(f, vs, t)
          case _ => None
        case _ => None

  /** how an intake ended */
  enum Outcome[+I]:
    /**
     * The filled FRAME, not a map of `Json`.
     *
     * The map was the defect: a slot parsed an answer to check it was
     * acceptable, stored the TEXT, and the caller parsed it again —
     * with whatever reference day it happened to have. The frame holds
     * the value the slot's own parser produced, and `valueOf` hands it
     * back at that type.
     */
    case Filled[I](frame: Frame[I]) extends Outcome[I]
    /** the person said no to the read-back; nothing was understood
     * wrongly, and nothing is written */
    case Declined extends Outcome[Nothing]
    case Interrupted(intent: String) extends Outcome[Nothing]

  /**
   * What a parked program is waiting to be told, without running the
   * program to find out — for a process that restarted and has a
   * question to render.
   */
  def pending(journal: Durable.Journal): Option[(Int, Say)] =
    Durable.awaiting(journal).filter(_.op == AskOp).flatMap { e =>
      Durable.argsOf(e).flatMap(Say.decode).map(e.seq -> _)
    }

  /** answer the question a program is parked on */
  def answer(journal: Durable.Journal, seq: Int, r: Reply): Unit =
    journal.complete(seq, Reply.encode(r))

  /**
   * The intake, as a straight-line program.
   *
   * It suspends at every `ask`, which is an ordinary tool call that
   * `Durable` parks on. Nothing here is a state machine: the state is
   * the journal, and re-running is how the program gets back to where
   * it stopped. The frame is carried through the recursion and rebuilt
   * on a replay from the same answers, so it never has to be stored.
   *
   * `opening` is the sentence that started this. It is offered to
   * every slot's extractor before anything is asked, so the intake
   * asks only for what it was not already told — which is also what
   * replaced the old `Frame.opening`: a slot that should swallow the
   * whole opening says so with its own extractor, rather than the
   * frame naming one slot as special.
   *
   * The LANGUAGE is not a parameter. It is the frame's, pinned when
   * the frame was built, which is the point of the merge: an intake
   * that takes a language per call can be handed a different one
   * mid-exchange, and one was — on the second-to-last question.
   */
  def intake[I](in: Intake[I], opening: String): Outcome[I] ! Tool =
    val name = in.frame.intent.toString

    def ask(say: Say, id: String): Reply ! Tool =
      okay.effect[Tool, String](Tool.Call(ToolCall(id, AskOp, Say.encode(say))))
        .map(s => Reply.decode(s).getOrElse(Reply.Answer(s)))

    /**
     * One question, and the single re-ask its parser earns.
     *
     * `take` is what makes an answer able to answer more than was
     * asked: told "Wrocław, and remote works" when asked where, the
     * frame keeps the city AND lets the terms slot read the rest, so
     * the next question is not one the person has already answered.
     */
    def fill(f: Frame[I], slot: String, skip: Set[String], again: Boolean)
    : Outcome[I] ! Tool =
      val question = f.missing.collectFirst { case (n, q) if n == slot => q }
        .getOrElse(slot)
      val left = Some(f.remaining)
      val say =
        if again then Say.AskAgain(name, slot, question, left)
        else Say.Ask(name, slot, question, left)
      ask(say, s"$name-$slot${if again then "-again" else ""}").flatMap {
        case Reply.Answer(text) =>
          val next = f.take(slot, text)
          if next.has(slot) then loop(next, skip)
          else if !again then fill(next, slot, skip, again = true)
          // asked once, and then taken as it was said: a person who
          // answers the same way twice means it, and holding an intake
          // hostage to a parser is worse than moving on with the words
          // kept. They are kept — `said(slot)` — rather than stored as
          // the value, which is what the old runtime had to do.
          else loop(next, skip + slot)
        case Reply.Interrupt(i) => okay.pure[Tool, Outcome[I]](Outcome.Interrupted(i))
        // a No or a Yes to a question that asked for neither is the
        // person declining to go on with it
        case _ => okay.pure[Tool, Outcome[I]](Outcome.Declined)
      }

    def loop(f: Frame[I], skip: Set[String]): Outcome[I] ! Tool =
      f.missing.map(_._1).find(n => !skip.contains(n)) match
        case None => finish(f)
        case Some(slot) => fill(f, slot, skip, again = false)

    def finish(f: Frame[I]): Outcome[I] ! Tool =
      if !in.confirm then okay.pure[Tool, Outcome[I]](Outcome.Filled(f))
      else ask(Say.ReadBack(name, f.filled.view.mapValues(JStr(_)).toMap, in.readBack(f)),
        s"$name-readback").map {
        case Reply.Yes => Outcome.Filled(f)
        case Reply.Interrupt(i) => Outcome.Interrupted(i)
        case _ => Outcome.Declined
      }

    loop(in.frame.fillFrom(opening), Set.empty)
