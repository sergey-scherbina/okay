package okay.agent

import okay.!
import okay.codec.Json
import okay.codec.Json.*

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
 * and the caller renders it; a slot's own `ask` is a function of
 * whatever the caller uses to mean "language", which this module
 * carries and never inspects. The alternative — phrasing in the
 * library — makes it accumulate four-language copy for every product
 * that ever uses it.
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
   * One question and what its answer means. The caller supplies
   * these; nothing here knows what a `name` refers to.
   *
   * `L` is whatever the caller means by a language — an enum, a tag,
   * `Unit` for a service that speaks one. It is carried and never
   * inspected, which is what lets the language be PINNED to an
   * exchange: it is an argument of the run, not something re-derived
   * from each three-word answer.
   */
  final case class Slot[L](name: String,
                           ask: L => String,
                           /** what the answer MEANS. `None` is not a
                            * failure to store — it is a question to
                            * ask again, once */
                           read: String => Option[Json] = (s: String) =>
                             Option.when(s.trim.nonEmpty)(JStr(s.trim)),
                           /** what the OPENING sentence already
                            * answered, so the intake does not ask for
                            * what it was just told */
                           extract: String => Option[Json] = (_: String) => None)

  /** the shape of one kind of request */
  final case class Frame[L](name: String,
                            slots: Vector[Slot[L]],
                            /** the whole thing said back, for a yes.
                             * A sentence about every answer at once is
                             * the caller's to compose — this module
                             * would have to invent the words */
                            readBack: (L, Map[String, Json]) => String,
                            /** which slot the opening sentence
                             * answers outright, if any */
                            opening: Option[String] = None,
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
    case Ask(frame: String, slot: String, text: String)
    /** the slot could not read the last answer. Asked once, and then
     * whatever was said is taken as said — the caller renders its own
     * "I could not read that" around the same question */
    case AskAgain(frame: String, slot: String, text: String)
    case ReadBack(frame: String, values: Map[String, Json], text: String)

  object Say:
    def text(s: Say): String = s match
      case Ask(_, _, t) => t
      case AskAgain(_, _, t) => t
      case ReadBack(_, _, t) => t

    def encode(s: Say): Json = s match
      case Ask(f, sl, t) => JObj(Vector(
        "say" -> JStr("ask"), "frame" -> JStr(f), "slot" -> JStr(sl), "text" -> JStr(t)))
      case AskAgain(f, sl, t) => JObj(Vector(
        "say" -> JStr("askAgain"), "frame" -> JStr(f), "slot" -> JStr(sl), "text" -> JStr(t)))
      case ReadBack(f, vs, t) => JObj(Vector(
        "say" -> JStr("readBack"), "frame" -> JStr(f),
        "values" -> JObj(vs.toVector.sortBy(_._1)), "text" -> JStr(t)))

    def decode(j: Json): Option[Say] =
      def str(fs: Vector[(String, Json)], k: String) =
        fs.collectFirst { case (`k`, JStr(v)) => v }
      j match
        case JObj(fs) => (str(fs, "say"), str(fs, "text")) match
          case (Some("ask"), Some(t)) =>
            for f <- str(fs, "frame"); s <- str(fs, "slot") yield Ask(f, s, t)
          case (Some("askAgain"), Some(t)) =>
            for f <- str(fs, "frame"); s <- str(fs, "slot") yield AskAgain(f, s, t)
          case (Some("readBack"), Some(t)) =>
            for
              f <- str(fs, "frame")
              vs <- fs.collectFirst { case ("values", JObj(v)) => v.toMap }
            yield ReadBack(f, vs, t)
          case _ => None
        case _ => None

  /** how an intake ended */
  enum Outcome:
    case Filled(values: Map[String, Json])
    /** the person said no to the read-back; nothing was understood
     * wrongly, and nothing is written */
    case Declined
    case Interrupted(intent: String)

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
   * it stopped.
   *
   * `opening` is the sentence that started this — read by the frame's
   * own `opening` slot and by every slot's `extract`, so the intake
   * asks only for what it was not already told.
   */
  def intake[L](frame: Frame[L], lang: L, opening: String): Outcome ! Tool =
    val seeded: Map[String, Json] =
      frame.slots.flatMap(s => s.extract(opening).map(s.name -> _)).toMap ++
        frame.opening.flatMap(n => frame.slots.find(_.name == n))
          .flatMap(s => s.read(opening).map(s.name -> _)).toMap

    def ask(say: Say, id: String): Reply ! Tool =
      okay.effect[Tool, String](Tool.Call(ToolCall(id, AskOp, Say.encode(say))))
        .map(s => Reply.decode(s).getOrElse(Reply.Answer(s)))

    /** one slot, and the single re-ask its `read` earns */
    def fill(s: Slot[L], again: Boolean): Either[Reply, Json] ! Tool =
      val say =
        if again then Say.AskAgain(frame.name, s.name, s.ask(lang))
        else Say.Ask(frame.name, s.name, s.ask(lang))
      ask(say, s"${frame.name}-${s.name}${if again then "-again" else ""}").flatMap {
        case Reply.Answer(text) => s.read(text) match
          case Some(v) => okay.pure[Tool, Either[Reply, Json]](Right(v))
          // asked once, and then taken as it was said: a person who
          // answers the same way twice means it, and holding an intake
          // hostage to a parser is worse than storing words as words
          case None if !again => fill(s, again = true)
          case None => okay.pure[Tool, Either[Reply, Json]](Right(JStr(text)))
        case other => okay.pure[Tool, Either[Reply, Json]](Left(other))
      }

    def loop(todo: List[Slot[L]], have: Map[String, Json]): Outcome ! Tool =
      todo match
        case Nil => finish(have)
        case s :: rest => fill(s, again = false).flatMap {
          case Right(v) => loop(rest, have + (s.name -> v))
          case Left(Reply.Interrupt(i)) => okay.pure[Tool, Outcome](Outcome.Interrupted(i))
          // a No or a Yes to a question that asked for neither is the
          // person declining to go on with it
          case Left(_) => okay.pure[Tool, Outcome](Outcome.Declined)
        }

    def finish(have: Map[String, Json]): Outcome ! Tool =
      if !frame.confirm then okay.pure[Tool, Outcome](Outcome.Filled(have))
      else ask(Say.ReadBack(frame.name, have, frame.readBack(lang, have)),
        s"${frame.name}-readback").map {
        case Reply.Yes => Outcome.Filled(have)
        case Reply.Interrupt(i) => Outcome.Interrupted(i)
        case _ => Outcome.Declined
      }

    loop(frame.slots.filterNot(s => seeded.contains(s.name)).toList, seeded)
