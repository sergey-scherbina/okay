package okay.demo

import okay.intent.*
import okay.rag.Embedding

/**
 * The caller okay-intent did not have (specs/intent-classify.md).
 *
 * Twenty lanes measured these tiers and nothing used them: inside okay
 * there was no path where a message arrives and a decision leaves. A
 * library with no callers has the wrong API and cannot find out, so
 * this exists to be the caller and to break on whatever is awkward.
 *
 * It is deliberately a ROUTER and not a demonstration of a classifier:
 * the interesting part is what happens AFTER the class — the frame the
 * class needs, the question it is still missing, and the decision to
 * ask a person instead of guessing.
 *
 * No generation anywhere on this path. The taxonomy is data, the
 * patterns need nothing, and the vector tier needs a
 * `String => Embedding` that the caller supplies — an HTTP gateway
 * here, an in-process encoder in a service that has one.
 */
object IntentRouter {

  /** what a router does with a message, rather than what it thinks */
  enum Action:
    /** enough is known to act, and the frame says so */
    case Act(intent: String, frame: Frame[String])
    /** the class is known and a slot is not: ask THIS, in their language */
    case Ask(intent: String, slot: String, question: String)
    /** not confident enough to act: show the alternatives to a person */
    case Escalate(candidates: Seq[String], why: String)

  /** the taxonomy, as DATA — a service edits this without a compiler */
  val taxonomy: Taxon = Taxon.parsed(
    Seq("MeetingProposal", "MeetingRequest", "MeetingNotification", "NotAboutMeetings"),
    Seq(
      "Are you free to meet on Wednesday afternoon?" -> "MeetingProposal",
      "Please forward me the signed contract." -> "MeetingRequest",
      "Note that payroll runs a day early this month." -> "MeetingNotification",
      "What is the capital of Portugal?" -> "NotAboutMeetings"))

  /**
   * The shipped cue set, under THIS router's names.
   *
   * The first version of this file translated the canonical class
   * names by hand, in a `match` that ended in `case _ =>` — so a cue
   * class it forgot, or one added upstream later, went to
   * `NotAboutMeetings` silently. `renamed` is total in both
   * directions: it fails unless every canonical class is named here
   * and every name it is given belongs to the taxonomy above, which
   * is why this can be a `get` — a mistake is caught at class
   * initialisation, on the first message, by the test suite, not in
   * production a month later.
   */
  val cues: Patterns.Cues =
    Patterns.meeting.renamed(taxonomy, Map(
      "Proposal" -> "MeetingProposal",
      "Request" -> "MeetingRequest",
      "Notification" -> "MeetingNotification",
      "Other" -> "NotAboutMeetings")).fold(m => sys.error(m), identity)

  /**
   * The slots this router's frames are built from, HELD AS VALUES.
   *
   * Not a namespace and not ceremony: `Frame.valueOf` identifies a
   * slot by identity, so a caller that cannot name the slot object
   * cannot get the typed value out — and the first draft of this file
   * built `Slots.when(today)` inside `frameFor`, where nobody could
   * reach it. The slots depend on the day the conversation is
   * happening, so they are a value parameterised by it rather than
   * constants.
   *
   * The wildcard in `Frame.slots` is deliberate on the other side of
   * the same fact: a `Slot[?]` recovered from a frame cannot be asked
   * for a type, which is what stops a caller inventing one.
   */
  final case class Meeting(today: Temporal.Date):
    val when: Slot[Temporal.When] = Slots.when(today)
    val who: Slot[String] =
      Slots.text("who", Map("en" -> "Who should be there?"), required = false)
    // the request IS the message, so asking "what would you like
    // done?" of someone who has just said is a question about nothing
    val what: Slot[String] =
      Slots.text("what", Map("en" -> "What would you like done?"), fromMessage = true)

    /** what each class needs before it can be acted on */
    def frameFor(intent: String): Frame[String] = intent match
      case "MeetingProposal" => Frame.of(intent, when, who)
      case "MeetingRequest" => Frame.of(intent, what)
      case _ => Frame.of(intent)

  /**
   * One message in, one action out.
   *
   * The tier order is the one the measurements argued for: the pattern
   * cues cost nothing and are 89% accurate WHERE THEY FIRE, so they go
   * first; the vector tier answers the rest; and below its threshold
   * nobody guesses — a person sees the two candidates instead.
   */
  def route(message: String, slots: Meeting,
            lang: String = "en",
            vectors: Option[(Centroid.Trained, String => Embedding)] = None,
            floor: Double = 0.02): Action =
    // No `.filter(taxonomy.has)` and no translation: `cues` was built
    // against `taxonomy`, so every class it can answer with is one.
    val fromCue = Patterns.classify(cues, message, floor = 0.4)

    val decided: Either[Seq[String], String] = fromCue match
      case Some(cls) => Right(cls)
      case None => vectors match
        case None => Left(Seq.empty)
        case Some((model, embed)) =>
          Centroid.score(model, embed(message)) match
            case Some(v) if v.margin >= floor => Right(v.best)
            case Some(v) => Left(Seq(v.best) ++ v.runnerUp)
            case None => Left(Seq.empty)

    decided match
      case Left(candidates) =>
        Action.Escalate(candidates,
          if candidates.isEmpty then "no cue fired and no vector model is loaded"
          else s"the top two are within $floor of each other")
      case Right(intent) =>
        // fill from the message BEFORE asking: "Are you free Wednesday
        // afternoon?" carries its own `when`, and a router that asks
        // for it is asking the person who just said
        val frame = slots.frameFor(intent).fillFrom(message)
        frame.missing(lang).headOption match
          case Some((slot, question)) => Action.Ask(intent, slot, question)
          case None => Action.Act(intent, frame)
}
