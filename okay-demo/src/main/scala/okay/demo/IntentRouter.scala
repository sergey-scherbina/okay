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

  /** what each class needs before it can be acted on */
  def frameFor(intent: String, today: Temporal.Date): Frame[String] = intent match
    case "MeetingProposal" => Frame.of(intent, Slots.when(today),
      Slots.text("who", Map("en" -> "Who should be there?"), required = false))
    case "MeetingRequest" => Frame.of(intent,
      Slots.text("what", Map("en" -> "What would you like done?")))
    case _ => Frame.of(intent)

  /**
   * One message in, one action out.
   *
   * The tier order is the one the measurements argued for: the pattern
   * cues cost nothing and are 89% accurate WHERE THEY FIRE, so they go
   * first; the vector tier answers the rest; and below its threshold
   * nobody guesses — a person sees the two candidates instead.
   */
  def route(message: String, today: Temporal.Date,
            lang: String = "en",
            vectors: Option[(Centroid.Trained, String => Embedding)] = None,
            floor: Double = 0.02): Action =
    val fromCue = Patterns.classify(Patterns.meeting, message, floor = 0.4)
      .map(canonicalToTaxonomy)
      .filter(taxonomy.has)

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
        val frame = frameFor(intent, today)
        frame.missing(lang).headOption match
          case Some((slot, question)) => Action.Ask(intent, slot, question)
          case None => Action.Act(intent, frame)

  /** the pattern tier speaks the canonical names; the taxonomy this
   * router carries is domain-bearing, and the measurements say the
   * domain-bearing names are worth keeping */
  private def canonicalToTaxonomy(cls: String): String = cls match
    case "Proposal" => "MeetingProposal"
    case "Request" => "MeetingRequest"
    case "Notification" => "MeetingNotification"
    case _ => "NotAboutMeetings"
}
