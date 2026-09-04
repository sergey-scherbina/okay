package okay.demo

import okay.intent.*
import okay.frame.{Frame, Slot}
import okay.rag.Embedding

/**
 * The caller okay-intent did not have (specs/intent-classify.md).
 *
 * Twenty lanes measured these tiers and nothing used them: inside okay
 * there was no path where a message arrives and a decision leaves. A
 * library with no callers has the wrong API and cannot find out, so
 * this exists to be the caller and to break on whatever is awkward.
 *
 * It has now done that four times, and the fourth is why this file is
 * SHORTER than it was: the composition it worked out — cues, then a
 * model, then a person — moved into `okay.intent.Router`, where a
 * caller outside this demo can find it. What is left here is what a
 * caller actually owns: a taxonomy, the names it uses, the frames its
 * classes need, and the day the conversation is happening.
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

  /** the four outcomes are the library's now — this file proved they
   * were the right four and no longer defines them */
  export Router.Action

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
  val canonicalNames: Map[String, String] = Map(
    "Proposal" -> "MeetingProposal",
    "Request" -> "MeetingRequest",
    "Notification" -> "MeetingNotification",
    "Other" -> "NotAboutMeetings")

  val cues: Patterns.Cues =
    Patterns.meeting.renamed(taxonomy, canonicalNames).fold(m => sys.error(m), identity)

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
  final case class Meeting(today: Temporal.Date, lang: String = "en"):
    val when: Slot[Temporal.When] = Slots.when(today)
    val who: Slot[String] =
      Slots.text("who", Map("en" -> "Who should be there?"), required = false)
    // the request IS the message, so asking "what would you like
    // done?" of someone who has just said is a question about nothing
    val what: Slot[String] =
      Slots.text("what", Map("en" -> "What would you like done?"), fromMessage = true)

    /** what each class needs before it can be acted on */
    def frameFor(intent: String): Frame[String] = (intent match
      case "MeetingProposal" => Frame.of(intent, when, who)
      case "MeetingRequest" => Frame.of(intent, what)
      case _ => Frame.of(intent)).in(lang)

  /** the shipped model, under THIS router's names.
   *
   * `renamed` is total in both directions, exactly as it is for the
   * cues, so a class the model knows and nobody mapped is an error
   * here rather than a wrong answer in production.
   */
  val model: CharGrams.Trained =
    CharGrams.renamed(Models.meeting, taxonomy, canonicalNames)
      .fold(m => sys.error(m), identity)

  /**
   * One message in, one action out — assembled from the library's
   * door rather than by hand.
   *
   * The tier order, the floors and the four outcomes are
   * `okay.intent.Router`'s. What this supplies is the domain: which
   * classes exist, what each one needs before it can be acted on, and
   * an optional embedder for the vector tier when a caller has a
   * gateway.
   */
  def router(slots: Meeting,
             vectors: Option[(Centroid.Trained, String => Embedding)] = None)
  : Router.Router =
    Router.Router.of(taxonomy, cues = Some(cues), grams = Some(model),
      vectors = vectors, frames = slots.frameFor)
      .fold(m => sys.error(m), identity)

  def route(message: String, slots: Meeting,
            vectors: Option[(Centroid.Trained, String => Embedding)] = None): Action =
    router(slots, vectors).route(message)
}
