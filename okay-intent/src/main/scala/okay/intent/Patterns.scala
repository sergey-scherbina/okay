package okay.intent

/**
 * The symbolic tier, done the way the ontology systems actually do it
 * (specs/intent-classify.md).
 *
 * The BM25 tier plateaued at 60-64% agreement however tight its
 * margin, and the diagnosis was mechanical: BM25 scores CONTENT words,
 * while an intent is carried by function words and syntax. "Can we
 * meet Tuesday?" and "Would Thursday morning work?" share no topic
 * vocabulary and are the same intent; "send me the agenda before the
 * meeting" and "shall we meet before the agenda goes out" share plenty
 * and are not.
 *
 * So this matches the cues that a frame actually turns on — a modal
 * plus a pronoun, an imperative politeness marker, a first-person
 * announcement — and nothing about the subject. That is FrameNet's
 * "lexical unit" in the only form that is cheap: a phrase, weighted,
 * anchored to where it may appear.
 *
 * Total and deterministic, no network, no model, so it runs in the
 * default gate.
 */
object Patterns {

  /**
   * A cue: the phrase, the class it evokes, its weight, and whether it
   * must open the message.
   *
   * `atStart` is not decoration. "Please send the agenda" is a
   * request; "I will ask whether you could please send it" is a
   * notification that mentions one, and the difference is position.
   */
  final case class Cue(phrase: String, cls: String, weight: Double = 1.0,
                       atStart: Boolean = false)

  final case class Verdict(best: String, score: Double, margin: Double,
                           runnerUp: Option[String], fired: Seq[String])

  /**
   * English cues for the meeting taxonomy.
   *
   * Written from the mechanism rather than from the fixture: each one
   * is a syntactic frame, and none of them names a subject. That is
   * deliberate — cues drawn from the fixture's own vocabulary would
   * measure the fixture.
   */
  val meeting: Vector[Cue] = Vector(
    // proposing: a modal with a first-person plural, or an offer of time
    Cue("shall we", "Proposal", 2.0),
    Cue("can we", "Proposal", 1.5),
    Cue("could we", "Proposal", 1.5),
    Cue("how about", "Proposal", 2.0),
    Cue("what if we", "Proposal", 2.0),
    Cue("let's", "Proposal", 2.0),
    Cue("lets ", "Proposal", 1.0),
    Cue("are you free", "Proposal", 2.0),
    Cue("would you be free", "Proposal", 2.0),
    Cue("does that work", "Proposal", 1.5),
    Cue("would.*work for", "Proposal", 1.5),
    Cue("i suggest", "Proposal", 2.0),
    Cue("i propose", "Proposal", 2.0),
    Cue("proposing", "Proposal", 2.0),
    Cue("i was thinking", "Proposal", 1.5),
    Cue("we could", "Proposal", 1.5),
    Cue("any chance of", "Proposal", 1.0),
    Cue("instead of", "Proposal", 0.5),

    // requesting: an imperative politeness marker, or a second-person modal
    Cue("please ", "Request", 1.5),
    Cue("could you", "Request", 2.0),
    Cue("can you", "Request", 2.0),
    Cue("would you mind", "Request", 2.0),
    Cue("would you be able", "Request", 2.0),
    Cue("i need", "Request", 1.5),
    Cue("send me", "Request", 1.5),
    Cue("let me know", "Request", 1.0),
    Cue("i would appreciate", "Request", 1.5),
    Cue("kindly", "Request", 1.5),

    // notifying: a first-person announcement, or a marker of no action
    Cue("fyi", "Notification", 2.5, atStart = true),
    Cue("just letting you know", "Notification", 2.5),
    Cue("heads up", "Notification", 2.5, atStart = true),
    Cue("please note", "Notification", 2.0, atStart = true),
    Cue("note that", "Notification", 2.0, atStart = true),
    Cue("i will be", "Notification", 1.5),
    Cue("i am no longer", "Notification", 1.5),
    Cue("has been", "Notification", 1.0),
    Cue("have been", "Notification", 1.0),
    Cue("is now", "Notification", 1.0),
    Cue("no action", "Notification", 2.0),
    Cue("nothing to do", "Notification", 2.0),
    Cue("reminder", "Notification", 1.5),
    Cue("for transparency", "Notification", 2.0, atStart = true),

    // out of domain: gratitude, greetings, and the register of support
    Cue("thank you", "Other", 2.0, atStart = true),
    Cue("thanks", "Other", 1.5, atStart = true),
    Cue("congratulations", "Other", 2.5, atStart = true),
    Cue("happy birthday", "Other", 3.0),
    Cue("i want a refund", "Other", 2.5),
    Cue("charged twice", "Other", 2.5),
    Cue("does not work", "Other", 1.0),
    Cue("crashes", "Other", 2.0),
    Cue("has expired", "Other", 2.0),
    Cue("not been delivered", "Other", 2.5),
    Cue("has not arrived", "Other", 2.5),
    Cue("i want to cancel my", "Other", 2.5))

  /**
   * Score a message: every cue that fires adds its weight to its
   * class, and the margin is the winner's share of the total, so it is
   * comparable across messages that fire different numbers of cues.
   */
  def score(cues: Vector[Cue], message: String): Option[Verdict] =
    val m = " " + message.toLowerCase.replaceAll("[\\n\\t]", " ") + " "
    val start = m.take(40)
    val hits = cues.filter { c =>
      val where = if c.atStart then start else m
      if c.phrase.contains(".*") then where.matches(s".*${c.phrase}.*") else where.contains(c.phrase)
    }
    if hits.isEmpty then None
    else
      val byClass = hits.groupBy(_.cls).map((cls, cs) => cls -> cs.map(_.weight).sum)
      val ranked = byClass.toSeq.sortBy(-_._2)
      val (best, s0) = ranked.head
      val total = ranked.map(_._2).sum
      val margin = if total <= 0.0 then 0.0 else (s0 - ranked.lift(1).map(_._2).getOrElse(0.0)) / total
      Some(Verdict(best, s0, margin, ranked.lift(1).map(_._1), hits.map(_.phrase)))

  /** the same contract as the other tiers: answer above the margin,
   * defer below it */
  def classify(cues: Vector[Cue], message: String, floor: Double = 0.3): Option[String] =
    score(cues, message).filter(_.margin >= floor).map(_.best)
}
