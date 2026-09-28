package okay.dlm

import okay.rag.Embedding

/** one lesson: the sentence as the person wrote it, the intent their
 * restatement reached, the journal offset of the teaching turn, and
 * whose lesson it is. NO SLOTS: the intent's own patterns read them
 * from the sentence at hand (`Router`) */
final case class Lesson(text: String, intent: String, offset: Long, who: String)

/**
 * WHAT THE PERSON TAUGHT, folded from the journal (specs/dlm.md,
 * "Memory").
 *
 * «Надо было: мои заявки» after a misroute pairs the earlier sentence
 * with the intent the restatement reached, and from the next turn on
 * that person's same words — or a typo away — route to it ahead of
 * the rules. Their lesson and not everyone's, until enough people
 * teach the same pair; a withdrawal takes it back; a restart folds it
 * back from the log.
 *
 * A PROJECTION OF THE LOG, never a file. `of` is a pure function of
 * the events below an offset, so a boot arms exactly what the log
 * says, and a replay that re-performs recorded actions learns
 * nothing.
 */
final case class Memory(mine: Map[String, Vector[Lesson]], shared: Vector[Lesson]):
  def isEmpty: Boolean = mine.isEmpty && shared.isEmpty
  /** the lessons a message from this person is compared with: theirs
   * first, then the ones enough people taught */
  def forPerson(who: String): Vector[Lesson] = mine.getOrElse(who, Vector.empty) ++ shared
  def size: Int = mine.values.map(_.length).sum

object Memory:

  val empty: Memory = Memory(Map.empty, Vector.empty)

  /** what the journal says about lessons, in journal order */
  enum Event:
    /** a person restated, and their restatement fired `intent` */
    case Taught(who: String, earlier: String, intent: String, offset: Long, at: Long)
    /** «не запоминай»: append-only, and the fold honours it */
    case Withdrawn(who: String, earlier: String)

  /**
   * The fold's parameters, each with the asymmetry that sets it.
   *
   * @param since   records before this instant are not lessons — a
   *                `taught` written by code that armed a label with no
   *                lifetime is not one
   * @param perPerson how many lessons one person may hold, oldest out:
   *                every per-person map is bounded, because the caller
   *                supplies the key
   * @param people  how many distinct people must teach the same pair
   *                before it serves everyone
   * @param teacher a role that shares a lesson on its own — waiting for
   *                two more people to agree with a trusted teacher would
   *                be theatre, not safety
   * @param control a reply that steers and carries no answer («да»,
   *                «отмена») teaches nothing — the caller's own word list
   */
  final case class Rules(since: Long = 0L, perPerson: Int = 64, people: Int = 3,
                         teacher: String => Boolean = _ => false,
                         control: String => Boolean = _ => false)

  val defaults: Rules = Rules()

  /** the words of a sentence, lowercased — what the exact band compares */
  def words(text: String): Vector[String] = Fuzzy.tokenize(text).map(_.text.toLowerCase)

  /** a sentence as the fold keys it: its words, one space apart, so
   * «Мои заявки!» and «мои  заявки» are one lesson */
  def key(text: String): String = words(text).mkString(" ")

  /** is this earlier sentence something a person can TEACH: not a
   * control word, and at least three letters of it */
  def teachable(earlier: String, control: String => Boolean = _ => false): Boolean =
    key(earlier).count(_.isLetter) >= 3 && !control(earlier)

  /**
   * THE FOLD.
   *
   *  - `Taught` adds; `Withdrawn` removes;
   *  - the latest teaching of one sentence wins, so twenty-five
   *    identical records are one lesson and a person may re-teach;
   *  - nothing before `since` is armed;
   *  - a person holds at most `perPerson`, oldest out;
   *  - a pair that `people` distinct persons taught to the same
   *    intent is SHARED and serves everyone, exact band only.
   */
  def of(events: Iterable[Event], rules: Rules = defaults): Memory =
    events.foldLeft(empty) {
      case (m, Event.Withdrawn(who, earlier)) => forget(m, who, earlier, rules)
      case (m, Event.Taught(who, earlier, intent, offset, at)) =>
        if at >= rules.since then learn(m, who, earlier, intent, offset, rules) else m
    }

  /** ONE LESSON APPLIED — the fold advanced by one record, which is
   * also what a live turn does the moment its record is appended */
  def learn(m: Memory, who: String, earlier: String, intent: String, offset: Long,
            rules: Rules = defaults): Memory =
    if !teachable(earlier, rules.control) then m
    else
      val k = key(earlier)
      val kept = m.mine.getOrElse(who, Vector.empty).filterNot(p => key(p.text) == k)
      val mine = m.mine.updated(who, (kept :+ Lesson(earlier.trim, intent, offset, who)).takeRight(rules.perPerson))
      Memory(mine, sharedOf(mine, rules))

  /** the lesson for these words is gone for this person, and a lesson
   * only they held stops being shared */
  def forget(m: Memory, who: String, earlier: String, rules: Rules = defaults): Memory =
    val k = key(earlier)
    val mine = m.mine.get(who).map(_.filterNot(p => key(p.text) == k)) match
      case Some(ps) if ps.nonEmpty => m.mine.updated(who, ps)
      case Some(_) => m.mine - who
      case None => m.mine
    Memory(mine, sharedOf(mine, rules))

  private def sharedOf(mine: Map[String, Vector[Lesson]], rules: Rules): Vector[Lesson] =
    mine.values.flatten.toVector
      .groupBy(p => (key(p.text), p.intent))
      .collect { case (_, ps) if ps.map(_.who).distinct.length >= rules.people || ps.exists(p => rules.teacher(p.who)) =>
        ps.minBy(_.offset) }
      .toVector.sortBy(_.offset)

  /** the most edits a whole sentence may absorb and still be «the same
   * words»: the tolerance of one long word, because this band sits
   * BEFORE the rules and a lesson applied to a different sentence is
   * worse than a lesson not applied */
  val maxEdits: Int = 2

  /**
   * THE EXACT BAND: the same words, or each word within
   * `Fuzzy.tolerance` of its counterpart with at most `maxEdits` in
   * all — the same number of words, in the same order. This person's
   * lessons first, then the shared ones; the nearest wins. The number
   * is 1.0 for the same words and `1.0 - 0.2·d` for d edits, the
   * convention `Support.Typo` already writes on the wire.
   */
  def exact(m: Memory, who: String, text: String): Option[(Lesson, Float)] =
    val ws = words(text)
    if ws.isEmpty then None
    else
      def near(p: Lesson): Option[Float] =
        val pw = words(p.text)
        if pw.length != ws.length then None
        else
          var total = 0
          var i = 0
          var ok = true
          while ok && i < ws.length do
            val a = ws(i)
            val b = pw(i)
            if a != b then
              val tol = Fuzzy.tolerance(math.min(a.length, b.length))
              val d = Fuzzy.distance(a, b, tol)
              if d > tol then ok = false else total += d
            i += 1
          if ok && total <= maxEdits then Some(1.0f - 0.2f * total) else None
      m.forPerson(who).flatMap(p => near(p).map(p -> _)).maxByOption(_._2)

  /**
   * THE NEAR BAND: this person's OWN lessons, to the intents the
   * vector layer may reach, by cosine against `bar`. Their own and not
   * the shared ones: a paraphrase is a weaker claim than the same
   * words, and a bar measured on one person's phrasing is theirs.
   */
  def near(m: Memory, who: String, text: String, embed: String => Embedding,
           semantic: String => Boolean, bar: Float): Option[(Lesson, Float)] =
    val tv = okay.intent.Centroid.normalise(embed(text))
    m.mine.getOrElse(who, Vector.empty)
      .filter(p => semantic(p.intent))
      .map(p => p -> okay.intent.Centroid.dot(tv, okay.intent.Centroid.normalise(embed(p.text))).toFloat)
      .filter(_._2 >= bar).maxByOption(_._2)
