package okay.dlm

/**
 * Yes or no, read narrowly on purpose (specs/dlm.md, "Polarity").
 *
 * A pending confirmation is answered by a short word, not by parsing
 * arbitrary agreement out of a sentence — «не, лучше по-другому» needs
 * to fall through to a fresh request rather than be misread as a bare
 * «no» that then gets stuck asking the same question again. So only a
 * WHOLE, short reply counts; anything else is read as the person
 * having moved on.
 *
 * The WORDS are the caller's, per language, in one value; the reading
 * is here. A rule reads these forms exactly, and a vector head reads
 * them worst — a bare «?» carries no words at all — which is why the
 * rule runs first and the head answers only where it is silent.
 */
final class Confirm(val words: Confirm.Words):
  import Confirm.*

  private def split(s: String): Vector[String] =
    s.toLowerCase.split("[^\\p{L}]+").toVector.filter(_.nonEmpty)

  /**
   * A CONTROL WORD: a reply that steers the conversation and carries
   * no answer of its own — a yes, a no, a courtesy, a cancel. A
   * control word stored as an answer is the defect this exists to
   * prevent: «отмена» became somebody's job title before cancel
   * existed. The WHOLE reply must be control words — «да, сантехник»
   * answers the question and is not one.
   */
  def control(text: String): Boolean =
    val ws = split(text)
    ws.nonEmpty && ws.forall(w => words.yes(w) || words.no(w) || words.polite(w) || words.steer(w))

  /** `None` means "not a yes/no at all" — the caller reads that as a
   * brand new message, not a shrug. Short only: agreement is short,
   * and a sentence with a yes buried in it is a sentence. A reply
   * counts when every word in it is either an ANSWER or a COURTESY,
   * so «yes please» is a yes */
  def of(text: String): Option[Boolean] =
    val ws = split(text)
    if ws.isEmpty || ws.length > 4 then None
    else
      val saidYes = ws.exists(words.yes)
      val saidNo = ws.exists(words.no)
      val onlyAnswerAndCourtesy = ws.forall(w => words.yes(w) || words.no(w) || words.polite(w))
      if !onlyAnswerAndCourtesy || saidYes == saidNo then None else Some(saidYes)

  /**
   * A yes or no that OPENS the message, with more after it. «Да,
   * только а где про то что Scala?» is a yes and a question: `of`
   * rightly refuses a sentence with a yes buried in it; this reads one
   * that LEADS with it, and hands back the rest so the caller can
   * answer that too.
   */
  def leading(text: String): Option[(Boolean, String)] =
    val ws = split(text)
    ws.headOption.flatMap { first =>
      val next = ws.slice(1, 3)
      if words.yes(first) && !next.exists(words.no) then Some(true)
      else if words.no(first) && !next.exists(words.yes) then Some(false)
      else None
    }.map { polarity =>
      val tail = "^\\s*\\S+[\\s,.!;:—-]*".r.replaceFirstIn(text.trim, "").trim
      (polarity, tail)
    }

  /**
   * WHAT WAS ANSWERED to a question the caller asked — three meanings,
   * not two. «написать ему?» is answered «да», «нет», and «а что там?»;
   * the third is what makes the other two honest, since a person who
   * cannot see what turned up cannot agree to it.
   *
   * A rule's reading, or `None` for a head: a bare question mark is a
   * question, and the only question standing is the caller's; up to
   * four words with a tell-word among them is «tell me more».
   */
  def rule(text: String): Option[Answer] =
    val t = text.trim
    if t.nonEmpty && t.forall(c => c == '?' || c.isWhitespace) then Some(Answer.Tell)
    else
      val ws = split(t).filterNot(words.leadIn)
      if ws.nonEmpty && ws.length <= 4 && ws.exists(words.tell) &&
        !ws.exists(words.yes) && !ws.exists(words.no) then Some(Answer.Tell)
      else of(t).map(if _ then Answer.Yes else Answer.No)
        .orElse(leading(t).map(p => if p._1 then Answer.Yes else Answer.No))

  /** the rules first, then a head answering "yes" / "no" / "tell" —
   * the order the router uses, and for the same reason */
  def answer(text: String, head: String => Option[String]): Option[Answer] =
    rule(text).orElse(head(text).flatMap(Answer.parse))

  /**
   * THE OFFER'S OWN VERB IS A YES. «могу добавить как источник.
   * Добавить?» — «Добавь». A question that NAMES AN ACT invites the
   * act's own word back; reading only «да» treats the most natural
   * answer as a new subject.
   *
   * The same discipline `of` keeps: the WHOLE reply is the answer —
   * every word the act, what the act is ABOUT, a yes, or a courtesy.
   * Only the act triggers it, and nothing here reads a no, because
   * refusing an offer is `of`'s job and an offer not taken lapses.
   */
  def taking(text: String, act: Set[String], about: Set[String] = Set.empty): Boolean =
    val ws = split(text)
    ws.nonEmpty && ws.length <= 4 && ws.exists(act.contains) &&
      !ws.exists(words.no) &&
      ws.forall(w => act.contains(w) || about.contains(w) || words.yes(w) || words.polite(w))

object Confirm:

  /** the three meanings of an answer to a yes/no question */
  enum Answer:
    case Yes, No, Tell

  object Answer:
    def parse(s: String): Option[Answer] = s match
      case "yes" => Some(Yes)
      case "no" => Some(No)
      case "tell" => Some(Tell)
      case _ => None

  /**
   * The caller's vocabulary, every language in one set: a person in a
   * Russian conversation still types «ok» sometimes.
   *
   * @param yes    agreement
   * @param no     refusal
   * @param polite courtesy that carries no answer («please», «thanks»)
   * @param steer  cancelling and stopping: not an answer either
   * @param tell   the question words of «tell me more» («что», «кто»)
   * @param leadIn particles a tell-question may open with («а», «ну»)
   */
  final case class Words(yes: Set[String], no: Set[String],
                         polite: Set[String] = Set.empty, steer: Set[String] = Set.empty,
                         tell: Set[String] = Set.empty, leadIn: Set[String] = Set.empty)

  /** English alone, so the reader works before anybody authored a
   * word: a caller adds its own languages beside it */
  val english: Words = Words(
    yes = Set("yes", "yeah", "yep", "sure", "correct", "right", "ok", "okay"),
    no = Set("no", "nope", "incorrect", "wrong"),
    polite = Set("please", "thanks", "thank", "you", "of", "course"),
    steer = Set("cancel", "stop"),
    tell = Set("what", "who", "which", "where", "how", "details", "show", "more"),
    leadIn = Set("and", "so", "well", "but", "ok"))

  def apply(words: Words): Confirm = new Confirm(words)
