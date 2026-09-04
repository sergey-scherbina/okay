package okay.intent

/**
 * What a slot IS (specs/intent-classify.md).
 *
 * The Overview of this feature promises that a label cannot be acted
 * on and a filled FRAME can — and until now no type held a frame.
 * `Temporal` parses one slot in one language and nothing said what a
 * slot is, so a second language was a rewrite and a learned tagger
 * would have been a separate design.
 *
 * The shape is a consumer's, arrived at from having built one: a slot
 * is a NAME, a QUESTION to ask when it is unanswered, and a PARSER
 * whose failure is a re-ask rather than a silently stored string.
 * Under that description `Temporal` is one parser among several,
 * another language is another `ask` entry and another parser, and the
 * CRF lane becomes an alternative implementation of `parse` instead of
 * a rival design.
 *
 * What this deliberately is NOT: a conversation. The descriptor
 * describes; it does not hold state, does not know what has been asked,
 * and does not decide when to ask. A classifier that is a pure function
 * of a message stays testable, cacheable and evaluable as a fold, and
 * that property is worth more than the convenience of putting a
 * dialogue here.
 */
final case class Slot[A](name: String,
                         ask: Map[String, String],
                         parse: String => Option[A],
                         required: Boolean = true):
  /** the question, in the reader's language, falling back to the one
   * language a slot must have */
  def question(lang: String): String =
    ask.getOrElse(lang, ask.getOrElse(Slot.Fallback, s"What is the $name?"))
  /**
   * Read an answer, or say what to ask again.
   *
   * A `Left` carries the QUESTION rather than an error string, because
   * the caller's next move is to ask it — an error message would have
   * to be translated into one at every call site.
   */
  def read(lang: String, answer: String): Either[String, A] =
    parse(answer).toRight(question(lang))
object Slot:
  /** the language a slot must have a question in; everything else
   * falls back to it */
  val Fallback: String = "en"
/**
 * A frame: the slots one intent needs before it can be acted on.
 *
 * `missing` is the whole point of the type. A classifier says
 * `Proposal`; a caller cannot act until it knows WHEN, and the
 * difference between "I have a class" and "I can act" is a list of
 * unanswered questions rather than a boolean.
 */
final case class Frame[I](intent: I, slots: Vector[Slot[?]],
                          filled: Map[String, String] = Map.empty):
  def has(name: String): Boolean = filled.contains(name)
  /** the questions still to ask, in order, in the reader's language */
  def missing(lang: String = Slot.Fallback): Vector[(String, String)] =
    slots.filter(s => s.required && !has(s.name)).map(s => s.name -> s.question(lang))
  def complete(lang: String = Slot.Fallback): Boolean = missing(lang).isEmpty
  /**
   * Take an answer to one slot.
   *
   * A parse failure returns the frame UNCHANGED with the question to
   * re-ask, which is the property the consumer asked for: a slot that
   * cannot read an answer must not store it. The alternative — keeping
   * the raw string and hoping — is how a frame ends up holding "next
   * thursday" in a field typed as a date.
   */
  def answer(name: String, lang: String, text: String): Either[String, Frame[I]] =
    slots.find(_.name == name) match
      case None => Left(s"no slot named $name")
      case Some(s) => s.read(lang, text).map(_ => copy(filled = filled.updated(name, text)))
object Frame:
  /** the frame an intent needs, described once beside the taxonomy */
  def of[I](intent: I, slots: Slot[?]*): Frame[I] = Frame(intent, slots.toVector)
/**
 * The slots this programme already has parsers for.
 *
 * `when` is `Temporal` wearing the descriptor: the same total,
 * deterministic parser, now one implementation of a named seam rather
 * than a special case. Its reference day is an argument, so a slot
 * built for a conversation happening today is a different value from
 * one built yesterday — which is the honest shape, and the reason this
 * is a function rather than a constant.
 */
object Slots:
  def when(today: Temporal.Date): Slot[Temporal.When] =
    Slot(
      name = "when",
      ask = Map(
        "en" -> "When would you like to meet?",
        "fr" -> "Quand souhaitez-vous nous voir ?",
        "de" -> "Wann möchten Sie sich treffen?",
        "es" -> "¿Cuándo le gustaría que nos reuniéramos?",
        "ru" -> "Когда вам удобно встретиться?",
        "ja" -> "いつがご都合よろしいですか。"),
      parse = s => Temporal.parse(s, today))
  /** a plain text slot, for the frames whose fields are not parsed at
   * all — most of them, and there is no shame in it */
  def text(name: String, ask: Map[String, String], required: Boolean = true): Slot[String] =
    Slot(name, ask, s => Option.when(s.trim.nonEmpty)(s.trim), required)
