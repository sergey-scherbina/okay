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
/**
 * A value together with the SPAN of the message it came from.
 *
 * The span is not decoration. A value a person TYPED needs no
 * evidence — they can see what they wrote. A value taken out of a
 * sentence they wrote for another purpose has to be echoable back
 * ("Thursday 10 Sep — right?"), and the whole message is not an echo.
 */
final case class Found[A](text: String, value: A)

final case class Slot[A](name: String,
                         ask: Map[String, String],
                         parse: String => Option[A],
                         required: Boolean = true,
                         /**
                          * Find this slot's value IN a message that
                          * was not written to answer anything.
                          *
                          * Defaults to finding nothing, which is the
                          * common case and keeps a plain slot two
                          * lines long. A slot that can extract says
                          * so; a slot that cannot stays silent rather
                          * than guessing.
                          */
                         extract: String => Option[Found[A]] = (_: String) => None):
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
 * One answered slot: the slot, the text a person typed, and the VALUE
 * it parsed to.
 *
 * The pair is the point. `Frame` used to keep only the text, so a
 * caller that had just proved "next thursday" was an acceptable date
 * got the string back and parsed it a SECOND time — with the same
 * reference day, which nothing in the type told it to remember. The
 * first caller, `okay.demo.IntentRouter`, demonstrated exactly that
 * and the demonstration is what this replaces.
 *
 * The abstract type is what lets one frame hold answers of different
 * types without a cast at the call site: each `Answered` remembers the
 * slot its value came from, and `valueOf` asks with that slot.
 */
sealed trait Answered:
  type T
  def slot: Slot[T]
  def text: String
  def value: T

object Answered:
  def apply[A](s: Slot[A], t: String, v: A): Answered { type T = A } =
    new Answered:
      type T = A
      val slot = s
      val text = t
      val value = v

/**
 * A frame: the slots one intent needs before it can be acted on.
 *
 * `missing` is the whole point of the type. A classifier says
 * `Proposal`; a caller cannot act until it knows WHEN, and the
 * difference between "I have a class" and "I can act" is a list of
 * unanswered questions rather than a boolean.
 */
final case class Frame[I](intent: I, slots: Vector[Slot[?]],
                          answers: Map[String, Answered] = Map.empty):

  def has(name: String): Boolean = answers.contains(name)

  /** what a person typed, for a frame being shown back to them */
  def filled: Map[String, String] = answers.view.mapValues(_.text).toMap

  /**
   * The parsed value, at the type the slot promised.
   *
   * It takes the SLOT rather than a name, and that is the whole
   * mechanism: the slot is the evidence that this answer has type `A`,
   * so there is no way to ask for a type the slot never had. The match
   * on `a.slot eq s` is what makes the cast beneath it true — the
   * value was produced by THIS slot's own parser and by no other.
   */
  def valueOf[A](s: Slot[A]): Option[A] =
    answers.get(s.name) match
      case Some(a) if a.slot eq s => Some(a.value.asInstanceOf[A])
      case _ => None

  /**
   * Fill what the message itself already says.
   *
   * This is the difference between a classifier and a router that can
   * act. "Are you free Wednesday afternoon?" is a proposal WITH its
   * `when` in it, and a frame that can only be asked will ask the
   * person who just said.
   *
   * An answered slot is never overwritten: a person's own reply
   * outranks a guess about their earlier sentence, and an extractor
   * that could revise an answer would make the order of calls matter.
   *
   * No `lang` here, though `missing` takes one. Asking is addressed to
   * a reader and must be in their language; extraction reads what is
   * in front of it, and today's one extractor is `Temporal`, which is
   * English. When that changes the language will reach the extractor
   * as an argument — adding it now would be a parameter every slot
   * ignores, which is how a signature starts lying.
   */
  def fillFrom(message: String): Frame[I] =
    slots.foldLeft(this)((f, s) => f.extracted(s, message))

  /** typed on its own, for the same reason `store` is: the found value
   * keeps its type on the way into the map */
  private def extracted[A](s: Slot[A], message: String): Frame[I] =
    if has(s.name) then this
    else s.extract(message) match
      case Some(f) => copy(answers = answers.updated(s.name, Answered(s, f.text, f.value)))
      case None => this

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
      case Some(s) => store(s, lang, text)

  /** typed on its own, so the parsed value never loses its type on the
   * way into the map */
  private def store[A](s: Slot[A], lang: String, text: String): Either[String, Frame[I]] =
    s.read(lang, text).map(v => copy(answers = answers.updated(s.name, Answered(s, text, v))))

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
      parse = s => Temporal.parse(s, today),
      extract = s => Temporal.find(s, today))
  /** a plain text slot, for the frames whose fields are not parsed at
   * all — most of them, and there is no shame in it */
  def text(name: String, ask: Map[String, String], required: Boolean = true,
           fromMessage: Boolean = false): Slot[String] =
    val read = (s: String) => Option.when(s.trim.nonEmpty)(s.trim)
    // `fromMessage` is for the frames where the request IS the
    // message: a "what would you like done?" asked of someone who has
    // just written a paragraph saying so is a question about nothing.
    // It is opt-in because the opposite frame — a field the message
    // happens not to mention — must keep asking.
    Slot(name, ask, read, required,
      extract = if fromMessage then s => read(s).map(v => Found(v, v)) else _ => None)
