package okay.frame

/**
 * What a FORM is: named slots, typed answers, and what is still
 * missing (specs/conversation.md, specs/intent-classify.md).
 *
 * This module exists because two slot models appeared in one
 * repository in one day, and the consumer of the older one asked for
 * the merge rather than a rival. Each half had exactly what the other
 * lacked: `okay.intent` had the FRAME — typed values, an answer
 * addressed by name, a list of what is still unanswered —
 * `okay.agent.Conversation` had the SUSPENSION, a straight-line intake
 * parked in a journal across a restart. They are not competitors, and
 * neither module may depend on the other, so the shared half lives
 * here and depends on nothing.
 *
 * What this deliberately is NOT: a conversation. A frame describes and
 * holds; it does not know what has been asked, does not decide when to
 * ask, and cannot suspend. That belongs to `okay.agent.Conversation`,
 * over this.
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

/**
 * One slot: a NAME, a QUESTION per language, and a PARSER whose
 * failure is a re-ask rather than a silently stored string.
 *
 * `ask` is a map rather than a function `L => String` — which is what
 * the conversation runtime carried before the merge — for two
 * reasons, and the second is the load-bearing one. A map is DATA: a
 * service adds a language without a compiler, the same argument that
 * made `Taxon` a value. And a language that has to survive a RESTART
 * must be something that can be written down: an opaque `L` cannot go
 * in a journal, which is precisely why the old runtime had to store
 * every rendered question as text instead.
 */
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
                         extract: String => Option[Found[A]] = Slot.NothingFound):

  /** the question, in the reader's language, falling back to the one
   * language a slot must have */
  def question(lang: String): String =
    ask.getOrElse(lang, ask.getOrElse(Slot.Fallback, s"What is the $name?"))

  /** whether this slot can be asked in that language AT ALL, as
   * opposed to falling back to another one — a four-language intake
   * that quietly asks one question in English is a defect a caller
   * should be able to see before it ships */
  def speaks(lang: String): Boolean = ask.contains(lang)

  /**
   * Read an answer, or say what to ask again.
   *
   * A `Left` carries the QUESTION rather than an error string, because
   * the caller's next move is to ask it — an error message would have
   * to be translated into one at every call site.
   *
   * A slot takes a language here because a slot HAS none; it is a
   * descriptor. The only caller that passes one is `Frame`, out of the
   * language it pinned when the exchange began.
   */
  def read(lang: String, answer: String): Either[String, A] =
    parse(answer).toRight(question(lang))

object Slot:
  /** the language a slot must have a question in; everything else
   * falls back to it */
  val Fallback: String = "en"

  /** the default extractor, typed `Option[Nothing]` on purpose: a
   * default that mentioned `A` would be elaborated before `A` is known
   * and fix it to `Nothing`, so `Slot("n", asks, parse)` would not
   * compile without writing the type out */
  val NothingFound: String => Option[Nothing] = _ => None

/**
 * One answered slot: the slot, the text a person said, and the VALUE
 * it parsed to.
 *
 * The pair is the point. A frame that keeps only the text hands a
 * caller back the string it had just proved was a date, and the caller
 * parses it a SECOND time — with the same reference day, which nothing
 * in the type tells it to remember. Both halves of the merge had that
 * defect, in different files, and it is what `Answered` closes.
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
  /** a case class rather than an anonymous instance so that two frames
   * holding the same answers COMPARE equal — an anonymous class gives
   * identity equality, and a test that says "this intake ended with
   * that frame" then cannot be written */
  private final case class Ans[A](slot: Slot[A], text: String, value: A) extends Answered:
    type T = A

  def apply[A](s: Slot[A], t: String, v: A): Answered { type T = A } = Ans(s, t, v)

/**
 * A frame: the slots one intent needs before it can be acted on, the
 * answers so far, and THE LANGUAGE OF THE EXCHANGE.
 *
 * The language is a field and not a parameter, and that is an
 * operational finding rather than a preference. Both halves of the
 * merge took a language per call — `question(lang)`, `answer(name,
 * lang, text)` — which makes a mid-conversation flip possible BY
 * ACCIDENT: an intake that re-derives the language from each incoming
 * message switches it on a three-word reply, and the consumer measured
 * exactly that, on the second-to-last question of a profile. Here the
 * language is decided once, when the frame is built, and no method
 * takes another.
 */
final case class Frame[I](intent: I,
                          slots: Vector[Slot[?]],
                          lang: String = Slot.Fallback,
                          answers: Map[String, Answered] = Map.empty,
                          /** what was SAID for a slot whose parser
                           * could not read it — which is not the same
                           * thing as a mistake. See `said`. */
                          unread: Map[String, String] = Map.empty):

  /**
   * Pin the language of this exchange.
   *
   * Called ONCE, where the exchange begins, and never per message —
   * that is the whole discipline. It is a separate method rather than
   * a constructor argument only because `I` is often a `String` too,
   * and `Frame.of(intent, lang, slots*)` would be two strings in a row
   * at every call site.
   */
  def in(l: String): Frame[I] = copy(lang = l)

  def has(name: String): Boolean = answers.contains(name)

  /** the text of the answers that PARSED, for a frame being shown
   * back to them. Not everything the person said — see `words`. */
  def filled: Map[String, String] = answers.view.mapValues(_.text).toMap

  /**
   * The words said for a slot whose parser could not read them.
   *
   * NOT AN ESCAPE HATCH — the other half of the answer, and the first
   * consumer to migrate onto this frame said so from a live domain. A
   * price slot parses money; "negotiable", "по договорённости",
   * "договорімось" are things a listing legitimately says, and no
   * parser will ever read one. They are CONTENT, not a failure.
   *
   * Which is why the words are kept beside the slot instead of being
   * stored AS its value: a `Money` field holding the sentence
   * "negotiable" is a lie the type checker cannot see, and dropping
   * the sentence loses what the person told you. Read `words` for
   * both, and `valueOf` when only a parsed value will do.
   */
  def said(name: String): Option[String] = unread.get(name)

  /**
   * EVERYTHING the person said, parsed or not.
   *
   * The door the first migrating consumer had to write by hand, on
   * the day they found their read-back had lost a real answer. A
   * frame shown back to someone should show what they told you; only
   * the code that must have a typed value should care which half it
   * came from.
   */
  def words: Map[String, String] = filled ++ unread

  /**
   * The parsed value, at the type the slot promised.
   *
   * It takes the SLOT rather than a name, and that is the whole
   * mechanism: the slot is the evidence that this answer has type `A`,
   * so there is no way to ask for a type the slot never had. The match
   * on `a.slot eq s` is what makes the cast beneath it true — the
   * value was produced by THIS slot's own parser and by no other.
   *
   * Identity, not equality, which has a consequence worth stating: a
   * caller must HOLD the slots it built, as values. A `def` handing
   * back a fresh equal slot per call is a different slot, and the
   * answer will not be found — the first caller was caught by exactly
   * that.
   */
  def valueOf[A](s: Slot[A]): Option[A] =
    answers.get(s.name) match
      case Some(a) if a.slot eq s => Some(a.value.asInstanceOf[A])
      case _ => None

  /** the questions still to ask, in order, in the language of the
   * exchange */
  def missing: Vector[(String, String)] =
    slots.filter(s => s.required && !has(s.name)).map(s => s.name -> s.question(lang))

  /** how many questions are left — which a caller should not have to
   * count for itself, and had to */
  def remaining: Int = missing.length

  def complete: Boolean = missing.isEmpty

  /** the slots that cannot be asked in this frame's language and would
   * fall back to another one. Empty is the thing to assert in a test
   * before a language ships. */
  def untranslated: Vector[String] =
    slots.filterNot(_.speaks(lang)).map(_.name)

  /**
   * Take an answer to one slot.
   *
   * A parse failure returns the frame UNCHANGED with the question to
   * re-ask, which is the property a consumer asked for: a slot that
   * cannot read an answer must not store it. The alternative — keeping
   * the raw string and hoping — is how a frame ends up holding "next
   * thursday" in a field typed as a date.
   */
  def answer(name: String, text: String): Either[String, Frame[I]] =
    slots.find(_.name == name) match
      case None => Left(s"no slot named $name")
      case Some(s) => store(s, text)

  /**
   * An answer to one question that may answer OTHERS TOO.
   *
   * This is what a dialogue uses, and it is the fix for a live defect:
   * asked where, told "Wrocław, and remote works", the runtime took
   * the city and then asked about terms it had just been told. So the
   * named slot is answered and the SAME sentence is offered to every
   * other slot's extractor.
   *
   * Nothing fails here. A text the named slot cannot read is KEPT, as
   * words, and the slot stays unanswered — so a caller can ask again,
   * and can also show what was said. That is the older runtime's rule
   * ("a person who answers the same way twice means it") without its
   * cost, which was storing the unparsed words AS the value.
   */
  def take(name: String, text: String): Frame[I] =
    val answered = answer(name, text) match
      case Right(f) => f
      case Left(_) => heard(name, text)
    answered.fillFrom(text)

  /** keep what was said for a slot that could not read it */
  def heard(name: String, text: String): Frame[I] =
    copy(unread = unread.updated(name, text))

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
   * No language here, though `missing` renders questions in one.
   * Asking is addressed to a reader; extraction reads what is in front
   * of it, and today's one extractor is English. When that changes the
   * language will reach the extractor from this frame, which now
   * carries it.
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

  /** typed on its own, so the parsed value never loses its type on the
   * way into the map */
  private def store[A](s: Slot[A], text: String): Either[String, Frame[I]] =
    s.read(lang, text).map(v =>
      copy(answers = answers.updated(s.name, Answered(s, text, v)),
        unread = unread - s.name))

object Frame:
  /** the frame an intent needs, described once beside the taxonomy */
  def of[I](intent: I, slots: Slot[?]*): Frame[I] = Frame(intent, slots.toVector)
