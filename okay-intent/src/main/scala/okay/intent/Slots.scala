package okay.intent

import okay.frame.{Found, Slot}

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
  /** how long, in minutes — `Duration`'s parser and extractor, the
   * question in the languages `when` asks in */
  val duration: Slot[Int] =
    Slot(
      name = "duration",
      ask = Map(
        "en" -> "How long should the meeting be?",
        "fr" -> "Combien de temps devrait durer la réunion ?",
        "de" -> "Wie lange soll die Besprechung dauern?",
        "es" -> "¿Cuánto debería durar la reunión?",
        "ru" -> "Сколько времени займёт встреча?",
        "ja" -> "会議はどのくらいの長さにしますか。"),
      parse = s => Duration.parse(s),
      extract = s => Duration.find(s),
      show = (v, _) => if v % 60 == 0 then s"${v / 60}h" else if v > 60 then s"${v / 60}h${v % 60}" else s"${v}min")

  /** how many people — `People`'s parser and extractor */
  val people: Slot[Int] =
    Slot(
      name = "people",
      ask = Map(
        "en" -> "How many people will attend?",
        "fr" -> "Combien de personnes participeront ?",
        "de" -> "Wie viele Personen nehmen teil?",
        "es" -> "¿Cuántas personas asistirán?",
        "ru" -> "Сколько человек будет?",
        "ja" -> "何名で参加されますか。"),
      parse = s => People.parse(s),
      extract = s => People.find(s))

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
