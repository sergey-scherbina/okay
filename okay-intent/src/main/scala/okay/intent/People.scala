package okay.intent

import okay.frame.Found

/**
 * How many people (specs/intent-classify.md, intent-extract-people).
 *
 * The third parsed slot, on `Temporal`'s two promises — total,
 * deterministic — for the count a meeting frame asks for after when
 * and how long: `a room for four people`, `4 people`, `six of us`, `a
 * team of 5`; and in the fixture's other languages `quatre
 * personnes`, `vier Personen`, `cuatro personas`, `на четверых`,
 * `4人用`, `на чотирьох`, `dla czterech osób`. A count needs a
 * PEOPLE-WORD beside it — `for 4` is not a count of anything — except
 * in the Slavic languages, whose collective numerals (`четверых`,
 * `чотирьох`, `czterech` after `dla`) count people by themselves.
 * Anything else, and any count outside 1..1000, is `None`.
 */
object People {

  private val digits = raw"(\d{1,4})".r
  private val ja = raw"(\d{1,4})(?:人|名)".r

  private final case class Lexicon(numbers: Map[String, Double], people: Seq[String],
                                   collective: Map[String, Int] = Map.empty, after: Seq[List[String]] = Nil)

  /** the words a count of people stands beside, as token prefixes */
  private val en = Lexicon(
    Map("a" -> 1.0, "an" -> 1.0, "one" -> 1.0, "two" -> 2.0, "three" -> 3.0, "four" -> 4.0, "five" -> 5.0,
      "six" -> 6.0, "seven" -> 7.0, "eight" -> 8.0, "nine" -> 9.0, "ten" -> 10.0, "eleven" -> 11.0,
      "twelve" -> 12.0, "fifteen" -> 15.0, "twenty" -> 20.0, "thirty" -> 30.0, "forty" -> 40.0, "fifty" -> 50.0),
    people = Seq("people", "person", "attendee", "participant", "guest", "colleague", "member", "seat"),
    after = Seq(List("of", "us"), List("of", "them")))
  private val fr = Lexicon(Numbers.fr, Seq("personne", "participant", "invité", "collègue", "place"))
  private val de = Lexicon(Numbers.de, Seq("person", "teilnehmer", "leute", "kolleg", "gäste", "gast", "plätze", "platz"))
  private val es = Lexicon(Numbers.es, Seq("persona", "participante", "invitado", "asistente", "plaza"))
  private val ru = Lexicon(Numbers.ru, Seq("человек", "людей", "участник", "гост", "мест", "чел"),
    collective = Map("двоих" -> 2, "троих" -> 3, "четверых" -> 4, "пятерых" -> 5, "шестерых" -> 6, "семерых" -> 7,
      "восьмерых" -> 8, "девятерых" -> 9, "десятерых" -> 10,
      "двое" -> 2, "трое" -> 3, "четверо" -> 4, "пятеро" -> 5, "шестеро" -> 6, "семеро" -> 7))
  private val uk = Lexicon(Numbers.uk, Seq("осіб", "особ", "людей", "людин", "учасник", "гост", "місц"),
    collective = Map("двох" -> 2, "трьох" -> 3, "чотирьох" -> 4, "п'ятьох" -> 5, "п’ятьох" -> 5, "шістьох" -> 6,
      "сімох" -> 7, "вісьмох" -> 8, "дев'ятьох" -> 9, "дев’ятьох" -> 9, "десятьох" -> 10,
      "двоє" -> 2, "троє" -> 3, "четверо" -> 4, "п'ятеро" -> 5, "п’ятеро" -> 5, "шестеро" -> 6))
  private val pl = Lexicon(Numbers.pl ++ Map("dwóch" -> 2.0, "trzech" -> 3.0, "czterech" -> 4.0, "pięciu" -> 5.0,
      "sześciu" -> 6.0, "siedmiu" -> 7.0, "ośmiu" -> 8.0, "dziewięciu" -> 9.0, "dziesięciu" -> 10.0),
    Seq("osób", "osob", "uczestnik", "gości", "gość", "miejsc"))
  private val lexicons = Vector(en, fr, de, es, ru, uk, pl)

  private def tokens(phrase: String): List[String] =
    phrase.toLowerCase.replaceAll("[.,!?;:()¿¡«»\"]", " ").split("\\s+").filter(_.nonEmpty).toList

  private def is(tok: String, forms: Seq[String]): Boolean = forms.exists(f => tok.startsWith(f))

  private def count(ts: List[String], lex: Lexicon): Option[Int] =
    def quantity(w: String): Option[Int] = w match
      case digits(n) => Some(n.toInt)
      case _ => lex.numbers.get(w).map(d => math.round(d).toInt).orElse(lex.collective.get(w))
    val beside = ts.indices.iterator.flatMap { i =>
      quantity(ts(i)).flatMap { n =>
        // "four people" / "vier Personen"; "four of us"; "a team of 5" reads
        // the people-word BEFORE the number
        val next = ts.lift(i + 1)
        val afterOk = lex.after.exists(f => ts.drop(i + 1).startsWith(f))
        val prevIsPeople = i > 0 && is(ts(i - 1), lex.people) && ts.lift(i - 1).exists(_ != "of")
        val teamOf = i > 1 && ts(i - 1) == "of" && is(ts(i - 2), Seq("team", "group", "party"))
        if next.exists(w => is(w, lex.people)) || afterOk || prevIsPeople || teamOf then Some(n) else None
      }
    }.nextOption()
    beside.orElse(ts.collectFirst { case w if lex.collective.contains(w) => lex.collective(w) })
      .filter(n => n >= 1 && n <= 1000)

  /** a count of people, or `None` */
  def parse(phrase: String): Option[Int] =
    val ts = tokens(phrase)
    if ts.isEmpty then None
    else lexicons.iterator.map(lex => count(ts, lex)).collectFirst { case Some(n) => n }
      .orElse(ja.findFirstMatchIn(phrase).map(_.group(1).toInt).filter(n => n >= 1 && n <= 1000))

  /** the same answer as `parse`, plus the shortest window of words
   * reproducing it — the evidence rule of `Temporal.find` */
  def find(message: String): Option[Found[Int]] =
    parse(message).map { v =>
      val toks = message.split("\\s+").filter(_.nonEmpty)
      val windows =
        for len <- 1 to toks.length; i <- 0 to toks.length - len yield (i, len)
      val span = windows
        .find((i, len) => parse(toks.slice(i, i + len).mkString(" ")).contains(v))
        .map((i, len) => toks.slice(i, i + len).mkString(" "))
        .getOrElse(message)
      Found(span.replaceAll("^[\\p{Punct}]+|[\\p{Punct}]+$", "").trim, v)
    }
}
