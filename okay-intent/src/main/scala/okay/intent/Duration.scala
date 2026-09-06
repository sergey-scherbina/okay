package okay.intent

import okay.frame.Found

/**
 * Duration phrases to minutes (specs/intent-classify.md,
 * intent-extract-duration).
 *
 * The second slot with a parser and an extractor, after `Temporal`,
 * and built on the same two promises: TOTAL — `None` rather than a
 * guess, because a wrong length is booked while a refused one is
 * asked about — and DETERMINISTIC, a function of the phrase alone.
 *
 * The shapes are the ones a meeting is asked for in: a number and a
 * unit (`30 minutes`, `45 min`, `2 hours`, `1.5 h`, `90m`, `1h30`),
 * the spoken ones (`an hour`, `half an hour`, `a quarter of an hour`,
 * `an hour and a half`, `two hours`, `twenty minutes`). Anything else
 * — `a while`, `all day`, `a couple of hours` — is `None`. English
 * words; the other languages' number-and-unit words are a lexicon
 * each, as `Temporal`'s were, and are filed.
 */
object Duration {

  private val words = Map(
    "a" -> 1, "an" -> 1, "one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4, "five" -> 5, "six" -> 6,
    "seven" -> 7, "eight" -> 8, "nine" -> 9, "ten" -> 10, "eleven" -> 11, "twelve" -> 12,
    "fifteen" -> 15, "twenty" -> 20, "thirty" -> 30, "forty" -> 40, "forty-five" -> 45, "fifty" -> 50,
    "sixty" -> 60, "ninety" -> 90)

  private val number = raw"(\d+(?:[.,]\d+)?)".r
  private val hm = raw"(\d{1,2})h(\d{2})".r            // 1h30
  private val glued = raw"(\d+(?:[.,]\d+)?)(h|hr|hrs|m|min|mins)".r  // 2h, 90m, 45min

  private def isHours(w: String): Boolean =
    w == "h" || w == "hr" || w == "hrs" || w == "hour" || w == "hours"
  private def isMinutes(w: String): Boolean =
    w == "m" || w == "min" || w == "mins" || w == "minute" || w == "minutes"

  private def tokens(phrase: String): List[String] =
    phrase.toLowerCase.replaceAll("[!?;:()]", " ")
      // a dot or comma BETWEEN digits is a decimal (1.5, 1,5); anywhere
      // else it is punctuation ("45 minutes, in room 4")
      .replaceAll("[.,](?!\\d)", " ").replaceAll("(?<!\\d)[.,]", " ")
      .replaceAll("(\\d)-(?=[a-z])", "$1 ")   // "2-hour" is a number and a unit; "forty-five" is a word
      .split("\\s+").filter(_.nonEmpty).toList

  private def quantity(w: String): Option[Double] =
    w match
      case number(n) => n.replace(',', '.').toDoubleOption
      case _ => words.get(w).map(_.toDouble)

  /** minutes, or `None` */
  def parse(phrase: String): Option[Int] =
    val ts = tokens(phrase)
    if ts.isEmpty then None
    else
      val set = ts.toSet
      // the spoken fractions first: they are whole phrases, not a
      // number beside a unit
      if ts.containsSlice(List("quarter", "of", "an", "hour")) || ts.containsSlice(List("quarter", "hour")) then Some(15)
      else if ts.containsSlice(List("half", "an", "hour")) || ts.containsSlice(List("half", "hour")) then
        // "an hour and a half" is caught below; this is "half an hour"
        Some(30)
      else
        // 1h30 / 2h / 90m / 45min as one token
        val direct = ts.collectFirst {
          case hm(h, m) if m.toInt < 60 => h.toInt * 60 + m.toInt
          case glued(n, u) => n.replace(',', '.').toDouble match
            case q if isHours(u) => math.round(q * 60).toInt
            case q => math.round(q).toInt
        }.orElse {
          // a number (or number word) followed by a unit; "and a half" adds thirty
          val i = ts.indices.collectFirst {
            case i if i + 1 < ts.length && quantity(ts(i)).isDefined && (isHours(ts(i + 1)) || isMinutes(ts(i + 1))) => i
          }
          i.flatMap { i =>
            val q = quantity(ts(i)).get
            val hours = isHours(ts(i + 1))
            val base = if hours then q * 60 else q
            val half = hours && ts.drop(i + 2).take(3) == List("and", "a", "half")
            val total = math.round(base).toInt + (if half then 30 else 0)
            if total > 0 && total <= 24 * 60 then Some(total) else None
          }
        }
        direct.orElse(
          if set.contains("hour") && ts.containsSlice(List("and", "a", "half")) then Some(90) else None)

  /**
   * The same answer as `parse`, plus the words it rests on — the
   * shortest window of words reproducing the whole message's value,
   * exactly as `Temporal.find` (read its comment for why the value is
   * the whole message's and only the evidence is searched for).
   */
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
