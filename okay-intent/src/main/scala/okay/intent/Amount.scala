package okay.intent

import okay.frame.Found

/** a sum of money: the number, and the currency as its ISO 4217 code */
final case class Amount(value: BigDecimal, currency: String):
  /** `20 USD`, `15.5 EUR` — no trailing zeros */
  def show: String = s"${value.bigDecimal.stripTrailingZeros.toPlainString} $currency"

/**
 * An amount of money (specs/intent-classify.md, intent-extract-amount).
 *
 * The fourth parsed slot, on `Temporal`'s two promises — total,
 * deterministic — for the sum a budget or a booking frame asks for:
 * `$20`, `20 dollars`, `twenty dollars`, `USD 20`, `€15.50`, `15,50 €`,
 * `1,000 dollars`, `two hundred euros`; and in the fixture's other
 * languages `vingt euros`, `zwanzig Euro`, `veinte euros`, `200
 * гривен`, `двести гривень`, `500 руб`, `20 zł`, `dwadzieścia
 * złotych`, `3000円`, `¥500`. A number needs a CURRENCY beside it — a
 * symbol before or after, a code, or the currency's name in the
 * language, inflected — and the number may be digits (with `.` or `,`
 * as the decimal or the thousands separator, told apart by how many
 * digits follow) or number words, composed (`two hundred`, `двести
 * пятьдесят`). Only unambiguous names count: `pound` and `livre` and
 * `libra` are weights and books too, so pounds are `£`, `GBP` and
 * `sterling` only. Anything else, and any amount at or below zero, is
 * `None`.
 */
object Amount {

  /** symbols, before or after the number */
  private val symbols: Map[String, String] = Map(
    "$" -> "USD", "€" -> "EUR", "£" -> "GBP", "¥" -> "JPY", "円" -> "JPY", "₴" -> "UAH", "₽" -> "RUB", "zł" -> "PLN", "грн" -> "UAH")
  private val codes: Map[String, String] =
    Seq("USD", "EUR", "GBP", "JPY", "UAH", "RUB", "PLN", "CHF", "CAD", "AUD").map(c => c.toLowerCase -> c).toMap

  /** the currency's name as a token prefix, so inflections match;
   * only names that mean money and nothing else */
  private val names: Seq[(String, String)] = Seq(
    // en
    "dollar" -> "USD", "buck" -> "USD", "euro" -> "EUR", "sterling" -> "GBP", "quid" -> "GBP", "yen" -> "JPY",
    "hryvnia" -> "UAH", "hryvnya" -> "UAH", "ruble" -> "RUB", "rouble" -> "RUB", "zloty" -> "PLN", "złoty" -> "PLN",
    "franc" -> "CHF",
    // fr / de / es (euro is shared)
    "dólar" -> "USD", "dolar" -> "USD", "yens" -> "JPY",
    // ru
    "доллар" -> "USD", "бакс" -> "USD", "евро" -> "EUR", "фунт" -> "GBP", "иен" -> "JPY", "йен" -> "JPY",
    "грив" -> "UAH", "руб" -> "RUB", "злот" -> "PLN",
    // uk
    "долар" -> "USD", "євро" -> "EUR", "єн" -> "JPY",
    // pl
    "funt" -> "GBP", "jen" -> "JPY", "hrywn" -> "UAH", "rubl" -> "RUB", "zlot" -> "PLN",
    // ja
    "ドル" -> "USD", "ユーロ" -> "EUR", "ポンド" -> "GBP")

  /** hundreds and thousands, per language, beside `Numbers`' words */
  private val multipliers: Map[String, Double] = Map(
    "hundred" -> 100, "thousand" -> 1000, "grand" -> 1000, "k" -> 1000,
    "cent" -> 100, "cents" -> 100, "mille" -> 1000,
    "hundert" -> 100, "tausend" -> 1000,
    "cien" -> 100, "ciento" -> 100, "mil" -> 1000,
    "сто" -> 100, "тысяча" -> 1000, "тысячи" -> 1000, "тысяч" -> 1000, "тыс" -> 1000,
    "тисяча" -> 1000, "тисячі" -> 1000, "тисяч" -> 1000, "тис" -> 1000,
    "sto" -> 100, "tysiąc" -> 1000, "tysiące" -> 1000, "tysięcy" -> 1000, "tys" -> 1000)
  /** the hundreds that are one word */
  private val hundreds: Map[String, Double] = Map(
    "двести" -> 200, "триста" -> 300, "четыреста" -> 400, "пятьсот" -> 500, "шестьсот" -> 600, "семьсот" -> 700,
    "восемьсот" -> 800, "девятьсот" -> 900,
    "двісті" -> 200, "триста" -> 300, "чотириста" -> 400, "п'ятсот" -> 500, "п’ятсот" -> 500, "шістсот" -> 600,
    "сімсот" -> 700, "вісімсот" -> 800, "дев'ятсот" -> 900, "дев’ятсот" -> 900,
    "dwieście" -> 200, "trzysta" -> 300, "czterysta" -> 400, "pięćset" -> 500, "sześćset" -> 600, "siedemset" -> 700,
    "osiemset" -> 800, "dziewięćset" -> 900,
    "doscientos" -> 200, "trescientos" -> 300, "cuatrocientos" -> 400, "quinientos" -> 500,
    "zweihundert" -> 200, "dreihundert" -> 300, "fünfhundert" -> 500)
  private val en: Map[String, Double] = Map("a" -> 1.0, "an" -> 1.0, "one" -> 1.0, "two" -> 2.0, "three" -> 3.0, "four" -> 4.0,
    "five" -> 5.0, "six" -> 6.0, "seven" -> 7.0, "eight" -> 8.0, "nine" -> 9.0, "ten" -> 10.0, "eleven" -> 11.0,
    "twelve" -> 12.0, "thirteen" -> 13.0, "fourteen" -> 14.0, "fifteen" -> 15.0, "sixteen" -> 16.0, "seventeen" -> 17.0,
    "eighteen" -> 18.0, "nineteen" -> 19.0, "twenty" -> 20.0, "thirty" -> 30.0, "forty" -> 40.0, "fifty" -> 50.0,
    "sixty" -> 60.0, "seventy" -> 70.0, "eighty" -> 80.0, "ninety" -> 90.0)
  private val words: Map[String, Double] =
    en ++ Numbers.fr ++ Numbers.de ++ Numbers.es ++ Numbers.ru ++ Numbers.uk ++ Numbers.pl ++ hundreds

  /** digits with separators: `1,000.50`, `1.000,50`, `20,5`, `1 000` (joined before) */
  private val number = raw"\d+(?:[.,]\d+)*".r

  private def tokens(phrase: String): List[String] =
    phrase.toLowerCase
      .replaceAll("[!?;:()¿¡«»\"]", " ")
      // a thousands space: `1 000` (a plain or a narrow no-break space) is one number
      .replaceAll("(?<=\\d)[   ](?=\\d{3}(?!\\d))", "")
      // a dot or comma BETWEEN digits belongs to the number; anywhere else it is punctuation
      .replaceAll("[.,](?!\\d)", " ").replaceAll("(?<!\\d)[.,]", " ")
      // a symbol glued to its number, a code glued to its number: `$20`, `20€`, `3000円`, `20zł`, `usd20`
      .replaceAll("(?<=\\d)(?=[^\\d\\s.,])", " ").replaceAll("(?<=[^\\d\\s.,])(?=\\d)", " ")
      .split("\\s+").filter(_.nonEmpty).toList

  /** `1,000.50` -> 1000.50, `1.000,50` -> 1000.50, `20,5` -> 20.5,
   * `1,000` -> 1000, `2.500` -> 2500: with both separators the last is
   * the decimal point; with one kind, more than once or before exactly
   * three digits it groups thousands, otherwise it is the decimal */
  private def digitsToValue(s: String): Option[BigDecimal] =
    val hasDot = s.contains('.'); val hasComma = s.contains(',')
    val plain =
      if hasDot && hasComma then
        val dec = if s.lastIndexOf('.') > s.lastIndexOf(',') then '.' else ','
        val thou = if dec == '.' then "," else "."
        s.replace(thou, "").replace(dec, '.')
      else if hasDot || hasComma then
        val sep = if hasDot then '.' else ','
        val parts = s.split(if sep == '.' then "\\." else ",")
        val grouping = parts.length > 2 || (parts.length == 2 && parts(1).length == 3)
        if grouping then parts.mkString("") else parts.mkString(".")
      else s
    scala.util.Try(BigDecimal(plain)).toOption

  private def currencyOf(tok: String): Option[String] =
    symbols.get(tok).orElse(codes.get(tok)).orElse(names.collectFirst { case (n, c) if tok.startsWith(n) => c })

  /** number words composed: `two hundred fifty`, `двести пятьдесят`,
   * `sto dwadzieścia`; digits with a multiplier word: `2 thousand` */
  private def compose(ts: List[String]): Option[BigDecimal] =
    if ts.isEmpty then None
    else
      var total = BigDecimal(0); var group = BigDecimal(0); var ok = true; var any = false
      for t <- ts if ok do
        t match
          case number(_*) => digitsToValue(t) match
            case Some(v) => group += v; any = true
            case None => ok = false
          case _ if multipliers.contains(t) =>
            val m = BigDecimal(multipliers(t))
            group = (if group == 0 then BigDecimal(1) else group) * m
            if m >= 1000 then { total += group; group = BigDecimal(0) }
            any = true
          case _ if words.contains(t) => group += BigDecimal(words(t)); any = true
          case _ => ok = false
      if ok && any then Some(total + group) else None

  private def isNumberish(t: String): Boolean =
    number.matches(t) || words.contains(t) || multipliers.contains(t)

  private def amountAt(ts: List[String]): Option[Amount] =
    ts.indices.iterator.flatMap { i =>
      currencyOf(ts(i)).flatMap { cur =>
        // the number before the currency: the longest run of number
        // words ending just before it; else the number after (a symbol
        // or a code before its number: `$ 20`, `usd 20`)
        val before = ts.take(i).reverse.takeWhile(isNumberish).reverse
        val after = ts.drop(i + 1).takeWhile(isNumberish)
        compose(before).orElse(compose(after)).filter(_ > 0).map(v => Amount(v, cur))
      }
    }.nextOption()

  /** an amount of money, or `None` */
  def parse(phrase: String): Option[Amount] =
    val ts = tokens(phrase)
    if ts.isEmpty then None else amountAt(ts)

  /** the same answer as `parse`, plus the shortest window of words
   * reproducing it — the evidence rule of `Temporal.find` */
  def find(message: String): Option[Found[Amount]] =
    parse(message).map { v =>
      val toks = message.split("\\s+").filter(_.nonEmpty)
      val windows =
        for len <- 1 to toks.length; i <- 0 to toks.length - len yield (i, len)
      val span = windows
        .find((i, len) => parse(toks.slice(i, i + len).mkString(" ")).contains(v))
        .map((i, len) => toks.slice(i, i + len).mkString(" "))
        .getOrElse(message)
      // trim punctuation, but a currency symbol at either edge is evidence
      Found(span.replaceAll("^[.,!?;:()\\[\\]{}\"'«»…]+|[.,!?;:()\\[\\]{}\"'«»…]+$", "").trim, v)
    }
}
