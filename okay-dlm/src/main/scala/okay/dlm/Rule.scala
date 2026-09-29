package okay.dlm

import java.util.regex.Pattern

/**
 * RULES FROM WORDS: most authored rules are only a list of trigger
 * words, and an author should write the words, not the regex
 * (specs/dlm-rule-keywords.md).
 *
 * `Rule.keywords` answers ordinary rule strings, so it goes straight
 * into `Intent(rules = …)` and combines with hand-written regexes by
 * `++`; the router, `Support.Exact` and the typo miner keep reading one
 * list. A plain word is a whole word, `payout*` a word prefix, a
 * keyword with spaces a phrase over any whitespace. Every rule is
 * case-insensitive in any script (`(?iU)`).
 *
 * Plain and prefix words compile to exactly the two shapes
 * `Fuzzy.literalTriggers` mines — `(?iU)\b(?:a|b)\b` and
 * `(?iU)\b(?:a|b)\w*\b` — so a typo of a keyword reaches the typo layer
 * like any hand-written trigger. A phrase, or a word carrying anything
 * but letters and digits, is its own rule bounded by lookarounds,
 * because `\b` never sits beside a `+`; the typo layer skips those, as
 * it skips every structural rule.
 */
object Rule:

  def keywords(words: String*): Vector[String] =
    val parsed = words.toVector.map { raw =>
      val k = raw.trim
      val prefix = k.endsWith("*")
      val body = if prefix then k.dropRight(1).trim else k
      require(body.nonEmpty, s"Rule.keywords: blank keyword \"$raw\"")
      (body.split("\\s+").toVector, prefix)
    }
    def simple(ws: Vector[String]) = ws.size == 1 && ws.head.forall(_.isLetterOrDigit)
    def group(ws: Vector[String], tail: String) =
      Option.when(ws.nonEmpty)(s"(?iU)\\b(?:${ws.mkString("|")})$tail")
    val exact = parsed.collect { case (ws, false) if simple(ws) => ws.head }.distinct
    val prefixes = parsed.collect { case (ws, true) if simple(ws) => ws.head }.distinct
    val others = parsed.filterNot((ws, _) => simple(ws)).map { (ws, prefix) =>
      val phrase = ws.map(quote).mkString("\\s+")
      s"(?iU)(?<!\\w)$phrase${if prefix then "\\w*" else ""}(?!\\w)"
    }.distinct
    group(exact, "\\b").toVector ++ group(prefixes, "\\w*\\b") ++ others

  /** letters and digits stay literal — the form the typo miner reads —
   * and every other character is quoted */
  private def quote(w: String): String =
    w.map(c => if c.isLetterOrDigit then c.toString else Pattern.quote(c.toString)).mkString
