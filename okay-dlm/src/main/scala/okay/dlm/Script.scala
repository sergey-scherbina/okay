package okay.dlm

import java.lang.Character.UnicodeScript

/**
 * WHICH ALPHABET a word is written in, and which languages that
 * alphabet could belong to.
 *
 * Typo tolerance and language detection both need one fact the
 * encoder does not give: a real word of one language can be one edit
 * away from a real word of another that shares its alphabet, and it is
 * then a DIFFERENT word, not a misspelling. Measured before it was
 * written: Russian «снимаю» (I film, or I rent) is two edits from
 * Ukrainian «знімаю», and it filed a photographer as looking for a
 * flat. The test is the script, and then the letters that exist in
 * ONE of the languages sharing it.
 *
 * `Script` knows only Unicode; `Alphabet` knows which languages a
 * caller speaks and which letters are theirs alone.
 */
object Script:

  /** the script of one letter; `None` for a digit, a mark or
   * punctuation, which belong to every script */
  def of(c: Char): Option[UnicodeScript] =
    if !Character.isLetter(c) then None
    else UnicodeScript.of(c.toInt) match
      case UnicodeScript.COMMON | UnicodeScript.INHERITED | UnicodeScript.UNKNOWN => None
      case s => Some(s)

  /** the scripts a text is written in, most letters first */
  def scripts(text: String): Vector[(UnicodeScript, Int)] =
    text.flatMap(of).groupBy(identity).map((s, cs) => s -> cs.length)
      .toVector.sortBy((s, n) => (-n, s.name))

  /** the script most of the letters are in, if any letter is */
  def dominant(text: String): Option[UnicodeScript] = scripts(text).headOption.map(_._1)

  /**
   * THE SCRIPT A TEXT IS WRITTEN IN, which is not the script most of
   * its letters are in. Latin is the script of the world's
   * identifiers — an email, a brand, a language's name, a command's
   * argument — and it rides inside a message written in any other
   * script: «сценарий deal seeker=anna@example.org» is a Russian
   * sentence with eight Cyrillic letters and twenty-two Latin ones.
   * So a text is in its most frequent NON-LATIN script when it has
   * one, and in Latin only when it has nothing else. Measured on the
   * source implementation: counting instead of this rule answered a
   * Russian speaker in English the moment they typed an address.
   */
  def native(text: String): Option[UnicodeScript] =
    scripts(text).find(_._1 != UnicodeScript.LATIN).map(_._1).orElse(dominant(text))

  /** a WORD in two scripts at once is a typo, always */
  def mixed(word: String): Boolean = scripts(word).length > 1

/**
 * The languages a caller speaks, by script, with the letters each
 * language owns ALONE among the ones sharing its script.
 *
 * `languagesOf` narrows a word to the languages it could be written
 * in: its script's languages, and — where the word carries a letter
 * that exists in one of them only — that one. Where nothing decides,
 * the word belongs to every language of its script, and a same-language
 * collision is a different problem with a different remedy (an
 * exact-only rule).
 *
 * `Alphabet.none` says nothing: every word belongs to every language,
 * which is the behaviour of a router that isolates no language.
 */
final case class Alphabet(languages: Map[String, UnicodeScript],
                          marks: Map[String, Set[Char]]):

  def isEmpty: Boolean = languages.isEmpty

  /** the languages written in this script */
  def of(script: UnicodeScript): Set[String] =
    languages.collect { case (l, s) if s == script => l }.toSet

  def scriptOf(lang: String): Option[UnicodeScript] = languages.get(lang)

  /** which languages this word could belong to; every one the caller
   * speaks when nothing narrows it */
  def languagesOf(word: String): Set[String] =
    if isEmpty then Set.empty
    else Script.native(word) match
      case None => languages.keySet
      case Some(script) =>
        val candidates = of(script)
        val w = word.toLowerCase
        val decided = candidates.filter(l => marks.getOrElse(l, Set.empty).exists(w.contains(_)))
        if decided.nonEmpty then decided else candidates

  /** does the text's alphabet agree with the language's? `true` when
   * either side is unknown — the gate refuses only a contradiction */
  def agrees(lang: String, text: String): Boolean =
    (scriptOf(lang), Script.native(text)) match
      case (Some(a), Some(b)) => a == b
      case _ => true

object Alphabet:

  val none: Alphabet = Alphabet(Map.empty, Map.empty)

  /**
   * The languages this library knows the exclusive letters of. A
   * letter is listed under a language when, AMONG THE LANGUAGES
   * SHARING ITS SCRIPT IN THIS TABLE, it is that language's alone;
   * `of` narrows the table to the languages a caller actually speaks,
   * so a letter two unspoken languages share still decides between
   * the two that are.
   */
  val known: Map[String, (UnicodeScript, Set[Char])] = Map(
    "ru" -> (UnicodeScript.CYRILLIC, Set('ы', 'э', 'ъ', 'ё')),
    "uk" -> (UnicodeScript.CYRILLIC, Set('і', 'ї', 'є', 'ґ')),
    "be" -> (UnicodeScript.CYRILLIC, Set('ў')),
    "bg" -> (UnicodeScript.CYRILLIC, Set('щ', 'ъ')),
    "pl" -> (UnicodeScript.LATIN, Set('ą', 'ć', 'ę', 'ł', 'ń', 'ś', 'ź', 'ż')),
    "cs" -> (UnicodeScript.LATIN, Set('ě', 'ř', 'ů', 'ď', 'ť', 'ň')),
    "de" -> (UnicodeScript.LATIN, Set('ä', 'ö', 'ü', 'ß')),
    "fr" -> (UnicodeScript.LATIN, Set('à', 'â', 'ç', 'è', 'ê', 'ë', 'î', 'ï', 'ô', 'û', 'ù', 'ÿ', 'œ')),
    "es" -> (UnicodeScript.LATIN, Set('ñ', '¿', '¡')),
    "en" -> (UnicodeScript.LATIN, Set.empty),
    "el" -> (UnicodeScript.GREEK, Set.empty),
    "ja" -> (UnicodeScript.HIRAGANA, Set.empty))

  /** the alphabet of the languages named, from the table above; an
   * unknown code is refused rather than guessed */
  def of(langs: String*): Either[String, Alphabet] =
    val unknown = langs.filterNot(known.contains)
    if unknown.nonEmpty then Left(s"no alphabet for: ${unknown.mkString(", ")}")
    else Right(Alphabet(
      langs.map(l => l -> known(l)._1).toMap,
      langs.map(l => l -> known(l)._2).toMap))
