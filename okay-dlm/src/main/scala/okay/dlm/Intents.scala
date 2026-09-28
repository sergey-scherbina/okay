package okay.dlm

import okay.codec.Json
import okay.codec.Json.*

/**
 * A slot is a regex with one capturing group, because every value a
 * deterministic model reads by rule is short: a phrase, an address, a
 * number. `fallback` says whether the WHOLE message is an acceptable
 * value when the pattern does not match — true for the free text a
 * search runs on, false for anything the caller must state exactly:
 * a deal number read off the wrong part of a sentence is worse than a
 * question.
 */
final case class Slot(name: String, pattern: String, fallback: Boolean = false)

/**
 * An intent is DATA. The whole point of the model is that the data is
 * authored once — by a person, or by a large model before the build —
 * and never consulted again at request time.
 *
 * Three fields, three different jobs:
 *
 *  - `rules` are anchored regular expressions. They fire with no
 *    model at all and they are the deterministic layer: what a rule
 *    decides is not a guess and cannot drift between deployments.
 *  - `byLang` holds ordinary human phrasings, keyed by language. They
 *    are embedded at BUILD time into `Exemplars`; at request time the
 *    message is embedded once and compared. The key is not
 *    decoration: the language detector is built from these very
 *    phrases, so declaring which language a phrasing is written in is
 *    what lets a caller answer in it.
 *  - `slots` pull the values out of the message.
 *
 * A `require` list makes an intent refuse to fire without the slots it
 * cannot work without — the difference between asking a question and
 * calling a tool with a hole in it. `ask` is the caller's own wording
 * for that question, per language: the library carries the fact that a
 * value is missing and never the sentence that asks for it.
 */
final case class Intent(name: String,
                        rules: Vector[String] = Vector.empty,
                        byLang: Map[String, Vector[String]] = Map.empty,
                        slots: Vector[Slot] = Vector.empty,
                        require: Vector[String] = Vector.empty,
                        ask: Map[String, String] = Map.empty,
                        /**
                         * May the VECTOR layer reach this intent?
                         *
                         * False for every command that carries an exact
                         * argument — a deal number, a flow id. Those are
                         * short, they read alike to an encoder, and acting
                         * on a fuzzy match would act on the wrong one.
                         * Their rules are exact and complete, so nothing
                         * is lost by keeping them out of the similarity
                         * contest, and the open-ended intents stop
                         * competing with noise.
                         */
                        semantic: Boolean = true,
                        /** what this capability IS, in the person's own
                         * language: one thing to say and what it does —
                         * written beside the rules that make the
                         * capability exist, so it cannot drift */
                        help: Map[String, Intent.Help] = Map.empty,
                        /** why this intent is NOT in the menu — a reason
                         * rather than a flag, because the next person to
                         * add an intent has to think about it */
                        internal: Option[String] = None,
                        /** where this sits in the answer to "what can
                         * you do?" — rules are matched in file order and
                         * moving one moves what it wins against, so the
                         * menu's order is carried separately */
                        rank: Int = 50):
  /** every phrasing, language forgotten — what the vector layer compiles */
  def examples: Vector[String] = byLang.toVector.sortBy(_._1).flatMap(_._2)

object Intent:
  /** one line of a caller's own answer to "what can you do?" */
  final case class Help(say: String, does: String)

  val defaultRank: Int = 50

/** the authored set: what the router reads, and what the build
 * compiles */
final case class Intents(intents: Vector[Intent]):
  def byName(n: String): Option[Intent] = intents.find(_.name == n)
  def names: Vector[String] = intents.map(_.name)
  /** the intents the vector layer may reach */
  def semantic: Vector[Intent] = intents.filter(_.semantic)
  /** `(label, phrase)` rows the vector layer is compiled from: only
   * the intents that opted in contribute, which is the whole point of
   * opting out */
  def rows: Vector[(String, String)] =
    semantic.flatMap(i => i.examples.map(i.name -> _))
  /** every phrasing by language, across intents — what a language
   * detector is built from */
  def byLang: Map[String, Vector[String]] =
    intents.flatMap(_.byLang.toVector).groupBy(_._1).map((k, vs) => k -> vs.flatMap(_._2))

  /**
   * THE MENU, SELECTED: what this model can be asked to do, in the
   * language asked — the answer to "what can you do?", and the choices
   * a caller offers after an `Unclear`.
   *
   * Three lines every consumer had written for itself: an intent with a
   * reason not to be offered (`internal`) is not in it; the order is
   * `rank`, then the name, so it is stable across builds and a caller
   * moving a rule does not move the menu; and the cell taken is the
   * `help` of the language asked, absent when this intent was never
   * described in it.
   *
   * NO WORDS ARE ADDED. `Intent.Help` is the caller's own sentence, and
   * what frames it — «I can:», a button, a numbered list — is the
   * caller's too.
   */
  def menu(lang: String): Vector[(String, Intent.Help)] =
    intents.filter(_.internal.isEmpty)
      .sortBy(i => (i.rank, i.name))
      .flatMap(i => i.help.get(lang).map(i.name -> _))

  /**
   * WHAT THE MENU IS MISSING: per language, the intents that would be
   * offered and carry no help cell in it. A menu that silently shrinks
   * in one language is the failure a caller cannot see from `menu`
   * alone — `Phrasing.holes` exists for the same reason.
   */
  def menuHoles(languages: Set[String]): Map[String, Vector[String]] =
    val offerable = intents.filter(_.internal.isEmpty)
    languages.toVector.sorted.map(l =>
      l -> offerable.filterNot(_.help.contains(l)).map(_.name)).filter(_._2.nonEmpty).toMap

object Intents:

  val empty: Intents = Intents(Vector.empty)

  def parse(raw: String): Either[String, Intents] =
    def strs(j: Json): Vector[String] = j match
      case JArr(xs) => xs.collect { case JStr(s) => s }
      case _ => Vector.empty
    def slotOf(j: Json): Option[Slot] = j match
      case JObj(fs) =>
        for
          n <- fs.collectFirst { case ("name", JStr(x)) => x }
          p <- fs.collectFirst { case ("pattern", JStr(x)) => x }
        yield Slot(n, p, fs.collectFirst { case ("fallback", JBool(b)) => b }.getOrElse(false))
      case _ => None
    def intentOf(j: Json): Option[Intent] = j match
      case JObj(fs) =>
        fs.collectFirst { case ("name", JStr(x)) => x }.map { name =>
          Intent(name,
            fs.collectFirst { case ("rules", v) => strs(v) }.getOrElse(Vector.empty),
            fs.collectFirst {
              // an object keys the phrasings by language; a bare array
              // is legal and means "language not declared"
              case ("examples", JObj(gs)) =>
                gs.map((k, v) => k -> strs(v)).filter(_._2.nonEmpty).toMap
              case ("examples", v @ JArr(_)) if strs(v).nonEmpty => Map("*" -> strs(v))
            }.getOrElse(Map.empty),
            fs.collectFirst { case ("slots", JArr(xs)) => xs.flatMap(slotOf) }.getOrElse(Vector.empty),
            fs.collectFirst { case ("require", v) => strs(v) }.getOrElse(Vector.empty),
            fs.collectFirst {
              case ("ask", JStr(x)) => Map("*" -> x)
              case ("ask", JObj(gs)) => gs.collect { case (k, JStr(v)) => k -> v }.toMap
            }.getOrElse(Map.empty),
            fs.collectFirst { case ("semantic", JBool(b)) => b }.getOrElse(true),
            fs.collectFirst {
              case ("help", JObj(gs)) => gs.flatMap { (lang, v) =>
                v match
                  case JObj(hs) =>
                    for
                      say <- hs.collectFirst { case ("say", JStr(x)) if x.trim.nonEmpty => x }
                      does <- hs.collectFirst { case ("does", JStr(x)) if x.trim.nonEmpty => x }
                    yield lang -> Intent.Help(say, does)
                  case _ => None
              }.toMap
            }.getOrElse(Map.empty),
            fs.collectFirst { case ("internal", JStr(x)) if x.trim.nonEmpty => x },
            fs.collectFirst { case ("rank", JNum(n)) => n.toInt }.getOrElse(Intent.defaultRank))
        }
      case _ => None
    Json.parse(raw) match
      case JObj(fs) =>
        fs.collectFirst { case ("intents", JArr(xs)) => xs.flatMap(intentOf) }
          .map(Intents(_)).toRight("no \"intents\" array")
      case JArr(xs) => Right(Intents(xs.flatMap(intentOf)))
      case _ => Left("not a JSON object")

  /** the set shipped as a resource of the caller's jar, absent when
   * there is none */
  def resource(name: String): Option[Intents] =
    Option(getClass.getResourceAsStream(name)).map { in =>
      val raw = try new String(in.readAllBytes(), "UTF-8") finally in.close()
      parse(raw).fold(e => throw IllegalStateException(s"$name: $e"), identity)
    }

  /**
   * A word carrying TWO SCRIPTS is a typo, always: a Cyrillic prefix in
   * front of a Latin verb compiles, validates, and can never match
   * anything. Two alphabets that share glyph shapes and one keyboard
   * is all it takes; this finds the next one at load, for any pair of
   * scripts and not only the pair it was first met with.
   */
  private def mixedScript(where: String, s: String): Vector[String] =
    // a regex escape (`\b`, `\w`) is not a letter beside a word, so
    // the escapes go before the words are read
    "\\p{L}+".r.findAllIn(s.replaceAll("\\\\[A-Za-z]", " ")).toVector.collect {
      case w if Script.mixed(w) =>
        s"$where: '$w' mixes two scripts — a typo that can never match"
    }

  /** malformations, as data: a rule or slot that does not compile is a
   * deployment-time error, never a request-time surprise */
  def validate(s: Intents): Vector[String] =
    val dupes = s.intents.groupBy(_.name).collect {
      case (n, xs) if xs.length > 1 => s"$n: declared ${xs.length} times"
    }.toVector
    val bad = s.intents.flatMap { i =>
      (i.rules ++ i.slots.map(_.pattern)).flatMap { p =>
        try { val _ = p.r; None } catch case _: Throwable => Some(s"${i.name}: bad regex '$p'")
      } ++ i.require.collect {
        case r if !i.slots.exists(_.name == r) => s"${i.name}: requires undeclared slot '$r'"
      } ++ Option.when(i.require.nonEmpty && i.ask.isEmpty)(
        s"${i.name}: requires slots but offers no prompt to ask for them") ++
      Option.when(i.rules.isEmpty && i.examples.isEmpty && !i.semantic)(
        // an intent with no rule is reachable only through the vector
        // layer, and its vectors are a SEPARATE artifact — so a packaged
        // file that no longer carries the authored examples cannot see
        // them from here. `semantic` is the declaration that they exist
        s"${i.name}: neither a rule nor an example — nothing can ever route to it") ++
      i.rules.flatMap(mixedScript(i.name, _)) ++
      i.examples.flatMap(mixedScript(i.name, _))
    }
    dupes ++ bad
