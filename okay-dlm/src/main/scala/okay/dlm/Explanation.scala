package okay.dlm

import okay.codec.Json
import okay.codec.Json.*

/**
 * WHY THE MODEL DECIDED WHAT IT DECIDED, as one value
 * (specs/dlm-learning.md §3): the layer that spoke, the rule
 * verbatim, the lesson that applied and whose it was, everything every
 * layer saw, the judge's ranking and its name, the encoder, the
 * tables by hash, the language. Data, not a sentence: a caller renders
 * it in the person's language and a test asserts on it. «Jev said so»
 * and «our probe said so» are told apart in every audit.
 */
final case class Explanation(text: String,
                             route: Route,
                             layer: Option[Layer],
                             rule: Option[String],
                             lesson: Option[Lesson],
                             noticed: Vector[(String, Support)],
                             scores: Vector[(String, Float)],
                             judge: Option[String],
                             encoder: String,
                             tables: Map[String, String],
                             language: Option[String])

object Explanation:

  /**
   * The explanation of one text, from the router that decides and the
   * memory it decides with. `who` is the person whose lessons count;
   * `tables` names the artifacts served by hash so a decision is tied
   * to the numbers that made it.
   */
  def of(router: Router, text: String, who: String, memory: Memory = Memory.empty,
         lang: Option[String] = None, encoder: String = "", tables: Map[String, String] = Map.empty): Explanation =
    val route = router.route(text, lang, memory, who)
    val noticed = router.noticed(text, lang, memory, who)
    // A MISSING SLOT STILL HAD A LAYER DECIDE ITS INTENT (dlm-explain-missing,
    // found by okay-watch): `Route.Missing` carries no `Support` — what to ask
    // is the caller's, and the route says only that a value is absent — so the
    // support is taken from `noticed`, where every layer's reading of every
    // intent it saw already is. Without this an audit reads «layer: none» for a
    // turn a lesson decided, which is the opposite of what learning must show.
    val support = route match
      case Route.Fires(_, _, s) => Some(s)
      case Route.Missing(intent, _) => noticed.collectFirst { case (`intent`, s) => s }
      case _ => None
    val rule = support.collect { case Support.Exact(Some(r)) => r }
    val lesson = support.collect { case Support.Remembered(offset, _) =>
      memory.forPerson(who).find(_.offset == offset) }.flatten
    val judge = support.collect { case Support.Semantic(_, _) => router.judge.map(_.name) }.flatten
    Explanation(text, route, support.map(_.layer), rule, lesson,
      noticed, router.scores(text), judge, encoder, tables, lang)

  def encode(e: Explanation): Json = JObj(Vector(
    "text" -> JStr(e.text),
    "route" -> Route.encode(e.route),
    "layer" -> e.layer.fold[Json](JNull)(l => JStr(l.toString.toLowerCase)),
    "rule" -> e.rule.fold[Json](JNull)(JStr(_)),
    "lesson" -> e.lesson.fold[Json](JNull)(l => JObj(Vector(
      "text" -> JStr(l.text), "intent" -> JStr(l.intent), "offset" -> JNum(l.offset.toDouble), "who" -> JStr(l.who)))),
    "noticed" -> JArr(e.noticed.map((i, s) => JObj(Vector(
      "intent" -> JStr(i), "by" -> JStr(s.layer.toString.toLowerCase), "score" -> JNum(s.score.toDouble))))),
    "scores" -> JArr(e.scores.map((i, p) => JObj(Vector("intent" -> JStr(i), "p" -> JNum(p.toDouble))))),
    "judge" -> e.judge.fold[Json](JNull)(JStr(_)),
    "encoder" -> JStr(e.encoder),
    "tables" -> JObj(e.tables.toVector.sortBy(_._1).map((k, v) => k -> JStr(v))),
    "language" -> e.language.fold[Json](JNull)(JStr(_))))
