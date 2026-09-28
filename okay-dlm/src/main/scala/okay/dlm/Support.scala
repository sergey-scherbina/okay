package okay.dlm

import okay.codec.Json
import okay.codec.Json.*

/** which layer decided — a route is always attributable */
enum Layer:
  case Rule, Fuzzy, Semantic, Memory

/**
 * WHY a route fired — typed, so that four incomparable scales never
 * share one number (specs/dlm.md, "Support"). `1.0` for a rule,
 * `1.0 - 0.2 * d` for a typo, a probability for the vector layer and a
 * nearness for a lesson are not the same kind of number; a single
 * `Float` invites a comparison that means nothing, and eventually
 * somebody sorts on it.
 *
 * The wire keeps carrying a layer name and a number (`by`, `score`),
 * because a log is a wire format and does not change because a type
 * did. What it ALSO carries, when known, is the rule that matched and
 * the runner-up the probe saw — the two facts an accuracy question
 * asked later will need.
 */
enum Support:
  /** a rule matched — a fact about the text. `None` for a record
   * written before the rule travelled in the log */
  case Exact(rule: Option[String])
  /** a trigger word a bounded edit distance away */
  case Typo(distance: Int)
  /** the probe's winner: its probability, and what it beat */
  case Semantic(p: Float, runnerUp: Option[String])
  /** a lesson this person — or enough people — gave: the journal
   * offset of the teaching turn, and how near the message is to the
   * sentence taught — 1.0 for the same words, `1.0 - 0.2·d` a typo
   * away, the convention `Typo` writes */
  case Remembered(lesson: Long, near: Float)

  def layer: Layer = this match
    case Exact(_) => Layer.Rule
    case Typo(_) => Layer.Fuzzy
    case Semantic(_, _) => Layer.Semantic
    case Remembered(_, _) => Layer.Memory

  /** the one number the wire has always carried — derived, never
   * compared across layers */
  def score: Float = this match
    case Exact(_) => 1.0f
    case Typo(d) => 1.0f - 0.2f * d
    case Semantic(p, _) => p
    case Remembered(_, near) => near

object Support:
  /** a support read back from the wire: the layer name and the number
   * it always carried, plus the rule and runner-up when the record has
   * them */
  def of(by: Layer, score: Float, rule: Option[String], runnerUp: Option[String]): Support = by match
    case Layer.Rule => Exact(rule)
    case Layer.Fuzzy => Typo(math.round((1.0f - score) / 0.2f).max(0))
    case Layer.Semantic => Semantic(score, runnerUp)
    // the lesson rides in the field the rule layer already writes,
    // `taught:<offset>`, so a record written before this layer decodes
    // as it always did and one written by it names its own evidence
    case Layer.Memory => Remembered(rule.flatMap(_.stripPrefix("taught:").toLongOption).getOrElse(-1L), score)

/**
 * What the router answers. `Unclear` is a first-class outcome and not
 * a failure: the caller asks a question instead of calling a tool it
 * is not sure about. Guessing is the one behaviour a deterministic
 * model cannot afford, because a wrong action writes a fact that
 * someone later has to disbelieve.
 */
enum Route:
  case Fires(intent: String, slots: Map[String, String], support: Support)
  /** the intent is clear, a value it cannot work without is not. WHAT
   * to say about it is the caller's, in the caller's language — the
   * router holds no words */
  case Missing(intent: String, slot: String)
  case Unclear(candidates: Vector[String], score: Float)

  /** the intent this route names, if it names one */
  def named: Option[String] = this match
    case Fires(i, _, _) => Some(i)
    case Missing(i, _) => Some(i)
    case Unclear(_, _) => None

object Route:
  /**
   * A verdict, written down — so a replay resumes from what was
   * DECIDED rather than re-deriving it from the text.
   *
   * Every layer is a function of the message AND of the authored data
   * behind it, and that data is edited: a rule added today changes
   * what a sentence from last week routes to. Recording the answer
   * keeps a rebuilt state equal to the one people were actually
   * shown; recomputing it keeps the state equal to today's opinion of
   * a conversation that already happened.
   */
  def encode(r: Route): Json = r match
    case Route.Fires(i, slots, sup) => JObj(Vector(
      "route" -> JStr("fires"), "intent" -> JStr(i),
      "slots" -> JObj(slots.toVector.sortBy(_._1).map((k, v) => k -> JStr(v))),
      "by" -> JStr(sup.layer.toString), "score" -> JNum(sup.score.toDouble)) ++
      (sup match
        case Support.Exact(Some(rule)) => Vector("rule" -> JStr(rule))
        case Support.Semantic(_, Some(ru)) => Vector("runnerUp" -> JStr(ru))
        case Support.Remembered(lesson, _) => Vector("rule" -> JStr(s"taught:$lesson"))
        case _ => Vector.empty))
    case Route.Missing(i, slot) => JObj(Vector(
      "route" -> JStr("missing"), "intent" -> JStr(i), "slot" -> JStr(slot)))
    case Route.Unclear(cands, score) => JObj(Vector(
      "route" -> JStr("unclear"),
      "candidates" -> JArr(cands.map(JStr(_))), "score" -> JNum(score.toDouble)))

  def decode(j: Json): Option[Route] =
    def str(fs: Vector[(String, Json)], k: String) =
      fs.collectFirst { case (`k`, JStr(v)) => v }
    def num(fs: Vector[(String, Json)], k: String) =
      fs.collectFirst { case (`k`, JNum(v)) => v.toFloat }
    j match
      case JObj(fs) => str(fs, "route") match
        case Some("fires") =>
          for
            i <- str(fs, "intent")
            by <- str(fs, "by").flatMap(b => Layer.values.find(_.toString == b))
          yield Route.Fires(i,
            fs.collectFirst { case ("slots", JObj(ss)) =>
              ss.collect { case (k, JStr(v)) => k -> v }.toMap }.getOrElse(Map.empty),
            Support.of(by, num(fs, "score").getOrElse(0f), str(fs, "rule"), str(fs, "runnerUp")))
        case Some("missing") =>
          for i <- str(fs, "intent"); sl <- str(fs, "slot") yield Route.Missing(i, sl)
        case Some("unclear") =>
          Some(Route.Unclear(
            fs.collectFirst { case ("candidates", JArr(xs)) =>
              xs.collect { case JStr(v) => v } }.getOrElse(Vector.empty),
            num(fs, "score").getOrElse(0f)))
        case _ => None
      case _ => None
