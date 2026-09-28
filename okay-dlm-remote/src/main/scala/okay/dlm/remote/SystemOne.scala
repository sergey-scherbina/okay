package okay.dlm.remote

import okay.codec.Json
import okay.codec.Json.*
import okay.dlm.Judge

/**
 * THE "SYSTEM ONE" PROTOCOL: typed questions over a state, answered
 * with typed values, probabilities and a confidence — never a
 * sentence. `POST /v1/systemone`, a JSON body of `state` and
 * `questions`, an `answers` object back keyed the way the questions
 * were. TypeSafe's hosted Jev speaks it; Convai's Laya serves the same
 * wire from a container, and says so («the same POST /v1/systemone
 * wire protocol as TypeSafe's hosted Jev API»). So there is one
 * client here and two configurations (`Jev`, `Laya`), and the model's
 * `Judge` seam is what both plug into.
 *
 * Three question types, and the one a `Judge` asks is `choice`:
 *
 *   {"state": {"text": …},
 *    "questions": {"q": {"type": "choice", "instructions": …,
 *                        "criteria": {"option": "what it means", …}}}}
 *   → {"answers": {"q": {"choice": "option", "probabilities": {…},
 *                        "confidence": 0.94}}, "usage": {…}}
 *
 * `score` (a rubric of ordered levels, an expected score back) and
 * `noul` (P(true) of a proposition) are here for a caller that wants
 * them; nothing in the model asks them yet.
 *
 * Built against the two vendors' documentation of 2026-09 and
 * measured against nothing: every number a caller acts on comes from
 * its own held-out rows (specs/dlm.md, "Backends").
 */
object SystemOne:

  final case class Config(name: String, base: String, apiKey: Option[String],
                          path: String = "/v1/systemone",
                          /** the key under which the text travels in `state` */
                          field: String = "text")

  /** one answer to a `choice` */
  final case class Answer(choice: String, probabilities: Map[String, Double], confidence: Option[Double])
  /** one answer to a `score`: the expected level and the distribution over levels */
  final case class Scored(score: Double, probabilities: Map[String, Double], confidence: Option[Double])

  // ---- the codec, pure ----------------------------------------------------

  def encodeChoice(text: String, q: Judge.Question, field: String = "text", key: String = "q"): Json =
    JObj(Vector(
      "state" -> JObj(Vector(field -> JStr(text))),
      "questions" -> JObj(Vector(key -> JObj(Vector(
        "type" -> JStr("choice"),
        "instructions" -> JStr(if q.instructions.nonEmpty then q.instructions else "Which of these applies?"),
        "criteria" -> JObj(q.options.map((n, d) => n -> JStr(if d.nonEmpty then d else n)))))))))

  def encodeScore(text: String, instructions: String, levels: Vector[(String, String)],
                  field: String = "text", key: String = "q"): Json =
    JObj(Vector(
      "state" -> JObj(Vector(field -> JStr(text))),
      "questions" -> JObj(Vector(key -> JObj(Vector(
        "type" -> JStr("score"), "instructions" -> JStr(instructions),
        "criteria" -> JObj(levels.map((n, d) => n -> JStr(d)))))))))

  def encodeNoul(text: String, instructions: String, field: String = "text", key: String = "q"): Json =
    JObj(Vector(
      "state" -> JObj(Vector(field -> JStr(text))),
      "questions" -> JObj(Vector(key -> JObj(Vector(
        "type" -> JStr("noul"), "instructions" -> JStr(instructions)))))))

  private def answerOf(raw: String, key: String): Either[String, Vector[(String, Json)]] =
    val j = try Json.parse(raw) catch case e: Exception => JStr(s"broken: ${e.getMessage}")
    j match
      case JObj(fs) =>
        fs.collectFirst { case ("answers", JObj(as)) => as }
          .flatMap(_.collectFirst { case (`key`, JObj(a)) => a })
          .toRight(s"no answer «$key» in ${raw.take(120)}")
      case _ => Left(s"not a JSON object: ${raw.take(120)}")

  private def probabilities(a: Vector[(String, Json)]): Map[String, Double] =
    a.collectFirst { case ("probabilities", JObj(ps)) => ps.collect { case (k, JNum(v)) => k -> v }.toMap }
      .getOrElse(Map.empty)

  private def confidence(a: Vector[(String, Json)]): Option[Double] =
    a.collectFirst { case ("confidence", JNum(v)) => v }
      .orElse(a.collectFirst { case ("answer_confidence", JNum(v)) => v })

  def decodeChoice(raw: String, key: String = "q"): Either[String, Answer] =
    answerOf(raw, key).flatMap { a =>
      a.collectFirst { case ("choice", JStr(c)) => c }.toRight(s"no choice in the answer")
        .map(c => Answer(c, probabilities(a), confidence(a)))
    }

  def decodeScore(raw: String, key: String = "q"): Either[String, Scored] =
    answerOf(raw, key).flatMap { a =>
      a.collectFirst { case ("score", JNum(s)) => s }.toRight("no score in the answer")
        .map(s => Scored(s, probabilities(a), confidence(a)))
    }

  def decodeNoul(raw: String, key: String = "q"): Either[String, Double] =
    answerOf(raw, key).flatMap(a => a.collectFirst { case ("noul", JNum(p)) => p }.toRight("no noul in the answer"))

  // ---- the client ---------------------------------------------------------

  /**
   * One configured endpoint. A `Left` names what went wrong and the
   * `Judge` view turns it into an abstention — the wire is never a
   * reason for the model to guess.
   */
  final class Client(val config: Config)(using wire: Wire):
    private def headers = Map("Content-Type" -> "application/json", "Accept" -> "application/json") ++
      config.apiKey.map(k => "Authorization" -> s"Bearer $k")
    private def url = config.base.stripSuffix("/") + config.path

    def raw(body: Json): Either[String, String] = wire.post(url, headers, Json.print(body))

    def choose(text: String, q: Judge.Question): Either[String, Answer] =
      raw(encodeChoice(text, q, config.field)).flatMap(decodeChoice(_))

    def score(text: String, instructions: String, levels: Vector[(String, String)]): Either[String, Scored] =
      raw(encodeScore(text, instructions, levels, config.field)).flatMap(decodeScore(_))

    def noul(text: String, instructions: String): Either[String, Double] =
      raw(encodeNoul(text, instructions, config.field)).flatMap(decodeNoul(_))

    /**
     * The client as the model's judge. A probability the wire did not
     * send for an option asked is 0; an option the wire sent that was
     * not asked is dropped; the choice named is what ranks first even
     * where the probabilities disagree, because the vendor's decision
     * is the vendor's. A failed call is `None`, and `report` hears why.
     */
    def judge(report: String => Unit = _ => ()): Judge = new Judge:
      val name = config.name
      def choose(text: String, q: Judge.Question): Option[Judge.Choice] =
        Client.this.choose(text, q) match
          case Left(why) => report(why); None
          case Right(a) =>
            val asked = q.names
            val ps = asked.map(n => n -> a.probabilities.getOrElse(n, 0.0))
            val ranked = ps.filterNot(_._1 == a.choice).sortBy(-_._2)
            val chosen = ps.find(_._1 == a.choice).getOrElse(a.choice -> 1.0)
            Some(Judge.Choice(chosen +: ranked, a.confidence))
