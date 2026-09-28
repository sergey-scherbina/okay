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

  // ---- the server: our model on the same wire -----------------------------

  /**
   * OUR MODEL ON THE SAME WIRE. A request as a client of Jev or Laya
   * would send it, answered by OUR judges — so a client written against
   * either vendor runs against this model unchanged, and three judges
   * are measured through one client.
   *
   * Which judge answers a question is decided by its OPTIONS: the first
   * judge that can rank them does — a head's judge knows its own
   * classes and abstains on any other set, which is how a `choice`
   * over acts reaches the act head and a `choice` over frames the
   * frame head. A question no judge can rank is answered with an
   * `error` in its own slot, never a guess; the other questions in the
   * same request are still answered. `noul` is a choice between «yes»
   * and «no», `score` a choice over the levels with an expected value
   * under the option order.
   *
   * WHAT WE DO NOT SAY: a `confidence` our judge does not have. The
   * probe has a margin and no calibration; the field is present only
   * where the judge answered one (a remote judge behind ours, or a
   * calibrated one later). A number invented here would be the one
   * thing that spoils the protocol.
   */
  object Service:

    final case class Asked(key: String, kind: String, instructions: String, options: Vector[(String, String)])

    /** the state's text: `text`, else `body`, else every string field
     * of the state joined — or the state itself when it is a string */
    def textOf(state: Json): String = state match
      case JStr(s) => s
      case JObj(fs) =>
        fs.collectFirst { case ("text", JStr(s)) => s }
          .orElse(fs.collectFirst { case ("body", JStr(s)) => s })
          .getOrElse(fs.collect { case (_, JStr(s)) => s }.mkString("\n"))
      case _ => ""

    def decode(raw: String): Either[String, (String, Vector[Asked])] =
      val j = try Json.parse(raw) catch case e: Exception => JStr(s"broken: ${e.getMessage}")
      j match
        case JObj(fs) =>
          val text = fs.collectFirst { case ("state", s) => textOf(s) }.getOrElse("")
          fs.collectFirst { case ("questions", JObj(qs)) => qs }.toRight("no \"questions\" object").map(qs => text -> qs.flatMap {
            case (key, JObj(q)) =>
              val kind = q.collectFirst { case ("type", JStr(t)) => t }.getOrElse("choice")
              val instructions = q.collectFirst { case ("instructions", JStr(i)) => i }.getOrElse("")
              val options = q.collectFirst {
                case ("criteria", JObj(cs)) => cs.map((n, d) => n -> (d match { case JStr(s) => s; case _ => "" }))
                case ("criteria", JArr(ns)) => ns.collect { case JStr(n) => n -> "" }
                case ("options", JArr(ns)) => ns.collect { case JStr(n) => n -> "" }
              }.getOrElse(Vector.empty)
              Some(Asked(key, kind, instructions, options))
            case _ => None
          })
        case _ => Left("not a JSON object")

    private def first(judges: Seq[Judge], text: String, q: Judge.Question): Option[(Judge, Judge.Choice)] =
      judges.iterator.flatMap(j => j.choose(text, q).map(j -> _)).nextOption()

    private def withConfidence(fields: Vector[(String, Json)], c: Judge.Choice, who: Judge): Json =
      JObj(fields ++
        Vector("probabilities" -> JObj(c.probabilities.map((n, p) => n -> JNum(p)))) ++
        c.confidence.map(v => "confidence" -> JNum(v)) ++
        Vector("judge" -> JStr(who.name)))

    /** one question answered, or its error */
    def answer(judges: Seq[Judge], text: String, a: Asked): Json = a.kind match
      case "choice" if a.options.nonEmpty =>
        first(judges, text, Judge.Question(a.options, a.instructions)) match
          case Some((who, c)) => withConfidence(Vector("choice" -> JStr(c.best)), c, who)
          case None => JObj(Vector("error" -> JStr(s"no judge here ranks: ${a.options.map(_._1).mkString(", ")}")))
      case "noul" =>
        val q = Judge.Question(Vector("yes" -> a.instructions, "no" -> s"not: ${a.instructions}"), a.instructions)
        first(judges, text, q) match
          case Some((who, c)) =>
            val p = c.probabilities.find(_._1 == "yes").map(_._2).getOrElse(0.0)
            withConfidence(Vector("noul" -> JNum(p)), c, who)
          case None => JObj(Vector("error" -> JStr("no judge here answers yes or no")))
      case "score" if a.options.nonEmpty =>
        first(judges, text, Judge.Question(a.options, a.instructions)) match
          case Some((who, c)) =>
            // the expected level under the option order, 1-based, as
            // the vendors count them — or by the level's own number
            // where the level is one
            val index = a.options.map(_._1).zipWithIndex.map((n, i) => n -> n.toDoubleOption.getOrElse(i + 1.0)).toMap
            val expected = c.probabilities.map((n, p) => index.getOrElse(n, 0.0) * p).sum
            withConfidence(Vector("score" -> JNum(expected)), c, who)
          case None => JObj(Vector("error" -> JStr(s"no judge here ranks: ${a.options.map(_._1).mkString(", ")}")))
      case other => JObj(Vector("error" -> JStr(s"unknown question type «$other», or no options")))

    /** the whole request: a status and a body, ready for a route */
    def serve(raw: String, judges: Seq[Judge]): (Int, Json) = decode(raw) match
      case Left(why) => (400, JObj(Vector("error" -> JStr(why))))
      case Right((text, asked)) =>
        (200, JObj(Vector(
          "answers" -> JObj(asked.map(a => a.key -> answer(judges, text, a))),
          "usage" -> JObj(Vector("input_tokens" -> JNum(text.length.toDouble), "output_tokens" -> JNum(0))))))
