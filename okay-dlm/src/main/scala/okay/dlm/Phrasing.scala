package okay.dlm

import okay.codec.Json
import okay.codec.Json.*

/**
 * THE CALLER'S OWN WORDS at the moments that sound most like a robot:
 * the read-back said before anything is written, the question that
 * separates two candidates the router could not, and the one or two
 * words a field is called by.
 *
 * All three are functions of AUTHORED data, not of the person's
 * sentence — which is why they never need a model. The strings are a
 * person's, one cell per key with every language beside it, and a
 * cell nobody has written yet is not a defect: the caller's generic
 * phrase answers. A cell written in SOME languages and not all IS a
 * defect — that is how a person got the wrong language back — and
 * `holes` names it for a test.
 *
 * `{what}` in a string is replaced by what the caller composes.
 */
final case class Phrasing(readback: Map[String, Map[String, String]],
                          distinguish: Map[String, Map[String, String]],
                          field: Map[String, Map[String, String]] = Map.empty):

  def readBack(key: String, lang: String, what: String): Option[String] =
    readback.get(key).flatMap(_.get(lang)).map(_.replace("{what}", what))

  def distinguishing(key: String, lang: String, what: String = ""): Option[String] =
    distinguish.get(key).flatMap(_.get(lang)).map(_.replace("{what}", what))

  /** the subject of a question, in one or two words: the field says
   * WHICH, the corpus says the word */
  def fieldName(name: String, lang: String): Option[String] =
    field.get(name).flatMap(_.get(lang))

  /** is there an authored question, in this language, that separates
   * exactly these two candidates? */
  def distinguishable(pair: Vector[String], lang: String): Boolean =
    pair.length == 2 && distinguish.get(Phrasing.pairKey(pair(0), pair(1))).exists(_.contains(lang))

  /** (kind, key, missing languages): cells written in some of
   * `languages` and not every one */
  def holes(languages: Set[String]): Vector[(String, String, Vector[String])] =
    def of(kind: String, m: Map[String, Map[String, String]]) =
      m.toVector.sortBy(_._1).flatMap((k, byLang) =>
        val missing = (languages -- byLang.keySet).toVector.sorted
        Option.when(missing.nonEmpty)((kind, k, missing)))
    of("readback", readback) ++ of("distinguish", distinguish) ++ of("field", field)

  /** which authored cell a reply came from, if any — by its template */
  def cellOf(reply: String): Option[(String, String, String)] =
    def hit(kind: String, m: Map[String, Map[String, String]]) =
      m.iterator.flatMap((k, byLang) => byLang.iterator.collect {
        case (lang, t) if Phrasing.fromTemplate(t, reply) => (kind, k, lang) }).nextOption()
    hit("readback", readback).orElse(hit("distinguish", distinguish))

object Phrasing:

  val empty: Phrasing = Phrasing(Map.empty, Map.empty, Map.empty)

  /** the key a distinguishing question for two intents is authored
   * under: order-free, so "a|b" and "b|a" are one cell */
  def pairKey(a: String, b: String): String = Vector(a, b).sorted.mkString("|")

  /** does `reply` instantiate `template`, where `{what}` (if present)
   * stood for anything? A prefix match on what precedes the
   * placeholder and a containment match on what follows it — a caller
   * may append a hint after a reply, so the end is not the template's
   * to claim */
  def fromTemplate(template: String, reply: String): Boolean =
    template.indexOf("{what}") match
      case -1 => reply.startsWith(template)
      case i =>
        val (pre, post) = (template.take(i), template.drop(i + 6))
        reply.startsWith(pre) && (post.isEmpty || reply.indexOf(post, pre.length) >= 0)

  def parse(raw: String): Either[String, Phrasing] =
    def table(v: Json): Either[String, Map[String, Map[String, String]]] = v match
      case JObj(cells) =>
        val bad = cells.collect { case (k, v) if !v.isInstanceOf[JObj] => k }
        if bad.nonEmpty then Left(s"cells must be objects by language: ${bad.mkString(", ")}")
        else Right(cells.collect { case (k, JObj(byLang)) =>
          k -> byLang.collect { case (l, JStr(s)) => l -> s }.toMap }.toMap)
      case _ => Left("not an object")
    Json.parse(raw) match
      case JObj(fs) =>
        // keys opening with "_" are notes and examples for the person
        // editing the file, never strings anybody says
        val live = fs.filterNot(_._1.startsWith("_"))
        for
          rb <- live.collectFirst { case ("readback", v) => table(v) }.getOrElse(Right(Map.empty))
          di <- live.collectFirst { case ("distinguish", v) => table(v) }.getOrElse(Right(Map.empty))
          fl <- live.collectFirst { case ("field", v) => table(v) }.getOrElse(Right(Map.empty))
        yield Phrasing(rb, di, fl)
      case _ => Left("not a JSON object")

  /** the strings shipped in a jar; `empty` when there are none, which
   * is a caller that says only its generic phrases */
  def resource(name: String): Phrasing =
    Option(getClass.getResourceAsStream(name)).fold(empty) { in =>
      val raw = try new String(in.readAllBytes(), "UTF-8") finally in.close()
      parse(raw).fold(e => throw IllegalStateException(s"$name: $e"), identity)
    }
