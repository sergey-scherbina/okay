package okay.matching

import okay.codec.Json
import okay.codec.Json.*
import okay.agent.{ToolCall, ToolSpec}

/**
 * The effect operations as LLM tools, 1:1 (specs/match.md): the
 * framework the structuring model works inside IS this table. The
 * pair (specs, table) is exactly what `mcp.Server.serve` takes, so
 * serving it over MCP is one call at the integration site — stage 0
 * keeps the module free of that dependency.
 */
object Tools {

  private def obj(fs: (String, Json)*): Json = JObj(fs.toVector)
  private def s(j: Json, k: String): Option[String] = j match
    case JObj(fs) => fs.collectFirst { case (`k`, JStr(v)) => v }
    case _ => None
  private def d(j: Json, k: String): Option[Double] = j match
    case JObj(fs) => fs.collectFirst { case (`k`, JNum(v)) => v }
    case _ => None
  private def a(j: Json, k: String): Vector[Json] = j match
    case JObj(fs) => fs.collectFirst { case (`k`, JArr(vs)) => vs }.getOrElse(Vector.empty)
    case _ => Vector.empty

  private def value(j: Json): Value = s(j, "t") match
    case Some("num") => Value.VNum(d(j, "n").getOrElse(0.0))
    case Some("range") => Value.VRange(d(j, "lo").getOrElse(0.0), d(j, "hi").getOrElse(0.0))
    case Some("geo") => Value.VGeo(d(j, "lat").getOrElse(0.0), d(j, "lon").getOrElse(0.0))
    case Some("time") => Value.VTime(s(j, "s").getOrElse(""))
    case Some("ref") => Value.VRef(ProfileId(s(j, "s").getOrElse("")))
    case _ => Value.VText(s(j, "s").getOrElse(""))

  private def valueJson(v: Value): Json = v match
    case Value.VText(x) => obj("t" -> JStr("text"), "s" -> JStr(x))
    case Value.VNum(n) => obj("t" -> JStr("num"), "n" -> JNum(n))
    case Value.VRange(lo, hi) => obj("t" -> JStr("range"), "lo" -> JNum(lo), "hi" -> JNum(hi))
    case Value.VGeo(la, lo) => obj("t" -> JStr("geo"), "lat" -> JNum(la), "lon" -> JNum(lo))
    case Value.VTime(x) => obj("t" -> JStr("time"), "s" -> JStr(x))
    case Value.VRef(p) => obj("t" -> JStr("ref"), "s" -> JStr(p.uuid))

  private def pred(j: Json): Pred = s(j, "op") match
    case Some("atLeast") => Pred.AtLeast(d(j, "x").getOrElse(0.0))
    case Some("atMost") => Pred.AtMost(d(j, "x").getOrElse(0.0))
    case Some("within") => Pred.Within(d(j, "lat").getOrElse(0.0),
      d(j, "lon").getOrElse(0.0), d(j, "km").getOrElse(0.0))
    case Some("hasText") => Pred.HasText(s(j, "s").getOrElse(""))
    case _ => Pred.Is(value(j))

  private def b(j: Json, k: String): Boolean = j match
    case JObj(fs) => fs.collectFirst { case (`k`, JBool(v)) => v }.getOrElse(false)
    case _ => false

  private def side(x: Option[String]): Side =
    if x.contains("need") then Side.Need else Side.Offer

  private def vis(x: Option[String]): Vis = x match
    case Some("matched") => Vis.Matched
    case Some("private") => Vis.Private
    case _ => Vis.Public

  private def factJson(f: Fact): Json = obj(
    "id" -> JNum(f.id.n.toDouble), "attr" -> JStr(f.attr),
    "side" -> JStr(f.side.toString.toLowerCase),
    "value" -> valueJson(f.value), "confidence" -> JNum(f.confidence),
    "chat" -> JStr(f.prov.chat), "span" -> JStr(f.prov.span),
    "superseded" -> JBool(f.supersededBy.isDefined))

  private def attrJson(x: AttrDef): Json = obj(
    "slug" -> JStr(x.slug), "kind" -> JStr(x.kind.toString.toLowerCase),
    "description" -> JStr(x.description),
    "synonyms" -> JArr(x.synonyms.map(JStr(_))),
    "status" -> JStr(x.status.toString))

  private def strSchema(props: (String, String)*): Json = obj(
    "type" -> JStr("object"),
    "properties" -> JObj(props.toVector.map((n, t) =>
      n -> obj("type" -> JStr(t)))))

  val specs: Seq[ToolSpec] = Seq(
    ToolSpec("registry_search",
      "Search the attribute registry BEFORE inventing a new attribute; reuse a hit.",
      strSchema("text" -> "string")),
    ToolSpec("registry_propose",
      "Register a new attribute if registry_search found nothing; a near-duplicate returns the existing one.",
      strSchema("slug" -> "string", "kind" -> "string", "description" -> "string")),
    ToolSpec("facts_register",
      "Register (or find) the profile for an email; returns its id.",
      strSchema("email" -> "string")),
    ToolSpec("facts_assert",
      "Record one extracted statement with provenance to the chat span.",
      strSchema("profile" -> "string", "attr" -> "string", "side" -> "string",
        "chat" -> "string", "span" -> "string", "vis" -> "string")),
    ToolSpec("facts_supersede",
      "Replace a fact's value after the user confirmed the update; keeps history.",
      strSchema("fact" -> "number", "reason" -> "string", "chat" -> "string", "span" -> "string")),
    ToolSpec("facts_profile",
      "Read a profile's current facts and history — for noticing conflicts mid-chat.",
      strSchema("profile" -> "string")),
    ToolSpec("ident_candidates",
      "Does another profile share this profile's identifying facts? Answers attribute + masked hint only.",
      strSchema("profile" -> "string")),
    ToolSpec("ident_request",
      "Mint a link token for the OLD profile; deliver it through the OLD channel, never this chat.",
      strSchema("from" -> "string", "to" -> "string")),
    ToolSpec("ident_confirm",
      "The user typed the token they received on the old channel: confirm the link.",
      strSchema("token" -> "string", "by" -> "string", "chat" -> "string", "span" -> "string")),
    ToolSpec("find_candidates",
      "Hybrid search: hard filters over typed facts, semantic ranking by text.",
      strSchema("side" -> "string", "text" -> "string")))

  /** the dispatch table over one store; args and answers are Json */
  def table(m: MatchStore): Map[String, ToolCall => String] = Map(
    "registry_search" -> { c =>
      Json.print(JArr(m.registrySearch(s(c.args, "text").getOrElse(""))
        .map(attrJson).toVector)) },
    "registry_propose" -> { c =>
      val k = s(c.args, "kind").map(_.capitalize).flatMap(x =>
        Kind.values.find(_.toString == x)).getOrElse(Kind.Text)
      Json.print(attrJson(m.propose(AttrDraft(
        s(c.args, "slug").getOrElse(""), k,
        s(c.args, "description").getOrElse(""),
        a(c.args, "synonyms").collect { case JStr(x) => x },
        identifying = b(c.args, "identifying"))))) },
    "facts_register" -> { c =>
      Json.print(obj("profile" ->
        JStr(m.register(s(c.args, "email").getOrElse("")).uuid))) },
    "facts_assert" -> { c =>
      val id = m.assert(
        ProfileId(s(c.args, "profile").getOrElse("")),
        s(c.args, "attr").getOrElse(""),
        side(s(c.args, "side")),
        value(c.args match { case JObj(fs) =>
          fs.collectFirst { case ("value", v) => v }.getOrElse(JNull); case _ => JNull }),
        Provenance(s(c.args, "chat").getOrElse(""),
          d(c.args, "offset").getOrElse(0.0).toLong,
          s(c.args, "span").getOrElse("")),
        d(c.args, "confidence").getOrElse(1.0),
        vis(s(c.args, "vis")))
      Json.print(obj("fact" -> JNum(id.n.toDouble))) },
    "facts_supersede" -> { c =>
      val id = m.supersede(
        FactId(d(c.args, "fact").getOrElse(0.0).toLong),
        value(c.args match { case JObj(fs) =>
          fs.collectFirst { case ("value", v) => v }.getOrElse(JNull); case _ => JNull }),
        s(c.args, "reason").getOrElse(""),
        Provenance(s(c.args, "chat").getOrElse(""),
          d(c.args, "offset").getOrElse(0.0).toLong,
          s(c.args, "span").getOrElse("")))
      Json.print(obj("fact" -> JNum(id.n.toDouble))) },
    "facts_profile" -> { c =>
      m.profileOf(ProfileId(s(c.args, "profile").getOrElse(""))) match
        case None => Json.print(JNull)
        case Some(p) => Json.print(obj(
          "email" -> JStr(p.email),
          "current" -> JArr(p.current.map(factJson)),
          "history" -> JArr(p.history.map(factJson)))) },
    "ident_candidates" -> { c =>
      Json.print(JArr(m.linkCandidates(ProfileId(s(c.args, "profile").getOrElse("")))
        .map(h => obj("attr" -> JStr(h.attr), "hint" -> JStr(h.hint))).toVector)) },
    "ident_request" -> { c =>
      m.requestLink(ProfileId(s(c.args, "from").getOrElse("")),
        ProfileId(s(c.args, "to").getOrElse(""))) match
        case None => Json.print(JNull)
        case Some(t) => Json.print(obj("token" -> JStr(t.token),
          "to" -> JStr(t.to.uuid), "expiresAt" -> JNum(t.expiresAt.toDouble))) },
    "ident_confirm" -> { c =>
      m.confirmLink(s(c.args, "token").getOrElse(""),
        ProfileId(s(c.args, "by").getOrElse("")),
        Provenance(s(c.args, "chat").getOrElse(""),
          d(c.args, "offset").getOrElse(0.0).toLong,
          s(c.args, "span").getOrElse(""))) match
        case None => Json.print(JNull)
        case Some(p) => Json.print(obj("linked" -> JStr(p.uuid))) },
    "find_candidates" -> { c =>
      val q = Query(side(s(c.args, "side")),
        a(c.args, "filters").map(f =>
          (s(f, "attr").getOrElse(""), pred(f))),
        s(c.args, "text").getOrElse(""),
        d(c.args, "k").getOrElse(10.0).toInt)
      Json.print(JArr(m.candidates(q).map(r => obj(
        "profile" -> JStr(r.profile.uuid), "score" -> JNum(r.score.toDouble),
        "facts" -> JArr(r.disclosed.map(factJson)))))) })
}
