package okay.matching

import okay.{!, pure, Pure}
import okay.given
import okay.codec.Json
import okay.{!, pure, Pure}
import okay.given
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

  /** a malformed tool value: the tag said one thing, the payload
   * another — the condition the parsing signals */
  final case class MalformedValue(tag: String, payload: Json)
  given okay.Condition.Answers[MalformedValue, Value] =
    okay.Condition.Answers.of[MalformedValue, Value]

  /** the silent coercions of v1, now a NAMED policy: what the
   * default table invokes so nothing changes for existing users */
  val legacy: (Any, Vector[String]) => okay.Condition.Decision =
    case (_: MalformedValue, menu) if menu.contains("legacy") =>
      okay.Condition.Decision.Invoke("legacy", ())
    case _ => okay.Condition.Decision.Fail

  /** parse a tagged value or SIGNAL; the legacy frame supplies the
   * old coercion when the policy asks for it */
  private def valueOr(j: Json): Value ! okay.Condition.Op =
    def field[A](ok: Option[A], mk: A => Value): Value ! okay.Condition.Op =
      ok match
        case Some(x) => pure(mk(x))
        case None => okay.Condition.raiseC(
          MalformedValue(s(j, "t").getOrElse("text"), j))
    s(j, "t") match
      case Some("num") => field(d(j, "n"), Value.VNum(_))
      case Some("range") =>
        (d(j, "lo"), d(j, "hi")) match
          case (Some(lo), Some(hi)) => pure(Value.VRange(lo, hi))
          case _ => okay.Condition.raiseC(MalformedValue("range", j))
      case Some("geo") =>
        (d(j, "lat"), d(j, "lon")) match
          case (Some(la), Some(lo)) => pure(Value.VGeo(la, lo))
          case _ => okay.Condition.raiseC(MalformedValue("geo", j))
      case Some("time") => pure(Value.VTime(s(j, "s").getOrElse("")))
      case Some("ref") => field(s(j, "s"), (x: String) => Value.VRef(ProfileId(x)))
      case _ => field(s(j, "s"), Value.VText(_))

  /** run a value parse under the policy; the legacy restart is on
   * the menu, and an Unhandled becomes a refusal the caller SEES */
  private def parsed(j: Json,
                     policy: (Any, Vector[String]) => okay.Condition.Decision)
  : Either[String, Value] =
    try Right(okay.Condition.run[Value, Pure](policy)(
      okay.Condition.within[Value, Pure]("legacy")(valueOr(j))(_ => value(j))
    ).runWith)
    catch case u: okay.Condition.Unhandled => Left(u.getMessage)

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

  private object FactIdOps:
    def deal(n: Option[Double]): DealId = DealId(n.getOrElse(0.0).toLong)
    def flow(n: Option[Double]): FlowId = FlowId(n.getOrElse(0.0).toLong)

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
      strSchema("side" -> "string", "text" -> "string")),
    ToolSpec("match_inquire",
      "Ask a candidate whether they take the job; several may be asked — someone agrees.",
      strSchema("seeker" -> "string", "provider" -> "string", "what" -> "string")),
    ToolSpec("match_respond",
      "The ASKED candidate accepts or declines the inquiry (accept: true/false).",
      strSchema("deal" -> "number", "by" -> "string", "accept" -> "boolean")),
    ToolSpec("match_deals",
      "A profile's inquiries, both directions, with their states.",
      strSchema("profile" -> "string")),
    ToolSpec("flow_start",
      "Start a registered scenario: parties = {role: profileId,...}; answers the flow id.",
      strSchema("scenario" -> "string", "what" -> "string")),
    ToolSpec("flow_advance",
      "Fire a transition on a flow; only the transition's role may. Answers the new state.",
      strSchema("flow" -> "number", "transition" -> "string", "by" -> "string")),
    ToolSpec("flow_state",
      "A flow's scenario, state, parties and history.",
      strSchema("flow" -> "number")),
    ToolSpec("scenario_get",
      "A registered scenario definition: roles, states, transitions.",
      strSchema("name" -> "string")),
    ToolSpec("match_contacts",
      "The other party's contact facts — unlocked ONLY by an accepted deal between the two.",
      strSchema("viewer" -> "string", "other" -> "string")))

  /** the dispatch table over one store; args and answers are Json.
   * The default POLICY for malformed values is `legacy` — the v1
   * coercions, unchanged; pass a policy to repair or refuse instead
   * (see MalformedValue): a strict policy turns a malformed value
   * into a {"refused": ...} answer the model can read and retry. */
  def table(m: MatchStore): Map[String, ToolCall => String] = table(m, legacy)

  def table(m: MatchStore,
            policy: (Any, Vector[String]) => okay.Condition.Decision)
  : Map[String, ToolCall => String] = Map(
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
      val raw = c.args match { case JObj(fs) =>
        fs.collectFirst { case ("value", v) => v }.getOrElse(JNull); case _ => JNull }
      parsed(raw, policy) match
       case Left(refusal) => Json.print(obj("refused" -> JStr(refusal)))
       case Right(v) =>
        val id = m.assert(
        ProfileId(s(c.args, "profile").getOrElse("")),
        s(c.args, "attr").getOrElse(""),
        side(s(c.args, "side")),
        v,
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
    "match_inquire" -> { c =>
      val id = m.inquire(ProfileId(s(c.args, "seeker").getOrElse("")),
        ProfileId(s(c.args, "provider").getOrElse("")),
        s(c.args, "what").getOrElse(""))
      Json.print(obj("deal" -> JNum(id.n.toDouble))) },
    "match_respond" -> { c =>
      m.respond(FactIdOps.deal(d(c.args, "deal")),
        ProfileId(s(c.args, "by").getOrElse("")),
        b(c.args, "accept")) match
        case None => Json.print(JNull)
        case Some(dl) => Json.print(obj("deal" -> JNum(dl.id.n.toDouble),
          "state" -> JStr(dl.state.toString))) },
    "match_deals" -> { c =>
      Json.print(JArr(m.dealsFor(ProfileId(s(c.args, "profile").getOrElse("")))
        .map(dl => obj("deal" -> JNum(dl.id.n.toDouble),
          "seeker" -> JStr(dl.seeker.uuid), "provider" -> JStr(dl.provider.uuid),
          "what" -> JStr(dl.what), "state" -> JStr(dl.state.toString))))) },
    "flow_start" -> { c =>
      val parties = (c.args match
        case JObj(fs) => fs.collectFirst { case ("parties", JObj(ps)) => ps }
          .getOrElse(Vector.empty)
        case _ => Vector.empty
      ).collect { case (r, JStr(u)) => r -> ProfileId(u) }.toMap
      m.startFlow(s(c.args, "scenario").getOrElse(""), parties,
        s(c.args, "what").getOrElse("")) match
        case Left(no) => Json.print(obj("refused" -> JStr(no.reason)))
        case Right(id) => Json.print(obj("flow" -> JNum(id.n.toDouble))) },
    "flow_advance" -> { c =>
      m.advanceFlow(FactIdOps.flow(d(c.args, "flow")),
        s(c.args, "transition").getOrElse(""),
        ProfileId(s(c.args, "by").getOrElse(""))) match
        case Left(no) => Json.print(obj("refused" -> JStr(no.reason)))
        case Right((f, _)) => Json.print(obj("flow" -> JNum(f.id.n.toDouble),
          "state" -> JStr(f.state))) },
    "flow_state" -> { c =>
      m.flow(FactIdOps.flow(d(c.args, "flow"))) match
        case None => Json.print(JNull)
        case Some(f) => Json.print(obj(
          "scenario" -> JStr(f.scenario), "state" -> JStr(f.state),
          "what" -> JStr(f.what),
          "parties" -> JObj(f.parties.toVector.map((r, p) => r -> JStr(p.uuid))),
          "history" -> JArr(f.history.map((t, by, ts) => obj(
            "transition" -> JStr(t), "by" -> JStr(by.uuid), "ts" -> JNum(ts.toDouble)))))) },
    "scenario_get" -> { c =>
      m.scenario(s(c.args, "name").getOrElse("")) match
        case None => Json.print(JNull)
        case Some(sc) => Json.print(obj(
          "name" -> JStr(sc.name),
          "roles" -> JArr(sc.roles.map(JStr(_))),
          "initial" -> JStr(sc.initial),
          "states" -> JArr(sc.states.map(JStr(_))),
          "terminal" -> JArr(sc.terminal.toVector.sorted.map(JStr(_))),
          "transitions" -> JArr(sc.transitions.map(t => obj(
            "name" -> JStr(t.name), "from" -> JStr(t.from), "to" -> JStr(t.to),
            "by" -> JStr(t.by)))))) },
    "match_contacts" -> { c =>
      Json.print(JArr(m.contacts(
        ProfileId(s(c.args, "viewer").getOrElse("")),
        ProfileId(s(c.args, "other").getOrElse("")))
        .map(factJson))) },
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
