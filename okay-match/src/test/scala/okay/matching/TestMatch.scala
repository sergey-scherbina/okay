package okay.matching

import okay.*
import okay.given
import okay.codec.Json
import okay.agent.ToolCall

/** specs/match.md, Behavior (stage 0) — one test per checkbox */
class TestMatch extends munit.FunSuite {

  def prov(chat: String, off: Long, quote: String) = Provenance(chat, off, quote)

  test("search-before-create: a near-duplicate propose returns the existing attribute") {
    val m = MemoryMatch()
    val first = m.propose(AttrDraft("schedule", Kind.Time,
      "when the provider is available for work during the week"))
    // exact synonym route
    val bySyn = m.propose(AttrDraft("working-hours", Kind.Time,
      "hours of availability", synonyms = Vector("schedule")))
    assertEquals(bySyn.id, first.id)
    // near-duplicate description route (identical text: cosine 1)
    val byDesc = m.propose(AttrDraft("timetable", Kind.Time,
      "when the provider is available for work during the week"))
    assertEquals(byDesc.id, first.id)
    // and a genuinely new one is genuinely new
    val other = m.propose(AttrDraft("price", Kind.Num, "price per hour in UAH"))
    assert(other.id != first.id)
    assertEquals(m.registrySearch("availability during the week").head.id, first.id)
  }

  test("facts carry provenance; supersede keeps history; profile shows both") {
    val m = MemoryMatch()
    val p = m.register("master@example.com")
    val f1 = m.assert(p, "schedule", Side.Offer, Value.VTime("weekends free"),
      prov("chat-1", 10, "по выходным свободен"), 0.9, Vis.Public)
    val f2 = m.supersede(f1, Value.VTime("weekends busy"),
      "user confirmed the update in chat-2", prov("chat-2", 4, "теперь по выходным занят"))
    val prof = m.profileOf(p).get
    assertEquals(prof.current.map(_.id), Vector(f2))
    assertEquals(prof.current.head.reason, Some("user confirmed the update in chat-2"))
    assertEquals(prof.history.find(_.id == f1).get.supersededBy, Some(f2))
    assertEquals(prof.history.find(_.id == f1).get.prov.span, "по выходным свободен")
  }

  test("visibility: Private never matches; platform-withheld matches but is not disclosed") {
    val m = MemoryMatch(policy = PlatformPolicy.withhold("phone"))
    val p = m.register("master@example.com")
    m.assert(p, "skill", Side.Offer, Value.VText("plumbing repair"),
      prov("c", 1, "умею чинить сантехнику"), 1.0, Vis.Public)
    m.assert(p, "phone", Side.Offer, Value.VText("+380501234567"),
      prov("c", 2, "мой телефон"), 1.0, Vis.Public)      // owner would show it; the platform won't
    m.assert(p, "secret", Side.Offer, Value.VText("do not match me on this"),
      prov("c", 3, "..."), 1.0, Vis.Private)
    val hits = m.candidates(Query(Side.Offer, text = "plumbing repair"))
    assertEquals(hits.head.profile, p)
    val attrs = hits.head.disclosed.map(_.attr)
    assert(attrs.contains("skill"))
    assert(!attrs.contains("phone"), "the platform gate leaked")
    assert(!attrs.contains("secret"))
    // and Private is excluded from matching itself
    val bySecret = m.candidates(Query(Side.Offer,
      filters = Vector("secret" -> Pred.HasText("match me"))))
    assert(bySecret.isEmpty, "a Private fact participated in matching")
  }

  test("replay idempotence: re-extracting the same chat changes nothing") {
    val m = MemoryMatch()
    val p = m.register("master@example.com")
    def extract(): Vector[FactId] = Vector(
      m.assert(p, "skill", Side.Offer, Value.VText("tiling"),
        prov("chat-1", 1, "кладу плитку"), 1.0, Vis.Public),
      m.assert(p, "price", Side.Offer, Value.VNum(500),
        prov("chat-1", 2, "беру 500 за метр"), 1.0, Vis.Public))
    val first = extract()
    val again = extract()
    assertEquals(again, first)
    assertEquals(m.profileOf(p).get.history.length, 2)
    assertEquals(m.register("master@example.com"), p)   // registration is idempotent too
  }

  test("hybrid search: hard filters exclude, similarity ranks, a stored need finds its provider") {
    val m = MemoryMatch()
    val kyiv = m.register("kyiv-tiler@example.com")
    m.assert(kyiv, "skill", Side.Offer, Value.VText("tiling bathrooms kitchens"),
      prov("c1", 1, "..."), 1.0, Vis.Public)
    m.assert(kyiv, "location", Side.Offer, Value.VGeo(50.45, 30.52),
      prov("c1", 2, "..."), 1.0, Vis.Public)
    m.assert(kyiv, "price", Side.Offer, Value.VNum(500), prov("c1", 3, "..."), 1.0, Vis.Public)
    val lviv = m.register("lviv-tiler@example.com")
    m.assert(lviv, "skill", Side.Offer, Value.VText("tiling bathrooms"),
      prov("c2", 1, "..."), 1.0, Vis.Public)
    m.assert(lviv, "location", Side.Offer, Value.VGeo(49.84, 24.03),
      prov("c2", 2, "..."), 1.0, Vis.Public)
    // the seeker: needs a tiler near Kyiv under 600
    val hits = m.candidates(Query(Side.Offer,
      filters = Vector(
        "location" -> Pred.Within(50.45, 30.52, 50),
        "price" -> Pred.AtMost(600)),
      text = "tiling a bathroom"))
    assertEquals(hits.map(_.profile), Vector(kyiv))     // Lviv excluded by geo, no price fact either
    // symmetric: the need is stored by the same machinery and searchable
    val seeker = m.register("seeker@example.com")
    m.assert(seeker, "need", Side.Need, Value.VText("need bathroom tiling in Kyiv"),
      prov("c3", 1, "нужно выложить плитку в ванной"), 1.0, Vis.Public)
    val needs = m.candidates(Query(Side.Need, text = "bathroom tiling"))
    assertEquals(needs.head.profile, seeker)
  }

  test("stage 2 — rerank: the effect reorders the top slice") {
    val m = MemoryMatch()
    val plumber = m.register("plumber@example.com")
    m.assert(plumber, "skill", Side.Offer, Value.VText("fixing pipes and taps"),
      prov("c1", 1, "..."), 1.0, Vis.Public)
    val tiler = m.register("tiler@example.com")
    m.assert(tiler, "skill", Side.Offer, Value.VText("tiling bathrooms"),
      prov("c2", 1, "..."), 1.0, Vis.Public)
    given Handler[Find + Rerank] = new:
      def handle[A](e: Find[A] | Rerank[A]): A = e match
        case f: Find.Candidates => m.find.handle(f)
        case r: Rerank.Order => Rerank.lexical.handle(r)
    val hits = top(Query(Side.Offer, text = "who can fix my taps", k = 2)).runWith
    assertEquals(hits.head.profile, plumber)
  }

  test("stage 2 — the policy engine names what waits behind the match gate") {
    val m = MemoryMatch(policy = PlatformPolicy.afterMatch("phone"))
    val p = m.register("master@example.com")
    m.assert(p, "skill", Side.Offer, Value.VText("welding"), prov("c", 1, "..."), 1.0, Vis.Public)
    m.assert(p, "phone", Side.Offer, Value.VText("+38050"), prov("c", 2, "..."), 1.0, Vis.Public)
    val hit = m.candidates(Query(Side.Offer, text = "welding")).head
    assert(!hit.disclosed.exists(_.attr == "phone"))
    assertEquals(hit.withheld, Vector("phone"))         // THAT it matched — not WHAT
  }

  test("stage 2 — volatility: a stale volatile fact drags the rank, a stable one does not") {
    var clock = 0L
    val m = MemoryMatch(halfLifeMs = 1000, now = () => clock)
    m.propose(AttrDraft("availability", Kind.Time, "when free this week", volatile = true))
    val stale = m.register("stale@example.com")
    m.assert(stale, "skill", Side.Offer, Value.VText("painting walls"), prov("c1", 1, "..."), 1.0, Vis.Public)
    m.assert(stale, "availability", Side.Offer, Value.VTime("this week"), prov("c1", 2, "..."), 1.0, Vis.Public)
    clock = 10_000                                       // ten half-lives later
    val fresh = m.register("fresh@example.com")
    m.assert(fresh, "skill", Side.Offer, Value.VText("painting walls"), prov("c2", 1, "..."), 1.0, Vis.Public)
    m.assert(fresh, "availability", Side.Offer, Value.VTime("this week"), prov("c2", 2, "..."), 1.0, Vis.Public)
    val hits = m.candidates(Query(Side.Offer, text = "painting walls"))
    assertEquals(hits.map(_.profile), Vector(fresh, stale))
    assert(hits(0).score > hits(1).score * 100)          // exp2(-10) is dust
  }

  test("stage 2 — recovery: the secret rebinds the email; without it a stranger gets nothing") {
    val m = MemoryMatch(hash = s => "h:" + s, verifyHash = (s, st) => "h:" + s == st)
    val p = m.register("old@example.com")
    m.setRecovery(p, "correct horse battery staple")
    assertEquals(m.rebind("old@example.com", "new@example.com", "wrong guess"), None)
    assertEquals(m.rebind("old@example.com", "new@example.com", "correct horse battery staple"), Some(p))
    assertEquals(m.register("new@example.com"), p)       // the new email finds the OLD profile
    assert(m.register("old@example.com") != p)           // and the old address is a stranger now
  }

  test("the tools mirror the operations: a two-side scripted scenario matches end to end") {
    val m = MemoryMatch()
    val t = Tools.table(m)
    def call(name: String, args: (String, Json)*): Json =
      Json.parse(t(name)(ToolCall("t", name, Json.JObj(args.toVector))))
    def str(j: Json, k: String): String = j match
      case Json.JObj(fs) => fs.collectFirst { case (`k`, Json.JStr(v)) => v }.get
      case _ => fail(s"no $k in $j")

    assertEquals(Tools.specs.map(_.name).toSet, t.keySet)

    // the provider's chat
    val provider = str(call("facts_register", "email" -> Json.JStr("master@example.com")), "profile")
    call("registry_propose", "slug" -> Json.JStr("skill"), "kind" -> Json.JStr("text"),
      "description" -> Json.JStr("what the provider can do"))
    call("facts_assert", "profile" -> Json.JStr(provider), "attr" -> Json.JStr("skill"),
      "side" -> Json.JStr("offer"), "chat" -> Json.JStr("chat-1"),
      "span" -> Json.JStr("клею обои, шпаклюю стены"),
      "value" -> Json.JObj(Vector("t" -> Json.JStr("text"),
        "s" -> Json.JStr("wallpapering and wall finishing"))))
    // the seeker's chat
    val hits = call("find_candidates", "side" -> Json.JStr("offer"),
      "text" -> Json.JStr("wallpapering my flat"))
    hits match
      case Json.JArr(vs) =>
        assert(vs.nonEmpty, "the seeker found nobody")
        assertEquals(str(vs.head, "profile"), provider)
      case other => fail(s"unexpected: $other")
    // and the profile tool shows the provenance the assert carried
    val prof = call("facts_profile", "profile" -> Json.JStr(provider))
    assert(Json.print(prof).contains("клею обои"))
  }
}
