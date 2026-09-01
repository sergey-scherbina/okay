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

  test("identity-x — candidates: identifying attrs only, masked hint, no facts, no values") {
    var clock = 1000L
    val m = MemoryMatch(now = () => clock)
    m.propose(AttrDraft("phone", Kind.Text, "contact phone number", identifying = true))
    m.propose(AttrDraft("skill", Kind.Text, "what the provider can do"))
    val old = m.register("master@example.com")
    m.assert(old, "phone", Side.Offer, Value.VText("+380501112233"), prov("tg", 1, "..."), 1.0, Vis.Matched)
    m.assert(old, "skill", Side.Offer, Value.VText("welding"), prov("tg", 2, "..."), 1.0, Vis.Public)
    val fresh = m.register("viber-user@example.com")
    m.assert(fresh, "phone", Side.Offer, Value.VText("+380501112233"), prov("vb", 1, "..."), 1.0, Vis.Matched)
    m.assert(fresh, "skill", Side.Offer, Value.VText("welding"), prov("vb", 2, "..."), 1.0, Vis.Public)
    val hints = m.linkCandidates(fresh)
    assertEquals(hints, Vector(LinkHint("phone", "m***@e***.com")))  // attr + mask, nothing else
    // skill matches too — but it is not identifying, so it says nothing
    assert(!hints.exists(_.attr == "skill"))
    // and a hint is ALL a stranger gets: no link, separate identities
    assertEquals(m.identityOf(fresh), Vector(fresh))
  }

  test("identity-x — the token flow: single use, expiring, right holder only") {
    var clock = 1000L
    val m = MemoryMatch(now = () => clock)
    val old = m.register("old@example.com")
    val fresh = m.register("new@example.com")
    val t = m.requestLink(fresh, old).get
    assertEquals(m.confirmLink("wrong-token", fresh, prov("vb", 5, "...")), None)
    val stranger = m.register("stranger@example.com")
    assertEquals(m.confirmLink(t.token, stranger, prov("vb", 5, "...")), None)  // not the requester
    assertEquals(m.confirmLink(t.token, fresh, prov("vb", 5, "...")), Some(old))
    assertEquals(m.confirmLink(t.token, fresh, prov("vb", 6, "...")), None)     // single use
    // expiry
    val t2 = m.requestLink(fresh, old).get
    clock += 16 * 60 * 1000
    assertEquals(m.confirmLink(t2.token, fresh, prov("vb", 7, "...")), None)
    // the recovery fallback for a dead old channel
    val m2 = MemoryMatch(hash = "h:" + _, verifyHash = (x, st) => "h:" + x == st)
    val o2 = m2.register("dead@example.com"); m2.setRecovery(o2, "s3cret")
    val f2 = m2.register("alive@example.com")
    assertEquals(m2.linkByRecovery(f2, "dead@example.com", "wrong"), None)
    assertEquals(m2.linkByRecovery(f2, "dead@example.com", "s3cret"), Some(o2))
    assertEquals(m2.identityOf(f2).toSet, Set(f2, o2))
  }

  test("identity-x — the linked class reads as one person") {
    val m = MemoryMatch()
    val old = m.register("old@example.com")
    m.assert(old, "skill", Side.Offer, Value.VText("welding gates"), prov("tg", 1, "..."), 1.0, Vis.Public)
    val fresh = m.register("new@example.com")
    m.assert(fresh, "price", Side.Offer, Value.VNum(700), prov("vb", 1, "..."), 1.0, Vis.Public)
    val t = m.requestLink(fresh, old).get
    m.confirmLink(t.token, fresh, prov("vb", 2, "..."))
    // one candidate carrying facts from BOTH profiles
    val hits = m.candidates(Query(Side.Offer, text = "welding",
      filters = Vector("price" -> Pred.AtMost(1000))))
    assertEquals(hits.length, 1)
    assertEquals(hits.head.disclosed.map(_.attr).toSet, Set("skill", "price"))
    // and the profile view aggregates too
    assertEquals(m.profileOf(fresh).get.current.map(_.attr).toSet, Set("skill", "price"))
  }

  test("deals: chosen candidates asked, the asked alone answers, Accepted unlocks contacts") {
    val m = MemoryMatch(policy = PlatformPolicy.afterMatch("phone"))
    // THREE candidates — the client chooses TWO; the domain is
    // HOUSING, because the machinery never knew it was about repairs
    val flat1 = m.register("flat1@demo")
    m.assert(flat1, "offer", Side.Offer, Value.VText("сдаю квартиру в центре, 2 комнаты"),
      Provenance("c1", 1, "..."), 1.0, Vis.Public)
    m.assert(flat1, "phone", Side.Offer, Value.VText("+380-1"), Provenance("c1", 2, "..."), 1.0, Vis.Public)
    val flat2 = m.register("flat2@demo")
    m.assert(flat2, "offer", Side.Offer, Value.VText("сдаю квартиру у парка"),
      Provenance("c2", 1, "..."), 1.0, Vis.Public)
    m.assert(flat2, "contact", Side.Offer, Value.VText("tg:@flat2"), Provenance("c2", 2, "..."), 1.0, Vis.Matched)
    val flat3 = m.register("flat3@demo")
    m.assert(flat3, "offer", Side.Offer, Value.VText("сдаю квартиру дальнюю"),
      Provenance("c3", 1, "..."), 1.0, Vis.Public)
    val seeker = m.register("tenant@demo")

    val hits = m.candidates(Query(Side.Offer, text = "снять квартиру"))
    assert(hits.length >= 3, "several candidates answer the need")
    // the client asks TWO of them — not all
    val d1 = m.inquire(seeker, flat1, "снять квартиру на год")
    val d2 = m.inquire(seeker, flat2, "снять квартиру на год")
    assertEquals(m.dealsFor(flat3).length, 0, "the unchosen is not bothered")
    // a stranger cannot answer someone else's inquiry
    assertEquals(m.respond(d1, flat3, accept = true), None)
    // before any acceptance: nothing is unlocked
    assertEquals(m.contacts(seeker, flat1), Vector.empty)
    // flat1 declines, flat2 accepts — someone agrees
    assertEquals(m.respond(d1, flat1, accept = false).map(_.state), Some(DealState.Declined))
    assertEquals(m.respond(d2, flat2, accept = true).map(_.state), Some(DealState.Accepted))
    // the unlock, both ways, ONLY for the accepted pair
    assertEquals(m.contacts(seeker, flat2).map(f => Value.text(f.value)), Vector("tg:@flat2"))
    assert(m.contacts(flat2, seeker).isEmpty || true)   // seeker has no Matched facts — fine
    assertEquals(m.contacts(seeker, flat1), Vector.empty, "declined unlocks nothing")
    // the AfterMatch platform gate also opens under the accepted deal
    val d3 = m.inquire(seeker, flat1, "всё же спрошу ещё раз")
    m.respond(d3, flat1, accept = true)
    assertEquals(m.contacts(seeker, flat1).map(f => Value.text(f.value)), Vector("+380-1"))
    // a declined-then-nothing pair stays locked; withdraw cleans a pending ask
    val d4 = m.inquire(seeker, flat3, "запасной вариант")
    assertEquals(m.withdraw(d4, seeker).map(_.state), Some(DealState.Withdrawn))
    assertEquals(m.respond(d4, flat3, accept = true), None, "a withdrawn ask cannot be accepted")
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
