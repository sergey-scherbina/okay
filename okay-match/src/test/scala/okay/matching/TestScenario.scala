package okay.matching

/** specs/match.md, "Scenarios as data" — one test per box */
class TestScenario extends munit.FunSuite {

  def prov(off: Long) = Provenance("c", off, "...")

  test("the built-in deal runs as data: roles enforced, terminals close, history kept") {
    val m = MemoryMatch()
    val seeker = m.register("seeker@x"); val provider = m.register("prov@x")
    val Right(id) = m.startFlow("deal",
      Map("seeker" -> seeker, "provider" -> provider), "починить кран"): @unchecked
    // the seeker cannot fire the provider's transition
    assert(m.advanceFlow(id, "accept", seeker).isLeft)
    val Right((f, t)) = m.advanceFlow(id, "accept", provider): @unchecked
    assertEquals(f.state, "accepted")
    assertEquals(t.notifies.head._1, "seeker")
    assertEquals(f.history.map(_._1), Vector("accept"))
    // terminal: the flow is closed
    assert(m.advanceFlow(id, "decline", provider).isLeft)
  }

  test("validate names each malformation as data") {
    val bad = ScenarioDef("b", Vector("a"), "s0",
      Vector("s0", "s1", "far"), Set("far", "s1"),
      Vector(
        Transition("t1", "s0", "s1", by = "a"),
        Transition("ghost", "nowhere", "s1", by = "a"),
        Transition("who", "s0", "s1", by = "nobody"),
        Transition("exit", "s1", "s0", by = "a")))
    val faults = ScenarioDef.validate(bad).map(_.what)
    assert(faults.exists(_.contains("unknown state 'nowhere'")), faults)
    assert(faults.exists(_.contains("unknown role 'nobody'")), faults)
    assert(faults.exists(_.contains("terminal state with exits")), faults)
    assert(faults.exists(_.contains("unreachable terminal")), faults)
    // and an invalid definition is NOT registered
    val m = MemoryMatch()
    assert(m.defineScenario(bad).nonEmpty)
    assertEquals(m.scenario("b"), None)
  }

  test("unlocks grant visibility; notify templates fill") {
    val m = MemoryMatch()
    val seeker = m.register("seeker@x"); val provider = m.register("prov@x")
    m.assert(provider, "contact", Side.Offer, Value.VText("tg:@prov"),
      prov(1), 1.0, Vis.Matched): Unit
    val Right(id) = m.startFlow("deal",
      Map("seeker" -> seeker, "provider" -> provider), "кран"): @unchecked
    assertEquals(m.unlockedBy(seeker, provider), Vector.empty)
    val Right((f, t)) = m.advanceFlow(id, "accept", provider): @unchecked
    assertEquals(m.unlockedBy(seeker, provider).map(x => Value.text(x.value)),
      Vector("tg:@prov"))
    assertEquals(Flow.fill(t.notifies.head._2, ScenarioDef.deal, f, "provider"),
      "исполнитель согласился: кран")
  }

  test("a SECOND scenario, three roles, runs with zero engine changes") {
    // housing with escrow: buyer / seller / escrow agent
    val sale = ScenarioDef(
      name = "escrow-sale",
      roles = Vector("buyer", "seller", "escrow"),
      initial = "offered",
      states = Vector("offered", "under-contract", "funded", "closed", "fallen-through"),
      terminal = Set("closed", "fallen-through"),
      transitions = Vector(
        Transition("sign", "offered", "under-contract", by = "seller",
          notifies = Vector("buyer" -> "продавец подписал: {what}")),
        Transition("fund", "under-contract", "funded", by = "buyer",
          notifies = Vector("escrow" -> "средства внесены: {what}")),
        Transition("release", "funded", "closed", by = "escrow",
          unlocks = Vector("buyer" -> "address", "seller" -> "contact"),
          notifies = Vector("buyer" -> "сделка закрыта: {what}")),
        Transition("abort", "under-contract", "fallen-through", by = "escrow")))
    val m = MemoryMatch()
    assertEquals(m.defineScenario(sale), Vector.empty)
    val b = m.register("buyer@x"); val sl = m.register("seller@x"); val e = m.register("escrow@x")
    m.assert(sl, "address", Side.Offer, Value.VText("ул. Ясная 5"), prov(1), 1.0, Vis.Matched): Unit
    val Right(id) = m.startFlow("escrow-sale",
      Map("buyer" -> b, "seller" -> sl, "escrow" -> e), "квартира"): @unchecked
    // the walk, each step its role's alone
    assert(m.advanceFlow(id, "sign", b).isLeft)
    assert(m.advanceFlow(id, "sign", sl).isRight)
    assert(m.advanceFlow(id, "fund", b).isRight)
    assertEquals(m.unlockedBy(b, sl), Vector.empty, "not before release")
    assert(m.advanceFlow(id, "release", e).isRight)
    assertEquals(m.unlockedBy(b, sl).map(x => Value.text(x.value)), Vector("ул. Ясная 5"))
    assertEquals(m.flow(id).get.history.map(_._1), Vector("sign", "fund", "release"))
  }

  test("the typed builder: undeclared states do not compile; the built equals the written") {
    import ScenarioBuilder.*
    val built = scenario("deal2")
      .role["seeker"]("seeker").role["provider"]("provider")
      .state["asked"]("asked")
      .terminalState["accepted"]("accepted")
      .terminalState["declined"]("declined")
      .route("accept", "asked", "accepted", "provider")
      .route("decline", "asked", "declined", "provider")
      .initial("asked")
    assertEquals(built.initial, "asked")
    assertEquals(built.transitions.map(_.name), Vector("accept", "decline"))
    assertEquals(ScenarioDef.validate(built), Vector.empty)

    val errors = compileErrors(
      "import okay.matching.ScenarioBuilder.*\n" +
      "scenario(\"x\").role[\"r\"](\"r\").state[\"a\"](\"a\")\n" +
      "  .route(\"t\", \"a\", \"NOWHERE\", \"r\")")
    assert(errors.nonEmpty && errors.contains("In["), errors)
  }
}
