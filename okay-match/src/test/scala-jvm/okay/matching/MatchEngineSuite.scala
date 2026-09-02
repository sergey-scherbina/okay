package okay.matching

import okay.*
import okay.given

/**
 * The open-backend principle, exercised: the SAME store guarantees
 * over every SQL engine — a different engine is a different
 * connection (demo-chat-match runs on exactly this). One suite, the
 * engine as a constructor: `fresh()` answers a store and a way to
 * REOPEN the same data, which is what "survives a restart" means.
 * Dynamic typing is the dialect trap: sqlite answers booleans as
 * integers, and this suite is what caught it.
 */
abstract class MatchEngineSuite extends munit.FunSuite {

  /** the engine's name, for the test titles */
  def engine: String

  /** a fresh, empty store and a reopen of the same data */
  def fresh(): (SqlMatch, () => SqlMatch)

  def prov(chat: String, off: Long) = Provenance(chat, off, "...")

  test(s"the guarantees hold on $engine; the store survives a restart") {
    val (m, reopen) = fresh()
    m.propose(AttrDraft("phone", Kind.Text, "contact phone", identifying = true)): Unit
    m.propose(AttrDraft("availability", Kind.Time, "when free", volatile = true)): Unit
    val p = m.register("master@example.com")
    val f1 = m.assert(p, "skill", Side.Offer, Value.VText("tiling"), prov("c", 1), 1.0, Vis.Public)
    assertEquals(m.assert(p, "skill", Side.Offer, Value.VText("tiling"), prov("c", 1), 1.0, Vis.Public), f1)
    m.assert(p, "phone", Side.Offer, Value.VText("+380"), prov("c", 2), 1.0, Vis.Matched): Unit
    // identifying survived the round-trip (the dialect trap)
    val other = m.register("other@example.com")
    m.assert(other, "phone", Side.Offer, Value.VText("+380"), prov("d", 1), 1.0, Vis.Matched): Unit
    assertEquals(m.linkCandidates(other).map(_.attr), Vector("phone"))
    // search + restart over the same data
    val m2 = reopen()
    assertEquals(m2.candidates(Query(Side.Offer, text = "tiling")).length, 2)
    assertEquals(m2.register("master@example.com"), p)
  }

  test(s"deals hold on $engine and survive a restart") {
    val (m, reopen) = fresh()
    val seeker = m.register("tenant@demo")
    val flat = m.register("flat@demo")
    m.assert(flat, "contact", Side.Offer, Value.VText("tg:@flat"),
      prov("c", 1), 1.0, Vis.Matched): Unit
    val d = m.inquire(seeker, flat, "снять квартиру")
    assertEquals(m.respond(d, seeker, accept = true), None)   // the seeker is not the asked one
    assertEquals(m.respond(d, flat, accept = true).map(_.state), Some(DealState.Accepted))
    val m2 = reopen()
    assertEquals(m2.contacts(seeker, flat).map(x => Value.text(x.value)), Vector("tg:@flat"))
    assertEquals(m2.dealsFor(flat).map(_.state), Vector(DealState.Accepted))
  }

  test(s"flows survive a restart on $engine; unlocks persist") {
    val (m, reopen) = fresh()
    val seeker = m.register("s@x"); val provider = m.register("p@x")
    m.assert(provider, "contact", Side.Offer, Value.VText("tg:@p"),
      prov("c", 1), 1.0, Vis.Matched): Unit
    val Right(id) = m.startFlow("deal",
      Map("seeker" -> seeker, "provider" -> provider), "кран"): @unchecked
    assert(m.advanceFlow(id, "accept", provider).isRight)
    val m2 = reopen()
    assertEquals(m2.flow(id).get.state, "accepted")
    assertEquals(m2.flow(id).get.history.map(_._1), Vector("accept"))
    assertEquals(m2.unlockedBy(seeker, provider).map(x => Value.text(x.value)),
      Vector("tg:@p"))
    assert(m2.advanceFlow(id, "decline", provider).isLeft, "closed stays closed")
  }

  test(s"reset drops the projection on $engine: facts, profiles, deals gone; ids restart; the reopened store is empty too") {
    val (m, reopen) = fresh()
    val p = m.register("gone@example.com")
    val f = m.assert(p, "skill", Side.Offer, Value.VText("tiling"), prov("c", 1), 1.0, Vis.Public)
    val q = m.register("other@example.com")
    m.inquire(q, p, "tiles"): Unit
    m.reset()
    assertEquals(m.candidates(Query(Side.Offer, text = "tiling")), Vector.empty)
    assertEquals(m.dealsFor(p), Vector.empty)
    assertEquals(m.profileOf(p), None)
    // ids restart: the first fact after a reset has the first id again
    val p2 = m.register("gone@example.com")
    assertEquals(m.assert(p2, "skill", Side.Offer, Value.VText("mosaic"), prov("c", 2), 1.0, Vis.Public), f)
    assertEquals(reopen().candidates(Query(Side.Offer, text = "mosaic")).length, 1)
  }
}
