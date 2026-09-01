package okay.matching

import okay.*
import okay.given
import okay.jdbc.JdbcSql
import okay.persist.{MemoryStore, Policy}
import java.sql.DriverManager

/**
 * Stage 1 (specs/match.md): the same guarantees as the memory
 * reference, now against a real database through the Sql seam — H2
 * here, sqlite or Postgres by swapping the connection string, which
 * is the whole point of the seam. Plus the two stage-1 stories: the
 * chat log on a persist topic REBUILDS the store, and a registry
 * merge migrates the projection while the log stands.
 */
class TestSqlMatch extends munit.FunSuite {

  def fresh(name: String, allow: String => Boolean = _ => true): SqlMatch =
    SqlMatch(JdbcSql(DriverManager.getConnection(s"jdbc:h2:mem:$name;DB_CLOSE_DELAY=-1")),
      platformAllows = allow)

  def prov(chat: String, off: Long, quote: String) = Provenance(chat, off, quote)

  test("the durable store keeps the reference's guarantees") {
    val m = fresh("guarantees", allow = _ != "phone")
    // search-before-create
    val a1 = m.propose(AttrDraft("schedule", Kind.Time, "when available for work"))
    assertEquals(m.propose(AttrDraft("hours", Kind.Time, "availability",
      synonyms = Vector("schedule"))).id, a1.id)
    // idempotent asserts + supersede history
    val p = m.register("master@example.com")
    val f1 = m.assert(p, "skill", Side.Offer, Value.VText("tiling"),
      prov("c1", 1, "кладу плитку"), 1.0, Vis.Public)
    assertEquals(m.assert(p, "skill", Side.Offer, Value.VText("tiling"),
      prov("c1", 1, "кладу плитку"), 1.0, Vis.Public), f1)
    val f2 = m.supersede(f1, Value.VText("tiling and mosaic"), "upsell confirmed",
      prov("c2", 7, "еще мозаику"))
    val profile = m.profileOf(p).get
    assertEquals(profile.current.map(_.id), Vector(f2))
    assertEquals(profile.history.find(_.id == f1).get.supersededBy, Some(f2))
    // visibility, both gates
    m.assert(p, "phone", Side.Offer, Value.VText("+380501112233"),
      prov("c1", 2, "тел"), 1.0, Vis.Public)
    m.assert(p, "secret", Side.Offer, Value.VText("hidden"), prov("c1", 3, "..."),
      1.0, Vis.Private)
    val hits = m.candidates(Query(Side.Offer, text = "tiling"))
    val attrs = hits.head.disclosed.map(_.attr)
    assert(attrs.contains("skill") && !attrs.contains("phone") && !attrs.contains("secret"))
    // hybrid: geo filter excludes
    val lviv = m.register("lviv@example.com")
    m.assert(lviv, "skill", Side.Offer, Value.VText("tiling"), prov("c3", 1, "..."), 1.0, Vis.Public)
    m.assert(lviv, "location", Side.Offer, Value.VGeo(49.84, 24.03), prov("c3", 2, "..."), 1.0, Vis.Public)
    m.assert(p, "location", Side.Offer, Value.VGeo(50.45, 30.52), prov("c1", 4, "..."), 1.0, Vis.Public)
    assertEquals(m.candidates(Query(Side.Offer,
      filters = Vector("location" -> Pred.Within(50.45, 30.52, 50)))).map(_.profile), Vector(p))
  }

  test("the store survives a restart: a second handler over the same database sees everything") {
    val m1 = fresh("restart")
    val p = m1.register("master@example.com")
    m1.assert(p, "skill", Side.Offer, Value.VText("wallpapering"),
      prov("c1", 1, "клею обои"), 1.0, Vis.Public)
    val m2 = fresh("restart")   // same jdbc url = same database
    assertEquals(m2.register("master@example.com"), p)
    assertEquals(m2.profileOf(p).get.current.map(_.attr), Vector("skill"))
    // and ids keep counting where the first handler stopped
    val f = m2.assert(p, "price", Side.Offer, Value.VNum(400), prov("c1", 2, "400"), 1.0, Vis.Public)
    assert(f.n > 1)
  }

  test("log-first: the chat log on a persist topic rebuilds a fresh store to the same state") {
    val topic = MemoryStore().topic("chats", 4, Policy.default)
    val log = ChatLog(topic)
    val live = fresh("live")
    // the live conversation: turns land in the log, extraction asserts as it goes
    def extractInto(m: SqlMatch)(t: ChatTurn, prov: Provenance): Unit =
      if t.role == "user" then
        val skill = t.text.stripPrefix("умею: ")
        if skill != t.text then
          m.assert(t.profile, "skill", Side.Offer, Value.VText(skill), prov, 1.0, Vis.Public)
          ()
    val master = live.register("master@example.com")
    Vector(
      ChatTurn(master, "user", "умею: tiling bathrooms"),
      ChatTurn(master, "assistant", "записал; что еще?"),
      ChatTurn(master, "user", "умею: mosaic")).foreach { t =>
      val off = log.append(t)
      extractInto(live)(t, Provenance(log.chat, off, t.text))
    }
    assertEquals(live.profileOf(master).get.current.length, 2)
    // the projection dies; the log does not: rebuild a FRESH store
    val rebuilt = fresh("rebuilt")
    assertEquals(rebuilt.register("master@example.com"), rebuilt.register("master@example.com"))
    val master2 = rebuilt.register("master@example.com")
    log.replay((t, prov) => extractInto(rebuilt)(t.copy(profile = master2), prov))
    assertEquals(rebuilt.profileOf(master2).get.current.map(_.value),
      live.profileOf(master).get.current.map(_.value))
    // and replaying over the LIVE store changes nothing (idempotence over the same log)
    log.replay(extractInto(live))
    assertEquals(live.profileOf(master).get.history.length, 2)
  }

  test("registry migration: a synonym merge moves the facts, the winner answers") {
    val m = fresh("merge")
    m.propose(AttrDraft("schedule", Kind.Time, "when available"))
    m.propose(AttrDraft("grafik", Kind.Time, "рабочий график недели"))  // the drift that slipped through
    val p = m.register("master@example.com")
    m.assert(p, "grafik", Side.Offer, Value.VTime("weekdays"), prov("c1", 1, "по будням"), 1.0, Vis.Public)
    m.mergeAttr(loser = "grafik", winner = "schedule")
    assertEquals(m.get("grafik"), None)
    assertEquals(m.profileOf(p).get.current.map(_.attr), Vector("schedule"))
    // and the merged slug proposed again lands on the winner via synonyms? no — by description
    val again = m.propose(AttrDraft("grafik2", Kind.Time, "when available"))
    assertEquals(again.slug, "schedule")
  }
}
