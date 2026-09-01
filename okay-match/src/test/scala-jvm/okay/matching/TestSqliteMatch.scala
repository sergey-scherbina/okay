package okay.matching

import okay.*
import okay.given
import okay.jdbc.JdbcSql
import java.sql.DriverManager

/**
 * The open-backend principle, exercised: the SAME store guarantees
 * over sqlite — a different engine is a different connection string
 * (demo-chat-match runs on exactly this). Dynamic typing is the
 * dialect trap: sqlite answers booleans as integers, and this suite
 * is what caught it.
 */
class TestSqliteMatch extends munit.FunSuite {

  def fresh(): (SqlMatch, java.nio.file.Path) =
    val f = java.nio.file.Files.createTempFile("okay-match", ".db")
    (SqlMatch(JdbcSql(DriverManager.getConnection(s"jdbc:sqlite:$f"))), f)

  def prov(chat: String, off: Long) = Provenance(chat, off, "...")

  test("the guarantees hold on sqlite; the store survives a restart") {
    val (m, f) = fresh()
    m.propose(AttrDraft("phone", Kind.Text, "contact phone", identifying = true))
    m.propose(AttrDraft("availability", Kind.Time, "when free", volatile = true))
    val p = m.register("master@example.com")
    val f1 = m.assert(p, "skill", Side.Offer, Value.VText("tiling"), prov("c", 1), 1.0, Vis.Public)
    assertEquals(m.assert(p, "skill", Side.Offer, Value.VText("tiling"), prov("c", 1), 1.0, Vis.Public), f1)
    m.assert(p, "phone", Side.Offer, Value.VText("+380"), prov("c", 2), 1.0, Vis.Matched)
    // identifying survived the integer round-trip (the dialect trap)
    val other = m.register("other@example.com")
    m.assert(other, "phone", Side.Offer, Value.VText("+380"), prov("d", 1), 1.0, Vis.Matched)
    assertEquals(m.linkCandidates(other).map(_.attr), Vector("phone"))
    // search + restart over the same file
    val m2 = SqlMatch(JdbcSql(DriverManager.getConnection(s"jdbc:sqlite:$f")))
    assertEquals(m2.candidates(Query(Side.Offer, text = "tiling")).length, 2)
    assertEquals(m2.register("master@example.com"), p)
  
  test("deals hold on sqlite and survive a restart") {
    val (m, f) = fresh()
    val seeker = m.register("tenant@demo")
    val flat = m.register("flat@demo")
    m.assert(flat, "contact", Side.Offer, Value.VText("tg:@flat"),
      prov("c", 1), 1.0, Vis.Matched)
    val d = m.inquire(seeker, flat, "снять квартиру")
    assertEquals(m.respond(d, seeker, accept = true), None)   // the seeker is not the asked one
    assertEquals(m.respond(d, flat, accept = true).map(_.state), Some(DealState.Accepted))
    val m2 = SqlMatch(JdbcSql(DriverManager.getConnection(s"jdbc:sqlite:$f")))
    assertEquals(m2.contacts(seeker, flat).map(x => Value.text(x.value)), Vector("tg:@flat"))
    assertEquals(m2.dealsFor(flat).map(_.state), Vector(DealState.Accepted))
  }
}

  test("deals hold on sqlite and survive a restart") {
    val (m, f) = fresh()
    val seeker = m.register("tenant@demo")
    val flat = m.register("flat@demo")
    m.assert(flat, "contact", Side.Offer, Value.VText("tg:@flat"),
      prov("c", 1), 1.0, Vis.Matched)
    val d = m.inquire(seeker, flat, "снять квартиру")
    assertEquals(m.respond(d, seeker, accept = true), None)   // the seeker is not the asked one
    assertEquals(m.respond(d, flat, accept = true).map(_.state), Some(DealState.Accepted))
    val m2 = SqlMatch(JdbcSql(DriverManager.getConnection(s"jdbc:sqlite:$f")))
    assertEquals(m2.contacts(seeker, flat).map(x => Value.text(x.value)), Vector("tg:@flat"))
    assertEquals(m2.dealsFor(flat).map(_.state), Vector(DealState.Accepted))
  }
}
