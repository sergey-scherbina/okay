package okay.matching

import okay.given
import okay.jdbc.JdbcSql
import okay.rag.{Embedding, Vectors}
import java.sql.DriverManager
import java.util.concurrent.atomic.AtomicInteger

/**
 * specs/match.md, match-vec-cache — one test per box.
 *
 * Every test counts CALLS to the encoder, because the point of the
 * cache is not that answers are right (they were right before) but
 * that the model is not asked twice for the same text.
 */
/** every call forwarded, the reads of the vector table counted */
final class CountingSql(inner: okay.sql.Sql) extends okay.sql.Sql:
  var vecReads = 0
  def query(q: String, ps: Vector[okay.sql.SqlValue]) =
    if q.contains("FROM match_vecs") then vecReads += 1
    inner.query(q, ps)
  def update(q: String, ps: Vector[okay.sql.SqlValue]) = inner.update(q, ps)
  def batch(q: String, rs: scala.collection.immutable.ArraySeq[Vector[okay.sql.SqlValue]]) =
    inner.batch(q, rs)
  def begin(i: okay.sql.Isolation) = inner.begin(i)
  def commit() = inner.commit()
  def rollback() = inner.rollback()
  def cancel(): Unit = inner.cancel()
  def describe(q: String) = inner.describe(q)

class TestVecCache extends munit.FunSuite {

  def counting: (AtomicInteger, String => Embedding) =
    val n = AtomicInteger(0)
    val f = Vectors.hashing()
    (n, (t: String) => { n.incrementAndGet(); f(t) })

  def onFile(tag: String = ""): (java.nio.file.Path, (String => Embedding) => SqlMatch) =
    val f = java.nio.file.Files.createTempFile("okay-vec", ".db")
    (f, (e: String => Embedding) =>
      SqlMatch(JdbcSql(DriverManager.getConnection(s"jdbc:sqlite:$f")),
        embed = e, embedTag = tag))

  def prov(n: Long) = Provenance("t", n, "span")

  test("a second search over the same facts embeds only the query") {
    val (calls, embed) = counting
    val (_, open) = onFile()
    val m = open(embed)
    val p = m.register("a@x")
    m.assert(p, "skill", Side.Offer, Value.VText("чинить велосипеды"), prov(1), 1.0, Vis.Public): Unit
    m.candidates(Query(Side.Offer, text = "велосипед")): Unit
    val afterFirst = calls.get()
    m.candidates(Query(Side.Offer, text = "велосипед")): Unit
    assertEquals(calls.get() - afterFirst, 1, "only the query text should be embedded")
  }

  test("a new fact changes the fingerprint and the summary is recomputed") {
    val (calls, embed) = counting
    val (_, open) = onFile()
    val m = open(embed)
    val p = m.register("a@x")
    m.assert(p, "skill", Side.Offer, Value.VText("чинить велосипеды"), prov(1), 1.0, Vis.Public): Unit
    m.candidates(Query(Side.Offer, text = "велосипед")): Unit
    val before = calls.get()
    m.assert(p, "skill", Side.Offer, Value.VText("и самокаты"), prov(2), 1.0, Vis.Public): Unit
    m.candidates(Query(Side.Offer, text = "велосипед")): Unit
    assertEquals(calls.get() - before, 2, "the query and the changed summary")
  }

  test("a superseded fact recomputes too") {
    val (calls, embed) = counting
    val (_, open) = onFile()
    val m = open(embed)
    val p = m.register("a@x")
    val f = m.assert(p, "skill", Side.Offer, Value.VText("чинить велосипеды"), prov(1), 1.0, Vis.Public)
    m.candidates(Query(Side.Offer, text = "велосипед")): Unit
    val before = calls.get()
    m.supersede(f, Value.VText("чинить мотоциклы"), "correction", prov(2)): Unit
    m.candidates(Query(Side.Offer, text = "велосипед")): Unit
    assertEquals(calls.get() - before, 2)
  }

  // the case a cache in memory cannot have, and the reason this one
  // lives in the database
  test("the cache survives a NEW store over the same file") {
    val (_, embed) = counting
    val (_, open) = onFile()
    val first = open(embed)
    val p = first.register("a@x")
    first.assert(p, "skill", Side.Offer, Value.VText("чинить велосипеды"), prov(1), 1.0, Vis.Public): Unit
    first.candidates(Query(Side.Offer, text = "велосипед")): Unit
    val (calls2, embed2) = counting
    val second = open(embed2)
    second.candidates(Query(Side.Offer, text = "велосипед")): Unit
    assertEquals(calls2.get(), 1, "a restart should embed the query and nothing else")
  }

  test("registrySearch embeds each live attribute once, not once per call") {
    val (calls, embed) = counting
    val (_, open) = onFile()
    val m = open(embed)
    m.propose(AttrDraft("skill", Kind.Text, "what someone can do")): Unit
    m.propose(AttrDraft("city", Kind.Text, "where they are")): Unit
    m.registrySearch("умение"): Unit
    val before = calls.get()
    m.registrySearch("умение"): Unit
    assertEquals(calls.get() - before, 1, "only the query text")
  }

  test("a different embedTag recomputes rather than serving the old vector") {
    val (_, embed) = counting
    val f = java.nio.file.Files.createTempFile("okay-vec", ".db")
    def open(tag: String, e: String => Embedding) =
      SqlMatch(JdbcSql(DriverManager.getConnection(s"jdbc:sqlite:$f")),
        embed = e, embedTag = tag)
    val a = open("model-a", embed)
    val p = a.register("a@x")
    a.assert(p, "skill", Side.Offer, Value.VText("чинить велосипеды"), prov(1), 1.0, Vis.Public): Unit
    a.candidates(Query(Side.Offer, text = "велосипед")): Unit
    val (calls2, embed2) = counting
    val b = open("model-b", embed2)
    b.candidates(Query(Side.Offer, text = "велосипед")): Unit
    assertEquals(calls2.get(), 2, "a different encoder must not reuse the vectors")
  }

  test("reset drops the cache with everything else") {
    val (calls, embed) = counting
    val (_, open) = onFile()
    val m = open(embed)
    val p = m.register("a@x")
    m.assert(p, "skill", Side.Offer, Value.VText("чинить велосипеды"), prov(1), 1.0, Vis.Public): Unit
    m.candidates(Query(Side.Offer, text = "велосипед")): Unit
    m.reset()
    val p2 = m.register("a@x")
    m.assert(p2, "skill", Side.Offer, Value.VText("чинить велосипеды"), prov(1), 1.0, Vis.Public): Unit
    val before = calls.get()
    m.candidates(Query(Side.Offer, text = "велосипед")): Unit
    assertEquals(calls.get() - before, 2, "the summary must be embedded again after a reset")
  }

  // the cache is an optimisation and nothing else: it must not move a
  // single score or reorder a single result
  // match-vec-batch: the reads, not just the inferences
  test("a warm candidates() over many profiles issues ONE read, not N") {
    val f = Vectors.hashing()
    val f0 = java.nio.file.Files.createTempFile("okay-vec", ".db")
    val counting = CountingSql(JdbcSql(DriverManager.getConnection(s"jdbc:sqlite:$f0")))
    val m = SqlMatch(counting, embed = f)
    (1 to 20).foreach { i =>
      val p = m.register(s"p$i@x")
      m.assert(p, "skill", Side.Offer, Value.VText(s"чинить велосипеды $i"),
        prov(i.toLong), 1.0, Vis.Public): Unit
    }
    m.candidates(Query(Side.Offer, text = "велосипед", k = 50)): Unit
    counting.vecReads = 0
    m.candidates(Query(Side.Offer, text = "велосипед", k = 50)): Unit
    assertEquals(counting.vecReads, 1,
      s"20 candidates should cost one read, cost ${counting.vecReads}")
  }

  test("a partly warm set embeds only the misses") {
    val (calls, embed) = counting
    val (_, open) = onFile()
    val m = open(embed)
    (1 to 5).foreach { i =>
      val p = m.register(s"p$i@x")
      m.assert(p, "skill", Side.Offer, Value.VText(s"чинить велосипеды $i"),
        prov(i.toLong), 1.0, Vis.Public): Unit
    }
    m.candidates(Query(Side.Offer, text = "велосипед", k = 50)): Unit
    val p6 = m.register("p6@x")
    m.assert(p6, "skill", Side.Offer, Value.VText("новый мастер"), prov(6), 1.0, Vis.Public): Unit
    val before = calls.get()
    m.candidates(Query(Side.Offer, text = "велосипед", k = 50)): Unit
    assertEquals(calls.get() - before, 2, "the query and the one new summary")
  }

  test("ranking is unchanged: same order, same scores, cached or not") {
    val f = Vectors.hashing()
    val (_, open) = onFile()
    val m = open(f)
    val people = Vector("велосипеды" -> "a@x", "мотоциклы" -> "b@x", "сантехника" -> "c@x")
    people.zipWithIndex.foreach { case ((what, mail), i) =>
      val p = m.register(mail)
      m.assert(p, "skill", Side.Offer, Value.VText(what), prov(i.toLong), 1.0, Vis.Public): Unit
    }
    val cold = m.candidates(Query(Side.Offer, text = "велосипед", k = 5))
    val warm = m.candidates(Query(Side.Offer, text = "велосипед", k = 5))
    assertEquals(warm.map(_.profile), cold.map(_.profile))
    assertEquals(warm.map(_.score), cold.map(_.score))
    assert(cold.nonEmpty)
  }
}
