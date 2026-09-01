package okay.rag

import okay.{!, Async}
import okay.given
import okay.lex.Span
import okay.pg.PgSql

/**
 * The pgvector box (specs/data.md): the okay-rag store contract
 * over the Sql seam against a real Postgres, and — the assertion
 * that matters — SEARCH AGREES WITH THE MEMORY ENGINE on a shared
 * deterministic fixture (the hashing embedder, no model, no
 * network). Live suite: skips where the server is absent.
 */
class TestPgVector extends munit.FunSuite {

  val host = sys.env.getOrElse("OKAY_PG_HOST", "127.0.0.1")
  val port = sys.env.get("OKAY_PG_PORT").flatMap(_.toIntOption).getOrElse(5432)

  lazy val available: Boolean =
    try { okay.!.run(okay.Async.run[PgSql, Nothing](PgSql.connect(host, port, "okay", "okay", "okay"))).close(); true }
    catch { case _: Throwable => false }

  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  val dim = 64
  val embed = Vectors.hashing(dim)

  def seg(source: String, n: Int, text: String): Segment =
    Segment(source, Span(n * 100, n, 0, text.length), text, Vector("mod", s"s$n"))

  val corpus: Vector[(Segment, Embedding)] =
    Vector(
      "the durable log is one primitive",
      "a cache is a materialized view",
      "verify catches drift at startup",
      "the partition is the unit of order",
      "damage is data and never throws",
      "offsets are the resume tokens",
      "a consumer is never invalid only behind",
      "the typed layer binds by column label",
    ).zipWithIndex.map((t, i) => (seg("doc", i, t), embed(t)))

  def withStore[A](f: PgVector => A): A =
    assume(available, s"no Postgres at $host:$port — the live suite skips")
    val db = okay.!.run(okay.Async.run[PgSql, Nothing](PgSql.connect(host, port, "okay", "okay", "okay")))
    try
      val store = PgVector(db, "rag_test", dim)
      run(store.ensure())
      run(store.truncate())
      f(store)
    finally db.close()

  test("the store contract: upsert, size, re-upsert replaces, delete removes") {
    withStore { store =>
      run(store.upsert(corpus))
      assertEquals(run(store.size), corpus.length)
      // re-indexing an edited segment replaces rather than doubles
      val edited = corpus.head match
        case (s, _) => (s.copy(text = "the durable log, edited"), embed("edited"))
      run(store.upsert(Vector(edited)))
      assertEquals(run(store.size), corpus.length)
      // and the edit is what search now sees under that identity
      val hit = run(store.search(embed("edited"), 1)).head
      assertEquals(hit.segment.text, "the durable log, edited")

      run(store.delete("doc", Vector(corpus.head._1.span)))
      assertEquals(run(store.size), corpus.length - 1)
      run(store.delete("doc", Vector.empty))   // a no-op stays a no-op
      assertEquals(run(store.size), corpus.length - 1)
    }
  }

  test("search agrees with the memory engine on the shared fixture — order and scores") {
    withStore { store =>
      run(store.upsert(corpus))
      val memory = MemoryStore(Vectors.cosine)
      !.run(memory.upsert(corpus))

      for query <- Vector("durable log primitive", "cache view", "column label binding") do
        val q = embed(query)
        val overPg = run(store.search(q, 5))
        val overMemory = !.run(memory.search(q, 5))
        assertEquals(overPg.map(_.segment.text), overMemory.map(_.segment.text),
          s"the engines ranked '$query' differently")
        overPg.zip(overMemory).foreach { (p, m) =>
          assert(math.abs(p.score - m.score) < 1e-4f,
            s"scores diverged on '${p.segment.text}': pg=${p.score} memory=${m.score}")
        }
    }
  }

  test("the segment round-trips whole: span, text, path") {
    withStore { store =>
      run(store.upsert(corpus.take(1)))
      val hit = run(store.search(corpus.head._2, 1)).head
      assertEquals(hit.segment, corpus.head._1)
      assert(hit.score > 0.999f, s"self-similarity was ${hit.score}")
    }
  }
}
