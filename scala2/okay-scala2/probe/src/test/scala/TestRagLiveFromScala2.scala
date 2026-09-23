package scala2probe

import okay.rag.{Ingest, Keyword, Source => Doc}
import okay.scala2._

/** okay-rag's PgVector from Scala 2.13, against Postgres with pgvector: Live,
 * out of the default gate (`sbt integrationTest`); skips when no server is at
 * OKAY_PG_HOST/OKAY_PG_PORT, as okay-rag's own TestPgVector does */
class TestRagLiveFromScala2 extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  val host = sys.env.getOrElse("OKAY_PG_HOST", "127.0.0.1")
  val port = sys.env.get("OKAY_PG_PORT").flatMap(p => scala.util.Try(p.toInt).toOption).getOrElse(5432)

  def reachable: Boolean =
    try { val s = new java.net.Socket(); s.connect(new java.net.InetSocketAddress(host, port), 1000); s.close(); true }
    catch { case _: java.io.IOException => false }

  val docs = Seq(
    Doc("Math.scala", "object Math {\n  def multiply(a: Int, b: Int): Int = a * b\n  def add(a: Int, b: Int): Int = a + b\n}\n"),
    Doc("Http.scala", "object Http {\n  def get(url: String): String = fetch(url)\n}\n"),
    Doc("Text.scala", "object Text {\n  def shout(s: String): String = s.toUpperCase\n}\n"))

  test("a pgvector index: add, search, hybrid, size — the same answers as the memory index") {
    assume(reachable, s"no Postgres at $host:$port — the live suite skips")
    val table = "s2vec_" + System.nanoTime
    val prog = for {
      db <- Postgres.connect(host, port, "okay", "okay", "okay")
      index <- Rag.pgvector(db, table, 64, Rag.hashing())
      progress <- index.add(docs)
      stored <- index.size
      hits <- index.search("multiply numbers", 3)
      fused <- index.hybrid(Keyword.index(docs.flatMap(d => Ingest.segment(d, 400)(_.length))), "fetch url", 2)
      _ <- db.update(s"drop table $table")
    } yield (progress.embedded, stored, hits.head.segment.source, fused.head.segment.source)
    val (embedded, stored, nearest, fusedFirst) = Eff.runAsync(prog)
    assertEquals(stored, embedded)
    assertEquals((nearest, fusedFirst), ("Math.scala", "Http.scala"))
    val memory = Rag.memory(Rag.hashing())
    memory.add(docs)
    assertEquals(memory.search("multiply numbers", 3).head.segment.source, nearest)
  }
}
