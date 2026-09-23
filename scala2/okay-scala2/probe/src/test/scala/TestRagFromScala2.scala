package scala2probe

import okay.rag.{Ingest, Keyword, Source => Doc}
import okay.scala2._

/** okay-rag from Scala 2.13 (specs/scala2-facade.md, stage 15.5) */
class TestRagFromScala2 extends munit.FunSuite {

  val docs = Seq(
    Doc("Math.scala", "object Math {\n  def multiply(a: Int, b: Int): Int = a * b\n  def add(a: Int, b: Int): Int = a + b\n}\n"),
    Doc("Http.scala", "object Http {\n  def get(url: String): String = fetch(url)\n}\n"),
    Doc("Text.scala", "object Text {\n  def shout(s: String): String = s.toUpperCase\n}\n"))

  test("ingest embeds and stores every segment; a vector search finds the file asked about") {
    val index = Rag.memory(Rag.hashing())
    val progress = index.add(docs)
    assertEquals((progress.sources, progress.embedded, index.size), (3, progress.segments, progress.segments))
    val hits = index.search("multiply numbers", 3)
    assertEquals(hits.head.segment.source, "Math.scala")
  }

  test("splitting and keyword search are plain functions, used directly; hybrid fuses both") {
    val segments = docs.flatMap(d => Ingest.segment(d, 400)(_.length))
    val keywords = Keyword.index(segments)
    assertEquals(Keyword.search(keywords, "toUpperCase", 1).map(_.segment.source), Seq("Text.scala"))

    val index = Rag.memory(Rag.hashing())
    index.add(docs)
    val hits = index.hybrid(keywords, "fetch url", 2)
    assertEquals(hits.head.segment.source, "Http.scala")
    assert(hits.size <= 2)
  }
}
