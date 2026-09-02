package okay.langchain4j.embed

import okay.given
import okay.rag.{Embed, Vectors, embed}
import dev.langchain4j.model.embedding.onnx.allminilml6v2.AllMiniLmL6V2EmbeddingModel

/** specs/llm-agentic.md, rag-langchain4j — one test per box. The
 * model is local (ONNX, bundled), so these run with no network. */
class TestLangchain4jEmbed extends munit.FunSuite {

  val model = AllMiniLmL6V2EmbeddingModel()

  test("разработчик and программист collide under the real embedder, not under hashing") {
    val real = Langchain4jEmbed.embed(model)
    val a = real("разработчик")
    val b = real("программист")
    val realScore = Vectors.cosine(a, b)

    val hashing = Vectors.hashing()
    val ha = hashing("разработчик")
    val hb = hashing("программист")
    val hashScore = Vectors.cosine(ha, hb)

    assert(realScore > 0.5f, s"expected a near-synonym collision, got $realScore")
    assert(realScore > hashScore,
      s"the real embedder ($realScore) should beat the lexical stand-in ($hashScore) on zero-shared-substring synonyms")
  }

  test("embed and handler agree: the same model wrapped either way answers the same vector") {
    val direct = Langchain4jEmbed.embed(model)("окей")
    given okay.Handler[Embed] = Langchain4jEmbed.handler(model)
    val viaHandler = embed(Seq("окей")).runWith
    assertEquals(viaHandler.head, direct)
  }
}
