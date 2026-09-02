package okay.demo.embed

import okay.demo.ChatDemo
import okay.langchain4j.embed.Langchain4jEmbed
import okay.matching.{AttrDraft, Kind}
import okay.rag.Vectors
import dev.langchain4j.model.embedding.onnx.allminilml6v2.AllMiniLmL6V2EmbeddingModel

/**
 * The demo's registry, wired to a REAL embedder (specs/demo-chat.md,
 * demo-embeddings-attr): proposing "разработчик" then "программист"
 * — no shared substring, so `Vectors.hashing()` (the demo's default)
 * never collides them, and the registry drifts into two attributes
 * naming the same thing. The real model catches it, through the
 * SAME `ChatDemo.marketOf` constructor point the ambient `market`
 * uses — this is the wiring the box asks for, not a re-test of
 * okay-langchain4j-embed's own raw-vector claim.
 *
 * The threshold is real, not invented: this ONNX MiniLM model scores
 * "разработчик"/"программист" at ~0.52 cosine (measured; matches
 * TestLangchain4jEmbed's own `> 0.5f` bound) — well under
 * `MemoryMatch`'s conservative 0.85 default, which exists to avoid
 * false merges on the coarse `Vectors.hashing()` fallback. Wiring a
 * real embedder is a package deal with recalibrating the threshold
 * to ITS distribution, exactly like choosing the embedder is — both
 * are `marketOf` parameters now.
 */
class TestDemoEmbed extends munit.FunSuite {

  val model = AllMiniLmL6V2EmbeddingModel()

  def draft(slug: String, word: String) =
    AttrDraft(slug, Kind.Text, word)

  test("the real embedder collides разработчик/программист in propose at ITS own threshold — hashing never does at the demo's default") {
    val hashed = ChatDemo.marketOf(":memory:")   // the demo's own defaults: hashing, threshold 0.85
    val a1 = hashed.propose(draft("developer", "разработчик"))
    val a2 = hashed.propose(draft("programmer", "программист"))
    assertNotEquals(a1.id, a2.id,
      "hashing collided on zero-shared-substring synonyms — the contrast this test needs is gone")

    val embedded = ChatDemo.marketOf(":memory:", embed = Langchain4jEmbed.embed(model), proposeThreshold = 0.5f)
    val b1 = embedded.propose(draft("developer", "разработчик"))
    val b2 = embedded.propose(draft("programmer", "программист"))
    assertEquals(b2.id, b1.id,
      "the real embedder, calibrated to its own similarity distribution, should have caught the near-duplicate")
  }

  test("propose still works normally through the wired store: an exact slug hit dedupes with either embedder") {
    val store = ChatDemo.marketOf(":memory:", embed = Langchain4jEmbed.embed(model))
    val a = store.propose(draft("skill", "какой-то навык"))
    val b = store.propose(draft("skill", "другое описание"))
    assertEquals(a.id, b.id, "the same slug must dedupe regardless of the embedder")
  }
}
