package okay.scala2

import scala.collection.immutable.ArraySeq
import okay.Handler
import okay.given
import okay.rag.{Embed, Fusion, Ingest, Keyword, MemoryStore, Postings, Retrieve, Scored, VectorStore, Vectors}

/**
 * okay-rag for Scala 2.13 (specs/scala2-facade.md, stage 15.5).
 *
 * Probed first. Splitting and keyword retrieval are plain functions and
 * a Scala 2 caller uses them directly: `Ingest.segment(source,
 * budget)(size)`, `Keyword.index(segments)`, `Keyword.search(index,
 * query, k)`, `Fusion.rrf(lists)`, `Corpus.of(sources)`. What Scala 2
 * cannot use is the vector side: a `VectorStore[okay.Pure]` cannot even
 * be NAMED (`Pure` is a top-level alias), every store operation answers
 * a program, and the embedding model is an `Embed` handler. A
 * `VectorIndex` holds the store and the model, which is a plain
 * function here: a batch of texts in, one vector per text out.
 */
final class VectorIndex private[scala2] (body: IndexBody) {

  /** split, embed in batches of `batch`, store; what was done */
  def add(sources: Seq[okay.rag.Source], budget: Int = 400, batch: Int = 32): Ingest.Progress = {
    given Handler[Embed] = body.handler
    Ingest.run[okay.Pure](body.store, sources, budget, batch)(_.length).runWith
  }

  /** the `k` segments nearest to `query` */
  def search(query: String, k: Int): Seq[Scored] = {
    given Handler[Embed] = body.handler
    Retrieve.vector[okay.Pure](body.store).retrieve(query, k).runWith
  }

  /** vector and keyword retrieval fused by reciprocal rank: the one to
   * use when queries name exact identifiers as often as ideas */
  def hybrid(keywords: Postings, query: String, k: Int): Seq[Scored] =
    Fusion.rrf(Seq(search(query, k * 2), Keyword.search(keywords, query, k * 2))).take(k)

  /** how many segments are stored */
  def size: Int = okay.!.run(body.store.size)
}

/** the store and the model, kept out of `VectorIndex`'s constructor,
 * whose parameter types the Scala 2 reader reads eagerly */
private[scala2] final class IndexBody(val store: VectorStore[okay.Pure], embed: Seq[String] => Seq[Array[Float]]) {
  val handler: Handler[Embed] = new Handler[Embed] {
    def handle[A](e: Embed[A]): A = e match {
      case Embed.Of(texts) => embed(texts).map(v => ArraySeq.unsafeWrapArray(v))
    }
  }
}

object Rag {

  /** an index in memory (okay-rag's reference store: brute force,
   * fine to ~10^5 segments), embedding with `embed` */
  def memory(embed: Seq[String] => Seq[Array[Float]]): VectorIndex =
    new VectorIndex(new IndexBody(new MemoryStore(), embed))

  /** a deterministic stand-in embedder (hashed character trigrams),
   * for tests and offline pipelines. Not semantic. */
  def hashing(dim: Int = 64): Seq[String] => Seq[Array[Float]] = {
    val f = Vectors.hashing(dim)
    texts => texts.map(t => f(t).toArray)
  }
}
