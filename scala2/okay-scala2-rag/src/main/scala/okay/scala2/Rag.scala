package okay.scala2

import scala.collection.immutable.ArraySeq
import okay.{+, Handler}
import okay.given
import okay.rag.{Embed, Fusion, Ingest, Keyword, MemoryStore, PgVector, Postings, Retrieve, Scored, VectorStore, Vectors}

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
    given Handler[Embed] = body.embedding.handler
    Ingest.run[okay.Pure](body.store, sources, budget, batch)(_.length).runWith
  }

  /** the `k` segments nearest to `query` */
  def search(query: String, k: Int): Seq[Scored] = {
    given Handler[Embed] = body.embedding.handler
    Retrieve.vector[okay.Pure](body.store).retrieve(query, k).runWith
  }

  /** vector and keyword retrieval fused by reciprocal rank: the one to
   * use when queries name exact identifiers as often as ideas */
  def hybrid(keywords: Postings, query: String, k: Int): Seq[Scored] =
    Fusion.rrf(Seq(search(query, k * 2), Keyword.search(keywords, query, k * 2))).take(k)

  /** how many segments are stored */
  def size: Int = okay.!.run(body.store.size)
}

/**
 * A `VectorIndex` whose store is across a wire — okay-rag's `PgVector`,
 * Postgres with the pgvector extension — so every operation is a program.
 * The embedding model is the same plain function.
 */
final class PgIndex private[scala2] (body: PgBody) {

  /** split, embed in batches of `batch`, store (an upsert: re-adding a
   * source replaces its segments) */
  def add(sources: Seq[okay.rag.Source], budget: Int = 400, batch: Int = 32): Eff[Async, Ingest.Progress] =
    Async.delay {
      given Handler[Embed] = body.embedding.handler
      given Handler[Embed + okay.Async] = Handler.union[okay.Async, Embed] // Async is the side tested: Embed has no TypeableK
      Ingest.run[okay.Async](body.store, sources, budget, batch)(_.length).runWith
    }

  /** the `k` segments nearest to `query`, scored on okay-rag's scale */
  def search(query: String, k: Int): Eff[Async, Seq[Scored]] =
    Async.delay {
      given Handler[Embed] = body.embedding.handler
      given Handler[Embed + okay.Async] = Handler.union[okay.Async, Embed]
      Retrieve.vector[okay.Async](body.store).retrieve(query, k).runWith
    }

  /** vector hits fused with keyword hits by reciprocal rank */
  def hybrid(keywords: Postings, query: String, k: Int): Eff[Async, Seq[Scored]] =
    search(query, k * 2).map(v => Fusion.rrf(Seq(v, Keyword.search(keywords, query, k * 2))).take(k))

  /** how many segments are stored */
  def size: Eff[Async, Int] = Async.lift(body.store.size)
}

private[scala2] final class PgBody(val store: PgVector, val embedding: Embedder)

/** the store and the model, kept out of `VectorIndex`'s constructor,
 * whose parameter types the Scala 2 reader reads eagerly */
private[scala2] final class IndexBody(val store: VectorStore[okay.Pure], val embedding: Embedder)

/** the embedding model as the `Embed` handler okay-rag asks for */
private[scala2] final class Embedder(embed: Seq[String] => Seq[Array[Float]]) {
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
    new VectorIndex(new IndexBody(new MemoryStore(), new Embedder(embed)))

  /**
   * an index in Postgres with the pgvector extension, over okay-scala2-sql's
   * `Db` (for example `Postgres.connect(...)` from okay-scala2-services):
   * the extension and `table` are created if absent. `dim` is the
   * embedder's vector length.
   */
  def pgvector(db: Db, table: String, dim: Int, embed: Seq[String] => Seq[Array[Float]],
               metric: PgVector.Metric = PgVector.Metric.Cosine): Eff[Async, PgIndex] = {
    val store = new PgVector(db.underlying, table, dim, metric)
    Async.lift(store.ensure()).map(_ => new PgIndex(new PgBody(store, new Embedder(embed))))
  }

  /** a deterministic stand-in embedder (hashed character trigrams),
   * for tests and offline pipelines. Not semantic. */
  def hashing(dim: Int = 64): Seq[String] => Seq[Array[Float]] = {
    val f = Vectors.hashing(dim)
    texts => texts.map(t => f(t).toArray)
  }
}
