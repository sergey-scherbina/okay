package okay.rag

import okay.{!, Aggregator, effect}
import okay.lex.Span
import scala.collection.immutable.ArraySeq

/**
 * Embeddings and the store (specs/rag.md, P10b). Two deliberate
 * choices: embedding is an EFFECT, so nothing here pulls a model
 * runtime and a test uses a deterministic handler instead of a
 * network; and the store is an INTERFACE with one reference
 * implementation, because the interesting work in this module is
 * upstream of the store, not inside it.
 */

/**
 * What an embedder answers.
 *
 * `ArraySeq[Float]` and not `Vector[Float]`, which is what this was
 * until it was measured. `Vector` is a generic trie over
 * `Array[AnyRef]`, so every component of a 1536-dimension provider
 * vector is a boxed `java.lang.Float` — four times the memory, and a
 * pointer chase per component in the scoring loop. `ArraySeq` wraps a
 * primitive `Array[Float]` and stays immutable, with structural
 * equality, so nothing above it changes.
 *
 * MEASURED, because this project guessed wrong about boxing once
 * before and wrote the correction into these docs: one cosine at 1536
 * components went 11.70us -> 1.04us, and scoring a 2000-segment
 * corpus 21.5ms -> 2.06ms. That is 11.3x and 10.4x, and the result
 * ties raw `Array[Float]` (1.034us) exactly — there is nothing left
 * on the table. The chunked-lexing case was 8%; this one is not, and
 * the difference is that a scoring loop reads three components per
 * iteration and does nothing else.
 */
type Embedding = ArraySeq[Float]

/** an embedding from its components, without boxing them */
def embedding(xs: Array[Float]): Embedding = ArraySeq.unsafeWrapArray(xs)

/** the embedding effect: a batch in, a batch out */
enum Embed[+A]:
  case Of(texts: Seq[String]) extends Embed[Seq[Embedding]]

/** embed a batch (one operation per batch — batching is the caller's
 * lever, and `Chunks` is how you pull it) */
def embed(texts: Seq[String]): Seq[Embedding] ! Embed = effect(Embed.Of(texts))

/** a hit: the segment and how well it matched */
final case class Scored(segment: Segment, score: Float)

/**
 * The store, parameterised by the row its operations live in: `Pure`
 * for the in-memory reference, `Async` for anything across a wire.
 */
trait VectorStore[F[+_]]:
  def upsert(items: Seq[(Segment, Embedding)]): Unit ! F
  def search(query: Embedding, k: Int): Seq[Scored] ! F
  def delete(source: String, spans: Seq[Span]): Unit ! F
  def size: Int ! F

/**
 * How near two embeddings are. A plain function, not a typeclass, and
 * deliberately: a typeclass asserts CANONICITY — one instance per
 * type — while a program may hold two stores with different metrics
 * (a normalized index scored by dot product beside an unnormalized
 * one scored by cosine), and there is nothing about `Embedding` that
 * picks one. Given resolution would have to be fought; a parameter
 * with a default is simply passed.
 */
type Similarity = (Embedding, Embedding) => Float

object Vectors {

  /** cosine similarity, the usual measure; zero vectors score 0 */
  def cosine(a: Embedding, b: Embedding): Float =
    var dot = 0.0f
    var na = 0.0f
    var nb = 0.0f
    var i = 0
    val n = math.min(a.length, b.length)
    while i < n do
      dot += a(i) * b(i)
      na += a(i) * a(i)
      nb += b(i) * b(i)
      i += 1
    if na == 0f || nb == 0f then 0f else dot / (math.sqrt(na * nb).toFloat)

  /**
   * The plain inner product — what most embedding providers actually
   * recommend, because they return unit vectors already, and then
   * cosine is this with two square roots wasted on 1.0.
   */
  def dot(a: Embedding, b: Embedding): Float =
    var acc = 0.0f
    var i = 0
    val n = math.min(a.length, b.length)
    while i < n do
      acc += a(i) * b(i)
      i += 1
    acc

  /** negated Euclidean distance, so that larger is still better */
  def euclidean(a: Embedding, b: Embedding): Float =
    var acc = 0.0f
    var i = 0
    val n = math.min(a.length, b.length)
    while i < n do
      val d = a(i) - b(i)
      acc += d * d
      i += 1
    -math.sqrt(acc.toDouble).toFloat

  /** unit length, so cosine becomes a dot product */
  def normalize(v: Embedding): Embedding =
    var sq = 0.0f
    var i = 0
    while i < v.length do { sq += v(i) * v(i); i += 1 }
    val n = math.sqrt(sq.toDouble).toFloat
    if n == 0f then v
    else
      val out = new Array[Float](v.length)
      i = 0
      while i < v.length do { out(i) = v(i) / n; i += 1 }
      embedding(out)

  /**
   * A deterministic embedder for tests and for the keyword-only
   * setups: hashed character trigrams into a fixed number of
   * buckets. It is NOT semantic — it is a stand-in that behaves like
   * an embedder (similar strings land near each other) without a
   * model, so the pipeline can be tested end to end offline.
   */
  def hashing(dim: Int = 64): String => Embedding = text =>
    val v = Array.fill(dim)(0.0f)
    val s = s"  ${text.toLowerCase}  "
    for i <- 0 until (s.length - 2) do
      val h = s.substring(i, i + 3).hashCode
      v(math.floorMod(h, dim)) += 1.0f
    normalize(embedding(v))

  /** the effect handler for it: no network, no model, reproducible */
  def hashingHandler(dim: Int = 64): okay.Handler[Embed] = new:
    private val f = hashing(dim)
    def handle[A](e: Embed[A]): A = e match
      case Embed.Of(texts) => texts.map(f)
}

/**
 * The reference store: brute force over a vector of records, with
 * the top-k selection done by the aggregator we already have. Honest
 * scope — linear in the corpus, which is fine to ~10^5 segments;
 * ANN indexes and real databases are adapters behind the same
 * interface.
 */
final class MemoryStore(similarity: Similarity = Vectors.cosine)
  extends VectorStore[okay.Pure] {

  private var items: Vector[(Segment, Embedding)] = Vector.empty

  def upsert(xs: Seq[(Segment, Embedding)]): Unit ! okay.Pure =
    // a segment is identified by its source and byte range, so
    // re-indexing an edited definition replaces rather than doubles
    val keys = xs.map((s, _) => (s.source, s.span)).toSet
    items = items.filterNot((s, _) => keys.contains((s.source, s.span))) ++ xs
    okay.pure(())

  def search(query: Embedding, k: Int): Seq[Scored] ! okay.Pure =
    given Ordering[Scored] = Ordering.by(_.score)
    val top = Aggregator.topK[Scored](k)
    okay.pure(
      items.foldLeft(top.init)((acc, it) =>
        top.add(acc, Scored(it._1, similarity(query, it._2)))) |> top.present)

  def delete(source: String, spans: Seq[Span]): Unit ! okay.Pure =
    val gone = spans.toSet
    items = items.filterNot((s, _) => s.source == source && gone.contains(s.span))
    okay.pure(())

  def size: Int ! okay.Pure = okay.pure(items.length)

  /** everything held, for persistence and inspection */
  def snapshot: Vector[(Segment, Embedding)] = items

  /** restore a snapshot (the codec round-trip lives in Persist) */
  def restore(xs: Vector[(Segment, Embedding)]): Unit = items = xs

  extension [A](a: A) private def |>[B](f: A => B): B = f(a)
}
