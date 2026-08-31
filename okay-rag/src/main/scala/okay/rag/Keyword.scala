package okay.rag

import okay.{Fold, Monoid}

/**
 * Keyword retrieval (specs/rag.md, P10e): an inverted index is a
 * FOLD and a MONOID, which is the same property the symbol index and
 * the compaction policy have — so building it distributes, partial
 * indexes merge, and updating it incrementally is that same merge
 * with a delete first. BM25 is then a scoring function over the
 * counts, not a subsystem.
 */
final case class Postings(byTerm: Map[String, Vector[(Int, Int)]] = Map.empty,
                          docs: Vector[Segment] = Vector.empty,
                          lengths: Vector[Int] = Vector.empty):
  def total: Int = docs.length
  def avgLength: Double = if lengths.isEmpty then 0.0 else lengths.sum.toDouble / lengths.length

object Postings:
  /** indexes combine: the postings of the right side shift by the
   * left side's document count. In the companion, so the implicit
   * scope of Postings finds it without an import. */
  given Monoid[Postings] with
    def empty: Postings = Postings()
    def combine(a: Postings, b: Postings): Postings =
      val shift = a.docs.length
      val moved = b.byTerm.view.mapValues(_.map((d, n) => (d + shift, n))).toMap
      val joined = moved.foldLeft(a.byTerm)((m, kv) =>
        m.updated(kv._1, m.getOrElse(kv._1, Vector.empty) ++ kv._2))
      Postings(joined, a.docs ++ b.docs, a.lengths ++ b.lengths)

object Keyword {

  /** the tokenizer of the keyword side: letters and digits, folded.
   * Hand-rolled rather than a regex on purpose — `\p{L}` needs
   * ES2018 property escapes that Scala.js cannot compile to, and
   * Character.isLetterOrDigit is both portable and unicode-correct. */
  def terms(text: String): Vector[String] =
    val out = Vector.newBuilder[String]
    val word = new StringBuilder
    def flush(): Unit =
      if word.nonEmpty then
        out += word.toString.toLowerCase
        word.clear()
    text.foreach { c =>
      if c.isLetterOrDigit || c == '_' then word.append(c) else flush()
    }
    flush()
    out.result()

  /**
   * One segment into an index of its own — the unit of the MERGE.
   *
   * Kept because the monoid is the contract (an index of a shard, to
   * be combined with another), but no longer what `fold` goes
   * through: see below.
   */
  def one(seg: Segment): Postings =
    val ts = terms(seg.text)
    val counts = ts.groupBy(identity).view.mapValues(_.length).toMap
    Postings(counts.map((t, n) => (t, Vector((0, n)))), Vector(seg), Vector(ts.length))

  /**
   * The index as a Fold over segments — accumulating DIRECTLY.
   *
   * It used to be `combine(p, one(s))`: a whole one-segment `Postings`
   * built per segment, with a `groupBy` that allocates a `Vector` of
   * duplicate strings per distinct term, a `mapValues.toMap`, a map
   * and two vectors for the singleton index — and then a merge that
   * shifts every one of its document ids and concatenates a vector per
   * term. The whole of that is thrown away one line later.
   *
   * Measured on an 8.5KB file's segments: 157.9us, of which the
   * tokenization it must do is 40.7. Building the accumulator in place
   * — count into a mutable map on one pass, append the postings at the
   * document index we already know — leaves the tokenization and drops
   * the rest.
   *
   * `combine` is untouched and still the monoid: shards merge, and
   * that is when the shift is real work rather than shifting by zero.
   */
  def fold: Fold[Segment, Postings] = new Fold[Segment, Postings]:
    def init: Postings = Postings()

    def add(p: Postings, s: Segment): Postings =
      val ts = terms(s.text)
      val doc = p.docs.length
      val counts = scala.collection.mutable.HashMap.empty[String, Int]
      var i = 0
      while i < ts.length do
        val t = ts(i)
        counts.update(t, counts.getOrElse(t, 0) + 1)
        i += 1
      var byTerm = p.byTerm
      counts.foreach { (t, n) =>
        byTerm = byTerm.updated(t, byTerm.getOrElse(t, Vector.empty) :+ (doc, n))
      }
      Postings(byTerm, p.docs :+ s, p.lengths :+ ts.length)

  def index(segs: Seq[Segment]): Postings = segs.foldLeft(Postings())(fold.add)

  /**
   * BM25 over the postings. Standard parameters; the point is that
   * scoring reads the fold's counts and nothing else, so a keyword
   * retriever needs no infrastructure at all.
   */
  def search(p: Postings, query: String, k: Int,
             k1: Double = 1.2, b: Double = 0.75): Seq[Scored] =
    if p.docs.isEmpty then Seq.empty
    else
      val n = p.total.toDouble
      val avg = math.max(p.avgLength, 1.0)
      val scores = Array.fill(p.docs.length)(0.0)
      for term <- terms(query).distinct do
        p.byTerm.get(term).foreach { posting =>
          val df = posting.length.toDouble
          val idf = math.log(1 + (n - df + 0.5) / (df + 0.5))
          for (doc, tf) <- posting do
            val len = math.max(p.lengths(doc).toDouble, 1.0)
            val num = tf * (k1 + 1)
            val den = tf + k1 * (1 - b + b * len / avg)
            scores(doc) += idf * num / den
        }
      scores.zipWithIndex
        .filter(_._1 > 0)
        .sortBy(-_._1)
        .take(k)
        .map((s, i) => Scored(p.docs(i), s.toFloat))
        .toSeq
}

/**
 * Fusion (P10c): reciprocal-rank fusion is an AGGREGATOR over ranked
 * lists — which means it merges, so fusing results that arrived from
 * different machines is the same operation as fusing two local
 * retrievers. It also needs no scores to be comparable, which is the
 * reason it is the default way to combine a vector list with a BM25
 * list.
 */
object Fusion {

  /** rank-based fusion; kConst dampens the head, 60 is the usual */
  def rrf(lists: Seq[Seq[Scored]], kConst: Double = 60.0): Seq[Scored] =
    val acc = scala.collection.mutable.LinkedHashMap[(String, okay.lex.Span), (Segment, Double)]()
    for list <- lists; (hit, rank) <- list.zipWithIndex do
      val key = (hit.segment.source, hit.segment.span)
      val add = 1.0 / (kConst + rank + 1)
      val (seg, sum) = acc.getOrElse(key, (hit.segment, 0.0))
      acc.update(key, (seg, sum + add))
    acc.values.toSeq.sortBy(-_._2).map((s, v) => Scored(s, v.toFloat))

  /** the same, as the Aggregator the spec promised: ranked lists in,
   * one fused list out, mergeable across machines */
  def aggregator(kConst: Double = 60.0): okay.Aggregator[Seq[Scored], Seq[Seq[Scored]], Seq[Scored]] =
    okay.Aggregator[Seq[Scored], Seq[Seq[Scored]], Seq[Scored]](Seq.empty)(
      (acc, l) => acc :+ l)((a, b) => a ++ b)(rrf(_, kConst))
}
