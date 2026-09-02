package okay.rag

import okay.{!, +, Choose, Handler, Logic, TypeableK, effect, pure, runChoice}
import okay.given

/**
 * Retrieval pipelines (specs/rag.md, P10c). Every stage that other
 * frameworks ship as a class is a combinator here, because the
 * pieces already existed: multi-query is `Choose`, combining
 * retrievers fairly is `Logic.interleave`, fusion is an `Aggregator`,
 * and a retriever is a function.
 */

/** a retriever is a function from a query to ranked hits, in a row */
trait Retriever[F[+_]]:
  def retrieve(query: String, k: Int): Seq[Scored] ! F

object Retrieve {

  /** the vector side: embed the query, search the store */
  def vector[F[+_]](store: VectorStore[F]): Retriever[Embed + F] =
    new Retriever[Embed + F]:
      def retrieve(query: String, k: Int): Seq[Scored] ! (Embed + F) =
        okay.!.widen[Seq[Embedding], Embed, F](embed(Seq(query))).flatMap { vs =>
          okay.!.widen[Seq[Scored], F, Embed](store.search(vs.head, k)): Seq[Scored] ! (Embed + F)
        }

  /**
   * Discharge a retriever's own effects, giving one that runs in no
   * row at all. That is not a convenience: `Grounded.context` builds
   * a COMONADIC `Handler[Context]`, which cannot suspend, so a
   * retriever used for grounded recall must already be pure. With a
   * pure embedder (`Vectors.hashingHandler`) or an in-process store
   * this makes the vector side usable there beside symbols and BM25;
   * with a handler that must do I/O it does not, and should not —
   * that retrieval belongs in the agent's row, through the search
   * tool, where it can park.
   */
  def handled[F[+_]](r: Retriever[F])(using Handler[F]): Retriever[okay.Pure] =
    new Retriever[okay.Pure]:
      def retrieve(query: String, k: Int): Seq[Scored] ! okay.Pure =
        pure(r.retrieve(query, k).runWith)

  /** the keyword side: no embedding, no store, just the fold */
  def keyword(index: Postings): Retriever[okay.Pure] = new:
    def retrieve(query: String, k: Int): Seq[Scored] ! okay.Pure =
      pure(Keyword.search(index, query, k))

  /** the symbol side: exact structural retrieval, no vectors at all */
  def symbols(idx: Index, sources: Map[String, Source]): Retriever[okay.Pure] = new:
    def retrieve(query: String, k: Int): Seq[Scored] ! okay.Pure =
      // an ITERATOR, so only the k segments actually returned are
      // cut out of their sources: a common name in a large project
      // has hundreds of definitions, and materializing all of them to
      // return eight was most of this retriever's cost
      val hits = Keyword.terms(query).distinct.iterator
        .flatMap(idx.definition)
        .flatMap(sym => sources.get(sym.source).map(Symbols.segment(sym, _)))
        // an exact definition is worth more than any similarity
        .map(Scored(_, 1.0f))
        .take(k).toSeq
      pure(hits)

  /**
   * Hybrid: run several retrievers and fuse by reciprocal rank —
   * the scores need not be comparable, which is exactly why RRF is
   * the default way to put a BM25 list beside a vector list.
   *
   * `fanOut` is how many candidates each retriever is asked for,
   * before fusion picks the k that are returned. Over-fetching is the
   * whole point of fusing: a document ranked fourth by BM25 and
   * fourth by vectors should beat one ranked first by only one of
   * them, and it cannot be seen at all if each list was cut to three.
   *
   * It was a dead parameter until the compiler said so — declared,
   * defaulted, documented by its own name, and never read, so a
   * caller asking for a wider fan-out silently got none.
   */
  def hybrid[F[+_]](rs: Seq[Retriever[F]], fanOut: Int = 10): Retriever[F] = new:
    def retrieve(query: String, k: Int): Seq[Scored] ! F =
      val each = math.max(fanOut, k)
      rs.foldLeft(pure[F, Seq[Seq[Scored]]](Seq.empty)) { (acc, r) =>
        acc.flatMap(ls => r.retrieve(query, each).map(ls :+ _))
      }.map(ls => Fusion.rrf(ls).take(k))

  /**
   * Multi-query: rewrites of one question explored as NONDETERMINISM
   * — `Choose` over the rewrites, `runChoice` collecting every
   * branch's hits, then fusion. The rewriter is a plain function
   * here; a model-backed one is the same shape one row up.
   */
  def multiQuery[F[+_]](r: Retriever[F])(rewrites: String => Seq[String])
  : Retriever[F] = new:
    def retrieve(query: String, k: Int): Seq[Scored] ! F =
      val qs = (query +: rewrites(query)).distinct
      val search: Seq[Scored] ! (Choose + F) =
        effect[Choose + F, String](Choose(qs)).flatMap(q =>
          okay.!.widen[Seq[Scored], F, Choose](r.retrieve(q, k)))
      runChoice[Seq[Scored], F](search).map(ls => Fusion.rrf(ls).take(k))

  /**
   * Fair combination: when one retriever is prolific and another is
   * slow but precise, taking turns keeps the precise one from being
   * starved. `Logic.interleave` says exactly that, and no
   * round-robin router expresses it — the alternation is over the
   * SEARCH, not over a list.
   */
  def fair[F[+_]](a: Seq[Scored], b: Seq[Scored], k: Int): Seq[Scored] =
    // the alternatives never perform an F: built in Choose + Pure and
    // observed there, no re-typing needed (F stays the signature's
    // promise to callers)
    def alts(xs: Seq[Scored]): Scored ! (Choose + okay.Pure) = effect(Choose(xs))
    val mixed = Logic.interleave[Scored, okay.Pure](alts(a), alts(b))
    // observe is the lazy take: only k answers are ever computed
    okay.!.run(Logic.observe[Scored, okay.Pure](k)(mixed))

  /** rerank with any scorer — a cross-encoder, a heuristic, a model
   * one row up: the pipeline does not care which */
  def rerank(hits: Seq[Scored])(score: Segment => Float): Seq[Scored] =
    hits.map(h => Scored(h.segment, score(h.segment))).sortBy(-_.score)
}
