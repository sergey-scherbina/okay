package okay.rag

import okay.{!, +, Aggregator, pure}
import okay.parse.Parse

/**
 * Ingestion, and the part that matters more (specs/rag.md, P10d):
 * keeping the index fresh at the price of the EDIT rather than the
 * document.
 *
 * The straight path is a stream job — sources in, segments out,
 * embedded in batches, upserted — and every resilience property comes
 * from machinery that already exists: `Chunks` for batching,
 * `retryChunks` for per-chunk recompute, `parMap` for a fiber per
 * batch on the JVM, `Cluster.distribute` across machines. Progress is
 * an Aggregator, like every other statistic here.
 */
object Ingest {

  /** what an ingestion run reports — a fold, so it merges */
  final case class Progress(sources: Int, segments: Int, embedded: Int, reused: Int):
    def +(that: Progress): Progress =
      Progress(sources + that.sources, segments + that.segments,
        embedded + that.embedded, reused + that.reused)

  val progress: Aggregator[Progress, Progress, Progress] =
    Aggregator[Progress, Progress, Progress](Progress(0, 0, 0, 0))(_ + _)(_ + _)(identity)

  /** split one source with the dialect that fits it */
  def segment(src: Source, budget: Int)(size: String => Int): Seq[Segment] =
    val tree = Code.source(src).tree
    Split.structural(src, tree, budget)(size)

  /**
   * Ingest a corpus: split, embed in batches, upsert. Batching is the
   * caller's lever (one embedding operation per batch), and the
   * whole thing is a program, so wrapping it in retry or running it
   * per-chunk on fibers needs nothing from this function.
   */
  def run[F[+_]](store: VectorStore[F], sources: Seq[Source],
                 budget: Int = 400, batch: Int = 32)
                (size: String => Int = _.length): Progress ! (Embed + F) =
    val segs = sources.flatMap(s => segment(s, budget)(size))
    segs.grouped(batch).foldLeft(
      pure[Embed + F, Progress](Progress(sources.length, segs.length, 0, 0))) {
      (acc, group) =>
        acc.flatMap { p =>
          okay.!.widen[Seq[Embedding], Embed, F](embed(group.map(_.text))).flatMap { vs =>
            // a row is a union: F + Embed IS Embed + F (an ascription)
            (okay.!.widen[Unit, F, Embed](store.upsert(group.zip(vs))): Unit ! (Embed + F))
              .map(_ => p.copy(embedded = p.embedded + group.length))
          }
        }
    }

  /**
   * Re-index one edited file at the price of the damage. The edit
   * range is exactly what an agent's own edit tool already knows, so
   * this closes the loop: the agent writes, the index follows, and
   * only the definitions whose TEXT actually changed are embedded
   * again — the rest keep their vectors, and the ones whose byte
   * ranges moved are re-keyed by deleting the stale spans first.
   */
  def reindex[F[+_]](store: VectorStore[F],
                     old: Parse.Parsed[Code.K, Code.S, Code.D],
                     oldSrc: Source, newText: String,
                     editStart: Int, editEndOld: Int, editEndNew: Int,
                     budget: Int = 400)
                    (size: String => Int = _.length)
  : (Parse.Parsed[Code.K, Code.S, Code.D], Progress) ! (Embed + F) =
    val fresh = Code.reparse(old, oldSrc.text, newText,
      editStart, editEndOld, editEndNew)
    val newSrc = Source(oldSrc.id, newText)

    val before = Split.structural(oldSrc, old.tree, budget)(size)
    val after = Split.structural(newSrc, fresh.tree, budget)(size)

    // a segment whose TEXT is unchanged keeps its embedding; only
    // its key may have moved, which the upsert of the survivors and
    // the delete of the stale spans settles
    val unchangedText = before.map(_.text).toSet
    val (reused, changed) = after.partition(s => unchangedText.contains(s.text))
    val staleSpans = before.filterNot(s => after.exists(_.span == s.span)).map(_.span)

    val p = Progress(1, after.length, changed.length, reused.length)
    (okay.!.widen[Unit, F, Embed](store.delete(oldSrc.id, staleSpans)): Unit ! (Embed + F))
      .flatMap { _ =>
        if changed.isEmpty then pure[Embed + F, (Parse.Parsed[Code.K, Code.S, Code.D], Progress)]((fresh, p))
        else
          okay.!.widen[Seq[Embedding], Embed, F](embed(changed.map(_.text))).flatMap { vs =>
            (okay.!.widen[Unit, F, Embed](store.upsert(changed.zip(vs))): Unit ! (Embed + F))
              .map(_ => (fresh, p))
          }
      }
}
