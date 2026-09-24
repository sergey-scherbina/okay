package okay2

import scala.collection.immutable.ArraySeq

/**
 * okay2-stream — okay-stream's PURE layer for the Scala 2 core
 * (specs/okay2.md, stage 3): chunked streams, the Take effect and the
 * coroutine pairing of a producer with a consumer, pipeline stages and
 * their composition, the pipeline as a value with its optimizer, line
 * framing, event-time windows. The asynchronous layer of okay-stream
 * (Channel, Source.merge, Fifo, Ring, Buffer, Queues, the adaptive and
 * sentinel channels, ParallelChunks) stands on okay-async and follows
 * okay2-async.
 */
package object stream {

  /** an immutable, array-backed batch: `ArraySeq` has primitive-backed
   * subclasses, so a chunk of Longs is a long[] */
  type Chunk[+A] = ArraySeq[A]

  /** the pure writer stream: a program that tells W and answers nothing */
  type Feed[W] = Unit ! Writer[W]

  /** a chunked stream is an ordinary pure writer stream of whole
   * batches: the tree steps once per CHUNK, an element inside costs an
   * array index */
  type Chunks[A] = Feed[Chunk[A]]

  /** a pipeline stage: a transducer as a program — it awaits I and
   * tells O, state is just its recursion parameters */
  type Stage[I, O, A] = A ! (Take[I] + Writer[O])

  /** an asynchronous SOURCE: a program that tells its elements as it
   * goes, performing Async between them — the shape every streaming
   * seam has, and by Writer's instance an ordinary stream in Async */
  type Source[W] = Unit ! (Writer[W] + okay2.async.Async)

  /** a source that also marks its own chunk boundaries (`Flush.now`). An
   * ordinary `Source` IS one — the row is contravariant, so a program
   * that never flushes needs no rebuilding to be read as this */
  type Flushing[W] = Unit ! (Flush + (Writer[W] + okay2.async.Async))
}
