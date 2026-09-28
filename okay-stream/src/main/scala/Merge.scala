package okay

/**
 * THE MECHANISM `merge` RUNS ON (specs/ready-merge.md, the second
 * stage; specs/own-or-standard.md's shape for a choice between two of
 * our own): the three joins `Source.merge` and `mergeFlushing` make —
 * elements, chunks, flushing chunks — as primitives two roads implement
 * each its own way, under the same `Wait`. `Merge.Ready`, the default:
 * a channel per side, joined by readiness on the consumer's own thread
 * of control (`ReadyMerge`), waiting by the given `Wait` before it
 * registers. `Merge.Shared`: one queue both producers feed
 * (`Channel.merge`, `mergeChunked`, `mergeFlushing`) — one mode in every
 * fork, a consumer that never catches up, and no poll-then-park yet:
 * its consumer blocks through the plain drive, which does not ask an
 * `Await` for its poll (an open door in the spec). A caller chooses
 * with one `given` in scope; every call site compiles unchanged.
 */
trait Merge:
  /** two element sources, each side buffered `capacity` deep */
  def elements[A](l: Source[A], r: Source[A], capacity: Int)
                 (using Scheduler, CanBlock, Timer, Wait, Pause): Source[A]
  /** two sources chunked at `size`, `slots` chunks a side, a partial
   * chunk flushed after `within` milliseconds when given */
  def chunks[A](l: Source[A], r: Source[A], slots: Int, size: Int, within: Option[Long])
               (using Scheduler, CanBlock, Timer, Wait, Pause): Source[Chunk[A]]
  /** the same over sources that mark their own boundaries (`Flush`) */
  def flushing[A](l: Flushing[A], r: Flushing[A], slots: Int, size: Int, within: Option[Long])
                 (using Scheduler, Timer, Wait, Pause): Source[Chunk[A]]

object Merge:
  private type S[W] = Unit ! Writer % W + Async

  /** by READINESS: a fiber per side into a ring of its own, read in
   * batches (`drained`), joined on the consumer's thread of control. A
   * ready side tells up to a batch in a row — measured 2-3% better than
   * one per turn — and `merge` promises no order between its sides,
   * only within each (source-merge-via-ready, Results) */
  object Ready extends Merge:
    def elements[A](l: Source[A], r: Source[A], capacity: Int)
                   (using Scheduler, CanBlock, Timer, Wait, Pause): Source[A] =
      ReadyMerge[A](Seq(
        Channel.buffer[A, S, Async](capacity)(l).drained,
        Channel.buffer[A, S, Async](capacity)(r).drained), quantum = Drain.Batch)
    def chunks[A](l: Source[A], r: Source[A], slots: Int, size: Int, within: Option[Long])
                 (using Scheduler, CanBlock, Timer, Wait, Pause): Source[Chunk[A]] =
      ReadyMerge[Chunk[A]](Seq(
        Channel.chunkedSideOf[A, S, Async](l, slots, size, within).drained,
        Channel.chunkedSideOf[A, S, Async](r, slots, size, within).drained), quantum = Drain.Batch)
    def flushing[A](l: Flushing[A], r: Flushing[A], slots: Int, size: Int, within: Option[Long])
                   (using Scheduler, Timer, Wait, Pause): Source[Chunk[A]] =
      ReadyMerge[Chunk[A]](Seq(
        Channel.chunkedSideFlushing[A](l, slots, size, within).drained,
        Channel.chunkedSideFlushing[A](r, slots, size, within).drained), quantum = Drain.Batch)

  /** ONE QUEUE both producers feed — the road before the ring, kept as
   * a door by choice (operator, 2026-09-28) */
  object Shared extends Merge:
    def elements[A](l: Source[A], r: Source[A], capacity: Int)
                   (using Scheduler, CanBlock, Timer, Wait, Pause): Source[A] =
      Channel.merge[A, S, Async, S, Async](l, r, capacity).drained
    def chunks[A](l: Source[A], r: Source[A], slots: Int, size: Int, within: Option[Long])
                 (using Scheduler, CanBlock, Timer, Wait, Pause): Source[Chunk[A]] =
      Writer.of(Channel.mergeChunked[A, S, Async, S, Async](l, r, slots, size, within))
    def flushing[A](l: Flushing[A], r: Flushing[A], slots: Int, size: Int, within: Option[Long])
                   (using Scheduler, Timer, Wait, Pause): Source[Chunk[A]] =
      Writer.of(Channel.mergeFlushing[A](l, r, slots, size, within))

  given Merge = Ready
