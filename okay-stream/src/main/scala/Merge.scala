package okay


import okay.freer.*
import okay.freer.given
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

  /** a merge cancelled or stopped early closes its channels, so every
   * producer parked on a full one wakes and ends
   * (merge-scopes-everywhere); a merge that ran to its end never calls it.
   * `Source.zip` releases its sides through the same door (source-zip) */
  private[okay] def closing(cs: Channel[?]*): () => Unit = () =>
    // only a channel still OPEN is a release: the Shared element join's
    // scope is never exited (below), so the drive releases it at a
    // normal end too, when its channel is closed already
    val open = cs.filterNot(_.isClosed)
    if open.nonEmpty then
      Source.mergeReleases.incrementAndGet(): Unit
      open.foreach(_.close())

  /** by READINESS: a fiber per side into a ring of its own, read in
   * batches (`drained`), joined on the consumer's thread of control. A
   * ready side tells up to a batch in a row — measured 2-3% better than
   * one per turn — and `merge` promises no order between its sides,
   * only within each (source-merge-via-ready, Results) */
  object Ready extends Merge:
    def elements[A](l: Source[A], r: Source[A], capacity: Int)
                   (using Scheduler, CanBlock, Timer, Wait, Pause): Source[A] =
      val cl = Channel.buffer[A, S, Async](capacity)(l)
      val cr = Channel.buffer[A, S, Async](capacity)(r)
      ReadyMerge[A](Seq(cl.drained, cr.drained), quantum = Drain.Batch, release = closing(cl, cr))
    def chunks[A](l: Source[A], r: Source[A], slots: Int, size: Int, within: Option[Long])
                 (using Scheduler, CanBlock, Timer, Wait, Pause): Source[Chunk[A]] =
      val cl = Channel.chunkedSideOf[A, S, Async](l, slots, size, within)
      val cr = Channel.chunkedSideOf[A, S, Async](r, slots, size, within)
      ReadyMerge[Chunk[A]](Seq(cl.drained, cr.drained), quantum = Drain.Batch, release = closing(cl, cr))
    def flushing[A](l: Flushing[A], r: Flushing[A], slots: Int, size: Int, within: Option[Long])
                   (using Scheduler, Timer, Wait, Pause): Source[Chunk[A]] =
      val cl = Channel.chunkedSideFlushing[A](l, slots, size, within)
      val cr = Channel.chunkedSideFlushing[A](r, slots, size, within)
      ReadyMerge[Chunk[A]](Seq(cl.drained, cr.drained), quantum = Drain.Batch, release = closing(cl, cr))

  /** ONE QUEUE both producers feed — the road before the ring, kept as
   * a door by choice (operator, 2026-09-28) */
  object Shared extends Merge:
    // released on an early stop like every other join, but NOT through
    // `Source.releasing`: its trailing `flatMap(_ => Exit)` is a Bind
    // over the whole element program, a rotation per element told. The
    // scope is ENTERED in front and EXITED where the channel ends, by
    // the drain's own last step (`drainedThen`), one operation a run.
    // That exit is also what keeps the scope reachable while the merge
    // runs. Entered and never named again, it was held by nothing on a
    // plain `runWith`, and a collection mid-run released it through its
    // collector door, closing the channel under running producers
    // (merge-shared-scope-gc-release; the zip had the same defect,
    // source-zip-lost-pairs). An early stop never reaches the exit: the
    // drive, a fiber's handler or, abandoned, the collector releases it
    def elements[A](l: Source[A], r: Source[A], capacity: Int)
                   (using Scheduler, CanBlock, Timer, Wait, Pause): Source[A] =
      okay.freer.pure[Writer % A + Async, Unit](()).flatMap: _ =>
        val ch = Channel.merge[A, S, Async, S, Async](l, r, capacity)
        val scope = Async.CancelScope(closing(ch))
        okay.freer.effect[Writer % A + Async, Unit](Async.Run(Async.Enter(scope)))
          .flatMap(_ => ch.drainedThen(okay.freer.effect[Writer % A + Async, Unit](Async.Run(Async.Exit(scope)))))
    def chunks[A](l: Source[A], r: Source[A], slots: Int, size: Int, within: Option[Long])
                 (using Scheduler, CanBlock, Timer, Wait, Pause): Source[Chunk[A]] =
      val ch = Channel.mergeChunked[A, S, Async, S, Async](l, r, slots, size, within)
      Source.releasing(closing(ch))(Writer.of(ch))
    def flushing[A](l: Flushing[A], r: Flushing[A], slots: Int, size: Int, within: Option[Long])
                   (using Scheduler, Timer, Wait, Pause): Source[Chunk[A]] =
      val ch = Channel.mergeFlushing[A](l, r, slots, size, within)
      Source.releasing(closing(ch))(Writer.of(ch))

  given Merge = Ready
