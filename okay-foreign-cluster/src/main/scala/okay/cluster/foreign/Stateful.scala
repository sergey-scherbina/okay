package okay.cluster.foreign

import okay.Chunks
import okay.cluster.Flow
import okay.codec.Schema
import okay.py.PyModule
import okay.r.RModule

/**
 * A STATEFUL STAGE, as an extension of the engine typeclass
 * (specs/foreign-map-reduce.md, stage 4) — not `Streams`
 * (specs/foreign-facade.md), which is a flow through a stateless frame
 * function one chunk at a time: a partition's chunks through a
 * far function that keeps STATE between them — a running normalisation,
 * a dedup, a window — `open()` making the state, `step(frame, state)`
 * answering rows and mutating it, `finish(frame, state)` flushing at the
 * partition's end (with an empty frame). The state is a held object in ONE
 * interpreter, kept for the partition's whole life (a `Pool.Lease`); a
 * death loses it, so a streaming stage does not retry a chunk — the
 * partition fails and the cluster recomputes it on a survivor, its own
 * fault model. `workers` should cover the partitions a JVM runs at once:
 * past that a partition WAITS for an interpreter, it does not deadlock.
 */
trait Stateful[-M]:
  def streamer[A: Schema, B: Schema](module: M, open: String, step: String, finish: String, workers: Int): Streamer[A, B]

trait Streamer[A, B]:
  def name: String
  /** the partition's state: on the far side, in one interpreter */
  type S
  def open(): Either[Batcher.Failed, S]
  def step(s: S, rows: Vector[A]): Either[Batcher.Failed, Vector[B]]
  /** the last rows, and the state released with whatever held it */
  def finish(s: S): Either[Batcher.Failed, Vector[B]]
  /** the partition ended WITHOUT its finish (a step failed, or it was
   * refused): the state, and whatever holds it, given back all the same */
  def abandon(s: S): Unit

object Stateful:
  given py: Stateful[PyModule] = py("python3")
  given r: Stateful[RModule] = r("Rscript")
  given jvm: Stateful[JvmModule] = new:
    def streamer[A: Schema, B: Schema](module: JvmModule, open: String, step: String, finish: String, workers: Int): Streamer[A, B] =
      module.streamer[A, B](open, step, finish)

  def py(python: String): Stateful[PyModule] = new:
    def streamer[A: Schema, B: Schema](module: PyModule, open: String, step: String, finish: String, workers: Int): Streamer[A, B] =
      ForeignStreamer[PyModule, A, B](Language.py(python), module, open, step, finish, workers)

  def r(rscript: String): Stateful[RModule] = new:
    def streamer[A: Schema, B: Schema](module: RModule, open: String, step: String, finish: String, workers: Int): Streamer[A, B] =
      ForeignStreamer[RModule, A, B](Language.r(rscript), module, open, step, finish, workers)

  /** the stage: `open` at the first chunk, `step` per chunk of `batch`
   * rows, `finish` when the partition's input ends — as one chunk
   * transformer, the seam the engine already has */
  def through[A, B](in: Flow[A], st: Streamer[A, B], batch: Int): Flow[B] =
    require(batch > 0, "a batch holds at least one row")
    Flow.Local(in, st.name, (c: Chunks[A]) => stateful(Chunks.rechunk(c)(batch), st))

  private[foreign] def stateful[A, B](src: Chunks[A], st: Streamer[A, B]): Chunks[B] =
    Chunks.fromIterator(new Iterator[B]:
      private var rest = src
      private var state: Option[st.S] = None
      private var buf: Iterator[B] = Iterator.empty
      private var done = false
      // the state is given back on EVERY path (foreign-one-pool): by
      // `finish`, which owns it once called, or by `abandon` when a step
      // (or the pull feeding it) fails first — a leased interpreter kept by
      // a failed partition was a worker of the pool gone for good
      private def fill(): Boolean =
        try
          while !buf.hasNext && !done do
            val s = state.getOrElse {
              val opened = Attempts.run(st.name, 1)(st.open())
              state = Some(opened)
              opened
            }
            Chunks.pull(rest) match
              case Some((chunk, r)) =>
                rest = r
                buf = Attempts.run(st.name, 1)(st.step(s, chunk.toVector)).iterator
              case None =>
                done = true
                state = None
                buf = Attempts.run(st.name, 1)(st.finish(s)).iterator
          buf.hasNext
        catch case t: Throwable =>
          state.foreach(st.abandon)
          state = None
          done = true
          throw t
      def hasNext: Boolean = fill()
      def next(): B = { fill(): Unit; buf.next() })
