package okay.cluster.foreign

import okay.Chunks
import okay.cluster.Flow
import okay.codec.Schema
import okay.py.{ForeignEval, ForeignWorker, PyFrame, PyModule, PyRef, PyValue}
import okay.r.{REval, RFrame, RModule, RSubprocess}

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

object Stateful:
  given py: Stateful[PyModule] = py("python3")
  given r: Stateful[RModule] = r("Rscript")
  given jvm: Stateful[JvmModule] = new:
    def streamer[A: Schema, B: Schema](module: JvmModule, open: String, step: String, finish: String, workers: Int): Streamer[A, B] =
      module.streamer[A, B](open, step, finish)

  def py(python: String): Stateful[PyModule] = new:
    def streamer[A: Schema, B: Schema](module: PyModule, open: String, step: String, finish: String, workers: Int): Streamer[A, B] =
      PyStreamer[A, B](module, open, step, finish, python, workers)

  def r(rscript: String): Stateful[RModule] = new:
    def streamer[A: Schema, B: Schema](module: RModule, open: String, step: String, finish: String, workers: Int): Streamer[A, B] =
      RStreamer[A, B](module, open, step, finish, rscript, workers)

  /** the stage: `open` at the first chunk, `step` per chunk of `batch`
   * rows, `finish` when the partition's input ends — as one chunk
   * transformer, the seam the engine already has */
  def through[A, B](in: Flow[A], st: Streamer[A, B], batch: Int): Flow[B] =
    require(batch > 0, "a batch holds at least one row")
    Flow.Local(in, st.name, (c: Chunks[A]) => stateful(Chunks.rechunk(c)(batch), st))

  private def stateful[A, B](src: Chunks[A], st: Streamer[A, B]): Chunks[B] =
    Chunks.fromIterator(new Iterator[B]:
      private var rest = src
      private var state: Option[st.S] = None
      private var buf: Iterator[B] = Iterator.empty
      private var done = false
      private def fill(): Boolean =
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
              buf = Attempts.run(st.name, 1)(st.finish(s)).iterator
        buf.hasNext
      def hasNext: Boolean = fill()
      def next(): B = { fill(): Unit; buf.next() })

final class PyStreamer[A, B](module: PyModule, openFn: String, stepFn: String, finishFn: String, python: String, workers: Int)
                            (using sa: Schema[A], sb: Schema[B]) extends Streamer[A, B]:
  val name = s"py:${module.name}:$openFn/$stepFn/$finishFn"
  private val pool = PyPool.of(module, python, workers)
  final class S(val lease: Pool[ForeignWorker]#Lease, val ref: PyRef)

  private def failed(c: okay.py.Condition) = Batcher.Failed(c.kind, c.message)

  /** one operation on the leased interpreter; a death releases the lease
   * as dead and is the wire's failure */
  private def on[X](s: S)(f: ForeignWorker => Either[okay.py.Condition, X]): Either[Batcher.Failed, X] =
    try f(s.lease.e).left.map(failed)
    catch case e: IllegalStateException if PyPool.dead(e) =>
      s.lease.release(dead = true)
      Left(Batcher.Failed("WorkerDied", e.getMessage))

  def open(): Either[Batcher.Failed, S] =
    val lease =
      try pool.lease()
      catch case e: Exception => return Left(Batcher.Failed("WorkerUnavailable", s"the python '$python' could not be opened: ${e.getMessage}"))
    val opened =
      try lease.e.handler.handle(ForeignEval.Hold(s"${module.name}:$openFn", Vector.empty)).left.map(failed)
      catch case e: IllegalStateException if PyPool.dead(e) => Left(Batcher.Failed("WorkerDied", e.getMessage))
    opened match
      case Right(ref) => Right(S(lease, ref))
      case Left(f) => lease.release(dead = f.kind == "WorkerDied"); Left(f)

  def step(s: S, rows: Vector[A]): Either[Batcher.Failed, Vector[B]] =
    PyFrame.of(rows).left.map(failed).flatMap(frame =>
      on(s)(_.handler.handle(ForeignEval.Frame(s"${module.name}:$stepFn", frame, Vector(PyValue.Ref(s.ref)))))
        .flatMap(_.rows[B].left.map(failed)))

  def finish(s: S): Either[Batcher.Failed, Vector[B]] =
    val last = on(s)(_.handler.handle(ForeignEval.Frame(s"${module.name}:$finishFn", PyFrame(Vector.empty), Vector(PyValue.Ref(s.ref)))))
      .flatMap(_.rows[B].left.map(failed))
    if last.isRight || last.left.exists(_.kind != "WorkerDied") then
      try s.lease.e.handler.handle(ForeignEval.Release(s.ref)) catch case _: Exception => ()
      s.lease.release(dead = false)
    last

final class RStreamer[A, B](module: RModule, openFn: String, stepFn: String, finishFn: String, rscript: String, workers: Int)
                           (using sa: Schema[A], sb: Schema[B]) extends Streamer[A, B]:
  val name = s"r:${module.name}:$openFn/$stepFn/$finishFn"
  private val pool = RPool.of(module, rscript, workers)
  final class S(val lease: Pool[RSubprocess]#Lease, val ref: PyRef)

  private def failed(c: okay.r.Condition) = Batcher.Failed(c.kind, c.message)

  private def on[X](s: S)(f: RSubprocess => Either[okay.r.Condition, X]): Either[Batcher.Failed, X] =
    try f(s.lease.e).left.map(failed)
    catch case e: IllegalStateException if RPool.dead(e) =>
      s.lease.release(dead = true)
      Left(Batcher.Failed("WorkerDied", e.getMessage))

  def open(): Either[Batcher.Failed, S] =
    val lease =
      try pool.lease()
      catch case e: Exception => return Left(Batcher.Failed("WorkerUnavailable", s"'$rscript' could not be opened: ${e.getMessage}"))
    val opened =
      try lease.e.handler.handle(REval.Hold(s"${module.name}::$openFn", Vector.empty)).left.map(failed)
      catch case e: IllegalStateException if RPool.dead(e) => Left(Batcher.Failed("WorkerDied", e.getMessage))
    opened match
      case Right(ref) => Right(S(lease, ref))
      case Left(f) => lease.release(dead = f.kind == "WorkerDied"); Left(f)

  def step(s: S, rows: Vector[A]): Either[Batcher.Failed, Vector[B]] =
    RFrame.of(rows).left.map(failed).flatMap(frame =>
      on(s)(_.handler.handle(REval.Frame(s"${module.name}::$stepFn", frame, Vector(PyValue.Ref(s.ref)))))
        .flatMap(_.rows[B].left.map(failed)))

  def finish(s: S): Either[Batcher.Failed, Vector[B]] =
    val last = on(s)(_.handler.handle(REval.Frame(s"${module.name}::$finishFn", RFrame(Vector.empty), Vector(PyValue.Ref(s.ref)))))
      .flatMap(_.rows[B].left.map(failed))
    if last.isRight || last.left.exists(_.kind != "WorkerDied") then
      try s.lease.e.handler.handle(REval.Release(s.ref)) catch case _: Exception => ()
      s.lease.release(dead = false)
    last
