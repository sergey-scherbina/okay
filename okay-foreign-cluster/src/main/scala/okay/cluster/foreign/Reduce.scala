package okay.cluster.foreign

import okay.cluster.{Bounds, Wire}
import okay.codec.Schema
import scala.collection.mutable.ArrayBuffer

/**
 * The REDUCE outside the JVM (specs/foreign-map-reduce.md, stage 2): an
 * `Aggregator`'s two functions, a step over a chunk and a merge of two
 * partials, answered by an interpreter. `PyReducer` and `RReducer` are
 * the implementations; a test's fake is a third.
 */
trait Reducer[A, Acc]:
  def name: String
  /** `acc` folded over one chunk; `None` for a partition's first chunk */
  def step(acc: Option[Acc], rows: Vector[A]): Either[Batcher.Failed, Acc]
  /** two partials into one — associative, as any merge is */
  def merge(a: Acc, b: Acc): Either[Batcher.Failed, Acc]

/**
 * A `Wire` whose accumulator lives on the far side between chunks and
 * crosses as a value between epochs: a partition buffers `batch` rows,
 * hands them to `step` as ONE frame with the running accumulator, and
 * `finish`/`peek` flush what is left and hand the partial over —
 * emptied, so handing over twice cannot double-count. The coordinator's
 * `absorb` folds partials in partition order through `merge`. A run
 * that saw no rows answers `None`.
 */
final class ForeignWire[A, Acc](reducer: Reducer[A, Acc], batch: Int, attempts: Int)
                               (using acc: Schema[Acc]) extends Wire[A, Option[Acc]]:
  require(batch > 0, "a batch holds at least one row")
  require(attempts > 0, "a chunk is attempted at least once")

  final class Part:
    val buf = ArrayBuffer.empty[A]
    var acc: Option[Acc] = None

  type P = Part
  type W = Option[Acc]
  type S = Option[Acc]

  private val optional: Schema[Option[Acc]] = Schema.SOption(() => acc)
  def wire: Schema[W] = optional
  def state: Schema[S] = optional

  def times: Vector[A => Long] = Vector.empty
  def slack: Long = 0L
  def start(bounds: Vector[Bounds]): P = new Part

  def step(p: P, a: A): Unit =
    p.buf += a
    if p.buf.length >= batch then flush(p)

  private def flush(p: P): Unit =
    if p.buf.nonEmpty then
      val rows = p.buf.toVector
      p.buf.clear()
      p.acc = Some(Attempts.run(reducer.name, attempts)(reducer.step(p.acc, rows)))

  def finish(p: P): W =
    flush(p)
    val w = p.acc
    p.acc = None
    w

  /** the same as `finish`: what is folded so far leaves, and the next
   * epoch starts from nothing — the merge is associative */
  def peek(p: P): W = finish(p)

  def empty: S = None
  def absorb(s: S, ws: Vector[W], watermark: Long): S =
    ws.foldLeft(s) {
      case (Some(a), Some(b)) => Some(Attempts.run(reducer.name, attempts)(reducer.merge(a, b)))
      case (None, b) => b
      case (a, None) => a
    }
  def emit(s: S): Option[Acc] = s
  def drops(ws: Vector[W]): Long = 0L
  def merged(ws: Vector[W]): Long = ws.count(_.isDefined).toLong

object Reduce:
  def through[A, Acc: Schema](reducer: Reducer[A, Acc], batch: Int = Stage.Batch, attempts: Int = 3): Wire[A, Option[Acc]] =
    ForeignWire[A, Acc](reducer, batch, attempts)

  /** the reduce in Python: `step(frame, acc)` answers one row as columns,
   * `merge(a, b)` two dicts of fields into one */
  def py[A: Schema, Acc: Schema](module: okay.py.PyModule, step: String, merge: String, python: String = "python3",
                                 batch: Int = Stage.Batch, workers: Int = Stage.Workers): Wire[A, Option[Acc]] =
    through(PyReducer[A, Acc](module, step, merge, python, workers), batch, 3)

  /** the reduce in R: `step(frame, acc)` answers a one-row data.frame,
   * `merge(a, b)` two named lists into one */
  def r[A: Schema, Acc: Schema](module: okay.r.RModule, step: String, merge: String, rscript: String = "Rscript",
                                batch: Int = Stage.Batch, workers: Int = Stage.Workers): Wire[A, Option[Acc]] =
    through(RReducer[A, Acc](module, step, merge, rscript, workers), batch, 3)
