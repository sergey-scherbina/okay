package okay.cluster.foreign

import okay.codec.Schema
import okay.py.{PyCodec, PyFrame, PyModule, PyValue}

/**
 * The reduce in Python (specs/foreign-map-reduce.md, stage 2), on the
 * same pool of interpreters as `PyStage` for the same module and python.
 *
 *   - `step(frame, acc)`: the chunk as the frame, `acc` a dict of the
 *     fields or `None`; answers ONE ROW AS COLUMNS (`{"n": [n]}`),
 *     because a frame function answers a frame;
 *   - `merge(a, b)`: two dicts of fields, answers one — a call answers a
 *     value.
 */
final class PyReducer[A, Acc](module: PyModule, stepFn: String, mergeFn: String, python: String, workers: Int)
                             (using sa: Schema[A], sacc: Schema[Acc]) extends Reducer[A, Acc]:
  val name = s"py:${module.name}:$stepFn/$mergeFn"
  PyStage.flat[A](name, "A")
  PyStage.flat[Acc](name, "Acc")
  private val pool = PyPool.of(module, python, workers)

  def step(acc: Option[Acc], rows: Vector[A]): Either[Batcher.Failed, Acc] =
    PyFrame.of(rows) match
      case Left(c) => Left(Batcher.Failed(c.kind, c.message))
      case Right(frame) =>
        val arg = acc.fold[PyValue](PyValue.PyNone)(PyCodec.encode(_))
        PyPool.frame(pool, python, s"${module.name}:$stepFn", frame, Vector(arg))
          .flatMap(_.rows[Acc]).left.map(c => Batcher.Failed(c.kind, c.message))
          .flatMap(PyReducer.one(name))

  def merge(a: Acc, b: Acc): Either[Batcher.Failed, Acc] =
    PyPool.call(pool, python, s"${module.name}:$mergeFn", Vector(PyCodec.encode(a), PyCodec.encode(b)))
      .flatMap(PyCodec.decode[Acc]).left.map(c => Batcher.Failed(c.kind, c.message))

object PyReducer:
  def apply[A: Schema, Acc: Schema](module: PyModule, step: String, merge: String,
                                    python: String = "python3", workers: Int = Stage.Workers): PyReducer[A, Acc] =
    new PyReducer[A, Acc](module, step, merge, python, workers)

  /** a partial is exactly one row */
  private[foreign] def one[Acc](name: String)(rows: Vector[Acc]): Either[Batcher.Failed, Acc] =
    rows match
      case Vector(a) => Right(a)
      case other => Left(Batcher.Failed("ReduceShape", s"$name: `step` answered ${other.length} rows, and a partial is one"))
