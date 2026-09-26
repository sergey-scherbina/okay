package okay.cluster.foreign

import okay.codec.Schema
import okay.py.{ForeignEval, ForeignWorker, PyFrame, PyModule, PyValue, PyWorkers}

/**
 * A chunk through a Python function as one frame (specs/foreign-map-reduce.md):
 * `fn` in `module` is called with the frame — a dict of lists, or the
 * `pyarrow.Table` itself under `@okay.arrow` — and answers the columns of
 * `B`. The pool of interpreters is this JVM's, shared by every stage
 * naming the same module and python.
 */
final class PyStage[A, B](module: PyModule, fn: String, python: String, workers: Int)
                         (using sa: Schema[A], sb: Schema[B]) extends Batcher[A, B]:
  val name = s"py:${module.name}:$fn"
  private val address = s"${module.name}:$fn"

  // a row type that is not a flat case class is refused HERE, not at the
  // first chunk on a worker
  PyStage.flat[A](name, "A")
  PyStage.flat[B](name, "B")

  private val pool = PyPool.of(module, python, workers)

  def apply(rows: Vector[A]): Either[Batcher.Failed, Vector[B]] =
    PyFrame.of(rows) match
      case Left(c) => Left(Batcher.Failed(c.kind, c.message))
      case Right(frame) =>
        PyPool.frame(pool, python, address, frame, Vector.empty)
          .flatMap(_.rows[B]).left.map(c => Batcher.Failed(c.kind, c.message))

object PyStage:
  def apply[A: Schema, B: Schema](module: PyModule, fn: String, python: String = "python3",
                                  workers: Int = Stage.Workers): PyStage[A, B] =
    new PyStage[A, B](module, fn, python, workers)

  private[foreign] def flat[X](stage: String, what: String)(using s: Schema[X]): Unit =
    PyFrame.of(Vector.empty[X]) match
      case Left(c) => throw IllegalArgumentException(s"$stage: the row type $what must be a flat case class — ${c.message}")
      case Right(_) => ()

/** the python interpreters of this JVM, one pool per (python, module):
 * the map stage and the reduce of one module share them */
object PyPool:
  /** the Python workers of `module` on this JVM (`Workers`) */
  def of(module: PyModule, python: String, workers: Int): PyWorkers =
    Workers.of(s"py|$python|${module.name}|${module.source.hashCode}", s"py:${module.name}", workers,
      () => ForeignWorker.start(python, modules = Seq(module)))

  private[foreign] def dead(e: Throwable): Boolean = Workers.dead(e)

  private[foreign] def use[X](pool: PyWorkers, python: String)
                             (f: ForeignWorker => Either[okay.py.Condition, X]): Either[okay.py.Condition, X] =
    Workers.use(pool, s"the python '$python'")(f)

  def frame(pool: PyWorkers, python: String, address: String, in: PyFrame, args: Vector[PyValue])
  : Either[okay.py.Condition, PyFrame] =
    use(pool, python)(_.handler.handle(ForeignEval.Frame(address, in, args)))

  /** a Table through `address`, as itself where Arrow is spoken (facade-frame-seam) */
  def frameTable(pool: PyWorkers, python: String, address: String, in: okay.arrow.Table, args: Vector[PyValue])
  : Either[okay.py.Condition, okay.arrow.Table] =
    use(pool, python)(_.frameTable(address, in, args))

  def call(pool: PyWorkers, python: String, address: String, args: Vector[PyValue])
  : Either[okay.py.Condition, PyValue] =
    use(pool, python)(_.handler.handle(ForeignEval.Call(address, args)))
