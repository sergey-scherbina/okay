package okay.cluster.foreign

import okay.codec.Schema
import okay.py.{ForeignEval, ForeignWorker, PyFrame, PyModule}

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

  private val pool: Pool[ForeignWorker] =
    Pools.get[ForeignWorker](s"py|$python|${module.name}|${module.source.hashCode}") {
      Pool[ForeignWorker](name, workers,
        () => ForeignWorker.start(python, modules = Seq(module)),
        _.alive, _.close())
    }

  def apply(rows: Vector[A]): Either[Batcher.Failed, Vector[B]] =
    PyFrame.of(rows) match
      case Left(c) => Left(Batcher.Failed(c.kind, c.message))
      case Right(frame) =>
        val got: Either[okay.py.Condition, PyFrame] =
          try
            pool.use { w =>
              val r = w.handler.handle(ForeignEval.Frame(address, frame, Vector.empty))
              (r, !w.alive)
            }
          catch
            case e: IllegalStateException if PyStage.dead(e) =>
              Left(okay.py.Condition("WorkerDied", e.getMessage))
            case e: Exception =>
              Left(okay.py.Condition("WorkerUnavailable", s"the python '$python' could not be opened: ${e.getMessage}"))
        got.flatMap(_.rows[B]).left.map(c => Batcher.Failed(c.kind, c.message))

object PyStage:
  def apply[A: Schema, B: Schema](module: PyModule, fn: String, python: String = "python3",
                                  workers: Int = Stage.Workers): PyStage[A, B] =
    new PyStage[A, B](module, fn, python, workers)

  private def dead(e: Throwable): Boolean =
    e.isInstanceOf[ForeignWorker.TimedOut] || Option(e.getMessage).exists(_.contains("DEAD"))

  private def flat[X](stage: String, what: String)(using s: Schema[X]): Unit =
    PyFrame.of(Vector.empty[X]) match
      case Left(c) => throw IllegalArgumentException(s"$stage: the row type $what must be a flat case class — ${c.message}")
      case Right(_) => ()
