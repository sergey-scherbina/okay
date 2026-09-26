package okay.cluster.foreign

import okay.codec.Schema
import okay.py.{ForeignWorker, PyWorkers}
import okay.r.{REval, RFrame, RModule, RSubprocess, RValue}

/**
 * A chunk through an R function as one frame (specs/foreign-map-reduce.md):
 * `fn` in `module` is called with a data.frame and answers one, whose
 * columns are `B`'s fields. The pool of R sessions is this JVM's, shared
 * by every stage naming the same module and Rscript.
 */
final class RStage[A, B](module: RModule, fn: String, rscript: String, workers: Int)
                        (using sa: Schema[A], sb: Schema[B]) extends Batcher[A, B]:
  val name = s"r:${module.name}:$fn"
  private val address = s"${module.name}::$fn"

  RStage.flat[A](name, "A")
  RStage.flat[B](name, "B")

  private val pool = RPool.of(module, rscript, workers)

  def apply(rows: Vector[A]): Either[Batcher.Failed, Vector[B]] =
    RFrame.of(rows) match
      case Left(c) => Left(Batcher.Failed(c.kind, c.message))
      case Right(frame) =>
        RPool.frame(pool, rscript, address, frame, Vector.empty)
          .flatMap(_.rows[B]).left.map(c => Batcher.Failed(c.kind, c.message))

object RStage:
  def apply[A: Schema, B: Schema](module: RModule, fn: String, rscript: String = "Rscript",
                                  workers: Int = Stage.Workers): RStage[A, B] =
    new RStage[A, B](module, fn, rscript, workers)

  private[foreign] def flat[X](stage: String, what: String)(using s: Schema[X]): Unit =
    RFrame.of(Vector.empty[X]) match
      case Left(c) => throw IllegalArgumentException(s"$stage: the row type $what must be a flat case class — ${c.message}")
      case Right(_) => ()

/** the R sessions of this JVM, one pool per (Rscript, module): the map
 * stage and the reduce of one module share them. An `RSubprocess` has no
 * `alive`: a death throws (DEAD in its message), and `use` reports it so
 * the pool replaces the session */
object RPool:
  /** the R workers of `module` on this JVM (`Workers`): R's are
   * `ForeignWorker`s speaking R (foreign-one-value), pooled as any other */
  def of(module: RModule, rscript: String, workers: Int): PyWorkers =
    Workers.of(s"r|$rscript|${module.name}|${module.source.hashCode}", s"r:${module.name}", workers,
      () => RSubprocess.worker(rscript, Seq(module)))

  private[foreign] def dead(e: Throwable): Boolean = Workers.dead(e)

  private[foreign] def use[X](pool: PyWorkers, rscript: String)
                             (f: ForeignWorker => Either[okay.r.Condition, X]): Either[okay.r.Condition, X] =
    Workers.use(pool, s"'$rscript'")(f)

  def frame(pool: PyWorkers, rscript: String, address: String, in: RFrame, args: Vector[RValue])
  : Either[okay.r.Condition, RFrame] =
    use(pool, rscript)(_.handler.handle(REval.Frame(address, in, args)))

  def call(pool: PyWorkers, rscript: String, address: String, args: Vector[RValue])
  : Either[okay.r.Condition, RValue] =
    use(pool, rscript)(_.handler.handle(REval.Call(address, args)))
