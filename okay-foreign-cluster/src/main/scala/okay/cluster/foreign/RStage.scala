package okay.cluster.foreign

import okay.codec.Schema
import okay.r.{REval, RFrame, RModule, RSubprocess}

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

  // an `RSubprocess` has no `alive`: a death throws (DEAD in its message),
  // and `use` reports it so the pool replaces the session
  private val pool: Pool[RSubprocess] =
    Pools.get[RSubprocess](s"r|$rscript|${module.name}|${module.source.hashCode}") {
      Pool[RSubprocess](name, workers,
        () => RSubprocess.start(rscript, modules = Seq(module)),
        _ => true, _.close())
    }

  def apply(rows: Vector[A]): Either[Batcher.Failed, Vector[B]] =
    RFrame.of(rows) match
      case Left(c) => Left(Batcher.Failed(c.kind, c.message))
      case Right(frame) =>
        val got: Either[okay.r.Condition, RFrame] =
          try
            pool.use { r =>
              val answer =
                try Right(r.handler.handle(REval.Frame(address, frame, Vector.empty)))
                catch case e: IllegalStateException if RStage.dead(e) => Left(e)
              answer match
                case Right(a) => (a, false)
                case Left(e) => (Left(okay.r.Condition("WorkerDied", e.getMessage)), true)
            }
          catch
            case e: Exception =>
              Left(okay.r.Condition("WorkerUnavailable", s"'$rscript' could not be opened: ${e.getMessage}"))
        got.flatMap(_.rows[B]).left.map(c => Batcher.Failed(c.kind, c.message))

object RStage:
  def apply[A: Schema, B: Schema](module: RModule, fn: String, rscript: String = "Rscript",
                                  workers: Int = Stage.Workers): RStage[A, B] =
    new RStage[A, B](module, fn, rscript, workers)

  private def dead(e: Throwable): Boolean = Option(e.getMessage).exists(_.contains("DEAD"))

  private def flat[X](stage: String, what: String)(using s: Schema[X]): Unit =
    RFrame.of(Vector.empty[X]) match
      case Left(c) => throw IllegalArgumentException(s"$stage: the row type $what must be a flat case class — ${c.message}")
      case Right(_) => ()
