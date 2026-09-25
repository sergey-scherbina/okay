package okay.cluster.foreign

import okay.codec.Schema
import okay.r.{RCodec, RFrame, RModule, RValue}

/**
 * The reduce in R (specs/foreign-map-reduce.md, stage 2), on the same
 * pool of sessions as `RStage` for the same module and Rscript:
 * `step(frame, acc)` takes a data.frame and a named list (or NULL) and
 * answers a one-row data.frame; `merge(a, b)` takes two named lists and
 * answers one.
 */
final class RReducer[A, Acc](module: RModule, stepFn: String, mergeFn: String, rscript: String, workers: Int)
                            (using sa: Schema[A], sacc: Schema[Acc]) extends Reducer[A, Acc]:
  val name = s"r:${module.name}:$stepFn/$mergeFn"
  RStage.flat[A](name, "A")
  RStage.flat[Acc](name, "Acc")
  private val pool = RPool.of(module, rscript, workers)

  def step(acc: Option[Acc], rows: Vector[A]): Either[Batcher.Failed, Acc] =
    RFrame.of(rows) match
      case Left(c) => Left(Batcher.Failed(c.kind, c.message))
      case Right(frame) =>
        val arg = acc.fold[RValue](RValue.RNull)(RCodec.encode(_))
        RPool.frame(pool, rscript, s"${module.name}::$stepFn", frame, Vector(arg))
          .flatMap(_.rows[Acc]).left.map(c => Batcher.Failed(c.kind, c.message))
          .flatMap(PyReducer.one(name))

  def merge(a: Acc, b: Acc): Either[Batcher.Failed, Acc] =
    RPool.call(pool, rscript, s"${module.name}::$mergeFn", Vector(RCodec.encode(a), RCodec.encode(b)))
      .flatMap(RCodec.decode[Acc]).left.map(c => Batcher.Failed(c.kind, c.message))

object RReducer:
  def apply[A: Schema, Acc: Schema](module: RModule, step: String, merge: String,
                                    rscript: String = "Rscript", workers: Int = Stage.Workers): RReducer[A, Acc] =
    new RReducer[A, Acc](module, step, merge, rscript, workers)
