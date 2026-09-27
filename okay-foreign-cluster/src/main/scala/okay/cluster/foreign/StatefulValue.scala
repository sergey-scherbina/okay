package okay.cluster.foreign

import okay.codec.Schema
import okay.foreign.{ForeignEval, ValueCodec, PyModule, Value}
import okay.r.RModule

/**
 * A FUNCTIONAL stateful stage (specs/foreign-map-reduce.md, stage 5): the
 * state a VALUE the JVM carries, each step answering the next one beside
 * its rows —
 *
 *   open(params)        -> state
 *   step(frame, state)  -> {rows: frame, state: state'}
 *   finish(state)       -> frame
 *
 * — three plain calls with a table among the arguments (`Value.Table`,
 * pyvalue-table), no held object, no ref. So it is the stateful stage the
 * compiled workers have: Go, Rust and Haskell hold values they never
 * change in place (specs/foreign-one.md, Decision 23), and a function
 * answering a new state is exactly that. Python and R have it too, beside
 * `Stateful` (a state changed in place); each an instance, each optional.
 *
 * Because the state travels, NO worker is leased for the partition: any
 * worker of the pool takes the next step, and a death costs the chunk a
 * retry, not the partition — the cheaper fault model of the two. The
 * price is the state's size on the wire per chunk: a running sum is
 * bytes, a window is its rows.
 */
trait StatefulValue[-M]:
  def streamer[A: Schema, B: Schema, St: Schema, P: Schema]
              (module: M, open: String, step: String, finish: String, params: P, workers: Int): Streamer[A, B]

object StatefulValue:
  given py: StatefulValue[PyModule] = of(Language.python)
  given r: StatefulValue[RModule] = of(Language.r("Rscript"))
  given worker: StatefulValue[WorkerModule] = of(Language.worker)
  given jvm: StatefulValue[JvmModule] = new:
    def streamer[A: Schema, B: Schema, St: Schema, P: Schema]
                (module: JvmModule, open: String, step: String, finish: String, params: P, workers: Int): Streamer[A, B] =
      module.valueStreamer[A, B, St, P](open, step, finish, params)

  def py(python: String): StatefulValue[PyModule] = of(Language.py(python))
  def r(rscript: String): StatefulValue[RModule] = of(Language.r(rscript))

  /** one body over any wire language */
  def of[M](lang: Language[M]): StatefulValue[M] = new:
    def streamer[A: Schema, B: Schema, St: Schema, P: Schema]
                (module: M, open: String, step: String, finish: String, params: P, workers: Int): Streamer[A, B] =
      ForeignValueStreamer[M, A, B, St, P](lang, module, open, step, finish, params, workers)

final class ForeignValueStreamer[M, A, B, St, P](lang: Language[M], module: M, openFn: String, stepFn: String, finishFn: String,
                                                 params: P, workers: Int)
                                                (using sa: Schema[A], sb: Schema[B], sst: Schema[St], sp: Schema[P]) extends Streamer[A, B]:
  val name = lang.label(module, s"$openFn/$stepFn/$finishFn")
  private val pool = lang.workers(module, workers)

  /** the partition's state: the latest value, replaced by each step */
  final class S(var state: St)

  private def call(fn: String, args: Vector[Value]): Either[Batcher.Failed, Value] =
    Workers.use(pool, lang.who)(_.handler.handle(ForeignEval.Call(lang.address(module, fn), args))).left.map(Language.failed)

  private def shaped(what: String, v: Value): Batcher.Failed =
    Batcher.Failed("StepShape", s"$name: $what, got ${v.getClass.getSimpleName}")

  private def rowsOf(what: String, v: Value): Either[Batcher.Failed, Vector[B]] = v match
    case Value.Table(f) => f.rows[B].left.map(Language.failed)
    case other => Left(shaped(s"$what answers a frame", other))

  def open(): Either[Batcher.Failed, S] =
    call(openFn, Vector(ValueCodec.encode(params)))
      .flatMap(v => ValueCodec.decode[St](v).left.map(Language.failed))
      .map(S(_))

  def step(s: S, rows: Vector[A]): Either[Batcher.Failed, Vector[B]] =
    lang.shape.frame(rows).left.map(Language.failed).flatMap { frame =>
      call(stepFn, Vector(Value.Table(frame), ValueCodec.encode(s.state))).flatMap {
        case Value.Dict(kv) =>
          val m = kv.toMap
          for
            out <- m.get("rows").toRight(shaped("`step` answers {rows, state}: no rows", Value.Dict(kv))).flatMap(rowsOf("`rows`", _))
            next <- m.get("state").toRight(shaped("`step` answers {rows, state}: no state", Value.Dict(kv)))
              .flatMap(v => ValueCodec.decode[St](v).left.map(Language.failed))
          yield { s.state = next; out }
        case other => Left(shaped("`step` answers a dict {rows, state}", other))
      }
    }

  def finish(s: S): Either[Batcher.Failed, Vector[B]] =
    call(finishFn, Vector(ValueCodec.encode(s.state))).flatMap(rowsOf("`finish`", _))

  /** nothing is held anywhere: the state was a value in this JVM */
  def abandon(s: S): Unit = ()
