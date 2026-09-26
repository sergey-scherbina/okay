package okay.cluster.foreign

import okay.codec.Schema
import okay.py.{ForeignEval, ForeignWorker, PyCodec, PyFrame, PyModule, PyRef, PyValue}
import okay.r.{RCodec, REval, RFrame, RModule}

/**
 * A MODEL ON THE FAR SIDE, as an extension of the engine typeclass
 * (specs/foreign-map-reduce.md, stage 4): fit once and used by every
 * chunk of a map, `predict(frame, model)`. It is not a HANDLE
 * (`Holds`, specs/foreign-facade.md: one object on one worker, named and
 * called): a stage runs over a POOL of interpreters, so a `Model` is a RECIPE
 * — the module, the function that makes it, its parameters — materialised
 * once per interpreter, the first time that interpreter takes a chunk of
 * the stage, and cached for it. An interpreter that dies takes its copy
 * with it; the fresh one makes its own.
 */
trait Models[-M]:
  def model[P: Schema](module: M, fn: String, params: P, workers: Int): Model

/** what a `Models` gives: the far object as an argument to a map */
trait Model:
  def name: String
  /** a batcher whose far function is called with the frame and this
   * held object: `fn(frame, held)` */
  def batcher[A: Schema, B: Schema](fn: String): Batcher[A, B]

object Models:
  given py: Models[PyModule] = py("python3")
  given r: Models[RModule] = r("Rscript")
  given jvm: Models[JvmModule] = new:
    def model[P: Schema](module: JvmModule, fn: String, params: P, workers: Int): Model = module.model(fn, params)

  def py(python: String): Models[PyModule] = new:
    def model[P: Schema](module: PyModule, fn: String, params: P, workers: Int): Model =
      PyModel[P](module, fn, params, python, workers)

  def r(rscript: String): Models[RModule] = new:
    def model[P: Schema](module: RModule, fn: String, params: P, workers: Int): Model =
      RModel[P](module, fn, params, rscript, workers)

/** the facade: `Model.in(module, "fit", Params(…))` */
object Model:
  def in[P: Schema](module: Any, fn: String, params: P, workers: Int = Stage.Workers)
                   (using m: Models[module.type]): Model =
    m.model[P](module, fn, params, workers)

final class PyModel[P](module: PyModule, fn: String, params: P, python: String, workers: Int)
                     (using sp: Schema[P]) extends Model:
  val name = s"py:${module.name}:$fn"
  private val pool = PyPool.of(module, python, workers)
  private val refs = java.util.WeakHashMap[ForeignWorker, PyRef]()

  /** this interpreter's copy, made on its first chunk */
  private def refFor(w: ForeignWorker): Either[okay.py.Condition, PyRef] = refs.synchronized {
    Option(refs.get(w)) match
      case Some(r) => Right(r)
      case None =>
        w.handler.handle(ForeignEval.Call(s"${module.name}:$fn", Vector(PyCodec.encode(params)), held = true)).flatMap(okay.py.Wire.asRef)
          .map { r => refs.put(w, r): Unit; r }
  }

  def batcher[A: Schema, B: Schema](mapFn: String): Batcher[A, B] = new:
    val name = s"py:${module.name}:$mapFn($fn)"
    def apply(rows: Vector[A]): Either[Batcher.Failed, Vector[B]] =
      PyFrame.of(rows) match
        case Left(c) => Left(Batcher.Failed(c.kind, c.message))
        case Right(frame) =>
          PyPool.use(pool, python) { w =>
            refFor(w).flatMap(ref =>
              w.handler.handle(ForeignEval.Frame(s"${module.name}:$mapFn", frame, Vector(PyValue.Ref(ref)))))
          }.flatMap(_.rows[B]).left.map(c => Batcher.Failed(c.kind, c.message))

final class RModel[P](module: RModule, fn: String, params: P, rscript: String, workers: Int)
                    (using sp: Schema[P]) extends Model:
  val name = s"r:${module.name}:$fn"
  private val pool = RPool.of(module, rscript, workers)
  private val refs = java.util.WeakHashMap[ForeignWorker, PyRef]()

  private def refFor(r: ForeignWorker): Either[okay.r.Condition, PyRef] = refs.synchronized {
    Option(refs.get(r)) match
      case Some(ref) => Right(ref)
      case None =>
        r.handler.handle(REval.Call(s"${module.name}::$fn", Vector(RCodec.encode(params)), held = true)).flatMap(okay.py.Wire.asRef)
          .map { ref => refs.put(r, ref): Unit; ref }
  }

  def batcher[A: Schema, B: Schema](mapFn: String): Batcher[A, B] = new:
    val name = s"r:${module.name}:$mapFn($fn)"
    def apply(rows: Vector[A]): Either[Batcher.Failed, Vector[B]] =
      RFrame.of(rows) match
        case Left(c) => Left(Batcher.Failed(c.kind, c.message))
        case Right(frame) =>
          RPool.use(pool, rscript) { r =>
            refFor(r).flatMap(ref =>
              r.handler.handle(REval.Frame(s"${module.name}::$mapFn", frame, Vector(PyValue.Ref(ref)))))
          }.flatMap(_.rows[B]).left.map(c => Batcher.Failed(c.kind, c.message))
