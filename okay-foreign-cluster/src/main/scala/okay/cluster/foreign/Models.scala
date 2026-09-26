package okay.cluster.foreign

import okay.codec.Schema
import okay.py.PyModule
import okay.r.RModule

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
  /** a model is a held object: TypeScript's, not yet a compiled worker's (foreign-held-values) */
  given ts: Models[TsModule] = new:
    def model[P: Schema](module: TsModule, fn: String, params: P, workers: Int): Model =
      ForeignModel[TsModule, P](Language.node, module, fn, params, workers)
  given jvm: Models[JvmModule] = new:
    def model[P: Schema](module: JvmModule, fn: String, params: P, workers: Int): Model = module.model(fn, params)

  def py(python: String): Models[PyModule] = new:
    def model[P: Schema](module: PyModule, fn: String, params: P, workers: Int): Model =
      ForeignModel[PyModule, P](Language.py(python), module, fn, params, workers)

  def r(rscript: String): Models[RModule] = new:
    def model[P: Schema](module: RModule, fn: String, params: P, workers: Int): Model =
      ForeignModel[RModule, P](Language.r(rscript), module, fn, params, workers)

/** the facade: `Model.in(module, "fit", Params(…))` */
object Model:
  def in[P: Schema](module: Any, fn: String, params: P, workers: Int = Stage.Workers)
                   (using m: Models[module.type]): Model =
    m.model[P](module, fn, params, workers)
