package okay.cluster.foreign

import okay.codec.Schema
import okay.foreign.{ForeignEval, ForeignWorker, PyFrame, PyModule, PyRef, PyValue, PyWorkers, Shape}
import okay.r.{RFrame, RModule, RSubprocess, RValue, REval}

/** a TypeScript module: its source, loaded by a Node worker
 * (foreign-more-languages) — the shape a `PyModule` has */
final case class TsModule(name: String, source: String)

/** a COMPILED worker — Go, Rust or Haskell (foreign-more-languages): the
 * command that starts it; its functions and programs are the ones it was
 * built with, addressed by their bare names. `language` is its word:
 * "go", "rust", "haskell" */
final case class WorkerModule(language: String, name: String, command: Seq[String])

/**
 * A WIRE LANGUAGE, as the cluster and the facade need it (foreign-one-runtime,
 * specs/foreign-one.md stage 4): where its workers come from, how its
 * functions are addressed, and the value rules its answers are read by.
 * That is ALL that differs between Python and R above the engine — one
 * engine, one value tree, one protocol, one pool since stages 1–3 — so every
 * capability below (a stage, a reducer, a stateful stage, a model, and the
 * facade's calls, frames, programs and handles) is ONE body over this, and a
 * new wire language is one more `Language`.
 */
trait Language[-M]:
  /** "py:python3", "r:Rscript" — which interpreter */
  def name: String
  /** "py", "r" — a stage's name begins with it */
  def tag: String
  /** the module's own name */
  def module(m: M): String

  /** what a stage over `fn` of `m` is called: "py:shop:total" */
  def label(m: M, fn: String): String = s"$tag:${module(m)}:$fn"
  /** the language's own word, for `Speaks` */
  def word: String
  /** the one pool of this module's workers on this JVM */
  def workers(module: M, n: Int): PyWorkers
  /** a function of the module, as this language addresses it */
  def address(module: M, fn: String): String
  /** how a Scala value becomes this language's value, and a frame's rows */
  def shape: Shape
  /** who could not be opened, in a refusal */
  def who: String

object Language:
  /** Python, `python3` on the PATH */
  val python: Language[PyModule] = py("python3")

  def py(python: String): Language[PyModule] = new:
    def name = s"py:$python"
    def tag = "py"
    def module(m: PyModule): String = m.name
    def word = "python"
    def workers(module: PyModule, n: Int): PyWorkers = PyPool.of(module, python, n)
    def address(module: PyModule, fn: String): String = s"${module.name}:$fn"
    def shape: Shape = Shape.python
    def who = s"the python '$python'"

  /** R, `Rscript` on the PATH */
  val rscript: Language[RModule] = r("Rscript")

  def r(rscript: String): Language[RModule] = new:
    def name = s"r:$rscript"
    def tag = "r"
    def module(m: RModule): String = m.name
    def word = "r"
    def workers(module: RModule, n: Int): PyWorkers = RPool.of(module, rscript, n)
    def address(module: RModule, fn: String): String = s"${module.name}::$fn"
    def shape: Shape = okay.r.R.shape
    def who = s"'$rscript'"

  /** TypeScript, `node` on the PATH (foreign-more-languages) */
  val node: Language[TsModule] = ts("node")

  def ts(node: String): Language[TsModule] = new:
    def name = s"ts:$node"
    def tag = "ts"
    def module(m: TsModule): String = m.name
    def word = "typescript"
    def workers(module: TsModule, n: Int): PyWorkers =
      Workers.of(s"ts|$node|${module.name}|${module.source.hashCode}", s"ts:${module.name}", n, () =>
        val dir = java.nio.file.Files.createTempDirectory("okay-ts-module")
        java.nio.file.Files.writeString(dir.resolve(s"${module.name}.ts"), module.source): Unit
        okay.foreign.TsWorker.start(dir, modules = Seq(module.name), node = node))
    def address(module: TsModule, fn: String): String = s"${module.name}:$fn"
    def shape: Shape = Shape.python
    def who = s"the node '$node'"

  /** a compiled worker, Go, Rust or Haskell: started by its command, its
   * functions by their bare names (foreign-more-languages) */
  val worker: Language[WorkerModule] = new:
    def name = "worker"
    def tag = "worker"
    def module(m: WorkerModule): String = m.name
    override def label(m: WorkerModule, fn: String): String = s"${m.language}:${m.name}:$fn"
    def word = "worker"
    def workers(module: WorkerModule, n: Int): PyWorkers =
      Workers.of(s"worker|${module.command.mkString(" ")}", s"${module.language}:${module.name}", n, () =>
        ForeignWorker.speaking(module.command))
    def address(module: WorkerModule, fn: String): String = fn
    def shape: Shape = Shape.python
    def who = "the worker"

  private[foreign] def failed(c: okay.foreign.Condition): Batcher.Failed = Batcher.Failed(c.kind, c.message)

  /** a row type that is not a flat case class is refused where the stage is
   * MADE, not at the first chunk on a worker */
  private[foreign] def flat[X](lang: Language[?], stage: String, what: String)(using Schema[X]): Unit =
    lang.shape.frame(Vector.empty[X]) match
      case Left(c) => throw IllegalArgumentException(s"$stage: the row type $what must be a flat case class — ${c.message}")
      case Right(_) => ()

/** THE STAGE, one body for every wire language: a chunk of rows as one table
 * through `fn`, its rows back — read by the rules of the worker that answered */
final class ForeignStage[M, A, B](lang: Language[M], module: M, fn: String, workers: Int)
                                (using sa: Schema[A], sb: Schema[B]) extends Batcher[A, B]:
  val name = lang.label(module, fn)
  private val address = lang.address(module, fn)
  Language.flat[A](lang, name, "A")
  Language.flat[B](lang, name, "B")
  private val pool = lang.workers(module, workers)

  def apply(rows: Vector[A]): Either[Batcher.Failed, Vector[B]] =
    lang.shape.frame(rows).flatMap(frame =>
      Workers.use(pool, lang.who)(_.handler.handle(ForeignEval.Frame(address, frame, Vector.empty))))
      .flatMap(_.rows[B]).left.map(Language.failed)

/** THE REDUCER, one body: `step(frame, acc)` folds a chunk into one row,
 * `merge(a, b)` two partials */
final class ForeignReducer[M, A, Acc](lang: Language[M], module: M, stepFn: String, mergeFn: String, workers: Int)
                                     (using sa: Schema[A], sacc: Schema[Acc]) extends Reducer[A, Acc]:
  val name = lang.label(module, s"$stepFn/$mergeFn")
  Language.flat[A](lang, name, "A")
  Language.flat[Acc](lang, name, "Acc")
  private val pool = lang.workers(module, workers)

  def step(acc: Option[Acc], rows: Vector[A]): Either[Batcher.Failed, Acc] =
    lang.shape.frame(rows).flatMap { frame =>
      val arg = acc.fold[PyValue](PyValue.PyNone)(lang.shape.encode(_))
      Workers.use(pool, lang.who)(_.handler.handle(ForeignEval.Frame(lang.address(module, stepFn), frame, Vector(arg))))
    }.flatMap(_.rows[Acc]).left.map(Language.failed).flatMap(ForeignReducer.one(name))

  def merge(a: Acc, b: Acc): Either[Batcher.Failed, Acc] =
    Workers.use(pool, lang.who)(_.handler.handle(ForeignEval.Call(lang.address(module, mergeFn),
      Vector(lang.shape.encode(a), lang.shape.encode(b)))))
      .flatMap(lang.shape.decode[Acc]).left.map(Language.failed)

object ForeignReducer:
  /** a partial is exactly one row */
  private[foreign] def one[Acc](name: String)(rows: Vector[Acc]): Either[Batcher.Failed, Acc] =
    rows match
      case Vector(a) => Right(a)
      case other => Left(Batcher.Failed("ReduceShape", s"$name: `step` answered ${other.length} rows, and a partial is one"))

/** THE STATEFUL STAGE, one body: the state a held object in ONE leased
 * worker for the partition's life — `open()`, `step(frame, state)`,
 * `finish(frame, state)` — given back on every path */
final class ForeignStreamer[M, A, B](lang: Language[M], module: M, openFn: String, stepFn: String, finishFn: String, workers: Int)
                                    (using sa: Schema[A], sb: Schema[B]) extends Streamer[A, B]:
  val name = lang.label(module, s"$openFn/$stepFn/$finishFn")
  private val pool = lang.workers(module, workers)
  final class S(val lease: okay.foreign.Pool[ForeignWorker]#Lease, val ref: PyRef)

  /** one operation on the leased worker; a death releases the lease as dead
   * and is the wire's failure */
  private def on[X](s: S)(f: ForeignWorker => Either[okay.foreign.Condition, X]): Either[Batcher.Failed, X] =
    try f(s.lease.e).left.map(Language.failed)
    catch case e: IllegalStateException if Workers.dead(e) =>
      s.lease.release(dead = true)
      Left(Batcher.Failed("WorkerDied", e.getMessage))

  def open(): Either[Batcher.Failed, S] =
    val leased =
      try Right(pool.lease())
      catch case e: Exception => Left(Batcher.Failed("WorkerUnavailable", s"${lang.who} could not be opened: ${e.getMessage}"))
    leased.flatMap { lease =>
      val opened =
        try lease.e.handler.handle(ForeignEval.Call(lang.address(module, openFn), Vector.empty, held = true))
          .flatMap(okay.foreign.Wire.asRef).left.map(Language.failed)
        catch case e: IllegalStateException if Workers.dead(e) => Left(Batcher.Failed("WorkerDied", e.getMessage))
      opened match
        case Right(ref) => Right(S(lease, ref))
        case Left(f) => lease.release(dead = f.kind == "WorkerDied"); Left(f)
    }

  def step(s: S, rows: Vector[A]): Either[Batcher.Failed, Vector[B]] =
    lang.shape.frame(rows).left.map(Language.failed).flatMap(frame =>
      on(s)(_.handler.handle(ForeignEval.Frame(lang.address(module, stepFn), frame, Vector(PyValue.Ref(s.ref)))))
        .flatMap(_.rows[B].left.map(Language.failed)))

  def finish(s: S): Either[Batcher.Failed, Vector[B]] =
    val last = on(s)(_.handler.handle(ForeignEval.Frame(lang.address(module, finishFn), PyFrame(Vector.empty, lang.shape),
      Vector(PyValue.Ref(s.ref))))).flatMap(_.rows[B].left.map(Language.failed))
    if last.isRight || last.left.exists(_.kind != "WorkerDied") then abandon(s)
    last

  def abandon(s: S): Unit =
    try s.lease.e.handler.handle(ForeignEval.Release(s.ref)) catch case _: Exception => ()
    s.lease.release(dead = !s.lease.e.alive)

/** THE MODEL, one body: fit once per worker of the pool from its
 * parameters, the second argument of every chunk's map */
final class ForeignModel[M, P](lang: Language[M], module: M, fn: String, params: P, workers: Int)
                             (using sp: Schema[P]) extends Model:
  val name = lang.label(module, fn)
  private val pool = lang.workers(module, workers)
  private val refs = java.util.WeakHashMap[ForeignWorker, PyRef]()

  /** this worker's copy, made on its first chunk */
  private def refFor(w: ForeignWorker): Either[okay.foreign.Condition, PyRef] = refs.synchronized {
    Option(refs.get(w)) match
      case Some(r) => Right(r)
      case None =>
        w.handler.handle(ForeignEval.Call(lang.address(module, fn), Vector(lang.shape.encode(params)), held = true))
          .flatMap(okay.foreign.Wire.asRef).map { r => refs.put(w, r): Unit; r }
  }

  def batcher[A: Schema, B: Schema](mapFn: String): Batcher[A, B] = new:
    val name = lang.label(module, s"$mapFn($fn)")
    def apply(rows: Vector[A]): Either[Batcher.Failed, Vector[B]] =
      lang.shape.frame(rows).flatMap(frame =>
        Workers.use(pool, lang.who) { w =>
          refFor(w).flatMap(ref => w.handler.handle(ForeignEval.Frame(lang.address(module, mapFn), frame, Vector(PyValue.Ref(ref)))))
        }).flatMap(_.rows[B]).left.map(Language.failed)

/** the Python workers of a module on this JVM, and one exchange with them */
object PyPool:
  /** the Python workers of `module` on this JVM (`Workers`) */
  def of(module: PyModule, python: String, workers: Int): PyWorkers =
    Workers.of(s"py|$python|${module.name}|${module.source.hashCode}", s"py:${module.name}", workers,
      () => ForeignWorker.start(python, modules = Seq(module)))

  private[foreign] def dead(e: Throwable): Boolean = Workers.dead(e)

  private[foreign] def use[X](pool: PyWorkers, python: String)
                             (f: ForeignWorker => Either[okay.foreign.Condition, X]): Either[okay.foreign.Condition, X] =
    Workers.use(pool, s"the python '$python'")(f)

  def frame(pool: PyWorkers, python: String, address: String, in: PyFrame, args: Vector[PyValue])
  : Either[okay.foreign.Condition, PyFrame] =
    use(pool, python)(_.handler.handle(ForeignEval.Frame(address, in, args)))

  /** a Table through `address`, as itself where Arrow is spoken (facade-frame-seam) */
  def frameTable(pool: PyWorkers, python: String, address: String, in: okay.arrow.Table, args: Vector[PyValue])
  : Either[okay.foreign.Condition, okay.arrow.Table] =
    use(pool, python)(_.frameTable(address, in, args))

  def call(pool: PyWorkers, python: String, address: String, args: Vector[PyValue])
  : Either[okay.foreign.Condition, PyValue] =
    use(pool, python)(_.handler.handle(ForeignEval.Call(address, args)))

/** the R workers of a module on this JVM — `ForeignWorker`s speaking R
 * (foreign-one-value), pooled as any other */
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
