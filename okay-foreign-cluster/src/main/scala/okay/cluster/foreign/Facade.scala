package okay.cluster.foreign

import okay.codec.Schema
import okay.arrow.{Rows, Table}
import okay.cluster.Flow
import okay.{!, +}
import okay.given

/**
 * THE FACADE OVER EVERY FOREIGN LANGUAGE (specs/foreign-facade.md): one
 * typeclass per CAPABILITY, by the module's type — the shape
 * foreign-engine-typeclass gave `Engine` (map) and `Reduces` (reduce),
 * carried to the rest. A job asks for the instances it needs and names a
 * module; the implicit says whether that is Python, R or the JVM, and a
 * capability a language lacks is a missing instance — a compile error at
 * the call, never a refusal at the first row.
 *
 * `Calls` is TIER 1 of the data model: one typed value in, one out,
 * crossing as one line of the wire. The value's vocabulary is `Schema`;
 * a language's own value enum (`PyValue`, `RValue`) is its codec's
 * business behind the instance, not the caller's.
 */
trait Calls[-M]:
  def name: String
  /** `fn` of `module` applied to one value, its answer decoded at `B`; a
   * refusal — the function raised, the value did not decode, the worker
   * died — is a `Batcher.Failed` by kind, as every road here answers */
  def call[A: Schema, B: Schema](module: M, fn: String)(a: A): Either[Batcher.Failed, B]

object Calls:
  /** ONE body for every wire language (foreign-one-runtime): the value by
   * the language's rules, through its one pool, the answer back by them */
  def of[M](lang: Language[M]): Calls[M] = new:
    def name = lang.name
    def call[A: Schema, B: Schema](module: M, fn: String)(a: A): Either[Batcher.Failed, B] =
      Workers.use(lang.workers(module, Stage.Workers), lang.who)(
        _.handler.handle(okay.foreign.ForeignEval.Call(lang.address(module, fn), Vector(lang.shape.encode(a)))))
        .flatMap(lang.shape.decode[B]).left.map(Language.failed)

  /** Python, `python3` on the PATH; `Calls.py(path)` for another */
  given py: Calls[okay.foreign.PyModule] = py("python3")
  def py(python: String): Calls[okay.foreign.PyModule] = of(Language.py(python))

  /** R, `Rscript` on the PATH; `Calls.r(path)` for another */
  given r: Calls[okay.r.RModule] = r("Rscript")
  def r(rscript: String): Calls[okay.r.RModule] = of(Language.r(rscript))

  /** TypeScript, `node` on the PATH; a compiled Go, Rust or Haskell worker */
  given ts: Calls[TsModule] = of(Language.node)
  given worker: Calls[WorkerModule] = of(Language.worker)

  /** the JVM's own languages: a function registered by name, called as
   * the Scala function it is — no wire, no codec, the zero-cost tier */
  given jvm: Calls[JvmModule] = new:
    def name = "jvm"
    def call[A: Schema, B: Schema](module: JvmModule, fn: String)(a: A): Either[Batcher.Failed, B] =
      module.caller[A, B](fn).flatMap(_(a))

/**
 * TIER 2 of the data model: a TABLE in, a table out — `okay.arrow.Table`,
 * crossing as Arrow IPC where the worker speaks it and as the columnar
 * JSON of r-frame-columnar-wire where it does not, the same `Table`
 * either way (`Speaks` says which). On the JVM the table crosses by
 * reference: the same object, nothing copied — the zero-cost tier the
 * spec makes the test of the model.
 *
 * THE SHAPE PICKS THE TIER, not a threshold: a function written for a
 * frame takes a dict of columns and one written for a record takes a
 * record, so a `Vector[A]` is always a frame (`Road.rows`) and one value
 * is always a call — there is no size at which the one becomes the
 * other (specs/foreign-facade.md, Decision 6).
 */
trait Frames[-M]:
  def name: String
  def frame(module: M, fn: String)(in: Table): Either[Batcher.Failed, Table]
  /** rows through a frame function, back as rows — the road a caller
   * with ROWS takes; by default through `frame` and the Rows codec, and
   * a language overrides it with its own shortest road (Python: rows to
   * its frame in one pass, facade-frame-seam) */
  def rows[A: Schema, B: Schema](module: M, fn: String)(in: Vector[A]): Either[Batcher.Failed, Vector[B]] =
    frame(module, fn)(Rows.table(in)).flatMap(t => Rows.rows[B](t).left.map(m => Batcher.Failed("Frame", m)))

object Frames:
  /** ONE body for every wire language: a Table goes to the wire AS ITSELF
   * where the worker speaks Arrow and is converted once, by the WORKER's
   * rules, where it does not (`ForeignWorker.frameTable`, facade-frame-seam
   * — R included since foreign-one-runtime, which converted twice); rows
   * take the language's own road, one frame by its rules each way */
  def of[M](lang: Language[M]): Frames[M] = new:
    def name = lang.name
    def frame(module: M, fn: String)(in: Table): Either[Batcher.Failed, Table] =
      Workers.use(lang.workers(module, Stage.Workers), lang.who)(_.frameTable(lang.address(module, fn), in, Vector.empty))
        .left.map(Language.failed)
    override def rows[A: Schema, B: Schema](module: M, fn: String)(in: Vector[A]): Either[Batcher.Failed, Vector[B]] =
      lang.shape.frame(in).flatMap(f =>
        Workers.use(lang.workers(module, Stage.Workers), lang.who)(
          _.handler.handle(okay.foreign.ForeignEval.Frame(lang.address(module, fn), f, Vector.empty))))
        .flatMap(_.rows[B]).left.map(Language.failed)

  given py: Frames[okay.foreign.PyModule] = py("python3")
  def py(python: String): Frames[okay.foreign.PyModule] = of(Language.py(python))
  given r: Frames[okay.r.RModule] = r("Rscript")
  def r(rscript: String): Frames[okay.r.RModule] = of(Language.r(rscript))
  given ts: Frames[TsModule] = of(Language.node)
  given worker: Frames[WorkerModule] = of(Language.worker)

  /** the JVM: the table by reference, the function as registered */
  given jvm: Frames[JvmModule] = new:
    def name = "jvm"
    def frame(module: JvmModule, fn: String)(in: Table): Either[Batcher.Failed, Table] =
      module.framer(fn).flatMap(_(in))

/**
 * TIER 3 of the data model: MORE THAN FITS IN MEMORY crosses one frame at
 * a time. A `Flow[A]` goes through a frame function chunk by chunk —
 * each chunk one tier-2 frame there and back — so neither side ever
 * holds more than `batch` rows of it: the memory bound is the frame.
 *
 * Every language with `Frames` has this road, DRIVEN FROM HERE
 * (`Streams.viaFrames`): the next frame is sent when the last answered,
 * which is the back-pressure. A far side that drives a stream itself
 * (a generator answering chunk by chunk) is what `Speaks.stream` will
 * say when a shim grows it; until then this is the road for all.
 */
trait Streams[-M]:
  def name: String
  def stream[A: Schema, B: Schema](module: M, fn: String, batch: Int = Stage.Batch)(in: Flow[A]): Flow[B]

object Streams:
  /** the road every `Frames` language has: one frame per chunk */
  given viaFrames[M](using f: Frames[M]): Streams[M] = new:
    def name = f.name
    def stream[A: Schema, B: Schema](module: M, fn: String, batch: Int)(in: Flow[A]): Flow[B] =
      Stage.through(in, batcher[M, A, B](module, fn), batch, 3)

  /** a frame function as a Batcher: rows to a table, over, back to rows */
  def batcher[M, A: Schema, B: Schema](module: M, fn: String)(using f: Frames[M]): Batcher[A, B] = new:
    val name = s"${f.name}:$fn"
    def apply(rows: Vector[A]): Either[Batcher.Failed, Vector[B]] = Road.rows[M, A, B](module, fn)(rows)

/**
 * A CALLBACK in the facade's vocabulary: a name, and a function at
 * `Schema` types — what a far-side program performs by name and this
 * side answers under its own handlers (Reader, Choice, …). okay-py's and
 * okay-r's `Callback` say the same thing over `PyValue` and `RValue`;
 * this one says it once, and each `Programs` instance turns it into its
 * language's (specs/foreign-facade.md, Decision 3).
 */
trait Cb[F[+_]]:
  type Arg
  type Res
  def name: String
  def arg: Schema[Arg]
  def res: Schema[Res]
  def run: Arg => Res ! F

object Cb:
  def apply[F[+_], A, B](name0: String)(f: A => B ! F)(using sa: Schema[A], sb: Schema[B]): Cb[F] { type Arg = A; type Res = B } = new Cb[F]:
    type Arg = A
    type Res = B
    val name = name0
    val arg = sa
    val res = sb
    val run = f

/**
 * PROGRAMS AS DATA, behind the same typeclass shape: the far side returns
 * its program one node at a time — an answer, or a named operation plus
 * the id of the continuation it keeps — and this side performs the
 * operation under the caller's handlers and continues it, as often as a
 * handler asks (specs/remote-foreign.md). `Op` is the language's own
 * effect (`ForeignEval`, `REval`): a program is `Out ! (F + Op)`, the
 * caller handles `F` first and hands the rest to `run`, which keeps the
 * WHOLE dialogue on one pooled worker, because that worker holds the
 * continuations.
 *
 * No JVM instance: a program on the JVM is a Scala function returning
 * `Out ! F` and there is nothing to cross — the compile error a
 * `Programs[JvmModule]` gives is the honest answer (Decision 7).
 */
trait Programs[-M]:
  type Op[+A]
  def name: String
  def program[Arg: Schema, Out: Schema, F[+_]](module: M, fn: String, cbs: Vector[Cb[F]])(a: Arg): Either[Batcher.Failed, Out] ! (F + Op)
  def run[A](module: M)(prog: A ! Op): A

object Programs:
  /** ONE body for every wire language: the one API at the language's value
   * rules, and the whole walk on the one pool, which routes a program by its
   * run so its continuations stay on one worker */
  def of[M](lang: Language[M]): Programs[M] = new:
    type Op[+A] = okay.foreign.ForeignEval[A]
    def name = lang.name
    private given okay.foreign.Shape = lang.shape
    private def cb[F[+_]](c: Cb[F]): okay.foreign.Foreign.Callback[F] =
      okay.foreign.Foreign.callback[c.Arg, c.Res](c.name)(using c.arg, c.res)(c.run)
    def program[Arg: Schema, Out: Schema, F[+_]](module: M, fn: String, cbs: Vector[Cb[F]])(a: Arg): Either[Batcher.Failed, Out] ! (F + Op) =
      okay.foreign.Foreign.program[Out](lang.address(module, fn)).calling(okay.foreign.Foreign.callbacks[F](cbs.map(cb[F])*))(a).program
        .map(_.left.map(Language.failed))
    def run[A](module: M)(prog: A ! Op): A =
      prog.runWith(using lang.workers(module, Stage.Workers).handler)

  given py: Programs[okay.foreign.PyModule] = py("python3")
  def py(python: String): Programs[okay.foreign.PyModule] = of(Language.py(python))
  given r: Programs[okay.r.RModule] = r("Rscript")
  def r(rscript: String): Programs[okay.r.RModule] = of(Language.r(rscript))
  given ts: Programs[TsModule] = of(Language.node)
  given worker: Programs[WorkerModule] = of(Language.worker)

/**
 * OBJECT HANDLES (foreign-object-handles): a value the far side KEEPS —
 * a fitted model, an open dataset — and this side names by a handle. A
 * handle is an argument like any other (`apply` hands it to a function
 * as its first argument), and it lives in ONE process: a Python pool
 * routes a call naming a handle to the worker holding it (`PyWorkers`),
 * R keeps every handle of a module on one worker of its own.
 * `Ref` is the instance's handle type; a given is refined with it so a
 * handle from `Holds` is what `Methods` takes.
 */
trait Holds[-M]:
  type Ref
  def name: String
  def hold[Arg: Schema](module: M, fn: String)(a: Arg): Either[Batcher.Failed, Ref]
  def apply[Arg: Schema, Out: Schema](module: M, fn: String)(ref: Ref, a: Arg): Either[Batcher.Failed, Out]
  def release(module: M)(ref: Ref): Unit

/** a held object's own methods and attributes — Python's road; R's
 * objects have no methods to call, so R has no instance (an honest
 * absence, as `Programs[JvmModule]`'s) */
trait Methods[-M]:
  type Ref
  def method[Arg: Schema, Out: Schema](module: M, ref: Ref, name: String)(a: Arg): Either[Batcher.Failed, Out]
  def attr[Out: Schema](module: M, ref: Ref, name: String): Either[Batcher.Failed, Out]

object Holds:
  /** the instance types with their handle type visible: a `given` cannot
   * carry a refinement (the parser reads its `{` as a body), an alias can.
   * The handle is the one handle (`okay.foreign.PyRef`) for every language. */
  type Py = Holds[okay.foreign.PyModule] { type Ref = okay.foreign.PyRef }
  type R = Holds[okay.r.RModule] { type Ref = okay.foreign.PyRef }

  /** ONE body for every wire language: a handle lives in one worker of the
   * one pool, which routes every call naming it there */
  def of[M](lang: Language[M]): Holds[M] { type Ref = okay.foreign.PyRef } = new Holds[M]:
    type Ref = okay.foreign.PyRef
    def name = lang.name
    private given okay.foreign.Shape = lang.shape
    private def on(module: M) = lang.workers(module, Stage.Workers).handler
    def hold[Arg: Schema](module: M, fn: String)(a: Arg): Either[Batcher.Failed, Ref] =
      okay.foreign.Foreign.hold(lang.address(module, fn))(a).runWith(using on(module)).left.map(Language.failed)
    def apply[Arg: Schema, Out: Schema](module: M, fn: String)(ref: Ref, a: Arg): Either[Batcher.Failed, Out] =
      okay.foreign.Foreign.fn[Out](lang.address(module, fn))(ref, a).runWith(using on(module)).left.map(Language.failed)
    def release(module: M)(ref: Ref): Unit =
      ref.release.runWith(using on(module))

  given py: Py = py("python3")
  def py(python: String): Py = of(Language.py(python))
  given r: R = r("Rscript")
  def r(rscript: String): R = of(Language.r(rscript))
  /** TypeScript keeps held objects, and since foreign-held-values a compiled
   * Go, Rust or Haskell worker keeps held VALUES (no methods by name) */
  type Ts = Holds[TsModule] { type Ref = okay.foreign.PyRef }
  given ts: Ts = of(Language.node)
  type Worker = Holds[WorkerModule] { type Ref = okay.foreign.PyRef }
  given worker: Worker = of(Language.worker)

object Methods:
  type Py = Methods[okay.foreign.PyModule] { type Ref = okay.foreign.PyRef }
  type Ts = Methods[TsModule] { type Ref = okay.foreign.PyRef }

  /** ONE body for a language whose objects have methods and attributes by
   * name: Python's and TypeScript's (R's have none, an honest absence) */
  def of[M](lang: Language[M]): Methods[M] { type Ref = okay.foreign.PyRef } = new Methods[M]:
    type Ref = okay.foreign.PyRef
    def method[Arg: Schema, Out: Schema](module: M, ref: Ref, name: String)(a: Arg): Either[Batcher.Failed, Out] =
      ref.call[Out](name)(a).runWith(using lang.workers(module, Stage.Workers).handler).left.map(Language.failed)
    def attr[Out: Schema](module: M, ref: Ref, name: String): Either[Batcher.Failed, Out] =
      ref.attr[Out](name).runWith(using lang.workers(module, Stage.Workers).handler).left.map(Language.failed)

  given py: Py = py("python3")
  def py(python: String): Py = of(Language.py(python))
  given ts: Ts = of(Language.node)

/**
 * The doors a job uses, each picking the tier by the SHAPE it is handed
 * (Decision 6): a value is a call, rows are one frame — and every road
 * answers the same `Either[Batcher.Failed, _]`.
 */
object Road:
  /** rows of `A` through a frame function, back as rows of `B`: ONE
   * frame each way, whatever the count — a function written for a frame
   * is a function written for a frame */
  def rows[M, A: Schema, B: Schema](module: M, fn: String)(rows: Vector[A])(using f: Frames[M]): Either[Batcher.Failed, Vector[B]] =
    f.rows[A, B](module, fn)(rows)

  /** a flow through a frame function, one frame per chunk of `batch` rows */
  def flow[M, A: Schema, B: Schema](module: M, fn: String, batch: Int = Stage.Batch)(in: Flow[A])(using s: Streams[M]): Flow[B] =
    s.stream[A, B](module, fn, batch)(in)

  /** one value through a record function */
  def value[M, A: Schema, B: Schema](module: M, fn: String)(a: A)(using c: Calls[M]): Either[Batcher.Failed, B] =
    c.call[A, B](module, fn)(a)

/**
 * WHAT A WORKER DOES, as against what its instances promise it CAN
 * (specs/foreign-facade.md, "Speaks"): the tiers and the link this
 * module's process has, read from the wire it negotiated. An instance
 * says a language can cross a frame; `Speaks` says whether THIS worker
 * crosses it as Arrow or as columnar JSON, and the conformance suite
 * holds the two together.
 */
trait Speaks[-M]:
  def speaks(module: M): Speaks.Report

object Speaks:
  /**
   * @param language  python, r, jvm, …
   * @param link      pipes, tcp, ffm, wasm, or in-jvm
   * @param frames    how a tier-2 frame crosses: arrow, columnar-json, or
   *                  by-reference (the JVM: the same Table object)
   * @param stream    whether the far side drives tier 3 itself (a
   *                  language without it gets frames driven from here)
   * @param programs  programs as data: multi-shot (continuations are
   *                  values there), one-shot, in-jvm, or none
   */
  final case class Report(language: String, link: String, frames: String, stream: Boolean, programs: String)

  /** ONE body: the language's word and what its worker negotiated. Every
   * wire language's continuations are values here (remote-foreign), so
   * programs are multi-shot */
  def of[M](lang: Language[M]): Speaks[M] = new:
    def speaks(module: M): Report =
      val wire = lang.workers(module, Stage.Workers).use(_.wire)
      Report(lang.word, "pipes", if wire.endsWith("+arrow") then "arrow" else "columnar-json", stream = false, "multi-shot")

  given py: Speaks[okay.foreign.PyModule] = py("python3")
  def py(python: String): Speaks[okay.foreign.PyModule] = of(Language.py(python))
  given r: Speaks[okay.r.RModule] = r("Rscript")
  def r(rscript: String): Speaks[okay.r.RModule] = of(Language.r(rscript))
  given ts: Speaks[TsModule] = of(Language.node)
  /** a compiled worker says which language it is in its module */
  given worker: Speaks[WorkerModule] = new:
    def speaks(module: WorkerModule): Report = of(Language.worker).speaks(module).copy(language = module.language)

  given jvm: Speaks[JvmModule] = new:
    def speaks(module: JvmModule): Report = Report("jvm", "in-jvm", "by-reference", stream = false, "in-jvm")
