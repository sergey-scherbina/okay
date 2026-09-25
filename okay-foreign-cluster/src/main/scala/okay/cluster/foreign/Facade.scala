package okay.cluster.foreign

import okay.codec.Schema
import okay.arrow.{Rows, Table}

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
  /** Python, `python3` on the PATH; `Calls.py(path)` for another */
  given py: Calls[okay.py.PyModule] = py("python3")

  def py(python: String): Calls[okay.py.PyModule] = new:
    def name = s"py:$python"
    def call[A: Schema, B: Schema](module: okay.py.PyModule, fn: String)(a: A): Either[Batcher.Failed, B] =
      val pool = PyPool.of(module, python, Stage.Workers)
      PyPool.call(pool, python, s"${module.name}:$fn", Vector(okay.py.PyCodec.encode(a)))
        .flatMap(v => okay.py.PyCodec.decode[B](v))
        .left.map(c => Batcher.Failed(c.kind, c.message))

  /** R, `Rscript` on the PATH; `Calls.r(path)` for another */
  given r: Calls[okay.r.RModule] = r("Rscript")

  def r(rscript: String): Calls[okay.r.RModule] = new:
    def name = s"r:$rscript"
    def call[A: Schema, B: Schema](module: okay.r.RModule, fn: String)(a: A): Either[Batcher.Failed, B] =
      val pool = RPool.of(module, rscript, Stage.Workers)
      RPool.call(pool, rscript, s"${module.name}:$fn", Vector(okay.r.RCodec.encode(a)))
        .flatMap(v => okay.r.RCodec.decode[B](v))
        .left.map(c => Batcher.Failed(c.kind, c.message))

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

object Frames:
  given py: Frames[okay.py.PyModule] = py("python3")
  def py(python: String): Frames[okay.py.PyModule] = new:
    def name = s"py:$python"
    def frame(module: okay.py.PyModule, fn: String)(in: Table): Either[Batcher.Failed, Table] =
      val pool = PyPool.of(module, python, Stage.Workers)
      // a column PyFrame cannot say (a decimal, a timestamp) is refused by
      // name here, before the wire — `ArrowFrames.frame` throws for it
      val sent = try Right(okay.py.ArrowFrames.frame(in))
        catch case e: IllegalStateException => Left(Batcher.Failed("Frame", Option(e.getMessage).getOrElse("")))
      sent.flatMap(f => PyPool.frame(pool, python, s"${module.name}:$fn", f, Vector.empty)
        .left.map(c => Batcher.Failed(c.kind, c.message)))
        .flatMap(f => okay.py.ArrowFrames.table(f).left.map(m => Batcher.Failed("Frame", m)))

  given r: Frames[okay.r.RModule] = r("Rscript")
  def r(rscript: String): Frames[okay.r.RModule] = new:
    def name = s"r:$rscript"
    def frame(module: okay.r.RModule, fn: String)(in: Table): Either[Batcher.Failed, Table] =
      val pool = RPool.of(module, rscript, Stage.Workers)
      val sent = try Right(okay.r.RArrowFrames.frame(in))
        catch case e: IllegalStateException => Left(Batcher.Failed("Frame", Option(e.getMessage).getOrElse("")))
      sent.flatMap(f => RPool.frame(pool, rscript, s"${module.name}:$fn", f, Vector.empty)
        .left.map(c => Batcher.Failed(c.kind, c.message)))
        .flatMap(f => okay.r.RArrowFrames.table(f).left.map(m => Batcher.Failed("Frame", m)))

  /** the JVM: the table by reference, the function as registered */
  given jvm: Frames[JvmModule] = new:
    def name = "jvm"
    def frame(module: JvmModule, fn: String)(in: Table): Either[Batcher.Failed, Table] =
      module.framer(fn).flatMap(_(in))

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
    f.frame(module, fn)(Rows.table(rows)).flatMap(t => Rows.rows[B](t).left.map(m => Batcher.Failed("Frame", m)))

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

  given py: Speaks[okay.py.PyModule] = py("python3")
  def py(python: String): Speaks[okay.py.PyModule] = new:
    def speaks(module: okay.py.PyModule): Report =
      val pool = PyPool.of(module, python, Stage.Workers)
      val wire = pool.use(w => (w.wire, false))
      // Python's lambdas are values, so a program's continuation can be
      // resumed twice (specs/remote-foreign.md)
      Report("python", "pipes", if wire.endsWith("+arrow") then "arrow" else "columnar-json", stream = false, "multi-shot")

  given r: Speaks[okay.r.RModule] = r("Rscript")
  def r(rscript: String): Speaks[okay.r.RModule] = new:
    def speaks(module: okay.r.RModule): Report =
      val pool = RPool.of(module, rscript, Stage.Workers)
      val wire = pool.use(w => (w.wire, false))
      Report("r", "pipes", if wire.endsWith("+arrow") then "arrow" else "columnar-json", stream = false, "multi-shot")

  given jvm: Speaks[JvmModule] = new:
    def speaks(module: JvmModule): Report = Report("jvm", "in-jvm", "by-reference", stream = false, "in-jvm")
