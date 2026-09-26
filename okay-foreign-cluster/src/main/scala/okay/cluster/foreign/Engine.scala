package okay.cluster.foreign

import okay.codec.Schema

/**
 * ONE API OVER EVERY LANGUAGE (specs/foreign-map-reduce.md, stage 3): the
 * engine that runs a module's functions is a TYPECLASS by the module's
 * type — a BASE (`Engine`: map) and EXTENSIONS each its own typeclass
 * (`Reduces`), every one implemented optionally — so the job's code names
 * a module and a function and nothing else —
 * `flow.mapIn[Out](module, "double")`, `Reduce.in[Rec, Stat](module,
 * "step", "merge")` — and the implicit says whether that is Python, R or
 * the JVM itself. Which interpreter runs is a given too
 * (`Engine.py("/venv/bin/python3")`), so a job moves between languages and
 * interpreters with an import, never an edit.
 *
 * CONTRAVARIANT, and that is what makes `mapIn[Out](module, …)` write
 * without naming the module's type: the extension asks for
 * `Engine[module.type]`, and `Engine[PyModule]` is one, since
 * `module.type <: PyModule`. A test's own engine over its own module type
 * is a given like any other.
 */
trait Engine[-M]:
  def name: String
  /** the BASE: a named function of the module over a chunk of rows */
  def batcher[A: Schema, B: Schema](module: M, fn: String, workers: Int): Batcher[A, B]

/**
 * AN EXTENSION, its own typeclass (the operator's shape, stage 3): what a
 * module CAN do beyond the base is a separate instance, implemented where
 * the language supports it and absent where it does not — so a job that
 * reduces asks for `Reduces[M]` and fails to COMPILE on a module type that
 * has none, while `mapIn` on the same module still works. The facade
 * assembles a working job out of whatever instances the module has.
 */
trait Reduces[-M]:
  def reducer[A: Schema, Acc: Schema](module: M, step: String, merge: String, workers: Int): Reducer[A, Acc]

object Engine:
  /** Python, `python3` on the PATH; `Engine.py(path)` for another */
  given py: Engine[okay.py.PyModule] = py("python3")

  def py(python: String): Engine[okay.py.PyModule] = new:
    def name = s"py:$python"
    def batcher[A: Schema, B: Schema](module: okay.py.PyModule, fn: String, workers: Int): Batcher[A, B] =
      ForeignStage[okay.py.PyModule, A, B](Language.py(python), module, fn, workers)

  /** R, `Rscript` on the PATH; `Engine.r(path)` for another */
  given r: Engine[okay.r.RModule] = r("Rscript")

  def r(rscript: String): Engine[okay.r.RModule] = new:
    def name = s"r:$rscript"
    def batcher[A: Schema, B: Schema](module: okay.r.RModule, fn: String, workers: Int): Batcher[A, B] =
      ForeignStage[okay.r.RModule, A, B](Language.r(rscript), module, fn, workers)

  /** the JVM's own languages — Scala, Clojure, Frege — as functions by
   * name in a `JvmModule`: no wire, no frame, no pool */
  given jvm: Engine[JvmModule] = new:
    def name = "jvm"
    def batcher[A: Schema, B: Schema](module: JvmModule, fn: String, workers: Int): Batcher[A, B] =
      module.batcher[A, B](fn)

/** the extension's instances live HERE, in its own companion, where the
 * implicit search for `Reduces[M]` looks — one per language, the
 * interpreter a given (`Reduces.py(path)`) as for `Engine` */
object Reduces:
  given py: Reduces[okay.py.PyModule] = py("python3")
  given r: Reduces[okay.r.RModule] = r("Rscript")
  given jvm: Reduces[JvmModule] = new:
    def reducer[A: Schema, Acc: Schema](module: JvmModule, step: String, merge: String, workers: Int): Reducer[A, Acc] =
      module.reducer[A, Acc](step, merge)
  def py(python: String): Reduces[okay.py.PyModule] = new:
    def reducer[A: Schema, Acc: Schema](module: okay.py.PyModule, step: String, merge: String, workers: Int): Reducer[A, Acc] =
      ForeignReducer[okay.py.PyModule, A, Acc](Language.py(python), module, step, merge, workers)
  def r(rscript: String): Reduces[okay.r.RModule] = new:
    def reducer[A: Schema, Acc: Schema](module: okay.r.RModule, step: String, merge: String, workers: Int): Reducer[A, Acc] =
      ForeignReducer[okay.r.RModule, A, Acc](Language.r(rscript), module, step, merge, workers)

/**
 * A module of the JVM's own — functions by name, the shape a `PyModule` or
 * an `RModule` has, so a job that names `("stats", "double")` reads the
 * same whether "stats" is Python, R or this. A Clojure `IFn` or a Frege
 * function is registered as the Scala function it is on the JVM.
 *
 * {{{
 * val stats = JvmModule("stats")
 *   .map[Rec, Out]("double")(rows => rows.map(r => Out(r.key, r.v * 2)))
 *   .reduce[Rec, Stat]("step", "merge")((acc, rows) => …, (a, b) => …)
 * }}}
 */
final class JvmModule private (val name: String, private val fns: Map[String, Any]):
  /** a batch function under `fn`: a chunk of `A` to rows of `B` */
  def map[A, B](fn: String)(f: Vector[A] => Vector[B]): JvmModule =
    val self = this
    new JvmModule(name, fns.updated(fn, new Batcher[A, B]:
      val name = s"jvm:${self.name}:$fn"
      def apply(rows: Vector[A]): Either[Batcher.Failed, Vector[B]] =
        try Right(f(rows))
        catch case e: Exception => Left(Batcher.Failed(e.getClass.getSimpleName, Option(e.getMessage).getOrElse(""))))
    )

  /** a reduce under `step`: its step over a chunk and its merge of partials;
   * `merge` names the pair for a job that reads like the Python one */
  def reduce[A, Acc](step: String, merge: String)(stepF: (Option[Acc], Vector[A]) => Acc, mergeF: (Acc, Acc) => Acc): JvmModule =
    val self = this
    new JvmModule(name, fns.updated(s"$step/$merge", new Reducer[A, Acc]:
      val name = s"jvm:${self.name}:$step/$merge"
      def step(acc: Option[Acc], rows: Vector[A]): Either[Batcher.Failed, Acc] =
        try Right(stepF(acc, rows))
        catch case e: Exception => Left(Batcher.Failed(e.getClass.getSimpleName, Option(e.getMessage).getOrElse("")))
      def merge(a: Acc, b: Acc): Either[Batcher.Failed, Acc] =
        try Right(mergeF(a, b))
        catch case e: Exception => Left(Batcher.Failed(e.getClass.getSimpleName, Option(e.getMessage).getOrElse("")))))

  /** an object made once from `params` under `fn`, and the maps that take
   * it: `mapWith[A, H, B](fn)((rows, held) => …)` */
  def model[P, H](fn: String)(f: P => H): JvmModule =
    new JvmModule(name, fns.updated(s"model:$fn", f))

  def mapWith[A, H, B](fn: String)(f: (Vector[A], H) => Vector[B]): JvmModule =
    new JvmModule(name, fns.updated(s"with:$fn", f))

  /** a stateful stage: `open` makes the partition's state, `step` folds a
   * chunk through it answering rows, `finish` flushes at the end */
  def stream[A, S0, B](openName: String, stepName: String, finishName: String)
                      (openF: () => S0, stepF: (S0, Vector[A]) => Vector[B], finishF: S0 => Vector[B]): JvmModule =
    val self = this
    new JvmModule(name, fns.updated(s"$openName/$stepName/$finishName", new Streamer[A, B]:
      val name = s"jvm:${self.name}:$openName/$stepName/$finishName"
      type S = S0
      private def guard[X](x: => X): Either[Batcher.Failed, X] =
        try Right(x)
        catch case e: Exception => Left(Batcher.Failed(e.getClass.getSimpleName, Option(e.getMessage).getOrElse("")))
      def open(): Either[Batcher.Failed, S] = guard(openF())
      def step(s: S, rows: Vector[A]): Either[Batcher.Failed, Vector[B]] = guard(stepF(s, rows))
      def finish(s: S): Either[Batcher.Failed, Vector[B]] = guard(finishF(s))
      // the JVM holds the state in the stage itself: nothing to give back
      def abandon(s: S): Unit = ()))

  private[foreign] def model[P](fn: String, params: P): Model =
    val self = this
    fns.get(s"model:$fn") match
      case Some(f: Function1[?, ?]) =>
        // once, here: the JVM has no interpreters to copy it into
        lazy val obj: Any = f.asInstanceOf[P => Any](params)
        new Model:
          val name = s"jvm:${self.name}:$fn"
          def batcher[A: Schema, B: Schema](mapFn: String): Batcher[A, B] =
            fns.get(s"with:$mapFn") match
              case Some(g: Function2[?, ?, ?]) => new Batcher[A, B]:
                val name = s"jvm:${self.name}:$mapFn($fn)"
                def apply(rows: Vector[A]): Either[Batcher.Failed, Vector[B]] =
                  try Right(g.asInstanceOf[(Vector[A], Any) => Vector[B]](rows, obj))
                  catch case e: Exception => Left(Batcher.Failed(e.getClass.getSimpleName, Option(e.getMessage).getOrElse("")))
              case _ => throw IllegalArgumentException(s"the JVM module '${self.name}' has no map-with function '$mapFn' (it has ${self.names})")
      case _ => throw IllegalArgumentException(s"the JVM module '$name' has no model function '$fn' (it has $names)")

  private[foreign] def streamer[A, B](open: String, step: String, finish: String): Streamer[A, B] =
    fns.get(s"$open/$step/$finish") match
      case Some(st: Streamer[?, ?]) => st.asInstanceOf[Streamer[A, B]]
      case _ => throw IllegalArgumentException(s"the JVM module '$name' has no stream '$open'/'$step'/'$finish' (it has $names)")

  private def names: String = fns.keys.toVector.sorted.mkString(", ")

  // the map is heterogeneous — one batcher type per name — and keyed by
  // the name the job gives, so the one cast the registry needs is here:
  // the types are the ones `map`/`reduce` registered under that name, and
  // a name never registered is refused, not guessed
  /** a function under `fn` for `Calls[JvmModule]`: one value to one value
   * (specs/foreign-facade.md, tier 1); a throw is a refusal by its class */
  def fn[A, B](fn: String)(f: A => B): JvmModule =
    val self = this
    new JvmModule(name, fns.updated(fn, new JvmModule.Caller[A, B]:
      val name = s"jvm:${self.name}:$fn"
      def apply(a: A): Either[Batcher.Failed, B] =
        try Right(f(a))
        catch case e: Exception => Left(Batcher.Failed(e.getClass.getSimpleName, Option(e.getMessage).getOrElse("")))))

  /** a missing name is a REFUSAL here, not a throw as `batcher`'s is:
   * `Calls` answers at call time on every road, and Python's
   * AttributeError comes back the same way */
  /** a frame function under `fn` for `Frames[JvmModule]`: a Table to a
   * Table, by reference — nothing crosses, nothing is copied */
  def frame(fn: String)(f: okay.arrow.Table => okay.arrow.Table): JvmModule =
    val self = this
    new JvmModule(name, fns.updated(fn, new JvmModule.Framer:
      val name = s"jvm:${self.name}:$fn"
      def apply(t: okay.arrow.Table): Either[Batcher.Failed, okay.arrow.Table] =
        try Right(f(t))
        catch case e: Exception => Left(Batcher.Failed(e.getClass.getSimpleName, Option(e.getMessage).getOrElse("")))))

  private[foreign] def framer(fn: String): Either[Batcher.Failed, JvmModule.Framer] =
    fns.get(fn) match
      case Some(f: JvmModule.Framer) => Right(f)
      case _ => Left(Batcher.Failed("NoSuchFunction", s"the JVM module '$name' has no frame function '$fn' (it has ${fns.keys.toVector.sorted.mkString(", ")})"))

  private[foreign] def caller[A, B](fn: String): Either[Batcher.Failed, JvmModule.Caller[A, B]] =
    fns.get(fn) match
      // the one cast, as `batcher`'s below: a map keyed by name holds
      // functions at their own types, and the name is what the caller
      // wrote beside the types it asks for
      case Some(c: JvmModule.Caller[?, ?]) => Right(c.asInstanceOf[JvmModule.Caller[A, B]])
      case _ => Left(Batcher.Failed("NoSuchFunction", s"the JVM module '$name' has no function '$fn' (it has ${fns.keys.toVector.sorted.mkString(", ")})"))

  private[foreign] def batcher[A, B](fn: String): Batcher[A, B] =
    fns.get(fn) match
      case Some(b: Batcher[?, ?]) => b.asInstanceOf[Batcher[A, B]]
      case _ => throw IllegalArgumentException(s"the JVM module '$name' has no map function '$fn' (it has ${fns.keys.toVector.sorted.mkString(", ")})")

  private[foreign] def reducer[A, Acc](step: String, merge: String): Reducer[A, Acc] =
    fns.get(s"$step/$merge") match
      case Some(r: Reducer[?, ?]) => r.asInstanceOf[Reducer[A, Acc]]
      case _ => throw IllegalArgumentException(s"the JVM module '$name' has no reduce '$step'/'$merge' (it has ${fns.keys.toVector.sorted.mkString(", ")})")

object JvmModule:
  def apply(name: String): JvmModule = new JvmModule(name, Map.empty)

  /** a table to a table, what `Frames[JvmModule]` runs */
  trait Framer:
    def name: String
    def apply(t: okay.arrow.Table): Either[Batcher.Failed, okay.arrow.Table]

  /** one value to one value, what `Calls[JvmModule]` runs */
  trait Caller[A, B]:
    def name: String
    def apply(a: A): Either[Batcher.Failed, B]
