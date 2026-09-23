package okay

import scala.annotation.tailrec
import okay.!.*
import okay.RowLift.plus

/**
 * The early end of a generation (specs/generators.md): `Gen.stop`
 * performs it, and every reader of a `Gen` ends there — the rest of
 * the body is never run. Python's `return` inside a generator.
 */
enum Stop[+A] derives Effect:
  case Now extends Stop[Nothing]

/**
 * A GENERATOR IS A PROGRAM THAT TELLS.
 *
 * Python's `yield` hands a value to whoever asked and does not run
 * again until asked again; the body ends when it returns; the reader
 * may stop asking at any point. Every piece of that is what a
 * `Writer` program already is: `Bind(Inject(Say(w)), k)` suspends
 * until a reader calls `k` (nothing past the tell exists until then —
 * the continuation is a function), a `Pure` is the end, and a reader
 * that does not call `k` has stopped the body. okay-stream's
 * `Source[W]` is this plus `Async`; `Gen[W]` is the synchronous one,
 * with `Stop` beside `Writer` so that a body can end itself from the
 * middle of a loop.
 *
 * What this file adds is the vocabulary: element-wise `map`/`flatMap`/
 * `withFilter`/`take`… so that a plain for-comprehension over a `Gen`
 * builds a generator with no macro at all; readers that STOP —
 * `first`, `find`, `take(n).toList` — through `FoldUntil`
 * (specs/fold-until.md); and `iterator`, the Python semantics made
 * literal: `next()` runs the body to its next tell and holds the
 * continuation for the call after. A `Gen` is a value, so reading it
 * twice runs the body twice (Python's does not; `toLazyList` is the
 * memoising view).
 *
 * The literature: Kiselyov, Peyton Jones & Sabry, *Lazy v. Yield:
 * Incremental, Linear Pretty-printing* (APLAS 2012) — yield as a
 * delimited-control effect, exactly the tell; James & Sabry, *Yield:
 * Mainstream Delimited Continuations* (TPDC 2011) — generators are
 * the delimited continuation programmers already use; PEP 255.
 */
/**
 * The generator, a VALUE CLASS over the program that tells — so that
 * `map`/`flatMap`/`withFilter` are MEMBERS: an extension on the alias
 * `Unit ! (Writer % W + Stop)` cannot infer `W` from a type-lambda row,
 * and an extension on an opaque type lost to the package's generic
 * `map` over `Id` in lexical scope (`y` in a for-comprehension typed as
 * the whole generator). A member beats both, and `AnyVal` costs no
 * allocation. `program` is the same value with its own name back.
 */
final class Gen[W](val chain: Gen.Chain[W]) extends AnyVal:
  /** the chain MATERIALISED as the walks it stands for, built when
   * first read (a `Delay`): what the roads that need a PROGRAM read —
   * `iterator`, the barriers below, a `Gen` marked in a block */
  def program: Unit ! Gen.Row[W] = chain.program

  // ---- element-wise: a STAGE appended (specs/gen-chain-fusion.md),
  // nothing walked — the readers apply the chain per element
  def map[V](f: W => V): Gen[V] = new Gen(chain.andThen(Gen.Xf.Map(f)))
  def filter(p: W => Boolean): Gen[W] = new Gen(chain.andThen(Gen.Xf.Filter(p)))
  def withFilter(p: W => Boolean): Gen[W] = filter(p)
  /** the first n — a fused read is done at the n-th kept element and
   * the body runs no further; materialised, `taking` drops the
   * continuation after the n-th tell */
  def take(n: Int): Gen[W] = new Gen(chain.andThen(Gen.Xf.Take(n)))
  def takeWhile(p: W => Boolean): Gen[W] = new Gen(chain.andThen(Gen.Xf.TakeWhile(p)))
  /** skip n: the skipped steps run (they must — the body is between
   * the tells) */
  def drop(n: Int): Gen[W] = new Gen(chain.andThen(Gen.Xf.Drop(n)))

  // ---- barriers: a new source from the materialised program
  /** Python's `yield from`: every told w replaced by f(w)'s tells */
  def flatMap[V](f: W => Gen[V]): Gen[V] = Gen.lazily(Gen.splice(program)(w => f(w).program))
  /** sequencing — one after the other */
  def ++(h: Gen[W]): Gen[W] = Gen.fromProgram(program.flatMap(_ => h.program))
  def zipWithIndex: Gen[(W, Int)] = Gen.lazily(Gen.indexed(program))

  // ---- readers: ONE walk of the source, the chain applied per
  // element; every one stops the body where it has read enough
  /** the general stopping reader (specs/fold-until.md): `done` is
   * asked before the first element and after each; a `Stop` in the
   * body ends the read as the body's end would */
  def foldUntil[S, R](using K: FoldUntil[W, S, R]): R = Gen.read(chain.source)(chain.xf.fold(K))
  def toList: List[W] = foldUntil(using Gen.collecting[W])
  def toVector: Vector[W] = toList.toVector
  def first: Option[W] = foldUntil(using FoldUntil.headOption[W])
  def find(p: W => Boolean): Option[W] = foldUntil(using FoldUntil.find(p))
  def exists(p: W => Boolean): Boolean = foldUntil(using FoldUntil.exists(p))
  def forall(p: W => Boolean): Boolean = foldUntil(using FoldUntil.forall(p))
  def foreach(f: W => Unit): Unit = foldUntil(using Gen.each(f))
  /** THE PYTHON ONE: `next()` runs the body to its next tell and
   * hands the value over; the continuation is held and applied only
   * by the call after, so the code between two yields runs when the
   * second is asked for, not when the first is delivered. Reading
   * again is a fresh walk of the same program. Over `program`: the
   * stepper's contract is the walks' (Decisions) */
  def iterator: Iterator[W] = new Gen.Stepper[W](program)
  /** memoising, lazy — read it twice and the body ran once */
  def toLazyList: LazyList[W] = LazyList.from(iterator)

object Gen:
  type Row[W] = Writer % W + Stop

  /** a program that tells, as a generator — the same value, named */
  def fromProgram[W](p: Unit ! Row[W]): Gen[W] = new Gen(Chain(p))

  /** a transformed generator, built when first READ: every walk below
   * resumes its input's head, which would run the body's first step at
   * construction — Python runs nothing before `next()`, and neither
   * does this (a `Delay`, which the runners force in constant stack) */
  private def lazily[W](p: => Unit ! Row[W]): Gen[W] = new Gen(Chain(Free.delay(() => p)))

  /**
   * A source program and the stages to read it through
   * (specs/gen-chain-fusion.md). The source's element type is an
   * existential the value class cannot name; a type member names it
   * once, and every reader is `read(source)(xf.fold(K))`.
   */
  abstract class Chain[W]:
    type A
    val source: Unit ! Row[A]
    val xf: Xf[A, W]
    /** the chain as a program: a plain chain IS its source (no node —
     * `say(w)` sits on every walk's hot path); a staged one
     * is its walks, built when first read */
    def program: Unit ! Row[W]
    def andThen[V](s: Xf[W, V]): Chain[V] =
      val self = this
      new Chain[V]:
        type A = self.A
        val source: Unit ! Row[A] = self.source
        val xf: Xf[A, V] = self.xf.andThen(s)
        def program: Unit ! Row[V] = Free.delay(() => xf.walk(source))

  object Chain:
    def apply[W](p: Unit ! Row[W]): Chain[W] = new Chain[W]:
      type A = W
      val source: Unit ! Row[W] = p
      val xf: Xf[W, W] = Xf.Id[W]()
      def program: Unit ! Row[W] = p

  /**
   * A stage with two readings: FUSED — a transformer of the reader,
   * Clojure's transducer with the state type it adds carried as `St`
   * so the fused reader is a `FoldUntil` at a known state type and
   * `read` is unchanged — and MATERIALISED, the walk it was (for
   * `program`). Stateless stages keep `St[S] = S`.
   */
  sealed trait Xf[A, B]:
    type St[S]
    def fold[S, R](k: FoldUntil[B, S, R]): FoldUntil[A, St[S], R]
    def walk(p: Unit ! Row[A]): Unit ! Row[B]
    def andThen[C](that: Xf[B, C]): Xf[A, C] = Xf.Compose(this, that)

  object Xf:
    final class Id[A] extends Xf[A, A]:
      type St[S] = S
      def fold[S, R](k: FoldUntil[A, S, R]): FoldUntil[A, S, R] = k
      def walk(p: Unit ! Row[A]): Unit ! Row[A] = p
      override def andThen[C](that: Xf[A, C]): Xf[A, C] = that

    final class Map[A, B](f: A => B) extends Xf[A, B]:
      type St[S] = S
      def fold[S, R](k: FoldUntil[B, S, R]): FoldUntil[A, S, R] = new:
        def init: S = k.init
        def add(s: S, a: A): S = k.add(s, f(a))
        def done(s: S): Boolean = k.done(s)
        def end(s: S): R = k.end(s)
      def walk(p: Unit ! Row[A]): Unit ! Row[B] = Writer.map[A, B, Unit, Stop](p)(f)

    final class Filter[A](p: A => Boolean) extends Xf[A, A]:
      type St[S] = S
      def fold[S, R](k: FoldUntil[A, S, R]): FoldUntil[A, S, R] = new:
        def init: S = k.init
        def add(s: S, a: A): S = if p(a) then k.add(s, a) else s
        def done(s: S): Boolean = k.done(s)
        def end(s: S): R = k.end(s)
      def walk(g: Unit ! Row[A]): Unit ! Row[A] = filtering(p)(g)

    /** a count beside the reader's state — a class, not a tuple, so
     * the count is a field and never boxed (a `(Int, S)` per element
     * measured at +40 B) */
    final class Counted[S](val n: Int, val s: S)

    final class Take[A](n: Int) extends Xf[A, A]:
      type St[S] = Counted[S]
      def fold[S, R](k: FoldUntil[A, S, R]): FoldUntil[A, Counted[S], R] = new:
        def init: Counted[S] = Counted(0, k.init)
        def add(s: Counted[S], a: A): Counted[S] = Counted(s.n + 1, k.add(s.s, a))
        def done(s: Counted[S]): Boolean = s.n >= n || k.done(s.s)
        def end(s: Counted[S]): R = k.end(s.s)
      def walk(g: Unit ! Row[A]): Unit ! Row[A] = taking(n)(g)

    final class TakeWhile[A](p: A => Boolean) extends Xf[A, A]:
      /** n = 1 once stopped */
      type St[S] = Counted[S]
      def fold[S, R](k: FoldUntil[A, S, R]): FoldUntil[A, Counted[S], R] = new:
        def init: Counted[S] = Counted(0, k.init)
        def add(s: Counted[S], a: A): Counted[S] = if p(a) then Counted(0, k.add(s.s, a)) else Counted(1, s.s)
        def done(s: Counted[S]): Boolean = s.n == 1 || k.done(s.s)
        def end(s: Counted[S]): R = k.end(s.s)
      def walk(g: Unit ! Row[A]): Unit ! Row[A] = takingWhile(p)(g)

    final class Drop[A](n: Int) extends Xf[A, A]:
      /** n = skipped so far */
      type St[S] = Counted[S]
      def fold[S, R](k: FoldUntil[A, S, R]): FoldUntil[A, Counted[S], R] = new:
        def init: Counted[S] = Counted(0, k.init)
        def add(s: Counted[S], a: A): Counted[S] =
          if s.n < n then Counted(s.n + 1, s.s) else Counted(s.n, k.add(s.s, a))
        // the reader's `done` decides alone: done before anything was
        // dropped means nothing needs reading at all
        def done(s: Counted[S]): Boolean = k.done(s.s)
        def end(s: Counted[S]): R = k.end(s.s)
      def walk(g: Unit ! Row[A]): Unit ! Row[A] = dropping(n)(g)

    final class Compose[A, B, C](val a: Xf[A, B], val b: Xf[B, C]) extends Xf[A, C]:
      type St[S] = a.St[b.St[S]]
      def fold[S, R](k: FoldUntil[C, S, R]): FoldUntil[A, St[S], R] = a.fold(b.fold(k))
      def walk(p: Unit ! Row[A]): Unit ! Row[C] = b.walk(a.walk(p))

  /** yield one value — a tell. Sequence with `++`; `flatMap` is
   * element-wise (Python's `yield from`) */
  def emit[W](w: W): Gen[W] = fromProgram(say(w))

  /** end the generation here: nothing after it runs, in a loop or not */
  def stop[W]: Gen[W] = fromProgram(ended[W])

  /** the tell and the end as PROGRAMS, for the walks below: a `Gen`
   * per element on a walk's hot path is a wrapper, a chain and a
   * node the reader never asked for (measured: +96 B per element) */
  private def say[W](w: W): Unit ! Row[W] = effect[Row[W], Unit](Writer(w))
  private def ended[W]: Unit ! Row[W] = effect[Row[W], Unit](Stop.Now)

  def empty[W]: Gen[W] = fromProgram(pure(()))

  def apply[W](ws: W*): Gen[W] = from(ws)

  /** the elements of a collection, one tell each — nothing is done at
   * construction (a `Delay`), and an Iterator argument is read once
   * into a memoising LazyList so the Gen stays re-runnable */
  def from[W](it: IterableOnce[W]): Gen[W] =
    def go(rest: LazyList[W]): Unit ! Row[W] = rest match
      case h #:: tl => say(h).flatMap(_ => go(tl))
      case _ => pure(())
    fromProgram(Free.delay(() => go(it.iterator.to(LazyList))))

  /** the producing side's `loop`: a state that decides when it ends */
  def unfold[S, W](s: S)(f: S => Option[(W, S)]): Gen[W] =
    def go(s: S): Unit ! Row[W] = Free.delay(() => f(s) match
      case Some((w, s2)) => say(w).flatMap(_ => go(s2))
      case None => pure(()))
    fromProgram(go(s))

  /** a plain Writer program as a generator — the row widened by `Stop` */
  def of[W](p: Unit ! Writer % W): Gen[W] = fromProgram(p.plus[Stop])

  // ---- the walks, over programs

  /**
   * Every told `w` replaced by `f(w)`'s tells, in place — `relay`'s
   * shape with a program-valued answer, one pass, the continuation
   * called only when `f(w)` has been read through (laziness kept).
   * A `Stop` drops the continuation: the end.
   */
  private[okay] def splice[W, V](g: Unit ! Row[W])(f: W => Unit ! Row[V]): Unit ! Row[V] =
    def loop(x: Unit ! Row[W]): Unit ! Row[V] = (x.resume: @unchecked) match
      case Pure(_) => pure(())
      case Inject(e) => split[Writer % W, Stop](e)
        { case Writer.Say(w) => f(w) }
        { _ => ended[V] }
      case Bind(Inject(e), k) => split[Writer % W, Stop](e)
        { w0 => (w0: @unchecked) match
            case Writer.Say(w) => f(w).flatMap(_ => loop(k(()))) }
        { _ => ended[V] }
    loop(g)

  private def indexed[W](g: Unit ! Row[W]): Unit ! Row[(W, Int)] =
    def loop(i: Int)(x: Unit ! Row[W]): Unit ! Row[(W, Int)] = (x.resume: @unchecked) match
      case Pure(_) => pure(())
      case Inject(e) => split[Writer % W, Stop](e)
        { case Writer.Say(w) => say((w, i)) } { _ => ended[(W, Int)] }
      case Bind(Inject(e), k) => split[Writer % W, Stop](e)
        { w0 => (w0: @unchecked) match
            case Writer.Say(w) => say((w, i)).flatMap(_ => loop(i + 1)(k(()))) }
        { _ => ended[(W, Int)] }
    loop(0)(g)

  private def taking[W](n: Int)(g: Unit ! Row[W]): Unit ! Row[W] =
    def loop(n: Int)(x: Unit ! Row[W]): Unit ! Row[W] =
      if n <= 0 then pure(())
      else (x.resume: @unchecked) match
        case Pure(_) => pure(())
        case Inject(e) => Inject(e)
        case Bind(Inject(e), k) => split[Writer % W, Stop](e)
          { w0 => (w0: @unchecked) match
              case Writer.Say(w) => if n == 1 then say(w) else say(w).flatMap(_ => loop(n - 1)(k(()))) }
          { _ => ended[W] }
    loop(n)(g)

  /** keep the tells `p` accepts: the accepted tell is the input's OWN
   * `Inject` node bound to the rest of the walk; a rejected one is
   * skipped by a `Free.delay` so a long run of rejections is flat
   * (`dropping`'s reason). Laziness kept: `k` runs only when the next
   * value is asked for, as the reader drives */
  private def filtering[W](p: W => Boolean)(g: Unit ! Row[W]): Unit ! Row[W] =
    // the rejected tell is a DEFERRED skip, one `Delay` per rejection:
    // recursing straight through a run of rejections (a budget of 64,
    // then a Delay) allocated less — 330 against 354 B per element —
    // and read 7% SLOWER: the runner's trampoline beats a chain of
    // calls through `split`'s closure (gen-filter-as-walk, measured)
    def loop(x: Unit ! Row[W]): Unit ! Row[W] = (x.resume: @unchecked) match
      case Pure(_) => pure(())
      case i @ Inject(e) => split[Writer % W, Stop](e)
        { case Writer.Say(w) => if p(w) then i else pure(()) } { _ => ended[W] }
      case Bind(i @ Inject(e), k) => split[Writer % W, Stop](e)
        { w0 => (w0: @unchecked) match
            case Writer.Say(w) =>
              if p(w) then Bind(i, (_: Any) => loop(k(())))
              else Free.delay(() => loop(k(()))) }
        { _ => ended[W] }
    loop(g)

  private def takingWhile[W](p: W => Boolean)(g: Unit ! Row[W]): Unit ! Row[W] =
    def loop(x: Unit ! Row[W]): Unit ! Row[W] = (x.resume: @unchecked) match
      case Pure(_) => pure(())
      case Inject(e) => split[Writer % W, Stop](e)
        { case Writer.Say(w) => if p(w) then say(w) else pure(()) } { _ => ended[W] }
      case Bind(Inject(e), k) => split[Writer % W, Stop](e)
        { w0 => (w0: @unchecked) match
            case Writer.Say(w) => if p(w) then say(w).flatMap(_ => loop(k(()))) else pure(()) }
        { _ => ended[W] }
    loop(g)

  private def dropping[W](n: Int)(g: Unit ! Row[W]): Unit ! Row[W] =
    def loop(n: Int)(x: Unit ! Row[W]): Unit ! Row[W] =
      if n <= 0 then x
      else (x.resume: @unchecked) match
        case Pure(_) => pure(())
        case Inject(e) => split[Writer % W, Stop](e)
          { case Writer.Say(_) => pure(()) } { _ => ended[W] }
        case Bind(Inject(e), k) => split[Writer % W, Stop](e)
          { w0 => (w0: @unchecked) match
              case Writer.Say(_) => Free.delay(() => loop(n - 1)(k(()))) }
          { _ => ended[W] }
    loop(n)(g)

  private def read[W, S, R](g: Unit ! Row[W])(K: FoldUntil[W, S, R]): R =
    @tailrec def loop(s: S)(x: Unit ! Row[W]): R =
      if K.done(s) then K.end(s)
      else (x.resume: @unchecked) match
        case Pure(_) => K.end(s)
        case Inject(e) => split[Writer % W, Stop](e)
          { case Writer.Say(w) => K.end(K.add(s, w)) } { _ => K.end(s) }
        case Bind(Inject(e), k) => split[Writer % W, Stop](e)
          { w0 => (w0: @unchecked) match
              case Writer.Say(w) =>
                // `done` BEFORE `k` — an argument is evaluated before the
                // call, and `k(())` runs the body between this yield and
                // the next; a reader that has read enough must not
                val s2 = K.add(s, w)
                if K.done(s2) then K.end(s2) else loop(s2)(k(())) }
          { _ => K.end(s) }
    loop(K.init)(g)

  /** collect everything told, never done */
  private def collecting[W]: FoldUntil[W, List[W], List[W]] = new:
    def init: List[W] = Nil
    def add(s: List[W], a: W): List[W] = a :: s
    def done(s: List[W]): Boolean = false
    def end(s: List[W]): List[W] = s.reverse

  private def each[W](f: W => Unit): FoldUntil[W, Unit, Unit] = new:
    def init: Unit = ()
    def add(s: Unit, a: W): Unit = f(a)
    def done(s: Unit): Boolean = false
    def end(s: Unit): Unit = ()

  private final class Stepper[W](start: Unit ! Row[W]) extends Iterator[W]:
    private var rest: Unit ! Row[W] = start
    private var cont: (Unit => Unit ! Row[W]) | Null = null
    private var head: W | Null = null
    private var has = false
    private var ended = false

    private def advance(): Unit =
      if !has && !ended then
        // the continuation held from the previous element is applied
        // NOW — this is where the body between two yields runs
        val x: Unit ! Row[W] = if cont != null then { val k = cont.nn; cont = null; k(()) } else rest
        (x.resume: @unchecked) match
          case Pure(_) => ended = true
          case Inject(e) => split[Writer % W, Stop](e)
            { case Writer.Say(w) => head = w; has = true; rest = pure(()) } { _ => ended = true }
          case Bind(Inject(e), k) => split[Writer % W, Stop](e)
            { w0 => (w0: @unchecked) match
                case Writer.Say(w) => head = w; has = true; cont = k }
            { _ => ended = true }

    def hasNext: Boolean = { advance(); has }
    def next(): W =
      advance()
      if !has then throw new NoSuchElementException("next on an exhausted generator")
      has = false
      head.nn
