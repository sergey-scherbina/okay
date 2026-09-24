package okay

import scala.annotation.tailrec
import okay.!.*
import okay.Row.plus

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
 * `Unit ! Writer % W + Stop` cannot infer `W` from a type-lambda row,
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

  /** Python's `yield from`: every told w replaced by f(w)'s tells —
   * fused, the inner generator's chain is read from the reader's
   * current state inside `add` (gen-flatmap-fusion); a `Stop` inside
   * it ends the whole generation, as the spliced program's would */
  def flatMap[V](f: W => Gen[V]): Gen[V] = new Gen(chain.andThen(Gen.Xf.FlatMap(f)))
  /** sequencing — one after the other: a `Cat` node, the reader's
   * state threaded from the left side into the right */
  def ++(h: Gen[W]): Gen[W] = new Gen(Gen.Chain.Cat(chain, h.chain, Gen.Xf.Id[W]()))
  def zipWithIndex: Gen[(W, Int)] = new Gen(chain.andThen(Gen.Xf.Indexed[W]()))

  /**
   * STRYMONAS'S HARD CASE (specs/strymonas-zip-fusion.md; Kiselyov,
   * Biboudis, Palladinos & Smaragdakis, "Stream fusion, to
   * completeness", POPL 2017): zip two generators element by element,
   * stopping when either ends. Not an `Xf` stage — a stage transforms
   * ONE chain's own elements; zipping needs both sides' NEXT tell at
   * once, which is pulled directly from each side's `program` (see
   * `Gen.zipping`'s doc for why that reaches through a `flatMap` with
   * no special case). Fused with whatever built either side: no `Gen`
   * wrapper per pair, one tell per pair, and everything chained AFTER
   * `zip` gets the same `Xf` fusion any other source does.
   */
  def zip[V](other: Gen[V]): Gen[(W, V)] = Gen.fromProgram(Gen.zipping(program, other.program))
  /** `zip`, then apply f to each pair in the same pass */
  def zipWith[V, U](other: Gen[V])(f: (W, V) => U): Gen[U] = zip(other).map(f.tupled)

  // ---- readers: ONE walk of the source, the chain applied per
  // element; every one stops the body where it has read enough
  /** the general stopping reader (specs/fold-until.md): `done` is
   * asked before the first element and after each; a `Stop` in the
   * body ends the read as the body's end would */
  def foldUntil[S, R](using K: FoldUntil[W, S, R]): R = K.end(chain.readState(K)(K.init).s)
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


  /** a read's answer: the reader's state where the walk ended, and
   * whether a `Stop` ended it — a concatenation's right side and a
   * flatMap's outer read must not go on after one */
  final class Halt[S](val s: S, val stopped: Boolean)

  /**
   * A source and the stages to read it through
   * (specs/gen-chain-fusion.md). The source's element type is an
   * existential the value class cannot name; a type member names it
   * once. Two node kinds: a program (`Plain`/`Staged`) and a
   * concatenation of two chains (`Cat`, gen-flatmap-fusion) — each
   * read FROM a downstream state, so a side or an inner generator is
   * read where the reader stands.
   */
  abstract class Chain[W]:
    type A
    val xf: Xf[A, W]
    /** the raw source(s) through a reader at A, from a state */
    def readSource[S, R](K: FoldUntil[A, S, R])(s0: S): Halt[S]
    /** the whole chain from a downstream state: the stages' own state
     * wrapped around it, the walk, and the reader's state back out */
    final def readState[S, R](K: FoldUntil[W, S, R])(s0: S): Halt[S] =
      val h = readSource(xf.fold(K))(xf.inject(s0))
      Halt(xf.project(h.s), h.stopped)
    /** the chain as a program: a plain chain IS its source (no node —
     * `say(w)` sits on every walk's hot path); a staged one is its
     * walks, built when first read */
    def program: Unit ! Row[W]
    def andThen[V](s: Xf[W, V]): Chain[V]

  object Chain:
    def apply[W](p: Unit ! Row[W]): Chain[W] = Plain(p)

    final class Plain[W](val source: Unit ! Row[W]) extends Chain[W]:
      type A = W
      val xf: Xf[W, W] = Xf.Id[W]()
      def readSource[S, R](K: FoldUntil[W, S, R])(s0: S): Halt[S] = Gen.readState(source)(K)(s0)
      def program: Unit ! Row[W] = source
      def andThen[V](s: Xf[W, V]): Chain[V] = Staged(source, s)

    final class Staged[A0, W](val source: Unit ! Row[A0], val xf: Xf[A0, W]) extends Chain[W]:
      type A = A0
      def readSource[S, R](K: FoldUntil[A0, S, R])(s0: S): Halt[S] = Gen.readState(source)(K)(s0)
      def program: Unit ! Row[W] = Free.delay(() => xf.walk(source))
      def andThen[V](s: Xf[W, V]): Chain[V] = Staged(source, xf.andThen(s))

    /** one after the other; `xf` applies AFTER both, its state
     * threaded across — a `take` over a concatenation counts through
     * it, each side's own stages count their own */
    final class Cat[A0, W](val l: Chain[A0], val r: Chain[A0], val xf: Xf[A0, W]) extends Chain[W]:
      type A = A0
      def readSource[S, R](K: FoldUntil[A0, S, R])(s0: S): Halt[S] =
        val h = l.readState(K)(s0)
        if h.stopped || K.done(h.s) then h else r.readState(K)(h.s)
      def program: Unit ! Row[W] = Free.delay(() => xf.walk(l.program.flatMap(_ => r.program)))
      def andThen[V](s: Xf[W, V]): Chain[V] = Cat(l, r, xf.andThen(s))

  /**
   * A stage with two readings: FUSED — a transformer of the reader,
   * Clojure's transducer with the state type it adds carried as `St`
   * so the fused reader is a `FoldUntil` at a known state type — and
   * MATERIALISED, the walk it was (for `program`). Stateless stages
   * keep `St[S] = S`. `inject`/`project` wrap and unwrap that state,
   * so a chain can be read FROM a downstream state (a concatenation's
   * side, a flatMap's inner generator).
   */
  sealed trait Xf[A, B]:
    type St[S]
    def fold[S, R](k: FoldUntil[B, S, R]): FoldUntil[A, St[S], R]
    def inject[S](s: S): St[S]
    def project[S](st: St[S]): S
    def walk(p: Unit ! Row[A]): Unit ! Row[B]
    def andThen[C](that: Xf[B, C]): Xf[A, C] = Xf.Compose(this, that)

  object Xf:
    /** a stage that adds no state */
    sealed trait Stateless[A, B] extends Xf[A, B]:
      type St[S] = S
      def inject[S](s: S): S = s
      def project[S](st: S): S = st

    /** a count beside the reader's state — a class, not a tuple, so
     * the count is a field and never boxed (a `(Int, S)` per element
     * measured at +40 B) */
    final class Counted[S](val n: Int, val s: S)

    /** a stage whose state is a count, fresh at zero when injected */
    sealed trait Counting[A, B] extends Xf[A, B]:
      type St[S] = Counted[S]
      def inject[S](s: S): Counted[S] = Counted(0, s)
      def project[S](st: Counted[S]): S = st.s

    final class Id[A] extends Stateless[A, A]:
      def fold[S, R](k: FoldUntil[A, S, R]): FoldUntil[A, S, R] = k
      def walk(p: Unit ! Row[A]): Unit ! Row[A] = p
      override def andThen[C](that: Xf[A, C]): Xf[A, C] = that

    final class Map[A, B](f: A => B) extends Stateless[A, B]:
      def fold[S, R](k: FoldUntil[B, S, R]): FoldUntil[A, S, R] = new:
        def init: S = k.init
        def add(s: S, a: A): S = k.add(s, f(a))
        def done(s: S): Boolean = k.done(s)
        def end(s: S): R = k.end(s)
      def walk(p: Unit ! Row[A]): Unit ! Row[B] = Writer.map[A, B, Unit, Stop](p)(f)

    final class Filter[A](p: A => Boolean) extends Stateless[A, A]:
      def fold[S, R](k: FoldUntil[A, S, R]): FoldUntil[A, S, R] = new:
        def init: S = k.init
        def add(s: S, a: A): S = if p(a) then k.add(s, a) else s
        def done(s: S): Boolean = k.done(s)
        def end(s: S): R = k.end(s)
      def walk(g: Unit ! Row[A]): Unit ! Row[A] = filtering(p)(g)

    /** `yield from`, fused: the inner generator's chain is read from
     * the reader's state inside `add`; n = 1 once an inner `Stop`
     * ended it, which ends the whole generation (the spliced program's
     * law: a `Stop` is one more member of the row) */
    final class FlatMap[A, B](f: A => Gen[B]) extends Counting[A, B]:
      def fold[S, R](k: FoldUntil[B, S, R]): FoldUntil[A, Counted[S], R] = new:
        def init: Counted[S] = Counted(0, k.init)
        def add(s: Counted[S], a: A): Counted[S] =
          val h = f(a).chain.readState(k)(s.s)
          Counted(if h.stopped then 1 else 0, h.s)
        def done(s: Counted[S]): Boolean = s.n == 1 || k.done(s.s)
        def end(s: Counted[S]): R = k.end(s.s)
      def walk(p: Unit ! Row[A]): Unit ! Row[B] = splice(p)(w => f(w).program)

    final class Indexed[A] extends Counting[A, (A, Int)]:
      def fold[S, R](k: FoldUntil[(A, Int), S, R]): FoldUntil[A, Counted[S], R] = new:
        def init: Counted[S] = Counted(0, k.init)
        def add(s: Counted[S], a: A): Counted[S] = Counted(s.n + 1, k.add(s.s, (a, s.n)))
        def done(s: Counted[S]): Boolean = k.done(s.s)
        def end(s: Counted[S]): R = k.end(s.s)
      def walk(g: Unit ! Row[A]): Unit ! Row[(A, Int)] = indexed(g)

    final class Take[A](n: Int) extends Counting[A, A]:
      def fold[S, R](k: FoldUntil[A, S, R]): FoldUntil[A, Counted[S], R] = new:
        def init: Counted[S] = Counted(0, k.init)
        def add(s: Counted[S], a: A): Counted[S] = Counted(s.n + 1, k.add(s.s, a))
        def done(s: Counted[S]): Boolean = s.n >= n || k.done(s.s)
        def end(s: Counted[S]): R = k.end(s.s)
      def walk(g: Unit ! Row[A]): Unit ! Row[A] = taking(n)(g)

    final class TakeWhile[A](p: A => Boolean) extends Counting[A, A]:
      /** n = 1 once stopped */
      def fold[S, R](k: FoldUntil[A, S, R]): FoldUntil[A, Counted[S], R] = new:
        def init: Counted[S] = Counted(0, k.init)
        def add(s: Counted[S], a: A): Counted[S] = if p(a) then Counted(0, k.add(s.s, a)) else Counted(1, s.s)
        def done(s: Counted[S]): Boolean = s.n == 1 || k.done(s.s)
        def end(s: Counted[S]): R = k.end(s.s)
      def walk(g: Unit ! Row[A]): Unit ! Row[A] = takingWhile(p)(g)

    final class Drop[A](n: Int) extends Counting[A, A]:
      /** n = skipped so far */
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
      def inject[S](s: S): St[S] = a.inject(b.inject(s))
      def project[S](st: St[S]): S = b.project(a.project(st))
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
      case Return(_) => pure(())
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
      case Return(_) => pure(())
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
        case Return(_) => pure(())
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
      case Return(_) => pure(())
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
      case Return(_) => pure(())
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
        case Return(_) => pure(())
        case Inject(e) => split[Writer % W, Stop](e)
          { case Writer.Say(_) => pure(()) } { _ => ended[W] }
        case Bind(Inject(e), k) => split[Writer % W, Stop](e)
          { w0 => (w0: @unchecked) match
              case Writer.Say(_) => Free.delay(() => loop(n - 1)(k(()))) }
          { _ => ended[W] }
    loop(n)(g)

  /**
   * ONE STEP of a chain's underlying program, resumed as far as the
   * next tell: the value and the rest, or `None` at the end (`Return`
   * or `Stop`) — the same shape `Stepper.advance` holds in a `cont`
   * var, as a pure function instead. Works uniformly on ANY program
   * this file builds, `flatMap`'s spliced ones included: `.resume`
   * walks past however many `Delay`/`Bind` nodes a `splice`/`Cat`
   * needed to reach the next real tell, so a step through a fused
   * `flatMap` costs exactly what a step through a plain source does.
   * That uniformity is `zipping`'s whole trick — see its own doc.
   */
  private def pull[A](x: Unit ! Row[A]): Option[(A, Unit ! Row[A])] = (x.resume: @unchecked) match
    case Return(_) => None
    case Inject(e) => split[Writer % A, Stop](e)
      { case Writer.Say(w) => Some((w, pure(()))) } { _ => None }
    case Bind(Inject(e), k) => split[Writer % A, Stop](e)
      { w0 => (w0: @unchecked) match { case Writer.Say(w) => Some((w, k(()))) } }
      { _ => None }

  /**
   * `zip`, as a walk: pull one element from EACH side and pair them,
   * stopping the moment either ends. `pull` does not know or care
   * whether `pa`/`pb` came from `Gen.from`, a fused `flatMap`, a
   * `Cat`, or another `zip` — it resumes ONE STEP of whatever program
   * it is handed, which is exactly what makes THIS zip work through
   * a flatMap with no special case, the shape strymonas's paper
   * names as the hard one (a `zip` whose side was built by `flatMap`,
   * needing the fused inner loop's OWN next element without
   * materializing it first). The recursive call sits inside `say`'s
   * `flatMap`, so it costs a stack frame only in the DRIVER that
   * resumes this program — nothing here recurses at build time.
   */
  private[okay] def zipping[A, B](pa: Unit ! Row[A], pb: Unit ! Row[B]): Unit ! Row[(A, B)] =
    def loop(x: Unit ! Row[A], y: Unit ! Row[B]): Unit ! Row[(A, B)] =
      pull(x) match
        case None => ended[(A, B)]
        case Some((a, xRest)) => pull(y) match
          case None => ended[(A, B)]
          case Some((b, yRest)) => say((a, b)).flatMap(_ => loop(xRest, yRest))
    loop(pa, pb)

  /** the walk, from a state: where it ended and whether a `Stop` did */
  private[okay] def readState[W, S, R](g: Unit ! Row[W])(K: FoldUntil[W, S, R])(s0: S): Halt[S] =
    @tailrec def loop(s: S)(x: Unit ! Row[W]): Halt[S] =
      if K.done(s) then Halt(s, false)
      else (x.resume: @unchecked) match
        case Return(_) => Halt(s, false)
        case Inject(e) => split[Writer % W, Stop](e)
          { case Writer.Say(w) => Halt(K.add(s, w), false) } { _ => Halt(s, true) }
        case Bind(Inject(e), k) => split[Writer % W, Stop](e)
          { w0 => (w0: @unchecked) match
              case Writer.Say(w) =>
                // `done` BEFORE `k` — an argument is evaluated before the
                // call, and `k(())` runs the body between this yield and
                // the next; a reader that has read enough must not
                val s2 = K.add(s, w)
                if K.done(s2) then Halt(s2, false) else loop(s2)(k(())) }
          { _ => Halt(s, true) }
    loop(s0)(g)

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
          case Return(_) => ended = true
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
