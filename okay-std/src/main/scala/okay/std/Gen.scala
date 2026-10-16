package okay.std

import okay.{!, +, %, Effect, Run, defer, effect, pure}
import okay.FoldUntil
import okay.one.machine.Machine
import okay.one.machine.Machine.Head

import scala.annotation.tailrec

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
 * `Writer` program already is: stepped (okay-core's `Machine.Head`), a
 * tell comes out paused with its rest and suspends until a reader resumes it
 * (nothing past the tell runs until then), the program's value is the
 * end, and a reader that does not resume has stopped the body. okay-stream's
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
   * first read (a `defer`): what the roads that need a PROGRAM read —
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
      def program: Unit ! Row[W] = defer(xf.walk(source))
      def andThen[V](s: Xf[W, V]): Chain[V] = Staged(source, xf.andThen(s))

    /** one after the other; `xf` applies AFTER both, its state
     * threaded across — a `take` over a concatenation counts through
     * it, each side's own stages count their own */
    final class Cat[A0, W](val l: Chain[A0], val r: Chain[A0], val xf: Xf[A0, W]) extends Chain[W]:
      type A = A0
      def readSource[S, R](K: FoldUntil[A0, S, R])(s0: S): Halt[S] =
        val h = l.readState(K)(s0)
        if h.stopped || K.done(h.s) then h else r.readState(K)(h.s)
      def program: Unit ! Row[W] = defer(xf.walk(l.program.flatMap(_ => r.program)))
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
      def walk(p: Unit ! Row[A]): Unit ! Row[B] = Writer.map[A, B, Unit, Row[A]](p)(f)

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
   * node the reader never asked for (measured: +96 B per element).
   * Each one node, its operation at its place in the row's paths */
  private def say[W](w: W): Unit ! Row[W] = effect[Writer % W, Unit](Writer(w)).at[Row[W]]
  private def ended[W]: Unit ! Row[W] = effect[Stop, Unit](Stop.Now).at[Row[W]]

  def empty[W]: Gen[W] = fromProgram(pure(()))

  def apply[W](ws: W*): Gen[W] = from(ws)

  /** the elements of a collection, one tell each — nothing is done at
   * construction (a `defer`), and an Iterator argument is read once
   * into a memoising LazyList so the Gen stays re-runnable */
  def from[W](it: IterableOnce[W]): Gen[W] =
    def go(rest: LazyList[W]): Unit ! Row[W] = rest match
      case h #:: tl => say(h).flatMap(_ => go(tl))
      case _ => pure(())
    fromProgram(defer(go(it.iterator.to(LazyList))))

  /** the producing side's `loop`: a state that decides when it ends */
  def unfold[S, W](s: S)(f: S => Option[(W, S)]): Gen[W] =
    def go(s: S): Unit ! Row[W] = defer(f(s) match
      case Some((w, s2)) => say(w).flatMap(_ => go(s2))
      case None => pure(()))
    fromProgram(go(s))

  /** a plain Writer program as a generator — the row widened by `Stop` */
  def of[W](p: Unit ! Writer % W): Gen[W] = fromProgram(p.at[Row[W]])

  // ---- the walks, over programs

  /**
   * THE ROW A GENERATOR IS STEPPED AT (okay-core's `Machine.Head`): its
   * two effects and nothing else — a `Gen`'s program performs no other
   * — `Writer % W` first. `Machine.head(p)` runs the body to its next
   * operation and hands it over paused, one object, the rest the
   * machine's own: a tell, or the end. A paused tell a reader does not
   * resume is dropped — the body stopped there.
   */
  private type St[W] = Head[okay.one.row.Pure, Unit]
  /** a tell paused: resumed (with `()`) for the rest */
  private type At[W] = Stepping.Paused[Unit]

  /** the program from its start, stepped to its first operation */
  private def started[W](g: Unit ! Row[W]): St[W] = Machine.head(Run.kernel[Unit, Row[W], okay.one.row.Pure](g))

  /** the body stepped on: from its start (`at` null), or from the paused tell `at`, resumed */
  private def stepped[W](g: Unit ! Row[W], at: At[W] | Null): St[W] =
    if at == null then started(g) else Stepping.resume(at, ())

  /** what a step came to: a tell, paused — its rest resumed when asked for; the end; a `Stop` */
  private enum Next[+W]:
    case Tell(w: W, rest: At[W @scala.annotation.unchecked.uncheckedVariance])
    case End
    case Stopped

  private def nextOf[W](st: St[W]): Next[W] = st match
    case Head.Done(_) => Next.End
    case p: Head.Paused[?, ?, ?, ?, ?, ?, ?, ?, ?] =>
      if !Stepping.own(p.in, Telling.key) then Next.Stopped
      else p.op match
        case Writer.Say(w) => Next.Tell(w.asInstanceOf[W], p)

  /**
   * Every told `w` replaced by `f(w)`'s tells, in place — each tell
   * TRANSLATED (the kernel's `Translating`): `f(w)` run where the tell
   * was, the rest resumed when it has been read through (laziness
   * kept). A `Stop`, in the body or in an `f(w)`, is the place's own:
   * the end of the whole generation.
   */
  private[okay] def splice[W, V](g: Unit ! Row[W])(f: W => Unit ! Row[V]): Unit ! Row[V] =
    g.translate[Writer % W, Row[V]]([X] => (op: Writer[W, X]) => op match
      case Writer.Say(w) => f(w)).at[Row[V]]

  /** keep the tells `p` accepts — each tell translated into itself or
   * into nothing, answered in place where it was: no walk, no node
   * per rejection */
  private def filtering[W](p: W => Boolean)(g: Unit ! Row[W]): Unit ! Row[W] =
    g.translate[Writer % W, Writer % W]([X] => (op: Writer[W, X]) => op match
      case Writer.Say(w) => if p(w) then Writer.tell(w) else pure[Unit, Writer % W](())).at[Row[W]]

  /** the walks that keep a count or end early: the body STEPPED, each
   * tell told again, the rest stepped when the reader resumes past it */
  private def indexed[W](g: Unit ! Row[W]): Unit ! Row[(W, Int)] =
    def loop(i: Int, at: At[W] | Null): Unit ! Row[(W, Int)] = defer(nextOf[W](stepped(g, at)) match
      case Next.Tell(w, rest) => say((w, i)).flatMap(_ => loop(i + 1, rest))
      case Next.End => pure(())
      case Next.Stopped => ended[(W, Int)])
    loop(0, null)

  private def taking[W](n: Int)(g: Unit ! Row[W]): Unit ! Row[W] =
    def loop(n: Int, at: At[W] | Null): Unit ! Row[W] = defer(
      if n <= 0 then pure(())
      else nextOf[W](stepped(g, at)) match
        case Next.Tell(w, rest) => if n == 1 then say(w) else say(w).flatMap(_ => loop(n - 1, rest))
        case Next.End => pure(())
        case Next.Stopped => ended[W])
    loop(n, null)

  private def takingWhile[W](p: W => Boolean)(g: Unit ! Row[W]): Unit ! Row[W] =
    def loop(at: At[W] | Null): Unit ! Row[W] = defer(nextOf[W](stepped(g, at)) match
      case Next.Tell(w, rest) => if p(w) then say(w).flatMap(_ => loop(rest)) else pure(())
      case Next.End => pure(())
      case Next.Stopped => ended[W])
    loop(null)

  /** the first `n` tells skipped — stepped through in a loop, constant
   * stack — and the rest of the body as it is, from where it stands */
  private def dropping[W](n: Int)(g: Unit ! Row[W]): Unit ! Row[W] =
    @tailrec def skip(n: Int, st: St[W]): Unit ! Row[W] =
      if n <= 0 then rest(st)
      else nextOf[W](st) match
        case Next.Tell(_, r) => skip(n - 1, Stepping.resume(r, ()))
        case Next.End => pure(())
        case Next.Stopped => ended[W]
    def rest(st: St[W]): Unit ! Row[W] = nextOf[W](st) match
      case Next.Tell(w, r) => say(w).flatMap(_ => defer(rest(Stepping.resume(r, ()))))
      case Next.End => pure(())
      case Next.Stopped => ended[W]
    if n <= 0 then g else defer(skip(n, started(g)))

  /**
   * `zip`, as a walk: one element from EACH side, stepped, and the
   * pair told, stopping the moment either ends. A side is stepped
   * whatever built it — `Gen.from`, a `flatMap`'s translation, a
   * `Cat`, another `zip`: `Machine.head` runs ONE program to its next tell,
   * which is exactly what makes this zip work through a flatMap with
   * no special case, the shape strymonas's paper names as the hard one
   * (a `zip` whose side was built by `flatMap`, needing the fused inner
   * loop's OWN next element without materializing it first). The
   * recursive call sits inside `say`'s `flatMap` and a `defer`, so
   * nothing here recurses at build time, and both sides are stepped
   * when the zipped program runs, each time it does.
   */
  private[okay] def zipping[A, B](pa: Unit ! Row[A], pb: Unit ! Row[B]): Unit ! Row[(A, B)] =
    def loop(x: At[A] | Null, y: At[B] | Null): Unit ! Row[(A, B)] = defer(nextOf[A](stepped(pa, x)) match
      case Next.Tell(a, xRest) => nextOf[B](stepped(pb, y)) match
        case Next.Tell(b, yRest) => say((a, b)).flatMap(_ => loop(xRest, yRest))
        case _ => ended[(A, B)]
      case _ => ended[(A, B)])
    loop(null, null)

  /** the walk, from a state: where it ended and whether a `Stop` did */
  private[okay] def readState[W, S, R](g: Unit ! Row[W])(K: FoldUntil[W, S, R])(s0: S): Halt[S] =
    @tailrec def loop(s: S, at: At[W] | Null): Halt[S] =
      if K.done(s) then Halt(s, false)
      else nextOf[W](stepped(g, at)) match
        case Next.Tell(w, rest) =>
          // `done` BEFORE the rest — resuming it runs the body between
          // this yield and the next; a reader that has read enough must not
          val s2 = K.add(s, w)
          if K.done(s2) then Halt(s2, false) else loop(s2, rest)
        case Next.End => Halt(s, false)
        case Next.Stopped => Halt(s, true)
    loop(s0, null)

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
    // the tell last handed over, paused — resumed for the next; `null` before the first
    private var at: At[W] | Null = null
    private var head: W | Null = null
    private var has = false
    private var ended = false

    private def advance(): Unit =
      if !has && !ended then
        // the rest held from the previous element is resumed NOW —
        // this is where the body between two yields runs
        nextOf[W](stepped(start, at)) match
          case Next.Tell(w, rest) => head = w; has = true; at = rest
          case _ => ended = true; at = null

    def hasNext: Boolean = { advance(); has }
    def next(): W =
      advance()
      if !has then throw new NoSuchElementException("next on an exhausted generator")
      has = false
      head.nn
