package okay2

import scala.annotation.tailrec
import Free.{Return, Inject, Bind}

/**
 * The early end of a generation: `Gen.stop` performs it, and every
 * reader of a `Gen` ends there — the rest of the body is never run.
 * Python's `return` inside a generator. The class IS the whole
 * identity (no parameter but the erased answer), so a split on it is
 * total.
 */
sealed trait Stop extends Row { type Op[+A] = Stop.Op[A] }

object Stop {
  sealed trait Op[+A]
  case object Now extends Op[Nothing]

  implicit val effect: Effect[Stop] = Effect.of[Stop]
}

/**
 * A GENERATOR IS A PROGRAM THAT TELLS.
 *
 * Python's `yield` hands a value to whoever asked and does not run
 * again until asked again; the body ends when it returns; the reader
 * may stop asking at any point. Every piece of that is what a `Writer`
 * program already is: `Bind(Inject(Say(w)), k)` suspends until a
 * reader calls `k`, a `Return` is the end, and a reader that does not
 * call `k` has stopped the body. `Gen[W]` is that program with `Stop`
 * beside `Writer`, so a body can end itself from the middle of a loop.
 *
 * What is added is the vocabulary: element-wise `map`/`flatMap`/
 * `withFilter`/`take`… as MEMBERS, so a plain for-comprehension over a
 * `Gen` builds a generator; readers that STOP (`first`, `find`,
 * `take(n).toList`) through `FoldUntil`; and `iterator`, the Python
 * semantics made literal. A `Gen` is a value, so reading it twice runs
 * the body twice (`toLazyList` is the memoising view).
 *
 * The element-wise stages are FUSED (the Scala 3 core's gen-chain-
 * fusion, ported whole): a chain of stages is a transducer applied per
 * element by the reader, and the same chain MATERIALISED is the walks
 * it stands for (`program`). The tests hold the two readings equal on
 * generated chains.
 *
 * The literature: Kiselyov, Peyton Jones & Sabry, "Lazy v. Yield"
 * (APLAS 2012); James & Sabry, "Yield: Mainstream Delimited
 * Continuations" (TPDC 2011); PEP 255; for `zip`, Kiselyov, Biboudis,
 * Palladinos & Smaragdakis, "Stream fusion, to completeness" (POPL
 * 2017).
 */
final class Gen[W](val chain: Gen.Chain[W]) extends AnyVal {
  /** the chain MATERIALISED as the walks it stands for */
  def program: Unit ! Gen.Row[W] = chain.program

  // ---- element-wise: a STAGE appended, nothing walked
  def map[V](f: W => V): Gen[V] = new Gen(chain.andThen(new Gen.Xf.Map(f)))
  def filter(p: W => Boolean): Gen[W] = new Gen(chain.andThen(new Gen.Xf.Filter(p)))
  def withFilter(p: W => Boolean): Gen[W] = filter(p)
  /** the first n — a fused read is done at the n-th kept element and
   * the body runs no further */
  def take(n: Int): Gen[W] = new Gen(chain.andThen(new Gen.Xf.Take[W](n)))
  def takeWhile(p: W => Boolean): Gen[W] = new Gen(chain.andThen(new Gen.Xf.TakeWhile(p)))
  /** skip n: the skipped steps run (the body is between the tells) */
  def drop(n: Int): Gen[W] = new Gen(chain.andThen(new Gen.Xf.Drop[W](n)))

  /** Python's `yield from`: every told w replaced by f(w)'s tells; a
   * `Stop` inside it ends the whole generation */
  def flatMap[V](f: W => Gen[V]): Gen[V] = new Gen(chain.andThen(new Gen.Xf.FlatMap(f)))
  /** sequencing — one after the other, the reader's state threaded
   * from the left side into the right */
  def ++(h: Gen[W]): Gen[W] = new Gen(new Gen.Chain.Cat(chain, h.chain, new Gen.Xf.Id[W]))
  def zipWithIndex: Gen[(W, Int)] = new Gen(chain.andThen(new Gen.Xf.Indexed[W]))

  /** zip two generators element by element, stopping when either
   * ends — pulled one step at a time from each side's program, which
   * reaches through a fused `flatMap` with no special case */
  def zip[V](other: Gen[V]): Gen[(W, V)] = Gen.fromProgram(Gen.zipping(program, other.program))
  /** `zip`, then apply f to each pair in the same pass */
  def zipWith[V, U](other: Gen[V])(f: (W, V) => U): Gen[U] = zip(other).map(f.tupled)

  // ---- readers: ONE walk of the source, the chain applied per element
  /** the general stopping reader: `done` is asked before the first
   * element and after each; a `Stop` in the body ends the read */
  def foldUntil[S, R](k: FoldUntil[W, S, R]): R = k.end(chain.readState(k)(k.init).s)
  def toList: List[W] = foldUntil(Gen.collecting[W])
  def toVector: Vector[W] = toList.toVector
  def first: Option[W] = foldUntil(FoldUntil.headOption[W])
  def find(p: W => Boolean): Option[W] = foldUntil(FoldUntil.find(p))
  def exists(p: W => Boolean): Boolean = foldUntil(FoldUntil.exists(p))
  def forall(p: W => Boolean): Boolean = foldUntil(FoldUntil.forall(p))
  def foreach(f: W => Unit): Unit = foldUntil(Gen.each(f))
  /** THE PYTHON ONE: `next()` runs the body to its next tell; the
   * continuation is held and applied only by the call after, so the
   * code between two yields runs when the second is asked for */
  def iterator: Iterator[W] = new Gen.Stepper[W](program)
  /** memoising, lazy — read it twice and the body ran once */
  def toLazyList: LazyList[W] = LazyList.from(iterator)
}

object Gen {
  type Row[W] = Writer[W] + Stop

  /** a program that tells, as a generator — the same value, named */
  def fromProgram[W](p: Unit ! Row[W]): Gen[W] = new Gen(Chain(p))

  /** a read's answer: the reader's state where the walk ended, and
   * whether a `Stop` ended it */
  final class Halt[S](val s: S, val stopped: Boolean)

  /**
   * A source and the stages to read it through. The source's element
   * type is an existential the value class cannot name; a type member
   * names it once. Two node kinds: a program (`Plain`/`Staged`) and a
   * concatenation (`Cat`), each read FROM a downstream state.
   */
  abstract class Chain[W] {
    type A
    val xf: Xf[A, W]
    /** the raw source(s) through a reader at A, from a state */
    def readSource[S, R](k: FoldUntil[A, S, R])(s0: S): Halt[S]
    /** the whole chain from a downstream state: the stages' own state
     * wrapped around it, the walk, and the reader's state back out */
    final def readState[S, R](k: FoldUntil[W, S, R])(s0: S): Halt[S] = {
      val h = readSource(xf.fold(k))(xf.inject(s0))
      new Halt(xf.project(h.s), h.stopped)
    }
    def program: Unit ! Row[W]
    def andThen[V](s: Xf[W, V]): Chain[V]
  }

  object Chain {
    def apply[W](p: Unit ! Row[W]): Chain[W] = new Plain(p)

    final class Plain[W](val source: Unit ! Row[W]) extends Chain[W] {
      type A = W
      val xf: Xf[W, W] = new Xf.Id[W]
      def readSource[S, R](k: FoldUntil[W, S, R])(s0: S): Halt[S] = Gen.readState(source)(k)(s0)
      def program: Unit ! Row[W] = source
      def andThen[V](s: Xf[W, V]): Chain[V] = new Staged(source, s)
    }

    final class Staged[A0, W](val source: Unit ! Row[A0], val xf: Xf[A0, W]) extends Chain[W] {
      type A = A0
      def readSource[S, R](k: FoldUntil[A0, S, R])(s0: S): Halt[S] = Gen.readState(source)(k)(s0)
      def program: Unit ! Row[W] = Free.delay(() => xf.walk(source))
      def andThen[V](s: Xf[W, V]): Chain[V] = new Staged(source, xf.andThen(s))
    }

    /** one after the other; `xf` applies AFTER both, its state
     * threaded across */
    final class Cat[A0, W](val l: Chain[A0], val r: Chain[A0], val xf: Xf[A0, W]) extends Chain[W] {
      type A = A0
      def readSource[S, R](k: FoldUntil[A0, S, R])(s0: S): Halt[S] = {
        val h = l.readState(k)(s0)
        if (h.stopped || k.done(h.s)) h else r.readState(k)(h.s)
      }
      def program: Unit ! Row[W] = Free.delay(() => xf.walk(l.program.flatMap(_ => r.program)))
      def andThen[V](s: Xf[W, V]): Chain[V] = new Cat(l, r, xf.andThen(s))
    }
  }

  /**
   * A stage with two readings: FUSED — a transformer of the reader,
   * with the state type it adds carried as `St` so the fused reader is
   * a `FoldUntil` at a known state type — and MATERIALISED, the walk.
   * `inject`/`project` wrap and unwrap that state, so a chain can be
   * read FROM a downstream state.
   */
  sealed trait Xf[A, B] {
    type St[S]
    def fold[S, R](k: FoldUntil[B, S, R]): FoldUntil[A, St[S], R]
    def inject[S](s: S): St[S]
    def project[S](st: St[S]): S
    def walk(p: Unit ! Row[A]): Unit ! Row[B]
    def andThen[C](that: Xf[B, C]): Xf[A, C] = new Xf.Compose(this, that)
  }

  object Xf {
    /** a stage that adds no state */
    sealed trait Stateless[A, B] extends Xf[A, B] {
      type St[S] = S
      def inject[S](s: S): S = s
      def project[S](st: S): S = st
    }

    /** a count beside the reader's state — a class, not a tuple, so
     * the count is a field and never boxed */
    final class Counted[S](val n: Int, val s: S)

    /** a stage whose state is a count, fresh at zero when injected */
    sealed trait Counting[A, B] extends Xf[A, B] {
      type St[S] = Counted[S]
      def inject[S](s: S): Counted[S] = new Counted(0, s)
      def project[S](st: Counted[S]): S = st.s
    }

    final class Id[A] extends Stateless[A, A] {
      def fold[S, R](k: FoldUntil[A, S, R]): FoldUntil[A, S, R] = k
      def walk(p: Unit ! Row[A]): Unit ! Row[A] = p
      override def andThen[C](that: Xf[A, C]): Xf[A, C] = that
    }

    final class Map[A, B](f: A => B) extends Stateless[A, B] {
      def fold[S, R](k: FoldUntil[B, S, R]): FoldUntil[A, S, R] = new FoldUntil[A, S, R] {
        def init: S = k.init
        def add(s: S, a: A): S = k.add(s, f(a))
        def done(s: S): Boolean = k.done(s)
        def end(s: S): R = k.end(s)
      }
      // Stop takes no parameter and is not a Writer: the row is distinct
      def walk(p: Unit ! Row[A]): Unit ! Row[B] = Writer.map[A, B, Unit, Stop](p)(f)(Distinct.unchecked)
    }

    final class Filter[A](p: A => Boolean) extends Stateless[A, A] {
      def fold[S, R](k: FoldUntil[A, S, R]): FoldUntil[A, S, R] = new FoldUntil[A, S, R] {
        def init: S = k.init
        def add(s: S, a: A): S = if (p(a)) k.add(s, a) else s
        def done(s: S): Boolean = k.done(s)
        def end(s: S): R = k.end(s)
      }
      def walk(g: Unit ! Row[A]): Unit ! Row[A] = filtering(p)(g)
    }

    /** `yield from`, fused: the inner generator's chain is read from
     * the reader's state inside `add`; n = 1 once an inner `Stop` ended
     * it, which ends the whole generation */
    final class FlatMap[A, B](f: A => Gen[B]) extends Counting[A, B] {
      def fold[S, R](k: FoldUntil[B, S, R]): FoldUntil[A, Counted[S], R] = new FoldUntil[A, Counted[S], R] {
        def init: Counted[S] = new Counted(0, k.init)
        def add(s: Counted[S], a: A): Counted[S] = {
          val h = f(a).chain.readState(k)(s.s)
          new Counted(if (h.stopped) 1 else 0, h.s)
        }
        def done(s: Counted[S]): Boolean = s.n == 1 || k.done(s.s)
        def end(s: Counted[S]): R = k.end(s.s)
      }
      def walk(p: Unit ! Row[A]): Unit ! Row[B] = splice(p)(w => f(w).program)
    }

    final class Indexed[A] extends Counting[A, (A, Int)] {
      def fold[S, R](k: FoldUntil[(A, Int), S, R]): FoldUntil[A, Counted[S], R] = new FoldUntil[A, Counted[S], R] {
        def init: Counted[S] = new Counted(0, k.init)
        def add(s: Counted[S], a: A): Counted[S] = new Counted(s.n + 1, k.add(s.s, (a, s.n)))
        def done(s: Counted[S]): Boolean = k.done(s.s)
        def end(s: Counted[S]): R = k.end(s.s)
      }
      def walk(g: Unit ! Row[A]): Unit ! Row[(A, Int)] = indexed(g)
    }

    final class Take[A](n: Int) extends Counting[A, A] {
      def fold[S, R](k: FoldUntil[A, S, R]): FoldUntil[A, Counted[S], R] = new FoldUntil[A, Counted[S], R] {
        def init: Counted[S] = new Counted(0, k.init)
        def add(s: Counted[S], a: A): Counted[S] = new Counted(s.n + 1, k.add(s.s, a))
        def done(s: Counted[S]): Boolean = s.n >= n || k.done(s.s)
        def end(s: Counted[S]): R = k.end(s.s)
      }
      def walk(g: Unit ! Row[A]): Unit ! Row[A] = taking(n)(g)
    }

    /** n = 1 once stopped */
    final class TakeWhile[A](p: A => Boolean) extends Counting[A, A] {
      def fold[S, R](k: FoldUntil[A, S, R]): FoldUntil[A, Counted[S], R] = new FoldUntil[A, Counted[S], R] {
        def init: Counted[S] = new Counted(0, k.init)
        def add(s: Counted[S], a: A): Counted[S] = if (p(a)) new Counted(0, k.add(s.s, a)) else new Counted(1, s.s)
        def done(s: Counted[S]): Boolean = s.n == 1 || k.done(s.s)
        def end(s: Counted[S]): R = k.end(s.s)
      }
      def walk(g: Unit ! Row[A]): Unit ! Row[A] = takingWhile(p)(g)
    }

    /** n = skipped so far */
    final class Drop[A](n: Int) extends Counting[A, A] {
      def fold[S, R](k: FoldUntil[A, S, R]): FoldUntil[A, Counted[S], R] = new FoldUntil[A, Counted[S], R] {
        def init: Counted[S] = new Counted(0, k.init)
        def add(s: Counted[S], a: A): Counted[S] =
          if (s.n < n) new Counted(s.n + 1, s.s) else new Counted(s.n, k.add(s.s, a))
        // the reader's `done` decides alone
        def done(s: Counted[S]): Boolean = k.done(s.s)
        def end(s: Counted[S]): R = k.end(s.s)
      }
      def walk(g: Unit ! Row[A]): Unit ! Row[A] = dropping(n)(g)
    }

    final class Compose[A, B, C](val a: Xf[A, B], val b: Xf[B, C]) extends Xf[A, C] {
      type St[S] = a.St[b.St[S]]
      def fold[S, R](k: FoldUntil[C, S, R]): FoldUntil[A, St[S], R] = a.fold(b.fold(k))
      def inject[S](s: S): St[S] = a.inject(b.inject(s))
      def project[S](st: St[S]): S = b.project(a.project(st))
      def walk(p: Unit ! Row[A]): Unit ! Row[C] = b.walk(a.walk(p))
    }
  }

  /** yield one value — a tell. Sequence with `++`; `flatMap` is
   * element-wise (Python's `yield from`) */
  def emit[W](w: W): Gen[W] = fromProgram(say(w))

  /** end the generation here: nothing after it runs, in a loop or not */
  def stop[W]: Gen[W] = fromProgram(ended[W])

  /** the tell and the end as PROGRAMS for the walks: a `Writer[W]`
   * program and a `Stop` program are both `Row[W]` programs by
   * contravariance, no widening needed */
  private def say[W](w: W): Unit ! Row[W] = Writer.tell(w)
  private def ended[W]: Unit ! Row[W] = Free.inject[Stop, Unit](Stop.Now)
  private def done[W]: Unit ! Row[W] = Return(())

  def empty[W]: Gen[W] = fromProgram(done[W])

  def apply[W](ws: W*): Gen[W] = from(ws)

  /** the elements of a collection, one tell each — nothing is done at
   * construction (a `Delay`), and an Iterator argument is read once
   * into a memoising LazyList so the Gen stays re-runnable */
  def from[W](it: IterableOnce[W]): Gen[W] = {
    def go(rest: LazyList[W]): Unit ! Row[W] = rest match {
      case h #:: tl => say(h).flatMap(_ => go(tl))
      case _ => done[W]
    }
    val all = it.iterator.to(LazyList)
    fromProgram(Free.delay(() => go(all)))
  }

  /** the producing side's `loop`: a state that decides when it ends */
  def unfold[S, W](s: S)(f: S => Option[(W, S)]): Gen[W] = {
    def go(s: S): Unit ! Row[W] = Free.delay(() => f(s) match {
      case Some((w, s2)) => say(w).flatMap(_ => go(s2))
      case None => done[W]
    })
    fromProgram(go(s))
  }

  /** a plain Writer program as a generator — a `Row[W]` program by
   * contravariance */
  def of[W](p: Unit ! Writer[W]): Gen[W] = fromProgram(p)

  // ---- the walks, over programs. The row is `Writer[W] + Stop` and
  // nothing else, so an operation that is not a `Say` IS the `Stop`:
  // `Writer.said` is the match (its one cast is Writer's own), and the
  // walks are plain pattern matches with no closure per operation.

  /** every told `w` replaced by `f(w)`'s tells, in place; the
   * continuation called only when `f(w)` has been read through. A
   * `Stop` drops the continuation: the end. */
  private[okay2] def splice[W, V](g: Unit ! Row[W])(f: W => Unit ! Row[V]): Unit ! Row[V] = {
    val Said = Writer.said[W]
    def loop(x: Unit ! Row[W]): Unit ! Row[V] = Free.resume(x) match {
      case Return(_) => done[V]
      case Inject(Said(w)) => f(w)
      case Bind(Inject(Said(w)), k) => f(w).flatMap(_ => loop(k(())))
      case _ => ended[V]
    }
    loop(g)
  }

  private def indexed[W](g: Unit ! Row[W]): Unit ! Row[(W, Int)] = {
    val Said = Writer.said[W]
    def loop(i: Int)(x: Unit ! Row[W]): Unit ! Row[(W, Int)] = Free.resume(x) match {
      case Return(_) => done[(W, Int)]
      case Inject(Said(w)) => say((w, i))
      case Bind(Inject(Said(w)), k) => say((w, i)).flatMap(_ => loop(i + 1)(k(())))
      case _ => ended[(W, Int)]
    }
    loop(0)(g)
  }

  private def taking[W](n: Int)(g: Unit ! Row[W]): Unit ! Row[W] = {
    val Said = Writer.said[W]
    def loop(n: Int)(x: Unit ! Row[W]): Unit ! Row[W] =
      if (n <= 0) done[W]
      else Free.resume(x) match {
        case Return(_) => done[W]
        case i @ Inject(_) => i
        case Bind(Inject(Said(w)), k) => if (n == 1) say(w) else say(w).flatMap(_ => loop(n - 1)(k(())))
        case _ => ended[W]
      }
    loop(n)(g)
  }

  /** keep the tells `p` accepts: the accepted tell is the input's OWN
   * `Inject` node bound to the rest of the walk; a rejected one is
   * skipped by a `Free.delay` so a long run of rejections is flat */
  private def filtering[W](p: W => Boolean)(g: Unit ! Row[W]): Unit ! Row[W] = {
    val Said = Writer.said[W]
    def loop(x: Unit ! Row[W]): Unit ! Row[W] = Free.resume(x) match {
      case Return(_) => done[W]
      case i @ Inject(Said(w)) => if (p(w)) i else done[W]
      case Bind(i @ Inject(Said(w)), k) =>
        if (p(w)) Bind[Row[W], Any, Unit](i, (_: Any) => loop(k(())))
        else Free.delay(() => loop(k(())))
      case _ => ended[W]
    }
    loop(g)
  }

  private def takingWhile[W](p: W => Boolean)(g: Unit ! Row[W]): Unit ! Row[W] = {
    val Said = Writer.said[W]
    def loop(x: Unit ! Row[W]): Unit ! Row[W] = Free.resume(x) match {
      case Return(_) => done[W]
      case Inject(Said(w)) => if (p(w)) say(w) else done[W]
      case Bind(Inject(Said(w)), k) => if (p(w)) say(w).flatMap(_ => loop(k(()))) else done[W]
      case _ => ended[W]
    }
    loop(g)
  }

  private def dropping[W](n: Int)(g: Unit ! Row[W]): Unit ! Row[W] = {
    val Said = Writer.said[W]
    def loop(n: Int)(x: Unit ! Row[W]): Unit ! Row[W] =
      if (n <= 0) x
      else Free.resume(x) match {
        case Return(_) => done[W]
        case Inject(Said(_)) => done[W]
        case Bind(Inject(Said(_)), k) => Free.delay(() => loop(n - 1)(k(())))
        case _ => ended[W]
      }
    loop(n)(g)
  }

  /** ONE STEP of a program, resumed as far as the next tell: the value
   * and the rest, or `None` at the end (`Return` or `Stop`). Works on
   * ANY program this file builds, fused `flatMap`s included: `resume`
   * walks past however many nodes a splice needed to reach the next
   * real tell — `zipping`'s whole trick */
  private def pull[A](x: Unit ! Row[A]): Option[(A, Unit ! Row[A])] = {
    val Said = Writer.said[A]
    Free.resume(x) match {
      case Return(_) => None
      case Inject(Said(w)) => Some((w, done[A]))
      case Bind(Inject(Said(w)), k) => Some((w, k(())))
      case _ => None
    }
  }

  /** `zip`, as a walk: one element from EACH side, paired, stopping the
   * moment either ends; the recursion sits inside `say`'s flatMap, so
   * nothing recurses at build time */
  private[okay2] def zipping[A, B](pa: Unit ! Row[A], pb: Unit ! Row[B]): Unit ! Row[(A, B)] = {
    def loop(x: Unit ! Row[A], y: Unit ! Row[B]): Unit ! Row[(A, B)] =
      pull(x) match {
        case None => ended[(A, B)]
        case Some((a, xRest)) => pull(y) match {
          case None => ended[(A, B)]
          case Some((b, yRest)) => say((a, b)).flatMap(_ => loop(xRest, yRest))
        }
      }
    loop(pa, pb)
  }

  /** the walk, from a state: where it ended and whether a `Stop` did.
   * One `@tailrec` loop — the Said match needs no closure, so this is
   * a real tail call per element */
  private[okay2] def readState[W, S, R](g: Unit ! Row[W])(k: FoldUntil[W, S, R])(s0: S): Halt[S] = {
    val Said = Writer.said[W]
    @tailrec def loop(s: S)(x: Unit ! Row[W]): Halt[S] =
      if (k.done(s)) new Halt(s, false)
      else Free.resume(x) match {
        case Return(_) => new Halt(s, false)
        case Inject(Said(w)) => new Halt(k.add(s, w), false)
        case Bind(Inject(Said(w)), c) =>
          // `done` BEFORE `c` — `c(())` runs the body between this yield
          // and the next; a reader that has read enough must not
          val s2 = k.add(s, w)
          if (k.done(s2)) new Halt(s2, false) else loop(s2)(c(()))
        case _ => new Halt(s, true)
      }
    loop(s0)(g)
  }

  /** collect everything told, never done */
  private def collecting[W]: FoldUntil[W, List[W], List[W]] = new FoldUntil[W, List[W], List[W]] {
    def init: List[W] = Nil
    def add(s: List[W], a: W): List[W] = a :: s
    def done(s: List[W]): Boolean = false
    def end(s: List[W]): List[W] = s.reverse
  }

  private def each[W](f: W => Unit): FoldUntil[W, Unit, Unit] = new FoldUntil[W, Unit, Unit] {
    def init: Unit = ()
    def add(s: Unit, a: W): Unit = f(a)
    def done(s: Unit): Boolean = false
    def end(s: Unit): Unit = ()
  }

  private final class Stepper[W](start: Unit ! Row[W]) extends Iterator[W] {
    private val Said = Writer.said[W]
    private var rest: Unit ! Row[W] = start
    private var cont: Any => Unit ! Row[W] = null
    private var head: W = _
    private var has = false
    private var ended = false

    private def advance(): Unit =
      if (!has && !ended) {
        // the continuation held from the previous element is applied
        // NOW — this is where the body between two yields runs
        val x: Unit ! Row[W] = if (cont != null) { val k = cont; cont = null; k(()) } else rest
        Free.resume(x) match {
          case Return(_) => ended = true
          case Inject(Said(w)) => head = w; has = true; rest = done[W]
          case Bind(Inject(Said(w)), k) => head = w; has = true; cont = k
          case _ => ended = true
        }
      }

    def hasNext: Boolean = { advance(); has }
    def next(): W = {
      advance()
      if (!has) throw new NoSuchElementException("next on an exhausted generator")
      has = false
      head
    }
  }
}
