package okay2

import scala.annotation.tailrec
import Free.{Return, Inject, Bind}
import Split.split

/**
 * A stream is codata: defined not by its constructors but by the one
 * observation uncons — the next element and the rest of the stream,
 * or None at the end. The observer holds the pace: nothing past the
 * asked element is computed. The observation is EFFECTFUL: uncons
 * answers in the row F, so a stream may perform work to produce its
 * next element; a pure stream takes F = Pure. Consumption needs a
 * Handler[F] in scope — for Pure it always is.
 */
trait Stream[S[_], F <: Row] {
  /** the next element and the rest (or None at the end), inside F */
  def uncons[A](s: S[A]): Option[(A, S[A])] ! F

  /** the linear view; an instance may specialize it to skip the
   * per-element Option and tuple of uncons */
  def iterator[A](s: S[A])(implicit H: Handler[F]): Iterator[A] =
    Iterator.unfold(s)(x => Effects.runFree(uncons(x)))
}

object Stream {
  /** the final coalgebra observes itself, purely */
  implicit val lazyList: Stream[LazyList, Pure] = new Stream[LazyList, Pure] {
    def uncons[A](s: LazyList[A]): Option[(A, LazyList[A])] ! Pure =
      pure(if (s.isEmpty) None else Some((s.head, s.tail)))
    override def iterator[A](s: LazyList[A])(implicit H: Handler[Pure]): Iterator[A] = s.iterator
  }

  /** a List is a (finite, strict, pure) stream */
  implicit val list: Stream[List, Pure] = new Stream[List, Pure] {
    def uncons[A](s: List[A]): Option[(A, List[A])] ! Pure = pure(s match {
      case a :: t => Some((a, t))
      case Nil => None
    })
    override def iterator[A](s: List[A])(implicit H: Handler[Pure]): Iterator[A] = s.iterator
  }

  implicit val vector: Stream[Vector, Pure] = new Stream[Vector, Pure] {
    def uncons[A](s: Vector[A]): Option[(A, Vector[A])] ! Pure =
      pure(if (s.isEmpty) None else Some((s.head, s.tail)))
    override def iterator[A](s: Vector[A])(implicit H: Handler[Pure]): Iterator[A] = s.iterator
  }

  /**
   * A writer program is a stream of its told values: the same
   * observation as Writer.uncons with the answer forgotten. The
   * specialized linear view walks the tree with no Option, no Either
   * and no program built per step — the twin of the Scala 3 core's
   * `feedStream`. Summoned by name: inference does not reach a Row's
   * element through a type lambda.
   */
  def feedStream[A]: Stream[({ type L[W] = A ! Writer[W] })#L, Pure] = new Stream[({ type L[W] = A ! Writer[W] })#L, Pure] {
    def uncons[W](s: Free[Writer[W], A]): Option[(W, A ! Writer[W])] ! Pure = pure(Writer.uncons(s).toOption)

    override def iterator[W](s: Free[Writer[W], A])(implicit H: Handler[Pure]): Iterator[W] = new Iterator[W] {
      private var cur: A ! Writer[W] = s
      private var ready = false
      private var ended = false
      private var elem: W = null.asInstanceOf[W]

      // `Say` is Writer's only constructor, so the two Inject shapes
      // are exhaustive over what a pure writer program resumes to
      private val Said = Writer.said[W]

      @tailrec private def advance(): Unit = cur match {
        case Return(_) => ended = true
        case Inject(Said(w)) => elem = w; ready = true; ended = true
        case Bind(Inject(Said(w)), k) => elem = w; ready = true; cur = k(())
        case _ => cur = Free.resume(cur); advance()
      }

      def hasNext: Boolean = {
        if (!ready && !ended) advance()
        ready
      }

      def next(): W = {
        if (!hasNext) throw new java.util.NoSuchElementException("empty writer stream")
        ready = false
        elem
      }
    }
  }

  /**
   * A writer program performing ARBITRARY effects G is a stream in G:
   * the same observation, the G-operations met on the way carried
   * into the answer; the linear view answers a forwarded G-operation
   * directly by the Handler.
   */
  def writerStreamIn[A, G <: Row]: Stream[({ type L[W] = A ! (Writer[W] + G) })#L, G] =
    new Stream[({ type L[W] = A ! (Writer[W] + G) })#L, G] {
      def uncons[W](s: Free[Writer[W] with G, A]): Option[(W, A ! (Writer[W] + G))] ! G =
        Writer.unconsIn[W, A, G](s).map(_.toOption)

      override def iterator[W](s: Free[Writer[W] with G, A])(implicit H: Handler[G]): Iterator[W] = new Iterator[W] {
        private var cur: A ! (Writer[W] + G) = s
        private var ready = false
        private var ended = false
        private var elem: W = null.asInstanceOf[W]

        @tailrec private def advance(): Unit = cur match {
          case Return(_) => ended = true
          case Inject(e) =>
            split[Writer[W], G, Any, Unit](e) {
              case Writer.Say(w) => elem = w; ready = true; ended = true
            } { g => val _ = H.handleOp[Any](g); ended = true }
          case Bind(Inject(e), k) =>
            split[Writer[W], G, Any, Unit](e) {
              case Writer.Say(w) => elem = w; ready = true; cur = k(())
            } { g => cur = k(H.handleOp[Any](g)) }
            if (!ready) advance()
          case _ => cur = Free.resume(cur); advance()
        }

        def hasNext: Boolean = {
          if (!ready && !ended) advance()
          ready
        }

        def next(): W = {
          if (!hasNext) throw new java.util.NoSuchElementException("empty writer stream")
          ready = false
          elem
        }
      }
    }

  /**
   * Consume with a Fold algebra over the LINEAR view, dispatched on
   * the accumulator as the Scala 3 core does: the four primitive
   * shapes keep it unboxed across the loop.
   */
  def fold[S[_], F <: Row, A, B](s: S[A])(fo: Fold[A, B])(implicit St: Stream[S, F], H: Handler[F]): B = fo match {
    case l: Fold.OfLong[A @unchecked] =>
      val it = St.iterator(s); var b = l.initLong
      while (it.hasNext) b = l.addLong(b, it.next())
      b
    case i: Fold.OfInt[A @unchecked] =>
      val it = St.iterator(s); var b = i.initInt
      while (it.hasNext) b = i.addInt(b, it.next())
      b
    case d: Fold.OfDouble[A @unchecked] =>
      val it = St.iterator(s); var b = d.initDouble
      while (it.hasNext) b = d.addDouble(b, it.next())
      b
    case bo: Fold.OfBoolean[A @unchecked] =>
      val it = St.iterator(s); var b = bo.initBoolean
      while (it.hasNext) b = bo.addBoolean(b, it.next())
      b
    case _ =>
      val it = St.iterator(s); var b = fo.init
      while (it.hasNext) b = fo.add(b, it.next())
      b
  }

  /** `fold` with a stop: the iterator is asked for an element only
   * while the state has not seen enough */
  def foldUntil[S[_], F <: Row, A, B, R](s: S[A])(fo: FoldUntil[A, B, R])(implicit St: Stream[S, F], H: Handler[F]): R = {
    val it = St.iterator(s)
    fo match {
      case l: FoldUntil.OfLong[A @unchecked, R @unchecked] =>
        var b = l.initLong
        while (!l.doneLong(b) && it.hasNext) b = l.addLong(b, it.next())
        l.endLong(b)
      case i: FoldUntil.OfInt[A @unchecked, R @unchecked] =>
        var b = i.initInt
        while (!i.doneInt(b) && it.hasNext) b = i.addInt(b, it.next())
        i.endInt(b)
      case d: FoldUntil.OfDouble[A @unchecked, R @unchecked] =>
        var b = d.initDouble
        while (!d.doneDouble(b) && it.hasNext) b = d.addDouble(b, it.next())
        d.endDouble(b)
      case bo: FoldUntil.OfBoolean[A @unchecked, R @unchecked] =>
        var b = bo.initBoolean
        while (!bo.doneBoolean(b) && it.hasNext) b = bo.addBoolean(b, it.next())
        bo.endBoolean(b)
      case _ =>
        var b = fo.init
        while (!fo.done(b) && it.hasNext) b = fo.add(b, it.next())
        fo.end(b)
    }
  }

  /** the standard combinators, over any Stream: every one observes by
   * uncons and lands in the final coalgebra (LazyList), so
   * transformation is lazy, memoized, and uniform across carriers */
  implicit final class StreamOps[S[_], F <: Row, A](private val s: S[A])(implicit St: Stream[S, F], H: Handler[F]) {
    /** the next element and the rest, or None at the end (F is handled here) */
    def uncons: Option[(A, S[A])] = Effects.runFree(St.uncons(s))
    /** the anamorphism into the final coalgebra: unfold into a LazyList, on demand and memoized */
    def toLazyList: LazyList[A] = LazyList.unfold(s)(x => Effects.runFree(St.uncons(x)))
    /** the LINEAR view: each element observed once and gone */
    def iterator: Iterator[A] = St.iterator(s)
    def map[B](f: A => B): LazyList[B] = toLazyList.map(f)
    def filter(p: A => Boolean): LazyList[A] = toLazyList.filter(p)
    def collect[B](pf: PartialFunction[A, B]): LazyList[B] = toLazyList.collect(pf)
    def drop(n: Int): LazyList[A] = toLazyList.drop(n)
    def takeWhile(p: A => Boolean): LazyList[A] = toLazyList.takeWhile(p)
    def dropWhile(p: A => Boolean): LazyList[A] = toLazyList.dropWhile(p)
    def zipWithIndex: LazyList[(A, Int)] = toLazyList.zipWithIndex
    def foldLeft[B](z: B)(op: (B, A) => B): B = St.iterator(s).foldLeft(z)(op)
    def foreach(f: A => Unit): Unit = St.iterator(s).foreach(f)
    def headOption: Option[A] = uncons.map(_._1)
    def find(p: A => Boolean): Option[A] = St.iterator(s).find(p)
    def exists(p: A => Boolean): Boolean = St.iterator(s).exists(p)
    def forall(p: A => Boolean): Boolean = St.iterator(s).forall(p)
    def toList: List[A] = St.iterator(s).toList
    /** consume with a Fold algebra */
    def fold[B](fo: Fold[A, B]): B = Stream.fold[S, F, A, B](s)(fo)
    /** a fold that stops */
    def foldUntil[B, R](fo: FoldUntil[A, B, R]): R = Stream.foldUntil[S, F, A, B, R](s)(fo)
  }

  /**
   * The same observations directly on a writer program, first-order:
   * inference cannot reach the writer carrier's instance through its
   * type lambda, so the shape gets them by name.
   */
  implicit final class FeedOps[W, A](private val a: Free[Writer[W], A]) extends AnyVal {
    /** the next told value and the rest, or None (the answer forgotten) */
    def uncons: Option[(W, A ! Writer[W])] = Writer.uncons(a).toOption
    /** unfold the told values into the final coalgebra, on demand */
    def toLazyList: LazyList[W] = LazyList.unfold(a)(x => Writer.uncons(x).toOption)
    /** the linear view, specialized */
    def iterator: Iterator[W] = feedStream[A].iterator(a)
    /** a fold that stops, by the coroutine-free road */
    def foldUntil[S, R](fo: FoldUntil[W, S, R]): R = Effects.run(Writer.foldUntilAt[W, S, A, R, Pure](a.plus[Pure])(fo))
  }

  /** the effectful writer program's observations: each pull runs its G by the Handler */
  implicit final class FeedInOps[W, A, G <: Row](private val a: Free[Writer[W] with G, A]) extends AnyVal {
    def uncons(implicit H: Handler[G]): Option[(W, A ! (Writer[W] + G))] = Effects.runFree(Writer.unconsIn[W, A, G](a)).toOption
    def toLazyList(implicit H: Handler[G]): LazyList[W] = LazyList.unfold(a)(x => Effects.runFree(Writer.unconsIn[W, A, G](x)).toOption)
    def iterator(implicit H: Handler[G]): Iterator[W] = writerStreamIn[A, G].iterator(a)
    def foldUntil[S, R](fo: FoldUntil[W, S, R])(implicit H: Handler[G]): R = Effects.runFree(Writer.foldUntilAt[W, S, A, R, G](a)(fo))
  }
}

/**
 * A SOURCE A PROGRAM READS ONE STEP AT A TIME: `step` is the next
 * element and the rest — or None — as a program in G. A `Stream`
 * carrier's uncons, a writer program's next told value, the `Take`
 * side of a stage. `loop(f)` is the consumer loop as a program.
 */
trait Pull[A, G <: Row] {
  /** the next element and the rest, or None at the end — in G */
  def step: Option[(A, Pull[A, G])] ! G

  /** the source with the elements `p` refuses skipped */
  def withFilter(p: A => Boolean): Pull[A, G] = {
    val self = this
    new Pull[A, G] {
      def step: Option[(A, Pull[A, G])] ! G =
        Effects.loop[Pull[A, G], Option[(A, Pull[A, G])], G](self) { s =>
          s.step.map {
            case Some((a, next)) if p(a) => Right(Some((a, next.withFilter(p))))
            case Some((_, next)) => Left(next)
            case None => Right(None)
          }
        }
    }
  }

  /** the loop with a pure body, AS A PROGRAM */
  def loop(f: A => Unit): Unit ! G =
    Effects.loop[Pull[A, G], Unit, G](this) { p =>
      p.step.map {
        case Some((a, next)) => f(a); Left(next)
        case None => Right(())
      }
    }
}

object Pull {
  /** any Stream carrier, read by its uncons */
  def of[S[_], A, G <: Row](s: S[A])(implicit St: Stream[S, G]): Pull[A, G] = new Pull[A, G] {
    def step: Option[(A, Pull[A, G])] ! G = St.uncons(s).map(_.map { case (a, rest) => (a, of[S, A, G](rest)) })
  }

  /** the told values of a writer program */
  def told[W, A](a: Free[Writer[W], A]): Pull[W, Pure] = of[({ type L[X] = A ! Writer[X] })#L, W, Pure](a)(Stream.feedStream[A])

  /** the same, the producer performing G between tells */
  def toldIn[W, G <: Row, A](a: Free[Writer[W] with G, A]): Pull[W, G] =
    of[({ type L[X] = A ! (Writer[X] + G) })#L, W, G](a)(Stream.writerStreamIn[A, G])
}
