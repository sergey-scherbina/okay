package okay

/**
 * A stream is codata: defined not by its constructors but by the one
 * observation uncons — the next element and the rest of the stream,
 * or None at the end. The observer holds the pace: nothing past the
 * asked element is computed, which is what makes early stop, zip and
 * interleave expressible (a push consumer like Fold cannot zip).
 *
 * The observation is EFFECTFUL: uncons answers in the effect F, so a
 * stream may perform work — wait, read, sleep — to produce its next
 * element. A pure stream takes F = Pure (= Nothing, the empty
 * signature), whose Handler is trivial; an asynchronous stream takes
 * F = Async, and on Loom its consumer just blocks a virtual thread
 * per element. Consumption needs a Handler[F] in scope — for Pure it
 * always is.
 *
 * LazyList is the final coalgebra of X => Option[(A, X)] — the
 * canonical carrier every stream unfolds into (see toLazyList); a
 * Producer observes by stepping its next operation. Writer is the
 * cousin with a richer observation, Either[A, (W, rest)] — the same
 * codata but with the answer carried at the end (see Writer.uncons).
 *
 * Re-observation contract: uncons is repeatable, but on a program
 * carrier a repeated uncons repeats the step's work (and its
 * effects!) — only the LazyList bridge memoizes.
 */
trait Stream[S[_], F[+_]]:
  /** the next element and the rest (or None at the end), inside the effect F */
  def uncons[A](s: S[A]): Option[(A, S[A])] ! F

  /** the linear view (see the iterator extension); an instance may
   * specialize it to skip the per-element Option and tuple of uncons */
  def iterator[A](s: S[A])(using Handler[F]): Iterator[A] =
    Iterator.unfold(s)(uncons(_).runWith)

/** the final coalgebra observes itself, purely */
given Stream[LazyList, Pure] with
  def uncons[A](s: LazyList[A]): Option[(A, LazyList[A])] ! Pure =
    pure(if s.isEmpty then None else Some((s.head, s.tail)))

/** a List is a (finite, strict, pure) stream */
given Stream[List, Pure] with
  def uncons[A](s: List[A]): Option[(A, List[A])] ! Pure = pure(s match
    case a :: t => Some((a, t))
    case Nil => None)

/**
 * The stream carrier is the canonical MonadPlus: the empty stream is
 * failure, appending is concatenation. (LazyList's own members keep
 * winning postfix calls; the instance is what generic code — >>>, a
 * MonadPlus-polymorphic function — summons.)
 */
given MonadPlus[LazyList] with
  override def pure[A](a: A): LazyList[A] = LazyList(a)
  override def empty[A]: LazyList[A] = LazyList.empty
  extension [A](x: LazyList[A])
    override def flatMap[B](f: A => LazyList[B]): LazyList[B] = x.flatMap(f)
    override def append(y: LazyList[A]): LazyList[A] = x #::: y

extension [S[_], F[+_], A](s: S[A])(using St: Stream[S, F], H: Handler[F])
  /** the next element and the rest, or None at the end (F is handled here) */
  def uncons: Option[(A, S[A])] = St.uncons(s).runWith

  /**
   * The anamorphism into the final coalgebra: unfold the stream into
   * a LazyList by repeated uncons, on demand and memoized — the
   * canonical bridge from any stream representation, and the free way
   * to every LazyList combinator. Each pulled element runs its F by
   * the Handler — on an Async stream the pull blocks (a virtual
   * thread, on Loom).
   */
  def toLazyList: LazyList[A] = LazyList.unfold(s)(St.uncons(_).runWith)

  /**
   * The LINEAR view: walk the stream as an Iterator — no cells, no
   * memoization, each element observed once and gone. This is the
   * fused consumption mode: iterator.map(f).filter(p).take(n).sum
   * runs the whole pipeline in one pass at Iterator speed, where the
   * LazyList bridge would pay a memoized cell per element per stage.
   * Use it when the pipeline is consumed once; toLazyList when the
   * stream is re-observed.
   */
  def iterator: Iterator[A] = St.iterator(s)

/**
 * The standard combinators, generically over any Stream: every one
 * observes by uncons and lands in the final coalgebra, so
 * transformation is lazy, memoized, and uniform across carriers —
 * transform a Producer, zip it with a LazyList, fold the result.
 * The elementwise map and flatMap are spelled Stream.map and
 * Stream.flatMap: on a program carrier the postfix .map/.flatMap
 * belong to the monad (they transform the ANSWER, not the elements),
 * so the stream versions keep the explicit name. On a writer program
 * convert with .toLazyList first — its Stream instance hides behind a
 * type lambda that extension inference cannot see through.
 */
object Stream:

  import scala.annotation.tailrec

  /** transform each element (the monad owns the postfix .map) */
  def map[S[_], F[+_], A, B](s: S[A])(f: A => B)(using Stream[S, F], Handler[F]): LazyList[B] =
    s.toLazyList.map(f)

  /** a stream for each element, concatenated (any carriers) */
  def flatMap[S[_], T[_], F[+_], G[+_], A, B](s: S[A])(f: A => T[B])
                                    (using Stream[S, F], Handler[F], Stream[T, G], Handler[G]): LazyList[B] =
    s.toLazyList.flatMap(f(_).toLazyList)

  /** consume with a Fold algebra, one uncons at a time */
  def fold[S[_], F[+_], A, B](s: S[A])(using f: Fold[A, B])(using St: Stream[S, F], H: Handler[F]): B = {
    @tailrec def loop(b: B, x: S[A]): B = St.uncons(x).runWith match
      case None => b
      case Some((a, t)) => loop(f.add(b, a), t)

    loop(f.init, s)
  }

extension [S[_], F[+_], A](s: S[A])(using St: Stream[S, F], H: Handler[F])
  /** keep the elements satisfying p */
  def filter(p: A => Boolean): LazyList[A] = s.toLazyList.filter(p)

  /** map the elements on which pf is defined */
  def collect[B](pf: PartialFunction[A, B]): LazyList[B] = s.toLazyList.collect(pf)

  /** all but the first n elements */
  def drop(n: Int): LazyList[A] = s.toLazyList.drop(n)

  /** the longest prefix satisfying p */
  def takeWhile(p: A => Boolean): LazyList[A] = s.toLazyList.takeWhile(p)

  /** the rest, after the longest prefix satisfying p */
  def dropWhile(p: A => Boolean): LazyList[A] = s.toLazyList.dropWhile(p)

  /** pair up with another stream (any carrier), until either ends */
  def zip[T[_], G[+_], B](that: T[B])(using Stream[T, G], Handler[G]): LazyList[(A, B)] =
    s.toLazyList.zip(that.toLazyList)

  /** pair each element with its position */
  def zipWithIndex: LazyList[(A, Int)] = s.toLazyList.zipWithIndex

  /** this stream, then that one (any carrier) */
  def ++[T[_], G[+_]](that: T[A])(using Stream[T, G], Handler[G]): LazyList[A] =
    s.toLazyList #::: that.toLazyList

  /** fold all the elements strictly (diverges on an infinite stream) */
  def foldLeft[B](z: B)(op: (B, A) => B): B = s.toLazyList.foldLeft(z)(op)

  /** run f on every element */
  def foreach(f: A => Unit): Unit = s.toLazyList.foreach(f)

  /** the first element, if any */
  def headOption: Option[A] = s.uncons.map(_._1)

  /** the first element satisfying p (stops as soon as it is found) */
  def find(p: A => Boolean): Option[A] = s.toLazyList.find(p)

  /** is there an element satisfying p (stops as soon as one is found) */
  def exists(p: A => Boolean): Boolean = s.toLazyList.exists(p)

  /** do all elements satisfy p (stops at the first that does not) */
  def forall(p: A => Boolean): Boolean = s.toLazyList.forall(p)

  /** all the elements, strictly */
  def toList: List[A] = s.toLazyList.toList

/**
 * The same observations directly on a writer program (as overloads
 * beside the generic ones, so both resolve): inference cannot reach
 * Writer's Stream instance through its type lambda — it would have to
 * abstract the element slot of a constant lambda, a higher-order
 * unification the compiler does not attempt — so the shape gets them
 * first-order.
 */
extension [W, A](a: A ! Writer % W)
  /** the next told value and the rest, or None (the answer forgotten) */
  def uncons: Option[(W, A ! Writer % W)] = Writer.uncons(a).toOption

  /** unfold the told values into the final coalgebra, on demand */
  def toLazyList: LazyList[W] = LazyList.unfold(a)(Writer.uncons(_).toOption)

/**
 * A writer program with ARBITRARY effects G is a stream too: the told
 * values are the elements (typed W, separate from the answer), the
 * G-operations run at each pull by the Handler. Structured effects
 * without a Handler — State, Reader, Throws — are run over the
 * program first: their handlers forward the telling, so they ARE
 * stream transformers, and what remains is the Handler-able residue.
 */
extension [W, A, G[+_]](a: A ! Writer % W + G)(using TypeableK[G], Handler[G])
  /** the next told value and the rest, or None (G handled here) */
  def uncons: Option[(W, A ! Writer % W + G)] = Writer.uncons(a).runWith.toOption

  /** unfold the told values into the final coalgebra; each pull runs its G */
  def toLazyList: LazyList[W] = LazyList.unfold(a)(Writer.uncons(_).runWith.toOption)
