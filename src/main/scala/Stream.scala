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

  /** the collection's own walk: no `Option`, no tuple, and — the
   * expensive part — no program built and interpreted per element.
   * A linear consumer that takes this route saves two interpreter
   * passes on every element it reads */
  override def iterator[A](s: LazyList[A])(using Handler[Pure]): Iterator[A] =
    s.iterator

/** a List is a (finite, strict, pure) stream */
given Stream[List, Pure] with
  def uncons[A](s: List[A]): Option[(A, List[A])] ! Pure = pure(s match
    case a :: t => Some((a, t))
    case Nil => None)

  override def iterator[A](s: List[A])(using Handler[Pure]): Iterator[A] =
    s.iterator

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

  /** transform each element (the monad owns the postfix .map) */
  def map[S[_], F[+_], A, B](s: S[A])(f: A => B)(using Stream[S, F], Handler[F]): LazyList[B] =
    s.toLazyList.map(f)

  /** a stream for each element, concatenated (any carriers) */
  def flatMap[S[_], T[_], F[+_], G[+_], A, B](s: S[A])(f: A => T[B])
                                    (using Stream[S, F], Handler[F], Stream[T, G], Handler[G]): LazyList[B] =
    s.toLazyList.flatMap(f(_).toLazyList)

  /**
   * Consume with a Fold algebra, over the LINEAR view.
   *
   * `iterator`, not `uncons` per element: every instance overrides
   * the linear view with a direct walk of its carrier, and the
   * default it replaces — an `Option`, a tuple and a program built
   * and run per element — was measured at 103.6–114.5 us per 10k on
   * the G-effectful producer against the walk's 57.3
   * (producer-effectful-stream-iterator). This loop used to be that
   * default, spelled out (stream-fold-via-iterator, 2026-09-20; the
   * numbers are in specs/stream-fold-via-iterator.md).
   *
   * AND dispatching on the accumulator, as `Chunks.fold` and
   * `Writer.fold` do — which this loop used to decline: on the old
   * walk the dispatch bought nothing (139.9 vs 157.2 us, bars
   * overlapping) because the per-element `uncons` program dwarfed it.
   * On the iterator it is the remaining cost: the generic
   * `add(Object, Object)Object` boxes a `Long` accumulator per
   * element — 240 KB and 40 us of a 98 us walk over 10k, measured
   * against the same iterator in a hand-written `while` (spec
   * Results, round 2). Each arm is four lines now, so five copies
   * cost what they weigh.
   */
  def fold[S[_], F[+_], A, B](s: S[A])(using fo: Fold[A, B])(using St: Stream[S, F], H: Handler[F]): B =
    // the element type is erased, so these tests see only the shape —
    // the same unavoidable `@unchecked` `Chunks.fold` carries
    fo match
      case l: Fold.OfLong[A @unchecked] =>
        val it = St.iterator(s)
        var b = l.initLong
        while it.hasNext do b = l.addLong(b, it.next())
        b
      case i: Fold.OfInt[A @unchecked] =>
        val it = St.iterator(s)
        var b = i.initInt
        while it.hasNext do b = i.addInt(b, it.next())
        b
      case d: Fold.OfDouble[A @unchecked] =>
        val it = St.iterator(s)
        var b = d.initDouble
        while it.hasNext do b = d.addDouble(b, it.next())
        b
      case bo: Fold.OfBoolean[A @unchecked] =>
        val it = St.iterator(s)
        var b = bo.initBoolean
        while it.hasNext do b = bo.addBoolean(b, it.next())
        b
      case _ =>
        val it = St.iterator(s)
        var b = fo.init
        while it.hasNext do b = fo.add(b, it.next())
        b

  /**
   * `fold` with a stop (specs/fold-until.md): the iterator is asked
   * for an element only while the state has not seen enough, so a
   * stream that computes on demand computes nothing past the stop.
   */
  def foldUntil[S[_], F[+_], A, B, R](s: S[A])(using fo: FoldUntil[A, B, R])(using St: Stream[S, F], H: Handler[F]): R =
    val it = St.iterator(s)
    // dispatched on the accumulator as `fold` above is, for the same
    // measured reason (fold-until-unboxed: the box is 25x on the loop)
    fo match
      case l: FoldUntil.OfLong[A @unchecked, R @unchecked] =>
        var b = l.initLong
        while !l.doneLong(b) && it.hasNext do b = l.addLong(b, it.next())
        l.endLong(b)
      case i: FoldUntil.OfInt[A @unchecked, R @unchecked] =>
        var b = i.initInt
        while !i.doneInt(b) && it.hasNext do b = i.addInt(b, it.next())
        i.endInt(b)
      case d: FoldUntil.OfDouble[A @unchecked, R @unchecked] =>
        var b = d.initDouble
        while !d.doneDouble(b) && it.hasNext do b = d.addDouble(b, it.next())
        d.endDouble(b)
      case bo: FoldUntil.OfBoolean[A @unchecked, R @unchecked] =>
        var b = bo.initBoolean
        while !bo.doneBoolean(b) && it.hasNext do b = bo.addBoolean(b, it.next())
        bo.endBoolean(b)
      case _ =>
        var b = fo.init
        while !fo.done(b) && it.hasNext do b = fo.add(b, it.next())
        fo.end(b)

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

  // The consumers below answer a VALUE, not a stream, so they walk the
  // linear view: `iterator` observes each element once and keeps
  // nothing, where `toLazyList` memoises a cell — and a synchronised
  // lazy state — per element for a value read once
  // (stream-fold-via-iterator). The combinators above answer a
  // LazyList and keep the bridge: a memoised stream is their contract.

  /** fold all the elements strictly (diverges on an infinite stream) */
  def foldLeft[B](z: B)(op: (B, A) => B): B = s.iterator.foldLeft(z)(op)

  /** run f on every element */
  def foreach(f: A => Unit): Unit = s.iterator.foreach(f)

  /** the first element, if any */
  def headOption: Option[A] = s.uncons.map(_._1)

  /** the first element satisfying p (stops as soon as it is found) */
  def find(p: A => Boolean): Option[A] = s.iterator.find(p)

  /** is there an element satisfying p (stops as soon as one is found) */
  def exists(p: A => Boolean): Boolean = s.iterator.exists(p)

  /** do all elements satisfy p (stops at the first that does not) */
  def forall(p: A => Boolean): Boolean = s.iterator.forall(p)

  /** all the elements, strictly */
  def toList: List[A] = s.iterator.toList

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

  /** a fold that stops (specs/fold-until.md): `Writer.foldUntil` with
   * the row's evidence found here, so the caller passes only the fold
   * — `countdown(n).foldUntil(using FoldUntil.find(p))` */
  def foldUntil[S, R](using fo: FoldUntil[W, S, R]): R =
    !.run(Writer.foldUntil[W, S, A, R, Nothing](a)(using summon)(using summon, fo))

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

/**
 * The effectful program's stopping fold, in a block of its own: an
 * explicit `(using fo)` at the call site is matched against the
 * EXTENSION's using clause when the extension has one, so the block
 * above cannot carry it — here the `Handler[G]` sits in the method's
 * own clause, after the fold, and `a.foldUntil(using fo)` reads as the
 * pure program's does.
 */
extension [W, A, G[+_]](a: A ! Writer % W + G)
  /** a fold that stops (specs/fold-until.md): `Writer.foldUntil`, its
   * forwarded G run by the Handler in scope — the effectful twin of
   * the pure program's `foldUntil` above, one `using` for the caller */
  def foldUntil[S, R](using fo: FoldUntil[W, S, R])(using Handler[G]): R =
    Writer.foldUntil[W, S, A, R, G](a)(using summon)(using summon, fo).runWith
