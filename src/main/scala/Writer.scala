package okay

/**
 * The Writer effect IS a stream: telling w emits w, and a writer
 * program is already the stream — run is a fold over it, toLazyList
 * consumes it lazily, no reinterpretation pass anywhere. The element
 * type is kept SEPARATE from the program's answer: `A ! Writer % W`
 * reads "computes A, telling W". (Producer is the diagonal cousin: it
 * emits its own answers, one type for both.)
 *
 * The operation is a GADT with ONE constructor, and both halves of
 * that matter.
 *
 * One constructor, `Say(w): Writer[W, Unit]`, because a tell answers
 * NOTHING — it emits a value, it does not produce one. Anything a
 * caller wants back it says explicitly (`tell(w).map(_ => w)`).
 *
 * A GADT, because the constructor is what makes that answer type
 * RECOVERABLE. Under a `Bind` the answer type is existential; matching
 * `Say(w)` refines it to `Unit`, so resuming the continuation is
 * `k(())` and asserts nothing. The previous encoding was an identity
 * signature — `opaque type Writer[W, +A] = W`, the operation IS the
 * told value, no node at all — which cost nothing to build and could
 * not recover the answer type afterwards, so twelve sites asserted it.
 * Measured, the wrapper costs 25% of a build-and-fold (59.8 -> 75.0us
 * per 10k tells); it buys back every one of those assertions and one
 * real limitation besides: a told String used to be just a String, so
 * a row could only forward effects whose operations were
 * class-distinct from W. A `Say` is class-distinct from everything.
 * docs/existentials.md has the five encodings tried before this one.
 */
enum Writer[W, +A]:
  case Say(w: W) extends Writer[W, Unit]

/**
 * An operation IS its element — now by pattern match rather than by
 * representation, so it is total and asserts nothing.
 */
def out[W, A](w: Writer[W, A]): W = w match
  case Writer.Say(x) => x

object Writer {

  /** the operation: telling w, answering nothing — the ONLY
   * constructor, which is what makes the answer type recoverable */
  inline def apply[W](w: W): Writer[W, Unit] = Say(w)

  /** tell w: emit it as an operation, which answers NOTHING */
  inline def tell[W](w: W): Unit ! Writer % W = effect(Writer(w))


  import scala.annotation.tailrec
  import !.*

  /**
   * Fold everything told into any Fold algebra, forwarding the
   * effects F. Like State.handle, a bespoke tail-recursive loop — the
   * accumulator has to be threaded through the loop itself, which an
   * answer-polymorphic relay handler cannot hold.
   */
  def fold[W, S, A, F[+_]](a: A ! Writer % W + F)
                          (using TypeableK[Writer % W], Fold[W, S]): (S, A) ! F =
    val K = summon[Fold[W, S]]
    // GADT refinement needs a stable path, not an expression: bound to
    // a val it gives `S` back from each test, so none of this casts
    K match
      // the same dispatch `Chunks.fold` makes, for the same reason: a
      // fold that arrives as data has no step to inline, so the one
      // thing left is to ask what its accumulator is. GADT refinement
      // gives `S` back from each type test, so none of this casts.
      case l: Fold.OfLong[W @unchecked] => foldWith[W, Long, A, F](a)(l.initLong)((s, w) => l.addLong(s, w))
      case i: Fold.OfInt[W @unchecked] => foldWith[W, Int, A, F](a)(i.initInt)((s, w) => i.addInt(s, w))
      case d: Fold.OfDouble[W @unchecked] => foldWith[W, Double, A, F](a)(d.initDouble)((s, w) => d.addDouble(s, w))
      case b: Fold.OfBoolean[W @unchecked] => foldWith[W, Boolean, A, F](a)(b.initBoolean)((s, w) => b.addBoolean(s, w))
      case _ => foldWith(a)(K.init)((s, w) => K.add(s, w))

  /**
   * The loop itself, with the step taken at the call site.
   *
   * `inline` so the four dispatched branches above each beta-reduce
   * their step into the loop body rather than calling through a
   * `Function2`, whose `apply` erases `(Object, Object)Object` and
   * would put back exactly the boxing the dispatch removed. That trap
   * cost 27.5us against 7.8 when it was first written without inline
   * in `Fold.long`, so it is worth spelling out.
   */
  inline def foldWith[W, S, A, F[+_]](a: A ! Writer % W + F)(z: S)
                                     (inline step: (S, W) => S)
                                     (using TypeableK[Writer % W]): (S, A) ! F = {
    def _loop(s: S)(x: A ! Writer % W + F): (S, A) ! F = loop(s)(x)

    @tailrec def loop(s: S)(x: A ! Writer % W + F): (S, A) ! F = (x.resume: @unchecked) match
      case Pure(a) => Pure((s, a))
      case Effect(e) => <|>[Writer % W, F](e) match
        // matching the constructor refines the answer type to Unit:
        // the program ends here, and a tell ends it with nothing
        case Left(Say(v)) => Pure((step(s, v), ()))
        case Right(e) => Effect(e).map((s, _))
      case Bind(Effect(e), k) => <|>[Writer % W, F](e) match
        // and here it refines the CONTINUATION's domain, so this is
        // an ordinary call and not an assertion
        case Left(Say(v)) => loop(step(s, v))(k(()))
        case Right(e) => Effect(e).flatMap(x => _loop(s)(k(x)))

    loop(z)(a)
  }

  /** collect everything told, in order, forwarding the effects F */
  def run[W, A, F[+_]](a: A ! Writer % W + F)
                      (using TypeableK[Writer % W]): (Seq[W], A) ! F =
    fold[W, Seq[W], A, F](a)

  /**
   * The observation of the writer as codata: the same shape as
   * Stream.uncons but with a richer functor — Either[A, (W, rest)]
   * instead of Option[(W, rest)]. The told values come out one by one
   * through Right, on demand; when they end, Left carries the
   * program's own answer (nothing is lost — by then every told value
   * has already been observed). run/fold are this loop with a Fold
   * accumulating the Rights.
   */
  def uncons[W, A](a: A ! Writer % W): Either[A, (W, A ! Writer % W)] = (a.resume: @unchecked) match
    case Free.Pure(a) => Left(a)
    case Effect(Say(w)) => Right((w, Free.Pure(())))
    case Bind(Effect(Say(w)), k) => Right((w, k(())))

  /**
   * The same observation for a writer program performing ARBITRARY
   * effects G alongside its telling: the next told value arrives
   * inside G — the G-operations met on the way are carried into the
   * answer (deferred, not run). Any structured effect handler (State,
   * Reader, Throws, ...) forwards the telling, so it can be run over
   * the program FIRST — handlers are stream transformers — and the
   * Handler-able residue (Async, say) is what the consumer pays at
   * each pull. G is split from the told values by its runtime class.
   */
  def uncons[W, A, G[+_] : TypeableK](a: A ! Writer % W + G)
  : Either[A, (W, A ! Writer % W + G)] ! G = (a.resume: @unchecked) match
    case Free.Pure(a) => okay.pure(Left(a))
    case Effect(e) => <|>[G, Writer % W](e) match
      case Left(g) => Effect(g).map(Left(_))
      case Right(Say(w)) => okay.pure(Right((w, Free.Pure(()))))
    case Bind(Effect(e), k) => <|>[G, Writer % W](e) match
      case Left(g) => Effect(g).flatMap(x => uncons[W, A, G](k(x)))
      case Right(Say(w)) => okay.pure(Right((w, k(()))))
}

/** the diagonal writer: it tells its own answers, like Producer but
 * with the element type visible in the signature */
type Teller[A] = A ! Writer % A

/**
 * The third corner of the triangle: generate materializes into the
 * diagonal writer too — one unfold, three carriers (LazyList by pure
 * laziness, Producer by identity operations, Teller by typed ones).
 * put is tell as a delimited-control operation: shift captures the
 * continuation and binds it after the emission.
 */
given Put[Teller] with
  final override inline def put[A](a: A): A /> Teller[A] =
    shift(k => Writer.tell(a).flatMap(_ => k(a)))

/**
 * A writer program is a stream of its told values: the same
 * observation as Writer.uncons with the answer forgotten (Left becomes
 * the end) — an infinite teller unfolds on demand like any stream.
 */
given [A]: Stream[[W] =>> A ! Writer % W, Pure] = new:
  def uncons[W](s: A ! Writer % W): Option[(W, A ! Writer % W)] ! Pure =
    pure(Writer.uncons(s).toOption)

/**
 * Writer's split is COMPLETE, and now unconditionally so.
 *
 * Two tests, in order: is this a writer operation at all (its own
 * class, distinct from every other value in the row), and if so, is it
 * THIS writer's (the told value's class, which separates
 * `Writer % String + Writer % Int` — `TestRowIdentity` asserts they
 * route correctly).
 *
 * The first test is what the identity encoding could not make. There
 * an operation WAS its element, so a told String and a bare String
 * from any other effect were the same runtime value, and the split
 * came with a caveat: forward only effects whose operations are
 * class-distinct from W. A `Say` is class-distinct from everything,
 * and the caveat is gone.
 */
given writerK[W](using t: scala.reflect.Typeable[W]): TypeableK[Writer % W] = new:
  def unapply[A](x: Any): Option[x.type & Writer[W, A]] = x match
    case s: Writer.Say[?, ?] =>
      t.unapply(s.w).map(_ => x.asInstanceOf[x.type & Writer[W, A]])
    case _ => None

