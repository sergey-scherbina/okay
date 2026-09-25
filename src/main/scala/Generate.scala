package okay

/**
 * Generators from delimited control: take is the input of a loop
 * iteration, put is the output, and generate ties them into an
 * unfolding stream. One program — two semantics, by the Put instance:
 * LazyList materializes the stream by laziness (the continuation is
 * captured in the #:: tail, no effect runtime at all), and Producer
 * materializes it by effects (each put is an operation of the freer
 * tree, stepped by next and interpreted by a Handler, e.g.
 * Producer.log).
 */

/**
 * A Loop R means (A => R) => (A => R): the body of an open-recursive
 * function A => R, whose continuation is the recursive call itself.
 * take is the argument of the current iteration.
 * loop ties the knot, i.e. it is the fixpoint.
 */
/** the aesthetic alias of Loop */
infix type <<[A, R] = Loop[A, R]
infix type Loop[A, R] = Cont[A, R, A => R]

/**
 * Run a loop from this seed.
 *
 * The `NotGiven` guard is not decoration (named-tuple-unblock,
 * 2026-09-12): this extension offers an `apply` on EVERY type, and a
 * named tuple's field access desugars to an apply BY INDEX, so
 * without the guard `import okay.*` made `t.route` fail with
 * "Found: (0 : Int)" and disabled a stable language feature for
 * anyone importing this package. Declining named tuples lets the
 * selection fall through to the compiler's own, and costs nothing
 * else: `seed(body)` still works for any other seed, a plain tuple
 * included. Guarding on `Tuple` instead does NOT work — a named tuple
 * is not `<:<` one, so the guard passes and the extension captures
 * the selection anyway. BUGS.md, `universal-apply-blocks-named-tuples`.
 */
extension [A](a: A)(using scala.util.NotGiven[A <:< NamedTuple.AnyNamedTuple])
  inline def apply[R](f: A Loop R): R = loop(f)(a)
/** the argument of the current iteration: shift identity captures the loop context */
inline def take[A, R]: A Loop R = shift(identity)

/** the first n elements of a stream (lives here to overload with the
 * Loop take above — toplevel overloads must share a file) */
extension [S[_], F[+_], A](s: S[A])(using Stream[S, F], Handler[F])
  def take(n: Int): LazyList[A] = s.toLazyList.take(n)
/** tie the knot: the fixpoint of the loop body, with a memoized stepper */
inline def loop[A, R](f: A Loop R): A => R =
  lazy val step: A => R = f / (step(_))
  step

/**
 * The interface of an S that accepts produced values.
 *
 * NOT diagonal (put-de-diagonal, 2026-09-19): the answer is `Unit`,
 * decoupled from the element `W`. The obvious signature —
 * `put[A](a: A): A /> F[A]` — forces the carrier to answer with the
 * value it was just told, which is why no real seam ever took one: a
 * real source is `Source[W] = Unit ! Writer % W + Async`, answer
 * `Unit`, element `W`. Every instance survives the change (the
 * continuation is now resumed with `()` instead of the element), and
 * `Source` gains one it structurally could not have had before — its
 * answer is never the element, so no diagonal `Put[Source]` instance
 * could exist.
 */
trait Put[S[_]]:
  def put[W](w: W): Unit /> S[W]

/** put a value through the instance of S */
inline def put[W, S[_] : Put as S](w: W): Unit /> S[W] = S.put(w)

/** unfold: take the seed, put f(a), continue with the seed g(a) */
inline def generate[A, B, F[_] : Put](a: A)(f: A => B)
                                     (g: A => A): F[B] = a:
  for a <- take[A, F[B]]; _ <- put(f(a)) yield g(a)

/** generate, materialized by laziness */
inline def generateLazy[A, B](a: A)(f: A => B)
                             (g: A => A): LazyList[B] =
  generate[A, B, LazyList](a)(f)(g)

/** put captures the continuation in the lazy tail */
given Put[LazyList] with
  final override inline def put[W](w: W): Unit /> LazyList[W] =
    shift(w #:: _(()))

/** the identity signature: an operation is the value it produces */
type Produce[A] = Id[A]

/** the freer monad over Produce: a computation that emits as it goes */
type Producer[A] = A ! Produce

/** a chunk: an immutable indexed batch of elements (O(1) index, no
 * copy over the generation array). It is an ALIAS for a standard
 * collection, which is why it stayed in the core when the chunked
 * machinery left for okay-stream (core-modules stage 1): `Chunks[A] =
 * Producer[Chunk[A]]` is over there, and so is everything that fills
 * one, but `Producer.concat` below is typed on this and five modules
 * call it. */
type Chunk[+A] = scala.collection.immutable.ArraySeq[A]

/**
 * Emit a value as an effect operation.
 *
 * Typed at `Produce` alone. In a wider row — `Blob.put` asks for
 * `Produce + Async` — say `produce(a).plus[Async]` or
 * `produce(a).at[Produce + Async]`: Row's zero-cost coerce, not
 * the tree walk `!.widen` makes.
 *
 * What NOT to write there is `pure(a)`. It type-checks at every row
 * and produces nothing: a producer's `Pure` is its END, and the
 * stream instance below reads it as `None`. The element type sits in
 * the answer position either way, so only the name tells the two
 * apart — measured the hard way in okay-watch (a zero-byte object
 * under the right key, blob-source-seam).
 */
inline def produce[A](a: A): Producer[A] = effect(a)

/**
 * The ANSWER of a produce operation is the value it produced.
 *
 * `Produce` is the identity signature, and `produce(a): Producer[A]`
 * is its only injector, so the answer type equals the element type
 * for every operation that can exist. The type system does not record
 * that — the answer is phantom on purpose — so this is the one place
 * that asserts it, and the walks over produced streams take it from
 * here rather than each writing an `asInstanceOf`.
 *
 * The twin of `okay.answer` for `Writer`, for the same reason and at
 * the same price: nothing at run time, and one place to be wrong.
 */
def produced[A](e: Any): A = e.asInstanceOf[A]

/** put suspends the value as an effect operation, and drops the echo
 * a diagonal Producer answer would have given — no caller used it */
given Put[Producer] with
  final override inline def put[W](w: W): Unit /> Producer[W] =
    shift(k => produce(w).flatMap(_ => k(())))

object Producer {

  import scala.util.chaining.*

  /**
   * Run every produced element through `f`, perform G as before, and
   * KEEP the producer's answer — which `uncons` would lose at its
   * None, and which for `Blob.get` is the outcome.
   *
   * `W` is the element type, named separately from the answer `A`
   * for the reason `Source.fromProducer` gives: the identity
   * signature cannot tell them apart. Three places hand-rolled this
   * walk before it was one function (okay-blob's Backup and its
   * contract suite, okay-watch's restore); a fourth would have too.
   */
  /**
   * Fold the produced elements, perform G as before, KEEP the answer.
   * The general form `each` is a special case of, and tail-recursive
   * across produced elements the way `Writer.collect` is: a walk that
   * re-enters through `flatMap` only when a G operation has to be
   * forwarded. The drains it replaced recursed through `map` per
   * chunk — `drain(rest).map(c.toVector ++ _)` — which is a closure
   * per chunk held until the end.
   */
  def fold[W, S, A, G[+_] : TypeableK](p: A ! Produce + G)(z: S)(f: (S, W) => S): (S, A) ! G =
    import !.*
    import scala.annotation.tailrec
    def again(acc: S)(x: A ! Produce + G): (S, A) ! G = loop(acc)(x)
    @tailrec def loop(acc: S)(x: A ! Produce + G): (S, A) ! G =
      (x.resume: @unchecked) match
        case Free.Return(a) => pure((acc, a))
        case Inject(e) => split[G, Produce](e)
          (g => Inject(g).map(a => (acc, a)): (S, A) ! G)
          (w => pure((f(acc, produced[W](w)), produced[A](w))))
        case Bind(Inject(e), k) => split[G, Produce](e)
          (g => Inject(g).flatMap(x => again(acc)(k(x))))
          (w => loop(f(acc, produced[W](w)))(k(w)))
    loop(z)(p)

  /**
   * `fold` with a stop (specs/fold-until.md): the same split walk with
   * an early `pure(end(s))` the moment the state is done — the
   * `Bind(Inject(e), k)` arm does not call `k` then, so nothing past
   * the satisfying production is built and a `G` operation that would
   * have followed it is never performed. Answers `R` alone: a fold that
   * stopped early never saw the producer's answer (`Writer.foldUntil`
   * makes the same choice for the same reason).
   */
  def foldUntil[W, S, R, A, G[+_] : TypeableK](p: A ! Produce + G)(using K: FoldUntil[W, S, R]): R ! G =
    import !.*
    import scala.annotation.tailrec
    def again(s: S)(x: A ! Produce + G): R ! G = loop(s)(x)
    @tailrec def loop(s: S)(x: A ! Produce + G): R ! G =
      if K.done(s) then pure(K.end(s))
      else (x.resume: @unchecked) match
        case Free.Return(_) => pure(K.end(s))
        case Inject(e) => split[G, Produce](e)
          (g => Inject(g).map(_ => K.end(s)): R ! G)
          (w => pure(K.end(K.add(s, produced[W](w)))))
        case Bind(Inject(e), k) => split[G, Produce](e)
          (g => Inject(g).flatMap(x => again(s)(k(x))))
          (w => loop(K.add(s, produced[W](w)))(k(w)))
    loop(K.init)(p)

  /**
   * A producer of CHUNKS as one Vector of their elements — the drain
   * that six modules had each written by hand (producer-drains),
   * every one summoning the same `Stream` instance and spelling the
   * same `go`. The answer is dropped, since for a `Chunk[X] !
   * (Produce + G)` it is phantom; `fold` keeps it where it is not.
   */
  def concat[X, G[+_] : TypeableK](p: Chunk[X] ! Produce + G): Vector[X] ! G =
    fold[Chunk[X], Vector[X], Chunk[X], G](p)(Vector.empty)((acc, c) => acc ++ c).map(_._1)

  /**
   * Run `f` on every production, G performed as before. A checked
   * loop: `split` is INLINE, so the produced arm's `loop(k(w))` is in
   * tail position, and a forwarded operation resumes through `again`
   * from inside flatMap. okay2, whose `split` is an ordinary method
   * taking closures, had this walk as one recursive method and
   * overflowed at 200 000 productions (okay2-split-at-rest);
   * TestFoldUntil pins it here.
   */
  def each[W, A, G[+_] : TypeableK](p: A ! Produce + G)(f: W => Unit): A ! G =
    import !.*
    def again(x: A ! Produce + G): A ! G = loop(x)
    @tailrec def loop(x: A ! Produce + G): A ! G =
      (x.resume: @unchecked) match
        case Free.Return(a) => pure(a)
        case Inject(e) => split[G, Produce](e)
          (g => Inject(g): A ! G)
          (w => { f(produced[W](w)); pure(produced[A](w)) })
        case Bind(Inject(e), k) => split[G, Produce](e)
          (g => Inject(g).flatMap(x => again(k(x))): A ! G)
          (w => { f(produced[W](w)); loop(k(w)) })
    loop(p)

  /** a Handler printing each produced value on the way through */
  def log(prefix: String = "", suffix: String = "\n"): Handler[Produce] = new:
    inline def handle[A](a: A): A = a.tap(_.pipe(prefix + _ + suffix).tap(print))

}

/**
 * A producer is a stream: it observes by stepping its next operation —
 * one op per element, on demand, no further than the observer asks
 * (the eager fronts crash here, see compare/TestLaziness). The end is
 * its Pure, observed as None. The cast is sound for producers built
 * by put/produce at one element type: Produce is the identity
 * signature, an operation IS its element, but the element types of
 * the operations are erased by the identity — the value type A is the
 * only witness left.
 */
given Stream[Producer, okay.Pure] with
  import !.*
  import scala.annotation.tailrec

  def uncons[A](p: Producer[A]): Option[(A, Producer[A])] ! okay.Pure = pure((p.resume: @unchecked) match
    case Free.Return(_) => None
    case Inject(e) => Some((e, Free.Return(e)))
    case Bind(Inject(e), k) => Some((produced[A](e), k(e))))

  /** the specialized linear view: a direct walk of the freer tree —
   * no Option, no tuple per element (measured; the generic default
   * pays both). What remains per element is the stepping itself. */
  override def iterator[A](p: Producer[A])(using Handler[okay.Pure]): Iterator[A] =
    new Iterator[A]:
      private var cur: Producer[A] = p
      private var ready = false
      private var ended = false
      private var elem: A = scala.compiletime.uninitialized

      @tailrec private def advance(): Unit = cur match
        case Free.Return(_) => ended = true
        case Inject(e) =>
          elem = e
          ready = true
          cur = Free.Return(e)
        case Bind(Inject(e), k) =>
          elem = produced[A](e)
          ready = true
          cur = k(e)
        case _ =>
          cur = cur.resume
          advance()

      def hasNext: Boolean =
        if !ready && !ended then advance()
        ready

      def next(): A =
        if !hasNext then throw java.util.NoSuchElementException("empty producer")
        ready = false
        elem

/** a producer folds as a stream without a result (push consumption) */
given Foldable[Producer] with
  def fold[A, S](p: Producer[A])(using f: Fold[A, S]): S = Stream.fold(p)
  def foldUntil[A, S, R](p: Producer[A])(using fo: FoldUntil[A, S, R]): R = Stream.foldUntil(p)

/**
 * An EFFECTFUL producer is a stream in the effect G: the program
 * emits its elements (the identity Produce side) and performs G along
 * the way — uncons steps to the next element, carrying the performed
 * G-operations in its answer. With G = Async this is the asynchronous
 * stream: the next element may have to be awaited, and on Loom the
 * consumer's Handler[Async] just blocks a virtual thread for it. G is
 * split from the elements by its runtime class (TypeableK), so G's
 * operations must be class-distinct from the element values.
 */
given [G[+_] : TypeableK]: Stream[[A] =>> A ! Produce + G, G] with
  import !.*

  def uncons[A](p: A ! Produce + G): Option[(A, A ! Produce + G)] ! G = (p.resume: @unchecked) match
    case Free.Return(_) => pure(None)
    // `split`, not `<|>`: no Either per element (split-over-either)
    case Inject(e) => split[G, Produce](e)
      (g => Inject(g).map(_ => None): Option[(A, A ! Produce + G)] ! G)
      (w => pure(Some((produced[A](w), Free.Return(produced[A](w))))))
    case Bind(Inject(e), k) => split[G, Produce](e)
      (g => Inject(g).flatMap(x => uncons(k(x))): Option[(A, A ! Produce + G)] ! G)
      (w => pure(Some((produced[A](w), k(w)))))

  /**
   * The specialized linear view, mirroring `writerStreamIn`'s own
   * (Writer.scala) and the pure instance's above: no `Option`, no
   * `Either`, no program built and run per step — the DEFAULT
   * `Iterator.unfold(s)(uncons(_).runWith)` pays all three per
   * element. A forwarded `G`-operation is answered by `Handler[G].
   * handle` directly (producer-effectful-stream-iterator; the writer
   * twin measured 6.29 -> 5.43 us and 32,952 -> 12,688 B/op on 157
   * chunks for the same move). The `produced` casts are the identity
   * signature's own, as in `uncons` above.
   */
  override def iterator[A](p: A ! Produce + G)(using H: Handler[G]): Iterator[A] =
    import scala.annotation.tailrec
    new Iterator[A]:
      private var cur: A ! Produce + G = p
      private var ready = false
      private var ended = false
      private var elem: A = scala.compiletime.uninitialized

      @tailrec private def advance(): Unit = cur match
        case Free.Return(_) => ended = true
        case Inject(e) =>
          split[G, Produce](e)(
            g => { val _ = H.handle(g); ended = true }
          )(
            w => { elem = produced[A](w); ready = true; ended = true }
          )
        case Bind(Inject(e), k) =>
          split[G, Produce](e)(
            g => { cur = k(H.handle(g)); advance() }
          )(
            w => { elem = produced[A](w); ready = true; cur = k(w) }
          )
        case _ =>
          cur = cur.resume
          advance()

      def hasNext: Boolean =
        if !ready && !ended then advance()
        ready

      def next(): A =
        if !hasNext then throw java.util.NoSuchElementException("empty producer")
        ready = false
        elem

import scala.math.Numeric.Implicits.given
import scala.annotation.tailrec

/** the naturals: 0, 1, 2, ... */
inline def nats[N: Numeric as N, F[_] : Put]: F[N] =
  generate(N.zero)(identity)(_ + N.one)

/** the Fibonacci numbers */
inline def fibs[N: Numeric as N, F[_] : Put]: F[N] =
  generate((N.zero, N.one))(_._1):
    (x, y) => (y, x + y)
