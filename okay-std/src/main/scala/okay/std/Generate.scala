package okay.std

import okay.{!, +, Answers, Handler, Id, Pure, Run, TypeableK, effect, pure}
import okay.core.{+:, Eff, Paths, Row, Split}
import okay.{Cps, />>}
import okay.{Fold, FoldUntil, Foldable}
import okay.one.Cont
import okay.one.handler.Stateful
import okay.one.row.{In, Pure as OnePure}

/**
 * Generators from delimited control: take is the input of a loop
 * iteration, put is the output, and generate ties them into an
 * unfolding stream. One program — two semantics, by the Put instance:
 * LazyList materializes the stream by laziness (the continuation is
 * captured in the #:: tail, no effect runtime at all), and Producer
 * materializes it by effects (each put is an operation of the
 * program, stepped by the kernel's `Machine.Head` and interpreted by an
 * Answers, e.g. Producer.log).
 */

/**
 * A Loop R means (A => R) => (A => R): the body of an open-recursive
 * function A => R, whose continuation is the recursive call itself.
 * take is the argument of the current iteration.
 * loop ties the knot, i.e. it is the fixpoint.
 */
/** the aesthetic alias of Loop */
infix type <<[A, R] = Loop[A, R]
infix type Loop[A, R] = Cps[A, R, A => R]

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
inline def take[A, R]: A Loop R = Cps.shift(identity)

/** the first n elements of a stream (lives here to overload with the
 * Loop take above — toplevel overloads must share a file) */
extension [S[_], F[_], A](s: S[A])(using Stream[S, F], Answers[F])
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
 * `put[A](a: A): A />> F[A]` — forces the carrier to answer with the
 * value it was just told, which is why no real seam ever took one: a
 * real source is `Source[W] = Unit ! Writer % W + Async`, answer
 * `Unit`, element `W`. Every instance survives the change (the
 * continuation is now resumed with `()` instead of the element), and
 * `Source` gains one it structurally could not have had before — its
 * answer is never the element, so no diagonal `Put[Source]` instance
 * could exist.
 */
trait Put[S[_]]:
  def put[W](w: W): Unit />> S[W]
  /** `generate`'s unfold: put `f(a)`, go on from `g(a)` — by default the `Cps` loop over `put` (`take` the seed,
   * the knot tied by `loop`); an instance whose `S` is a program writes it as one, a step a bind (no `Cps` run an
   * element: the kernel's `Cps` gives a shift its `k` as a run of its own) */
  def unfold[A, B](a: A)(f: A => B)(g: A => A): S[B] = a(for x <- take[A, S[B]]; _ <- put(f(x)) yield g(x))

/** put a value through the instance of S */
inline def put[W, S[_] : Put as S](w: W): Unit />> S[W] = S.put(w)

/** unfold: take the seed, put f(a), continue with the seed g(a) */
inline def generate[A, B, F[_] : Put as F](a: A)(f: A => B)
                                          (g: A => A): F[B] = F.unfold(a)(f)(g)

/** generate, materialized by laziness */
inline def generateLazy[A, B](a: A)(f: A => B)
                             (g: A => A): LazyList[B] =
  generate[A, B, LazyList](a)(f)(g)

/** put captures the continuation in the lazy tail */
given Put[LazyList] with
  final override inline def put[W](w: W): Unit />> LazyList[W] =
    Cps.shift(w #:: _(()))
  override def unfold[A, B](a: A)(f: A => B)(g: A => A): LazyList[B] = LazyList.iterate(a)(g).map(f)

/** the identity signature: an operation is the value it produces */
type Produce[A] = Id[A]

/** a program over Produce: a computation that emits as it goes */
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
 * Typed at `Produce` alone. Where more effects are needed — `Blob.put`
 * asks for `Produce + Async` — say `produce(a).at[Produce + Async]`,
 * or bind it among them: a program needing fewer effects fits where
 * more are expected.
 *
 * What NOT to write there is `pure(a)`. It type-checks at every row
 * and produces nothing: a producer's `Pure` is its END, and the
 * stream instance below reads it as `None`. The element type sits in
 * the answer position either way, so only the name tells the two
 * apart — measured the hard way in okay-watch (a zero-byte object
 * under the right key, blob-source-seam).
 */
inline def produce[A](a: A): Producer[A] = effect[Produce, A](a)

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

/** a `Produce` test for code that still splits by class (okay-freer's walks) — `Produce` is the identity
 * signature, so its operations are the ones that are not the rest's; the root's programs find it by its path */
def producing[G[+_]](using g: TypeableK[G]): TypeableK[Produce] = new TypeableK[Produce]:
  def test(x: Any): Boolean = !g.test(x)

/** put suspends the value as an effect operation, and drops the echo
 * a diagonal Producer answer would have given — no caller used it */
given Put[Producer] with
  final override inline def put[W](w: W): Unit />> Producer[W] =
    Cps.shift(k => produce(w).flatMap(_ => k(())))
  /** each element produced, the next step deferred into the bind: the machine's loop, no host frame */
  override def unfold[A, B](a: A)(f: A => B)(g: A => A): Producer[B] =
    produce(f(a)).flatMap(_ => okay.defer(unfold(g(a))(f)(g)))

/** what a stepped production hands over — the value — and what it answers: the same value */
private[std] object Producing:
  /** the key of `Produce`: its name's (an identity effect has no constructor) */
  val key: AnyRef = summon[okay.KeyOf[Produce]].ref
private[std] final class Producing[W] extends Stepping.Told[Produce, W]:
  def element[X](op: Produce[X]): W = produced[W](op)
  def answer[X](op: Produce[X]): X = op

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
   * Fold the produced elements, perform the rest of the effects as
   * before, KEEP the answer — the kernel's `Stateful`: the accumulator
   * a register of the machine, each production answered in place with
   * itself. The drains it replaced recursed through `map` per chunk —
   * `drain(rest).map(c.toVector ++ _)` — which is a closure per chunk
   * held until the end.
   */
  def fold[W, S, A, Es[_]](p: A ! Es)(z: S)(f: (S, W) => S)(using s: Split[Es, Produce]): (S, A) ! s.Rest =
    Run.install[Produce, A, (S, A), Es, s.Rest](p)(new Stateful[Produce, OnePure, S, A, (S, A)]:
      def initial: S = z
      def ret(st: S, a: A) = Cont.pure((st, a))
      def value[X](st: S, op: Produce[X]): X = op
      def next[X](st: S, op: Produce[X]): S = f(st, produced[W](op)))

  /**
   * `fold` with a stop (specs/fold-until.md): the program STEPPED (the
   * kernel's `Machine.Head`), the moment the state is done its rest dropped —
   * nothing past the satisfying production runs, and an operation that
   * would have followed it is never performed. Answers `R` alone: a
   * fold that stopped early never saw the producer's answer
   * (`Writer.foldUntil` makes the same choice for the same reason).
   */
  def foldUntil[W, S, R, A, Es[_]](p: A ! Es)(using s: Split[Es, Produce], K: FoldUntil[W, S, R]): R ! s.Rest =
    if K.done(K.init) then pure(K.end(K.init))
    else Run.of(Cont.defer(Stepping.until(Stepping.start(p), Producing.key, K.init, Producing[W](), K)))

  /**
   * A producer of CHUNKS as one Vector of their elements — the drain
   * that six modules had each written by hand (producer-drains),
   * every one summoning the same `Stream` instance and spelling the
   * same `go`. The answer is dropped, since for a `Chunk[X] !
   * (Produce + G)` it is phantom; `fold` keeps it where it is not.
   */
  def concat[X, Es[_]](p: Chunk[X] ! Es)(using s: Split[Es, Produce]): Vector[X] ! s.Rest =
    fold[Chunk[X], Vector[X], Chunk[X], Es](p)(Vector.empty)((acc, c) => acc ++ c).map(_._1)

  /**
   * Run `f` on every production, the rest of the effects performed as
   * before: the kernel's `Answering`, each production answered in
   * place with itself after `f` has seen it — no loop of its own, so
   * none to overflow (okay2's walk overflowed at 200 000 productions,
   * okay2-split-at-rest; TestFoldUntil pins it).
   */
  def each[W, A, Es[_]](p: A ! Es)(f: W => Unit)(using s: Split[Es, Produce]): A ! s.Rest =
    p.handle(Handler.answering[Produce]([X] => (w: Produce[X]) => { f(produced[W](w)); w }))

  /** an Answers printing each produced value on the way through */
  def log(prefix: String = "", suffix: String = "\n"): Answers[Produce] = new:
    inline def handle[A](a: A): A = a.tap(_.pipe(prefix + _ + suffix).tap(print))

}

/**
 * A producer is a stream: it observes by stepping its next operation
 * (the kernel's `Machine.Head`) — one op per element, on demand, no further
 * than the observer asks (the eager fronts crash here, see
 * compare/TestLaziness). The end is its value, observed as None. The
 * element is the operation itself: Produce is the identity signature,
 * an operation IS its element, but the element types of the operations
 * are erased by the identity — the value type A is the only witness
 * left (`produced`).
 */
given Stream[Producer, Pure] with
  def uncons[A](p: Producer[A]): Option[(A, Producer[A])] ! Pure =
    pure(Stepping.nextPure(Stepping.start(p), Producing.key, Producing[A]()).toOption)

  /** the specialized linear view: the program stepped production by
   * production — no Option, no tuple per element (measured; the
   * generic default pays both). What remains per element is the
   * stepping itself. */
  override def iterator[A](p: Producer[A])(using Answers[Pure]): Iterator[A] =
    Stepping.Iter[Produce, A, A](() => Stepping.start(p), Producing.key, Producing[A](), Stepping.Other.none)

/** a producer folds as a stream without a result (push consumption) */
given Foldable[Producer] with
  def fold[A, S](p: Producer[A])(using f: Fold[A, S]): S = Stream.fold(p)
  def foldUntil[A, S, R](p: Producer[A])(using fo: FoldUntil[A, S, R]): R = Stream.foldUntil(p)

/**
 * An EFFECTFUL producer is a stream in the effect G: the program
 * emits its elements (the identity Produce side) and performs G along
 * the way — uncons steps to the next element, performing the
 * G-operations met on the way where it runs. With G = Async this is
 * the asynchronous stream: the next element may have to be awaited,
 * and on Loom the consumer's Answers[Async] just blocks a virtual
 * thread for it. A production and a G-operation are told apart by
 * their PATHS, not by class: G's operations need not be class-distinct
 * from the element values any more. `G` is ONE effect (a place of the
 * row), as the `Answers[G]` its consumers run it by.
 */
given [G[_]](using okay.KeyOf[G]): Stream[[A] =>> A ! Produce + G, G] with
  def uncons[A](p: A ! Produce + G): Option[(A, A ! Produce + G)] ! G =
    Run.of[Either[A, (A, A ! Produce + G)], G](Cont.defer(Stepping.next[Produce + G, Produce, A, A](Stepping.start(p), Producing.key, Producing[A]()))).map(_.toOption)

  /**
   * The specialized linear view, mirroring `writerStreamIn`'s own
   * (Writer.scala) and the pure instance's above: no `Option`, no
   * `Either`, no program built and run per step — the DEFAULT
   * `Iterator.unfold(s)(uncons(_).runWith)` pays all three per
   * element. A forwarded `G`-operation is answered by `Answers[G].
   * handle` directly (producer-effectful-stream-iterator; the writer
   * twin measured 6.29 -> 5.43 us and 32,952 -> 12,688 B/op on 157
   * chunks for the same move).
   */
  override def iterator[A](p: A ! Produce + G)(using H: Answers[G]): Iterator[A] =
    Stepping.Iter[Produce, A, A](() => Stepping.start(p), Producing.key, Producing[A](),
      Stepping.Other.answers(H))

import scala.math.Numeric.Implicits.given

/** the naturals: 0, 1, 2, ... */
inline def nats[N: Numeric as N, F[_] : Put]: F[N] =
  generate(N.zero)(identity)(_ + N.one)

/** the Fibonacci numbers */
inline def fibs[N: Numeric as N, F[_] : Put]: F[N] =
  generate((N.zero, N.one))(_._1):
    (x, y) => (y, x + y)
