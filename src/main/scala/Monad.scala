package okay

import scala.annotation.implicitNotFound

/**
 * Robert Atkey. Parameterised notions of computation. (2009)
 * https://bentnib.org/paramnotions-jfp.html
 *
 * Parametrised monad M[A, S, R] represents a computation of value A
 * which changes a state from S to R, i.e. it's indexed by
 * an arrow S -> R in a category S of "states".
 */
trait ParaMonad[M[_, _, _]] {
  // identity: R -> R
  def pure[A, R](a: A): M[A, R, R]

  // composition: (S -> S2) o (S2 -> R) = S -> R
  inline def flatten[A, S, S2, R](m: M[M[A, S, S2], S2, R]): M[A, S, R] =
    m.flatMap(identity)

  extension [A, S, R](m: M[A, S, R])
    /**
     * NOT `inline`, and that is load-bearing: an inline method is
     * final, so no carrier could replace this default — and the
     * default is `flatMap` into a `pure`, which costs a node per
     * element that a carrier able to absorb the function does not
     * need. Measured 2026-09-15 (specs/freer-base.md Results): 96 B
     * per `shift.map(f)` through this default against 40 B through
     * `Shift.mapped`, and +24 B/op with 7-19% on every Fib lane,
     * because the generator maps once per element. The `inline` was
     * there for staging (specs/staged-tagless.md), which prices
     * `pure`/`flatMap`/`shift` chains and not `map`.
     */
    def map[B](f: A => B): M[B, S, R] = m.flatMap(x => pure(f(x)))
    // composition: (S -> R) o (S2 -> S) = S2 -> R
    def flatMap[B, S2](f: A => M[B, S2, S]): M[B, S2, R]
}

/**
 * Every parameterised monad is a family of ordinary monads,
 * one on each diagonal M[*, R, R] (e.g. the Monad of A /> R).
 */
/**
 * A named class with a PUBLIC `P`, not an anonymous `given … with`.
 * The difference is binary compatibility: an `inline` method reaching
 * a given that the anonymous class captured privately makes the
 * compiler synthesize an accessor whose name is unstable across
 * compiler versions, so a downstream JAR compiled against it can
 * break when this library is merely recompiled. Reaching a public
 * member needs no accessor, and the `inline` is kept.
 */
final class DiagonalMonad[M[_, _, _], R](val P: ParaMonad[M])
  extends Monad[[A] =>> M[A, R, R]]:
  override inline def pure[A](a: A): M[A, R, R] = P.pure(a)
  extension [A](m: M[A, R, R])
    override inline def flatMap[B](f: A => M[B, R, R]): M[B, R, R] = P.flatMap(m)(f)

given [M[_, _, _] : ParaMonad as P, R]: Monad[[A] =>> M[A, R, R]] =
  DiagonalMonad[M, R](P)

/**
 * KLEISLI COMPOSITION, and the glyph is `>=>` because that is what
 * the literature calls it (`Control.Monad.>=>`). It was `>>>` until
 * 2026-09-18 (`arrow-glyphs`), which is the ARROW's glyph in the same
 * literature, and holding it here cost more than it looked:
 *
 *   - it had ZERO call sites. Every `>>>` in the tree was either a
 *     `Long` bit shift (`Uid`, `Hlc`, `Sketch`) or a local one;
 *   - TWO test files were hand-rolling their own `>>>` for `Proc`
 *     because the arrow one could not be written while this held the
 *     name;
 *   - and an arrow `>>>` added BESIDE it does not coexist, it
 *     COLLIDES: `A => M[B]` is also a `P[A, B]`, so the arrow
 *     extension wins resolution and then fails to typecheck. The
 *     first cut of `Optic.arrows` had exactly that bug, and
 *     `TestArrowGlyphs` is where it showed.
 *
 * So each keeps the name its own literature gives it: `>=>` composes
 * effectful functions, `>>>` composes arrows (`Optic.arrows`), and a
 * plain function IS an arrow, so `f >>> g` works on one too.
 */
extension [M[_] : Monad, A, B](f: A => M[B])
  infix def >=>[C](g: B => M[C]): A => M[C] = f(_).flatMap(g)

/**
 * Natural transformation
 */
infix type ==>[F[_], G[_]] = [A] => F[A] => G[A]

/** map a function over the structure */
trait Functor[F[_]]:
  def fmap[A, B](a: F[A], f: A => B): F[B]
  extension [A](a: F[A])
    inline def map[B](f: A => B): F[B] = fmap(a, f)

/** lift values and apply lifted functions; fmap derives from pure and app */
@implicitNotFound("no Applicative[${F}].\nOkay's instances ride the given import: `import okay.given` (a bare `import okay.*` does not bring givens).")
trait Applicative[F[_]] extends Functor[F]:
  override def fmap[A, B](a: F[A], f: A => B): F[B] = pure(f).app(a)
  def pure[A](a: A): F[A]
  extension [A, B](f: F[A => B])
    def app(a: F[A]): F[B]
    /** the idiom bracket's own spelling: `pure(f) <*> fa <*> fb` */
    inline def <*>(a: F[A]): F[B] = app(a)

/**
 * Selective applicative functors (Mokhov et al. 2019): declare both
 * branches statically, run at most one — between Applicative (all
 * effects known, all run) and Monad (continuations opaque).
 */
trait Selective[F[_]] extends Applicative[F]:
  extension [A, B](fe: F[Either[A, B]])
    /**
     * BY NAME, and in Scala that is the whole point of the class.
     *
     * "Runs at most one branch" is free in a lazy language; here a
     * branch is an ordinary argument, so passing it by value does the
     * work whether or not it is chosen. Measured the plain way, by a
     * validator that records each check it performs: with a by-value
     * handler the skipped branch still ran, and only its ERRORS were
     * dropped. By name, it does not run at all.
     *
     * A handler is used at most once by every instance here, so the
     * repeated-evaluation cost of a by-name parameter does not arise.
     */
    def select(f: => F[A => B]): F[B]
    def branch[C](fa: => F[A => C])(fb: => F[B => C]): F[C] =
      fe.map(_.map(Left(_))).select(fa.map(_.andThen(Right(_)))).select(fb)
  extension (x: F[Boolean])
    // branch sends Left to its FIRST argument, so true must become
    // Left — Either.cond puts true on the Right and inverted the
    // whole conditional (caught the day ifS was first tested)
    def ifS[A](t: => F[A])(e: => F[A]): F[A] =
      x.map(b => if b then Left(()) else Right(()))
        .branch(t.map(Function.const))(e.map(Function.const))

/** sequence computations; fmap, app and select all derive from flatMap by the laws */
@implicitNotFound("no Monad[${F}].\nOkay's instances ride the given import: `import okay.given`.\nFor a program monad A ! Row the instance lives in Free's companion and is always found;\nif ${F} is a row containing Choose, summon MonadPlus explicitly where empty/append are needed\n(Choice.scala documents the overlap).")
trait Monad[F[_]] extends Selective[F]:
  override def fmap[A, B](a: F[A], f: A => B): F[B] = a.flatMap(f.andThen(pure))
  extension [A](a: F[A])
    def flatMap[B](f: A => F[B]): F[B]
    inline def >>=[B](f: A => F[B]): F[B] = flatMap(f)
  extension [A, B](f: F[A => B])
    // NOT `a.flatMap(a => f.app(pure(a)))`: that recursion never grounds
    // (app of a pure argument is again app) — on a lazy carrier it
    // builds an infinite tree. Lay dormant until traverse first USED
    // the derived app; the generic combinators are also the test bed.
    def app(a: F[A]): F[B] = f.flatMap(g => fmap(a, g))
  extension [A, B](e: F[Either[A, B]])
    override def select(f: => F[A => B]): F[B] =
      e.flatMap(_.fold(a => f.map(_(a)), pure))

/** choice with a neutral element */
trait Alternative[F[_]] extends Applicative[F]:
  def empty[A]: F[A]
  extension [A](x: F[A])
    def append(y: F[A]): F[A]

// ----------------------------------------------------------------
// the generic combinators the classes exist for: written once, they
// run over programs (A ! F), LazyList, Choose — any instance

/** effectful map over a sequence, effects in order, results collected */
def traverse[F[_], A, B](xs: Seq[A])(f: A => F[B])(using M: Applicative[F]): F[Seq[B]] =
  xs.foldLeft(M.pure(Vector.empty[B]): F[Seq[B]]) { (acc, a) =>
    M.fmap(acc, (s: Seq[B]) => (b: B) => s :+ b).app(f(a))
  }

/** a sequence of computations into a computation of the sequence */
def sequence[F[_] : Applicative, A](xs: Seq[F[A]]): F[Seq[A]] =
  traverse(xs)(identity)

/** the same computation n times, results collected */
def replicateA[F[_] : Applicative, A](n: Int)(fa: F[A]): F[Seq[A]] =
  sequence(Seq.fill(n)(fa))

/** MonadPlus's pruning conditional: the branch dies here unless p —
 * the backbone of backtracking search (see Logic) */
def guard[F[_]](p: Boolean)(using M: MonadPlus[F]): F[Unit] =
  if p then M.pure(()) else M.empty

extension [F[_], A](fa: F[A])(using M: Applicative[F])
  /** sequence, keep the right */
  def *>[B](fb: F[B]): F[B] = M.fmap(fa, (_: A) => (b: B) => b).app(fb)

  /** sequence, keep the left */
  def <*[B](fb: F[B]): F[A] = M.fmap(fa, (a: A) => (_: B) => a).app(fb)

extension [F[_]](cond: F[Boolean])(using S: Selective[F])
  /** run the effect only when the condition holds — both branches
   * DECLARED statically (a Selective, not a Monad, is enough) */
  def whenS(body: F[Unit]): F[Unit] =
    S.ifS(cond)(body)(S.pure(()))

  /** run the effect only when the condition fails */
  def unlessS(body: F[Unit]): F[Unit] =
    S.ifS(cond)(S.pure(()))(body)

/** a Monad that is also an Alternative, under the traditional names */
@implicitNotFound("no MonadPlus[${F}].\nNondeterminism needs Choose in the row: MonadPlus exists for [A] =>> A ! Choose and for rows Choose + F\n(Choice.scala); `import okay.given` brings it.")
trait MonadPlus[F[_]]
  extends Alternative[F], Monad[F]:
  def mzero[A]: F[A] = empty
  extension [A](x: F[A])
    def mplus(y: F[A]): F[A] = x.append(y)

/**
 * extract a value from a context — the basis of pure per-operation
 * effect handlers: given [F: Comonad]: Handler[F] handles by extract
 */
trait Comonad[F[_]] extends Functor[F]:
  extension [A](a: F[A])
    def extract: A
    def coflatMap[B](f: F[A] => B): F[B]

/** the identity context (the Id functor): a bare value */
type Id[A] = A

object Comonad:
  /**
   * a value is trivially its own context. HERE, not at package level
   * (comonad-id-map-capture, 2026-09-23): as a package-level given it
   * put `Functor`'s `map` extension on EVERY type in package `okay`
   * and under `import okay.given`, where it beat facade companions
   * (`Static` became a class for it; `Cont` and `Prog` needed an
   * explicit `import ….{flatMap, map}`), contested the throws union's
   * `.map` and hijacked kyo's. In the companion it is still in the
   * implicit scope of `Comonad[Id]` — `summon`, and the `Handler[Id]`
   * derived from it, find it unchanged — but not in the lexical scope
   * of a bare value.
   */
  given id: Comonad[Id] with
    override inline def fmap[A, B](a: A, f: A => B): B = f(a)
    extension [A](a: A) {
      override inline def extract: A = a
      override inline def coflatMap[B](f: A => B): B = f(a)
    }

/**
 * Option is a Monad, globally (optics-core, at the operator's ask): the
 * instance three direct-style suites each carried locally, so that
 * `Star[Option]` — the traversal where one failing element fails the
 * whole — and every other Option-shaped program find it with
 * `import okay.given`. A suite's own local instance still wins by
 * scope, so the three keep working unchanged.
 */
given Monad[Option] with
  def pure[A](a: A): Option[A] = Some(a)
  override def fmap[A, B](a: Option[A], f: A => B): Option[B] = a.map(f)
  extension [A](a: Option[A])
    def flatMap[B](f: A => Option[B]): Option[B] = a.flatMap(f)
