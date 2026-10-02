package okay.cats

import okay.{!, Async, Choose, Par, Scheduler, Static, Validated}
import okay.given
import _root_.cats.{~>, Eval}
import _root_.cats.effect.IO

/**
 * THE CLASS LADDER BOTH WAYS (specs/interop-classes.md).
 *
 * CatsInterop.scala put cats' `Monad`/`MonadError` on okay PROGRAMS.
 * This file does the rest of the ladder, for the types each side has
 * that the other's classes did not reach:
 *
 *   - cats' classes for okay's NON-monadic carriers, whose point is
 *     what they refuse to be: `Validated` accumulates, `Static` lists
 *     its operations before running, `Par` forks. Under cats'
 *     `traverse`/`mapN`/`parTraverse` each keeps that point, because
 *     each instance delegates to okay's own and never goes through a
 *     `flatMap`.
 *   - okay's classes for cats' types: `cats.data.Validated` gets a
 *     REAL `Selective` (cats has no such class), `IO` a `Monad`.
 *
 * Everything here is a SPECIFIC type, so it sits in the default
 * `import okay.cats.given` without competing with anything. The
 * GENERIC bridges — any cats class to ours, any of ours to cats' —
 * are [[FromCats]] and [[ToCats]], one import each.
 */

/** okay's accumulating `Validated` under cats' Applicative: `ap` keeps
 * EVERY error (`okay.Validated.selective`), so `List(...).traverse`
 * from cats reports all of them — combined by okay's semigroup or
 * cats-kernel's ([[Combine]]) */
given catsValidated[E](using C: Combine[E]): _root_.cats.Applicative[[A] =>> Validated[E, A]] with
  // okay's semigroup or cats-kernel's, whichever the caller holds (CatsKernel.scala)
  private val V = Validated.selective[E](using (x, y) => C.combine(x, y))
  def pure[A](a: A): Validated[E, A] = V.pure(a)
  override def map[A, B](fa: Validated[E, A])(f: A => B): Validated[E, B] = V.fmap(fa, f)
  def ap[A, B](ff: Validated[E, A => B])(fa: Validated[E, A]): Validated[E, B] = V.app(ff)(fa)

/** a free selective under cats' Applicative: what cats builds is still
 * a `Static`, its operations listable before it runs */
given catsStatic[F[+_]]: _root_.cats.Applicative[[A] =>> Static[F, A]] with
  private val S = Static.static[F]
  def pure[A](a: A): Static[F, A] = S.pure(a)
  override def map[A, B](fa: Static[F, A])(f: A => B): Static[F, B] = S.fmap(fa, f)
  def ap[A, B](ff: Static[F, A => B])(fa: Static[F, A]): Static[F, B] = S.app(ff)(fa)

/**
 * `A ! Async` and `Par` as cats' `Parallel` pair — the relation cats
 * draws between `IO` and `IO.Par`, and the one `Par.scala` draws
 * between a program and a leaf. `parTraverse`, `parMapN` and friends
 * fork through `Async.par`; the monad side is the default instance of
 * CatsInterop.scala.
 */
given catsPar(using Scheduler): _root_.cats.Parallel.Aux[[A] =>> A ! Async, Par] =
  new _root_.cats.Parallel[[A] =>> A ! Async]:
    type F[X] = Par[X]
    private val P = Par.parApplicative
    val applicative: _root_.cats.Applicative[Par] = new:
      def pure[A](a: A): Par[A] = P.pure(a)
      override def map[A, B](fa: Par[A])(f: A => B): Par[B] = P.fmap(fa, f)
      def ap[A, B](ff: Par[A => B])(fa: Par[A]): Par[B] = P.app(ff)(fa)
    val monad: _root_.cats.Monad[[A] =>> A ! Async] = summon
    val sequential: Par ~> ([A] =>> A ! Async) = new (Par ~> ([A] =>> A ! Async)):
      def apply[A](p: Par[A]): A ! Async = p.seq
    val parallel: ([A] =>> A ! Async) ~> Par = new (([A] =>> A ! Async) ~> Par):
      def apply[A](p: A ! Async): Par[A] = Par(p)

/**
 * Nondeterminism as cats sees it: `combineK` is choice, `empty` prunes.
 *
 * ONLY `MonoidK` is a given. A given `Alternative` — an `Applicative`
 * too — would tie with the program monad above for every
 * `cats.Applicative[A ! Choose]`, and two top-level givens in two files
 * have no priority between them: cats' `traverse` over a choice program
 * would stop compiling. `<+>` needs no more than this; the full
 * `Alternative` (`guard`, `unite`) is [[CatsClasses.chooseAlternative]],
 * passed where it is wanted.
 */
given catsChoose: _root_.cats.MonoidK[[A] =>> A ! Choose] with
  private val M = summon[okay.MonadPlus[[A] =>> A ! Choose]]
  def empty[A]: A ! Choose = M.empty
  def combineK[A](x: A ! Choose, y: A ! Choose): A ! Choose = M.append(x)(y)

object CatsClasses:

  /** `A ! Choose` as cats' `Alternative` AND `Monad` at once — explicit,
   * for the reason [[catsChoose]] is only a `MonoidK` */
  val chooseAlternative: _root_.cats.StackSafeMonad[[A] =>> A ! Choose] & _root_.cats.Alternative[[A] =>> A ! Choose] =
    new _root_.cats.StackSafeMonad[[A] =>> A ! Choose] with _root_.cats.Alternative[[A] =>> A ! Choose]:
      def pure[A](a: A): A ! Choose = okay.pure(a)
      def flatMap[A, B](fa: A ! Choose)(f: A => B ! Choose): B ! Choose = fa.flatMap(f)
      def empty[A]: A ! Choose = catsChoose.empty
      def combineK[A](x: A ! Choose, y: A ! Choose): A ! Choose = catsChoose.combineK(x, y)

/**
 * cats' `Validated` with the rung cats does not have: `select` runs the
 * handler only for a valid `Left` (Mokhov et al.'s instance for
 * `Validation`), `app` accumulates through either library's semigroup ([[Combine]]).
 */
given okayCatsValidated[E](using S: Combine[E]): okay.Selective[[A] =>> _root_.cats.data.Validated[E, A]] with
  import _root_.cats.data.Validated.{Valid, Invalid}
  def pure[A](a: A): _root_.cats.data.Validated[E, A] = Valid(a)
  override def fmap[A, B](a: _root_.cats.data.Validated[E, A], f: A => B): _root_.cats.data.Validated[E, B] = a.map(f)
  extension [A, B](f: _root_.cats.data.Validated[E, A => B])
    def app(a: _root_.cats.data.Validated[E, A]): _root_.cats.data.Validated[E, B] = (f, a) match
      case (Valid(g), Valid(x)) => Valid(g(x))
      case (Invalid(e1), Invalid(e2)) => Invalid(S.combine(e1, e2))
      case (Invalid(e1), _) => Invalid(e1)
      case (_, Invalid(e2)) => Invalid(e2)
  extension [A, B](e: _root_.cats.data.Validated[E, Either[A, B]])
    override def select(f: => _root_.cats.data.Validated[E, A => B]): _root_.cats.data.Validated[E, B] = e match
      case Valid(Right(b)) => Valid(b)              // the handler is SKIPPED
      case Valid(Left(a)) => f.map(_(a))
      case Invalid(err) => Invalid(err)

/** cats-effect's IO under okay's Monad: `okay.traverse`, `whenS` and a
 * `direct[IO]` block over it. IO's own `flatMap` is the bind, so stack
 * safety is IO's run loop. */
given okayIO: okay.Monad[IO] with
  def pure[A](a: A): IO[A] = IO.pure(a)
  override def fmap[A, B](a: IO[A], f: A => B): IO[B] = a.map(f)
  extension [A](a: IO[A])
    def flatMap[B](f: A => IO[B]): IO[B] = a.flatMap(f)

/** cats' `Eval` under okay's Monad — cats' own trampoline, so a deep
 * `okay.traverse` over it is stack-safe */
given okayEval: okay.Monad[Eval] with
  def pure[A](a: A): Eval[A] = Eval.now(a)
  override def fmap[A, B](a: Eval[A], f: A => B): Eval[B] = a.map(f)
  extension [A](a: Eval[A])
    def flatMap[B](f: A => Eval[B]): Eval[B] = a.flatMap(f)

/**
 * Any cats class as okay's (`import okay.cats.FromCats.given`):
 * `okay.traverse`, `sequence`, `whenS`, `direct` over every type cats
 * has an instance for. Priority, highest first: MonadPlus (cats Monad
 * and Alternative both), Monad, Alternative, Applicative, Functor.
 *
 * NOT together with [[ToCats]]: each derives the other's class from its
 * own, and a search that can go round that loop diverges.
 */
object FromCats extends FromCatsMonad:
  given monadPlus[F[_]](using M: _root_.cats.Monad[F], A: _root_.cats.Alternative[F]): okay.MonadPlus[F] with
    def pure[A](a: A): F[A] = M.pure(a)
    override def fmap[A, B](a: F[A], f: A => B): F[B] = M.map(a)(f)
    def empty[A]: F[A] = A.empty
    extension [A](a: F[A])
      def flatMap[B](f: A => F[B]): F[B] = M.flatMap(a)(f)
      def append(b: F[A]): F[A] = A.combineK(a, b)
    extension [A, B](f: F[A => B])
      override def app(a: F[A]): F[B] = M.ap(f)(a)

trait FromCatsMonad extends FromCatsAlternative:
  given monad[F[_]](using M: _root_.cats.Monad[F]): okay.Monad[F] with
    def pure[A](a: A): F[A] = M.pure(a)
    override def fmap[A, B](a: F[A], f: A => B): F[B] = M.map(a)(f)
    extension [A](a: F[A])
      def flatMap[B](f: A => F[B]): F[B] = M.flatMap(a)(f)
    extension [A, B](f: F[A => B])
      override def app(a: F[A]): F[B] = M.ap(f)(a)

trait FromCatsAlternative extends FromCatsApplicative:
  given alternative[F[_]](using A: _root_.cats.Alternative[F]): okay.Alternative[F] with
    def pure[X](a: X): F[X] = A.pure(a)
    override def fmap[X, B](a: F[X], f: X => B): F[B] = A.map(a)(f)
    def empty[X]: F[X] = A.empty
    extension [X](a: F[X])
      def append(b: F[X]): F[X] = A.combineK(a, b)
    extension [X, B](f: F[X => B])
      def app(a: F[X]): F[B] = A.ap(f)(a)

trait FromCatsApplicative extends FromCatsFunctor:
  given applicative[F[_]](using A: _root_.cats.Applicative[F]): okay.Applicative[F] with
    def pure[X](a: X): F[X] = A.pure(a)
    override def fmap[X, B](a: F[X], f: X => B): F[B] = A.map(a)(f)
    extension [X, B](f: F[X => B])
      def app(a: F[X]): F[B] = A.ap(f)(a)

trait FromCatsFunctor:
  given functor[F[_]](using F: _root_.cats.Functor[F]): okay.Functor[F] with
    def fmap[A, B](a: F[A], f: A => B): F[B] = F.map(a)(f)

/**
 * Any okay class as cats' (`import okay.cats.ToCats.given`): cats'
 * `traverse`, `mapN`, `flatMap`, `combineK` over every type okay has an
 * instance for. Monad, Alternative, Applicative, Functor, in that
 * priority. cats' `Monad` demands a stack-safe `tailRecM`; okay's
 * `TailRecM` is one for every okay `Monad`, eager ones included
 * (derived in Effects.scala, specs/monad-tailrecm.md), and the bridge
 * passes it through.
 *
 * NOT together with [[FromCats]] — see there.
 */
object ToCats extends ToCatsAlternative:
  given monad[F[_]](using M: okay.Monad[F], R: okay.TailRecM[F]): _root_.cats.Monad[F] with
    def pure[X](a: X): F[X] = M.pure(a)
    override def map[X, B](fa: F[X])(f: X => B): F[B] = M.fmap(fa, f)
    override def ap[X, B](ff: F[X => B])(fa: F[X]): F[B] = M.app(ff)(fa)
    def flatMap[X, B](fa: F[X])(f: X => F[B]): F[B] = M.flatMap(fa)(f)
    def tailRecM[X, B](a: X)(f: X => F[Either[X, B]]): F[B] = R.tailRecM(a)(f)

trait ToCatsAlternative extends ToCatsApplicative:
  given alternative[F[_]](using A: okay.Alternative[F]): _root_.cats.Alternative[F] with
    def pure[X](a: X): F[X] = A.pure(a)
    override def map[X, B](fa: F[X])(f: X => B): F[B] = A.fmap(fa, f)
    def ap[X, B](ff: F[X => B])(fa: F[X]): F[B] = A.app(ff)(fa)
    def empty[X]: F[X] = A.empty
    def combineK[X](x: F[X], y: F[X]): F[X] = A.append(x)(y)

trait ToCatsApplicative extends ToCatsFunctor:
  given applicative[F[_]](using A: okay.Applicative[F]): _root_.cats.Applicative[F] with
    def pure[X](a: X): F[X] = A.pure(a)
    override def map[X, B](fa: F[X])(f: X => B): F[B] = A.fmap(fa, f)
    def ap[X, B](ff: F[X => B])(fa: F[X]): F[B] = A.app(ff)(fa)

trait ToCatsFunctor:
  given functor[F[_]](using F: okay.Functor[F]): _root_.cats.Functor[F] with
    def map[A, B](fa: F[A])(f: A => B): F[B] = F.fmap(fa, f)
