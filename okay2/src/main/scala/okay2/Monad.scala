package okay2

import scala.annotation.implicitNotFound

/**
 * THE MONAD CLASSES of the Scala 3 core's Monad.scala, in Scala 2.13
 * (specs/okay2.md, stage 12): the same hierarchy, the same derived
 * operations, the same generic combinators, and the same instances.
 *
 * What Scala 2 spells differently:
 * - a type lambda where Scala 3 writes `[A] =>> Free[R, A]`:
 *   `({ type L[A] = Free[R, A] })#L`. Partial unification (on by
 *   default in 2.13) solves `F[A]` against `Free[R, A]` as
 *   `F = Free[R, *]`, so a call site never writes one: `traverse(xs)(f)`
 *   with `f: A => B ! R` finds `Monad` for the program monad itself;
 * - the operations of a class are methods OF the class taking the value
 *   (`M.flatMap(fa)(f)`), and the `fa.flatMap(f)` spelling is a syntax
 *   class (`MonadSyntax`, in the package object) that exists only where
 *   the instance does — Scala 3's extension methods on the class,
 *   which are likewise found only through a given;
 * - no `inline`: the JIT decides.
 *
 * WHERE THE INSTANCES LIVE, and why nothing needs importing: an
 * implicit search for `Monad[X]` looks in the companions of `Monad`'s
 * BASE classes too, so every instance for a type okay2 does not own
 * (`Option`) sits in `object Functor` and answers a query for any class
 * of the hierarchy. The program monad lives in `Free`'s companion, the
 * `Choose` MonadPlus in `Choose`'s — each in the implicit scope of the
 * type it is about, as cats keeps `Monad[Kleisli[F, A, *]]` in
 * `Kleisli`'s.
 *
 * Literature: Wadler, "Monads for functional programming" (1995);
 * McBride & Paterson, "Applicative programming with effects" (2008);
 * Mokhov, Lukyanov, Marlow & Dimino, "Selective applicative functors"
 * (2019); Atkey, "Parameterised notions of computation" (2009);
 * Uustalu & Vene, "Comonadic notions of computation" (2008).
 */

/** map a function over the structure */
@implicitNotFound("no Functor[${F}].\nThe instance for programs lives in Free's companion; for your own type, give one in ITS companion.\nIf ${F} reads `[R]X ! R`, the value is typed with the `!` alias, which partial unification takes AS WRITTEN — parameters reversed.\nType the value `Free[R, A]`, or use the program-shaped twins: !.traverse / !.sequence / !.replicateA, and *>, <*, >>=, ifS on the program itself.")
trait Functor[F[_]] {
  def fmap[A, B](a: F[A], f: A => B): F[B]
}

object Functor {
  /**
   * `Option` is a Monad, globally, as in the Scala 3 core. HERE because
   * the implicit scope of `Monad[Option]` (and of `Applicative`,
   * `Selective`, `Functor`) includes the companions of every base class
   * of the queried class, and `Functor` is the base of them all.
   */
  implicit val option: Monad[Option] = new Monad[Option] {
    def pure[A](a: A): Option[A] = Some(a)
    override def fmap[A, B](a: Option[A], f: A => B): Option[B] = a.map(f)
    def flatMap[A, B](a: Option[A])(f: A => Option[B]): Option[B] = a.flatMap(f)
  }
}

/** lift values and apply lifted functions; fmap derives from pure and app */
@implicitNotFound("no Applicative[${F}].\nThe instance for programs lives in Free's companion, Option's in Functor's; for your own type, give a Monad in its companion.\nIf ${F} reads `[R]X ! R`, the value is typed with the `!` alias, which partial unification takes AS WRITTEN — parameters reversed.\nType the value `Free[R, A]`, or use the program-shaped twins: !.traverse / !.sequence / !.replicateA, and *>, <*, >>=, ifS on the program itself.")
trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def app[A, B](f: F[A => B], a: F[A]): F[B]
  override def fmap[A, B](a: F[A], f: A => B): F[B] = app(pure(f), a)
}

/**
 * Selective applicative functors (Mokhov et al. 2019): declare both
 * branches statically, run at most one — between Applicative (all
 * effects known, all run) and Monad (continuations opaque).
 */
trait Selective[F[_]] extends Applicative[F] {
  /** BY NAME, and in Scala that is the whole point of the class: by
   * value the skipped branch would still RUN and only its result would
   * be dropped (measured in the Scala 3 core, Monad.scala) */
  def select[A, B](fe: F[Either[A, B]], f: => F[A => B]): F[B]

  def branch[A, B, C](fe: F[Either[A, B]])(fa: => F[A => C])(fb: => F[B => C]): F[C] =
    select[B, C](
      select[A, Either[B, C]](
        fmap(fe, (e: Either[A, B]) => e.map(b => Left(b): Either[B, C])),
        fmap(fa, (g: A => C) => g.andThen(c => Right(c): Either[B, C]))),
      fb)

  /** `branch` sends Left to its FIRST argument, so true must become Left
   * (the Scala 3 core's comment: Either.cond inverted the conditional) */
  def ifS[A](x: F[Boolean])(t: => F[A])(e: => F[A]): F[A] =
    branch[Unit, Unit, A](fmap(x, (b: Boolean) => if (b) Left(()) else Right(())))(
      fmap(t, (a: A) => (_: Unit) => a))(fmap(e, (a: A) => (_: Unit) => a))
}

/** sequence computations; fmap, app and select all derive from flatMap by the laws */
@implicitNotFound("no Monad[${F}].\nThe instance for programs lives in Free's companion, Option's in Functor's.\nIf ${F} is a row containing Choose and you need empty/append, ask for MonadPlus — Choose's companion has it.\nIf ${F} reads `[R]X ! R`, the value is typed with the `!` alias, which partial unification takes AS WRITTEN — parameters reversed.\nType the value `Free[R, A]`, or use the program-shaped twins: !.traverse / !.sequence / !.replicateA, and *>, <*, >>=, ifS on the program itself.")
trait Monad[F[_]] extends Selective[F] {
  def flatMap[A, B](a: F[A])(f: A => F[B]): F[B]

  override def fmap[A, B](a: F[A], f: A => B): F[B] = flatMap(a)((x: A) => pure(f(x)))

  // NOT `flatMap(a)(x => app(f, pure(x)))`: that recursion never grounds
  // (app of a pure argument is again app) — the Scala 3 core's comment
  override def app[A, B](f: F[A => B], a: F[A]): F[B] = flatMap(f)((g: A => B) => fmap(a, g))

  override def select[A, B](e: F[Either[A, B]], f: => F[A => B]): F[B] =
    flatMap(e)(_.fold((a: A) => fmap(f, (g: A => B) => g(a)), (b: B) => pure(b)))

  def flatten[A](m: F[F[A]]): F[A] = flatMap(m)(identity)
}

object Monad {
  def apply[F[_]](implicit M: Monad[F]): Monad[F] = M
}

/** choice with a neutral element */
trait Alternative[F[_]] extends Applicative[F] {
  def empty[A]: F[A]
  def append[A](x: F[A], y: => F[A]): F[A]
}

/** a Monad that is also an Alternative, under the traditional names */
@implicitNotFound("no MonadPlus[${F}].\nNondeterminism needs Choose in the row: the instance is `Choose.monadPlus[F]` for the row `Choose + F` (Choose's companion).\nFor an `if` or a refutable pattern in a `for`, no MonadPlus is needed: withFilter asks for CanFail.")
trait MonadPlus[F[_]] extends Alternative[F] with Monad[F] {
  def mzero[A]: F[A] = empty[A]
  def mplus[A](x: F[A], y: => F[A]): F[A] = append(x, y)
}

object MonadPlus {
  def apply[F[_]](implicit M: MonadPlus[F]): MonadPlus[F] = M
}

/**
 * extract a value from a context — the basis of pure per-operation
 * effect handlers
 */
trait Comonad[F[_]] extends Functor[F] {
  def extract[A](a: F[A]): A
  def coflatMap[A, B](a: F[A])(f: F[A] => B): F[B]
}

object Comonad {
  /** a value is trivially its own context. In the companion, as in the
   * Scala 3 core (comonad-id-map-capture): found by `Comonad[Id]`, not
   * put on every bare value in scope */
  implicit val id: Comonad[Id] = new Comonad[Id] {
    def fmap[A, B](a: A, f: A => B): B = f(a)
    def extract[A](a: A): A = a
    def coflatMap[A, B](a: A)(f: A => B): B = f(a)
  }
}

/**
 * Robert Atkey. Parameterised notions of computation. (2009)
 *
 * `M[A, S, R]` computes A while changing a state from S to R — indexed
 * by an arrow S -> R in a category of states. `Control` (Cont.scala)
 * is one, as in the Scala 3 core.
 */
trait ParaMonad[M[_, _, _]] {
  /** identity: R -> R */
  def pure[A, R](a: A): M[A, R, R]
  /** composition: (S2 -> S) then (S -> R) = S2 -> R */
  def flatMap[A, B, S, S2, R](m: M[A, S, R])(f: A => M[B, S2, S]): M[B, S2, R]
  def map[A, B, S, R](m: M[A, S, R])(f: A => B): M[B, S, R] = flatMap(m)((a: A) => pure[B, S](f(a)))
  def flatten[A, S, S2, R](m: M[M[A, S, S2], S2, R]): M[A, S, R] = flatMap(m)((x: M[A, S, S2]) => x)
}

object ParaMonad {
  def apply[M[_, _, _]](implicit P: ParaMonad[M]): ParaMonad[M] = P

  /** every `Control` is one (`Control[M] extends ParaMonad[M]`), and its
   * instances live in Control's companion, which a query for ParaMonad
   * does not look in */
  implicit def ofControl[M[_, _, _]](implicit C: Control[M]): ParaMonad[M] = C

  /** every parameterised monad is a family of ordinary monads, one on
   * each diagonal `M[*, R, R]` — asked for by name, since no call site
   * can partially unify `M[A, R, R]` as `F[A]` */
  def diagonal[M[_, _, _], R](implicit P: ParaMonad[M]): Monad[({ type L[A] = M[A, R, R] })#L] =
    new DiagonalMonad[M, R](P)
}

final class DiagonalMonad[M[_, _, _], R](val P: ParaMonad[M]) extends Monad[({ type L[A] = M[A, R, R] })#L] {
  def pure[A](a: A): M[A, R, R] = P.pure[A, R](a)
  def flatMap[A, B](m: M[A, R, R])(f: A => M[B, R, R]): M[B, R, R] = P.flatMap[A, B, R, R, R](m)(f)
}

/**
 * A step a program may DECLINE — which is what a refutable pattern on
 * the left of `<-` asks for, and what an `if` guard asks for too: both
 * desugar to `withFilter`. A plain `A ! R` may not stop early, and it
 * must not pretend to, so the witness is the whole design (the Scala 3
 * core's CanFail, Throws.scala):
 *
 *   Choose   the BRANCH dies and the search goes on
 *   Abort    the PROGRAM stops and `runOption` answers None
 *
 * Choose wins where a row carries both: in a searching row an `if`
 * already means prune.
 *
 * MEMBERSHIP IS SUBTYPING here: a row containing Choose is a SUBTYPE of
 * Choose (`State[Int] + Choose <: Choose`), so the instances are bounded
 * type parameters and resolve at any concrete row. The Scala 3 core
 * reached the same place through `In` after a MonadPlus-based first cut
 * failed on a higher-order unification.
 */
@implicitNotFound("this row cannot drop a step, so a refutable pattern (`case Some(x) <- p`) and an `if` guard have no meaning in it: ${R}\nBoth desugar to withFilter, which needs somewhere for the dropped step to GO.\nPut a failing effect in the row and it works: Abort (the program stops, runOption answers None) or Choose (the branch dies, the search goes on).\nOr keep the row as it is and write the branch yourself: `old.fold(pure(()))(...)` says the same thing and hides nothing.")
trait CanFail[R <: Row] { def fail[A]: A ! R }

trait CanFailLow {
  /** stop: the rest of the program does not run */
  implicit def viaAbort[R <: Abort]: CanFail[R] = new CanFail[R] {
    def fail[A]: A ! R = abort[A]
  }
}

object CanFail extends CanFailLow {
  /** prune: this branch dies, the search continues */
  implicit def viaChoose[R <: Choose]: CanFail[R] = new CanFail[R] {
    def fail[A]: A ! R = Choose.fail[A]
  }
}

/**
 * The generic combinators the classes exist for, written once: they run
 * over programs, Option, a Choose row — any instance. Mixed into the
 * package object, so `import okay2._` brings them.
 */
trait Monads {
  /** the identity context (the Id functor): a bare value */
  type Id[A] = A

  /** effectful map over a sequence, effects in order, results collected */
  def traverse[F[_], A, B](xs: Seq[A])(f: A => F[B])(implicit M: Applicative[F]): F[Seq[B]] =
    xs.foldLeft(M.pure(Vector.empty[B]): F[Seq[B]]) { (acc, a) =>
      M.app(M.fmap(acc, (s: Seq[B]) => (b: B) => s :+ b), f(a))
    }

  /** a sequence of computations into a computation of the sequence */
  def sequence[F[_], A](xs: Seq[F[A]])(implicit M: Applicative[F]): F[Seq[A]] =
    traverse[F, F[A], A](xs)(identity)

  /** the same computation n times, results collected */
  def replicateA[F[_], A](n: Int)(fa: F[A])(implicit M: Applicative[F]): F[Seq[A]] =
    sequence[F, A](Seq.fill(n)(fa))

  /** MonadPlus's pruning conditional: the branch dies here unless p */
  def guard[F[_]](p: Boolean)(implicit M: MonadPlus[F]): F[Unit] =
    if (p) M.pure(()) else M.empty[Unit]

  /** the same demand as an `if` guard, outside a for-comprehension: hold
   * or stop — prune where the row searches, stop where it can abort, a
   * compile error where it can do neither */
  def ensure[R <: Row](p: Boolean)(implicit C: CanFail[R]): Unit ! R =
    if (p) pure[R, Unit](()) else C.fail[Unit]

  /** Kleisli composition, `>=>` as the literature names it */
  implicit final class KleisliSyntax[F[_], A, B](f: A => F[B])(implicit M: Monad[F]) {
    // the instance on the CLASS, not the method: `(f >=> g)(x)` would
    // otherwise pass x as the implicit argument
    def >=>[C](g: B => F[C]): A => F[C] = a => M.flatMap(f(a))(g)
  }

  /**
   * The operations of the classes as methods on a value — present only
   * where an instance is (the conversion itself asks for it), as Scala
   * 3's extension methods are. A member of the same name wins, so
   * `Free`'s and `Option`'s own `map`/`flatMap` are untouched.
   */
  implicit final class FunctorSyntax[F[_], A](fa: F[A])(implicit F: Functor[F]) {
    def map[B](f: A => B): F[B] = F.fmap(fa, f)
  }

  implicit final class ApplicativeSyntax[F[_], A](fa: F[A])(implicit F: Applicative[F]) {
    /** sequence, keep the right */
    def *>[B](fb: F[B]): F[B] = F.app(F.fmap(fa, (_: A) => (b: B) => b), fb)
    /** sequence, keep the left */
    def <*[B](fb: F[B]): F[A] = F.app(F.fmap(fa, (a: A) => (_: B) => a), fb)
  }

  implicit final class ApSyntax[F[_], A, B](f: F[A => B])(implicit F: Applicative[F]) {
    def app(a: F[A]): F[B] = F.app(f, a)
    /** the idiom bracket's own spelling: `pure(f) <*> fa <*> fb` */
    def <*>(a: F[A]): F[B] = F.app(f, a)
  }

  implicit final class SelectiveSyntax[F[_], A, B](fe: F[Either[A, B]])(implicit F: Selective[F]) {
    def select(f: => F[A => B]): F[B] = F.select(fe, f)
    def branch[C](fa: => F[A => C])(fb: => F[B => C]): F[C] = F.branch(fe)(fa)(fb)
  }

  implicit final class SelectiveBoolSyntax[F[_]](cond: F[Boolean])(implicit F: Selective[F]) {
    def ifS[A](t: => F[A])(e: => F[A]): F[A] = F.ifS(cond)(t)(e)
    /** run the effect only when the condition holds — both branches
     * DECLARED statically (a Selective, not a Monad, is enough) */
    def whenS(body: => F[Unit]): F[Unit] = F.ifS(cond)(body)(F.pure(()))
    /** run the effect only when the condition fails */
    def unlessS(body: => F[Unit]): F[Unit] = F.ifS(cond)(F.pure(()))(body)
  }

  implicit final class MonadSyntax[F[_], A](fa: F[A])(implicit M: Monad[F]) {
    def flatMap[B](f: A => F[B]): F[B] = M.flatMap(fa)(f)
    def >>=[B](f: A => F[B]): F[B] = M.flatMap(fa)(f)
  }

  implicit final class AlternativeSyntax[F[_], A](x: F[A])(implicit F: Alternative[F]) {
    def append(y: => F[A]): F[A] = F.append(x, y)
    def mplus(y: => F[A]): F[A] = F.append(x, y)
  }

  implicit final class ComonadSyntax[F[_], A](a: F[A])(implicit F: Comonad[F]) {
    def extract: A = F.extract(a)
    def coflatMap[B](f: F[A] => B): F[B] = F.coflatMap(a)(f)
  }

  /** the desugaring target of a pattern bind and of an `if` guard in a
   * `for` over programs: the row must be able to fail (CanFail) */
  implicit final class FilterSyntax[R <: Row, A](private val p: Free[R, A]) {
    def withFilter(q: A => Boolean)(implicit C: CanFail[R]): A ! R =
      p.flatMap[R, A](a => if (q(a)) pure[R, A](a) else C.fail[A])
    def filter(q: A => Boolean)(implicit C: CanFail[R]): A ! R = withFilter(q)
  }
}
