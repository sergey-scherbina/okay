package okay2

import scala.language.experimental.macros
import scala.reflect.ClassTag

/**
 * Optics on profunctors — the Scala 3 core's okay-optics. An optic is a
 * function polymorphic in a profunctor `P`, and its CONSTRAINT on `P` is
 * a type parameter: a lens asks for `Strong`, a prism for `Choice`, a
 * traversal for `Traversing`, an iso for a bare `Profunctor`.
 * Composition takes the INTERSECTION of the two constraints, the meet
 * of the lattice — `lens andThen prism` asks for `Strong[P] with
 * Choice[P]`, the affine traversal — and an interpretation satisfies it
 * by subtyping, since `Traversing` extends both.
 *
 * CONTRAVARIANT in the constraint, which the Scala 3 core does not need
 * and Scala 2 does: an optic asking for less can stand where more is
 * offered, so `lens andThen prism andThen lens` — whose constraint is
 * `Strong with Choice with Strong` — IS an `Affine`, where Scala 2's
 * type-lambda equality alone would not see it.
 *
 * Everything is reached with ONE import, `import okay2.Optic._`: the
 * families (`Lens`, `Prism`, ...) are type aliases, which Scala 2 can
 * only hold in an object, and a second module cannot add to okay2's
 * package object.
 */
trait Optic[-C[_[_, _]], S, T, A, B] { self =>
  def apply[P[_, _]](p: P[A, B])(implicit c: C[P]): P[S, T]

  /** composition: the constraint is the intersection — the meet */
  def andThen[C2[_[_, _]], A2, B2](o: Optic[C2, A, B, A2, B2]): Optic[Optic.Meet[C, C2]#L, S, T, A2, B2] =
    new Optic[Optic.Meet[C, C2]#L, S, T, A2, B2] {
      def apply[P[_, _]](p: P[A2, B2])(implicit c: C[P] with C2[P]): P[S, T] = self[P](o[P](p)(c))(c)
    }
}

object Optic {

  /** the intersection of two constraints, as a type-level function */
  type Meet[C[_[_, _]], C2[_[_, _]]] = { type L[P[_, _]] = C[P] with C2[P] }

  type Iso[S, T, A, B] = Optic[Profunctor, S, T, A, B]
  type Lens[S, T, A, B] = Optic[Strong, S, T, A, B]
  type Prism[S, T, A, B] = Optic[Choice, S, T, A, B]
  type Affine[S, T, A, B] = Optic[Meet[Strong, Choice]#L, S, T, A, B]
  type Traversal[S, T, A, B] = Optic[Traversing, S, T, A, B]
  /** the aggregating families (Clarke et al., "Profunctor Optics, a
   * Categorical Update", 2024): a kaleidoscope lifts through an
   * APPLICATIVE and collapses many focuses into one answer; an
   * algebraic lens puts by an ALGEBRA over many wholes */
  type Kaleidoscope[S, T, A, B] = Optic[Reflecting, S, T, A, B]
  type AlgebraicLens[S, T, A, B] = Optic[Classifying, S, T, A, B]

  // ---------------------------------------------------------------- the lattice

  trait Profunctor[P[_, _]] {
    def dimap[A, B, C, D](p: P[A, B])(f: C => A, g: B => D): P[C, D]
  }

  /** a lens's requirement: `first` is the structure map of a Tambara
   * module; `lens` is derived from it and overridable by a direct road */
  trait Strong[P[_, _]] extends Profunctor[P] {
    def first[A, B, C](p: P[A, B]): P[(A, C), (B, C)]
    /** the mirror of `first`, derived by swapping; overridable */
    def second[A, B, C](p: P[A, B]): P[(C, A), (C, B)] =
      dimap(first[A, B, C](p))((ca: (C, A)) => (ca._2, ca._1), (bc: (B, C)) => (bc._2, bc._1))
    def lens[S, T, A, B](get: S => A, set: (S, B) => T)(p: P[A, B]): P[S, T] =
      dimap(first[A, B, S](p))((s: S) => (get(s), s), (bs: (B, S)) => set(bs._2, bs._1))
  }

  /** sequential composition — arrows and optics are neighbours (Rivas &
   * Jaskelioff 2017), both written on `Profunctor` */
  trait Category[P[_, _]] {
    def id[A]: P[A, A]
    def compose[A, B, C](g: P[B, C], f: P[A, B]): P[A, C]
  }

  /** Category + Strong + a lifted function */
  trait Arrow[P[_, _]] extends Category[P] with Strong[P] {
    def arr[A, B](f: A => B): P[A, B]
    def id[A]: P[A, A] = arr(identity)
    /** the two sides of a pair, each through its own arrow */
    def split[A, B, C, D](f: P[A, B], g: P[C, D]): P[(A, C), (B, D)] =
      compose(second[C, D, B](g), first[A, B, C](f))
    /** one input, both arrows, both answers */
    def fanout[A, B, C](f: P[A, B], g: P[A, C]): P[A, (B, C)] =
      compose(split(f, g), arr((a: A) => (a, a)))
  }

  /** a prism's requirement; `prism` derived from `right`, overridable */
  trait Choice[P[_, _]] extends Profunctor[P] {
    def right[A, B, C](p: P[A, B]): P[Either[C, A], Either[C, B]]
    /** the mirror of `right`, derived by swapping the sum */
    def left[A, B, C](p: P[A, B]): P[Either[A, C], Either[B, C]] = {
      val swapAC: Either[A, C] => Either[C, A] = _.fold(Right(_), Left(_))
      val swapCB: Either[C, B] => Either[B, C] = _.fold(Right(_), Left(_))
      dimap(right[A, B, C](p))(swapAC, swapCB)
    }
    def prism[S, T, A, B](preview: S => Either[T, A], review: B => T)(p: P[A, B]): P[S, T] =
      dimap(right[A, B, T](p))(preview, (e: Either[T, B]) => e.fold(identity, review))
  }

  /** a traversable shape, applicative-polymorphic (Purescript's
   * `Wander`); a trait because Scala 2 has no polymorphic function type */
  trait Walk[S, T, A, B] {
    def apply[F[_]](f: A => F[B])(implicit F: Applicative[F]): S => F[T]
  }

  /** every element of a Vector, as a Walk */
  def vectorWalk[A, B]: Walk[Vector[A], Vector[B], A, B] = new Walk[Vector[A], Vector[B], A, B] {
    def apply[F[_]](f: A => F[B])(implicit F: Applicative[F]): Vector[A] => F[Vector[B]] =
      as => as.foldLeft(F.pure(Vector.empty[B]))((acc, a) => F.app(F.fmap(acc, (v: Vector[B]) => (b: B) => v :+ b), f(a)))
  }

  /** a traversal's requirement; `eachVector` derived from `wander`, overridable */
  trait Traversing[P[_, _]] extends Strong[P] with Choice[P] {
    def wander[S, T, A, B](w: Walk[S, T, A, B])(p: P[A, B]): P[S, T]
    def eachVector[A, B](p: P[A, B]): P[Vector[A], Vector[B]] = wander(vectorWalk[A, B])(p)
  }

  /** NOT implicit: an implicit `Applicative[Id]` would apply to every type */
  val idApplicative: Applicative[Id] = new Applicative[Id] {
    def pure[A](a: A): A = a
    def app[A, B](f: A => B, a: A): B = f(a)
  }

  /** a kaleidoscope's requirement: lift through ANY Applicative — one
   * answer, where `wander` keeps a shape */
  trait Reflecting[P[_, _]] extends Profunctor[P] {
    def reflected[F[_], A, B](p: P[A, B])(implicit F: Applicative[F]): P[F[A], F[B]]
  }

  /** an algebraic lens's requirement: view one whole, put by an algebra
   * over ALL the wholes. No Tambara derivation is claimed */
  trait Classifying[P[_, _]] extends Profunctor[P] {
    def classifying[S, T, A, B](view: S => A, classify: (Vector[S], B) => T)(p: P[A, B]): P[S, T]
  }

  // ---------------------------------------------------------------- the carriers

  /** the aggregating interpretation: many focuses in, one answer out */
  final case class Aggregating[A, B](run: Vector[A] => B)

  /** `Vector[F[A]] => F[Vector[A]]`, which is `vectorWalk` at identity */
  def sequenceVector[F[_], A](vs: Vector[F[A]])(implicit F: Applicative[F]): F[Vector[A]] =
    vectorWalk[F[A], A].apply[F]((fa: F[A]) => fa).apply(vs)

  /** the ZIP applicative — lazy, because a lawful zip `pure` is the
   * infinite repeat. Not implicit: the cartesian one is the usual one */
  val zipLazy: Applicative[LazyList] = new Applicative[LazyList] {
    def pure[A](a: A): LazyList[A] = LazyList.continually(a)
    def app[A, B](f: LazyList[A => B], a: LazyList[A]): LazyList[B] = f.zip(a).map { case (g, x) => g(x) }
  }

  /** read and forget the rest: `get`, and with a Monoid `preview`, `foldMap`, `toVector` */
  final case class Forget[R, A, B](run: A => R)
  object Forget {
    /** the carrier at a fixed answer `R`, as a two-hole type */
    type Of[R] = { type L[A, B] = Forget[R, A, B] }
  }

  /** the constant functor, an Applicative by the Monoid — what `foldMap` walks with */
  final case class Const[R, A](value: R)
  object Const {
    type Of[R] = { type L[A] = Const[R, A] }
    def applicative[R](implicit M: Monoid[R]): Applicative[Of[R]#L] = new Applicative[Of[R]#L] {
      def pure[A](a: A): Const[R, A] = Const(M.empty)
      def app[A, B](f: Const[R, A => B], a: Const[R, A]): Const[R, B] = Const(M.combine(f.value, a.value))
    }
  }

  /** first-wins: `preview`'s monoid */
  final case class First[A](value: Option[A])
  object First {
    implicit def monoid[A]: Monoid[First[A]] = new Monoid[First[A]] {
      def empty: First[A] = First(None)
      def combine(x: First[A], y: First[A]): First[A] = if (x.value.isDefined) x else y
    }
  }

  /** effectful functions: `traverseOf` for any Applicative — a program row included */
  final case class Star[F[_], A, B](run: A => F[B])
  object Star {
    type Of[F[_]] = { type L[A, B] = Star[F, A, B] }
  }

  /** THE KLEISLI ARROW, by name and not implicit: `Star` is already
   * `Traversing` for every Applicative, and a second instance extending
   * `Strong` would make every optic call at `Star` ambiguous */
  def kleisliArrow[F[_]](implicit F: Monad[F]): Arrow[Star.Of[F]#L] with Choice[Star.Of[F]#L] =
    new Arrow[Star.Of[F]#L] with Choice[Star.Of[F]#L] {
      def dimap[A, B, C, D](p: Star[F, A, B])(f: C => A, g: B => D): Star[F, C, D] = Star(c => F.fmap(p.run(f(c)), g))
      def first[A, B, C](p: Star[F, A, B]): Star[F, (A, C), (B, C)] = Star(ac => F.fmap(p.run(ac._1), (b: B) => (b, ac._2)))
      def right[A, B, C](p: Star[F, A, B]): Star[F, Either[C, A], Either[C, B]] =
        Star(_.fold(c => F.pure(Left(c)), a => F.fmap(p.run(a), (b: B) => Right(b))))
      def arr[A, B](f: A => B): Star[F, A, B] = Star(a => F.pure(f(a)))
      def compose[A, B, C](g: Star[F, B, C], f: Star[F, A, B]): Star[F, A, C] = Star(a => F.flatMap(f.run(a))(g.run))
    }

  /**
   * The affine optic's concrete pair, itself a `Strong` and `Choice`
   * profunctor in (S, T): run an optic once at the identity market and
   * out comes its own pair (Boisseau & Gibbons 2018). MEASURED SLOWER
   * than the optic in the core (8.0 ns against 3.0): for the PAIR — to
   * hand across a boundary — never for speed.
   */
  final case class Market[A, B, S, T](look: S => Either[T, A], put: (S, B) => T)
  object Market {
    type Of[A, B] = { type L[S, T] = Market[A, B, S, T] }
  }

  /** a compiled optic: the pair, with the operations as direct calls */
  final class Compiled[S, T, A, B](val look: S => Either[T, A], val put: (S, B) => T) {
    def preview(s: S): Option[A] = look(s).toOption
    def set(b: B): S => T = s => look(s) match {
      case Right(_) => put(s, b)
      case Left(t) => t
    }
    def modify(f: A => B): S => T = s => look(s) match {
      case Right(a) => put(s, f(a))
      case Left(t) => t
    }
  }

  private[okay2] def idMarket[A, B]: Market[A, B, A, B] = Market[A, B, A, B](Right(_), (_, b) => b)

  /** the lens's concrete pair — no Either, the focus is always there */
  final case class Shop[A, B, S, T](get: S => A, put: (S, B) => T)
  object Shop {
    type Of[A, B] = { type L[S, T] = Shop[A, B, S, T] }
  }

  /** a compiled lens: two direct calls */
  final class CompiledLens[S, T, A, B](val get: S => A, val put: (S, B) => T) {
    def set(b: B): S => T = s => put(s, b)
    def modify(f: A => B): S => T = s => put(s, f(get(s)))
  }

  private[okay2] def idShop[A, B]: Shop[A, B, A, B] = Shop[A, B, A, B](identity, (_, b) => b)

  // ---------------------------------------------------------------- the interpretations
  //
  // In `Profunctor`'s companion, which is in the implicit scope of every
  // class of the lattice — no import needed. `forgetStrong` sits a
  // level lower than `forgetTraversing`: with a Monoid both apply, and
  // the Traversing one's `lens` is the same function.

  object Profunctor extends LowProfunctor {

    /** plain functions: `modify` and `set` — AND the arrow, on the same
     * instance: a second one carrying `Arrow` (which extends `Strong`)
     * would make `Strong[Function1]` ambiguous at every optic call */
    implicit val function1: Traversing[Function1] with Arrow[Function1] = new Traversing[Function1] with Arrow[Function1] {
      def dimap[A, B, C, D](p: A => B)(f: C => A, g: B => D): C => D = f.andThen(p).andThen(g)
      def first[A, B, C](p: A => B): ((A, C)) => (B, C) = ac => (p(ac._1), ac._2)
      def right[A, B, C](p: A => B): Either[C, A] => Either[C, B] = _.map(p)
      def wander[S, T, A, B](w: Walk[S, T, A, B])(p: A => B): S => T = w.apply[Id](p)(idApplicative)
      // the direct roads: no tuple, no Either, no fold
      override def lens[S, T, A, B](get: S => A, set: (S, B) => T)(p: A => B): S => T = s => set(s, p(get(s)))
      override def prism[S, T, A, B](preview: S => Either[T, A], review: B => T)(p: A => B): S => T =
        s => preview(s) match {
          case Right(a) => review(p(a))
          case Left(t) => t
        }
      override def eachVector[A, B](p: A => B): Vector[A] => Vector[B] = _.map(p)
      def arr[A, B](f: A => B): A => B = f
      def compose[A, B, C](g: B => C, f: A => B): A => C = f.andThen(g)
    }

    implicit def forgetTraversing[R](implicit M: Monoid[R]): Traversing[Forget.Of[R]#L] = new Traversing[Forget.Of[R]#L] {
      def dimap[A, B, C, D](p: Forget[R, A, B])(f: C => A, g: B => D): Forget[R, C, D] = Forget(f.andThen(p.run))
      def first[A, B, C](p: Forget[R, A, B]): Forget[R, (A, C), (B, C)] = Forget(ac => p.run(ac._1))
      def right[A, B, C](p: Forget[R, A, B]): Forget[R, Either[C, A], Either[C, B]] = Forget(_.fold(_ => M.empty, p.run))
      def wander[S, T, A, B](w: Walk[S, T, A, B])(p: Forget[R, A, B]): Forget[R, S, T] =
        Forget(s => w.apply[Const.Of[R]#L](a => Const[R, B](p.run(a)))(Const.applicative[R]).apply(s).value)
      override def lens[S, T, A, B](get: S => A, set: (S, B) => T)(p: Forget[R, A, B]): Forget[R, S, T] = Forget(s => p.run(get(s)))
      override def prism[S, T, A, B](preview: S => Either[T, A], review: B => T)(p: Forget[R, A, B]): Forget[R, S, T] =
        Forget(s => preview(s).fold(_ => M.empty, p.run))
      override def eachVector[A, B](p: Forget[R, A, B]): Forget[R, Vector[A], Vector[B]] =
        Forget(_.foldLeft(M.empty)((r, a) => M.combine(r, p.run(a))))
    }

    /** BOTH aggregating classes in one instance, because `algebraic
     * andThen kaleidoscope` asks for the intersection. NOT `Strong`:
     * `first` would have to answer a C from a Vector of Cs */
    implicit val aggregating: Reflecting[Aggregating] with Classifying[Aggregating] = new Reflecting[Aggregating] with Classifying[Aggregating] {
      def dimap[A, B, C, D](p: Aggregating[A, B])(f: C => A, g: B => D): Aggregating[C, D] = Aggregating(cs => g(p.run(cs.map(f))))
      def reflected[F[_], A, B](p: Aggregating[A, B])(implicit F: Applicative[F]): Aggregating[F[A], F[B]] =
        Aggregating(fas => F.fmap(sequenceVector(fas), p.run))
      def classifying[S, T, A, B](view: S => A, classify: (Vector[S], B) => T)(p: Aggregating[A, B]): Aggregating[S, T] =
        Aggregating(ss => classify(ss, p.run(ss.map(view))))
    }

    implicit def star[F[_]](implicit F: Applicative[F]): Traversing[Star.Of[F]#L] = new Traversing[Star.Of[F]#L] {
      def dimap[A, B, C, D](p: Star[F, A, B])(f: C => A, g: B => D): Star[F, C, D] = Star(c => F.fmap(p.run(f(c)), g))
      def first[A, B, C](p: Star[F, A, B]): Star[F, (A, C), (B, C)] = Star(ac => F.fmap(p.run(ac._1), (b: B) => (b, ac._2)))
      def right[A, B, C](p: Star[F, A, B]): Star[F, Either[C, A], Either[C, B]] =
        Star(_.fold(c => F.pure(Left(c)), a => F.fmap(p.run(a), (b: B) => Right(b))))
      def wander[S, T, A, B](w: Walk[S, T, A, B])(p: Star[F, A, B]): Star[F, S, T] = Star(w.apply[F](p.run))
      override def lens[S, T, A, B](get: S => A, set: (S, B) => T)(p: Star[F, A, B]): Star[F, S, T] =
        Star(s => F.fmap(p.run(get(s)), (b: B) => set(s, b)))
      override def prism[S, T, A, B](preview: S => Either[T, A], review: B => T)(p: Star[F, A, B]): Star[F, S, T] =
        Star(s => preview(s).fold(t => F.pure(t), a => F.fmap(p.run(a), review)))
    }

    /** the lens's pair: Strong only, so only the always-there families run here */
    implicit def shop[A, B]: Strong[Shop.Of[A, B]#L] = new Strong[Shop.Of[A, B]#L] {
      def dimap[S, T, C, D](p: Shop[A, B, S, T])(f: C => S, g: T => D): Shop[A, B, C, D] =
        Shop[A, B, C, D](c => p.get(f(c)), (c, b) => g(p.put(f(c), b)))
      def first[S, T, C](p: Shop[A, B, S, T]): Shop[A, B, (S, C), (T, C)] =
        Shop[A, B, (S, C), (T, C)](sc => p.get(sc._1), (sc, b) => (p.put(sc._1, b), sc._2))
    }

    /** the affine pair: Strong AND Choice, deliberately NOT Traversing —
     * a pair holds one focus, so a traversal does not get `compiled` */
    implicit def market[A, B]: Strong[Market.Of[A, B]#L] with Choice[Market.Of[A, B]#L] =
      new Strong[Market.Of[A, B]#L] with Choice[Market.Of[A, B]#L] {
        def dimap[S, T, C, D](p: Market[A, B, S, T])(f: C => S, g: T => D): Market[A, B, C, D] =
          Market[A, B, C, D](c => p.look(f(c)).left.map(g), (c, b) => g(p.put(f(c), b)))
        def first[S, T, C](p: Market[A, B, S, T]): Market[A, B, (S, C), (T, C)] =
          Market[A, B, (S, C), (T, C)](sc => p.look(sc._1).left.map(t => (t, sc._2)), (sc, b) => (p.put(sc._1, b), sc._2))
        def right[S, T, C](p: Market[A, B, S, T]): Market[A, B, Either[C, S], Either[C, T]] =
          Market[A, B, Either[C, S], Either[C, T]](
            _.fold(c => Left(Left(c)), s => p.look(s).left.map(Right(_))),
            (e, b) => e.fold(Left(_), s => Right(p.put(s, b))))
      }

    /** a typestate transition is `Strong` in its state: `first` carries
     * the right half of a paired state past the program untouched —
     * which is what a lens does to a record, so `PState.zoom` is the
     * optic run at this carrier */
    implicit def zooming[X, R]: Strong[PState.Zooming[X, R]#L] = new Strong[PState.Zooming[X, R]#L] {
      def dimap[A, B, C, D](p: Cont[X, B => R, A => R])(f: C => A, g: B => D): Cont[X, D => R, C => R] =
        shift[X, D => R, C => R](k => (c: C) => (p / ((x: X) => (b: B) => k(x)(g(b))))(f(c)))
      def first[A, B, C](p: Cont[X, B => R, A => R]): Cont[X, ((B, C)) => R, ((A, C)) => R] =
        shift[X, ((B, C)) => R, ((A, C)) => R](k => (ac: (A, C)) => (p / ((x: X) => (b: B) => k(x)((b, ac._2))))(ac._1))
      // no tuple per zoom: the body `zoom` had before this instance existed
      override def lens[S1, S2, A1, A2](get: S1 => A1, set: (S1, A2) => S2)(p: Cont[X, A2 => R, A1 => R]): Cont[X, S2 => R, S1 => R] =
        shift[X, S2 => R, S1 => R](k => (s1: S1) => (p / ((x: X) => (a2: A2) => k(x)(set(s1, a2))))(get(s1)))
    }
  }

  trait LowProfunctor {
    implicit def forgetStrong[R]: Strong[Forget.Of[R]#L] = new Strong[Forget.Of[R]#L] {
      def dimap[A, B, C, D](p: Forget[R, A, B])(f: C => A, g: B => D): Forget[R, C, D] = Forget(f.andThen(p.run))
      def first[A, B, C](p: Forget[R, A, B]): Forget[R, (A, C), (B, C)] = Forget(ac => p.run(ac._1))
      override def lens[S, T, A, B](get: S => A, set: (S, B) => T)(p: Forget[R, A, B]): Forget[R, S, T] = Forget(s => p.run(get(s)))
    }
  }

  // ---------------------------------------------------------------- the operations

  /** the operations, on any optic the interpretation meets. The core
   * FUSES `set`/`modify`/`get` at compile time (its `Fuse` macro); here
   * they are the interpretation, which the core itself falls back to
   * for an optic held in a `val`.
   *
   * SCALA 2 SPELLS THE CORE'S `(using ...)` AS A CLASS-LEVEL IMPLICIT
   * where the operation answers a function: `l.set(b)(s)` must pass `s`
   * to the function, and an implicit parameter list on `set` itself
   * would take `s` as the evidence instead. So `set`/`modify` live on a
   * view that needs `C[Function1]`, `aggregate` on one that needs
   * `C[Aggregating]`, and `traverseOf` takes the whole as a second list
   * (`o.traverseOf(f)(s)`, the shape `foldMap` already has). */
  implicit final class FunctionOps[C[_[_, _]], S, T, A, B](o: Optic[C, S, T, A, B])(implicit fn: C[Function1]) {
    /** every focus through `f` */
    def modify(f: A => B): S => T = o[Function1](f)
    /** every focus replaced */
    def set(b: B): S => T = o[Function1]((_: A) => b)
  }

  implicit final class AggregateOps[C[_[_, _]], S, T, A, B](o: Optic[C, S, T, A, B])(implicit ag: C[Aggregating]) {
    /** many wholes in, their focuses aggregated by `f`, one whole out */
    def aggregate(f: Vector[A] => B): Vector[S] => T = o[Aggregating](Aggregating(f)).run
    /** the same, with a named aggregation algebra */
    def aggregateWith[Acc](agg: Aggregator[A, Acc, B]): Vector[S] => T =
      aggregate(as => agg.present(as.foldLeft(agg.init)(agg.add)))
  }

  implicit final class OpticOps[C[_[_, _]], S, T, A, B](private val o: Optic[C, S, T, A, B]) extends AnyVal {
    /** the focus of a lens (or an iso) */
    def get(s: S)(implicit fn: C[Forget.Of[A]#L]): A = o[Forget.Of[A]#L](Forget[A, A, B](identity)).run(s)
    /** the first focus, if any */
    def preview(s: S)(implicit fn: C[Forget.Of[First[A]]#L]): Option[A] =
      o[Forget.Of[First[A]]#L](Forget[First[A], A, B](a => First(Some(a)))).run(s).value
    /** the foci, combined */
    def foldMap[R](f: A => R)(s: S)(implicit fn: C[Forget.Of[R]#L]): R = o[Forget.Of[R]#L](Forget[R, A, B](f)).run(s)
    /** the foci, in order */
    def toVector(s: S)(implicit fn: C[Forget.Of[Vector[A]]#L]): Vector[A] =
      o[Forget.Of[Vector[A]]#L](Forget[Vector[A], A, B](Vector(_))).run(s)
    /** every focus through an effectful `f`, effects in order */
    def traverseOf[F[_]](f: A => F[B])(s: S)(implicit fn: C[Star.Of[F]#L]): F[T] = o[Star.Of[F]#L](Star[F, A, B](f)).run(s)
    /** a LENS run once at its own pair — Strong only */
    def compiledLens(implicit sh: C[Shop.Of[A, B]#L]): CompiledLens[S, T, A, B] = {
      val p = o[Shop.Of[A, B]#L](idShop[A, B])
      new CompiledLens(p.get, p.put)
    }
    /** the optic run once at `Market`: the affine pair. A traversal has
     * no instance here, and the missing implicit says so */
    def compiled(implicit mk: C[Market.Of[A, B]#L]): Compiled[S, T, A, B] = {
      val m = o[Market.Of[A, B]#L](idMarket[A, B])
      new Compiled(m.look, m.put)
    }
  }

  /**
   * THE LITERATURE'S GLYPHS, BEHIND AN IMPORT (`import
   * okay2.Optic.arrows._`) — Hughes (2000), `Control.Arrow`. Behind an
   * import because they apply to every two-parameter type. No glyph for
   * optic composition: `andThen` is that, one glyph, one meaning.
   */
  object arrows {
    /** the evidence at the CLASS, so `(f >>> g)(x)` applies the arrow
     * rather than passing `x` as the evidence */
    implicit final class ArrowOps[P[_, _], A, B](p: P[A, B])(implicit Ar: Arrow[P]) {
      /** left to right: `p` then `q` */
      def >>>[C](q: P[B, C]): P[A, C] = Ar.compose(q, p)
      /** right to left */
      def <<<[Z](q: P[Z, A]): P[Z, B] = Ar.compose(p, q)
      /** a pair, each half through its own arrow */
      def ***[C, D](q: P[C, D]): P[(A, C), (B, D)] = Ar.split(p, q)
      /** one input, both arrows, both answers */
      def &&&[C](q: P[A, C]): P[A, (B, C)] = Ar.fanout(p, q)
    }

    implicit final class ArrowChoiceOps[P[_, _], A, B](p: P[A, B])(implicit Ar: Arrow[P], Ch: Choice[P]) {
      /** a sum, each side through its own arrow */
      def +++[X, Y](q: P[X, Y]): P[Either[A, X], Either[B, Y]] = Ar.compose(Ch.right[X, Y, B](q), Ch.left[A, B, X](p))
      /** a sum, both sides to one answer */
      def |||[X](q: P[X, B]): P[Either[A, X], B] = Ar.compose(Ar.arr((e: Either[B, B]) => e.merge), +++[X, B](q))
    }
  }

  // ---------------------------------------------------------------- constructors

  object Iso {
    /** Kmett's `non`: an absent value READS as `d`, and writing `d`
     * makes it absent again — an iso modulo `Some(d) ~ None` */
    def non[A](d: A): Iso[Option[A], Option[A], A, A] = Iso[Option[A], Option[A], A, A](_.getOrElse(d), a => if (a == d) None else Some(a))

    def apply[S, T, A, B](to: S => A, from: B => T): Iso[S, T, A, B] = new Iso[S, T, A, B] {
      def apply[P[_, _]](p: P[A, B])(implicit P: Profunctor[P]): P[S, T] = P.dimap(p)(to, from)
    }
  }

  object Lens {
    def apply[S, T, A, B](get: S => A, set: (S, B) => T): Lens[S, T, A, B] = new Lens[S, T, A, B] {
      def apply[P[_, _]](p: P[A, B])(implicit P: Strong[P]): P[S, T] = P.lens(get, set)(p)
    }

    /** `Lens[S](_.field)` — the selector macro */
    def apply[S <: Product]: Focus[S] = new Focus[S]

    /** `Lens.field[S]("name")` — by name, typed by the case class */
    def field[S <: Product]: FieldOf[S] = new FieldOf[S]
  }

  /** `Lens[S](_.f)`: the lambda is the getter, the setter a `copy` of
   * that one field; anything but `_.f` is refused at compile time */
  final class Focus[S] {
    def apply[A](get: S => A): Lens[S, S, A, A] = macro OpticMacros.focus[S, A]
  }

  /** `Lens.field[S]("name")`: the focus type is the field's declared
   * type, and a name that is not a case field does not compile */
  final class FieldOf[S] {
    def apply(name: String): Lens[S, S, _, _] = macro OpticMacros.field[S]
  }

  /** the affine traversal: zero or one focus. Composing a lens with a
   * prism yields this type on its own; this is for the ones that are
   * not such a composition */
  object Affine {
    def apply[S, T, A, B](preview: S => Either[T, A], set: (S, B) => T): Affine[S, T, A, B] = new Affine[S, T, A, B] {
      def apply[P[_, _]](p: P[A, B])(implicit P: Strong[P] with Choice[P]): P[S, T] =
        P.prism[S, T, (A, S), (B, S)](s => preview(s).map(a => (a, s)), (bs: (B, S)) => set(bs._2, bs._1))(P.first[A, B, S](p))
    }
  }

  object Prism {
    def apply[S, T, A, B](preview: S => Either[T, A], review: B => T): Prism[S, T, A, B] = new Prism[S, T, A, B] {
      def apply[P[_, _]](p: P[A, B])(implicit P: Choice[P]): P[S, T] = P.prism(preview, review)(p)
    }

    /** Option's Some */
    def some[A, B]: Prism[Option[A], Option[B], A, B] = Prism[Option[A], Option[B], A, B](_.toRight(None), Some(_))

    /** one case of a hierarchy: the preview is the type test, the review the widening */
    def of[S, A <: S](implicit ct: ClassTag[A]): Prism[S, S, A, A] = Prism[S, S, A, A](s => ct.unapply(s).toRight(s), identity)
  }

  object Traversal {
    def apply[S, T, A, B](w: Walk[S, T, A, B]): Traversal[S, T, A, B] = new Traversal[S, T, A, B] {
      def apply[P[_, _]](p: P[A, B])(implicit P: Traversing[P]): P[S, T] = P.wander(w)(p)
    }

    /** every element of a Vector — through `eachVector`, the interpretation's direct road */
    def each[A, B]: Traversal[Vector[A], Vector[B], A, B] = new Traversal[Vector[A], Vector[B], A, B] {
      def apply[P[_, _]](p: P[A, B])(implicit P: Traversing[P]): P[Vector[A], Vector[B]] = P.eachVector(p)
    }

    /** every element of a List */
    def eachList[A, B]: Traversal[List[A], List[B], A, B] = Traversal(new Walk[List[A], List[B], A, B] {
      def apply[F[_]](f: A => F[B])(implicit F: Applicative[F]): List[A] => F[List[B]] =
        as => as.foldRight(F.pure(List.empty[B]))((a, acc) => F.app(F.fmap(f(a), (b: B) => (l: List[B]) => b :: l), acc))
    })
  }

  /** the kaleidoscope: aggregate the focuses of many wholes, position-wise
   * by whatever the applicative's `app` means */
  object Kaleidoscope {
    def each[F[_], A, B](implicit F: Applicative[F]): Kaleidoscope[F[A], F[B], A, B] = new Kaleidoscope[F[A], F[B], A, B] {
      def apply[P[_, _]](p: P[A, B])(implicit P: Reflecting[P]): P[F[A], F[B]] = P.reflected(p)
    }
  }

  /** the algebraic (classifying) lens: `view` one whole, `classify`
   * from ALL the wholes and the aggregated focus */
  object AlgebraicLens {
    def apply[S, T, A, B](view: S => A, classify: (Vector[S], B) => T): AlgebraicLens[S, T, A, B] = new AlgebraicLens[S, T, A, B] {
      def apply[P[_, _]](p: P[A, B])(implicit P: Classifying[P]): P[S, T] = P.classifying(view, classify)(p)
    }
  }

  // ---------------------------------------------------------------- zooming by a lens

  /** the lens spelling of `State.zoomWith`: a program over a PART of
   * the state, run over the whole */
  def zoomLens[S, A, X, R <: Row](l: Lens[S, S, A, A])(p: Free[State[A] with R, X])(implicit d: Distinct[State[A] with R]): Free[State[S] with R, X] =
    State.zoomWith[S, A, X, R](s => l.get(s), a => s => l.set(a)(s))(p)

  implicit final class StateZoom(private val st: State.type) extends AnyVal {
    /** `State.zoom(lens)(prog)`, as every caller of the core writes it */
    def zoom[S, A, X, R <: Row](l: Lens[S, S, A, A])(p: Free[State[A] with R, X])(implicit d: Distinct[State[A] with R]): Free[State[S] with R, X] =
      zoomLens[S, A, X, R](l)(p)
  }

  implicit final class PStateZoom(private val ps: PState.type) extends AnyVal {
    /** a typestate program over a PART, run over the whole: the
     * four-parameter lens is a type-changing update, and zooming by it
     * is the optic run at the `Zooming` carrier (theory ch. 3) */
    def zoom[S1, S2, A1, A2, X, R](l: Lens[S1, S2, A1, A2])(m: Cont[X, A2 => R, A1 => R]): Cont[X, S2 => R, S1 => R] =
      l[PState.Zooming[X, R]#L](m)

    /** A PRISM CANNOT BE AN INSTANCE THERE: on the absent case the
     * program has no `X` to give. So the door says what it costs — the
     * answer becomes `Option[X]` */
    def zoomCase[S1, S2, A1, A2, X, R](p: Prism[S1, S2, A1, A2])(m: Cont[X, A2 => R, A1 => R]): Cont[Option[X], S2 => R, S1 => R] = {
      val pair = p.compiled
      shift[Option[X], S2 => R, S1 => R](k => (s1: S1) => pair.look(s1) match {
        case Right(a1) => (m / ((x: X) => (a2: A2) => k(Some(x))(pair.put(s1, a2))))(a1)
        case Left(s2) => k(None)(s2)
      })
    }

    /** the `Strong` instance for the zooming carrier, by name */
    def strong[X, R]: Strong[PState.Zooming[X, R]#L] = Profunctor.zooming[X, R]
  }
}
