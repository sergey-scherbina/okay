package okay

import scala.deriving.Mirror
import scala.compiletime.constValue
import scala.reflect.ClassTag
import scala.annotation.unused

/**
 * Optics on profunctors (specs/optics.md). An optic is a function
 * polymorphic in a profunctor `P`, and its CONSTRAINT on `P` is a type
 * parameter: a lens asks for `Strong`, a prism for `Choice`, a
 * traversal for `Traversing`, an iso for a bare `Profunctor`.
 * Composition takes the INTERSECTION of the two constraints, which is
 * the meet of the lattice — `lens andThen prism` asks for
 * `Strong[P] & Choice[P]`, the affine traversal — and an
 * interpretation satisfies the meet by subtyping, since `Traversing`
 * extends both. Nobody writes a table of family pairs.
 *
 * Nominal on purpose: a transparent alias for the polymorphic
 * function value cannot have its constraint inferred by an extension
 * method (found by the prototype); a trait costs one object per optic.
 *
 * `S` and `T`, `A` and `B`: the whole before and after, the part
 * before and after — the type-changing optic, which is Atkey's
 * parameterised state seen from the other side (theory textbook
 * ch. 3; stage 3 of the spec makes that literal with `PState`).
 */
trait Optic[C[_[_, _]], S, T, A, B]:
  self =>
  def apply[P[_, _]](p: P[A, B])(using C[P]): P[S, T]

  /** composition: the constraint is the intersection — the meet */
  def andThen[C2[_[_, _]], A2, B2](o: Optic[C2, A, B, A2, B2]): Optic[[P[_, _]] =>> C[P] & C2[P], S, T, A2, B2] =
    new Optic[[P[_, _]] =>> C[P] & C2[P], S, T, A2, B2]:
      def apply[P[_, _]](p: P[A2, B2])(using c: C[P] & C2[P]): P[S, T] = self(o(p))

type Iso[S, T, A, B] = Optic[Optic.Profunctor, S, T, A, B]
type Lens[S, T, A, B] = Optic[Optic.Strong, S, T, A, B]
type Prism[S, T, A, B] = Optic[Optic.Choice, S, T, A, B]
type Affine[S, T, A, B] = Optic[[P[_, _]] =>> Optic.Strong[P] & Optic.Choice[P], S, T, A, B]
type Traversal[S, T, A, B] = Optic[Optic.Traversing, S, T, A, B]

/**
 * The aggregating families (Clarke, Elkins, Gibbons, Loregian,
 * Milewski, Pillmore and Roman, "Profunctor Optics, a Categorical
 * Update", Compositionality 2024; the practitioner's account is
 * Penner's Kaleidoscopes).
 *
 * A traversal WALKS a Traversable and keeps its shape. A kaleidoscope
 * lifts through an APPLICATIVE and collapses many focuses into one
 * answer — grouping, not iteration. An algebraic (classifying) lens
 * puts by an ALGEBRA over many wholes rather than by a value, which is
 * what "decide what this is, given everything seen" needs.
 */
type Kaleidoscope[S, T, A, B] = Optic[Optic.Reflecting, S, T, A, B]
type AlgebraicLens[S, T, A, B] = Optic[Optic.Classifying, S, T, A, B]

/** the lattice, and the carriers of the interpretations */
object Optic {

  trait Profunctor[P[_, _]]:
    def dimap[A, B, C, D](p: P[A, B])(f: C => A, g: B => D): P[C, D]

  /**
   * A lens's requirement: `first` is the structure map of a Tambara
   * module. `lens` is DERIVED from it — the default is the textbook
   * `dimap(first(p))(s => (get(s), s), (b, s) => set(s, b))` — and an
   * interpretation may override it with a direct road: the gate
   * measured the tuple that default builds at 12x a `copy`
   * (specs/optics.md Results), and `Function1`'s override is
   * `s => set(s, p(get(s)))`. TestOptics asserts every override
   * agrees with the default it replaces.
   */
  trait Strong[P[_, _]] extends Profunctor[P]:
    def first[A, B, C](p: P[A, B]): P[(A, C), (B, C)]
    /** the mirror of `first`, derived by swapping; overridable */
    def second[A, B, C](p: P[A, B]): P[(C, A), (C, B)] =
      dimap(first[A, B, C](p))((ca: (C, A)) => (ca._2, ca._1), (bc: (B, C)) => (bc._2, bc._1))
    def lens[S, T, A, B](get: S => A, set: (S, B) => T)(p: P[A, B]): P[S, T] =
      dimap(first[A, B, S](p))((s: S) => (get(s), s), (bs: (B, S)) => set(bs._2, bs._1))

  /**
   * Sequential composition — the OTHER row of the same table.
   *
   * Rivas and Jaskelioff ("Notions of Computation as Monoids", JFP
   * 2017): a monad is a monoid in endofunctors, an applicative a
   * monoid for Day convolution, and an ARROW a strong monoid in the
   * category of profunctors. An optic is not a monoid there at all,
   * it is a Tambara module — an action of a monoidal category on a
   * profunctor. So arrows and optics are neighbours, not rivals, and
   * both are written on `Profunctor` here for that reason.
   */
  trait Category[P[_, _]]:
    def id[A]: P[A, A]
    def compose[A, B, C](g: P[B, C], f: P[A, B]): P[A, C]

  /** Category + Strong + a lifted function, the usual decomposition */
  trait Arrow[P[_, _]] extends Category[P] with Strong[P]:
    def arr[A, B](f: A => B): P[A, B]
    def id[A]: P[A, A] = arr(identity)
    /** the two sides of a pair, each through its own arrow */
    def split[A, B, C, D](f: P[A, B], g: P[C, D]): P[(A, C), (B, D)] =
      compose(second[C, D, B](g), first[A, B, C](f))
    /** one input, both arrows, both answers */
    def fanout[A, B, C](f: P[A, B], g: P[A, C]): P[A, (B, C)] =
      compose(split(f, g), arr((a: A) => (a, a)))

  /** a prism's requirement; `prism` derived from `right`, overridable likewise */
  trait Choice[P[_, _]] extends Profunctor[P]:
    def right[A, B, C](p: P[A, B]): P[Either[C, A], Either[C, B]]
    def prism[S, T, A, B](preview: S => Either[T, A], review: B => T)(p: P[A, B]): P[S, T] =
      dimap(right[A, B, T](p))(preview, (e: Either[T, B]) => e.fold(identity, review))

  /** a traversable shape, applicative-polymorphic: the `Wander` of Purescript */
  type Walk[S, T, A, B] = [F[_]] => Applicative[F] ?=> (A => F[B]) => S => F[T]

  /** every element of a Vector, as a Walk */
  def vectorWalk[A, B]: Walk[Vector[A], Vector[B], A, B] =
    [F[_]] => (F: Applicative[F]) ?=> (f: A => F[B]) => (as: Vector[A]) =>
      as.foldLeft(F.pure(Vector.empty[B]))((acc, a) => F.fmap(acc, (v: Vector[B]) => (b: B) => v :+ b).app(f(a)))

  /** a traversal's requirement; also a lens's and a prism's, by
   * extension. `eachVector` is derived from `wander` over `vectorWalk`
   * and overridable: `Function1`'s is `_.map(p)` */
  trait Traversing[P[_, _]] extends Strong[P] with Choice[P]:
    def wander[S, T, A, B](w: Walk[S, T, A, B])(p: P[A, B]): P[S, T]
    def eachVector[A, B](p: P[A, B]): P[Vector[A], Vector[B]] = wander(vectorWalk[A, B])(p)

  /** NOT a given: an `Applicative[[X] =>> X]` in scope would be ambiguous
   * with the package's `Comonad[Id]`, which already puts `map` on every
   * type; `Function1`'s `wander` passes this one explicitly */
  val idApplicative: Applicative[Id] = new Applicative[Id]:
    def pure[A](a: A): A = a
    extension [A, B](f: A => B) def app(a: A): B = f(a)

  /**
   * A kaleidoscope's requirement: lift through ANY Applicative.
   *
   * Next to `Traversing.wander`, which needs a traversable SHAPE and
   * returns it, this needs an applicative and returns one answer. That
   * is the whole difference between iterating and aggregating, and it
   * is why the two families are not one.
   */
  trait Reflecting[P[_, _]] extends Profunctor[P]:
    def reflected[F[_], A, B](p: P[A, B])(using Applicative[F]): P[F[A], F[B]]

  /**
   * An algebraic lens's requirement: view one whole, and put by an
   * algebra that sees ALL the wholes. `Strong.lens` puts a value;
   * this classifies — the paper's example is a measurement placed
   * against a dataset, ours is in TestAggregationOptics.
   *
   * Given as its constructor rather than as a structure map, and the
   * reason is honest: the paper says the laws of a mixed optic are
   * not settled outside particular cases (monadic lenses being one),
   * so this claims no Tambara derivation and states no law it has not
   * tested.
   */
  trait Classifying[P[_, _]] extends Profunctor[P]:
    def classifying[S, T, A, B](view: S => A, classify: (Vector[S], B) => T)(p: P[A, B]): P[S, T]

  /** the aggregating interpretation: many focuses in, one answer out */
  final case class Aggregating[A, B](run: Vector[A] => B)

  /** `Vector[F[A]] => F[Vector[A]]`, which is `vectorWalk` at identity */
  def sequenceVector[F[_], A](vs: Vector[F[A]])(using Applicative[F]): F[Vector[A]] =
    vectorWalk[F[A], A].apply[F]((fa: F[A]) => fa)(vs)

  /**
   * The ZIP applicative, and why it is a LazyList and not a Vector.
   *
   * Column-wise aggregation is zipping, and `pure` for zipping must
   * be the INFINITE repeat: `pure(f) <*> xs == fmap(xs)(f)` fails for
   * any finite pure the moment `xs` is longer than it. A Vector cannot
   * hold that, so the lawful zip applicative on a strict sequence does
   * not exist and this one is lazy. Not a given: the cartesian
   * (monadic) applicative for a sequence is the usual one, and two in
   * scope would be ambiguous.
   */
  val zipLazy: Applicative[LazyList] = new Applicative[LazyList]:
    def pure[A](a: A): LazyList[A] = LazyList.continually(a)
    extension [A, B](f: LazyList[A => B])
      def app(a: LazyList[A]): LazyList[B] = f.zip(a).map((g, x) => g(x))

  /** read and forget the rest: `get` (Strong), and with a Monoid `preview`, `foldMap`, `toVector` */
  final case class Forget[R, A, B](run: A => R)

  /** the constant functor, an Applicative by the Monoid — what `foldMap` walks with */
  final case class Const[R, A](value: R)

  /** first-wins: `preview`'s monoid */
  final case class First[A](value: Option[A])
  object First:
    given monoid[A]: Monoid[First[A]] with
      def empty: First[A] = First(None)
      def combine(x: First[A], y: First[A]): First[A] = if x.value.isDefined then x else y

  /** effectful functions: `traverse` for any Applicative — with `[x] =>> x ! Row`, in the effect row */
  final case class Star[F[_], A, B](run: A => F[B])

  // ---------------------------------------------------------------- compiling an optic (optics-fast)

  /**
   * THE CONCRETE REPRESENTATION, WHICH IS ITSELF A PROFUNCTOR.
   *
   * `Market[A, B, S, T]` is the pair an affine optic is made of — how
   * to look at an `S` (a focus, or the `T` it already is) and how to
   * put a `B` back. It is a `Strong` and a `Choice` profunctor in
   * `(S, T)`, so an optic can be INSTANTIATED at it: run the optic
   * once at the identity market and what comes out is that optic's own
   * pair. That is the existential-to-profunctor isomorphism of theory
   * chapter 10 (Boisseau & Gibbons 2018), executed rather than cited.
   *
   * IT IS NOT A FAST PATH, and it was built to be one. The lane's
   * premise was that a composed optic re-interprets itself on every
   * call and that paying the chain once would help. Measured
   * (specs/optics.md, optics-fast), it is the other way round:
   *
   *   one field, `Lens[S](_.f).set`     3.0 ns
   *   the same, compiled without Either 3.9 ns
   *   the same, compiled through Market 8.0 ns
   *   composed, live                   15.1 ns
   *   composed, compiled               26.7 ns
   *
   * Two reasons, both instructive. The `Either` a `Market` must carry
   * costs more than everything it saves (3.9 against 8.0 is the same
   * compilation with it and without). And the chain was never the
   * cost: an optic held in a `val` gives the JIT a monomorphic call
   * site it inlines through, while a compiled pair is a field holding
   * a lambda — one indirect call it does not. THE JIT ALREADY DOES
   * THIS COMPILATION, AND BETTER.
   *
   * So this is kept the way `Fused` is kept: the artifact a
   * measurement was taken on, lawful and tested, so the number can be
   * taken again — and so that the isomorphism chapter 10 cites can be
   * run. Reach for it when you need the PAIR (to hand across a
   * boundary, to store an optic as data), never for speed.
   */
  final case class Market[A, B, S, T](look: S => Either[T, A], put: (S, B) => T)

  /** a compiled optic: the pair, with the operations as direct calls
   * — which is slower than the optic, see above */
  final class Compiled[S, T, A, B](val look: S => Either[T, A], val put: (S, B) => T):
    /** the focus, if this optic has one here */
    inline def preview(s: S): Option[A] = look(s).toOption
    /** every focus replaced — one call, no interpretation */
    inline def set(b: B): S => T = s => look(s) match
      case Right(_) => put(s, b)
      case Left(t) => t
    /** the focus through `f` */
    inline def modify(f: A => B): S => T = s => look(s) match
      case Right(a) => put(s, f(a))
      case Left(t) => t

  /** the identity market: what an optic is run at to yield its own pair */
  private[okay] def idMarket[A, B]: Market[A, B, A, B] = Market(Right(_), (_, b) => b)

  /**
   * The lens's concrete pair — `Market` without the `Either`, because
   * a lens's focus is always there. `Strong` only: a prism cannot be
   * run here, which is the point of having both.
   */
  final case class Shop[A, B, S, T](get: S => A, put: (S, B) => T)

  /** a compiled lens: two direct calls, and no Either between them */
  final class CompiledLens[S, T, A, B](val get: S => A, val put: (S, B) => T):
    inline def set(b: B): S => T = s => put(s, b)
    inline def modify(f: A => B): S => T = s => put(s, f(get(s)))

  private[okay] def idShop[A, B]: Shop[A, B, A, B] = Shop(identity, (_, b) => b)

  // ---------------------------------------------------------------- Lens.field: by name, typed by the Mirror

  /** the index of a label in the Mirror's label tuple, at the type level */
  type IndexOf[Labels <: Tuple, L, N <: Int] <: Int = Labels match
    case L *: _ => N
    case _ *: rest => IndexOf[rest, L, compiletime.ops.int.S[N]]

  /** `Lens.field[S]("name")`: the case-class type first, then the name as a literal */
  final class FieldOf[S <: Product]:
    /**
     * A lens on a case-class field BY NAME, typed by the Mirror: the
     * focus type is the field's declared type, a wrong name is a
     * compile error (the match type has no case for it). No macro: the
     * Mirror knows the labels and the types. The one cast is the same
     * `productElement` cast `Schema.eachField` isolates, for the same
     * reason — the Mirror's parts are erased.
     */
    inline def apply[L <: String & Singleton](@unused name: L)(using m: Mirror.ProductOf[S])
      : Lens[S, S, Tuple.Elem[m.MirroredElemTypes, IndexOf[m.MirroredElemLabels, L, 0]],
                   Tuple.Elem[m.MirroredElemTypes, IndexOf[m.MirroredElemLabels, L, 0]]] =
      val i = constValue[IndexOf[m.MirroredElemLabels, L, 0]]
      type A = Tuple.Elem[m.MirroredElemTypes, IndexOf[m.MirroredElemLabels, L, 0]]
      Lens[S, S, A, A](
        s => s.productElement(i).asInstanceOf[A],
        (s, a) => m.fromProduct(Replaced(s, i, a)))

  /** a product with ONE element replaced, as `fromProduct` reads it:
   * a view over the original, no array and no Tuple in between */
  final class Replaced(s: Product, i: Int, a: Any) extends Product:
    def canEqual(that: Any): Boolean = false
    def productArity: Int = s.productArity
    def productElement(n: Int): Any = if n == i then a else s.productElement(n)
}

// ---------------------------------------------------------------- the interpretations (ride `import okay.given`)

import Optic.{Profunctor, Strong, Choice, Traversing, Reflecting, Classifying, Aggregating, Walk, Forget, Const, First, Star, Market, Compiled, Shop, CompiledLens}

/** plain functions: `modify` and `set` */
given opticFunction1: Traversing[Function1] with
  def dimap[A, B, C, D](p: A => B)(f: C => A, g: B => D): C => D = f.andThen(p).andThen(g)
  def first[A, B, C](p: A => B): ((A, C)) => (B, C) = ac => (p(ac._1), ac._2)
  def right[A, B, C](p: A => B): Either[C, A] => Either[C, B] = _.map(p)
  def wander[S, T, A, B](w: Walk[S, T, A, B])(p: A => B): S => T = w[Id](using Optic.idApplicative)(p)
  // the direct roads: no tuple, no Either, no fold — the same functions
  override def lens[S, T, A, B](get: S => A, set: (S, B) => T)(p: A => B): S => T = s => set(s, p(get(s)))
  override def prism[S, T, A, B](preview: S => Either[T, A], review: B => T)(p: A => B): S => T =
    s => preview(s) match
      case Right(a) => review(p(a))
      case Left(t) => t
  override def eachVector[A, B](p: A => B): Vector[A] => Vector[B] = _.map(p)

given opticForgetStrong[R]: Strong[[A, B] =>> Forget[R, A, B]] with
  def dimap[A, B, C, D](p: Forget[R, A, B])(f: C => A, g: B => D): Forget[R, C, D] = Forget(f.andThen(p.run))
  def first[A, B, C](p: Forget[R, A, B]): Forget[R, (A, C), (B, C)] = Forget(ac => p.run(ac._1))
  override def lens[S, T, A, B](get: S => A, set: (S, B) => T)(p: Forget[R, A, B]): Forget[R, S, T] = Forget(s => p.run(get(s)))

given opticForgetTraversing[R](using M: Monoid[R]): Traversing[[A, B] =>> Forget[R, A, B]] with
  def dimap[A, B, C, D](p: Forget[R, A, B])(f: C => A, g: B => D): Forget[R, C, D] = Forget(f.andThen(p.run))
  def first[A, B, C](p: Forget[R, A, B]): Forget[R, (A, C), (B, C)] = Forget(ac => p.run(ac._1))
  def right[A, B, C](p: Forget[R, A, B]): Forget[R, Either[C, A], Either[C, B]] =
    Forget(_.fold(_ => M.empty, p.run))
  def wander[S, T, A, B](w: Walk[S, T, A, B])(p: Forget[R, A, B]): Forget[R, S, T] =
    Forget(s => w[[X] =>> Const[R, X]](a => Const(p.run(a)))(s).value)
  override def lens[S, T, A, B](get: S => A, set: (S, B) => T)(p: Forget[R, A, B]): Forget[R, S, T] = Forget(s => p.run(get(s)))
  override def prism[S, T, A, B](preview: S => Either[T, A], review: B => T)(p: Forget[R, A, B]): Forget[R, S, T] =
    Forget(s => preview(s).fold(_ => M.empty, p.run))
  override def eachVector[A, B](p: Forget[R, A, B]): Forget[R, Vector[A], Vector[B]] =
    Forget(_.foldLeft(M.empty)((r, a) => M.combine(r, p.run(a))))

/**
 * The aggregating interpretation is BOTH classes at once, in one
 * instance, because `algebraic andThen kaleidoscope` asks for the
 * intersection and an intersection is satisfied by one value.
 *
 * Deliberately NOT `Strong`: `first` would have to answer a `C` from
 * a `Vector[C]`, and there is no honest choice. So an ordinary lens
 * does not compose into this road — the classifying lens is what
 * stands in its place, which is the papers' point and not a gap.
 */
given opticAggregating: (Reflecting[Aggregating] & Classifying[Aggregating]) =
  new Reflecting[Aggregating] with Classifying[Aggregating]:
    def dimap[A, B, C, D](p: Aggregating[A, B])(f: C => A, g: B => D): Aggregating[C, D] =
      Aggregating(cs => g(p.run(cs.map(f))))
    def reflected[F[_], A, B](p: Aggregating[A, B])(using F: Applicative[F]): Aggregating[F[A], F[B]] =
      Aggregating(fas => F.fmap(Optic.sequenceVector(fas), p.run))
    def classifying[S, T, A, B](view: S => A, classify: (Vector[S], B) => T)(
        p: Aggregating[A, B]): Aggregating[S, T] =
      Aggregating(ss => classify(ss, p.run(ss.map(view))))

given opticConstApplicative[R](using M: Monoid[R]): Applicative[[A] =>> Const[R, A]] with
  def pure[A](a: A): Const[R, A] = Const(M.empty)
  extension [A, B](f: Const[R, A => B]) def app(a: Const[R, A]): Const[R, B] = Const(M.combine(f.value, a.value))

given opticStarTraversing[F[_]](using F: Applicative[F]): Traversing[[A, B] =>> Star[F, A, B]] with
  def dimap[A, B, C, D](p: Star[F, A, B])(f: C => A, g: B => D): Star[F, C, D] = Star(c => F.fmap(p.run(f(c)), g))
  def first[A, B, C](p: Star[F, A, B]): Star[F, (A, C), (B, C)] = Star(ac => F.fmap(p.run(ac._1), (b: B) => (b, ac._2)))
  def right[A, B, C](p: Star[F, A, B]): Star[F, Either[C, A], Either[C, B]] =
    Star(_.fold(c => F.pure(Left(c)), a => F.fmap(p.run(a), (b: B) => Right(b))))
  def wander[S, T, A, B](w: Walk[S, T, A, B])(p: Star[F, A, B]): Star[F, S, T] = Star(w[F](p.run))
  override def lens[S, T, A, B](get: S => A, set: (S, B) => T)(p: Star[F, A, B]): Star[F, S, T] =
    Star(s => F.fmap(p.run(get(s)), (b: B) => set(s, b)))
  override def prism[S, T, A, B](preview: S => Either[T, A], review: B => T)(p: Star[F, A, B]): Star[F, S, T] =
    Star(s => preview(s).fold(t => F.pure(t), a => F.fmap(p.run(a), review)))

/**
 * The concrete pair as a profunctor: `Strong` AND `Choice`, and
 * deliberately NOT `Traversing`. A pair holds one focus, so there is
 * no honest `wander` for it — which means `.compiled` is available for
 * an iso, a lens, a prism and an affine, and a traversal does not get
 * it rather than getting a lie. (A traversal has no measured problem:
 * stage 0's gate put `Traversal.each.modify` at 1.00x of `Vector.map`.)
 */
/** the lens's pair as a Strong profunctor — no Choice, so only the
 * always-there families run here */
given opticShop[A, B]: Strong[[S, T] =>> Shop[A, B, S, T]] with
  def dimap[S, T, C, D](p: Shop[A, B, S, T])(f: C => S, g: T => D): Shop[A, B, C, D] =
    Shop(c => p.get(f(c)), (c, b) => g(p.put(f(c), b)))
  def first[S, T, C](p: Shop[A, B, S, T]): Shop[A, B, (S, C), (T, C)] =
    Shop({ case (s, _) => p.get(s) }, { case ((s, c), b) => (p.put(s, b), c) })

given opticMarket[A, B]: (Strong[[S, T] =>> Market[A, B, S, T]] & Choice[[S, T] =>> Market[A, B, S, T]]) =
  new Strong[[S, T] =>> Market[A, B, S, T]] with Choice[[S, T] =>> Market[A, B, S, T]]:
    def dimap[S, T, C, D](p: Market[A, B, S, T])(f: C => S, g: T => D): Market[A, B, C, D] =
      Market(c => p.look(f(c)).left.map(g), (c, b) => g(p.put(f(c), b)))
    def first[S, T, C](p: Market[A, B, S, T]): Market[A, B, (S, C), (T, C)] =
      Market({ case (s, c) => p.look(s).left.map(t => (t, c)) }, { case ((s, c), b) => (p.put(s, b), c) })
    def right[S, T, C](p: Market[A, B, S, T]): Market[A, B, Either[C, S], Either[C, T]] =
      Market(
        _.fold(c => Left(Left(c)), s => p.look(s).left.map(Right(_))),
        (e, b) => e.fold(Left(_), s => Right(p.put(s, b))))

// ---------------------------------------------------------------- the operations, on any optic the interpretation meets

/**
 * `set` and `modify` FUSE BY DEFAULT (optics-fuse-by-default).
 *
 * The receiver is inline and the body is the same planner `Fuse` uses,
 * so `o.set(b)(s)` emits the update a person would write wherever the
 * optic's shape can be read at compile time — a lens written literally
 * or named by an `inline def`, `Lens[S](_.f)`, and chains of them. It
 * ALWAYS compiles: anything unreadable (an optic behind a `val`, one
 * chosen at run time, a traversal) falls back to the interpretation,
 * which is what this extension used to do in every case.
 *
 * `Fuse.set(o)(b)(s)` remains as the form that takes the whole
 * directly, which needs no lambda at all.
 */
extension [C[_[_, _]], S, T, A, B](inline o: Optic[C, S, T, A, B])
  /** every focus through `f`, fused where the shape allows */
  inline def modify(inline f: A => B)(using fn: C[Function1]): S => T =
    ${ Fuse.modifyFnImpl('o, 'f, 'fn) }
  /** every focus replaced, fused where the shape allows */
  inline def set(inline b: B)(using fn: C[Function1]): S => T =
    ${ Fuse.setFnImpl('o, 'b, 'fn) }

  // the READ side, fused the same way: a lens chain's `get` is the
  // projection `s.a.b`, and the interpretation it replaces allocates
  // a `Forget` per level. A prism in the chain falls back, because an
  // absent focus is not a value and the interpretation knows that.
  /** the focus of a lens (or an iso) */
  inline def get(inline s: S)(using fn: C[[X, Y] =>> Forget[A, X, Y]]): A =
    ${ Fuse.getImpl('o, 's, 'fn) }
  /** the first focus, if any */
  inline def preview(inline s: S)(using fn: C[[X, Y] =>> Forget[First[A], X, Y]]): Option[A] =
    ${ Fuse.previewImpl('o, 's, 'fn) }
  /** the foci, combined */
  inline def foldMap[R](inline f: A => R)(inline s: S)(using fn: C[[X, Y] =>> Forget[R, X, Y]]): R =
    ${ Fuse.foldMapImpl('o, 'f, 's, 'fn) }
  /** the foci, in order */
  inline def toVector(inline s: S)(using fn: C[[X, Y] =>> Forget[Vector[A], X, Y]]): Vector[A] =
    ${ Fuse.toVectorImpl('o, 's, 'fn) }
  /** every focus through an effectful `f`, effects in order */
  inline def traverseOf[F[_]](inline f: A => F[B])(using fn: C[[X, Y] =>> Star[F, X, Y]]): S => F[T] =
    ${ Fuse.traverseOfImpl('o, 'f, 'fn) }

extension [C[_[_, _]], S, T, A, B](o: Optic[C, S, T, A, B])
  /** many wholes in, their focuses aggregated by `f`, one whole out */
  def aggregate(f: Vector[A] => B)(using C[Aggregating]): Vector[S] => T =
    o[Aggregating](Aggregating(f)).run
  /** the same, said with a named aggregation algebra */
  def aggregateWith[Acc](agg: Aggregator[A, Acc, B])(using C[Aggregating]): Vector[S] => T =
    aggregate(as => agg.present(as.foldLeft(agg.init)(agg.add)))

  /**
   * The optic run once at its own concrete representation
   * (`Optic.Market`, and see its comment): the affine pair, with
   * `preview`, `set` and `modify` as direct calls. Affine by nature —
   * the pair holds one focus — so a traversal cannot be compiled here
   * at all, and the missing given says so.
   *
   * MEASURED SLOWER THAN THE OPTIC (specs/optics.md, optics-fast).
   * For the pair, not for speed.
   */
  /** a LENS run once at its own pair — no Either, since the focus is
   * always there; available only where the optic is at least Strong.
   * The cheaper of the two compilations and still slower than the
   * optic: 3.9 ns against 3.0 (specs/optics.md, optics-fast). */
  def compiledLens(using C[[X, Y] =>> Shop[A, B, X, Y]]): CompiledLens[S, T, A, B] =
    val sh = o[[X, Y] =>> Shop[A, B, X, Y]](Optic.idShop[A, B])
    CompiledLens(sh.get, sh.put)

  def compiled(using C[[X, Y] =>> Market[A, B, X, Y]]): Compiled[S, T, A, B] =
    val m = o[[X, Y] =>> Market[A, B, X, Y]](Optic.idMarket[A, B])
    Compiled(m.look, m.put)

// ---------------------------------------------------------------- constructors

object Iso:
  /**
   * Kmett's `non`: an absent value READS as `d`, and writing `d` back
   * makes it absent again. This is what turns "create the missing
   * parent" from an unlawful lens into a lawful composition — absence
   * stops being a special case on the way down and becomes part of
   * the focus.
   *
   * An iso modulo one normalisation, stated rather than hidden:
   * `Some(d)` and `None` are the same point, so the round trip
   * `from(to(_))` sends `Some(d)` to `None`. Where the default MEANS
   * absence — an empty object in JSON, an empty string in a form —
   * that is the intended reading and the test pins both directions.
   */
  def non[A](d: A): Iso[Option[A], Option[A], A, A] =
    Iso(_.getOrElse(d), (a: A) => if a == d then None else Some(a))

  def apply[S, T, A, B](to: S => A, from: B => T): Iso[S, T, A, B] = new Iso[S, T, A, B]:
    def apply[P[_, _]](p: P[A, B])(using P: Profunctor[P]): P[S, T] = P.dimap(p)(to, from)

object Lens:
  def apply[S, T, A, B](get: S => A, set: (S, B) => T): Lens[S, T, A, B] = new Lens[S, T, A, B]:
    def apply[P[_, _]](p: P[A, B])(using P: Strong[P]): P[S, T] = P.lens(get, set)(p)

  /** `Lens[S](_.field)` — the selector macro, in Focus.scala */
  def apply[S <: Product]: Focus[S] = new Focus[S]

  /** `Lens.field[S]("name")` — by name, typed by the Mirror */
  def field[S <: Product]: Optic.FieldOf[S] = new Optic.FieldOf[S]

/**
 * The affine traversal — zero or one focus, and a whole that may
 * change type: `preview` says whether the focus is there, `set`
 * rebuilds. Composing a lens with a prism yields this type on its
 * own (the meet); this constructor is for the affines that are not
 * such a composition, an array index among them.
 */
object Affine:
  def apply[S, T, A, B](preview: S => Either[T, A], set: (S, B) => T): Affine[S, T, A, B] =
    new Affine[S, T, A, B]:
      def apply[P[_, _]](p: P[A, B])(using P: Strong[P] & Choice[P]): P[S, T] =
        // the textbook affine: pair the focus with the whole (Strong),
        // then choose (Choice) — through the overridable `prism`, so an
        // interpretation's direct road still applies
        P.prism[S, T, (A, S), (B, S)](
          s => preview(s).map(a => (a, s)),
          (bs: (B, S)) => set(bs._2, bs._1))(P.first[A, B, S](p))

object Prism:
  def apply[S, T, A, B](preview: S => Either[T, A], review: B => T): Prism[S, T, A, B] = new Prism[S, T, A, B]:
    def apply[P[_, _]](p: P[A, B])(using P: Choice[P]): P[S, T] = P.prism(preview, review)(p)

  /** Option's Some */
  def some[A, B]: Prism[Option[A], Option[B], A, B] = Prism(_.toRight(None), Some(_))

  /** one case of a hierarchy: the preview is the type test, the review the widening */
  def of[S, A <: S](using ct: ClassTag[A]): Prism[S, S, A, A] =
    Prism(s => ct.unapply(s).toRight(s), identity)

object Traversal:
  def apply[S, T, A, B](w: Walk[S, T, A, B]): Traversal[S, T, A, B] = new Traversal[S, T, A, B]:
    def apply[P[_, _]](p: P[A, B])(using P: Traversing[P]): P[S, T] = P.wander(w)(p)

  /** every element of a Vector — through `eachVector`, so an interpretation may take its direct road */
  def each[A, B]: Traversal[Vector[A], Vector[B], A, B] = new Traversal[Vector[A], Vector[B], A, B]:
    def apply[P[_, _]](p: P[A, B])(using P: Traversing[P]): P[Vector[A], Vector[B]] = P.eachVector(p)

  /** every element of a List */
  def eachList[A, B]: Traversal[List[A], List[B], A, B] =
    Traversal([F[_]] => (F: Applicative[F]) ?=> (f: A => F[B]) => (as: List[A]) =>
      as.foldRight(F.pure(List.empty[B]))((a, acc) => F.fmap(f(a), (b: B) => (l: List[B]) => b :: l).app(acc)))

/**
 * The kaleidoscope: aggregate the focuses of many wholes.
 *
 * `each` is the whole family in one constructor — an Applicative
 * container of focuses, aggregated position-wise by whatever the
 * applicative's `app` means. With `Optic.zipLazy` that is column-wise;
 * with a cartesian applicative it is every combination, which is a
 * different and equally honest reading of the same optic.
 */
object Kaleidoscope:
  def each[F[_], A, B](using F: Applicative[F]): Kaleidoscope[F[A], F[B], A, B] =
    new Kaleidoscope[F[A], F[B], A, B]:
      def apply[P[_, _]](p: P[A, B])(using P: Optic.Reflecting[P]): P[F[A], F[B]] =
        P.reflected(p)

/**
 * The algebraic, or classifying, lens: `view` reads one whole, and
 * `classify` decides the answer from ALL the wholes together with the
 * aggregated focus. A lens's `set` takes a value; this takes a
 * dataset, which is why the literature's example is a measurement
 * classified against everything measured before.
 */
object AlgebraicLens:
  def apply[S, T, A, B](view: S => A, classify: (Vector[S], B) => T): AlgebraicLens[S, T, A, B] =
    new AlgebraicLens[S, T, A, B]:
      def apply[P[_, _]](p: P[A, B])(using P: Optic.Classifying[P]): P[S, T] =
        P.classifying(view, classify)(p)
