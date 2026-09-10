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
    def lens[S, T, A, B](get: S => A, set: (S, B) => T)(p: P[A, B]): P[S, T] =
      dimap(first[A, B, S](p))((s: S) => (get(s), s), (bs: (B, S)) => set(bs._2, bs._1))

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

import Optic.{Profunctor, Strong, Choice, Traversing, Walk, Forget, Const, First, Star, Market, Compiled, Shop, CompiledLens}

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
    Shop({ case (s, c) => p.get(s) }, { case ((s, c), b) => (p.put(s, b), c) })

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

extension [C[_[_, _]], S, T, A, B](o: Optic[C, S, T, A, B])
  /** every focus through `f` */
  def modify(f: A => B)(using C[Function1]): S => T = o[Function1](f)
  /** every focus replaced */
  def set(b: B)(using C[Function1]): S => T = o[Function1](_ => b)
  /** the focus of a lens (or an iso) */
  def get(s: S)(using C[[X, Y] =>> Forget[A, X, Y]]): A =
    o[[X, Y] =>> Forget[A, X, Y]](Forget(identity)).run(s)
  /** the first focus, if any */
  def preview(s: S)(using C[[X, Y] =>> Forget[First[A], X, Y]]): Option[A] =
    o[[X, Y] =>> Forget[First[A], X, Y]](Forget(a => First(Some(a)))).run(s).value
  /** the foci, combined */
  def foldMap[R](f: A => R)(s: S)(using C[[X, Y] =>> Forget[R, X, Y]]): R =
    o[[X, Y] =>> Forget[R, X, Y]](Forget(f)).run(s)
  /** the foci, in order */
  def toVector(s: S)(using C[[X, Y] =>> Forget[Vector[A], X, Y]]): Vector[A] =
    o[[X, Y] =>> Forget[Vector[A], X, Y]](Forget(a => Vector(a))).run(s)
  /** every focus through an effectful `f`, effects in order — in the row when `F = [x] =>> x ! Row` */
  def traverseOf[F[_]](f: A => F[B])(using C[[X, Y] =>> Star[F, X, Y]]): S => F[T] =
    o[[X, Y] =>> Star[F, X, Y]](Star(f)).run

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
