package okay.scala2

import scala.reflect.ClassTag
import okay.{Applicative, get, modify, preview, set, toVector}
import okay.given

/*
 * okay-optics for Scala 2.13 (specs/scala2-facade.md, stage 15.6).
 *
 * Probed first, and nothing of okay-optics is usable from Scala 2: an
 * optic is `Optic[C[_[_, _]], S, T, A, B]`, its kinds are top-level
 * aliases (`Lens`, `Prism`, ...) Scala 2 cannot see, its constraints are
 * type lambdas over intersections, a traversal is a polymorphic function
 * type, and every operation is an `inline` extension. The seven optic
 * companions are among the objects the survey found unreadable.
 *
 * So the five kinds are Scala 2 classes here, each a shell over okay's
 * own optic: building one calls okay's constructor, composing calls
 * okay's `andThen`, and `get`/`set`/`modify`/`preview`/`toVector` are
 * okay's (fused) operations. The laws are okay's, not restated. The
 * shells are monomorphic (`Lens[S, A]`, the type does not change on a
 * set), which is what almost every use needs.
 *
 * Composition follows the lattice: an iso keeps the kind it meets; a
 * lens then a prism is an affine; anything then a traversal is a
 * traversal.
 */

/** an isomorphism: `S` and `A` are two views of the same thing */
final class Iso[S, A] private[scala2] (body: IsoBody[S, A]) {
  private[scala2] def o: okay.Iso[S, S, A, A] = body.o
  def get(s: S): A = o.get(s)
  def reverseGet(a: A): S = body.from(a)
  def modify(f: A => A): S => S = o.modify(f)
  def andThen[B](that: Iso[A, B]): Iso[S, B] = Iso.of(o.andThen(that.o), (b: B) => reverseGet(that.reverseGet(b)))
  def andThen[B](that: Lens[A, B]): Lens[S, B] = Lens.of(o.andThen(that.o))
  def andThen[B](that: Prism[A, B]): Prism[S, B] = Prism.of(o.andThen(that.o), (b: B) => reverseGet(that.review(b)))
  def andThen[B](that: Affine[A, B]): Affine[S, B] = Affine.of(o.andThen(that.o))
  def andThen[B](that: Traversal[A, B]): Traversal[S, B] = Traversal.of(o.andThen(that.o))
}

/** a part that is always there */
final class Lens[S, A] private[scala2] (body: LensBody[S, A]) {
  private[scala2] def o: okay.Lens[S, S, A, A] = body.o
  def get(s: S): A = o.get(s)
  def set(a: A): S => S = o.set(a)
  def modify(f: A => A): S => S = o.modify(f)
  def andThen[B](that: Iso[A, B]): Lens[S, B] = Lens.of(o.andThen(that.o))
  def andThen[B](that: Lens[A, B]): Lens[S, B] = Lens.of(o.andThen(that.o))
  def andThen[B](that: Prism[A, B]): Affine[S, B] = Affine.of(o.andThen(that.o))
  def andThen[B](that: Affine[A, B]): Affine[S, B] = Affine.of(o.andThen(that.o))
  def andThen[B](that: Traversal[A, B]): Traversal[S, B] = Traversal.of(o.andThen(that.o))
}

/** one case of a sum: present or not, and buildable from its part */
final class Prism[S, A] private[scala2] (body: PrismBody[S, A]) {
  private[scala2] def o: okay.Prism[S, S, A, A] = body.o
  def preview(s: S): Option[A] = o.preview(s)
  def review(a: A): S = body.review(a)
  /** replace the part if this case is the one present; otherwise unchanged */
  def set(a: A): S => S = o.set(a)
  def modify(f: A => A): S => S = o.modify(f)
  def andThen[B](that: Iso[A, B]): Prism[S, B] = Prism.of(o.andThen(that.o), (b: B) => review(that.reverseGet(b)))
  def andThen[B](that: Lens[A, B]): Affine[S, B] = Affine.of(o.andThen(that.o))
  def andThen[B](that: Prism[A, B]): Prism[S, B] = Prism.of(o.andThen(that.o), (b: B) => review(that.review(b)))
  def andThen[B](that: Affine[A, B]): Affine[S, B] = Affine.of(o.andThen(that.o))
  def andThen[B](that: Traversal[A, B]): Traversal[S, B] = Traversal.of(o.andThen(that.o))
}

/** at most one part: a lens into a prism's case, or an optional field */
final class Affine[S, A] private[scala2] (body: AffineBody[S, A]) {
  private[scala2] def o: okay.Affine[S, S, A, A] = body.o
  def preview(s: S): Option[A] = o.preview(s)
  def set(a: A): S => S = o.set(a)
  def modify(f: A => A): S => S = o.modify(f)
  def andThen[B](that: Iso[A, B]): Affine[S, B] = Affine.of(o.andThen(that.o))
  def andThen[B](that: Lens[A, B]): Affine[S, B] = Affine.of(o.andThen(that.o))
  def andThen[B](that: Prism[A, B]): Affine[S, B] = Affine.of(o.andThen(that.o))
  def andThen[B](that: Affine[A, B]): Affine[S, B] = Affine.of(o.andThen(that.o))
  def andThen[B](that: Traversal[A, B]): Traversal[S, B] = Traversal.of(o.andThen(that.o))
}

/** any number of parts, in order */
final class Traversal[S, A] private[scala2] (body: TraversalBody[S, A]) {
  private[scala2] def o: okay.Traversal[S, S, A, A] = body.o
  def toVector(s: S): Vector[A] = o.toVector(s)
  def set(a: A): S => S = o.set(a)
  def modify(f: A => A): S => S = o.modify(f)
  def andThen[B](that: Iso[A, B]): Traversal[S, B] = Traversal.of(o.andThen(that.o))
  def andThen[B](that: Lens[A, B]): Traversal[S, B] = Traversal.of(o.andThen(that.o))
  def andThen[B](that: Prism[A, B]): Traversal[S, B] = Traversal.of(o.andThen(that.o))
  def andThen[B](that: Affine[A, B]): Traversal[S, B] = Traversal.of(o.andThen(that.o))
  def andThen[B](that: Traversal[A, B]): Traversal[S, B] = Traversal.of(o.andThen(that.o))
}

// the okay optic behind each shell, out of the shells' constructors
// (whose parameter types the Scala 2 reader reads eagerly)
private[scala2] final class IsoBody[S, A](val o: okay.Iso[S, S, A, A], val from: A => S)
private[scala2] final class LensBody[S, A](val o: okay.Lens[S, S, A, A])
private[scala2] final class PrismBody[S, A](val o: okay.Prism[S, S, A, A], val review: A => S)
private[scala2] final class AffineBody[S, A](val o: okay.Affine[S, S, A, A])
private[scala2] final class TraversalBody[S, A](val o: okay.Traversal[S, S, A, A])

object Iso {
  def apply[S, A](to: S => A, from: A => S): Iso[S, A] = of(okay.Iso[S, S, A, A](to, from), from)
  private[scala2] def of[S, A](o: okay.Iso[S, S, A, A], from: A => S): Iso[S, A] = new Iso(new IsoBody(o, from))
}

object Lens {
  def apply[S, A](get: S => A, set: (S, A) => S): Lens[S, A] = of(okay.Lens[S, S, A, A](get, set))
  private[scala2] def of[S, A](o: okay.Lens[S, S, A, A]): Lens[S, A] = new Lens(new LensBody(o))
}

object Prism {
  def apply[S, A](preview: S => Option[A], review: A => S): Prism[S, A] =
    of(okay.Prism[S, S, A, A](s => preview(s).toRight(s), review), review)

  /** the case of a sealed hierarchy that is an `A`, by its class */
  def subtype[S, A <: S](implicit ct: ClassTag[A]): Prism[S, A] =
    of(okay.Prism.of[S, A], (a: A) => a)

  /** the value inside a `Some` */
  def some[A]: Prism[Option[A], A] = of(okay.Prism.some[A, A], (a: A) => Some(a))

  // `review` travels beside the optic: okay's profunctor encoding has
  // no source-free build, so a composition composes the reviews itself
  private[scala2] def of[S, A](o: okay.Prism[S, S, A, A], review: A => S): Prism[S, A] =
    new Prism(new PrismBody(o, review))
}

object Affine {
  def apply[S, A](preview: S => Option[A], set: (S, A) => S): Affine[S, A] =
    of(okay.Affine[S, S, A, A](s => preview(s).toRight(s), set))
  private[scala2] def of[S, A](o: okay.Affine[S, S, A, A]): Affine[S, A] = new Affine(new AffineBody(o))
}

object Traversal {
  /** every element of a vector */
  def each[A]: Traversal[Vector[A], A] = of(okay.Traversal.each[A, A])

  /** every element of a list */
  def eachList[A]: Traversal[List[A], A] = of(okay.Traversal.eachList[A, A])

  /** the parts `parts` finds, put back in the same order by `rebuild` */
  def apply[S, A](parts: S => Vector[A], rebuild: (S, Vector[A]) => S): Traversal[S, A] =
    of(okay.Traversal[S, S, A, A]([F[_]] => (F: Applicative[F]) ?=> (f: A => F[A]) => (s: S) =>
      F.fmap(okay.Optic.sequenceVector(parts(s).map(f)), (as: Vector[A]) => rebuild(s, as))))

  private[scala2] def of[S, A](o: okay.Traversal[S, S, A, A]): Traversal[S, A] = new Traversal(new TraversalBody(o))
}
