package okay2

import scala.language.experimental.macros
import scala.reflect.ClassTag
import okay2.Optic._

/**
 * THE TYPED ZIPPER: a cursor whose position is a type — the Scala 3
 * core's okay-optics `TypedZipper`. It walks a case class by FIELD, a
 * sum by CASE, a `Vector` by index, and the focus has the field's
 * declared type; `up` answers the PARENT's type, statically. Every
 * frame is an optic the library already has (McBride's derivative, per
 * field). The parent's type is an F-bounded type PARAMETER of the
 * frame, carried down and given back by `up`, never searched for.
 */
sealed trait TypedZipper[S, A, Self <: TypedZipper[S, A, Self]] {
  def focus: A
  /** the focus replaced, the cursor's own type kept */
  def set(a: A): Self
  def modify(f: A => A): Self = set(f(focus))
  /** the whole, with every edit folded in */
  def root: S
  def depth: Int

  /** a lens frame: the move is total */
  def down[B](l: Lens[A, A, B, B]): TypedZipper.Below[S, A, B, Self] =
    TypedZipper.Below[S, A, B, Self](self, a => Right(l.get(a)), (a, b) => l.set(b)(a), l.get(focus))

  /** an affine frame: the move may not exist */
  def downPartial[B](o: Affine[A, A, B, B]): Option[TypedZipper.Below[S, A, B, Self]] =
    o.preview(focus).map(b => TypedZipper.Below[S, A, B, Self](self, a => o.preview(a).toRight(a), (a, b) => o.set(b)(a), b))

  /** a prism frame: into one case of a sum, if the focus is that case
   * (transparent to `pathKey`, as to a form's key) */
  def downCase[B <: A](implicit ct: ClassTag[B]): Option[TypedZipper.Below[S, A, B, Self]] = {
    val p = Prism.of[A, B]
    p.preview(focus).map(b => TypedZipper.Below[S, A, B, Self](self, a => p.preview(a).toRight(a), (a, b) => p.set(b)(a), b, name = Some("")))
  }

  /** the path from the root as an optic on the TREE — affine, because
   * an index or a case frame may not be there on another tree */
  def asAffine: Affine[S, S, A, A]

  /** the path as the DOTTED KEY a form uses (`customer.address.city`,
   * `lines[1].qty`) — `Some` only when every frame was taken by NAME */
  def pathKey: Option[String]

  // (a frame's `asAffine` names `andThen`'s constraint: scalac 2 would
  // infer it from the expected `Affine` and pick `Strong` alone)

  /** the focus as a lens on THIS cursor's type */
  def focusLens: Lens[Self, Self, A, A] = TypedZipper.focus[S, A, Self]

  /** this cursor at its own type — the one thing an F-bound asks */
  protected def self: Self
}

object TypedZipper {

  /** the root: focus and whole are the same value */
  final case class Top[S](focus: S) extends TypedZipper[S, S, Top[S]] {
    def set(a: S): Top[S] = Top(a)
    def root: S = focus
    def depth: Int = 0
    def asAffine: Affine[S, S, S, S] = Affine[S, S, S, S](Right(_), (_, v) => v)
    def pathKey: Option[String] = Some("")
    protected def self: Top[S] = this
  }

  /** a frame below `parent`: how to put the focus back, the focus, and
   * whether it was ever set — `up` puts back only then, so a walk
   * without edits hands the parent back as it was */
  final case class Below[S, P, A, Z <: TypedZipper[S, P, Z]](parent: Z, look: P => Either[P, A], put: (P, A) => P,
                                                             focus: A, dirty: Boolean = false, name: Option[String] = None)
    extends TypedZipper[S, A, Below[S, P, A, Z]] {
    def set(a: A): Below[S, P, A, Z] = copy(focus = a, dirty = true)
    def up: Z = if (dirty) parent.set(put(parent.focus, focus)) else parent
    def root: S = up.root
    def depth: Int = parent.depth + 1
    def asAffine: Affine[S, S, A, A] = parent.asAffine.andThen[Meet[Strong, Choice]#L, A, A](Affine[P, P, A, A](look, put))
    def pathKey: Option[String] = for { p <- parent.pathKey; n <- name } yield TypedZipper.key(p, n)
    protected def self: Below[S, P, A, Z] = this
  }

  /** an ELEMENT frame: the i-th of a `Vector` focus — the one frame
   * whose siblings have the focus's own type, so `left`/`right` are
   * typed here and nowhere else. A sideways move commits the focus to
   * the parent first, so an edit survives it */
  final case class Elem[S, B, Z <: TypedZipper[S, Vector[B], Z]](parent: Z, i: Int, focus: B, dirty: Boolean = false)
    extends TypedZipper[S, B, Elem[S, B, Z]] {
    def set(b: B): Elem[S, B, Z] = copy(focus = b, dirty = true)
    def up: Z = if (dirty) parent.set(parent.focus.updated(i, focus)) else parent
    def root: S = up.root
    def depth: Int = parent.depth + 1
    def index: Int = i
    def left: Option[Elem[S, B, Z]] = sibling(i - 1)
    def right: Option[Elem[S, B, Z]] = sibling(i + 1)
    private def sibling(j: Int): Option[Elem[S, B, Z]] = {
      val p = up
      p.focus.lift(j).map(b => Elem[S, B, Z](p, j, b))
    }
    def asAffine: Affine[S, S, B, B] = parent.asAffine.andThen[Meet[Strong, Choice]#L, B, B](Affine[Vector[B], Vector[B], B, B](
      v => v.lift(i).toRight(v),
      (v, b) => if (v.isDefinedAt(i)) v.updated(i, b) else v))
    def pathKey: Option[String] = parent.pathKey.map(_ + s"[$i]")
    protected def self: Elem[S, B, Z] = this
  }

  def apply[S](s: S): Top[S] = Top(s)

  /** a key joined the way a form joins: an empty segment (a case
   * frame) leaves the prefix as it is */
  private[okay2] def key(prefix: String, name: String): String =
    if (name.isEmpty) prefix else if (prefix.isEmpty) name else s"$prefix.$name"

  /** THE TYPE-CHANGING CURSOR: a focus and the plug — put a `B` back and
   * the whole is a `T`. No frames and no `up`: once the focus changes
   * type there is no parent of the old type to return to; the way up is
   * `set`, which IS the new whole */
  final case class Poly[A, B, T](focus: A, put: B => T) {
    def set(b: B): T = put(b)
    def modify(f: A => B): T = put(f(focus))
    def down[C, D](l: Lens[A, B, C, D]): Poly[C, D, T] = Poly[C, D, T](l.get(focus), d => put(l.set(d)(focus)))
    def downPartial[C, D](o: Affine[A, B, C, D]): Option[Poly[C, D, T]] = o.preview(focus).map(c => Poly[C, D, T](c, d => put(o.set(d)(focus))))
    def downCase[C, D](p: Prism[A, B, C, D]): Option[Poly[C, D, T]] = p.preview(focus).map(c => Poly[C, D, T](c, d => put(p.set(d)(focus))))
  }

  object Poly {
    /** the root: focus `S`, and the whole becomes `T` when set */
    def of[S, T](s: S): Poly[S, T, T] = Poly[S, T, T](s, identity)
  }

  /** the focus as a lens on the CURSOR: `State.zoom(TypedZipper.focus)(p)` */
  def focus[S, A, Z <: TypedZipper[S, A, Z]]: Lens[Z, Z, A, A] = Lens[Z, Z, A, A](_.focus, (z, a) => z.set(a))

  /** a field by NAME, as a frame that remembers the name for `pathKey`;
   * a whitebox macro so the focus is the field's declared type (the
   * core reads the Mirror). `Z with TypedZipper[S, A, Z]` is how scalac
   * infers S and A from a cursor's own type */
  implicit final class FieldOps[S, A, Z <: TypedZipper[S, A, Z]](val z: Z with TypedZipper[S, A, Z]) {
    def field(name: String): Any = macro OpticMacros.zipperField
  }

  /** the i-th element of a `Vector` focus, or None past the end */
  implicit final class ElemOps[S, B, Z <: TypedZipper[S, Vector[B], Z]](private val z: Z with TypedZipper[S, Vector[B], Z]) {
    def at(i: Int): Option[Elem[S, B, Z]] = z.focus.lift(i).map(b => Elem[S, B, Z](z, i, b))
  }
}
