package okay

import scala.deriving.Mirror
import scala.reflect.ClassTag

/**
 * THE TYPED ZIPPER: a cursor whose position is a type (specs/zipper.md,
 * stage 2). `Zipper[T]` walks one node type by position; this one
 * walks a case class by FIELD, a sum by CASE, a `Vector` by index, and
 * the focus has the field's declared type — `TypedZipper[Order,
 * Address, _]` says where it is, `set` takes an `Address` and nothing
 * else, and `up` answers the PARENT's type, statically.
 *
 * McBride's derivative is per field: the one-hole context of `Order`
 * at `customer` is a different type from its context at `lines`. A
 * lens into that field IS that derivative with `put` as the plug, a
 * prism into a case the derivative of a sum, an index the affine the
 * plate zipper walks by position. So every frame here is an optic the
 * library already has, and `field("name")` is `Lens.field` — the
 * Mirror consulted where it already is.
 *
 * The parent's type is a type PARAMETER of the frame, F-bounded
 * (`Below[S, P, A, Z <: TypedZipper[S, P, Z]]`): carried down and
 * given back by `up`, never searched for — the row-membership rule
 * (AGENTS.md), applied to a chain of frames. Nobody writes these
 * types; `val c = TypedZipper(order).down(customer)` infers them, and
 * `c.down(address).set(a).up.up` is a `Top[Order]` the compiler
 * checked.
 *
 * What it deliberately lacks: a loop over "the children" (fields are
 * not a sequence — that is stage 1's plate zipper), `left`/`right`
 * (same), and a type-changing `set[B]` (it would retype every frame
 * up to the root, which is `PState.zoom` over the composed lens).
 */
sealed trait TypedZipper[S, A, Self <: TypedZipper[S, A, Self]]:
  def focus: A
  /** the focus replaced, the cursor's own type kept */
  def set(a: A): Self
  def modify(f: A => A): Self = set(f(focus))
  /** the whole, with every edit folded in */
  def root: S
  def depth: Int

  /** a lens frame: the move is total */
  def down[B](l: Lens[A, A, B, B]): TypedZipper.Below[S, A, B, Self] =
    TypedZipper.Below(self, (a, b) => l.set(b)(a), l.get(focus))

  /** an affine frame: the move may not exist */
  def downPartial[B](o: Affine[A, A, B, B]): Option[TypedZipper.Below[S, A, B, Self]] =
    o.preview(focus).map(b => TypedZipper.Below(self, (a, b) => o.set(b)(a), b))

  /** a prism frame: into one case of a sum, if the focus is that case */
  def downCase[B <: A](using ClassTag[B]): Option[TypedZipper.Below[S, A, B, Self]] =
    val p = Prism.of[A, B]
    p.preview(focus).map(b => TypedZipper.Below(self, (a, b) => p.set(b)(a), b))

  /** the focus as a lens on THIS cursor's type, every parameter
   * inferred from the receiver: `State.zoom(c.focusLens)(p)` */
  def focusLens: Lens[Self, Self, A, A] = TypedZipper.focus[S, A, Self]

  /** this cursor at its own type — the one thing an F-bound asks a
   * subclass to say, and each says it once */
  protected def self: Self

object TypedZipper:

  /** the root: focus and whole are the same value */
  final case class Top[S](focus: S) extends TypedZipper[S, S, Top[S]]:
    def set(a: S): Top[S] = Top(a)
    def root: S = focus
    def depth: Int = 0
    protected def self: Top[S] = this

  /** a frame below `parent`: how to put the focus back into the
   * parent's focus, the focus itself, and whether it was ever set —
   * `up` puts back only then, so a walk without edits hands the parent
   * back as it was and `root` is the input, `eq`. `up` is the parent's
   * TYPE. Two cursors are the same when focus, path and root agree;
   * the flag is not part of that (Zipper.scala says the same). */
  final case class Below[S, P, A, Z <: TypedZipper[S, P, Z]](parent: Z, put: (P, A) => P, focus: A, dirty: Boolean = false)
    extends TypedZipper[S, A, Below[S, P, A, Z]]:
    def set(a: A): Below[S, P, A, Z] = copy(focus = a, dirty = true)
    def up: Z = if dirty then parent.set(put(parent.focus, focus)) else parent
    def root: S = up.root
    def depth: Int = parent.depth + 1
    protected def self: Below[S, P, A, Z] = this

  def apply[S](s: S): Top[S] = Top(s)

  /** the focus as a lens on the CURSOR: `State.zoom(TypedZipper.focus)(p)`
   * runs a `State % A` program at the focus with the frames riding
   * along in the outer state (Zoom.scala) */
  def focus[S, A, Z <: TypedZipper[S, A, Z]]: Lens[Z, Z, A, A] =
    Lens(_.focus, (z, a) => z.set(a))

extension [S, A <: Product, Z <: TypedZipper[S, A, Z]](z: TypedZipper[S, A, Z])
  /** a field by name, typed by the Mirror — `Lens.field[A](name)` as
   * a frame; a wrong name is a compile error, as it is there */
  inline def field[L <: String & Singleton](inline name: L)(using m: Mirror.ProductOf[A])
    : TypedZipper.Below[S, A, Tuple.Elem[m.MirroredElemTypes, Optic.IndexOf[m.MirroredElemLabels, L, 0]], Z] =
    z.down(Lens.field[A](name))

extension [S, B, Z <: TypedZipper[S, Vector[B], Z]](z: TypedZipper[S, Vector[B], Z])
  /** the i-th element of a `Vector` focus, or None past the end */
  def at(i: Int): Option[TypedZipper.Below[S, Vector[B], B, Z]] =
    z.downPartial(Affine[Vector[B], Vector[B], B, B](
      v => v.lift(i).toRight(v),
      (v, b) => if v.isDefinedAt(i) then v.updated(i, b) else v))
