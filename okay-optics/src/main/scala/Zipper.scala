package okay

import scala.annotation.tailrec

/**
 * How a tree exposes its children — Uniplate's `children`/`descend`
 * pair (specs/zipper.md). ARITY-PRESERVING by contract: the zipper
 * only ever calls `withChildren` with a vector of the length
 * `children` answered; what a plate does with another length is its
 * own business (`Json`'s keeps the node, since a key cannot be
 * invented), and a structural edit — delete, insert — is a `modify`
 * of the PARENT node, never a plate call.
 *
 * Two methods rather than a `(Vector[T], Vector[T] => T)` pair: the
 * pair allocates a put-back closure per descent, which is what
 * `Ui.childAt` — the hand-written frame this replaces for the
 * editor's purpose — was paying.
 */
trait Plate[T]:
  def children(t: T): Vector[T]
  def withChildren(t: T, cs: Vector[T]): T

object Plate:
  /**
   * A plate from a self-traversal, for anyone holding one: `children`
   * is the traversal's `toVector`, `withChildren` a `modify` that
   * hands each visited child the next replacement. The counter is the
   * one `var` inside a pure function here, and it is local to the
   * call — the traversal visits in order, and `toVector` is that same
   * order, which is the whole correctness argument.
   */
  def of[T](tr: Traversal[T, T, T, T]): Plate[T] = new Plate[T]:
    def children(t: T): Vector[T] = tr.toVector(t)
    def withChildren(t: T, cs: Vector[T]): T =
      var i = -1
      tr.modify { c => i += 1; if cs.isDefinedAt(i) then cs(i) else c }(t)

/**
 * Huet's zipper over a `Plate`: a focus and the frames above it,
 * nearest first. A frame keeps the PARENT NODE as it was, its
 * children and which one the focus is, so `up` is one
 * `updated(i, focus)` and one `withChildren` — and when nothing under
 * the frame was modified, `up` answers the parent itself: a walk
 * without edits rebuilds nothing, and `root` is the input tree.
 *
 * Why not the textbook `(left.reverse, right)` pair: `left`/`right`
 * cost the same on both, `up` is a `reverse ++ ::` walk on the pair
 * and follows every edit, and the plate already answers a `Vector`.
 *
 * Every move answers an `Option` — an affine, `Ui.path`'s refusal at
 * a leaf — so inside a program a move is a pattern-bind refusal, not
 * an exception. `modify` alone is total.
 *
 * Two cursors are THE SAME when they point at the same place in the
 * same tree: `focus`, `path` and `root`. The `dirty` flags are an
 * optimisation and not part of that; the laws in `TestZipper` compare
 * cursors that way, and `==` on the case class is stricter than the
 * concept.
 */
final case class Zipper[T](focus: T, frames: List[Zipper.Frame[T]]):
  import Zipper.Frame

  /** into the i-th child */
  def down(i: Int)(using P: Plate[T]): Option[Zipper[T]] =
    val cs = P.children(focus)
    if cs.isDefinedAt(i) then Some(Zipper(cs(i), Frame(focus, cs, i) :: frames)) else None

  /** into the first child */
  def first(using Plate[T]): Option[Zipper[T]] = down(0)

  /** to the parent, rebuilt only if something below it changed */
  def up(using P: Plate[T]): Option[Zipper[T]] = frames match
    case Nil => None
    case Frame(parent, cs, i, dirty) :: rest =>
      if !dirty then Some(Zipper(parent, rest))
      else Some(Zipper(P.withChildren(parent, cs.updated(i, focus)), Zipper.soil(rest)))

  /** to the previous sibling — the frame knows them; no plate needed */
  def left: Option[Zipper[T]] = sibling(-1)

  /** to the next sibling */
  def right: Option[Zipper[T]] = sibling(1)

  private def sibling(d: Int): Option[Zipper[T]] = frames match
    case Frame(parent, cs, i, dirty) :: rest if cs.isDefinedAt(i + d) =>
      // a sideways move commits the focus into the frame's vector, so
      // the frame stays truthful; the flag rides along
      val cs2 = if dirty then cs.updated(i, focus) else cs
      Some(Zipper(cs2(i + d), Frame(parent, cs2, i + d, dirty) :: rest))
    case _ => None

  /** down along a root-first index path */
  def at(path: List[Int])(using Plate[T]): Option[Zipper[T]] =
    path.foldLeft(Option(this))((z, i) => z.flatMap(_.down(i)))

  /** the focus rewritten; the frame above is marked so `up` rebuilds */
  def modify(f: T => T): Zipper[T] =
    Zipper(f(focus), Zipper.soil(frames))

  /** the focus replaced */
  def set(t: T): Zipper[T] = modify(_ => t)

  /** the whole tree, with every edit folded in */
  def root(using Plate[T]): T =
    @tailrec def go(z: Zipper[T]): T = z.up match
      case Some(p) => go(p)
      case None => z.focus
    go(this)

  /** the root-first index path of the focus */
  def path: List[Int] = frames.foldLeft(List.empty[Int])((acc, f) => f.i :: acc)

  /** the focus's position among its siblings; None at the root */
  def index: Option[Int] = frames.headOption.map(_.i)

  def isTop: Boolean = frames.isEmpty

object Zipper:
  /** the parent as it was, its children, which one the focus is, and
   * whether anything below has changed since the parent was read */
  final case class Frame[T](parent: T, siblings: Vector[T], i: Int, dirty: Boolean = false)

  /** a cursor at the root of `t` */
  def apply[T](t: T): Zipper[T] = Zipper(t, Nil)

  /** the nearest frame marked dirty — its parent must be rebuilt */
  private def soil[T](frames: List[Frame[T]]): List[Frame[T]] = frames match
    case f :: rest if !f.dirty => f.copy(dirty = true) :: rest
    case fs => fs

  /** the focus as a lens on the CURSOR: `State.zoom(Zipper.focus)(p)`
   * runs a `State % T` program at the focus with the frames riding
   * along in the outer state (Zoom.scala) */
  def focus[T]: Lens[Zipper[T], Zipper[T], T, T] =
    Lens(_.focus, (z, t) => z.set(t))

  /** the path as an affine on the TREE — `Ui.path` by hand; the plate
   * is captured here, so the optic needs none where it is used */
  def at[T](path: List[Int])(using Plate[T]): Affine[T, T, T, T] =
    Affine(
      t => Zipper(t).at(path).map(_.focus).toRight(t),
      (t, v) => Zipper(t).at(path).map(_.set(v).root).getOrElse(t))
