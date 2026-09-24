package okay2

import scala.annotation.tailrec
import okay2.Optic._

/**
 * How a tree exposes its children — Uniplate's `children`/`descend`
 * pair, the Scala 3 core's okay-optics `Plate`. ARITY-PRESERVING by
 * contract: the zipper only calls `withChildren` with a vector of the
 * length `children` answered; a structural edit is a `modify` of the
 * PARENT node, never a plate call.
 */
trait Plate[T] {
  def children(t: T): Vector[T]
  def withChildren(t: T, cs: Vector[T]): T
}

object Plate {
  /** a plate from a self-traversal: `children` is its `toVector`,
   * `withChildren` a `modify` handing each visited child the next
   * replacement — the traversal visits in `toVector`'s order, which is
   * the whole correctness argument */
  def of[T](tr: Traversal[T, T, T, T]): Plate[T] = new Plate[T] {
    def children(t: T): Vector[T] = tr.toVector(t)
    def withChildren(t: T, cs: Vector[T]): T = {
      var i = -1
      tr.modify { c => i += 1; if (cs.isDefinedAt(i)) cs(i) else c }(t)
    }
  }
}

/**
 * Huet's zipper over a `Plate`: a focus and the frames above it,
 * nearest first. A frame keeps the parent as it was, its children and
 * which one the focus is, so `up` is one `updated` and one
 * `withChildren` — and when nothing below was modified, `up` answers
 * the parent itself: a walk without edits rebuilds nothing, and `root`
 * is the input tree. Every move answers an `Option`; `modify` alone is
 * total. Two cursors are THE SAME when `focus`, `path` and `root`
 * agree; the dirty flags are an optimisation.
 */
final case class Zipper[T](focus: T, frames: List[Zipper.Frame[T]]) {
  import Zipper.Frame

  /** into the i-th child */
  def down(i: Int)(implicit P: Plate[T]): Option[Zipper[T]] = {
    val cs = P.children(focus)
    if (cs.isDefinedAt(i)) Some(Zipper(cs(i), Frame(focus, cs, i) :: frames)) else None
  }

  /** into the first child */
  def first(implicit P: Plate[T]): Option[Zipper[T]] = down(0)

  /** to the parent, rebuilt only if something below it changed */
  def up(implicit P: Plate[T]): Option[Zipper[T]] = frames match {
    case Nil => None
    case Frame(parent, cs, i, dirty) :: rest =>
      if (!dirty) Some(Zipper(parent, rest))
      else Some(Zipper(P.withChildren(parent, cs.updated(i, focus)), Zipper.soil(rest)))
  }

  /** to the previous sibling — the frame knows them; no plate needed */
  def left: Option[Zipper[T]] = sibling(-1)

  /** to the next sibling */
  def right: Option[Zipper[T]] = sibling(1)

  private def sibling(d: Int): Option[Zipper[T]] = frames match {
    case Frame(parent, cs, i, dirty) :: rest if cs.isDefinedAt(i + d) =>
      // a sideways move commits the focus into the frame's vector
      val cs2 = if (dirty) cs.updated(i, focus) else cs
      Some(Zipper(cs2(i + d), Frame(parent, cs2, i + d, dirty) :: rest))
    case _ => None
  }

  /** down along a root-first index path */
  def at(path: List[Int])(implicit P: Plate[T]): Option[Zipper[T]] =
    path.foldLeft(Option(this))((z, i) => z.flatMap(_.down(i)))

  /** the focus rewritten; the frame above is marked so `up` rebuilds */
  def modify(f: T => T): Zipper[T] = Zipper(f(focus), Zipper.soil(frames))

  /** the focus replaced */
  def set(t: T): Zipper[T] = modify(_ => t)

  /** the whole tree, with every edit folded in */
  def root(implicit P: Plate[T]): T = {
    @tailrec def go(z: Zipper[T]): T = z.up match {
      case Some(p) => go(p)
      case None => z.focus
    }
    go(this)
  }

  /** the root-first index path of the focus */
  def path: List[Int] = frames.foldLeft(List.empty[Int])((acc, f) => f.i :: acc)

  /** the focus's position among its siblings; None at the root */
  def index: Option[Int] = frames.headOption.map(_.i)

  def isTop: Boolean = frames.isEmpty
}

object Zipper {
  /** the parent as it was, its children, which one the focus is, and
   * whether anything below has changed since the parent was read */
  final case class Frame[T](parent: T, siblings: Vector[T], i: Int, dirty: Boolean = false)

  /** a cursor at the root of `t` */
  def apply[T](t: T): Zipper[T] = Zipper(t, Nil)

  /** the nearest frame marked dirty — its parent must be rebuilt */
  private def soil[T](frames: List[Frame[T]]): List[Frame[T]] = frames match {
    case f :: rest if !f.dirty => f.copy(dirty = true) :: rest
    case fs => fs
  }

  /** the focus as a lens on the CURSOR: `State.zoom(Zipper.focus)(p)`
   * runs a program at the focus with the frames riding along */
  def focus[T]: Lens[Zipper[T], Zipper[T], T, T] = Lens[Zipper[T], Zipper[T], T, T](_.focus, (z, t) => z.set(t))

  /** the path as an affine on the TREE; the plate is captured here */
  def at[T](path: List[Int])(implicit P: Plate[T]): Affine[T, T, T, T] =
    Affine[T, T, T, T](
      t => Zipper(t).at(path).map(_.focus).toRight(t),
      (t, v) => Zipper(t).at(path).map(_.set(v).root).getOrElse(t))
}
