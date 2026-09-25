package okay2

import scala.annotation.tailrec
import Free.{Return, Inject}

/**
 * THE FREE SELECTIVE: a program whose EFFECTS ARE KNOWN BEFORE IT RUNS.
 *
 * `Free` is the free MONAD, and its `Bind` carries a function — so what
 * a program does after its first operation is a closure nobody can read
 * without running it. Some programs are not worth that opacity: one
 * that fetches twenty keys, or declares what a module needs, has a PURE
 * SPINE — a term whose arguments happen to be effects. `Static` is that
 * program as data: Capriotti and Kaposi's free applicative (2014) with
 * Mokhov, Lukyanov, Marlow and Dimino's `select` (2019) on top.
 *
 *  - `leaves`: every operation the program MAY perform, both branches
 *    of every `select` included, without running anything.
 *  - `toFree`: the ordinary program, which runs AT MOST ONE handler of
 *    each `select`.
 *  - `foldMap(nt)`: the program read into ANY Selective — a batch, a
 *    count, a validation — one leaf at a time.
 *
 * An operation is held as `Any` (as `Free.Inject` holds it: a row's
 * `#Op` is not a type to read at) and typed at one signature, by
 * `Split.only`, where it is handed to a handler.
 */
sealed abstract class Static[F <: Row, A] {
  import Static._

  def map[B](f: A => B): Static[F, B] = Ap(Pure[F, A => B](f), this)

  /** every operation the program may perform, in spine order, both
   * branches of every select included; stack-safe */
  def leaves: Vector[Any] = {
    val out = Vector.newBuilder[Any]
    var todo: List[Static[F, _]] = this :: Nil
    while (todo.nonEmpty) {
      val head = todo.head
      todo = todo.tail
      head match {
        case Pure(_) => ()
        case Op(fa) => out += fa
        case Ap(f, a) => todo = f :: a :: todo
        case Select(e, f) => todo = e :: f :: todo
      }
    }
    out.result()
  }

  /** the ordinary program: a select runs its handler only for a `Left`;
   * each side is DEFERRED, so a right-nested spine converts in constant
   * stack */
  def toFree: A ! F = this match {
    case Pure(a) => Return(a)
    case Op(fa) => Inject[F, A](fa)
    case ap: Ap[F, x, A] => Free.defer(() => ap.f.toFree)(g => ap.a.now.map(g))
    case s: Select[F, x, A] =>
      Free.defer(() => s.e.toFree) {
        case Left(x) => s.f.now.map(_(x))
        case Right(b) => Return[F, A](b)
      }
  }

  private def now: A ! F = this match {
    case Pure(a) => Return(a)
    case Op(fa) => Inject[F, A](fa)
    case _ => Free.delay(() => toFree)
  }

  /**
   * The program read into any Selective G, one leaf at a time through
   * `nt`. The left spine a `traverse` builds (`Ap(Ap(Pure(g), acc), x)`)
   * is walked in one tail-recursive loop over a list of pending frames
   * (an argument to fold, a function to apply, a select's two sides),
   * so nothing is folded by an ordinary call and no nesting, along any
   * axis, needs stack (specs/stack-safety.md).
   */
  def foldMap[G[_]](nt: To[F, G])(implicit G: Selective[G]): G[A] = {
    // the work still to do, innermost first (specs/stack-safety.md):
    // every pending piece is a frame here, so ONE loop walks down and up
    // and nothing is folded by an ordinary call
    sealed trait Arg
    /** an argument still to fold, then apply the function on the way up */
    final case class More(arg: Static[F, _]) extends Arg
    /** the function side is folded; the argument's value applies it */
    final case class AppTo(gf: G[Any]) extends Arg
    final case class Mapped(f: Any => Any) extends Arg
    /** a Select's condition is folded next; then its function side */
    final case class SelectE(f: Static[F, _]) extends Arg
    /** both sides folded: select. The function side is folded BEFORE
     * `select` is asked, not by-name inside it: folding only builds a G,
     * so the value is the same, and a G whose select skips that side
     * receives it folded anyway */
    final case class SelectF(ge: G[Any]) extends Arg

    sealed trait Step
    final case class Down(s: Static[F, _]) extends Step
    final case class Up(g: G[Any]) extends Step

    @tailrec def fold(step: Step, args: List[Arg]): G[Any] = step match {
      case Down(s) => s match {
        case Ap(Pure(g), a) => fold(Down(a), Mapped(erased(g)) :: args)
        case Ap(f, a) => fold(Down(f), More(a) :: args)
        case Pure(a) => fold(Up(G.pure[Any](a)), args)
        case Op(fa) => fold(Up(claim[Any](nt(Split.only[F, Any](fa)))), args)
        case sel: Select[F, x, y] => fold(Down(sel.e), SelectE(sel.f) :: args)
      }
      case Up(g) => args match {
        case Nil => g
        case More(arg) :: rest => fold(Down(arg), AppTo(g) :: rest)
        case AppTo(gf) :: rest => fold(Up(G.app(claim[Any => Any](gf), g)), rest)
        case Mapped(f) :: rest => fold(Up(G.fmap(g, f)), rest)
        case SelectE(f) :: rest => fold(Down(f), SelectF(g) :: rest)
        case SelectF(ge) :: rest => fold(Up(claim[Any](G.select(claim[Either[Any, Any]](ge), claim[Any => Any](g)))), rest)
      }
    }

    /** THE ONE CLAIM, for the walk above: it runs at `Any` because the
     * spine's types chain through existentials a loop cannot carry, and
     * every one was checked when the tree was BUILT — an `Ap` is made
     * only from an `F[A => B]` and an `F[A]`, a `Select` from matching
     * sides. Erased on the JVM; nothing is converted. */
    def claim[X](g: G[_]): G[X] = g.asInstanceOf[G[X]]
    def erased(f: Any): Any => Any = f.asInstanceOf[Any => Any]

    claim[A](fold(Down(this), Nil))
  }
}

object Static {
  final case class Pure[F <: Row, A](a: A) extends Static[F, A]
  /** one operation of F, held as `Any` */
  final case class Op[F <: Row, A](fa: Any) extends Static[F, A]
  final case class Ap[F <: Row, A, B](f: Static[F, A => B], a: Static[F, A]) extends Static[F, B]
  final case class Select[F <: Row, A, B](e: Static[F, Either[A, B]], f: Static[F, A => B]) extends Static[F, B]

  /** one operation, as a program */
  def op[F <: Row, A](fa: F#Op[A]): Static[F, A] = Op(fa)

  /** a leaf read into G: `F ==> G`, as a trait since Scala 2 has no
   * polymorphic function values */
  trait To[F <: Row, G[_]] { def apply[X](op: F#Op[X]): G[X] }

  /** the instance: the operations BUILD the tree, nothing runs */
  implicit def selective[F <: Row]: Selective[({ type L[A] = Static[F, A] })#L] =
    new Selective[({ type L[A] = Static[F, A] })#L] {
      def pure[A](a: A): Static[F, A] = Pure(a)
      override def fmap[A, B](a: Static[F, A], f: A => B): Static[F, B] = Ap(Pure[F, A => B](f), a)
      def app[A, B](f: Static[F, A => B], a: Static[F, A]): Static[F, B] = Ap(f, a)
      def select[A, B](e: Static[F, Either[A, B]], f: => Static[F, A => B]): Static[F, B] = Select(e, f)
    }
}
