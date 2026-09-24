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
   * is walked in one tail-recursive loop that collects the arguments
   * still to apply; each is applied as the walk returns, also in a loop,
   * so 50 000 leaves need no stack.
   */
  def foldMap[G[_]](nt: To[F, G])(implicit G: Selective[G]): G[A] = {
    sealed trait Arg
    final case class More(arg: Static[F, _]) extends Arg
    final case class Mapped(f: Any => Any) extends Arg

    @tailrec def spine(s: Static[F, _], args: List[Arg]): G[Any] = s match {
      case Ap(Pure(g), a) => spine(a, Mapped(erased(g)) :: args)
      case Ap(f, a) => spine(f, More(a) :: args)
      case Pure(a) => applyArgs(G.pure[Any](a), args)
      case Op(fa) => applyArgs(claim[Any](nt(Split.only[F, Any](fa))), args)
      case sel: Select[F, x, y] => applyArgs(claim[Any](G.select(sel.e.foldMap(nt), sel.f.foldMap(nt))), args)
    }

    @tailrec def applyArgs(g: G[Any], args: List[Arg]): G[Any] = args match {
      case Nil => g
      case More(arg) :: rest => applyArgs(G.app(claim[Any => Any](g), claim[Any](arg.foldMap(nt))), rest)
      case Mapped(f) :: rest => applyArgs(G.fmap(g, f), rest)
    }

    /** THE ONE CLAIM, for the walk above: it runs at `Any` because the
     * spine's types chain through existentials a loop cannot carry, and
     * every one was checked when the tree was BUILT — an `Ap` is made
     * only from an `F[A => B]` and an `F[A]`, a `Select` from matching
     * sides. Erased on the JVM; nothing is converted. */
    def claim[X](g: G[_]): G[X] = g.asInstanceOf[G[X]]
    def erased(f: Any): Any => Any = f.asInstanceOf[Any => Any]

    claim[A](spine(this, Nil))
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
