//> using scala 3.9.0
//> using options -Wunused:all -feature -deprecation -Werror
package okay.min2

import scala.annotation.tailrec

/** A': three-place G kept for index-moving signatures; a UNARY operation enters through its own node, on the
 *  diagonal, so the equation S = R sits on the node and a matched Bind recovers its middle index by GADT */
enum Freer[+G[_, _, +_], S, R, +A]:
  case Return[R, A](a: A) extends Freer[Pure, R, R, A]
  case Perform[G[_, _, +_], S, R, A](op: G[S, R, A]) extends Freer[G, S, R, A]
  /** a DIAGONAL operation: `Perform` with the equation S = R on the node, so a matched `Bind` recovers its
   *  middle index from the node, not from the operation. A unary signature enters here, as `Diag[F]` */
  case Op[G[_, _, +_], T, A](op: G[T, T, A]) extends Freer[G, T, T, A]
  case Bind[G[_, _, +_], S, T, R, A, B](m: Freer[G, T, R, A], k: A => Freer[G, S, T, B]) extends Freer[G, S, R, B]

  def flatMap[H[_, _, +_], S2, B](f: A => Freer[H, S2, S, B]): Freer[G + H, S2, R, B] = Bind(this, f)
  def map[B](f: A => B): Freer[G, S, R, B] = Bind(this, a => Return(f(a)))

  @tailrec final def resume: Freer[G, S, R, A] = this match
    case Bind(Bind(m, f), g) => Bind(m, x => Bind(f(x), g)).resume
    case Bind(Return(a), f)  => f(a).resume
    case p                   => p

type Pure = [S, R, A] =>> Nothing
infix type +[G[_, _, +_], H[_, _, +_]] = [S, R, A] =>> G[S, R, A] | H[S, R, A]
/** a unary signature as a row: the indexes are not its business */
type Diag[F[+_]] = [S, R, A] =>> F[A]

def pure[A, R](a: A): Freer[Pure, R, R, A] = Freer.Return(a)
def perform[G[_, _, +_], S, R, A](op: G[S, R, A]): Freer[G, S, R, A] = Freer.Perform(op)
def effect[F[+_], T, A](op: F[A]): Freer[Diag[F], T, T, A] = Freer.Op[Diag[F], T, A](op)

object Probe:
  import Freer.*
  // plain unary effects, as today
  enum Ask[+A]:
    case Number extends Ask[Int]
  enum Say[+A]:
    case Line(s: String) extends Say[Unit]
  // an index-moving signature beside them: type-changing state (Atkey's example), three-place by need
  enum PState[S, R, +A]:
    case Get[S]() extends PState[S, S, S]
    case Put[S, T](t: T) extends PState[T, S, Unit]   // reads R => (S, A): before T... written as the ATM reads it

  type Row = Diag[Ask] + Diag[Say]

  val one: Freer[Row, Unit, Unit, Int] =
    for
      n <- effect(Ask.Number)
      _ <- effect(Say.Line(n.toString))
    yield n + 1

  // the unary op dispatched: `F$ <: Ask | Say` pointwise from the node's GADT, T = R from the node
  def step[X, B](op: Ask[X] | Say[X], k: X => Freer[Row, Unit, Unit, B], in: Int, out: StringBuilder)
    : Freer[Row, Unit, Unit, B] = op match
    case Ask.Number => k(in)
    case Say.Line(l) => out.append(l).append('\n'); k(())

  @tailrec def run[A](p: Freer[Row, Unit, Unit, A], in: Int, out: StringBuilder): A =
    p.resume match
      case Return(a)  => a
      case Op(op)     => run(step(op, (x: A) => Return(x), in, out), in, out)
      case Perform(op) => run(step(op, (x: A) => Return(x), in, out), in, out)   // a Row op built non-diagonally
      case Bind(h, k) => (h: @unchecked) match
        case Op(op) => run(step(op, k, in, out), in, out)

  // a mixed row: unary effects and an index-moving signature in one program, the index moving Int -> String
  val mixed: Freer[Row + PState, String, Int, Unit] =
    for
      n <- effect(Ask.Number)
      s <- perform(PState.Get[Int]())
      _ <- perform(PState.Put[Int, String]((s + n).toString))
      _ <- effect(Say.Line("moved"))
    yield ()

  def main(args: Array[String]): Unit =
    val out = StringBuilder()
    println(run(one, 41, out) -> out.toString.trim)
    val chain = (1 to 100000).foldLeft(pure[Int, Unit](0): Freer[Row, Unit, Unit, Int])((p, _) => p.map(_ + 1))
    println(run(chain, 0, out))
    println(mixed.getClass.getSimpleName)
