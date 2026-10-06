package okay.min3
import scala.annotation.tailrec

/** the same kernel, G INVARIANT: the union bind needs one claim (members of a union erase) */
enum Freer[G[_, _, +_], S, R, +A]:
  case Return[G[_, _, +_], R, A](a: A) extends Freer[G, R, R, A]
  case Op[G[_, _, +_], T, A](op: G[T, T, A]) extends Freer[G, T, T, A]
  case Perform[G[_, _, +_], S, R, A](op: G[S, R, A]) extends Freer[G, S, R, A]
  case Bind[G[_, _, +_], S, T, R, A, B](m: Freer[G, T, R, A], k: A => Freer[G, S, T, B]) extends Freer[G, S, R, B]
  def flatMap[H[_, _, +_], S2, B](f: A => Freer[H, S2, S, B]): Freer[G + H, S2, R, B] =
    Bind(this.asInstanceOf[Freer[G + H, S, R, A]], f.asInstanceOf[A => Freer[G + H, S2, S, B]])   // THE claim
  def map[B](f: A => B): Freer[G, S, R, B] = Bind(this, a => Return(f(a)))
  @tailrec final def resume: Freer[G, S, R, A] = this match
    case Bind(Bind(m, f), g) => Bind(m, x => Bind(f(x), g)).resume
    case Bind(Return(a), f)  => f(a).resume
    case p                   => p

infix type +[G[_, _, +_], H[_, _, +_]] = [S, R, A] =>> G[S, R, A] | H[S, R, A]
type Diag[F[+_]] = [S, R, A] =>> F[A]
def effect[F[+_], T, A](op: F[A]): Freer[Diag[F], T, T, A] = Freer.Op[Diag[F], T, A](op)
def perform[G[_, _, +_], S, R, A](op: G[S, R, A]): Freer[G, S, R, A] = Freer.Perform(op)

object RestInv:
  import okay.min2.Probe.{Ask, Say}
  enum Cnt[S, R, +A]:
    case Tick[T]() extends Cnt[T, T, Unit]
  type Three = Diag[Ask] + Diag[Say] + Cnt
  val three: Freer[Three, Unit, Unit, Int] =
    for
      n <- effect(Ask.Number)
      _ <- effect(Say.Line("x"))
      _ <- perform(Cnt.Tick[Unit]())
    yield n
  def runAsk[G[_, _, +_], A](p: Freer[Diag[Ask] + G, Unit, Unit, A], in: Int): Freer[G, Unit, Unit, A] = ???
  val rest2: Freer[Diag[Say] + Cnt, Unit, Unit, Int] = runAsk(three, 1)
