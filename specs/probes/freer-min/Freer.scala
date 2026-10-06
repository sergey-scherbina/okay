//> using scala 3.9.0
//> using options -Wunused:all -feature -deprecation -Werror
package okay.min

import scala.annotation.tailrec

/** `(A => S) => R` over the signature `G`: a computation of `A` that, given a continuation into `S`, answers `R`.
 *  The freer monad, indexed for answer-type modification (Danvy–Filinski on the node, Atkey's parameterised
 *  monad as the algebra). `G` is COVARIANT: a program over a row is a program over any wider row, and `flatMap`
 *  joins the two sides' rows — the row is BUILT by `pure`/`perform`/`flatMap`, never declared. */
enum Freer[+G[_, _, +_], S, R, +A]:
  /** the value: the inner answer is the outer one (the diagonal), over the empty row */
  case Return[R, A](a: A) extends Freer[Pure, R, R, A]
  /** one operation of the signature, at its own indexes */
  case Perform[G[_, _, +_], S, R, A](op: G[S, R, A]) extends Freer[G, S, R, A]
  /** sequencing as data: the answer types meet at `T` */
  case Bind[G[_, _, +_], S, T, R, A, B](m: Freer[G, T, R, A], k: A => Freer[G, S, T, B]) extends Freer[G, S, R, B]

  /** the row of the result is the join of the rows of the two sides */
  def flatMap[H[_, _, +_], S2, B](f: A => Freer[H, S2, S, B]): Freer[G + H, S2, R, B] = Bind(this, f)
  def map[B](f: A => B): Freer[G, S, R, B] = Bind(this, a => Return(f(a)))

  /** head form — `Return`, `Perform`, or `Bind(Perform, k)` — in constant stack; associativity is the proof */
  @tailrec final def resume: Freer[G, S, R, A] = this match
    case Bind(Bind(m, f), g) => Bind(m, x => Bind(f(x), g)).resume
    case Bind(Return(a), f)  => f(a).resume
    case p                   => p

/** the empty row: no operation at all */
type Pure = [S, R, A] =>> Nothing
/** the row join */
infix type +[G[_, _, +_], H[_, _, +_]] = [S, R, A] =>> G[S, R, A] | H[S, R, A]

def pure[A, R](a: A): Freer[Pure, R, R, A] = Freer.Return(a)
def perform[G[_, _, +_], S, R, A](op: G[S, R, A]): Freer[G, S, R, A] = Freer.Perform(op)
/** a deferred program is not a node: a bind off the unit, forced by `resume`'s loop */
def delay[G[_, _, +_], S, R, A](t: () => Freer[G, S, R, A]): Freer[G, S, R, A] = Freer.Bind(Freer.Return(()), _ => t())

/** a delimiter's identity: by allocation, compared by `eq` */
final class Prompt[S, Y](val label: String)

/** The control signature, over the row `G` its bodies are written in: λ$'s two operations (Materzok–Biernacki),
 *  typed by Danvy–Filinski's answer-type modification. The machine is their interpreter; the tree knows nothing of
 *  them, and a captured `k` is an ordinary function (the machine's stack implements it). */
enum Control[+G[_, _, +_], S, R, +A]:
  /** `ret $_p body`: delimit at `p`, `ret` the frame first above it (`reset p body = Return(_) $_p body`).
   *  Typed as `Bind(body, ret)` is — the delimiter sits between the two */
  case Reset[G[_, _, +_], S, T, R, X, Y](p: Prompt[S, Y], ret: X => Freer[G, S, T, Y], body: Freer[G, T, R, X])
    extends Control[G, S, R, Y]
  /** capture up to `p`, the delimiter included: `k` is the segment as a function, `ret`'s shape; `f(k)` goes
   *  on in the delimiter's place */
  case Shift0[G[_, _, +_], S, T, R, X, Y](p: Prompt[S, Y], f: (X => Freer[G, S, T, Y]) => Freer[G, S, R, Y])
    extends Control[G, T, R, X]

/** `Control` over a row, as a row */
type Ctl[G[_, _, +_]] = [S, R, A] =>> Control[G, S, R, A]

// ---- probe: two signatures, a row built by flatMap, a run over the head form ----
object Probe:
  import Freer.*
  enum Ask[S, R, +A]:
    case Number() extends Ask[Unit, Unit, Int]
  enum Say[S, R, +A]:
    case Line(s: String) extends Say[Unit, Unit, Unit]

  val one: Freer[Ask + Say, Unit, Unit, Int] =
    for
      n <- perform(Ask.Number())
      _ <- perform(Say.Line(n.toString))
    yield n + 1

  // (Ask + Say) + Ask is accepted where Ask + Say is expected: a row is a union, so its join is pointwise
  val two: Freer[Ask + Say, Unit, Unit, Int] = one.flatMap(a => perform(Ask.Number()).map(_ + a))

  // a Return alone is a program of every row
  val three: Freer[Ask + Say, Unit, Unit, Int] = pure(3)

  type Row = Ask + Say
  /** one operation answered. `T` is the index a matched `Bind` leaves unknown; a type test on the union picks
   *  the signature, and the GADT match on its DIAGONAL operation recovers `T` (here `Unit`) and `X` — no cast */
  def step[T, X, B](op: Ask[T, Unit, X] | Say[T, Unit, X], k: X => Freer[Row, Unit, T, B], in: Int,
                    out: StringBuilder): Freer[Row, Unit, Unit, B] = op match
    case a: Ask[T, Unit, X] => a match
      case Ask.Number() => k(in)
    case s: Say[T, Unit, X] => s match
      case Say.Line(l) => out.append(l).append('\n'); k(())

  @tailrec def run[A](p: Freer[Row, Unit, Unit, A], in: Int, out: StringBuilder): A =
    p.resume match
      case Return(a)   => a
      case Perform(op) => run(step(op, (x: A) => Return(x), in, out), in, out)
      case Bind(h, k)  => (h: @unchecked) match
        case Perform(op) => run(step(op, k, in, out), in, out)

  def main(args: Array[String]): Unit =
    val out = StringBuilder()
    println(run(two, 41, out) -> out.toString.trim)
    // left-nested chain: constant stack
    val chain = (1 to 100000).foldLeft(pure[Int, Unit](0): Freer[Ask + Say, Unit, Unit, Int])((p, _) => p.map(_ + 1))
    println(run(chain, 0, out))
    val p = Prompt[Unit, Int]("p")
    val c: Freer[Ctl[Row] + Row, Unit, Unit, Int] =
      perform(Control.Reset(p, (x: Int) => pure(x), one)).flatMap(y => perform(Ask.Number()).map(_ + y))
    println(c.getClass.getSimpleName)
