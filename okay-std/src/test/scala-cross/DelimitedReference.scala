package okay.freer

/**
 * specs/delimited.md stage 2: a SECOND instance of `Delimited`, the
 * reference — what `Control[Func]` is to `Control[Cps]`. The context is
 * an immutable list of frames and delimiters, innermost first; a value
 * is plugged into it, `shift0` splits it at the delimiter (`k` WITH the
 * delimiter), `resume` prepends `k`. Nothing in it can be subtle: no
 * segments, no catenation, no fast paths — and NOT stack-safe (`plug`
 * recurses through `go`), so it runs the suites' small programs, where
 * its answers are the ones the frame machine must give.
 */
object DelimitedReference:

  /** the context's two kinds of element */
  sealed trait Elem
  final class Fr(val f: Any => Prog) extends Elem
  final class Del(val d: AnyRef, val ret: Any => Prog) extends Elem

  /** a program, given the context it runs in, to the final answer */
  trait Prog:
    def go(ctx: List[Elem]): Any

  /** the carrier: a `Prog` at phantom indexes */
  final class P[S, R, A](val prog: Prog)

  final class Tag[Y, I](val label: String)

  /**
   * THE REFERENCE'S ONE CLAIM: a value plugged into a frame or a `ret` is
   * the `A` that frame was pushed for. The context is a list of elements
   * of every type, so it is untyped by construction — the frame machine
   * is the typed version of exactly this list.
   */
  private def as[A](v: Any): A = v.asInstanceOf[A]

  object Ref extends LambdaDollar[P]:
    type Delimiter[Y, I] = Tag[Y, I]

    final class K[A, S, T, Z](val elems: List[Elem]) extends (A => P[S, T, Z]):
      def apply(a: A): P[S, T, Z] = resume(this)(pure[A, T](a))
    type SubCont[A, S, T, Z] = K[A, S, T, Z]

    def delimiter[Y, I](using at: At): Tag[Y, I] = new Tag(s"ref @ ${at.where}")

    /** a value into the context: the innermost frame or delimiter takes it */
    private def plug(v: Any, ctx: List[Elem]): Any = ctx match
      case Nil => v
      case (f: Fr) :: rest => f.f(v).go(rest)
      case (d: Del) :: rest => d.ret(v).go(rest)

    def pure[A, R](a: A): P[R, R, A] = P(ctx => plug(a, ctx))

    def bind[A, B, S, T, R](m: P[T, R, A])(f: A => P[S, T, B]): P[S, R, B] =
      P(ctx => m.prog.go(Fr(v => f(as[A](v)).prog) :: ctx))

    def dollar[Y, A, T, R](d: Tag[Y, T])(ret: A => P[T, T, Y])(body: P[T, R, A]): P[T, R, Y] =
      P(ctx => body.prog.go(Del(d, v => ret(as[A](v)).prog) :: ctx))

    def shift0[Y, I, T, R, X](d: Tag[Y, I])(f: K[X, I, T, Y] => P[I, R, Y])(using at: At): P[T, R, X] =
      P { ctx =>
        val i = ctx.indexWhere { case e: Del => e.d eq d; case _ => false }
        if i < 0 then throw NoPrompt(at.where, d.label, Nil)
        val (k, rest) = ctx.splitAt(i + 1)
        f(K(k)).prog.go(rest)
      }

    def resume[A, S, T, R, Z](k: K[A, S, T, Z])(m: P[T, R, A]): P[S, R, Z] =
      P(ctx => m.prog.go(k.elems ++ ctx))

    /** a reference program is already what it computes, in any context: its head form is itself. The machine's
     * `runHead` runs its loop and must agree with this in every context (TestDelimitedDifferential) */
    def runHead[T, R, A](m: P[T, R, A]): P[T, R, A] = m

    def runHeadAt[A, S, T, Z](k: K[A, S, T, Z])(a: A): P[S, T, Z] = k(a)

    def run[A](m: P[A, A, A]): A = as[A](m.prog.go(Nil))
