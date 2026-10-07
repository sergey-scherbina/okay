package okay.freer


import scala.annotation.tailrec

/**
 * PROBE, stage 2a of specs/cont-atm.md: a MULTI-PROMPT λ$ machine whose delimiters are typed per INSTALLATION.
 * A delimiter is two things: its `ret`, an ordinary frame at the bottom of its level's `K`, and its boundary,
 * a `Level` of the meta-continuation `M`. A capture to the NEAREST delimiter changes the answer type (D-F's
 * ATM) and is typed by the state alone; a capture to a NAMED prompt through other levels is diagonal there
 * and rests on one claim, the generative-prompt axiom (`same`). Everything else is a GADT match.
 */
object DelimAtm:

  /** a prompt's name, with the answer `Y` every installation of it has (a capture by name reads it) */
  final class Prompt[Y](val name: String)

  /** `(A => S) => R` as data, `(S, R)` the answer pair of the NEAREST delimiter */
  enum C[A, S, R]:
    case Pure[A, R](a: A) extends C[A, R, R]
    case Bind[A, B, S, T, R](c: C[A, T, R], f: A => C[B, S, T]) extends C[B, S, R]
    /** `ret $ body` at `p`: inside, a level of its own answering `R`; outside, that answer as a value */
    case Dollar[A, B, T, R, X](p: Prompt[R], ret: A => C[B, B, T], body: C[A, T, R]) extends C[R, X, X]
    /** a capture to the nearest delimiter, WITH answer-type modification: `k` from `A` to `S`, the body
     * answering `R` in the delimiter's place — at whatever the level outside answers, so polymorphic in it */
    case Near[A, S, R](body: [X] => K[A, S] => C[R, X, X]) extends C[A, S, R]
    /** `k(a)` as a computation: `k` under a boundary of its own, its answer the value */
    case Resume[A, S, X](k: K[A, S], a: A) extends C[S, X, X]
    /** a capture to the prompt `p`, through any levels between: diagonal here */
    case To[A, Y, X](p: Prompt[Y], body: Away[A, Y]) extends C[A, X, X]
    /** a captured multi-level `k` resumed: its levels pushed back, its answer the value */
    case ResumeCap[A, Z, X](k: Cap[A, Z], a: A) extends C[Z, X, X]

  /** a named capture's body: in the target level's place, at whatever answers outside it */
  type Away[A, Y] = [W] => Cap[A, Y] => C[Y, W, W]

  /** a level's continuation, from `A` to the level's answer `S` */
  enum K[A, S]:
    case Done[A]() extends K[A, A]
    case Push[A, B, S, T](f: A => C[B, S, T], k: K[B, S]) extends K[A, T]

  /** THE DELIMITERS, typed per installation: from the innermost level's answer `T` to the run's `R` */
  enum M[T, R]:
    case Top[R]() extends M[R, R]
    /** a boundary: the level inside answers `T` — its prompt says so, or it has none (a resumed `k`'s) —
     * and outside it `out` takes that answer on to the levels below */
    case Level[T, U, R](p: Prompt[T] | Null, out: K[T, U], m: M[U, R]) extends M[T, R]

  /** the levels a named capture crossed, innermost first, the last one outermost: `k`'s middle */
  enum Rev[A0, A]:
    case Nil[A0]() extends Rev[A0, A0]
    case Snoc[A0, A, T](prev: Rev[A0, A], k: K[A, T], q: Prompt[T] | Null) extends Rev[A0, T]

  /** a `k` captured to a named prompt: the levels crossed, the target level's continuation, its prompt —
   * the value where the crossed levels meet the target's is the capture's own (`A`), named by a member */
  sealed abstract class Cap[A0, Y]:
    type A
    def rev: Rev[A0, A]
    def k: K[A, Y]
    def p: Prompt[Y]

  private def cap[A0, A1, Y](r: Rev[A0, A1], k1: K[A1, Y], p1: Prompt[Y]): Cap[A0, Y] = new Cap[A0, Y]:
    type A = A1
    def rev = r
    def k = k1
    def p = p1

  private enum State[R]:
    case Eval[A, S, T, R](c: C[A, S, T], k: K[A, S], m: M[T, R]) extends State[R]
    case Apply[A, S, R](k: K[A, S], a: A, m: M[S, R]) extends State[R]
    case Give[T, R](t: T, m: M[T, R]) extends State[R]
    case Finish(r: R)

  @tailrec private def loop[R](s: State[R]): R = s match
    case State.Eval(c, k, m) => loop(eval(c, k, m))
    case State.Apply(k, a, m) => loop(apply(k, a, m))
    case State.Give(t, m) => loop(give(t, m))
    case State.Finish(r) => r

  private def eval[A, S, T, R](c: C[A, S, T], k: K[A, S], m: M[T, R]): State[R] = c match
    case C.Pure(a) => State.Apply(k, a, m)
    case C.Bind(c0, f) => State.Eval(c0, K.Push(f, k), m)
    case C.Dollar(p, ret, body) => State.Eval(body, K.Push(ret, K.Done()), M.Level(p, k, m))
    case C.Near(body) => m match
      case M.Level(_, out, m2) => State.Eval(body(k), out, m2)
      // no delimiter: the run's own top is the nearest
      case M.Top() => State.Eval(body(k), K.Done(), M.Top())
    case C.Resume(k1, a) => State.Apply(k1, a, M.Level(null, k, m))
    case C.To(p, body) => cut(p, body, Rev.Nil(), k, m)
    case C.ResumeCap(c, a) => reinstall(c.rev, c.k, M.Level(c.p, k, m), a)

  private def apply[A, S, R](k: K[A, S], a: A, m: M[S, R]): State[R] = k match
    case K.Done() => State.Give(a, m)
    case K.Push(f, k2) => State.Eval(f(a), k2, m)

  private def give[T, R](t: T, m: M[T, R]): State[R] = m match
    case M.Top() => State.Finish(t)
    case M.Level(_, out, m2) => State.Apply(out, t, m2)

  /** walk out to `p`'s level, the levels crossed into `k`; the body runs in that level's place */
  @tailrec private def cut[A0, A, T, R, Y](p: Prompt[Y], body: Away[A0, Y],
                                            rev: Rev[A0, A], k: K[A, T], m: M[T, R]): State[R] = m match
    case M.Top() => throw AtmNoPrompt(p.name)
    case l: M.Level[T, u, R] =>
      val q = l.p
      if q != null && (q eq p) then
        val is = same(q, p)
        State.Eval(body[u](cap(rev, is.substituteCo[[t] =>> K[A, t]](k), p)), is.substituteCo[[t] =>> K[t, u]](l.out), l.m)
      else cut(p, body, Rev.Snoc(rev, k, q), l.out, l.m)

  /** a captured `k`'s levels pushed back over `m`, outermost first, then `a` into the innermost */
  @tailrec private def reinstall[A0, A, T, R](rev: Rev[A0, A], out: K[A, T], m: M[T, R], a: A0): State[R] = rev match
    case Rev.Nil() => State.Apply(out, a, m)
    case Rev.Snoc(prev, k, q) => reinstall(prev, k, M.Level(q, out, m), a)

  /**
   * THE ONE CLAIM, the generative-prompt axiom (Dybvig, Peyton Jones & Sabry's `eqPrompt`, an `unsafeCoerce`
   * there too): one prompt object, one answer type. True by construction: a `Level` holding a prompt is built by
   * a `Dollar` at that prompt's own type, or put back from a captured `k` at the type it was captured at.
   */
  private def same[T, Y](@annotation.unused a: Prompt[T], @annotation.unused b: Prompt[Y]): T =:= Y =
    summon[T =:= T].asInstanceOf[T =:= Y]

  final class AtmNoPrompt(name: String) extends RuntimeException(s"no delimiter for prompt $name")

  /** a run, from the outside */
  def run[A](c: C[A, A, A]): A = loop(State.Eval(c, K.Done(), M.Top()))

  /** a strict `k`: run to its own boundary now */
  def runK[A, S](k: K[A, S], a: A): S = loop(State.Apply(k, a, M.Top()))
  def runCap[A, Z](c: Cap[A, Z], a: A): Z = loop(reinstall(c.rev, c.k, M.Top(), a))

  // ---- the operations, spelled ----

  def pure[A, R](a: A): C[A, R, R] = C.Pure(a)
  def dollar[A, B, T, R, X](p: Prompt[R])(ret: A => C[B, B, T])(body: C[A, T, R]): C[R, X, X] = C.Dollar(p, ret, body)
  def reset[A, R, X](p: Prompt[R])(body: C[A, A, R]): C[R, X, X] = C.Dollar(p, (a: A) => C.Pure[A, A](a), body)

  extension [A, S, R](c: C[A, S, R])
    def flatMap[B, S2](f: A => C[B, S2, S]): C[B, S2, R] = C.Bind(c, f)
    def map[B](f: A => B): C[B, S, R] = C.Bind(c, (a: A) => C.Pure[B, S](f(a)))

  /** Cps over it: D-F's shift is a capture to the nearest, its body under a level of its own */
  object Cps:
    def shift[A, S, R](body: (A => S) => R): C[A, S, R] =
      C.Near([X] => (k: K[A, S]) => C.Pure[R, X](body(a => runK(k, a))))
    def shiftLazy[A, S, R](body: [X] => K[A, S] => C[R, X, X]): C[A, S, R] = C.Near(body)
    def call[A, S, X](k: K[A, S], a: A): C[S, X, X] = C.Resume(k, a)
    /** `c / k`: a run whose own delimiter's `ret` is the user's `k` */
    def run[A, S, R](c: C[A, S, R])(k: A => S): R =
      DelimAtm.run(C.Dollar(new Prompt[R]("Cps.run"), (a: A) => C.Pure[S, S](k(a)), c))
