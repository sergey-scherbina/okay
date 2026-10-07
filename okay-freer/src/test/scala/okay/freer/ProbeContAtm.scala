package okay.freer

import okay.*

import scala.annotation.tailrec

/**
 * PROBE (specs/cont-atm.md): Danvy–Filinski `shift`/`reset` WITH answer-type modification on a typed CK machine
 * with a meta-continuation (Biernacka, Biernacki & Danvy, LMCS 2005). No cast anywhere: every transition is a
 * GADT match, and its equations type the next state.
 */
object ContAtm:

  /** `(A => S) => R` as data */
  enum C[A, S, R]:
    case Pure[A, R](a: A) extends C[A, R, R]
    case Bind[A, B, S, T, R](c: C[A, T, R], f: A => C[B, S, T]) extends C[B, S, R]
    /** an opaque body, given a strict `k`: a nested run of the machine */
    case Strict[A, S, R](body: (A => S) => R) extends C[A, S, R]
    /** an answer-using body after the CPS transform: a program over the lazy `k` */
    case Lazily[A, S, R](body: K[A, S] => Body[R]) extends C[A, S, R]

  /** the continuation up to the `reset`, from `A` to `S` */
  enum K[A, S]:
    case Done[A]() extends K[A, A]
    case Push[A, B, S, T](f: A => C[B, S, T], k: K[B, S]) extends K[A, T]

  /** a body over a lazy `k`, answering `R` (ContMacro's `Lazy[R]`) */
  enum Body[R]:
    case Answer(r: R)
    /** `k(a)`, then `rest` with what it answered */
    case Call[A, S, R](k: K[A, S], a: A, rest: S => Body[R]) extends Body[R]

  /** THE DELIMITER WITH ATM: the reset boundaries, each typed by its own answer, from `T` to the run's `R` */
  enum M[T, R]:
    case Top[R]() extends M[R, R]
    case Then[S, T, R](rest: S => Body[T], m: M[T, R]) extends M[S, R]

  /** the machine's four states and its end, every one typed to the run's answer `R` */
  private enum State[R]:
    case Eval[A, S, T, R](c: C[A, S, T], k: K[A, S], m: M[T, R]) extends State[R]
    case Apply[A, S, R](k: K[A, S], a: A, m: M[S, R]) extends State[R]
    case Exec[T, R](b: Body[T], m: M[T, R]) extends State[R]
    case Give[T, R](t: T, m: M[T, R]) extends State[R]
    case Finish(r: R)

  @tailrec private def loop[R](s: State[R]): R = s match
    case State.Eval(c, k, m) => loop(eval(c, k, m))
    case State.Apply(k, a, m) => loop(apply(k, a, m))
    case State.Exec(b, m) => loop(exec(b, m))
    case State.Give(t, m) => loop(give(t, m))
    case State.Finish(r) => r

  private def eval[A, S, T, R](c: C[A, S, T], k: K[A, S], m: M[T, R]): State[R] = c match
    case C.Pure(a) => State.Apply(k, a, m)
    case C.Bind(c0, f) => State.Eval(c0, K.Push(f, k), m)
    case C.Strict(body) => State.Give(body(a => runK(k, a)), m)
    case C.Lazily(body) => State.Exec(body(k), m)

  private def apply[A, S, R](k: K[A, S], a: A, m: M[S, R]): State[R] = k match
    case K.Done() => State.Give(a, m)
    case K.Push(f, k2) => State.Eval(f(a), k2, m)

  private def exec[T, R](b: Body[T], m: M[T, R]): State[R] = b match
    case Body.Answer(t) => State.Give(t, m)
    case Body.Call(k, a, rest) => State.Apply(k, a, M.Then(rest, m))

  private def give[T, R](t: T, m: M[T, R]): State[R] = m match
    case M.Top() => State.Finish(t)
    case M.Then(rest, m2) => State.Exec(rest(t), m2)

  /** a strict `k`: `k` from `a` to its `reset`, a run of its own */
  private def runK[A, S](k: K[A, S], a: A): S = loop(State.Apply(k, a, M.Top[S]()))

  // ---- the facade, Cont's spelling ----

  def pure[A, R](a: A): C[A, R, R] = C.Pure(a)
  def shift[A, S, R](body: (A => S) => R): C[A, S, R] = C.Strict(body)
  def shiftLazy[A, S, R](body: K[A, S] => Body[R]): C[A, S, R] = C.Lazily(body)
  def done[R](r: R): Body[R] = Body.Answer(r)
  def call[A, S, R](k: K[A, S], a: A)(rest: S => Body[R]): Body[R] = Body.Call(k, a, rest)

  extension [A, S, R](c: C[A, S, R])
    def flatMap[B, S2](f: A => C[B, S2, S]): C[B, S2, R] = C.Bind(c, f)
    def map[B](f: A => B): C[B, S, R] = C.Bind(c, (a: A) => C.Pure[B, S](f(a)))

  /** apply to a continuation: `k` the bottom of `K`, the run's answer `R` */
  def run[A, S, R](c: C[A, S, R])(k: A => S): R =
    loop(State.Eval(c, K.Push((a: A) => C.Pure[S, S](k(a)), K.Done[S]()), M.Top[R]()))

  def reset[A, R](c: C[A, A, R]): R = run(c)(identity)
