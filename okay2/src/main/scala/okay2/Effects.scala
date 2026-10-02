package okay2

import scala.annotation.unused

import scala.annotation.tailrec
import Free.{Return, Inject, Bind, Delay}

/**
 * THE TAGLESS INTERFACE: what a carrier of effectful programs offers —
 * `pure`, `perform`, `flatMap`, a deferred bind, and `foldCont`, the
 * reflection of a program into `Cont` that gives every operation its
 * meaning. Code written over `M: Effects` runs on any encoding: `Free`
 * (the initial one, a tree to step and inspect) or `Eager` (a pure
 * computation IS its value). The Scala 3 core's `trait Effects`, whose
 * companion is the toolkit object below, as there.
 */
trait Effects[M[_, _]] {
  def pure[F <: Row, A](a: A): M[F, A]
  def perform[F <: Row, A](e: F#Op[A]): M[F, A]
  /** a bind whose left side is deferred: forced only where the
   * encoding's own interpreter reaches it, so mutually-recursive
   * functions can call each other in tail position */
  def defer[F <: Row, A, B](thunk: () => M[F, A])(f: A => M[F, B]): M[F, B]
  /** mark a call as a tail call — the tagless `!.tailcall` */
  def tailcall[F <: Row, A](thunk: => M[F, A]): M[F, A] = defer[F, A, A](() => thunk)(a => pure[F, A](a))
  def flatMap[F <: Row, A, B](m: M[F, A])(f: A => M[F, B]): M[F, B]
  def map[F <: Row, A, B](m: M[F, A])(f: A => B): M[F, B] = flatMap(m)((a: A) => pure[F, B](f(a)))
  /** interpret the operations: reflect the computation into Cont */
  def foldCont[F <: Row, A, S](m: M[F, A])(h: F !> S): A /> S
  /** run every effect by a comonadic Answers; encodings may override
   * with an equivalent fast path */
  def runWith[F <: Row, A](m: M[F, A])(implicit H: Answers[F]): A = foldCont[F, A, A](m)(Interpr.of[F, A]) / identity

  /**
   * The program folded into any `Monad` G, each operation translated by `nt`. IT IS G's `tailRecM`, as cats'
   * `Free.foldMap` is: each step resumes the tree once and answers `Left(the rest)` or `Right(the value)`, so
   * the fold is exactly as stack-safe as `TailRecM[G]` — the carrier's own loop (specs/eager-carrier-depth.md).
   */
  def foldMap[F <: Row, A, G[_]](m: M[F, A])(nt: Static.To[F, G])(implicit G: Monad[G], R: TailRecM[G]): G[A] =
    Effects.foldMapFree[F, A, G](Effects.reify[M, F, A](m)(this))(nt)(G, R)

  /** handle the effect F by h (and the values by ret), forwarding the
   * effects G; for mass tail-resumption prefer `!.relay` (measured). The
   * definition goes through the tree; `Effects[Free]` is `!.handleWith`,
   * the loop that enters `Cont` only for an operation it claims. One
   * parameter list where Scala 3 has two type clauses: name all four */
  def handle[F <: Row, G <: Row, A, B](m: M[F with G, A])(ret: A => M[G, B])(h: F !> M[G, B])
                                     (implicit T: TypeableK[F], d: Distinct[F with G]): M[G, B] = {
    val E = this
    // each claimed operation answered in M, read back as the tree's: the definition, at one capture apiece
    val inTree: F !> (B ! G) = new Interpr[F, B ! G] {
      def apply[X](e: F#Op[X]): Cont[X, B ! G, B ! G] =
        Cont.shift[X, B ! G, B ! G](k => Effects.reify[M, G, B](h[X](e) / (x => Effects.reflect[M, G, B](k(x))(E)))(E))
    }
    Effects.reflect[M, G, B](Effects.handleWith[A, B, F, G](Effects.reify[M, F with G, A](m)(E))(a => Effects.reify[M, G, B](ret(a))(E))(inTree)(T, d))(E)
  }

  // LEVEL 1 (specs/shift-effect.md): continuations and the ready handlers, in any encoding. The default goes
  // through the tree (`reify`, the top-level function, `reflect`); `Effects[Free]` is the top-level functions
  // themselves, so there is one definition of each. The Scala 3 core's trait, member for member.

  /** Danvy-Filinski's capture (the top-level `shift`) */
  def shift[R, A, F <: Row](f: (A => M[Shift[R] + F, R]) => M[Shift[R] + F, R])(implicit k: Shift.Key[R], at: At): M[Shift[R] + F, A] = {
    val E = this
    Effects.reflect[M, Shift[R] + F, A](okay2.shift[R, A, F](kk =>
      Effects.reify[M, Shift[R] + F, R](f(a => Effects.reflect[M, Shift[R] + F, R](kk(a))(E)))(E))(k, at))(E)
  }

  /** the capture whose body runs outside its `reset` (the top-level `shift0`) */
  def shift0[R, A, F <: Row](f: (A => M[F, R]) => M[F, R])(implicit k: Shift.Key[R], at: At): M[Shift[R] + F, A] = {
    val E = this
    Effects.reflect[M, Shift[R] + F, A](okay2.shift0[R, A, F](kk =>
      Effects.reify[M, F, R](f(a => Effects.reflect[M, F, R](kk(a))(E)))(E))(k, at))(E)
  }

  /** delimit (the top-level `reset`) */
  def reset[R, F <: Row](body: M[Shift[R] + F, R])(implicit k: Shift.Key[R], n: Shift.Machine[F]): M[F, R] =
    Effects.reflect[M, F, R](okay2.reset[R, F](Effects.reify[M, Shift[R] + F, R](body)(this))(k, n))(this)

  /** take a ready handler's effect off the row (the program's `p.handle(h)`; two arguments in one list, so the
   * level-2 `handle(m)(ret)(h)` above stays its own overload). The row is written `E with F`, so a call names
   * the rest where scalac cannot take E off by inference */
  def handle[A, E <: Row, I, O[_], N[_ <: Row], F <: Row](m: M[E with F, A], h: Handler.Full[E, I, O, N])
                                                        (implicit ok: A <:< I, d: Distinct[E with F], n: N[F]): M[F, O[A]] =
    Effects.reflect[M, F, O[A]](h.run[A, F](Effects.reify[M, E with F, A](m)(this))(ok, d, n))(this)

  /** a program with no effect left, to its value (the program's `run`) */
  def run[A](m: M[Pure, A]): A = Effects.run(Effects.reify[M, Pure, A](m)(this))
}

/**
 * Any Effects program in ANY other Effects encoding, and the two ends of it: the Scala 3 core's top-level
 * `convert`, `reify`, `reflect`, here members of `object Effects` (`!.reify`, `Effects.reflect`) — at the
 * package level Scala 2 would let them shadow monadic reflection's `Layered.reify` in a file importing it.
 */
trait Conversions {
  /**
   * The initiality of the interface made a function: an encoding is fixed by `pure` and `perform`, `foldCont`
   * is the fold, and so there is exactly one structure-preserving way across. The handler rebuilds each
   * operation in the target, `N.perform(e)`, and the values land through `N.pure`.
   */
  def convert[M[_, _], N[_, _], F <: Row, A](m: M[F, A])(implicit M: Effects[M], N: Effects[N]): N[F, A] =
    M.foldCont[F, A, N[F, A]](m)(new Interpr[F, N[F, A]] {
      def apply[X](e: F#Op[X]): Cont[X, N[F, A], N[F, A]] = Cont.shift[X, N[F, A], N[F, A]](k => N.flatMap(N.perform[F, X](e))(k))
    }) / (a => N.pure[F, A](a))

  /** any Effects program materialized as a Free tree: building the syntax is itself an interpretation */
  def reify[M[_, _], F <: Row, A](m: M[F, A])(implicit M: Effects[M]): A ! F =
    convert[M, Free, F, A](m)(M, Effects.free)

  /**
   * The other direction: a Free tree read INTO any encoding. `reify` observes an encoding as syntax, which is
   * what a debugger or a rewriter wants; `reflect` spends syntax at an encoding, which is what running it fast
   * wants. Together a round trip (TestReflect). A tree is already syntax, so it folds straight into the target
   * with no continuation reified on the way.
   */
  def reflect[M[_, _], F <: Row, A](m: Free[F, A])(implicit M: Effects[M]): M[F, A] =
    m.fold[F, A, M[F, A]](a => M.pure[F, A](a))(new Free.Step[F, A, M[F, A]] {
      def apply[X](e: Any, k: X => Free[F, A]): M[F, A] = M.flatMap[F, X, A](M.perform[F, X](Split.only[F, X](e)))(x => reflect[M, F, A](k(x)))
    })
}

/**
 * The toolkit over Free: running, stepping, and the three
 * interpreters — the tail-resumptive `relay`, the row-rewriting
 * `translate`, and the Cont-valued `handle` (abort, multi-shot).
 * Aliased as `!` in the package object, as in the Scala 3 core. Also
 * the companion of `trait Effects`, holding its `Free` instance.
 */
object Effects extends Conversions {

  def apply[M[_, _]](implicit E: Effects[M]): Effects[M] = E

  /** the freer monad is the initial encoding: `Inject` a suspended
   * shift, given its meaning by `foldCont` */
  implicit val free: Effects[Free] = new Effects[Free] {
    def pure[F <: Row, A](a: A): Free[F, A] = Return(a)
    def perform[F <: Row, A](e: F#Op[A]): Free[F, A] = Inject[F, A](e)
    def defer[F <: Row, A, B](thunk: () => Free[F, A])(f: A => Free[F, B]): Free[F, B] = Free.defer(thunk)(f)
    def flatMap[F <: Row, A, B](m: Free[F, A])(f: A => Free[F, B]): Free[F, B] = m.flatMap(f)
    override def map[F <: Row, A, B](m: Free[F, A])(f: A => B): Free[F, B] = m.map(f)
    def foldCont[F <: Row, A, S](m: Free[F, A])(h: F !> S): A /> S = foldContFree(m)(h)
    override def runWith[F <: Row, A](m: Free[F, A])(implicit H: Answers[F]): A = runFree(m)
    override def handle[F <: Row, G <: Row, A, B](m: Free[F with G, A])(ret: A => B ! G)(h: F !> (B ! G))
                                                 (implicit T: TypeableK[F], d: Distinct[F with G]): B ! G =
      handleWith[A, B, F, G](m)(ret)(h)(T, d)

    // level 1: the top-level functions themselves
    override def shift[R, A, F <: Row](f: (A => R ! (Shift[R] + F)) => R ! (Shift[R] + F))(implicit k: Shift.Key[R], at: At): A ! (Shift[R] + F) =
      okay2.shift[R, A, F](f)(k, at)
    override def shift0[R, A, F <: Row](f: (A => R ! F) => R ! F)(implicit k: Shift.Key[R], at: At): A ! (Shift[R] + F) =
      okay2.shift0[R, A, F](f)(k, at)
    override def reset[R, F <: Row](body: R ! (Shift[R] + F))(implicit k: Shift.Key[R], n: Shift.Machine[F]): R ! F =
      okay2.reset[R, F](body)(k, n)
    override def handle[A, E <: Row, I, O[_], N[_ <: Row], F <: Row](m: Free[E with F, A], h: Handler.Full[E, I, O, N])
                                                                    (implicit ok: A <:< I, d: Distinct[E with F], n: N[F]): O[A] ! F =
      h.run[A, F](m)(ok, d, n)
    override def run[A](m: A ! Pure): A = runFree(m)
    override def foldMap[F <: Row, A, G[_]](m: Free[F, A])(nt: Static.To[F, G])(implicit G: Monad[G], R: TailRecM[G]): G[A] =
      foldMapFree[F, A, G](m)(nt)(G, R)
  }

  /** `foldMap` over the tree itself: one resume per step of G's loop */
  def foldMapFree[F <: Row, A, G[_]](m: Free[F, A])(nt: Static.To[F, G])(implicit G: Monad[G], R: TailRecM[G]): G[A] =
    R.tailRecM[A ! F, A](m) { p =>
      Free.resume(p) match {
        case Return(a) => G.pure[Either[A ! F, A]](Right(a))
        case Inject(e) => G.fmap(nt[A](Split.only[F, A](e)), (a: A) => Right(a): Either[A ! F, A])
        case Bind(Inject(e), k) => G.fmap(nt[Any](Split.only[F, Any](e)), (x: Any) => Left(k(x)): Either[A ! F, A])
        case other => throw new IllegalStateException("resume left a non-head form: " + other)
      }
    }

  /** a program reflected into Cont: each operation by `h`, the rest of
   * the program deferred into Cont's own trampoline */
  def foldContFree[F <: Row, A, S](m: Free[F, A])(h: F !> S): A /> S = Free.resume(m) match {
    case Return(a) => Cont.Pure[A, S](a)
    case Inject(e) => h[A](Split.only[F, A](e))
    case Bind(Inject(e), k) => Cont.bind(h[Any](Split.only[F, Any](e)))((x: Any) => Cont.delay(() => foldContFree(k(x))(h)))
    case other => throw new IllegalStateException("resume left a non-head form: " + other)
  }

  /**
   * `traverse`/`sequence`/`replicateA` AT PROGRAMS, where the generic
   * ones (Monad.scala) cannot see a program typed `A ! R` — partial
   * unification reads the alias with its parameters reversed (see
   * `ProgOps`). Effects in order, results collected.
   */
  def traverse[A, B, R <: Row](xs: Seq[A])(f: A => Free[R, B]): Seq[B] ! R =
    okay2.traverse[({ type L[X] = Free[R, X] })#L, A, B](xs)(f)

  def sequence[A, R <: Row](ps: Seq[Free[R, A]]): Seq[A] ! R =
    traverse[Free[R, A], A, R](ps)(identity)

  def replicateA[A, R <: Row](n: Int)(p: Free[R, A]): Seq[A] ! R =
    sequence[A, R](Seq.fill(n)(p))

  /** run a closed computation */
  def run[A](p: Free[Pure, A]): A = runFree(p)

  /** run all the effects by a comonadic Answers: one pass, the
   * `runWith` fast path */
  @tailrec def runFree[R <: Row, A](p: Free[R, A])(implicit H: Answers[R]): A = Free.resume(p) match {
    case Return(a) => a
    case Inject(e) => H.handleOp[A](e)
    case Bind(Inject(e), k) => runFree(k(H.handleOp[Any](e)))
    case other => throw new IllegalStateException("resume left a non-head form: " + other)
  }

  /** step through the next n operations by the Answers */
  @tailrec def next[R <: Row, A](p: Free[R, A], steps: Long)(implicit H: Answers[R]): A ! R = Free.resume(p) match {
    case Bind(Inject(e), k) if steps > 0 => next(k(H.handleOp[Any](e)), steps - 1)
    case a => a
  }

  /** peek the nearest answer: the value, or the first operation handled */
  @tailrec def peek[R <: Row, A](p: Free[R, A])(implicit H: Answers[R]): Any = p match {
    case Bind(a, _) => peek(a)
    case Inject(e) => H.handleOp[Any](e)
    case Return(a) => a
    case Delay(t) => peek(t())
  }

  /** mark a call to a mutually-recursive function returning `A ! R` as
   * a tail call, so the interpreter trampolines it instead of nesting
   * a JVM stack frame per call */
  def tailcall[R <: Row, A](thunk: => Free[R, A]): A ! R = Free.delay(() => thunk)

  /** `tailRecM` for programs: run `f` from `s`, continue from a `Left`,
   * answer a `Right`. Stack-safe without a trampoline of its own: the
   * recursive call sits INSIDE the flatMap's continuation */
  def loop[S, A, R <: Row](s: S)(f: S => Either[S, A] ! R): A ! R =
    f(s).flatMap {
      case Left(next) => loop(next)(f)
      case Right(a) => Return(a)
    }

  /** the same program in a wider row: effect subsumption as a COERCION */
  def widen[A, F <: Row, G <: Row](p: Free[F, A]): A ! (F + G) = p

  /** p, run at most once (call-by-need for programs): the first demand
   * runs it, every later demand of THIS value answers from the cell
   * `Once.run` keeps — see `Once` */
  def once[A, F <: Row](p: => Free[Once with F, A]): A ! (Once + F) = Once.once[A, F](p)

  /**
   * handle_relay (Kiselyov): tail-resumptive handling. `g` is
   * answer-polymorphic, so by parametricity it must resume the
   * continuation exactly once, which keeps the loop tail-recursive —
   * stack-safe on any number of handled operations. For handlers that
   * abort or perform G, use `handle`.
   */
  def relay[A, B, F <: Row, G <: Row](a: Free[F with G, A])(f: A => B ! G)(g: Relay[F])(implicit T: TypeableK[F], @unused d: Distinct[F with G]): B ! G = {
    // the split as a pattern: the handled arm is the loop's own tail
    // call, nothing allocated for the step (okay2-handler-allocs)
    val Mine = Split.at[F](T)
    @tailrec def loop(x: Free[F with G, A]): B ! G = Free.resume(x) match {
      // `g(e) / k`: the Cont's application; the handler answers, k continues
      case Bind(Inject(Mine(op)), k) => loop(g[Any, A ! (F + G)](op) / k)
      case Bind(Inject(e), k) => Inject[G, Any](e).flatMap(x => relay[A, B, F, G](k(x))(f)(g))
      // a lone operation is a Bind with a pure continuation (see package.scala)
      case Inject(e) => loop(Bind(Inject[F + G, A](e), (x: A) => Return[F + G, A](x)))
      case Return(v) => f(v)
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }
    loop(a)
  }

  /**
   * Interpret F into ANOTHER ROW rather than into a value: a handler
   * valued in a PROGRAM, so an operation may answer with more
   * computation. Every step suspends under a flatMap, so the recursion
   * lives in closures rather than on the stack.
   */
  def translate[A, F <: Row, G <: Row](prog: Free[F with G, A])(h: Interpret[F, G])(implicit T: TypeableK[F], @unused d: Distinct[F with G]): A ! G = {
    val Mine = Split.at[F](T)
    // a call from inside flatMap cannot be a jump; `again` takes it, so the walk
    // itself stays a checked loop (specs/stack-safety.md)
    def again(prog: Free[F with G, A]): A ! G = go(prog)
    @tailrec def go(prog: Free[F with G, A]): A ! G = Free.resume(prog) match {
      case Return(a) => Return(a)
      case Inject(e) => go(Bind(Inject[F + G, A](e), (x: A) => Return[F + G, A](x)))
      case Bind(Inject(Mine(f)), k) => h[Any](f).flatMap(x => again(k(x)))
      case Bind(Inject(g), k) => Inject[G, Any](g).flatMap(x => again(k(x)))
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }
    go(prog)
  }

  /** `translate` with the widening done for you: interpret F into
   * G + H, carrying H through untouched */
  def interpret[A, F <: Row, G <: Row, H <: Row](prog: Free[F with H, A])(h: Interpret[F, G + H])(implicit T: TypeableK[F], d: Distinct[F with (G + H)]): A ! (G + H) =
    translate[A, F, G + H](prog)(h)(T, d)

  /**
   * Handle F by a Cont-valued handler `h` (abort, multi-shot, answer
   * in G), forwarding G; the values by `ret`. A forwarded operation is
   * re-emitted on the G side exactly as `relay` does it, and `Cont` is
   * entered ONLY for an operation the handler claims. A handler that
   * does not capture answers with `Cont.Pure`, and the loop simply
   * CONTINUES on that answer — one tail call, nothing allocated; only a
   * handler that really captures needs the rest of the program
   * reified, under a `Delay` so that deep programs trampoline.
   */
  def handle[F <: Row, G <: Row]: Handling[F, G] = new Handling[F, G](true)

  /**
   * `handle`'s second half: okay's `Effects[Free].handle[F, G][A, B](m)
   * (ret)(h)` names the effect and the rest FIRST and lets the program
   * and the answer be inferred. Scala 2 has no second type-parameter
   * clause, so `handle[F, G]` answers this, and its `apply` takes the
   * rest: `!.handle[Throws[String], Produce](prog)(ret)(h)`. A value
   * class, so nothing is allocated for the split.
   */
  final class Handling[F <: Row, G <: Row](private val u: Boolean) extends AnyVal {
    def apply[A, B](m: Free[F with G, A])(ret: A => B ! G)(h: F !> (B ! G))(implicit T: TypeableK[F], d: Distinct[F with G]): B ! G =
      handleWith[A, B, F, G](m)(ret)(h)(T, d)
  }

  /** the handler loop itself, every type named */
  def handleWith[A, B, F <: Row, G <: Row](m: Free[F with G, A])(ret: A => B ! G)(h: F !> (B ! G))(implicit T: TypeableK[F], @unused d: Distinct[F with G]): B ! G = {
    // the split as a pattern (okay2-handler-allocs)
    val Mine = Split.at[F](T)

    def capture(c: Cont[Any, B ! G, B ! G], k: Any => A ! (F + G)): B ! G =
      c / (x => Free.delay(() => _loop(k(x))))

    // the closures' entry: a call from inside a closure is not a tail
    // call, and `@tailrec` reads it as one that is not in tail position
    def _loop(x: Free[F with G, A]): B ! G = loop(x)

    // the answered arm is a REAL tail call (`Left` back to the loop),
    // which is what keeps 1M handled operations off the JVM stack:
    // the first cut continued inside a closure and overflowed
    @tailrec def loop(x: Free[F with G, A]): B ! G = Free.resume(x) match {
      case Return(a) => ret(a)
      case Inject(e) => loop(Bind(Inject[F + G, A](e), (x: A) => Return[F + G, A](x)))
      case Bind(Inject(Mine(op)), k) =>
        // `h` is asked ONCE: the answered test and the fallback both
        // read the same program, and a handler is not assumed pure
        val c = h[Any](op)
        if (Cont.isAnswer(c)) loop(k(Cont.answerOf(c))) else capture(c, k)
      case Bind(Inject(e), k) => Inject[G, Any](e).flatMap(x => _loop(k(x)))
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }

    loop(m)
  }
}
