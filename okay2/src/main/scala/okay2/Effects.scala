package okay2

import scala.annotation.unused

import scala.annotation.tailrec
import Free.{Return, Inject, Bind, Delay}
import Split.split

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
  /** run every effect by a comonadic Handler; encodings may override
   * with an equivalent fast path */
  def runWith[F <: Row, A](m: M[F, A])(implicit H: Handler[F]): A = foldCont[F, A, A](m)(Interpr.of[F, A]) / identity
}

/**
 * The toolkit over Free: running, stepping, and the three
 * interpreters — the tail-resumptive `relay`, the row-rewriting
 * `translate`, and the Cont-valued `handle` (abort, multi-shot).
 * Aliased as `!` in the package object, as in the Scala 3 core. Also
 * the companion of `trait Effects`, holding its `Free` instance.
 */
object Effects {

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
    override def runWith[F <: Row, A](m: Free[F, A])(implicit H: Handler[F]): A = runFree(m)
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

  /** run all the effects by a comonadic Handler: one pass, the
   * `runWith` fast path */
  @tailrec def runFree[R <: Row, A](p: Free[R, A])(implicit H: Handler[R]): A = Free.resume(p) match {
    case Return(a) => a
    case Inject(e) => H.handleOp[A](e)
    case Bind(Inject(e), k) => runFree(k(H.handleOp[Any](e)))
    case other => throw new IllegalStateException("resume left a non-head form: " + other)
  }

  /** step through the next n operations by the Handler */
  @tailrec def next[R <: Row, A](p: Free[R, A], steps: Long)(implicit H: Handler[R]): A ! R = Free.resume(p) match {
    case Bind(Inject(e), k) if steps > 0 => next(k(H.handleOp[Any](e)), steps - 1)
    case a => a
  }

  /** peek the nearest answer: the value, or the first operation handled */
  @tailrec def peek[R <: Row, A](p: Free[R, A])(implicit H: Handler[R]): Any = p match {
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
    @tailrec def loop(x: Free[F with G, A]): B ! G = Free.resume(x) match {
      case Bind(Inject(e), k) =>
        // `g(e) / k`: the Cont's application; the handler answers, k continues
        split[F, G, Any, Either[A ! (F + G), B ! G]](e) { e =>
          Left(g[Any, A ! (F + G)](e) / k)
        } { e =>
          Right(Inject[G, Any](e).flatMap(x => relay[A, B, F, G](k(x))(f)(g)))
        } match {
          case Left(next) => loop(next)
          case Right(done) => done
        }
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
  def translate[A, F <: Row, G <: Row](prog: Free[F with G, A])(h: Interpret[F, G])(implicit T: TypeableK[F], @unused d: Distinct[F with G]): A ! G =
    Free.resume(prog) match {
      case Return(a) => Return(a)
      case Inject(e) => translate[A, F, G](Bind(Inject[F + G, A](e), (x: A) => Return[F + G, A](x)))(h)
      case Bind(Inject(e), k) =>
        split[F, G, Any, A ! G](e) { f =>
          h[Any](f).flatMap(x => translate[A, F, G](k(x))(h))
        } { g =>
          Inject[G, Any](g).flatMap(x => translate[A, F, G](k(x))(h))
        }
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
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
      case Bind(Inject(e), k) =>
        // `h` is asked ONCE: the answered test and the fallback both
        // read the same program, and a handler is not assumed pure
        split[F, G, Any, Either[A ! (F + G), B ! G]](e) { e =>
          val c = h[Any](e)
          if (Cont.isAnswer(c)) Left(k(Cont.answerOf(c))) else Right(capture(c, k))
        } { e =>
          Right(Inject[G, Any](e).flatMap(x => _loop(k(x))))
        } match {
          case Left(next) => loop(next)
          case Right(done) => done
        }
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }

    loop(m)
  }
}
