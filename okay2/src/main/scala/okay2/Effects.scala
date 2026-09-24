package okay2

import scala.annotation.tailrec
import Free.{Return, Inject, Bind, Delay}
import Split.split

/**
 * The toolkit over Free: running, stepping, and the three
 * interpreters — the tail-resumptive `relay`, the row-rewriting
 * `translate`, and the Cont-valued `handle` (abort, multi-shot).
 * Aliased as `!` in the package object, as in the Scala 3 core.
 */
object Effects {

  /** run a closed computation */
  def run[A](p: A ! Pure): A = runFree(p)

  /** run all the effects by a comonadic Handler: one pass, the
   * `runWith` fast path */
  @tailrec def runFree[R <: Row, A](p: A ! R)(implicit H: Handler[R]): A = Free.resume(p) match {
    case Return(a) => a
    case Inject(e) => H.handle(e)
    case Bind(Inject(e), k) => runFree(k(H.handle(e)))
    case other => throw new IllegalStateException("resume left a non-head form: " + other)
  }

  /** step through the next n operations by the Handler */
  @tailrec def next[R <: Row, A](p: A ! R, steps: Long)(implicit H: Handler[R]): A ! R = Free.resume(p) match {
    case Bind(Inject(e), k) if steps > 0 => next(k(H.handle(e)), steps - 1)
    case a => a
  }

  /** peek the nearest answer: the value, or the first operation handled */
  @tailrec def peek[R <: Row, A](p: A ! R)(implicit H: Handler[R]): Any = p match {
    case Bind(a, _) => peek(a)
    case Inject(e) => H.handle(e)
    case Return(a) => a
    case Delay(t) => peek(t())
  }

  /** mark a call to a mutually-recursive function returning `A ! R` as
   * a tail call, so the interpreter trampolines it instead of nesting
   * a JVM stack frame per call */
  def tailcall[R <: Row, A](thunk: => A ! R): A ! R = Free.delay(() => thunk)

  /** `tailRecM` for programs: run `f` from `s`, continue from a `Left`,
   * answer a `Right`. Stack-safe without a trampoline of its own: the
   * recursive call sits INSIDE the flatMap's continuation */
  def loop[S, A, R <: Row](s: S)(f: S => Either[S, A] ! R): A ! R =
    f(s).flatMap {
      case Left(next) => loop(next)(f)
      case Right(a) => Return(a)
    }

  /** the same program in a wider row: effect subsumption as a COERCION */
  def widen[A, F <: Row, G <: Row](p: A ! F): A ! (F + G) = Member.coerce(p)

  /**
   * handle_relay (Kiselyov): tail-resumptive handling. `g` is
   * answer-polymorphic, so by parametricity it must resume the
   * continuation exactly once, which keeps the loop tail-recursive —
   * stack-safe on any number of handled operations. For handlers that
   * abort or perform G, use `handle`.
   */
  def relay[A, B, F <: Row, G <: Row](a: A ! (F + G))(f: A => B ! G)(g: Relay[F])(implicit T: TypeableK[F]): B ! G = {
    @tailrec def loop(x: A ! (F + G)): B ! G = Free.resume(x) match {
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
  def translate[A, F <: Row, G <: Row](prog: A ! (F + G))(h: Interpret[F, G])(implicit T: TypeableK[F]): A ! G =
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
  def interpret[A, F <: Row, G <: Row, H <: Row](prog: A ! (F + H))(h: Interpret[F, G + H])(implicit T: TypeableK[F]): A ! (G + H) =
    translate[A, F, G + H](Member.coerce[A, F + H, F + (G + H)](prog))(h)

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
  def handle[A, B, F <: Row, G <: Row](m: A ! (F + G))(ret: A => B ! G)(h: F !> (B ! G))(implicit T: TypeableK[F]): B ! G = {
    def capture(c: Cont[Any, B ! G, B ! G], k: Any => A ! (F + G)): B ! G =
      c / (x => Free.delay(() => _loop(k(x))))

    // the closures' entry: a call from inside a closure is not a tail
    // call, and `@tailrec` reads it as one that is not in tail position
    def _loop(x: A ! (F + G)): B ! G = loop(x)

    // the answered arm is a REAL tail call (`Left` back to the loop),
    // which is what keeps 1M handled operations off the JVM stack:
    // the first cut continued inside a closure and overflowed
    @tailrec def loop(x: A ! (F + G)): B ! G = Free.resume(x) match {
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
