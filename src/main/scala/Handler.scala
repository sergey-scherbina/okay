package okay

import okay.Free.{Return, Inject, Bind}
import okay.Row.up
import scala.annotation.tailrec

/**
 * A handler (Plotkin & Pretnar's sense, level 1, specs/api-levels.md): a VALUE that takes the effect `E` off
 * any program's row and answers `O[A]` — `p.handle(State(5))`, `p.handle(State(5)).handle(Throws.either).run`.
 * `Handler[E, O]` is the usual one: any answer, nothing needed of the rest of the row. `Handler.Full` bounds the
 * answer by `I` and needs `Needs[F]` of the rest `F` (`Reset[R]`: the answer is `R`, the rest's `Shift.Machine`).
 * An answer per operation and no more, the old `Handler[F]`, is `Answers[F]`.
 */
type Handler[E[+_], O[_]] = Handler.Full[E, Any, O, Handler.Nothing]

object Handler:
  /** the handler in full: the answers it takes (`I`) and what it needs of the rest of the row (`Needs`) */
  trait Full[E[+_], I, O[_], Needs[_[+_]]]:
    def run[A, F[+_]](p: A ! E + F)(using A <:< I, Distinct[E + F], Needs[F]): O[A] ! F

  /** the evidence of nothing: always there */
  final class Nothing[F[+_]] private[Handler] ()
  object Nothing:
    given any[F[+_]]: Nothing[F] = new Nothing[F]()

  // THE AUTHOR'S DOOR (level 2, specs/handler-forms.md): four forms by power, each a level-1 value on the
  // machinery that is already fastest for its case.

  /**
   * The effect named once, and its forms with nothing more to write: `Handler[Accounts] { case Find(id) => … }`
   * is `Handler.answer[Accounts] { … }`, `Handler[Accounts].state(s0)` is `Handler.state[Accounts, S](s0)` with
   * `S` read off `s0`. Helpers for type inference only; each form has its one implementation below.
   */
  def apply[F[+_]]: For[F] = new For[F]

  final class For[F[+_]] private[Handler] ():
    /** the default form, 1: `Handler[Accounts] { case Find(id) => … }`, `Handler.answer[Accounts] { … }` */
    inline def apply(using s: Seen[F])(inline cases: F[s.T] => Any)(using TypeableK[F]): Handler[F, [A] =>> A] =
      Handler.answer[F].apply(using s)(cases)
    /** `Handler.answer[F]` */
    def answer: Answering[F] = Handler.answer[F]
    /** `Handler.state[F, S](init)`, `S` read off `init` */
    def state[S](init: S): Stating[F, S] = Handler.state[F, S](init)
    /** `Handler.into[F, G]` */
    def into[G[+_]]: Into[F, G] = Handler.into[F, G]
    /** `Handler.control[F, O](ret)(f)` */
    def control[O[_]](ret: [A] => A => O[A])(f: [X, A, G[+_]] => (F[X], X => O[A] ! G) => O[A] ! G)
                     (using TypeableK[F]): Handler[F, O] = Handler.control[F, O](ret)(f)

  /**
   * 1 · answer each operation with a value, and the program goes on (`!.relay`):
   * `Handler.answer[Users] { case Find(id) => … }`, or `.poly { [X] => (e: Users[X]) => … }`
   */
  def answer[F[+_]]: Answering[F] = new Answering[F]

  final class Answering[F[+_]] private[Handler] ():
    /** cases, each checked at compile time against its operation's answer type */
    inline def apply(using s: Seen[F])(inline cases: F[s.T] => Any)(using TypeableK[F]): Handler[F, [A] =>> A] =
      // THE CASTS the check licenses: an F[X] seen at the type the effect's cases see it at, each case
      // answering its operation's type
      answerOf[F]([X] => (e: F[X]) => checkAnswers[F, s.T](cases)(e.asInstanceOf[F[s.T]]).asInstanceOf[X])
    /** a polymorphic function, typed by the compiler with no macro */
    def poly(f: [X] => F[X] => X)(using TypeableK[F]): Handler[F, [A] =>> A] = answerOf[F](f)

  def answerOf[F[+_]](f: [X] => F[X] => X)(using TypeableK[F]): Handler[F, [A] =>> A] = new Handler[F, [A] =>> A]:
    def run[A, G[+_]](p: A ! F + G)(using A <:< Any, Distinct[F + G], Nothing[G]): A ! G =
      Effects.relay[A, A, F, G](p)(pure(_))([X, Y] => (e: F[X]) => Cont.Pure[X, Y](f(e)))

  /** 1 · the same from an `Answers[F]` (its own name: an overload would cost the lambda form its expected type) */
  def from[F[+_]](a: Answers[F])(using TypeableK[F]): Handler[F, [A] =>> A] =
    answerOf[F]([X] => (e: F[X]) => a.handle(e))

  /**
   * 2 · a state threaded through the operations: `(s, op) => (s', answer)`, the result carrying the last state:
   * `Handler[Users].state(0) { case (n, Find(id)) => … }`, or `.poly { [X] => (n: Int, e: Users[X]) => … }`. The
   * effect is named: read off the cases it would leave the pair's second an `Any`, which the compiler's own
   * exhaustiveness check over the tuple cannot see covered
   */
  /** 2 · a state threaded through the operations */
  def state[F[+_], S](init: S): Stating[F, S] = new Stating[F, S](init)

  final class Stating[F[+_], S] private[Handler] (val init: S):
    /** cases, each checked at compile time: the second of the pair answers the operation's type */
    inline def apply(using seen: Seen[F])(inline cases: (S, F[seen.T]) => Any)(using TypeableK[F]): Handler[F, [A] =>> (S, A)] =
      // THE CASTS the check licenses (checkStates)
      stateOf[F, S](init)([X] => (s: S, e: F[X]) => checkStates[F, S, seen.T](cases)(s, e.asInstanceOf[F[seen.T]]).asInstanceOf[(S, X)])
    /** a polymorphic function, typed by the compiler with no macro */
    inline def poly(inline f: [X] => (S, F[X]) => (S, X))(using TypeableK[F]): Handler[F, [A] =>> (S, A)] =
      stateOf[F, S](init)(f)

  @scala.annotation.nowarn("msg=New anonymous class definition will be duplicated")
  inline def stateOf[F[+_], S](init: S)(inline f: [X] => (S, F[X]) => (S, X))(using TypeableK[F]): Handler[F, [A] =>> (S, A)] =
    // a class per call site is the point: the clause expands into that site's own loop, where the JIT can
    // drop the pair it answers (handler-forms: 1.60x with the clause a function value)
    new Handler[F, [A] =>> (S, A)]:
      def run[A, G[+_]](p: A ! F + G)(using A <:< Any, Distinct[F + G], Nothing[G]): (S, A) ! G =
        // a call from inside flatMap cannot be a jump; `again` takes it, so the walk stays a checked loop
        def again(s: S)(x: A ! F + G): (S, A) ! G = loop(s)(x)
        // a run nested here (handle-frames): this loop becomes a frame of the machine over the rest
        def upgrade(s: S)(x: A ! F + G): (S, A) ! G =
          HandleFrames.pending[(S, A), G](HandleFrames.state[F, S, A, G](f, summon[TypeableK[F]])(s, x))
        @tailrec def loop(s: S)(x: A ! F + G): (S, A) ! G = (x.resumeRun: @unchecked) match
          case Return(a) => Return((s, a))
          case i @ Inject(e) => split[F, G](e)(op => { val (s2, v) = f(s, op); Return((s2, v)): (S, A) ! G })
                                               (_ => forwarded[F, G](i).map((s, _)))
          case Bind(i @ Inject(e), k) => split[F, G](e)(op => { val (s2, v) = f(s, op); loop(s2)(k(v)) })
                                                       (_ => forwarded[F, G](i).flatMap(x => again(s)(k(x))))
          case y => upgrade(s)(y)
        // a value: run by whoever forces it, a frame for a machine that meets it
        Free.delay(new HandleFrames.Run[(S, A), G]:
          def apply(): (S, A) ! G = loop(init)(p)
          def program: Shift.U[G, (S, A)] = HandleFrames.state[F, S, A, G](f, summon[TypeableK[F]])(init, p))

  /** what `into` needs of the rest of the row: that it holds `G` */
  type Holds[G[+_]] = [R[+_]] =>> Row.Sub[G, R]

  /**
   * 3 · each operation a program in the effects `G`, which the rest of the row must hold (`!.translate`):
   * `Handler[Users].into[State % M] { case Find(id) => … }`, or `.poly { [X] => (e: Users[X]) => … }`
   */

  /** 3 · each operation a program in the effects `G` */
  def into[F[+_], G[+_]]: Into[F, G] = new Into[F, G]

  final class Into[F[+_], G[+_]] private[Handler] ():
    /** cases, each checked at compile time: the program's value answers the operation's type */
    inline def apply(using s: Seen[F])(inline cases: F[s.T] => Any ! G)(using TypeableK[F]): Full[F, Any, [A] =>> A, Holds[G]] =
      // THE CASTS the check licenses (checkInto)
      intoOf[F, G]([X] => (e: F[X]) => checkInto[F, G, s.T](cases)(e.asInstanceOf[F[s.T]]).asInstanceOf[X ! G])
    /** a polymorphic function, typed by the compiler with no macro */
    def poly(f: [X] => F[X] => X ! G)(using TypeableK[F]): Full[F, Any, [A] =>> A, Holds[G]] = intoOf[F, G](f)

  def intoOf[F[+_], G[+_]](f: [X] => F[X] => X ! G)(using TypeableK[F]): Full[F, Any, [A] =>> A, Holds[G]] =
    new Full[F, Any, [A] =>> A, Holds[G]]:
      def run[A, R[+_]](p: A ! F + R)(using A <:< Any, Distinct[F + R], Row.Sub[G, R]): A ! R =
        Effects.translate[A, F, R](p)([X] => (e: F[X]) => f(e).up[R])

  /**
   * 4 · the continuation in hand, `Handler[F].control[O](ret) { … }`: `resume` once, twice, or not at all
   * (`Effects.handle`). `ret` shapes a
   * finished program's answer; the clause is polymorphic in that answer and in the rest of the row.
   */
  def control[F[+_], O[_]](ret: [A] => A => O[A])(f: [X, A, G[+_]] => (F[X], X => O[A] ! G) => O[A] ! G)
                          (using TypeableK[F]): Handler[F, O] = new Handler[F, O]:
    def run[A, G[+_]](p: A ! F + G)(using A <:< Any, Distinct[F + G], Nothing[G]): O[A] ! G =
      Effects[Free].handle[F, G](p)(a => pure[G, O[A]](ret(a)))(
        [X] => (e: F[X]) =>
          val resume = new Resume[X, O[A], G]
          val out = f[X, A, G](e, resume)
          // `resume(x)` once, as the clause's answer: the program goes on, nothing to capture
          if resume.calls == 1 && (out eq resume.last) then Cont.Pure[X, O[A] ! G](resume.arg)
          else Cont.shift[X, O[A] ! G, O[A] ! G](k => { resume.k = k; out }))

  /**
   * The `resume` a `control` clause gets. Called once and returned as the clause's answer, it is a tail
   * resume, answered with no capture; otherwise each call is a program that enters the captured `k`, which
   * the capture fills in before any of them runs.
   */
  private final class Resume[X, B, G[+_]] extends (X => B ! G), (() => B ! G):
    var k: X => B ! G = scala.compiletime.uninitialized
    var arg: X = scala.compiletime.uninitialized
    var last: B ! G = scala.compiletime.uninitialized
    var calls: Int = 0
    def apply(x: X): B ! G =
      calls += 1
      // the first call's node defers to this object itself (its `arg` is that call's, and only a second call
      // could change it, which gets a closure of its own): one allocation less a resumed operation
      last = if calls == 1 then { arg = x; Free.delay[G, B](this) } else Free.delay[G, B](() => k(x))
      last
    /** the first call's resumption */
    def apply(): B ! G = k(arg)

  // THE CHECKS behind the `{ case … }` forms: each returns its cases unchanged, after proving that every case
  // answers what its operation answers (the pattern's constructor, read as an `F[T]`, answers `T`). A case
  // with no constructor to read (`case _`) must not answer at all (a `throw`).

  inline def checkAnswers[F[+_], T](inline cases: F[T] => Any): F[T] => Any = ${ okay.macros.HandlerMacros.checkImpl[F, Unit, T, F[T] => Any]('cases, 0) }
  // the pair is checked here, not by the expected type: a case the macro must refuse with a reason
  // (`tied`, below) would otherwise fail first on its pair, with none
  inline def checkStates[F[+_], S, T](inline cases: (S, F[T]) => Any): (S, F[T]) => Any =
    ${ okay.macros.HandlerMacros.checkImpl[F, S, T, (S, F[T]) => Any]('cases, 1) }
  inline def checkInto[F[+_], G[+_], T](inline cases: F[T] => Any ! G): F[T] => Any ! G = ${ okay.macros.HandlerMacros.checkImpl[F, Unit, T, F[T] => Any ! G]('cases, 2) }

  /**
   * The type an effect's cases see its operations at, picked from the effect BEFORE the cases are typed
   * (handler-shape). `Answer`, abstract, is the default: an operation whose caller chooses its answer
   * (`Asks[R, A]`) can then only be answered from its own data. An effect with an operation whose answer is
   * its own field's type (`Put(k, v: V) extends KV[V, V]`) and none chosen by its caller is seen at `Any`
   * instead: at `Answer` the typer could not keep the field's type. An effect with both keeps `Answer`, and
   * the macro refuses the field's operation by name, pointing at `.poly`.
   */
  trait Seen[F[+_]]:
    type T

  object Seen:
    transparent inline given of[F[+_]]: Seen[F] = ${ okay.macros.HandlerMacros.seenImpl[F] }

  /** the answer the cases see an operation at (`HandlerAnswer.Answer`) */
  type Answer = HandlerAnswer.Answer

/** apart from `Handler`, so that inside the macro too `Answer` is abstract and not `Any` */
object HandlerAnswer:
  /**
   * The answer the cases see an operation at: abstract, with no value of its own. An operation whose answer is
   * fixed (`Find`: `Option[String]`) is seen at that type; one whose caller chooses it (`Asks[R, A]`) is seen
   * at `Answer`, which only the operation's own data can produce — `g(1)`, never `"oops"`. Parametricity, as
   * the polymorphic form has it, without the `[X] =>`.
   */
  opaque type Answer = Any
