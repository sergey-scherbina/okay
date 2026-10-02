package okay

import okay.Free.{Return, Inject, Bind}
import okay.Row.up
import scala.annotation.tailrec

/**
 * A handler (Plotkin & Pretnar's sense, level 1, specs/api-levels.md): a VALUE that takes the effect `E` off
 * any program's row and answers `O[A]` — `p.handle(State(5))`, `p.handle(State(5)).handle(Throws.either).run`.
 * `Handler[E, O]` is the usual one: any answer, nothing needed of the rest of the row. `Handler.Full` bounds the
 * answer by `I` and needs `Needs[F]` of the rest `F` (`Reset[R]`: the answer is `R`, the rest's `Nesting`).
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

  /** 1 · answer each operation with a value, and the program goes on (`!.relay`) */
  def answer[F[+_]](f: [X] => F[X] => X)(using TypeableK[F]): Handler[F, [A] =>> A] = new Handler[F, [A] =>> A]:
    def run[A, G[+_]](p: A ! F + G)(using A <:< Any, Distinct[F + G], Nothing[G]): A ! G =
      Effects.relay[A, A, F, G](p)(pure(_))([X, Y] => (e: F[X]) => Cont.Pure[X, Y](f(e)))

  /** 1 · the same from an `Answers[F]` (its own name: an overload would cost the lambda form its expected type) */
  def from[F[+_]](a: Answers[F])(using TypeableK[F]): Handler[F, [A] =>> A] =
    answer[F]([X] => (e: F[X]) => a.handle(e))

  /** 2 · a state threaded through the operations: `(s, op) => (s', answer)`; the result carries the last state */
  def state[F[+_], S](init: S)(f: [X] => (S, F[X]) => (S, X))(using TypeableK[F]): Handler[F, [A] =>> (S, A)] =
    new Handler[F, [A] =>> (S, A)]:
      def run[A, G[+_]](p: A ! F + G)(using A <:< Any, Distinct[F + G], Nothing[G]): (S, A) ! G =
        // a call from inside flatMap cannot be a jump; `again` takes it, so the walk stays a checked loop
        def again(s: S)(x: A ! F + G): (S, A) ! G = loop(s)(x)
        @tailrec def loop(s: S)(x: A ! F + G): (S, A) ! G = (x.resume: @unchecked) match
          case Return(a) => Return((s, a))
          case i @ Inject(e) => split[F, G](e)(op => { val (s2, v) = f(s, op); Return((s2, v)): (S, A) ! G })
                                               (_ => forwarded[F, G](i).map((s, _)))
          case Bind(i @ Inject(e), k) => split[F, G](e)(op => { val (s2, v) = f(s, op); loop(s2)(k(v)) })
                                                       (_ => forwarded[F, G](i).flatMap(x => again(s)(k(x))))
        loop(init)(p)

  /** what `into` needs of the rest of the row: that it holds `G` */
  type Holds[G[+_]] = [R[+_]] =>> Row.Sub[G, R]

  /** 3 · each operation a program in the effects `G`, which the rest of the row must hold (`!.translate`) */
  def into[F[+_], G[+_]](f: [X] => F[X] => X ! G)(using TypeableK[F]): Full[F, Any, [A] =>> A, Holds[G]] =
    new Full[F, Any, [A] =>> A, Holds[G]]:
      def run[A, R[+_]](p: A ! F + R)(using A <:< Any, Distinct[F + R], Row.Sub[G, R]): A ! R =
        Effects.translate[A, F, R](p)([X] => (e: F[X]) => f(e).up[R])

  /**
   * 4 · the continuation in hand: `resume` once, twice, or not at all (`Effects.handle`). `ret` shapes a
   * finished program's answer; the clause is polymorphic in that answer and in the rest of the row.
   */
  def control[F[+_], O[_]](ret: [A] => A => O[A])(f: [X, A, G[+_]] => (F[X], X => O[A] ! G) => O[A] ! G)
                          (using TypeableK[F]): Handler[F, O] = new Handler[F, O]:
    def run[A, G[+_]](p: A ! F + G)(using A <:< Any, Distinct[F + G], Nothing[G]): O[A] ! G =
      Effects[Free].handle[F, G](p)(a => pure[G, O[A]](ret(a)))(
        [X] => (e: F[X]) => Cont.shift[X, O[A] ! G, O[A] ! G](k => f[X, A, G](e, k)))
