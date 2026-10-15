package okay.std

import okay.{!, %, Effect, Handler, effect}
import okay.core.{Row, Split}
import okay.one.handler.{Stateful, Handler as OneHandler}


/**
 * A SOURCE OF FRESH VALUES (specs/core-gaps.md, 2026-09-26): each draw
 * answers one not answered before. Launchbury's value supply, the
 * `Fresh` effect of fused-effects and polysemy.
 *
 *     val three = for a <- Fresh.next; b <- Fresh.next; c <- Fresh.next yield List(a, b, c)
 *     Fresh.run(three).run   // List(0, 1, 2)
 *
 * `State % Long` with `modify(_ + 1)` does the same and exposes `set`
 * to code that should only ever DRAW. A supply has one operation, so
 * the code drawing from it cannot rewind it.
 *
 * PARAMETERISED; on the kernel's rows an operation goes to its handler
 * by its PATH, so two supplies in a row are two places, each draw going
 * to its own handler (okay-freer's `Distinct` was the union's).
 */
enum Supply[S, +A] derives Effect:
  /** the next value */
  case Next() extends Supply[S, S]

/** fresh numbers: 0, 1, 2 … under `Fresh.run` */
type Fresh = Supply % Long

/** the numbers' own door. Not a top-level `fresh`: that name is DI's
 * (`Provide.scala`, the ability to make a new A) */
object Fresh:
  /** a fresh number */
  inline def next: Long ! Fresh = Supply.next[Long]

  /** the handler as a value: `p.handle(Fresh.counter)` — fresh numbers from 0, the next one a register of the
   * machine (the kernel's `Stateful`), the answer the body's value */
  def counter: Handler[Fresh, [A] =>> A] = new Handler[Fresh, [A] =>> A]:
    def kernel[T <: Row, A]: OneHandler[Fresh, T, A, A] = new Stateful[Fresh, T, Long, A, A]:
      def initial: Long = 0L
      def ret(n: Long, a: A) = okay.one.Cont.pure(a)
      def value[X](n: Long, op: Supply[Long, X]): X = op match
        case Supply.Next() => n
      def next[X](n: Long, op: Supply[Long, X]): Long = n + 1

  /** handle Fresh from 0, wherever it is among the program's effects: the rest of them forwarded */
  def run[A, Es[_]](p: A ! Es)(using s: Split[Es, Fresh]): A ! s.Rest = p.handle(counter)

object Supply:
  /** draw the next value */
  inline def next[S]: S ! Supply % S = effect(Next())

  /**
   * the handler: the first draw answers `first`, each later one `step`
   * of the one before; the answer carries the next UNDRAWN value, so a
   * later supply can continue where this one stopped.
   *
   * The seed is the handler's state, as State's is, not kept in a
   * mutable cell: the residual program stays re-runnable, and under a
   * multi-shot handler (Choose) each branch continues from the seed it
   * was captured with (single-shot-row priced a cell in 2026-09 and
   * refuted it).
   */
  /** the handler as a value: `p.handle(Supply.from(first)(step))`, answering the next supply and `A` (the kernel's
   * `Stateful`) */
  def from[S](first: S)(next: S => S): Handler[Supply % S, [A] =>> (S, A)] =
    Handler.state[Supply % S, S](first)([X] => (s: S, op: Supply[S, X]) => op match
      case Next() => (next(s), s))

  /** `p`'s draws from `first`, wherever Supply is among its effects: the rest of them forwarded */
  def run[S](first: S)(step: S => S)[A, Es[_]](p: A ! Es)(using sp: Split[Es, Supply % S]): (S, A) ! sp.Rest =
    p.handle(from(first)(step))
