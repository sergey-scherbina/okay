package okay.std

import okay.freer.*

import okay.{Effect, TypeableK}


/**
 * A SOURCE OF FRESH VALUES (specs/core-gaps.md, 2026-09-26): each draw
 * answers one not answered before. Launchbury's value supply, the
 * `Fresh` effect of fused-effects and polysemy.
 *
 *     val three = for a <- Fresh.next; b <- Fresh.next; c <- Fresh.next yield List(a, b, c)
 *     !.run(Fresh.run(three))   // List(0, 1, 2)
 *
 * `State % Long` with `modify(_ + 1)` does the same and exposes `set`
 * to code that should only ever DRAW. A supply has one operation, so
 * the code drawing from it cannot rewind it.
 *
 * PARAMETERISED, so the derived test is by class only and a row may
 * hold one Supply (`Distinct` says so at compile time; `Tag` names two).
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

  /** the handler as a value: `p.handle(Fresh.counter)` — fresh numbers from 0 */
  def counter: Handler[Fresh, [A] =>> A] = new Handler.Stepped[Fresh, Long, [A] =>> A]:
    def run[A, F[+_]](p: A ! Fresh + F)(using A <:< Any, Distinct[Fresh + F], Handler.Nothing[F]): A ! F = Fresh.run(p)
    // the step `run` walks with, exposed for a walk over a stack of handlers (handler-single-pass)
    def takes: TypeableK[Fresh] = summon[TypeableK[Fresh]]
    def init: Long = 0L
    def step(n: Long, op: Any): (Long, Any) | Handler.Halt[Long] = (n + 1, n)
    def ret[A, F[+_]](n: Long, a: A): A ! F = pure(a)

  /** handle Fresh from 0, forwarding the effects F */
  def run[A, F[+_]](p: A ! Fresh + F)(using Distinct[Fresh + F]): A ! F =
    Supply.run(0L)(_ + 1)(p).map(_._2)

object Supply:
  /** draw the next value */
  inline def next[S]: S ! Supply % S = effect(Next())

  /**
   * the handler: the first draw answers `first`, each later one `step`
   * of the one before; the answer carries the next UNDRAWN value, so a
   * later supply can continue where this one stopped.
   *
   * The seed is threaded through the loop, as State's is, not kept in a
   * mutable cell: the residual program stays re-runnable, and under a
   * multi-shot handler (Choose) each branch continues from the seed it
   * was captured with (single-shot-row priced a cell in 2026-09 and
   * refuted it).
   */
  /** the handler as a value: `p.handle(Supply.from(first)(step))`, answering the next supply and `A` */
  def from[S](first: S)(next: S => S): Handler[Supply % S, [A] =>> (S, A)] = new Handler.Stepped[Supply % S, S, [A] =>> (S, A)]:
    def run[A, F[+_]](p: A ! Supply % S + F)(using A <:< Any, Distinct[Supply % S + F], Handler.Nothing[F]): (S, A) ! F =
      Supply.run(first)(next)(p)
    // the step `run` walks with, exposed for a walk over a stack of handlers (handler-single-pass)
    def takes: TypeableK[Supply % S] = summon[TypeableK[Supply % S]]
    def init: S = first
    def step(s: S, op: Any): (S, Any) | Handler.Halt[S] = (next(s), s)
    def ret[A, F[+_]](s: S, a: A): (S, A) ! F = pure((s, a))

  def run[S](first: S)(step: S => S)[A, F[+_]](p: A ! Supply % S + F)
            (using Distinct[Supply % S + F]): (S, A) ! F = {
    // one step, both faces (handler-one-step): `Next` answers the state and steps it
    HandleFrames.stateRun[Supply % S, S, A, (S, A), F](summon[TypeableK[Supply % S]], (s, a) => pure((s, a)))(
      (s, _) => (step(s), s))(first, p)
  }
