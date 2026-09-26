package okay

import scala.annotation.tailrec
import okay.!.*

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
  def run[S](first: S)(step: S => S)[A, F[+_]](p: A ! Supply % S + F)
            (using Distinct[Supply % S + F]): (S, A) ! F = {
    def _loop(s: S)(x: A ! Supply % S + F): (S, A) ! F = loop(s)(x)

    @tailrec def loop(s: S)(x: A ! Supply % S + F): (S, A) ! F = (x.resume: @unchecked) match
      case Return(a) => Return((s, a))
      case Inject(e) => split[Supply % S, F](e) {
          case Next() => Return((step(s), s)): (S, A) ! F
        } { e => Inject(e).map((s, _)) }
      case Bind(Inject(e), k) => split[Supply % S, F](e) {
          case Next() => loop(step(s))(k(s))
        } { e => Inject(e).flatMap(x => _loop(s)(k(x))) }

    loop(first)(p)
  }
