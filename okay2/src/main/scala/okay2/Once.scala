package okay2


import scala.annotation.tailrec
import Free.{Return, Inject, Bind}

/**
 * Call-by-need for programs, as an EFFECT. `Delay` is by-name: the loop
 * forces its thunk every time it reaches the node, and a shared node
 * reached twice runs twice. By-need is by-name plus one cell that
 * remembers the answer, and for a program that cell is a SEMANTICS —
 * "run these effects at most once" is observable — so it is not hidden
 * in the tree: it is an effect, and the cells live in the handler as
 * state threaded through its own loop, exactly as `State.handleAt`
 * threads `S`.
 *
 * Two operations, one handle:
 *
 *   Force(h)    : Option[A]   what the cell holds, or None — and on None
 *                             the handler marks the cell RUNNING
 *   Store(h, a) : A           fill the cell; answers what the cell holds
 *                             AFTER, so the first answer stored wins
 *
 * The handle carries no program, which keeps the effect's type free of
 * the row it lives in. MULTI-SHOT is decided by handler ORDER: a
 * search run INSIDE `Once.run` shares one store across its branches, a
 * search run OUTSIDE it backtracks the cells with the branches. A
 * handle demanded while its own program is still running (a knot) is
 * a loud `IllegalStateException`, not a second run and not a hang.
 */
sealed trait Once extends Row { type Op[+A] = Once.Op[A] }

object Once {
  sealed trait Op[+A]
  /** what the cell holds, or None (which marks it running) */
  final case class Force[A](h: Handle[A]) extends Op[Option[A]]
  /** fill the cell; answers what it holds after — the first store wins */
  final case class Store[A](h: Handle[A], a: A) extends Op[A]

  implicit val effect: Effect[Once] = Effect.of[Once]

  /** the cell's identity: compared by reference, holds nothing */
  final class Handle[A]

  /**
   * p, run at most once: the first demand runs it and stores the
   * answer under a fresh handle, every later demand of THIS value
   * answers from the store. `Free.delay` keeps p unbuilt until that
   * first demand — construction does no work. Two calls make two
   * handles: share the VALUE to share the cell.
   */
  def once[A, F <: Row](p: => Free[Once with F, A]): A ! (Once + F) =
    at[A, Once + F](new Handle[A])(
      h => Free.inject[Once, Option[A]](Force(h)).plus[F])(
      (h, a) => Free.inject[Once, A](Store(h, a)).plus[F])(
      () => p)

  /** the same over the WHOLE row R, with the two operations already
   * injected by the caller; `once` is this at `Once + F` */
  def at[A, R <: Row](h: Handle[A])
                     (force: Handle[A] => Option[A] ! R)
                     (store: (Handle[A], A) => A ! R)
                     (p: () => A ! R): A ! R =
    force(h).flatMap {
      case Some(a) => pure[R, A](a)
      case None => Free.delay(p).flatMap(a => store(h, a))
    }

  /** a cell whose program is in flight */
  private object Running

  /** the cells, threaded through the handler's loop */
  private type Cells = Map[Handle[_], Any]

  /**
   * One operation against the cells: the cells after, and the answer.
   * At the answer type `Any`, as every handler here runs (the split
   * hands the operation over at `Any`, and the continuation takes
   * `Any`), so the heterogeneous map needs no cast: a value under `h`
   * was put there by `Store(h, a)`, the only writer, and goes back out
   * through a continuation typed at the operation's own answer.
   */
  private def step(c: Cells, o: Op[Any]): (Cells, Any) = o match {
    case f: Force[_] => c.get(f.h) match {
      case None => (c + (f.h -> Running), None)
      case Some(Running) => throw new IllegalStateException(
        "Once: a handle was demanded while its own program is still running — a knot " +
          "(the program demands itself), or an interleaved search with Once.run OUTSIDE " +
          "it; put Once.run inside the search, or break the cycle")
      case Some(v) => (c, Some(v))
    }
    case s: Store[_] => c.get(s.h) match {
      case None | Some(Running) => (c + (s.h -> s.a), s.a)
      case Some(v) => (c, v)
    }
  }

  /** the handler, for a program whose row mentions `Once` anywhere */
  def run[A, R <: Row](a: Free[Once with R, A]): A ! R =
    runAt[A, R](a)

  /** the handler at its own shape: a tail-recursive loop threading the
   * cells, like `State.handleAt`; a forwarded F-effect suspends with
   * the cells captured immutably, so the residual is re-runnable */
  def runAt[A, F <: Row](a: Free[Once with F, A]): A ! F = {
    val Mine = Split.at[Once]   // the split as a pattern (okay2-handler-allocs)
    def _loop(c: Cells)(x: Free[Once with F, A]): A ! F = loop(c)(x)

    @tailrec def loop(c: Cells)(x: Free[Once with F, A]): A ! F = Free.resume(x) match {
      case Return(v) => Return(v)
      case Inject(e) => loop(c)(Bind(Inject[Once + F, A](e), (x: A) => Return[Once + F, A](x)))
      case Bind(Inject(Mine(o)), k) =>
        val (c2, v) = step(c, o)
        loop(c2)(k(v))
      case Bind(Inject(g), k) => Inject[F, Any](g).flatMap(x => _loop(c)(k(x)))
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }

    loop(Map.empty)(a)
  }
}
