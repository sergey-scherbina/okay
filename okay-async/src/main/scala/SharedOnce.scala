package okay

import okay.RowLift.plus
import scala.collection.mutable

/**
 * ONE STORE FOR MANY FIBRES (once-across-fibres, 2026-09-23).
 *
 * `Once.run` threads its cells through its own loop, which is what
 * makes a program under it replayable — and what makes a handle
 * shared between fibres run TWICE: each fibre has its own `Once.run`,
 * so each has its own cells, and a fibre forked inside a program
 * takes a snapshot. This is the other reading, the `memoize`/
 * `Deferred` of the async libraries: one store for every fibre that
 * runs through it, and a demand met while the program is in flight
 * WAITS for its answer — an `Async` await, resumed by the store —
 * instead of running the program again (the threaded reading) or
 * throwing (the threaded reading's knot).
 *
 *     val store = SharedOnce()
 *     Async.par(store.run(p), store.run(p))   // p runs once; both get its answer
 *
 * What it gives up is what the threading bought. A program under a
 * shared store is not replayable — the cells are a mutable object
 * outside the tree. And a KNOT — a program demanding its own handle
 * while it runs — is a HANG here, not `Once.run`'s exception: the
 * waiter is the very fibre that would have stored, and nothing in
 * `Async` names a fibre to tell the two apart. Reach for this only
 * where the handle genuinely crosses fibres; `Once.run` is still the
 * default reading, and `direct`'s `lazy val` compiles to it.
 *
 * Built on `translate`: each `Once` operation is answered with a
 * program in `Async + F`, so a waiting demand suspends the fibre the
 * way any await does.
 */
final class SharedOnce:
  import SharedOnce.Cell

  private val cells = mutable.Map.empty[Once.Handle[?], Cell[?]]

  /** the one cast, isolated, as in `Once.stored`: the cell under `h`
   * was made by `force[A](h: Handle[A])` or `store[A](h: Handle[A],
   * a: A)`, the only writers, and `h`'s type parameter IS that `A`.
   * The map is heterogeneous by construction and the type system has
   * no dependent map to say so. */
  private def cell[A](h: Once.Handle[A]): Option[Cell[A]] =
    cells.get(h).map(_.asInstanceOf[Cell[A]])

  /** None: this fibre runs the program (the cell is now Running).
   * Some: the answer, now or — through an await — when it arrives. */
  private def force[A](h: Once.Handle[A]): Option[A] ! Async =
    synchronized {
      cell(h) match
        case None => cells(h) = Cell.Running[A](Nil); pure(None)
        case Some(Cell.Done(v)) => pure(Some(v))
        case Some(Cell.Running(_)) => okay.await[Option[A]] { k =>
          // registered under the lock, against the store that may
          // have happened between the check above and this call
          val ready = synchronized {
            cell(h) match
              case Some(Cell.Done(v)) => Some(v)
              case Some(Cell.Running(ws)) => cells(h) = Cell.Running(k :: ws); None
              case None => cells(h) = Cell.Running(List(k)); None
          }
          ready.foreach(v => k(Some(v)))
        }
    }

  /** fill the cell — the first store wins — and wake every waiter */
  private def store[A](h: Once.Handle[A], a: A): A ! Async =
    val (v, waiters) = synchronized {
      cell(h) match
        case Some(Cell.Done(v)) => (v, Nil)
        case Some(Cell.Running(ws)) => cells(h) = Cell.Done(a); (a, ws)
        case None => cells(h) = Cell.Done(a); (a, Nil)
    }
    // outside the lock: a waiter resumes a fibre
    waiters.foreach(w => w(Some(v)))
    pure(v)

  /** one operation, answered. A METHOD, because the GADT refinement
   * holds in a method's match and not inside a polymorphic function
   * literal. `Once[+A]` is covariant, so the refinement is `Option[A']
   * <: X` rather than equality — which is enough since `Free` is
   * covariant in its answer (free-answer-variance, 2026-09-23); until
   * then each arm paid a `map(x => x)` for that upcast. */
  private def answer[X](o: Once[X]): X ! Async = o match
    case Once.Force(h) => force(h)
    case Once.Store(h, v) => store(h, v)

  /** the `Once` operations of `a` answered from THIS store, the rest
   * of the row forwarded */
  def runIn[A, F[+_]](a: A ! (Once + (Async + F))): A ! (Async + F) =
    !.translate[A, Once, Async + F](a)([X] => (o: Once[X]) => answer(o).plus[F])

  /** the common case: a program whose only other effect is `Async` */
  def run[A](a: A ! (Once + Async)): A ! Async =
    !.translate[A, Once, Async](a)([X] => (o: Once[X]) => answer(o))

object SharedOnce:
  /** a cell: the program is in flight (with everyone waiting for it),
   * or it has answered */
  private enum Cell[A]:
    case Running(waiters: List[Option[A] => Unit])
    case Done(v: A)
