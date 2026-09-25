package okay2.async

import scala.collection.mutable
import okay2._
import okay2.Free.{Return, Inject, Bind}

/**
 * ONE STORE FOR MANY FIBRES. `Once.run` threads its cells through its
 * own loop, which is what makes a program under it replayable — and
 * what makes a handle shared between fibres run TWICE: each fibre has
 * its own `Once.run`, so each has its own cells. This is the other
 * reading, the `memoize`/`Deferred` of the async libraries: one store
 * for every fibre that runs through it, and a demand met while the
 * program is in flight WAITS for its answer — an `Async` await,
 * resumed by the store — instead of running the program again or
 * throwing.
 *
 *     val store = new SharedOnce
 *     Async.par(store.run(p), store.run(p))   // p runs once; both get its answer
 *
 * What it gives up is what the threading bought: a program under a
 * shared store is not replayable (the cells are a mutable object
 * outside the tree), and a KNOT — a program demanding its own handle
 * while it runs — is a HANG here, not `Once.run`'s exception. Reach
 * for this only where the handle genuinely crosses fibres.
 *
 * Each `Once` operation is answered with a program in `Async + F`, so
 * a waiting demand suspends the fibre the way any await does; the walk
 * suspends every step under a flatMap, as `translate` does, and runs
 * at the answer type `Any` like every handler here, so the only cast
 * is the heterogeneous map's.
 */
final class SharedOnce {
  import SharedOnce.Cell

  private val cells = mutable.Map.empty[Once.Handle[_], Cell[_]]

  /** the one cast, isolated, as in the Scala 3 core: the cell under `h`
   * was made by `force[A](h: Handle[A])` or `store[A](h: Handle[A], a:
   * A)`, the only writers, and `h`'s type parameter IS that `A`. */
  private def cell[A](h: Once.Handle[A]): Option[Cell[A]] =
    cells.get(h).map(_.asInstanceOf[Cell[A]])

  /** None: this fibre runs the program (the cell is now Running).
   * Some: the answer, now or — through an await — when it arrives. */
  private def force[A](h: Once.Handle[A]): Option[A] ! Async =
    synchronized {
      cell(h) match {
        case None => cells(h) = Cell.Running[A](Nil); pure[Async, Option[A]](None)
        case Some(Cell.Done(v)) => pure[Async, Option[A]](Some(v))
        case Some(Cell.Running(_)) => await[Option[A]] { k =>
          // registered under the lock, against the store that may
          // have happened between the check above and this call
          val ready: Option[A] = synchronized {
            cell(h) match {
              case Some(Cell.Done(v)) => Some(v)
              case Some(Cell.Running(ws)) => cells(h) = Cell.Running(k :: ws); None
              case None => cells(h) = Cell.Running(List(k)); None
            }
          }
          ready.foreach(v => k(Some(v)))
        }
      }
    }

  /** fill the cell — the first store wins — and wake every waiter */
  private def store[A](h: Once.Handle[A], a: A): A ! Async = {
    val (v, waiters) = synchronized {
      cell(h) match {
        case Some(Cell.Done(v)) => (v, Nil)
        case Some(Cell.Running(ws)) => cells(h) = Cell.Done(a); (a, ws)
        case None => cells(h) = Cell.Done(a); (a, Nil)
      }
    }
    // outside the lock: a waiter resumes a fibre
    waiters.foreach(w => w(Some(v)))
    pure[Async, A](v)
  }

  /** one operation, answered — at `Any`, as the split hands it over */
  private def answer(o: Once.Op[Any]): Any ! Async = o match {
    // a type VARIABLE in the pattern, not a wildcard: `Store[_]` against
    // a covariant `Op[Any]` scrutinee is instantiated to `Store[Any]`
    // and its two fields then disagree with each other
    case f: Once.Force[a] => force[a](f.h)
    case s: Once.Store[a] => store[a](s.h, s.a)
  }

  /** the `Once` operations of `a` answered from THIS store, the rest
   * of the row forwarded */
  def runIn[A, F <: Row](a: A ! (Once + (Async + F))): A ! (Async + F) = {
    type Rw = Once + (Async + F)
    val Mine = okay2.Split.at[Once]
    def loop(x: Free[Rw, A]): A ! (Async + F) = Free.resume(x) match {
      case Return(v) => Return(v)
      case Inject(e) => loop(Bind(Inject[Rw, A](e), (v: A) => Return[Rw, A](v)))
      case Bind(Inject(Mine(o)), k) => answer(o).plus[F].flatMap(v => loop(k(v)))
      case Bind(Inject(g), k) => Inject[Async + F, Any](g).flatMap(v => loop(k(v)))
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }
    loop(a)
  }

  /** the common case: a program whose only other effect is `Async` */
  def run[A](a: Free[Once with Async, A]): A ! Async =
    runIn[A, Pure](a.at[Once + (Async + Pure)]).at[Async]
}

object SharedOnce {
  /** a cell: the program is in flight (with everyone waiting for it),
   * or it has answered */
  private sealed trait Cell[A]
  private object Cell {
    final case class Running[A](waiters: List[Option[A] => Unit]) extends Cell[A]
    final case class Done[A](v: A) extends Cell[A]
  }
}
