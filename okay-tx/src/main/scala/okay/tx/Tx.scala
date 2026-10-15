package okay.tx

import okay.one.*
import okay.one.Cont.{failed, perform, pure}
import okay.one.handler.*
import okay.one.row.*
import okay.one.std.{Reader, %}

/** A TRANSACTION IS A SUBTREE of the program: `Atomic` a bracket that holds its body — so brackets cannot mismatch,
 * the closing one is the return from the node, there is no commit to forget; `Rollback` a leaf, of no value: nothing
 * after it types. Inside a bracket the body has the transaction's effect again (brackets inside brackets) and the
 * RESOURCE `C` — a connection — to read (`Reader % C`); `T` is the row outside, the resource's owner's.
 *
 * The owner (`Transactor`) says how a transaction begins, commits and rolls back — programs over `T` — and the handler
 * walks the tree with one bit, open or closed: the first bracket met closed opens (`begin`), its subtree under the
 * handler `open`, where every inner bracket is nothing but its body; the return from the first bracket commits; a
 * `Rollback`, or any failure, anywhere inside rolls back — the rest of the subtree dropped, the finalizers between
 * run — and goes on outward as a failure (`Rolled` for a `Rollback`). One begin, one commit, however deep. The
 * subtree is a value: a failure the owner says to retry on (a deadlock) rolls back and runs it again */
enum Tx[C, T <: Row, X]:
  case Atomic[C, T <: Row, A](body: A ! (Txs[C, T] +: Reader % C +: T)) extends Tx[C, T, A]
  case Rollback[C, T <: Row](reason: Throwable) extends Tx[C, T, Nothing]

/** the transaction effect of the resource `C`, its owner over `T` */
type Txs[C, T <: Row] = [x] =>> Tx[C, T, x]

object Tx:
  /** a bracket: `body` atomic — in the transaction open, or in one opened for it */
  def atomic[C, T <: Row, A, R <: Row](body: A ! (Txs[C, T] +: Reader % C +: T))(using In[Txs[C, T], R]): A ! R =
    perform[Txs[C, T], R, A](Atomic(body))
  /** the transaction rolled back for `reason`: a leaf — the rest of the bracket never runs; outside, `Rolled(reason)` */
  def rollback[C, T <: Row, R <: Row](reason: Throwable)(using In[Txs[C, T], R]): Nothing ! R =
    perform[Txs[C, T], R, Nothing](Rollback(reason))
  /** the resource inside a bracket */
  def resource[C, R <: Row](using In[Reader % C, R]): C ! R = Reader.ask[C, R]

  /** a `Rollback` as it leaves its bracket: a failure, never retried */
  final class Rolled(val reason: Throwable) extends Exception(reason.getMessage, reason)

  /** THE RESOURCE'S OWNER: a transaction's begin, commit and rollback as programs over `T`, the row outside */
  trait Transactor[C, T <: Row]:
    def begin: C ! T
    def commit(c: C): Unit ! T
    def rollback(c: C): Unit ! T
    /** a failure to run the bracket again after (a deadlock, a serialization failure), `attempt` from 1: none by
     * default */
    def retry(e: Throwable, attempt: Int): Boolean = false

  /** THE HANDLER, CLOSED: no transaction open. A bracket opens one and runs its subtree under `open`, the rest `k`
   * after the commit; a failure of the bracket is `k`'s, raised where the bracket was — a `recover` around it sees
   * it; a `Rollback` with none open the same */
  def closed[C, T <: Row, A](tr: Transactor[C, T]): Deep[Txs[C, T], T, A, A] = new Deep[Txs[C, T], T, A, A]:
    def ret(a: A): A ! T = pure(a)
    def apply[X](op: Tx[C, T, X], k: X => A ! T): A ! T = op match
      case at: Atomic[c, ?, ?] => bracket(tr, at.body, 1, k)
      case Rollback(reason) => k.failed(Rolled(reason))

  /** THE HANDLER, OPEN: inside a bracket. An inner bracket is its body, nothing more; a `Rollback` is a failure
   * raised where it was — through the levels inside the first bracket, to its guard */
  def open[C, T <: Row, A]: Deep[Txs[C, T], Reader % C +: T, A, A] = new Deep[Txs[C, T], Reader % C +: T, A, A]:
    def ret(a: A): A ! (Reader % C +: T) = pure(a)
    def apply[X](op: Tx[C, T, X], k: X => A ! (Reader % C +: T)): A ! (Reader % C +: T) = op match
      case at: Atomic[c, ?, a] => at.body.handle(open[C, T, a]).flatMap(k)
      case Rollback(reason) => k.failed(Rolled(reason))

  /** the first bracket: begin; the subtree, open, the resource read inside; its value commits, then the rest; a
   * failure rolls back and — unless the owner says to run the subtree again — fails the rest; an abort or a cancel
   * crossing rolls back */
  private def bracket[C, T <: Row, A, B](tr: Transactor[C, T], body: A ! (Txs[C, T] +: Reader % C +: T), attempt: Int,
      k: A => B ! T): B ! T =
    tr.begin.flatMap { c =>
      body.handle(open[C, T, A]).handle(Reader[C, T, A](c)).guard(new Recovering[T, A, B]:
        def ret(a: A): B ! T = tr.commit(c).flatMap(_ => k(a))
        def recover(e: Throwable): B ! T = tr.rollback(c).flatMap { _ =>
          if !e.isInstanceOf[Rolled] && tr.retry(e, attempt) then bracket(tr, body, attempt + 1, k) else k.failed(e) }
        override def finalizer: Unit ! T = tr.rollback(c))
    }

// -- the facade's words: a bracket an operation, the transactions a handler around a body

import okay.core.{Eff, Op, Place, Under, handleIn, perform as performOp}

/** `atomic(body)`: a bracket — its body under the transaction's effect again and the resource to read (`ask[C]`) */
def atomic[C, T <: Row, A](body: Under[Txs[C, T], Reader % C +: T, A]): Op[Txs[C, T], A] =
  performOp(Tx.Atomic(body(using Place.head)))
/** the transaction rolled back: a leaf, nothing after it; outside the bracket, `Tx.Rolled(reason)` */
def rollback[C, T <: Row](reason: Throwable): Op[Txs[C, T], Nothing] = performOp(Tx.Rollback(reason))
/** `transactions(owner)(body)`: the brackets of the body handled by the resource's owner */
def transactions[C, A, T <: Row](owner: Tx.Transactor[C, T])(body: Under[Txs[C, T], T, A]): Eff[T, A] =
  handleIn(Tx.closed[C, T, A](owner))(body)
