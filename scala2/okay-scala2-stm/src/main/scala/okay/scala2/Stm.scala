package okay.scala2

import okay.{!, Foreign}
import okay.std.{TRef}
import okay.given
import Rows.{erase, typed}

/**
 * okay-stm for Scala 2.13 (specs/scala2-facade.md, stage 15.3).
 *
 * The cell is okay's own: `TRef(init)`, `ref.get` and `ref.modify(f)`
 * are plain methods and a Scala 2 caller uses them directly. What Scala
 * 2 cannot use is the transaction language — every operation answers
 * an `A ! Tx` — and the door that runs one. `Tx` is that language as a
 * capability of `Eff`, and `Stm.atomically` runs it as one atomic step
 * of an `Eff[Async, A]`, through the platform's own strategy (TL2 on
 * the JVM).
 *
 * A transaction's row is `Tx` alone, so `Async` inside one is a type
 * error, as it is in Scala 3: I/O cannot run twice when a conflict
 * re-runs the transaction.
 */
sealed trait Tx

object Tx {
  def read[A](r: TRef[A]): Eff[Tx, A] = Eff.of(erase(okay.Tx.read(r)))
  def write[A](r: TRef[A], a: A): Eff[Tx, Unit] = Eff.of(erase(okay.Tx.write(r, a)))
  def modify[A, B](r: TRef[A])(f: A => (A, B)): Eff[Tx, B] = Eff.of(erase(okay.Tx.modify(r)(f)))
  def update[A](r: TRef[A])(f: A => A): Eff[Tx, Unit] = Eff.of(erase(okay.Tx.update(r)(f)))

  /** block until something this transaction READ changes, then run it again */
  def retry[A]: Eff[Tx, A] = Eff.of(erase(okay.Tx.retry[A]))

  /** `retry` unless the condition holds */
  def check(cond: Boolean): Eff[Tx, Unit] = Eff.of(erase(okay.Tx.check(cond)))

  /** `a`, or `b` if `a` retries; `a`'s writes are discarded */
  def orElse[A](a: Eff[Tx, A], b: Eff[Tx, A]): Eff[Tx, A] =
    Eff.of(erase(okay.Tx.orElse(core(a), core(b))))

  /** which values are `okay.Tx`'s operations: by class, as `Tx` is one signature with only its answer type */
  private val member: Foreign.Member[okay.Tx] = Foreign.Member.of[okay.Tx](using okay.typeableK(classOf[okay.Tx[?]]))

  private[scala2] def core[A](e: Eff[Tx, A]): A ! okay.Tx = typed[okay.Tx, A](e.program)(using member)
}

object Stm {
  /** a new cell; the same as okay's `TRef(init)` */
  def ref[A](init: A): TRef[A] = TRef(init)

  /** run `tx` as one atomic step: every read consistent, every write
   * committed together or not at all */
  def atomically[A](tx: Eff[Tx, A]): Eff[Async, A] =
    Async.lift(okay.Stm.atomically[A, okay.Async](Tx.core(tx)))
}
