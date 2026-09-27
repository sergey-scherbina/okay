package okay.diagnose

/**
 * A VALUE THAT CAN SAY WHAT STATE IT IS IN (specs/okay-diagnose.md). A
 * channel's flags and counts, a pool's attempts, a scheduler's workers:
 * the text a failure should carry. The first instance was hand-written as
 * `SentinelChannel.debugState` for sentinel-single-consumer-lost-end.
 * Components give an instance, and a diagnosis takes a snapshot of any of
 * them with `d.snapshot(x)` and no string of its own.
 */
trait Diagnosable[-A]:
  def describe(a: A): String

object Diagnosable:
  def describe[A](a: A)(using d: Diagnosable[A]): String = d.describe(a)

  /** an instance from a function */
  def of[A](f: A => String): Diagnosable[A] = new Diagnosable[A]:
    def describe(a: A): String = f(a)

extension (d: Diagnostics)
  /** register `a`'s state as a snapshot, taken only if the run fails */
  def snapshot[A](a: A)(using D: Diagnosable[A]): Unit = d.onFailure(D.describe(a))
