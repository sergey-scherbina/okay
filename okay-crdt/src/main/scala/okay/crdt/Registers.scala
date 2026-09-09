package okay.crdt

import okay.Hlc

/**
 * LAST WRITE WINS, and the honesty about what "last" can mean.
 *
 * This is where the arc's shared primitive does its second job: the
 * stamp is an `Hlc.Stamp`, the same hybrid logical clock `Uid` uses.
 * A wall clock alone cannot order two writes from two machines — one
 * of them is wrong, and NTP will step it while you look — so a
 * register stamped with `System.currentTimeMillis` loses writes
 * arbitrarily and calls it a policy.
 *
 * With an HLC, a write that CAUSALLY follows another (the writer had
 * seen it, and called `observe`) always wins. That is the whole
 * guarantee, and it is smaller than it sounds: two writes that never
 * saw each other are CONCURRENT, and no clock can order them. For
 * those the winner is decided by the node id — arbitrary, but the
 * same arbitrary choice on every replica, which is what convergence
 * needs. A tie broken differently on two nodes is not a tie, it is a
 * permanent disagreement.
 *
 * So: `merge` prefers the higher stamp, then the higher node id, and
 * `value` is whatever that write said. WHAT IS LOST is a real write —
 * that is what LWW means, and a program that cannot afford it wants
 * `OrSet`, or a counter, or its own type with a merge that keeps
 * both.
 *
 * THE PRECONDITION, and it is load-bearing: `(at, by)` must identify
 * a write uniquely. It does when each node stamps from its OWN
 * `Hlc.Clock`, because a clock never issues one stamp twice — that is
 * the guarantee `Hlc` exists for. Hand-build two registers with the
 * same stamp, the same node and DIFFERENT values and the merge stops
 * being commutative, because there is nothing left to decide with.
 * That is not a defect to patch with a hash of the value (hashes
 * differ across platforms, and a merge that disagrees between JVM and
 * JS is worse than one that refuses); it is a precondition, stated
 * here and respected by the law check.
 */
final case class LwwRegister[A](value: A, at: Hlc.Stamp, by: NodeId)

object LwwRegister:

  /** a write, stamped from `clock` — take the clock as a parameter so
   * a test can order two writes without waiting a millisecond */
  def write[A](value: A, clock: Hlc.Clock, by: NodeId): LwwRegister[A] =
    LwwRegister(value, clock.next(), by)

  /** a write that has SEEN another: `observe` lifts this clock above
   * the stamp it was told about, so the new write wins the merge */
  def after[A](value: A, seen: LwwRegister[?], clock: Hlc.Clock, by: NodeId): LwwRegister[A] =
    LwwRegister(value, clock.observe(seen.at), by)

  given [A]: Crdt[LwwRegister[A]] with
    def merge(x: LwwRegister[A], y: LwwRegister[A]): LwwRegister[A] =
      val sx = x.at.toLong
      val sy = y.at.toLong
      if sx > sy then x
      else if sy > sx then y
      // the same instant on two nodes: the node id decides, the SAME
      // way on every replica, because a tie broken differently is not
      // a tie but a lasting disagreement
      else if x.by.name >= y.by.name then x
      else y
