package okay2.persist

import okay2.codec.Schema

/**
 * Elected leadership (okay-persist's Election.scala; specs/consensus.md):
 * leadership changes are RECORDS of a totally-ordered control topic, and
 * election is a FOLD of it — the first `Take` at an epoch wins that epoch
 * on every node's fold, so there are no votes and no new wire messages.
 * The `Operator` record outranks any automatic claim at its epoch.
 *
 * Leases decide LIVENESS only — when a takeover may start (after `until`
 * plus the declared skew). Safety stays where stage 2 put it: epochs
 * fence, the high-water mark bounds visibility.
 */
final class Election(control: Topic, val node: String,
                     leaseMillis: Long = 5000, skewMillis: Long = 1000,
                     clock: () => Long = () => System.currentTimeMillis()) {
  import Election._

  private val typed = Typed[Claim](control, version = 1, upcasts = Map.empty)
  private var consumed = control.begin(0)

  /** per data partition: the decided leadership and the last lease of
   * the deciding epoch */
  private var state = Map.empty[Int, Decided]

  /** fold forward: every node running this fold agrees, because the
   * log's order is the only input */
  def refresh(): Unit = synchronized {
    var going = true
    while (going) {
      typed.read(0, consumed, 256) match {
        case Typed.Read.TooEarly(b) => consumed = b
        case Typed.Read.Records(rs) =>
          if (rs.isEmpty) going = false
          else
            rs.foreach {
              case Typed.Decoded.Ok(off, _, _, claim) =>
                applyClaim(claim)
                consumed = off + 1
              case Typed.Decoded.Bad(off, _) =>
                consumed = off + 1 // damage in the control log: skip, never guess
            }
      }
    }
  }

  private def applyClaim(c: Claim): Unit = c match {
    case Claim.Take(p, e, n) =>
      state.get(p) match {
        case Some(d) if e < d.epoch => ()                       // an old claim, lost to history
        case Some(d) if e == d.epoch => ()                      // the first at this epoch already won
        case _ => state = state.updated(p, Decided(e, n, operator = false, lease = None))
      }
    case Claim.Operator(p, e, n) =>
      state.get(p) match {
        case Some(d) if e < d.epoch => ()
        case Some(d) if e == d.epoch && d.operator => ()        // an operator already spoke
        case _ => state = state.updated(p, Decided(e, n, operator = true, lease = None))
      }
    case Claim.Lease(p, e, n, until) =>
      state.get(p) match {
        case Some(d) if d.epoch == e && d.node == n => state = state.updated(p, d.copy(lease = Some(until)))
        case _ => ()                                            // a deposed leader's heartbeat: noise
      }
  }

  private def claim(c: Claim): Unit = {
    val _ = typed.append(0, Array.empty[Byte], c, Ack.Durable)
  }

  /** who leads this partition, per the fold */
  def leader(partition: Int): Option[(Long, String)] = synchronized {
    refresh()
    state.get(partition).map(d => (d.epoch, d.node))
  }

  /** true when a takeover MAY start: no leader yet, or the deciding
   * epoch's lease has expired past the skew allowance */
  def vacant(partition: Int): Boolean = synchronized {
    refresh()
    state.get(partition) match {
      case None => true
      case Some(d) => d.node != node && d.lease.forall(u => clock() > u + skewMillis)
    }
  }

  /** the leader's heartbeat: renew every partition this node holds */
  def heartbeat(): Unit = synchronized {
    refresh()
    for ((p, d) <- state if d.node == node) claim(Claim.Lease(p, d.epoch, node, clock() + leaseMillis))
    refresh()
  }

  /**
   * Claim the partition at the next epoch, when the fold says the seat is
   * vacant. The answer comes from the FOLD, not from the append:
   * `Some(epoch)` says this node won and should now drive stage 2's
   * `promote`; `None` says another claim was first.
   */
  def tryTakeover(partition: Int): Option[Long] = synchronized {
    refresh()
    if (!vacant(partition)) None
    else {
      val e = state.get(partition).map(_.epoch + 1).getOrElse(1L)
      claim(Claim.Take(partition, e, node))
      refresh()
      state.get(partition) match {
        case Some(d) if d.epoch == e && d.node == node && !d.operator =>
          // hold the seat immediately, so a racing second claimant sees a
          // live lease, not a vacancy
          claim(Claim.Lease(partition, e, node, clock() + leaseMillis))
          refresh()
          Some(e)
        case Some(d) if d.epoch == e && d.node == node => Some(e) // the operator chose us
        case _ => None
      }
    }
  }

  /** the human's word: appended like any claim, outranks them all at its
   * epoch on every fold */
  def operatorAssign(partition: Int, chosen: String): Long = synchronized {
    refresh()
    val e = state.get(partition).map(_.epoch + 1).getOrElse(1L)
    claim(Claim.Operator(partition, e, chosen))
    refresh()
    e
  }
}

object Election {

  /** leadership changes ARE records; the control topic's total order is
   * the whole election protocol */
  sealed trait Claim
  object Claim {
    final case class Take(partition: Int, epoch: Long, node: String) extends Claim
    final case class Lease(partition: Int, epoch: Long, node: String, untilMillis: Long) extends Claim
    final case class Operator(partition: Int, epoch: Long, node: String) extends Claim
    implicit lazy val schema: Schema[Claim] = Schema.derived
  }

  private final case class Decided(epoch: Long, node: String, operator: Boolean, lease: Option[Long])
}
