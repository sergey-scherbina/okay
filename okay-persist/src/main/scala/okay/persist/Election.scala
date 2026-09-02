package okay.persist

import okay.codec.Schema

/**
 * Elected leadership (specs/consensus.md): the operator removed
 * from the loop, the guarantees untouched. Leadership changes are
 * RECORDS of a totally-ordered control topic, and election is a
 * FOLD of it: the first `Take` at an epoch wins that epoch on
 * every node's fold — total order is the arbiter, so there are no
 * votes, no terms of our own, no new wire messages. The `Operator`
 * record outranks any automatic claim at its epoch: automation
 * must never lock a human out.
 *
 * Leases decide LIVENESS only — when a takeover may start (after
 * `until` plus the declared skew allowance). Safety stays where
 * stage 2 put it: epochs fence, the high-water mark bounds
 * visibility; if every clock lies at once, the worst outcome is a
 * fenced append and an ops event, not forked history.
 *
 * Where the control topic's total order comes from is an ENGINE
 * choice: KafkaStore (its KRaft did the twenty years), a FileStore
 * arbiter for dev (failover availability traded, never
 * correctness), an own RaftStore later — the machinery here does
 * not change, by construction.
 */
final class Election(control: Topic, val node: String,
                     leaseMillis: Long = 5000, skewMillis: Long = 1000,
                     clock: () => Long = () => System.currentTimeMillis()):
  import Election.*

  private val typed = Typed[Claim](control, version = 1, upcasts = Map.empty)
  private var consumed = control.begin(0)

  /** per data partition: the decided leadership and the last lease
   * of the deciding epoch */
  private var state = Map.empty[Int, Decided]

  /** fold forward: every node running this fold agrees, because
   * the log's order is the only input */
  def refresh(): Unit = synchronized {
    var going = true
    while going do
      typed.read(0, consumed, 256) match
        case Typed.Read.TooEarly(b) => consumed = b
        case Typed.Read.Records(rs) =>
          if rs.isEmpty then going = false
          else
            for d <- rs do d match
              case Typed.Decoded.Ok(off, _, _, claim) =>
                apply(claim)
                consumed = off + 1
              case Typed.Decoded.Bad(off, _) =>
                consumed = off + 1 // damage in the control log: skip, never guess
  }

  private def apply(c: Claim): Unit = c match
    case Claim.Take(p, e, n) =>
      state.get(p) match
        case Some(d) if e < d.epoch => ()                       // an old claim, lost to history
        case Some(d) if e == d.epoch => ()                      // the first at this epoch already won
        case _ => state = state.updated(p, Decided(e, n, operator = false, lease = None))
    case Claim.Operator(p, e, n) =>
      state.get(p) match
        case Some(d) if e < d.epoch => ()
        case Some(d) if e == d.epoch && d.operator => ()        // an operator already spoke
        case _ => state = state.updated(p, Decided(e, n, operator = true, lease = None))
    case Claim.Lease(p, e, n, until) =>
      state.get(p) match
        case Some(d) if d.epoch == e && d.node == n =>
          state = state.updated(p, d.copy(lease = Some(until)))
        case _ => ()                                            // a deposed leader's heartbeat: noise

  /** who leads this partition, per the fold */
  def leader(partition: Int): Option[(Long, String)] = synchronized {
    refresh()
    state.get(partition).map(d => (d.epoch, d.node))
  }

  /** true when a takeover MAY start: no leader yet, or the deciding
   * epoch's lease has expired past the skew allowance */
  def vacant(partition: Int): Boolean = synchronized {
    refresh()
    state.get(partition) match
      case None => true
      case Some(d) => d.node != node && d.lease.forall(u => clock() > u + skewMillis)
  }

  /** the leader's heartbeat: renew every partition this node holds */
  def heartbeat(): Unit = synchronized {
    refresh()
    for (p, d) <- state if d.node == node do
      typed.append(0, Array.empty, Claim.Lease(p, d.epoch, node, clock() + leaseMillis),
        Ack.Durable): Unit
      ()
    refresh()
  }

  /**
   * Claim the partition at the next epoch, when the fold says the
   * seat is vacant. The answer comes from the FOLD, not from the
   * append: the claim lands in total order, and this node reads
   * back whether it landed first — `Some(epoch)` says this node
   * won and should now drive stage 2's `promote` (the same code
   * path the operator uses); `None` says another claim was first.
   */
  def tryTakeover(partition: Int): Option[Long] = synchronized {
    refresh()
    if !vacant(partition) then None
    else
      val e = state.get(partition).map(_.epoch + 1).getOrElse(1L)
      typed.append(0, Array.empty, Claim.Take(partition, e, node), Ack.Durable): Unit
      refresh()
      state.get(partition) match
        case Some(d) if d.epoch == e && d.node == node && !d.operator =>
          // hold the seat immediately, so a racing second claimant
          // sees a live lease, not a vacancy
          typed.append(0, Array.empty, Claim.Lease(partition, e, node, clock() + leaseMillis),
            Ack.Durable): Unit
          refresh()
          Some(e)
        case Some(d) if d.epoch == e && d.node == node => Some(e) // the operator chose us
        case _ => None
  }

  /** the human's word: appended like any claim, outranks them all
   * at its epoch on every fold */
  def operatorAssign(partition: Int, chosen: String): Long = synchronized {
    refresh()
    val e = state.get(partition).map(_.epoch + 1).getOrElse(1L)
    typed.append(0, Array.empty, Claim.Operator(partition, e, chosen), Ack.Durable): Unit
    refresh()
    e
  }

object Election:

  /** leadership changes ARE records; the control topic's total
   * order is the whole election protocol */
  enum Claim derives Schema:
    case Take(partition: Int, epoch: Long, node: String)
    case Lease(partition: Int, epoch: Long, node: String, untilMillis: Long)
    case Operator(partition: Int, epoch: Long, node: String)

  private final case class Decided(epoch: Long, node: String,
                                   operator: Boolean, lease: Option[Long])
