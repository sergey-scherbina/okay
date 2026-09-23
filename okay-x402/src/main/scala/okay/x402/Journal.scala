package okay.x402

import okay.*
import okay.chain.Network
import okay.codec.Schema
import okay.persist.{Ack, Topic, Typed, of}

/**
 * One decision about a payment, as a record (specs/x402.md stage 4). The
 * client side writes `asked`, `refused`, `reserved`, `returned` and `paid`
 * (with the transaction); the server side `claimed` and `released`.
 * `subject` says whose record it is — a budget's id, a payment's key — so
 * one journal can carry several budgets and a server's replay record
 * without their folds mixing. `ref` ties a `returned` to the reservation
 * it gives back.
 */
final case class PaymentEvent(at: Long, kind: String, subject: String,
                              network: Option[Network] = None, asset: Option[String] = None,
                              amount: Option[BigInt] = None, payTo: Option[String] = None,
                              resource: Option[String] = None, transaction: Option[String] = None,
                              ref: Option[String] = None, reason: Option[String] = None)
    derives Schema

object PaymentEvent:
  val Asked = "asked"
  val Refused = "refused"
  val Reserved = "reserved"
  val Returned = "returned"
  val Paid = "paid"
  val Claimed = "claimed"
  val Released = "released"

  /** an event about one chosen requirement */
  def about(at: Long, kind: String, subject: String, c: PaymentRequirements,
            resource: Option[ResourceInfo] = None): PaymentEvent =
    PaymentEvent(at, kind, subject, Some(c.network), Some(c.asset), Some(c.amount), Some(c.payTo),
      resource.map(_.url))

/**
 * Where payment decisions are written. The journal is the MEMORY of a
 * budget and of a server's replay record: both are rebuilt by folding it,
 * so what survives a restart is exactly what was written — `on(topic)` is
 * an okay-persist topic (a `FileStore` on disk), `inMemory` is for tests
 * and for a process that may forget.
 */
trait PaymentJournal:
  def record(e: PaymentEvent): Unit
  def events: Vector[PaymentEvent]

object PaymentJournal:
  def inMemory(): PaymentJournal = new PaymentJournal:
    private val es = scala.collection.mutable.ArrayBuffer.empty[PaymentEvent]
    def record(e: PaymentEvent): Unit = synchronized { es += e; () }
    def events: Vector[PaymentEvent] = synchronized(es.toVector)

  /**
   * Over an okay-persist topic, partition 0, each record `Ack.Durable` —
   * on the disk before the decision it records is acted on. A record that
   * does not decode is SKIPPED by the fold rather than taken as nothing
   * happened: an undecodable reservation would otherwise refill a budget,
   * so `damaged` counts them and a caller can refuse to start.
   */
  def on(topic: Topic): Durable = Durable(topic.of[PaymentEvent]())

  final class Durable(typed: Typed[PaymentEvent]) extends PaymentJournal:
    @volatile var damaged: Int = 0
    def record(e: PaymentEvent): Unit =
      typed.append(0, e.subject.getBytes("UTF-8"), e, Ack.Durable): Unit
    def events: Vector[PaymentEvent] =
      val t = typed.topic
      def go(from: Long, acc: Vector[PaymentEvent]): Vector[PaymentEvent] =
        if from >= t.end(0) then acc
        else typed.read(0, from, 1024) match
          case Typed.Read.TooEarly(b) => go(b, acc)
          case Typed.Read.Records(rs) if rs.isEmpty => acc
          case Typed.Read.Records(rs) =>
            val next = rs.last match
              case Typed.Decoded.Ok(o, _, _, _) => o + 1
              case Typed.Decoded.Bad(o, _) => o + 1
            val ok = rs.collect { case Typed.Decoded.Ok(_, _, _, e) => e }
            damaged += rs.size - ok.size
            go(next, acc ++ ok)
      go(t.begin(0), Vector.empty)

/**
 * A running total with a MEMORY (specs/x402.md stage 4): each approval
 * RESERVES its amount (two calls racing cannot both spend the last of
 * it), a payment that was not taken gives its reservation back, and the
 * state is the fold of the budget's own records in `journal` — so a
 * restart over a durable journal does not refill it. With a `window`
 * (milliseconds), only reservations made inside it count: a daily cap is
 * `window = Some(86_400_000)`.
 */
final class Budget(val id: String, total: BigInt, network: Network, asset: String,
                   window: Option[Long] = None,
                   journal: PaymentJournal = PaymentJournal.inMemory(),
                   clock: () => Long = () => System.currentTimeMillis()) extends Consent:

  // open reservations: ref -> (when, how much); rebuilt from the journal
  private val open = scala.collection.mutable.LinkedHashMap.empty[String, (Long, BigInt)]
  private var counter = 0L
  locally {
    journal.events.filter(_.subject == id).foreach { e =>
      e.kind match
        case PaymentEvent.Reserved =>
          for r <- e.ref; a <- e.amount do open(r) = (e.at, a)
          counter += 1
        case PaymentEvent.Returned => e.ref.foreach(open.remove)
        case _ => ()
    }
  }

  private def live(now: Long): Iterable[(Long, BigInt)] =
    window.fold(open.values)(w => open.values.filter(_._1 > now - w))

  def remaining: BigInt = synchronized(total - live(clock()).map(_._2).sum)

  private def mine(c: PaymentRequirements): Boolean =
    c.network == network && c.asset.equalsIgnoreCase(asset)

  def approve(c: PaymentRequirements, r: ResourceInfo): Boolean ! Async = okay.async {
    mine(c) && synchronized {
      val now = clock()
      if c.amount > total - live(now).map(_._2).sum then false
      else
        counter += 1
        val ref = s"$id#$counter"
        journal.record(PaymentEvent.about(now, PaymentEvent.Reserved, id, c, Some(r)).copy(ref = Some(ref)))
        open(ref) = (now, c.amount)
        true
    }
  }

  /** the most recent open reservation of this amount — equal amounts are
   * interchangeable, so which one does not change the total */
  override def returned(c: PaymentRequirements): Unit =
    if mine(c) then synchronized {
      open.toSeq.findLast(_._2._2 == c.amount).foreach { (ref, _) =>
        journal.record(PaymentEvent.about(clock(), PaymentEvent.Returned, id, c).copy(ref = Some(ref)))
        open.remove(ref)
      }
    }

  override def paid(c: PaymentRequirements, s: SettlementResponse): Unit =
    if mine(c) then
      journal.record(PaymentEvent.about(clock(), PaymentEvent.Paid, id, c).copy(transaction = Some(s.transaction)))
