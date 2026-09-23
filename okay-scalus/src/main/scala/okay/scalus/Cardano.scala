package okay.scalus

import okay.chain.*
import scalus.cardano.ledger.SlotConfig

/** a Cardano network: its CAIP-2 id, its handshake magic, its slots */
final case class CardanoNetwork(network: Network, magic: Long, slots: Option[SlotConfig])

object CardanoNetwork:
  val mainnet = CardanoNetwork(Network.cardano, 764824073L, Some(SlotConfig.mainnet))
  val preprod = CardanoNetwork(Network.cardanoPreprod, 1L, Some(SlotConfig.preprod))
  val preview = CardanoNetwork(Network.cardanoPreview, 2L, None)

/** a point a consumer checkpointed: chain-sync needs the slot, the
 * follower the block number, and both the hash */
final case class Checkpoint(slot: Long, hash: String, blockNo: Long):
  def pt: N2N.Pt = N2N.Pt(slot, Header.unhex(hash))
  def point: Point = Point(blockNo, BlockId(hash))

/**
 * Cardano as an okay-chain PUSH source (specs/chain.md §3): chain-sync
 * announces headers, block-fetch brings their bodies in BATCHES — the
 * headers read until the relay says "wait" (or `batch` of them), then
 * one range request — and the result is `Observed` for a `Tracker`.
 *
 * Heights: chain-sync's rollback names a (slot, hash) point, the
 * follower counts blocks. The height of a point is known for every
 * header seen (a bounded map) and for the checkpoint; the relay's
 * opening rollback to the intersection, before anything was observed,
 * voids nothing and is dropped.
 */
final class ChainSyncSource(session: Session, net: CardanoNetwork,
                            from: Option[Checkpoint], batch: Int = 100, remember: Int = 4096):
  private val heights = scala.collection.mutable.LinkedHashMap.empty[String, Long]
  private var outstanding = false
  private var observed = false
  from.foreach(c => heights(c.hash) = c.blockNo)

  private def know(h: Header): Unit =
    heights(h.hash) = h.blockNo
    if heights.size > remember then heights.remove(heights.head._1): Unit

  private def okayTip(t: N2N.Tip): Tip =
    Tip(Point(t.blockNo, BlockId(t.point.fold("")(_.hex))))

  /**
   * Find where to start: at the checkpoint, or — with none — at the
   * relay's current TIP. Not the origin: reading in real time is the
   * point, and a replay from genesis would begin with Byron, which is
   * not modelled. With no checkpoint the tip is found by asking for an
   * intersection with nothing (the answer names the tip) and then
   * intersecting there.
   */
  def open(): Either[String, N2N.Tip] =
    def intersect(p: N2N.Pt): Either[String, N2N.Tip] =
      session.send(N2N.ChainSync, N2N.findIntersect(Seq(Some(p))))
      session.receive(N2N.ChainSync).flatMap(N2N.sync).flatMap {
        case N2N.Sync.IntersectFound(_, tip) => Right(tip)
        case N2N.Sync.IntersectNotFound(_) =>
          Left(s"the relay's chain does not contain ${p.slot} ${p.hex}")
        case other => Left(s"expected an intersection, got $other")
      }
    from match
      case Some(c) => intersect(c.pt)
      case None =>
        session.send(N2N.ChainSync, N2N.findIntersect(Seq.empty))
        session.receive(N2N.ChainSync).flatMap(N2N.sync).flatMap {
          case N2N.Sync.IntersectNotFound(N2N.Tip(Some(tp), no)) =>
            heights(tp.hex) = no
            intersect(tp)
          case other => Left(s"expected the relay's tip, got $other")
        }

  /** the next observations: a batch of blocks, a rollback, or the tip */
  def next(): Either[String, Vector[Observed[CardanoBlock]]] =
    val headers = Vector.newBuilder[Header]
    var n = 0
    var result: Option[Either[String, Vector[Observed[CardanoBlock]]]] = None
    while result.isEmpty do
      if !outstanding then { session.send(N2N.ChainSync, N2N.requestNext); outstanding = true }
      session.receive(N2N.ChainSync).flatMap(N2N.sync) match
        case Left(e) => result = Some(Left(e))
        case Right(N2N.Sync.RollForward(era, bytes, tip)) =>
          outstanding = false
          lastTip = Some(okayTip(tip))
          Header.parse(era, bytes) match
            case Left(e) => result = Some(Left(e))
            case Right(h) =>
              know(h); headers += h; n += 1
              if n >= batch then result = Some(bodies(headers.result()))
        case Right(N2N.Sync.AwaitReply) =>
          // the answer to this request comes later: do not ask again
          val hs = headers.result()
          result = Some((if hs.isEmpty then Right(Vector.empty) else bodies(hs))
            .map(_ ++ lastTip.map(Observed.AtTip(_))))
        case Right(N2N.Sync.RollBackward(to, tip)) =>
          outstanding = false
          lastTip = Some(okayTip(tip))
          val hs = headers.result()
          val back: Either[String, Vector[Observed[CardanoBlock]]] = to match
            case None => Left("the relay rolled back to the origin")
            case Some(p) => heights.get(p.hex) match
              case Some(h) => Right(Vector(Observed.Backward(Point(h, BlockId(p.hex)))))
              case None if !observed && hs.isEmpty => Right(Vector.empty)   // the opening rollback
              case None => Left(s"rolled back to ${p.slot} ${p.hex}, a point never seen")
          result = Some(for f <- (if hs.isEmpty then Right(Vector.empty) else bodies(hs)); b <- back yield f ++ b)
        case Right(other) => result = Some(Left(s"unexpected in chain-sync: $other"))
    result.get.map { os =>
      if os.exists { case Observed.Forward(_) => true; case _ => false } then observed = true
      os
    }

  private var lastTip: Option[Tip] = None

  /** one range request for the headers, in order; each body is paired
   * with the header chain-sync announced for it */
  private def bodies(hs: Vector[Header]): Either[String, Vector[Observed[CardanoBlock]]] =
    session.send(N2N.BlockFetch, N2N.requestRange(hs.head.point, hs.last.point))
    def time(h: Header) = net.slots.map(_.slotToTime(h.slot))
    session.receive(N2N.BlockFetch).flatMap(N2N.fetch).flatMap {
      case N2N.Fetch.NoBlocks => Left(s"the relay no longer has blocks ${hs.head.blockNo}..${hs.last.blockNo} (rolled back meanwhile)")
      case N2N.Fetch.StartBatch =>
        val out = Vector.newBuilder[Observed[CardanoBlock]]
        var i = 0
        var err: Option[String] = None
        var done = false
        while !done && err.isEmpty do
          session.receive(N2N.BlockFetch).flatMap(N2N.fetch) match
            case Right(N2N.Fetch.Block(b)) if i < hs.size =>
              out += Observed.Forward(CardanoBlock(hs(i), b, time(hs(i)))); i += 1
            case Right(N2N.Fetch.BatchDone) => done = true
            case Right(other) => err = Some(s"unexpected in block-fetch: $other")
            case Left(e) => err = Some(e)
        err.toLeft(()).flatMap(_ =>
          if i == hs.size then Right(out.result())
          else Left(s"asked for ${hs.size} blocks, received $i"))
      case other => Left(s"expected a batch, got $other")
    }

/**
 * A relay followed to confirmed blocks: the session, the chain-sync
 * source and okay-chain's `Tracker` in one place. `step` blocks until
 * the relay has something to say (keep-alives go out meanwhile) and
 * answers what a consumer may act on.
 */
final class CardanoFollower private (session: Session, source: ChainSyncSource, private var tracker: Tracker[CardanoBlock]):
  def step(): Either[String, Vector[Event[CardanoBlock]]] =
    source.next().flatMap { os =>
      os.foldLeft[Either[String, Vector[Event[CardanoBlock]]]](Right(Vector.empty)) { (acc, o) =>
        acc.flatMap(es => tracker.feed(o).left.map(_.reason).map { (t, more) => tracker = t; es ++ more })
      }
    }
  def close(): Unit = session.close()

object CardanoFollower:
  /** connect to `relay`, handshake for `net`, start at `from` (or the
   * tip), confirm under `finality` */
  def open(wire: Wire, net: CardanoNetwork, from: Option[Checkpoint] = None,
           finality: Finality = Finality.Depth(15)): Either[String, CardanoFollower] =
    for
      session <- Session.open(wire, net.magic)
      source = ChainSyncSource(session, net, from)
      tip <- source.open()
    yield
      val start = from.map(_.point).orElse(tip.point.map(p => Point(tip.blockNo, BlockId(p.hex))))
      new CardanoFollower(session, source, Tracker[CardanoBlock](finality, start))
