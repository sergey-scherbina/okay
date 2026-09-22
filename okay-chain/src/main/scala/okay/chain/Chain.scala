package okay.chain

import okay.codec.Schema

/** a position on a chain: its height and the block there */
final case class Point(height: Long, id: BlockId) derives Schema

/** what the follower needs of a block: where it is, what it extends,
 * and when (epoch millis), when the chain says */
final case class BlockRef(point: Point, parent: BlockId, time: Option[Long]) derives Schema

/** where the source's chain is: its newest block, and the block the
 * chain itself declares final, on a chain that declares one. A POINT,
 * not a height: the source's chain may not be the one followed so far
 * (a reorg not yet seen), and a height alone would confirm whatever
 * block the follower holds there */
final case class Tip(point: Point, finalized: Option[Point] = None) derives Schema

/** when a block counts as final (specs/chain.md §2) */
enum Finality derives Schema:
  /** probabilistic: `blocks` on top of it ON THE CHAIN FOLLOWED
   * (Bitcoin, Cardano) — counted over linked blocks, never off the
   * source's head, which may sit on a fork not yet seen */
  case Depth(blocks: Int)
  /** the chain says so: Ethereum's `finalized`, Solana's commitment,
   * Tron's solidified blocks — `Tip.finalized` */
  case Finalized

/** the native block `B` as the follower sees it; `Tx` is the native
 * transaction a `Ledger` projects */
trait BlockOf[B]:
  type Tx
  def ref(b: B): BlockRef
  def txs(b: B): Vector[Tx]

object BlockOf:
  type Aux[B, T] = BlockOf[B] { type Tx = T }

/** what a SOURCE saw (specs/chain.md §3) — a push source produces these
 * directly, a poll source through `Poller` */
enum Observed[+B]:
  /** the next block on the chain the source follows */
  case Forward(block: B)
  /** the source's chain switched: everything after `to` is void, and
   * `to` is on the chain already followed */
  case Backward(to: Point)
  /** progress with nothing new: here is where the chain is */
  case AtTip(tip: Tip)

/** what the FOLLOWER says */
enum Event[+B]:
  /** final under the policy; emitted once, in chain order */
  case Confirmed(block: B)
  /** blocks after `to`, up to and including `from`, were Confirmed and
   * are void. okay-watch's `Rewound(from, to)` is
   * `(from.height, to.height + 1)`: it names the height to re-emit
   * from, this names the last block still standing */
  case RolledBack(to: Point, from: Point)

/** the chain did something a follower must not paper over */
final case class Broken(reason: String)

/**
 * The follower, sans-I/O (specs/chain.md §3): it holds the blocks not
 * yet final, and turns what a source observed into what a consumer may
 * act on. It performs no I/O and has no clock, so every rollback case
 * is a deterministic test, and one machine serves a blocking driver
 * (okay-watch), a Spark driver and an okay stream alike.
 *
 * - a `Forward` block must extend the previous one (its parent is the
 *   last pending block, or the confirmed frontier): anything else is
 *   `Broken`, never a silent hole;
 * - a `Backward` to a pending block (or the frontier) is ABSORBED —
 *   nothing it voids was ever said;
 * - a `Backward` below the frontier voids confirmed blocks: that is
 *   `RolledBack`, and the frontier moves back to `to`.
 */
final case class Tracker[B] private (
    finality: Finality,
    frontier: Option[Point],
    pending: Vector[B],
    tip: Option[Tip])(using val block: BlockOf[B]):

  /** the next height a consumer has not seen confirmed */
  def nextHeight: Option[Long] = frontier.map(_.height + 1)

  def feed(o: Observed[B]): Either[Broken, (Tracker[B], Vector[Event[B]])] = o match
    case Observed.Forward(b) =>
      val r = block.ref(b)
      val expected = pending.lastOption.map(p => block.ref(p).point).orElse(frontier)
      expected match
        case Some(e) if e.id != r.parent =>
          Left(Broken(s"block ${r.point.height} ${r.point.id.value} extends ${r.parent.value}, " +
            s"but the chain followed ends at ${e.height} ${e.id.value}"))
        case Some(e) if r.point.height <= e.height =>
          Left(Broken(s"block ${r.point.height} does not advance past ${e.height}"))
        case _ =>
          Right(copy(pending = pending :+ b, tip = higher(tip, Tip(r.point))).confirm)

    case Observed.Backward(to) =>
      val at = pending.indexWhere(p => block.ref(p).point == to)
      if at >= 0 then Right((copy(pending = pending.take(at + 1)), Vector.empty))
      else frontier match
        case Some(f) if f == to => Right((copy(pending = Vector.empty), Vector.empty))
        case Some(f) if to.height < f.height =>
          Right((copy(frontier = Some(to), pending = Vector.empty), Vector(Event.RolledBack(to, f))))
        case None => Right((copy(pending = Vector.empty), Vector.empty))
        case Some(f) =>
          Left(Broken(s"rolled back to ${to.height} ${to.id.value}, which is not on the chain followed " +
            s"(confirmed to ${f.height} ${f.id.value})"))

    case Observed.AtTip(t) => Right(copy(tip = higher(tip, t)).confirm)

  /** a later tip wins; a finalized height only ever rises */
  private def higher(old: Option[Tip], t: Tip): Option[Tip] = old match
    case None => Some(t)
    case Some(o) =>
      val point = if t.point.height >= o.point.height then t.point else o.point
      Some(Tip(point, (o.finalized ++ t.finalized).maxByOption(_.height)))

  /**
   * FOUND BY TestWatchShape (2026-09-23): the first cut counted depth
   * off the source's head. After a reorg the head is on the NEW fork
   * while the pending blocks are still the old one's, so a dead block
   * (a8) was Confirmed and RolledBack in the same step — where
   * okay-watch, which fetches only what it is about to emit, said
   * nothing. Depth counts linked blocks; `Finalized` confirms only the
   * very block the chain declared final, when the follower holds it.
   */
  private def confirm: (Tracker[B], Vector[Event[B]]) =
    val count: Int = finality match
      case Finality.Depth(n) =>
        pending.lastOption.fold(0) { last =>
          val limit = block.ref(last).point.height - n
          pending.indexWhere(b => block.ref(b).point.height > limit) match
            case -1 => pending.size
            case i => i
        }
      case Finality.Finalized =>
        tip.flatMap(_.finalized).fold(0)(f => pending.indexWhere(b => block.ref(b).point == f) + 1)
    val (done, rest) = pending.splitAt(count)
    if done.isEmpty then (this, Vector.empty)
    else (copy(frontier = Some(block.ref(done.last).point), pending = rest), done.map(Event.Confirmed(_)))

object Tracker:
  /** a follower starting after `from` — the last point a consumer has
   * already seen (its checkpoint), or none to take the first block the
   * source offers */
  def apply[B](finality: Finality, from: Option[Point] = None)(using BlockOf[B]): Tracker[B] =
    new Tracker[B](finality, from, Vector.empty, None)
