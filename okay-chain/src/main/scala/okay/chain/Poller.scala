package okay.chain

/** what a poll source answered for one height */
enum Polled[+B]:
  case Found(block: B)
  /** not there yet (a chain's newest height may be unindexed): ask later */
  case Missing
  /** a height the chain skipped (a Solana slot with no block): nothing
   * to link and nothing to confirm */
  case Gap

/** what the poller wants next */
enum Request:
  case Head
  case Block(height: Long)
  /** a bounded run reached its end */
  case Done

/**
 * A POLL source (`head`, `block(n)`) turned into `Observed`, sans-I/O
 * (specs/chain.md §3): it says what it wants, is told the answer, and
 * emits observations. It fetches up to the head and leaves finality to
 * the `Tracker`.
 *
 * A switched chain shows as a block whose parent is not the last block
 * taken; the poller then REWINDS — asks for each kept block again,
 * newest first, until one still matches — and emits `Backward` to it.
 * That is okay-watch's `Follower.rewind`, with one difference stated:
 * a rewind past every kept block is `Broken` here, where okay-watch
 * restarted from its first height.
 */
final case class Poller[B] private (
    next: Long,
    ring: Vector[Point],
    keep: Int,
    head: Option[Tip],
    waiting: Boolean,
    rewinding: Boolean,
    until: Option[Long])(using block: BlockOf[B]):

  def want: Request =
    if rewinding then Request.Block(ring.last.height)
    else if until.exists(next > _) then Request.Done
    else head match
      case Some(t) if !waiting && next <= t.point.height => Request.Block(next)
      case _ => Request.Head

  def onHead(t: Tip): (Poller[B], Vector[Observed[B]]) =
    (copy(head = Some(t), waiting = false), Vector(Observed.AtTip(t)))

  def onBlock(height: Long, p: Polled[B]): Either[Broken, (Poller[B], Vector[Observed[B]])] =
    if rewinding then rewound(height, p)
    else p match
      case Polled.Missing => Right((copy(waiting = true), Vector.empty))
      case Polled.Gap => Right((copy(next = height + 1), Vector.empty))
      case Polled.Found(b) =>
        val r = block.ref(b)
        ring.lastOption match
          case Some(last) if last.id != r.parent => Right((copy(rewinding = true), Vector.empty))
          case _ =>
            val kept = (ring :+ r.point).takeRight(keep)
            Right((copy(next = r.point.height + 1, ring = kept), Vector(Observed.Forward(b))))

  /** one step of the walk back: the kept block at `height` still there? */
  private def rewound(height: Long, p: Polled[B]): Either[Broken, (Poller[B], Vector[Observed[B]])] =
    val still = p match
      case Polled.Found(b) => block.ref(b).point == ring.last
      case _ => false
    if still then
      Right((copy(next = height + 1, rewinding = false), Vector(Observed.Backward(ring.last))))
    else
      val rest = ring.init
      if rest.isEmpty then
        Left(Broken(s"the chain was replaced below every one of the $keep blocks kept (down to $height)"))
      else Right((copy(ring = rest), Vector.empty))

object Poller:
  /** start polling at height `from`; `keep` bounds how deep a reorg
   * can be recognised; `until` bounds a replay */
  def apply[B](from: Long, keep: Int = 64, until: Option[Long] = None)(using BlockOf[B]): Poller[B] =
    new Poller[B](from, Vector.empty, keep, None, false, false, until)

/** a poll source as okay-watch's `Chain` is one: calls that block at the
 * transport edge */
trait PollSource[B]:
  def head: Tip
  def block(height: Long): Polled[B]

/**
 * The blocking driver: one `step` asks the head once and takes blocks
 * until it has caught up, the next one is missing, or a bounded run is
 * done — okay-watch's `Follower.step`, over the two pure machines.
 */
final class Follow[B](source: PollSource[B], finality: Finality, from: Long,
                      keep: Int = 64, until: Option[Long] = None)(using BlockOf[B]):
  private var poller = Poller[B](from, keep, until)
  private var tracker = Tracker[B](finality)

  /** the next height a consumer has not seen confirmed */
  def next: Long = tracker.nextHeight.getOrElse(from)

  def step(): Either[Broken, Vector[Event[B]]] =
    val out = Vector.newBuilder[Event[B]]
    var gained = 0
    def feed(os: Vector[Observed[B]]): Either[Broken, Unit] =
      os.foldLeft[Either[Broken, Unit]](Right(())) { (acc, o) =>
        acc.flatMap(_ => tracker.feed(o).map { (t, es) => tracker = t; out ++= es; gained += es.size })
      }
    val (p0, os0) = poller.onHead(source.head)
    poller = p0
    var result = feed(os0)
    var go = true
    while go && result.isRight do
      poller.want match
        case Request.Block(h) =>
          // A SOURCE THAT FAILS AFTER PROGRESS keeps the progress
          // (follow-keeps-progress): the events already produced are
          // blocks the tracker has CONFIRMED — its frontier moved — and
          // throwing here would drop them where no caller sees them. The
          // step ends with what it has; the poller asks this height again
          // next step, where a failure that gains nothing is thrown.
          val polled =
            try Some(source.block(h))
            catch case scala.util.control.NonFatal(_) if gained > 0 => None
          polled match
            case Some(p) => result = poller.onBlock(h, p).flatMap { (q, os) => poller = q; feed(os) }
            case None => go = false
        case Request.Head | Request.Done => go = false
    result.map(_ => out.result())
