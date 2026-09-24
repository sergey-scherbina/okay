package okay2.stream

/**
 * Building a channel out of the pieces — okay-stream's Queues.scala: pick
 * what it must PROMISE, then what it runs ON. The contract is semantics
 * (a caller sees it in what the program does); the mechanism is
 * performance (seen only in timing). `Channel.apply` picks both and is
 * right nearly always.
 *
 * {{{
 * Queues.strong[Int].bounded(1024).build
 * Queues.strong[Int].unbounded.build
 * Queues.strong[Int].relaxed.parts(8).each(256).build
 * Queues.strong[Int].adaptive.each(1024).build
 * Queues.strong[Int].growing(1024).build
 * Queues.strong[Int].on(() => new Ring[Any](64)).build
 * Queues.weak[Int].bounded(64).build          // close discards
 * Queues.composable[Int](1024).build          // StmChannel
 * Queues.rendezvous[Int].build
 * }}}
 *
 * A mechanism is a `() => Buffer[Any]`: the channels store `Any`, and the
 * Scala 3 core's polymorphic factory exists only to keep its `Mark`
 * private, which a private class with a private constructor does here.
 */
object Queues {

  /** the strong contract: an accepted element is delivered, and close ends
   * the stream only once the buffer is spent */
  def strong[A]: Strong[A] = Strong[A]()

  /** the weak one: close ends it at once and the buffer is abandoned */
  def weak[A]: Weak[A] = Weak[A]()

  /** the strong contract on the single-CAS reference implementation */
  def composable[A](capacity: Int = Int.MaxValue): Composable[A] = Composable[A](capacity)

  /** no buffer at all: a sender waits for a receiver */
  def rendezvous[A]: Rendezvous[A] = Rendezvous[A]()

  /** every mechanism the strong and weak contracts run on, in one place */
  private[stream] object Mechanism {
    def ring(capacity: Int, singleConsumer: Boolean = false): () => Buffer[Any] =
      () => new Ring[Any](capacity, singleConsumer)
    def segments: () => Buffer[Any] = () => new Segments[Any]()
    /** `each` is PER PART, and parts open lazily: what the channel holds
     * is `each x producers that arrived`, never `each x parts` */
    def growing(each: Int, parts: Int): () => Buffer[Any] = () => {
      val cap = if (each < 2) 2 else each
      val n = if (parts < 2) 2 else parts
      new Growing[Any](new Ring[Any](cap), n, () => new Ring[Any](cap))
    }
  }

  final case class Strong[A](private val buffer: Option[() => Buffer[Any]] = None) {
    /** a fixed ring; `singleConsumer` is a PROMISE of one receiving thread
     * of control (an actor's mailbox), which then skips a CAS per element */
    def bounded(capacity: Int, singleConsumer: Boolean = false): Strong[A] =
      copy(buffer = Some(Mechanism.ring(capacity, singleConsumer)))
    /** ONE ring, so EXACT FIFO ACROSS PRODUCERS — the name to ask for when
     * the order between producers is part of your correctness */
    def fifo(capacity: Int, singleConsumer: Boolean = false): Strong[A] = bounded(capacity, singleConsumer)
    /** a ring that grows by segments, so a producer never waits */
    def unbounded: Strong[A] = copy(buffer = Some(Mechanism.segments))
    /** `parts` fixed buffers, a producer bound to one */
    def relaxed: Parted[A] = Parted[A](this, eager = true)
    /** a plain ring until two producers are seen, then partitioned —
     * what `Channel.apply` builds */
    def growing(each: Int, parts: Int = 8): Strong[A] = copy(buffer = Some(Mechanism.growing(each, parts)))
    /** parts appear as producers do, up to the cap; never adopts, so each
     * producer's order is exact */
    def adaptive: Parted[A] = Parted[A](this, eager = false)
    /** your own mechanism */
    def on(buffer: () => Buffer[Any]): Strong[A] = copy(buffer = Some(buffer))

    def build: Channel[A] = buffer match {
      case Some(b) => new SentinelChannel[A](b())
      case None => Channel[A]()
    }
  }

  /** a partitioned mechanism waiting for its one semantic choice:
   * `each` (per part) or `unbounded` */
  final case class Parted[A](private val back: Strong[A], private val eager: Boolean,
                             private val maxParts: Int = Runtime.getRuntime.availableProcessors) {
    def parts(n: Int): Parted[A] = copy(maxParts = if (n < 1) 1 else n)
    def each(capacity: Int): Strong[A] = {
      val n = if (maxParts < 1) 1 else maxParts
      back.on(() => new AdaptiveFifo[Any](n, () => new Ring[Any](math.max(2, capacity)), eager))
    }
    def unbounded: Strong[A] = back.on(() => new AdaptiveFifo[Any](maxParts, () => new Segments[Any](), eager))
  }

  final case class Weak[A](private val buffer: Option[() => Buffer[Any]] = None) {
    def bounded(capacity: Int): Weak[A] = copy(buffer = Some(Mechanism.ring(capacity)))
    def unbounded: Weak[A] = copy(buffer = Some(Mechanism.segments))
    def relaxed: PartedWeak[A] = PartedWeak[A](this, eager = true)
    def adaptive: PartedWeak[A] = PartedWeak[A](this, eager = false)
    def on(buffer: () => Buffer[Any]): Weak[A] = copy(buffer = Some(buffer))
    def build: Channel[A] = new AbruptChannel[A](buffer.getOrElse(Mechanism.ring(1024))())
  }

  final case class PartedWeak[A](private val back: Weak[A], private val eager: Boolean,
                                 private val maxParts: Int = Runtime.getRuntime.availableProcessors) {
    def parts(n: Int): PartedWeak[A] = copy(maxParts = if (n < 1) 1 else n)
    def each(capacity: Int): Weak[A] =
      back.on(() => new AdaptiveFifo[Any](maxParts, () => new Ring[Any](math.max(2, capacity)), eager))
    def unbounded: Weak[A] = back.on(() => new AdaptiveFifo[Any](maxParts, () => new Segments[Any](), eager))
  }

  final case class Composable[A](capacity: Int, private val buf: Option[() => Fifo[A]] = None) {
    def arrayBuffer: Composable[A] = copy(buf = Some(() => Fifo.array[A]))
    def on(buf: () => Fifo[A]): Composable[A] = copy(buf = Some(buf))
    def build: Channel[A] = new StmChannel[A](capacity, buf.getOrElse(() => Fifo.array[A]))
  }

  final case class Rendezvous[A]() {
    def build: Channel[A] = new StmChannel[A](0)
  }
}
