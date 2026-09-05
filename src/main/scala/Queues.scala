package okay

/**
 * Building a channel out of the pieces: pick what it must PROMISE,
 * then pick what it runs ON.
 *
 * The two choices are independent, and they are not the same KIND of
 * choice — which is the whole reason this exists rather than one
 * constructor with eight arguments. The contract is semantics: a
 * caller can see the difference in what their program does. The
 * mechanism is performance: measured, and invisible except in the
 * timing. `Channel.apply` picks both and is the right answer nearly
 * always; this is for when it is not.
 *
 * {{{
 * Queues.strong[Int].bounded(1024).build              // the default, spelled out
 * Queues.strong[Int].unbounded.build                  // grows by segments
 * Queues.strong[Int].relaxed.parts(8).each(256).build
 * Queues.strong[Int].relaxed.parts(8).unbounded.build
 * Queues.strong[Int].adaptive.each(1024).build
 * Queues.strong[Int].on([T] => (n: Int) => Ring[T](n)).build
 *
 * Queues.weak[Int].bounded(64).build                  // close discards
 * Queues.weak[Int].unbounded.build
 * Queues.weak[Int].relaxed.parts(4).each(64).build
 *
 * Queues.composable[Int](1024).arrayBuffer.build      // STM, and slower
 * Queues.composable[Int](1024).listBuffer.build
 * Queues.rendezvous[Int].build                        // no buffer at all
 * }}}
 */
object Queues {

  /** the strong contract: an accepted element is delivered, and close
   * ends the stream only once the buffer is spent */
  def strong[A]: Strong[A] = Strong[A]()

  /** the weak one: close ends it at once and whatever is buffered is
   * abandoned. For a feed whose remainder is stale the moment the
   * consumer stops — a UI's events, a cancelled request's frames */
  def weak[A]: Weak[A] = Weak[A]()

  /** the strong contract PLUS composability: a single STM cell, so it
   * works inside `Tx.orElse`, `retry` and multi-cell transactions.
   * Nothing else here does, and it is the slowest, because
   * persistence is what composability costs */
  def composable[A](capacity: Int = Int.MaxValue): Composable[A] = Composable[A](capacity)

  /** no buffer at all: a sender waits for a receiver. Its own
   * mechanism, because the stamp scheme a ring uses cannot express a
   * capacity below two */
  def rendezvous[A]: Rendezvous[A] = Rendezvous[A]()

  /** every mechanism the strong and weak contracts can run on, in one
   * place so the two menus cannot drift apart */
  private object Mechanism {
    def ring(capacity: Int): [T] => Int => Buffer[T] =
      [T] => (_: Int) => Ring[T](capacity)
    def segments: [T] => Int => Buffer[T] =
      [T] => (_: Int) => Segments[T]()
    def multiRing(parts: Int, each: Int): [T] => Int => Buffer[T] =
      [T] => (_: Int) => AdaptiveFifo[T](parts, () => Ring[T](each), eager = true)
    def multiSegments(parts: Int): [T] => Int => Buffer[T] =
      [T] => (_: Int) => AdaptiveFifo[T](parts, () => Segments[T](), eager = true)
  }

  final case class Strong[A](private val buffer: Option[[T] => Int => Buffer[T]] = None) {

    /** a fixed ring: the fastest, and it makes a full producer wait */
    def bounded(capacity: Int): Strong[A] = copy(buffer = Some(Mechanism.ring(capacity)))

    /** a ring that grows by segments, so a producer never waits */
    def unbounded: Strong[A] = copy(buffer = Some(Mechanism.segments))

    /**
     * RELAXED: `parts` independent buffers, a producer bound to one,
     * so producers stop contending for a single tail. The price is
     * the order BETWEEN producers; one producer's own elements still
     * arrive in the order it sent them.
     *
     * {{{
     * Queues.strong[Int].relaxed.parts(8).each(256).build
     * Queues.strong[Int].relaxed.parts(8).unbounded.build
     * }}}
     *
     * Fixed parts, unlike `adaptive`: all of them exist from the
     * start. Use it when the producer count is known; `adaptive` when
     * it is not.
     */
    def relaxed: Parted[A] = Parted[A](this, eager = true)

    /**
     * ADAPTIVE: parts appear as producers do, up to the cap.
     *
     * {{{
     * Queues.strong[Int].adaptive.each(1024).build
     * Queues.strong[Int].adaptive.unbounded.build
     * Queues.strong[Int].adaptive.parts(4).each(256).build
     * }}}
     *
     * The right part count depends on how many producers there turn
     * out to be — a plain ring wins at one, this is 38x faster at
     * sixteen — and that is usually not known where the channel is
     * made. No element ever migrates: a new producer gets a NEW part,
     * empty, so there is no stop-the-world moment inside a lock-free
     * structure.
     *
     * It costs 19% at one producer, which is the price of
     * PARTITIONING rather than of adapting — a hand-tuned `relaxed`
     * measured the same there. So this is not the default, and
     * `Channel.apply` still gives a plain ring.
     *
     * What it may decide, and why. The laws promise each producer's
     * own order, no loss and no duplication, and say NOTHING about
     * the order BETWEEN producers. This trades exactly that silence
     * — if your consumer relies on how two producers interleave, do
     * not use it. Nothing ever promised you that, but it may have
     * happened to hold.
     */
    def adaptive: Parted[A] = Parted[A](this, eager = false)

    /** your own mechanism, whatever it is */
    def on(buffer: [T] => Int => Buffer[T]): Strong[A] = copy(buffer = Some(buffer))

    def build: Channel[A] = buffer match
      case Some(b) => SentinelChannel.over[A](0)(b)
      case None => Channel[A]()
  }

  /**
   * A partitioned mechanism, waiting for the one choice it will not
   * make for you: `each`/`unbounded` is backpressure, which is
   * semantics, and no benchmark decides it.
   *
   * `eager` is the difference between `relaxed` (every part from the
   * start) and `adaptive` (parts as producers arrive) — one flag, not
   * two mechanisms.
   *
   * THE UNIT IS IN THE NAME, deliberately. `each(n)` is per part;
   * the plain `bounded(n)` on a single buffer is a total. A day of
   * adding mechanisms one at a time left four spellings with three
   * meanings, and a number copied between them silently meant
   * something else. Documenting each case separately was the wrong
   * fix.
   */
  final case class Parted[A](private val back: Strong[A],
                             private val eager: Boolean,
                             private val maxParts: Int =
                               Runtime.getRuntime.availableProcessors) {

    def parts(n: Int): Parted[A] = copy(maxParts = if n < 1 then 1 else n)

    /**
     * `capacity` PER PART, and it cannot be a total: a part's ring is
     * fixed when the part opens, and how many parts there will be is
     * exactly what an adaptive buffer does not know yet. Dividing a
     * total by the cap gave a lone producer a sixteenth of its buffer
     * and parked it constantly — 969.8us against a plain ring's
     * 113.9.
     */
    def each(capacity: Int): Strong[A] =
      val n = if maxParts < 1 then 1 else maxParts
      back.on([T] => (_: Int) => AdaptiveFifo[T](n, () => Ring[T](math.max(2, capacity)), eager))

    /** parts that grow instead of filling, so no producer ever waits */
    def unbounded: Strong[A] =
      back.on([T] => (_: Int) => AdaptiveFifo[T](maxParts, () => Segments[T](), eager))
  }

  final case class Weak[A](private val buffer: Option[Buffer[A]] = None) {

    def bounded(capacity: Int): Weak[A] = copy(buffer = Some(Ring[A](capacity)))
    def unbounded: Weak[A] = copy(buffer = Some(Segments[A]()))

    /** `parts` fixed buffers — the same spelling as on the strong
     * contract, because the two menus must not drift apart */
    def relaxed: PartedWeak[A] = PartedWeak[A](this, eager = true)

    /** parts as producers arrive */
    def adaptive: PartedWeak[A] = PartedWeak[A](this, eager = false)

    /** your own mechanism. The weak contract needs no polymorphic
     * factory: nothing rides this buffer but the elements, because
     * this channel has no end mark to hide */
    def on(buffer: Buffer[A]): Weak[A] = copy(buffer = Some(buffer))

    def build: Channel[A] = AbruptChannel[A](buffer.getOrElse(Ring[A](1024)))
  }

  /** the weak contract's partitioned mechanisms, spelled as the
   * strong one's are. It needs no polymorphic factory: nothing rides
   * this buffer but the elements, because this channel has no end
   * mark to hide */
  final case class PartedWeak[A](private val back: Weak[A],
                                 private val eager: Boolean,
                                 private val maxParts: Int =
                                   Runtime.getRuntime.availableProcessors) {

    def parts(n: Int): PartedWeak[A] = copy(maxParts = if n < 1 then 1 else n)

    /** capacity PER PART */
    def each(capacity: Int): Weak[A] =
      back.on(AdaptiveFifo[A](maxParts, () => Ring[A](math.max(2, capacity)), eager))

    def unbounded: Weak[A] =
      back.on(AdaptiveFifo[A](maxParts, () => Segments[A](), eager))
  }

  final case class Composable[A](capacity: Int, private val buf: Option[() => Fifo[A]] = None) {

    /** the front as an immutable chunk plus an index — the default,
     * measured 7% ahead elementwise and level in batches */
    def arrayBuffer: Composable[A] = copy(buf = Some(() => Fifo.array[A]))

    /** the banker's pair of lists */
    def listBuffer: Composable[A] = copy(buf = Some(() => Fifo.list[A]))

    /** your own persistent buffer. It must be persistent: a losing
     * transaction has to leave nothing behind, which is the whole
     * reason this contract cannot use the mutable ones above */
    def on(buf: () => Fifo[A]): Composable[A] = copy(buf = Some(buf))

    def build: Channel[A] = StmChannel[A](capacity, buf.getOrElse(() => Fifo.array[A]))
  }

  final case class Rendezvous[A]() {
    def build: Channel[A] = StmChannel[A](0)
  }
}
