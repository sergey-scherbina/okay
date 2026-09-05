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
 * Queues.strong[Int].relaxed(parts = 8, each = 256).build
 * Queues.strong[Int].relaxedUnbounded(parts = 8).build
 * Queues.strong[Int].on([T] => (n: Int) => Ring[T](n)).build
 *
 * Queues.weak[Int].bounded(64).build                  // close discards
 * Queues.weak[Int].unbounded.build
 * Queues.weak[Int].relaxed(parts = 4, each = 64).build
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
      [T] => (_: Int) => MultiFifo[T](parts, _ => Ring[T](each))
    def multiSegments(parts: Int): [T] => Int => Buffer[T] =
      [T] => (_: Int) => MultiFifo[T](parts, _ => Segments[T]())
  }

  final case class Strong[A](private val buffer: Option[[T] => Int => Buffer[T]] = None) {

    /** a fixed ring: the fastest, and it makes a full producer wait */
    def bounded(capacity: Int): Strong[A] = copy(buffer = Some(Mechanism.ring(capacity)))

    /** a ring that grows by segments, so a producer never waits */
    def unbounded: Strong[A] = copy(buffer = Some(Mechanism.segments))

    /**
     * RELAXED over BOUNDED parts: producers stop contending for one
     * tail, and a full buffer still makes a producer wait — the
     * relaxation with backpressure kept.
     *
     * Measured 485us at sixteen producers, against 2586 for a single
     * ring and 2653 for `zio.Queue`, and it gets faster as producers
     * are added (780 at one, 597 at four, 485 at sixteen).
     *
     * It read 111546us until senders learned to wait PER PART. One
     * queue of waiting senders over a partitioned buffer means a
     * freed slot wakes an arbitrary sender, who finds its own part
     * still full and parks again — one useful wakeup in k. Waking
     * where the room appeared is worth 230x here, which is worth
     * knowing before designing the next partitioned thing.
     */
    def relaxed(parts: Int, each: Int): Strong[A] =
      copy(buffer = Some(Mechanism.multiRing(parts, each)))

    /**
     * RELAXED over parts that GROW — the fastest channel here under
     * many producers, and the one to reach for.
     *
     * Parts that never fill mean senders never park, so the waiting
     * mismatch that ruins the bounded form cannot arise. Measured
     * 169.9us at 16 producers against `zio.Queue`'s 2967 — 17.5x —
     * and it gets FASTER as producers are added (354 at one, 178 at
     * four, 170 at sixteen), which is the scaling relaxed queues are
     * for.
     *
     * What it costs: no backpressure, so a producer that outruns the
     * consumer is bounded only by memory; and the order BETWEEN
     * producers, though each producer's own order is kept.
     */
    def relaxedUnbounded(parts: Int): Strong[A] =
      copy(buffer = Some(Mechanism.multiSegments(parts)))

    /**
     * ADAPTIVE: parts appear as producers do, and the two choices
     * stay apart — `adaptive` picks the part count for you,
     * `bounded`/`unbounded` is still yours, because backpressure is
     * semantics and no benchmark can decide it.
     *
     * {{{
     * Queues.strong[Int].adaptive.bounded(1024).build
     * Queues.strong[Int].adaptive.unbounded.build
     * Queues.strong[Int].adaptive.parts(4).bounded(256).build
     * }}}
     *
     * The right part count depends on how many producers there turn
     * out to be — one ring wins at one producer, a relaxed buffer by
     * 19.6x at sixteen — and that is usually not known where the
     * channel is made. This decides it by watching: the first
     * producer gets one part, the second a second, and no element
     * ever migrates, so there is no stop-the-world moment inside a
     * lock-free structure.
     *
     * What it may decide, and why. The laws promise each producer's
     * own order, no loss and no duplication, and say NOTHING about
     * the order BETWEEN producers. This trades exactly that silence
     * — so if your consumer relies on how two different producers
     * interleave, do not use it. Nothing ever promised you that, but
     * it may have happened to hold.
     */
    def adaptive: Adaptive[A] = Adaptive[A](this)

    /** your own mechanism, whatever it is */
    def on(buffer: [T] => Int => Buffer[T]): Strong[A] = copy(buffer = Some(buffer))

    def build: Channel[A] = buffer match
      case Some(b) => SentinelChannel.over[A](0)(b)
      case None => Channel[A]()
  }

  /**
   * The adaptive mechanism, waiting for the one choice it will not
   * make for you. `parts` caps how many it may open; the default is
   * the machine's processor count, since more parts than cores buys
   * nothing but scan.
   */
  final case class Adaptive[A](private val back: Strong[A],
                               private val maxParts: Int =
                                 Runtime.getRuntime.availableProcessors) {

    def parts(n: Int): Adaptive[A] = copy(maxParts = if n < 1 then 1 else n)

    /**
     * `capacity` is PER PART, like `relaxed(parts, each)` and unlike
     * the plain `bounded`.
     *
     * It cannot be the total, and the reason is measured. A part's
     * ring is fixed when the part is opened, and how many parts there
     * will be is exactly what this mechanism does not know yet —
     * so dividing a total by the CAP gives a lone producer a
     * sixteenth of the buffer it asked for and parks it constantly:
     * 969.8us against a plain ring's 113.9. Per part, one producer
     * gets the ring it asked for and sixteen producers get sixteen of
     * them, which is also the honest reading of "how much may be in
     * flight" when the number of producers is the thing that varies.
     */
    def bounded(capacity: Int): Strong[A] =
      val n = if maxParts < 1 then 1 else maxParts
      back.on([T] => (_: Int) => AdaptiveFifo[T](n, () => Ring[T](math.max(2, capacity))))

    def unbounded: Strong[A] =
      back.on([T] => (_: Int) => AdaptiveFifo[T](maxParts, () => Segments[T]()))
  }

  final case class Weak[A](private val buffer: Option[Buffer[A]] = None) {

    def bounded(capacity: Int): Weak[A] = copy(buffer = Some(Ring[A](capacity)))
    def unbounded: Weak[A] = copy(buffer = Some(Segments[A]()))

    def relaxed(parts: Int, each: Int): Weak[A] =
      copy(buffer = Some(MultiFifo[A](parts, _ => Ring[A](each))))

    def relaxedUnbounded(parts: Int): Weak[A] =
      copy(buffer = Some(MultiFifo[A](parts, _ => Segments[A]())))

    /** your own mechanism. The weak contract needs no polymorphic
     * factory: nothing rides this buffer but the elements, because
     * this channel has no end mark to hide */
    def on(buffer: Buffer[A]): Weak[A] = copy(buffer = Some(buffer))

    def build: Channel[A] = AbruptChannel[A](buffer.getOrElse(Ring[A](1024)))
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
