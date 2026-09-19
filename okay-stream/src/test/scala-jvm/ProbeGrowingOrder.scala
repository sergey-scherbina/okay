package okay

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong}
import scala.jdk.CollectionConverters.*

/**
 * INSTRUMENTED REPRODUCER for `growing-channel-order`, ignored by
 * default (BACKLOG `growing-order-instrumented-repro`).
 *
 * Five sightings of one shape exist and a sixth adds nothing. What is
 * missing is a break that SAYS WHICH ROAD IT TOOK, so this runs the
 * round-based law with every push into the buffer recorded, and prints
 * the offending producer's whole history when the law breaks.
 *
 * HOW TO RUN IT (load, not repetition — 2 000 rounds on a quiet box
 * reproduced nothing, and every red was on a loaded one):
 *
 *   okayJVM/testOnly okay.ProbeGrowingOrder -- --include-tags=probe
 *
 * or un-ignore the test below and
 *
 *   OKAY_PROBE_ROUNDS=4000 OKAY_PROBE_BURNERS=24 \
 *     sbt 'okayJVM/testOnly okay.ProbeGrowingOrder'
 *
 * ENVIRONMENT, NOT `-D`: this module forks its tests and passes only
 * `-Xmx1g` through, so a system property given to sbt does not reach
 * the JVM the probe runs in — the first attempt was a silent
 * `Ignored 1`. `OKAY_PROBE_ROUNDS` is also the switch: without it the
 * test stays ignored, so a gate never spends an hour here.
 * `OKAY_PROBE_BURNERS` spins that many CPU burners for the duration
 * (default 0 — SET IT; the entry's condition is a load of ~20).
 *
 * WHAT IT INSTRUMENTS, both candidates named by the BUGS.md
 * retraction as not yet examined:
 *
 *   1. THE PARKING PATH. `pushDecidingAtOnBehalf` runs on whichever
 *      thread freed the slot and deliberately does NOT repair the
 *      route it was parked with. Counter `resumedOntoForeignRoute`:
 *      a resumed push whose route differs from the route that value's
 *      OWN producer last used. The comparison is possible here and
 *      not inside the buffer, because this probe knows which producer
 *      every value belongs to — odd values are producer 1's, even
 *      values producer 0's — while the buffer cannot know.
 *
 *   2. THE PART-0 WINDOW. `grow()` sets `grown` by CAS, then builds
 *      the AdaptiveFifo, then assigns `inner`. Anything reading
 *      `grown == true` in between still sees the ring.
 *      Counter `landedInPart0AfterSwapSeen`: a push that landed in
 *      part 0 after some push had already landed in a part above it.
 *      Part 0 is the ADOPTED part and is drained FIRST, so a later
 *      element arriving there overtakes the producer's earlier ones —
 *      which is exactly the observed damage.
 *
 * ONE CORRECTION TO THE ENTRY, found by reading rather than running,
 * and stated here because the counter above is shaped by it: the
 * entry calls `Growing.inner` "a plain `var` assigned AFTER the
 * `grown` CAS, so a reader seeing `grown == true` has no
 * happens-before edge to the new buffer". `inner` is `@volatile`, and
 * has been since the file was created (3f3adca1), so there is no
 * publication hole — a reader that sees the new buffer sees it whole.
 * What is real is the STALENESS window above: `grown` is true while
 * `inner` is still the ring. Counter 2 measures the observable
 * consequence of that window instead of the private flag, which a
 * test cannot see and should not be given a back door to.
 *
 * WHAT WOULD CLOSE IT: a break whose counters are non-zero names the
 * road. A break whose counters are BOTH zero refutes both candidates
 * and is worth as much.
 *
 * WHAT THIS PROBE COSTS, said before anyone trusts a green run: every
 * successful push allocates one record, takes one `AtomicLong`
 * increment for its sequence number, and appends to one
 * `ConcurrentLinkedQueue` shared by both producers and the waker.
 * That is real perturbation on a hot path — a shared tail is the very
 * thing `Growing` exists to avoid — and a race this narrow can be
 * masked by it. A run that reproduces nothing WITH tracing is
 * therefore not evidence that the bug is gone; it is evidence about
 * the traced build. If that happens, run the counters alone
 * (`-Dokay.probe.trace=false`): they cost two atomics on the paths
 * that fire and touch no shared queue.
 */
class ProbeGrowingOrder extends munit.FunSuite {

  override def munitTimeout: scala.concurrent.duration.Duration =
    scala.concurrent.duration.Duration(30, "min")

  /**
   * ENV FIRST, and that is not a preference — `okayJVM` has
   * `Test / fork := true` and passes only `-Xmx1g` through, so a
   * `-Dokay.probe.rounds` given to sbt never reaches the JVM this
   * test runs in. The first run of this probe was a silent
   * `Ignored 1` for exactly that reason. Environment IS inherited by
   * the fork, so `OKAY_PROBE_ROUNDS` is the form that works; the
   * system property is kept for an unforked module or a `set
   * Test/javaOptions` session.
   */
  private def setting(name: String): Option[String] =
    sys.props.get(name).orElse(sys.env.get(name.replace('.', '_').toUpperCase))

  private val Burners: Int = setting("okay.probe.burners").flatMap(_.toIntOption).getOrElse(0)
  private val Rounds: Int = setting("okay.probe.rounds").flatMap(_.toIntOption).getOrElse(2000)
  private val Tracing: Boolean = setting("okay.probe.trace").forall(_ != "false")

  /** OFF unless asked for, and asked for by the property that also
   * says how long to run: the gate must never spend an hour here, and
   * a static `.ignore` made the invocation in the header above a lie —
   * it told you to pass `-Dokay.probe.rounds` and then ignored the
   * test anyway. Now the property IS the switch. */
  private val Enabled: Boolean = setting("okay.probe.rounds").isDefined

  /** the shipped construction's numbers, copied from Channel.scala so
   * a drift there shows up here as a difference rather than silently:
   * `Channel(4)` is `SentinelChannel(Growing(Ring(4), 8, () => Ring(4)))` */
  private val Cap = 4
  private val Parts = 8
  private val PerRound = 400

  // ---------------------------------------------------------------- the record

  /** one call into the buffer, as the probe saw it */
  private final case class Ev(seq: Long, thread: String, kind: String,
                              route: Int, value: Int, landedIn: Int)

  private final class Run {
    val seq = AtomicLong(0)
    /** which part each buffer instance is: 0 is the ring (and, after
     * the swap, the adopted part); 1.. are the parts `each()` made */
    val ids = AtomicInteger(0)
    /** every call, in `seq` order — the FULL trace, opt-in, because it
     * MASKS THE RACE. Measured 2026-09-18: with it off, the break came
     * at round 1959 of 2000; with it on, 6000 rounds found nothing.
     * One shared queue append per push is enough to close the window,
     * which is the instrument being bigger than the effect. */
    val log = java.util.concurrent.ConcurrentLinkedQueue[Ev]()

    /** THE CHEAP TRACE, always on: which part each value landed in,
     * one byte per value, written ONCE by whoever pushed it. No
     * sharing, no allocation, no counter — the only thing on the push
     * path besides the two counters, and it is what actually names
     * the mechanism when the law breaks. */
    val landedIn = Array.fill[Byte](PerRound)(-1)
    /** the route each producer last took for itself, by value parity —
     * an array and not a map, because there are exactly two producers
     * and a boxing lookup on this path is overhead for nothing */
    val lastRouteOfProducer = Array(AtomicInteger(-1), AtomicInteger(-1))

    // the two counters the entry asks for
    val resumedOntoForeignRoute = AtomicLong(0)
    val landedInPart0AfterSwapSeen = AtomicLong(0)
    /** set once a push lands in a part above 0: from then on the swap
     * is visible to somebody, which is what makes counter 2 readable */
    val swapSeen = AtomicBoolean(false)

    def record(kind: String, route: Int, value: Int, landedIn: Int): Unit =
      if Tracing then
        val t = Thread.currentThread()
        val _ = log.add(Ev(seq.getAndIncrement(), t.getName.nn, kind, route, value, landedIn))

    def merged: Vector[Ev] = log.asScala.toVector.sortBy(_.seq)
  }

  // ---------------------------------------------------------------- the decorator

  /**
   * A `Buffer` that records what the channel asks of it and what the
   * buffer under it did, and delegates everything else untouched.
   *
   * EVERY member is overridden, including the ones `Buffer` gives a
   * default: a default here would answer for the decorator instead of
   * the buffer inside it (`route()` would be 0, `parts` would be 1),
   * and the probe would then be measuring itself. That is the one way
   * a decorator like this goes quietly wrong.
   */
  private final class Traced[A](under: Buffer[A], id: Int, run: Run,
                                value: A => Int) extends Buffer[A] {

    private def note(kind: String, route: Int, a: A, ok: Boolean): Unit =
      if ok then
        val v = value(a)
        if v >= 0 then
          run.record(kind, route, v, id)
          if v < run.landedIn.length then run.landedIn(v) = id.toByte
          if id > 0 then run.swapSeen.set(true)
          else if run.swapSeen.get then
            val _ = run.landedInPart0AfterSwapSeen.incrementAndGet()

    // ---- the push side: what the channel asks, and where it landed
    override def push(a: A): Boolean =
      val ok = under.push(a); note("push", -1, a, ok); ok

    override def pushAt(route: Int, a: A): Boolean =
      val ok = under.pushAt(route, a); note("pushAt", route, a, ok); ok

    override def pushDeciding(a: A, unless: AtomicBoolean, orElse: A): A | Null =
      val out = under.pushDeciding(a, unless, orElse)
      note("deciding", -1, a, out != null); out

    override def pushDecidingAt(route: Int, a: A, unless: AtomicBoolean, orElse: A): A | Null =
      val out = under.pushDecidingAt(route, a, unless, orElse)
      if out != null then
        val v = value(a)
        if v >= 0 then
          run.lastRouteOfProducer(v % 2).set(route)
      note("decidingAt", route, a, out != null); out

    /**
     * THE PARKING PATH, candidate 1. This runs on the thread that
     * freed the slot, carrying the route the producer parked with.
     * The producer it belongs to is known HERE — by the value's
     * parity, which is the probe's own convention — so the route can
     * be compared with what that producer last used.
     */
    override def pushDecidingAtOnBehalf(route: Int, a: A, unless: AtomicBoolean, orElse: A): A | Null =
      val out = under.pushDecidingAtOnBehalf(route, a, unless, orElse)
      if out != null then
        val v = value(a)
        if v >= 0 then
          val mine = run.lastRouteOfProducer(v % 2).get
          if mine >= 0 && mine != route then
            val _ = run.resumedOntoForeignRoute.incrementAndGet()
      note("onBehalf", route, a, out != null); out

    override def pushMany(n: Int)(src: Int => A): Int = under.pushMany(n)(src)

    // ---- everything else: delegated, never defaulted
    override def capacity: Int = under.capacity
    override def size: Int = under.size
    override def isEmpty: Boolean = under.isEmpty
    override def hasReady: Boolean = under.hasReady
    override def hasRoom: Boolean = under.hasRoom
    override def hasRoomAt(route: Int): Boolean = under.hasRoomAt(route)
    override def parts: Int = under.parts
    override def maxParts: Int = under.maxParts
    override def route(): Int = under.route()
    override def lastRoute: Int = under.lastRoute
    override def pop(): A | Null = under.pop()
    override def popMany(max: Int)(sink: A => Unit): Int = under.popMany(max)(sink)
    override def seal(mark: A): Int = under.seal(mark)
  }

  // ---------------------------------------------------------------- the load

  private def burners(n: Int): Vector[Thread] =
    (0 until n).toVector.map(_ => Thread.ofPlatform().daemon().start { () =>
      var x = 1L
      while !Thread.currentThread().isInterrupted do
        var i = 0
        while i < 1000000 do { x = x * 31 + i; i += 1 }
        if x == Long.MinValue then println("")   // never; keeps the loop alive
    })

  // ---------------------------------------------------------------- the law, instrumented

  private val probeName =
    "growing-channel-order, instrumented: each producer's own order survives the swap"

  // BOTH BRANCHES ASCRIBED: `if a then "s" else "s".ignore` infers
  // `String | TestOptions`, which matches neither overload of `test`
  private val probeOptions: munit.TestOptions =
    if Enabled then (probeName: munit.TestOptions)
    else ((s"$probeName (set -Dokay.probe.rounds to run)"): munit.TestOptions).ignore

  test(probeOptions) {
    val load = burners(Burners)
    println(s"[probe] burners=$Burners rounds=$Rounds tracing=$Tracing " +
      s"cpus=${Runtime.getRuntime.nn.availableProcessors()}")
    try
      var round = 0
      var broken = 0
      // A NEGATIVE RESULT MUST SHOW IT REACHED THE STATE UNDER TEST.
      // "4000 rounds, no break" says nothing if the buffer never grew
      // — the bug is about a producer's order ACROSS THE SWAP, and a
      // round where `Growing` stayed one ring never tested it. Found
      // by asking the first green run what it had actually exercised.
      var grew = 0
      while round < Rounds do
        val run = Run()
        // THE SHIPPED CONSTRUCTION, with every buffer in it traced:
        // `Channel(4)` is exactly this, minus the decorators
        def part(): Buffer[Int | Mark] =
          Traced[Int | Mark](Ring[Int | Mark](Cap), run.ids.incrementAndGet(), run,
            { case i: Int => i; case _ => -1 })
        val ring = Traced[Int | Mark](Ring[Int | Mark](Cap), 0, run,
          { case i: Int => i; case _ => -1 })
        val growing = Growing[Int | Mark](ring, Parts, () => part())
        // NOT wrapped again on the outside: `Growing` forwards
        // `pushDecidingAtOnBehalf` to the part, so the parts already
        // see the parking path, and one less decorator is one less
        // thing perturbing the race
        val ch = SentinelChannel[Int](growing)

        val live = AtomicInteger(2)
        val producers = (0 until 2).map(p => Thread.startVirtualThread { () =>
          var i = p
          var going = true
          while going && i < PerRound do { going = ch.sendBlocking(i); i += 2 }
          if live.decrementAndGet() == 0 then ch.close()
        })
        val got = List.newBuilder[Int]
        var open = true
        while open do ch.receiveBlocking() match
          case Some(v) => got += v
          case None => open = false
        producers.foreach(_.join())
        val out = got.result()

        if run.swapSeen.get then grew += 1
        List(0, 1).foreach { p =>
          val own = out.filter(_ % 2 == p)
          if own != own.sorted then
            broken += 1
            report(round, p, own, run)
        }
        round += 1
      println(s"[probe] $Rounds rounds; the buffer GREW in $grew of them " +
        f"(${100.0 * grew / math.max(1, Rounds)}%.1f%%)")
      if grew == 0 then
        println("[probe] THE SWAP NEVER HAPPENED — this run tested nothing about it. " +
          "Raise the element count or lower the capacity until `grew` is most of the rounds.")
      if broken == 0 then
        println(s"[probe] no break in $grew rounds that actually swapped. Counters over " +
          "the last round only are meaningless; a break is what this probe is for.")
      assertEquals(broken, 0, s"$broken break(s) — the report above names the road")
    finally load.foreach(_.interrupt())
  }

  /** what a break prints: the two counters, then the producer's own
   * history with the part each element landed in */
  private def report(round: Int, p: Int, own: List[Int], run: Run): Unit =
    println(s"\n[probe] ===== BREAK, round $round, producer $p =====")
    println(s"[probe] its order came back: ${own.take(40).mkString(", ")}")
    val firstBad = own.zip(own.tail).indexWhere((a, b) => a > b)
    println(s"[probe] first inversion at index $firstBad: " +
      s"${own.lift(firstBad).getOrElse("?")} then ${own.lift(firstBad + 1).getOrElse("?")}")
    println(s"[probe] counter resumedOntoForeignRoute    = ${run.resumedOntoForeignRoute.get}")
    println(s"[probe] counter landedInPart0AfterSwapSeen = ${run.landedInPart0AfterSwapSeen.get}")
    // the cheap trace, which is always there: the part each of this
    // producer's elements landed in, IN SEND ORDER. Read it against
    // the drain order above — part 0 is the adopted part and is
    // drained first, so an element in a higher part is overtaken by
    // every later element that went to 0.
    val sent = own.sorted
    println("[probe] this producer's elements, in SEND order, with the part each landed in:")
    println("[probe]   " + sent.map(v =>
      s"$v->${if v < run.landedIn.length then run.landedIn(v).toInt else -1}").mkString(" "))
    val backToZero = sent.sliding(2).count(w => w.size == 2 &&
      w(0) < run.landedIn.length && w(1) < run.landedIn.length &&
      run.landedIn(w(0)) > 0 && run.landedIn(w(1)) == 0.toByte)
    println(s"[probe] this producer went from a part ABOVE 0 back to part 0 $backToZero time(s)")
    if !Tracing then println("[probe] full trace off (it masks the race); counters + the map above")
    else
      val mine = run.merged.filter(e => e.value >= 0 && e.value % 2 == p)
      println(s"[probe] this producer's ${mine.size} pushes, as the buffer saw them:")
      mine.foreach(e =>
        println(f"[probe]   seq=${e.seq}%6d  value=${e.value}%4d  part=${e.landedIn}%2d  " +
          f"route=${e.route}%2d  ${e.kind}%-10s  ${e.thread}"))
      // the two things to read off it, named so the reader does not
      // have to re-derive them from the dump
      val backToZero = mine.sliding(2).count(w => w.size == 2 && w(0).landedIn > 0 && w(1).landedIn == 0)
      val onBehalf = mine.count(_.kind == "onBehalf")
      println(s"[probe] this producer went BACK to part 0 $backToZero time(s); " +
        s"$onBehalf of its pushes were resumed on another thread")
}
