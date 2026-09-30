package okay

/**
 * REPRODUCER, ignored by default (windowjoin-spin-fix, 2026-09-30;
 * okay-stream/BUGS.md `ready-merge-side-starves`): a ready merge of two
 * ENDLESS buffered sides under any scheduler with owned workers (`own`,
 * the adaptive default) stops delivering one side within ~20 rounds —
 * the consumer keeps taking the hot side, the other side's channel
 * shows one element ever pushed and popped, and the hot side's ring
 * ends with head/tail some thirty laps AHEAD of every slot's stamp, so
 * it reads "full" to its pusher and "empty" to its popper at once.
 * Measured with the ring instrumented (not landed): no two pops and no
 * two pushes ever overlapped, the feeder fiber never ran on two
 * threads, and the commit before channel-route-per-producer
 * (a01fcf9c7) starves the same way at round 64. Un-ignore, run alone:
 *
 *   scripts/gate.sh "okayStreamJVM/testOnly okay.ProbeReadyMergeStarve"
 *
 * On a starvation the scheduler's counters, both channels' state, the
 * feeder-overlap verdict and the live worker stacks are printed, then
 * the round fails.
 */
class ProbeReadyMergeStarve extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(600, "s")

  /** the left source's own code, entered and left per element: a depth
   * of 2 would mean the feeder fiber ran on two threads at once */
  private val inside = java.util.concurrent.atomic.AtomicInteger(0)
  @volatile private var overlap: String | Null = null
  @volatile private var lastThread: String = ""
  private def l = Source.of(LazyList.from(0).map { i =>
    val me = Thread.currentThread().getName
    if inside.incrementAndGet() > 1 && overlap == null then overlap = s"element $i on $me while $lastThread was inside"
    lastThread = me
    inside.decrementAndGet()
    i
  })
  private def rt = Source.of(LazyList.from(1000000))
  private val thirdRight = FoldUntil[Either[Int, Int], Int, Int](0)((n, e) => if e.isRight then n + 1 else n)(_ >= 3)(identity)

  private def watch[A](name: String, r: Int, run: () => Either[Throwable, A], more: () => Unit)(using sch: Scheduler): Either[Throwable, A] =
    val done = java.util.concurrent.atomic.AtomicBoolean(false)
    val starved = java.util.concurrent.atomic.AtomicBoolean(false)
    Thread.ofPlatform().daemon().start(() => {
      Thread.sleep(8000)
      if !done.get then
        starved.set(true)
        println(s"$name round $r: STARVED — the third Right never came; feeder overlap=$overlap")
        sch match
          case o: Schedulers.Owned => println("  scheduler: " + o.stats)
          case _ => ()
        more()
        import scala.jdk.CollectionConverters.*
        for (t, st) <- Thread.getAllStackTraces.asScala if t.getName.startsWith("okay") && st.nonEmpty && !st.exists(_.getMethodName == "park") do
          println(s"  ${t.getName} ${t.getState}: " + st.take(10).map(_.toString).mkString("\n      ", "\n      ", ""))
    })
    val out = run()
    done.set(true)
    assert(!starved.get, s"$name round $r starved")
    out

  private def ready[X](cl: Channel[X], cr: Channel[X]): Source[X] =
    ReadyMerge[X](Seq(cl.drained, cr.drained), quantum = Drain.Batch, release = Merge.closing(cl, cr))
  private def state(cl: Channel[?], cr: Channel[?]): () => Unit = () =>
    for (name, ch) <- List("left" -> cl, "right" -> cr) do ch match
      case sc: SentinelChannel[?] => println(s"  $name: " + sc.debugState)
      case other => println(s"  $name: ${other.getClass.getSimpleName}")

  for (sname, sch) <- List("default" -> summon[Scheduler], "own" -> Schedulers.own.build) do
    test(s"$sname: either's shape, the sides' channels caught — the third Right comes in every round".ignore) {
      given Scheduler = sch
      for r <- 1 to 300 do
        @volatile var cl: Channel[Either[Int, Int]] | Null = null
        @volatile var cr: Channel[Either[Int, Int]] | Null = null
        val m = pure[Writer % Either[Int, Int] + Async, Unit](()).flatMap: _ =>
          val l0 = Channel.buffer[Either[Int, Int], Source, Async](4)(Writer.map[Int, Either[Int, Int], Unit, Async](l)(Left(_)))
          val r0 = Channel.buffer[Either[Int, Int], Source, Async](4)(Writer.map[Int, Either[Int, Int], Unit, Async](rt)(Right(_)))
          cl = l0; cr = r0
          ready(l0, r0)
        val more = () => { val (a, b) = (cl, cr); if a != null && b != null then state(a, b)() else println("  channels not made") }
        assertEquals(watch(sname, r, () => sch.fork(() => m.runFoldUntil(using thirdRight)).joinEither(), more), Right(3), s"round $r")
    }

  test("default: Source.either itself — the third Right comes in every round".ignore) {
    for r <- 1 to 300 do
      val m = l.either(rt, capacity = 4)
      assertEquals(watch("either", r, () => summon[Scheduler].fork(() => m.runFoldUntil(using thirdRight)).joinEither(), () => ()), Right(3), s"round $r")
  }
}
