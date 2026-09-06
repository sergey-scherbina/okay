package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.actor.*
import okay.reactive.Reactive

/**
 * actor-reactive-bench: the two modules that had no numbers.
 *
 * Both sit on the channel measured all day. An actor's mailbox IS a
 * `Channel[M](256)`; `tell` is `mailbox.send(m)`, one `Async.Await`
 * per message; the loop reads `receiveBlocking()` one message at a
 * time ON PURPOSE (supervision must know which message was the
 * poisonous one). `ask` builds a `Reply` -- a `Channel[R](2)` -- and
 * races its receive against `Async.sleep(within)`: a timer per ask.
 * The reactive bridge runs demand in half-window batches through a
 * channel of `capacity`.
 *
 * Every lane has a control so the module's price reads as a RATIO:
 *   actorTell      against channelBuffer  (the mailbox's own shape, no actor)
 *   reactiveRound  against plainSource    (the same elements, no bridge)
 * actorAsk and actorSpawnStop are absolute; their per-op cost is what
 * a caller pays for one round trip and one lifecycle.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class ActorReactiveBenchmark {

  final val N = 4000
  final val Asks = 200
  final val Lifecycles = 100
  private val list: List[Long] = (0L until N.toLong).toList

  enum Msg:
    case Add(n: Long)
    case Get(reply: Reply[Long])

  private val summing: Behavior[Long, Msg] = (s, m) =>
    m match
      case Msg.Add(n) => pure(s + n)
      case Msg.Get(r) => { r(s); pure(s) }

  /** N tells as ONE program, then one ask to read the sum, then stop */
  @Benchmark
  def actorTell(): Long =
    val a = Actor.spawn(0L)(summing).runWith
    def go(i: Long): Unit ! Async =
      if i >= N then pure(())
      else a.tell(Msg.Add(i)).flatMap(_ => go(i + 1))
    val sum = go(0).flatMap(_ => a.ask(Msg.Get.apply, within = 10_000)).runWith
    a.stop().runWith
    sum.getOrElse(-1L)

  /** the control: the same N through the mailbox's own shape, no actor */
  @Benchmark
  def channelBuffer(): Long =
    var sum = 0L
    Channel.buffer(256)(list).drained.runForeach(x =>
      okay.effect[Async, Unit](Async.Run(() => sum += x))).runWith
    sum

  /** Asks sequential round trips: a Reply channel, a send, a race with a timer, each */
  @Benchmark
  def actorAsk(): Long =
    val a = Actor.spawn(1L)(summing).runWith
    def go(i: Int, acc: Long): Long ! Async =
      if i >= Asks then pure(acc)
      else a.ask(Msg.Get.apply, within = 10_000).flatMap(r => go(i + 1, acc + r.getOrElse(0L)))
    val out = go(0, 0L).runWith
    a.stop().runWith
    out

  /** Lifecycles spawn+stop pairs: a channel, a fiber, a drain, each */
  @Benchmark
  def actorSpawnStop(): Int =
    var n = 0
    while n < Lifecycles do
      val a = Actor.spawn(0L)(summing).runWith
      a.stop().runWith
      n += 1
    n

  /** N elements out through a Flow.Publisher and back in through a Flow.Subscriber */
  @Benchmark
  def reactiveRound(): Long =
    var sum = 0L
    Reactive.source(Reactive.publisher(Source.range(0L, N.toLong)), capacity = 256).runForeach(x =>
      okay.effect[Async, Unit](Async.Run(() => sum += x))).runWith
    sum

  /** the control: the same source, no bridge */
  @Benchmark
  def plainSource(): Long =
    var sum = 0L
    Source.range(0L, N.toLong).runForeach(x =>
      okay.effect[Async, Unit](Async.Run(() => sum += x))).runWith
    sum
}
