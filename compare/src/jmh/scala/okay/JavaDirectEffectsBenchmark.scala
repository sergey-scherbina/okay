package okay


import okay.freer.*


import okay.std.*
import okay.freer.given
import okay.std.given
import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.{SynchronousQueue, TimeUnit}

/**
 * ONE OPERATION, THREE MACHINES (specs/java-direct-effects.md, the probe's
 * second half): what a handled operation costs when the resumption is
 * okay's `Free` (what okay-java's `Eff` runs on), a virtual thread handing
 * the operation to the handler and parking (the public road to a direct
 * style), or `jdk.internal.vm.Continuation` (the machinery under it, used
 * directly; `vthreadBoth` is `vthread`'s control). Each lane runs `N`
 * operations answered 1 and returns their sum,
 * so per operation is the score / N. Same shape on every lane, a
 * tail-resumptive handler, the one form all three machines can express; no
 * competitor library, so the lane rules' (1)–(3) do not arise.
 *
 * `loom` needs `-jvmArgsAppend --add-exports=java.base/jdk.internal.vm=ALL-UNNAMED`.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class JavaDirectEffectsBenchmark {
  import JavaDirectEffectsBenchmark.*

  final val N = 1000

  @Benchmark
  def free(): Int = loop(N, 0).handle(answering).run

  @Benchmark
  def vthread(): Int = handoff()

  /**
   * The control for `vthread`: there the handler is the benchmark's own
   * PLATFORM thread, which parks in the OS at every handoff. Here the
   * handler runs on a virtual thread too, so both sides park as
   * continuations, the one join costing once per N operations.
   */
  @Benchmark
  def vthreadBoth(): Int =
    val out = java.util.concurrent.CompletableFuture[Integer]()
    Thread.ofVirtual().start(() => out.complete(handoff()): Unit): Unit
    out.join()

  private def handoff(): Int =
    val ops = SynchronousQueue[Msg]()
    val answers = SynchronousQueue[Integer]()
    Thread.ofVirtual().start { () =>
      var acc = 0
      var i = 0
      while i < N do
        ops.put(Next)
        acc += answers.take()
        i += 1
      ops.put(Done(acc))
    }: Unit
    var result = -1
    while result < 0 do
      ops.take() match
        case Next => answers.put(1)
        case Done(v) => result = v
    result

  @Benchmark
  def loom(): Int = LoomEffects.run(N)
}

object JavaDirectEffectsBenchmark {
  enum Tick[+A] derives Effect:
    case Tock extends Tick[Int]

  val answering: Handler[Tick, [A] =>> A] = Handler[Tick] { case Tick.Tock => 1 }

  /** `n` operations summed; each step deferred under the bind, so the recursion is trampolined */
  def loop(n: Int, acc: Int): Int ! Tick =
    if n == 0 then pure(acc) else Tick.Tock.perform.flatMap(a => loop(n - 1, acc + a))

  /** what the virtual thread hands the handler */
  sealed trait Msg
  case object Next extends Msg
  final case class Done(v: Int) extends Msg
}
