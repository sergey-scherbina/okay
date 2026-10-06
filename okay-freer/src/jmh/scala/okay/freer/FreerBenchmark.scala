package okay.freer

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import Freer.*

/** an operation carrying its own answer, as master's `Ask` (HandlerBenchmark) */
enum Ask[+A]:
  case Value[A](a: A) extends Ask[A]
/** master's `Produce`: the operation forwarded by the inner handler to the outer one */
enum Tick[+A]:
  case Value[A](a: A) extends Tick[A]

/**
 * THE BASIS AGAINST MASTER (specs/freer-min.md, stage 20): the same workloads as master's HandlerBenchmark and
 * DelimDepthBenchmark, on handlers that are delimiters — every operation a capture to its handler's delimiter,
 * every one of the 99% forwarded a capture to the inner delimiter whose body captures to the outer.
 *
 *  - handleForward / buildOnly / handlePrebuilt: 10 000 operations, every 100th handled by the inner handler,
 *    the rest forwarded through it to the outer (master: `handle[Ask, Produce]` over `prog`)
 *  - tailcallChain: 10 000 mutual tail calls through `delay` (master: `!.tailcall`)
 *  - stateAtm: 1 000 get/set on the state-passing answer (master: `stateEffect` / `statePara`)
 *  - delimCaptureDepth: a capture through `depth` binds, `k` called `shots` times (master's `bind` shape)
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class FreerBenchmark {

  final val N = 10000
  final val M = 1000

  /** an operation's own value: the inner handler, of `Ask`, leaving `Tick` */
  val askH: Handler[Ask, Tick + Pure, Int, Int] = new Handler[Ask, Tick + Pure, Int, Int]:
    def ret(a: Int): Int = a
    def apply[X, Oc <: Ctx](using o: Oc)(op: Ask[X], k: X => Freer[Tick + Pure, o.Here, o.Here, Int]): Freer[Tick + Pure, o.Here, o.Here, Int] =
      op match
        case Ask.Value(a) => k(a)
  /** the outer handler, of `Tick`, leaving nothing */
  val tickH: Handler[Tick, Pure, Int, Int] = new Handler[Tick, Pure, Int, Int]:
    def ret(a: Int): Int = a
    def apply[X, Oc <: Ctx](using o: Oc)(op: Tick[X], k: X => Freer[Pure, o.Here, o.Here, Int]): Freer[Pure, o.Here, o.Here, Int] =
      op match
        case Tick.Value(a) => k(a)

  /** the program of the two handlers' body: 10k ops, every 100th handled by the inner (Ask), the rest forwarded (Tick) */
  def body[Oc <: Ctx](using c: In[Ask + (Tick + Pure), Tick + Pure, Int, Oc], pa: Perform[Ask, Ask + (Tick + Pure), c.type],
           pt: Perform[Tick, Ask + (Tick + Pure), c.type]): c.Body[Int] =
    var p: c.Body[Int] = perform(Ask.Value(0))
    var i = 1
    while i <= N do
      val j = i
      p = p.flatMap(x => if j % 100 == 0 then perform(Ask.Value(x + 1)) else perform(Tick.Value(x + 1)))
      i += 1
    p

  def prog: Top[Pure, Int] = handle[Tick, Pure, Int, Int](tickH)(handle[Ask, Tick + Pure, Int, Int](askH)(body))

  def value[A](p: Top[Pure, A]): A =
    val head: Top[Pure, A] = Machine.run(p)
    head match
      case Return(a) => a
      case other => throw new IllegalStateException(s"not a value: $other")

  @Benchmark
  def buildOnly(): Any = prog

  @Benchmark
  def handleForward(): Int = value(prog)

  private var built: Top[Pure, Int] = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def buildOnce(): Unit = built = prog

  @Benchmark
  def handlePrebuilt(): Int = value(built)

  def isEven(n: Int): Top[Pure, Boolean] = if n == 0 then pure(true) else delay(isOdd(n - 1))
  def isOdd(n: Int): Top[Pure, Boolean] = if n == 0 then pure(false) else delay(isEven(n - 1))

  @Benchmark
  def tailcallChain(): Boolean = value(isEven(N))

  // the state-passing answer, as TestState: `get`/`put` two shifts at the run's delimiter
  type St[H[+_], S, W] = S => Top[H, W]
  final class State[H[+_], W]:
    type Outer = EmptyTuple
    def get[S]: Freer[H, At[H, H, Outer, St[H, S, W]] *: Outer, At[H, H, Outer, St[H, S, W]] *: Outer, S] =
      Freer.Shift0((k: S => Freer[H, Outer, Outer, St[H, S, W]]) => pure((s: S) => k(s).flatMap(f => f(s))))
    def put[S]: PutFrom[S] = PutFrom[S]()
    final class PutFrom[S]:
      def apply[S2](s2: S2): Freer[H, At[H, H, Outer, St[H, S2, W]] *: Outer, At[H, H, Outer, St[H, S, W]] *: Outer, Unit] =
        Freer.Shift0((k: Unit => Freer[H, Outer, Outer, St[H, S2, W]]) => pure((_: S) => k(()).flatMap(f => f(s2))))
  def runState[H[+_], S0, S1, W](s0: S0)(body: State[H, W] => Freer[H, At[H, H, EmptyTuple, St[H, S1, W]] *: EmptyTuple, At[H, H, EmptyTuple, St[H, S0, W]] *: EmptyTuple, W]): Top[H, W] =
    Freer.Reset[H, H, EmptyTuple, EmptyTuple, St[H, S1, W], St[H, S0, W]](body(State()).map(a => (_: S1) => pure(a))).flatMap(f => f(s0))

  /** M times get then set, as master's `stateEffect`; one `get` at the end for the value */
  @Benchmark
  def stateAtm(): Long =
    value(runState(0L): st =>
      (1 to M).foldLeft(st.put[Long](0L)): (m, _) =>
        m.flatMap(_ => st.get[Long].flatMap(s => st.put[Long](s + 1)))
      .flatMap(_ => st.get[Long]))

  @Param(Array("1", "16", "256"))
  var depth: Int = 0

  @Param(Array("1", "8"))
  var shots: Int = 0

  @Benchmark
  def delimCaptureDepth(): Int =
    value(reset[Pure, Int] { in ?=>
      def callK(k: Unit => Freer[Pure, in.D, in.D, Int], n: Int, acc: Int): Freer[Pure, in.D, in.D, Int] =
        if n == 0 then pure(acc) else k(()).flatMap(r => callK(k, n - 1, acc + r))
      var p: in.Body[Int] = shift0[Unit](k => callK(k, shots, 0)).map(_ => 1)
      var i = 0
      while i < depth do
        p = p.map(_ + 1)
        i += 1
      p
    })
}
