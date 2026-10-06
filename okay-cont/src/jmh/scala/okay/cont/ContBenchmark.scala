package okay.cont

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import Cont.*
import Machine.value
import okay.cont.State as SE

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
class ContBenchmark {

  final val N = 10000
  final val M = 1000

  /** an operation's own value: the inner handler, of `Ask`, leaving `Tick` — the general clause, a capture an op */
  val askH: Handler[Ask, Int, Int] = new Handler[Ask, Int, Int]:
    def ret(a: Int): Int = a
    def apply[X, Oc <: Ctx](using o: Oc)(op: Ask[X], k: X => Cont[o.Here, o.Here, Int]): Cont[o.Here, o.Here, Int] =
      op match
        case Ask.Value(a) => k(a)
  /** the outer handler, of `Tick`, leaving nothing — the general clause */
  val tickH: Handler[Tick, Int, Int] = new Handler[Tick, Int, Int]:
    def ret(a: Int): Int = a
    def apply[X, Oc <: Ctx](using o: Oc)(op: Tick[X], k: X => Cont[o.Here, o.Here, Int]): Cont[o.Here, o.Here, Int] =
      op match
        case Tick.Value(a) => k(a)
  /** the same two, declared TAIL-RESUMPTIVE: answered in place, no capture */
  val askA: Answering[Ask, Int, Int] = new Answering[Ask, Int, Int]:
    def ret(a: Int): Int = a
    def value[X](op: Ask[X]): X = op match
      case Ask.Value(a) => a
  val tickA: Answering[Tick, Int, Int] = new Answering[Tick, Int, Int]:
    def ret(a: Int): Int = a
    def value[X](op: Tick[X]): X = op match
      case Tick.Value(a) => a

  /** the program of the two handlers' body: 10k ops, every 100th handled by the inner (Ask), the rest forwarded (Tick) */
  def body[Oc <: Ctx](using c: In[Int, Oc], pa: Perform[Ask, c.type], pt: Perform[Tick, c.type]): c.Body[Int] =
    var p: c.Body[Int] = perform(Ask.Value(0))
    var i = 1
    while i <= N do
      val j = i
      p = p.flatMap(x => if j % 100 == 0 then perform(Ask.Value(x + 1)) else perform(Tick.Value(x + 1)))
      i += 1
    p

  def prog: Top[Int] = handle[Tick, Int, Int](tickH)(handle[Ask, Int, Int](askH)(body))
  /** the same program under the answering handlers */
  def progA: Top[Int] = handle[Tick, Int, Int](tickA)(handle[Ask, Int, Int](askA)(body))

  @Benchmark
  def buildOnly(): Any = prog

  @Benchmark
  def handleForward(): Int = value(prog)

  private var built: Top[Int] = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def buildOnce(): Unit = built = prog

  @Benchmark
  def handlePrebuilt(): Int = value(built)

  private var builtA: Top[Int] = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def buildOnceA(): Unit = builtA = progA

  /** the same 10 000 operations, the handlers answering in place: no capture, no forwarding capture */
  @Benchmark
  def handlePrebuiltAnswering(): Int = value(builtA)

  def isEven(n: Int): Top[Boolean] = if n == 0 then pure(true) else delay(isOdd(n - 1))
  def isOdd(n: Int): Top[Boolean] = if n == 0 then pure(false) else delay(isEven(n - 1))

  @Benchmark
  def tailcallChain(): Boolean = value(isEven(N))

  // the state-passing answer, as TestState: `get`/`put` two shifts at the run's delimiter
  type St[S, W] = S => Top[W]
  final class State[W]:
    type Outer = EmptyTuple
    def get[S]: Cont[At[Outer, St[S, W]] *: Outer, At[Outer, St[S, W]] *: Outer, S] =
      Cont.Shift0((k: S => Cont[Outer, Outer, St[S, W]]) => pure((s: S) => k(s).flatMap(f => f(s))))
    def put[S]: PutFrom[S] = PutFrom[S]()
    final class PutFrom[S]:
      def apply[S2](s2: S2): Cont[At[Outer, St[S2, W]] *: Outer, At[Outer, St[S, W]] *: Outer, Unit] =
        Cont.Shift0((k: Unit => Cont[Outer, Outer, St[S2, W]]) => pure((_: S) => k(()).flatMap(f => f(s2))))
  def runState[S0, S1, W](s0: S0)(body: State[W] => Cont[At[EmptyTuple, St[S1, W]] *: EmptyTuple, At[EmptyTuple, St[S0, W]] *: EmptyTuple, W]): Top[W] =
    Cont.Reset[EmptyTuple, EmptyTuple, St[S1, W], St[S0, W]](body(State()).map(a => (_: S1) => pure(a))).flatMap(f => f(s0))

  /** M times get then set, as master's `stateEffect`; one `get` at the end for the value */
  @Benchmark
  def stateAtm(): Long =
    value(runState(0L): st =>
      (1 to M).foldLeft(st.put[Long](0L)): (m, _) =>
        m.flatMap(_ => st.get[Long].flatMap(s => st.put[Long](s + 1)))
      .flatMap(_ => st.get[Long]))

  /** M times get then put, answered from the cell (master: `stateEffect`, the handler) */
  @Benchmark
  def stateAnswering(): Long =
    def body(using c: In[(Long, Long), ?], p: Perform[[X] =>> SE[Long, X], c.type]): c.Body[Long] =
      var m: c.Body[Unit] = perform(SE.Put(0L))
      var i = 0
      while i < M do
        m = m.flatMap(_ => perform(SE.Get[Long]()).flatMap(s => perform(SE.Put(s + 1))))
        i += 1
      m.flatMap(_ => perform(SE.Get[Long]()))
    value(state[Long, Long](0L)(body))._1

  @Param(Array("1", "16", "256"))
  var depth: Int = 0

  @Param(Array("1", "8"))
  var shots: Int = 0

  @Benchmark
  def delimCaptureDepth(): Int =
    value(reset[Int] { in ?=>
      def callK(k: Unit => Cont[in.D, in.D, Int], n: Int, acc: Int): Cont[in.D, in.D, Int] =
        if n == 0 then pure(acc) else k(()).flatMap(r => callK(k, n - 1, acc + r))
      var p: in.Body[Int] = shift0[Unit](k => callK(k, shots, 0)).map(_ => 1)
      var i = 0
      while i < depth do
        p = p.map(_ + 1)
        i += 1
      p
    })
}
