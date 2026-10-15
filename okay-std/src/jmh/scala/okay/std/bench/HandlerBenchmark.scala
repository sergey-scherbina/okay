package okay.std.bench

import okay.{!, +, %, Effect, Handler, Pure, effect, pure, tailcall}
import okay.std.*
import okay.given
import okay.std.given

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/** an operation carrying its own answer, for handler benchmarks */
case class Ask[+A](a: A) derives Effect

/**
 * The program lanes of the classic's HandlerBenchmark on the new road: the same forwarding-heavy program handled
 * (an answer in place, and a capture), its build alone, a tailcall chain, stepping a producer, and State and
 * Writer over M operations. The classic's `relay*` (okay-freer's relay), `*Cps` (the CPS carrier), `contAnswer*`
 * (Cps alone), `statePara`/`stateThreaded` (PState: Cps and freer's indexed tree) and `stateIndexedForward`
 * (`handleIndexed`) have no counterpart.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class HandlerBenchmark {

  final val N = 10000

  type AP = Ask + Produce

  /** 10k ops, every 100th handled (Ask), the rest forwarded (Produce) */
  def prog: Int ! AP =
    (1 to N).foldLeft(effect[Ask, Int](Ask(0)).at[AP]): (m, i) =>
      m.flatMap(x => if i % 100 == 0 then effect[Ask, Int](Ask(x + 1)).at[AP] else produce(x + 1).at[AP])

  /** Ask answered in place (the classic's `FreeEffects.control.pure(a.a)`: no capture) */
  val askAnswer: Handler[Ask, [A] =>> A] = Handler.answering[Ask]([X] => (a: Ask[X]) => a.a)

  /** Ask answered by resuming its captured `k` (the classic's `FreeEffects.control.shift(k => k(a.a))`) */
  val askCapture: Handler[Ask, [A] =>> A] =
    Handler.deep[Ask, [A] =>> A]([A] => (a: A) => a)([A, X] => (op: Ask[X], k: X => A ! Pure) => k(op.a))

  /** construction alone (a program is a value: on this road one `Run` node a bind, not a tree node) */
  @Benchmark
  def buildOnly(): Any = prog

  private var built: Int ! AP = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def buildOnce(): Unit = built = prog

  /** the Ask handler over `prog` built per call, Produce forwarded and answered outside by `Answers[Produce]` */
  @Benchmark
  def handleForward(): Int =
    okay.std.Stream.runWith(prog.handle(askAnswer))

  /** the same over the tree built once */
  @Benchmark
  def handlePrebuilt(): Int =
    okay.std.Stream.runWith(built.handle(askAnswer))

  /** the same, the handler capturing `k` at every Ask */
  @Benchmark
  def handleCapture(): Int =
    okay.std.Stream.runWith(built.handle(askCapture))

  /** `tailcall` between two mutually recursive functions, N deep */
  def isEven(n: Int): Boolean ! Pure =
    if n == 0 then pure(true) else tailcall(isOdd(n - 1))
  def isOdd(n: Int): Boolean ! Pure =
    if n == 0 then pure(false) else tailcall(isEven(n - 1))

  @Benchmark
  def tailcallChain(): Boolean = isEven(N).run

  private val St = summon[okay.std.Stream[Producer, Pure]]

  /** NEAREST EQUIVALENT: the classic steps N operations by its `Answers` (`next(N)`, freer's `resume` loop) and
   * peeks; here the producer is walked N elements by the std's linear view (the kernel's `Step`) and the next read */
  @Benchmark
  def stepBulk(): Any =
    St.iterator(fibs[Int, Producer]).drop(N).next()

  /** NEAREST EQUIVALENT: the classic's `p = p.next(1)` N times, a program value per step; here `uncons` N times,
   * the rest handed out each time (a stepped program's rest, re-stepped), then the next element */
  @Benchmark
  def stepOneByOne(): Any =
    var p = fibs[Int, Producer]
    var i = 0
    while i < N do
      p = St.uncons(p).run.get._2
      i += 1
    St.uncons(p).run.get._1

  final val M = 1000

  @Benchmark
  def stateEffect(): (Long, Long) =
    State.run(0L):
      (1 to M).foldLeft(0L.state[Long]): (m, _) =>
        m.flatMap(_ => State.get[Long].flatMap(s => State.set[Long](s + 1)))

  /** M tells under the Writer handler */
  @Benchmark
  def writerTell(): Int =
    Writer.run[Int, Int, Writer % Int]((1 to M).foldLeft(Writer.tell(0).map(_ => 0))((m, i) => m.flatMap(_ => Writer.tell(i).map(_ => i)))).run._1.length

  /** `stateSmall` through `p.handle(State(s))`: what a single `handle` costs to start */
  @Benchmark
  def stateSmallHandle(): Long =
    var acc = 0L
    var i = 0
    while i < 100 do
      acc += State.get[Long].flatMap(s => State.set[Long](s + 1)).handle(State(i.toLong)).run._1
      i += 1
    acc

  @Benchmark
  def stateSmall(): Long =
    var acc = 0L
    var i = 0
    while i < 100 do
      acc += State.run(i.toLong)(State.get[Long].flatMap(s => State.set[Long](s + 1)))._1
      i += 1
    acc

  // the SAME M-step get/set workload with every 10th step an `Ask` the state handler does not own, forwarded

  type FRow = State % Long + Ask
  val forwardProg: Long ! FRow =
    (1 to M).foldLeft(effect[State % Long, Long](State.Get[Long, Long]()).at[FRow]): (m, i) =>
      m.flatMap(_ => effect[State % Long, Long](State.Get[Long, Long]()).at[FRow].flatMap(s =>
          effect[State % Long, Long](State.Update[Long, Long](_ => (s + 1, s + 1))).at[FRow]))
       .flatMap(s => if i % 10 == 0 then effect[Ask, Long](Ask(s)).at[FRow] else pure[Long, FRow](s))

  /** NEAREST EQUIVALENT for the residue: the classic runs the forwarded `Ask` tree by a tail loop over freer's
   * nodes (`askLoop`, `Ask(a)` answers `a`); here by `Handler.answering` — the same answer, the kernel's handler */
  @Benchmark
  def stateForward(): (Long, Long) = State.handle(0L)(forwardProg).handle(askAnswer).run
}
