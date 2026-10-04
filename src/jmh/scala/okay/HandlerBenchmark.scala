package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

import !.*
import scala.annotation.{nowarn, tailrec}

/** an operation carrying its own answer, for handler benchmarks */
// `derives Effect`: since f9417643 (2026-09-08) every signature declares
// its own runtime test — the erasure fallback is gone — and this lane's
// relay/handle split needs one. Found by handler-fusion-gate: the
// landing that removed the fallback did not reach the Jmh configuration.
case class Ask[+A](a: A) derives Effect

/**
 * The previously unbenchmarked paths: tail-resumptive relay vs the
 * general Effects.handle on the same forwarding-heavy program; bulk
 * vs one-by-one stepping (the reflection-without-remorse probe that
 * makes the type-aligned-queue idea falsifiable); and the State
 * effect handler vs the parameterised PState on the same workload.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class HandlerBenchmark {

  final val N = 10000

  /** 10k ops, every 100th handled (Ask), the rest forwarded (Produce) */
  def prog: Int ! Ask + Produce =
    (1 to N).foldLeft(effect[Ask + Produce, Int](Ask(0))): (m, i) =>
      m.flatMap(x => effect[Ask + Produce, Int](if i % 100 == 0 then Ask(x + 1) else x + 1))

  // relay/handle inline a type test on the operation type, which
  // erasure cannot verify for Ask[Nothing] — the trusted kernel's
  // warning (Effects.scala), not a cast this file adds
  @nowarn("msg=cannot be checked at runtime")
  @Benchmark
  def relayForward(): Int =
    relay[Int, Int, Ask, Produce](prog)(pure(_))([X, Y] => a => Cont.Pure(a.a)).runWith

  /**
   * relayForward's `prog` is a `def`, so that lane BUILDS a 10 000-node
   * Free tree on every invocation and then relays it. These two split
   * the measurement, because a delta on the first is not a delta in
   * any handler: `buildOnly` is construction alone, `relayPrebuilt`
   * relays a tree built once (a program is a value here — relaying the
   * same one twice is exactly the contract, not a trick).
   */
  @Benchmark
  def buildOnly(): Any = prog

  private var built: Int ! Ask + Produce = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def buildOnce(): Unit = built = prog

  @nowarn("msg=cannot be checked at runtime")
  @Benchmark
  def relayPrebuilt(): Int =
    relay[Int, Int, Ask, Produce](built)(pure(_))([X, Y] => a => Cont.Pure(a.a)).runWith

  // relay/handle inline a type test on the operation type, which
  // erasure cannot verify for Ask[Nothing] — the trusted kernel's
  // warning (Effects.scala), not a cast this file adds
  @nowarn("msg=cannot be checked at runtime")
  @Benchmark
  def handleForward(): Int =
    Effects[Free].handle[Ask, Produce](prog)(pure(_))([X] => a => Cont.Pure(a.a)).runWith

  /**
   * `handleForward` builds its 10 000-node tree on every invocation,
   * exactly as `relayForward` used to before `relayPrebuilt` split
   * the two. Without this lane the documented "relay is 1.45x faster
   * than handle" compares one number that includes construction with
   * another that includes construction, and neither says what the
   * HANDLER costs — which is the only part either of them controls.
   * Same pre-built tree as `relayPrebuilt`, so the pair differs in
   * nothing but the handler.
   */
  @nowarn("msg=cannot be checked at runtime")
  @Benchmark
  def handlePrebuilt(): Int =
    Effects[Free].handle[Ask, Produce](built)(pure(_))([X] => a => Cont.Pure(a.a)).runWith

  /**
   * THE SHAPE `delay-node` IS ABOUT (specs/core-cleanup.md Decisions):
   * a handler that CAPTURES its continuation, so `handle` cannot go on
   * from an answer and reifies the rest of the program under a
   * `Defer` whose continuation is `Pure`. That node rotates into a
   * left-nested `Bind`, and the `.flatMap(pure)` tail then travels
   * down every one of the ~100 forwarded operations that follow each
   * capture. `hff-defer-cost` priced the shape at 210 vs 151 µs when
   * EVERY handled operation took it; this lane keeps it only where a
   * capture forces it, which is what ships. Same pre-built tree as
   * `handlePrebuilt`, so the pair differs in nothing but the capture.
   */
  @nowarn("msg=cannot be checked at runtime")
  @Benchmark
  def handleCapture(): Int =
    Effects[Free].handle[Ask, Produce](built)(pure(_))([X] => a => Cont.shift(k => k(a.a))).runWith

  /** the other road to the same node: `!.tailcall` between two
   * mutually recursive functions, N deep — every hop WAS a
   * `Defer(thunk, pure)` before delay-node, and is one `Delay` now */
  def isEven(n: Int): Boolean ! okay.Pure =
    if n == 0 then pure(true) else !.tailcall(isOdd(n - 1))
  def isOdd(n: Int): Boolean ! okay.Pure =
    if n == 0 then pure(false) else !.tailcall(isEven(n - 1))

  @Benchmark
  def tailcallChain(): Boolean = !.run(isEven(N))

  @Benchmark
  def stepBulk(): Any =
    fibs[Int, Producer].next(N).peek

  @Benchmark
  def stepOneByOne(): Any =
    var p = fibs[Int, Producer]
    var i = 0
    while i < N do
      p = p.next(1)
      i += 1
    p.peek

  final val M = 1000

  @Benchmark
  def stateEffect(): (Long, Long) =
    State.run(0L):
      (1 to M).foldLeft(0L.state[Long]): (m, _) =>
        m.flatMap(_ => State.get[Long].flatMap(s => State.set[Long](s + 1)))

  /** handle-frames: a handler's run per CALL — 100 small `State.run`s, two operations each — prices what a
   * handler costs to start (since handle-frames a `Delay` and a `Handled` before its loop), not per operation */
  @Benchmark
  def stateSmall(): Long =
    var acc = 0L
    var i = 0
    while i < 100 do
      acc += State.run(i.toLong)(State.get[Long].flatMap(s => State.set[Long](s + 1)))._1
      i += 1
    acc

  /** a plain answer-using body, `k(x + 1) + 1`, M levels: since
   * cont-stack-layer1-b walked by the runner (a `Call` and a pending
   * part per level) instead of a frame per level */
  @Benchmark
  def contAnswer(): Int =
    Cont.reset((1 to M).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => Cont.shift[Int, Int, Int](k => k(x + 1) + 1))))

  @Benchmark
  def statePara(): (Long, Long) =
    PState.run(0L):
      (1 to M).foldLeft(PState.get[Long, (Long, Long)]): (m, _) =>
        m.flatMap(_ => PState.get.flatMap(s => PState.set(s + 1)))

  /** the SAME M-step workload as `statePara`, the state moved by the
   * indexed DATA signature and threaded by the tail-recursive loop
   * (pstate-threaded): typestate at `stateEffect`'s cost, or not */
  @Benchmark
  def stateThreaded(): (Long, Long) =
    PState.Threaded.run(
      (1 to M).foldLeft(PState.Threaded.get[Long]): (m, _) =>
        m.flatMap(_ => PState.Threaded.get[Long].flatMap(s => PState.Threaded.put[Long, Long](s + 1))))(0L)

  // ------------------------------------------------------------------
  // splitI AGAINST split ON A FORWARDING HANDLER (indexed-effects-measure-2,
  // the row's deferred question, specs/indexed-effects.md stage 3). The
  // SAME M-step get/set workload with every 10th step an `Ask` the state
  // handler does not own: `stateForward` is `State.handle` over the unary
  // row `State % Long + Ask`, forwarding through `split`; `stateIndexedForward`
  // is `State.handleIndexed` over the indexed row `Unary[Ask] +~
  // Unary[State[Long, *]]`, forwarding through `splitI`. Both residues are
  // an `Ask` tree run by the same tail loop, so the pair prices the two
  // helpers and the two handlers' loops and nothing else. Both programs
  // are VALUES built once (a program is a value; `relayPrebuilt`'s rule).

  type FRow = State % Long + Ask
  val forwardProg: Long ! FRow =
    (1 to M).foldLeft(effect[FRow, Long](State.Get[Long, Long]())): (m, i) =>
      m.flatMap(_ => effect[FRow, Long](State.Get[Long, Long]()).flatMap(s => effect[FRow, Long](State.Update[Long, Long](_ => (s + 1, s + 1)))))
       .flatMap(s => if i % 10 == 0 then effect[FRow, Long](Ask(s)) else pure[FRow, Long](s))

  type IRow = Unary[Ask] +~ Unary[State[Long, *]]
  // a match-typed member erases to no class `derived` could read: the
  // constant-class test written by hand (TypeableI's note)
  given TypeableI[Unary[Ask]] = new TypeableI[Unary[Ask]] { def test(x: Any): Boolean = x.isInstanceOf[Ask[?]] }
  val indexedForwardProg: Freer[IRow, Unit, Unit, Long] =
    (1 to M).foldLeft(Indexed.unary[IRow, Unit, Long](State.Get[Long, Long]())): (m, i) =>
      m.flatMap(_ => Indexed.unary[IRow, Unit, Long](State.Get[Long, Long]()).flatMap(s => Indexed.unary[IRow, Unit, Long](State.Update[Long, Long](_ => (s + 1, s + 1)))))
       .flatMap(s => if i % 10 == 0 then Indexed.unary[IRow, Unit, Long](Ask(s)) else Indexed.pure[IRow, Unit, Long](s))

  /** the residue's runner, over the tree's own nodes: a forwarded `Ask`
   * is an `Inject` (the unary handler re-emits it so; the indexed one
   * forwards the node it came in), and `Ask(a)` answers `a` in place.
   * `Free.Bind` pins the bind's middle index to `Unit` (Free.scala: the
   * tree's constant claim), which the enum's own extractor leaves
   * existential. */
  @tailrec private def askLoop[A](x: A ! Ask): A = (x.resume: @unchecked) match
    case Free.Return(a) => a
    case Free.Inject(g) => g.a
    case Free.Bind(Free.Inject(g), k) => askLoop(k(g.a))

  @Benchmark
  def stateForward(): (Long, Long) = askLoop(State.handle(0L)(forwardProg))

  /** the residue of `handleIndexed` at every index `Unit` IS an `Ask`
   * tree (`Unary[Ask][Unit, Unit, X]` reduces to `Ask[X]`, `Unary[Ask]`'s
   * shape); the cast names the two spellings of one runtime tree, and is
   * the one this lane adds — the handler has none */
  @Benchmark
  def stateIndexedForward(): (Long, Long) =
    askLoop(State.handleIndexed(0L)(indexedForwardProg).asInstanceOf[(Long, Long) ! Ask])
}
