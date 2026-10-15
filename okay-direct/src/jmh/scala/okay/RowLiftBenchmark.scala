package okay

import okay.std.*
import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.Direct.*
import okay.Row.plus

/**
 * What does attaching a row COST?
 *
 * A constructor builds at its own row; a program usually has a wider
 * one. Six spellings move an operation across, and this asks which of
 * them are free. Read B/op (-prof gc) before us/op: allocation does
 * not drift with host load the way time does, and this box is not
 * quiet.
 *
 * MEASURED, 2026-09-08 (N=1000, so a 16-byte node per operation shows
 * up as 16 000 B/op):
 *
 *   viaEffect    272 016   construct AT the row -- the floor
 *   viaDirect    272 016   the macro emits Inject at the block's row
 *   viaAt        272 016   cast under the In witness -- AT THE FLOOR
 *   viaLiftAt    288 000   lift the operation, +1 node per op
 *   viaWiden     288 016   today's spelling, +1 node per op
 *   viaAtWalk    304 000   the same postfix done by walking, +2
 *
 * So the ergonomic spelling and the cheap one are the same spelling.
 * The second Inject that `.atWalk` and `widen` build does NOT get
 * scalarised away by escape analysis -- that hypothesis was tested
 * here and refuted; it survives into the tree and costs bytes.
 *
 * This does NOT make `widen` redundant. Its walk is also a
 * NORMALISATION, and `Source.merge` runs 5-7% slower without it
 * (specs/writer-covariance.md, free-row-variance). `.at` is for single
 * operations, which are already head-normal.
 *
 * ON THE KERNEL (freer-on-core): the table above is okay-freer's. The
 * lanes now run the root's program: `at`/`plus`/`widen` are one `Run`
 * wrapper each, `atWalk` walks by `okay.Walk`, `viaEffect` builds at the
 * operation's own row and places the whole chain once (no constructor
 * builds AT a wider row any more). `viaLiftAt` is removed: it re-injected
 * an operation into a union row through a callable witness, and a kernel
 * row is not a union of operation types — nothing to inject into.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class RowLiftBenchmark {

  final val N = 1000

  type R = State % Int + Writer % String

  /** on the PROGRAM, by walking: the postfix done the expensive way —
   * `Walk.view` to each operation, performed again at `G`. Trampolined:
   * the recursion is in the bind's continuation */
  extension [A, F[_]](p: A ! F)
    def atWalk[G[_]](using w: Within[F, G]): A ! G = Walk.view(p) match
      case Walk.Done(a) => pure[A, G](a)
      case o: Walk.Op[F, x, A, F] => effect(o.op).at[G].flatMap((y: x) => o.resume(y).atWalk[G])

  /** the floor: on the kernel no constructor builds at a wider row, so
   * the chain is built at the operation's own row and placed at R ONCE */
  @Benchmark
  def viaEffect(): Int =
    var m: Int ! State % Int = effect[State % Int, Int](State.Get())
    var i = 1
    while i < N do
      m = m.flatMap(_ => effect[State % Int, Int](State.Get()))
      i += 1
    State.run[Int, Int](0)(Writer.run(m.at[R]).map(_._2))._2

  /** the program-level lift — the ergonomic one under test */
  @Benchmark
  def viaAtWalk(): Int =
    var m: Int ! R = State.get[Int].atWalk[R]
    var i = 1
    while i < N do
      m = m.flatMap(_ => State.get[Int].atWalk[R])
      i += 1
    State.run[Int, Int](0)(Writer.run(m).map(_._2))._2

  /** today's spelling: widen, naming the complement */
  @Benchmark
  def viaWiden(): Int =
    var m: Int ! R = !.widen[Int, State % Int, Writer % String](State.get[Int])
    var i = 1
    while i < N do
      m = m.flatMap(_ =>
        !.widen[Int, State % Int, Writer % String](State.get[Int]))
      i += 1
    State.run[Int, Int](0)(Writer.run(m).map(_._2))._2

  /** direct style: the macro reads the row off the block's expected
   * type and emits Free.Inject AT it — no widen, no evidence, no
   * intermediate node. The candidate for "zero cost AND ergonomic". */
  @Benchmark
  def viaDirect(): Int =
    var m: Int ! R = direct { State.Get[Int, Int]().? }
    var i = 1
    while i < N do
      m = m.flatMap(_ => direct { State.Get[Int, Int]().? })
      i += 1
    State.run[Int, Int](0)(Writer.run(m).map(_._2))._2

  /** the same cast, under a witness that has no runtime existence at
   * all: `In` is an opaque `Unit`, so the givens allocate nothing and
   * the design holds exactly one asInstanceOf */
  @Benchmark
  def viaAt(): Int =
    var m: Int ! R = State.get[Int].at[R]
    var i = 1
    while i < N do
      m = m.flatMap(_ => State.get[Int].at[R])
      i += 1
    State.run[Int, Int](0)(Writer.run(m).map(_._2))._2

  /** the same cast, naming only what is ADDED rather than the whole
   * target row — the spelling a helper wants */
  @Benchmark
  def viaPlus(): Int =
    var m: Int ! R = State.get[Int].plus[Writer % String]
    var i = 1
    while i < N do
      m = m.flatMap(_ => State.get[Int].plus[Writer % String])
      i += 1
    State.run[Int, Int](0)(Writer.run(m).map(_._2))._2
}
