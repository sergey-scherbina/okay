package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.Direct.*
import okay.Rowlift.{at, plus}

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

  // ---- the LOSING lanes keep the OLD witness on purpose.
  //
  // `Rowlift.In` is an opaque `Unit` and carries no `inj`, because the
  // shipping route does not need one. `liftAt` and `atWalk` do: they
  // re-inject the operation, so they need a witness that can be
  // CALLED. This is that shape, kept here and only here, as the guard
  // on the table above — the three casts below are what the shipping
  // design no longer pays.
  trait In[F[+_], G[+_]]:
    def inj[A](fa: F[A]): G[A]

  // inj is identity at every instance, so one cached object serves
  // them all; a parameterised `given` is a METHOD and would allocate a
  // fresh witness per use site.
  private object IdIn extends In[[A] =>> Any, [A] =>> Any]:
    def inj[A](fa: Any): Any = fa

  trait InLow:
    given self[F[+_]]: In[F, F] = IdIn.asInstanceOf[In[F, F]]

  object In extends InLow:
    given left[F[+_], G[+_]]: In[F, F + G] = IdIn.asInstanceOf[In[F, F + G]]
    given deeper[F[+_], G[+_], H[+_]](using i: In[F, G]): In[F, G + H] =
      IdIn.asInstanceOf[In[F, G + H]]

  /** on the OPERATION: constructs at R, nothing to rebuild */
  extension [F[+_], A](fa: F[A])
    inline def liftAt[G[+_]](using i: In[F, G]): A ! G = effect[G, A](i.inj(fa))

  /** on the PROGRAM, by walking: the postfix done the expensive way */
  // NOT inline: it recurses over the tree, and an inline recursion
  // does not terminate at compile time (found by trying)
  extension [A, F[+_]](p: A ! F)
    def atWalk[G[+_]](using i: In[F, G]): A ! G =
      import okay.!.*
      (p.resume: @unchecked) match
        case Pure(a) => Free.Pure(a)
        case Effect(e) => Free.inject(i.inj(e))
        case Bind(Effect(e), k) => Free.inject(i.inj(e)).flatMap(x => k(x).atWalk[G])

  /** the floor: construct at R directly */
  @Benchmark
  def viaEffect(): Int =
    var m: Int ! R = effect[R, Int](State.Get())
    var i = 1
    while i < N do
      m = m.flatMap(_ => effect[R, Int](State.Get()))
      i += 1
    State.run[Int, Int](0)(Writer.run[String, Int, State % Int](m).map(_._2))._2

  /** the operation-level lift */
  @Benchmark
  def viaLiftAt(): Int =
    var m: Int ! R = (State.Get[Int, Int](): State[Int, Int]).liftAt[R]
    var i = 1
    while i < N do
      m = m.flatMap(_ => (State.Get[Int, Int](): State[Int, Int]).liftAt[R])
      i += 1
    State.run[Int, Int](0)(Writer.run[String, Int, State % Int](m).map(_._2))._2

  /** the program-level lift — the ergonomic one under test */
  @Benchmark
  def viaAtWalk(): Int =
    var m: Int ! R = State.get[Int].atWalk[R]
    var i = 1
    while i < N do
      m = m.flatMap(_ => State.get[Int].atWalk[R])
      i += 1
    State.run[Int, Int](0)(Writer.run[String, Int, State % Int](m).map(_._2))._2

  /** today's spelling: widen, naming the complement */
  @Benchmark
  def viaWiden(): Int =
    var m: Int ! R = !.widen[Int, State % Int, Writer % String](State.get[Int])
    var i = 1
    while i < N do
      m = m.flatMap(_ =>
        !.widen[Int, State % Int, Writer % String](State.get[Int]))
      i += 1
    State.run[Int, Int](0)(Writer.run[String, Int, State % Int](m).map(_._2))._2

  /** direct style: the macro reads the row off the block's expected
   * type and emits Free.Inject AT it — no widen, no evidence, no
   * intermediate node. The candidate for "zero cost AND ergonomic". */
  @Benchmark
  def viaDirect(): Int =
    var m: Int ! R = direct { State.Get[Int, Int]().!? }
    var i = 1
    while i < N do
      m = m.flatMap(_ => direct { State.Get[Int, Int]().!? })
      i += 1
    State.run[Int, Int](0)(Writer.run[String, Int, State % Int](m).map(_._2))._2

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
    State.run[Int, Int](0)(Writer.run[String, Int, State % Int](m).map(_._2))._2

  /** the same cast, naming only what is ADDED rather than the whole
   * target row — the spelling a helper wants */
  @Benchmark
  def viaPlus(): Int =
    var m: Int ! R = State.get[Int].plus[Writer % String]
    var i = 1
    while i < N do
      m = m.flatMap(_ => State.get[Int].plus[Writer % String])
      i += 1
    State.run[Int, Int](0)(Writer.run[String, Int, State % Int](m).map(_._2))._2
}
