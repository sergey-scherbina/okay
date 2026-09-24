package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.Row.at

/**
 * specs/handler-fusion.md, stage 0 — the gate.
 *
 * What does running a row's handlers ONE AT A TIME cost against
 * running them fused, on the same program? The program is built once
 * per fork (@Setup) and only HANDLING is timed: a Free tree is
 * immutable and `resume` allocates fresh nodes on every walk, so the
 * same tree can be walked by every lane.
 *
 * Read B/op (-prof gc) before us/op. Stage A (split-without-either)
 * moved the fused loops and the shipping runners onto `split`; the
 * `<|>` numbers they replaced are the stage-0 rows in history.tsv. The cost model in the spec says
 * the nested run pays one Bind + one closure per operation per pass
 * that does not own it: on `nestedSW` every State operation once, on
 * `nestedTSW` every State operation once and every Writer operation
 * once more, plus a third walk that owns nothing because the program
 * never raises. The fused lanes should pay none of that. If the µs
 * ratio is under 1.3x the spec's Results record the refutation.
 *
 * N = 1000 operations, get/set/tell in rotation, foldLeft-built (the
 * left-nested shape, so every lane pays the same rotation).
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class FusionBenchmark {

  final val N = 1000

  type SW = State % Int + Writer % String
  type TSW = Throws % String + State % Int + Writer % String

  var sw: Int ! SW = scala.compiletime.uninitialized
  var tsw: Int ! TSW = scala.compiletime.uninitialized
  /** the RIGHT-NESTED twin (docs/benchmarks.md lane rules): the same
   * operations bound by recursion, so `resume` has nothing to rotate
   * and the lanes over it price the rebuild alone */
  var swR: Int ! SW = scala.compiletime.uninitialized

  def rightSW(i: Int, acc: Int): Int ! SW =
    if i >= N then pure(acc)
    else (i % 3) match
      case 0 => State.get[Int].at[SW].flatMap(x => rightSW(i + 1, acc + x))
      case 1 => State.set[Int](i).at[SW].flatMap(x => rightSW(i + 1, acc + x))
      case _ => Writer.tell("w").at[SW].flatMap(_ => rightSW(i + 1, acc + 1))

  // ---- stage B: the same right-nested program as a handler-passing
  // program over a Control carrier. (The `effSWr`/`effSW` lanes — the
  // Church encoding, no tree — went with `Eff` in defer-eff-removal;
  // their numbers are history.tsv rows `effSW*`, 0.58–0.86x of the
  // fused Free loop.)

  type R = Fused.Answer[Int, String, Int]
  def rightCtrl[C[_, _, _]](h: Interpr[SW, C, R])(using C: Control[C])(i: Int, acc: Int): C[Int, R, R] =
    if i >= N then C.pure(acc)
    else (i % 3) match
      case 0 => C.flatMap(h(State.Get()))(x => rightCtrl(h)(i + 1, acc + x))
      case 1 => C.flatMap(h(State.Set(i)))(x => rightCtrl(h)(i + 1, acc + x))
      case _ => C.flatMap(h(Writer.Say("w")))(_ => rightCtrl(h)(i + 1, acc + 1))

  @Benchmark
  def ctrlContSWr(): Int = Fused.runCtrl[Cont, Int, String, Int](0)(h => rightCtrl[Cont](h)(0, 0))._2

  @Benchmark
  def ctrlFuncSWr(): Int = Fused.runCtrl[Func, Int, String, Int](0)(h => rightCtrl[Func](h)(0, 0))._2

  // ---- staged-block-lanes (2026-09-22): a STATIC 10-op block inside a
  // recursive loop — road 2's real shape (continuations-roadmap.md).
  // The same 1 000 operations as rightSW, ten per iteration: inside an
  // iteration nothing is dynamic, so a program over Func can
  // beta-reduce the ten binds at compile time; the recursion between
  // iterations is the one bind no staging removes. The block is spelled
  // as explicit flatMaps, not a for-comprehension, so the tree and the
  // carrier versions have the SAME bind structure to the node.

  final val Iters = N / 10

  def blockFree(i: Int, acc: Int): Int ! SW =
    if i >= Iters then pure(acc)
    else
      State.get[Int].at[SW].flatMap(a =>
      State.set[Int](i).at[SW].flatMap(_ =>
      Writer.tell("w").at[SW].flatMap(_ =>
      State.get[Int].at[SW].flatMap(b =>
      State.set[Int](i + 1).at[SW].flatMap(_ =>
      Writer.tell("w").at[SW].flatMap(_ =>
      State.get[Int].at[SW].flatMap(c =>
      State.set[Int](i + 2).at[SW].flatMap(_ =>
      Writer.tell("w").at[SW].flatMap(_ =>
      State.get[Int].at[SW].flatMap(d =>
        blockFree(i + 1, acc + a + b + c + d)))))))))))

  var blockR: Int ! SW = scala.compiletime.uninitialized

  /** the block with the handler passed as a VALUE: the binds are static
   * (C is concrete at each caller below, so Control[Func]'s inline
   * flatMap reduces), the operation is still dispatched at run time by
   * `split` inside `h` — what a `direct` block emitted over Control
   * with an opaque handler would be */
  // `inline h`: a caller that passes a runtime value passes its name
  // through unchanged; a caller that passes the inline interpreter
  // itself gets the lambda literal at every operation, beta-reduced
  inline def block10[C[_, _, _]](inline h: Interpr[SW, C, R], C: Control[C])(i: Int, acc: Int)
                                (inline next: Int => C[Int, R, R]): C[Int, R, R] =
    C.flatMap(h(State.Get()))(a =>
    C.flatMap(h(State.Set(i)))(_ =>
    C.flatMap(h(Writer.Say("w")))(_ =>
    C.flatMap(h(State.Get()))(b =>
    C.flatMap(h(State.Set(i + 1)))(_ =>
    C.flatMap(h(Writer.Say("w")))(_ =>
    C.flatMap(h(State.Get()))(c =>
    C.flatMap(h(State.Set(i + 2)))(_ =>
    C.flatMap(h(Writer.Say("w")))(_ =>
    C.flatMap(h(State.Get()))(d => next(acc + a + b + c + d)))))))))))

  def blockFunc(h: Interpr[SW, Func, R])(i: Int, acc: Int): Func[Int, R, R] =
    if i >= Iters then Control[Func].pure(acc)
    else block10[Func](h, Control[Func])(i, acc)(acc2 => blockFunc(h)(i + 1, acc2))

  def blockCont(h: Interpr[SW, Cont, R])(i: Int, acc: Int): Cont[Int, R, R] =
    if i >= Iters then Control[Cont].pure(acc)
    else block10[Cont](h, Control[Cont])(i, acc)(acc2 => blockCont(h)(i + 1, acc2))

  /** the CEILING: binds AND handler static — each operation written as
   * its shift directly (the arms of `Fused.stateWriterInterp`, chosen
   * at compile time instead of by `split` at run time), which is what a
   * macro could emit only when it sees the handler at the call site.
   * Nothing is dispatched; if this does not clear the bar, no emission
   * target can. */
  def blockFuncStaged(i: Int, acc: Int): Func[Int, R, R] =
    if i >= Iters then Control[Func].pure(acc)
    else
      val C = Control[Func]
      inline def get: Func[Int, R, R] = C.shift[Int, R, R](k => st => k(st._1)(st))
      inline def set(s2: Int): Func[Int, R, R] = C.shift[Int, R, R](k => st => k(s2)((s2, st._2)))
      inline def say: Func[Unit, R, R] = C.shift[Unit, R, R](k => st => k(())((st._1, st._2 :+ "w")))
      C.flatMap(get)(a =>
      C.flatMap(set(i))(_ =>
      C.flatMap(say)(_ =>
      C.flatMap(get)(b =>
      C.flatMap(set(i + 1))(_ =>
      C.flatMap(say)(_ =>
      C.flatMap(get)(c =>
      C.flatMap(set(i + 2))(_ =>
      C.flatMap(say)(_ =>
      C.flatMap(get)(d => blockFuncStaged(i + 1, acc + a + b + c + d)))))))))))

  /** direct-staged's decisive lane: the SAME block10 as blockFunc, but
   * the handler is the inline interpreter applied at each operation —
   * `split` and the constructor match run on a `State.Get()` built two
   * lines up, in one method body. What a `direct` block over a Func
   * carrier with an INLINE handler given would compile to, with no
   * change to how operations are spelled. The question is whether C2
   * folds the test and the match on a fresh, non-escaping operation:
   * at `blockFuncStagedR` it does the whole job; at `blockFuncR` the
   * spelling of operations must change. */
  def blockFuncInlineH(i: Int, acc: Int): Func[Int, R, R] =
    if i >= Iters then Control[Func].pure(acc)
    else block10[Func](Fused.stateWriterInterp[Func, Int, String, Int], Control[Func])(i, acc)(
      acc2 => blockFuncInlineH(i + 1, acc2))

  @Benchmark
  def blockFuncInlineHR(): Int =
    val C = Control[Func]
    (C./(blockFuncInlineH(0, 0))(a => st => (st, a)))((0, Vector.empty))._2

  /** the row's staged interpreter as an INLINE MATCH on the operation:
   * the scrutinee at every use is a constructor application written
   * two lines up (`State.Get()`), so the compiler — not the JIT —
   * picks the arm; no `split`, no test, no match survives to
   * bytecode if the reduction happens. This is the shape a `direct`
   * block could target with the operations spelled exactly as today. */
  inline def stageSW[X](inline e: SW[X]): Func[X, R, R] =
    val C = Control[Func]
    inline e match
      case State.Get() => C.shift[X, R, R](k => st => k(st._1)(st))
      case State.Set(s2) => C.shift[X, R, R](k => st => k(s2)((s2, st._2)))
      case Writer.Say(v) => C.shift[X, R, R](k => st => k(())((st._1, st._2 :+ v)))

  // `stageSW` applied to the operation TERM at each mark — what the
  // macro would emit. Not through block10's `h`: a polymorphic lambda
  // `[X] => e => stageSW(e)` is not beta-reduced before the inline
  // match is tried, and the match then fails to reduce on `e`
  // ("cannot reduce inline match with scrutinee: e") — the compiler
  // says so, which is the useful part
  def blockFuncInlineMatch(i: Int, acc: Int): Func[Int, R, R] =
    if i >= Iters then Control[Func].pure(acc)
    else
      val C = Control[Func]
      C.flatMap(stageSW(State.Get()))(a =>
      C.flatMap(stageSW(State.Set(i)))(_ =>
      C.flatMap(stageSW(Writer.Say("w")))(_ =>
      C.flatMap(stageSW(State.Get()))(b =>
      C.flatMap(stageSW(State.Set(i + 1)))(_ =>
      C.flatMap(stageSW(Writer.Say("w")))(_ =>
      C.flatMap(stageSW(State.Get()))(c =>
      C.flatMap(stageSW(State.Set(i + 2)))(_ =>
      C.flatMap(stageSW(Writer.Say("w")))(_ =>
      C.flatMap(stageSW(State.Get()))(d => blockFuncInlineMatch(i + 1, acc + a + b + c + d)))))))))))

  @Benchmark
  def blockFuncInlineMatchR(): Int =
    val C = Control[Func]
    (C./(blockFuncInlineMatch(0, 0))(a => st => (st, a)))((0, Vector.empty))._2

  /** the tree, prebuilt: the walk alone (the fused fixture's floor on this shape) */
  @Benchmark
  def blockFreeR(): Int = Fused.stateWriter(0)(blockR)._2

  /** the tree built and run: what a user pays for a `direct` block today */
  @Benchmark
  def blockFreeBuildR(): Int = Fused.stateWriter(0)(blockFree(0, 0))._2

  /** the SHIPPING runners on the prebuilt tree — the baseline that is
   * actually in the library, not the fixture */
  @Benchmark
  def nestedBlockR(): Int =
    State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, State % Int](blockR))._2._2

  @Benchmark
  def blockFuncR(): Int = Fused.runCtrl[Func, Int, String, Int](0)(h => blockFunc(h)(0, 0))._2

  @Benchmark
  def blockContR(): Int = Fused.runCtrl[Cont, Int, String, Int](0)(h => blockCont(h)(0, 0))._2

  @Benchmark
  def blockFuncStagedR(): Int =
    val C = Control[Func]
    (C./(blockFuncStaged(0, 0))(a => st => (st, a)))((0, Vector.empty))._2

  @Setup
  def up(): Unit =
    blockR = blockFree(0, 0)
    swR = rightSW(0, 0)
    sw = (0 until N).foldLeft(pure[SW, Int](0)): (m, i) =>
      m.flatMap: acc =>
        (i % 3) match
          case 0 => State.get[Int].at[SW].map(acc + _)
          case 1 => State.set[Int](i).at[SW].map(acc + _)
          case _ => Writer.tell("w").at[SW].map(_ => acc + 1)
    tsw = (0 until N).foldLeft(pure[TSW, Int](0)): (m, i) =>
      m.flatMap: acc =>
        (i % 3) match
          case 0 => State.get[Int].at[TSW].map(acc + _)
          case 1 => State.set[Int](i).at[TSW].map(acc + _)
          case _ => Writer.tell("w").at[TSW].map(_ => acc + 1)

  // ---- two effects

  /** Writer first, then State: State operations rebuilt once */
  @Benchmark
  def nestedSW(): Int =
    State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, State % Int](sw))._2._2

  /** State first, then Writer: Writer operations rebuilt once */
  @Benchmark
  def nestedWS(): Int =
    !.run(Writer.run[String, (Int, Int), Pure](State.handle[Int](0)(sw)))._2._2

  @Benchmark
  def fusedSW(): Int =
    Fused.stateWriter(0)(sw)._2

  // ---- the right-nested twin: no rotation, the rebuild alone

  @Benchmark
  def nestedSWr(): Int =
    State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, State % Int](swR))._2._2

  @Benchmark
  def fusedSWr(): Int =
    Fused.stateWriter(0)(swR)._2

  // ---- three effects, the program never raises

  /** Writer, then State, then Throws: two rebuilds per Writer op's
   * neighbours and a third walk that handles nothing */
  @Benchmark
  def nestedTSW(): Int =
    !.run(runEither[(Int, (Seq[String], Int)), Pure, String](
      State.handle[Int](0)(
        Writer.run[String, Int, Throws % String + State % Int](tsw)))).toOption.get._2._2

  @Benchmark
  def fusedTSW(): Int =
    Fused.throwsStateWriter(0)(tsw).toOption.get._2
}
