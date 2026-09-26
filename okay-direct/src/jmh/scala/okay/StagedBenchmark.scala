package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.Direct.*
import okay.Row.at

/**
 * specs/direct-staged.md, the number: the SAME block text three ways
 * — a Free `direct` block run by the shipping runners (what a user
 * has today), the staged `direct` block, and the staged program
 * written by hand (`sw.stage(op)` at every operation — the parity
 * target, 84 568 B/op on core's FusionBenchmark.blockFuncInlineMatchR).
 * 1 000 operations as a static 10-op block inside a 100-iteration
 * loop, the shape staged-block-lanes measured.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class StagedBenchmark {

  final val Iters = 100

  type Row = State % Int + Writer % String
  type R = Stager.Answer[Int, String, Int]
  val sw = Stager.StateWriter[Int, String, Int]()

  def freeBlock(i: Int, acc: Int): Int ! Row =
    if i >= Iters then pure(acc)
    else direct[[A] =>> A ! Row] {
      val a = State.get[Int].?
      val _ = State.set[Int](i).?
      Writer.tell("w").?
      val b = State.get[Int].?
      val _ = State.set[Int](i + 1).?
      Writer.tell("w").?
      val c = State.get[Int].?
      val _ = State.set[Int](i + 2).?
      Writer.tell("w").?
      val d = State.get[Int].?
      freeBlock(i + 1, acc + a + b + c + d).?
    }

  def stagedBlock(i: Int, acc: Int): Handled[Row, R, Int] =
    if i >= Iters then Handled.pure(acc)
    else Direct.staged(sw) {
      val a = State.get[Int].?
      val _ = State.set[Int](i).?
      Writer.tell("w").?
      val b = State.get[Int].?
      val _ = State.set[Int](i + 1).?
      Writer.tell("w").?
      val c = State.get[Int].?
      val _ = State.set[Int](i + 2).?
      Writer.tell("w").?
      val d = State.get[Int].?
      stagedBlock(i + 1, acc + a + b + c + d).?
    }

  /** the parity target: what the macro should emit, by hand */
  def handBlock(i: Int, acc: Int): Handled[Row, R, Int] =
    if i >= Iters then Handled.pure(acc)
    else
      val M = summon[Monad[Handled[Row, R, *]]]
      M.flatMap(sw.stage(State.Get()))(a =>
      M.flatMap(sw.stage(State.Set(i)))(_ =>
      M.flatMap(sw.stage(Writer.Say("w")))(_ =>
      M.flatMap(sw.stage(State.Get()))(b =>
      M.flatMap(sw.stage(State.Set(i + 1)))(_ =>
      M.flatMap(sw.stage(Writer.Say("w")))(_ =>
      M.flatMap(sw.stage(State.Get()))(c =>
      M.flatMap(sw.stage(State.Set(i + 2)))(_ =>
      M.flatMap(sw.stage(Writer.Say("w")))(_ =>
      M.flatMap(sw.stage(State.Get()))(d => handBlock(i + 1, acc + a + b + c + d)))))))))))

  /** direct-inline-bind-free's ceiling: the SAME Free block by hand,
   * `Free.flatMap` (inline: `Bind(this, f)`) at every bind — what the
   * macro would emit if its binds resolved the given's `override
   * inline flatMap` instead of the trait's virtual one */
  def freeHandNested(i: Int, acc: Int): Int ! Row =
    if i >= Iters then pure(acc)
    else
      State.get[Int].at[Row].flatMap(a =>
      State.set[Int](i).at[Row].flatMap(_ =>
      Writer.tell("w").at[Row].flatMap(_ =>
      State.get[Int].at[Row].flatMap(b =>
      State.set[Int](i + 1).at[Row].flatMap(_ =>
      Writer.tell("w").at[Row].flatMap(_ =>
      State.get[Int].at[Row].flatMap(c =>
      State.set[Int](i + 2).at[Row].flatMap(_ =>
      Writer.tell("w").at[Row].flatMap(_ =>
      State.get[Int].at[Row].flatMap(d => freeHandNested(i + 1, acc + a + b + c + d)))))))))))

  @Benchmark
  def freeHandNestedRun(): Int =
    State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, State % Int](freeHandNested(0, 0)))._2._2

  /** the user's baseline: the Free block, built and run by the shipping runners */
  @Benchmark
  def freeDirectNested(): Int =
    State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, State % Int](freeBlock(0, 0)))._2._2

  @Benchmark
  def stagedDirect(): Int = sw.run(0)(stagedBlock(0, 0))._2

  @Benchmark
  def stagedHand(): Int = sw.run(0)(handBlock(0, 0))._2

  // ---- handlers-vs-plain-loop: the SAME 1 000 operations with no
  // effect machinery at all — a `while` loop and a `var` state. The
  // lane returns (answer, state, log) so JMH consumes all three and
  // the loop cannot be dropped; that is one 24 B tuple a run, against
  // the thousand operations measured.

  /** the effect machinery isolated: the log is the SAME persistent
   * `Vector` the staged answer threads, appended the same way */
  @Benchmark
  def plainLoopVector(): (Int, Int, Vector[String]) =
    var s = 0
    var log = Vector.empty[String]
    var acc = 0
    var i = 0
    while i < Iters do
      val a = s
      s = i
      log = log :+ "w"
      val b = s
      s = i + 1
      log = log :+ "w"
      val c = s
      s = i + 2
      log = log :+ "w"
      val d = s
      acc = acc + a + b + c + d
      i += 1
    (acc, s, log)

  /** what an imperative programmer writes: a mutable buffer */
  @Benchmark
  def plainLoopBuffer(): (Int, Int, scala.collection.mutable.ArrayBuffer[String]) =
    var s = 0
    val log = scala.collection.mutable.ArrayBuffer.empty[String]
    var acc = 0
    var i = 0
    while i < Iters do
      val a = s
      s = i
      val _ = log.addOne("w")
      val b = s
      s = i + 1
      val _ = log.addOne("w")
      val c = s
      s = i + 2
      val _ = log.addOne("w")
      val d = s
      acc = acc + a + b + c + d
      i += 1
    (acc, s, log)

  /** the lanes' own control: every road computes the same answer,
   * state and log, or the comparison is between different programs */
  @Setup(Level.Trial)
  def sameProgram(): Unit =
    val ((s, log), a) = sw.run(0)(handBlock(0, 0))
    val (pa, ps, plog) = plainLoopVector()
    val (ba, bs, blog) = plainLoopBuffer()
    if a != pa || s != ps || log != plog then
      throw new IllegalStateException(s"plainLoopVector differs: ($a, $s, ${log.size}) vs ($pa, $ps, ${plog.size})")
    if a != ba || s != bs || log != blog.toVector then
      throw new IllegalStateException(s"plainLoopBuffer differs: ($a, $s, ${log.size}) vs ($ba, $bs, ${blog.size})")
    if stagedDirect() != a || freeDirectNested() != a then
      throw new IllegalStateException("the staged or Free block answers differently from the hand block")

  // ---- handlers-vs-virtual-calls: the handler as an ordinary class,
  // `get`/`set`/`tell` virtual methods, the rung between the plain
  // loop and the staged handler. MONO: one subclass, so C2 inlines the
  // calls. MEGA: four identical subclasses rotated per invocation, so
  // every call site in `vBlock`/`bBlock` sees four receivers and stays
  // a vtable call (C2 inlines at most two). One object per run is made
  // through the rotation — 1 allocation against 1 000 operations.

  import StagedBenchmark.*
  private var turn = 0
  private val vMake: Array[() => VOps] = Array(() => V0(), () => V1(), () => V2(), () => V3())
  private val bMake: Array[() => BOps] = Array(() => B0(), () => B1(), () => B2(), () => B3())

  @Benchmark
  def virtualMonoVector(): (Int, Int, Vector[String]) =
    val o = V0()
    (vBlock(o, Iters), o.s, o.log)

  @Benchmark
  def virtualMegaVector(): (Int, Int, Vector[String]) =
    val o = vMake(turn & 3)()
    turn += 1
    (vBlock(o, Iters), o.s, o.log)

  @Benchmark
  def virtualMonoBuffer(): (Int, Int, scala.collection.mutable.ArrayBuffer[String]) =
    val o = B0()
    (bBlock(o, Iters), o.s, o.log)

  @Benchmark
  def virtualMegaBuffer(): (Int, Int, scala.collection.mutable.ArrayBuffer[String]) =
    val o = bMake(turn & 3)()
    turn += 1
    (bBlock(o, Iters), o.s, o.log)

  /** the virtual lanes' control. It never calls `vBlock`/`bBlock` with
   * a second subclass — that would pollute the MONO lanes' profile in
   * their own forks — so the subclasses are checked one operation at a
   * time here, at this method's own call sites, and the block once on
   * the first subclass against the plain loop */
  @Setup(Level.Trial)
  def sameVirtualProgram(): Unit =
    val (pa, ps, plog) = plainLoopVector()
    val v = V0()
    val b = B0()
    if (vBlock(v, Iters), v.s, v.log) != (pa, ps, plog) || (bBlock(b, Iters), b.s, b.log.toVector) != (pa, ps, plog) then
      throw new IllegalStateException("the virtual block answers differently from the plain loop")
    for o <- vMake.map(_()) do
      o.set(7); o.tell("w")
      if o.get() != 7 || o.s != 7 || o.log != Vector("w") then throw new IllegalStateException(s"${o.getClass} differs")
    for o <- bMake.map(_()) do
      o.set(7); o.tell("w")
      if o.get() != 7 || o.s != 7 || o.log.toVector != Vector("w") then throw new IllegalStateException(s"${o.getClass} differs")

  // ---- specs/direct-stagers.md: (1) the SAME block through Stager.All
  // with two unused slots — the price of the layout

  val aw = Stager.All[Unit, Int, String, Nothing, Int]()

  def allBlock(i: Int, acc: Int): Handled[aw.Row, aw.R, Int] =
    if i >= Iters then Handled.pure(acc)
    else Direct.staged(aw) {
      val a = State.get[Int].?
      val _ = State.set[Int](i).?
      Writer.tell("w").?
      val b = State.get[Int].?
      val _ = State.set[Int](i + 1).?
      Writer.tell("w").?
      val c = State.get[Int].?
      val _ = State.set[Int](i + 2).?
      Writer.tell("w").?
      val d = State.get[Int].?
      allBlock(i + 1, acc + a + b + c + d).?
    }

  @Benchmark
  def stagedAllSW(): Int = aw.run((), 0)(allBlock(0, 0))._2 match
    case Right(a) => a
    case Left(n) => n

  // ---- (2) a Reader + Throws block three ways: the shape a block
  // that reads a configuration and may fail has — nine asks and one
  // guarded raise (never taken: the loop must run) per iteration

  case class Cfg(k: Int, limit: Int)
  type RowRT = Reader % Cfg + Throws % String
  val rt = Stager.All[Cfg, Unit, Nothing, String, Int]()
  val cfg = Cfg(3, Int.MaxValue)

  def freeRT(i: Int, acc: Int): Int ! RowRT =
    if i >= Iters then pure(acc)
    else direct[[A] =>> A ! RowRT] {
      val e1 = Reader.ask[Cfg].?
      val e2 = Reader.ask[Cfg].?
      val e3 = Reader.ask[Cfg].?
      val a = e1.k + e2.k + e3.k + i
      val e4 = Reader.ask[Cfg].?
      val e5 = Reader.ask[Cfg].?
      val g = if a > e4.limit then raise[String, Int]("over").? else a + e5.k
      val e6 = Reader.ask[Cfg].?
      val e7 = Reader.ask[Cfg].?
      val e8 = Reader.ask[Cfg].?
      val e9 = Reader.ask[Cfg].?
      freeRT(i + 1, acc + g + e6.k + e7.k + e8.k + e9.k).?
    }

  def stagedRT(i: Int, acc: Int): Handled[rt.Row, rt.R, Int] =
    if i >= Iters then Handled.pure(acc)
    else Direct.staged(rt) {
      val e1 = Reader.ask[Cfg].?
      val e2 = Reader.ask[Cfg].?
      val e3 = Reader.ask[Cfg].?
      val a = e1.k + e2.k + e3.k + i
      val e4 = Reader.ask[Cfg].?
      val e5 = Reader.ask[Cfg].?
      val g = if a > e4.limit then raise[String, Int]("over").? else a + e5.k
      val e6 = Reader.ask[Cfg].?
      val e7 = Reader.ask[Cfg].?
      val e8 = Reader.ask[Cfg].?
      val e9 = Reader.ask[Cfg].?
      stagedRT(i + 1, acc + g + e6.k + e7.k + e8.k + e9.k).?
    }

  /** the parity target for (2), by hand — the SAME shape as `handBlock`:
   * `rt.stage(op)` as the direct argument of every bind, nothing named */
  def handRT(i: Int, acc: Int): Handled[rt.Row, rt.R, Int] =
    if i >= Iters then Handled.pure(acc)
    else
      val M = summon[Monad[Handled[rt.Row, rt.R, *]]]
      M.flatMap(rt.stage(Reader.Ask()))(e1 =>
      M.flatMap(rt.stage(Reader.Ask()))(e2 =>
      M.flatMap(rt.stage(Reader.Ask()))(e3 =>
      M.flatMap(rt.stage(Reader.Ask()))(e4 =>
      M.flatMap(rt.stage(Reader.Ask()))(e5 =>
      M.flatMap(if e1.k + e2.k + e3.k + i > e4.limit then rt.stage[Int](Throws("over")) else Handled.pure[rt.Row, rt.R, Int](e1.k + e2.k + e3.k + i + e5.k))(g =>
      M.flatMap(rt.stage(Reader.Ask()))(e6 =>
      M.flatMap(rt.stage(Reader.Ask()))(e7 =>
      M.flatMap(rt.stage(Reader.Ask()))(e8 =>
      M.flatMap(rt.stage(Reader.Ask()))(e9 =>
        handRT(i + 1, acc + g + e6.k + e7.k + e8.k + e9.k)))))))))))

  @Benchmark
  def freeDirectRT(): Int =
    !.run(runEither[Int, okay.Pure, String](Reader.run[Cfg, Int, Throws % String](cfg)(freeRT(0, 0)))) match
      case Right(a) => a
      case Left(_) => -1

  @Benchmark
  def stagedDirectRT(): Int = rt.run(cfg, ())(stagedRT(0, 0))._2 match
    case Right(a) => a
    case Left(_) => -1

  @Benchmark
  def stagedHandRT(): Int = rt.run(cfg, ())(handRT(0, 0))._2 match
    case Right(a) => a
    case Left(_) => -1
}

object StagedBenchmark {

  /** the State+Writer handler as a plain class hierarchy; every
   * subclass overrides every method with the same body, so class
   * hierarchy analysis cannot devirtualize the calls to one target */
  abstract class VOps {
    var s: Int = 0
    var log: Vector[String] = Vector.empty
    def get(): Int
    def set(x: Int): Unit
    def tell(w: String): Unit
  }
  final class V0 extends VOps { def get(): Int = s; def set(x: Int): Unit = s = x; def tell(w: String): Unit = log = log :+ w }
  final class V1 extends VOps { def get(): Int = s; def set(x: Int): Unit = s = x; def tell(w: String): Unit = log = log :+ w }
  final class V2 extends VOps { def get(): Int = s; def set(x: Int): Unit = s = x; def tell(w: String): Unit = log = log :+ w }
  final class V3 extends VOps { def get(): Int = s; def set(x: Int): Unit = s = x; def tell(w: String): Unit = log = log :+ w }

  abstract class BOps {
    var s: Int = 0
    val log: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer.empty
    def get(): Int
    def set(x: Int): Unit
    def tell(w: String): Unit
  }
  final class B0 extends BOps { def get(): Int = s; def set(x: Int): Unit = s = x; def tell(w: String): Unit = { val _ = log.addOne(w) } }
  final class B1 extends BOps { def get(): Int = s; def set(x: Int): Unit = s = x; def tell(w: String): Unit = { val _ = log.addOne(w) } }
  final class B2 extends BOps { def get(): Int = s; def set(x: Int): Unit = s = x; def tell(w: String): Unit = { val _ = log.addOne(w) } }
  final class B3 extends BOps { def get(): Int = s; def set(x: Int): Unit = s = x; def tell(w: String): Unit = { val _ = log.addOne(w) } }

  /** the same 10-operation block as every other lane, each operation a
   * virtual call on `o` */
  def vBlock(o: VOps, iters: Int): Int =
    var acc = 0
    var i = 0
    while i < iters do
      val a = o.get()
      o.set(i)
      o.tell("w")
      val b = o.get()
      o.set(i + 1)
      o.tell("w")
      val c = o.get()
      o.set(i + 2)
      o.tell("w")
      val d = o.get()
      acc = acc + a + b + c + d
      i += 1
    acc

  def bBlock(o: BOps, iters: Int): Int =
    var acc = 0
    var i = 0
    while i < iters do
      val a = o.get()
      o.set(i)
      o.tell("w")
      val b = o.get()
      o.set(i + 1)
      o.tell("w")
      val c = o.get()
      o.set(i + 2)
      o.tell("w")
      val d = o.get()
      acc = acc + a + b + c + d
      i += 1
    acc
}
