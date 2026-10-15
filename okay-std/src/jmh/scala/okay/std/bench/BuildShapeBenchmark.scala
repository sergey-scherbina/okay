package okay.std.bench

import okay.{!, +, %, defer, pure}
import okay.std.*

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{State as JmhState, *}

/**
 * THE SHAPE A PROGRAM IS BUILT IN (left-nested-build-cost, 2026-09-27), on the new road. The same N operations,
 * built and run: once by `foldLeft` over `flatMap` (left-nested) and once by `each` / `foldM` (right-nested). The
 * build is inside the measured method because every real caller builds per call.
 *
 * `foldM` is the std's (`okay.std.foldM`): the classic's `!.foldM` folded a trailing `map` of a step into the
 * next bind by matching freer's `Bind`/`Mapped` nodes, which a program as a value (`Run`) cannot — so on this road
 * a `.map`-written step pays its map. The root has no `each`/`foldEach`: `BuildShape.each`/`foldEach` below are
 * okay-freer's library bodies (`Classic.scala`) on the root's words, `defer` for `Free.delay`. The classic's
 * `rowUnwrap` and `stateFoldMInline` match freer's tree nodes and have no counterpart.
 */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@JmhState(Scope.Benchmark)
class BuildShapeBenchmark {
  import BuildShapeBenchmark.{each, foldEach}

  val N = 1000
  val items: Vector[Int] = Vector.tabulate(N)(identity)

  /** a Writer telling every element, the shape of a Stage telling a chunk */
  @Benchmark
  def tellsFoldLeft(): Int =
    val p = items.foldLeft(pure[Unit, Writer % Int](()))((m, i) => m.flatMap(_ => Writer.tell(i)))
    Writer.run[Int, Unit, Writer % Int](p).run._1.length

  @Benchmark
  def tellsEach(): Int =
    val p = each(items)(i => Writer.tell(i))
    Writer.run[Int, Unit, Writer % Int](p).run._1.length

  /** a State counter with an accumulator, the shape of a fold with effects */
  @Benchmark
  def stateFoldLeft(): Int =
    val p = items.foldLeft(pure[Int, State % Int](0))((m, i) => m.flatMap(acc => State.modify[Int](_ + i).map(acc + _)))
    State.handle[Int](0)(p).run._2

  @Benchmark
  def stateFoldM(): Int =
    val p = foldM(items)(0)((acc, i) => State.modify[Int](_ + i).map(acc + _))
    State.handle[Int](0)(p).run._2

  /** TWO handlers over a mixed row, FusionBenchmark's `sw` program built per call, the only difference the build */
  type SW = State % Int + Writer % String

  private def op(i: Int, acc: Int): Int ! SW = (i % 3) match
    case 0 => State.get[Int].at[SW].map(acc + _)
    case 1 => State.set[Int](i).at[SW].map(acc + _)
    case _ => Writer.tell("w").at[SW].map(_ => acc + 1)

  @Benchmark
  def rowFoldLeft(): Int =
    val p = items.foldLeft(pure[Int, SW](0))((m, i) => m.flatMap(acc => op(i, acc)))
    State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, SW](p))._2._2

  @Benchmark
  def rowFoldM(): Int =
    val p = foldM(items)(0)((acc, i) => op(i, acc))
    State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, SW](p))._2._2

  /** the SAME right-nested program as rowFoldM, built per call, each step ONE bind */
  private def oneBind(i: Int, acc: Int): Int ! SW =
    if i >= N then pure(acc)
    else (i % 3) match
      case 0 => State.get[Int].at[SW].flatMap(x => oneBind(i + 1, acc + x))
      case 1 => State.set[Int](i).at[SW].flatMap(x => oneBind(i + 1, acc + x))
      case _ => Writer.tell("w").at[SW].flatMap(_ => oneBind(i + 1, acc + 1))

  @Benchmark
  def rowOneBind(): Int =
    State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, SW](oneBind(0, 0)))._2._2

  /** stateFoldM's work written by hand with ONE flatMap a step */
  private def stateOne(i: Int, acc: Int): Int ! State % Int =
    if i >= N then pure(acc) else State.modify[Int](_ + i).flatMap(s => stateOne(i + 1, acc + s))

  @Benchmark
  def stateOneBind(): Int =
    State.handle[Int](0)(stateOne(0, 0)).run._2

  /** a step that is `State.update`: ONE `Update` operation */
  @Benchmark
  def stateUpdate(): Int =
    val p = foldM(items)(0)((acc, i) => State.update[Int, Int](s => (s, s + i)).map(acc + _))
    State.handle[Int](0)(p).run._2

  /** a step that is `Reader.lift`: ONE `Asks` */
  @Benchmark
  def readerLift(): Int =
    val p = foldM(items)(0)((acc, i) => Reader.lift[Int, Int]((e: Int) ?=> e + i).map(acc + _))
    Reader.run[Int, Int, Reader % Int](7)(p).run

  /** boxing control: the same two programs over a `case class` accumulator */
  final case class Acc(n: Int)

  private def opAcc(i: Int, acc: Acc): Acc ! SW = (i % 3) match
    case 0 => State.get[Int].at[SW].map(x => Acc(acc.n + x))
    case 1 => State.set[Int](i).at[SW].map(x => Acc(acc.n + x))
    case _ => Writer.tell("w").at[SW].map(_ => Acc(acc.n + 1))

  @Benchmark
  def rowFoldMAcc(): Int =
    val p = foldM(items)(Acc(0))((acc, i) => opAcc(i, acc))
    State.run[Int, (Seq[String], Acc)](0)(Writer.run[String, Acc, SW](p))._2._2.n

  private def oneBindAcc(i: Int, acc: Acc): Acc ! SW =
    if i >= N then pure(acc)
    else (i % 3) match
      case 0 => State.get[Int].at[SW].flatMap(x => oneBindAcc(i + 1, Acc(acc.n + x)))
      case 1 => State.set[Int](i).at[SW].flatMap(x => oneBindAcc(i + 1, Acc(acc.n + x)))
      case _ => Writer.tell("w").at[SW].flatMap(_ => oneBindAcc(i + 1, Acc(acc.n + 1)))

  @Benchmark
  def rowOneBindAcc(): Int =
    State.run[Int, (Seq[String], Acc)](0)(Writer.run[String, Acc, SW](oneBindAcc(0, Acc(0))))._2._2.n

  /** stateFoldM's work with the step split into the element's program and a pure combine */
  @Benchmark
  def stateFoldEach(): Int =
    val p = foldEach(items)(0)(i => State.modify[Int](_ + i))(_ + _)
    State.handle[Int](0)(p).run._2

  // ---- the ladder from stateFoldEach DOWN to stateOneBind

  /** rung D1: foldEach's `go` with the accumulator and the element's answer fixed to `Int` */
  private def foldEachInt[X](xs: IndexedSeq[X])(f: X => Int ! State % Int)(combine: (Int, Int) => Int): Int ! State % Int =
    val n = xs.length
    def go(i: Int, acc: Int): Int ! State % Int =
      if i >= n then pure(acc) else f(xs(i)).flatMap(a => go(i + 1, combine(acc, a)))
    defer(go(0, 0))

  @Benchmark
  def stateFoldEachInt(): Int =
    val p = foldEachInt(items)(i => State.modify[Int](_ + i))(_ + _)
    State.handle[Int](0)(p).run._2

  /** rung D2: stateOne's loop, the element read out of the Vector */
  private def stateOneVec(i: Int, acc: Int): Int ! State % Int =
    if i >= N then pure(acc)
    else
      val x = items(i)
      State.modify[Int](_ + x).flatMap(s => stateOneVec(i + 1, acc + s))

  @Benchmark
  def stateOneVecBind(): Int =
    State.handle[Int](0)(stateOneVec(0, 0)).run._2

  /** foldEach verbatim, the elements copied into a flat `ArraySeq` once per program */
  private def foldEachArr[X, A, B](xs: Iterable[X])(z: B)(f: X => A ! State % Int)(combine: (B, A) => B): B ! State % Int =
    val v = scala.collection.immutable.ArraySeq.untagged.from(xs)
    val n = v.length
    def go(i: Int, acc: B): B ! State % Int =
      if i >= n then pure(acc) else f(v(i)).flatMap(a => go(i + 1, combine(acc, a)))
    defer(go(0, z))

  @Benchmark
  def stateFoldEachArr(): Int =
    val p = foldEachArr(items)(0)(i => State.modify[Int](_ + i))(_ + _)
    State.handle[Int](0)(p).run._2

  /** foldEach under `inline`: `go` expanded at the call site */
  private inline def foldEachInline[X, A, B, Es[_]](xs: Iterable[X])(z: B)(inline f: X => A ! Es)(inline combine: (B, A) => B): B ! Es =
    val v = scala.collection.immutable.ArraySeq.untagged.from(xs)
    val n = v.length
    def go(i: Int, acc: B): B ! Es =
      if i >= n then pure(acc) else f(v(i)).flatMap(a => go(i + 1, combine(acc, a)))
    defer(go(0, z))

  @Benchmark
  def stateFoldEachInline(): Int =
    val p = foldEachInline(items)(0)(i => State.modify[Int](_ + i))(_ + _)
    State.handle[Int](0)(p).run._2
}

object BuildShapeBenchmark:
  /** okay-freer's `!.foldEach` (Classic.scala) on the root's words: one bind a step, delayed, indexed */
  def foldEach[X, A, B, Es[_]](xs: Iterable[X])(z: B)(f: X => A ! Es)(combine: (B, A) => B): B ! Es =
    val v = scala.collection.immutable.ArraySeq.untagged.from(xs)
    val n = v.length
    def go(i: Int, acc: B): B ! Es =
      if i >= n then pure(acc)
      else f(v(i)).flatMap(a => go(i + 1, combine(acc, a)))
    defer(go(0, z))

  /** okay-freer's `!.each` (Classic.scala): `foldM` with no accumulator */
  def each[X, Es[_]](xs: Iterable[X])(f: X => Unit ! Es): Unit ! Es =
    foldM[X, Unit, Es](xs)(())((_, x) => f(x))
