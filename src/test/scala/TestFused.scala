package okay

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import okay.RowLift.at

/**
 * specs/handler-fusion.md, stage 0: the hand-written fused loops agree
 * with the nested runners — for BOTH nestings where the row has no
 * abort, and for the Throws-outermost nesting where it has.
 *
 * Programs are generated as instruction lists and compiled by
 * foldLeft (the left-nested shape, so `resume`'s rotation is
 * exercised on every step), with the answer of each operation folded
 * into the final value so that a wrong resumption shows in the
 * result and not only in the accumulators.
 */
class TestFused extends munit.ScalaCheckSuite {

  enum Ins:
    case Get
    case Set(n: Int)
    case Tell(w: String)
    case Raise(e: String)

  val plain: Gen[Ins] = Gen.frequency(
    3 -> Gen.const(Ins.Get),
    3 -> Gen.choose(-1000, 1000).map(Ins.Set(_)),
    3 -> Gen.alphaStr.map(s => Ins.Tell(s.take(4))))

  val raising: Gen[Ins] = Gen.frequency(9 -> plain, 1 -> Gen.alphaStr.map(s => Ins.Raise(s.take(3))))

  type SW = State % Int + Writer % String
  type TSW = Throws % String + State % Int + Writer % String

  /** each instruction adds its answer into the running sum */
  def compileSW(ins: List[Ins]): Int ! SW =
    ins.foldLeft(pure[SW, Int](0)): (m, i) =>
      m.flatMap: acc =>
        i match
          case Ins.Get => State.get[Int].at[SW].map(acc + _)
          case Ins.Set(n) => State.set[Int](n).at[SW].map(acc + _)
          case Ins.Tell(w) => Writer.tell(w).at[SW].map(_ => acc + w.length)
          case Ins.Raise(_) => pure(acc)   // not in this row

  def compileTSW(ins: List[Ins]): Int ! TSW =
    ins.foldLeft(pure[TSW, Int](0)): (m, i) =>
      m.flatMap: acc =>
        i match
          case Ins.Get => State.get[Int].at[TSW].map(acc + _)
          case Ins.Set(n) => State.set[Int](n).at[TSW].map(acc + _)
          case Ins.Tell(w) => Writer.tell(w).at[TSW].map(_ => acc + w.length)
          case Ins.Raise(e) => raise[String, Int](e).at[TSW]

  property("State + Writer: the fused loop agrees with BOTH nestings") {
    forAll(Gen.listOf(plain), Gen.choose(-50, 50)) { (ins, s0) =>
      val p = compileSW(ins)
      val ((s, w), a) = Fused.stateWriter(s0)(p)
      // Writer outside State: (log, (state, a))
      val (w1, (s1, a1)) = !.run(Writer.run[String, (Int, Int), Pure](
        State.handle[Int, Int, Writer % String](s0)(p)))
      // State outside Writer: (state, (log, a))
      val (s2, (w2, a2)) = !.run(State.handle[Int, (Seq[String], Int), Pure](s0)(
        Writer.run[String, Int, State % Int](p)))
      (s, w, a) == (s1, w1, a1) && (s, w, a) == (s2, w2, a2)
    }
  }

  property("Throws + State + Writer: the fused loop agrees with Throws outermost, aborts included") {
    forAll(Gen.listOf(raising), Gen.choose(-50, 50)) { (ins, s0) =>
      val p = compileTSW(ins)
      val fused = Fused.throwsStateWriter(s0)(p)
      val nested = !.run(runEither[(Int, (Seq[String], Int)), Pure, String](
        State.handle[Int, (Seq[String], Int), Throws % String](s0)(
          Writer.run[String, Int, Throws % String + State % Int](p))))
      (fused, nested) match
        case (Left(e), Left(f)) => e == f
        case (Right(((s, w), a)), Right((s1, (w1, a1)))) => (s, w, a) == (s1, w1, a1)
        case _ => false
    }
  }

  test("stack-safe on a million operations, left-nested") {
    val n = 1_000_000
    val p = (1 to n).foldLeft(pure[SW, Int](0)): (m, _) =>
      m.flatMap(_ => State.get[Int].at[SW].flatMap(s => State.set[Int](s + 1).at[SW]))
    val ((s, w), a) = Fused.stateWriter(0)(p)
    assertEquals(s, n)
    assertEquals(a, n)
    assert(w.isEmpty)
  }

  test("the accumulators are values: the residual after a forwarded operation is re-runnable") {
    // a program that tells, then asks the state, run twice from the
    // same fused answer must give the same answer twice — the law
    // State.handle keeps, restated for the product
    val p: Int ! SW =
      for
        _ <- Writer.tell("a").at[SW]
        s <- State.get[Int].at[SW]
        _ <- State.set[Int](s + 1).at[SW]
        _ <- Writer.tell("b").at[SW]
      yield s
    assertEquals(Fused.stateWriter(5)(p), Fused.stateWriter(5)(p))
    assertEquals(Fused.stateWriter(5)(p), ((6, Vector("a", "b")), 5))
  }
}
