package okay

import okay.given
import okay.Eager.given

/**
 * `reify` and `reflect` are one function at two ends, and the claim
 * that makes them worth naming is that they are a ROUND TRIP: an
 * encoding is fixed by `pure` and `perform`, `foldCont` is its fold,
 * so there is exactly one structure-preserving way across and going
 * both ways must land where it started.
 *
 * "Where it started" means the same ANSWERS, not the same object — the
 * encodings are different data by construction, which is the whole
 * reason to have more than one.
 */
class TestReflect extends munit.FunSuite {

  enum Op[+A]:
    case Get() extends Op[Int]
    case Say(s: String) extends Op[Unit]

  given TypeableK[Op] = typeableK(classOf[Op[?]])

  val said = scala.collection.mutable.Buffer[String]()
  given Handler[Op] with
    def handle[A](o: Op[A]): A = o match
      case Op.Get() => 7
      case Op.Say(s) => said += s; ()

  /** a program with values, operations and a bind chain */
  def program[M[_[+_], _]](using M: Effects[M]): M[Op, Int] =
    M.perform(Op.Get()).flatMap(a =>
      M.perform(Op.Say(s"got $a")).flatMap(_ =>
        M.perform(Op.Get()).map(b => a + b)))

  def run[M[_[+_], _] : Effects](m: M[Op, Int]): (Int, List[String]) =
    said.clear()
    val v = m.runWith
    (v, said.toList)

  test("reflect: a Free tree read into every other encoding") {
    val tree: Int ! Op = program[Free]
    val expected = run[Free](tree)
    assertEquals(expected, (14, List("got 7")))

    assertEquals(run[Eff](reflect[Eff, Op, Int](tree)), expected)
    assertEquals(run[Eager](reflect[Eager, Op, Int](tree)), expected)
    assertEquals(run[Free](reflect[Free, Op, Int](tree)), expected)
  }

  test("reify: every encoding observed back as syntax") {
    val expected = (14, List("got 7"))
    assertEquals(run[Free](reify[Eff, Op, Int](program[Eff])), expected)
    assertEquals(run[Free](reify[Eager, Op, Int](program[Eager])), expected)
    assertEquals(run[Free](reify[Free, Op, Int](program[Free])), expected)
  }

  test("the round trip: reify after reflect, for each encoding") {
    val tree: Int ! Op = program[Free]
    val expected = run[Free](tree)

    assertEquals(run[Free](reify[Eff, Op, Int](reflect[Eff, Op, Int](tree))), expected)
    assertEquals(run[Free](reify[Eager, Op, Int](reflect[Eager, Op, Int](tree))), expected)
  }

  test("convert: the two names are one function, at its two ends") {
    val tree: Int ! Op = program[Free]
    val expected = run[Free](tree)
    // reflect IS convert into M; reify IS convert into Free
    assertEquals(run[Eff](convert[Free, Eff, Op, Int](tree)), expected)
    assertEquals(run[Free](convert[Eff, Free, Op, Int](program[Eff])), expected)
    // and it crosses between two non-Free encodings without passing
    // through a tree at all
    assertEquals(run[Eager](convert[Eff, Eager, Op, Int](program[Eff])), expected)
  }
}
