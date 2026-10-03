package okay

import okay.Freer.Return
import DesignDelimited.*

/** the design sketch's primitives: a segment is the function it stands for; capture and resume are inverse */
class TestDesignDelimited extends munit.FunSuite:

  private type G = [S, R, A] =>> Nothing
  private final class Mark[T](val name: String) extends Tag[T]

  private def id[A]: Segment[G, A, A, A, A] = Segment.Id()
  private def plus(n: Int): Segment[G, Int, Int, Int, Int] =
    Segment.Frame[G, Int, Int, Int, Int, Int, Int]((a: Int) => Return(a + n), id[Int])

  /** the marks of a slice, innermost first */
  private def marks[A, T](s: Slice[G, A, T]): List[String] = s match
    case Slice.Seg(_) => Nil
    case Slice.Under(_, t, rest) => (t match { case m: Mark[?] => m.name; case _ => "-" }) :: marks(rest)

  test("a segment is the function it stands for: Bind's join") {
    assertEquals(plus(2)(40).resume, Return(42))
  }

  test("capture to a marked boundary, then resume: the same stack") {
    val (p, q, r) = (Mark[Int]("p"), Mark[Int]("q"), Mark[Int]("r"))
    // live segment, then boundaries p, q, r, each over a segment
    val below: Below[G, Int, Int] =
      Below.At(p, Slice.Under(plus(1), q, Slice.Under(plus(2), r, Slice.Seg(plus(3)))))
    val f = capture(plus(0), below, _ eq q)
    assert(f != null)
    val got = f.nn
    assertEquals(marks(got.taken), List("p"))         // crossed p, stopped at q
    assertEquals(marks(got.under), List("r"))         // under q: r and the last segment
    assertEquals(marks(resume(got.taken, got.tag, got.under)), marks(slice(plus(0), below)))
  }
