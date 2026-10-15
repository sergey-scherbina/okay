package okay.std.tests

import okay.*
import okay.given
import scala.language.implicitConversions
import okay.std.*

import okay.std.given
import scala.util.chaining.*

class TestState extends munit.FunSuite {

  test("modify: get and set, said once") {
    val p: Int ! State % Int =
      for
        _ <- State.modify[Int](_ + 1)
        _ <- State.modify[Int](_ * 10)
        n <- State.get[Int]
      yield n
    assertEquals(State.run[Int, Int](4)(p), (50, 50))
    // it answers the NEW state, as get and set do
    assertEquals(State.run[Int, Int](4)(State.modify[Int](_ + 1)), (5, 5))
  }

  test("update answers what the write destroys") {
    // the value that WAS there, which `modify` cannot give back
    val p: Int ! State % Int = State.update[Int, Int](s => (s, s * 10))
    assertEquals(State.run[Int, Int](4)(p), (40, 4))
  }

  test("swap answers both states") {
    assertEquals(State.run[Int, (Int, Int)](4)(State.swap[Int](_ * 10)), (40, (4, 40)))
  }

  test("index") {
    val x = State.index(List("a", "b", "c", "d", "e", "f", "g"), 1).tap(println)
    assertEquals(x, (8L, List((7L, "g"), (6L, "f"), (5L, "e"), (4L, "d"), (3L, "c"), (2L, "b"), (1L, "a"))))
  }

  test("stack safety: indexing a 1M stream") {
    val n = 1000000
    assertEquals(State.index(fibs[Int, LazyList].take(n))._1, n.toLong)
  }

  test("Modify answers the new state, and chains as modify always has") {
    val p: (Int, Int) ! State % Int =
      for
        a <- State.modify[Int](_ + 1)
        b <- State.modify[Int](_ * 10)
      yield (a, b)
    assertEquals(State.run(0)(p), (10, (1, 10)))
  }

  test("a forwarded Modify is handled by the outer State handler") {
    type Fx = State % Int + Writer % String
    val p: Int ! Fx =
      State.modify[Int](_ + 5).at[Fx].flatMap(n => Writer.tell(s"n=$n").at[Fx].map(_ => n))
    assertEquals(State.run[Int, (Seq[String], Int)](1)(Writer.run(p)), (6, (Seq("n=6"), 6)))
  }

  // op-map-constructors: update was a get, a set and a map
  test("update is ONE operation, and answers from the OLD state") {
    val p: (String, (Int, Int)) ! State % Int =
      for
        a <- State.update[Int, String](s => (s"was $s", s + 1))
        b <- State.swap[Int](_ * 10)
      yield (a, b)
    assertEquals(State.run(4)(p), (50, ("was 4", (5, 50))))
  }

  test("a forwarded Update is handled by the outer State handler") {
    type Fx = State % Int + Writer % String
    val p: Int ! Fx =
      State.update[Int, Int](s => (s * 2, s + 1)).at[Fx].flatMap(n => Writer.tell(s"n=$n").at[Fx].map(_ => n))
    assertEquals(State.run[Int, (Seq[String], Int)](3)(Writer.run(p)), (4, (Seq("n=6"), 6)))
  }

  test("zoomWith: every operation on the part, update included, reaches the whole") {
    val p: (Int, String, Int) ! State % Int =
      for
        a <- State.get[Int]
        b <- State.update[Int, String](s => (s"part $s", s + 1))
        c <- State.modify[Int](_ * 2)
      yield (a, b, c)
    val whole: (Int, String, Int) ! State % (String, Int) = State.zoomWith[(String, Int), Int, (Int, String, Int), State % Int](_._2, n => w => (w._1, n))(p)
    assertEquals(State.run(("k", 5))(whole), (("k", 12), (5, "part 5", 12)))
  }

  test("State.get twice in a bind") {
    assertEquals(State.run(7)(State.get[Int].flatMap(a => State.get[Int].map(b => a + b))), (7, 14))
  }

  test("Reader.ask twice in a bind") {
    assertEquals(Reader.run(5)(Reader.ask[Int].flatMap(a => Reader.ask[Int].map(_ * a))).run, 25)
  }

  test("two of one effect in a row are two places: the nearer handler answers (okay-freer refused it: Distinct)") {
    val two: Int ! State % Int + State % Int = State.get[Int].at[State % Int + State % Int]
    val inner = State.handle[Int](10)(two)
    val outer = State.handle[Int](0)(inner)
    assertEquals(outer.run._2._2, 10)
  }
}
