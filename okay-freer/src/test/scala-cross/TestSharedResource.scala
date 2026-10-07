package okay.freer


import okay.freer.Row.*
import okay.freer.Logic.*

/**
 * A RESOURCE SHARED BY BRANCHES (logic-cut-releases, specs/backtracking.md): acquired before a `choose`, it is
 * acquired once, used by every branch, and released ONCE — after the last branch, or when the rest of the search
 * is dropped (`cut`, `observe`) or a throw leaves it.
 */
class TestSharedResource extends munit.FunSuite:

  type R = Choose + Pure
  given Failing[R] = new Failing[R]:
    def guard[X](e: R[X], onFailure: () => Unit): R[X] = e

  final class Boom extends RuntimeException("boom")

  type Log = scala.collection.mutable.ArrayBuffer[String]

  /** a scope acquiring "a", then a choice among `xs`, each branch logging its use */
  def shared(log: Log)(xs: Seq[Int]): Int ! R =
    Resource.run[Int, R](Resource.acquire("a")(x => log += s"release $x").at[Resource + R].flatMap(_ =>
      effect[Choose, Int](Choose(xs)).at[Resource + R].map { x => log += s"use $x"; x }))

  test("every branch: the shared resource released once, after the last branch") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    assertEquals(!.run(runChoice[Int, Pure](shared(log)(Seq(1, 2, 3)))), Seq(1, 2, 3))
    assertEquals(log.toList, List("use 1", "use 2", "use 3", "release a"))
  }

  test("cut: released once") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    assertEquals(!.run(runChoice[Int, Pure](cut[Int, Pure](shared(log)(Seq(1, 2, 3))))), Seq(1))
    assertEquals(log.toList, List("use 1", "release a"))
  }

  test("observe 2 of 3: released once") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    assertEquals(!.run(observe[Int, Pure](2)(shared(log)(Seq(1, 2, 3)))), Seq(1, 2))
    assertEquals(log.toList, List("use 1", "use 2", "release a"))
  }

  test("observe 3 of an infinite choice: released once") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    assertEquals(!.run(observe[Int, Pure](3)(shared(log)(LazyList.from(1)))), Seq(1, 2, 3))
    assertEquals(log.toList, List("use 1", "use 2", "use 3", "release a"))
  }

  test("every branch of a lazy finite choice: released once, after the last") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    assertEquals(!.run(runChoice[Int, Pure](shared(log)(LazyList(1, 2)))), Seq(1, 2))
    assertEquals(log.toList, List("use 1", "use 2", "release a"))
  }

  test("a scope inside each branch: its own resource once per branch, the shared one once") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    val p: Int ! R =
      Resource.run[Int, R](Resource.acquire("a")(x => log += s"release $x").at[Resource + R].flatMap(_ =>
        effect[Choose, Int](Choose(Seq(1, 2))).at[Resource + R].flatMap(x =>
          Resource.run[Int, R](Resource.acquire(s"b$x")(y => log += s"release $y").at[Resource + R].flatMap(_ =>
            effect[Choose, Int](Choose(Seq(10, 20))).at[Resource + R].map(y => x + y))).at[Resource + R])))
    assertEquals(!.run(runChoice[Int, Pure](p)), Seq(11, 21, 12, 22))
    assertEquals(log.toList, List("release b1", "release b2", "release a"))
  }

  test("a throw out of the search: released once") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    val p: Int ! R = shared(log)(Seq(1, 2, 3)).map(x => if x == 2 then throw Boom() else x)
    val _ = intercept[Boom](!.run(runChoice[Int, Pure](p)))
    assertEquals(log.toList, List("use 1", "use 2", "release a"))
  }

  test("a throw out of a split: released once") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    val p: Int ! R = shared(log)(Seq(1, 2, 3)).map(x => if x == 1 then throw Boom() else x)
    val _ = intercept[Boom](!.run(observe[Int, Pure](3)(p)))
    assertEquals(log.toList, List("use 1", "release a"))
  }

  // ---- on a machine: the scope and the search as frames

  type M = Choose + Shift % ? + Pure
  given Failing[M] = new Failing[M]:
    def guard[X](e: M[X], onFailure: () => Unit): M[X] = e

  def sharedM(log: Log)(xs: Seq[Int]): Int ! M =
    Resource.run[Int, M](Resource.acquire("a")(x => log += s"release $x").at[Resource + M].flatMap(_ =>
      effect[Choose, Int](Choose(xs)).at[Resource + M].map { x => log += s"use $x"; x }))

  test("on a machine, every branch: released once, after the last") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    assertEquals(!.run(Shift.run[Seq[Int], Pure](runChoice[Int, Shift % ? + Pure](sharedM(log)(Seq(1, 2, 3))))), Seq(1, 2, 3))
    assertEquals(log.toList, List("use 1", "use 2", "use 3", "release a"))
  }

  test("on a machine, cut: released once") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    assertEquals(!.run(Shift.run[Seq[Int], Pure](runChoice[Int, Shift % ? + Pure](
      cut[Int, Shift % ? + Pure](sharedM(log)(Seq(1, 2, 3)))))), Seq(1))
    assertEquals(log.toList, List("use 1", "release a"))
  }
