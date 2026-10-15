package okay.std.tests

import okay.*
import okay.std.*

import okay.std.Logic.*

/**
 * A RESOURCE SHARED BY BRANCHES (logic-cut-releases, specs/backtracking.md): acquired before a `choose`, it is
 * acquired once, used by every branch, and released ONCE — after the last branch, or when the rest of the search
 * is dropped (`cut`, `observe`) or a throw leaves it.
 */
class TestSharedResource extends munit.FunSuite:

  type R = Choose

  final class Boom extends RuntimeException("boom")

  type Log = scala.collection.mutable.ArrayBuffer[String]

  /** a scope acquiring "a", then a choice among `xs`, each branch logging its use */
  def shared(log: Log)(xs: Seq[Int]): Int ! R =
    Resource.run(Resource.acquire("a")(x => log += s"release $x").at[Resource + R].flatMap(_ =>
      effect[Choose, Int](Choose(xs)).at[Resource + R].map { x => log += s"use $x"; x }))

  test("every branch: the shared resource released once, after the last branch") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    assertEquals(runChoice(shared(log)(Seq(1, 2, 3))).run, Seq(1, 2, 3))
    assertEquals(log.toList, List("use 1", "use 2", "use 3", "release a"))
  }

  test("cut: released once") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    assertEquals(runChoice(cut[Int, R](shared(log)(Seq(1, 2, 3)))).run, Seq(1))
    assertEquals(log.toList, List("use 1", "release a"))
  }

  test("observe 2 of 3: released once") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    assertEquals(observe[Int, R](2)(shared(log)(Seq(1, 2, 3))).run, Seq(1, 2))
    assertEquals(log.toList, List("use 1", "use 2", "release a"))
  }

  test("observe 3 of an infinite choice: released once") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    assertEquals(observe[Int, R](3)(shared(log)(LazyList.from(1))).run, Seq(1, 2, 3))
    assertEquals(log.toList, List("use 1", "use 2", "use 3", "release a"))
  }

  test("every branch of a lazy finite choice: released once, after the last") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    assertEquals(runChoice(shared(log)(LazyList(1, 2))).run, Seq(1, 2))
    assertEquals(log.toList, List("use 1", "use 2", "release a"))
  }

  test("a scope inside each branch: its own resource once per branch, the shared one once") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    val p: Int ! R =
      Resource.run(Resource.acquire("a")(x => log += s"release $x").at[Resource + R].flatMap(_ =>
        effect[Choose, Int](Choose(Seq(1, 2))).at[Resource + R].flatMap(x =>
          Resource.run(Resource.acquire(s"b$x")(y => log += s"release $y").at[Resource + R].flatMap(_ =>
            effect[Choose, Int](Choose(Seq(10, 20))).at[Resource + R].map(y => x + y))).at[Resource + R])))
    assertEquals(runChoice(p).run, Seq(11, 21, 12, 22))
    assertEquals(log.toList, List("release b1", "release b2", "release a"))
  }

  test("a throw out of the search: released once") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    val p: Int ! R = shared(log)(Seq(1, 2, 3)).map(x => if x == 2 then throw Boom() else x)
    val _ = intercept[Boom](runChoice(p).run)
    assertEquals(log.toList, List("use 1", "use 2", "release a"))
  }

  test("a throw out of a split: released once") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    val p: Int ! R = shared(log)(Seq(1, 2, 3)).map(x => if x == 1 then throw Boom() else x)
    val _ = intercept[Boom](observe[Int, R](3)(p).run)
    assertEquals(log.toList, List("use 1", "release a"))
  }
