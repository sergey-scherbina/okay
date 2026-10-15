package okay.std.tests

import okay.*
import okay.std.*

/**
 * A SCOPE WHOSE REST A HANDLER OUTSIDE DROPS (bracket-final): an operation that never resumes — a `None` of `Maybe`,
 * `Chronicle`'s `halt`, a choice of none — passing out of a `Resource` scope releases it before it goes, once.
 */
class TestResourceDropped extends munit.FunSuite:

  type Log = scala.collection.mutable.ArrayBuffer[String]

  def acquired[R[_]](log: Log, name: String): String ! Resource + R =
    Resource.acquire(name)(x => log += s"release $x").at[Resource + R]

  test("Maybe.none out of a scope: released, the answer None") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    val p: Int ! Maybe = Resource.run(acquired[Maybe](log, "a").flatMap(_ =>
      acquired[Maybe](log, "b").flatMap(_ => Maybe.none[Int].at[Resource + Maybe])))
    assertEquals(Maybe.run(p).run, None)
    assertEquals(log.toList, List("release b", "release a"))
  }

  test("Chronicle.halt out of a scope: released, the verdict Failed") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    val p: Int ! Chronicle % String = Resource.run(acquired[Chronicle % String](log, "a").flatMap(_ =>
      Chronicle.confess[String, Int]("bad").at[Resource + Chronicle % String]))
    assertEquals(Chronicle.run(p).run, Chronicle.Verdict.Failed(Vector("bad")))
    assertEquals(log.toList, List("release a"))
  }

  test("a choice of none out of a scope: released") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    val p: Int ! Choose = Resource.run(acquired[Choose](log, "a").flatMap(_ =>
      effect[Choose, Int](Choose(Seq.empty)).at[Resource + Choose]))
    assertEquals(runChoice(p).run, Seq.empty)
    assertEquals(log.toList, List("release a"))
  }

  test("one branch of three pruned: released once, after the last") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    val p: Int ! Choose = Resource.run(acquired[Choose](log, "a").flatMap(_ =>
      effect[Choose, Int](Choose(Seq(1, 2, 3))).at[Resource + Choose].flatMap(x =>
        if x == 2 then effect[Choose, Int](Choose(Seq.empty)).at[Resource + Choose]
        else pure[Int, Resource + Choose](x).map { x => log += s"use $x"; x })))
    assertEquals(runChoice(p).run, Seq(1, 3))
    assertEquals(log.toList, List("use 1", "use 3", "release a"))
  }

  test("bracket around Maybe.none: released") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    val p: Int ! Maybe = bracket[String, Int, Maybe]("a")(x => log += s"release $x")(_ => Maybe.none[Int])
    assertEquals(Maybe.run(p).run, None)
    assertEquals(log.toList, List("release a"))
  }

  test("Some passes: released at the scope's end, once") {
    val log: Log = scala.collection.mutable.ArrayBuffer.empty
    val p: Int ! Maybe = Resource.run(acquired[Maybe](log, "a").flatMap(_ =>
      Some(1).maybe.at[Resource + Maybe].map { x => log += s"use $x"; x }))
    assertEquals(Maybe.run(p.map { x => log += "after"; x }).run, Some(1))
    assertEquals(log.toList, List("use 1", "release a", "after"))
  }
