package okay.persist

import munit.FunSuite
import okay.{!, +, Delim, Pure}
import okay.Direct.*
import scala.language.implicitConversions

/**
 * THE FOUR WAYS A DURABLE DIALOGUE USED TO BREAK (durable-workflow
 * stage 0, 2026-09-17). Each test here was a probe that FAILED
 * against the first cut — a poisoned journal, a silent mis-mapping
 * after a deploy, a side effect performed twice, and two writers both
 * accepted. They are kept as the suite because the fixes are all
 * invisible until something goes wrong, which is exactly the kind of
 * property that rots.
 */
class TestDialogueHardening extends FunSuite {

  type Row = Delim + Pure

  // ==== A: an answer the program cannot digest ======================

  def strict(using Delim.Asking[String, String, String, Row]): String ! Row = direct:
    val n = !Delim.pause("how many?")
    s"ok ${n.toInt * 2}"

  test("A: an answer the program refuses is NOT journalled, and the dialogue lives") {
    val t = MemoryStore().topic("poison")
    def d = Dialogue[String, String, String, Pure](t, "p-1", "strict/1")(strict)

    // the program throws on it — the caller sees that, where they ran it
    val _ = intercept[NumberFormatException](!.run(d.answer("abc")))

    // ...and the journal is untouched, so the dialogue is where it was
    assertEquals(d.journal, Nil)
    assertEquals((!.run(d.at)).toOption.flatMap(_.asking), Some("how many?"))

    // a good answer still lands, which is the whole point: before
    // this, "abc" was in the log forever and every process replaying
    // it threw
    val next = !.run(d.answer("4"))
    assert(next.isInstanceOf[Dialogue.Answered.Advanced[?, ?, ?, ?]], s"got $next")
    assertEquals(d.journal, List("4"))
  }

  // ==== B: the program changed under the journal ====================

  def v1(using Delim.Asking[String, String, String, Row]): String ! Row = direct:
    val city = !Delim.pause("city?")
    val nights = !Delim.pause("nights?")
    s"$city/$nights"

  def v2(using Delim.Asking[String, String, String, Row]): String ! Row = direct:
    val promo = !Delim.pause("promo code?")     // a NEW first question
    val city = !Delim.pause("city?")
    val nights = !Delim.pause("nights?")
    s"$promo/$city/$nights"

  test("B: a journal written by another program STOPS the fold instead of mis-mapping") {
    val t = MemoryStore().topic("versions")
    val one = Dialogue[String, String, String, Pure](t, "v-1", "booking/1")(v1)
    val _ = !.run(one.answer("Kyiv"))
    assertEquals(one.journal, List("Kyiv"))

    // the deploy happens here: same dialogue, new program. Before
    // stage 0 this read "Kyiv" as the promo code and carried on.
    val two = Dialogue[String, String, String, Pure](t, "v-1", "booking/2")(v2)
    assertEquals(two.journal, Nil)
    val stopped = two.recovered.stopped
    assert(stopped.exists(_.isInstanceOf[Dialogue.Stopped.Mismatch]), s"got $stopped")
    stopped match
      case Some(Dialogue.Stopped.Mismatch(_, found, expected)) =>
        assertEquals(found, "booking/1")
        assertEquals(expected, "booking/2")
      case other => fail(s"expected a Mismatch, got $other")

    // and it refuses to say where it stands, rather than saying
    // something wrong
    assert((!.run(two.at)).isLeft)
    assert((!.run(two.answer("x"))).isInstanceOf[Dialogue.Answered.Broken[?, ?, ?, ?]])

    // the ORIGINAL program still reads its own journal
    assertEquals(one.journal, List("Kyiv"))
  }

  // ==== C: the crash window, and the key that survives it ===========

  test("C: the oracle's Attempt is the journal's position, stable across a restart") {
    val t = MemoryStore().topic("once")
    var seen = List.empty[(String, Dialogue.Attempt)]
    def oracle(q: String, a: Dialogue.Attempt): String ! Pure =
      seen = seen :+ ((q, a))
      if q == "nights?" then throw new RuntimeException("the process dies here")
      okay.pure("Kyiv")

    val d = Dialogue[String, String, String, Pure](t, "c-1", "booking/1")(v1)
    val _ = intercept[RuntimeException](!.run(d.run(oracle)))
    assertEquals(seen.map(_._1), List("city?", "nights?"))
    assertEquals(seen.map(_._2), List(Dialogue.Attempt("c-1", 0), Dialogue.Attempt("c-1", 1)))
    assertEquals(d.journal, List("Kyiv"))     // the crash lost the second

    // the restart re-asks "nights?" — AT-LEAST-ONCE, stated rather
    // than hidden — and it comes with the SAME key, so an idempotent
    // oracle performs it once in the world
    seen = Nil
    val d2 = Dialogue[String, String, String, Pure](t, "c-1", "booking/1")(v1)
    def ok(q: String, a: Dialogue.Attempt): String ! Pure =
      seen = seen :+ ((q, a))
      okay.pure("3")
    assertEquals(!.run(d2.run(ok)), "Kyiv/3")
    assertEquals(seen, List(("nights?", Dialogue.Attempt("c-1", 1))))
  }

  // ==== D: two writers, one dialogue ================================

  test("D: the second writer LOSES, is told so, and changes nothing") {
    val t = MemoryStore().topic("race")
    val a = Dialogue[String, String, String, Pure](t, "d-1", "booking/1")(v1)
    val b = Dialogue[String, String, String, Pure](t, "d-1", "booking/1")(v1)

    // both processes HOLD the dialogue, standing at the same question
    // — the warm path, which is where the window is wide. (The cold
    // `answer` re-reads the log first, so its window is only between
    // the fold and the append; `expect` covers both the same way.)
    val held = (!.run(b.at)).toOption.get
    assertEquals(held.asking, Some("city?"))

    val first = !.run(a.answer("Kyiv"))
    assert(first.isInstanceOf[Dialogue.Answered.Advanced[?, ?, ?, ?]], s"got $first")

    // b answers from what it was holding: position 0, already taken
    !.run(b.step(held, "Lviv", 0)) match
      case Dialogue.Answered.Lost(to) =>
        // the loser is told where it ACTUALLY stands, which is what it
        // needs to decide what to do next
        assertEquals(to.asking, Some("nights?"))
      case other => fail(s"the second writer was not told it lost: $other")

    // one answer accepted, the other recorded and ignored
    assertEquals(a.journal, List("Kyiv"))
    val r = a.recovered
    assertEquals(r.rejected.map(l => (l.expect, l.had)), List((0, 1)))
    assert(r.intact, "a lost race is not damage")
  }

  test("D: a lost race does not stop the dialogue finishing") {
    val t = MemoryStore().topic("race2")
    val a = Dialogue[String, String, String, Pure](t, "d-2", "booking/1")(v1)
    val b = Dialogue[String, String, String, Pure](t, "d-2", "booking/1")(v1)
    val held = (!.run(b.at)).toOption.get
    val _ = !.run(a.answer("Kyiv"))
    val _ = !.run(b.step(held, "Lviv", 0))      // ignored
    !.run(a.answer("3")) match
      case Dialogue.Answered.Advanced(to) => assertEquals(to.finished, Some("Kyiv/3"))
      case other => fail(s"the winner could not carry on: $other")
  }

  // ==== the ANSWER type's own evolution =============================

  test("the record envelope's version and upcasts are wired, and drift is loud") {
    val t = MemoryStore().topic("evolving")
    val one = Dialogue[String, String, String, Pure](t, "e-1", "booking/1")(v1)
    val _ = !.run(one.answer("Kyiv"))

    // a reader that expects version 2 and was given no way up from 1
    // does not guess: the record is damage, named at its offset
    val two = Dialogue[String, String, String, Pure](
      t, "e-1", "booking/1", None, 0, version = 2)(v1)
    two.recovered.stopped match
      case Some(Dialogue.Stopped.Damage(off, err)) =>
        assertEquals(off, 0L)
        assert(err.contains("upcast"), s"the error does not name the missing step: $err")
      case other => fail(s"a version gap went unnoticed: $other")

    // with the step supplied, the same log reads (identity here: the
    // point is that the chain RUNS, not what it does)
    val three = Dialogue[String, String, String, Pure](
      t, "e-1", "booking/1", None, 0, version = 2,
      upcasts = Map(1 -> ((b: Array[Byte]) => Right(b))))(v1)
    assertEquals(three.journal, List("Kyiv"))
  }

  // ==== the discipline, where it is relied upon =====================

  test("a durable dialogue whose body reaches outside does not COMPILE") {
    // the sentence the whole design rests on, checked at the one place
    // that depends on it (dialogue-replay-discipline). Before this, a
    // body that called a service between two pauses compiled, and
    // every replay called the service again.
    val e = compileErrors("""
      okay.persist.Dialogue[String, String, String, okay.Async](
        okay.persist.MemoryStore().topic("t"), "d", "p/1")(
          okay.Direct.direct(""))""")
    assert(e.nonEmpty, "an Async-rowed dialogue compiled")
    assert(e.contains("PERFORM AGAIN"), s"refused for the wrong reason: $e")
  }
}
