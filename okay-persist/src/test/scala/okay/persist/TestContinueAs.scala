package okay.persist

import munit.FunSuite
import okay.{!, +, Delim, Pure}
import okay.Direct.*
import scala.language.implicitConversions

/**
 * BOUNDED HISTORY (dialogue-continue-as, 2026-09-17), at the journal
 * level: what a `Continued` record does to a fold.
 *
 * The fourth test is the one the design turns on. A continuation
 * resets the JOURNAL but not the RECORD COUNT, and that is what keeps
 * `expect` sound across a chapter boundary: a writer still standing in
 * the old chapter carries a number this fold has already passed, so
 * its answer is rejected instead of being read onto a question that is
 * not the one it answered. Reset both counts and that writer's stale
 * answer lands at position 1 of the NEW chapter, silently — the exact
 * class of corruption the envelope exists to prevent.
 */
class TestContinueAs extends FunSuite {

  type Row = Delim + Pure

  def three(using Delim.Asking[String, String, String, Row]): String ! Row = direct:
    val a = !Delim.pause("1?")
    val b = !Delim.pause("2?")
    val c = !Delim.pause("3?")
    s"$a$b$c"

  /** two answers in, still asking the third */
  def standing(t: Topic, id: String = "c-1", program: String = "three/1") =
    val d = Dialogue[String, String, String, Pure](t, id, program)(three)
    val _ = !.run(d.answer("x"))
    val _ = !.run(d.answer("y"))
    d

  test("a continuation SUPERSEDES the history: the journal becomes the seed") {
    val t = MemoryStore().topic("cont")
    val d = standing(t)
    assertEquals(d.journal, List("x", "y"))
    assertEquals(d.recovered.accepted, 2)

    assert(d.continueAs("seed", 2), "the continuation lost a race nobody else was in")
    assertEquals(d.journal, List("seed"), "the history was not superseded")

    // and the program now stands where ONE answer puts it
    assertEquals((!.run(d.at)).toOption.flatMap(_.asking), Some("2?"))
  }

  test("the RECORD count carries on: it is what expect counts, and it never resets") {
    val t = MemoryStore().topic("cont")
    val d = standing(t)
    val _ = d.continueAs("seed", 2)
    assertEquals(d.recovered.accepted, 3, "the record count was reset with the journal")
    assertEquals(d.journal.size, 1)

    // a further answer occupies the NEXT record position, not position 1
    val _ = !.run(d.answer("z"))
    assertEquals(d.journal, List("seed", "z"))
    assertEquals(d.recovered.accepted, 4)
  }

  test("two writers that both continue produce ONE new chapter") {
    val t = MemoryStore().topic("cont")
    val a = standing(t)
    val b = Dialogue[String, String, String, Pure](t, "c-1", "three/1")(three)
    assertEquals(b.recovered.accepted, 2, "the second writer did not see the same position")

    assert(a.continueAs("mine", 2))
    assert(!b.continueAs("yours", 2), "both continuations were accepted")
    assertEquals(a.journal, List("mine"))
    assertEquals(a.recovered.rejected.size, 1)
  }

  test("THE POINT: a writer still in the OLD chapter is rejected, not mis-read") {
    val t = MemoryStore().topic("cont")
    val a = Dialogue[String, String, String, Pure](t, "c-1", "three/1")(three)
    val _ = !.run(a.answer("x"))

    // b folds the journal and holds the program: it is standing at
    // position ONE of the old chapter — and position one is exactly
    // where the new chapter's first answer goes. A fold that reset its
    // record count along with its journal would accept what b writes
    // next, onto a question b never saw. (Written at position one on
    // purpose: at any other position the stale record is rejected by
    // arithmetic rather than by the invariant, and the test proves
    // nothing. It was first written at position two and passed
    // against the broken fold.)
    val b = Dialogue[String, String, String, Pure](t, "c-1", "three/1")(three)
    val p = (!.run(b.at)).toOption.get

    assert(a.continueAs("seed", 1))

    // ...and only now does b answer, with the position it remembered
    val late = !.run(b.step(p, "late", 1))
    assert(late.isInstanceOf[Dialogue.Answered.Lost[?, ?, ?, ?]],
      s"a stale answer was accepted into the new chapter: $late")
    assertEquals(a.journal, List("seed"),
      "the stale answer was read onto a question it did not answer")
  }

  test("a continuation written by ANOTHER program stops the fold") {
    val t = MemoryStore().topic("cont")
    val d = standing(t)
    val other = Dialogue[String, String, String, Pure](t, "c-1", "three/2")(three)
    val _ = other.continueAs("theirs", 2)

    d.recovered.stopped match
      case Some(Dialogue.Stopped.Mismatch(_, found, expected)) =>
        assertEquals(found, "three/2")
        assertEquals(expected, "three/1")
      case other => fail(s"a foreign continuation was folded anyway: $other")
  }

  test("a CHAPTER written before the continuation still folds to the seed") {
    val store = MemoryStore()
    val t = store.topic("cont")
    val snaps = Snapshots(store, "cont__chapters")
    def dialogue = Dialogue[String, String, String, Pure](t, "c-1", "three/1",
      Some(snaps), snapshotEvery = 1)(three)

    val d = dialogue
    val _ = !.run(d.answer("x"))
    val _ = !.run(d.answer("y"))          // a chapter exists at this point
    assert(d.continueAs("seed", 2))

    // a process that was never holding any of this reads the chapter
    // AND the continuation after it
    assertEquals(dialogue.journal, List("seed"))
    assertEquals(dialogue.recovered.accepted, 3)
  }
}
