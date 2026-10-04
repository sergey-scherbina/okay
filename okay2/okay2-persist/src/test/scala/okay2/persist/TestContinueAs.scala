package okay2.persist

import munit.FunSuite
import okay2.{!, Shift}

/**
 * BOUNDED HISTORY (okay-persist's TestContinueAs; dialogue-continue-as),
 * at the journal level: what a `Continued` record does to a fold. A
 * continuation resets the JOURNAL but not the RECORD COUNT, which keeps
 * `expect` sound across a chapter boundary.
 */
class TestContinueAs extends FunSuite {
  import DialogueFixtures._

  def three(s: Shift.Asking.Aux[String, String, String, P]): String ! Rw = for {
    a <- Shift.pause(s)("1?")
    b <- Shift.pause(s)("2?")
    c <- Shift.pause(s)("3?")
  } yield s"$a$b$c"

  def fresh(t: Topic, id: String = "c-1", program: String = "three/1"): Dialogue[String, String, String, P] =
    Dialogue[String, String, String, P](t, id, program)(three)

  /** two answers in, still asking the third */
  def standing(t: Topic, id: String = "c-1", program: String = "three/1"): Dialogue[String, String, String, P] = {
    val d = fresh(t, id, program)
    val _ = !.run(d.answer("x"))
    val _ = !.run(d.answer("y"))
    d
  }

  test("a continuation SUPERSEDES the history: the journal becomes the seed") {
    val t = new MemoryStore().topic("cont")
    val d = standing(t)
    assertEquals(d.journal, List("x", "y"))
    assertEquals(d.recovered.accepted, 2)

    assert(d.continueAs("seed", 2), "the continuation lost a race nobody else was in")
    assertEquals(d.journal, List("seed"), "the history was not superseded")

    // and the program now stands where ONE answer puts it
    assertEquals(!.run(d.at).toOption.flatMap(_.asking), Some("2?"))
  }

  test("the RECORD count carries on: it is what expect counts, and it never resets") {
    val t = new MemoryStore().topic("cont")
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
    val t = new MemoryStore().topic("cont")
    val a = standing(t)
    val b = fresh(t)
    assertEquals(b.recovered.accepted, 2, "the second writer did not see the same position")

    assert(a.continueAs("mine", 2))
    assert(!b.continueAs("yours", 2), "both continuations were accepted")
    assertEquals(a.journal, List("mine"))
    assertEquals(a.recovered.rejected.size, 1)
  }

  test("THE POINT: a writer still in the OLD chapter is rejected, not mis-read") {
    val t = new MemoryStore().topic("cont")
    val a = fresh(t)
    val _ = !.run(a.answer("x"))

    // b holds the program at position ONE of the old chapter — exactly
    // where the new chapter's first answer goes
    val b = fresh(t)
    val p = place(b)

    assert(a.continueAs("seed", 1))

    // ...and only now does b answer, with the position it remembered
    val late = !.run(b.step(p, "late", 1))
    assert(late.isInstanceOf[Dialogue.Answered.Lost[_, _, _, _]], s"a stale answer was accepted into the new chapter: $late")
    assertEquals(a.journal, List("seed"), "the stale answer was read onto a question it did not answer")
  }

  test("a continuation written by ANOTHER program stops the fold") {
    val t = new MemoryStore().topic("cont")
    val d = standing(t)
    val foreign = fresh(t, program = "three/2")
    val _ = foreign.continueAs("theirs", 2)

    d.recovered.stopped match {
      case Some(Dialogue.Stopped.Mismatch(_, found, expected)) =>
        assertEquals(found, "three/2")
        assertEquals(expected, "three/1")
      case other => fail(s"a foreign continuation was folded anyway: $other")
    }
  }

  test("a CHAPTER written before the continuation still folds to the seed") {
    val store = new MemoryStore
    val t = store.topic("cont")
    val snaps = Snapshots(store, "cont__chapters")
    def dialogue = Dialogue[String, String, String, P](t, "c-1", "three/1", Some(snaps), snapshotEvery = 1)(three)

    val d = dialogue
    val _ = !.run(d.answer("x"))
    val _ = !.run(d.answer("y"))          // a chapter exists at this point
    assert(d.continueAs("seed", 2))

    // a process that was never holding any of this reads the chapter AND
    // the continuation after it
    assertEquals(dialogue.journal, List("seed"))
    assertEquals(dialogue.recovered.accepted, 3)
  }
}
