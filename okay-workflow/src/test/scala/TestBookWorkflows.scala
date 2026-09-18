package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * THE BOOK'S CHAPTER 23, COMPILED (docs/continuations/23-durable-workflows.md).
 *
 * The engine's own suites (TestWf, TestWorker, TestDialogue) prove
 * each mechanism separately. This is the end-to-end story a reader of
 * the chapter would copy: a program runs, the process "dies", and a
 * NEW process over the same journal stands where the old one stood --
 * without asking the world anything twice.
 */
class TestBookWorkflows extends munit.FunSuite {

  type P = okay.Pure
  type Row = Delim + P

  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_700_000_000_000L,
                                         id = "bk-1", dice = 0.25)

  def done[Q, R](s: Wf.Step[Q, R]): R = s match
    case Wf.Step.Done(r) => r
    case other => fail(s"expected the drive to finish, it said $other")

  /** the chapter's program: straight-line code that happens to pause */
  def booking(using w: Wf.Asks[String, String, String, P]): String ! Row = direct:
    val city = !w.pause("which city?")
    val when = !w.now
    val ref = !w.uuid
    val nights = !w.pause(s"how many nights in $city?")
    s"$city/$nights/$when/$ref"

  test("the world is asked once, and the runtime's answers are journalled beside it") {
    var asked = List.empty[String]
    val start = !.run(Wf.resumable[String, String, String, P](booking))
    val (st, journal) = !.run(Wf.drive(start): q =>
      asked = asked :+ q
      okay.pure(if q.startsWith("which") then "Kyiv" else "3"))

    assertEquals(done(st), "Kyiv/3/1700000000000/bk-1")
    assertEquals(asked, List("which city?", "how many nights in Kyiv?"),
      "the oracle was asked the RUNTIME's questions too")
    // both kinds are in one journal, tagged
    assert(journal.exists(_ == Right("Kyiv")), journal.toString)
    assert(journal.exists(_ == Left(Wf.SysA.Millis(1_700_000_000_000L))), journal.toString)
  }

  test("A NEW PROCESS: the same journal, nobody asked again, the same answer") {
    // run one: the "first process"
    val (st1, journal) = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, String, P](booking))): q =>
        okay.pure(if q.startsWith("which") then "Kyiv" else "3"))
    val first = done(st1)

    // run two: only the journal survived. `Wf.replay` TAKES NO
    // RUNTIME -- its signature is the proof that it cannot read a
    // clock, so every value it produces came out of the journal.
    val second = !.run(Wf.replay[String, String, String, P](booking)(journal))

    assertEquals(second.finished, Some(first),
      "the new process landed somewhere the first run never was")
  }

  test("the clock does not move across a restart, however often the program reruns") {
    val (_, journal) = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, String, P](booking))): q =>
        okay.pure(if q.startsWith("which") then "Kyiv" else "3"))

    // replay it three times: a wall clock would give three answers
    val answers = (1 to 3).map: _ =>
      !.run(Wf.replay[String, String, String, P](booking)(journal)).finished
    assertEquals(answers.distinct.size, 1,
      s"three replays disagreed, so something was read rather than remembered: $answers")
    assert(answers.head.exists(_.contains("1700000000000")),
      "the journalled instant is not what came back")
  }
}
