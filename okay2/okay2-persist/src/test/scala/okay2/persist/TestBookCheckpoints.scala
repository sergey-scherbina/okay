package okay2.persist

import munit.FunSuite
import okay2.{!, Shift, pure}

/**
 * THE BOOK'S CHAPTER 22, COMPILED (okay-persist's TestBookCheckpoints;
 * docs/continuations/22-checkpoints.md): a snapshot cuts READING, not
 * RUNNING. The program is still executed over every answer, because
 * executing it is the only way to find out where it stands — so this
 * suite counts PROGRAM STEPS and asserts the number does NOT improve.
 */
class TestBookCheckpoints extends FunSuite {
  import DialogueFixtures._

  val N = 40

  /** a counter the PROGRAM touches, so it counts executions, not reads */
  var steps = 0

  def counted(n: Int)(s: Shift.Asking.Aux[Int, Int, Int, P]): Int ! Rw =
    if (n == 0) pure[Rw, Int](0)
    else {
      steps += 1
      for { a <- Shift.pause(s)(n); rest <- counted(n - 1)(s) } yield a + rest
    }

  def oracle(q: Int, a: Dialogue.Attempt): Int ! P = { val _ = a; pure[P, Int](q * 2) }

  test("a snapshot cuts READING, and leaves RUNNING exactly where it was") {
    val store = new MemoryStore

    // ---- two identical runs, one snapshotted, one not
    val plainT = new Counting(store.topic("plain"))
    val plain = Dialogue[Int, Int, Int, P](plainT, "p", "count/1")(counted(N))
    assertEquals(!.run(plain.run(oracle)), (1 to N).sum * 2)

    val snapT = new Counting(store.topic("snap"))
    val snapsT = new Counting(store.topic("__snaps", 1, Policy(compact = true)))
    val snaps = new Snapshots(snapsT)
    val snapped = Dialogue[Int, Int, Int, P](snapT, "s", "count/1", Some(snaps), snapshotEvery = 10)(counted(N))
    assertEquals(!.run(snapped.run(oracle)), (1 to N).sum * 2)

    // ---- now a COLD start over each log, counting both things
    plainT.records = 0; snapT.records = 0; snapsT.records = 0

    // constructing a Dialogue does not fold the journal — asking it
    // WHERE IT STANDS is what replays the program
    steps = 0
    val p2 = Dialogue[Int, Int, Int, P](plainT, "p", "count/1")(counted(N))
    assertEquals(p2.journal.size, N)
    val _ = place(p2)
    val plainSteps = steps
    val plainReads = plainT.records

    steps = 0
    val s2 = Dialogue[Int, Int, Int, P](snapT, "s", "count/1", Some(snaps))(counted(N))
    assertEquals(s2.journal.size, N)
    val _ = place(s2)
    val snapSteps = steps
    val snapReads = snapT.records + snapsT.records

    println(s"[ch22] cold start over $N answers: plain read $plainReads records / ran $plainSteps steps; " +
      s"snapshotted read $snapReads records / ran $snapSteps steps")

    // THE MEASUREMENT THIS SUITE EXISTS FOR
    assertEquals(snapSteps, plainSteps,
      s"the snapshot changed how much the PROGRAM ran ($snapSteps vs $plainSteps) -- if this ever passes by being smaller, chapter 22 is wrong")
    assert(plainSteps > 0, "the program never ran at all: the counter proves nothing")

    // and the thing it DOES cut, for contrast
    assert(snapReads * 3 < plainReads, s"snapshotted cold start read $snapReads records, plain $plainReads")
  }

  test("the log is the truth: with no snapshot store the same journal replays") {
    val store = new MemoryStore
    val t = store.topic("bothways")
    val snaps = Snapshots(store, "__snaps2")
    val d = Dialogue[Int, Int, Int, P](t, "d", "count/1", Some(snaps), snapshotEvery = 7)(counted(12))
    assertEquals(!.run(d.run(oracle)), (1 to 12).sum * 2)

    // a reader that knows nothing about chapters stands in the same place
    val bare = Dialogue[Int, Int, Int, P](t, "d", "count/1")(counted(12))
    assertEquals(bare.journal.size, 12, "a chapter-less reader saw a different journal: the shortcut became the truth")
  }
}
