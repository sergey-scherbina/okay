package okay.persist

import munit.FunSuite
import okay.{!, +, Delim, Pure}
import okay.Direct.*
import scala.annotation.unused
import scala.language.implicitConversions

/**
 * THE BOOK'S CHAPTER 22, COMPILED (docs/continuations/22-checkpoints.md).
 *
 * TestDialogue already measures what a chapter saves in RECORDS READ.
 * The claim this book makes is a different one and was pinned
 * nowhere: a snapshot cuts READING, not RUNNING. The program is still
 * executed over every answer, because executing it is the only way to
 * find out where it stands.
 *
 * So this suite counts PROGRAM STEPS, not records, and asserts that
 * the number does NOT improve.
 */
class TestBookCheckpoints extends FunSuite {

  type Row = Delim + Pure

  /** counts records read, like TestDialogue's own helper */
  final class Counting(under: Topic) extends Topic:
    var records = 0
    def name: String = under.name
    def partitions: Int = under.partitions
    def append(p: Int, k: Array[Byte], v: Array[Byte], a: Ack): Long = under.append(p, k, v, a)
    def read(p: Int, from: Long, max: Int): Topic.Read =
      val r = under.read(p, from, max)
      r match
        case Topic.Read.Records(rs) => records += rs.size
        case _ => ()
      r
    def begin(p: Int): Long = under.begin(p)
    def end(p: Int): Long = under.end(p)
    def compact(p: Int): Unit = under.compact(p)

  val N = 40

  /** a counter the PROGRAM touches, so it counts executions, not reads */
  var steps = 0

  def counted(n: Int)(using Delim.Asking[Int, Int, Int, Row]): Int ! Row = direct:
    if n == 0 then 0
    else
      steps += 1
      (!Delim.pause(n)) + (!counted(n - 1))

  def oracle(q: Int, @unused a: Dialogue.Attempt): Int ! Pure = okay.pure(q * 2)

  test("a snapshot cuts READING, and leaves RUNNING exactly where it was") {
    val store = MemoryStore()

    // ---- two identical runs, one snapshotted, one not
    val plainT = Counting(store.topic("plain"))
    val plain = Dialogue[Int, Int, Int, Pure](plainT, "p", "count/1")(counted(N))
    assertEquals(!.run(plain.run(oracle)), (1 to N).sum * 2)

    val snapT = Counting(store.topic("snap"))
    val snapsT = Counting(store.topic("__snaps", 1, Policy(compact = true)))
    val snaps = new Snapshots(snapsT)
    val snapped = Dialogue[Int, Int, Int, Pure](
      snapT, "s", "count/1", Some(snaps), snapshotEvery = 10)(counted(N))
    assertEquals(!.run(snapped.run(oracle)), (1 to N).sum * 2)

    // ---- now a COLD start over each log, counting both things
    plainT.records = 0; snapT.records = 0; snapsT.records = 0

    // NB: constructing a Dialogue does not fold the journal -- asking
    // it WHERE IT STANDS is what replays the program. The first cut
    // of this test measured the constructor and proved nothing; the
    // `plainSteps > 0` assertion below is what caught it.
    steps = 0
    val p2 = Dialogue[Int, Int, Int, Pure](plainT, "p", "count/1")(counted(N))
    assertEquals(p2.journal.size, N)
    val _ = (!.run(p2.at)).toOption.get
    val plainSteps = steps
    val plainReads = plainT.records

    steps = 0
    val s2 = Dialogue[Int, Int, Int, Pure](snapT, "s", "count/1", Some(snaps))(counted(N))
    assertEquals(s2.journal.size, N)
    val _ = (!.run(s2.at)).toOption.get
    val snapSteps = steps
    val snapReads = snapT.records + snapsT.records

    println(s"[ch22] cold start over $N answers: " +
      s"plain read $plainReads records / ran $plainSteps steps; " +
      s"snapshotted read $snapReads records / ran $snapSteps steps")

    // THE MEASUREMENT THIS SUITE EXISTS FOR
    assertEquals(snapSteps, plainSteps,
      s"the snapshot changed how much the PROGRAM ran ($snapSteps vs $plainSteps) " +
        "-- if this ever passes by being smaller, chapter 22 is wrong")
    assert(plainSteps > 0, "the program never ran at all: the counter proves nothing")

    // and the thing it DOES cut, for contrast
    assert(snapReads * 3 < plainReads,
      s"snapshotted cold start read $snapReads records, plain $plainReads")
  }

  test("the log is the truth: with no snapshot store the same journal replays") {
    val store = MemoryStore()
    val t = store.topic("bothways")
    val snaps = Snapshots(store, "__snaps2")
    val d = Dialogue[Int, Int, Int, Pure](t, "d", "count/1", Some(snaps), snapshotEvery = 7)(counted(12))
    assertEquals(!.run(d.run(oracle)), (1 to 12).sum * 2)

    // a reader that knows nothing about chapters stands in the same place
    val bare = Dialogue[Int, Int, Int, Pure](t, "d", "count/1")(counted(12))
    assertEquals(bare.journal.size, 12,
      "a chapter-less reader saw a different journal: the shortcut became the truth")
  }
}
