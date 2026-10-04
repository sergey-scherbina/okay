package okay2.persist

import munit.FunSuite
import okay2.{!, +, Pure, Shift, pure}

/** the shared fixtures of the dialogue suites, at the top level */
object DialogueFixtures {
  type P = Pure
  type Rw = Shift[Any] + P

  /** where an answer left a dialogue, for a test that expects it taken */
  def now[Q, A, R, G <: okay2.Row](x: Dialogue.Answered[Q, A, R, G]): Shift.Dialogue[Q, A, R, G] = x match {
    case Dialogue.Answered.Advanced(to) => to
    case Dialogue.Answered.NotAsking(to) => to
    case Dialogue.Answered.Lost(to) => to
    case Dialogue.Answered.Broken(w) => sys.error(s"broken: $w")
  }

  /** where a dialogue stands, for a test that knows the log is intact */
  def place[Q, A, R](d: Dialogue[Q, A, R, P]): Shift.Dialogue[Q, A, R, P] =
    !.run(d.at).getOrElse(sys.error("the log does not fold"))

  /** a Topic that counts what is read through it */
  final class Counting(under: Topic) extends Topic {
    var reads = 0
    var records = 0
    def name: String = under.name
    def partitions: Int = under.partitions
    def append(p: Int, k: Array[Byte], v: Array[Byte], a: Ack): Long = under.append(p, k, v, a)
    def read(p: Int, from: Long, max: Int): Topic.Read = {
      reads += 1
      val r = under.read(p, from, max)
      r match {
        case Topic.Read.Records(rs) => records += rs.size
        case _ => ()
      }
      r
    }
    def begin(p: Int): Long = under.begin(p)
    def end(p: Int): Long = under.end(p)
    def compact(p: Int): Unit = under.compact(p)
  }
}

/**
 * THE DIALOGUE WHOSE JOURNAL IS A TOPIC (okay-persist's TestDialogue):
 * a new process over the same log stands exactly where the old one
 * stood, and asks the outside world nothing it has already been told.
 */
class TestDialogue extends FunSuite {
  import DialogueFixtures._

  /** the program: straight-line code that happens to pause */
  def booking(s: Shift.Asking.Aux[String, String, String, P]): String ! Rw = for {
    city <- Shift.pause(s)("Which city?")
    nights <- Shift.pause(s)(s"How many nights in $city?")
    pay <- Shift.pause(s)(s"Pay ${nights.toInt * 90} for $city?")
  } yield if (pay == "yes") s"Booked $city for $nights nights" else "Cancelled"

  def dialogue(t: Topic, id: String = "b-1"): Dialogue[String, String, String, P] =
    Dialogue[String, String, String, P](t, id, "booking/1")(booking)

  test("a new process stands where the old one stood") {
    val t = new MemoryStore().topic("bookings")

    // ---- process 1
    val d1 = dialogue(t)
    assertEquals(place(d1).asking, Some("Which city?"))
    assertEquals(now(!.run(d1.answer("Kyiv"))).asking, Some("How many nights in Kyiv?"))
    assertEquals(now(!.run(d1.answer("3"))).asking, Some("Pay 270 for Kyiv?"))
    assertEquals(d1.journal, List("Kyiv", "3"))

    // ---- process 1 dies; the topic is all that is left
    val d2 = dialogue(t)
    assertEquals(d2.journal, List("Kyiv", "3"))
    assertEquals(place(d2).asking, Some("Pay 270 for Kyiv?"))
    assertEquals(now(!.run(d2.answer("yes"))).finished, Some("Booked Kyiv for 3 nights"))

    // ---- and a third process reads the finished dialogue as finished
    assertEquals(place(dialogue(t)).finished, Some("Booked Kyiv for 3 nights"))
  }

  test("the oracle is never asked what the journal already knows") {
    val t = new MemoryStore().topic("bookings")
    var asked = List.empty[String]
    def oracle(q: String, a: Dialogue.Attempt): String ! P = {
      val _ = a
      asked = asked :+ q
      pure[P, String](if (q.startsWith("Which")) "Lviv" else if (q.startsWith("How")) "2" else "yes")
    }

    assertEquals(!.run(dialogue(t).run(oracle)), "Booked Lviv for 2 nights")
    assertEquals(asked.size, 3)

    // a second process over the same log: the outside world is not
    // touched again
    asked = Nil
    assertEquals(!.run(dialogue(t).run(oracle)), "Booked Lviv for 2 nights")
    assertEquals(asked, Nil)
  }

  test("two dialogues in one topic do not see each other") {
    // ONE partition on purpose: the key filter is the thing under test
    val t = new MemoryStore().topic("bookings", partitions = 1)
    val a = dialogue(t, "a")
    val b = dialogue(t, "b")
    val _ = !.run(a.answer("Kyiv"))
    assertEquals(a.journal, List("Kyiv"))
    assertEquals(b.journal, Nil)
    assertEquals(place(b).asking, Some("Which city?"))
  }

  test("a record that does not decode stops the journal and names itself") {
    val t = new MemoryStore().topic("bookings")
    val d = dialogue(t)
    val _ = !.run(d.answer("Kyiv"))
    // something else wrote to this partition — no envelope, no CBOR
    val _ = t.append(0, "b-1".getBytes("UTF-8"), Array[Byte](1, 2), Ack.Durable)
    val r = d.recovered
    assertEquals(r.answers, List("Kyiv"))
    assert(!r.intact, "damage went unnoticed")
    assertEquals(r.stopped.collect { case Dialogue.Stopped.Damage(o, _) => o }, Some(1L))
    // the intact prefix is still READABLE, but `at` REFUSES to say where
    // the program stands: a record we cannot read might be an answer
    assert(!.run(d.at).isLeft, "a damaged log still claimed a place")
  }

  // ---- the cost of a step (dialogue-snapshots)

  /** a program with as many pauses as you like */
  def sumUp(n: Int)(s: Shift.Asking.Aux[Int, Int, Int, P]): Int ! Rw =
    if (n == 0) pure[Rw, Int](0)
    else for { a <- Shift.pause(s)(n); rest <- sumUp(n - 1)(s) } yield a + rest

  val N = 40

  def doubling(q: Int, a: Dialogue.Attempt): Int ! P = { val _ = a; pure[P, Int](q * 2) }

  test("the warm path does not replay: run is O(n), a loop of answer is O(n squared)") {
    val warm = new Counting(new MemoryStore().topic("warm"))
    val dw = Dialogue[Int, Int, Int, P](warm, "w", "sum/1")(sumUp(N))
    assertEquals(!.run(dw.run(doubling)), (1 to N).sum * 2)

    // the same answers, each taken from a standing start
    val cold = new Counting(new MemoryStore().topic("cold"))
    val dc = Dialogue[Int, Int, Int, P](cold, "c", "sum/1")(sumUp(N))
    var p = place(dc)
    while (p.finished.isEmpty) p = now(!.run(dc.answer(p.asking.get * 2)))
    assertEquals(p.finished, Some((1 to N).sum * 2))

    // the cold loop replays i answers at step i, so it reads 0+1+...+N-1
    // (the fold happens BEFORE the append); the warm one never reads
    assertEquals(warm.records, 0, "the warm path re-read the journal")
    assertEquals(cold.records, N * (N - 1) / 2, "the cold loop's shape is not O(n squared)")
  }

  test("a chapter makes a cold start read a tail, not a history") {
    val store = new MemoryStore

    val plainT = new Counting(store.topic("plain"))
    val plain = Dialogue[Int, Int, Int, P](plainT, "p", "sum/1")(sumUp(N))
    assertEquals(!.run(plain.run(doubling)), (1 to N).sum * 2)

    val snapT = new Counting(store.topic("snap"))
    // the snapshot topic is counted TOO: a chapter is not free
    val snapsT = new Counting(store.topic("__snaps", 1, Policy(compact = true)))
    val snaps = new Snapshots(snapsT)
    val snapped = Dialogue[Int, Int, Int, P](snapT, "s", "sum/1", Some(snaps), snapshotEvery = 10)(sumUp(N))
    assertEquals(!.run(snapped.run(doubling)), (1 to N).sum * 2)

    // ---- a new process over each log reads it from scratch
    plainT.records = 0
    snapT.records = 0
    snapsT.records = 0
    val p2 = Dialogue[Int, Int, Int, P](plainT, "p", "sum/1")(sumUp(N))
    val s2 = Dialogue[Int, Int, Int, P](snapT, "s", "sum/1", Some(snaps))(sumUp(N))
    assertEquals(p2.journal.size, N)
    assertEquals(s2.journal.size, N)          // the same journal...
    val withChapter = snapT.records + snapsT.records
    assert(withChapter * 3 < plainT.records,
      s"snapshotted start read $withChapter records (${snapT.records} journal + ${snapsT.records} chapters), plain ${plainT.records}")
    assertEquals(plainT.records, N, "the plain start did not read the whole journal")
  }

  test("the log is the truth: a chapter that is missing costs time, not correctness") {
    val store = new MemoryStore
    val t = store.topic("bothways")
    val snaps = Snapshots(store, "__snaps2")
    val d = Dialogue[Int, Int, Int, P](t, "d", "sum/1", Some(snaps), snapshotEvery = 7)(sumUp(12))
    assertEquals(!.run(d.run(doubling)), (1 to 12).sum * 2)

    // a reader with NO snapshot store sees exactly the same journal
    val bare = Dialogue[Int, Int, Int, P](t, "d", "sum/1")(sumUp(12))
    assertEquals(bare.journal, d.journal)
    assertEquals(place(bare).finished, place(d).finished)
  }
}
