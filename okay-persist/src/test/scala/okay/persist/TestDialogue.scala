package okay.persist

import munit.FunSuite
import okay.{!, +, Delim, Pure}
import okay.Direct.*
import okay.codec.Schema
import scala.language.implicitConversions

/**
 * THE DIALOGUE WHOSE JOURNAL IS A TOPIC (durable-dialogue,
 * 2026-09-17). The property under test is the event-sourcing one: a
 * new process over the same log stands exactly where the old one
 * stood, and asks the outside world nothing it has already been told.
 */
class TestDialogue extends FunSuite {

  type Row = Delim + Pure

  /** the program: straight-line code that happens to pause */
  def booking(using Delim.Asking[String, String, String, Row]): String ! Row = direct:
    val city = !Delim.pause("Which city?")
    val nights = !Delim.pause(s"How many nights in $city?")
    val pay = !Delim.pause(s"Pay ${nights.toInt * 90} for $city?")
    if pay == "yes" then s"Booked $city for $nights nights" else "Cancelled"

  def dialogue(t: Topic, id: String = "b-1") =
    Dialogue[String, String, String, Pure](t, id, "booking/1")(booking)

  /** where a dialogue stands, for a test that knows the log is intact */
  extension [Q, A, R](d: Dialogue[Q, A, R, Pure])
    def place: Delim.Dialogue[Q, A, R, Pure] = (!.run(d.at)).toOption.get

  /** where an answer left it, for a test that expects it to be taken */
  extension [Q, A, R, G[+_]](x: Dialogue.Answered[Q, A, R, G])
    def now: Delim.Dialogue[Q, A, R, G] = x match
      case Dialogue.Answered.Advanced(to) => to
      case Dialogue.Answered.NotAsking(to) => to
      case Dialogue.Answered.Lost(to) => to
      case Dialogue.Answered.Broken(w) => sys.error(s"broken: $w")

  test("a new process stands where the old one stood") {
    val t = MemoryStore().topic("bookings")

    // ---- process 1
    val d1 = dialogue(t)
    assertEquals(d1.place.asking, Some("Which city?"))
    assertEquals((!.run(d1.answer("Kyiv"))).now.asking, Some("How many nights in Kyiv?"))
    assertEquals((!.run(d1.answer("3"))).now.asking, Some("Pay 270 for Kyiv?"))
    assertEquals(d1.journal, List("Kyiv", "3"))

    // ---- process 1 dies. d1 and every continuation it held go with
    //      it; the topic is all that is left.
    val d2 = dialogue(t)
    assertEquals(d2.journal, List("Kyiv", "3"))
    assertEquals(d2.place.asking, Some("Pay 270 for Kyiv?"))
    assertEquals((!.run(d2.answer("yes"))).now.finished, Some("Booked Kyiv for 3 nights"))

    // ---- and a third process reads the finished dialogue as finished
    assertEquals(dialogue(t).place.finished, Some("Booked Kyiv for 3 nights"))
  }

  test("the oracle is never asked what the journal already knows") {
    val t = MemoryStore().topic("bookings")
    var asked = List.empty[String]
    def oracle(q: String, a: Dialogue.Attempt): String ! Pure =
      asked = asked :+ q
      okay.pure(if q.startsWith("Which") then "Lviv"
                else if q.startsWith("How") then "2" else "yes")

    assertEquals(!.run(dialogue(t).run(oracle)), "Booked Lviv for 2 nights")
    assertEquals(asked.size, 3)

    // a second process over the same log: the whole dialogue is
    // already journaled, so the outside world is not touched again
    asked = Nil
    assertEquals(!.run(dialogue(t).run(oracle)), "Booked Lviv for 2 nights")
    assertEquals(asked, Nil)
  }

  test("two dialogues in one topic do not see each other") {
    // ONE partition on purpose: with two, routing would separate them
    // and the key filter — the thing under test — would never run
    val t = MemoryStore().topic("bookings", partitions = 1)
    val a = dialogue(t, "a")
    val b = dialogue(t, "b")
    val _ = !.run(a.answer("Kyiv"))
    assertEquals(a.journal, List("Kyiv"))
    assertEquals(b.journal, Nil)
    assertEquals(b.place.asking, Some("Which city?"))
  }

  test("a record that does not decode stops the journal and names itself") {
    val t = MemoryStore().topic("bookings")
    val d = dialogue(t)
    val _ = !.run(d.answer("Kyiv"))
    // something else wrote to this partition — no envelope, no CBOR
    val _ = t.append(0, "b-1".getBytes("UTF-8"), Array[Byte](1, 2), Ack.Durable)
    val r = d.recovered
    assertEquals(r.answers, List("Kyiv"))
    assert(!r.intact, "damage went unnoticed")
    assertEquals(r.stopped.collect { case Dialogue.Stopped.Damage(o, _) => o }, Some(1L))
    // the intact prefix is still READABLE...
    assertEquals(r.answers, List("Kyiv"))
    // ...but `at` REFUSES to say where the program stands, which
    // changed with durable-workflow stage 0 and is the safer half of
    // the trade: a record we cannot read might be an answer, and
    // carrying on past it re-asks a question the outside world has
    // already answered — a duplicate side effect. An operator tool
    // that deliberately wants the prefix has `recovered` + replay.
    assert((!.run(d.at)).isLeft, "a damaged log still claimed a place")
  }

  // ---- the cost of a step (dialogue-snapshots, 2026-09-17)

  /** a Topic that counts what is read through it */
  final class Counting(under: Topic) extends Topic:
    var reads = 0
    var records = 0
    def name: String = under.name
    def partitions: Int = under.partitions
    def append(p: Int, k: Array[Byte], v: Array[Byte], a: Ack): Long = under.append(p, k, v, a)
    def read(p: Int, from: Long, max: Int): Topic.Read =
      reads += 1
      val r = under.read(p, from, max)
      r match
        case Topic.Read.Records(rs) => records += rs.size
        case _ => ()
      r
    def begin(p: Int): Long = under.begin(p)
    def end(p: Int): Long = under.end(p)
    def compact(p: Int): Unit = under.compact(p)

  /** a program with as many pauses as you like */
  def sumUp(n: Int)(using Delim.Asking[Int, Int, Int, Row]): Int ! Row = direct:
    if n == 0 then 0 else (!Delim.pause(n)) + (!sumUp(n - 1))

  val N = 40

  test("the warm path does not replay: run is O(n), a loop of answer is O(n squared)") {
    def oracle(q: Int, a: Dialogue.Attempt): Int ! Pure = okay.pure(q * 2)

    val warm = Counting(MemoryStore().topic("warm"))
    val dw = Dialogue[Int, Int, Int, Pure](warm, "w", "sum/1")(sumUp(N))
    assertEquals(!.run(dw.run(oracle)), (1 to N).sum * 2)

    // the same answers, each taken from a standing start
    val cold = Counting(MemoryStore().topic("cold"))
    val dc = Dialogue[Int, Int, Int, Pure](cold, "c", "sum/1")(sumUp(N))
    def loop(p: Delim.Dialogue[Int, Int, Int, Pure]): Int = p match
      case Delim.Paused.Done(r) => r
      case Delim.Paused.Ask(q, _) => loop((!.run(dc.answer(q * 2))).now)
    assertEquals(loop(dc.place), (1 to N).sum * 2)

    // Both answered the same questions; only one of them re-read the
    // journal to do it. Exact numbers, because MemoryStore is
    // deterministic and a chunk of 256 swallows the whole journal:
    // the cold loop replays i answers at step i, so it reads
    // 1+2+...+N; the warm one starts from an empty journal and then
    // never reads at all.
    assertEquals(warm.records, 0, s"the warm path re-read the journal")
    // N*(N-1)/2, not N*(N+1)/2: since durable-workflow stage 0 the
    // fold happens BEFORE the append (that is what lets a refused
    // answer leave the journal alone), so step i reads i records
    // rather than i+1. The shape is the point, and it is unchanged.
    assertEquals(cold.records, N * (N - 1) / 2, s"the cold loop's shape is not O(n squared)")
  }

  test("a chapter makes a cold start read a tail, not a history") {
    def oracle(q: Int, a: Dialogue.Attempt): Int ! Pure = okay.pure(q * 2)
    val store = MemoryStore()

    val plainT = Counting(store.topic("plain"))
    val plain = Dialogue[Int, Int, Int, Pure](plainT, "p", "sum/1")(sumUp(N))
    assertEquals(!.run(plain.run(oracle)), (1 to N).sum * 2)

    val snapT = Counting(store.topic("snap"))
    // the snapshot topic is counted TOO: a chapter is not free, it is
    // one scan of a compacted topic, and the comparison is only
    // honest if that scan is on the bill
    val snapsT = Counting(store.topic("__snaps", 1, Policy(compact = true)))
    val snaps = new Snapshots(snapsT)
    val snapped = Dialogue[Int, Int, Int, Pure](snapT, "s", "sum/1", Some(snaps), snapshotEvery = 10)(sumUp(N))
    assertEquals(!.run(snapped.run(oracle)), (1 to N).sum * 2)

    // ---- a new process over each log reads it from scratch
    plainT.records = 0
    snapT.records = 0
    snapsT.records = 0
    val p2 = Dialogue[Int, Int, Int, Pure](plainT, "p", "sum/1")(sumUp(N))
    val s2 = Dialogue[Int, Int, Int, Pure](snapT, "s", "sum/1", Some(snaps))(sumUp(N))
    assertEquals(p2.journal.size, N)
    assertEquals(s2.journal.size, N)          // the same journal...
    val withChapter = snapT.records + snapsT.records
    assert(withChapter * 3 < plainT.records,
      s"snapshotted start read $withChapter records (${snapT.records} journal + " +
        s"${snapsT.records} chapters), plain ${plainT.records}")
    assertEquals(plainT.records, N, "the plain start did not read the whole journal")
  }

  test("the log is the truth: a chapter that is missing costs time, not correctness") {
    def oracle(q: Int, a: Dialogue.Attempt): Int ! Pure = okay.pure(q * 2)
    val store = MemoryStore()
    val t = store.topic("bothways")
    val snaps = Snapshots(store, "__snaps2")
    val d = Dialogue[Int, Int, Int, Pure](t, "d", "sum/1", Some(snaps), snapshotEvery = 7)(sumUp(12))
    assertEquals(!.run(d.run(oracle)), (1 to 12).sum * 2)

    // a reader with NO snapshot store sees exactly the same journal
    val bare = Dialogue[Int, Int, Int, Pure](t, "d", "sum/1")(sumUp(12))
    assertEquals(bare.journal, d.journal)
    assertEquals(bare.place.finished, d.place.finished)
  }
}