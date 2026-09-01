package okay.ui

import okay.*
import okay.given
import okay.codec.{Json, Schema}
import okay.persist.MemoryStore

/**
 * Event sourcing by construction: the journal is the inbound line
 * stream, the refold is the same pure stage, and every claim here is
 * an equality between a live run and a recovery.
 */
class TestSessions extends munit.FunSuite {

  final case class Count(n: Int)
  given Schema[Count] = Schema.derived

  def view(s: Count): Ui = Ui.Column(Vector(
    Ui.Text(s"count: ${s.n}"),
    Ui.Row(Vector(Ui.Button("+", "inc"), Ui.Button("-", "dec")))))

  // the update COUNTS its calls, so a snapshot test can prove the
  // refold saw only the tail rather than assume it
  val folded = new java.util.concurrent.atomic.AtomicInteger(0)
  def update(s: Count, e: Event): Count =
    folded.incrementAndGet()
    e match
      case Event.Pressed("inc") => Count(s.n + 1)
      case Event.Pressed("dec") => Count(s.n - 1)
      case _ => s

  def press(k: String): String = Json.print(WireJson.eventJson(Event.Pressed(k)))
  def closed: String = Json.print(WireJson.eventJson(Event.Closed))

  /** a fresh session on a fresh in-memory topic */
  def fresh(key: String = "s1", partitions: Int = 2): Sessions.Session =
    Sessions.Session(MemoryStore().topic("ui", partitions), key)

  /** run the live journaled serve over a fixed list of lines */
  def live(s: Sessions.Session, lines: Seq[String]): Count =
    val sent = scala.collection.mutable.Buffer[String]()
    Sessions.serve(s)(Count(0))(view)(update)(
      okay.Source.of(lines.toList), l => async { sent += l; () }).runWith

  test("refold determinism: recovery reaches the live run's state, hostile lines included") {
    val s = fresh()
    val stream = Seq(press("inc"), press("inc"),
      press("forged-key-not-on-screen"), "{ damaged", press("dec"), closed)
    val liveEnd = live(s, stream)
    assertEquals(liveEnd, Count(1))
    val (recovered, _) = Sessions.recover(s)(Count(0))(view)(update)
    assertEquals(recovered, liveEnd)
  }

  test("crash and continue equals the uninterrupted run") {
    val whole = Seq(press("inc"), press("inc"), press("dec"), press("inc"), closed)
    // the reference: one uninterrupted run
    val ref = live(fresh("ref"), whole)

    // the crash: two lines, then the transport dies (no Closed)
    val s = fresh("crashy")
    live(s, whole.take(2))
    // a new process: recover and feed the rest
    val continued = live(s, whole.drop(2))
    assertEquals(continued, ref)
  }

  test("intent-first: appended but never processed is still history") {
    val s = fresh()
    live(s, Seq(press("inc"), closed))
    // the crash happened after the append, before the update
    Sessions.append(s, press("inc"))
    val (recovered, _) = Sessions.recover(s)(Count(0))(view)(update)
    assertEquals(recovered, Count(2))
  }

  test("snapshot bounds the refold: only the tail is folded, and the state agrees") {
    val s = fresh()
    live(s, Seq(press("inc"), press("inc"), press("inc"), closed))
    val (full, upTo) = Sessions.recover(s)(Count(0))(view)(update)
    assertEquals(full, Count(3))
    Sessions.snapshot(s, full, upTo - 1)

    // two more events after the snapshot
    Sessions.append(s, press("dec"))
    Sessions.append(s, press("dec"))

    folded.set(0)
    val (recovered, _) = Sessions.recover(s)(Count(0))(view)(update)
    assertEquals(recovered, Count(1))
    assertEquals(folded.get(), 2, "the refold saw more than the tail")

    // and the newest snapshot wins over an older one
    Sessions.snapshot(s, recovered, s.topic.end(s.partition) - 1)
    assertEquals(Sessions.latest[Count](s).map(_._1), Some(Count(1)))
  }

  test("two sessions in one topic and partition do not bleed") {
    val store = MemoryStore()
    val topic = store.topic("ui", 1)   // ONE partition: forced sharing
    val a = Sessions.Session(topic, "alice")
    val b = Sessions.Session(topic, "bob")
    live(a, Seq(press("inc"), press("inc"), closed))
    live(b, Seq(press("dec"), closed))
    assertEquals(Sessions.recover(a)(Count(0))(view)(update)._1, Count(2))
    assertEquals(Sessions.recover(b)(Count(0))(view)(update)._1, Count(-1))
  }

  test("serve = recover + journal + continue: the patches keep flowing after recovery") {
    val s = fresh()
    live(s, Seq(press("inc")))
    // the second serve recovers at 1 and continues from there; its
    // FIRST outbound line is the full tree at the recovered state
    val sent = scala.collection.mutable.Buffer[String]()
    val end = Sessions.serve(s)(Count(0))(view)(update)(
      okay.Source.of(List(press("inc"), closed)), l => async { sent += l; () }).runWith
    assertEquals(end, Count(2))
    assertEquals(WireJson.uiOf(Json.parse(sent.head)), Some(view(Count(1))))
  }
}
