package okay.outbox

import okay.*
import okay.given
import okay.persist.{Ack, MemoryStore, Offsets, Record, Typed, of}

/**
 * Dead-lettering over a MemoryStore (specs/outbox.md): a poison
 * record is retried, parked with its error, and passed; the
 * partition goes on; a replay brings it back.
 */
class TestDeadLetter extends munit.FunSuite:

  def run[A](p: A ! Async): A = Async.run(p).runWith

  def text(b: Array[Byte]): String = new String(b, "UTF-8")

  test("a poison record is retried `attempts` times, parked with its error, and the partition is not blocked") {
    val store = MemoryStore()
    val orders = store.topic("orders")
    val offsets = Offsets(store)
    val dlq = store.topic("orders.dlq").of[DeadLetter.Dead]()
    for i <- 1 to 4 do orders.append("k".getBytes, s"o$i".getBytes("UTF-8"), Ack.Durable): Unit

    var handled = Vector.empty[String]
    var triesOnPoison = 0
    def handle(r: Record): Unit ! Async = okay.async {
      val v = text(r.value)
      if v == "o2" then { triesOnPoison += 1; throw IllegalStateException(s"cannot ship $v") }
      handled :+= v
    }

    val passed = run(DeadLetter.consume(orders, 0, "shipping", offsets, dlq, attempts = 3, clock = () => 7L)(handle))
    assertEquals(passed, 4)
    assertEquals(handled, Vector("o1", "o3", "o4"))
    assertEquals(triesOnPoison, 3)
    assertEquals(offsets.committed("shipping", "orders", 0), Some(4L))

    val dead = dlq.read(0, 0, 10) match
      case Typed.Read.Records(rs) => rs.collect { case Typed.Decoded.Ok(_, _, _, d) => d }
      case _ => fail("too early")
    assertEquals(dead.size, 1)
    assertEquals((dead.head.topic, dead.head.part, dead.head.offset, dead.head.attempts, dead.head.at), ("orders", 0, 1L, 3, 7L))
    assertEquals(text(dead.head.value), "o2")
    assertEquals(dead.head.error, "cannot ship o2")

    // a second pass from the committed offset meets nothing new
    assertEquals(run(DeadLetter.consume(orders, 0, "shipping", offsets, dlq)(handle)), 0)
  }

  test("a handler that succeeds on a retry parks nothing") {
    val store = MemoryStore()
    val t = store.topic("t")
    val offsets = Offsets(store)
    val dlq = store.topic("t.dlq").of[DeadLetter.Dead]()
    t.append("k".getBytes, "flaky".getBytes, Ack.Durable): Unit
    var tries = 0
    val passed = run(DeadLetter.consume(t, 0, "g", offsets, dlq, attempts = 3) { _ =>
      okay.async { tries += 1; if tries < 2 then throw RuntimeException("once") }
    })
    assertEquals((passed, tries), (1, 2))
    assertEquals(store.topic("t.dlq").end(0), 0L)
  }

  test("replay: a dead record goes back onto its topic and the consumer meets it again") {
    val store = MemoryStore()
    val orders = store.topic("orders")
    val offsets = Offsets(store)
    val dlq = store.topic("orders.dlq").of[DeadLetter.Dead]()
    orders.append("k1".getBytes, "bad".getBytes, Ack.Durable): Unit
    var fixed = false
    def handle(r: Record): Unit ! Async = okay.async { val _ = r; if !fixed then throw RuntimeException("not yet") }

    assertEquals(run(DeadLetter.consume(orders, 0, "g", offsets, dlq, attempts = 2)(handle)), 1)
    assertEquals(store.topic("orders.dlq").end(0), 1L)

    fixed = true
    assertEquals(run(DeadLetter.replay(dlq, store, from = 0)), 1L)
    assertEquals(orders.end(0), 2L)                           // the record is back, at a new offset
    assertEquals(run(DeadLetter.consume(orders, 0, "g", offsets, dlq)(handle)), 1)
    assertEquals(store.topic("orders.dlq").end(0), 1L)        // nothing new parked
    assertEquals(offsets.committed("g", "orders", 0), Some(2L))
  }
