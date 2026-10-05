package okay.durable

import okay.Answers
import okay.codec.Journalled
import scala.collection.mutable

class TestRunKeys extends okay.testkit.Munit.Diagnosed:
  enum Request[+A]:
    case Call(input: String, key: Option[String] = None) extends Request[String]

  given Journalled[Request] with
    def name[A](op: Request[A]): String = "request"
    def fingerprint[A](op: Request[A]): String = op match
      case Request.Call(input, _) => input
    def withKey[A](op: Request[A], key: String): Request[A] = op match
      case Request.Call(input, _) => Request.Call(input, Some(key))
    def perform[A](op: Request[A], inner: Answers[Request]): (A, String) = op match
      case call: Request.Call =>
        val answer = inner.handle(call)
        (answer, answer)
    def decode[A](op: Request[A], written: String): A = op match
      case Request.Call(_, _) => written

  private class Provider extends Answers[Request]:
    val requests = mutable.ArrayBuffer.empty[Option[String]]
    private val receipts = mutable.Map.empty[String, String]
    var actions = 0
    def handle[A](op: Request[A]): A = op match
      case Request.Call(_, key) =>
        requests += key
        note(s"request $key")
        receipts.getOrElseUpdate(key.getOrElse(s"unkeyed-${requests.size}"), {
          actions += 1
          s"receipt-$actions"
        })

  private class Unscoped extends Durable.Journal:
    var entries = Vector.empty[Durable.Entry]
    var appends = 0
    def append(e: Durable.Entry): Unit =
      appends += 1
      entries :+= e
    def complete(seq: Int, answer: String): Unit =
      entries = entries.map(e => if e.seq == seq then e.copy(answer = Some(answer)) else e)
    def all: Vector[Durable.Entry] = entries

  private class Trace extends OpTrace:
    val keys = mutable.ArrayBuffer.empty[String]
    def span[A](name: String, attrs: (String, String)*)(body: => A): A =
      keys += attrs.toMap.apply("durable.key")
      body

  test("tenant-local run numbers are isolated and explicit identities survive restart") {
    val op = Request.Call("pay")
    val first = Durable.MemoryJournal("tenant-a/run-1")
    val second = Durable.MemoryJournal("tenant-b/run-1")
    val provider = Provider()
    onFailure(s"first=${first.all} second=${second.all} requests=${provider.requests}")
    assertEquals(Durable.over[Request](provider, first)(_ => Durable.OnRepeat.WithKey).handle(op), "receipt-1")
    assertEquals(Durable.over[Request](provider, second)(_ => Durable.OnRepeat.WithKey).handle(op), "receipt-2")
    assertEquals(provider.actions, 2)
    assertNotEquals(first.all.head.key, second.all.head.key)
    val restarted = Durable.MemoryJournal("tenant-a/run-1")
    assertEquals(Durable.keyFor(restarted, 0, op), first.all.head.key)
    assertEquals(Durable.over[Request](provider, restarted)(_ => Durable.OnRepeat.WithKey).handle(op), "receipt-1")
    assertEquals(provider.actions, 2)
  }

  test("old hash collisions do not alias independent runs and same-position drift still fails") {
    val a = Request.Call("Aa")
    val b = Request.Call("BB")
    assertEquals("Aa".hashCode, "BB".hashCode)
    assertEquals(Durable.keyFor(0, a), Durable.keyFor(0, b))
    val first = Durable.MemoryJournal("run-a")
    val second = Durable.MemoryJournal("run-b")
    val provider = Provider()
    onFailure(s"first=${first.all} second=${second.all} requests=${provider.requests}")
    assertEquals(Durable.over[Request](provider, first)(_ => Durable.OnRepeat.WithKey).handle(a), "receipt-1")
    assertEquals(Durable.over[Request](provider, second)(_ => Durable.OnRepeat.WithKey).handle(b), "receipt-2")
    val _ = intercept[Durable.Drift] {
      Durable.over[Request](provider, first)(_ => Durable.OnRepeat.WithKey).handle(b)
    }
    assertEquals(provider.actions, 2)
    assertEquals(provider.requests.size, 2)
  }

  test("legacy incomplete recovery and completed replay trace the stored key verbatim") {
    val journal = Unscoped()
    journal.append(Durable.Entry(0, "request", "pay", "old/provider-key", None))
    val provider = Provider()
    val trace = Trace()
    val op = Request.Call("pay")
    onFailure(s"journal=${journal.all} requests=${provider.requests} trace=${trace.keys}")
    assertEquals(Durable.over[Request](provider, journal)(_ => Durable.OnRepeat.WithKey,
      trace = Some(trace)).handle(op), "receipt-1")
    assertEquals(provider.requests.toVector, Vector(Some("old/provider-key")))
    assertEquals(Durable.replayingOver[Request](journal, Some(trace)).handle(op), "receipt-1")
    // A changed or invalid configured identity must not rewrite already stored keys.
    val restored = Durable.MemoryJournal("")
    restored.append(journal.all.head)
    assertEquals(Durable.over[Request](provider, restored)(trace = Some(trace)).handle(op), "receipt-1")
    assertEquals(Durable.replayingOver[Request](restored, Some(trace)).handle(op), "receipt-1")
    assertEquals(trace.keys.toVector, Vector.fill(4)("old/provider-key"))
    assertEquals(provider.actions, 1)
    assertEquals(journal.appends, 1)
  }

  test("fresh WithKey rejects absent identity before intent and external call; Redo still works") {
    val journal = Unscoped()
    val provider = Provider()
    onFailure(s"journal=${journal.all} requests=${provider.requests}")
    val _ = intercept[IllegalArgumentException] {
      Durable.over[Request](provider, journal)(_ => Durable.OnRepeat.WithKey).handle(Request.Call("pay"))
    }
    assertEquals(journal.appends, 0)
    assertEquals(provider.actions, 0)
    assertEquals(Durable.over[Request](provider, journal)(_ => Durable.OnRepeat.Redo)
      .handle(Request.Call("pay")), "receipt-1")
    assertEquals(journal.all.head.key, Durable.keyFor(0, Request.Call("pay")))
  }

  test("default identity is stable per journal and independent between journals") {
    val first = Durable.MemoryJournal()
    val second = Durable.MemoryJournal()
    val op = Request.Call("pay")
    onFailure(s"first=${first.runId} second=${second.runId}")
    assert(first.runId.nonEmpty)
    assertNotEquals(first.runId, second.runId)
    assertEquals(Durable.keyFor(first, 0, op), Durable.keyFor(first, 0, op))
    assertNotEquals(Durable.keyFor(first, 0, op), Durable.keyFor(second, 0, op))
  }

  test("keys encode valid Unicode without collisions and enforce provider-friendly bounds") {
    val op = Request.Call("pay")
    val known = Durable.keyFor(Durable.MemoryJournal("run"), 0, op)
    assertEquals(known, "okay-cnVu-0")
    val maximal = Durable.keyFor(Durable.MemoryJournal("x" * 96), Int.MaxValue, op)
    assertEquals(maximal.length, 144)
    assert(maximal.matches("[A-Za-z0-9_-]+"))
    val unicode = Durable.keyFor(Durable.MemoryJournal("tenant/付款/run1"), 0, op)
    assert(unicode.matches("[A-Za-z0-9_-]+"))
    assertNotEquals(unicode, Durable.keyFor(Durable.MemoryJournal("tenant/付款/run2"), 0, op))
    for run <- Vector("", "x" * 97, "付" * 33, "\ud800") do
      val journal = Durable.MemoryJournal(run)
      val provider = Provider()
      note(s"invalid run length=${run.length}")
      val _ = intercept[IllegalArgumentException] {
        Durable.over[Request](provider, journal)(_ => Durable.OnRepeat.WithKey).handle(op)
      }
      assertEquals(journal.all, Vector.empty)
      assertEquals(provider.actions, 0)
    val _ = intercept[IllegalArgumentException] {
      Durable.keyFor(Durable.MemoryJournal("run"), -1, op)
    }
  }
