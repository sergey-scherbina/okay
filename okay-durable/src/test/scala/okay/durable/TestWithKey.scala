package okay.durable

import okay.Answers
import okay.codec.Journalled
import scala.collection.mutable

class TestWithKey extends okay.testkit.Munit.Diagnosed:
  enum Pay[+A]:
    case Charge(amount: Int, key: Option[String] = None) extends Pay[String]

  given Journalled[Pay] with
    def name[A](op: Pay[A]): String = "charge"
    def fingerprint[A](op: Pay[A]): String = op match
      case Pay.Charge(amount, key) => s"charge($amount,$key)"
    def withKey[A](op: Pay[A], key: String): Pay[A] = op match
      case Pay.Charge(amount, _) => Pay.Charge(amount, Some(key))
    def perform[A](op: Pay[A], inner: Answers[Pay]): (A, String) = op match
      case charge: Pay.Charge =>
        val answer = inner.handle(charge)
        (answer, answer)
    def decode[A](op: Pay[A], written: String): A = op match
      case Pay.Charge(_, _) => written

  private class Provider(before: () => Unit = () => ()) extends Answers[Pay]:
    val requests = mutable.ArrayBuffer.empty[Option[String]]
    private val receipts = mutable.Map.empty[String, String]
    var actions = 0
    def handle[A](op: Pay[A]): A = op match
      case Pay.Charge(_, key) =>
        before()
        requests += key
        note(s"provider request $key")
        receipts.getOrElseUpdate(key.getOrElse(s"unkeyed-${requests.size}"), {
          actions += 1
          s"receipt-$actions"
        })

  private class CompleteFailsOnce(val underlying: Durable.MemoryJournal) extends Durable.Journal:
    private var fail = true
    def append(entry: Durable.Entry): Unit = underlying.append(entry)
    def all: Vector[Durable.Entry] = underlying.all
    def complete(seq: Int, answer: String): Unit =
      if fail then
        fail = false
        throw IllegalStateException("completion was not persisted")
      underlying.complete(seq, answer)

  test("fresh WithKey writes intent first and transports its key") {
    val j = Durable.MemoryJournal()
    val provider = Provider(() => assertEquals(j.all.size, 1, "intent precedes the external call"))
    onFailure(s"journal=${j.all} requests=${provider.requests}")
    assertEquals(Durable.over[Pay](provider, j)(_ => Durable.OnRepeat.WithKey)
      .handle(Pay.Charge(100)), "receipt-1")
    assertEquals(provider.requests.toVector, Vector(Some(j.all.head.key)))
    assertEquals(j.all.head.fingerprint, "charge(100,None)")
  }

  test("remote success before lost completion retries one keyed business action") {
    val j = CompleteFailsOnce(Durable.MemoryJournal())
    val provider = Provider(() => assertEquals(j.all.size, 1))
    val op = Pay.Charge(100)
    onFailure(s"journal=${j.all} requests=${provider.requests} actions=${provider.actions}")
    val _ = intercept[IllegalStateException] {
      Durable.over[Pay](provider, j)(_ => Durable.OnRepeat.WithKey).handle(op)
    }
    assertEquals(j.all.head.answer, None)
    assertEquals(Durable.over[Pay](provider, j)(_ => Durable.OnRepeat.WithKey).handle(op), "receipt-1")
    assertEquals(provider.actions, 1)
    assertEquals(provider.requests.toVector, Vector.fill(2)(Some(j.all.head.key)))
    assertEquals(Durable.over[Pay](provider, j)(_ => Durable.OnRepeat.WithKey).handle(op), "receipt-1")
    assertEquals(Durable.replayingOver[Pay](j).handle(op), "receipt-1")
    assertEquals(provider.requests.size, 2, "completed replay never reaches the provider")
    val _ = intercept[Durable.Drift] {
      Durable.over[Pay](provider, j)(_ => Durable.OnRepeat.WithKey).handle(Pay.Charge(101))
    }
    assertEquals(provider.requests.size, 2)
  }

  test("failed intent append never executes the external request") {
    val j = new Durable.Journal:
      def append(entry: Durable.Entry): Unit = throw IllegalStateException("intent unavailable")
      def complete(seq: Int, answer: String): Unit = fail("complete must not run")
      def all: Vector[Durable.Entry] = Vector.empty
    val provider = Provider()
    onFailure(s"requests=${provider.requests}")
    val _ = intercept[IllegalStateException] {
      Durable.over[Pay](provider, j)(_ => Durable.OnRepeat.WithKey).handle(Pay.Charge(100))
    }
    assertEquals(provider.actions, 0)
  }

  test("other fresh-call policies leave the operation's key unchanged") {
    for policy <- Vector(Durable.OnRepeat.Redo, Durable.OnRepeat.Fail,
      Durable.OnRepeat.Reconcile, Durable.OnRepeat.Escalate) do
      val provider = Provider()
      val j = Durable.MemoryJournal()
      val op = Pay.Charge(100, Some("client-key"))
      note(s"fresh policy $policy")
      assertEquals(Durable.over[Pay](provider, j)(_ => policy).handle(op), "receipt-1")
      assertEquals(provider.requests.toVector, Vector(Some("client-key")))
  }
