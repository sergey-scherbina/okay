package okay.dlm

import munit.FunSuite

/** The DLM counterpart of the Jev SDK's examples
 * (https://github.com/ticofab/scala-jev-sdk, `examples/Triage.scala`):
 * one triage pass over an inbound support message. Jev asks a hosted
 * model and gets probabilities back; DLM answers from authored rules,
 * offline, or says it cannot. */
object TestDlmJevExamples:
  object Triage:

    enum Team:
      case Billing, Technical, Sales

    /** Rules are written as keywords: `payout*` is a word prefix, a
     * keyword with a space is a phrase (specs/dlm-rule-keywords.md). */
    val model: Dlm = Dlm.rules(Intents(Vector(
      Intent("billing", rules = Rule.keywords("payout*", "invoice*"), semantic = false),
      Intent("technical", rules = Rule.keywords("crash*", "outage", "bug"), semantic = false),
      Intent("sales", rules = Rule.keywords("pricing", "upgrade"), semantic = false),
      Intent("refund", rules = Rule.keywords("charged twice"),
        slots = Vector(Slot("order", "(?i)\\border\\s+(\\d+)")), require = Vector("order"),
        ask = Map("en" -> "Which order was charged twice?"), semantic = false))))

    /** The answer comes back as a `Team`, not as a string to re-parse;
     * `None` when no rule owns the message. */
    def department(message: String): Option[Team] = model.router.route(message) match
      case Route.Fires("billing" | "refund", _, _) => Some(Team.Billing)
      case Route.Fires("technical", _, _) => Some(Team.Technical)
      case Route.Fires("sales", _, _) => Some(Team.Sales)
      case _ => None

    private val urgent = Rule.keywords("help!", "asap", "urgent").map(_.r)

    def isUrgent(message: String): Boolean = urgent.exists(_.findFirstIn(message).nonEmpty)

    val message: String = "Help! My payouts have been failing for 3 days and nobody has replied."

    enum Refund:
      case Started(order: String)
      case NoPayment(order: String)
      case AskOrder(question: String)

    /** Text never proves a payment: the refund decision reads the ledger. */
    final class Ledger(payments: Map[String, Int]):
      var started = Vector.empty[String]
      def refund(message: String): Option[Refund] = model.router.route(message) match
        case Route.Fires("refund", slots, _) =>
          val order = slots("order")
          if payments.getOrElse(order, 0) < 2 then Some(Refund.NoPayment(order))
          else
            started :+= order
            Some(Refund.Started(order))
        case Route.Missing("refund", _) =>
          model.intents.byName("refund").flatMap(_.ask.get("en")).map(Refund.AskOrder(_))
        case _ => None

class TestDlmJevExamples extends FunSuite:
  import TestDlmJevExamples.Triage.*

  test("showcase") {
    assertEquals(department(message), Some(Team.Billing))
    assert(isUrgent(message))

    assertEquals(department("The app crashes when I open settings."), Some(Team.Technical))
    assertEquals(department("Is there a discount if we upgrade to the annual plan?"), Some(Team.Sales))
    assertEquals(department("What is the weather like?"), None, "no rule owns it: no team is invented")
    assert(!isUrgent("The app crashes when I open settings."))

    val ledger = Ledger(Map("4411" -> 2))
    assertEquals(ledger.refund("I was charged twice for order 4411."), Some(Refund.Started("4411")))
    assertEquals(ledger.refund("I was charged twice for order 9931."), Some(Refund.NoPayment("9931")))
    assertEquals(ledger.refund("I was charged twice!"), Some(Refund.AskOrder("Which order was charged twice?")))
    assertEquals(ledger.started, Vector("4411"))
  }
