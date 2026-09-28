package okay.dlm

import munit.FunSuite

/** One product-shaped smoke fixture: authored understanding, explicit
 * conversation state, a replayable verdict, and a payment fact owned by the
 * caller. It deliberately has no network or model dependency. */
class TestDlmLayaSmoke extends FunSuite:
  import Decision.*

  val duplicateCharge = Intent(
    "duplicate-charge",
    rules = Vector("(?iU)\\bдважды\\s+списали\\b"),
    slots = Vector(Slot("order", "(?iU)за\\s+заказ\\s+(\\d+)")),
    require = Vector("order"),
    ask = Map("ru" -> "Укажите номер заказа."),
    semantic = false)
  val dlm = Dlm.rules(Intents(Vector(duplicateCharge)))

  final case class Turn(route: Option[Route], pendingAct: Option[String] = None) extends Evidence:
    val courtesy = false
    val hasModel = false

  enum RefundOutcome:
    case RefundStarted(order: String)
    case NoPayment(order: String)
    case NoDuplicateCharge(order: String)
    case NotARefund

  /** This is the caller's authority, not a DLM capability: text opens a
   * review; a refund side effect happens only after ledger evidence. */
  final class PaymentLedger(settledPayments: Map[String, Int]):
    private var started = Vector.empty[String]

    def refundsStarted: Vector[String] = started

    def review(action: Action): RefundOutcome = action match
      case Action.Act(Route.Fires("duplicate-charge", slots, _)) =>
        slots.get("order") match
          case Some(order) if settledPayments.getOrElse(order, 0) == 0 => RefundOutcome.NoPayment(order)
          case Some(order) if settledPayments(order) < 2 => RefundOutcome.NoDuplicateCharge(order)
          case Some(order) =>
            started :+= order
            RefundOutcome.RefundStarted(order)
          case None => RefundOutcome.NotARefund
      case _ => RefundOutcome.NotARefund

  test("double charge: rule, state and record form a deterministic trace") {
    val state = State("ann", "ru")
    val route = dlm.router.route("С меня дважды списали за заказ 4411. Верните лишнее.", lang = Some("ru"))

    route match
      case Route.Fires("duplicate-charge", slots, Support.Exact(Some(rule))) =>
        assert(rule.contains("дважды"), rule)
        assertEquals(slots, Map("order" -> "4411"))
      case other => fail(s"expected the authored duplicate-charge rule, got $other")

    val action = decide(state, Turn(Some(route)))
    assertEquals(action, Action.Act(route))
    val record = Decision.record(action, asked = None)
    assertEquals(Decision.recall(record), Some(action))

    // The next line is interpreted in the explicit confirmation scope. Its
    // richer wording does not re-route into a new financial operation.
    val confirmation = decide(state.copy(pending = Some(Pending.Answer("refund-confirmation"))),
      Turn(None, pendingAct = Some("confirm-refund-last-order")))
    assertEquals(confirmation, Action.AnswerPending(Some("confirm-refund-last-order")))
  }

  test("a refund claim without a payment is refused before any refund side effect") {
    val forged = dlm.router.route("С меня дважды списали за заказ 9931. Верните лишнее.", lang = Some("ru"))
    val action = decide(State("mallory", "ru"), Turn(Some(forged)))
    val payments = new PaymentLedger(Map("4411" -> 2))

    assertEquals(payments.review(action), RefundOutcome.NoPayment("9931"))
    assertEquals(payments.refundsStarted, Vector.empty)
  }
