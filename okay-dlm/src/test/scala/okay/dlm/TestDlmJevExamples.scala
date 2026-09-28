package okay.dlm

import munit.FunSuite

/** One offline, deterministic counterpart of the public Jev/SystemOne
 * examples: typed support triage, urgency, an explicit dialogue state and a
 * financial side-effect boundary. */
class TestDlmJevExamples extends FunSuite:
  import Decision.*

  enum Team:
    case Billing, Technical, Unclear

  enum Urgency:
    case CanWait, ThisWeek, Today

  final case class Triage(billing: Boolean, team: Team,
                          urgency: Option[Urgency], support: Option[Support])

  final case class Turn(route: Option[Route], pendingAct: Option[String] = None) extends Evidence:
    val courtesy = false
    val hasModel = false

  enum RefundOutcome:
    case RefundStarted(order: String)
    case NoPayment(order: String)
    case NoDuplicateCharge(order: String)
    case NotARefund

  val model = Dlm.rules(Intents(Vector(
    Intent("billing", rules = Vector("(?iU)\\bcharged\\s+twice\\b"), semantic = false),
    Intent("technical", rules = Vector("(?iU)\\b(?:app|site)\\s+(?:is\\s+)?(?:down|crashing)\\b"), semantic = false),
    Intent("duplicate-charge", rules = Vector("(?iU)\\bдважды\\s+списали\\b"),
      slots = Vector(Slot("order", "(?iU)за\\s+заказ\\s+(\\d+)")),
      require = Vector("order"), ask = Map("ru" -> "Укажите номер заказа."), semantic = false))))

  private def urgencyOf(text: String): Urgency =
    if "(?iU)\\b(?:asap|today)\\b".r.findFirstIn(text).nonEmpty then Urgency.Today
    else if "(?iU)\\bthis\\s+week\\b".r.findFirstIn(text).nonEmpty then Urgency.ThisWeek
    else Urgency.CanWait

  private def triage(text: String): Triage = model.router.route(text) match
    case Route.Fires("billing", _, support) => Triage(true, Team.Billing, Some(urgencyOf(text)), Some(support))
    case Route.Fires("technical", _, support) => Triage(false, Team.Technical, Some(urgencyOf(text)), Some(support))
    case _ => Triage(false, Team.Unclear, None, None)

  /** The caller owns financial facts. A route opens review; it never proves a
   * payment or performs a refund without the ledger. */
  final class PaymentLedger(settledPayments: Map[String, Int]):
    private var started = Vector.empty[String]
    def refundsStarted: Vector[String] = started
    def review(action: Action): RefundOutcome = action match
      case Action.Act(Route.Fires("duplicate-charge", slots, _)) =>
        slots.get("order") match
          case Some(order) if settledPayments.getOrElse(order, 0) == 0 => RefundOutcome.NoPayment(order)
          case Some(order) if settledPayments(order) < 2 => RefundOutcome.NoDuplicateCharge(order)
          case Some(order) => started :+= order; RefundOutcome.RefundStarted(order)
          case None => RefundOutcome.NotARefund
      case _ => RefundOutcome.NotARefund

  test("showcase: local deterministic counterparts of Jev examples") {
    // quickstart: billing Noul, team Choice and urgency Score
    val quickstart = triage("I was charged twice. Please fix this ASAP.")
    assertEquals((quickstart.billing, quickstart.team, quickstart.urgency),
      (true, Team.Billing, Some(Urgency.Today)), "quickstart: billing, team and urgency")
    quickstart.support match
      case Some(Support.Exact(Some(rule))) => assert(rule.contains("charged"), rule)
      case other => fail(s"quickstart must name the duplicate-charge rule, got $other")

    // ticket triage: technical tickets are owned by technical, not billing
    val technical = triage("The app is crashing; this week is fine.")
    assertEquals((technical.billing, technical.team, technical.urgency),
      (false, Team.Technical, Some(Urgency.ThisWeek)), "ticket triage: technical ownership")

    // typed uncertainty: an unowned ticket is not forced into a team or urgency
    val unknown = triage("What is the weather ASAP?")
    assertEquals((unknown.billing, unknown.team, unknown.urgency, unknown.support),
      (false, Team.Unclear, None, None), "typed uncertainty: no forced answer")

    // refund request: record/replay keeps the authored action stable
    val state = State("ann", "ru")
    val route = model.router.route("С меня дважды списали за заказ 4411. Верните лишнее.", lang = Some("ru"))
    route match
      case Route.Fires("duplicate-charge", slots, Support.Exact(Some(rule))) =>
        assert(rule.contains("дважды"), rule)
        assertEquals(slots, Map("order" -> "4411"))
      case other => fail(s"refund request must name duplicate-charge, got $other")
    val action = decide(state, Turn(Some(route)))
    assertEquals(Decision.recall(Decision.record(action, asked = None)), Some(action), "refund request: replay")

    // multi-turn support: a scoped confirmation is not a new financial command
    val confirmation = decide(state.copy(pending = Some(Pending.Answer("refund-confirmation"))),
      Turn(None, pendingAct = Some("confirm-refund-last-order")))
    assertEquals(confirmation, Action.AnswerPending(Some("confirm-refund-last-order")), "pending confirmation")

    // financial boundary: no ledger payment means no refund side effect
    val forged = model.router.route("С меня дважды списали за заказ 9931. Верните лишнее.", lang = Some("ru"))
    val payments = new PaymentLedger(Map("4411" -> 2))
    assertEquals(payments.review(decide(State("mallory", "ru"), Turn(Some(forged)))), RefundOutcome.NoPayment("9931"), "payment ledger: refusal")
    assertEquals(payments.refundsStarted, Vector.empty, "payment ledger: no refund side effect")
  }
