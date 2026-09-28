package okay.dlm

import munit.FunSuite

/** The same support-ticket shape as kataras/jev's quickstart, but answered
 * in-process by authored DLM rules rather than a hosted probability model. */
class TestDlmJevQuickstart extends FunSuite:

  enum Team:
    case Billing, Technical, Unclear

  enum Urgency:
    case CanWait, ThisWeek, Today

  final case class Answers(billing: Boolean, team: Team,
                           urgency: Option[Urgency], support: Option[Support])

  val model = Dlm.rules(Intents(Vector(
    Intent("billing", rules = Vector("(?iU)\\bcharged\\s+twice\\b"), semantic = false),
    Intent("technical", rules = Vector("(?iU)\\b(?:app|site)\\s+(?:is\\s+)?(?:down|crashing)\\b"), semantic = false))))

  private def urgencyOf(text: String): Urgency =
    if "(?iU)\\b(?:asap|today)\\b".r.findFirstIn(text).nonEmpty then Urgency.Today
    else if "(?iU)\\bthis\\s+week\\b".r.findFirstIn(text).nonEmpty then Urgency.ThisWeek
    else Urgency.CanWait

  /** Application policy composes independently testable, deterministic
   * answers. The route makes a team owned; urgency is deliberately absent
   * when the ticket belongs to nobody. */
  def answer(text: String): Answers = model.router.route(text) match
    case Route.Fires("billing", _, support) =>
      Answers(billing = true, Team.Billing, Some(urgencyOf(text)), Some(support))
    case Route.Fires("technical", _, support) =>
      Answers(billing = false, Team.Technical, Some(urgencyOf(text)), Some(support))
    case _ => Answers(billing = false, Team.Unclear, None, None)

  test("the Jev quickstart ticket has local billing, team and urgency answers") {
    val answers = answer("I was charged twice. Please fix this ASAP.")
    assertEquals(answers.billing, true)
    assertEquals(answers.team, Team.Billing)
    assertEquals(answers.urgency, Some(Urgency.Today))
    answers.support match
      case Some(Support.Exact(Some(rule))) => assert(rule.contains("charged"), rule)
      case other => fail(s"expected the duplicate-charge rule, got $other")
  }

  test("the same policy distinguishes technical work and refuses an unowned ticket") {
    val technical = answer("The app is crashing; this week is fine.")
    assertEquals((technical.billing, technical.team, technical.urgency),
      (false, Team.Technical, Some(Urgency.ThisWeek)))

    val unknown = answer("What is the weather ASAP?")
    assertEquals((unknown.billing, unknown.team, unknown.urgency, unknown.support),
      (false, Team.Unclear, None, None))
  }
