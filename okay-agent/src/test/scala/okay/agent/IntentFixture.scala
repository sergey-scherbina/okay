package okay.agent

import okay.codec.Schema

/**
 * The labelled fixture the intent lanes measure against
 * (specs/intent-classify.md).
 *
 * It lives in a source rather than a resource file so both platforms
 * can read it, and it is shared rather than private to one suite so
 * the next lane compares against the SAME 24 messages instead of
 * inventing its own and quietly moving the baseline.
 *
 * The taxonomy here is deliberately FLAT and its cases carry one
 * plain slot. The question these messages are asked to settle is
 * whether the out-of-domain bucket survives, and a hierarchy or a
 * slot that can itself fail (an ISO-8601 `When`) would add failure
 * modes that confound that answer. `TestClassify` keeps the nested
 * taxonomy for the structural claims.
 */
object IntentFixture {

  enum Support derives Schema:
    case Proposal(what: String)
    case Request(what: String)
    case Notification(what: String)
    case Other(what: String)

  /** message, and the case name it should be read as */
  val labelled: List[(String, String)] = List(
    "Je vous propose de faire une reunion jeudi prochain a 14h." -> "Proposal",
    "Can we move Thursday's sync to Friday morning instead?" -> "Proposal",
    "How about we meet next Tuesday to go over the numbers?" -> "Proposal",
    "I'd like to suggest a 30-minute call sometime this week." -> "Proposal",
    "Shall we reschedule our 1:1 to after the release?" -> "Proposal",
    "Proposing we push the design review to next Monday 10am." -> "Proposal",
    "Could you send me the deck before tomorrow's meeting?" -> "Request",
    "Please confirm whether you can attend on Friday." -> "Request",
    "Can you share the notes from yesterday's standup?" -> "Request",
    "I need the invoice for last month, could you forward it?" -> "Request",
    "Would you be able to review my PR before the demo?" -> "Request",
    "Please book a room for six people for Wednesday." -> "Request",
    "Just letting you know the office will be closed on Monday." -> "Notification",
    "FYI the meeting room has been changed to B2." -> "Notification",
    "Heads up: I will be on leave next week." -> "Notification",
    "The quarterly report has been published on the intranet." -> "Notification",
    "Reminder that the deadline is this Friday." -> "Notification",
    "Our call tomorrow is cancelled, no action needed." -> "Notification",
    // out of domain: none of these is a meeting intent at all, and
    // every one of them was absorbed into a positive class by the
    // first measurement
    "My card was charged twice this month, please refund." -> "Other",
    "The app crashes every time I open the billing page." -> "Other",
    "Happy birthday! Hope you have a great day." -> "Other",
    "Thanks a lot, that was really helpful." -> "Other",
    "I want to cancel my subscription effective immediately." -> "Other",
    "Here is the recipe you asked about at lunch." -> "Other")

  /** examples for the prompt — deliberately NOT drawn from `labelled`,
   * so an arm that shows examples is not being scored on its own
   * teaching material */
  val examples: List[(String, Support)] = List(
    "Are you free to meet on Wednesday afternoon?" -> Support.Proposal("meet Wednesday"),
    "Please forward me the signed contract." -> Support.Request("forward the contract"),
    "Note that payroll runs a day early this month." -> Support.Notification("payroll early"),
    "What is the capital of Portugal?" -> Support.Other("general knowledge"),
    "My headphones arrived broken, I want a replacement." -> Support.Other("a support issue"))

  val classes: List[String] = List("Proposal", "Request", "Notification", "Other")
}
