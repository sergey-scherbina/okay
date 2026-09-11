package okay.chat.leads

import okay.chat.Chat
import okay.http.Request
import okay.llm.Anthropic

/**
 * THE HOOK, and the reason this ledger lives in okay-chat rather than
 * beside it: `Chat.chatRoute` already hands a `TurnOverride` first
 * refusal at every turn, with the request and the whole history in
 * hand. Recording is therefore a DECORATOR of that seam and not a
 * second place where turns are intercepted —
 *
 *     Chat.chatRoute(model, budget,
 *       Recording.turns(ledger, salt)(myOverride))
 *
 * — and a consumer that has no override of its own passes none.
 *
 * What it records is the LAST USER message, because that is the ask;
 * the assistant's answer is not a lead and the history was recorded
 * when it arrived. Recording never intercepts: it returns whatever the
 * wrapped override returned, so a ledger cannot change what a person
 * is told, which is the same rule the sponsored-results question
 * answers in the other direction.
 */
object Recording:

  /** where a conversation's identity comes from. A header by default —
   * the chat client sends one it generated; it is not a user id and
   * this module never tries to make it one (`Lead.pseudonym` hashes it
   * anyway). A request without one is one conversation per request,
   * which undercounts people and is the honest failure. */
  def sessionOf(header: String = "x-chat-session"): Request => String =
    r =>
      val n = header.toLowerCase
      r.headers.collectFirst { case (k, v) if k.toLowerCase == n => v }
        .getOrElse("anonymous-" + java.util.UUID.randomUUID().toString)

  /** the last thing the person said, which is the ask */
  def lastUser(messages: Seq[Anthropic.Message]): Option[String] =
    messages.reverseIterator.find(_.role == "user").map(_.content).filter(_.trim.nonEmpty)

  /**
   * Record every turn into the ledger, then defer to `next`. Failures
   * to write are SWALLOWED on purpose: a full disk must not take the
   * chat down with it, and a lost row is worth less than a refused
   * answer. What it costs is one parse of one message and one appended
   * line — no model, no network.
   */
  def turns(ledger: Ledger, salt: String, session: Request => String = sessionOf())
           (next: Chat.TurnOverride = (_, _) => None): Chat.TurnOverride =
    (r, messages) =>
      lastUser(messages).foreach: text =>
        try Leads.watch(ledger, salt)(session(r), text)
        catch case scala.util.control.NonFatal(_) => ()
      next(r, messages)
