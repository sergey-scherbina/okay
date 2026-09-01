package okay.chatweb

import okay.ui.{Event, Style, Ui}

/**
 * The chat's brain, PURE and CROSS (specs/demo-chat.md, the React
 * frontend): the state, the view as a Ui tree, the update as a fold
 * of events — all of it tested on the JVM with scripted events,
 * exactly the way every okay-ui application is. The browser adds
 * only glue: okay-ui's ReactJs renders this tree through a real
 * React, and the fetch reader feeds tokens back as ordinary events.
 *
 * The streaming protocol is three private event keys: `$token`
 * appends to the open bot bubble, `$done` closes the turn, `$cut`
 * marks the guard's scissors — the same fold serves the scripted
 * model, the local one and the wire.
 */
object ChatUi {

  final case class Msg(role: String, text: String, cut: Option[String] = None)

  final case class State(messages: Vector[Msg] = Vector.empty,
                         draft: String = "",
                         busy: Boolean = false)

  /** what the loop's effect slot should DO after this event; the
   * glue interprets it (fetch the reply, or nothing) */
  enum Go:
    case Send(history: Vector[Msg])
    case Stay

  def view(s: State): Ui =
    Ui.Column(Vector(
      Ui.Text("okay chat — okay-ui on React; /match wires okay-match (умею… / нужен…)",
        Style(bold = true)),
      Ui.Column(s.messages.zipWithIndex.map { (m, i) =>
        val bubble = Ui.Text(m.role match
          case "user" => "you: " + m.text
          case "match" => "🔔 " + m.text
          case _ => "bot: " + m.text)
        m.cut match
          case Some(rule) => Ui.Column(Vector(bubble,
            Ui.Text(s"✂ generation cut: $rule", Style(dim = true))), key = s"m$i")
          case None => Ui.Row(Vector(bubble), key = s"m$i")
      }, key = "log"),
      Ui.Row(Vector(
        Ui.Input(s.draft, key = "draft", label = ""),
        Ui.Button(if s.busy then "…" else "send", key = "send")), key = "bar")))

  def update(s: State, e: Event): (State, Go) = e match
    case Event.Edited("draft", v) => (s.copy(draft = v), Go.Stay)
    case Event.Pressed("send") if !s.busy && s.draft.trim.nonEmpty =>
      val history = s.messages :+ Msg("user", s.draft.trim)
      (State(history :+ Msg("assistant", ""), "", busy = true), Go.Send(history))
    case Event.Edited("$token", t) if s.busy =>
      val last = s.messages.last
      (s.copy(messages = s.messages.init :+ last.copy(text = last.text + t)), Go.Stay)
    case Event.Pressed("$done") => (s.copy(busy = false), Go.Stay)
    case Event.Edited("$match", note) =>
      (s.copy(messages = s.messages :+ Msg("match", note)), Go.Stay)
    case Event.Edited("$cut", rule) =>
      val last = s.messages.last
      (s.copy(busy = false,
        messages = s.messages.init :+ last.copy(cut = Some(rule))), Go.Stay)
    case _ => (s, Go.Stay)
}
