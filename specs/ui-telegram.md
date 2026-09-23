# ui-telegram — a chat as one more host

## Overview

okay-ui's claim is that one application is drawn by any host without
the program knowing which (specs/ui.md): a terminal repaints, React
reconciles, Swing and GTK build widgets, and a browser or a native app
is a CLIENT of the wire (`Wire.serve` on the server, `Wire.client(host)`
beside the screen). A Telegram chat is one more screen, with the
narrowest vocabulary of all: one message of text, a keyboard of buttons
under it that the bot can EDIT in place, and the person's next message.

This module is that host. It is a `Host` like every other — `render(ui)`
and `events` — so the same application runs in a chat with `Ui.run`, or
behind `Wire.serve` with `Wire.client(Telegram.host(…))`, byte for byte
the program a browser is served. The operator's words, which are the
requirement: «уи для телеграма был тоже как и остальные уи … еще один
бекенд … и для веб версии тоже. Чтобы это была одна и та же логика
буквально».

It carries NO HTTP. What a chat does — send a message, edit it, answer
a press, ask for a reply — leaves this module as DATA (`Telegram.Act`),
and the consumer's Bot API client performs it; what the chat says comes
in as data (`Telegram.Update`). So the whole host is tested with no
network, the way the React mapping is tested with no browser.

## Interface

```scala
object Telegram:
  /** one message: its text (Telegram's HTML subset) and the inline
   * keyboard under it, row by row */
  final case class Message(text: String, keyboard: Vector[Vector[Key]])
  enum Key:
    case Press(label: String, data: String)   // a callback button
    case Open(label: String, url: String)     // a URL button (a Link)

  /** what the chat said */
  enum Update:
    case Pressed(data: String, callbackId: String)
    case Said(text: String)

  /** what the host asks the chat to do — performed by the consumer */
  enum Act:
    case Send(m: Message)
    case Edit(messageId: Long, m: Message)
    case Answer(callbackId: String, notice: String = "")
    case Ask(prompt: String)                   // a message with ForceReply
  
  /** the vocabulary a chat draws natively; everything else is lowered */
  val vocab: Set[String] = Set(Ui.Vocab.link)

  /** the PURE mapping: a (lowered) tree as one message, and what each
   * button's data means — asserted on the JVM like the React mapping */
  def render(ui: Ui, frame: Int): (Message, Map[String, Meaning])

  /** the pure state machine the host is: a tree to show, an update from
   * the chat → the events for the application and the acts for the chat */
  final case class Session(...)
  object Session:
    def show(s: Session, ui: Ui): (Session, Vector[Act])
    def hear(s: Session, u: Update): (Session, Vector[Event], Vector[Act])
    def sent(s: Session, messageId: Long): Session   // the id a Send got

  /** the Host: acts go to `perform` (the consumer's Bot API call, which
   * answers a Send's message id), updates are fed by the consumer */
  def host(perform: Act => Option[Long] ! Async): (Host, Update => Unit ! Async)
```

## Behavior

- [x] **the mapping**: `Text` a line of the message (bold / emphasis
      `<b>`, muted `<i>`, text HTML-escaped); `Button` a callback
      button; buttons of one `Row` (or horizontal `Box`) share a keyboard
      row, a vertical container gives each its own; `Check` a toggle
      button «☑ label» / «☐ label»; `Select` a button per option, the
      chosen one marked «● »; `Input` a line «label: value» (a secret
      shown as dots, an empty one as «—») and an edit button «✎ label»;
      `Form` its fields and its submit button; `Link` a URL button
      (vocabulary `link`); `Image` its alt as a line; `Scroll` its child.
      Every other node is LOWERED first (`Ui.lower`, vocabulary `link`)
      — a Table, Tabs, a Modal, a Disclosure mean in a chat what they
      mean on a terminal.
- [x] **callback data is small and bound to the frame**: `f<frame>.<n>`,
      never the key itself — a key can be longer than Telegram's 64
      bytes, and a press on an OLDER frame's button is refused with a
      notice («устарело» / «outdated»), not read against the new tree.
- [x] **a press is an event**: a Button → `Pressed(key)`; a Check →
      `Toggled(key, !on)`; a Select option → `Chosen(key, i)`; every
      press is ANSWERED (`Answer`), or the person's client spins.
- [x] **typing is an edit**: the edit button focuses its Input and
      `Ask`s for the value (ForceReply, the prompt is the label); the
      next `Said` is `Edited(key, text)`. A Form's submit button is
      `Pressed(form)`, as on every host. The HYBRID is not this host's:
      behind the wire, `Wire.client` keeps a Form's edits locally and
      turns its button into `Submitted(form, edits)` — for a chat exactly
      as for a browser, so a server cannot tell them apart. A `Said`
      with nothing focused is not an event.
- [x] **one message, edited in place**: the first frame is `Send`, and
      every later frame `Edit`s that message (its id from `sent`); a
      frame equal to the one shown sends nothing.
- [x] **the limits are the chat's, met honestly**: text past 4096
      characters is cut with «…»; more than 100 buttons keep the first
      100 and say so in the text; an empty frame shows «·» (Telegram
      refuses an empty message).
- [x] **the seam's claim, the gate**: one application answers the SAME
      final state (a) run by `Ui.run` on the scripted test host and (b)
      served by `Wire.serve`, drawn by `Wire.client(Telegram.host(…))`,
      driven by scripted `Update`s — presses, a Check, a Select, an
      edit inside a Form and its submit.

## Design

The host is a pure `Session` and a thin shell. `Session.show` is what
`render` does with a tree: lower it, map it, number its buttons for this
frame, and answer `Send` or `Edit`. `Session.hear` is what a press or a
message means against the frame SHOWN — which is the capability rule
`Wire.permitted` states for the server, applied on the client too: a
datum not on the current frame is not an event.

A Form's local edits are NOT the Session's: `Wire.client` already keeps
them for every host it draws (`foldLocal`/`submit`, specs/frontend.md
«the hybrid wire») and re-renders the host with the typed value. A chat
behind the wire therefore behaves as a browser does, by construction
rather than by a second copy of the rule.

Why a callback button and not a reply keyboard: a reply keyboard SENDS
TEXT into the chat, one message per press, and cannot be edited; a
callback button is answered and the message it hangs on is edited, so a
screen stays one message however many times it changes.

## Decisions

- The Bot API is the consumer's. okay has no Telegram client and this
  lane does not add one: the consumer already has a transport, retries
  and a token, and the acts are few and plain.
- Text above, keyboard below — a chat cannot interleave them. A tree
  that puts a button between two paragraphs draws both paragraphs, then
  the button. Recorded, not hidden.
- `Key`, `Resized`, `Closed`: a chat has none of them to send. A
  consumer that wants a «close» gives the app a button.

## Out of scope

- Telegram Mini Apps (a web page inside the chat): that is a BROWSER,
  and the browser client already exists — serving it inside Telegram is
  a deployment of `Wire`, not a host.
- Photos, files, locations as inputs.

## Results

- `okay-ui/src/main/scala/okay/ui/Telegram.scala`: the mapping
  (`render`), the `Session` (`show`/`hear`/`sent`) and the `host` over a
  consumer's `perform`. Shared source — it compiles on JVM, JS and
  Native.
- `TestTelegram`, 7 (JVM): the mapping as a value, the lowering, the
  frame-bound callback data and the stale press, press/edit/answer, the
  edited-in-place message and the chat's limits, and THE SEAM — one app
  (counter, Select, a Form with an Input and a Check, a quit button)
  reaches `S(n=1, name="ada", even=false, pick=2, saved=true)` both on
  the scripted test host through `Ui.run` and through `Wire.serveClosing`
  + `Wire.client(Telegram.host(…))` driven by scripted chat updates, the
  Form's edits folded by `Wire.client` and crossing as one `Submitted`.
