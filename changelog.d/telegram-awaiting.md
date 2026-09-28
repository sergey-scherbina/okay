## telegram-awaiting - whose message is this: the screen's question, or the consumer's own reading

okay-watch's check bot reads a sentence with okay-dlm before it hands
anything to the screen, and found the gap: a press is opaque callback
data, so a consumer cannot tell that the screen just asked for a typed
value. It then either steals that value or hands the screen a sentence
it has no focus for — and `Session.hear` of an unfocused `Said` is no
event and no act, so the person gets silence.

- `Chats.awaiting(chat)`: true between the screen's `Ask` and that
  chat's next message or press. `Chats.perform` takes an `asked`
  callback, because the fact already passes through the act it performs
  — the pure host and `Telegram.Session` are untouched.
- One test: the pencil pressed, `awaiting` true, the typed value
  delivered, `awaiting` false again.
