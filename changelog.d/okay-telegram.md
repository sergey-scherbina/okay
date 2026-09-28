## okay-telegram - the Bot API as a module of okay

The operator, 2026-09-28: «переиспользуемые технологии общего
назначения — добавляй их в окей». The Telegram client was written three
times (okay-chat, okay-watch's registry, okay-watch's check bot to be),
and specs/ui-telegram.md had recorded «okay has no Telegram client» —
reversed, with the reasoning kept: the acts are few and plain, so the
client is one small module.

- specs/telegram-bot.md (committed first). `okay.telegram.Bot` over
  okay-http: `call` answers `Either[Refused, Json]` — the API's refusal
  as a value, the method named and the token never; `getUpdates`,
  `poll` (the next offset after the highest update seen) and `serve`
  (the loop, a refused round re-asked with the same offset, stopped when
  told); `send`/`edit` with okay-ui's `Telegram.Key` keyboard, HTML and
  ForceReply; `answerCallback`, `setCommands`; Telegram Stars —
  `invoice` (XTR, no provider), `answerPreCheckout`, `refundStars`.
- `Update` — `Message`, `Callback`, `PreCheckout`, `Paid`, `Other(kind)`
  — a total parse: what this module does not model is named, not
  dropped; a message without text is `Other("message")`.
- `Chats` — the consumer okay-ui's chat host assumed: `perform` turns a
  `Send`/`Edit`/`Answer`/`Ask` into the Bot API call (a refusal is told
  and answers None, the host is not stopped), `heard` routes a press or
  a message to its chat, and the class opens one application per chat on
  its first message.
- Tests against a recording `Http`, no network: `TestUpdate` (shared),
  `TestBot`, `TestChats` with the gate — a counter application pressed
  through `Chats` sends one message and edits it — and `TestReadme`, the
  README's examples compiled.
