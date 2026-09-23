## ui-telegram - a chat as one more host of okay-ui

The operator, from okay-chat 2026-09-23: the Telegram UI should be «как
и остальные уи … еще один бекенд … и для веб версии тоже. Чтобы это
была одна и та же логика буквально».

- specs/ui-telegram.md (committed first). okay-ui `Telegram`: a `Host`
  like the terminal's or Swing's, so an application runs in a chat with
  `Ui.run`, or behind `Wire.serve` with `Wire.client(Telegram.host(…))`
  — the program a browser is served. One message, edited in place; text
  above, an inline keyboard below; a Check a toggle, a Select a button
  per option, an Input a line and an edit button that asks for the next
  message, a Link a URL button, everything else lowered (vocabulary
  `link`). Callback data is `f<frame>.<n>`, so a press on an older frame
  is answered «outdated» and not read against the new tree.
- No HTTP: the chat's acts leave as data (`Send`/`Edit`/`Answer`/`Ask`),
  performed by the consumer's Bot API client; its updates come in as
  data. The Form hybrid is `Wire.client`'s, as for every host.
- `TestTelegram` 7, including the seam's gate: the same app, the same
  final state on the test host and in a scripted chat behind the wire.
