- [ ] okay-telegram — the Telegram Bot API as a module of okay
      (operator, 2026-09-28: «переиспользуемые технологии общего
      назначения — добавляй их в окей»). Today the client is written
      three times — okay-chat's `Telegram.over`, okay-watch's registry
      `Notify`, and the one okay-watch's check bot would be — and
      specs/ui-telegram.md recorded «okay has no Telegram client», a
      decision the operator now reverses. The module: `Bot` over
      okay-http (`call`, `getUpdates` long polling, `send`/`edit`/
      `answerCallback`, `setCommands`, the Stars road `invoice`/
      `answerPreCheckout`/`refundStars`), `Update` as a total parse of
      the API's JSON (message, callback, pre-checkout, payment, other —
      named, never dropped), and `Chats`: the performer of okay-ui's
      `Telegram.Act`s and one host per chat, so an okay-ui application
      runs in a chat with nothing but a token. Tests with a recording
      `Http`, no network; the seam's gate: a counter app pressed through
      `Chats` edits its one message. specs/telegram-bot.md first.
