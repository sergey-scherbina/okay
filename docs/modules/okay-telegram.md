# okay-telegram

The Telegram Bot API over okay-http, as values: a call answers
`Either[Refused, Json]`, an update is one total enum, long polling is
one loop, and `Chats` is the consumer okay-ui's chat host
(specs/ui-telegram.md) always assumed — the performer of its acts, one
host per chat.

## Why a module

specs/ui-telegram.md drew the line at the Bot API: «the consumer already
has a transport, retries and a token». Three consumers then wrote the
same thirty lines — okay-chat's `Telegram.over`, okay-watch's registry
notifier, okay-watch's check bot — and the operator set the rule the
other way on 2026-09-28: general-purpose platform code belongs in okay,
shown rather than hidden. The acts are still few and plain; that is why
the client is one small module and not three copies.

## The pieces

| | |
|---|---|
| `Bot` | `call`, `getMe`, `getUpdates`/`poll`/`serve`, `send`/`edit`/`answerCallback`/`setCommands`, `invoice`/`answerPreCheckout`/`refundStars` |
| `Refused` | the method, the API's error code and its words — never the URL, which carries the token |
| `Update` | `Message`, `Callback`, `PreCheckout`, `Paid`, `Other(kind)` |
| `Chats` | `perform(bot, chat)` for okay-ui's `Telegram.Act`, `heard(update)` to the chat's `Telegram.Update`, and the class that keeps one host per chat |

## What it is not

Not a framework: no command dispatcher, no conversation state, no
middleware. An application is `Ui.run` or a function of `Update`; this
module carries messages. Webhooks are an okay-http route the consumer
writes; file uploads, inline mode and Mini Apps are out of scope
(specs/telegram-bot.md).

Cross-built JVM + JS. The effectful suites are JVM-only (they run
programs); `Update.parse` is pure and tested on both.
