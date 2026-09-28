# okay-telegram — the Bot API as values

The Telegram Bot API over [`okay-http`](../okay-http): a call whose
refusal is a value, updates as one total enum, long polling as one loop,
Telegram Stars invoices — and `Chats`, which puts [`okay-ui`](../okay-ui)'s
chat host behind a real bot, one host per chat, so an application drawn
for a browser runs in Telegram with nothing but a token.

## The pieces

| | |
|---|---|
| `Bot` | `call(method, params)` — a POST of JSON to `/bot<token>/<method>`; `Right(result)` or `Left(Refused(method, code, words))`, never a throw, never the token |
| `Bot.getUpdates` / `poll` / `serve` | long polling: one round answers the next offset; `serve` keeps it, waits and re-asks on a refusal, stops when told |
| `Bot.send` / `edit` / `answerCallback` / `setCommands` | a message with an inline keyboard (`okay.ui.Telegram.Key`), HTML, ForceReply; an edit in place; every press answered |
| `Bot.invoice` / `answerPreCheckout` / `refundStars` | Telegram Stars (`XTR`, no provider token) — digital goods only, which is Telegram's rule |
| `Update` | `Message`, `Callback`, `PreCheckout`, `Paid`, and `Other(kind)` for everything else — named, never dropped |
| `Chats` | the performer of okay-ui's `Telegram.Act`s and one host per chat: the first message from a chat opens the application |

## An okay-ui application, in a chat

```scala
import okay.*
import okay.given
import okay.http.Transports
import okay.telegram.{Bot, Chats}
import okay.ui.{Event, Ui}

val bot = Bot(Transports.http(), sys.env("BOT_TOKEN"))

def view(n: Int): Ui = Ui.Column(Vector(Ui.Text(s"count: $n"), Ui.Button("+", "inc")))
def update(n: Int, e: Event): Int = e match
  case Event.Pressed("inc") => n + 1
  case _ => n

// one application per chat — the same `view` and `update` a browser is served
val chats = Chats(bot, (_, host) => Ui.run(0)(view)(update)(host).map(_ => ()))

// the loop: every update to its chat's host; payments and the rest are yours
Async.run[Long, Pure](bot.serve(chats.hear)).runWith
```

The message the chat sees is `count: 0` with a `+` button under it. A
press is answered and the SAME message is edited to `count: 1` — one
screen, however many times it changes (specs/ui-telegram.md).

## A plain bot

```scala
import okay.telegram.{Bot, Update}

def answer(bot: Bot)(u: Update): Unit ! Async = u match
  case Update.Message(_, chat, _, _, text) => bot.send(chat, s"you said: $text").map(_ => ())
  case Update.PreCheckout(_, _, id, _, _, _) => bot.answerPreCheckout(id, ok = true).map(_ => ())
  case _ => pure(())
```

Everything is tested against a recording `Http` — no network, no real
token — including the gate: a counter pressed through `Chats` sends one
message and then edits it.

## Further

| | |
|---|---|
| [`specs/telegram-bot.md`](../specs/telegram-bot.md) | the design, its behaviour boxes, the decision it reverses |
| [`specs/ui-telegram.md`](../specs/ui-telegram.md) | the chat as a host of okay-ui, which this module performs |
| [`docs/modules/okay-telegram.md`](../docs/modules/okay-telegram.md) | what it is, and the reasoning |
