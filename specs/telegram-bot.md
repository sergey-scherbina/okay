# telegram-bot — the Bot API as a module of okay

## Overview

okay-ui already draws an application in a Telegram chat
(specs/ui-telegram.md): a `Host` whose acts leave as data and whose
updates arrive as data. That spec decided the Bot API client was the
consumer's — «the consumer already has a transport, retries and a
token». Three consumers later (okay-chat's `Telegram.over`, okay-watch's
registry notifier, okay-watch's check bot), the client is the same
thirty lines each time, and the operator, 2026-09-28, set the rule the
other way: general-purpose platform code belongs in okay, shown rather
than hidden. This module is that client.

What it is: the Bot API's HTTPS calls over okay-http, the API's answers
and updates as VALUES, and the one bridge okay-ui's host needs — a
performer of its `Act`s and a router of updates to one host per chat.
What it is not: a framework. There is no command dispatcher, no
conversation state, no middleware; an application is an okay-ui program
(`Ui.run`) or a function of `Update`, and this module carries messages
to and from it.

## Interface

```scala
package okay.telegram

/** what the chat said — total: an update this module does not model is
 * `Other(kind)`, named, never dropped */
enum Update:
  case Message(updateId: Long, chat: Long, from: Long, messageId: Long, text: String)
  case Callback(updateId: Long, chat: Long, from: Long, messageId: Long, data: String, callbackId: String)
  case PreCheckout(updateId: Long, from: Long, queryId: String, payload: String, currency: String, total: Long)
  case Paid(updateId: Long, chat: Long, from: Long, payload: String, currency: String, total: Long,
            chargeId: String, providerChargeId: String)
  case Other(updateId: Long, kind: String)
object Update:
  def parse(u: Json): Update

/** one call refused, as the API said it: the method, its error code, its words */
final case class Refused(method: String, code: Int, description: String)

final class Bot(http: Http, token: String, base: String = "https://api.telegram.org"):
  def call(method: String, params: Json = JObj(Vector.empty)): Either[Refused, Json] ! Async
  def getMe: Either[Refused, Json] ! Async
  def getUpdates(offset: Long, timeoutSeconds: Int = 25): Either[Refused, Vector[Update]] ! Async
  /** one round: every update to `handle`, in order; answers the next offset */
  def poll(offset: Long, handle: Update => Unit ! Async, timeoutSeconds: Int = 25): Long ! Async
  /** the loop, until `stop` says so; a refused poll waits `retryMs` and asks again */
  def serve(handle: Update => Unit ! Async, from: Long = 0, retryMs: Long = 2000,
            stop: () => Boolean = () => false): Long ! Async
  def send(chat: Long, text: String, keyboard: Vector[Vector[Key]] = Vector.empty,
           html: Boolean = true, forceReply: Boolean = false): Either[Refused, Long] ! Async
  def edit(chat: Long, messageId: Long, text: String, keyboard: Vector[Vector[Key]] = Vector.empty,
           html: Boolean = true): Either[Refused, Unit] ! Async
  def answerCallback(callbackId: String, notice: String = ""): Either[Refused, Unit] ! Async
  def setCommands(commands: Vector[(String, String)]): Either[Refused, Unit] ! Async
  // Telegram Stars — digital goods only, which is Telegram's rule, not ours
  def invoice(chat: Long, title: String, description: String, payload: String,
              stars: Long, label: String): Either[Refused, Long] ! Async
  def answerPreCheckout(queryId: String, ok: Boolean, error: String = ""): Either[Refused, Unit] ! Async
  def refundStars(user: Long, chargeId: String): Either[Refused, Unit] ! Async

/** okay-ui's host, per chat, over a Bot */
object Chats:
  /** the performer of one chat's acts: a Send answers its message id */
  def perform(bot: Bot, chat: Long, refused: Refused => Unit ! Async = _ => pure(())): Telegram.Act => Option[Long] ! Async
  /** an update as what a chat's host hears, and which chat */
  def heard(u: Update): Option[(Long, Telegram.Update)]
final class Chats(bot: Bot, open: (Long, Host) => Unit ! Async, refused: Refused => Unit ! Async = _ => pure(())):
  /** a press or a message: the chat's host hears it; the first from a chat opens its application */
  def hear(u: Update): Unit ! Async
```

`Key` is okay-ui's `Telegram.Key` (`Press(label, data)` a callback
button, `Open(label, url)` a URL button): one keyboard vocabulary, not
two.

## Behavior

- [x] **a call is a POST of JSON** to `<base>/bot<token>/<method>`, its
      answer read whole: `{"ok":true,"result":…}` is `Right(result)`;
      `{"ok":false,"error_code":n,"description":s}` is
      `Left(Refused(method, n, s))`; a transport failure or a body that
      is not the API's shape is `Left(Refused(method, status, body))`.
      No exception crosses the seam: the API's refusal is data, as
      okay-http's 4xx is.
- [x] **the token never appears in a `Refused`**: the method is named,
      the URL is not.
- [x] **`Update.parse` is total**: a text message, a callback press, a
      pre-checkout query and a successful payment become their case;
      an edited message, a join, a photo, a channel post become
      `Other(kind)` with the update's own key as the kind — so a consumer
      can count what it ignores. A message whose `text` is absent is
      `Other("message")`, not a `Message("")`.
- [x] **a refused poll is REPORTED and retried, in that order** (`serve`'s
      `onRefused`): the loop never dies of one, and never swallows one. The
      two failures a live bot meets are a 409 — Telegram refusing a second
      `getUpdates` on one token, which is what a redeploy that left the old
      process running looks like — and a 401, a token wrong or revoked; both
      look exactly like «the bot does not answer» from outside. `Refused.fatal`
      says which of them waiting cannot fix.
- [x] **a FATAL refusal backs off** (telegram-fatal-backoff): a 401 or 404
      doubles the wait each time up to `fatalCapMs` (five minutes), and the
      first good poll resets it. The loop still does not die, so a token the
      operator fixes is picked up without a restart. Found by running the
      real okay-watch server against the real Bot API with a bad token: it
      said «waiting will not fix this» and then asked again every two
      seconds, forever.
- [x] **how long it waits is the API's answer where the API gave one**: a 429
      carries `parameters.retry_after`, read into `Refused.retryAfter`, and
      `retryMs` is only the fallback — answering a rate limit at our own
      interval is hammering.
- [x] **`poll` answers the offset after the highest `update_id` it saw**,
      and the offset it was given when the round was empty — the
      Bot API's contract for acknowledging updates. `serve` starts from
      `from`, keeps that offset, and on a `Refused` waits `retryMs` and
      asks again with the SAME offset; it ends when `stop` says so,
      answering the offset to resume from.
- [x] **`send`/`edit` carry the keyboard as `reply_markup.inline_keyboard`**
      row by row, `parse_mode: "HTML"` when `html`, and `force_reply`
      when asked (the host's `Ask`); `send` answers the new message's id.
- [x] **Stars**: `invoice` sends `sendInvoice` with `currency: "XTR"`,
      an OMITTED `provider_token` (the Bot API changelog: it must be omitted
      for payments in Telegram Stars) and one price line in Stars;
      `answerPreCheckout` answers `answerPreCheckoutQuery` (`ok`, and
      `error_message` when refusing); `refundStars` calls
      `refundStarPayment` with `user_id` and
      `telegram_payment_charge_id`. This module knows the shape, never
      the rule of what may be sold — Telegram's terms say digital goods
      only, and the consumer's spec says so where it applies.
- [x] **`Chats.perform` is the host's other half**: `Send(m)` →
      `send(chat, m.text, m.keyboard)` and its id; `Edit(id, m)` → `edit`;
      `Answer(cb, notice)` → `answerCallback`; `Ask(prompt)` → `send`
      with `force_reply`. A `Refused` reaches `refused` and the act
      answers `None`; the host is not stopped by one failed call.
- [x] **`Chats.heard`**: a `Callback` is `(chat, Pressed(data, callbackId))`,
      a `Message` is `(chat, Said(text))`, everything else is `None` — a
      payment or a pre-checkout is the consumer's, before or beside the
      host.
- [x] **`Chats.awaiting(chat)`** is true between the screen's `Ask` (an
      `Input` focused) and the next message or press of that chat: the
      one fact a consumer that understands text ITSELF needs before it
      decides whose the next message is. Without it the consumer either
      takes the value the screen asked for or hands the screen a sentence
      it has no focus for, which is no event, no act and no reply.
- [x] **`Chats.open(chat)`** opens a chat's application without hearing a
      word: for a consumer that understood the message itself (a
      sentence with an address in it is not the field's value) and
      wants the screen drawn; opening an open chat does nothing.
- [x] **one host per chat**: the first update from a chat builds
      `Telegram.host(perform(bot, chat))`, spawns `open(chat, host)` (the
      consumer's `Ui.run`), and hands the host every later update of
      that chat. The gate is the seam's claim: a counter application
      pressed through `Chats` over a recording `Http` sends one message
      and then EDITS it, with the count in the text.

## Design

`Bot.call` is the whole transport: `Request.post` with a JSON body,
`Http.bytes` for the answer, `Json.parse`. Everything else builds a
`JObj` and reads two or three fields of the result, so the typed
methods are thin and the untyped `call` stays for whatever this module
did not name — the Bot API grows monthly and a module that lists every
method is stale on release.

`Update.parse` reads by the update's second key (`message`,
`callback_query`, `pre_checkout_query`, …), which is how the API
distinguishes them; `successful_payment` is a field of a message and is
checked before the text.

`Chats` owns a `Map[Long, Update => Unit ! Async]` under a lock, like
the host owns its session — the same two-line discipline as
`Telegram.host`. It does not run the application: `open` is the
consumer's `Ui.run(…)(host)` with its own `Scheduler` and `CanBlock`,
so this module stays free of both.

## Decisions

- **Reverses specs/ui-telegram.md «okay has no Telegram client»**
  (operator, 2026-09-28). That spec's own reasoning stands — the acts
  are few and plain — which is exactly why the client is small enough
  to be one module rather than three copies.
- `Update` is an enum of the cases the two okay consumers act on, plus
  `Other`. Photos, locations, inline queries, channel posts: `Other`,
  and the consumer that needs one adds the case here, in one place.
- Long polling, not webhooks: an outbound connection, so a bot runs from
  a laptop and behind any firewall; a webhook is a route on okay-http a
  consumer already knows how to write, and needs a public TLS address
  this module cannot assume.
- No retry inside `call`: okay-resilience is where retries live, and a
  poll's retry is the loop's, stated in `serve`.

## Out of scope

- Webhooks (a consumer's route), file uploads (multipart), Mini Apps
  (a browser), inline mode, the local Bot API server.

## Results

2026-09-28, the lane. `okayTelegramJVM/testOnly okay.telegram.*`: 12 tests,
all green, none on the network —

- `TestUpdate` (shared, pure): the four cases and `Other` for an edit, a
  join, a photo; damaged input is a value.
- `TestBot`: the POST's URL and header; `Right(result)`, the API's
  refusal as `Refused(method, 403, words)` without the token, a gateway's
  HTML as `Refused(method, 500, …)`; `poll` answers 12 after updates 10
  and 11 and the given offset on an empty round; `serve` re-asks a 429'd
  round with the SAME offset (offsets asked: 0, 21, 21) and stops when
  told; the keyboard, `parse_mode`, `force_reply` and the message id;
  the Stars road (`XTR`, empty `provider_token`, one price line, the
  pre-checkout refusal's words, the refund's charge id).
- `TestChats`: `perform` for all four acts, a refused act told and
  answering None; `heard`; THE GATE — a counter application opened by a
  chat's first message sends `count: 0` with a `+` button, and the press
  (its callback data read off the recorded keyboard) is answered and
  EDITS message 1 to `count: 1`; a second chat opens its own application
  at `count: 0`.
- `TestReadme`: both README examples compiled and the plain one run.

Checked against the Bot API's own documentation, 2026-09-29
(telegram-serve-says), before the first consumer went live: every
parameter name this module sends matches the current documentation, and
three things did not — `serve` swallowed its refusals, `retry_after` was
ignored, and `provider_token` was sent empty where the changelog says it
must be omitted. All three fixed, each with a test.

`Chats.awaiting` (2026-09-28, telegram-awaiting): the flag is kept where
the fact already passes — `perform` sees the `Ask` — so no okay-ui change
was needed and the pure host stayed pure.

The JS leg: `okayTelegramJS/Test/compile` green — the same sources and
the shared `TestUpdate`; the effectful suites are JVM-only by placement.
