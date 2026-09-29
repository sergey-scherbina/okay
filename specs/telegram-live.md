# telegram-live — a card that keeps up with an agent, and a command menu that cannot drift

## Overview

okay-telegram carries an okay-ui program into a chat: `Chats.perform`
turns each `Telegram.Act` into a Bot API call, one edit per `Act.Edit`.
That is right for a person pressing buttons and wrong for a screen that
watches a running agent: a 4B model makes a tool call a second, the card
wants to show each one, and the Bot API refuses edits past roughly one
per second per message and a few dozen per minute per chat. The first
consumer to hit it is `../nadia`'s Agent card (nadia `docs/specs/app.md`:
"at most one edit per two seconds per message"). Two small additions,
both with no nadia in them:

1. **A throttle on edits**, per message, last write wins — the chat sees
   the newest state at most every N ms, and never a stale one.
2. **The command menu from a table** — `setCommands` derived from the
   same value the screens are drawn from, so `/agents` in the client's
   menu and the Agents screen cannot disagree.

## Interface

```scala
package okay.telegram

object Chats:
  /** `perform`, with edits to one message coalesced: the first edit goes
   * out at once; further edits within `everyMs` are held and the LAST
   * one is sent when the window closes. Sends and answers are not held —
   * a new message is not an edit and a callback must be answered now */
  def performThrottled(bot: Bot, chat: Long, everyMs: Long = 2000,
                       refused: Refused => Unit ! Async = _ => pure(()))
                      (using Scheduler): Act => Option[Long] ! Async

/** a command the client lists in its menu: the same table a program's
 * screens are keyed by, so there is one place a command is named */
final case class Command(name: String, description: String, screen: String)
object Command:
  /** `setMyCommands` from the table; a name that is not `[a-z0-9_]{1,32}`
   * is refused HERE, by name, before the API refuses it by index */
  def install(bot: Bot, commands: Vector[Command]): Either[Refused, Unit] ! Async
  /** `/name` in a message → the screen it opens, `None` for text */
  def dispatch(commands: Vector[Command], text: String): Option[String]
```

## Behavior

- [x] three `Edit`s of one message within 100 ms produce ONE `editMessageText` call, with the
      third's text and keyboard (scripted `Bot`: the calls are recorded, the clock is a test
      timer)
- [x] an `Edit` after the window closes goes out at once — the throttle is per burst, not a
      fixed cadence, so a quiet card is still instant
- [x] edits to two different messages do not hold each other
- [x] `Send` and `Answer` are never held; an `Answer` arriving during a held edit is sent
      before the edit
- [x] an `Edit` whose text and keyboard equal the last sent is dropped (the API answers
      "message is not modified" with an error otherwise)
- [x] a held edit that is refused by the API reaches `refused` once, with the method name
- [x] `Command.install` with a name `Agents` (capital) is `Left(Refused("setMyCommands", 400,
      …))` naming the command, without a network call
- [x] `Command.dispatch(cmds, "/agents")` and `"/agents@botname"` both give `agents`;
      `"/agents now"` gives `agents`; `"agents"` gives `None`

## Out of scope

- Rate limiting of sends (a chat's per-minute limit). Sends are the program's; a
  program that sends in a loop is wrong, not throttled.
- Retries on refused calls — okay-resilience, wrapped by the consumer.
- Webhooks, files, media, groups, topics: unchanged, still the consumer's or absent.
- Streaming tokens into a message. A token stream through the throttle is a stream
  of edits and works; whether a card should show tokens is the consumer's choice.

## Design

- **The throttle is a `Scheduler` timer and a `Map[Long, Pending]` behind one lock**,
  the same shape as `Chats`'s doors. No fiber per message: a held edit is a value
  replaced in place, and one timer per message-in-window fires the send.
- **`Command` carries the screen name** so the consumer's dispatch is a lookup, not a
  second table. `Chats` does not dispatch — a program is a function of `Update`, as
  specs/telegram-bot.md says — but it can hand the program the screen the command
  named.

## Decisions

- **Last write wins, not a queue** — chosen because a card is a state, not a log; two
  intermediate states nobody saw cost two API calls and show nothing. Rejected: a
  bounded queue of edits — it delivers stale frames late.
- **First edit immediate** — chosen so a single change is as fast as today; only a
  burst pays the window. Rejected: fixed cadence — a two-second delay on every press
  reads as lag.

## Implementation lane

`telegram-live` — okay-telegram, additive (`Chats.performThrottled`, `Command`,
`TestThrottle`, `TestCommand`). Consumer: `../nadia` `app/`.

## Results

Implemented 2026-09-29 (lane `telegram-live`): `Chats.performThrottled`, a
window length on `Chats` (`everyMs`, 0 = the plain performer), `Command`.
`TestLive` drives the throttle with a manual `Timer` — a window closes when
the test says so, so no test sleeps on the wall clock. Two rows in
`specs/stack-safety-okay.tsv`: the `arm`/`close` cycle is a timer re-arm,
each hop a fresh frame.
