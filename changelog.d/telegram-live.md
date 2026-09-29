## telegram-live - a card that keeps up with an agent, and a command menu from a table

- `Chats.performThrottled(bot, chat, everyMs)` — edits to one message
  coalesced: the first at once, then the LAST held one when the window
  closes (which opens the next); a quiet card is instant, an edit equal
  to the last sent is dropped, sends and answers are never held, and
  two messages do not hold each other. `Chats(…, everyMs)` picks it by
  one number (0 = the plain performer, the default; `Timer` joins the
  class's using clause).
- `Command(name, description, screen)`: `install` is `setMyCommands`
  with a bad name refused here by name and no call made; `dispatch`
  reads `/name`, `/name@bot`, `/name args` to the screen.
- `TestLive` drives the throttle with a manual `Timer` — no test sleeps
  on the wall clock. Two rows in the stack-safety inventory (the
  `arm`/`close` cycle is a timer re-arm). Spec: specs/telegram-live.md,
  all behavior items checked. Consumer: `../nadia` `app/`.
