## telegram-fatal-backoff - a refusal waiting cannot fix is not asked again every two seconds

Found by RUNNING okay-watch's assembled server against the real Bot API
with a bad token, 2026-09-29 — the first run of the bot outside a
recording transport. The loop reported the 401 correctly and said
«waiting will not fix this», then waited two seconds and asked again,
forever: thirty log lines a minute and an invalid token hammered at
Telegram, which rate-limits and can block an address that does it.

- `serve` backs a FATAL refusal off: 2s, 4s, 8s, … to `fatalCapMs` (five
  minutes by default), and the first good poll resets it. It still never
  dies of one, so a token the operator fixes is picked up without a
  restart. A non-fatal refusal waits as before: the API's `retry_after`,
  or `retryMs`.
- One test with the counting timer: 2, 4, 8, then the cap, and the reset
  after a good round.
