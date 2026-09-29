- [ ] telegram-fatal-backoff — found by RUNNING okay-watch's server against
      the real Bot API with a bad token (2026-09-29): `serve` reports a 401
      correctly and says «waiting will not fix this» — and then waits two
      seconds and asks again, forever. Thirty lines a minute in the log,
      and an invalid token hammered at Telegram, which rate-limits and can
      block an address that does it. The loop must not die of it (a token
      fixed by the operator should be picked up without a restart), so the
      answer is a BACK-OFF: a fatal refusal doubles the wait each time, up
      to a cap, and the first successful poll resets it. specs/telegram-bot.md;
      a test over the recording transport and the counting timer.
