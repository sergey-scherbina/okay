- [ ] telegram-serve-says — three things checked against the Bot API's own
      documentation before okay-watch's bot goes live, each the difference
      between a working bot and one that is silently dead:
      (1) `serve` swallows every refusal — a 409 (Telegram refuses a second
      getUpdates on one token, which is what a redeploy that left the old
      process running looks like) and a 401 (a wrong or revoked token) are
      answered by sleeping and asking again, forever, with nobody told. A
      loop that cannot poll must SAY so;
      (2) a 429 carries `parameters.retry_after` and we ignore it, so a
      rate limit is answered by hammering at our own interval;
      (3) `provider_token` must be OMITTED for payments in Telegram Stars
      (the Bot API changelog; the older docs allowed an empty string, which
      is what we send).
      specs/telegram-bot.md's behaviour box; a test each over the recording
      transport.
