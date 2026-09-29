## telegram-serve-says - the wire checked against the Bot API's own docs, and three silences fixed

Before okay-watch's check bot goes live, every parameter name this module
sends was compared with the current Bot API documentation. The names all
matched; three behaviours did not.

- **A loop that cannot poll now says so.** `serve` takes `onRefused` and
  reports every refusal before retrying; it still never dies of one. The
  two failures a live bot meets — a 409 (Telegram refusing a second
  `getUpdates` on one token, i.e. a redeploy that left the old process
  running) and a 401 (a token wrong or revoked) — both look like «the bot
  does not answer» from outside and were invisible. `Refused.fatal` says
  which of them waiting cannot fix.
- **`retry_after` is read and honoured.** A 429 carries
  `parameters.retry_after`; `Refused.retryAfter` holds it and `serve`
  waits that long, with `retryMs` as the fallback. Answering a rate limit
  at our own interval is hammering.
- **`provider_token` is OMITTED for Telegram Stars**, as the Bot API
  changelog requires; it was sent as an empty string, which only the
  older documentation allowed.

Two tests: the three refusals reported with their codes and the waits
they caused (2s, 3s from the API, 2s), and the invoice without the field.
