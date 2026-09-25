## limiter-blocking-pace - Limiter.pace: a blocking caller's interval, in /metrics

Found from okay-watch: its `Throttle` (a request interval for a blocking
HTTP caller) was a token bucket of burst one that parks the thread and never
refuses — `Limiter`, but only on the Async road and with a refusal past
`maxWaitMillis`.

- `Limiter.pace(key)(sleep)`: the token taken now, the caller sleeps for its
  turn, never refused; counted in `stats` as admitted (and delayed), so the
  pace is in /metrics through `Prom.guards`. `sleep` injected for tests.
- TestLimiterPace (4): one a second at sixty a minute, time spent counted,
  never refused whatever `maxWaitMillis`, at least a millisecond at any rate.
  docs/modules/okay-resilience.md.
- Gate: `affected master` 363 GREEN, no warnings.

Landed as 9dc50edf6.
