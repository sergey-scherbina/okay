- [ ] limiter-blocking-pace — found from okay-watch (2026-09-25): its
      `Throttle` (a request interval for a blocking HTTP caller, a public
      index's pace) is a token bucket of burst 1 that PARKS the thread and
      never refuses, which `Limiter` is but only on the `Async` road and
      with a `maxWaitMillis` it refuses past. `Limiter.pace(key)(sleep)`:
      the token taken now, the caller sleeps for it, never refused,
      counted in `stats` as the rest are — so the caller's pace shows in
      /metrics (Prom.guards) and okay-watch's Throttle is this.
