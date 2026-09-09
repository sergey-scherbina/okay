# okay-resilience

> Five handlers around one operation (specs/resilience.md): a
> circuit breaker, a bulkhead, a keyed token-bucket limiter, hedged
> requests and a deadline that travels with the request. The program
> stays blind to them — it says `http.send(r)` — and the edge decides
> what stands between that and the wire, the way `Tracer.traced`
> wraps a Handler and `Secure.bearer` wraps a route.

Depends on: `okay`, `okay-http` (the `Request` the deadline header
rides on). JVM + JS.

## Guide

**Every piece is a program transformer** over `A ! Async`, with its
state in one cell and its clock injected:

```scala
import okay.*, okay.given
import okay.resilience.*

val breaker  = Breaker("payments", failures = 5, openMillis = 10_000)
val bulkhead = Bulkhead("payments", permits = 32, queue = 64)
val limiter  = Limiter("payments", ratePerSecond = 200, burst = 50)

val guarded: Response ! Async =
  breaker.protect(bulkhead.limit(limiter.admit()(http.send(r))))(_.exists(_.status >= 500))
```

| piece | what it decides | the refusal |
|---|---|---|
| `Breaker(name, failures, openMillis)` | `failures` consecutive failures open it; open refuses without running; after `openMillis` one probe runs, its outcome closes or re-opens. `failing` says what a failure is — a returned 5xx can be one | `Refused.BreakerOpen`, with the remaining open time |
| `Bulkhead(name, permits, queue)` | at most `permits` in flight, at most `queue` parked for one, the rest refused at once; a permit is released on completion, failure, and a cancel while parked | `Refused.BulkheadFull` |
| `Limiter(name, ratePerSecond, burst, maxWaitMillis)` | a token bucket per key (`admit(key)`); no token → refused naming the wait, or parked for it when the wait is within `maxWaitMillis`; full buckets are evicted, so the map follows activity | `Refused.Exhausted`, with the wait |
| `Hedge.run(afterMillis, max)` | a slow attempt is joined by another after the delay, up to `max` in flight; the first SUCCESS answers, the rest are cancelled; a failure starts nothing — this is not retry, attempts overlap, so only repeat-safe operations belong here | — |
| `Deadline` | an absolute instant on this node; `enforce` refuses an expired budget before running and cancels a run that outlives it; `carry`/`read` move the REMAINING budget through the `x-deadline-ms` header (gRPC's model, no clock agreement needed) | `Refused.DeadlineExceeded` |

**A refusal is one type.** Every piece refuses with a `Refused` — a
named exception carrying `retryAfterMillis` when the refuser knows
it. That is the one thing a server maps to a status (429 / 503 /
504 with `Retry-After`, stage 1). It is deliberately an exception in
`Async` and not a synthesized `Response`: a 4xx/5xx is DATA from the
far end, a local refusal is not one — it is a dropped wire's shape.

**State is a value.** `stats` on a breaker, a bulkhead and a limiter
is a case class with a `Schema`, so a `/metrics` row and a span
attribute need no second definition (stage 1 wires them into
okay-ops). The clock is a `() => Long` you can hand in, which is how
the suite drives every state transition without sleeping.

**Order matters, and stage 1 fixes it** for `Resilient.http`:
deadline outermost (the budget bounds the waits too), then breaker
(an open circuit spends no permit and no token), then bulkhead, then
limiter, hedge innermost (each hedged attempt is one call of the
inner). The standalone pieces let you compose otherwise, knowingly.

**What is already elsewhere.** Retry is `okay.retry` (policies are
streams, specs/parallel-resilience.md); timeout is `Async.timeout`;
tracing is okay-obs; health is okay-ops; idempotency keys and
recovery are okay-agent's `Durable`. This module adds only what the
2026-09-09 audit found missing.

## Gotchas

- A cancel that lands while a program is IN FLIGHT under a bulkhead
  reaches it as an interrupt on the JVM (the permit comes back); on
  JS a cancelled drive simply stops, so a program cancelled mid-way
  under a bulkhead there holds its permit until it would have
  finished. A cancel while PARKED returns the permit on both.
- The limiter's bucket goes briefly negative when a caller is
  admitted with a wait: that reservation is what keeps two parked
  callers from both taking the same token.
- Hedging a Post is your decision, not the library's: `Hedge.run`
  has no way to know what the program does. Stage 1's Http adapter
  hedges safe methods only unless told otherwise.
