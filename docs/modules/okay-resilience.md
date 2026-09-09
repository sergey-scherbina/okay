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

**Around `Http`, in one order.** `Resilient.http(inner, budgetMillis,
breaker, bulkhead, limiter, hedge)` composes deadline outermost (the
budget bounds the waits too), then breaker (an open circuit spends
no permit and no token), then bulkhead, then limiter, hedge
innermost (each hedged attempt is one call of the inner). A 5xx
counts as a breaker failure, a 4xx does not; hedging applies to
Get/Head/Options unless `hedgeable` says otherwise; a deadline the
request already carries is honoured, the earlier of it and the
budget wins, and the outgoing header says what is left.

```scala
val client = Resilient.http(Transports.http(), budgetMillis = Some(2000),
  breaker = Some(breaker), bulkhead = Some(bulkhead),
  limiter = Some((limiter, _ => "payments")), hedge = Some((100L, 2)))
```

**On the server, a refusal is a status.** `Resilient.route(limiter =
Some((l, Resilient.byPeer)), bulkhead = Some(b))(routes)` keeps the
routes defined exactly where they were and answers 429 for the
limiter, 503 for a bulkhead, 504 for a carried deadline that ran
out — with `Retry-After` in whole seconds where the refusal knows
one. `Request.peer` is the key a server wants: the sender as the
transport saw it, not a header anyone can write.

**Propagating a deadline** is two calls: `Deadline.read(r)` where a
request enters, `Deadline.carry(out, d)` where a call leaves — or
just `Resilient.http`, which reads a carried header itself. The
header is relative, so transit is not charged; the work a hop does
before calling on is.

**`/metrics`.** `Ops.routes(store, guards = Vector(breaker, bulkhead,
limiter))` renders `okay_breaker_*`, `okay_bulkhead_*`,
`okay_limiter_*` rows beside the store's, `name` as the label;
`Prom.guards` is the pure mapping if you render elsewhere.

**What is already elsewhere.** Retry is `okay.retry` (policies are
streams, specs/parallel-resilience.md); timeout is `Async.timeout`;
tracing is okay-obs; health is okay-ops; idempotency keys and
recovery are okay-agent's `Durable`. This module adds only what the
2026-09-09 audit found missing.

**Discovery and balancing.** A service is N addresses behind a name.
`Discovery` turns the name into endpoints — `Discovery.static(table)`
for tests and compose files, `Discovery.env(lookup)` for what
Kubernetes writes into every pod (`ORDERS_SERVICE_HOST`/`_PORT`) or
a comma list `OKAY_SERVICE_ORDERS=h1:p1,h2:p2`, `DiscoveryJvm.dns(port)`
for a headless Service's A-records (wrap it in `Discovery.cached`
for a ttl), `Discovery.chain` to try them in order. `Balanced(d).http
(inner)` is the client: the program says `http://orders/v1/…`, the
balancer picks an endpoint round-robin among those not cooling down
and sends `http://10.0.3.7:8080/v1/…`; a host the discovery does not
know passes through unchanged, so one client serves service calls
and the outside world. A THROWN wire error cools its endpoint down
(default 5 s) and the next call goes elsewhere; an answered 5xx is
the far end speaking and is left to the breaker. All endpoints
cooling down → the least recently failed is tried; an empty
resolution refuses with `Refused.NoEndpoint` (503 on a server).
`Resilient.http(..., balanced = Some(b))` puts it innermost, so each
hedged attempt picks its own endpoint and the breaker counts per
service. `stats` names what is down.

```scala
val discovery = Discovery.chain(DiscoveryJvm.env(), Discovery.cached(DiscoveryJvm.dns(8080), 5_000))
val client = Resilient.http(Transports.http(), breaker = Some(breaker), balanced = Some(Balanced(discovery)))
client.send(Request.get("http://orders/v1/orders/42"))
```

**Seams that stream.** `Resilient.http` fits `Request => Response !
Async`. The repo's other seams — `okay.llm.Transport`, `okay.mcp
.Link`, `okay.cluster.Remote` — post and then TELL their answer, so
their programs are `A ! (F + Async)`. `Resilient.guarded` is the same
order for those, and the pieces have row variants of their own
(`protectIn`, `limitIn`, `admitIn`):

```scala
val guardedTransport: Transport = new Transport:
  def post(url: String, headers: Map[String, String], body: String) =
    Resilient.guarded(inner.post(url, headers, body),
      breaker = Some(breaker), limiter = Some(limiter), key = "anthropic")
```

The permit and the circuit span the WHOLE stream, not its first
line — a guard that released early would let N callers into a seam
with one permit, and the test that proves otherwise parks a seam
mid-stream. okay-resilience takes no dependency on llm, mcp or
cluster: this is wired at the caller's edge.

**Testing the composite: `Faults.http`.** A seeded adversary between
your client and a fake far end: `Faults.http(seed, Faults.Plan(dropAt
= Set(2, 3), slowAt = Map(1L -> 5000L), failRate = 0.2))(far)`. A
call's fate is a pure function of the seed and its ordinal, so a
hedged race replays exactly; `log` says what each ordinal met, and a
found bug is a seed. This is how `TestFaults` proves the pieces'
contracts hold through the whole stack rather than one at a time.

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
