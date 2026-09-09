# Resilience: five handlers around one operation

## Overview

The microservices audit of 2026-09-09 (the operator's direction:
"support microservices in all their beauty and complexity") found
the stack already holding most of what a service needs to survive
its neighbours — retry policies as streams (`Retry`, `retry`,
`supervised`, specs/parallel-resilience.md), `Async.timeout` and
`Async.race`, supervision (okay-actor), W3C tracing (okay-obs),
health and Prometheus (okay-ops), durable execution with idempotency
keys (okay-agent's `Durable`), and Raft-backed leadership
(okay-persist). What it did NOT find, by grep over specs, docs and
every module: a circuit breaker (0 hits — only "short-circuit"), a
bulkhead (0), a rate limiter (0 — specs/http.md added `Request.peer`
FOR one and no one wrote it), hedged requests (0), and a deadline
that travels with a request instead of restarting at every hop.

These five are one family, and the family already has a house shape:
a HANDLER around one operation, with the program blind to it. That is
how `Tracer.traced` wraps any `Handler[F]`, how `Secure.bearer` wraps
a route, how `Durable` wraps a tool call. The program says
`http.send(r)`; the edge decides what stands between that and the
wire. This spec adds the five, keeps their state as VALUES (a
`Schema`, so `/metrics` and a span attribute get them for free — the
observability doctrine of specs/obs.md), and injects the clock so
every state transition is testable without waiting.

## Interface

A new module, `okay-resilience` (JVM + JS, like okay-http; depends on
`okay` and `okay-http`). The generic pieces work over any `A ! Async`;
the `Http` adapters are the stage-1 layer over them.

```scala
package okay.resilience

// ---- the shared vocabulary

/** every piece reports itself as a value; a Schema makes it a metric
  * and a span attribute without a second definition */
trait Reporting[S]:
  def stats: S

/** a refusal is a NAMED exception carrying what a caller can act on —
  * how long to wait — never a bare RuntimeException; and it is the
  * same thing a server turns into a status and a Retry-After */
sealed abstract class Refused(msg: String) extends RuntimeException(msg, null, false, false):
  def retryAfterMillis: Option[Long]

// ---- 1. Circuit breaker

object Breaker:
  enum State derives Schema:
    case Closed, Open, HalfOpen
  final case class Stats(state: State, consecutiveFailures: Int,
                         calls: Long, failures: Long, rejected: Long,
                         opened: Long) derives Schema
  final class Open(name: String, val retryAfterMillis: Option[Long]) extends Refused(...)

final class Breaker(name: String,
                    failures: Int,           // consecutive failures that open it
                    openMillis: Long,        // how long Open lasts before one probe
                    clock: () => Long = () => System.currentTimeMillis)
  extends Reporting[Breaker.Stats]:
  /** the operation, or `Breaker.Open` without running it */
  def protect[A](prog: => A ! Async)(failing: Either[Throwable, A] => Boolean = _.isLeft): A ! Async

// ---- 2. Bulkhead

object Bulkhead:
  final case class Stats(permits: Int, inFlight: Int, waiting: Int,
                         rejected: Long) derives Schema
  final class Full(name: String) extends Refused(...)   // retryAfter None

final class Bulkhead(name: String, permits: Int, queue: Int = 0)
  extends Reporting[Bulkhead.Stats]:
  /** runs with a permit, parks in the queue for one, or refuses with
    * `Bulkhead.Full` when the queue is full too; the permit is released
    * on every exit, cancellation included */
  def limit[A](prog: => A ! Async): A ! Async

// ---- 3. Rate limiter (token bucket)

object Limiter:
  final case class Stats(keys: Int, admitted: Long, delayed: Long,
                         rejected: Long) derives Schema
  final class Exhausted(name: String, key: String, val retryAfterMillis: Option[Long]) extends Refused(...)

final class Limiter(name: String,
                    ratePerSecond: Double, burst: Int,
                    maxWaitMillis: Long = 0,     // 0: refuse instead of parking
                    clock: () => Long = () => System.currentTimeMillis)
  extends Reporting[Limiter.Stats]:
  /** one bucket per key; a full bucket is indistinguishable from an
    * absent one, so full buckets are evicted and the map stays
    * proportional to the keys active within burst/rate seconds */
  def admit[A](key: String = "")(prog: => A ! Async)(using Timer): A ! Async

// ---- 4. Hedged requests

object Hedge:
  /** start one; if it has not answered after `afterMillis`, start
    * another, up to `max` in flight; the first SUCCESS wins and the
    * rest are cancelled. Only for operations safe to repeat. */
  def run[A](afterMillis: Long, max: Int = 2)(prog: => A ! Async)
            (using Scheduler, Timer): A ! Async

// ---- 5. Deadline

/** an absolute instant on THIS node's clock; what travels is the
  * REMAINING budget, recomputed at every hop (gRPC's model) */
final case class Deadline(atMillis: Long):
  def remaining(now: Long): Long = atMillis - now

object Deadline:
  val header = "x-deadline-ms"
  final class Exceeded(val remainingMillis: Long) extends Refused(...)
  def in(millis: Long, clock: () => Long = ...): Deadline
  /** the program within the budget, or `Exceeded` — before starting it
    * when the budget is already gone, and by cancellation otherwise */
  def enforce[A](d: Deadline, clock: () => Long = ...)(prog: => A ! Async)
                (using Scheduler, Timer): A ! Async
  /** header in / header out */
  def read(r: okay.http.Request, clock: () => Long = ...): Option[Deadline]
  def carry(r: okay.http.Request, d: Deadline, clock: () => Long = ...): okay.http.Request

// ---- stage 1: around trait Http, in one fixed order

object Resilient:
  /** deadline → breaker → bulkhead → limiter → hedge → inner. Each
    * piece optional; the order is the decision, stated below. A 5xx
    * COUNTS as a failure for the breaker; a 4xx does not. Hedging
    * applies to safe methods only unless told otherwise. */
  def http(inner: Http,
           deadline: Option[Deadline] = None,
           breaker: Option[Breaker] = None,
           bulkhead: Option[Bulkhead] = None,
           limiter: Option[(Limiter, Request => String)] = None,
           hedge: Option[(Long, Int)] = None,
           hedgeable: Request => Boolean = r => r.method is Get/Head/Options)
          (using Scheduler, Timer): Http

  /** server side: a refusal becomes a status — 429 for a limiter,
    * 503 for a breaker or a bulkhead, 504 for a deadline — with
    * `Retry-After` in whole seconds where the refusal knows one */
  def route(limiter: Option[(Limiter, Request => String)] = None,
            bulkhead: Option[Bulkhead] = None)
           (routes: PartialFunction[Request, Response ! Async])
           (using Timer): PartialFunction[Request, Response ! Async]
```

okay-ops grows `Prom.render` rows for these stats (stage 1), keyed by
the piece's `name`, the same pure mapping it does for `Store.Stats`.

## Behavior

Stage 0 — the five, generic, deterministic under an injected clock:

- [ ] breaker: `failures` consecutive failures open it; while Open
      the operation is NOT run and `Breaker.Open` carries the
      remaining open time; after `openMillis` ONE probe runs
      (HalfOpen) — its success closes, its failure re-opens for a
      fresh `openMillis`; a success in Closed resets the count; the
      `failing` predicate decides what a failure is (a returned
      value can be one)
- [ ] breaker stats: `calls`, `failures`, `rejected`, `opened` count
      what happened; `state` is the current state
- [ ] bulkhead: with N permits, N+1 concurrent programs hold N in
      flight and one waiting when `queue >= 1`; with `queue = 0` the
      (N+1)th is refused with `Bulkhead.Full` at once, nothing runs
      twice; the permit is released when the program completes, when
      it fails, and when it is cancelled while waiting
- [ ] limiter: `burst` calls pass at once, the next is refused with
      `Exhausted` naming the wait; with `maxWaitMillis > 0` it PARKS
      instead and passes when the bucket has refilled (the clock is
      the test's); keys are independent buckets; a full bucket is
      evicted so `stats.keys` falls back to the active ones
- [ ] hedge: a slow first attempt is joined by a second after
      `afterMillis`; the FIRST SUCCESS answers and the other is
      cancelled; a fast first attempt never starts a second; when
      every attempt fails, the failure is the last one's; `max`
      bounds attempts in flight
- [ ] deadline: an expired budget refuses BEFORE running; a budget
      that expires mid-run cancels the run and answers `Exceeded`;
      `carry` writes the REMAINING budget (not the absolute instant)
      and `read` turns a header back into a local `Deadline`; a
      damaged or negative header reads as `None`
- [ ] every refusal is a `Refused` with a name and, where it has
      one, a `retryAfterMillis` — one type for the server to map

Stage 1 — around Http, and visible:

- [ ] `Resilient.http` composes in the fixed order and a request
      passes every piece exactly once; a 5xx trips the breaker, a
      404 does not; a Post is not hedged by default
- [ ] `Resilient.route` answers 429 + `Retry-After` for an exhausted
      limiter keyed by `Request.peer`, 503 for a full bulkhead; the
      route stays defined exactly where the wrapped one is
- [ ] `Deadline.read` on the server + `carry` on the client: a
      budget shrinks across two hops in a test with a controlled
      clock, and the second hop refuses what the first would have
      let through
- [ ] okay-ops renders the four `Stats` as Prometheus gauges and
      counters, `name` as the label; the existing `/metrics` route
      takes them beside `Store.Stats`

Stage 2 — proving it under faults:

- [ ] `Faults.http(seed, plan)(inner)`: a seeded fault-injecting
      `Http` (delays, drops, 5xx by ordinal) — the deterministic
      adversary; the composite under a plan behaves per the
      pieces' contracts (breaker opens on the drops, hedging hides
      the delays, the deadline bounds the whole)
- [ ] adaptive concurrency (a bulkhead whose permits follow observed
      latency, Netflix's gradient) is DEFERRED with a measured reason
      or landed here — not before stage 1 is in use

## Out of scope

- Retry: exists (`okay.retry`, JVM/Native). Its JS twin over
  `Async.sleep` is a separate small lane (retry-js), noted in BACKLOG.
- Service discovery, client-side load balancing, sagas, outbox/inbox:
  the audit's next items, each its own spec.
- Distributed rate limiting (a shared bucket across nodes): the
  bucket is per process. Across nodes the answer is the log
  (okay-persist), and it is not designed here.
- Sliding-window failure RATES for the breaker. Consecutive failures
  is the first cut because it has no window to size; a rate-based
  trip is additive later and stats already carry the counts.
- Mapping refusals to statuses for servers other than the
  `PartialFunction` route shape (jetty/netty share it already).

## Design

**State is one cell.** Each piece keeps its state in a single
`TRef` and moves it with one `modify` — the same shape okay's own
STM consumers settled on (Ui close, Tx.orElse, McpHttp sessions).
No locks, no second cell, and the JS platform runs it unchanged
because a one-cell CAS on one thread is a plain write.

**Time is injected.** Every piece that reasons about time takes a
`clock: () => Long`, as `Tracer` does; a test drives the clock and
never sleeps. Parking (the bulkhead's queue, the limiter's wait, the
hedge's delay) goes through `Async.await` and `Async.sleep`, so it
is cross-platform and cancellable through the `Await` canceller.

**Refusal is a value with a name.** `Refused` is the one type a
server maps to a status; each subclass says which piece refused and
carries the wait when it knows one. Nothing here returns a
synthesized `Response` on the client side: the seam's contract is
that a 4xx/5xx is DATA from the far end, and a local refusal is not
one — it is an exception in `Async`, like a dropped wire.

**The order is a decision, not a default.** Deadline outermost,
because the budget bounds everything under it including the waits;
breaker next, so an open circuit costs nothing (no permit taken, no
token spent); bulkhead before limiter, so a refused permit does not
burn a token; hedge innermost, so every hedged attempt is one call
of the inner — and the breaker sees hedged failures as what they
are. `Resilient.http` fixes this; the standalone pieces let anyone
compose otherwise, knowingly.

**Hedging is not retry.** Retry waits for a failure; hedging acts on
SLOWNESS and lets attempts overlap. Only safe operations belong
under it, which the Http adapter enforces by method unless told
otherwise, and the generic `Hedge.run` states in its contract.

**Deadline travels as remaining time.** Absolute instants need
synchronized clocks; a remaining budget needs none and loses only
the transit time, which is the loss gRPC accepts (`grpc-timeout`).
The header name is ours because REST has no standard one; the
semantics are gRPC's so that a gateway can translate.

## Decisions

- **A module, not the core.** The core carries `Retry` because a
  policy is a stream and streams are the core's; these five carry
  state, time and a `Schema`, and the Http adapters need okay-http.
  Rejected: okay-http (a client that knows about breakers is the
  framework smell this stack avoids); the core (would pull `Schema`
  in or leave stats untyped).
- **Consecutive failures, not a rate window** — the smaller model
  with no parameter to get wrong first; the rate form is additive.
- **Full-bucket eviction for keyed limiting** — a full bucket IS the
  absent state, so evicting it changes no answer; memory follows
  activity, not history. Rejected: an LRU with a cap (a hot key can
  evict a live bucket and hand a flooder a fresh burst).
- **Exceptions for refusals, not `Either`** — `Http.send` answers
  `Response ! Async` and the existing retry in TestHttp already
  treats a thrown wire error as the failure shape; a second channel
  would make every caller handle two.
- **`x-deadline-ms`, relative** — see Design. Rejected: W3C
  `baggage` (a pass-through, no tool acts on it); an absolute
  epoch (needs clock agreement).

## Results

(filled in as stages land)
