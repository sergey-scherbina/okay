# okay-cache: caching with NAMED invalidation

## Overview

A cache is a materialized view of authoritative data, and the only
question that decides whether it is correct is INVALIDATION. The
industry's failure mode is answering that question with a default
TTL and hope. This spec's rule: **every cache names its invalidation
regime at construction** — where the truth lives determines what
"correct" can even mean, and there are exactly three honest cases.
Everything else (single-flight, bounds, Redis, metrics) is
machinery in service of that rule.

The stack changes the usual story in one place: for data whose
truth lives in okay-persist, the invalidation problem does not need
solving — a cache over a log is a CONSUMER, and consumers are never
invalid, only behind, by a measurable amount. One of the two hard
things falls out of the architecture already built.

## The three regimes (where the truth lives)

1. **Truth in the log (okay-persist)** — the preferred regime for
   our own data. The cache is a fold of a (compacted) topic at
   offset N: `latest(key)` is the fold's state, staleness is
   `end - N` — the SAME number as consumer lag in `Store.stats`,
   because it is consumer lag. Invalidation = reading on;
   coherence across nodes = each node folds the same topic (that
   is replication, stage-2 machinery, not cache machinery). This
   is the ui-durable snapshot story generalized, and it pairs with
   persist-stage1's `Snapshots`.
2. **Truth in a foreign system, writes flowing through us** (the
   specs/jdbc.md world) — WRITE-THROUGH: inside the same code path
   that commits, after the commit, the entry is updated or
   invalidated. Correct in-process by ordering; across processes
   the invalidation is an EVENT appended to a persist topic that
   every node's cache consumes — invalidation events are events,
   so audit and replay come free and no second bus is invented.
   The honest window is stated, not hidden: between their COMMIT
   and our invalidate, a reader can be served the old value;
   write-through shrinks the window to microseconds, nothing
   eliminates it without their transaction knowing about us.
3. **Truth changes behind our back** (others write to their
   database; no CDC) — no correct invalidation EXISTS. The only
   honest tool is a STALENESS BUDGET: "this value may be up to N
   seconds behind, and the business signed off on N." TTL is that
   budget — declared per cache or per entry, with NO default:
   a constructor without a named budget or regime does not
   compile/construct. Hope is not a config value.

## Interface

Traits, not effect rows (the `Store`/`Secrets` precedent); methods
speak `Async` because a distributed engine must and the memory
engine trivially can:

```scala
package okay.cache

/** how this cache is allowed to be wrong — named, not defaulted */
enum Regime:
  case Budget(ttlMillis: Long)      // regime 3: declared staleness
  case Invalidated                  // regime 2: write-through owns it
  // regime 1 is not a Regime value: a log-fed view is its own
  // constructor and is never invalid, only behind

trait Cache[K, V]:
  def get(k: K): Option[V] ! Async
  def put(k: K, v: V): Unit ! Async
  def invalidate(k: K): Unit ! Async
  /** the only read most callers should use: on a miss, ONE load per
   * key runs (single-flight); concurrent callers await it */
  def getOrLoad(k: K)(load: K => V ! Async): V ! Async
  def stats: Cache.Stats            // hits, misses, loads, evictions, size

object Cache:
  /** bounded ALWAYS — an unbounded cache is a leak with an alibi */
  def memory[K, V](regime: Regime, maxEntries: Int): Cache[K, V]

  /** regime 1: the fold of a compacted keyed topic; never invalid,
   * only behind — `lag` says by how much (pairs persist-stage1) */
  def view[K, V](topic: Topic, fold: (Option[V], Record) => Option[V]): View[K, V]
trait View[K, V]:
  def latest(k: K): Option[V] ! Async
  def lag: Long
```

Values and keys cross process boundaries only as bytes via `Schema`/
CBOR (the persist layering; a Redis value is a CBOR blob, readable
as JSON when a human needs to look). Negative results are cached by
making absence a value (`V = Option[A]`) under its own budget —
stated so the "cache the 404" question has one answer.

## Engines

- **Memory** (v1): bounded, LRU eviction, single-flight in
  `getOrLoad`, budgets enforced on read (an expired entry is a
  miss). Cross-platform — it is plain state.
- **Redis** (stage 2): the same trait over a MINIMAL RESP client —
  GET/SET PX/DEL/PING and little more — written over the same
  cross-platform Async I/O the cluster transport already proved.
  This stack already speaks SSE, JSON-RPC and MCP on the wire;
  RESP is simpler than any of them, and four commands do not
  justify a client-library dependency. Budget maps to `SET PX`
  (Redis enforces expiry server-side); `invalidate` is `DEL`.
  Connection config per specs/conf.md: address fields plus
  `password: Secret` — resolved at the edge, never stored. TLS
  (the `rediss` deployments) rides specs/tls.md — the RESP client
  adds nothing of its own.
- **Cross-node invalidation** (stage 2, regime 2): an invalidation
  topic in okay-persist, each node's cache a consumer. Redis
  pub/sub is the stated alternative when Redis is already the
  fabric and the loss-on-disconnect semantics are acceptable —
  named trade: pub/sub is fire-and-forget (a disconnected node
  misses the event and serves stale until budget/reconnect), the
  topic replays.

## What this spec refuses to build

- **Distributed locks over Redis** (Redlock et al.): a cache that
  needs a lock to be correct is a consensus problem wearing a hat.
  The single-flight guarantee is PER PROCESS; across nodes,
  concurrent loads are allowed and harmless (last write wins under
  the same regime). If a workload truly needs mutual exclusion,
  that is persist stage 4's consensus discussion, not a cache
  feature with famous correctness asterisks.
- **Cache hierarchies / near-far tiers**: composition of two
  `Cache`s by the caller if ever needed; not machinery.
- **A default TTL**: the absence of this feature is the feature.

## Behavior

- [ ] single-flight: N concurrent `getOrLoad` misses on one key run
      the loader ONCE; all N get its value; a second key loads
      independently (no global lock)
- [ ] a budget expires: within N the value serves, after N the
      entry is a miss and reloads; no budget can be left unnamed
      (construction requires a Regime)
- [ ] `invalidate` removes: the next read loads; `put` replaces
- [ ] eviction: at maxEntries the least-recently-USED entry leaves;
      hit/miss/eviction counts in `stats` match the scenario
- [ ] negative caching: an absent answer (None) is cached under the
      same budget and counted as a hit while fresh
- [ ] write-through with specs/jdbc.md: invalidate runs AFTER
      commit (ordering asserted); the stale window between commit
      and invalidate is demonstrated and documented, not denied
- [ ] regime 1 view: fold of a compacted keyed topic serves
      `latest`; `lag` equals end minus consumed; appends move it;
      a rebuilt view (cold start refold) agrees with the warm one
- [ ] (stage 2) Redis engine passes the same contract suite as
      memory (the StoreSuite pattern from okay-persist); expiry is
      server-side (`SET PX` observed); values round-trip as CBOR
- [ ] (stage 2) an invalidation event appended by node A is
      consumed by node B's cache: B's next read reloads; a node
      that was down replays the topic and converges

## Out of scope

- memcached and other engines — one more trait implementation when
  a deployment names one
- cache warming/preloading policies — a consumer folding a topic IS
  warm; for regime 3 warming is the caller iterating keys
- request-level memoization inside a single program run — that is a
  Map, not a module
- fighting the regime-2 stale window beyond write-through — closing
  it fully requires their transaction to carry our event (the
  outbox their no-DDL database forbids; specs/jdbc.md already
  states this honestly)

## Decisions

- **Named regimes, no default TTL** — the entire point: every cache
  states where its truth lives and how wrong it may be; a default
  would let the hardest question go unanswered silently. Rejected:
  TTL defaulting (the industry norm and the industry incident
  report).
- **Log-fed view as its own constructor, not a Regime** — regime 1
  is not a way of being wrong; it is a consumer with lag, and
  conflating it with expiring caches would cost the property that
  makes it valuable. Rejected: modeling views as TTL=∞ caches.
- **Single-flight inside `getOrLoad`** — dogpile protection is a
  correctness property of the read path, not caller discipline.
  Rejected: caller-side locking conventions.
- **Per-process single-flight only** — the distributed version is a
  lock with consensus obligations; duplicate loads across nodes
  are cheap and safe, mutual exclusion is not our claim. Rejected:
  Redlock-style locks (correctness asterisks the house style does
  not print).
- **Minimal own RESP over a client dependency** — four commands,
  a wire protocol simpler than the three this stack already
  speaks, zero new dependencies, and the trait hides it entirely.
  Rejected: jedis/lettuce (a dependency tree for GET/SET/DEL).
- **Invalidation via a persist topic first** — one machinery,
  replayable, auditable; pub/sub kept as a stated lossy
  alternative rather than the default. Rejected: Redis pub/sub as
  the primary bus.
- **Bytes at the engine edge via Schema/CBOR** — the persist
  layering, a third time, for the same reason. Rejected: JVM
  serialization or JSON-only values.
- **Bounded always** — `memory` without a bound does not exist in
  the API. Rejected: unbounded convenience constructors.

## Results

(after implementation — contract-suite counts against memory and
Redis, the measured stale window under write-through, view refold
agreement)
