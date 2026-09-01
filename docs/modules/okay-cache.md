# okay-cache

> Caching with NAMED invalidation: every cache states at
> construction where its truth lives and how wrong it may be. There
> is no default TTL — the absence of that feature is the feature
> (specs/cache.md).

Depends on: the core only. Cross-built JVM/JS/Native.

## Guide

**Three regimes, one rule.** Truth in the log (okay-persist) → the
cache is a consumer, never invalid, only behind (the `View`,
arriving with cache-view). Truth in a foreign system with writes
through us → `Regime.Invalidated`, write-through owns correctness.
Truth changes behind our back → `Regime.Budget(n)`: a staleness
budget the business signed off on, declared, never defaulted.

**The read path is `getOrLoad`.** On a miss ONE load per key runs —
single-flight, per process — and concurrent callers await that
load's result instead of dogpiling the source. The loader runs
under its own drive, so a failure anywhere in it reaches every
waiter (no hangs) and releases the key. Across nodes concurrent
loads are allowed and harmless; a cache that needs a distributed
lock to be correct is a consensus problem wearing a hat, and this
module refuses to print that asterisk.

**Bounded always.** `Cache.memory(regime, maxEntries)` — there is
no unbounded constructor; eviction is LRU; an expired entry is a
miss. `stats` (hits, misses, loads, evictions, size) is a plain
value: an endpoint, a log line and a test assertion are the same
thing.

**Negative caching** is not a feature, it is a type: make absence a
value (`V = Option[A]`) and it caches under the same budget.

Redis (a minimal own RESP client) and the cross-node invalidation
topic ride later slugs behind the same trait.
