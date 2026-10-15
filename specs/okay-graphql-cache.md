# okay-graphql-cache: one execution's cache of fetched keys

The executor groups the keys of ONE round; a key asked again in a LATER round went to its source again. DataLoader
caches per request; so does this, per execution — the same scope, nothing shared between requests.

## Behavior

- A key fetched once in an execution is answered from the cache when asked again, in any later round: its source
  is not called for it. Inside one round, keys were already asked once.
- An error is cached as a value is: a key the source left out, or a failed call, fails every later ask of it the
  same way, without calling again.
- A mutation's top-level fields run one after another; the cache is CLEARED between them, so a field sees what the
  previous one wrote.
- A source opts out with `override def cached = false`: every round asks it again (data that changes during the
  request, a clock).
- The cache holds what the execution fetched, no more: it lives in the execution and ends with it.

- [ ] a key in two rounds: one call
- [ ] an error cached
- [ ] a mutation's fields see fresh data
- [ ] a source that opts out is asked each round
- [ ] async sources cached the same

## Decisions

- Cleared between mutation fields rather than never cached in a mutation: the fields below a mutation's top level
  still batch and cache as a query's do; only the serial boundary, where writes happen, forgets.
- The cache and the round's waiting keys are one per-source state, found by identity — the one cast of `batchOf`
  stays the one cast.
