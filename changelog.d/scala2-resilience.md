## scala2-resilience - okay-resilience from Scala 2.13: Guards

The first lane of stage 15 (operator: "делай всё что возможно чтобы
работало в скале 2").

- Probed from scalac 2.13.18: `Breaker`, `Bulkhead`, `Limiter`,
  `Deadline`, `Refused.*` and `okay.Retry`'s policies are readable, and
  Scala 2 builds them directly. What each piece DOES, transforming an
  `A ! Async`, is not usable.
- The new module okay-scala2-resilience adds `Guards`: `breaker` (with a
  `failing` predicate), `bulkhead`, `limiter` (keyed), `hedge`,
  `deadline` and `retry`, each over `Eff[Async, A]` and each one call
  into okay-resilience or okay-async. Named `Guards` so that it cannot
  collide with `okay.resilience.Resilient` under two wildcard imports.
- `TestResilienceFromScala2` has 7 tests: a breaker opens and refuses
  without running, a returned value counted as a failure, a bulkhead
  refusal (made deterministic with a latch instead of a sleep), a keyed
  limiter, retry to success and to exhaustion, a hedge overtaking a
  slow attempt, and a deadline cancelling a slow run.
- Docs: section 8j of docs/scala2.md (copied from the probe), a module
  page, API reference, and spec stage 15.1.
