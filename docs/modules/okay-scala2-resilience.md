# okay-scala2-resilience

okay-resilience for **Scala 2.13**. The pieces (`Breaker`, `Bulkhead`,
`Limiter`, `Deadline`), the refusal (`Refused.*`) and the retry policies
(`okay.Retry.*`) are okay's own and are used directly from Scala 2. What
Scala 2 cannot use is what each piece DOES, which is to transform a
program. `Guards` provides those transformations over `Eff[Async, A]`:

| | |
|---|---|
| `Guards.breaker(b)(prog, failing)` | refuse while open; `failing` says what counts as a failure |
| `Guards.bulkhead(b)(prog)` | at most the permits in flight, the queue parked, the rest refused |
| `Guards.limiter(l, key)(prog)` | a token per call from `key`'s bucket |
| `Guards.hedge(afterMillis, max)(prog)` | a slow attempt joined by another; the first success wins |
| `Guards.deadline(d)(prog)` | refuse an expired budget, cancel a run that outlives it |
| `Guards.retry(policy)(prog)` | run again after each failure, waiting the policy's delays |

The walkthrough is section 8j of
[okay from Scala 2.13](../scala2.md#8j-resilience-breaker-bulkhead-limiter-hedge-deadline-retry), and the signatures are in
[okay-scala2](okay-scala2.md#api-reference).
