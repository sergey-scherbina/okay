## okay2-producer-generate - producers and generators in okay2

`Produce` is okay2's real effect now (it was a test helper of the same
shape): `produce`, `type Producer[A]`, stream instances, and
`Producer.fold`/`foldUntil`/`concat`/`each`/`log`. And okay's generators
from delimited control: `Loop`, `take`, `loop`, `Put` (LazyList and
Producer), `generate`, `generateLazy`, `nats`, `fibs` — one generator,
two streams (specs/okay2.md stage 18). 7 tests.

Docs: docs/okay2.md section 22.
