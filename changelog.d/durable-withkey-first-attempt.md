## durable-withkey-first-attempt — key transport before the first external action

WithKey now transports the journal key on fresh calls as well as retries,
while retaining the original request fingerprint and intent-first order.
The regression reproduced two provider actions when answer persistence
failed; it now proves one action and no remote call on completed replay.
Supplied Tool key fields are replaced by the journal key.

Implementation commits: `durable: send WithKey on the first external
attempt` and `durable: repair Scala 2 facade types and remaining foreign
consumer`. The affected gate exposed two prior extraction omissions:
Scala 2 cannot construct exported class aliases/read an exported enum,
and a foreign-workflow test retained the agent import. Explicit facade
aliases/constructors and the neutral import repair both.

Validation: 9 JVM/JS targeted regressions, 5 compatibility checks and
GREEN affected staged gate (2094 results), no compile warnings. Contract:
specs/durable-withkey-first-attempt.md. Run-scoped identities and actual
process-crash recovery remain separate follow-ups.
