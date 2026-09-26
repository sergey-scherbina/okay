## stateful-early-stop — a partition's end gives back what its stages hold (2026-09-26)

A stateful stage (`statefulIn`) under a downstream `take` never ran `finish`
and never failed, so its leased interpreter stayed out of the pool until the
JVM ended. The engine now opens an `okay.cluster.Scope` around every
partition fold (six sites in `Flows`, three in the distributed worker; a
streaming session's closes at `finish`), and a stage that holds something is
a `Flow.Owned` that registers its close there — `Stateful.through` abandons a
state neither `finish` nor a failure gave back. TestStatefulLease: early stop
by a `take`, directly and through `Flows.fold`, gives the state back once;
a full partition only by `finish`. Docs: docs/modules/okay-cluster.md.
specs/foreign-one.md Decision 20. The far-side source half is filed as
foreign-source-early-stop.
