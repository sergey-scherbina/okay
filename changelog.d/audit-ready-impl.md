## audit-ready-impl — package-aware audit layers and deterministic clock/random ports

The audit can now classify package prefixes within one artifact, with each
module's mapping kept private to that module. `Clock` and `Random` are core
ports with deterministic test handlers and platform implementations; `Hlc`
and `Uid` receive their sources from `okay-platform`, so `okay-data` is again
a business module. `sbt audit` produces a passing report with the updated
inventory.

Spec: `specs/audit-ready.md`, stages 1–3. Tests: `TestAudit`,
`TestClockRandom`, `TestAmbient`, `TestUid`, CRDT suites, `TestCapability`,
`TestOkayNpm`, and `sbt audit`.
