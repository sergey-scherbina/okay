## docs-adapters-merge - the Docs engines move into okay-docs

`okay-docs-mongo`, `okay-docs-dynamo` and `okay-docs-cassandra` are gone
as modules; `MongoDocs`, `DynamoDocs` and `CassandraDocs` live in
`okay-docs` as JVM-only sources (`okay-docs/src/{main,test}/scala-jvm`,
packages unchanged: `okay.docs.mongo`, `.dynamo`, `.cassandra`), so the
seam and every engine behind it are one module (operator ask).

The price, recorded in specs/data.md's Decisions: `okay-docs` on the JVM
now carries mongodb-driver-sync 5.2.1, java-driver-core 4.18.1,
`okay-blob` and `okay-http` (plus `okay-sql`/`okay-pg` in Test for the
persistence e2e); JS and Native are unchanged. The JVM test run forks,
as the satellites' did.

Found on the way: two of the three satellites (dynamo, cassandra) were
never in the root aggregate, so no gate compiled them, and
`TestPersistenceE2E` carried an unused `+` import from the day it was
written. It is removed; the gate sees all three engines now.

Docs: the three module pages fold into docs/modules/okay-docs.md
("Engines"), the index rows into okay-docs' row; `TestNoCredentialLogs`
scans okay-docs, which now includes them.
