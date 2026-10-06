# SQL semantic adapter bugs

## jdbc-discovery-fixture — driver discovery varies under family execution
<!-- status: fixed
     fixed-in: 1eb9b8eb0
     area: test-fixture
     gate: okaySemanticSqlJVM/testOnly okay.semantic.sql.TestSemanticSql -->

Found in CI 20261006T064054Z on 2026-10-06: the first SQL parity test
failed with No suitable driver found for jdbc:h2:mem:. All nine passed
when the runner retried them alone. The fixture currently uses
DriverManager; replace global discovery with a direct test driver and
verify absence of registry discovery in an isolated child JVM.

Fixed by the direct optional-test H2Fixture. The bounded isolated child JVM
proves DriverManager fails after deregistration while the same direct fixture
executes SELECT 42. SQL suite: 10 passed. The exact family-run classloader
interleaving was not reproduced; its global-discovery dependency is removed.
