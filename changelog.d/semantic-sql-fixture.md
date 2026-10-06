## Semantic SQL tests independent of JDBC discovery

Fix: 1eb9b8eb0. TestSemanticSql now opens its H2 databases through a direct
optional-test driver. It no longer relies on global driver discovery, which
failed once in the family gate and passed when retried alone.

An isolated child JVM removes driver registrations, proves DriverManager cannot
open H2 and executes a query through the same fixture. All 10 SQL tests pass;
production semantic APIs are unchanged. Spec: specs/semantic-sql-fixture.md.
