# Independent SQL parity fixture

## Problem
The post-landing family JVM run failed in TestSemanticSql with
`No suitable driver found for jdbc:h2:mem:`. Its isolated nine-test rerun
passed. JDBC service discovery is global/classloader-sensitive; the fixture
must not depend on another suite having registered H2 first.

## Design
Use an explicit H2 Driver in a test-only fixture, retaining the existing
optional test dependency. Open anonymous in-memory databases directly through
Driver.connect, without DriverManager. Production semantic/SQL APIs do not change.
An isolated child JVM deregisters its drivers, proves DriverManager cannot open
H2, then executes a real query through the same fixture used by parity tests.
The child has a bounded wait and its output is included on failure. No other
suite's process registry is mutated.

## Behavior
- [x] Fixture runs a query when the child JVM's driver registry has no H2.
- [x] All existing SQL parity and cardinality tests pass.
- [x] New test/fixture compile without warnings.

## Results
Focused SQL gate passed all 10 tests, including the isolated no-registry
regression. The module and test fixture compiled without warnings; cold
meta-build compilation emitted an existing ReleaseWave.scala task-lint warning,
which the gate excludes from module warning verdicts. No semantic runtime or
production dependency changed.
