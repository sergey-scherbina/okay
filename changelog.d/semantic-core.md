## Independent semantic core and SQL analytics

Implemented in d2d561dbf: okay-semantic defines business entities, relationships,
versioned datasets, dimensions, measures and validated metric plans independently
of agents and storage. Memory execution supports equality filters, grouping,
exact sums, counts, averages and ratios of sums with explicit null semantics.
okay-semantic-sql binds the same plan to one table through Sql, parameterizes
filters and shares the decimal finalizer. Invalid bindings and decode damage
are named errors. Definitions and examples are documented in specs/semantic.md
and the module pages.

Validation: 10 core/example tests on JVM, JS and Native; 6 SQL tests against H2;
all new platform targets Test/compile; docs/index/board/changelog checks green,
no changed-module compile warnings and no unbounded recursion. Joins, time and
derived metrics, graph facts and context assembly have separate backlog entries.
