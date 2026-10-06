## Semantic analytics across data engines

Implementation: 268147235. Specification: specs/semantic-layer.md.

Business definitions now support derived metric DAGs, comparison filters,
having/order/pagination, exact mergeable partial statistics, fixed/civil time,
unique safe relation routes and typed chained lookups. SQL bindings preserve
business cardinality, preflight duplicate keys and share metric finalization.

New okay-semantic-data interprets Source/chunks, Bulk/Tables, files, CSV and typed
JSON, and exposes an exact-decimal JSON query/describe/explain API.
okay-semantic-arrow supplies Arrow table/IPC execution; existing ParquetFormat
feeds the same file interpreter. Definitions have no agent dependency.

Validation: 41 JVM feature tests, 27 JS and 27 Native feature tests; SQL compiles
on all three platforms. JVM docs/board guards pass, with no compile warnings.
The runner performs the serialized whole-build check after landing.
