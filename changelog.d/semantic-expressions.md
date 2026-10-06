## semantic-expressions — portable analytics expressions across data sources

Implemented in 2c12ed7d5, API in 90eec9298, transferable plans in 10cb71aff,
and final precision fix in 9fe2a79f7. Core Constant and shared DecimalMath remove
constant/offset/reciprocal gaps without rounding exact terminating quotients.

Ossie Execution compiles row arithmetic, CASE/null/scalar predicates, FILTER and
DISTINCT aggregates, median/percentiles/statistics and result-grain windows.
Checked composite-key lookups preserve fact grain and diagnose ambiguity, missing
fields, fanout, type/unit mismatches and resource overflow. Functions/Language
facades extend vendor semantics explicitly. Existing interchange and core plans
remain available.

Collections, Source/chunks, Bulk/Tables/files, schema JSON, Arrow columns/IPC,
typed SQL projections and the existing JSON analytics API share these semantics.
Holistic/window plans materialize input within an explicit budget; source SQL is
never executed from document metadata. Worker serialization restores and runs a
typed job without erasure casts.

Verified: 365 affected tests GREEN, JVM/JS/Native, no compile warnings; parser
recursion is bounded to 128 and named in the inventory. Specs:
`specs/semantic-expressions.md`; public contract: `docs/modules/okay-semantic-ossie.md`.
