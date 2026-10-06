# Semantic layer across data engines

## Overview
Complete the analytical semantic-layer contract over the existing okay data
seams, extending specs/semantic.md without an agent dependency. Business
meaning, storage reading and serving are separate. A finite query can consume
rows, asynchronous events, batches, files, Arrow or distributed Bulk values.
The same aggregation algebra computes every backend's result.

## Interface and design
Core `okay-semantic` remains standard-library-only and cross-platform.
- Extend Calculation with Minimum/Maximum, Distinct(dimension), derived Add,
  Subtract, Multiply, Divide and Scale of named metrics. Validate references and
  cycles iteratively; evaluate dependencies before dependents, after aggregation.
  Add/Subtract require equal declared units. Units of products/quotients are
  explicitly declared; no automatic FX or dimensional-analysis claims.
- Extend Filter with Comparison: Eq/Ne/Lt/Le/Gt/Ge/In/IsNull/IsNotNull. Value kinds
  must match. Null is only equal through Eq(Null), Ne(Null), IsNull/IsNotNull;
  ordered comparisons with null are false. Filters AND together; In is OR within
  a filter. Text ordering is Unicode string ordering in memory (database collation
  remains a SQL binding contract). Bool supports equality/set membership only.
- Request adds Having(metric,comparison,value), Order(field,descending,nullsFirst),
  offset and limit. Having runs after metric finalization, order then pagination.
  Output metric/dimension names cannot overlap when ordering would be ambiguous.
- Plan.accumulator consumes rows once, keeps sufficient statistics and exact
  distinct sets. Snapshot is a serializable Partial with Signature. Merge refuses
  other models, versions, definitions or requests. Merge sums/counts/extremes and
  unions distinct sets; never merges already-finalized averages or ratios.
  Global empty aggregate semantics remain unchanged. Input is finite unless the
  caller provides a finite event window; no implicit infinite-stream termination.
- FixedBucket(widthMicros,anchorMicros) computes exact half-open epoch buckets
  using BigInt floor division, including pre-epoch times and overflow boundaries.
  Time.dimension extracts these as numeric microsecond bucket keys; Time.window
  builds half-open Gte/Lt filters. Calendar is a facade for civil calendar
  bucketing; JdkCalendar in the data module's JVM source implements hour/day/week/
  month/quarter/year in an explicit zone using java.time, including DST. Fixed
  UTC buckets work on all platforms; civil zones are supplied through Calendar.
- Lookup[A,B,K] enriches fact rows with Option[B] and maps an existing fact Model
  plus named right-side dimensions to Model[(A,Option[B])]. Relation endpoints
  must match model IDs; only ManyToOne/OneToOne are accepted. Right key duplicates
  fail; OneToOne also checks non-null left keys. Missing/null keys are retained
  as absent dimensions. No fact multiplication. Chaining typed lookups supports federation and snowflakes. Catalog.route finds
  a unique fanout-safe path between entities; multiple paths, relevant cycles or
  fanout-only routes are refused. Explicit relation IDs can disambiguate a route;
  typed key bindings remain supplied by the application.
  Origin records both source versions, and grain is the fact's grain. The enriched
  model carries its relation declarations; SQL bindings must name those same
  relations/cardinalities, so storage cannot weaken the business declaration.

`okay-semantic-sql` extends SQL statistics with min/max and count-distinct,
comparison filters, fixed-bucket dimension metadata and derived finalization.
Time.dimension carries its transformation in the definition, so SQL derives it
without a second declaration. Civil calendar dimensions require an explicitly
materialized column when SQL cannot push down that calendar; otherwise rendering
refuses by name. They can always run on typed SQL rows through Data.source.
Having/order/page use the shared finalizer. Explicit lookup join bindings quote
qualified columns through structured references; preflight duplicate-key queries
run in the caller's transaction and reject cardinality violations before facts
are joined. The caller must use repeatable-read or a stable snapshot if data can
change between preflight and aggregation. Unsafe/missing bindings are refused.

`okay-semantic-data` depends on core, okay-stream and okay-codec; no mandatory
external jars. Its Data interpreter provides:
- chunk/event Source execution over Async, accumulating without collecting rows;
- Aggregator bridge for Bulk[D] (local/Spark/Flink) and Tables effect programs;
- file execution through Bulk.Format[A] (including existing ParquetFormat);
- typed JSON rows through Schema, with row-indexed decode errors;
- explicit named CSV bindings with kind/null validation, using existing Csv;
- Wire request/result/schema JSON contracts. Decimals travel as strings, never
  JNum/Double. Api exposes describe/query/explain over a typed endpoint; HTTP,
  notebooks, BI or agent transports can call it without a new query language.
  Request validation precedes source evaluation. Endpoint selection and identity
  are host responsibilities; no implicit network server or authentication policy.

`okay-semantic-arrow` adds typed Arrow Table/IPC execution through existing
Rows, ArrowCodec and Compression facades. File readers retain their existing
resource and platform contracts. Parquet needs no duplicate reader: its
Bulk.Format supplies rows to the same aggregate. A semantic query is read-only.

## Behavior
- [x] Existing semantic-core scenarios and SQL parity remain green.
- [x] Derived dependencies, missing references, cycles and unit mistakes are checked.
- [x] Deep metric dependency chains do not recurse on the call stack.
- [x] Minimum, maximum and distinct match null/empty/group semantics on memory and SQL.
- [x] All comparisons, having, ordering/null placement and pagination are validated.
- [x] Partition merge equals one-pass execution for sums, averages, ratios and distinct.
- [x] Incompatible partials cannot merge; snapshots do not alias mutable accumulators.
- [x] Fixed time buckets handle boundaries, negative epochs and large timestamps.
- [x] Civil zone buckets cover month/quarter/year, leap days and DST transitions.
- [x] Lookup refuses fanout and duplicate keys, preserves unmatched facts and provenance.
- [x] Lookup SQL preflights cardinality and agrees with in-memory enriched facts.
- [x] Source handles empty/chunked/asynchronous inputs without retaining source rows.
- [x] Bulk and Tables use the mergeable aggregate; partitioned execution agrees locally.
- [x] CSV and typed JSON read real data with named parse/decode errors.
- [x] Arrow table/IPC and Parquet Format data yield the same metrics as typed rows.
- [x] JSON API preserves exact decimals and validates requests before invoking sources.
- [x] Core/data/Arrow adapters compile cross-platform; diagnosed focused tests pass.

## Decisions and boundaries
The word "all" means a complete useful analytical execution contract, not an
unbounded commitment to every database and vendor protocol. Reuse existing
facades: no Spark/Flink, CSV, Parquet, Arrow or HTTP framework reimplementation.
Distributed tests exercise the Bulk contract with partitioned instances; remote
cluster deployment is the host's job. General many-to-many joins require an
explicit allocation/deduplication rule and are refused until such a rule is
provided; guesses must never inflate a metric. OWL reasoning and persistent
knowledge-graph storage belong to knowledge-facts, outside the semantic layer.
Exact distinct is memory proportional to distinct values; grouped aggregation
is memory proportional to groups. No invisible group eviction or approximate
answers. No automatic authorization policy, cache staleness policy, FX conversion,
or arbitrary user-code serialization. Definitions must change their version
when business meaning changes. SQL database numeric precision and collation
remain explicit binding limitations.

## Results
Implemented in okay-semantic, okay-semantic-sql, okay-semantic-data and
okay-semantic-arrow. Focused JVM gate: 41 feature tests plus 19 documentation/
board checks, all green. JavaScript and Native: 27 feature tests each, plus
SQL adapter compilation, all green with no compile warnings. Recscan reports
zero recursive definitions in the four changed modules.

TestSemanticLayer pins a 20,000-metric dependency chain, exact partial merges,
null/filter laws, negative/overflow time boundaries, unique/ambiguous/cyclic
routes and typed snowflake lookups. TestSemanticSql compares H2 with the memory
interpreter, including fixed buckets and duplicate-key preflights.
TestSemanticData and TestSemanticSources cover partitioned Bulk, Tables,
stream failure/empty/chunked input, CSV/JSON errors, request validation before
source reads, civil DST/leap-year buckets and Java serialization.
TestSemanticArrow uses real IPC and multi-row-group Parquet bytes.

Root build additions register only the new projects; scoped gates cover the
four semantic modules and their dependent closure rather than starting the
whole family on the shared machine. The serialized post-landing CI runner
owns the whole-build check. Remote Spark/Flink clusters and external HTTP hosts
are adapter consumers, not deployments claimed by these tests.

The dependent-closure plan names exactly the twelve platform projects of these
four modules and no other dependents. A final link check exposed an existing
okay-audit README pointer to a removed backlog item; it now points at the
backlog directory. No audit code changed.
