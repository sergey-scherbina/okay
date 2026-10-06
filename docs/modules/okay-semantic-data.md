# okay-semantic-data

The shared semantic execution algebra over okay's data interfaces. Depends on
okay-semantic, okay-stream and okay-codec; no mandatory vendor or agent jar.

| Entry point | Input | Execution |
|---|---|---|
| Data.source | Source of typed events | Async, one pass |
| Data.chunks | Source of typed batches | Async, one pass |
| Data.bulk | Any Bulk platform | Its native aggregate/merge |
| Data.table | Tables.Table | Tables effect program |
| Data.file | Bulk.Format | Existing split/row-group file reader |
| Data.json | Typed JSON records via Schema | Decode errors name row |
| CsvData.run | Header-first CSV lines | Explicit scalar bindings |

A validated plan and its definitions stay the same across these entry points.
Data.aggregator supplies the associative partial-state merge Spark/Flink's Bulk
adapters already accept. It merges sums/counts/extremes and unions distinct sets
before computing averages, ratios and derived metrics. No input-row collection
in the Source interpreter; grouped state is proportional to groups, and exact
distinct state is proportional to distinct values. Sources must be finite or
explicitly windowed by the caller. Async failure and cancellation use Source's
existing contract. Exceptions in authored extractors and backend failures retain
those components' behavior.

Given `events`, their semantic `plan` and a `Bulk` backend, the same calculation
runs locally, through Bulk, or through an effect program:

```scala
val locally = plan.run(events)
val distributed = Data.bulk(plan, events)(using backend)
val program = Tables.of(events).flatMap(t => Data.table(plan, t))
val viaTables = Tables.run(backend)(program)
```

The three answers are asserted equal in TestSemanticData. For Spark and Flink,
use their existing Bulk instance; the semantic adapter never names vendor types.
A distributed backend may change group order. Supply an explicit Request.order
when deterministic presentation matters. The tests exercise partitioned Bulk and
serialization locally; they do not deploy a remote cluster.

Data.lookup broadcasts a checked dimension index and maps fact rows on Bulk.
Only dimension rows and cardinality validation state are local. ManyToOne keeps
unmatched facts; OneToOne also checks left key uniqueness. A right key must be
non-null; represent a missing foreign key as None on the fact side. Chained
explicit lookups support multiple systems without a federated SQL engine.

Data.file accepts ParquetFormat, or any existing Bulk.Format. Opening files and
range-reading object storage remain the format/host's responsibility. CSV uses
okay.Csv's line-oriented parser (one record per line); it validates headers,
field arity, exact decimals, booleans and declared nulls. It does not add a new
multiline CSV parser. Typed JSON inherits Schema's field decoding; use textual
exact-number representations when the source must retain precision beyond the
JSON dialect's Double numeric node.

JdkCalendar (JVM) supplies explicit civil-zone hour/day/week/month/quarter/year
buckets, including DST and leap years. Weeks start Monday. Fixed duration buckets
and half-open epoch windows are available in the cross-platform semantic core.
Calendar remains a facade for other platform interpreters. Civil calendar
buckets on SQL require materialized columns or typed row execution.

## Service contract

Wire encodes requests, exact results and model metadata as JSON. Decimal keys,
filter operands and results travel as tagged/string decimals; they never pass
through Double. Api provides an Endpoint with describe, explain and query.
Invalid requests are rejected before the source is evaluated. Api.local uses
rows; Api.source uses Async events; Api.apply accepts another plan executor.

An HTTP host can map describe to a schema route and query/explain to POST routes.
The same contract works in notebooks, applications, BI integration and agent
tools. This module starts no network server and implements no GraphQL protocol;
transport, identity, authorization and source snapshot policy belong to the
host. [Specification](../../specs/semantic-layer.md).
