# Independent semantic definitions and analytics

## Overview
`okay-semantic` describes business meaning independently of agents, storage and
presentation. Applications own definitions; okay validates and interprets them.
The same validated query is executable in memory or via `okay-semantic-sql`.
This is stage 1 of semantic layers, ontology, knowledge graphs and context:
a usable analytical core, not a claim of full OWL or graph support.

## Interface
Package `okay.semantic`: `Catalog`, `Entity`, `Relation`, `Cardinality` describe
business types and relationships. Catalog construction validates identifiers,
uniqueness and endpoints. `Model[A]` binds a dataset to typed Scala row extractors:
`Dimension[A]` returns `Value` (Text, Number, Bool, Null); `Measure[A]` returns
`Option[BigDecimal]`. Each definition carries a description; a model carries
`Origin(source, version)` and a grain description. Extractors are ordinary pure
functions authored by the application, not serialized code.

`Metric` is Sum, Count, Average or Ratio of named measures. Count without a
measure counts rows; count with a measure counts non-null values. Ratio divides
SUM(numerator) by SUM(denominator), not an average of per-row ratios.
`Request(metrics, dimensions, filters)` uses public names. `Model.plan` returns
`Either[Vector[String], Plan[A]]`. Plans cannot be directly constructed by callers;
`explain` states source/version/grain, selected definitions and aggregation rules.
`Plan.run(rows)` returns `Either[Vector[String], Result]` whose groups contain
ordered dimension values and ordered optional numeric metric values, plus origin.
A `Filter` matches dimension equality, including explicit Null; filters combine
by AND and apply before aggregation. Group ordering is first encounter order.

`okay.semantic.sql.Render(plan, Binding(table, dimensionColumns, measureColumns))`
returns a parameterized `Statement` or named errors. Identifiers are restricted
to ASCII SQL identifiers; dotted names and arbitrary expressions are refused.
`Statement.execute(using Sql)` yields `Either[Vector[String], Result] ! Async` through the existing
SQL seam. SQL binding is explicit and separate from the business definition;
the SQL interpreter supports exact Num, I32, I64 cells and refuses F64 totals.
SQL aggregates sums and non-null counts; the same decimal finalizer computes
averages and ratios, avoiding integer division and backend rounding differences.
SQL totals are subject to the database numeric precision.
SQL result group order is backend-defined; parity compares groups by key.

## Behavior
- [ ] Catalog refuses duplicate/empty ids and dangling relation endpoints.
- [ ] Model refuses duplicate names, blank metadata and missing metric measures.
- [ ] Planning refuses unknown or duplicate selections, no metrics, invalid filters.
- [ ] Sum is exact decimal addition; absent values are ignored and all-null is None.
- [ ] Count/average distinguish null from zero; averages divide sum by non-null count.
- [ ] Ratios aggregate both operands first; zero or absent denominator produces None.
- [ ] Filters precede grouping; dimension order is request order, groups stable in memory.
- [ ] Empty ungrouped input returns one group (count zero, other metrics None);
      empty grouped input returns no groups.
- [ ] Invalid extracted dimension kinds return named errors rather than coercion.
- [ ] Result and explanation retain model source/version and business grain.
- [ ] SQL uses bound filter values, IS NULL and explicit column binding;
      refuses invalid/missing identifiers and decodes output without casts.
- [ ] Memory and SQL agree on sums, counts, averages, ratios, filters and nulls.
- [ ] The modules compile on JVM, JS and Native without any agent dependency.

## Design
CrossType.Pure modules. Core depends only on the standard Scala library;
tests depend on okay-test's diagnosed adapter. SQL adapter depends on the core
and okay-sql, not JDBC. Standard engines are optional existing Sql implementations.
No arbitrary expression trees or recursive graph walks in stage 1. Aggregation
uses an iterative fold, bounded state per group/measure and decimal arithmetic.
BigDecimal division uses Scala's decimal context; repeating fractions round.
No currency conversion or unit inference: metric units are descriptive, and
applications must explicitly filter/group currency when a dataset mixes currencies.
The interpreter trusts authored extractors; their exceptions are application
errors. Input rows are already at the declared grain.

## Decisions
- Business meaning is separate from Schema: shape alone cannot derive revenue,
  identity or a join cardinality. No automatic inference from case-class names.
- A validated plan is a value interpreted by both backends. Agent tools may later
  consume it without introducing a dependency into the core.
- Single-model aggregation first. Declaring a relation does not authorize a join.
  Automatic joins require key bindings, temporal semantics and fanout proofs;
  multiplying a fact by child rows must never silently inflate a metric.
- No built-in year/month transformation yet: declare a materialized dimension
  (e.g. recognitionMonth) and bind its column for cross-backend parity.
- Catalog is a lightweight domain model, not formal ontology reasoning.

## Out of scope / subsequent stages
Automatic joins and multi-model queries; derived metric DAGs; time-window DSL;
Schema derivation; persistent fact graph with identity reconciliation, provenance
and traversal; RDF/OWL/SHACL adapters and inference; permission-aware context
assembly and agent tools. These are separate increments, not hidden stubs.

## Results
Pending focused tests and cross-platform compile gates.
