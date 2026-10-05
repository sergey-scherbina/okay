# okay-semantic

Independent business definitions and executable analytics, without an agent or
storage dependency. Applications define `Model[A]` with dimensions, numeric
measures, metrics, source/version and a grain description. `Catalog` describes
entity types and relations with declared cardinalities.

Build definitions through `Model.build` and `Catalog.build`: errors name duplicate
identifiers, blank metadata and missing references. `Model.plan(Request(...))`
validates selections and equality filters. `Plan.explain` records definitions,
source/version and aggregation rules; `Plan.run(rows)` executes in memory.

Supported metrics: sum, row/non-null count, average and ratio of sums. Null
measures are ignored; empty/all-null sums and averages are absent; division by
zero is absent. Filters apply before aggregation. Group keys and metric values
follow request order; memory groups follow first encounter order. Ungrouped
empty data yields one group; grouped empty data yields none.

Money is BigDecimal, and sums do not round. Divisions use DECIMAL128
(34 significant digits, HALF_EVEN), including repeating fractions. Units are descriptive: the application must explicitly
separate currencies and provide rows at the declared grain. Extractors are
application functions; exceptions in them remain application errors.

`okay-semantic-sql` executes the same plan through the Sql driver seam.
Catalog relations describe meaning; they do not enable joins. Formal ontology
reasoning, graph storage, time expressions and multi-model joins are future
stages in [the specification](../../specs/semantic.md).

## Example

With `okay.semantic.*` imported, an application defines and executes revenue:

```scala
case class Order(month: String, eur: BigDecimal)
val result = for
  model <- Model.build[Order]("orders", Origin("accounting", "v1"), "one order",
    Vector(Dimension("month", "Recognition month", Kind.Text, o => Value.Text(o.month))),
    Vector(Measure("amount", "Recognized amount in EUR", o => Some(o.eur))),
    Vector(Metric("revenue", "Recognized revenue", "EUR", Calculation.Sum("amount"))))
  plan <- model.plan(Request(Vector("revenue"), Vector("month")))
  result <- plan.run(Vector(Order("2026-01", BigDecimal("10.50")), Order("2026-01", BigDecimal("2.25"))))
yield result
```

The result is either named validation errors or January revenue of 12.75 EUR,
with source `accounting` and definition version `v1`. This example is compiled
and asserted in TestSemanticExample.
