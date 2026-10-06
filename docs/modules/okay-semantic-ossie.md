# okay-semantic-ossie

Version-pinned Apache Ossie core interchange above okay-semantic and okay-codec.
No agent, database or mandatory third-party runtime dependency. This adapter
reads and writes the core 0.2.0.dev0 schema at revision
891f007945b5666464a45e2c75c1a0a8be9cd7f7. The draft version is mutable upstream;
compatibility here means this revision, not every future document with the same
version string. See the [specification](../../specs/semantic-ossie.md).

## Interchange and validation

Document.readJson, Document.readYaml and Document.fromJson return either named
errors or an immutable Document. Its typed views expose datasets, logical fields,
composite/unique keys, relationships, expression dialects, type/time roles, AI
context and extensions. The original Json tree remains in Document.raw. Export
retains optional-property presence, arbitrary AI members, opaque expressions and
custom_extensions.data strings; it does not normalize away vendor metadata.
Field labels remain available in raw. Formatting and YAML comments are not part
of the semantic round-trip contract.

Validation follows the pinned schema, including enums, required properties and
additionalProperties rules, then checks duplicates and field/key references.
Unknown root properties are refused; vendor data belongs in custom_extensions,
and additional AI context members are allowed by the standard. Composite foreign
keys must have equal arity and reference real fields. If target unique keys are
declared, a relationship must target one of them. Reading a relationship does
not authorize an executable join.

Numeric business literals stay exact in expression strings. Extension data is
an opaque string, so embedded exact numbers survive unchanged. Numeric metadata
that would change value through the codec's parse/print round trip is rejected
with a precision error; use a string for such values. Duplicate JSON/YAML keys,
unknown versions and old semantic_model envelopes are refused. There is no
implicit migration, filesystem lookup or network connection.

## JSON and YAML syntax

Syntax is an optional facade. Its portable default accepts JSON for both entry
points and emits JSON flow syntax, which is also valid YAML 1.2. This default
works on JVM, JavaScript and Native. Ordinary block/flow YAML on JVM uses
SnakeYaml with the optional org.snakeyaml:snakeyaml-engine 2.9 dependency;
import SnakeYaml.given or pass it explicitly as the Syntax instance. No caller
or business model changes. Syntax.byName("snakeyaml") selects the imported instance by name;
SnakeYaml.missing reports an absent jar. Other names are refused.

The optional YAML interpreter accepts quoted strings, block scalars and flow
collections. It refuses aliases, custom tags, non-string mapping keys and
multiple documents. A pre-pass limits nesting to 128 and input to one million
code points before the dependency composes a tree. Its exporter emits block
YAML. Both interpreters are tested to read each other's portable output.
The repository also tests a real upstream Flights model and keeps the source
schema/license/notice beside that fixture.

## Executing a supported subset

Bridge.bind takes a Document, explicit fact dataset and Bindings[A]. Bindings
supply typed logical dimensions/measures keyed by FieldKey(dataset,field), an
Origin with a business version, a grain and units per metric. An accessor reads
the logical field's value; it does not receive an SQL expression to execute.
Missing accessors, incompatible declared scalar types and missing units fail.
Temporal type and is_time are independent metadata; timezone/bucket extraction
is supplied explicitly through the existing semantic Time/Calendar definitions.

Given the document's raw Json, typed bindings and rows, a tested query is:

```scala
val d = Document.fromJson(raw).toOption.get
val model = Bridge.bind(d,"orders",bindings).toOption.get
val request = Request(metricDefinitions.map(_._1),Vector("segment"))
val result = model.plan(request).toOption.get.run(rows).toOption.get
```

TestOssie compares this execution with an authored core plan. Production callers
can compose the Either values instead of unwrapping them. The resulting Model
uses the existing collection, Source, Bulk/Tables, files, Arrow and SQL adapters.
SQL storage bindings must still satisfy those adapters' own contracts.

The execution subset supports SUM, COUNT, AVG, MIN, MAX, COUNT(DISTINCT field),
named metric references, parentheses, unary signs and + - * / over metrics.
Numeric literals remain exact decimal values.
Constants and offsets now compile to core Constant and arithmetic calculations.
Terminating quotients stay exact; recurring decimals round at execution with
DECIMAL128, never through an intermediate reciprocal.
Unquoted references are case-insensitive; quoted references are exact; ambiguity
is an error. ANSI_SQL and OSSIE_SQL_2026 can be selected explicitly. Supporting
this subset does not claim complete OSSIE_SQL_2026 language conformance.

An optional metric-name selection compiles its dependency closure, so an
unsupported unrelated metric can still be retained in the document. Unsupported
functions, dialects and cross-dataset operands produce execution errors rather
than partial results. This bridge executes fact-only expressions; application
joins remain the core's explicit checked Lookup workflow. No expression text is
forwarded to a database and no join is inferred from a relationship declaration.

## Exporting core definitions

Export.model requires explicit storage-column mappings. It exports supported
core calculations to ANSI_SQL and stores units, source/version, grain and time
transformations in OKAY extensions. Export.business validates and recovers the
business metadata; its Business.bindings constructs the typed binding container.
Export.temporal recovers fixed/civil bucket parameters. Scala extractor functions
are not serialized: callers provide those typed readers again.

Missing columns, conflicting field types and model relationships without key
bindings are refused; export never returns a silently incomplete document.
For already-imported models, Document.json/yaml preserve all relationships and
metadata directly. Ontology rules, mappings and inference belong to a separate
adapter; this module implements the Ossie core document contract.

## Portable expression plans

Use `Execution.bind` with the same `Document` and typed `Bindings` when a metric
needs row expressions, holistic aggregates or windows. Its model has the same
`plan(Request)` and `run(rows)` shape, and returns the existing semantic `Result`.
`Bridge.bind` remains the decomposable core-plan path. It now supports literal
constants, offsets, constant numerators and division such as revenue / 3, including
constant-only metrics. Terminating division happens exactly at execution; recurring decimals use
DECIMAL128 precision;
no reciprocal is rounded before multiplication. Zero denominators produce null.

The expression plan supports:

| Category | Default implementation |
|---|---|
| Arithmetic | Exact decimal +, -, *, terminating /, %, DECIMAL128 recurring /, unary signs |
| Predicates | Comparisons, AND/OR/NOT, SQL three-valued null logic, IS NULL/IS NOT NULL, IN, BETWEEN, LIKE/ILIKE |
| Conditional | Searched and simple CASE, IF/IFF, COALESCE, NULLIF, IFNULL/NVL, NVL2, ZEROIFNULL, NULLIFZERO |
| Aggregates | SUM, COUNT, AVG, MIN/MAX, DISTINCT operands, FILTER, MEDIAN, PERCENTILE_CONT/DISC WITHIN GROUP, sample/population variance and standard deviation |
| Scalars | ROUND, TRUNC/TRUNCATE, ABS, FLOOR, CEIL/CEILING, SIGN, MOD, POWER, SQRT, EXP, LN, LOG/LOG10, GREATEST/LEAST |
| Strings | CONCAT and concatenation operator, LENGTH, LOWER/UPPER, TRIM/LTRIM/RTRIM, LEFT/RIGHT, SUBSTRING, REPLACE, SPLIT_PART, CONTAINS, STARTSWITH/ENDSWITH, CHARINDEX |
| Windows | Aggregate OVER, ROW_NUMBER, RANK, DENSE_RANK, LAG/LEAD, FIRST_VALUE/LAST_VALUE, NTILE; PARTITION BY, ORDER BY, explicit ROWS frames |

Aggregates accept scalar row expressions, for example SUM(price * quantity),
SUM(CASE WHEN status = 'completed' THEN amount ELSE 0 END), and
COUNT(DISTINCT COALESCE(segment, 'unknown')). Scalar functions also work on
metric results, such as ROUND(revenue / 3, 2). Median and continuous percentiles
interpolate exact decimal observations. Variance uses DECIMAL128 division;
standard deviation and transcendental math use floating-point approximations.

Windows operate on the requested result grain. A grouped total can use
SUM(SUM(amount)) OVER (); a running total can use SUM(revenue) OVER (ORDER BY
segment ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW). Window results are
computed before having, final ordering and pagination. Without an explicit frame,
a window with ORDER BY includes ordering peers through the current value (the
SQL RANGE default); without ORDER BY it includes the complete partition. Nulls
sort last by default; NULLS FIRST/LAST overrides this. Ranking peers share rank;
ROW_NUMBER follows stable input order for ties. To calculate raw-row windows such
as SUM(amount) OVER (), include the declared fact primary key in the request
dimensions. This makes the row grain explicit and checks for duplicate/null keys.

Other row fields cannot be combined with group metrics unless selected as grouping
dimensions. Nested aggregates need a window stage; nested windows need another
query stage. Scalar metric results are numeric or null, matching core Result.
Logical field bindings read values already interpreted by the application; source
field-expression strings remain metadata rather than being executed as SQL.

## Relationships and other dialects

Expression plans resolve qualified foreign fields through declared relationships.
Supply right rows with `Table.of(dataset, rows, typedReaders)` and pass the resulting
tables by dataset name to `plan.run`. Composite keys are matched as tuples. Missing
matches produce null foreign values; duplicate right keys fail rather than
multiplying fact measures. Required right fields must be present; an explicitly
null field is different from a missing binding. Multiple paths require explicit
relationship IDs through `model.plan(request, via = ...)`. Fanout requires an
application allocation rule and cannot silently duplicate facts.

`Functions` supplies `accepts(name, arity)` and `call(name, values)`. Extend the pure
scalar catalogue with `Functions.orElse(custom, Functions.portable)`; the same
plan uses the extension on every data source. `Language.normalize` converts a
chosen vendor expression into portable syntax. Default execution accepts ANSI_SQL
and OSSIE_SQL_2026. Explicit `Language.sqlFamily` accepts the common expression
subset from BIGQUERY, SNOWFLAKE and DATABRICKS, including backtick identifiers.
Vendor-specific functions or syntax need a registered function or normalizer;
whole DAX/MDX or vendor SQL equivalence is not implied by a dialect label.
Unsupported function names and malformed expressions produce planning diagnoses.

## Data sources and budgets

Expression plans expose `run`, `source`, `chunks`, `bulk`, `table` and `json`.
A Bulk file reader feeds the same `bulk` method, so Parquet and other registered
file formats use the same semantics. ArrowData.table/ipc accept expression plans
and decode with the caller's Schema. ExpressionSql executes an identifier-only
physical table/column projection with an explicitly typed row decoder, then runs
the portable plan. Data Api.local/source also accept expression models and expose
the same JSON query protocol for HTTP or tool transports. This fallback supports
median, CASE and windows regardless of
the database's native function catalogue. It does not push these expressions down.
The existing Render path still pushes decomposable core sufficient statistics.

Holistic aggregates and windows materialize rows. `model.plan` has a default
`maxRows` of 1,000,000; collection and transport paths return a diagnosed error
on overflow. Bulk merges retain at most maxRows + 1 observations; no partition
is silently dropped. Plans and default functions are serializable for distributed
Bulk workers; host readers and custom functions must capture serializable state.
Lookup tables have the same budget; Table.of accepts its
own explicit limit. The parser rejects nesting beyond 128 descent levels and
limits decimal rounding scale to 10,000. Streaming transports must complete;
these are terminal analytics plans rather than continuously updating views.
