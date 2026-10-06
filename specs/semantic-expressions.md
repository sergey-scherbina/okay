# Portable semantic expression execution

## Overview
Remove the execution gaps identified after the Ossie importer landed. Keep
interchange metadata lossless and add a compiled portable analytics plan for
expressions beyond decomposable totals. The standalone okay core remains usable
without SQL, agents or external engines.

## Interface
- Core `Calculation.Constant(BigDecimal)` represents constants; existing
  `Bridge.bind` compiles offsets and reciprocals through core arithmetic, including
  exact terminating division at execution with DECIMAL128 fallback for recurring
  decimal results, never a prematurely rounded reciprocal.
- `Execution.bind(document, dataset, bindings, dialect, functions, language)`
  produces an `ExpressionModel[A]`; `.plan(Request)` produces an
  `ExpressionPlan[A]`; `.run(IterableOnce[A], tables)` returns the existing Result.
- `Table.of` converts heterogeneous typed lookup rows to uniform logical Values.
  Joins follow declared composite-key relationships, check right uniqueness and
  preserve unmatched facts. Multiple paths require explicit relationship names.
- `Functions` and `Language` are dependency-free facades. The default portable
  implementation supports the documented syntax. Custom functions and vendor
  language normalizers are selected explicitly, never arbitrary source SQL.
- Plans consume collections, Source/chunks, Bulk/Tables and decoded JSON through
  the same evaluator. SQL/Arrow callers supply decoded typed rows through these
  interfaces. ArrowData adds overloads for expression plans; ExpressionSql reads
  explicitly bound identifier-only projections through a typed decoder. No source
  expression or source declaration is sent to a database. Existing data Api gains
  expression-model overloads for local and Source-backed endpoints, using the
  same JSON request/response protocol. Expression plans and default scalar
  functions are Serializable for distributed Bulk backends; host readers and
  custom functions must capture serializable application state.

## Behavior
- [x] Constants, offsets and both orders of arithmetic execute, including /3,
  exact large coefficients, empty groups, null operands and zero denominators.
- [x] Row expressions inside aggregates support arithmetic, predicates, searched
  and simple CASE, null literals, COALESCE/NULLIF/IF, ROUND and scalar functions.
- [x] Aggregate DISTINCT and FILTER, median, statistical and percentile functions
  execute with correct empty/null semantics and exact decimal arithmetic.
- [x] Window aggregates and ranking/offset functions execute at the requested
  result grain, before having/order/pagination; partition, ordering, peers and
  explicit ROWS frames have documented semantics.
- [x] Qualified cross-dataset fields follow checked composite-key lookup routes;
  duplicate right keys, ambiguous routes and missing tables return diagnoses.
- [x] Unknown identifiers, functions, malformed syntax, mixed row/group levels,
  metric cycles and unavailable dialects fail during planning where possible.
- [x] Registered functions and explicit language adapters extend the same plan;
  common SQL-family dialects can use explicit portable translation.
- [x] Source/chunk/Bulk/Tables/JSON consumption agrees with collection results.
- [x] Existing import/export and Bridge contracts remain tested; new core
  constants export/reimport. All platform checks and affected staged gates pass.

## Design
An arena of expression nodes avoids recursive evaluation. Parsing is explicitly
bounded to 128 nested syntactic constructs (checked before descent), metric
ordering and join routing use worklists. Arithmetic is exact for +,-,* and rounds
only nonterminating division to DECIMAL128. SQL boolean/null rules apply inside expressions.
Holistic and window execution materializes the selected input, with an explicit
row budget; it makes no claim to constant-memory distributed median or windows.
The existing decomposable Plan continues to serve streaming/SQL pushdown workloads.

## Decisions
- Add an expression plan beside the sufficient-statistics plan: a median/window
  cannot be reconstructed from sums and counts. Rejected inventing approximate
  results or silently executing row expressions as aggregate expressions.
- Lookup joins preserve fact grain, with declared keys and uniqueness checks.
  Fanout without an allocation rule is an error, never duplicated revenue.
- Vendor dialects are adapted through Language; automatic equivalence across
  unrelated languages (DAX, MDX, SQL) is not claimed. All source text stays inert.
- The OSSIE_SQL_2026 draft includes more functions than the previous adapter;
  this change removes the reported categories, documents the default function
  catalogue and provides explicit extension points. It does not promise every
  vendor's entire query language, subqueries or DDL.

## Results
- Initial cross-platform pass: 73 feature checks green. Full affected semantic
  closure: 365 tests green, no module warnings (19 compiles), bounded parser
  inventory checked. JVM serialization restored and ran typed data; class loading
  uses the worker loader, as the existing distributed-plan test does.
- Final precision audit: replacing the old exact finite reciprocal Scale with
  unconditional DECIMAL128 division would round terminating results over 34
  digits. Preserve those exactly and use DECIMAL128 only for recurring decimals.
  The large-coefficient /4 regression was observed RED, then GREEN on all three
  platforms after sharing DecimalMath between core and expression execution.
- Final affected gate: 365 tests GREEN (16 semantic/doc projects), no compile
  warnings. Feature coverage: JVM 32, JS 24, Native 24. Exact/recurring division,
  core constants, SQL H2, Arrow IPC, files/Bulk/Tables/JSON, Source/chunks, API,
  serialization, joins, windows, nulls, type/unit errors and budgets are exercised.
- Gate command uses explicit changed semantic module files because build.sbt only
  changes their dependency edges. This closes over all five semantic modules on
  every platform plus the doc project; the whole build belongs to ci-runner.
