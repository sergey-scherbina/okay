# Apache Ossie interchange

## Overview
Add optional okay-semantic-ossie above okay-semantic and okay-codec. Pin the
Apache Ossie core 0.2.0.dev0 schema to revision
891f007945b5666464a45e2c75c1a0a8be9cd7f7 (2026-10-06); do not follow mutable
main at runtime. Import/export and execution capability are separate contracts.

## Document and validation
Document exposes typed datasets, fields, composite/unique keys, relationships,
metrics, multi-dialect expressions, time-role/type metadata, AI context and
vendor extensions. Retain the source Json tree so missing vs empty properties,
field ordering, expressions and arbitrary AI context members survive export.
Schema validation covers required fields, types, enums and additionalProperties;
semantic validation additionally catches duplicate identifiers, missing dataset/
field/key references and inconsistent composite relationship arity. Unknown
versions are refused. Legacy semantic_model envelopes are refused explicitly.
No arbitrary SQL, network source lookup or automatic old-version migration.

## Syntax
Syntax is a facade: the portable default uses existing Json and accepts JSON
syntax for JSON and YAML (JSON is valid YAML 1.2). It emits that same portable
syntax. Optional JVM SnakeYaml adapter supports ordinary block/flow YAML through
SnakeYAML Engine with safe composition, duplicate-key checks and bounded parser
nesting. No mandatory third-party jar; a missing adapter is refused by name.
Syntax.byName selects the imported Syntax instance, so configuration callers
change only the import when choosing the optional implementation.
All inputs containing numeric metadata that cannot survive the codec's Double
projection exactly are refused, never silently rounded. Exact business numbers
in expression strings and custom_extensions.data are preserved. The adapter is
an interchange implementation with an execution subset, not a claim of complete
OSSIE_SQL_2026 language conformance or ontology conformance.

## Execution bridge
Bind explicit typed dimensions/measures to logical dataset.field identifiers,
with an explicit fact dataset, source/version, grain and units per metric.
No serialized Scala extractors or inferred currency/timezone. Compile selected
metric dependency closures: SUM/COUNT/AVG/MIN/MAX, COUNT(DISTINCT field), arithmetic
+ - * /, parentheses, exact numeric scale factors and references to named metrics.
Constant-only metrics and additive offsets are refused because the core has no
constant-aggregate calculation; no synthetic data rows are invented.
Division by a constant compiles to Scale only when its reciprocal terminates
exactly; otherwise execution is refused rather than rounding before the divide.
Use an iterative tokenizer/shunting-yard worklist, no stack recursion. Preserve
unsupported dialect expressions; executing them returns named capability errors.
ANSI_SQL and OSSIE_SQL_2026 are selectable explicitly; do not choose the first
vendor dialect silently. Unquoted identifiers resolve case-insensitively;
quoted identifiers resolve exactly; ambiguity is an error. Unsupported fields,
aggregations and functions are refused. This bridge refuses cross-dataset operands; checked Lookup enrichment remains
a separate application execution workflow. No implicit joins or metric fanout.
The application can choose a single-dataset metric subset from a larger document.
Export core model declarations through explicit logical field mappings; keep
units/grain/source/version/time/cardinality details in OKAY extensions. Reimport
preserves those extensions. Export.business reads origin/grain/units and
Export.temporal reads bucket/calendar metadata with named validation errors.
Unsupported model exports return errors, not a
partial document. SQL expression text is never forwarded directly to a driver.

## Behavior
- [x] JSON/YAML interchange retains metadata, omissions and opaque expressions.
- [x] Pinned schema shape/enums and semantic references reject damaged documents.
- [x] Composite keys, unknown AI members, custom extensions and time-role defaults work.
- [x] Unsupported versions, legacy envelopes, duplicate keys and unsafe numbers fail.
- [x] Supported selected expressions produce the same metrics as authored core plans.
- [x] Unsupported dialects/functions/cross-dataset references fail only execution.
- [x] Deep expression/dependency inputs are stack safe and errors name their location.
- [x] Core export and reimport retain analytical meaning and OKAY metadata.
- [x] Optional YAML adapter and portable implementation interoperate; missing jar is named.
- [x] Focused JVM/JS/Native tests and doc guards pass with no module warnings.

## Results
Implemented in okay-semantic-ossie. Final focused gate passed 15 JVM tests,
10 JavaScript tests, 10 Native tests and 22 documentation/board checks (57
total), with no module compile warnings. Recscan names zero recursive methods
in the new module. The dependent-closure plan contains only its three platform
projects and no dependents; the new root build entries register this module.
The serialized post-landing CI runner owns the whole-build check.

Tests cover real upstream Flights YAML, a byte-identical schema snapshot,
metadata/extension round trips, composite keys, scalar and metric type errors,
explicit execution refusals, core-plan parity and business/time recovery.
20,000 nested parentheses, 20,000 levels of arbitrary AI context and 2,000
metric dependencies pass on all three platforms. An isolated child JVM without
SnakeYAML proves the optional adapter loads and refuses by name. Block/flow
YAML, quoted/block strings, parser depth, aliases, tags and multiple documents
are covered on JVM; portable JSON/YAML flow interchange is shared.

Adversarial tests found and fixed chained schema $ref resolution, the JVM-only
Locale dependency, malformed explicitly tagged YAML scalars and incompatible
numeric metric declarations. Nonterminating constant division was intentionally
refused: converting it to an approximate Scale changed the analytical result.
Optional YAML syntax and source attribution are packaged behind the facade;
LICENSE/NOTICE/PROVENANCE ship in META-INF/ossie.
