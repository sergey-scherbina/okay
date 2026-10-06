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
+ - * /, parentheses, exact numeric constants and references to named metrics.
Use an iterative tokenizer/shunting-yard worklist, no stack recursion. Preserve
unsupported dialect expressions; executing them returns named capability errors.
ANSI_SQL and OSSIE_SQL_2026 are selectable explicitly; do not choose the first
vendor dialect silently. Unquoted identifiers resolve case-insensitively;
quoted identifiers resolve exactly; ambiguity is an error. Unsupported fields,
aggregations and functions are refused. Cross-dataset operands require explicit
checked enrichment outside this bridge; no implicit joins or metric fanout.
The application can choose a single-dataset metric subset from a larger document.
Export core model declarations through explicit logical field mappings; keep
units/grain/source/version/time/cardinality details in OKAY extensions. Reimport
preserves those extensions. Unsupported model exports return errors, not a
partial document. SQL expression text is never forwarded directly to a driver.

## Behavior
- [ ] JSON/YAML interchange retains metadata, omissions and opaque expressions.
- [ ] Pinned schema shape/enums and semantic references reject damaged documents.
- [ ] Composite keys, unknown AI members, custom extensions and time-role defaults work.
- [ ] Unsupported versions, legacy envelopes, duplicate keys and unsafe numbers fail.
- [ ] Supported selected expressions produce the same metrics as authored core plans.
- [ ] Unsupported dialects/functions/cross-dataset references fail only execution.
- [ ] Deep expression/dependency inputs are stack safe and errors name their location.
- [ ] Core export and reimport retain analytical meaning and OKAY metadata.
- [ ] Optional YAML adapter and portable implementation interoperate; missing jar is named.
- [ ] Focused JVM/JS/Native tests and doc guards pass with no module warnings.

## Results
Pending implementation.
