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
Exact scale factors and terminating constant reciprocals compile to Scale.
Constant-only aggregates, constant offsets and nonterminating constant division
are refused: rounding a reciprocal before multiplying would change the answer.
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
