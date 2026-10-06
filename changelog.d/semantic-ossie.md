## Apache Ossie interchange and typed analytics

Implementation: 4fb867206; syntax configuration/provenance: c25ea4905.
Spec: specs/semantic-ossie.md. New okay-semantic-ossie imports/exports the
revision-pinned 0.2.0.dev0 core document with schema and reference validation,
composite/unique keys, opaque dialect expressions, context and extensions.

Typed bindings compile a supported metric subset to the existing semantic
engine; unsupported functions, dialects, types and cross-dataset operands are
refused rather than executed as raw SQL. Core exports retain business/time
metadata in OKAY extensions. Syntax is portable JSON/YAML flow by default;
optional JVM SnakeYAML supplies block YAML and refuses by name without its jar.

Validation: 15 JVM + 10 JS + 10 Native feature tests and 22 doc/board checks,
all green, no module warnings. Includes real upstream YAML, actual missing-jar
JVM, exact-number boundaries and deep worklist tests. Full build belongs to
the serialized post-landing runner.
