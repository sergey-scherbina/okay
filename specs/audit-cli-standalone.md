# audit-cli-standalone — run the bytecode boundary audit without SBT

## Overview

`okay-audit` must run in Maven and Gradle builds as an ordinary JVM main
class. A JSON manifest describes the already-built class directories and jars;
the existing scanner and report remain the single implementation of the
boundary rule.

## Interface

```text
java -cp <okay-audit runtime classpath> okay.audit.Main \
  --manifest audit.json --report build/audit
```

`audit.json` is a UTF-8 object:

```json
{
  "modules": [{
    "name": "orders",
    "layer": "business",
    "paths": ["target/classes", "lib/orders-dependency.jar"],
    "packages": { "com.example.orders.adapter": "handlers" }
  }],
  "allows": [{
    "module": "orders",
    "api": "com.example.legacy.",
    "owner": "architecture",
    "reason": "retired with migration ORD-42"
  }]
}
```

`layer` is exactly `business`, `handlers`, `runtime`, or `untracked`.
`paths` is required and non-empty; paths resolve relative to the manifest.
`packages` and `allows` default to empty. The command writes `report.txt`
and `report.json` below `--report`, returns 0 for a pass, 1 for findings and
2 for malformed input or an invalid allow. The TSV invocation remains
supported for the SBT task.

## Behavior

- [x] a JSON manifest whose business module contains the socket fixture exits
  with a finding and writes both reports
- [x] a handler package within a business module is inventoried, while the
  rest of the module remains subject to the business rule
- [x] manifest paths are relative to the manifest rather than the process
  working directory
- [x] an unknown layer, duplicate module name, missing/non-string path or
  malformed JSON is refused by name before a scan
- [x] documented Maven/Gradle invocations call the JVM main class and need no
  SBT runtime classes or SBT settings

## Out of scope

- Maven and Gradle plugins; each build tool invokes the same command itself.
- Launcher/JVM flag evidence and JPMS descriptors (stage 3).
- A general JSON library or a dependency of `okay-audit`.

## Design

The module keeps its zero-dependency boundary by parsing the small JSON value
language it accepts into private values, then validates and translates those
values to `Boundary` and the existing `Audit.run` input. The report writer
and scanner are unchanged. A single `run` method returns an exit code so unit
tests exercise the CLI contract without calling `System.exit`.

## Decisions

- **One manifest instead of layer flags** — package-prefix layers and named
  allows are structured data; flags would be ambiguous and hard to keep in
  a Maven or Gradle file.
- **Strict schema** — an unknown layer must not silently become `untracked`,
  because a typo would make an audit look less strict than the build intends.
- **Small local parser** — the command must remain usable with only its own
  runtime classpath; adding a JSON library would violate that deployment
  property.

## Results

Implemented 2026-10-05. `Manifest` is a zero-dependency strict JSON parser
and schema validator; `Main.run` makes the command's 0/1/2 result testable
without terminating the test JVM. `TestAudit` covers the socket finding,
package handler inventory, manifest-relative paths, and malformed input.
