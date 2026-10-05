# jpms-boundary — JVM module evidence beside the bytecode boundary scan

## Overview

The bytecode scanner remains the authority for reaches in `java.base`; JPMS
adds a second, JVM-enforced witness for APIs outside it. Stage A records that
evidence from the same inputs an audit already scans, without requiring the
audited application to become a named module.

## Interface

`Report` gains a deterministic `jpms` section in text and JSON. For every
audited input that has `module-info.class`, it records the module name,
requires and the enforcement state of every default rule:

- `jvm-enforced`: the rule is wholly in a JPMS module the descriptor does not
  read: a descriptor requiring only `java.base` proves this. Descriptors with
  other dependencies remain `scan-only`, since those may grant transitive
  readability and this report does not resolve a deployment module graph;
- `scan-only`: the input is unnamed/automatic, the rule reaches `java.base`,
  or the descriptor reads that module.

The section also records package names appearing in more than one scanned
input as split packages, and relevant launcher arguments from an optional
`jvmOptions` file in the JSON manifest: `--illegal-native-access`,
`--add-opens`, `--add-exports`, `--add-reads` and `--enable-native-access`.
The enforcement label is conditional on deployment as a named module, not
on the classpath. Any recorded `--add-reads` conservatively makes all labels
`scan-only`; this report does not resolve launcher overrides.

```json
{
  "modules": [{ "name": "orders", "layer": "business", "paths": ["target/classes"] }],
  "jvmOptions": "config/jvm.options"
}
```

`Audit.runtime()` returns the boot-layer module names and requires, JVM input
arguments, and native-access arguments for an application evidence journal.

## Behavior

- [x] a java.base-only named descriptor that omits `requires java.sql` marks the SQL rules
  `jvm-enforced`; one that requires it marks them `scan-only`
- [x] a `java.base` rule such as `java.net.` stays `scan-only` for every
  descriptor
- [x] two scanned inputs defining the same package are named as a split
  package in text and JSON
- [x] allowed launcher arguments are recorded from a manifest-relative
  `jvmOptions` file; other options are absent from the evidence
- [x] `Audit.runtime()` reports the boot layer and current JVM arguments
- [x] a malformed descriptor or missing/malformed options file is refused by name;
  an absent descriptor denotes an unnamed input

## Out of scope

- Generating `module-info.java`, changing the module path, or failing a build
  because a module is unnamed.
- Enforcing any `java.base` reach through JPMS; those rules remain scanner-only.
- The named okay-watch deployable and jlink image (stage B), and one package
  per okay module (stage C in okay2).

## Design

`ModuleDescriptor.read` reads an input's `module-info.class` directly from a
directory or jar. Package collection walks the same class entries as the
scanner. The mapping from audit rule to JPMS module is deliberately small and
explicit; a rule that mixes or names `java.base` is never overstated as JVM
enforced.

## Decisions

- **Evidence, not a new gate** — an unnamed classpath application remains
  auditable today; the scanner catches its direct reaches while the report
  tells a deployer exactly what JPMS could enforce after migration.
- **Only named launcher flags** — the report preserves security-relevant
  configuration without becoming a dump of unrelated JVM tuning.

## Results

Implemented Stage A. The focused TestAudit suite covers directory and JAR
descriptors, SQL readability, java.base and sun rules, launcher overrides,
split-package deduplication, manifest options, runtime evidence and refusal.
Stages B and C remain deployment work, not a claim that JPMS now constrains
the existing classpath application.
