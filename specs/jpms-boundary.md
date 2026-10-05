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
  read (for example `java.sql` without `requires java.sql`);
- `scan-only`: the input is unnamed/automatic, the rule reaches `java.base`,
  or the descriptor reads that module.

The section also records package names appearing in more than one scanned
input as split packages, and relevant launcher arguments from an optional
`jvmOptions` file in the JSON manifest: `--illegal-native-access`,
`--add-opens`, `--add-exports` and `--enable-native-access`.

```json
{
  "modules": [{ "name": "orders", "layer": "business", "paths": ["target/classes"] }],
  "jvmOptions": "config/jvm.options"
}
```

`Audit.runtime()` returns the boot-layer module names and requires, JVM input
arguments, and native-access arguments for an application evidence journal.

## Behavior

- [ ] a named descriptor that omits `requires java.sql` marks the SQL rules
  `jvm-enforced`; one that requires it marks them `scan-only`
- [ ] a `java.base` rule such as `java.net.` stays `scan-only` for every
  descriptor
- [ ] two scanned inputs defining the same package are named as a split
  package in text and JSON
- [ ] allowed launcher arguments are recorded from a manifest-relative
  `jvmOptions` file; other options are absent from the evidence
- [ ] `Audit.runtime()` reports the boot layer and current JVM arguments
- [ ] a missing or malformed descriptor/options file is refused by name

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

Pending implementation.
