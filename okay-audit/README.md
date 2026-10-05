# okay-audit — the effect boundary, checked at every build

The type `A ! Db + Payments` says what a program *declares*. On the JVM
nothing stops its body from opening a socket as well. This module checks
the bytecode: a `business` module may reference none of the APIs that reach
past the boundary (network, files, sql, processes, time, randomness,
threads, reflection, class loading, native code); `handlers` and `runtime`
modules are listed, by provider — and that listing is the dependency
inventory DORA asks for (Regulation (EU) 2022/2554, Art. 8).

- Spec and results: [`specs/okay-audit.md`](../specs/okay-audit.md)
- The self-report over this repository's own build:
  [`dogfood/report.txt`](dogfood/report.txt) — regenerate with `sbt audit`
  (`target/audit/report.{txt,json}`)
- What is left: [`backlog.d/okay-audit/`](../backlog.d/okay-audit/okay-audit.md)
  (CLI for Maven/Gradle, launcher-flags check, hermetic replay) and the first
  real findings, [`backlog.d/okay-data/`](../backlog.d/okay-data/data-clock-and-random-reach.md)

Zero dependencies. The report says what the check chose not to count
(compiler bootstraps, the lazy-val idiom) in its header, every run. It is
evidence for an auditor to read, not a compliance claim.
