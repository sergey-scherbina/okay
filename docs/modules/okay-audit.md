# okay-audit

The effect boundary, checked in the bytecode at every build. A program's type
says what it *declares* — `A ! Db + Payments` — but on the JVM nothing stops its
body from opening a socket as well. okay-audit reads the compiled classes (the
constant pool, `ACC_NATIVE`) and checks them against declared layers:

| | |
|---|---|
| `business` modules | may reference none of the APIs that reach past the boundary: network, files, sql, processes, time, randomness, threads, reflection, class loading, native code |
| `handlers` and `runtime` modules | are listed, by provider — the dependency inventory DORA asks for (Regulation (EU) 2022/2554, Art. 8) |
| `sbt audit` | the report over a build, `target/audit/report.{txt,json}`; this repository's own is `okay-audit/dogfood/report.txt` |

Zero dependencies. Every report says in its header what the check chose not to
count (compiler bootstraps, the lazy-val idiom). It is evidence for an auditor
to read, not a compliance claim.

Spec and results: specs/okay-audit.md. What is left: backlog.d/okay-audit/ (a
CLI for Maven and Gradle, a launcher-flags check, hermetic replay) and the first
real findings, backlog.d/okay-data/data-clock-and-random-reach.md.
