# P0 — Module infrastructure

## Overview
The build grows from one root into a family: the core stays plain
`okay` (no suffix — the user's naming decision), every satellite
carries one. Modules are kept SMALL. The core is dependency-free
forever; ScalaCheck is allowed in test scope only.

## Interface (the layout)
- `okay` — the core, crossProject (JVM / JS / Native): effects,
  streams, chunks, algebra, Stage. Platform-split source sets: the
  blocking side of Async/Channel/Fiber/Scheduler in jvm+native (see
  cross-platform-async.md); everything else shared.
- Satellites (each its own sbt project, published separately):
  okay-lex, okay-parse, okay-codec, okay-cats, okay-zio, okay-kyo,
  okay-fs2, okay-kafka, okay-spark, okay-flink, okay-jdbc, okay-llm,
  okay-cluster. Created on demand, not up front — only the sbt
  infrastructure (shared settings, publish, cross) lands in P0.
- `compare` — internal benchmark module, never published.

## Publishing
- groupId `io.sergiy-shcherbyna` (domain verification settled by
  publication time — deliberately NOT tied to GitHub).
- Scala: latest (3.7+), not LTS — a deliberate decision.
- License: Apache-2.0 (in repo).
- Artifact of the core is literally `"…" %% "okay" % v`.

## Behavior
- [ ] `okay` compiles and tests green on JVM, JS and Native
- [ ] the blocking API is absent from the JS platform at compile time
      (not failing at runtime)
- [ ] CI: test + Jmh/compile on push; publish workflow dry-runs
- [ ] `compare` keeps working against the JVM core unchanged

## Out of scope
- actually publishing (waits for the domain); satellite content
