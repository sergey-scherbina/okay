## kernel - a microkernel: versioned contracts, plugins, ServiceLoader, forbidden module edges

The operator, 2026-09-25: «сделать в окей общие механизмы для микроядерной
архитектуры и потом ее использовать у нас … Сервис лоадер и контракты
версий тоже». specs/kernel.md. The half of composition the compiler cannot
do — parts known only at start, built separately, possibly against another
version of their contract; for parts known at compile time `Module` stays.

- **okay-kernel** (new; JVM/JS/Native; the core only): `Version`/`Range`
  (SemVer, `^1.2` `=1.2.3` `>=1.2 <2` `*`), `Port[A]` (a contract:
  name, version, One/Many, laws), `Plugin` (id, version, kernel range,
  needs, provisions — each stating the contract version it was BUILT
  against, literally), `Kernel.plan` (every problem at once: DuplicateId,
  KernelMismatch, PortConflict, Incompatible, Missing, Unserved, Ambiguous,
  UnknownChoice, Cycle — or a topological order, by id where free),
  `Kernel.start`/`assemble` (a `Resource`: made in order, laws checked,
  released in reverse; a plugin reads only what it declared),
  `Running.providers`.
- **Discover** (JVM): `services` over `META-INF/services/okay.kernel.Plugin`
  and `jars(dir)` over a plugins directory, reading those jars' own service
  files; a provider that cannot load or construct is a `LoadFailed`, the
  rest load.
- **OkayModules** (okay-deploy's sbt plugin): forbidden module edges refused
  at load over the COMPILE closure, test scope exempt. okay's four rules:
  okay-http reaches no MCP/agent/LLM/RAG, okay-ops no okay-docs, the core
  nothing, the kernel only the core. Checked by hand both ways: an
  `okayHttp dependsOn okayMcp` failed the load with its path and reason; the
  same edge `% Test` loaded.
- docs/modules/okay-kernel.md (its example pinned: TestDocExamplesKernel),
  the index row.
- Tests: TestVersion, TestKernel, TestDocExamplesKernel on three platforms,
  TestDiscover on the JVM — 65 results. Gate: `affected master` 7373
  GREEN, no warnings (an earlier run red on clojure-go-block-timeout-under-
  load, third sighting recorded, and on the unpinned example, fixed).

Landed as 47f303251.
