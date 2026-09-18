- core-modularise-data — stage 3 of specs/core-modules.md. `Sketch`,
  `Uid` and `Hlc` (683 lines) become `okay-data`; `Aggregator` stays in
  the core, which is the law's fourth confirmation and its clearest:
  ten modules are typed on the interface and ZERO on the implementation
  beside it. Two `dependsOn` edges, okay-crdt and okay-security, both
  named by the survey before the compiler saw them.
