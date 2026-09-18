- core-modularise-workflow — stage 2 of specs/core-modules.md. `Wf`,
  `Proc` and `ProcMacro` (2 229 lines) become `okay-workflow`; the core
  keeps `Replayable`, the marker `Delim` is typed on. The core named
  none of the three in code, on any platform directory, so this is the
  cheap stage: one `dependsOn` in the whole repository.
