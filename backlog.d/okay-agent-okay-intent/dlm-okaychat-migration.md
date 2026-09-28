- [ ] dlm-okaychat-migration — stage 2 of specs/dlm.md: Okay!Chat
      switches to `okay-dlm` and deletes its copies — its `Router`,
      `Fuzzy`, `Acts`/`Answering`/`Presence`/`Frames`, `Decision`,
      `TurnState`, `Standing`, `Memory`, `Checkpoint`, `CompiledIntents`,
      `Lang.Detector`/`Profiles`, `Phrasing`, `ModelChain` and
      `Calibration`'s arithmetic (about 3 300 lines), keeping its corpus,
      its `Catalog`, its `Phrases`, its readers and its `Executor`.
      Its own `recall` keeps the `which-side` name its log was written
      under; its `Turn` record maps to `Decision.Record` at the journal.
      Criterion: the service's golden transcript byte-identical, the
      submodule pin moved, `Executor.replay` unchanged over the live log.
      Filed by dlm-module (2026-09-28); the operator asked for the two
      stages separately.
