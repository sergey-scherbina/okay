- [ ] foreign-one-program — stage 2b of specs/foreign-one.md (split from
      foreign-one-protocol, 2026-09-26): ONE PROGRAM PROTOCOL. The direct
      style (`start`, `ask`, `resume`) and programs as data (`program`,
      `perform`, `continue`) carry the same three messages; a direct
      function IS a program whose continuations are its parked stack. So
      `program{run, fn, args, callbacks}` starts either kind, the far side
      answers `{"done": v}` or `{"perform": name, "args", "k", "once"}`
      (`once`: the continuation is a parked stack, continued at most once,
      a second continue refused by name), and `continue{run, k, answer |
      condition}` resumes either. `start`/`resume`/`ask` and the host's
      `PyStep`, `ForeignEval.Start`/`Resume` go; the supervisor reads
      `once` (WorkerDied as data) instead of inferring it from the op
      (replay otherwise); the pool routes both by run. Go and Rust gain a
      plain `call` (a direct function with no callbacks). All six far sides
      (shim 7, R shim 10). Gate: every Live suite of okay-py, okay-r, okay-rust
      FFM/wasm, okay-foreign-cluster, okay-foreign-workflow green with only
      constructors renamed; `WireConformance`, `CrashConformance` on every row.
