- [ ] r-supervised-replay — okay-r's `RSubprocess` respawns a fresh R after
      a timeout, but an R program-as-data run's continuations die with the
      old process. `ForeignWorker.supervised` (wire-read-deadline) recovers
      them by replaying each continuation's answer path, and the same
      mechanism would serve `REval.Program`/`Continue`. Wait for an R user
      whose program outlives a timeout: today no R program in the repo runs
      that long. Found by wire-read-deadline (2026-09-24).
