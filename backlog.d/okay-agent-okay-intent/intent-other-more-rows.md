- [ ] intent-other-more-rows — SHARPENED 2026-09-07 by
      intent-offline-other, which turned this from "more data would
      presumably help" into a measured blocker: an out-of-domain
      detector over the same rows already RANKS at AUC 0.843, and
      every decision rule built on it is starved by 15 training rows
      (by argmax it cannot fire at all; balanced it destroys the
      tier). 40-60 real out-of-domain English rows — the operator's,
      or harvested from the service's own traffic into
      okay-chat/corpus/harvested.json — and TestOfflineGate re-runs
      unchanged to settle it. Still needs human rows: the distillation
      lanes measured generated ones to be worth nothing (they carry
      the generator's register).
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)
