- [ ] okay2-cross — Scala.js and Scala Native for okay2. STAGE A LANDED
      (specs/okay2.md stage 31, 2026-09-24): the core, okay2-data,
      okay2-optics and okay2-workflow are crossProjects and every suite
      runs on all three. Remaining, stage B: okay2-async, okay2-platform,
      okay2-stream and okay2-stm — the platform files (Native's
      `FiberCell`/`TaskQueue`, JS's event loop, `NodeConn`, `Web`) behind
      the same `Async`, as okay-platform has them. (2026-09-24)
