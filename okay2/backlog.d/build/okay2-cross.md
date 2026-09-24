- [ ] okay2-cross — Scala.js and Scala Native for okay2. LANDED 2026-09-24:
      stage A (specs/okay2.md stage 31: core, data, optics, workflow),
      stage B (stage 32: okay2-async and okay2-platform with the JS and
      Native platform files), stage C's first half (stage 34:
      okay2-stm). Remaining: okay2-stream — after `adaptive-seal-race`
      lands, since its channel suites (virtual threads) must move to
      `src/test/scala-jvm` and that lane is editing them; the fast
      channels' atomics are the javalibs' own. (2026-09-24)
