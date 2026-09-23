- [ ] gate-command-chain — PRIORITY: MEDIUM. `scripts/gate.sh "a; b"`
      runs `a` and silently drops `b`: met twice today (three suites
      in one call, only the first ran, "0 test results" in the
      verdict line was the only tell). Either the script splits on
      `;` and passes each as its own sbt command (sbt's own syntax is
      `sbt "a" "b"`), or it REFUSES a `;` with a message naming the
      spelling — silence is the one wrong answer. A test in
      gate-selftest for both. (2026-09-23)
