- [ ] cont-stack-statepara-time-residual — after cont-stack-fastpath
      (2026-09-28) statePara with no switch possible allocates LESS than
      the pre-cont-stack base b4934c052 (300 960 vs 321 888 B/op) yet
      read 28.20 µs against a base 27.13 measured in an EARLIER session
      (history.d `cont-stack-fastpath-r3-noswitch`). First re-read the
      base in the SAME alternating series (a base worktree builds in
      minutes with `scripts/gate.sh "export okayJVM/Jmh/fullClasspath"`)
      before believing a ~1 µs gap exists; only then look for it — the
      bytes are no longer the lead (specs/cont-stack.md stage C, round 4).
