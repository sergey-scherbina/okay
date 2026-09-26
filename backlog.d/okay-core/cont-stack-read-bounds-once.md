- [ ] cont-stack-read-bounds-once — the exact road's price per read,
      found by cont-stack-jmh-native-access (2026-09-26, history.d
      `cont-stack-jmh-native-access`): at the tests' room of 64,
      statePara's exact road is 1.53x the count road (+19.5 KB a run);
      at the derived room of 873 it is 0.99x (+2.9 KB). Every read in
      `StackSwitch.more` calls `StackRoom.sp()` AND `top()` AND
      `floor()`, and each allocates: a confined `Arena` plus a 1 KB
      (8 KB on glibc) `ucontext_t` for the pointer, and on glibc
      another `Arena` and a `pthread_attr_t` for each bound. The bounds
      never change for a thread. THE LANE: read the bounds once per
      thread (the `Gauge` already keeps `top`; keep the floor beside
      it, or a thread-local pair), and reuse one `ucontext_t` buffer
      per thread instead of an `Arena` a read; then re-run the room-64
      pair (`-jvmArgsAppend --enable-native-access=ALL-UNNAMED` against
      the plain lane) and the room-873 pair. DONE WHEN the room-64
      exact road is within noise of the count road, or the residual is
      measured and named. PRIORITY: LOW — at the room a user runs the
      two roads already cost the same.
