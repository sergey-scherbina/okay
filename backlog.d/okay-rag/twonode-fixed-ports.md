- [ ] twonode-fixed-ports — `okay.demo.TestTwoNode` spawns two REAL
      JVMs on HARDCODED ports 18091/18092 and is not `Live`-tagged, so
      two agents gating at once collide on them. Seen 2026-09-11 in
      generalized-method-syntax's first matrix: the test failed in
      **0.337 s** with `java.io.IOException: HTTP/1.1 header parser
      received no bytes`, and passed alone a minute later in 3.763 s.
      The timing is the evidence, not a guess: two JVMs cannot boot in
      300 ms, and the suite's own `whoami` swallows every exception
      and waits up to 15 s for both to answer — so for the run to get
      PAST readiness and then die on a later request, something was
      already listening on those ports, and it was not this run's
      children. A sibling's matrix running the same suite is the only
      candidate left; nothing else in the repository uses 1809x.
      nio-port-scope's survey could not have caught this one: it greps
      the test tree for `new ServerSocket`/`serve(0)`/`listen(0)`, and
      here the port is bound by a CHILD process, passed in through
      `OKAY_CHAT_PORT`. Two fixes, and the choice is the point: an
      ephemeral port pair (the test would have to read the port back
      from the child, which is real work), or the `Live` tag that its
      two neighbours in okay-demo already carry (TestChatDemo,
      TestRepoAgent) — which keeps it out of `sbt test` and costs the
      default gate a real distributed-failover test. Price both before
      taking either.
