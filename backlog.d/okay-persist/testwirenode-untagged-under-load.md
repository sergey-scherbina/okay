- [ ] testwirenode-untagged-under-load — `TestWireNode` (okay-persist,
      scala-js: "the shared client speaks to a Node server: no JVM in
      this process") sits in the default gate untagged and timed out at
      30 s in a whole-family `affected master staged` run on
      2026-09-25 (7 330 results, this the only red, box load 3.5 with
      siblings gating), then passed alone through
      `scripts/gate.sh "okayPersistJS/testOnly okay.persist.TestWireNode"`
      a minute later. It starts a Node server and talks to it over a
      socket, which is the shape the `Live` policy (AGENTS.md, "no
      flaky tests in the default gate") tags: its result depends on
      the box's timing, not on the tree. Tag it `Live` (whole-suite
      `munitTests` override), or split the socket part from the
      codec/protocol part so the latter stays in the gate. Found by
      mrjar-jdk25-ci-gap, whose lane never touched okay-persist, so
      filed rather than fixed in that lane.
