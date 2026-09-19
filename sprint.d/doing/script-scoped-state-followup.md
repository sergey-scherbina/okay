- [ ] script-scoped-state-followup — fix the doc drift script-scoped-state
      (175a2993) left behind, plus a regression test for the specific
      fragility it closed.

      WHY: landed the Scoped[A] migration, then a follow-up question
      surfaced that the doc comments weren't updated. Api.scala:17's
      file-header comment still describes "Per-request state lives in
      a ThreadLocal... the container sets Web/Response/Session...
      setCurrent" -- the OLD shape. specs/okay-script.md quotes the
      old `def setCurrent(w: Web): Unit` signature in ~10 places
      (the "Request context" and "Hot-reload" sections and their code
      blocks). Also: "no leak across sequential requests on a reused
      thread" is currently proven by CONSTRUCTION (every `where` has
      a `finally`) but has no test that would catch a regression if
      `Requested.run`'s nesting were ever broken.

      HOW: update Api.scala's header comment and every stale
      `setCurrent`-shaped code block in specs/okay-script.md to the
      Scoped[A]/where shape (see specs/script-scoped-state.md for the
      current shape). Add a test in okay-script (TestSecure.scala or
      TestSite.scala, wherever Access.Granted is already exercised):
      two sequential servePage calls on the SAME calling thread --
      first a secure: page (Access.Granted), then a plain Access.Open
      page -- asserting api.Principal.current == None during/after
      the second, i.e. nothing survives from the first past
      Requested.run's unwind.

      DONE WHEN: no remaining `setCurrent`-shaped text in
      specs/okay-script.md or Api.scala's header; the new leak test
      exists and passes; `scripts/gate.sh "affected master"` green,
      run COLD (clean okayScript first) so the warnings check is
      honest.
