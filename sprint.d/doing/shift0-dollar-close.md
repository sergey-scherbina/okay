- [ ] shift0-dollar-close — docs-only (2026-09-25, operator approved):
      close the questions the arc left open, in writing. (1)
      specs/shift0-dollar.md Decisions: stage 4 (the CPS hierarchy
      shift_i/reset_i via APLAS 2012's translation) DECLINED until a
      consumer asks for shift_i — stage 3 adopted nothing in
      Handler.scala, and Layered/Lexical took `$` without the hierarchy;
      a typed control0-to-`$` DECLINED with stage 3's reason (a shallow
      handler's return clause rides inside a plain push); stacked
      `control0` and the refused ICFP 2011 example recorded as a known
      incompleteness, with ONE backlog item `stacked-k-requirements`
      (LOW: per-continuation stack requirements, the road that would
      accept the paper's example). (2) specs/layered-reflection.md stage
      3: a section in docs/direct-style.md (Layer 1½) with the
      capabilities paper's `reify[Option](reify[List](…))` example
      translated and pinned, plus its literature; a Behavior box for it.
      (3) specs/cont-stack.md: a line that after stages C and D
      `stateLexDeep`/`stateShallow` are re-priced and the table in
      docs/many-instances.md updated — deep's 4x is the capture path
      that plan is about. Gate: `affected master` maps docs to nothing,
      so run `scripts/gate.sh "okayDeploy/testOnly okay.deploy.TestDocSnippets"`
      and `okayJVM/testOnly` for the pinned example. DONE WHEN: no open
      question in the two specs, snippet-debt did not grow.
