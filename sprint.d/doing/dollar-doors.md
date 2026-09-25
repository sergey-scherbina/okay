- [ ] dollar-doors — API, compile time where the arc promised it
      (2026-09-25, operator approved). Four small pieces, one lane:
      (1) an EVIDENCE door for `$`: `Delim.dollar(using Prompted[R])(ret)
      (body)` beside the `Prompted` doors, and `scopeWith(ret)` beside
      `scope`, so the recommended pattern spelling can write a dollar
      without a raw prompt; (2) `Delim.Stacked.control(p)` to a prompt
      that `Stacked.dollar` made compiles today and throws at run time
      (the machine refuses a control-capture to a dollar) — make it a
      compile error: `dollar` hands a distinct `In` subtype (as
      `Lexical.Stacked.SInst` does) and `control` requires the plain one;
      pinned with `compileErrors` like TestStackedShift0's CLOSED test;
      (3) `!Delim.shift0[A]` in direct blocks, mirror of the inline
      `shift` door (delim-one-type); (4) docs: `abort` to a dollar skips
      `ret` (the `$/S0` rule with k dropped) said in
      docs/continuations/11-four-captures.md, and TestDollar's "abort"
      body gets its expected value ("gone!") instead of only
      macro-vs-primitive agreement. Gate: `scripts/gate.sh "affected
      master staged"` plus TestDocSnippets for the docs line. DONE WHEN:
      three new tests, one compileErrors, docs lines pinned.
