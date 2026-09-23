- [ ] staging-scope-extrusion — PRIORITY: MEDIUM. okay-staging generates code from
      programs that may perform effects, and NOTHING tests the hygiene
      failure that effects in a generator produce: a generated binder
      captured into a `State`/`Once` cell and spliced outside its
      `let` is ill-scoped code — "scope extrusion". Kameyama, Kiselyov
      & Sunada, "Combinators for impure yet hygienic code generation"
      (PEPM 2014; JFP 2016), and "Shifting the stage" (PEPM 2009) for
      let-insertion with delimited control, are the references; MetaOCaml
      detects it at run time, BER MetaOCaml by a scope check on
      splice. THE LANE: first the test that TRIES it (a stager whose
      body stores a staged variable in a cell and reads it after the
      binder closed) and reads what happens today; then either a
      refusal by name at splice time (the cheaper road) or the paper's
      typed discipline for let-insertion, decided by what the test
      shows. (2026-09-23)
