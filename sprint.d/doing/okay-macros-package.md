- [ ] okay-macros-package — the operator's ask (2026-10-02): the core's macro
      implementations in their own package, `okay.macros`
      (src/main/scala/macros/), private to `okay`; the `inline def`s stay in
      the API and call `${ okay.macros.X.impl(…) }`. Stage 1: ContMacro whole,
      and the impls in Shift, Answers, Distinct, Indexed, Provide. Stage 2,
      after handle-frames-forms lands: Handler's. Stage 3, separately: the
      satellites (okay-direct's macros/ is package `okay` today). A move: no
      behaviour changes. (2026-10-02)
