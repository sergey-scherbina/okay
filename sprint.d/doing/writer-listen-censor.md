- [ ] writer-listen-censor — PRIORITY: LOW (trigger). Found by the core
      review 2026-09-26. Writer has no SCOPED operations. Reader has
      `local` (run a sub-program under a changed environment), and Throws
      has `recover` (answer a failure inside a sub-program). The Writer
      duals (mtl's MonadWriter) are:
        listen(p): (A, W) ! Writer % W + F  — run p, answer its value AND
                   what p told, and still tell it outward
        censor(f)(p): A ! Writer % W + F    — run p, and rewrite what p
                   told (with f) before it goes outward
      Both are a handler over a PART of the program, like `recover`: run
      `Writer.run` on p, then re-tell (f of) the collected output. Mind
      the laws (specs/scoped-effects-laws.md): a scoped op over a
      forwarded effect must not duplicate or reorder it. TRIGGER: a
      consumer that wants the log of a sub-step (audit trail per request,
      the cost of one stage in a pipeline).
