- [ ] shift-in-scope — backlog shift-effect-level1's (3), the operator's
      "Продолжай" (2026-10-02): every `shift` names `[R, A, F]` because
      `k`'s type holds R and F and Scala fixes them before it types the
      lambda. `reset` hands its block a `Shift.In[R, F]` (Delim's
      `Prompted` pattern), so inside it `shift[A](k => …)` takes R and F
      from it, and `reset { … }` takes R and F from its expected type. The
      full `shift[R, A, F]` stays for code outside a block. Behaviour
      unchanged. specs/shift-effect.md. (2026-10-02)
