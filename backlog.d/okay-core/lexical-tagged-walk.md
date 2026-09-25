- [ ] lexical-tagged-walk — PRIORITY: LOW (design + measurement; trigger). The +72 B per
      operation that `Lexical.tail` still pays over the row is a `Delay`,
      its thunk and a `Return`, which keep the operation lazy. A tail
      installation could instead WALK its body the way a row handler
      does. Its operations would be inert `Inject(Local(inst, e))` nodes
      of ONE shared signature `Local` (identity-keyed like Delim's
      prompts, with one isolated cast justified by identity), and the
      state would thread through the walk purely. Expected near the row,
      and multi-shot safe without a cell. THE CATCH, and why it is not the
      default: with `Delim` in the row, the walk sits OUTSIDE the machine's
      prompt ordering. A multi-shot capture INSIDE the body would then give
      each branch its own state, where `deep` (and the guarded cell)
      thread it: the ListT-vs-StateT difference, silently. So it could be
      the default only for rows WITHOUT Delim (where nothing can capture),
      and `Local` would appear in those rows. TRIGGER: a consumer for whom
      instance operations in a Delim-free hot loop cost too much. Measure
      first. (2026-09-25)
