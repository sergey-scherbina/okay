- [ ] delim-region-prompts — NARROWED 2026-09-17 by
      delim-forward-not-throw: the NESTING case is solved (the nested
      forms, the OneMachine guard, and `runNested` for a machine you
      genuinely have), so this is now only about evidence that
      ESCAPES its own `delimited`. Nothing has asked for it.
      `Prompted[R]` proves a delimiter was
      installed, not that the machine running the capture is the one
      holding it, so an outer evidence used inside an inner
      `delimited` is still a runtime `NoPrompt` (pinned in
      TestDelimLimits). `scope`/`collecting`/`pausing` make the
      RIGHT spelling available (delim-nesting), but the wrong one is
      still only caught at run time. Closing it is the region trick
      `runST` uses — a rank-2 scope parameter on the evidence — and it
      would also close the "evidence escapes its delimited" case the
      header already names. Trigger: someone actually hitting it
      twice; the nested form is now the documented road.
      LITERATURE (biernacki-literature, 2026-09-24): besides `runST`,
      Biernacki, Piróg, Polesiuk & Sieczkowski, "Binders by day, labels
      by night" (POPL 2020) is the typed account of exactly this:
      a lexically bound instance whose runtime label cannot be used
      outside its handler. See `effect-instances-tunnelling`, which
      would close this with the same mechanism.
      UPDATE 2026-09-25: effect-instances-tunnelling closed through
      lexical-instances. Its STACKED instances (`Lexical.Stacked`) refuse
      use outside their installation at compile time, as `Delim.Stacked`
      does for prompts. What this item asks is unchanged: the UNSTACKED
      `Prompted[R]` evidence escaping its `delimited` is still a run-time
      `NoPrompt`. The stacked doors are the compile-time answer where a
      caller can use them.
