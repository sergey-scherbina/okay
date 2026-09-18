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
