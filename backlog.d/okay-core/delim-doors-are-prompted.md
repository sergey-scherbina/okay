- [ ] delim-doors-are-prompted — okay-llm's `Cut` and okay-ui's
      `Scope` take their ambient delimiter as a `Prompt[A] ?=>`, which
      ANY caller can construct (`Delim.prompt[A]` is one line and
      proves nothing), where `Delim.Prompted` cannot be forged. Moving
      them would make `cut`/`cancel` outside a guard a compile error
      rather than a runtime `NoPrompt`. It changes a public signature
      in two modules, so it is a decision, not a tidy-up. Found while
      landing delim-patterns-in-modules (2026-09-17), which left them
      alone on purpose: they duplicate nothing.
