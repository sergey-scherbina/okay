- [ ] ui-terminal-v2 — what is LEFT of v1's minimum after
      ui-terminal-keys (2026-09-18) read the escape sequences: an
      `Input` edits by append and backspace with no CURSOR, `Scroll`
      renders its child whole and scrolls nothing, `Resized` is an
      event no host consumes (so a row divides its NATURAL width by
      weight, never the screen's), and there is no mouse. Each is a
      lane of its own and each needs a decision the tree is part of:
      a caret is host state but Left/Right are reserved for it
      already; a scrolling viewport and a width-aware layout both mean
      `Frame.render` takes a size, which is the one change that would
      touch every terminal test. TRIGGER unchanged: a product that
      runs on the terminal host for a PERSON, not a test — it will say
      which of the four it needs, and the keys lane is the pattern
      (one gap, one lane, all of it values).
