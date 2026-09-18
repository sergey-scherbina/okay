- [x] ui-terminal-v2 — CLOSED 2026-09-18, eight lanes: keys
      (4ecfbcc4), width (ae6c3cf2), scroll (8574e913), caret
      (325a3655), mouse (f41591e0), column-minimum (0bda68ea), the
      layout map (fba0d574) and the viewport. The terminal host was
      v1's minimum this morning — Tab and nothing else, no caret, a
      `Scroll` that scrolled nothing, a `Resized` nobody heard, no
      mouse.
      WHAT MADE IT CHEAP, worth keeping for the next host: everything
      that can be a value is one in `Frame`, and the host keeps only
      what cannot — the decoder state between two reads, the caret,
      the view. Every lane was tested with no tty at all.
      WHAT IS NOT DONE, and neither is a lane: a terminal that reports
      a RESIZE (SIGWINCH needs a signal handler this file cannot
      install, so a resize needs a new host today, stated in
      `Terminal.size`), and Windows raw mode, which has its own entry
      and needs a Windows box to verify on.
