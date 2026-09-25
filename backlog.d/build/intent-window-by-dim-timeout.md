- [ ] intent-window-by-dim-timeout — `okay.intent.TestWindowByDim`
      "per-class F1 by window and width, grams alone and behind the
      cues, clean and under a typo" is a heavy computation IN the
      default gate that ran 30.47 s against munit's 30 s limit in
      okay-arrow's full gate (2026-09-25, load 80–150); alone, green at
      once. A CPU-bound suite at the edge of the limit on a quiet box
      reds every loaded full gate. Measure its quiet time first: if it
      is close to 30 s, raise its own `munitTimeout` with the measured
      figure in the comment, or shrink the grid (the F1 table does not
      need every width). (2026-09-25)
