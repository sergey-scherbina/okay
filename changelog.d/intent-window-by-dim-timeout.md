## intent-window-by-dim-timeout - a measured timeout, not a guess

`TestWindowByDim` reded okay-arrow's full gate (2026-09-25, load
80-150) at 30.47s against munit's 30s default. Measured alone on this
box (load ~25-56): 1.728s — the suite's own cost is nowhere near the
limit; the 30.47s was contention on a saturated box, the same shape
`TestGenerate`'s own `munitTimeout` override already documents for an
unrelated suite. Gave it the same treatment: `override val
munitTimeout = Duration(120, "s")`, with the measured quiet-box figure
in the comment rather than a guessed one. No grid change — the six
(window, width) pairs stay; the fix is the clock, not the work.

Closes backlog.d/build/intent-window-by-dim-timeout.md.
