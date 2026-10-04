- shift-capture-objects — DONE 2026-10-04. The allocation profile of a
  capture (delimDollarResume, async-profiler alloc) has no `Found` and no
  `Cut` at all: C2 already removes them. What it did have is `Frames.End`
  at 4.7% of the bytes, a fieldless node allocated by every `end` and by
  the dollar cut's `Frame(ret, End())`. It is now ONE instance for every
  index, `Frames.end`, with one isolated cast: it holds nothing that could
  be read at a wrong type. statePara 0.96x (-32 KB), contAnswer 0.95x
  (-16 KB), delimDollarResume 0.96x (-32 KB), delimGenerator 1.00x
  (-16 KB) (history.d shift-capture-end). What is left of a dollar capture
  is the cut's `Snoc` + `Frame` re-wrapping `ret` (3.6% + part of Frame's
  10.5%). A piece case for a one-frame segment would save them, but it
  needs a new `Piece` arm in `reinstall`/`resume`. It is not filed until a
  workload is dominated by dollar captures.
