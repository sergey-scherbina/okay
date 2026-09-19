- dataflow-rescale-windowed — ANSWERED 2026-09-18 by
  `dataflow-windowed-rescale` (21d96006), and the answer is not the
  one this entry proposed. It asked to JOURNAL a windowed operator's
  open panes so a re-cut could rescale it, "rather than relying on
  replay (which a re-cut cannot do)". A re-cut can replay after all:
  box 2b's horizon (`dataflow-horizon-seek`) says how far back a
  windowed sink's state reaches, so new sessions open at the last
  epoch whose maximum is a horizon below the run and rebuild every
  open pane from there. `MeasureWindowedSeek` had already priced the
  two roads against each other — 16 KB once per resume against 3.3 KB
  every epoch for ever — and this is the cheaper one.
  WHAT THE ENTRY GOT RIGHT is the reason it was hard: a re-cut's
  partitions do not share the old ones' watermarks, so a WORKER cannot
  say which panes the coordinator already holds. The answer is that it
  does not try — it hands everything over, and two coordinator rules
  (`Sink.reopen`, `Sink.sift`) do the deciding, where retirement is
  the local decision and the number it used is known. Both have a
  control that fails without them; `reopen`'s took a sweep to find,
  because an in-order feed never leaves a pane handed-but-unretired.
