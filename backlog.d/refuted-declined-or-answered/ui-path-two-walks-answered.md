- ui-path-two-walks-answered — ANSWERED 2026-09-18, not a lane (moved here from
  optics-arrows-effects by backlog-audit-0923): the two walks
  STAY two, and the measurement is why — `PathWalkProbe` at depths 4,
  16 and 64 puts the affine at 2.9-7.9x the time and a steady ~5.5x
  the allocation of the hand walk, because it is built from a runtime
  `List[Int]` and pays the interpreter per step. What the question
  found instead was worth more than the tidy-up would have been: the
  two walks DISAGREED about totality. `Ui.path` answers an Option;
  `Ui.patch` indexed its Vectors and threw on a path naming nothing —
  and a Patch arrives over a WIRE, so a well-formed message with a path
  that is not on the client's tree killed the session inside
  `Wire.client`'s receive loop. Guarded now, with the law in
  TestUiOptic extended to cover it.
