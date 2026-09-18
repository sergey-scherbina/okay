- [x] optics-cont-profunctor — DONE 2026-09-18, HALF LANDED AND HALF
      REFUTED, which is the useful shape. `Strong` exists
      (`PState.Zooming` + top-level `opticZooming`), `PState.zoom` is
      now `l[Zooming[X, R]](m)` with TestZoom's six passing unchanged,
      and the instance bought three roads the hand-written shift never
      had: an iso zooms, `first` zooms a pair state, a composed optic
      zooms. `Choice` CANNOT EXIST: on the absent case the zoomed
      program must still answer the inner program's `X`, and `X` is
      universally quantified — parametricity, not a type error, and
      the spec's predicted mechanism ("the indices will not line up")
      was wrong. The door that does exist prices itself in its type:
      `PState.zoomCase` answers `Option[X]`. TestContProfunctor pins
      the absence with the `Strong` summon beside it as the control.
      Not measured and not claimed: `zoom` now summons a fieldless
      instance per call; its four callers are all tests and no JMH
      lane covers it, so a lane that gives it a production caller
      prices that first.
