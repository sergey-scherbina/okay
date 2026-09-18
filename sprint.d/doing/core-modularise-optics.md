- core-modularise-optics — stage 4 of specs/core-modules.md. `Optic`,
  `Fuse` and `Focus` (1 244 lines) become `okay-optics`. The lane the
  spec had filed as BLOCKED: one of its two seams was removed by stage
  2 taking `Proc` away, and the other turned out not to be about optics
  at all — `State.zoom` used a lens for `get` and `set` and nothing
  else. The core keeps `State.zoomWith`, the module restores the
  spelling by extending `State.type`, and no call site changed.
