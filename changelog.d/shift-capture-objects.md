## shift-capture-objects: one `Frames.End` for every index — statePara 0.96x, contAnswer 0.95x

- The capture's own objects (`Found`, `Cut`) were already scalar-replaced; the profile named `Frames.End`
  (4.7% of a capture's bytes), a node with no fields allocated per segment end. `Frames.end` is now one
  instance behind one isolated cast. statePara 0.96x (-32 KB), contAnswer 0.95x (-16 KB),
  delimDollarResume 0.96x (-32 KB), delimGenerator 1.00x (-16 KB) (history.d shift-capture-end).
- delimited-simplify-costs closed: both of its parts were answered earlier (state-foreign-shape, strict-k-cost).
