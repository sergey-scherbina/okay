- cont-stack-statepara-time-residual — MOOT 2026-10-04 (stack-host-three).
  It asked whether a ~1 µs gap between statePara and a pre-cont-stack base
  (b4934c052) was real. statePara has since been rebuilt on another
  machine: cont-atm 0.73x, strict-k-cost 0.94x, shift-capture-objects
  0.96x, now ~42.5 µs at 469 KB on its new lane shape. A comparison with
  that base measures nothing current.
