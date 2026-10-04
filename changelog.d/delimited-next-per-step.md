## delimited-next-per-step: refuted — one mutable Next per run is slower than allocating it

- Probe (discarded): every `Step` writes one per-run slot instead of allocating a `Next`. statePara
  1.05x, fib100 1.01x, contAnswer 1.08x, delimGenerator 0.98x (history.d delimited-next-slot).
  Where bytes stayed the same, C2 already removes the `Next`. Where they fell, the per-step writes cost
  more than the allocation did. The slot also needed a cast per step.
