## core-pin-shift-thin — okay-core pinned at 0c31bb0: shift on the kernel's own delimiter, reset and shift in Effects

Two okay-core changes come with the pin. `shift-thin`: a shift's `k` in its body is put back as ONE level (the
handler's and the view's merged), and std `reset` is a `Delimiting` handler the machine knows — a shift's body runs
on its level as it stands, each `k(x)` one node (no closure, `View`, `Resume` or `HandleK`); okay-core's
KernelBenchmark capture 0.915 → 0.661 µs, captureDeep 9.17 → 7.34, the other lanes within noise. `effects-control`:
`Effects[M]` (okay-core's `RowEffects[M]`) has delimited control beside `pure`, `perform`, `flatMap` — `reset`,
`shiftAt`/`shift0At` by path, the words `shift`/`shift0` bound where used as `effect(op)` is; `TestEffectsRows`
writes `k` twice over the interface. okay, okay-std, okay-async, okay-stream, okay-tx and okay-durable pass (896
tests), the whole build compiled with no warnings; suites named by hand (`affected` sees no change for a pin).
