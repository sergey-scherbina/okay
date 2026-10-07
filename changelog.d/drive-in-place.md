## the machine's callback drive answers in place what it can: 2.38x → 1.15x

Lane drive-in-place (specs/freer-min.md, stage 50). `AsyncCont.runAsync`
answers a Run on the answering road and an Await whose callback fired
during its registration by a tail `k(x)`; only a pending Await is
captured. okay-cont gains `Cap.Split` (with `Partly`), a capability that
picks its road per operation. AsyncDriveBenchmark.contRunAsync10k: 562 →
273 us per 10k Runs, the classic drive 237.
