# one-monad-carrier (2026-10-14)

THE PLATFORM ON OKAY-CORE'S ONE MONAD, at its seam: the classic tree (okay-freer) interprets a program over a
`Control[M[_, _, _]]` carrier — `F !> S` a handler as a program of the carrier, `A /> R` its diagonal — and the
carrier was okay-cont's machine (`Carrier`, stages 26–48). It is okay-core's control monad now: `okay.Carrier[A, S, R]`
is `Cont[Reset, S ! Pure, R ! Pure, A]`, `Control[Carrier]` gives `shift` as a shift0 to the one `$`, `/` as the `$`
with `k` as its return, run, `flatMap` a `Bind`; the `k` a body is given is strict, a run of its own, as before.
Nothing of the 114 modules on okay-freer changed. The module okay-cont is gone with it: okay's `Effects[M]` is
okay-core's `RowEffects[M]` over the kernel's rows (`Effects.machine = RowEffects.eff`, `Handler`/`Answering`
okay-core's, `Op` its `Operation`); `Rowed`, the tree under the facade on okay-cont, and `TestRowed` with it;
`AsyncCont` drives on okay-core's `Step` (a Run answered in place, an Await parked with the rest as data);
`StreamCont` and the stream's machine arm are over `Eff`. okay depends on `okay-core` (local, `0.1.0-SNAPSHOT`,
JVM/JS/Native); okay-core's facade std moved to `okay.core.std` so that okay-std keeps the package `okay.std`.
Suites: okay-freer 111, okay-async 15, okay-stream 533, okay-std 788, the root 3 — as before.
