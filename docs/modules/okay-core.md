# okay-core

THE KERNEL, A SUBMODULE: the okay-core repository checked out at `okay-core/` and built from source with this
build — its projects `coreJVM`, `coreJS`, `coreNative` are what the root `okay` depends on, nothing else depends
on it directly (`build.sbt`: `okayCoreJVM`, `okayCoreJS`, `okayCoreNative`, `ProjectRef`s into the submodule). It
is the one monad: `Cont[R, I, O, A]` — effects with rows in the monad of shift0 and `$`, a CEK machine with `k` as
data — and the facade over it (`okay.core`, `okay.core.std`; `okay.one` the kernel). Here it is two things:

| | |
|---|---|
| `okay.Effects[M]`, `A ! R`, `effect`, `handle`, `value` | okay-core's `RowEffects[M]` and its instance on the kernel's program, `Eff[R, A]` — the interface over rows every encoding implements, the default found with no import (`src/main/scala/Effects.scala`) |
| `okay.Carrier[A, S, R]`, `Control[Carrier]` | the classic tree's control carrier (okay-freer: a handler `F !> S`, a program `A /> R`) — a program of control under one `$` over no effects, `shift` a shift0 to it, `/` the `$` with `k` as its return, run; what the machine of okay-cont (stages 26–48) was, since 2026-10-14 (changelog.d/one-monad-carrier.md) |

Checked out with `git submodule update --init okay-core` — in a fresh clone and in every worktree, since the
build needs it where it builds. The pin moves by committing the submodule's commit here; okay-core is pushed before
okay, so the pin resolves. Its own suites run in its own repository (`sbt test` there); its account is its
README and docs (`okay-core/docs`).
