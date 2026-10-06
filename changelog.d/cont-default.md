## The machine is the default carrier of `Effects[Free]`; the CPS one by import

Feature, lane cont-default (specs/freer-min.md, stage 33). `okay-cont` is
dependency-free and below the core, beside `okay-freer`, the two knowing
nothing of each other; the core holds `Control` with all three instances
(`Cont`, `Func`, the machine's `Carrier`) and `Prog`. `FreeEffects` is a
class over its carrier, `FreeEffectsAt[C]`: the default given is at the
machine's `Carrier`, so a handler `F !> S` is a program of the machine and
`Effects[Free].handle`, `foldCont`, `convert`, `reify` run on it; `import
okay.cps.{given_Effects_Free, *}` chooses the CPS `Cont` as it was, with its
`!>` and `handler`. `Control` gained the "already an answer?" probe
(`isAnswer`/`answerOf`) every carrier answers, which the handle loop uses
in place of `Cont.onAnswer`. The core's handlers (`Throws`, `Maybe`,
`Choice`, `Prob`, `Reader.local`, `Handler.control`, okay-java's `Eff`)
are written through the instance's `control`, so they run at either
carrier; `relay`, `translate` and `HandleFrames` stay on the CPS machine.
