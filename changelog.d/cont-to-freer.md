## The CPS `Cont` and its machine move below the core, into okay-freer

Refactor, lane cont-to-freer (specs/freer-min.md, stage 31). `Cont.scala`,
`Delimited.scala`, `ContReplay.scala`, `macros/ContMacro.scala`, the
stack-switch runtime (`StackSwitch`, `StackRoom`, `StackPool`, the JDK 22
variant) and `DirectCtx` move from the core to `okay-freer`, package `okay`
unchanged, so nothing written against `okay.*` changes. `Control`, `Func`
and the `Control[Cont]` instance stay in the core (`Control.scala`): the
interface is common to this `Cont` and to `okay-cont`'s machine.

The machine's `Run` no longer names the core's `Free`/`Unary`: it is
generic in the tree signature operations leave at, `Delimited.Leaving[F, O]`
(the core's identity at `Unary[F]`, beside the doors in `Diagonal`'s
companion). The multi-release jar with the `jdk22/` StackRoom is
`okay-freer`'s, exported (`exportJars`), so the core's tests and the JMH
lanes see the versioned class; `versioned` variants compile against their
host's class directory and dependencies, not its `fullClasspath` (a cycle
with an exporting host).
