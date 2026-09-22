## plutus-spec: Cardano scripts on okay's machinery — spec, before code

specs/plutus.md answers the operator's "can we run Cardano contracts on
our Free monad": yes, as a CEK machine whose `step` is an okay program
over Budget/Builtin/Trace and whose state is an immutable value. Reading
scalus 1.2.0 first corrected the pitch: its `CekMachine` already takes a
`BudgetSpender`, a `Logger` and a builtin-runtime function, so validator,
profiler and what-if runs exist there; what it lacks is its state as a
value (`private var ctx/env/value/term`). That is the module's reason:
stepping with back-step, save/resume, fork. Builtins and cost models stay
scalus's. Verification is the Plutus conformance suite with exact budgets,
a differential test against scalus, then the chain itself as an oracle via
okay-scalus-spark. Backlog section okay-plutus: plutus-machine,
plutus-price, plutus-tools, plutus-chain-oracle.
