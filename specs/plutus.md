# okay-plutus: Cardano scripts on okay's machinery

## Overview

The operator's question (2026-09-23): can Cardano smart contracts
(and scalus's) be EXECUTED on okay's own Free monad and the rest of
what okay has? Yes — and the spec exists to say what that is FOR,
because the answer "another CEK machine" is not worth a module.

A Cardano script is a UPLC program (Untyped Plutus Core): untyped
lambda calculus plus constants, `Constr`/`Case`, and ~100 builtins.
It runs on a CEK machine (Control term, Environment, Kontinuation
stack) that charges an execution budget — CPU and memory — per step
kind and per builtin call, from the cost model in the protocol
parameters. The answer is accept/fail plus the budget spent, and the
budget must match the Haskell reference TO THE UNIT: a different
number is a different consensus verdict.

## What scalus already has (read, not assumed — scalus 1.2.0)

- `scalus.uplc.Term` (Var, LamAbs, Apply, Force, Delay, Const, Builtin,
  Error, Constr, Case), `ProgramFlatCodec`, `DeBruijn`;
- `scalus.uplc.eval.CekMachine(params, budgetSpender, logger,
  getBuiltinRuntime, caseOnBuiltinsEnabled, profiling, tracing)`;
  cost models (`CostModel`, `BuiltinCostModel`, `MachineParams`),
  `ProfilingData`; ~6.9k lines in `uplc/eval`;
- every builtin including BLS12-381, secp256k1, ed25519;
- `scalus-uplc-jit-compiler` (master): UPLC compiled to JVM code.

**Correction to the first answer given in chat**: scalus's machine is
ALREADY parameterised where the effect-handler pitch pointed —
`BudgetSpender` (a strict or restricting budget), `Logger` (traces),
and `getBuiltinRuntime: DefaultFun => BuiltinRuntime` (substituting a
builtin's answer). A strict validator, a budget recorder and a
"what if this signature were valid" run are all possible on scalus
today. Building those again on Free would be a slower copy.

What scalus's machine does NOT have is its STATE as a value:
`CekMachine` keeps `private var ctx`, `env`, `value`, `term` and runs
to completion inside `evaluateTerm`. That is the right choice for a
validator and the one thing okay's machinery changes.

## Design

### 1. The machine as a program; its state as data

```scala
enum Frame:                                   // the K of CEK
  case ApplyArg(arg: Term, env: Env)
  case ApplyFun(fn: Value)
  case Force
  case ConstrField(tag: Long, done: Vector[Value], rest: List[Term], env: Env)
  case CaseScrutinee(branches: Vector[Term], env: Env)

final case class Machine(control: Control, stack: List[Frame], spent: ExBudget)
enum Control:
  case Compute(term: Term, env: Env)
  case Return(value: Value)
  case Done(value: Value)
  case Failed(error: MachineError)

def step(m: Machine): Machine ! (Budget + Builtin + Trace)
```

- `step` is ONE transition of the CEK machine, written as an okay
  program over three effects:
  - `Budget` — `spend(kind, cost)`;
  - `Builtin` — `call(fun, args): Value`;
  - `Trace` — `Writer` of trace strings.
- A run is `step` iterated until `Done`/`Failed` (`!.loop`, the
  fold-until arc). The strict validator is the fold with the strict
  handlers.
- `Machine` is a plain immutable value: it can be stopped between two
  steps, printed, serialized (a `Schema` for it — `Term` and `Value`
  from scalus get instances as in specs/scalus.md §3), resumed, or
  forked into two continuations.
- Builtins and cost model are SCALUS'S, called through
  `BuiltinRuntime` — never re-implemented. The cryptography and the
  cost formulas are the part where a second implementation can only
  be wrong.

### 2. What the value-state buys (the reason for the module)

1. **A stepping debugger with time travel.**
   - The run is a sequence of `Machine` values, and the journal of
     answers (continuation journal = event sourcing, the persist arc)
     makes any step reachable again without storing every state.
   - `Zipper` (specs/zipper.md) over the current term shows where in
     the source the machine is.
   - Breakpoints on a builtin, on a budget threshold, or on a
     `Constr` tag.
   - Step back.
2. **Save and resume.** A long evaluation (or a failing mainnet
   script) is saved as its `Machine` at the failing step and reopened
   later, on another machine, with the same scalus version.
3. **Fork.** Two continuations from one state with different `Builtin`
   answers: "this branch if the signature verifies, that one if not",
   without re-running the prefix.
4. **Cost attribution by term.** The budget handler charges the cost to
   the term under `control` (or its source position, from scalus's
   `UplcAnnotation`), giving a flame graph over the contract's source.
   scalus's `ProfilingData` also profiles; the difference to measure,
   not assume, is attribution to TERMS vs to builtins/step kinds.

The following do not need our machine, and the spec says so rather
than claiming them. Both run on scalus's CEK in a Spark UDF today:

- a strict validator;
- a "what if" cost model — a different `MachineParams` is enough.

### 3. Verification: conformance first, then the chain

- **Plutus conformance suite** (IntersectMBO/plutus,
  `plutus-conformance`: UPLC programs with expected result and
  expected budget). Every case must pass with the exact budget before
  anything else is built on the machine. This is the threshold, not
  a nice-to-have.
- **Differential test against scalus's `CekMachine`**: same term, same
  params → same result and same budget, on generated terms as well as
  the conformance set.
- **The chain as an oracle** (needs okay-scalus-spark, specs/scalus.md):
  - every mainnet transaction with scripts carries redeemers with
    declared `ExUnits` and an `isValid` flag;
  - replaying every script of an epoch — scripts, datums and redeemers
    from the `cbor` columns, the ScriptContext from `outputs` ⋈
    `inputs` — gives a table of (script, verdict, budget) to compare
    against what the network accepted;
  - a divergence is either our bug or a finding; either way it is a
    row, not a guess.

### 4. Price

The Free machine will be slower than scalus's `var` loop. The okay
measurements say a Free node beats a CPS closure (handler-fusion
stage B); they do not say a Free step beats a mutable CEK loop, and
nothing here claims it.

- The JMH lane is scalus `CekMachine` vs `okay-plutus` strict run on
  the conformance programs and on a few real mainnet scripts.
- It is priced, with lane rules (docs/benchmarks.md), and the number
  decides only one thing: whether the strict validator path should
  DELEGATE to scalus's machine and keep ours for the debugging modes.
  That delegation is the expected outcome and is fine.

## Stages

- **Stage 0 — the machine**
  - [ ] okay-plutus module (JVM first), scalus_3 1.2.0 for `Term`,
        flat codec, `BuiltinRuntime`, cost models
  - [ ] `Machine`/`Frame`/`Control` + `step` over Budget/Builtin/Trace
  - [ ] Plutus conformance suite vendored as test resources, every
        case green with exact budgets
  - [ ] differential test vs scalus `CekMachine`
- **Stage 1 — price**
  - [ ] JMH lane vs scalus; decision recorded: delegate strict runs or
        not
- **Stage 2 — the tools**
  - [ ] save/resume a `Machine` (Schema instances, round-trip test on
        a mid-run state)
  - [ ] stepper with back-step through the answer journal;
        breakpoints
  - [ ] fork with substituted builtin answers
  - [ ] cost attribution by term, compared with scalus `ProfilingData`
        on the same script
- **Stage 3 — the chain as oracle** (after okay-scalus-spark)
  - [ ] ScriptContext from the Spark tables; replay an epoch's scripts;
        a divergence table

## Decisions

- 2026-09-23 — **builtins and cost model are scalus's, never ours.**
  They are where a second implementation can only diverge from
  consensus.
- 2026-09-23 — **the module's reason is the state as a value**
  (debugger, save/resume, fork), not the effect handlers: scalus's
  `CekMachine` already takes a `BudgetSpender`, a `Logger` and a
  builtin runtime function, so validator/profiler/what-if exist
  there. Recorded because the first answer in chat claimed them as
  okay's advantage.

## Results

(none yet)
