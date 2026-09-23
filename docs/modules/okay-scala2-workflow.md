# okay-scala2-workflow

okay-workflow for **Scala 2.13**. A durable program's state is the journal
of the answers it was given. A new process over the same journal replays
it to where the old one stood, asking nobody anything twice.

| | |
|---|---|
| `Workflow[Q, A]` | the operations: `ask`, `now`, `uuid`, `random`, `patch`, `sleep`, `awaitSignal`, `awaitChild`, `cancelled` |
| `Workflows.drive(wf, journal, runtime)(oracle)` | run until done, or waiting for time or a signal |
| `Workflows.advance(wf, journal, runtime)` | a worker's step: stop at the author's next question |
| `Workflows.replay(wf, journal)` | the answer, from the journal alone |

A Scala 2 workflow is an ordinary `Eff[Workflow[Q, A], R]`; the engine
running it is okay-workflow's own. Durable agents are okay-scala2-agent's
`Chat` with a `journal`.

The walkthrough is section 8p of
[okay from Scala 2.13](../scala2.md#8p-durable-workflows-and-durable-agents), and the signatures are in
[okay-scala2](okay-scala2.md#api-reference).
