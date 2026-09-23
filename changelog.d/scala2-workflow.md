## scala2-workflow - okay-workflow from Scala 2.13, and durable agents

Stage 15.7 of specs/scala2-facade.md (operator: "делай всё что возможно
чтобы работало в скале 2").

- Probed first: the driver's data (`Wf.Step`, `Wf.Wait`, `Wf.SysA`,
  `Wf.Runtime`) is plain and used directly. A durable body is not: it is
  a context function over `Wf.Asks`, whose doors exist only while the
  driver runs it.
- The new module okay-scala2-workflow: a workflow is an ordinary
  `Eff[Workflow[Q, A], R]`. `Workflow[Q, A]` holds the operations (ask,
  now, uuid, random, patch, sleep, awaitSignal, awaitChild, cancelled)
  over a facade-owned GADT, and `!.translate` rewrites each one into its
  `Asks` door inside `Wf.resumable`. `Workflows.drive`, `advance` and
  `replay` are okay's drivers over a journal that is plain data.
- Durable agents: okay-scala2-agent's `Chat` takes an optional
  `okay.agent.Durable.Journal` (and a per-tool `onRepeat`), and wraps its
  tool handler in `Durable.tools`, so a restart over the same journal
  replays tool calls instead of repeating them.
- Tests: `TestWorkflowFromScala2` (5: drive, replay as a new process, the
  worker loop, a durable sleep then a signal, `patch` on an old journal)
  and `TestDurableAgentFromScala2` (2, one of them the control: without a
  journal the restart pays twice).
- Docs: section 8p of docs/scala2.md (copied from the probes), the module
  page, the API reference, and the spec's stage 15.7.
