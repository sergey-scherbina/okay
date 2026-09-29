## fleet-approvals - the REPL's y/n answered from any host

- `Ctx.ask(step, call): Boolean ! Async`: the ask goes on the agents record
  (`Event.Asked(id, Ask(seq, tool, args))`) and the runner parks until
  `Control.Approve(seq, yes)` — through the mailbox or the `commands` topic
  (`approve{id, seq, yes, by}`); `Event.Answered`; `Status.asking`.
- A stop or a kill declines an open ask, so no runner is left parked; a
  wrong seq changes nothing; a fleet restored over an unanswered ask shows
  it Interrupted with the ask still visible.
- Which calls to ask about is the runner's policy (nadia SPEC §3.3); a runner
  that never asks is auto-approve. `TestFleetApprovals` (3, JVM). Spec:
  specs/agent-fleet.md "Approvals". nadia NAD-14/NAD-21.
