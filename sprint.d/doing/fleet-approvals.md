- [~] fleet-approvals — the REPL's y/n/a answered from any host (nadia SPEC
      §3.3, BACKLOG NAD-14/NAD-21): `Ctx.ask(step, call): Boolean ! Async`
      appends `asked{id, seq, tool, args}` and parks until
      `Control.Approve(seq, yes)` (a stop or kill answers false);
      `Event.Asked`/`Event.Answered`; `Status.asking: Option[Ask]`; the
      `approve` command. Auto-approve is a runner that never asks. On top of
      fleet-commands. Additive.
