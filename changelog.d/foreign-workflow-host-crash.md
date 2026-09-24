## foreign-workflow-host-crash - a durable foreign program survives its host

Stage 3 of specs/foreign-workflow.md, which closes the operator's
"интегрировать все это в дюрабле и воркфлоу".

- okay-agent: `Durable.over(...)(replayed = ...)`, told of each operation
  it answers from the journal (a no-op by default).
- okay-py: `SupervisedWorker.witness`. It rebuilds the continuation table
  from the replayed program records, marked as standing on no live
  worker, so the first live step re-derives each continuation on the new
  far side. Durable's replay and the supervisor's replay compose.
- Test, live (Python): a host killed in the middle of a multi-shot
  program (its worker with it); a fresh host and worker on the same
  journal finish every branch. Without the witness it is refused by
  name, never answered wrongly.
- Docs: one-language.md, "When the HOST dies too". The card
  `foreign-in-durable-workflow` is done: activities in do-notation
  (stage 1), the static Proc and proc-notation (stage 2), and this.
