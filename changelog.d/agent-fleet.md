## agent-fleet - agents as supervised actors, a hierarchy the parent grows

- `Fleet.open(store, runner)`: a root actor whose children are agents.
  `spawn(spec)` answers an id at once; `send(id, Fleet.Control.*)` — tell,
  pause, resume, stop (finish the tool, then halt), kill (now) — is the
  mailbox; `status`/`all` are values; `transcript`; `await` for a parent;
  the record on the store's `agents` topic, folded by `restore()` so a
  dead process leaves `Interrupted` agents with their transcripts and
  ids that continue.
- `Runner` + `Fleet.Ctx`: the consumer's loop asks `checkpoint(step, tool)`
  between tool calls — the step is recorded, a pause waits there, a stop
  or an exhausted budget (steps or wall clock) is answered there.
- `Fleet.delegate(fleet, parent)`: one `Toolbox.In[Async]` tool; the child
  runs to completion in the parent's workspace (or under it), its steps
  come out of the parent's budget, too big a child is refused naming both
  numbers, a crashed child is a tool error the parent reads.
- okay-agent gains `.dependsOn(okayActor)`. `Control` is nested in `Fleet`
  (okay's core exports one). Spec: specs/agent-fleet.md, all items
  checked, five decisions recorded; `TestFleet` (7, JVM, a scripted
  runner stepped by ticks). Consumer: `../nadia` `app/`.
