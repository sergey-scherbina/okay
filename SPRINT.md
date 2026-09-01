# Sprint

## Doing
- [ ] ui-durable — event-sourced UI sessions on okay-persist stage 0:
      journal inbound lines intent-first (one session = one key),
      recover by refold through the SAME pure stage, snapshots by
      fold-the-tail until persist-stage1
      (spec: specs/ui.md; claim: .work/active/ui-durable.claim)

## Queue
(next candidates from BACKLOG.md: ui-wire, ui-screens)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
