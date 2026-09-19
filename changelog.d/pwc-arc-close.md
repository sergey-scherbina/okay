## pwc-arc-close - producer-to-writer-carrier closes; Producer stays whole

The arc's last bullet — "deletions land with the last module" —
closes as a decision, not as work. With every module lane landed
(persist 543da10b, sql 14bd60e2, docs 74fa7605, kafka 23b01f6e, after
the `Chunks` retype 76408290 and the pure iterator bf15904c), no
module's streaming seam is typed on `Produce` any more, and the
operator's call on what to do with `Producer` itself is: keep it
whole. It is the GENERATOR carrier — `produce` is yield, `Put[Producer]`
is what `generate`/`nats`/`fibs` unfold into, `fold/each/concat/log`
are its own walks — and `Source.fromProducer`/`ofProducer`/`toProducer`
keep either carrier convertible into the other. A minimal-deletion
branch (`Producer.fold/each/concat`, zero callers) was built, compiled
clean, and dropped unmerged at the operator's "wait — don't delete".

The spec's Interface block and checklist say so; the Decisions
section records the call and what was rejected on either side; the
sprint item is deleted — the arc is done. `foldwriter-js-incompatible`
(backlog, moot) and `chunks-fold-vs-foldleft-2x-gap` (a JIT
curiosity) are the two entries that outlive it.

Files: specs/producer-to-writer-carrier.md,
sprint.d/queue/producer-to-writer-carrier.md (deleted).
