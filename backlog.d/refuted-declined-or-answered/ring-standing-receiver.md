- ring-standing-receiver — REFUTED TWICE 2026-09-27: no receive-side
      design removes the chunked ring road's slow regime. The ring
      merge's sides read their channels with one-shot receives, and on
      the chunked road (f0f355bd4's) a caught-up consumer makes a second
      regime (~250 us against ~200, 84-217 side wakes per op against
      11-20; ring-chunk-bimodal-forks). Tried: (1) zero-allocation side
      wakes — a `Pending` per registration, an int ring of wake-ups, the
      continuation applied by the drive — slow regime unchanged (6/10
      forks), elementwise parity; (2) a NOTIFYING receive in
      `SentinelChannel` (`receiveManyOrWatch`: an armed waiter only says
      "look again", the elements stay in the ring and the reader takes
      everything on its own thread) — slow forks 1/10 and 5/10 against
      the one-shot's 2/10 and 5/10 in the same session, and its slow
      forks made MORE wake cycles (137-149/op against 91-106). A
      caught-up consumer finds one chunk per look whichever thread pops
      it: batching what has not been produced yet is not a receive's to
      do. The shared channel of today's road stays in one mode (0/10)
      because two producers feed one queue. Both designs dropped, no
      code landed. Rows: `src/jmh/history.d/…-ring-standing-receiver.tsv`
      and `…-ring-standing-receiver-notify.tsv`; specs/source-merge-via-ready.md
      (the stage). REOPEN only with a design that changes the RELATIVE
      speed (the producer's side, or a merge that does not run caught
      up), not the receive. ADDENDUM 2026-09-28 (ready-merge-chunk-forward):
      "no receive-side design" was too strong by one — WHEN a side
      registers was never varied. Registering only when the merge has
      nothing else to do (poll-then-park, LANDED in `ReadyMerge`) took
      the registration storm to zero and the fast forks to 188-197 us,
      and still left a 2-4/10 tail at 215-247 against the shared
      channel's 195-204: the residual is a caught-up consumer waiting on
      the producers, in that entry.
