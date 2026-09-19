- [ ] growing-order-drain-guarantee — OPTIONAL, NOT A BUG. The
      default channel's per-producer order across its one-shot swap
      was CLOSED AS A DOCUMENTED TRADE by the operator on 2026-09-18
      (8af62bc7, "the default says what it keeps"): `Growing` promises
      a producer's own order broken in at most ONE place, ONCE, across
      the swap; `TestChannelLaws` and `TestGrowing` state that law and
      still fail on mass reordering; the two spellings that keep the
      exact order are named where a caller meets the choice —
      `Queues.strong[A].adaptive` (never adopts a buffer, so no swap)
      and `Queues.strong[A].fifo` (one tail) — in docs/queues.md's
      table, `ActorRef`'s header, and `TestMailboxChoice`. BUGS.md
      `growing-stale-route` carries the whole history: three sightings
      (2026-09-10/11/17, each a hoist of exactly `Channel(4)`'s
      capacity), the retracted stale-route mechanism, and the real one
      `ProbeGrowingOrder` named — a producer's elements SPLIT across
      the swap between part 0 and its new part, and the consumer has
      already passed part 0.
      An earlier cut of this entry (2026-09-18 14:29, before the
      decision that afternoon) argued against weakening; that argument
      was heard and the decision went the other way. Nobody has asked
      for the guarantee back since.
      WHAT BUYING IT BACK WOULD TAKE, kept so the price is known if
      somebody does: SEAL part 0 to pushes at the moment of adoption,
      so a straggler is refused and reroutes to its own part, landing
      AFTER its predecessors — the refusal path already exists (`push`
      false -> `refused()` -> a part of its own). It puts a check on
      the ring's push path; `ChannelGuaranteeBenchmark`,
      `ManyProducersBenchmark` and `ChannelGranularityBenchmark` are
      the three lanes that price it, and a matched pair is the bar.
      Two candidates REFUTED by the trace, not to be re-tried: merging
      `grown` and `inner` into one atomic (the straggler's push is
      legitimate until the swap, atomicity changes nothing), and
      "drain part 0 to empty before reading others" (the consumer DID
      see it empty — that is the window, named in
      `AdaptiveFifo.popManyAdoptedFirst`). Reproduce with LOAD, not
      repetition: `ProbeGrowingOrder`, burners to ~20, the round-based
      law in a loop, and count the rounds in which the buffer actually
      grew.
