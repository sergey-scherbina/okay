- [ ] growing-order-drain-guarantee — WHAT THE PROBE LEFT, with the
      window NAMED (2026-09-18, adopted-window). The first cut of this
      entry said "two candidate fixes, both hot path" and left it
      there; that was a hand-wave, and reading `AdaptiveFifo` replaced
      it with something exact.
      THE WINDOW, NAMED. The fix this needs is already THERE and is
      already a rule rather than a phase: `popManyAdoptedFirst` reads
      the adopted part 0 first whenever it has anything in it, and its
      comment claimed the rule "has no window at all". It has one, and
      the code shows it in three lines:

          if took > 0 then ...
          else if open.get == 1 then 0
          else popManyScanning(max)(sink)   // part 0 was EMPTY

      THE RULE HOLDS PER CALL AND THE CALL IS NOT ATOMIC. Part 0 comes
      up empty, the consumer goes to scan other parts, and a straggler
      whose route was read before the swap lands in part 0 during that
      scan. Its element is then delivered behind its own successors —
      the very shape the rule was written to fix. The comment is
      corrected in place rather than deleted: the rule is the
      improvement it claims to be, only its last clause was false.
      THE CANDIDATE, and why it is not taken here: SEAL part 0 to
      pushes at the moment of adoption. A straggler is then refused
      and reroutes to its own part, landing AFTER its predecessors
      instead of before them, and the refusal path already exists
      (`push` false -> `refused()` -> a part of its own). Two other
      candidates are REFUTED by the trace and should not be re-tried:
      merging `grown` and `inner` into one atomic does NOT fix it (the
      straggler's push into the ring is legitimate until the swap, so
      atomicity changes nothing), and "drain part 0 to empty before
      reading others" does NOT fix it either (the consumer DID see it
      empty — that is the window).
      WHAT IT COSTS is the open question: sealing puts a check on the
      ring's push path, in a class whose header carries four benchmark
      tables. `ChannelGuaranteeBenchmark`, `ManyProducersBenchmark`
      and `ChannelGranularityBenchmark` are the three lanes that would
      price it, and a matched pair is the bar.
      THE THIRD OPTION, and it belongs to the operator rather than to
      an agent: weaken the promise. `Growing` says per-producer FIFO;
      it could say "per-producer FIFO except across the one-shot
      swap", and `TestGrowing`'s law would then assert what is true.
      Cheaper than any fix, and it gives something up.
      WHO STANDS ON THE PROMISE, surveyed 2026-09-18 so the choice is
      made with the consumers in hand rather than in the abstract:
        - `TestChannelLaws` states it as a LAW — "one producer's
          elements arrive in the order it sent them".
        - `Channel.scala` already gave up EXACT FIFO ACROSS producers
          deliberately, and names the escape hatch
          (`Queues.strong[A].fifo(capacity)`); per-producer is the
          half it kept.
        - `Source.merge` RESTATES the promise in its own words to its
          own callers ("each source keeps its own order"), so
          weakening the law means editing that too — and the whole
          streaming stack goes through it. `merge-chunked-order` is
          where the defect showed as `1..16, 49, 50, 17..48`.
        - THE ACTOR MAILBOX IS THE UNGUARDED ONE. A mailbox is a
          `Channel[M]` (default 256) and `Actor.scala` promises
          nothing about order — but "messages from one sender arrive
          in send order" is what every reader of an actor model
          assumes. Weakening the channel law weakens that silently,
          which is the worst shape a weakening can take.
        - NOT AFFECTED: okay-persist. The durable journal writes
          through a JDK `FileChannel`, not this one, so durability is
          not in this decision.
        - The rest (okay-cluster `Remote`, the HTTP transports and
          SSE, `ChatDemo`) are multi-producer but either do not need
          the order or recover it from sequence numbers.
      AND THE SHAPE OF THE DEFECT ARGUES AGAINST WEAKENING: it is ONE
      element displaced across a ONE-SHOT swap, rare enough to need
      thousands of rounds under load. A user cannot reproduce that,
      will not connect it to this, and will look in their own code. A
      promise broken once in thousands, and only while the buffer
      grows, is worse than no promise: people lean on it precisely
      because it is almost always true. `docs/queues.md` already warns
      about the defect with its sightings — weakening would turn "rare
      bug, being fixed" into "by design", which is a different message.
      Reproducer and evidence: `ProbeGrowingOrder`, BUGS.md.
      HOW TO RUN IT: load, not repetition — that is what distinguished
      the green runs from the red ones, and 2 000 rounds on a quiet box
      reproduced nothing. Burners to a load of ~20, the round-based law
      in a loop, every break printed with both counters beside it.
      WHAT WOULD CLOSE IT: a break whose counters are non-zero names
      the road; a break whose counters are both zero refutes both
      candidates and is worth as much.
      RULED OUT ALREADY, so nobody walks it again: a stale route
      surviving into `pushDecidingAt`. Measured 12 crossings of that
      window in 156 578 sends, all in the harmful direction, and
      `Growing.ours` repairs every one (BUGS.md, the retraction).
