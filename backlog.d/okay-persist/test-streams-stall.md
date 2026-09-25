- [ ] test-streams-stall — SIGHTING (stack-safety-stream-stm's staged gate,
      2026-09-25): the gate STALLED at 2 920 results with the workers at
      0 CPU, and the dump names `okay.persist.TestStreams.takeChunks`
      parked in `Channel.receiveBlocking` while a `Reactive.Pump` thread
      and a scheduler thread were present and parked too — the shape of a
      lost wakeup between the pump and the channel (memory:
      parked-workers-refute-exhaustion), not a runner handshake. The lane
      touched okay-stream's flushing feed only, which `Streams.stream`
      does not use. Evidence: okay-gate.pav7xm0nfj.stall.json/.stall.ps
      in $TMPDIR. TWICE in the same lane's staged gate (the second at 3 091
      results, okay-gate.EW... beside it), and GREEN alone on the branch
      and on master in the same hour — so it is load-dependent: the
      staged gate ran beside two other gates on a box at load 60+. A
      finding to chase with the dumps: the pump and the consumer both
      parked. (2026-09-25)
