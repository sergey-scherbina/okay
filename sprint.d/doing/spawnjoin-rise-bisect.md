- [ ] spawnjoin-rise-bisect — `OwnMonitorBenchmark.spawnJoinSeq`
      (own, monitor 100us) read 86.8 us on 104f5f360 (2026-09-27/28,
      adaptive-outside-long-fibers-serial) and ~112 on 563d13e5b
      (2026-09-29, same-session A/B in adaptive-chunked-merge-cost: base
      112.4, lane 112.8) — a 1.3x rise on the lane where `own` beats kyo
      2.3x (five-way sequential spawn/join). FIRST confirm it in ONE
      session (104f5f360 vs master, arms alternating); then bisect the
      okay-async/okay-platform commits between them (first suspect
      cda0a94b5, the drive's slice hooks: a volatile write, a monitor exit
      and a thread-local read+write per slice). Fix, or record the price
      and why it is paid. (2026-09-29)
