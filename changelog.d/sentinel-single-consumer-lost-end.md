## sentinel-single-consumer-lost-end - the close-races-offers law tells a LATE consumer from a LOST one

- The law's two failures (2026-09-25, 2026-09-27) were both inside
  whole-build JVMs. Alone, 240 000 rounds with every core burning never
  hung. So the law no longer fails on a consumer that is merely slow
  under load. At 5 s it records the consumer's thread state and stack
  and the channel's new `debugState`, then waits up to 60 s more. A late
  end is logged. A lost one fails and carries the diagnosis.
- Not closed: the item stays in backlog.d/okay-core with what to read
  next time.
