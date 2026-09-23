## scalus-executor-fetch — executors fetch their own ranges (an option), and keep-alive by the clock

- okay-scalus: `BlockFetch.range` (one range request, bodies paired with
  their headers), `HeaderSync[A]` (the chain-sync source generic in what
  a header batch completes into), `ChainFollower[A]` with
  `CardanoFollower` kept as its body-fetching alias and
  `CardanoFollower.headers` confirming headers only (`BlockOf[Header]`).
- okay-scalus-spark: option `fetch = driver | executor`. With `executor`
  the driver follows headers and a `RangePartition` fetches its range on
  the executor, on its own session; same rows as `driver` (batch, every
  table, stream), a lost range fails the task naming blocks and relay.
- MEASURED on preprod, 1 000 blocks: no win (driver 86.6/164.3 s,
  executor 175.0/71.2 s; headers alone 43.7 s — a round trip per header
  on the driver in both modes). Default stays `driver`; filed
  `scalus-chainsync-pipelining`.
- FIXED on the way: the session pinged only when the wire was idle, so a
  streaming connection was reset by the relay at ~98 s (twice, measured).
  Keep-alive is now every 20 s busy or idle, one in flight
  (`TestKeepAlive`, failing first on the old code).
