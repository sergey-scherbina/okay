## scalus-chainsync-pipelining — chain-sync with up to 100 requests in flight

- okay-scalus: `HeaderSync` pipelines `RequestNext` behind the tip, never
  past it (at the tip, one in flight as before). `TestPipelining` counts
  the requests in flight on the wire (1 before, failing first) and holds
  the order of headers and a mid-pipeline rollback to what one request
  at a time read.
- MEASURED, the same preprod backfill of 1 000 blocks: headers alone
  43.7 s → 2.26 s; `fetch = driver` 86.6/164.3 s → 10.2/9.0 s;
  `fetch = executor` 175.0/71.2 s → 5.8/5.7 s — executors now 1.6–1.8x
  faster on a backfill. Default stays `driver` (the tip gains nothing);
  docs say to use `executor` for backfills.
