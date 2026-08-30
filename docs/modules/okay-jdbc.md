# okay-jdbc

- `query(conn, sql)(row)` — the result set streams fetch-size rows
  per chunk, each read inside one Async operation: constant memory
  for any result size; the statement closes itself at exhaustion
  (an abandoned stream leaks it — scope the CONNECTION).
- `batch(conn, sql)(bind)(chunk)` — one chunk, one executeBatch.
- `connection(url)` — under the Resource region: the connection
  closes on the scope's end, on handled aborts and on exceptions
  (tested on H2 with an abort inside the region).
