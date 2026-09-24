## okay2-stm - software transactional memory for the Scala 2 core

okay-stm as the subproject `okay2-stm` (specs/okay2.md stage 17): `Tx`
(read/write/modify/update/retry/check/orElse), `Stm[F].atomically`, and
three runtimes over `TRef` — `tl2` (Transactional Locking II over Async;
`retry` parks on every cell read, holding no thread), `direct` and `sim`
(under the deterministic scheduler). okay-stm's suites, 19 tests: 8
threads of transfers with no torn snapshot, a thousand parked
transactions freed by one commit, orElse's five laws, and the invariant
on 60 simulated seeds. The core gains `TMap.foreachUnordered`.

Docs: docs/okay2.md section 21.
