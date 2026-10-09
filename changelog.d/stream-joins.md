## zip, sorted joins and windows in the stream front

Lane stream-joins. `Streaming` gains primitives (`unconsChunk`, an effectful
`unfoldP`, `pureP`/`flatMapP`) and, written once over them for every backend,
`zip`, `zipWith`, `joinSorted`, `leftJoinSorted`, `fullJoinSorted` and
`windowed` — the engines `SortMerge` and `Windows` driven by a pull, so they run
on JS as well (the classic `Source.zip`/`joinSorted` need `CanBlock`). One suite
runs them on both backends.
