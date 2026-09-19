## json-raw-nesting-jmh-pending - the JMH gate, done right: no regression

The deferred measurement's own problem was the box, not the code:
`parseOnly` and `parseValueOnly` (identical `Json.parse(text)`
bodies) read 282±26 vs 600±237 ns/op under host load 20-88 —
proof by itself that no number taken there meant anything.

`uptime` checked first this time (2.7-4.8 across the run, quiet on
14 cores): the two now agree, **250.562±9.074 vs 244.897±3.788
ns/op**, ~2% apart, error bars overlapping. Correctness was already
fully proven (184 tests, three platforms, an A/B at 100 000 levels);
this closes the remaining open question with no regression found.

`specs/iterative-recursive-decode.md`'s "Targets 1+2 done" entry
updated with the real delta; `src/jmh/history.tsv` gets the row.
