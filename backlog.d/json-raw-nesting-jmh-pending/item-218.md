- [ ] `json-raw-nesting-threshold-trampoline` landed with correctness
      fully proven (184 tests, three platforms, an A/B at 100 000
      levels via a scratch `Codecs.maxDepth`/`NativeThreshold`) but NO
      trustworthy JMH number: system load climbed from ~20 to 88 while
      measuring (a shared box, unrelated to this lane), and
      `parseOnly`/`parseValueOnly` — two benchmarks with IDENTICAL
      bodies — read 282±26 vs 600±237 ns/op in the SAME run, proof the
      box could not answer the question regardless of fork count.
      Re-run `compare/Jmh/run -i 5 -wi 3 -f 5 .*parseOnly.*|.*parseValueOnly.*`
      once `uptime`'s load average is near the core count, and update
      `specs/iterative-recursive-decode.md`'s "Targets 1+2 done" entry
      with the real delta.
