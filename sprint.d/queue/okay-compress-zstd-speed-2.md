- [ ] okay-compress-zstd-speed-2 — after okay-compress-zstd-speed, ZSTD
      on 4 MiB of lines against aircompressor: compress 16.5 vs 6.45 ms,
      decompress 4.28 vs 2.29 ms (history.d okay-compress-zstd-speed).
      Profile with async-profiler (`brew install async-profiler`; JMH
      `-prof async:libPath=/opt/homebrew/lib/libasyncProfiler.dylib`),
      NOT `-prof stack`: its safepoint bias doubled copyMatch's share
      and sent a whole experiment the wrong way. Leads, each measured
      before and after:
      - compression is the SEARCH: `Matcher.best` 22%, `insertUpTo`
        13%, `BitWriter.write` 10%. aircompressor's level 3 is "double
        fast" (two hash tables, one probe each, no chains): a strategy
        of that shape at the default level, the chain kept for higher
        levels, with the ratio compared to pyarrow's as well as the time;
      - decompression: FSE `next` + `BackBits` are 31% and the sequence
        loop 31%. One packed Int per state (base | bits | symbol) would
        replace three array loads; the bit reads could live in loop
        locals rather than the `BackBits` object's fields;
      - Native was never timed: a plain timing loop in a Native main,
        beside the JVM's. (2026-09-25)
