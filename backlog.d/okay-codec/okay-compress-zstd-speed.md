- [ ] okay-compress-zstd-speed — after okay-compress-jvm-fast-paths
      (8-byte loads, compares and copies through a per-platform `Mem`),
      ZSTD is still behind aircompressor on the JVM, measured lane by
      lane (history.d okay-compress-jvm-fast-paths): decompress 11.1 vs
      3.0 ms and compress 21.8 vs 6.3 ms on 4 MiB of lines; LZ4 decompress
      1.9 vs 0.9 ms. The 8-byte paths are in; what is left is per-symbol
      work. Where to look, each to be MEASURED before and after:
      - `BackBits.read` checks and branches per call, where
        aircompressor keeps a 64-bit container and refills it once per
        sequence;
      - FSE `next` goes through three arrays and a method per state,
        where one packed Long per state would do;
      - `Out.bytes` checks room per literal run;
      - the encoder re-hashes every position, and one table could be
        probed instead (aircompressor's level 3 is a "double fast"
        strategy with no chains).
      Native was never timed: a plain timing loop in a Native main, beside
      the JVM's, belongs here. (2026-09-25)
