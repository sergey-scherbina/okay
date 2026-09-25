- [ ] okay-compress-zstd-speed-3 — after okay-compress-zstd-speed-2
      (double fast): compress 7.54 vs aircompressor 6.45 ms, decompress
      4.28 vs 2.29 ms on 4 MiB of lines (history.d). What is left of the
      earlier lead list:
      - decompression: FSE `next` plus `BackBits` are ~31% and the
        sequence loop ~31% (async-profiler). One packed Int per state
        (base | bits | symbol) would replace three array loads, and the
        bit reads could live in the loop's locals;
      - Native was never timed: a plain timing loop in a Native main,
        beside the JVM's.
      Profile with `-prof async:libPath=/opt/homebrew/lib/libasyncProfiler.dylib`.
      (2026-09-25)
