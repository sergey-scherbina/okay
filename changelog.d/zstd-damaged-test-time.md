## zstd-damaged-test-time - TestZstd's damaged-frame test fits the 30 s limit on Native

- okay-compress-zstd-speed's "a damaged frame fails as Corrupt" ran
  1 500 decompressions of a 256 KiB frame. In the runner's loaded whole
  build that took 43 s on Native, past munit's 30 s, and the build went
  red (runs over 42861945..e5abf5ca).
- It runs ~450 now: 0.4 / 0.8 / 1.1 s on JVM / JS / Native on a quiet
  box. The mutant without the block-size check still fails it (26
  damaged frames run off the buffer).
