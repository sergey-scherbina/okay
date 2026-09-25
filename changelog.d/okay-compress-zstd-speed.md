## okay-compress-zstd-speed - ZSTD decompression 2.6x faster, compression 1.3x

- On 4 MiB of lines, against aircompressor:
  - decompress 11.1 -> 4.28 ms (air 2.29);
  - compress 21.8 -> 16.5 ms (air 6.45).
- What paid:
  - match copies at offsets under 8, a whole period at a time;
  - XXH64/XXH32 read through `Mem`;
  - the sequence loop on locals, with room taken once per block;
  - the encoder's tables without boxing every Int.
- A damaged length is now refused against the block's 128 KiB. A new
  test holds that a damaged frame fails as `Corrupt`, never as an index
  error.
- Refuted: wild copies (level, reverted). Also JMH's `-prof stack`,
  whose safepoint bias doubled the match copy's share; profile with
  async-profiler.
- What is left, with its leads: `okay-compress-zstd-speed-2`. Also
  filed: `jmh-lane-foreign-jmh-lock`. Commits: see the lane's branch.
