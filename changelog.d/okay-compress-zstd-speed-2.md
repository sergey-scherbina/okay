## okay-compress-zstd-speed-2 - ZSTD compression 2.2x faster: double fast at the default level

- Levels 1 to 3 use "double fast", the reference's and aircompressor's
  strategy at those levels (`ZSTD_compressBlock_doubleFast`):
  - an 8-byte and a 5-byte hash table, one probe each, no chain;
  - a repeat offset tried one byte on;
  - backward extension, a growing step through input that does not
    match, and the tables filled at a few positions inside a match.
  The hash chain stays for levels 4 and up.
- On 4 MiB of lines: 16.5 -> 7.54 ms, against aircompressor's 6.45. The
  ratio at level 3 moved by 2% at most (`numbers` 7% smaller).
- Every sample round-trips at levels 1, 3, 6 and 19 (TestZstd), and
  pyarrow interop still holds.
- Filed: zstd-level6-worse-than-3 (level 6 compresses worse than level 3
  did), okay-compress-zstd-speed-3 (FSE packing, Native timing).
