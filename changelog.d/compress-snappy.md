## compress-snappy — Snappy in the Compression facade

okay-compress gained `Snappy`, the raw format a Parquet page holds (a
varint length, literals, copies with 1/2/4-byte offsets): greedy 4-byte
hash matching, a decoder that refuses a copy before the start, a cut
element or a wrong length as `Corrupt`. It is `Compression.snappy` —
ours by default on every platform, aircompressor's raw Snappy behind
`Aircompressor.given` — and each reads the other's output on every
sample, ours within 1.3x of the library's size. Part of
engine-object-store-io (a Parquet reader without Spark needs it).
