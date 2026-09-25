## okay-compress-zstd-ratio - Arrow+ZSTD as small as pyarrow's, and faster than CBOR+ZSTD

- The gap was measured buffer by buffer, and the first hypothesis (the
  int64 column) was REFUTED: that buffer compressed to exactly pyarrow's
  size. Arrow's OFFSET buffers went raw: their literal bytes span 0–255,
  and our Huffman wrote only the direct weight form (at most 128
  weights).
- FSE-coded Huffman weights (RFC 8878 4.2.1.2, two interleaved states).
  pyarrow decodes them: an offsets buffer is now 7 452 bytes against its
  7 447.
- The ZSTD encoder's hash table is sized to the input; it was 512 KiB per
  call.
- At 1 000 rows, Arrow+ZSTD bytes went 9 168 -> 5 848 (trades),
  23 584 -> 14 648 (events) and 21 816 -> 14 200 (orders); pyarrow gets
  5 808, 14 344 and 16 680. The round trip went from 7.8 to 0.76 ms, now
  faster than CBOR+ZSTD on every shape. CBOR+ZSTD stays 2.2–2.4x smaller
  where there is text or nesting, which is the format's own cost.
