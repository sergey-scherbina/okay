- [ ] okay-compress-zstd-ratio — our ZSTD writes columnar buffers 1.3–1.6x
      larger than pyarrow's zstd (arrow-vs-cbor, 2026-09-25, Arrow IPC
      with per-buffer ZSTD, 1000 rows):
      - trades 9168 vs 5808 bytes;
      - events 23584 vs 14344;
      - orders 21816 vs 16680;
      - at 100k rows, 504744 vs 379384 (trades).

      The likely cause, to be CONFIRMED by printing each literals
      section's form: `HuffmanEncoder` writes only the DIRECT weight
      form, which holds at most 128 weights. A binary buffer's literal
      bytes span 0–255, so its literals go RAW. The fix is FSE-compressed
      Huffman weights (the header byte < 128: a table description, then
      two interleaved states over up to 255 weights, accuracy log at most
      6), written from the decoder's own table as the sequence encoder
      already is.

      Second, for time: the encoder allocates a 2^17-entry head table per
      call, 512 KiB for a buffer of a few bytes. Size it to the input, as
      LZ4's already is. Measure both with ArrowVsCborBench and
      TestZstdPyArrow's ratio print. (2026-09-25)
