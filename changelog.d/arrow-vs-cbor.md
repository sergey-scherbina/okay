## arrow-vs-cbor - measured: typed record batches as Arrow against CBOR

okay-arrow stage 7a (specs/okay-arrow.md): `ArrowVsCborBench`, the same
`Schema` rows through both, three shapes and two batch sizes, with
pyarrow's writer as a reference.

- Uncompressed, Arrow wins on every shape: 0.6–0.9x the bytes, a quarter
  of the allocation, and 3.5–5x faster round trips at 1 000 rows.
- Compressed with ZSTD, CBOR wins wherever there is text or nesting, even
  against pyarrow's own Arrow+ZSTD (2.2–2.3x smaller): per-buffer
  compression loses the row-wise repetition. Arrow wins only on flat
  numeric tables.
- The rule for moving transports: Arrow for batches that travel
  uncompressed; CBOR+ZSTD over the network unless the batch is flat and
  numeric.
- Found: our ZSTD is 1.3–1.6x larger than pyarrow's on columnar buffers,
  most likely because binary literals go raw (Huffman in the direct
  weight form only). Filed as `okay-compress-zstd-ratio`.
