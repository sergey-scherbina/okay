# okay-semantic-arrow

Semantic execution over Arrow tables and IPC streams, using the existing
okay-arrow Rows/ArrowCodec/Compression facades. Applications keep a typed Schema
and the same semantic Plan used for collections or SQL. ArrowData.table reads a
Table; ArrowData.ipc reads IPC bytes; both return named errors or the semantic
result. No Arrow Java jar or agent dependency is required.

The adapter currently decodes a batch to typed rows, then uses the shared core
aggregate. It is not a vectorized expression compiler and does not claim
zero-copy aggregation. Use Data.chunks with decoded batches for bounded input
memory. Arrow dictionaries, decimal representations and shape checking are
owned by the existing Arrow codec.

Parquet uses Data.file with the existing ParquetFormat: row groups are independent
splits, decoded using Schema and aggregated by Bulk. The focused tests write
Arrow IPC and multi-row-group Parquet data and compare their analytics with the
original typed records. CSV/JSON are covered by okay-semantic-data. Local files,
range GET from blob storage, Python/R Arrow interchange, or a distributed Bulk
reader therefore share the same metric definitions.

[Specification](../../specs/semantic-layer.md).
