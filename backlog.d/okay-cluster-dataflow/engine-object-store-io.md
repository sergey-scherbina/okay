- [ ] engine-object-store-io — S3-compatible storage and Parquet as the
      cluster engine's SOURCE and SINK (operator, 2026-09-26). okay-blob,
      okay-delta, okay-arrow's files and okay-jdbc are client libraries: a
      `Src` partition can wrap one by hand, none is a source or a sink. For
      bulk reads and writes: objects under a prefix as partitions (a large
      one split by byte range or row group), Parquet read WITHOUT Spark
      (bulk-parquet is the reader itself), Arrow/Parquet written per
      partition with the sink's commit (stage 9) naming what is visible, and
      okay-blob's S3 put gaining MULTIPART upload — today a put buffers the
      whole body in memory. Gate: 100 GB of Parquet on Live MinIO read by 4
      workers and written back, memory held per partition, a worker killed
      mid-write leaving no half-visible output.
