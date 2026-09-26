## engine-object-store-io — object storage and Parquet as source and sink

New module okay-lake. `ParquetSource.plan(lake, prefix)` makes every
Parquet row group under a prefix (or named by its manifest) a partition,
read by byte range through any okay-blob `Blob` with okay-parquet and
decoded as a case class; `ParquetSink.to[A]` writes each partition as one
Parquet object built on the worker's disk a row group at a time and put
whole, and the batch commit writes `_manifest.json` naming exactly the
run's objects and deletes strays. Lakes are registered by name per
process, so no credential travels with a job. `TestLake` (a directory)
and `TestLakeS3` (Live MinIO, sized by `OKAY_LAKE_MB`; 6.7M rows, a
worker killed mid-write, 57 s). Closes the item with s3-multipart-put,
compress-snappy and parquet-codec.
