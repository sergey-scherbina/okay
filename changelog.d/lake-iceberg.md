## lake-iceberg — an Iceberg table as the cluster engine's source

okay-lake's `IcebergSource.files/plan(lake, metadataKey, root)` reads an
Iceberg table's metadata JSON, its current snapshot's manifest list and
manifests, and plans the live Parquet data files as row groups; delete
files and non-Parquet files are refused by name. The manifests are Avro:
okay-lake gained `AvroReader` — ours (`OkayAvro`: the object container,
the binary encoding, null/deflate/snappy/zstandard blocks, bounded
depth) as the default and Apache Avro behind `ApacheAvro.given` over an
optional dependency, each giving the same records for every pyiceberg
manifest (specs/own-or-standard.md). `TestIceberg`: pyiceberg's table
(appends, then a delete) read equal; a DELETED manifest entry — asserted present — not planned.
