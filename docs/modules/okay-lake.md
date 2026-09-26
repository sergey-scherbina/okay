# okay-lake

Object storage and Parquet as the cluster engine's source and sink
(specs/dataflow.md, stage 17): the shape of a risk or fraud model run
over a data lake — Parquet in, scores out — with no Spark, no Hadoop.

## Guide

**A lake is a `Blob` known by name.** S3 (and MinIO, R2), or a directory
— okay-blob's engines. Every process registers the lakes it can reach,
with its own credentials, and a job's parameters name one; no credential
travels with a job:

```scala
Lakes.register(bucket, S3(http, "http://127.0.0.1:9000", bucket, "us-east-1", creds))
```

**The source: one partition per row group.** `ParquetSource.plan` lists
the Parquet objects under a prefix (or reads its manifest, when it has
one) and makes each ROW GROUP a partition, so a large object is several.
A partition reads its group by byte range through okay-parquet and
decodes the rows as a case class by its Schema:

```scala
val plan = ParquetSource.plan(name, "in")
```

```scala
ParquetSource.flow[Trip](p.plan).map { t =>
```

The model in the map may be R or Python: okay-foreign-cluster's
`mapPy`/`mapR` take the flow's rows as Arrow frames.

**The sink: whole objects, and a manifest.** `ParquetSink.to[Scored]`
writes each partition as ONE Parquet object under `prefix/_data/` —
built on the worker's disk a row group (`groupRows`) at a time and put
whole at the partition's end (multipart above 8 MiB) — so a worker killed
mid-write uploaded nothing. The run's commit writes `prefix/_manifest.json`
naming the objects the run's partials named, one per partition, and
deletes anything else under `_data/`; a reader plans from the manifest.

```scala
def sink(p: ScoreParams): Wire[Scored, Manifest] = ParquetSink.to[Scored](p.plan.lake, p.out, p.groupRows)
```

**A Delta table as the source.** `DeltaSource.plan(lake, table)` replays
the table's `_delta_log` — the last checkpoint, then the JSON commits
after it — to its live data files, and plans them as row groups like any
prefix; a file a DELETE removed is not read, and partition values (not in
the files) arrive as columns typed by the table's schema:

```scala
val plan = DeltaSource.plan(lake, "visits")
val rows = Flows.collect(ParquetSource.flow[Visit](plan)).runWith
```

Deletion vectors, column mapping and v2 checkpoints are refused by name
— reading past them would return wrong rows.

**An Iceberg table as the source.** `IcebergSource.plan(lake,
metadataKey, root)` reads the table's metadata JSON, its current
snapshot's manifest list and manifests (Avro), and plans the live data
files; `root` is the URI the lake's keys are relative to (`s3://bucket`,
`file:///dir`), since Iceberg's metadata holds absolute paths:

```scala
val plan = IcebergSource.plan(lake, metadata, root)
```

Delete files (position or equality deletes) and non-Parquet data files
are refused by name. Columns are read by name, not Iceberg's field ids —
a table whose columns were renamed after its files were written misses
them. Avro is read by ours (`OkayAvro`, the default) or Apache Avro's
(`import okay.lake.ApacheAvro.given`, an optional dependency); both give
the same records for pyiceberg's manifests.

**A Hudi copy-on-write table as the source.** `HudiSource.plan(lake,
table)` reads the `.hoodie` timeline (layout 1, or Hudi 1.x's layout 2)
and plans, per file group, the latest base file a COMPLETED commit
wrote; an upsert's older slices, an inflight write's files and file
groups a replacecommit (insert_overwrite, clustering) replaced are not
read:

```scala
val rows = Flows.collect(ParquetSource.flow[Reading](HudiSource.plan(lake, "readings"))).runWith
```

Merge-on-read tables are refused by name — their log files hold rows
their base files do not. Hudi writes GZIP pages by default; okay-parquet
reads them with the platform's `java.util.zip`.

**SQL over the output: DuckDB, by the manifest.** An analyst's DuckDB
reads exactly what a run made visible — the manifest's objects, never a
glob a late or lost writer could join:

```scala
s"select count(*), sum(score), count(distinct id) from ${m.duckdb(root(name))}")
```

where `m` is `Manifest.of(lake, prefix)` and the root is `s3://bucket`
(DuckDB's httpfs with a secret for the store) or a lake's directory.
DuckDB reads through okay-sql's `Sql` seam as `JdbcSql` does everywhere
(specs/data.md); okay-parquet and DuckDB read each other's files.

## Gotchas

- A batch run only (`Cluster.run`): a stream is refused by name, since a
  partition's object is whole only at its end — a stream's output goes
  through a staging sink (okay-kafka's `EpochLog`).
- Local disk: a worker holds one partition's output file on disk until
  it is put.

## Literature

- Apache Parquet, *File Format* — row groups as the unit of splitting.
- Armbrust et al., *Delta Lake: High-Performance ACID Table Storage over
  Cloud Object Stores*, VLDB 2020 — a log of which objects are visible,
  of which this manifest is the one-commit case.
