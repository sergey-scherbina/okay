## lake-hudi — a Hudi copy-on-write table as the cluster engine's source

okay-lake's `HudiSource.snapshot/plan(lake, table)` reads a Hudi table's
`.hoodie` timeline (layout 1, or Hudi 1.x's layout 2 with Avro commit
metadata) and plans per file group the latest base file of a completed
commit; replacecommit-replaced groups, older slices and inflight writes
are left out, files older than the active timeline count as committed.
Merge-on-read and non-Parquet base files are refused by name.
okay-parquet now reads GZIP pages (Hudi's default) through the
platform's `java.util.zip`, refusing them on Scala.js. `TestHudi` (Live):
a table written by Hudi 1.2.1 on Spark 4.1 via pyspark — insert,
upsert, delete, insert_overwrite — read equal to Hudi's own read.
