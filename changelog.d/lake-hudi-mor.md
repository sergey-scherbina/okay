## lake-hudi-mor — Hudi merge-on-read tables as the engine's source

`HudiSource.plan` now reads MERGE_ON_READ tables: a file slice with log
files is one partition — its base Parquet read whole, the `#HUDI#` log
blocks merged over it by record key in instant order under the table's
merge mode (event-time ordering by its ordering fields, or commit
time); delete blocks and `_hoodie_is_deleted` remove records; blocks of
an instant that never completed are skipped. A slice without logs is
read a row group at a time, as copy-on-write. Parquet/HFile/CDC log
blocks, log-only file groups, tables without meta fields and CUSTOM
merge modes are refused by name. `AvroReader.decoder` (ours and Apache
Avro) decodes single Avro-binary values. TestHudi (Live): a MOR table
written by Hudi 1.2.1 — insert, upsert, an older event's upsert, delete
— reads equal to Hudi's own read, and equal to its pre-delete read with
the delete's instant uncommitted. specs/dataflow.md stage 18,
docs/modules/okay-lake.md.
