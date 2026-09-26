## lake-delta — a Delta table as the cluster engine's source

okay-lake's `DeltaSource.snapshot/plan(lake, table)` replays `_delta_log`
without Hadoop — the last checkpoint (nested Parquet, read by
okay-parquet) and the JSON commits after it, gap-checked — into the live
data files, planned as row groups; partition values ride on each `Part`
and `ParquetSource.flow` adds them as columns typed by the table's
schema. Deletion vectors, column mapping, v2 checkpoints and unknown
reader features are refused by name. `TestDelta`: a table written by
delta-rs (appends, DELETEs, a checkpoint, commits after it) read equal
to delta-rs, a removed file not planned; hand-written logs for the
refusals. lake-iceberg and lake-hudi filed.
