# okay-flink

The smallest module in the family, and that is the point:
`toFlink(agg)` maps an okay Aggregator onto Flink's
`AggregateFunction` field for field (createAccumulator/add/merge/
getResult). Exercised the way a window driver does: panes accumulated
separately, merged, presented — equal to the direct run.
`flink-core` is pure Java: no cross-version pain.
