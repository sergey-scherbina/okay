## wroclaw-pipeline-named - one Gtfs.departures, named throughout

Merges `GtfsNamed.departures` into `Gtfs.departures` in
`TestWroclawAlgebra`: the join chain's payloads are named tuples
(`route`, `service`, `time`, `tram`) instead of positional ones with
a trailing comment on every step saying what each held. The
twin-equality test (asserting the named and plain pipelines computed
the same thing, named-tuples-stage0's own regression check) is
replaced with an assertion against the RECORDED summary —
`Aggregator.Summary(4593288, 1679478374436, 7203, 730111)`, measured
on the real feed — since the twin it used to compare against is
gone. `GtfsNamed.scala` deleted.

Found in passing: `TestWroclawAlgebra` and `TestTaxiAlgebra` still
guarded on `javaFeature >= 24`, the same stale guard
`spark-jdk25-guard-fix` already corrected in `TestSparkInterop` to
`== 24` (Spark 4.2.0 supports JDK 25, SPARK-51167) — both were
silently skipping themselves on this box's JDK 25 fork the whole
time, `Passed: Total 0`. Fixed the same way; ran this suite Live for
the first time under JDK 25 to get the recorded summary above.
