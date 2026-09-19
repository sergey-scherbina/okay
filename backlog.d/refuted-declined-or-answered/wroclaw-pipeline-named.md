- wroclaw-pipeline-named — DONE 2026-09-19: `GtfsNamed.departures`
  merged into `Gtfs.departures` (named payloads throughout, the
  trailing shape comments deleted — the type says it now); the
  twin-equality test replaced with an assertion against the recorded
  summary (`Aggregator.Summary(4593288, 1679478374436, 7203,
  730111)`) rather than a twin that no longer exists, per the
  entry's own instruction. `GtfsNamed.scala` deleted.
