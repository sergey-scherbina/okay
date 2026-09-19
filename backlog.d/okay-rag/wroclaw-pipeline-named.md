- [ ] wroclaw-pipeline-named — `Gtfs.departures` in okay-spark's
      TestWroclawAlgebra still writes the shape of every join step in
      a trailing comment (`// trip -> (time, (route, service))`),
      because its tuples cannot. `GtfsNamed.departures` beside it is
      the same pipeline with the payloads named and those comments
      deleted, proven equal on the real feed (4 593 288 departures,
      identical summary) — it exists as the measurement from
      named-tuples-stage0 and as the regression test for
      named-tuple-unblock. Merging the two into one named pipeline is
      the obvious follow-on and was deliberately NOT done in either
      lane: both had a rule that nothing migrates. Whoever takes it
      keeps the equality test by comparing against a recorded summary
      rather than against a twin that no longer exists.
