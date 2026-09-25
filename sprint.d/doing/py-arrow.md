- [ ] py-arrow — frames via pyarrow (twin of r-arrow). RE-FILED
      2026-09-07 with an honest number: the measurement meant to
      justify it found that 60% of a 500k-row frame's 9.7 s round trip
      was OUR OWN `Json.parse` taking the lossless road
      (json-parse-fast-road). The same frame is now 0.94 s, of which
      the Python side is roughly half and our encode 0.3 s. Arrow
      would still take the serialization hop out, but "the JSON-frame
      road hurts" is ten times less true than when this was filed and
      no consumer has asked. Measure again before building.
      PROMOTED 2026-09-25 (operator): measured again first — the Python
      side of a 500k-row frame is 909 ms today and 0.5 ms as Arrow IPC
      (62 ms to a dict); specs/py-arrow.md, four stages.
